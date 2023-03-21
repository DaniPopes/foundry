use crate::{
    document::DocumentContent,
    helpers::{merge_toml_table, source_files_iter},
    AsDoc, Buffer, Document, ParseItem, ParseSource, Parser, Preprocessor,
};
use forge_fmt::{FormatterConfig, Visitable};
use foundry_config::DocConfig;
use foundry_utils::glob::expand_globs;
use itertools::Itertools;
use mdbook::MDBook;
use rayon::prelude::*;
use std::{
    cmp::Ordering,
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
};
use toml::value;

/// Build Solidity documentation for a project from natspec comments.
/// The builder parses the source files using [Parser],
/// then formats and writes the elements as the output.
#[derive(Debug)]
pub struct DocBuilder<'p> {
    /// The project root
    pub root: PathBuf,
    /// Path to Solidity source files.
    pub sources: PathBuf,
    /// Flag whether to build mdbook.
    pub should_build: bool,
    /// Documentation configuration.
    pub config: DocConfig,
    /// The array of preprocessors to apply.
    pub preprocessors: &'p [&'p dyn Preprocessor],
    /// The formatter config.
    pub fmt: FormatterConfig,
}

// TODO: consider using `tfio`
impl<'p> DocBuilder<'p> {
    const SRC: &'static str = "src";
    const SOL_EXT: &'static str = "sol";
    const README: &'static str = "README.md";
    const SUMMARY: &'static str = "SUMMARY.md";

    /// Create new instance of builder.
    pub fn new(root: PathBuf, sources: PathBuf) -> Self {
        Self {
            root,
            sources,
            should_build: false,
            config: DocConfig::default(),
            preprocessors: Default::default(),
            fmt: Default::default(),
        }
    }

    /// Set `shoul_build` flag on the builder
    pub fn with_should_build(mut self, should_build: bool) -> Self {
        self.should_build = should_build;
        self
    }

    /// Set config on the builder.
    pub fn with_config(mut self, config: DocConfig) -> Self {
        self.config = config;
        self
    }

    /// Set formatter config on the builder.
    pub fn with_fmt(mut self, fmt: FormatterConfig) -> Self {
        self.fmt = fmt;
        self
    }

    /// Set preprocessors on the builder.
    pub fn with_preprocessors(mut self, preprocessors: &'p [&'p dyn Preprocessor]) -> Self {
        self.preprocessors = preprocessors;
        self
    }

    /// Get the output directory
    pub fn out_dir(&self) -> PathBuf {
        self.root.join(&self.config.out)
    }

    /// Parse the sources and build the documentation.
    pub fn build(self) -> eyre::Result<()> {
        // Expand ignore globs
        let ignored = expand_globs(&self.root, self.config.ignore.iter())?;

        // Collect and parse source files
        let sources = source_files_iter(&self.sources)
            .filter(|file| !ignored.contains(file))
            .collect::<Vec<_>>();

        if sources.is_empty() {
            println!("No sources detected at {}", self.sources.display());
            return Ok(())
        }

        let documents = sources
            .par_iter()
            .enumerate()
            .map(|(i, path)| {
                // Read and parse source file
                let source = fs::read_to_string(path)?;
                let (mut source_unit, comments) =
                    solang_parser::parse(&source, i).map_err(|diags| {
                        eyre::eyre!(
                            "Failed to parse Solidity code for {}\nDebug info: {:?}",
                            path.display(),
                            diags
                        )
                    })?;

                // Visit the parse tree
                let mut parser = Parser::new(comments, source).with_config(self.fmt.clone());
                source_unit
                    .visit(&mut parser)
                    .map_err(|err| eyre::eyre!("Failed to parse source: {err}"))?;

                // Split the parsed items on top-level constants and rest.
                let (items, consts) =
                    parser.into_items().into_iter().partition::<Vec<_>, _>(|item| {
                        !matches!(item.source, ParseSource::Variable(_))
                    });

                // Attempt to group overloaded top-level functions
                let mut remaining = Vec::with_capacity(items.len());
                let mut funcs: HashMap<String, Vec<ParseItem>> = HashMap::default();
                for item in items {
                    if matches!(item.source, ParseSource::Function(_)) {
                        funcs.entry(item.source.ident().to_string()).or_default().push(item);
                    } else {
                        // Put the item back
                        remaining.push(item);
                    }
                }
                let mut iter = funcs.into_iter();

                let overloaded: HashMap<_, _> =
                    iter.by_ref().filter(|(_, v)| v.len() > 1).collect();

                // Each regular item will be written into its own file.
                let relative_path = path.strip_prefix(&self.root)?;
                let mut files = remaining
                    .into_iter()
                    .chain(iter.flat_map(|(_, v)| v))
                    .map(|item| {
                        let relative_path = relative_path.join(item.filename());
                        let target_path = self.config.out.join(Self::SRC).join(relative_path);
                        let ident = item.source.ident().to_string();
                        Document::new(path.clone(), target_path)
                            .with_content(DocumentContent::Single(item), ident)
                    })
                    .collect::<Vec<_>>();

                // If top-level constants exist, they will be written to the same file.
                if !consts.is_empty() {
                    let filestem = path.file_stem().and_then(|stem| stem.to_str());

                    let filename = {
                        let mut name = "constants".to_owned();
                        if let Some(stem) = filestem {
                            name.push('.');
                            name.push_str(stem);
                        }
                        name.push_str(".md");
                        name
                    };
                    let relative_path = relative_path.join(filename);
                    let target_path = self.config.out.join(Self::SRC).join(relative_path);

                    let identity = match filestem {
                        Some(stem) if stem.to_lowercase().contains("constants") => stem.to_owned(),
                        Some(stem) => stem.to_owned() + " constants",
                        None => "constants".to_owned(),
                    };

                    files.push(
                        Document::new(path.clone(), target_path)
                            .with_content(DocumentContent::Constants(consts), identity),
                    )
                }

                // If overloaded functions exist, they will be written to the same file
                if !overloaded.is_empty() {
                    for (ident, funcs) in overloaded {
                        let filename = funcs.first().expect("no overloaded functions").filename();
                        let relative_path = relative_path.join(filename);
                        let target_path = self.config.out.join(Self::SRC).join(relative_path);
                        files.push(
                            Document::new(path.clone(), target_path)
                                .with_content(DocumentContent::OverloadedFunctions(funcs), ident),
                        );
                    }
                }

                Ok(files)
            })
            .collect::<eyre::Result<Vec<_>>>()?;

        // Flatten results and apply preprocessors to files
        let mut documents = documents.into_iter().flatten().collect::<Vec<_>>();
        for p in self.preprocessors.iter() {
            p.preprocess(&mut documents)?;
        }

        // Sort the results
        // note: this doesn't work because of lifetimes:
        // documents.sort_unstable_by_key(|doc| &doc.item_path);
        documents.sort_unstable_by(|a, b| a.item_path.cmp(&b.item_path));

        // Write mdbook related files
        self.write_mdbook(documents)?;

        // Build the book if requested
        if self.should_build {
            MDBook::load(self.out_dir())
                .and_then(|book| book.build())
                .map_err(|err| eyre::eyre!("failed to build book: {err:?}"))?;
        }

        Ok(())
    }

    fn write_mdbook(&self, documents: Vec<Document>) -> eyre::Result<()> {
        let out_dir = self.out_dir();
        let out_dir_src = out_dir.join(Self::SRC);
        fs::create_dir_all(&out_dir_src)?;

        // Write readme content if any
        let readme_content = {
            let src_readme = self.sources.join(Self::README);
            let root_readme = self.root.join(Self::README);
            if src_readme.exists() {
                fs::read_to_string(src_readme)?
            } else if root_readme.exists() {
                fs::read_to_string(root_readme)?
            } else {
                String::new()
            }
        };
        let readme_path = out_dir_src.join(Self::README);
        fs::write(&readme_path, readme_content)?;

        // Write summary and section readmes
        let mut summary = Buffer::default();
        summary.write_title("Summary")?;
        summary.write_link_list_item("Home", Self::README, 0)?;
        self.write_summary_section(&mut summary, &documents.iter().collect::<Vec<_>>(), None, 0)?;
        fs::write(out_dir_src.join(Self::SUMMARY), summary.finish())?;

        // Write solidity syntax highlighting
        fs::write(out_dir.join("solidity.min.js"), include_str!("../static/solidity.min.js"))?;

        // Write css files
        fs::write(out_dir.join("book.css"), include_str!("../static/book.css"))?;

        // Write book config
        fs::write(self.out_dir().join("book.toml"), self.book_config()?)?;

        // Write .gitignore
        let gitignore = "book/";
        fs::write(self.out_dir().join(".gitignore"), gitignore)?;

        // Write doc files
        for document in documents {
            fs::create_dir_all(
                document
                    .target_path
                    .parent()
                    .ok_or(eyre::format_err!("empty target path; noop"))?,
            )?;
            fs::write(&document.target_path, document.as_doc()?)?;
        }

        Ok(())
    }

    fn book_config(&self) -> eyre::Result<String> {
        // Read the default book first
        let mut book: value::Table = toml::from_str(include_str!("../static/book.toml"))?;
        book["book"]
            .as_table_mut()
            .unwrap()
            .insert(String::from("title"), self.config.title.clone().into());
        if let Some(ref repo) = self.config.repository {
            book["output"].as_table_mut().unwrap()["html"]
                .as_table_mut()
                .unwrap()
                .insert(String::from("git-repository-url"), repo.clone().into());
        }

        // Attempt to find the user provided book path
        let book_path = {
            if self.config.book.is_file() {
                Some(self.config.book.clone())
            } else {
                let book_path = self.config.book.join("book.toml");
                if book_path.is_file() {
                    Some(book_path)
                } else {
                    None
                }
            }
        };

        // Merge two book configs
        if let Some(book_path) = book_path {
            merge_toml_table(&mut book, toml::from_str(&fs::read_to_string(book_path)?)?);
        }

        Ok(toml::to_string_pretty(&book)?)
    }

    fn write_summary_section(
        &self,
        summary: &mut Buffer,
        files: &[&Document],
        base_path: Option<&Path>,
        depth: usize,
    ) -> eyre::Result<()> {
        if files.is_empty() {
            return Ok(())
        }

        if let Some(path) = base_path {
            let title = path.iter().last().unwrap().to_string_lossy();
            if depth == 1 {
                summary.write_title(&title)?;
            } else {
                let summary_path = path.join(Self::README);
                summary.write_link_list_item(
                    &format!("❱ {title}"),
                    &summary_path.display().to_string(),
                    depth - 1,
                )?;
            }
        }

        // Group entries by path depth
        let mut grouped = HashMap::new();
        for file in files {
            let path = file.item_path.strip_prefix(&self.root)?;
            let key = path.iter().take(depth + 1).collect::<PathBuf>();
            grouped.entry(key).or_insert_with(Vec::new).push(*file);
        }

        // Sort entries by path depth
        let grouped = grouped.into_iter().sorted_by(|(a, _), (b, _)| {
            let is_sol = |p: &Path| p.extension().map_or(false, |ext| ext == Self::SOL_EXT);
            let sol_a = is_sol(a);
            let sol_b = is_sol(b);
            if sol_a == sol_b {
                a.cmp(b)
            } else if sol_a {
                Ordering::Greater
            } else {
                Ordering::Less
            }
        });

        let mut readme = Buffer::new();
        readme.write_raw("\n\n# Contents\n")?;
        for (path, files) in grouped {
            if path.extension().map_or(false, |ext| ext.eq(Self::SOL_EXT)) {
                for file in files {
                    let ident = &file.identity;

                    let summary_path = file
                        .target_path
                        .strip_prefix(self.out_dir().strip_prefix(&self.root)?.join(Self::SRC))?;
                    summary.write_link_list_item(
                        ident,
                        &summary_path.display().to_string(),
                        depth,
                    )?;

                    let readme_path = base_path
                        .map(|path| summary_path.strip_prefix(path))
                        .transpose()?
                        .unwrap_or(summary_path);
                    readme.write_link_list_item(ident, &readme_path.display().to_string(), 0)?;
                }
            } else {
                let name = path.iter().last().unwrap().to_string_lossy();
                let readme_path = Path::new("/").join(&path).display().to_string();
                readme.write_link_list_item(&name, &readme_path, 0)?;
                self.write_summary_section(summary, &files, Some(&path), depth + 1)?;
            }
        }
        if !readme.is_empty() {
            if let Some(path) = base_path {
                let path = self.out_dir().join(Self::SRC).join(path);
                fs::create_dir_all(&path)?;
                fs::write(path.join(Self::README), readme.finish())?;
            }
        }
        Ok(())
    }
}
