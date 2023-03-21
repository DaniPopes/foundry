use std::path::{Path, PathBuf};
use toml::{value::Table, Value};
use walkdir::WalkDir;

/// Merge original toml table with the override.
pub(crate) fn merge_toml_table(table: &mut Table, override_table: Table) {
    for (key, override_value) in override_table {
        match table.get_mut(&key) {
            Some(Value::Table(inner_table)) => {
                // Override value must be a table, otherwise discard
                if let Value::Table(inner_override) = override_value {
                    merge_toml_table(inner_table, inner_override);
                }
            }
            Some(Value::Array(inner_array)) => {
                // Override value must be an array, otherwise discard
                if let Value::Array(inner_override) = override_value {
                    for entry in inner_override {
                        if !inner_array.contains(&entry) {
                            inner_array.push(entry);
                        }
                    }
                }
            }
            _ => {
                table.insert(key, override_value);
            }
        };
    }
}

/// Returns an iterator that yields all solidity/yul files funder under the given root path or the
/// `root` itself, if it is a sol/yul file
///
/// This also follows symlinks.
///
/// Taken from `ethers_solc::utils::source_files_iter`
pub(crate) fn source_files_iter(root: impl AsRef<Path>) -> impl Iterator<Item = PathBuf> {
    WalkDir::new(root)
        .follow_links(true)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().is_file())
        .filter(|e| {
            e.path().extension().map(|ext| (ext == "sol") || (ext == "yul")).unwrap_or_default()
        })
        .map(|e| e.path().into())
}
