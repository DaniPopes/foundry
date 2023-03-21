use crate::{AsDoc, AsDocResult};
use std::fmt;

/// The markdown format.
#[derive(Debug)]
pub enum Markdown<'a> {
    /// H1 heading item.
    H1(&'a str),
    /// H2 heading item.
    H2(&'a str),
    /// H3 heading item.
    H3(&'a str),
    /// Italic item.
    Italic(&'a str),
    /// Bold item.
    Bold(&'a str),
    /// Link item.
    Link(&'a str, &'a str),
    /// Code item.
    Code(&'a str),
    /// Code block item.
    CodeBlock(&'a str, &'a str),
}

impl<'a> AsDoc for Markdown<'a> {
    fn as_doc(&self) -> AsDocResult {
        Ok(self.to_string())
    }
}

impl<'a> fmt::Display for Markdown<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            // add an extra new line after headings
            Self::H1(val) => writeln!(f, "# {val}"),
            Self::H2(val) => writeln!(f, "## {val}"),
            Self::H3(val) => writeln!(f, "### {val}"),

            Self::Italic(val) => write!(f, "*{val}*"),
            Self::Bold(val) => write!(f, "**{val}**"),

            Self::Link(val, link) => write!(f, "[{val}]({link})"),

            Self::Code(val) => write!(f, "`{val}`"),
            Self::CodeBlock(lang, val) => write!(f, "```{lang}\n{val}\n```"),
        }
    }
}
