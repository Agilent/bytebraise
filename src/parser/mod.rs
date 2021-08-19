use thiserror::Error;

pub mod parser;
pub mod tests;

pub use parser::parse_bitbake_from_str;

#[derive(Error, Debug)]
#[error("{line_no}: {kind}")]
pub struct ParseError {
    line_no: usize,
    kind: ParseErrorKind,
}

#[derive(Error, Debug, Eq, PartialEq)]
pub enum ParseErrorKind {
    #[error("AAA")]
    A,
    #[error("AAA")]
    B,
}

pub type ParserResult<T> = Result<T, ParseError>;

pub type Identifier<'ast> = &'ast str;

#[derive(Debug, PartialEq, Eq)]
pub enum BitBakeParserMode {
    /// .conf
    Conf,
    /// .bb, .bbclass, .inc
    BB,
}

impl BitBakeParserMode {
    pub fn is_conf(&self) -> bool {
        matches!(self, BitBakeParserMode::Conf)
    }

    pub fn is_bb(&self) -> bool {
        matches!(&self, BitBakeParserMode::BB)
    }
}
