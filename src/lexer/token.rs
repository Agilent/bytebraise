use thiserror::Error;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Token {
    pub(crate) kind: TokenKind,
    /// Byte length of token
    pub(crate) len: usize,
}

impl Token {
    pub fn is_whitespace(&self) -> bool {
        self.kind.is_whitespace()
    }

    pub fn is_end_of_input(&self) -> bool {
        self.kind.is_end_of_input()
    }

    pub fn is_assignment_operator(&self) -> bool {
        self.kind.is_assignment_operator()
    }

    pub fn get_fixed_str(&self) -> Option<&'static str> {
        self.kind.get_fixed_str()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Error(LexerError),

    AddHandler,
    AddTask,
    After,
    Before,
    CloseParenthesis,
    Colon,
    ColonEquals,
    Comment,
    DefaultEquals,
    DelTask,
    DotEquals,
    DoubleQuote,
    DoubleQuotedValue,
    EndOfInput,
    Equals,
    EqualsDot,
    EqualsPlus,
    EscapedNewline,
    Export,
    ExportFunctions,
    Fakeroot,
    Identifier,
    DirectiveArgument,
    Include,
    Inherit,
    Newline,
    OpenParenthesis,
    PlusEquals,
    Python,
    PythonDefKeyword,
    PythonDefFunctionName,
    PythonDefFunctionArgs,
    PythonDefFunctionBody,
    Require,
    SingleQuote,
    SingleQuotedValue,
    SquareCloseBracket,
    SquareOpenBracket,
    Task,
    UnquotedValue,
    Unset,
    Varflag,
    WeakEquals,
    Whitespace,
}

#[derive(Error, Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[error("{kind}")]
pub struct LexerError {
    pub(super) kind: LexerErrorKind,
}

#[derive(Error, Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub enum LexerErrorKind {
    #[error("TODO")]
    UnterminatedPythonDefDeclaration,
    #[error("TODO")]
    UnterminatedQuotedValue,
    #[error("TODO")]
    UnterminatedLineContinuation,
    #[error("TODO")]
    UnparsedEmbeddedQuotes,
}

impl TokenKind {
    pub fn is_whitespace(&self) -> bool {
        matches!(self, TokenKind::Whitespace | TokenKind::Newline)
    }

    pub fn is_end_of_input(&self) -> bool {
        matches!(self, TokenKind::EndOfInput)
    }

    pub fn is_assignment_operator(&self) -> bool {
        matches!(
            self,
            TokenKind::Equals
                | TokenKind::EqualsPlus
                | TokenKind::EqualsDot
                | TokenKind::WeakEquals
                | TokenKind::DefaultEquals
                | TokenKind::DotEquals
                | TokenKind::PlusEquals
                | TokenKind::ColonEquals
        )
    }

    pub fn get_fixed_str(&self) -> Option<&'static str> {
        let ret = match self {
            TokenKind::AddHandler => "addhandler",
            TokenKind::AddTask => "addtask",
            TokenKind::After => "after",
            TokenKind::Before => "before",
            TokenKind::CloseParenthesis => ")",
            TokenKind::Colon => ":",
            TokenKind::ColonEquals => ":=",
            TokenKind::DefaultEquals => "?=",
            TokenKind::DelTask => "deltask",
            TokenKind::DotEquals => ".=",
            TokenKind::DoubleQuote => "\"",
            TokenKind::Equals => "=",
            TokenKind::EqualsDot => "=.",
            TokenKind::EqualsPlus => "=+",
            TokenKind::EscapedNewline => "\\\n",
            TokenKind::Export => "export",
            TokenKind::ExportFunctions => "EXPORT_FUNCTIONS",
            TokenKind::Fakeroot => "fakeroot",
            TokenKind::Include => "include",
            TokenKind::Inherit => "inherit",
            TokenKind::Newline => "\n",
            TokenKind::OpenParenthesis => "(",
            TokenKind::PlusEquals => "+=",
            TokenKind::Python => "python",
            TokenKind::PythonDefKeyword => "def",
            TokenKind::Require => "require",
            TokenKind::SingleQuote => "'",
            TokenKind::SquareCloseBracket => "]",
            TokenKind::SquareOpenBracket => "[",
            TokenKind::Unset => "unset",
            TokenKind::WeakEquals => "??=",
            _ => return None,
        };

        Some(ret)
    }
}

pub fn make_fixed_str_token(kind: crate::lexer::token::TokenKind) -> Token {
    Token {
        kind,
        len: kind.get_fixed_str().unwrap().len(),
    }
}

#[macro_export]
macro_rules! T {
    ['('] => { $crate::lexer::token::make_fixed_str_token($crate::lexer::token::TokenKind::OpenParenthesis) } ;
    [')'] => { crate::lexer::token::make_fixed_str_token(crate::lexer::token::TokenKind::CloseParenthesis) } ;
    [=] => { crate::lexer::token::make_fixed_str_token(crate::lexer::token::TokenKind::Equals) } ;
    [+=] => { crate::lexer::token::make_fixed_str_token(crate::lexer::token::TokenKind::PlusEquals) } ;
    [=+] => { crate::lexer::token::make_fixed_str_token(crate::lexer::token::TokenKind::EqualsPlus) } ;
    [.=] => { crate::lexer::token::make_fixed_str_token(crate::lexer::token::TokenKind::DotEquals) } ;
    [=.] => { crate::lexer::token::make_fixed_str_token(crate::lexer::token::TokenKind::EqualsDot) } ;
    [:=] => { crate::lexer::token::make_fixed_str_token(crate::lexer::token::TokenKind::ColonEquals) } ;
    [?=] => { crate::lexer::token::make_fixed_str_token(crate::lexer::token::TokenKind::DefaultEquals) } ;
    [??=] => { crate::lexer::token::make_fixed_str_token(crate::lexer::token::TokenKind::WeakEquals) } ;
    [def] => { crate::lexer::token::make_fixed_str_token(crate::lexer::token::TokenKind::PythonDefKeyword) } ;
    [:] => { crate::lexer::token::make_fixed_str_token(crate::lexer::token::TokenKind::Colon) } ;
    [addhandler] => { crate::lexer::token::make_fixed_str_token(crate::lexer::token::TokenKind::AddHandler) } ;
    [addtask] => { crate::lexer::token::make_fixed_str_token(crate::lexer::token::TokenKind::AddTask) } ;
    [deltask] => { crate::lexer::token::make_fixed_str_token(crate::lexer::token::TokenKind::DelTask) } ;
    [inherit] => { crate::lexer::token::make_fixed_str_token(crate::lexer::token::TokenKind::Inherit) } ;
    [include] => { crate::lexer::token::make_fixed_str_token(crate::lexer::token::TokenKind::Include) } ;
    [require] => { crate::lexer::token::make_fixed_str_token(crate::lexer::token::TokenKind::Require) } ;
    [export] => { crate::lexer::token::make_fixed_str_token(crate::lexer::token::TokenKind::Export) } ;
    [unset] => { crate::lexer::token::make_fixed_str_token(crate::lexer::token::TokenKind::Unset) } ;
    [after] => { crate::lexer::token::make_fixed_str_token(crate::lexer::token::TokenKind::After) } ;
    [before] => { crate::lexer::token::make_fixed_str_token(crate::lexer::token::TokenKind::Before) } ;

}

pub use T;
