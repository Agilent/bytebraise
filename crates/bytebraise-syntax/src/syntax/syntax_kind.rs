use bytebraise_lexer::token::TokenKind;

pub fn syntax_kind_for_token_kind(token_kind: TokenKind) -> SyntaxKind {
    match token_kind {
        TokenKind::Error(_) => unimplemented!("TODO return error too"),
        TokenKind::AddPyLib => SyntaxKind::AddPyLib,
        TokenKind::AddHandler => SyntaxKind::AddHandler,
        TokenKind::AddTask => SyntaxKind::AddTask,
        TokenKind::After => SyntaxKind::After,
        TokenKind::Before => SyntaxKind::Before,
        TokenKind::CloseParenthesis => SyntaxKind::CloseParenthesis,
        TokenKind::Colon => SyntaxKind::Colon,
        TokenKind::ColonEquals => SyntaxKind::ColonEquals,
        TokenKind::Comment => SyntaxKind::Comment,
        TokenKind::DefaultEquals => SyntaxKind::DefaultEquals,
        TokenKind::DelTask => SyntaxKind::DelTask,
        TokenKind::DotEquals => SyntaxKind::DotEquals,
        TokenKind::DoubleQuote => SyntaxKind::DoubleQuote,
        TokenKind::DoubleQuotedValue => SyntaxKind::DoubleQuotedValue,
        TokenKind::EndOfInput => SyntaxKind::EndOfInput,
        TokenKind::Equals => SyntaxKind::Equals,
        TokenKind::EqualsDot => SyntaxKind::EqualsDot,
        TokenKind::EqualsPlus => SyntaxKind::EqualsPlus,
        TokenKind::EscapedNewline => SyntaxKind::EscapedNewline,
        TokenKind::Export => SyntaxKind::Export,
        TokenKind::ExportFunctions => SyntaxKind::ExportFunctions,
        TokenKind::Fakeroot => SyntaxKind::Fakeroot,
        TokenKind::Identifier => SyntaxKind::Identifier,
        TokenKind::DirectiveArgument => SyntaxKind::DirectiveArgument,
        TokenKind::Include => SyntaxKind::Include,
        TokenKind::Inherit => SyntaxKind::Inherit,
        TokenKind::Newline => SyntaxKind::Newline,
        TokenKind::OpenParenthesis => SyntaxKind::OpenParenthesis,
        TokenKind::PlusEquals => SyntaxKind::PlusEquals,
        TokenKind::Python => SyntaxKind::Python,
        TokenKind::PythonDefKeyword => SyntaxKind::PythonDefKeyword,
        TokenKind::PythonDefFunctionName => SyntaxKind::PythonDefFunctionName,
        TokenKind::PythonDefFunctionArgs => SyntaxKind::PythonDefFunctionArgs,
        TokenKind::PythonDefFunctionBody => SyntaxKind::PythonDefFunctionBody,
        TokenKind::Require => SyntaxKind::Require,
        TokenKind::SingleQuote => SyntaxKind::SingleQuote,
        TokenKind::SingleQuotedValue => SyntaxKind::SingleQuotedValue,
        TokenKind::SquareCloseBracket => SyntaxKind::SquareCloseBracket,
        TokenKind::SquareOpenBracket => SyntaxKind::SquareOpenBracket,
        TokenKind::Task => SyntaxKind::Task,
        TokenKind::UnquotedValue => SyntaxKind::UnquotedValue,
        TokenKind::Unset => SyntaxKind::Unset,
        TokenKind::Varflag => SyntaxKind::Varflag,
        TokenKind::WeakEquals => SyntaxKind::WeakEquals,
        TokenKind::Whitespace => SyntaxKind::Whitespace,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    Error = 0,

    // Tokens
    AddHandler,
    AddTask,
    AddPyLib,
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
    Whitespace = 0x200, // << last token

    // Compound nodes representing top-level metadata items
    AssignmentNode,
    ExportNode,
    IncludeNode,
    RequireNode,
    TaskNode,
    UnsetNode,
    ExportFunctionsNode,
    InheritNode,
    PythonDefNode,
    AddTaskNode,
    DelTaskNode,
    AddHandlerNode,

    // Helper compound nodes that do not appear at the root of the AST
    IdentifierExpressionNode,

    // Compound top-level node (i.e. the metadata or config file)
    // Must be the last entry in the enum
    RootNode,
}

impl SyntaxKind {
    pub fn is_token(&self) -> bool {
        (*self as u16) <= (SyntaxKind::Whitespace as u16)
    }

    pub fn is_node(&self) -> bool {
        !self.is_token()
    }

    pub fn is_whitespace(&self) -> bool {
        matches!(self, SyntaxKind::Whitespace | SyntaxKind::Newline)
    }

    pub fn is_end_of_input(&self) -> bool {
        matches!(self, SyntaxKind::EndOfInput)
    }

    pub fn is_assignment_operator(&self) -> bool {
        matches!(
            self,
            SyntaxKind::Equals
                | SyntaxKind::EqualsPlus
                | SyntaxKind::EqualsDot
                | SyntaxKind::WeakEquals
                | SyntaxKind::DefaultEquals
                | SyntaxKind::DotEquals
                | SyntaxKind::PlusEquals
                | SyntaxKind::ColonEquals
        )
    }
}
