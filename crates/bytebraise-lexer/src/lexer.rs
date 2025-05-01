use std::ops::Range;

use muncher::Muncher;
use phf::{self, phf_map};
use pretty_assertions::assert_eq;

use crate::token::{LexerError, LexerErrorKind, Token, TokenKind};

pub type Span = Range<usize>;

#[inline]
fn scan_comment(start: usize, m: &mut Muncher) -> Token {
    while let Some(c) = m.peek() {
        match c {
            '\n' => {
                // Done processing this line
                break;
            }
            '\\' => {
                let f = m.fork();
                f.peek();
                if let Some('\n') = f.peek() {
                    break;
                }
                m.eat();
            }
            _ => {
                m.eat();
            }
        }
    }

    Token {
        len: m.position() - start,
        kind: TokenKind::Comment,
    }
}

static BITBAKE_KEYWORDS: phf::Map<&'static str, TokenKind> = phf_map! {
    "after" => TokenKind::After,
    "addtask" => TokenKind::AddTask,
    "addhandler" => TokenKind::AddHandler,
    "before" => TokenKind::Before,
    "def" => TokenKind::PythonDefKeyword,
    "deltask" => TokenKind::DelTask,
    "export" => TokenKind::Export,
    "fakeroot" => TokenKind::Fakeroot,
    "inherit" => TokenKind::Inherit,
    "include" => TokenKind::Include,
    "python" => TokenKind::Python,
    "require" => TokenKind::Require,
    "unset" => TokenKind::Unset,
    "EXPORT_FUNCTIONS" => TokenKind::ExportFunctions,
};

#[inline]
fn scan_keyword(first_char: char, m: &mut Muncher) -> Option<Token> {
    'outer: for keyword in BITBAKE_KEYWORDS.entries() {
        if first_char == keyword.0.chars().next().unwrap() {
            let f = m.fork();

            for c in keyword.0[1..].chars() {
                if Some(&c) != f.peek() {
                    continue 'outer;
                }
            }

            // Make sure there is a whitespace character or parenthesis after the keyword
            if !matches!(f.peek(), Some(&' ' | &'(')) {
                continue;
            }

            // Only eat len() - 1, since the first character was already been eaten by caller
            for _ in 0..keyword.0.len() - 1 {
                m.eat();
            }

            return Some(Token {
                kind: *keyword.1,
                len: keyword.0.len(),
            });
        }
    }

    None
}

fn is_bitbake_identifier_char(c: char) -> bool {
    // a-zA-Z0-9\-_+.${}/~
    is_bitbake_first_identifier_char(c)
        || match c {
            '-' | '+' | '.' | '$' | '{' | '}' | '/' | '~' | ':' => true,
            _ => false,
        }
}

fn is_bitbake_first_identifier_char(c: char) -> bool {
    match c {
        c if c.is_alphanumeric() => true,
        '_' => true,
        _ => false,
    }
}

fn is_bitbake_varflag_char(c: char) -> bool {
    // a-zA-Z0-9\-_+.
    match c {
        c if c.is_alphanumeric() => true,
        '-' | '_' | '+' | '.' => true,
        _ => false,
    }
}

#[inline]
fn scan_varflag(start: usize, m: &mut Muncher) -> Token {
    m.eat_until_count(|c| !is_bitbake_varflag_char(*c));

    assert_eq!(m.eat(), Some(']'));
    let p = m.position();

    Token {
        kind: TokenKind::Varflag,
        len: p - start,
    }
}

#[inline]
fn scan_task(start: usize, m: &mut Muncher) -> Token {
    let mut on_new_line = false;

    // A bitbake task ends on the first line with a '}' by itself
    while let Some(c) = m.eat() {
        match c {
            '\n' => {
                on_new_line = true;
            }
            '}' if on_new_line => {
                let ret = Token {
                    kind: TokenKind::Task,
                    len: m.position() - start,
                };
                assert!(
                    matches!(m.peek(), None | Some(&' ') | Some(&'\n')),
                    "{}",
                    "'}' should be on a line by itself"
                );
                m.reset_peek();
                return ret;
            }
            _ => {
                on_new_line = false;
            }
        }
    }

    panic!("TODO");
}

#[inline]
fn scan_python_def_body(start_of_def: usize, m: &mut Muncher) -> Token {
    // A Python def function ends on the first non-indented line (or end of input)
    while let Some(c) = m.peek() {
        match c {
            '\n' => {
                // If the next character is not whitespace, then the Python def has ended.
                match m.peek() {
                    // Include newlines so use |is_whitespace| instead of |is_space_or_tab|
                    Some(c2) if c2.is_whitespace() => {
                        m.eat();
                        // Keep consuming - still in def
                        continue;
                    }
                    _ => {
                        break;
                    }
                }
            }
            _ => {
                m.eat();
            }
        }
    }

    Token {
        kind: TokenKind::PythonDefFunctionBody,
        len: m.position() - start_of_def,
    }
}

/// A big feature BitBake is inline Python expansion; for more info see:
/// https://www.yoctoproject.org/docs/latest/bitbake-user-manual/bitbake-user-manual.html#inline-python-variable-expansion
///
/// This permits assignments like the following:
/// VAR = "${@"value" if True else "another value\" with quote :)"}"
///
/// As a consequence the way BitBake parses quoted values with embedded quotes is strange.
///
/// This is not allowed:
/// VAR = " "\
///     library1 "
///
/// .. but this is parsed as a single assignment:
/// VAR = " ""\
///     library1 "
///
/// ... and this is also fine:
/// VAR = " " " \
///      library1 "
///
/// ... and so is this:
/// VAR = " "\
/// ""
///
/// Likewise, this is actually a single assignment to VAR:
/// VAR = " " \
/// VAR2 = ""
///
/// ...but adding a newline between the lines causes a parse error:
/// VAR = " " \
///
/// VAR2 = ""
#[inline]
fn scan_quoted_value(start: usize, m: &mut Muncher, quote_char: char) -> Token {
    let start_of_value_pos = m.position();
    // Keep track of positions of escaped newlines so we can generate the escaped value later
    let mut escaped_newlines_positions = 0;

    // Count the number of embedded quotes we encounter. From the examples above, you can see that
    // BitBake only allows embedded quotes in values if there are at least 2 (possibly on different
    // lines in the case of multiline values)
    let mut embedded_quotes = 0;

    let mut last_char = None;
    'outer: while let Some(c) = m.eat() {
        last_char = Some(c);

        match c {
            c if c == quote_char => {
                // If the quote is followed by end-of-input or a newline (possibly with
                // whitespace between), consider this value done
                loop {
                    match m.peek() {
                        Some(p) if *p == ' ' || *p == '\t' => continue,
                        None | Some('\n') => {
                            m.reset_peek();
                            break 'outer;
                        }
                        _ => {
                            embedded_quotes += 1;
                            m.reset_peek();
                            continue 'outer;
                        }
                    }
                }
            }
            '\\' => {
                let p = m.peek();
                m.reset_peek();
                if let Some('\n') = p {
                    escaped_newlines_positions += 1;
                    assert_eq!(m.eat(), Some('\n'));

                    // Escaped newline followed by end-of-input or an empty line is an error
                    let p = m.peek();
                    m.reset_peek();
                    if let None | Some('\n') = p {
                        let end = m.position();
                        let e = LexerError {
                            kind: LexerErrorKind::UnterminatedLineContinuation,
                        };
                        return Token {
                            len: end - start_of_value_pos,
                            kind: TokenKind::Error(e),
                        };
                    }
                }
            }

            // Unescaped newlines are not allowed
            '\n' => break,
            _ => {}
        }
    }

    // If the loop terminated early due to end-of-input or unescaped newline, give error
    match last_char {
        None | Some('\n') => {
            let end = m.position();
            return Token {
                len: end - start_of_value_pos,
                kind: TokenKind::Error(LexerError {
                    kind: LexerErrorKind::UnterminatedQuotedValue,
                }),
            };
        }
        Some(c) if c == quote_char => {}
        Some(c) => unreachable!("unexpected char: {}", c),
    }

    let closing_quote_pos = m.position();

    if escaped_newlines_positions > 0 && embedded_quotes > 0 && embedded_quotes < 2 {
        return Token {
            kind: TokenKind::Error(LexerError {
                kind: LexerErrorKind::UnparsedEmbeddedQuotes,
            }),
            len: closing_quote_pos - start,
        };
    }

    let kind = match quote_char {
        '\'' => TokenKind::SingleQuotedValue,
        '"' => TokenKind::DoubleQuotedValue,
        _ => unreachable!(),
    };

    Token {
        kind,
        len: closing_quote_pos - start,
    }
}

#[derive(Debug)]
enum UnquotedValueFirstChar {
    EscapedNewline { position: Range<usize> },
    OtherChar { start: usize },
}

fn scan_unquoted_value(first_char: UnquotedValueFirstChar, m: &mut Muncher) -> Token {
    let start_of_value_pos = match &first_char {
        UnquotedValueFirstChar::EscapedNewline { position } => position.start,
        UnquotedValueFirstChar::OtherChar { start } => *start,
    };

    // An unquoted value ends at the end of the line (or input).
    'outer: while let Some(c) = m.peek() {
        match c {
            '\\' => {
                m.eat();

                // An escaped newline continues the value. Unlike with quoted values, an escaped
                // newline followed by a blank (empty or entirely whitespace) line is not an error.
                if let Some('\n') = m.peek() {
                    while let Some(c) = m.peek() {
                        match *c {
                            // Next line is empty, so bail
                            '\n' => break,
                            // Ignore whitespace
                            c if is_space_or_tab(c) => {}
                            // Something other than whitespace or newline means the next line will be
                            // included in this unquoted value
                            _ => {
                                continue 'outer;
                            }
                        }
                    }

                    // Consume the initial '\n' that we peeked
                    m.eat();
                    break;
                }
            }
            '\n' => {
                break;
            }
            _ => {
                m.eat();
            }
        }
    }
    let end_of_value_pos = m.position();

    Token {
        kind: TokenKind::UnquotedValue,
        len: end_of_value_pos - start_of_value_pos,
    }
}

pub fn is_space_or_tab(c: char) -> bool {
    match c {
        ' ' | '\t' => true,
        _ => false,
    }
}

#[derive(Debug, PartialEq, Eq)]
enum BitbakeLexerState {
    Normal,
    ExpectUnquotedValue,
    LexingPythonDef(LexingPythonDefSubState),
    EatDirectiveArguments,
}

#[derive(Debug, PartialEq, Eq)]
enum LexingPythonDefSubState {
    ExpectName,
    ExpectArgs,
    ExpectBody,
}

pub struct BitbakeLexer<'input> {
    input: &'input str,
    muncher: Muncher<'input>,
    pub(crate) token: Token,
    skip_whitespace: bool,
    state: BitbakeLexerState,
}

impl<'input> BitbakeLexer<'input> {
    pub fn pos(&self) -> (usize, usize) {
        self.muncher.cursor_position()
    }

    pub fn new(input: &'input str, skip_whitespace: bool) -> BitbakeLexer<'input> {
        let mut ret = BitbakeLexer {
            input,
            muncher: Muncher::new(input),
            skip_whitespace,
            token: Token {
                kind: TokenKind::EndOfInput,
                len: 0,
            },
            state: BitbakeLexerState::Normal,
        };
        ret.advance();
        ret
    }

    pub fn token(&self) -> Token {
        self.token.clone()
    }

    pub fn advance(&mut self) {
        self._advance_internal();

        if self.skip_whitespace && self.token.is_whitespace() {
            self.advance();
            if self.token.is_end_of_input() {}
        }
    }

    fn _single_char_token(&self, kind: TokenKind) -> Token {
        Token { kind, len: 1 }
    }

    fn _double_char_token(&self, kind: TokenKind) -> Token {
        Token { kind, len: 2 }
    }

    fn _advance_internal(&mut self) {
        let start = self.muncher.position();
        if let Some(c) = self.muncher.eat() {
            // Common whitespace handling for all modes - just spaces and tabs, newlines are
            // potentially handled differently depending on lexer state
            if is_space_or_tab(c) {
                let whitespace = self.muncher.eat_until_count(|c| !is_space_or_tab(*c));
                self.token = Token {
                    kind: TokenKind::Whitespace,
                    len: whitespace.1 - start,
                };
                return;
            }

            match &mut self.state {
                BitbakeLexerState::LexingPythonDef(sub_state) => match (&sub_state, c) {
                    (LexingPythonDefSubState::ExpectName, c) if c.is_alphanumeric() || c == '_' => {
                        let function_name = self.muncher.eat_until_count(|c| *c == '(');
                        self.token = Token {
                            len: function_name.1 - start,
                            kind: TokenKind::PythonDefFunctionName,
                        };
                        *sub_state = LexingPythonDefSubState::ExpectArgs;
                    }
                    (LexingPythonDefSubState::ExpectName, _) => {
                        unimplemented!("TODO error message: {}", c)
                    }
                    (LexingPythonDefSubState::ExpectArgs, '(') => {
                        self.muncher.eat_until_count(|c| *c == ')');
                        assert!(self.muncher.eat_close_paren());
                        let end = self.muncher.position();
                        self.token = Token {
                            len: end - start,
                            kind: TokenKind::PythonDefFunctionArgs,
                        };
                        *sub_state = LexingPythonDefSubState::ExpectBody;
                    }
                    (LexingPythonDefSubState::ExpectBody, ':') => {
                        self.token = self._single_char_token(TokenKind::Colon);
                    }
                    (LexingPythonDefSubState::ExpectBody, _) => {
                        self.token = scan_python_def_body(start, &mut self.muncher);
                        self.state = BitbakeLexerState::Normal;
                    }
                    _ => unimplemented!("TODO error message"),
                },
                BitbakeLexerState::ExpectUnquotedValue => {
                    match c {
                        '\n' => unimplemented!("newline definitely shouldn't be here"),
                        '\\' => {
                            if self.muncher.peek() == Some(&'\n') {
                                self.muncher.eat();
                                self.token = scan_unquoted_value(
                                    UnquotedValueFirstChar::EscapedNewline {
                                        position: start..self.muncher.position(),
                                    },
                                    &mut self.muncher,
                                );
                                self.state = BitbakeLexerState::Normal;
                            }
                        }
                        _ => {
                            self.token = scan_unquoted_value(
                                UnquotedValueFirstChar::OtherChar { start },
                                &mut self.muncher,
                            )
                        }
                    }

                    self.state = BitbakeLexerState::Normal;
                }
                BitbakeLexerState::EatDirectiveArguments => {
                    match c {
                        '\n' => {
                            self.token = self._single_char_token(TokenKind::Newline);
                            self.state = BitbakeLexerState::Normal;
                        }
                        '\\' => {
                            if self.muncher.peek() == Some(&'\n') {
                                self.muncher.eat();
                                self.token = self._double_char_token(TokenKind::EscapedNewline);
                            } else {
                                unimplemented!();
                            }
                        }
                        _ => {
                            // TODO: should only try to scan keywords for addtask
                            if let Some(token) = scan_keyword(c, &mut self.muncher) {
                                self.token = token;
                            } else if is_bitbake_first_identifier_char(c) {
                                // TODO: relax requirements - e.g. can you do addtask ${VAR}?
                                let extent = self
                                    .muncher
                                    .eat_until_count(|c| !is_bitbake_identifier_char(*c));

                                self.token = Token {
                                    kind: TokenKind::DirectiveArgument,
                                    len: extent.1 - start,
                                };
                            } else {
                                unimplemented!("unexpected character {}", c);
                            }
                        }
                    }
                }
                BitbakeLexerState::Normal => {
                    match c {
                        '#' => self.token = scan_comment(start, &mut self.muncher),
                        '\n' => {
                            self.token = self._single_char_token(TokenKind::Newline);
                            self.state = BitbakeLexerState::Normal;
                        }
                        '[' => match self.muncher.peek() {
                            Some(c) if is_bitbake_varflag_char(*c) => {
                                self.token = scan_varflag(start, &mut self.muncher);
                            }
                            _ => self.token = self._single_char_token(TokenKind::SquareOpenBracket),
                        },
                        // a ']' not part of a varflag, otherwise it would be chomped in '[' handling
                        ']' => self.token = self._single_char_token(TokenKind::SquareCloseBracket),
                        '(' => self.token = self._single_char_token(TokenKind::OpenParenthesis),
                        ')' => self.token = self._single_char_token(TokenKind::CloseParenthesis),
                        '{' => self.token = scan_task(start, &mut self.muncher),
                        '\\' => {
                            if self.muncher.peek() == Some(&'\n') {
                                self.muncher.eat();
                                self.token = self._double_char_token(TokenKind::EscapedNewline);
                            } else {
                                unimplemented!();
                            }
                        }
                        '\'' | '"' => {
                            self.token = scan_quoted_value(start, &mut self.muncher, c);
                        }
                        '=' => match self.muncher.peek() {
                            Some('+') => {
                                self.muncher.eat();
                                self.token = self._double_char_token(TokenKind::EqualsPlus);
                            }
                            Some('.') => {
                                self.muncher.eat();
                                self.token = self._double_char_token(TokenKind::EqualsDot);
                            }
                            _ => self.token = self._single_char_token(TokenKind::Equals),
                        },
                        '.' | '+' | '?' | ':' => match self.muncher.peek() {
                            Some('=') => {
                                self.muncher.eat();
                                match c {
                                    '.' => {
                                        self.token = self._double_char_token(TokenKind::DotEquals)
                                    }
                                    '+' => {
                                        self.token = self._double_char_token(TokenKind::PlusEquals)
                                    }
                                    '?' => {
                                        self.token =
                                            self._double_char_token(TokenKind::DefaultEquals)
                                    }
                                    ':' => {
                                        self.token = self._double_char_token(TokenKind::ColonEquals)
                                    }
                                    _ => panic!("TODO"),
                                }
                            }
                            // handle ??=
                            Some('?') if c == '?' => {
                                if let Some('=') = self.muncher.peek() {
                                    self.muncher.eat();
                                    self.muncher.eat();
                                    self.token = Token {
                                        kind: TokenKind::WeakEquals,
                                        len: 3,
                                    };
                                } else {
                                    panic!("unexpected character");
                                }
                            }
                            _ => panic!("TODO"),
                        },
                        _ => {
                            if let Some(token) = scan_keyword(c, &mut self.muncher) {
                                // Special handling for certain tokens
                                match token.kind {
                                    TokenKind::Inherit
                                    | TokenKind::Include
                                    | TokenKind::Require => {
                                        self.state = BitbakeLexerState::ExpectUnquotedValue
                                    }
                                    TokenKind::PythonDefKeyword => {
                                        self.state = BitbakeLexerState::LexingPythonDef(
                                            LexingPythonDefSubState::ExpectName,
                                        )
                                    }
                                    TokenKind::AddTask
                                    | TokenKind::DelTask
                                    | TokenKind::AddHandler
                                    | TokenKind::ExportFunctions => {
                                        self.state = BitbakeLexerState::EatDirectiveArguments;
                                    }
                                    _ => {}
                                };

                                self.token = token;
                            } else if is_bitbake_first_identifier_char(c) {
                                let extent = self
                                    .muncher
                                    .eat_until_count(|c| !is_bitbake_identifier_char(*c));

                                self.token = Token {
                                    kind: TokenKind::Identifier,
                                    len: extent.1 - start,
                                };
                            } else {
                                unimplemented!("unexpected character {}", c);
                            }
                        }
                    }
                }
            }
        } else {
            self.token = Token {
                kind: TokenKind::EndOfInput,
                len: 0,
            };
        }
    }
}

pub fn tokenize(data: &str) -> Vec<Token> {
    let mut lexer = BitbakeLexer::new(data, false);

    let mut ret = vec![];

    while lexer.token().kind != TokenKind::EndOfInput {
        ret.push(lexer.token());
        lexer.advance();
    }

    ret
}
