#[cfg(test)]
use crate::lexer::lexer::is_space_or_tab;
#[cfg(test)]
use crate::lexer::token::TokenKind;
#[cfg(test)]
use crate::lexer::Token;
#[cfg(test)]
use std::borrow::Cow;

#[cfg(test)]
#[derive(Debug, Eq, PartialEq)]
pub(super) struct TokenMock<'a> {
    pub(super) text: Cow<'a, str>,
    pub(super) kind: TokenKind,
    pub(super) len: usize,
}

#[cfg(test)]
impl<'a> From<&'a str> for TokenMock<'a> {
    fn from(val: &'a str) -> Self {
        match val {
            "\n" => TokenMock {
                text: Cow::Borrowed(val),
                kind: TokenKind::Newline,
                len: val.len(),
            },
            val if val.chars().all(is_space_or_tab) => TokenMock {
                text: Cow::Borrowed(val),
                kind: TokenKind::Whitespace,
                len: val.len(),
            },
            _ => panic!(),
        }
    }
}

#[cfg(test)]
impl<'a> From<Token> for TokenMock<'a> {
    fn from(val: Token) -> Self {
        if let Some(fixed_str) = val.get_fixed_str() {
            return TokenMock {
                kind: val.kind,
                text: Cow::Borrowed(fixed_str),
                len: fixed_str.len(),
            };
        }

        panic!("no fixed representation for token {:?}", val)
    }
}

#[cfg(test)]
impl From<char> for TokenMock<'_> {
    fn from(val: char) -> Self {
        match val {
            '\n' => TokenMock {
                text: Cow::Borrowed("\n"),
                len: 1,
                kind: TokenKind::Newline,
            },
            ' ' => TokenMock {
                text: Cow::Borrowed(" "),
                len: 1,
                kind: TokenKind::Whitespace,
            },
            _ => unimplemented!(),
        }
    }
}

#[macro_export]
macro_rules! assert_tokenizes_as {
        ($input:expr, $($tokens:expr),+) => {
            // TODO: also inject some newlines and retest
            let mut expected_tokens = vec![];
            for token in vec![$($crate::lexer::tests::TokenMock::from($tokens)),+] {
                expected_tokens.push(token);
            }

            let mut start = 0;
            let mut lexed_tokens = vec![];
            for token in $crate::lexer::tokenize($input) {
                lexed_tokens.push($crate::lexer::tests::TokenMock {
                    kind: token.kind,
                    len: token.len,
                    text: std::borrow::Cow::Borrowed(&$input[start..start + token.len])
                });
                start += token.len;
            }

            pretty_assertions::assert_eq!(lexed_tokens, expected_tokens);
        };
    }

#[cfg(test)]
impl TokenMock<'static> {
    pub fn comment(comment: &'static str) -> TokenMock<'static> {
        TokenMock {
            kind: TokenKind::Comment,
            text: Cow::Borrowed(comment),
            len: comment.len(),
        }
    }
    pub fn identifier(comment: &'static str) -> TokenMock<'static> {
        TokenMock {
            kind: TokenKind::Identifier,
            text: Cow::Borrowed(comment),
            len: comment.len(),
        }
    }
    pub fn unquoted(text: &'static str) -> TokenMock<'static> {
        TokenMock {
            kind: TokenKind::UnquotedValue,
            text: Cow::Borrowed(text),
            len: text.len(),
        }
    }
    pub fn directive_arg(arg: &'static str) -> TokenMock<'static> {
        TokenMock {
            kind: TokenKind::DirectiveArgument,
            text: Cow::Borrowed(arg),
            len: arg.len(),
        }
    }
    pub fn task(task: &'static str) -> TokenMock<'static> {
        TokenMock {
            kind: TokenKind::Task,
            text: Cow::Borrowed(task),
            len: task.len(),
        }
    }
    pub fn escaped_newline() -> TokenMock<'static> {
        TokenMock {
            kind: TokenKind::EscapedNewline,
            text: Cow::Borrowed("\\\n"),
            len: 2,
        }
    }
    pub fn varflag(varflag: &'static str) -> TokenMock<'static> {
        TokenMock {
            kind: TokenKind::Varflag,
            text: Cow::Owned(format!("[{varflag}]")),
            len: varflag.len() + 2,
        }
    }
    pub fn python_def_function_name(t: &'static str) -> TokenMock<'static> {
        TokenMock {
            kind: TokenKind::PythonDefFunctionName,
            text: Cow::Borrowed(t),
            len: t.len(),
        }
    }

    pub fn python_def_function_args(t: &'static str) -> TokenMock<'static> {
        TokenMock {
            kind: TokenKind::PythonDefFunctionArgs,
            text: Cow::Borrowed(t),
            len: t.len(),
        }
    }
    pub fn python_def_function_body(t: &'static str) -> TokenMock<'static> {
        TokenMock {
            kind: TokenKind::PythonDefFunctionBody,
            text: Cow::Borrowed(t),
            len: t.len(),
        }
    }

    pub fn double_quote_token(input: &'static str) -> TokenMock<'static> {
        TokenMock {
            kind: TokenKind::DoubleQuotedValue,
            len: input.len() + 2,
            text: Cow::Owned(format!("\"{input}\"")),
        }
    }

    pub fn single_quote_token(input: &'static str) -> TokenMock<'static> {
        TokenMock {
            kind: TokenKind::SingleQuotedValue,
            len: input.len() + 2,
            text: Cow::Owned(format!("'{input}'")),
        }
    }
}
