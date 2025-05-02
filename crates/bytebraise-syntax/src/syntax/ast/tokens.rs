use crate::syntax::ast::{AstToken, SyntaxKind, SyntaxToken};

use std::fmt::Debug;

use crate::ast_token;

ast_token!(Identifier, Identifier);
ast_token!(Task, Task);
ast_token!(Python, Python);
ast_token!(Fakeroot, Fakeroot);
ast_token!(DoubleQuotedValue, DoubleQuotedValue);
ast_token!(SingleQuotedValue, SingleQuotedValue);
ast_token!(Export, Export);
ast_token!(UnquotedValue, UnquotedValue);
ast_token!(DirectiveArgument, DirectiveArgument);

ast_token!(Equals, Equals);
ast_token!(EqualsPlus, EqualsPlus);
ast_token!(EqualsDot, EqualsDot);
ast_token!(DotEquals, DotEquals);
ast_token!(PlusEquals, PlusEquals);
ast_token!(DefaultEquals, DefaultEquals);
ast_token!(WeakEquals, WeakEquals);
ast_token!(ColonEquals, ColonEquals);

ast_token!(PythonDefKeyword, PythonDefKeyword);
ast_token!(PythonDefFunctionName, PythonDefFunctionName);
ast_token!(PythonDefFunctionArgs, PythonDefFunctionArgs);
ast_token!(Colon, Colon);
ast_token!(PythonDefFunctionBody, PythonDefFunctionBody);

ast_token!(Varflag, Varflag);

ast_token!(Comment, Comment);

impl Varflag {
    pub fn value(&self) -> &str {
        // Slice off the square brackets
        let len = self.text().len();
        &self.text()[1..len - 1]
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AssignmentOperator {
    Equals(Equals),
    EqualsPlus(EqualsPlus),
    EqualsDot(EqualsDot),
    DotEquals(DotEquals),
    PlusEquals(PlusEquals),
    DefaultEquals(DefaultEquals),
    WeakEquals(WeakEquals),
    ColonEquals(ColonEquals),
}

impl AstToken for AssignmentOperator {
    fn can_cast(token: SyntaxKind) -> bool
    where
        Self: Sized,
    {
        token.is_assignment_operator()
    }

    fn cast(syntax: SyntaxToken) -> Option<Self>
    where
        Self: Sized,
    {
        let ret = match syntax.kind() {
            SyntaxKind::ColonEquals => AssignmentOperator::ColonEquals(ColonEquals { syntax }),
            SyntaxKind::DefaultEquals => {
                AssignmentOperator::DefaultEquals(DefaultEquals { syntax })
            }
            SyntaxKind::DotEquals => AssignmentOperator::DotEquals(DotEquals { syntax }),
            SyntaxKind::Equals => AssignmentOperator::Equals(Equals { syntax }),
            SyntaxKind::EqualsDot => AssignmentOperator::EqualsDot(EqualsDot { syntax }),
            SyntaxKind::EqualsPlus => AssignmentOperator::EqualsPlus(EqualsPlus { syntax }),
            SyntaxKind::PlusEquals => AssignmentOperator::PlusEquals(PlusEquals { syntax }),
            SyntaxKind::WeakEquals => AssignmentOperator::WeakEquals(WeakEquals { syntax }),
            _ => return None,
        };

        Some(ret)
    }

    fn syntax(&self) -> &SyntaxToken {
        match self {
            AssignmentOperator::Equals(it) => &it.syntax,
            AssignmentOperator::EqualsPlus(it) => &it.syntax,
            AssignmentOperator::EqualsDot(it) => &it.syntax,
            AssignmentOperator::DotEquals(it) => &it.syntax,
            AssignmentOperator::PlusEquals(it) => &it.syntax,
            AssignmentOperator::DefaultEquals(it) => &it.syntax,
            AssignmentOperator::WeakEquals(it) => &it.syntax,
            AssignmentOperator::ColonEquals(it) => &it.syntax,
        }
    }
}
