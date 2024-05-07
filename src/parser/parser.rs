use std::iter::Peekable;

use crate::lexer::tokenize;
use rowan::{Checkpoint, GreenNodeBuilder, NodeOrToken};

use crate::syntax::ast::nodes::Root;
use crate::syntax::ast::{AstNode, AstToken};
use crate::syntax::syntax_kind::{syntax_kind_for_token_kind, SyntaxKind};
use crate::syntax::syntax_node::{SyntaxElement, SyntaxNode};

struct Parser<'text, I: Iterator<Item = (SyntaxKind, &'text str)>> {
    builder: GreenNodeBuilder<'static>,
    iter: Peekable<I>,
}

struct TaskContext {
    is_python: bool,
    is_fakeroot: bool,
}

impl<'text, I: Iterator<Item = (SyntaxKind, &'text str)>> Parser<'text, I> {
    fn eat_ws(&mut self) {
        while self
            .iter
            .peek()
            .map(|&(t, _)| t == SyntaxKind::Whitespace)
            .unwrap_or(false)
        {
            self.bump();
        }
    }

    fn peek(&mut self) -> Option<SyntaxKind> {
        self.eat_ws();
        self.iter.peek().map(|&(t, _)| t)
    }

    fn peek_no_ws(&mut self) -> Option<SyntaxKind> {
        self.iter.peek().map(|&(t, _)| t)
    }

    fn bump(&mut self) {
        if let Some((token, string)) = self.iter.next() {
            self.builder.token(token.into(), string);
        }
    }

    fn parse_include_or_require(&mut self) -> bool {
        let checkpoint = self.builder.checkpoint();

        match self.peek() {
            Some(SyntaxKind::Require) => self
                .builder
                .start_node_at(checkpoint, SyntaxKind::RequireNode.into()),
            Some(SyntaxKind::Include) => self
                .builder
                .start_node_at(checkpoint, SyntaxKind::IncludeNode.into()),
            _ => unreachable!(),
        }

        self.bump();

        self.expect(SyntaxKind::UnquotedValue);

        self.builder.finish_node();

        true
    }

    fn parse_export(&mut self) -> bool {
        self.builder.start_node(SyntaxKind::ExportNode.into());
        self.expect(SyntaxKind::Export);
        self.eat_ws();

        let checkpoint = self.builder.checkpoint();

        self.parse_identifier_expression();
        match self.peek() {
            Some(t) if t.is_assignment_operator() => {
                self.builder
                    .start_node_at(checkpoint, SyntaxKind::AssignmentNode.into());
                self.bump();
                match self.peek() {
                    Some(SyntaxKind::DoubleQuotedValue | SyntaxKind::SingleQuotedValue) => {
                        self.bump()
                    }
                    _ => panic!(),
                }
                self.builder.finish_node();
            }
            Some(SyntaxKind::Newline) | None => {} // fine, this is just an export
            _ => panic!("{:?}", self.peek()),
        }

        self.builder.finish_node();
        true
    }

    fn parse_assignment_or_task(
        &mut self,
        task_checkpoint: Option<Checkpoint>,
        maybe_anonymous_python: bool,
    ) -> bool {
        let checkpoint = self.builder.checkpoint();
        if !self.parse_identifier_expression() && !maybe_anonymous_python {
            return false; // TODO
        }

        if self.allow(SyntaxKind::OpenParenthesis) {
            // TODO check no varflag with task

            self.expect(SyntaxKind::CloseParenthesis);
            self.expect(SyntaxKind::Task);
            let checkpoint = task_checkpoint.unwrap_or(checkpoint);
            self.builder
                .start_node_at(checkpoint, SyntaxKind::TaskNode.into());
            self.builder.finish_node();
            return true;
        } else {
            assert!(task_checkpoint.is_none(), "required a task in this context")
        };

        // Has to be an assignment
        self.builder
            .start_node_at(checkpoint, SyntaxKind::AssignmentNode.into());
        match self.peek() {
            Some(t) if t.is_assignment_operator() => self.bump(),
            _ => panic!(),
        }
        match self.peek() {
            Some(SyntaxKind::DoubleQuotedValue | SyntaxKind::SingleQuotedValue) => self.bump(),
            _ => panic!(),
        }
        self.builder.finish_node();

        true
    }

    fn expect(&mut self, token: SyntaxKind) {
        // TODO don't panic, instead add an error token and continue?
        assert!(
            self.allow(token),
            "expected {:?}, got {:?}",
            token,
            self.peek()
        );
    }

    fn allow_no_ws(&mut self, token: SyntaxKind) -> bool {
        if Some(token) == self.peek_no_ws() {
            self.bump();
            return true;
        }

        false
    }

    fn allow(&mut self, token: SyntaxKind) -> bool {
        if Some(token) == self.peek() {
            self.bump();
            return true;
        }

        false
    }

    fn parse_identifier_expression(&mut self) -> bool {
        self.eat_ws();

        let checkpoint = self.builder.checkpoint();
        if self.allow(SyntaxKind::Identifier) {
            self.builder
                .start_node_at(checkpoint, SyntaxKind::IdentifierExpressionNode.into());
            self.allow_no_ws(SyntaxKind::Varflag);
            self.builder.finish_node();
            self.eat_ws();
            return true;
        }

        false
    }

    fn python_keyword(&mut self) -> bool {
        let checkpoint = self.builder.checkpoint();
        self.expect(SyntaxKind::Python);
        // TODO: can anonymous python funcs be fakeroot?
        self.allow(SyntaxKind::Fakeroot);
        self.parse_assignment_or_task(Some(checkpoint), true);
        true
    }

    fn fakeroot_keyword(&mut self) -> bool {
        let checkpoint = self.builder.checkpoint();
        self.expect(SyntaxKind::Fakeroot);
        // TODO: can anonymous python funcs be fakeroot?
        let maybe_anonymous_python = self.allow(SyntaxKind::Python);
        self.parse_assignment_or_task(Some(checkpoint), maybe_anonymous_python);
        true
    }

    fn parse_unset_node(&mut self) -> bool {
        self.builder.start_node(SyntaxKind::UnsetNode.into());
        self.expect(SyntaxKind::Unset);
        self.parse_identifier_expression();
        self.builder.finish_node();
        true
    }

    fn parse_export_functions_node(&mut self) -> bool {
        self.builder
            .start_node(SyntaxKind::ExportFunctionsNode.into());
        self.expect(SyntaxKind::ExportFunctions);
        while self.peek() == Some(SyntaxKind::DirectiveArgument) {
            self.bump();
        }
        self.builder.finish_node();
        true
    }

    fn parse_inherit_node(&mut self) -> bool {
        self.builder.start_node(SyntaxKind::InheritNode.into());
        self.expect(SyntaxKind::Inherit);
        self.expect(SyntaxKind::UnquotedValue);
        self.builder.finish_node();
        true
    }

    fn parse_python_def_function_node(&mut self) -> bool {
        self.builder.start_node(SyntaxKind::PythonDefNode.into());
        self.expect(SyntaxKind::PythonDefKeyword);
        self.expect(SyntaxKind::PythonDefFunctionName);
        self.expect(SyntaxKind::PythonDefFunctionArgs);
        self.expect(SyntaxKind::Colon);
        self.expect(SyntaxKind::PythonDefFunctionBody);
        self.builder.finish_node();
        true
    }

    fn parse_add_task_node(&mut self) -> bool {
        self.builder.start_node(SyntaxKind::AddTaskNode.into());
        self.expect(SyntaxKind::AddTask);
        loop {
            match self.peek() {
                None => break,
                Some(SyntaxKind::DirectiveArgument | SyntaxKind::After | SyntaxKind::Before) => {
                    self.bump()
                }
                Some(SyntaxKind::EscapedNewline) => self.bump(),
                Some(SyntaxKind::Newline) => break,
                Some(other) => panic!("unexpected token: {:?}", other),
            }
        }
        self.builder.finish_node();
        true
    }

    fn parse_del_task_node(&mut self) -> bool {
        self.builder.start_node(SyntaxKind::DelTaskNode.into());
        self.expect(SyntaxKind::DelTask);
        while self.peek() == Some(SyntaxKind::DirectiveArgument) {
            self.bump();
        }
        self.builder.finish_node();
        true
    }

    fn parse_add_handler_node(&mut self) -> bool {
        self.builder.start_node(SyntaxKind::AddHandlerNode.into());
        self.expect(SyntaxKind::AddHandler);
        while self.peek() == Some(SyntaxKind::DirectiveArgument) {
            self.bump();
        }
        self.builder.finish_node();
        true
    }

    fn parse_next(&mut self) -> bool {
        match self.peek() {
            Some(token) => match token {
                SyntaxKind::Newline | SyntaxKind::Comment => {
                    self.bump();
                    true
                }
                SyntaxKind::Export => self.parse_export(),
                SyntaxKind::Include | SyntaxKind::Require => self.parse_include_or_require(),
                SyntaxKind::Identifier => self.parse_assignment_or_task(None, false),
                SyntaxKind::Python => self.python_keyword(),
                SyntaxKind::Fakeroot => self.fakeroot_keyword(),
                SyntaxKind::Unset => self.parse_unset_node(),
                SyntaxKind::ExportFunctions => self.parse_export_functions_node(),
                SyntaxKind::Inherit => self.parse_inherit_node(),
                SyntaxKind::PythonDefKeyword => self.parse_python_def_function_node(),
                SyntaxKind::AddTask => self.parse_add_task_node(),
                SyntaxKind::DelTask => self.parse_del_task_node(),
                SyntaxKind::AddHandler => self.parse_add_handler_node(),

                _ => panic!("{:?}", token),
            },

            None => return false,
        };

        true
    }

    fn parse(mut self) -> SyntaxNode {
        self.builder.start_node(SyntaxKind::RootNode.into());
        while self.parse_next() {}
        self.builder.finish_node();

        SyntaxNode::new_root(self.builder.finish())
    }
}

fn print(indent: usize, element: SyntaxElement) {
    let kind: SyntaxKind = element.kind();
    print!("{:indent$}", "", indent = indent);
    match element {
        NodeOrToken::Node(node) => {
            println!("- {:?}", kind);
            for child in node.children_with_tokens() {
                print(indent + 2, child);
            }
        }

        NodeOrToken::Token(token) => println!("- {:?} {:?}", token.text(), kind),
    }
}

// TODO error messages
pub fn parse_bitbake_from_str(input: &str) -> Root {
    let mut start = 0;
    let tokens = tokenize(input).into_iter().map(|token| {
        let text = &input[start..start + token.len];
        start += token.len;
        (syntax_kind_for_token_kind(token.kind), text)
    });

    let ast = Parser {
        builder: GreenNodeBuilder::new(),
        iter: tokens.peekable(),
    }
    .parse();

    Root::cast(ast).unwrap()
}

#[cfg(test)]
mod test {

    use crate::parser::parser::parse_bitbake_from_str;

    use crate::syntax::ast::AstNode;

    #[test]
    fn wat() {
        let input = r##"
RECIPE_MAINTAINER_pn-apt = "Aníbal Limón <limon.anibal@gmail.com>"
RECIPE_MAINTAINER_pn-apt-native = "Aníbal Limón <limon.anibal@gmail.com>"

export WAT_DO_YOU_SAY = "contents ${"buddy"}"
include OK buddy then
A = "OK then"
python fakeroot do_my_task() {
    friend :)
}
fakeroot do_another_thing_pal(){
    echo "A"
}
do_p() {
    echo true
}
unset TEST
unset TEST[wat]

EXPORT_FUNCTIONS this is buddy

def this_is_(test, d):
    print("OK")

THIS = "a"

addtask do_compile after do_friend before \
    do_buddy

python() {
    d.setVar("OK")
}

deltask do_configure do_fetch

        "##;

        let r = parse_bitbake_from_str(input);
        // for t in r.tasks() {
        //     let p = t.name();
        //     println!("{:?}", p.map(|c| c.syntax.text().to_string()));
        //     println!("is_python: {}", t.is_python());
        //     println!("is_fakeroot: {}", t.is_fakeroot());
        //     println!("is_anonymous_python: {}", t.is_anonymous_python());
        // }

        println!();
        for i in r.items() {
            println!("{:?}", i.syntax().text());
        }
    }
}
