use crate::ast_node;
use crate::syntax::ast;
use crate::syntax::ast::quoted_value::QuotedValue;
use crate::syntax::ast::tokens::{
    AssignmentOperator, DirectiveArgument, Identifier, PythonDefFunctionName, Varflag,
};
use crate::syntax::ast::{support, tokens, AstChildren, AstNode, AstToken, SyntaxKind, SyntaxNode};

ast_node!(Assignment, AssignmentNode);
impl Assignment {
    pub fn left(&self) -> IdentifierExpression {
        support::child(&self.syntax).unwrap()
    }

    pub fn right(&self) -> QuotedValue {
        support::token(&self.syntax).unwrap()
    }

    pub fn op(&self) -> AssignmentOperator {
        support::token(&self.syntax).unwrap()
    }
}

ast_node!(Export, ExportNode);
impl Export {
    pub fn export_kw(&self) -> tokens::Export {
        support::token(&self.syntax).unwrap()
    }

    pub fn var(&self) -> IdentifierExpression {
        support::child(&self.syntax).unwrap()
    }

    pub fn assignment(&self) -> Option<Assignment> {
        support::child(&self.syntax)
    }
}

ast_node!(Include, IncludeNode);
impl Include {
    pub fn value(&self) -> tokens::UnquotedValue {
        support::token(&self.syntax).unwrap()
    }
}

ast_node!(Require, RequireNode);
impl Require {
    pub fn value(&self) -> tokens::UnquotedValue {
        support::token(&self.syntax).unwrap()
    }
}

ast_node!(Unset, UnsetNode);
ast_node!(ExportFunctions, ExportFunctionsNode);
ast_node!(Inherit, InheritNode);
impl Inherit {
    pub fn value(&self) -> tokens::UnquotedValue {
        support::token(&self.syntax).unwrap()
    }
}

ast_node!(PythonDef, PythonDefNode);
impl PythonDef {
    /// Warning: may contain trailing spaces!
    pub fn function_name(&self) -> PythonDefFunctionName {
        support::token(&self.syntax).unwrap()
    }
}

ast_node!(AddTask, AddTaskNode);
impl AddTask {
    pub fn task_name(&self) -> DirectiveArgument {
        self.syntax
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            // only consider tokens up until we encounter 'after' or 'before'
            .take_while(|it| !matches!(it.kind(), SyntaxKind::After | SyntaxKind::Before))
            .find_map(DirectiveArgument::cast)
            .unwrap()
    }

    pub fn after(&self) -> Vec<DirectiveArgument> {
        self.syntax
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .skip_while(|it| !matches!(it.kind(), SyntaxKind::After))
            .take_while(|it| !matches!(it.kind(), SyntaxKind::Before))
            .filter_map(DirectiveArgument::cast)
            .collect()
    }

    pub fn after_names(&self) -> Vec<String> {
        self.after()
            .into_iter()
            .map(|t| t.syntax.text().to_string())
            .collect()
    }

    pub fn before(&self) -> Vec<DirectiveArgument> {
        self.syntax
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .skip_while(|it| !matches!(it.kind(), SyntaxKind::Before))
            .take_while(|it| !matches!(it.kind(), SyntaxKind::After))
            .filter_map(DirectiveArgument::cast)
            .collect()
    }

    pub fn before_names(&self) -> Vec<String> {
        self.before()
            .into_iter()
            .map(|t| t.syntax.text().to_string())
            .collect()
    }
}

ast_node!(DelTask, DelTaskNode);
ast_node!(AddHandler, AddHandlerNode);
ast_node!(Comment, Comment);

ast_node!(IdentifierExpression, IdentifierExpressionNode);
impl IdentifierExpression {
    pub fn identifier(&self) -> Identifier {
        support::token(&self.syntax).unwrap()
    }

    pub fn varflag(&self) -> Option<Varflag> {
        support::token(&self.syntax)
    }
}

ast_node!(Root, RootNode);
impl Root {
    pub fn items(&self) -> AstChildren<RootItem> {
        support::children(self.syntax())
    }

    pub fn tasks(&self) -> AstChildren<Task> {
        support::children(self.syntax())
    }

    pub fn assignments(&self) -> AstChildren<Assignment> {
        support::children(self.syntax())
    }

    // TODO: handle returning override variants?
    pub fn identifier_assignments<'a, 'b: 'a>(
        &'a self,
        identifier: &'b str,
    ) -> IdentifierAssignments<'a> {
        IdentifierAssignments {
            inner: self.assignments(),
            identifier,
        }
    }
}

#[derive(Debug, Clone)]
pub struct IdentifierAssignments<'a> {
    inner: AstChildren<Assignment>,
    identifier: &'a str,
}

impl<'a> Iterator for IdentifierAssignments<'a> {
    type Item = Assignment;

    fn next(&mut self) -> Option<Self::Item> {
        let identifier = self.identifier;
        self.inner.find(|i| i.left().syntax.text() == identifier)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RootItem {
    Task(Task),
    Comment(Comment),
    Directive(Directive),
    PythonDef(PythonDef),
    Assignment(Assignment),
}

impl AstNode for RootItem {
    fn can_cast(kind: SyntaxKind) -> bool
    where
        Self: Sized,
    {
        match kind {
            SyntaxKind::TaskNode
            | SyntaxKind::Comment
            | SyntaxKind::PythonDefNode
            | SyntaxKind::AssignmentNode => true,
            k if Directive::can_cast(k) => true,
            _ => false,
        }
    }

    fn cast(syntax: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        let res = match syntax.kind() {
            SyntaxKind::TaskNode => RootItem::Task(Task { syntax }),
            SyntaxKind::Comment => RootItem::Comment(Comment { syntax }),
            SyntaxKind::PythonDefNode => RootItem::PythonDef(PythonDef { syntax }),
            SyntaxKind::AssignmentNode => RootItem::Assignment(Assignment { syntax }),
            k if Directive::can_cast(k) => RootItem::Directive(Directive::cast(syntax).unwrap()),
            _ => return None,
        };

        Some(res)
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            RootItem::Task(it) => &it.syntax,
            RootItem::Comment(it) => &it.syntax,
            RootItem::Directive(it) => it.syntax(),
            RootItem::PythonDef(it) => &it.syntax,
            RootItem::Assignment(it) => &it.syntax,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Directive {
    AddHandler(AddHandler),
    AddTask(AddTask),
    DelTask(DelTask),
    Unset(Unset),
    Inherit(Inherit),
    Include(Include),
    Require(Require),
    Export(Export),
    ExportFunctions(ExportFunctions),
}

impl AstNode for Directive {
    fn can_cast(kind: SyntaxKind) -> bool
    where
        Self: Sized,
    {
        use SyntaxKind::*;
        match kind {
            AddHandlerNode | AddTaskNode | DelTaskNode | UnsetNode | InheritNode | IncludeNode
            | RequireNode | ExportNode | ExportFunctionsNode => true,
            _ => false,
        }
    }

    fn cast(syntax: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        let res = match syntax.kind() {
            SyntaxKind::AddHandlerNode => Directive::AddHandler(AddHandler { syntax }),
            SyntaxKind::AddTaskNode => Directive::AddTask(AddTask { syntax }),
            SyntaxKind::DelTaskNode => Directive::DelTask(DelTask { syntax }),
            SyntaxKind::UnsetNode => Directive::Unset(Unset { syntax }),
            SyntaxKind::InheritNode => Directive::Inherit(Inherit { syntax }),
            SyntaxKind::IncludeNode => Directive::Include(Include { syntax }),
            SyntaxKind::RequireNode => Directive::Require(Require { syntax }),
            SyntaxKind::ExportNode => Directive::Export(Export { syntax }),
            SyntaxKind::ExportFunctionsNode => {
                Directive::ExportFunctions(ExportFunctions { syntax })
            }
            _ => return None,
        };

        Some(res)
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Directive::AddHandler(it) => &it.syntax,
            Directive::AddTask(it) => &it.syntax,
            Directive::DelTask(it) => &it.syntax,
            Directive::Unset(it) => &it.syntax,
            Directive::Inherit(it) => &it.syntax,
            Directive::Include(it) => &it.syntax,
            Directive::Require(it) => &it.syntax,
            Directive::Export(it) => &it.syntax,
            Directive::ExportFunctions(it) => &it.syntax,
        }
    }
}

ast_node!(Task, TaskNode);
impl Task {
    pub fn name(&self) -> Option<Identifier> {
        let id_node: Option<IdentifierExpression> = support::child(self.syntax());
        id_node.map(|id| id.identifier())
    }

    pub fn body(&self) -> tokens::Task {
        support::token(&self.syntax).unwrap()
    }

    pub fn name_or_anonymous(&self) -> String {
        self.name()
            .as_ref()
            .map(|n| n.text().to_string())
            .unwrap_or(String::from("__anonymous"))
    }

    pub fn is_python(&self) -> bool {
        self.syntax
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find_map(ast::tokens::Python::cast)
            .is_some()
    }

    pub fn is_fakeroot(&self) -> bool {
        self.syntax
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find_map(ast::tokens::Fakeroot::cast)
            .is_some()
    }

    pub fn is_anonymous_python(&self) -> bool {
        self.is_python()
            && matches!(
                self.name().as_ref().map(|n| n.text()),
                Some("__anonymous") | None
            )
    }
}
