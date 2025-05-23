// This module adapted from Rust Analyzer (https://github.com/rust-analyzer/rust-analyzer), which
// is under MIT license.

use crate::syntax::syntax_kind::SyntaxKind;
use std::marker::PhantomData;

use crate::syntax::syntax_node::{SyntaxNode, SyntaxNodeChildren, SyntaxToken};

pub mod nodes;
pub mod quoted_value;
pub mod tokens;

/// The main trait to go from untyped `SyntaxNode`  to a typed ast. The
/// conversion itself has zero runtime cost: ast and syntax nodes have exactly
/// the same representation: a pointer to the tree root and a pointer to the
/// node itself.
pub trait AstNode {
    fn can_cast(kind: SyntaxKind) -> bool
    where
        Self: Sized;

    fn cast(syntax: SyntaxNode) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxNode;

    fn clone_for_update(&self) -> Self
    where
        Self: Sized,
    {
        Self::cast(self.syntax().clone_for_update()).unwrap()
    }

    fn clone_subtree(&self) -> Self
    where
        Self: Sized,
    {
        Self::cast(self.syntax().clone_subtree()).unwrap()
    }
}

/// Like `AstNode`, but wraps tokens rather than interior nodes.
pub trait AstToken {
    fn can_cast(token: SyntaxKind) -> bool
    where
        Self: Sized;

    fn cast(syntax: SyntaxToken) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxToken;

    fn text(&self) -> &str {
        self.syntax().text()
    }
}

#[macro_export]
macro_rules! ast_node {
    ($ast:ident, $kind:ident) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        #[repr(transparent)]
        pub struct $ast {
            pub(crate) syntax: SyntaxNode,
        }
        impl $crate::syntax::ast::AstNode for $ast {
            #[allow(unused)]
            fn cast(node: SyntaxNode) -> Option<Self> {
                if node.kind() == SyntaxKind::$kind {
                    Some(Self { syntax: node })
                } else {
                    None
                }
            }

            #[allow(unused)]
            fn can_cast(kind: SyntaxKind) -> bool {
                kind == SyntaxKind::$kind
            }

            #[allow(unused)]
            fn syntax(&self) -> &SyntaxNode {
                &self.syntax
            }
        }
    };
}

#[macro_export]
macro_rules! ast_token {
    ($ast:ident, $kind:ident) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        #[repr(transparent)]
        pub struct $ast {
            pub(crate) syntax: SyntaxToken,
        }
        impl $crate::syntax::ast::AstToken for $ast {
            #[allow(unused)]
            fn cast(token: SyntaxToken) -> Option<Self> {
                if token.kind() == SyntaxKind::$kind {
                    Some(Self { syntax: token })
                } else {
                    None
                }
            }

            #[allow(unused)]
            fn can_cast(kind: SyntaxKind) -> bool {
                kind == SyntaxKind::$kind
            }

            #[allow(unused)]
            fn syntax(&self) -> &SyntaxToken {
                &self.syntax
            }
        }
    };
}

/// An iterator over `SyntaxNode` children of a particular AST type.
#[derive(Debug, Clone)]
pub struct AstChildren<N> {
    inner: SyntaxNodeChildren,
    ph: PhantomData<N>,
}

impl<N> AstChildren<N> {
    fn new(parent: &SyntaxNode) -> Self {
        AstChildren {
            inner: parent.children(),
            ph: PhantomData,
        }
    }
}

impl<N: AstNode> Iterator for AstChildren<N> {
    type Item = N;
    fn next(&mut self) -> Option<N> {
        self.inner.find_map(N::cast)
    }
}

pub mod support {
    use super::{AstChildren, AstNode, SyntaxNode};
    use crate::syntax::ast::AstToken;

    pub(crate) fn child<N: AstNode>(parent: &SyntaxNode) -> Option<N> {
        parent.children().find_map(N::cast)
    }

    pub(crate) fn children<N: AstNode>(parent: &SyntaxNode) -> AstChildren<N> {
        AstChildren::new(parent)
    }

    pub(crate) fn token<N: AstToken>(parent: &SyntaxNode) -> Option<N> {
        parent
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find_map(N::cast)
    }
}

// fn ast_from_text<N: AstNode>(text: &str) -> N {
//     let parse = SourceFile::parse(text);
//     let node = match parse.tree().syntax().descendants().find_map(N::cast) {
//         Some(it) => it,
//         None => {
//             panic!("Failed to make ast node `{}` from text {}", std::any::type_name::<N>(), text)
//         }
//     };
//     let node = node.clone_subtree();
//     assert_eq!(node.syntax().text_range().start(), 0.into());
//     node
// }
