use crate::petgraph2;
use crate::petgraph2::OverrideScore;
use crate::variable_operation::{Operator, OverrideOperator, VariableOperation};
use bytebraise_util::fifo_heap::FifoHeap;
use indexmap::IndexSet;
use petgraph::graph::NodeIndex;
use std::borrow::Cow;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::fmt::{Display, Formatter, Write};

#[derive(Debug)]
pub(crate) struct Variable {
    pub(crate) name: String,
    pub(crate) operations: FifoHeap<VariableOperation>,
    cached_value: RefCell<Option<String>>,
    // map of varflag name => heap of operations
    // for example, in:
    //   A[depends] = "q'
    // the varflag name is 'depends', and a single operation is added to assign "q"

    // TODO: unlike with the implicit _content vargflag (which is covered by the first-class citizen
    //  `operations`), varflag operations do not care about `OVERRIDES`. However, we still make use of
    //  the operation lhs, since you can do stuff like this:
    //      A[depends] = "q"
    //      A:pn-specific[depends] = "d"
    //  Calling get_var_flag on "A" in the context of 'specific' recipe does NOT however return "d".
    //  Calling get_var_flag "A:pn-specific" does give "d", however.
    //  This is kind of confusiong, so perhaps we shouldn't blinding re-use `VariableOperation` here,
    //  since the semantics are so different.
    varflags: BTreeMap<String, FifoHeap<VariableOperation>>, // TODO: iterative cache for OVERRIDES
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct StmtNode {
    // TODO: remove? can be computed from StatementKind
    pub(crate) override_str: Option<String>,

    pub(crate) operator: Operator,
    pub(crate) kind: StatementKind,

    /// The value
    pub(crate) rhs: String,
}

#[derive(Debug)]
pub(crate) enum GraphItem {
    Variable(Variable),
    StmtNode(StmtNode),
}

impl Display for GraphItem {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            GraphItem::Variable(var) => f.write_str(&var.name),
            GraphItem::StmtNode(stmt) => f.write_str(&format!("{:#?}", &stmt)),
        }
    }
}

impl GraphItem {
    pub(crate) fn variable_mut(&mut self) -> &mut Variable {
        match self {
            GraphItem::Variable(v) => v,
            _ => panic!("Expected GraphItem::Variable"),
        }
    }

    pub(crate) fn variable(&self) -> &Variable {
        match self {
            GraphItem::Variable(v) => v,
            _ => panic!("Expected GraphItem::Variable"),
        }
    }

    pub(crate) fn to_variable(self) -> Variable {
        match self {
            GraphItem::Variable(v) => v,
            _ => panic!("Expected GraphItem::Variable"),
        }
    }

    pub(crate) fn statement(&self) -> &StmtNode {
        match self {
            GraphItem::StmtNode(stmt) => stmt,
            _ => panic!("Expected GraphItem::Statement"),
        }
    }

    pub(crate) fn statement_mut(&mut self) -> &mut StmtNode {
        match self {
            GraphItem::StmtNode(stmt) => stmt,
            _ => panic!("Expected GraphItem::Statement"),
        }
    }
}

impl GraphItem {
    pub(crate) fn new_variable<T: Into<String>>(name: T) -> GraphItem {
        GraphItem::Variable(Variable {
            name: name.into(),
            operations: FifoHeap::new(),
            cached_value: RefCell::new(None),
            varflags: BTreeMap::new(),
        })
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StatementKind {
    Operation {
        scope: Vec<String>,
        filter: IndexSet<String>,
    },
    PureOverride {
        scope: Vec<String>,
    },
    Unconditional,
}

impl StatementKind {
    pub(crate) fn is_active(
        &self,
        override_selection_context: &Cow<IndexSet<String>>,
        active_overrides: &Cow<IndexSet<String>>,
    ) -> bool {
        match self {
            Self::Operation { filter, scope, .. } => {
                let scope_set: IndexSet<String> = scope.iter().cloned().collect();

                // For scope, consider selection context (active set + direct variant lookup)
                let lhs_valid = scope_set.is_subset(override_selection_context);

                // For filter, consider active override set
                let rhs_valid = filter.is_subset(active_overrides);

                rhs_valid && lhs_valid
            }
            Self::PureOverride { scope } => {
                let scope_set: IndexSet<String> = scope.iter().cloned().collect();
                scope_set.is_subset(override_selection_context)
            }
            // TODO?
            Self::Unconditional => true,
        }
    }

    pub(crate) fn score(&self, active_overrides: &Cow<IndexSet<String>>) -> Option<OverrideScore> {
        // The score is derived from the scope alone
        match self {
            Self::Operation { scope, .. } => petgraph2::score_override(active_overrides, scope),
            Self::PureOverride { scope } => petgraph2::score_override(active_overrides, scope),
            Self::Unconditional => Some((vec![], 0, 0)),
        }
    }

    pub(crate) fn override_scope(&self) -> Vec<String> {
        match self {
            StatementKind::Unconditional => Vec::new(),
            StatementKind::Operation { scope, .. } => scope.clone(),
            StatementKind::PureOverride { scope } => scope.clone(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ScoredOperation<'a> {
    pub(crate) score: OverrideScore,
    pub(crate) stmt: &'a StmtNode,
    pub(crate) stmt_index: NodeIndex,
}

impl<'a> Ord for ScoredOperation<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.score
            .cmp(&other.score)
            .reverse()
            .then(self.stmt.operator.cmp(&other.stmt.operator))
    }
}

impl<'a> PartialOrd for ScoredOperation<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
