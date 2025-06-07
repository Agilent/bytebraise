use crate::petgraph2::StatementOverrides;
use crate::variable_operation::{StmtKind, VariableOperation};
use bytebraise_util::fifo_heap::FifoHeap;
use std::cell::RefCell;
use std::collections::BTreeMap;

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
    pub(crate) kind: StmtKind,

    pub(crate) override_str: Option<String>,

    pub(crate) overrides_data: Option<StatementOverrides>,

    /// The value
    pub(crate) rhs: String,
}

#[derive(Debug)]
pub(crate) enum GraphItem {
    Variable(Variable),
    StmtNode(StmtNode),
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
