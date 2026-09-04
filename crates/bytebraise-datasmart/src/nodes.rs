use crate::petgraph2::OverrideScore;
use crate::variable_operation::VariableOperation;
use crate::variable_parser::StatementNode2;
use bytebraise_util::fifo_heap::FifoHeap;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub(crate) struct Variable {
    pub(crate) name: String,
    // Canonical statement order. Evaluation derives its priority heap from this sequence.
    pub(crate) statements: Vec<StatementNode2>,
    pub(crate) cached_value: RefCell<Option<String>>,
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
    pub(crate) varflags: BTreeMap<String, FifoHeap<VariableOperation>>, // TODO: iterative cache for OVERRIDES
}

impl Display for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

impl Variable {
    pub(crate) fn new<T: Into<String>>(name: T) -> Variable {
        Variable {
            name: name.into(),
            statements: Vec::new(),
            cached_value: RefCell::new(None),
            varflags: BTreeMap::new(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ScoredOperation<'a> {
    pub(crate) score: OverrideScore,
    pub(crate) stmt: &'a StatementNode2,
    pub(crate) stmt_index: usize,
}

impl<'a> Ord for ScoredOperation<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.score.cmp(&other.score).reverse().then(
            self.stmt
                .resolved_operator()
                .cmp(&other.stmt.resolved_operator()),
        )
    }
}

impl<'a> PartialOrd for ScoredOperation<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
