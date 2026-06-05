use crate::petgraph2::DataSmart;
use crate::variable_operation::NormalOperator;
use default_args::default_args;
use petgraph::graph::NodeIndex;
use petgraph::stable_graph::DefaultIx;
use std::fmt::Debug;

extern crate self as _current_crate;

default_args! {
    export pub fn crate::macros::get_var<S: AsRef<str> + Debug>(d: &DataSmart, var: S, expand: bool = true, no_weak_default: bool = false, parsing: bool = false) -> Option<String> {
        d.get_var(var, parsing, expand, no_weak_default)
    }
}

default_args! {
    export pub fn crate::macros::set_var_ex<S, V>(d: &mut DataSmart, var: S, value: V, parsing: bool = false, operator: NormalOperator = NormalOperator::Assign) -> Option<NodeIndex<DefaultIx>>
    where
        S: Into<String> + Debug,
        V: Into<String> + Debug
    {
        d.set_var_ex(var, value, parsing, operator)
    }
}

default_args! {
    export pub fn crate::macros::set_var<S, V>(d: &mut DataSmart, var: S, value: V, parsing: bool = false) -> Option<NodeIndex<DefaultIx>>
    where
        S: Into<String> + Debug,
        V: Into<String> + Debug
    {
        d.set_var(var, value, parsing)
    }
}

pub(crate) use get_var;
pub(crate) use set_var;
pub(crate) use set_var_ex;
