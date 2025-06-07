use crate::petgraph2::DataSmart;
use default_args::default_args;
use std::fmt::Debug;

extern crate self as _current_crate;

default_args! {
    export pub fn crate::macros::get_var<S: AsRef<str> + Debug>(d: &DataSmart, var: S, parsing: bool = false, expand: bool = true) -> Option<String> {
        d.get_var(var, parsing, expand)
    }
}

pub(crate) use get_var;
