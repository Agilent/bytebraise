use crate::petgraph2::DataSmart;
use default_args::default_args;

extern crate self as _current_crate;

default_args! {
    export pub fn crate::macros::get_var<S: AsRef<str>>(d: &DataSmart, var: S, parsing: bool = false) -> Option<String> {
        d.get_var(var, parsing)
    }
}

pub(crate) use get_var;
