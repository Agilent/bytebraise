/*!
Helper functions and data structures for manipulating overrides and conditional variables.
*/

use core::iter;

use im_rc::HashMap;
use lazy_static::lazy_static;
use regex::Regex;

use crate::data_smart::utils::RSplitAll;

lazy_static! {
    static ref OVERRIDE_REGEX: Regex = Regex::new(r"[a-z0-9]+").unwrap();
}

/// Maintains back-references to the full conditional forms for each variable.
#[derive(Clone, Debug)]
pub struct PerVarOverrideData {
    data: im_rc::HashMap<String, Vec<VarAndOverrideTuple>>,
}

impl PerVarOverrideData {
    pub fn new() -> Self {
        PerVarOverrideData {
            data: HashMap::new(),
        }
    }

    pub fn record_overrides<T: AsRef<str>>(&mut self, var: T) {
        for (shortvar, entry) in decompose_variable(var.as_ref()) {
            self.data.entry(shortvar).or_default().push(entry);
        }
    }

    // TODO rename and document
    pub fn remove_overrides<T: AsRef<str>>(&mut self, var: T) {
        let var = var.as_ref();
        self.data.remove(var);

        for (shortvar, entry) in decompose_variable(var) {
            if let Some(data) = self.data.get_mut(&shortvar) {
                if let Some(needle) = data.iter().position(|x| *x == entry) {
                    data.remove(needle);
                }
            }
        }
    }

    // TODO rename and document
    pub fn remove<T: AsRef<str>>(&mut self, var: T) -> Option<Vec<VarAndOverrideTuple>> {
        self.data.remove(var.as_ref())
    }

    pub fn get<T: AsRef<str>>(&self, var: T) -> Option<&Vec<VarAndOverrideTuple>> {
        self.data.get(var.as_ref())
    }

    pub fn collect(&self) -> Vec<(String, VarAndOverrideTuple)> {
        self.data
            .iter()
            .flat_map(|pair| {
                let var = iter::repeat(pair.0.clone());
                var.zip(pair.1.iter().cloned()).collect::<Vec<_>>()
            })
            .collect::<Vec<_>>()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VarAndOverrideTuple {
    pub full_var: String,
    pub override_str: String,
}

/// Given a variable with overrides like:
/// VAR_foo_bar_baz
/// produces:
/// [("VAR_foo_bar", ("VAR_foo_bar_baz", "baz")),
///  ("VAR_foo", ("VAR_foo_bar_baz", "bar_baz")),
///  ("VAR", ("VAR_foo_bar_baz", "foo_bar_baz"))]
fn decompose_variable(var: &str) -> Vec<(String, VarAndOverrideTuple)> {
    let mut ret = vec![];

    for (shortvar, override_var) in var.rsplit_all('_') {
        if shortvar.is_empty() || !OVERRIDE_REGEX.is_match(override_var) {
            break;
        }

        ret.push((
            shortvar.to_string(),
            VarAndOverrideTuple {
                full_var: var.to_owned(),
                override_str: override_var.to_owned(),
            },
        ));
    }

    ret
}
