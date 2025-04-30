use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use anyhow::Context;

use crate::data_smart::errors::DataSmartResult;
use crate::data_smart::variable_contents::VariableContents;
use crate::data_smart::variable_parse::VariableParse;
use crate::data_smart::{expand_keys, DataSmartInner, GetVarFlagReturn, GetVarOptions};

#[cfg(feature = "python")]
use pyo3::pyclass;

#[cfg_attr(feature = "python", pyclass(unsendable))]
#[derive(Clone, Debug)]
pub struct DataSmart {
    pub(crate) data: Rc<RefCell<DataSmartInner>>,
}

impl DataSmart {
    pub fn new() -> DataSmart {
        DataSmart {
            data: DataSmartInner::new(),
        }
    }

    pub fn set_var_opt<T: Into<VariableContents>, K: Into<String>>(
        &self,
        var: K,
        value: T,
        parsing: bool,
    ) -> DataSmartResult<()> {
        RefCell::borrow(&self.data).set_var(var, value, parsing)
    }

    pub fn set_var<T: Into<VariableContents>, K: Into<String>>(
        &self,
        var: K,
        value: T,
    ) -> DataSmartResult<()> {
        self.set_var_opt(var, value, false)
    }

    pub fn prepend_var<T: AsRef<str>, V: Into<VariableContents>>(
        &self,
        var: T,
        val: V,
    ) -> DataSmartResult<()> {
        RefCell::borrow(&self.data).prepend_var(var, val)
    }

    pub fn append_var<T: AsRef<str>, V: Into<VariableContents>>(
        &self,
        var: T,
        val: V,
    ) -> DataSmartResult<()> {
        RefCell::borrow(&self.data).append_var(var, val)
    }

    pub fn set_var_flag<T: Into<VariableContents>, K: Into<String>, F: Into<String>>(
        &self,
        var: K,
        flag: F,
        value: T,
    ) -> DataSmartResult<()> {
        RefCell::borrow(&self.data).set_var_flag(var, flag, value)
    }

    pub fn get_var_opt(
        &self,
        key: &str,
        options: GetVarOptions,
    ) -> DataSmartResult<Option<VariableContents>> {
        RefCell::borrow(&self.data).get_var_opt(key, options)
    }

    pub fn get_var(&self, var: &str) -> DataSmartResult<Option<VariableContents>> {
        self.get_var_opt(var, GetVarOptions::default())
    }

    pub fn expand(&self, s: &str) -> DataSmartResult<Option<String>> {
        RefCell::borrow(&self.data)
            .expand(s, Option::<String>::None)
            .with_context(|| format!("expand: {}", &s))
    }

    pub fn expand_varref<V: AsRef<str>>(&self, variable: V) -> DataSmartResult<()> {
        let variable = variable.as_ref();
        RefCell::borrow(&self.data)
            .expand_varref(variable)
            .with_context(|| format!("expand_varref: {}", &variable))
    }

    pub fn expand_with_refs<T: Into<String>, V: Into<String>>(
        &self,
        str: Option<T>,
        var: Option<V>,
    ) -> DataSmartResult<VariableParse> {
        RefCell::borrow(&self.data).expand_with_refs(str, var)
    }

    pub fn expand_with_varname<V: Into<String>>(
        &self,
        s: &str,
        varname: V,
    ) -> DataSmartResult<Option<String>> {
        RefCell::borrow(&self.data).expand(s, Some(varname))
    }

    pub fn rename_var<T: AsRef<str>, N: Into<String>>(
        &self,
        var: T,
        new_key: N,
    ) -> DataSmartResult<()> {
        RefCell::borrow(&self.data).rename_var(var, new_key)
    }

    pub fn del_var<T: AsRef<str>>(&self, var: T) {
        RefCell::borrow(&self.data).del_var(var);
    }

    pub fn create_copy(&self) -> DataSmart {
        DataSmart {
            data: RefCell::borrow(&self.data).create_copy(),
        }
    }

    pub fn keys(&self) -> DataSmartResult<Vec<String>> {
        RefCell::borrow(&self.data).keys()
    }

    pub fn expand_keys(&self) -> DataSmartResult<()> {
        expand_keys(self)
    }

    pub fn get_var_flag_contents(
        &self,
        var: &str,
        flag: &str,
        options: GetVarOptions,
    ) -> DataSmartResult<Option<VariableContents>> {
        RefCell::borrow(&self.data).get_var_flag_contents(var, flag, options)
    }

    pub fn get_var_flag(
        &self,
        key: &str,
        flag: &str,
        options: GetVarOptions,
        ret_parser: bool,
    ) -> DataSmartResult<Option<GetVarFlagReturn>> {
        RefCell::borrow(&self.data).get_var_flag(key, flag, options, ret_parser)
    }

    pub fn get_var_flags(
        &self,
        var: &str,
        expand: Option<HashSet<String>>,
        internal_flags: bool,
    ) -> DataSmartResult<HashMap<String, VariableContents>> {
        RefCell::borrow(&self.data).get_var_flags(var, expand, internal_flags)
    }

    pub fn del_var_flag<V: AsRef<str>, F: AsRef<str>>(&self, var: V, flag: F) {
        RefCell::borrow(&self.data).del_var_flag(var, flag)
    }
}
