use anyhow::Context;
use pyo3::exceptions::{PyException, PyRuntimeError};
use pyo3::prelude::*;
use pyo3::{PyObject, PyRefMut, create_exception};
use std::collections::HashSet;

use bytebraise_datasmart::errors::DataSmartError;
use crate::data_smart::variable_contents::VariableContents;
use crate::data_smart::{DataSmart, GetVarFlagReturn, GetVarOptions};

#[pyclass(unsendable)]
pub struct PyDataSmart {
    pub(crate) data: DataSmart,
}

#[allow(non_snake_case)]
#[pymethods]
impl PyDataSmart {
    #[new]
    pub fn new(data: DataSmart) -> Self {
        PyDataSmart { data }
    }

    #[args(expand = "true", noweakdefault = "false", parsing = "false")]
    pub fn getVar(
        self_: PyRefMut<Self>,
        var: &str,
        expand: bool,
        noweakdefault: bool,
        parsing: bool,
    ) -> anyhow::Result<PyObject> {
        match self_
            .data
            .get_var_opt(var, GetVarOptions::new(expand, noweakdefault, parsing))
            .map(|v| match v {
                Some(contents) => contents.into_py(self_.py()),
                None => self_.py().None(),
            })
            .with_context(|| format!("Python getVar: {}", var))
        {
            Ok(val) => Ok(val),
            // TODO
            Err(e) => Err(e.into()),
        }
    }

    #[args(
        expand = "true",
        noweakdefault = "false",
        parsing = "false",
        retparser = "false"
    )]
    pub fn getVarFlag(
        self_: PyRefMut<Self>,
        var: &str,
        flag: &str,
        expand: bool,
        noweakdefault: bool,
        parsing: bool,
        retparser: bool,
    ) -> anyhow::Result<PyObject> {
        let data = self_
            .data
            .get_var_flag(
                var,
                flag,
                GetVarOptions::default()
                    .expand(expand)
                    .no_weak_default(noweakdefault)
                    .parsing(parsing),
                retparser,
            )
            .with_context(|| format!("Python getVarFlag: {}", var))?;

        let data = match data {
            None => self_.py().None(),
            Some(ret) => match ret {
                GetVarFlagReturn::ContentsOnly(contents) => contents.into_py(self_.py()),
                GetVarFlagReturn::ContentsWithParser((contents, parser)) => unimplemented!(),
            },
        };

        Ok(data)
    }

    #[args(expand = "None", internalflags = "false")]
    pub fn getVarFlags(
        self_: PyRefMut<Self>,
        var: &str,
        expand: Option<HashSet<String>>,
        internalflags: bool,
    ) -> anyhow::Result<PyObject> {
        let ret = self_.data.get_var_flags(var, expand, internalflags)?;
        if ret.is_empty() {
            return Ok(self_.py().None());
        }

        Ok(ret.into_py(self_.py()))
    }

    #[args(varname = "None")]
    pub fn expand(
        self_: PyRefMut<Self>,
        s: &str,
        varname: Option<String>,
    ) -> anyhow::Result<PyObject> {
        let ret = match varname {
            None => self_.data.expand(s),
            Some(varname) => self_.data.expand_with_varname(s, varname),
        }?;

        if ret.is_none() {
            return Ok(self_.py().None());
        }

        Ok(ret.unwrap().into_py(self_.py()))
    }
}

create_exception!(bytebraise, DataExpansionError, PyException);

impl IntoPy<PyObject> for VariableContents {
    fn into_py(self, py: Python) -> PyObject {
        match self {
            VariableContents::String(str) => str.into_py(py),
            VariableContents::Bool(bool) => bool.into_py(py),
            VariableContents::Map(map) => map.into_py(py),
            VariableContents::Set(set) => set.into_py(py),
            VariableContents::Vec(vec) => vec.into_py(py),
            VariableContents::Option(opt) => opt.map(|v| v.into_py(py)).into_py(py),
            VariableContents::Tuple(tup) => (tup.0.into_py(py), tup.1.into_py(py)).into_py(py),
        }
    }
}
