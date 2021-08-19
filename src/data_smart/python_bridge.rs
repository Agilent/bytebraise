#[cfg(feature = "python")]
pub mod integration {
    use anyhow::Context;
    use pyo3::exceptions::{PyException, PyRuntimeError};
    use pyo3::prelude::*;
    use pyo3::{create_exception, PyObject, PyRefMut};

    use crate::data_smart::errors::DataSmartError;
    use crate::data_smart::variable_contents::VariableContents;
    use crate::data_smart::{DataSmart, GetVarOptions};

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

    impl std::convert::From<DataSmartError> for PyErr {
        // TODO: more specific error types
        fn from(err: DataSmartError) -> PyErr {
            PyRuntimeError::new_err(err.to_string())
        }
    }
}
