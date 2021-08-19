use std::collections::{HashMap, HashSet};

use regex::Captures;
#[cfg(feature = "python")]
use {
    crate::data_smart::python_bridge::integration::PyDataSmart,
    crate::utils::contains,
    pyo3::exceptions::PySyntaxError,
    pyo3::ffi::PyModule_AddFunctions,
    pyo3::once_cell::GILOnceCell,
    pyo3::prelude::*,
    pyo3::types::IntoPyDict,
    pyo3::types::PyList,
    pyo3::{PyCell, Python, ToPyObject},
};

use crate::data_smart::errors::{DataSmartError, DataSmartResult};
use crate::data_smart::variable_contents::VariableContents;
use crate::data_smart::{DataSmart, GetVarOptions};

#[derive(Debug, Clone)]
pub struct VariableParse {
    name: Option<String>,
    pub(crate) value: Option<String>,
    d: DataSmart,
    pub(crate) references: HashSet<String>,
    pub(crate) removes: Option<HashSet<String>>,
    execs: HashSet<String>,
    pub(crate) contains: HashMap<String, String>, // TODO type
}

#[cfg(feature = "python")]
static LIST_CELL: GILOnceCell<Py<PyModule>> = GILOnceCell::new();

// TODO this is just a mess of test code
#[cfg(feature = "python")]
pub fn get_shared_list(py: Python) -> &PyModule {
    LIST_CELL
        .get_or_init(py, || {
            let bb = PyModule::new(py, "bb").unwrap();
            let utils = PyModule::new(py, "utils").unwrap();
            utils
                .add_function(wrap_pyfunction!(py_which, utils).unwrap())
                .unwrap();
            utils
                .add_function(wrap_pyfunction!(py_contains, utils).unwrap())
                .unwrap();
            bb.add_submodule(&utils).unwrap();
            let parse = PyModule::new(py, "parse").unwrap();
            parse
                .add_function(wrap_pyfunction!(py_vars_from_file, parse).unwrap())
                .unwrap();
            bb.add_submodule(&parse).unwrap();
            bb.into()
        })
        .as_ref(py)
}

#[cfg(feature = "python")]
#[pyfunction]
#[pyo3(name = "vars_from_file")]
fn py_vars_from_file(
    file_name: Option<&str>,
    _d: &PyDataSmart,
) -> anyhow::Result<(Option<String>, Option<String>, Option<String>)> {
    if let Some(file_name) = file_name.map(|p| PathBuf::from(p)) {
        if matches!(
            file_name.extension().unwrap().to_str(),
            Some("bb") | Some("bbappend")
        ) {
            let stem = file_name.file_stem().unwrap().to_str().unwrap();
            let parts = stem
                .split("_")
                .map(|part| part.to_string())
                .collect::<Vec<_>>();

            if parts.len() > 3 {
                return Err(pyo3::exceptions::PyRuntimeError::new_err(
                    "too many underscores in file",
                )
                .into());
            }

            return Ok((
                parts.get(0).cloned(),
                parts.get(1).cloned(),
                parts.get(2).cloned(),
            ));
        }
    }

    Ok((None, None, None))
}

#[cfg(feature = "python")]
#[pyfunction(direction = 0, history = false, executable = false)]
#[pyo3(name = "which")]
fn py_which(
    path: Option<&str>,
    item: &str,
    direction: u8,
    history: bool,
    executable: bool,
) -> String {
    // TODO
    String::from("")
}

#[cfg(feature = "python")]
#[pyfunction]
#[pyo3(name = "contains")]
fn py_contains<'a>(
    variable: &str,
    checkvalues: &str,
    truevalue: &'a str,
    falsevalue: &'a str,
    d: &PyDataSmart,
) -> &'a str {
    contains(variable, checkvalues, truevalue, falsevalue, &d.data).unwrap()
}

impl VariableParse {
    // TODO: this needs to take DataSmartInner, not DataSmart, to avoid multiple borrows
    pub fn new<T: Into<String>>(var: Option<T>, d: DataSmart) -> VariableParse {
        VariableParse {
            name: var.map(Into::into),
            value: None,
            d,
            references: HashSet::new(),
            removes: None,
            execs: HashSet::new(),
            contains: HashMap::new(),
        }
    }

    pub fn var_sub(&mut self, caps: &Captures) -> DataSmartResult<String> {
        let match_str = caps.get(0).unwrap().as_str();
        let referenced_var = &match_str[2..match_str.len() - 1];
        if let Some(var) = &self.name {
            if var.as_str() == referenced_var {
                return Err(DataSmartError::RecursiveReferenceError { var: var.clone() }.into());
            }
        }
        self.references.insert(referenced_var.to_string());
        Ok(self
            .d
            .get_var_opt(referenced_var, GetVarOptions::default())?
            .map(|v| {
                if let VariableContents::String(str) = v {
                    return str;
                }

                panic!("Non-string value");
            })
            .unwrap_or(match_str.to_string()))
    }

    #[cfg(feature = "python")]
    pub fn python_sub(&mut self, caps: &Captures) -> DataSmartResult<String> {
        let match_str = caps.get(0).unwrap().as_str();
        let code = &match_str[3..match_str.len() - 1];

        // TODO: parse Python code to extract references!
        // let span = make_strspan(code);
        // let parsed = parse_single_input(span);
        // dbg!(parsed);

        Python::with_gil(|py| {
            let os = PyModule::import(py, "os")?;
            let time = PyModule::import(py, "time")?;

            //             use pyo3::py_run;
            //
            //             let locals = [("module_name", "oe.types"), ("file_path", "/home/laplante/yocto/sources/poky/meta/lib/oe/types.py")].into_py_dict(py);
            //             py_run!(py, *locals, r##"
            // import importlib.util
            // import sys
            // spec = importlib.util.spec_from_file_location(module_name, file_path)
            // module = importlib.util.module_from_spec(spec)
            // sys.modules[module_name] = module
            // spec.loader.exec_module(module)
            //     "##);
            //
            //             let locals = [("module_name", "oe.utils"), ("file_path", "/home/laplante/yocto/sources/poky/meta/lib/oe/utils.py")].into_py_dict(py);
            //             py_run!(py, *locals, r##"
            // import importlib.util
            // import sys
            // spec = importlib.util.spec_from_file_location(module_name, file_path)
            // module = importlib.util.module_from_spec(spec)
            // sys.modules[module_name] = module
            // spec.loader.exec_module(module)
            //     "##);
            let sys: &PyModule = py.import("sys").unwrap();
            let syspath: &PyList = sys.getattr("path").unwrap().try_into().unwrap();

            // syspath
            //     .insert(0, "/home/laplante/yocto/sources/poky/meta/lib/")
            //     .unwrap();

            let bb = get_shared_list(py);

            let locals = None;

            let oe = PyModule::new(py, "oe")?;
            let oe_utils = py.import("oe.utils").unwrap();
            let oe_types = py.import("oe.types").unwrap();
            oe.setattr("utils", oe_utils)?;
            oe.setattr("types", oe_types)?;

            let builtins = PyModule::import(py, "builtins")?;

            let globals = [
                ("os", os.to_object(py)),
                ("bb", bb.to_object(py)),
                ("time", time.to_object(py)),
                ("oe", oe.to_object(py)),
                ("sys", sys.to_object(py)),
                ("__builtins__", builtins.to_object(py)),
                (
                    "d",
                    PyCell::new(py, PyDataSmart::new(self.d.clone()))
                        .unwrap()
                        .to_object(py),
                ),
            ]
            .into_py_dict(py);

            match py.eval(code, Some(globals), locals) {
                Ok(result) => Ok(result.str().unwrap().to_string()),
                Err(e) => {
                    if e.is_instance::<PySyntaxError>(py) {
                        let err_value = e.pvalue(py).str().unwrap();
                        return if err_value
                            .to_str()
                            .unwrap()
                            .starts_with("EOL while scanning string literal")
                        {
                            // ignore, just mismatched braces
                            Ok(match_str.to_string())
                        } else {
                            // TODO: possible to shove PySyntaxError itself into result?
                            Err(DataSmartError::PythonSyntaxError { source: e }.into())
                        };
                    }

                    let ret: anyhow::Error = e.into();

                    Err(ret).context(format!("couldn't expand expression: {}", &code))
                }
            }
        })
    }
}
