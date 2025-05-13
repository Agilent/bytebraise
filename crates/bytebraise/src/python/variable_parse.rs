use bytebraise_datasmart::errors::{DataSmartError, DataSmartResult};
use crate::data_smart::variable_parse::VariableParse;
use crate::python::PYTHON_EXPANSION_REGEX;
use crate::python::data_smart::PyDataSmart;
use crate::python::method_pool::method_pool;
use anyhow::Context;
use pyo3::py_run;
use regex::{Captures, Regex};
use std::borrow::Cow;
use std::env;
use std::error::Error;
use {
    crate::utils::contains,
    pyo3::exceptions::PySyntaxError,
    pyo3::once_cell::GILOnceCell,
    pyo3::prelude::*,
    pyo3::types::IntoPyDict,
    pyo3::types::PyList,
    pyo3::{PyCell, Python, ToPyObject},
};
use bytebraise_util::split::ReplaceFallible;

pub fn handle_python<'a>(
    input: &'a Cow<'a, str>,
    ret: &'a mut VariableParse,
) -> DataSmartResult<Cow<'a, str>> {
    PYTHON_EXPANSION_REGEX
        .replace_fallible(input.as_ref(), |caps: &Captures| ret.python_sub(caps))
        .with_context(|| format!("unable to expand {}", input))
}

static LIST_CELL: GILOnceCell<Py<PyModule>> = GILOnceCell::new();

// TODO this is just a mess of test code
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

#[pyfunction]
#[pyo3(name = "vars_from_file")]
fn py_vars_from_file(
    file_name: Option<&str>,
    _d: &PyDataSmart,
) -> anyhow::Result<(Option<String>, Option<String>, Option<String>)> {
    use std::path::PathBuf;
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

static ENVIRONMENT_SETUP: GILOnceCell<()> = GILOnceCell::new();

pub fn setup_environment(py: Python) {
    ENVIRONMENT_SETUP.get_or_init(py, || {
        let sys = py.eval("__import__('sys')", None, None).unwrap();
        py_run!(
            py,
            sys,
            &format!(
                "sys.argv = ['{}']",
                env::current_exe().unwrap().to_str().unwrap()
            )
        );
    });
}

impl VariableParse {
    pub fn python_sub(&mut self, caps: &Captures) -> DataSmartResult<String> {
        use anyhow::Context;

        let match_str = caps.get(0).unwrap().as_str();
        let code = &match_str[3..match_str.len() - 1];

        // TODO: parse Python code to extract references!
        // let span = make_strspan(code);
        // let parsed = parse_single_input(span);
        // dbg!(parsed);

        Python::with_gil(|py| {
            let os = PyModule::import(py, "os")?;
            let time = PyModule::import(py, "time")?;

            let sys: &PyModule = py.import("sys").unwrap();

            let bb = get_shared_list(py);

            let builtins = PyModule::import(py, "builtins")?;

            let globals = [
                ("os", os.to_object(py)),
                ("bb", bb.to_object(py)),
                ("time", time.to_object(py)),
                ("sys", sys.to_object(py)),
                ("__builtins__", builtins.to_object(py)),
            ]
            .into_py_dict(py);

            for item in method_pool(py) {
                globals.set_item(item.0, item.1).unwrap();
            }

            let locals = [(
                "d",
                PyCell::new(py, PyDataSmart::new(self.d.clone()))
                    .unwrap()
                    .to_object(py),
            )]
            .into_py_dict(py);

            // TODO: move some of the stuff from above into one-time env setup?
            setup_environment(py);

            match py.eval(&code, Some(globals), Some(locals)) {
                Ok(result) => Ok(result.str().unwrap().to_string()),
                Err(e) => {
                    if e.is_instance_of::<PySyntaxError>(py) {
                        let err_value = e.value(py).str().unwrap().to_str().unwrap().to_string();
                        return if err_value.contains("EOL while scanning string literal")
                            || err_value.starts_with("unterminated string literal")
                        {
                            // ignore, just mismatched braces - leave unexpanded
                            Ok(match_str.to_string())
                        } else {
                            // TODO: possible to shove PySyntaxError itself into result?
                            Err(DataSmartError::PythonSyntaxError { source: e }.into())
                        };
                    }

                    let traceback = e.traceback(py).unwrap().format().unwrap();
                    let ret: anyhow::Error = e.into();

                    Err(ret).context(traceback)
                }
            }
        })
    }
}
