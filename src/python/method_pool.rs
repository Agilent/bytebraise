use crate::python::fixups::methods::fixup_oe_import;
use pyo3::once_cell::GILOnceCell;
use pyo3::prelude::PyModule;
use pyo3::types::PyDict;
use pyo3::{Py, PyAny, PyErr, PyResult, Python};

static METHOD_POOL: GILOnceCell<Py<PyDict>> = GILOnceCell::new();

pub fn method_pool(py: Python) -> &PyDict {
    METHOD_POOL
        .get_or_init(py, || PyDict::new(py).into())
        .as_ref(py)
}

pub fn compile_function<C: AsRef<str>>(name: &String, code: C) -> PyResult<()> {
    let mut code = code.as_ref().to_string();
    let name = name.trim();

    if name == "oe_import" {
        fixup_oe_import(&mut code);
    }

    Python::with_gil(|py| -> PyResult<()> {
        let fun: Py<PyAny> = PyModule::from_code(py, &code, "", "")?
            .getattr(name)?
            .into();

        method_pool(py).set_item(&name, fun).unwrap();

        Ok(())
    })?;

    Ok(())
}
