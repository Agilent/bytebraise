use std::io;

use thiserror::Error;

#[derive(Error, Debug)]
pub enum DataSmartError {
    #[error("Unable to convert")]
    DataConversionError,

    #[cfg(test)]
    #[error("Attempt to use ? operator on None")]
    UnwrapNoneError,

    #[error("")]
    #[cfg(feature = "python")]
    PythonError {
        #[from]
        source: pyo3::PyErr,
    },

    #[error("TODO")]
    IoError {
        #[from]
        source: io::Error,
    },

    #[error("")]
    #[cfg(feature = "python")]
    PythonSyntaxError { source: pyo3::PyErr },

    #[error("A variable references itself")]
    RecursiveReferenceError { var: String },
}

pub type DataSmartResult<T> = anyhow::Result<T>;
