use once_cell::sync::Lazy;
use regex::Regex;

#[cfg(feature = "python")]
pub mod data_smart;

#[cfg(feature = "python")]
pub mod variable_parse;

static PYTHON_EXPANSION_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(r"\$\{@.+?}").unwrap());

#[cfg(not(feature = "python"))]
mod no_python;

mod fixups;
#[cfg(feature = "python")]
pub mod method_pool;

#[cfg(not(feature = "python"))]
pub use no_python::handle_python;

#[cfg(feature = "python")]
pub use variable_parse::handle_python;
