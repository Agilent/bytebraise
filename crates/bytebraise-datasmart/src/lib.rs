//!
//! ## data_smart
//!
//! This crate is tasked with implementing the syntax described here: <https://docs.yoctoproject.org/bitbake/bitbake-user-manual/bitbake-user-manual-metadata.html>. At first glance,
//! it seems relatively straightforward. But unfortunately it is deceptively complex.
//!
//!
//! ### Assignments, defaults, weak defaults
//! ```text
//! A ?= "b"
//! A ??= "c"
//! A = "a"
//! ```
//!
//!
//! ### Parsing context
//!
//! Metadata with:
//! ```text
//! A ?= "b"
//! A ??= "c"
//! A = "a"
//! ```
//!
//! is not directly equivalent to:
//!
//! ```rust
//! use bytebraise_datasmart::petgraph2::DataSmart;
//! let mut d = DataSmart::new();
//! d.default_var("A", "b");
//! d.weak_default_var("A", "c");
//! d.set_var("A", "a");
//! ```
//!
//! The former uses 'parsing context' whereas the latter does not.


pub mod errors;
pub mod petgraph2;
pub mod variable_operation;
pub mod evaluate;
