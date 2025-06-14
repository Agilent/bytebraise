//!
//! ## Introduction
//! This crate is tasked with implementing the syntax described here: <https://docs.yoctoproject.org/bitbake/bitbake-user-manual/bitbake-user-manual-metadata.html>. This document
//! does not strive to fully explain the syntax - see the previous link for that. Instead, it tries to explain how `bytebraise` implements it.
//!
//!
//!
//! ## Terminology
//! `bytebraise` implements BitBake syntax in a much different way than BitBake itself does. Below
//! you'll find some terminology I've come up with to try to explain how `bytebraise` works.
//! ### Base variables and override variants
//! Consider this snippet:
//!
//! ```text
//! MY_VAR = "base"
//! MY_VAR:a = "a"
//! MY_VAR:a:b = "ab"
//! ```
//!
//! Let `MY_VAR` be the **base variable**, and let `MY_VAR:a` and `MY_VAR:a:b` be
//! **override-qualified variables**. `My_VAR:a` and `MY_VAR:a:b` can also be called **override-qualified variants** of `MY_VAR`.
//!
//! BitBake informally calls the latter ["versions"](https://docs.yoctoproject.org/bitbake/bitbake-user-manual/bitbake-user-manual-metadata.html#conditional-metadata) of a variable.
//!
//! ### Override-scoped, override-filtered, and unqualified operators
//! Consider these statements involving `append`:
//!
//! ```text
//! MY_VAR = "base"
//! MY_VAR:append = " more"
//! MY_VAR:append:a = " a"
//! MY_VAR:b:append = " b"
//! MY_VAR:a:append:b = " ab"
//! ```
//!
//! Terminology:
//! - `MY_VAR:append` is an **unqualified** append operator.
//! - `MY_VAR:append:a` is an **override-filtered** operator - it applies if the `a` override is active.
//! - `MY_VAR:b:append` is an **override-scoped** operator - it applies to the `MY_VAR:b` override variant.
//! - `MY_VAR:a:append:b` is both - it applies to the `MY_VAR:b` override variant if `a` is active. We can call it a **scoped-and-filtered operator**.
//!
//! ### Direct variant lookup
//! Consider:
//!
//! ```text
//! MY_VAR = "base"
//! MY_VAR:a = "flavor"
//! ```
//!
//! We can explicitly get the value of `MY_VAR:a` with `d.getVar("MY_VAR:a")` (in BitBake) or `get_var!($d, "MY_VAR:a")` (in `bytebraise`).
//!
//! This is called a **direct variant lookup**. What's interesting is the interaction between direct variant lookups and override-scoped operators:
//!
//! ```text
//! MY_VAR = "base"
//! MY_VAR:a = "different"
//! MY_VAR:a:append = "!"
//! MY_VAR:append:a = "?"
//! ```
//!
//! Calling `get_var!($d, "MY_VAR")` gives `"base"`, `get_var!($d, "MY_VAR:a")` gives `"different!"`.
//!
//! The presence of `'a'` in `MY_VAR:a` influences the override scope, but not the override filter (since `a` is not actually
//! in the override set, the `MY_VAR:append:a = "?"` doesn't apply).
//!
//! ### Override score
//! The basic idea behind BitBake overrides is that the more "specific" override wins. Overrides
//! (via the `OVERRIDES` variable) are listed in increasing priority order. See [bitbake.conf in poky](https://github.com/yoctoproject/poky/blob/e3d24e5cd8eee5d0d232abdfef33cacf0ad96787/meta/conf/bitbake.conf#L795)
//! for more details of how it's setup in Yocto.
//!
//! In `bytebraise`, the notion of override specificity is encoded as the **override score**. For the sake
//! of this section, the actual values don't matter. But here are some examples:
//!
//! ```text
//! OVERRIDES = "a:b:c"
//! MY_VAR = "base"
//! MY_VAR:a = "a override"
//! MY_VAR:b = "b override"
//! MY_VAR:c = "c override'
//! ```
//!
//! When you call `get_var("MY_VAR")`, the result is `"c override"` because `c` is the highest scoring (priority)
//! active override. So the override score for `:c` is the highest, whereas that of `:a` is the lowest.
//!
//! See the later section on the override score algorithm for all the gory details. It gets messy.
//!
//! ## BitBake internals
//!
//! ### remove, prepend, append
//! These are stored as varflags on the target variable. However, if unexpanded (i.e. contains '${')
//! then no varflag is added. See DataSmart::setVar and `__setvar_regexp__`.
//!
pub mod errors;
pub mod evaluate;
pub mod keys_iter;
pub mod macros;
pub(crate) mod nodes;
pub mod petgraph2;
mod tests;
pub mod variable_operation;
