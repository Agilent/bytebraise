//!
//! ## Introduction
//! This crate is tasked with implementing the syntax described here: <https://docs.yoctoproject.org/bitbake/bitbake-user-manual/bitbake-user-manual-metadata.html>. This document
//! does not strive to fully explain the syntax - see the previous link for that. Instead, it tries to explain how `bytebraise` implements it.
//!
//! ## Terminology
//! `bytebraise` implements BitBake syntax in a much different way than BitBake itself does. Below
//! you'll find some terminology I've come up with to try to explain how `bytebraise` works.
//!
//! ### Base variables and override variants
//! Consider this snippet:
//!
//! ```text
//! MY_VAR = "base"
//! MY_VAR:a = "a"
//! MY_VAR:a:b = "ab"
//! ```
//!
//! Let `MY_VAR` be the **base variable**, and let `MY_VAR:a` and `MY_VAR:a:b` be its
//! **override variants**.
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
//! ```
//!
//! Terminology:
//! - `MY_VAR:append` is an **unqualified** append operator.
//! - `MY_VAR:append:a` is an **override-filtered** operator - it applies if the `a` override is active.
//! - `MY_VAR:b:append` is an **override-scoped** operator - it applies to the `MY_VAR:b` override variant.
//!
//! You can also have an assignment that is a combination of an override-filtered and override-scoped operator:
//!
//! ```text
//! MY_VAR = "base"
//! MY_VAR:a:append:b = " ab"
//! ```
//!
//! In this example, `MY_VAR:a:append:b` is scoped to the `MY_VAR:a`, but filtered to only apply
//! when the `b` override is active. I don't have a great term for this yet (not sure if it needs one?),
//! but if you insist, maybe *scoped-and-filtered* operator?
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
pub mod errors;
pub mod evaluate;
pub mod macros;
pub mod petgraph2;
pub mod variable_operation;
