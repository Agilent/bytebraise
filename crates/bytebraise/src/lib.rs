#![feature(min_specialization)]
#![feature(assert_matches)]

pub mod build;
pub mod cooker_data;
pub mod data_smart;
pub mod editor;
pub mod python;
pub mod split_var_value;
pub mod utils;
pub mod evaluate;

pub type ByteBraiseResult<T> = anyhow::Result<T>;
