#![feature(cell_filter_map)]
#![feature(box_syntax)]
#![feature(derive_default_enum)]
#![feature(min_specialization)]
#![feature(assert_matches)]

pub mod build;
pub mod cooker_data;
pub mod data_smart;
pub mod editor;
pub mod lexer;
pub mod parser;
pub mod python;
pub mod split_var_value;
pub mod syntax;
pub mod utils;

pub type ByteBraiseResult<T> = anyhow::Result<T>;
