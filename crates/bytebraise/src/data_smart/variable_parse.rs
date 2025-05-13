use std::collections::{HashMap, HashSet};

use regex::Captures;

use crate::data_smart::variable_contents::VariableContents;
use crate::data_smart::{DataSmart, GetVarOptions};
use bytebraise_datasmart::errors::{DataSmartError, DataSmartResult};

#[derive(Debug, Clone)]
pub struct VariableParse {
    name: Option<String>,
    pub(crate) value: Option<String>,
    pub(crate) d: DataSmart,
    pub(crate) references: HashSet<String>,
    pub(crate) removes: Option<HashSet<String>>,
    execs: HashSet<String>,
    pub(crate) contains: HashMap<String, String>, // TODO type
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
}
