use std::borrow::Cow;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::{BTreeSet, HashSet};
use std::convert::{TryFrom, TryInto};
use std::iter::FromIterator;
use std::rc::{Rc, Weak};

use anyhow::Context;
use im_rc::HashMap;
use indexmap::set::IndexSet;
use once_cell::sync::Lazy;
pub use public_interface::DataSmart;
use regex::{Captures, Regex};
use scopeguard::{ScopeGuard, defer, guard};
use utils::split_filter_empty;
use variable_contents::{VariableContents, VariableContentsAccessors};

use crate::data_smart::errors::{DataSmartError, DataSmartResult};
use crate::data_smart::overrides::{PerVarOverrideData, VarAndOverrideTuple};
use crate::data_smart::utils::{ReplaceFallible, split_keep};
use crate::data_smart::variable_parse::VariableParse;
use crate::python::handle_python;

pub mod errors;
pub mod overrides;
mod public_interface;
mod tests;
pub mod utils;
pub mod variable_contents;
pub mod variable_parse;

static VAR_EXPANSION_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"\$\{[a-zA-Z0-9\-_+./~]+?}").unwrap());
static SETVAR_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"^(?P<base>.*?)(?P<keyword>_append|_prepend|_remove)(?:_(?P<add>[^A-Z]*))?$")
        .unwrap()
});
static WHITESPACE_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(r"\s").unwrap());

const CONTENT_FLAG: &str = "_content";
const EXPORT_LIST_ITEM: &str = "__exportlist";
const APPEND_FLAG: &str = "_append";
const PREPEND_FLAG: &str = "_prepend";
const REMOVE_FLAG: &str = "_remove";

#[derive(Clone, Debug)]
struct OverrideState {
    // The set of variables that affect the value of OVERRIDES.
    // Defaults to the OVERRIDES variable itself, and also FILE for some reason. TODO: why FILE?
    // If for example you do:
    //      OVERRIDES .= ":${MY_VAR}"
    // then MY_VAR will be added to this set.
    vars: HashSet<String>,

    // The active set of overrides.
    // Basically just the value of OVERRIDES split at the ':' character.
    active_overrides: Option<IndexSet<String>>,
}

impl OverrideState {
    pub fn new() -> Self {
        OverrideState {
            vars: HashSet::from_iter(vec!["OVERRIDES".into(), "FILE".into()]),
            active_overrides: None,
        }
    }

    pub fn is_override_var<T: AsRef<str>>(&self, var: T) -> bool {
        self.vars.contains(var.as_ref())
    }

    pub fn is_override_active(&self, override_str: &str) -> bool {
        if override_str.is_empty() {
            return true;
        }

        if let Some(active) = &self.active_overrides {
            if active.contains(override_str) {
                return true;
            } else if override_str.contains('_') {
                let os = split_filter_empty(override_str, "_")
                    .map(|v| v.to_string())
                    .collect::<IndexSet<_>>();
                if os.is_subset(active) {
                    return true;
                }
            }
        }

        false
    }
}

type DataSmartFlags = HashMap<String, VariableContents>;

#[derive(Debug)]
struct ExpansionState {
    visited: HashSet<String>,
}

impl ExpansionState {
    pub fn new() -> Self {
        ExpansionState {
            visited: HashSet::new(),
        }
    }
}

#[derive(Debug)]
pub struct DataSmartInner {
    // TODO: document fundamental differences with how this is implemented and how BitBake implements it
    data: RefCell<im_rc::HashMap<String, DataSmartFlags>>,

    // TEST_local_foo_bar = "..."
    // {'TEST_local_foo': [('TEST_local_foo_bar', 'bar')], 'TEST_local': [('TEST_local_foo_bar', 'foo_bar')], 'TEST': [('TEST_local_foo_bar', 'local_foo_bar')]}
    per_var_override_data: RefCell<PerVarOverrideData>,

    override_state: RefCell<OverrideState>,
    inside_need_overrides: RefCell<()>,

    weak_ref: Weak<RefCell<Self>>,

    expand_state: RefCell<Option<ExpansionState>>,
}

#[derive(Copy, Clone, Debug)]
pub struct GetVarOptions {
    expand: bool,
    no_weak_default: bool,
    parsing: bool,
}

impl Default for GetVarOptions {
    fn default() -> Self {
        GetVarOptions {
            expand: true,
            no_weak_default: false,
            parsing: false,
        }
    }
}

impl GetVarOptions {
    pub fn new(expand: bool, no_weak_default: bool, parsing: bool) -> Self {
        GetVarOptions {
            expand,
            no_weak_default,
            parsing,
        }
    }

    pub fn no_weak_default(self, no_weak_default: bool) -> Self {
        GetVarOptions {
            no_weak_default,
            ..self
        }
    }
    pub fn expand(self, expand: bool) -> Self {
        GetVarOptions { expand, ..self }
    }
    pub fn parsing(self, parsing: bool) -> Self {
        GetVarOptions { parsing, ..self }
    }
}

pub enum GetVarFlagReturn {
    ContentsOnly(VariableContents),
    ContentsWithParser((VariableContents, VariableParse)),
}
//
// impl GetVarFlagReturn {
//     fn contents(&self) -> &VariableContents {
//         match self {
//             GetVarFlagReturn::ContentsOnly(c) => c,
//             GetVarFlagReturn::ContentsWithParser((c, _)) => c,
//         }
//     }
// }

impl DataSmartInner {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new_cyclic(|weak| RefCell::new(Self::new_inner(weak.clone())))
    }

    fn new_inner(weak: Weak<RefCell<DataSmartInner>>) -> DataSmartInner {
        DataSmartInner {
            data: RefCell::new(HashMap::new()),
            override_state: RefCell::new(OverrideState::new()),
            inside_need_overrides: RefCell::new(()),
            per_var_override_data: RefCell::new(PerVarOverrideData::new()),
            weak_ref: weak,
            expand_state: RefCell::new(None),
        }
    }

    fn upgrade(&self) -> DataSmart {
        DataSmart {
            data: self.weak_ref.upgrade().unwrap(),
        }
    }

    pub fn create_copy(&self) -> Rc<RefCell<Self>> {
        Rc::new_cyclic(|weak| {
            RefCell::new(DataSmartInner {
                data: RefCell::new(RefCell::borrow(&self.data).clone()),
                override_state: RefCell::new(RefCell::borrow(&self.override_state).clone()), // TODO: ok for this to clone inner?
                per_var_override_data: self.per_var_override_data.clone(),
                ..Self::new_inner(weak.clone())
            })
        })
    }

    fn _klist(&self) -> HashSet<String> {
        RefCell::borrow(&self.data)
            .keys()
            .cloned()
            .collect::<HashSet<_>>()
    }

    pub fn keys(&self) -> DataSmartResult<Vec<String>> {
        let mut overrides = HashSet::new();

        self.need_overrides()?;
        for (var, override_data) in RefCell::borrow(&self.per_var_override_data).collect() {
            if RefCell::borrow(&self.override_state).is_override_active(&override_data.override_str)
            {
                overrides.insert(var);
            }
        }

        let mut ret = self
            ._klist()
            .drain()
            .filter(|v| !overrides.contains(v))
            .collect::<Vec<_>>();
        ret.extend(overrides.drain());
        Ok(ret)
    }

    pub fn expand_varref<V: AsRef<str>>(&self, variable: V) -> DataSmartResult<()> {
        let variable = variable.as_ref();
        let needle = format!("${{{variable}}}");
        // TODO: is unwrap ok?
        let value = self
            .get_var_opt(variable, GetVarOptions::default().expand(false))?
            .unwrap()
            .as_string();

        for key in self._klist() {
            if let Some(referrervalue) = self
                .get_var_opt(&key, GetVarOptions::default().expand(false))
                .with_context(|| format!("expand_varref: {variable}"))?
            {
                let referrervalue = referrervalue.as_string();
                if referrervalue.contains(&needle) {
                    let referrervalue = referrervalue.replace(&needle, &value);
                    self.set_var(key, referrervalue, false)?;
                }
            }
        }

        Ok(())
    }

    pub fn expand_with_refs<T: Into<String>, V: Into<String>>(
        &self,
        str: Option<T>,
        var: Option<V>,
    ) -> DataSmartResult<VariableParse> {
        let mut ret = VariableParse::new(var.map(Into::into), self.upgrade());
        if str.is_none() {
            return Ok(ret);
        }

        // |expand_state| is used to track which variables are accessed during an expansion, across
        // recursive calls to this method. The first (i.e. non-recursive) call to this method is
        // responsible for setting up and tearing down the state.

        // Create a scope guard that will clear out the expansion state upon scope exit
        let scope = guard((), |()| {
            RefCell::borrow_mut(&self.expand_state).take();
        });

        // Check if we are in the middle of recursion by seeing if the expand state exists yet
        {
            let mut s = RefCell::borrow_mut(&self.expand_state);
            if s.is_none() {
                // The expansion state doesn't exist yet, so create it
                *s = Some(ExpansionState::new());
            } else {
                // Expansion state exists; defuse scope guard - we are not responsible for the state
                ScopeGuard::into_inner(scope);
            }
        }

        let mut value = str.unwrap().into();
        while value.contains("${") {
            let new_value =
                VAR_EXPANSION_REGEX.replace_fallible(value.as_ref(), |caps: &Captures| {
                    let match_str = caps.get(0).unwrap().as_str();
                    let referenced_var = &match_str[2..match_str.len() - 1];

                    {
                        let mut s = RefCell::borrow_mut(&self.expand_state);
                        let set = s.as_mut().unwrap();
                        if set.visited.contains(referenced_var) {
                            return Err(DataSmartError::RecursiveReferenceError {
                                var: referenced_var.to_string(),
                            }
                            .into());
                        } else {
                            // TODO: remove hack - when accessing contents of 'OVERRIDES' variable,
                            if referenced_var != "TOOLCHAIN" && referenced_var != "RUNTIME" {
                                set.visited.insert(referenced_var.to_string());
                            }
                        }
                    }

                    defer! {
                        let mut s = RefCell::borrow_mut(&self.expand_state);
                        let set = s.as_mut().unwrap();
                        set.visited.remove(referenced_var);
                    }

                    ret.var_sub(caps)
                })?;

            let new_value = handle_python(&new_value, &mut ret)?.to_string();
            if value == new_value {
                break;
            }
            value = new_value;
        }

        ret.value = Some(value);
        Ok(ret)
    }

    pub fn expand<V: Into<String>>(
        &self,
        s: &str,
        varname: Option<V>,
    ) -> DataSmartResult<Option<String>> {
        self.expand_with_refs(Some(s.to_string()), varname)
            .map(|v| v.value)
            .with_context(|| format!("unable to expand: {s}"))
    }

    pub fn get_var_opt(
        &self,
        var: &str,
        options: GetVarOptions,
    ) -> DataSmartResult<Option<VariableContents>> {
        self.get_var_flag_contents(var, CONTENT_FLAG, options)
            .with_context(|| format!("get_var_opt {var}, {options:?}"))
    }

    pub fn get_var(&self, var: &str) -> DataSmartResult<Option<VariableContents>> {
        self.get_var_opt(var, GetVarOptions::default())
            .with_context(|| format!("get_var {var}"))
    }

    pub fn get_var_flag_with_parser(
        &self,
        var: &str,
        flag: &str,
        options: GetVarOptions,
    ) -> DataSmartResult<Option<(VariableContents, VariableParse)>> {
        self.get_var_flag(var, flag, options, true).map(|v| {
            v.map(|v2| match v2 {
                GetVarFlagReturn::ContentsOnly(_) => unreachable!(),
                GetVarFlagReturn::ContentsWithParser(r) => r,
            })
        })
    }

    pub fn get_var_flag_contents(
        &self,
        var: &str,
        flag: &str,
        options: GetVarOptions,
    ) -> DataSmartResult<Option<VariableContents>> {
        self.get_var_flag(var, flag, options, false).map(|v| {
            v.map(|v2| match v2 {
                GetVarFlagReturn::ContentsOnly(c) => c,
                GetVarFlagReturn::ContentsWithParser(_) => unreachable!(),
            })
        })
    }

    pub fn get_var_flags(
        &self,
        var: &str,
        expand: Option<HashSet<String>>,
        internal_flags: bool,
    ) -> DataSmartResult<std::collections::HashMap<String, VariableContents>> {
        let var_flags = self.find_var(var);
        if var_flags.is_none() {
            return Ok(std::collections::HashMap::new());
        }

        // TODO: simpler way?
        let var_flags: Box<dyn Iterator<Item = (String, VariableContents)>> = match internal_flags {
            true => Box::new(var_flags.unwrap().clone().into_iter()),
            false => Box::new(
                var_flags
                    .unwrap()
                    .clone()
                    .into_iter()
                    .filter(|(flag, _data)| !flag.starts_with('_')),
            ),
        };

        let ret = var_flags
            .into_iter()
            .map(|(flag, data)| {
                if expand.as_ref().is_some_and(|set| set.contains(&flag)) {
                    // TODO: don't unwrap
                    let expanded = self
                        .expand(&flag, Some(format!("[{flag}]")))
                        .unwrap()
                        .unwrap();
                    (flag, expanded.into())
                } else {
                    (flag, data)
                }
            })
            .collect();

        Ok(ret)
    }

    pub fn get_var_flag(
        &self,
        key: &str,
        flag: &str,
        options: GetVarOptions,
        ret_parser: bool,
    ) -> DataSmartResult<Option<GetVarFlagReturn>> {
        if flag.is_empty() {
            // TODO: warn here, or in Python bindings?
            return Ok(None);
        }

        // TODO cache
        let cache_name = if flag == CONTENT_FLAG {
            Cow::Borrowed(key)
        } else {
            Cow::Owned(format!("{key}[{flag}]"))
        };

        let var_overrides = RefCell::borrow(&self.per_var_override_data)
            .get(key)
            .cloned();
        let mut value: Option<_> = None;
        let mut removes: HashSet<String> = HashSet::new();

        // Process overrides if accessing the _content flag (e.g. via get_var/getVar)
        if var_overrides.is_some() && (flag == CONTENT_FLAG) && !options.parsing {
            let var_overrides = var_overrides.unwrap();
            let mut the_match = None;

            self.need_overrides()?;

            {
                // We may have been called by |needs_overrides| so guard against None
                // TODO: shouldn't this be try_borrow?
                let override_state = RefCell::borrow(&self.override_state);
                if let Some(active_overrides) = &override_state.active_overrides {
                    let mut active_override_to_full_var_map = HashMap::new();

                    // Find the subset of variable overrides that are active
                    for entry in var_overrides.iter() {
                        let o = &entry.override_str;
                        if override_state.is_override_active(o) {
                            active_override_to_full_var_map.insert(o.clone(), &entry.full_var);
                        }
                    }

                    // This loop finds the most specific active override
                    // TODO: possible to come up with a simpler algorithm?
                    let mut map_was_modified = true;
                    while map_was_modified {
                        map_was_modified = false;
                        for o in active_overrides {
                            for a in active_override_to_full_var_map.clone().keys() {
                                let suffix = format!("_{o}");
                                if a.ends_with(&suffix) {
                                    let t = active_override_to_full_var_map.remove(a).unwrap();
                                    // Note this is only removing the override once at the end, as
                                    // opposed to BitBake which just uses |replace|.
                                    active_override_to_full_var_map
                                        .insert(a[..a.len() - suffix.len()].to_string(), t);

                                    map_was_modified = true;
                                } else if a == o {
                                    the_match =
                                        Some(active_override_to_full_var_map.remove(a).unwrap());
                                }
                            }
                        }
                    }
                }
            }

            if let Some(the_match) = the_match {
                if let Some((value_inner, subparser)) = self.get_var_flag_with_parser(
                    the_match.as_str(),
                    "_content",
                    GetVarOptions::default().expand(false),
                )? {
                    value = Some(value_inner);
                    removes = subparser.removes.unwrap_or_default();
                }
            }
        }

        let var_flags = self.find_var(key);

        if let Some(var_flags) = &var_flags {
            if value.is_none() {
                if var_flags.contains_key(flag) {
                    value = Some(var_flags.get(flag).cloned().unwrap());
                } else if (flag == CONTENT_FLAG)
                    && var_flags.contains_key("_defaultval")
                    && !options.no_weak_default
                {
                    value = Some(var_flags.get("_defaultval").cloned().unwrap());
                }
            }

            if (flag == CONTENT_FLAG) && !options.parsing {
                // Apply _append flags
                if let Some(append_flag_entry) = var_flags.get(APPEND_FLAG) {
                    if value.is_none() {
                        value = Some(VariableContents::String(String::new()));
                    }
                    self.need_overrides()?;

                    let data: &Vec<VariableContents> = append_flag_entry.try_into().unwrap();

                    for v in data {
                        let v: &(Box<VariableContents>, Box<VariableContents>) =
                            v.try_into().unwrap();
                        let r = v.0.as_ref().as_string();
                        let o: &Option<Box<VariableContents>> = v.1.as_ref().try_into().unwrap();

                        let override_string = o
                            .as_ref()
                            .map(|v| v.as_ref().as_string())
                            .unwrap_or_default();
                        if RefCell::borrow(&self.override_state)
                            .is_override_active(override_string.as_str())
                        {
                            value = Some(VariableContents::String(value.as_string() + &*r));
                        }
                    }
                }

                // Apply _prepend flags
                if let Some(prepend_flag_entry) = var_flags.get(PREPEND_FLAG) {
                    if value.is_none() {
                        value = Some(VariableContents::String(String::new()));
                    }
                    self.need_overrides()?;

                    let data: &Vec<VariableContents> = prepend_flag_entry.try_into().unwrap();
                    for v in data {
                        let v: &(Box<VariableContents>, Box<VariableContents>) =
                            v.try_into().unwrap();
                        let r = v.0.as_ref().as_string();
                        let o: &Option<Box<VariableContents>> = v.1.as_ref().try_into().unwrap();

                        let override_string = o
                            .as_ref()
                            .map(|v| v.as_ref().as_string())
                            .unwrap_or_default();

                        if RefCell::borrow(&self.override_state)
                            .is_override_active(override_string.as_str())
                        {
                            value =
                                Some(VariableContents::String(r.to_string() + &value.as_string()));
                        }
                    }
                }
            }
        }

        let mut parser = None;
        if options.expand || ret_parser {
            let value = value.clone().map(|v| v.as_string());
            parser = Some(self.expand_with_refs(value, Some(cache_name.to_owned()))?);
        }
        if options.expand {
            value = parser
                .clone()
                .and_then(|p| p.value)
                .map(|v| VariableContents::from(v.as_str()));
        }

        if let (Some(_value), Some(var_flags)) = (&value, &var_flags) {
            // Grab any _remove flags
            if (flag == CONTENT_FLAG) && !options.parsing {
                if let Some(removes2) = var_flags.get(REMOVE_FLAG) {
                    self.need_overrides()?;

                    let data: &Vec<VariableContents> = removes2.try_into().unwrap();
                    for v in data {
                        let v: &(Box<VariableContents>, Box<VariableContents>) =
                            v.try_into().unwrap();
                        let r = v.0.as_ref().as_string();
                        let o: &Option<Box<VariableContents>> = v.1.as_ref().try_into().unwrap();

                        let override_string = o
                            .as_ref()
                            .map(|v| v.as_ref().as_string())
                            .unwrap_or_default();
                        if RefCell::borrow(&self.override_state)
                            .is_override_active(override_string.as_str())
                        {
                            removes.insert(r.clone());
                        }
                    }
                }
            }
        }

        // Apply _remove flags
        if let (Some(value), Some(parser)) = (&mut value, &mut parser) {
            if !removes.is_empty() && (flag == CONTENT_FLAG) && !options.parsing {
                let mut expanded_removes = HashMap::new();
                for r in &removes {
                    expanded_removes.insert(
                        r.clone(),
                        self.expand(r, Option::<String>::None)?
                            .unwrap()
                            .split_whitespace()
                            .map(|v| v.to_string())
                            .collect::<Vec<_>>(),
                    );
                }

                parser.removes = Some(HashSet::new());
                let mut val = String::new();
                for v in split_keep(&WHITESPACE_REGEX, parser.value.as_ref().unwrap()) {
                    let mut skip = false;
                    for r in &removes {
                        if expanded_removes.get(r).unwrap().contains(&v.to_string()) {
                            parser.removes.as_mut().unwrap().insert(r.clone());
                            skip = true;
                        }
                    }
                    if skip {
                        continue;
                    }
                    val += v;
                }
                parser.value = Some(val.clone());
                if options.expand {
                    *value = VariableContents::String(val);
                }
            }
        }

        // TODO cache

        if ret_parser {
            return Ok(Some(GetVarFlagReturn::ContentsWithParser((
                value.unwrap(),
                parser.unwrap(),
            ))));
        }

        Ok(value.map(GetVarFlagReturn::ContentsOnly))
    }

    pub fn rename_var<T: AsRef<str>, N: Into<String>>(
        &self,
        var: T,
        new_key: N,
    ) -> DataSmartResult<()> {
        let var = var.as_ref();
        let new_key = new_key.into();
        if var == new_key {
            // TODO warn?
            return Ok(());
        }

        let val = self
            .get_var_opt(var, GetVarOptions::default().expand(false).parsing(true))
            .with_context(|| format!("rename_var: {var}"))?;
        if let Some(val) = &val {
            // TODO var history
            println!("set {}", &new_key);
            self.set_var(new_key.clone(), val.clone(), true)?;
        } else {
            println!("no value for var {}", &var);
        }

        for i in &[APPEND_FLAG, PREPEND_FLAG, REMOVE_FLAG] {
            if let Some(keyword_val) = self
                .get_var_flag_contents(var, i, GetVarOptions::default().expand(false))?
                .map(|v| <Vec<VariableContents>>::try_from(v).unwrap())
            {
                let mut dest: Vec<VariableContents> = self
                    .get_var_flag_contents(
                        new_key.clone().as_str(),
                        i,
                        GetVarOptions::default().expand(false),
                    )?
                    .as_or_default();
                dest.extend(keyword_val);
                self.set_var_flag(new_key.clone(), i.to_string(), dest)?;
            }
        }

        let data = RefCell::borrow_mut(&self.per_var_override_data).remove(var);
        if let Some(data) = data {
            let mut new_data = vec![];
            for entry in &data {
                let renamed_key = entry.full_var.replace(var, &new_key.clone());
                new_data.push(VarAndOverrideTuple {
                    full_var: renamed_key.clone(),
                    override_str: entry.override_str.clone(),
                });
                self.rename_var(entry.full_var.clone(), renamed_key)?;
            }
        }

        if val.is_none() {
            self.setvar_update_overrides(new_key);
        }

        self.del_var(var);

        Ok(())
    }

    pub fn set_var_flag<T: Into<VariableContents>, K: Into<String>, F: Into<String>>(
        &self,
        var: K,
        flag: F,
        value: T,
    ) -> DataSmartResult<()> {
        let var = var.into();
        let flag = flag.into();
        let value = value.into();

        {
            let mut data = self.find_or_create_var(var.clone());
            data.insert(flag.clone(), value.clone());
        }

        match flag.as_str() {
            "_defaultval" => {
                self.setvar_update_overrides(var.clone());
                self.setvar_update_overridevars(var.as_str(), value.as_string())?;
            }
            "unexport" | "export" => {
                let mut data = self.find_or_create_var(EXPORT_LIST_ITEM);
                let set: &mut BTreeSet<VariableContents> = data
                    .entry(CONTENT_FLAG.to_string())
                    .or_insert(VariableContents::Set(BTreeSet::new()))
                    .try_into()
                    .unwrap();
                set.insert(var.into());
            }
            _ => {}
        }

        Ok(())
    }

    fn setvar_update_overrides<T: AsRef<str>>(&self, var: T) {
        RefCell::borrow_mut(&self.per_var_override_data).record_overrides(var);
    }

    fn setvar_update_overridevars<T: Into<String>, K: Into<String>>(
        &self,
        var: K,
        value: T,
    ) -> DataSmartResult<()> {
        let var = var.into();
        if !RefCell::borrow(&self.override_state).is_override_var(&var) {
            return Ok(());
        }

        let vardata = self.expand_with_refs(Some(value.into()), Some(var))?;
        let mut new_refs = vardata.references;
        new_refs.extend(vardata.contains.keys().cloned());

        while !new_refs.is_subset(&RefCell::borrow(&self.override_state).vars) {
            let mut nextnew = HashSet::new();
            RefCell::borrow_mut(&self.override_state)
                .vars
                .extend(nextnew.iter().cloned());
            for i in &new_refs {
                if let Some(v) = self.get_var_opt(i.as_str(), GetVarOptions::default())? {
                    let str: String = v.as_string();
                    let vardata = self.expand_with_refs(Some(str), Some(i))?;
                    nextnew.extend(vardata.references);
                    nextnew.extend(vardata.contains.keys().cloned())
                }
            }
            new_refs = nextnew;
        }

        RefCell::borrow_mut(&self.override_state).active_overrides = None;
        Ok(())
    }

    fn find_var<T: AsRef<str>>(&self, var: T) -> Option<Ref<DataSmartFlags>> {
        Ref::filter_map(RefCell::borrow(&self.data), |d| d.get(var.as_ref())).ok()
    }

    fn find_var_mut<T: AsRef<str>>(&self, var: T) -> Option<RefMut<DataSmartFlags>> {
        RefMut::filter_map(RefCell::borrow_mut(&self.data), |d| d.get_mut(var.as_ref())).ok()
    }

    fn find_or_create_var<T: AsRef<str>>(&self, var: T) -> RefMut<DataSmartFlags> {
        RefMut::map(RefCell::borrow_mut(&self.data), |d| {
            d.entry(var.as_ref().into()).or_default()
        })
    }

    pub fn del_var<T: AsRef<str>>(&self, var: T) {
        let var = var.as_ref();
        // TODO: need to actually track deleted vars. BitBake sets them to an empty dict.
        //  Also need to fix `keys()`
        RefCell::borrow_mut(&self.data).insert(var.to_string(), Default::default());
        RefCell::borrow_mut(&self.per_var_override_data).remove_overrides(var);
    }

    pub fn del_var_flag<V: AsRef<str>, F: AsRef<str>>(&self, var: V, flag: F) {
        if let Some(mut data) = self.find_var_mut(var.as_ref()) {
            data.remove(flag.as_ref());
        }
    }

    pub fn prepend_var<T: AsRef<str>, V: Into<VariableContents>>(
        &self,
        var: T,
        val: V,
    ) -> DataSmartResult<()> {
        let target = format!("{}_prepend", var.as_ref());
        self.set_var(target, val.into(), true)
    }

    pub fn append_var<T: AsRef<str>, V: Into<VariableContents>>(
        &self,
        var: T,
        val: V,
    ) -> DataSmartResult<()> {
        let target = format!("{}_append", var.as_ref());
        self.set_var(target, val.into(), true)
    }

    pub fn set_var<T: Into<VariableContents>, K: Into<String>>(
        &self,
        var: K,
        value: T,
        parsing: bool,
    ) -> DataSmartResult<()> {
        let var = var.into();
        let value = value.into();

        // TODO: some way to avoid the regex when we already know the capture groups (e.g. in the
        //  case of prepend_var/append_var).
        if let Some(regex_match) = SETVAR_REGEX.captures(&var) {
            let base = regex_match.name("base").unwrap().as_str();
            let keyword = regex_match.name("keyword").unwrap().as_str();
            let overridestr = regex_match.name("add").map(|o| o.as_str().to_string());

            let mut ret: Vec<VariableContents> = self
                .get_var_flag_contents(base, keyword, GetVarOptions::default().expand(false))?
                .map(|v| v.try_into().unwrap_or_default())
                .unwrap_or_default();

            let b = (
                Box::new(value.clone()),
                Box::new(VariableContents::from(
                    overridestr.map(|v| Box::new(VariableContents::from(v))),
                )),
            );
            ret.push(b.into());

            self.set_var_flag(base, keyword, ret)?;
            self.setvar_update_overrides(base);
            self.setvar_update_overridevars(var.clone(), value.as_string())?;
            return Ok(());
        }

        if !parsing {
            RefCell::borrow_mut(&self.data).get_mut(&var).map(|f| {
                f.remove(APPEND_FLAG);
                f.remove(PREPEND_FLAG);
                f.remove(REMOVE_FLAG);
            });

            let mut active = None;
            if let Some(v) = RefCell::borrow(&self.per_var_override_data).get(&var) {
                self.need_overrides()?;

                active = Some(vec![]);
                for entry in v.iter() {
                    let o = &entry.override_str;
                    if RefCell::borrow(&self.override_state).is_override_active(o.as_str()) {
                        active.as_mut().unwrap().push(entry.full_var.clone());
                    }
                }
            }

            if let Some(active) = active {
                for var in active {
                    self.del_var(var);
                }

                RefCell::borrow_mut(&self.per_var_override_data).remove(&var);
            }
        }

        self.setvar_update_overrides(&var);
        self.set_var_flag(var.clone(), CONTENT_FLAG.to_owned(), value.clone())?;
        self.setvar_update_overridevars(var, value.as_string())?;

        Ok(())
    }

    fn need_overrides(&self) -> DataSmartResult<()> {
        if let Ok(_) = RefCell::try_borrow_mut(&self.inside_need_overrides) {
            if RefCell::borrow(&self.override_state)
                .active_overrides
                .is_some()
            {
                return Ok(());
            }

            // The value of OVERRIDES itself can depend on any other value in the datastore, so we need
            // to iteratively recompute the OVERRIDES until they are stable.
            for _i in 0..5 {
                let s = self
                    .get_var_opt("OVERRIDES", GetVarOptions::default())
                    .with_context(|| "OVERRIDES")?
                    .split_filter_empty_collect::<IndexSet<String>>(":")
                    .into();
                RefCell::borrow_mut(&self.override_state).active_overrides = s;

                let new_set: Option<IndexSet<String>> = self
                    .get_var_opt("OVERRIDES", GetVarOptions::default())
                    .with_context(|| "OVERRIDES")?
                    .split_filter_empty_collect::<IndexSet<String>>(":")
                    .into();

                assert!(new_set.is_some());
                if RefCell::borrow(&self.override_state).active_overrides == new_set {
                    return Ok(());
                }

                RefCell::borrow_mut(&self.override_state).active_overrides = new_set;
            }

            panic!("HAHA")
        }

        Ok(())
    }
}

pub fn expand_keys(data: &DataSmart) -> DataSmartResult<()> {
    let mut todo_list = HashMap::new();
    for key in data.keys()?.iter().filter(|k| k.contains("${")) {
        let expanded_key = data.expand(key)?.unwrap();
        todo_list.insert(key.clone(), expanded_key);
    }

    let mut sorted_keys = todo_list.keys().collect::<Vec<_>>();
    sorted_keys.sort();

    for key in sorted_keys {
        let expanded_key = todo_list.get(key).unwrap();
        if let Some(_new_value) =
            data.get_var_opt(expanded_key, GetVarOptions::default().expand(false))?
        {
            if let Some(_val) = data.get_var_opt(key, GetVarOptions::default().expand(false))? {
                // TODO warn
                panic!("TODO warning");
            }
        }
        data.rename_var(key, expanded_key)?;
    }

    Ok(())
}
