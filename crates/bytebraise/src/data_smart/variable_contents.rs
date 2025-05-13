use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::convert::{TryFrom, TryInto};
use std::fmt::Debug;
use std::iter::FromIterator;
use std::path::{Path, PathBuf};

use derive_more::{From, TryInto};
use lazy_static::lazy_static;
use bytebraise_util::split::split_filter_empty;

// TODO box more?
#[derive(Clone, Debug, PartialEq, Eq, From, Hash, Ord, PartialOrd, TryInto)]
#[try_into(owned, ref, ref_mut)]
pub enum VariableContents {
    String(String),
    Bool(bool),
    Option(Option<Box<VariableContents>>),
    #[try_into(ignore)]
    Map(BTreeMap<String, VariableContents>),
    #[try_into(ignore)]
    Set(BTreeSet<VariableContents>),
    #[try_into(ignore)]
    Vec(Vec<VariableContents>),
    Tuple((Box<VariableContents>, Box<VariableContents>)),
}

lazy_static! {
    pub static ref EMPTY_STRING: String = String::new();
}

pub trait VariableContentsAccessors
where
    Self: Sized,
{
    fn as_or_default<T>(self) -> T
    where
        T: Default + TryFrom<VariableContents>,
    {
        self.as_or_none().unwrap_or_default()
    }

    fn as_or_none<T>(self) -> Option<T>
    where
        T: TryFrom<VariableContents>;

    fn as_string_or_empty(self) -> String {
        self.as_or_default()
    }

    fn as_string(self) -> String {
        self.as_or_none().unwrap()
    }

    fn as_string_or_empty_2(&self) -> &String;

    /// TODO document
    fn split_filter_empty_collect<T>(&self, pattern: &str) -> T
    where
        T: FromIterator<String> + Default;
}

impl VariableContentsAccessors for &VariableContents {
    fn as_or_none<T>(self) -> Option<T>
    where
        T: TryFrom<VariableContents>,
    {
        T::try_from(self.clone()).ok()
    }

    fn as_string_or_empty_2(&self) -> &String {
        match self {
            VariableContents::String(s) => s,
            _ => &EMPTY_STRING,
        }
    }

    fn split_filter_empty_collect<T>(&self, pattern: &str) -> T
    where
        T: FromIterator<String> + Default,
    {
        (*self).clone().split_filter_empty_collect(pattern)
    }
}

impl VariableContentsAccessors for VariableContents {
    fn as_or_none<T>(self) -> Option<T>
    where
        T: TryFrom<VariableContents>,
    {
        T::try_from(self).ok()
    }

    fn as_string_or_empty_2(&self) -> &String {
        match self {
            VariableContents::String(s) => s,
            _ => &EMPTY_STRING,
        }
    }

    fn split_filter_empty_collect<T>(&self, pattern: &str) -> T
    where
        T: FromIterator<String> + Default,
    {
        <&String>::try_from(self)
            .map(|s| {
                split_filter_empty(s.as_str(), pattern)
                    .map(|v| v.to_string())
                    .collect()
            })
            .unwrap_or_default()
    }
}

impl VariableContentsAccessors for Option<VariableContents> {
    fn as_or_none<T>(self) -> Option<T>
    where
        T: TryFrom<VariableContents>,
    {
        self.and_then(VariableContentsAccessors::as_or_none)
    }

    fn as_string_or_empty_2(&self) -> &String {
        match self.as_ref() {
            Some(VariableContents::String(s)) => s,
            _ => &EMPTY_STRING,
        }
    }

    fn split_filter_empty_collect<T>(&self, pattern: &str) -> T
    where
        T: FromIterator<String> + Default,
    {
        self.as_ref()
            .map(|v| v.split_filter_empty_collect(pattern))
            .unwrap_or_default()
    }
}

impl From<&str> for VariableContents {
    fn from(value: &str) -> Self {
        VariableContents::String(value.to_string())
    }
}

impl<T> From<Vec<T>> for VariableContents
where
    VariableContents: From<T>,
{
    default fn from(value: Vec<T>) -> Self {
        value
            .into_iter()
            .map(|v| VariableContents::from(v))
            .collect::<Vec<_>>()
            .into()
    }
}

impl<T> From<HashMap<String, T>> for VariableContents
where
    VariableContents: From<T>,
{
    default fn from(value: HashMap<String, T>) -> Self {
        value
            .into_iter()
            .map(|(k, v)| (k, v.into()))
            .collect::<BTreeMap<String, _>>()
            .into()
    }
}

impl<T> From<BTreeMap<String, T>> for VariableContents
where
    VariableContents: From<T>,
{
    default fn from(value: BTreeMap<String, T>) -> Self {
        value
            .into_iter()
            .map(|(k, v)| (k, v.into()))
            .collect::<BTreeMap<String, _>>()
            .into()
    }
}

impl<T> From<BTreeSet<T>> for VariableContents
where
    VariableContents: From<T>,
{
    default fn from(value: BTreeSet<T>) -> Self {
        value
            .into_iter()
            .map(|v| v.into())
            .collect::<BTreeSet<_>>()
            .into()
    }
}

impl<T> TryFrom<VariableContents> for BTreeSet<T>
where
    T: TryFrom<VariableContents> + Ord,
    <T as TryFrom<VariableContents>>::Error: Debug,
{
    type Error = &'static str;

    fn try_from(value: VariableContents) -> Result<Self, Self::Error> {
        match value {
            VariableContents::Set(set) => Ok(set
                .into_iter()
                .map(|v| v.try_into().unwrap())
                .collect::<BTreeSet<T>>()),
            _ => Err("wrong data type"),
        }
    }
}

impl<'a> TryFrom<&'a VariableContents> for &'a BTreeSet<VariableContents> {
    type Error = &'static str;

    fn try_from(value: &'a VariableContents) -> Result<Self, Self::Error> {
        match value {
            VariableContents::Set(set) => Ok(set),
            _ => Err("wrong data type"),
        }
    }
}

impl<'a> TryFrom<&'a mut VariableContents> for &'a mut BTreeSet<VariableContents> {
    type Error = &'static str;

    fn try_from(value: &'a mut VariableContents) -> Result<Self, Self::Error> {
        match value {
            VariableContents::Set(set) => Ok(set),
            _ => Err("wrong data type"),
        }
    }
}

impl<T> TryFrom<VariableContents> for BTreeMap<String, T>
where
    T: TryFrom<VariableContents>,
    <T as TryFrom<VariableContents>>::Error: Debug,
{
    type Error = &'static str;

    fn try_from(value: VariableContents) -> Result<Self, Self::Error> {
        match value {
            VariableContents::Map(map) => Ok(map
                .into_iter()
                .map(|(k, v)| (k, v.try_into().unwrap()))
                .collect::<BTreeMap<String, T>>()),
            _ => Err("wrong data type"),
        }
    }
}

impl<'a> TryFrom<&'a VariableContents> for &'a BTreeMap<String, VariableContents> {
    type Error = &'static str;

    fn try_from(value: &'a VariableContents) -> Result<Self, Self::Error> {
        match value {
            VariableContents::Map(map) => Ok(map),
            _ => Err("wrong data type"),
        }
    }
}

impl<'a> TryFrom<&'a mut VariableContents> for &'a mut BTreeMap<String, VariableContents> {
    type Error = &'static str;

    fn try_from(value: &'a mut VariableContents) -> Result<Self, Self::Error> {
        match value {
            VariableContents::Map(map) => Ok(map),
            _ => Err("wrong data type"),
        }
    }
}

impl<T> TryFrom<VariableContents> for Vec<T>
where
    T: TryFrom<VariableContents>,
    <T as TryFrom<VariableContents>>::Error: Debug,
{
    type Error = &'static str;

    #[inline]
    fn try_from(value: VariableContents) -> Result<Self, Self::Error> {
        match value {
            VariableContents::Vec(vec) => Ok(vec
                .into_iter()
                .map(|v| v.try_into().unwrap())
                .collect::<Vec<T>>()),
            _ => Err("wrong data type"),
        }
    }
}

impl<'a> TryFrom<&'a VariableContents> for &'a Vec<VariableContents> {
    type Error = &'static str;

    fn try_from(value: &'a VariableContents) -> Result<Self, Self::Error> {
        match value {
            VariableContents::Vec(vec) => Ok(vec),
            _ => Err("wrong data type"),
        }
    }
}

impl<'a> TryFrom<&'a mut VariableContents> for &'a mut Vec<VariableContents> {
    type Error = &'static str;

    fn try_from(value: &'a mut VariableContents) -> Result<Self, Self::Error> {
        match value {
            VariableContents::Vec(vec) => Ok(vec),
            _ => Err("wrong data type"),
        }
    }
}

impl From<PathBuf> for VariableContents {
    fn from(value: PathBuf) -> Self {
        VariableContents::from(value.to_str().unwrap())
    }
}

impl From<&Path> for VariableContents {
    fn from(value: &Path) -> Self {
        VariableContents::from(value.to_str().unwrap())
    }
}

impl PartialEq<&str> for VariableContents {
    fn eq(&self, other: &&str) -> bool {
        match self {
            VariableContents::String(str) => str == other,
            _ => false,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_try_extract_none() {
        let _none: Option<VariableContents> = None;
        //assert_eq!(none.try_extract::<String>(), None);

        //assert_eq!(none.try_convert::<String>(), None);
    }

    #[test]
    fn nested_stuff() {
        let a = String::from("data");
        let b = String::from("q");
        let c = vec![a, b];

        //let mut m: HashMap<String, Vec<String>> = HashMap::new();
        //m.insert(String::from("test"), c);

        let mut p: BTreeMap<String, BTreeSet<Vec<String>>> = BTreeMap::new();
        let mut set = BTreeSet::new();
        set.insert(c);
        p.insert(String::from("test2"), set);

        //let mut s: BTreeSet<_> = BTreeSet::new();
        //s.insert(p);

        let v: VariableContents = p.into();

        let q: BTreeMap<String, BTreeSet<Vec<String>>> = v.try_into().unwrap();

        let v: VariableContents = q.into();

        let _q: &BTreeMap<String, VariableContents> = (&v).try_into().unwrap();

        // for val in q.iter() {
        //     let p: &BTreeSet<VariableContents> = val.1.try_into().unwrap();
        //
        //     println!("{:#?}", p);
        // }

        //panic!("{:#?}", q);

        //let qq = q.unwrap();
        //panic!("{:#?}", qq);
    }

    #[test]
    fn try_extract_string() {
        let v = VariableContents::from("test");
        let _s: String = v.try_into().unwrap();

        //assert_eq!(s, Some("test".into()));
    }

    #[test]
    fn get_mut_string() {
        let _v = VariableContents::from("test");
        // if let Some(f) = v.try_get_mut::<String>() {
        //     f.push_str("OK");
        // }
        //
        // assert_eq!(v.convert::<String>(), "testOK".to_owned());
    }
}
