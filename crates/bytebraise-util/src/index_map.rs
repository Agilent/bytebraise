use indexmap::map::IndexMap;
use itertools::Itertools;
use std::hash::Hash;

/// Return an `IndexMap` of keys mapped to a list of their corresponding values.
///
/// Code based on [`.into_group_map()`](Itertools::into_group_map)
pub fn into_index_map<I, K, V>(iter: I) -> IndexMap<K, Vec<V>>
where
    I: Iterator<Item = (K, V)>,
    K: Hash + Eq,
{
    let mut lookup = IndexMap::new();

    iter.for_each(|(key, val)| {
        lookup.entry(key).or_insert_with(Vec::new).push(val);
    });

    lookup
}

pub fn into_index_map_by<I, K, V>(iter: I, f: impl Fn(&V) -> K) -> IndexMap<K, Vec<V>>
where
    I: Iterator<Item = V>,
    K: Hash + Eq,
{
    into_index_map(iter.map(|v| (f(&v), v)))
}

pub trait IntoIndexMap: Iterator {
    fn into_index_map_by<K, V, F>(self, f: F) -> IndexMap<K, Vec<V>>
    where
        Self: Iterator<Item = V> + Sized,
        K: Hash + Eq,
        F: Fn(&V) -> K,
    {
        into_index_map_by(self, f)
    }
}

impl<T> IntoIndexMap for T where T: Iterator + ?Sized {}
