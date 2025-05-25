use std::borrow::Borrow;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone, Debug)]
pub struct FifoHeap<T> {
    seq: usize,
    heap: BTreeSet<(T, usize)>,
}

impl<T: Ord> Default for FifoHeap<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Ord> FifoHeap<T> {
    pub fn new() -> Self {
        FifoHeap {
            seq: usize::MIN,
            heap: BTreeSet::new(),
        }
    }

    pub fn push(&mut self, val: T) {
        let seq = self.seq.checked_add(1).unwrap();
        self.seq = seq;
        self.heap.insert((val, seq));
    }

    pub fn len(&self) -> usize {
        self.heap.len()
    }

    pub fn is_empty(&self) -> bool {
        self.heap.is_empty()
    }

    pub fn first(&self) -> Option<&T> {
        self.heap.first().map(|v| &v.0)
    }

    pub fn retain<F>(&mut self, mut f: F)
    where
        T: Ord,
        F: FnMut(&T) -> bool,
    {
        self.heap.retain(|v| f(&v.0));
    }
}

impl<T: Ord> FifoHeap<T> {
    pub fn remove(&mut self, value: &T) {
        // TODO: more efficient
        self.heap.retain(|(a, b)| a != value)
    }
}

impl<T: Ord> FromIterator<T> for FifoHeap<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut ret = FifoHeap::new();
        for i in iter {
            ret.push(i);
        }
        ret
    }
}

pub struct IntoIter<T> {
    inner: <BTreeSet<(T, usize)> as IntoIterator>::IntoIter,
}

impl<T> IntoIterator for FifoHeap<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            inner: self.heap.into_iter(),
        }
    }
}

impl<T> Iterator for IntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|v| v.0)
    }
}

pub struct Iter<'a, T: 'a> {
    inner: std::collections::btree_set::Iter<'a, (T, usize)>,
}

impl<T> FifoHeap<T> {
    pub fn iter(&self) -> Iter<T> {
        Iter {
            inner: self.heap.iter(),
        }
    }
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|v| &v.0)
    }
}
