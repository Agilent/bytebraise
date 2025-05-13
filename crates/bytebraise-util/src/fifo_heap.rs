use std::collections::BTreeSet;

#[derive(Clone, Debug)]
pub struct FifoHeap<T> {
    seq: usize,
    pub heap: BTreeSet<(T, usize)>,
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
