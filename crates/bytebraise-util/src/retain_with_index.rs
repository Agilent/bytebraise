pub trait RetainWithIndex<T> {
    fn retain_with_index<F>(&mut self, f: F)
    where
        F: FnMut(&T, usize) -> bool;
}

impl<T> RetainWithIndex<T> for Vec<T> {
    fn retain_with_index<F>(&mut self, mut f: F)
    where
        F: FnMut(&T, usize) -> bool,
    {
        let mut i = 0;
        self.retain(|x| {
            let should_retain = f(x, i);
            i += 1; // Increment index on each iteration
            should_retain
        });
    }
}
