use std::str;
use std::iter::{Enumerate, Rev};
use std::str::Chars;
use std::borrow::Cow;
use regex::{Captures, Regex};

pub fn split_filter_empty<'a>(input: &'a str, separator: &'a str) -> impl Iterator<Item = &'a str> {
    input.split(separator).filter(|v| !v.is_empty())
}

pub trait RSplitAll
where
    Self: AsRef<str>,
{
    /// Returns an iterator that produces all possible splits at the given character, moving from
    /// right to left in the input.
    ///
    /// # Example
    ///
    /// ```
    /// use bytebraise_util::split::RSplitAll;
    /// let input = "VAR_foo_bar_local";
    /// let mut iter = input.rsplit_all('_');
    /// assert_eq!(iter.next(), Some(("VAR_foo_bar", "local")));
    /// assert_eq!(iter.next(), Some(("VAR_foo", "bar_local")));
    /// assert_eq!(iter.next(), Some(("VAR", "foo_bar_local")));
    /// assert_eq!(iter.next(), None);
    /// ```
    fn rsplit_all(&self, c: char) -> RSplit {
        RSplit::new(self.as_ref(), c)
    }
}

pub struct RSplit<'a> {
    str: &'a str,
    c: char,
    iter: Enumerate<Rev<Chars<'a>>>,
}

impl<'a> RSplit<'a> {
    pub fn new(str: &'a str, c: char) -> Self {
        Self {
            str,
            c,
            iter: str.chars().rev().enumerate(),
        }
    }
}

impl<'a> Iterator for RSplit<'a> {
    type Item = (&'a str, &'a str);

    fn next(&mut self) -> Option<Self::Item> {
        let search = self.c;
        self.iter.find(|(_, c)| *c == search).map(|(i, _)| {
            // Fixup the index since Enumerate is operating on a Rev adaptor
            let i = self.str.len() - i;
            (&self.str[..i - 1], &self.str[i..])
        })
    }
}

impl RSplitAll for &str {}

// https://stackoverflow.com/a/56923739
pub fn split_keep<'a>(r: &Regex, text: &'a str) -> Vec<&'a str> {
    let mut result = Vec::new();
    let mut last = 0;
    for (index, matched) in text.match_indices(r) {
        if last != index {
            result.push(&text[last..index]);
        }
        result.push(matched);
        last = index + matched.len();
    }
    if last < text.len() {
        result.push(&text[last..]);
    }
    result
}

pub trait ReplaceFallible {
    fn replace_fallible<'t, F, E>(&self, text: &'t str, rep: F) -> Result<Cow<'t, str>, E>
    where
        F: FnMut(&Captures) -> Result<String, E>;
}

impl ReplaceFallible for Regex {
    fn replace_fallible<'t, F, E>(&self, text: &'t str, mut rep: F) -> Result<Cow<'t, str>, E>
    where
        F: FnMut(&Captures) -> Result<String, E>,
    {
        let mut error = None;

        let ret = self.replace(text, |caps: &Captures| match rep(caps) {
            Err(e) => {
                error = Some(e);
                Cow::Borrowed("")
            }
            Ok(v) => Cow::Owned(v),
        });

        match error {
            Some(e) => Err(e),
            None => Ok(ret),
        }
    }
}

// From https://docs.rs/regex/latest/regex/struct.Regex.html#method.replace_all
pub fn replace_all<E>(
    re: &Regex,
    haystack: &str,
    replacement: impl Fn(&Captures) -> Result<String, E>,
) -> Result<String, E> {
    let mut new = String::with_capacity(haystack.len());
    let mut last_match = 0;
    for caps in re.captures_iter(haystack) {
        let m = caps.get(0).unwrap();
        new.push_str(&haystack[last_match..m.start()]);
        new.push_str(&replacement(&caps)?);
        last_match = m.end();
    }
    new.push_str(&haystack[last_match..]);
    Ok(new)
}