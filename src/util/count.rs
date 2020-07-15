use rustc_hash::FxHashMap;
use std::collections::VecDeque;
use std::hash::Hash;

#[derive(Clone, Debug)]
pub struct Count<T: Eq + Hash>(FxHashMap<T, usize>);

#[derive(Debug, Clone)]
pub struct CountIter<'a, T: Eq + Hash> {
    values: Vec<&'a T>,
    nth: usize,
}

#[derive(Debug, Clone)]
pub struct CountIntoIter<T: Eq + Hash> {
    values: VecDeque<T>,
}

impl<T: Eq + Hash> Count<T> {
    pub fn new() -> Self {
        Self(FxHashMap::default())
    }

    pub fn add(&mut self, t: T) {
        *self.0.entry(t).or_insert(0) += 1;
    }

    pub fn remove(&mut self, t: &T) {
        if !self.0.contains_key(&t) {
            return;
        }

        let c = self.0.get_mut(t).unwrap();
        *c = c.saturating_sub(1);

        if *c == 0 {
            self.0.remove(t);
        }
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn iter(&self) -> CountIter<T> {
        CountIter {
            values: self.0.iter().map(|(k, _)| k).collect(),
            nth: 0,
        }
    }

    pub fn into_iter2(self) -> CountIntoIter<T> {
        CountIntoIter {
            values: self.0.into_iter().map(|(k, _)| k).collect(),
        }
    }
}

impl<'a, T: Eq + Hash> Iterator for CountIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let v = *self.values.get(self.nth)?;
        self.nth += 1;
        Some(v)
    }
}

impl<T: Eq + Hash> Iterator for CountIntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.values.pop_front()
    }
}

impl<T: Eq + Hash> IntoIterator for Count<T> {
    type Item = T;
    type IntoIter = CountIntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.into_iter2()
    }
}

impl<'a, T: Eq + Hash> IntoIterator for &'a Count<T> {
    type Item = &'a T;
    type IntoIter = CountIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
