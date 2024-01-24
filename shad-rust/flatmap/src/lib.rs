#![forbid(unsafe_code)]

use std::{borrow::Borrow, iter::FromIterator, ops::Index};

#[derive(Default, Debug, PartialEq, Eq)]
pub struct FlatMap<K, V>(Vec<(K, V)>);

impl<K: Ord, V> FlatMap<K, V> {
    pub fn new() -> Self {
        FlatMap(Vec::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.len() == 0
    }

    pub fn capacity(&self) -> usize {
        self.0.capacity()
    }

    pub fn as_slice(&self) -> &[(K, V)] {
        self.0.as_slice()
    }

    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        match self.0.binary_search_by(|pair| pair.0.cmp(&key)) {
            Err(index) => {
                self.0.insert(index, (key, value));
                None
            }
            Ok(index) => Some(std::mem::replace(&mut self.0[index].1, value)),
        }
    }

    pub fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Ord + ?Sized,
    {
        match self.0.binary_search_by(|pair| pair.0.borrow().cmp(key)) {
            Err(_) => None,
            Ok(index) => Some(&self.0[index].1),
        }
    }

    pub fn remove<Q>(&mut self, key: &Q) -> Option<V>
    where
        K: Borrow<Q>,
        Q: Ord + ?Sized,
    {
        match self.0.binary_search_by(|pair| pair.0.borrow().cmp(key)) {
            Err(_) => None,
            Ok(index) => Some(self.0.remove(index).1),
        }
    }

    pub fn remove_entry<Q>(&mut self, key: &Q) -> Option<(K, V)>
    where
        K: Borrow<Q>,
        Q: Ord + ?Sized,
    {
        match self.0.binary_search_by(|pair| pair.0.borrow().cmp(key)) {
            Err(_) => None,
            Ok(index) => {
                let tmp = self.0.remove(index);
                Some((tmp.0, tmp.1))
            }
        }
    }
}

impl<K: Ord, V, Q> Index<&Q> for FlatMap<K, V>
where
    K: Borrow<Q>,
    Q: Ord + ?Sized,
{
    type Output = V;

    fn index(&self, key: &Q) -> &Self::Output {
        match self.get(key) {
            None => panic!("no key"),
            Some(val) => val,
        }
    }
}

impl<K: Ord, V> Extend<(K, V)> for FlatMap<K, V> {
    fn extend<T: IntoIterator<Item = (K, V)>>(&mut self, iter: T) {
        for elem in iter {
            self.insert(elem.0, elem.1);
        }
    }
}

impl<K: Ord, V> From<Vec<(K, V)>> for FlatMap<K, V> {
    fn from(mut value: Vec<(K, V)>) -> Self {
        value.reverse();
        value.dedup_by(|a, b| a.0.eq(&b.0));
        value.sort_by(|a, b| a.0.cmp(&b.0));
        FlatMap(value)
    }
}

impl<K: Ord, V> From<FlatMap<K, V>> for Vec<(K, V)> {
    fn from(value: FlatMap<K, V>) -> Self {
        value.0
    }
}

impl<K: Ord, V> FromIterator<(K, V)> for FlatMap<K, V> {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        let mut f_map = FlatMap::new();
        for elem in iter {
            f_map.insert(elem.0, elem.1);
        }
        f_map
    }
}

impl<K, V> IntoIterator for FlatMap<K, V> {
    type Item = (K, V);
    type IntoIter = std::vec::IntoIter<Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
