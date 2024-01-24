#![forbid(unsafe_code)]

pub use gc_derive::Scan;

use std::{
    cell::RefCell,
    //cell::RefCell,
    collections::{HashMap, HashSet},
    marker::PhantomData,
    ops::Deref,
    rc::{Rc, Weak},
};

////////////////////////////////////////////////////////////////////////////////

pub struct Gc<T> {
    weak: Weak<T>,
}

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Self {
            weak: self.weak.clone(),
        }
    }
}

impl<T> Gc<T> {
    pub fn borrow(&self) -> GcRef<'_, T> {
        GcRef {
            rc: self.weak.upgrade().unwrap(),
            lifetime: PhantomData::<&'_ Gc<T>>,
        }
    }
}

pub struct GcRef<'a, T> {
    rc: Rc<T>,
    lifetime: PhantomData<&'a Gc<T>>,
}

impl<'a, T> Deref for GcRef<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.rc
    }
}

////////////////////////////////////////////////////////////////////////////////

pub trait Scan {
    fn get_pointers(&self) -> Vec<usize>;
}

impl<T: Scan + 'static> Scan for Gc<T> {
    fn get_pointers(&self) -> Vec<usize> {
        vec![self.weak.as_ptr() as usize]
    }
}

impl<T: Scan + 'static> Scan for Vec<T> {
    fn get_pointers(&self) -> Vec<usize> {
        let mut ans = vec![];
        for el in self.iter() {
            ans.append(&mut el.get_pointers())
        }
        ans
    }
}

impl<T: Scan + 'static> Scan for Option<T> {
    fn get_pointers(&self) -> Vec<usize> {
        match self {
            None => Vec::new(),
            Some(val) => val.get_pointers(),
        }
    }
}

impl<T: Scan + 'static> Scan for RefCell<T> {
    fn get_pointers(&self) -> Vec<usize> {
        self.borrow().get_pointers()
    }
}

impl Scan for i32 {
    fn get_pointers(&self) -> Vec<usize> {
        Vec::new()
    }
}

////////////////////////////////////////////////////////////////////////////////

#[derive(Default)]
pub struct Arena {
    allocations: HashMap<usize, Rc<dyn Scan>>,
}

impl Arena {
    pub fn new() -> Self {
        Arena {
            allocations: HashMap::new(),
        }
    }

    pub fn allocation_count(&self) -> usize {
        self.allocations.len()
    }

    pub fn alloc<T: Scan + 'static>(&mut self, obj: T) -> Gc<T> {
        let new_el = Rc::new(obj);
        let weak = Rc::downgrade(&new_el);
        self.allocations
            .insert(Rc::as_ptr(&new_el) as usize, new_el);
        Gc { weak }
    }

    pub fn sweep(&mut self) {
        let actual_weak_count = self.get_weak_count();
        let internal_weak_count = self.get_internal_count();

        let mut marked = HashSet::new();
        for (addr, _) in self.allocations.iter() {
            let actual_cnt = actual_weak_count.get(addr).unwrap();
            let inner_cnt = internal_weak_count.get(addr).unwrap();
            if actual_cnt > inner_cnt {
                marked.insert(*addr);
            }
        }

        let mut visited = HashSet::new();
        for node in marked.iter() {
            self.dfs(*node, &mut visited);
        }

        let to_delete = self.get_hanging(&visited);
        for addr in to_delete.iter() {
            self.allocations.remove(addr);
        }
    }

    fn get_internal_count(&self) -> HashMap<usize, usize> {
        let mut internal_weak_count = HashMap::new();
        for (addr, _) in self.allocations.iter() {
            internal_weak_count.insert(*addr, 0);
        }
        for (_, rc) in self.allocations.iter() {
            let internal_gcs = rc.get_pointers();
            for addr in internal_gcs.iter() {
                *internal_weak_count.get_mut(addr).unwrap() += 1;
            }
        }
        internal_weak_count
    }

    fn get_weak_count(&self) -> HashMap<usize, usize> {
        let mut actual_weak_count = HashMap::new();
        for (addr, rc) in self.allocations.iter() {
            actual_weak_count.insert(*addr, Rc::weak_count(rc));
        }
        actual_weak_count
    }

    fn get_hanging(&self, visited: &HashSet<usize>) -> HashSet<usize> {
        let mut to_delete = HashSet::new();
        for (addr, _) in self.allocations.iter() {
            if !visited.contains(addr) {
                to_delete.insert(*addr);
            }
        }
        to_delete
    }

    fn dfs(&self, addr: usize, visited: &mut HashSet<usize>) {
        if visited.contains(&addr) {
            return;
        }
        visited.insert(addr);
        let pointers = self.allocations.get(&addr).unwrap().get_pointers();
        for node in pointers.iter() {
            self.dfs(*node, visited);
        }
    }
}

