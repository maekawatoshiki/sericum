use rustc_hash::FxHashSet;
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};

#[derive(Debug, Clone)]
pub struct Raw<T>(*mut T);

impl<T: Clone> Copy for Raw<T> {}

impl<T> PartialEq for Raw<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 as u64 == other.0 as u64
    }
}

impl<T> Hash for Raw<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.0 as u64).hash(state)
    }
}

impl<T> Eq for Raw<T> {}

pub struct RawAllocator<T> {
    allocated: FxHashSet<Raw<T>>,
}

impl<T> RawAllocator<T> {
    pub fn new() -> Self {
        RawAllocator {
            allocated: FxHashSet::default(),
        }
    }

    pub fn alloc(&mut self, val: T) -> Raw<T> {
        let raw = Box::into_raw(Box::new(val));
        self.allocated.insert(Raw(raw));
        Raw(raw)
    }

    pub fn allocated_ref(&self) -> &FxHashSet<Raw<T>> {
        &self.allocated
    }
}

impl<T> Drop for RawAllocator<T> {
    fn drop(&mut self) {
        for raw in &self.allocated {
            unsafe {
                Box::from_raw(raw.inner());
            }
        }
    }
}

impl<T> Raw<T> {
    pub fn inner(&self) -> *mut T {
        self.0
    }

    pub fn inner_ref(&self) -> &T {
        unsafe { &*self.0 }
    }

    pub fn inner_ref_mut(&self) -> &mut T {
        unsafe { &mut *self.0 }
    }
}

impl<T> Deref for Raw<T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { &*self.inner() }
    }
}

impl<T> DerefMut for Raw<T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { &mut *self.inner() }
    }
}
