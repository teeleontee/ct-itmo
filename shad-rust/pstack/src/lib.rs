#![forbid(unsafe_code)]

use std::rc::Rc;

pub struct PRef<T> {
    element: Rc<T>,
}

impl<T> std::ops::Deref for PRef<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.element
    }
}

pub struct PStack<T> {
    len: usize,
    node: Node<T>,
}

enum Node<T> {
    Empty,
    Cons(PRef<T>, Rc<Node<T>>),
}

impl<T> Default for PStack<T> {
    fn default() -> Self {
        PStack::new()
    }
}

impl<T> Clone for PStack<T> {
    fn clone(&self) -> Self {
        PStack {
            len: self.len,
            node: self.node.clone(),
        }
    }
}

impl<T> Clone for Node<T> {
    fn clone(&self) -> Self {
        match self {
            Node::Empty => Node::Empty,
            Node::Cons(val, ptr) => Node::Cons(
                PRef {
                    element: Rc::clone(&val.element),
                },
                Rc::clone(ptr),
            ),
        }
    }
}

impl<T> PStack<T> {
    pub fn new() -> Self {
        PStack {
            len: 0,
            node: Node::Empty,
        }
    }

    pub fn push(&self, value: T) -> Self {
        match self.is_empty() {
            true => PStack {
                len: 1,
                node: Node::Cons(
                    PRef {
                        element: Rc::new(value),
                    },
                    Rc::new(Node::Empty),
                ),
            },
            false => PStack {
                len: self.len + 1,
                node: Node::Cons(
                    PRef {
                        element: Rc::new(value),
                    },
                    Rc::new(self.node.clone()),
                ),
            },
        }
    }

    pub fn pop(&self) -> Option<(PRef<T>, Self)> {
        match &self.node {
            Node::Empty => None,
            Node::Cons(val, ptr) => {
                let s = PStack {
                    len: self.len - 1,
                    node: ptr.as_ref().clone(),
                };
                let ptr2 = PRef {
                    element: Rc::clone(&val.element),
                };
                Some((ptr2, s))
            }
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn iter(&self) -> impl Iterator<Item = PRef<T>> {
        let mut v = Vec::new();
        let mut cur_el = self.node.clone();
        loop {
            match cur_el {
                Node::Empty => break,
                Node::Cons(val, ptr) => {
                    v.push(val);
                    cur_el = ptr.as_ref().clone();
                }
            }
        }
        v.into_iter()
    }
}
