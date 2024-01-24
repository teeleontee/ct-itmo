#![forbid(unsafe_code)]

use std::{cell::RefCell, collections::VecDeque, fmt::Debug, rc::Rc};

use thiserror::Error;

#[derive(Error, Debug)]
#[error("channel is closed")]
pub struct SendError<T: Debug> {
    pub value: T,
}

#[derive(Debug)]
pub struct Sender<T> {
    buf: Rc<RefCell<VecDeque<T>>>,
    is_closed: Rc<RefCell<bool>>,
}

impl<T: Debug> Sender<T> {
    pub fn send(&self, value: T) -> Result<(), SendError<T>> {
        if self.is_closed() {
            return Err(SendError { value });
        }
        let mut buffer = self.buf.borrow_mut();
        buffer.push_back(value);
        Ok(())
    }

    pub fn is_closed(&self) -> bool {
        *self.is_closed.borrow()
    }

    pub fn same_channel(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.buf, &other.buf)
    }
}

impl<T> Clone for Sender<T> {
    fn clone(&self) -> Self {
        Sender {
            buf: Rc::clone(&self.buf),
            is_closed: Rc::clone(&self.is_closed),
        }
    }
}

#[derive(Error, Debug)]
pub enum ReceiveError {
    #[error("channel is empty")]
    Empty,
    #[error("channel is closed")]
    Closed,
}

pub struct Receiver<T> {
    buf: Rc<RefCell<VecDeque<T>>>,
    is_closed: Rc<RefCell<bool>>,
}

impl<T> Receiver<T> {
    pub fn recv(&mut self) -> Result<T, ReceiveError> {
        if Rc::strong_count(&self.buf) == 1 {
            return Err(ReceiveError::Closed);
        }
        let mut buffer = self.buf.borrow_mut();
        match buffer.pop_front() {
            None => {
                if *self.is_closed.borrow() {
                    Err(ReceiveError::Closed)
                } else {
                    Err(ReceiveError::Empty)
                }
            }
            Some(value) => Ok(value),
        }
    }

    pub fn close(&mut self) {
        *self.is_closed.borrow_mut() = true;
    }
}

impl<T> Drop for Receiver<T> {
    fn drop(&mut self) {
        self.close();
        println!("dropped")
    }
}

pub fn channel<T>() -> (Sender<T>, Receiver<T>) {
    let buffer: Rc<RefCell<VecDeque<T>>> = Rc::new(RefCell::new(VecDeque::new()));
    let is_closed = Rc::new(RefCell::new(false));
    let rcv = Receiver {
        buf: Rc::clone(&buffer),
        is_closed: Rc::clone(&is_closed),
    };
    let snd = Sender {
        buf: Rc::clone(&buffer),
        is_closed: Rc::clone(&is_closed),
    };
    (snd, rcv)
}
