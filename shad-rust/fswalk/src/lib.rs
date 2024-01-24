#![forbid(unsafe_code)]

use std::{
    fs::{self, File},
    io::{self, Error, Read},
    path::Path,
};

////////////////////////////////////////////////////////////////////////////////

type Callback<'a> = dyn FnMut(&mut Handle) + 'a;

#[derive(Default)]
pub struct Walker<'a> {
    callbacks: Vec<Box<Callback<'a>>>,
}

impl<'a> Walker<'a> {
    pub fn new() -> Self {
        Walker {
            callbacks: Vec::new(),
        }
    }

    pub fn add_callback<F>(&mut self, callback: F)
    where
        F: FnMut(&mut Handle) + 'a,
    {
        self.callbacks.push(Box::new(callback));
    }

    pub fn walk<P: AsRef<Path>>(&mut self, path: P) -> io::Result<()> {
        if self.callbacks.is_empty() {
            return Ok(());
        }
        let path = path.as_ref();
        if path.is_dir() {
            let mut vec_tmp = self
                .callbacks
                .iter_mut()
                .map(|cb| cb.as_mut())
                .collect::<Vec<_>>();
            return Self::recursive_walk(path, &mut vec_tmp[..]);
        }
        Err(Error::new(io::ErrorKind::Unsupported, "expected directory"))
    }

    #[rustfmt::skip]
    fn path_to_handle(path: &Path) -> Option<Handle> {
        match path {
            _ if path.is_dir()  => Some(Handle::Dir(DirHandle::new(path))),
            _ if path.is_file() => Some(Handle::File(FileHandle::new(path))),
            _                   => None
        }
    }

    #[rustfmt::skip]
    fn check_if_visited(handle: &mut Handle) -> bool {
        match handle {
            Handle::Dir(directory) => directory.checked,
            Handle::File(file)     => file.checked,
            Handle::Content { .. } => false,
        }
    }

    #[rustfmt::skip]
    fn reset_handle(handle: &mut Handle) {
        match handle {
            Handle::Dir(directory) => directory.checked = false,
            Handle::File(file)     => file.checked = false,
            _                      => { },
        }

    }

    fn recursive_walk(path_to_dir: &Path, callbacks: &mut [&mut Callback]) -> io::Result<()> {
        for entry in fs::read_dir(path_to_dir)? {
            let entry = entry?;
            let path = entry.path();
            let handle_opt = Self::path_to_handle(&path);
            if handle_opt.is_none() {
                continue;
            }
            let mut handle = handle_opt.unwrap();
            let mut applied_cbs = 0;
            for i in 0..callbacks.len() {
                callbacks[i](&mut handle);
                if Self::check_if_visited(&mut handle) {
                    callbacks.swap(applied_cbs, i);
                    Self::reset_handle(&mut handle);
                    applied_cbs += 1;
                }
            }
            match handle {
                Handle::Dir(directory) => {
                    let _ = Self::recursive_walk(directory.path(), &mut callbacks[0..applied_cbs]);
                }
                Handle::File(file) => {
                    let content = get_file_as_bytes(file.path());
                    let mut content = Handle::Content {
                        file_path: file.path(),
                        content: &content,
                    };
                    for cb in callbacks.iter_mut().take(applied_cbs) {
                        cb(&mut content)
                    }
                }
                _ => {}
            };
        }
        Ok(())
    }
}

fn get_file_as_bytes(filename: &Path) -> Vec<u8> {
    let mut f = File::open(filename).expect("bad file");
    let mut buf = Vec::new();
    let _ = f.read_to_end(&mut buf);
    buf
}

////////////////////////////////////////////////////////////////////////////////

pub enum Handle<'a> {
    Dir(DirHandle<'a>),
    File(FileHandle<'a>),
    Content {
        file_path: &'a Path,
        content: &'a [u8],
    },
}

pub struct DirHandle<'a> {
    path: &'a Path,
    checked: bool,
}

impl<'a> DirHandle<'a> {
    pub fn new(path: &'a Path) -> Self {
        DirHandle {
            path,
            checked: false,
        }
    }

    pub fn descend(&mut self) {
        self.checked = true;
    }

    pub fn path(&self) -> &Path {
        self.path
    }
}

pub struct FileHandle<'a> {
    path: &'a Path,
    checked: bool,
}

impl<'a> FileHandle<'a> {
    pub fn new(path: &'a Path) -> Self {
        FileHandle {
            path,
            checked: false,
        }
    }

    pub fn read(&mut self) {
        self.checked = true;
    }

    pub fn path(&self) -> &Path {
        self.path
    }
}
