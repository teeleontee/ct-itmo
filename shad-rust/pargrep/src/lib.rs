#![forbid(unsafe_code)]

use std::{
    fs::{self, File},
    io::{self, BufRead, BufReader},
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
    thread,
};

// use rayon::prelude::*;

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, PartialEq, Eq)]
pub struct Match {
    pub path: PathBuf,
    pub line: String,
    pub line_number: usize,
}

#[derive(Debug)]
pub struct Error {
    pub path: PathBuf,
    pub error: io::Error,
}

#[derive(Debug)]
pub enum Event {
    Match(Match),
    Error(Error),
}

pub fn run<P: AsRef<Path>>(path: P, pattern: &str) -> Vec<Event> {
    let path = path.as_ref();
    let ans = Arc::new(Mutex::new(vec![]));

    let res = if path.is_file() {
        read_file(path, String::from(pattern), Arc::clone(&ans))
    } else {
        intermediate_run(path, pattern, Arc::clone(&ans))
    };

    let mut v = ans.lock().unwrap();
    match res {
        Ok(_) => {}
        Err(error) => {
            let err = Error {
                path: path.to_path_buf(),
                error,
            };
            v.push(Event::Error(err));
        }
    };

    let s: Vec<Event> = std::mem::take(&mut Arc::try_unwrap(v.into()).unwrap());
    s
}

fn intermediate_run(path: &Path, pattern: &str, list: Arc<Mutex<Vec<Event>>>) -> io::Result<()> {
    let mut threads = vec![];
    if path.is_dir() {
        for entry in fs::read_dir(path)? {
            let clone = Arc::clone(&list);
            let pat = String::from(pattern);
            threads.push(thread::spawn(move || {
                if let Ok(entry) = entry {
                    let path = entry.path();
                    if path.is_dir() {
                        let _ = recurse_run(&path, pat, clone);
                    } else {
                        let _ = read_file(&path, pat, clone);
                    }
                }
            }));
        }
    } else {
        return read_file(path, pattern.into(), list.clone());
    }

    for t in threads {
        t.join().unwrap();
    }

    Ok(())
}

fn recurse_run(path: &Path, pattern: String, list: Arc<Mutex<Vec<Event>>>) -> io::Result<()> {
    for entry in fs::read_dir(path)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            let _ = recurse_run(&path, pattern.clone(), list.clone());
        } else {
            let _ = read_file(&path, pattern.clone(), list.clone());
        }
    }
    Ok(())
}

fn read_file(path: &Path, pattern: String, list: Arc<Mutex<Vec<Event>>>) -> io::Result<()> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    for (i, line) in reader.lines().enumerate() {
        let line = line.unwrap();
        if line.contains(&pattern) {
            let tmp = Match {
                path: path.to_path_buf(),
                line,
                line_number: i + 1,
            };

            {
                let mut list_temp = list.lock().unwrap();
                list_temp.push(Event::Match(tmp));
            }
        }
    }
    Ok(())
}

/*
fn recurse_run(path: &Path, pattern: &str, list: &mut Vec<Event>) -> io::Result<()> {
    for entry in fs::read_dir(path)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            let _ = recurse_run(&path, pattern, list);
        } else {
            let _ = read_file(&path, pattern, list);
        }
    }
    Ok(())
}


fn read_file(path: &Path, pattern: &str, list: &mut Vec<Event>) -> io::Result<()> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    for (i, line) in reader.lines().enumerate() {
        let line = line.unwrap();
        if line.contains(pattern) {
            let tmp = Match {
                path: path.to_path_buf(),
                line,
                line_number: i + 1,
            };
            list.push(Event::Match(tmp));
        }
    }
    Ok(())
}
*/
