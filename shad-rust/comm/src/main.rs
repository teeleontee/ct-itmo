#![forbid(unsafe_code)]

use std::{collections::HashSet, env, fs, io::BufRead};

fn main() {
    let args: Vec<String> = env::args().collect();
    let file1 = fs::File::open(&args[1]).unwrap();
    let reader = std::io::BufReader::new(file1);

    let mut lines_set: HashSet<String> = HashSet::new();

    for line in reader.lines() {
        lines_set.insert(line.unwrap());
    }

    let file2 = fs::File::open(&args[2]).unwrap();
    let reader2 = std::io::BufReader::new(file2);

    for line in reader2.lines() {
        let str = line.unwrap();
        if lines_set.contains(&str) {
            println!("{}", &str);
            lines_set.remove(&str);
        }
    }
}
