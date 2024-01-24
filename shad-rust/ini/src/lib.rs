#![forbid(unsafe_code)]

use std::collections::HashMap;

pub type IniFile = HashMap<String, HashMap<String, String>>;

pub fn parse(content: &str) -> IniFile {
    let mut ini_file = IniFile::new();
    let mut lines = content.lines();
    let mut current_section: Option<&str> = None;
    loop {
        let line = lines.next();
        match line {
            Some(_) => {
                let trimmed_line = line.unwrap().trim();
                if trimmed_line.is_empty() {
                    continue;
                }
                if trimmed_line.starts_with('[') && trimmed_line.ends_with(']') {
                    let section = &trimmed_line[1..trimmed_line.len() - 1];
                    if section.contains(']') || section.contains('[') {
                        panic!("bad section");
                    }
                    if !ini_file.contains_key(section) {
                        ini_file.insert(String::from(section), HashMap::new());
                    }
                    current_section = Some(section);
                } else {
                    if current_section.is_none() {
                        panic!("no section");
                    }
                    let key_value: Vec<&str> = trimmed_line.split('=').map(|s| s.trim()).collect();
                    let (k, v): (Option<String>, Option<String>) = match key_value.len() {
                        1 => (Some(String::from(key_value[0])), Some(String::from(""))),
                        2 => (
                            Some(String::from(key_value[0])),
                            Some(String::from(key_value[1])),
                        ),
                        _ => {
                            panic!("bad key-value pair")
                        }
                    };
                    ini_file
                        .get_mut(current_section.unwrap())
                        .unwrap()
                        .insert(k.unwrap(), v.unwrap());
                }
            }
            None => break,
        }
    }
    ini_file
}
