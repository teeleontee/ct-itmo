#![forbid(unsafe_code)]

use std::{
    collections::{HashMap, VecDeque},
    fmt::Display,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Number(f64),
    Symbol(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(num) => write!(f, "{}", num),
            Self::Symbol(sym) => write!(f, "'{}", sym),
        }
    }
}

pub struct Interpreter {
    stack: VecDeque<Value>,
    vars: HashMap<String, f64>,
    vars_num: HashMap<String, String>,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            stack: VecDeque::new(),
            vars: HashMap::new(),
            vars_num: HashMap::new(),
        }
    }

    pub fn eval(&mut self, expr: &str) {
        let exp_lines = expr.split_whitespace().map(|s| s.trim());
        for sym in exp_lines {
            match () {
                _ if is_number(sym) => {
                    let num = sym.parse::<f64>().unwrap();
                    self.stack.push_back(Value::Number(num));
                }
                _ if is_var(sym) => {
                    self.stack.push_back(Value::Symbol(sym[1..].to_string()));
                }
                _ if is_known(sym) => {
                    if self.vars.contains_key(&sym[1..]) {
                        let num = self.vars.get(&sym[1..]);
                        self.stack.push_back(Value::Number(*num.unwrap()))
                    } else {
                        let var = self.vars_num.get(&sym[1..]);
                        self.stack
                            .push_back(Value::Symbol(var.unwrap().to_string()));
                    }
                }
                _ if is_set(sym) => {
                    let var = get_var_name(self.stack.pop_back().unwrap());
                    let var2 = self.stack.pop_back().unwrap();
                    match var2 {
                        Value::Symbol(z) => {
                            self.vars_num.insert(var, z);
                        }
                        Value::Number(num) => {
                            self.vars.insert(var, num);
                        }
                    };
                }
                _ if is_operator(sym) => {
                    let num1 = get_number(self.stack.pop_back().unwrap());
                    let num2 = get_number(self.stack.pop_back().unwrap());
                    match sym {
                        "+" => self.stack.push_back(Value::Number(num1 + num2)),
                        "-" => self.stack.push_back(Value::Number(num1 - num2)),
                        "*" => self.stack.push_back(Value::Number(num1 * num2)),
                        "/" => self.stack.push_back(Value::Number(num1 / num2)),
                        _ => panic!("unexpected operation"),
                    }
                }
                _ => {
                    dbg!(sym);
                    panic!("Unknown symbol given: {sym}");
                }
            }
        }
    }

    pub fn stack(&mut self) -> &[Value] {
        self.stack.make_contiguous()
    }
}

fn get_var_name(v: Value) -> String {
    match v {
        Value::Symbol(s) => s,
        Value::Number(_) => panic!("expected string"),
    }
}

fn get_number(v: Value) -> f64 {
    match v {
        Value::Number(num) => num,
        Value::Symbol(_) => panic!("expected number"),
    }
}

fn is_number(s: &str) -> bool {
    let test = s.parse::<f64>();
    test.is_ok()
}

fn is_operator(s: &str) -> bool {
    matches!(s, "+" | "-" | "*" | "/")
}

fn is_var(s: &str) -> bool {
    s.starts_with('\'')
}

fn is_known(s: &str) -> bool {
    s.starts_with('$')
}

fn is_set(s: &str) -> bool {
    if s.len() == 3 {
        &s[0..3] == "set"
    } else {
        false
    }
}
