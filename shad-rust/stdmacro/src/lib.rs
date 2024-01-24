#![forbid(unsafe_code)]

#[macro_export]
macro_rules! deque {
    () => {
        __VecDeque::new()
    };
    ($elem:expr; $n:expr) => {
        __VecDeque::from([$elem; $n])
    };
    ($($el:expr),+ $(,)?) => {
        __VecDeque::from([$($el),+])
    };
}

#[macro_export]
macro_rules! sorted_vec {
    () => {
        Vec::new()
    };
    ($elem:expr; $n:expr) => {
        Vec::from_elem($elem, $n)
    };
    ($($el:expr),+ $(,)?) => {{
        let mut tmp = Vec::from([$($el),+]);
        tmp.sort();
        tmp
    }};
}

#[macro_export]
macro_rules! map {
    () => {
        __HashMap::new()
    };
    ($($a:expr => $b:expr),+ $(,)?) => {
        __HashMap::from([$( ($a, $b) ),+])
    };
}
