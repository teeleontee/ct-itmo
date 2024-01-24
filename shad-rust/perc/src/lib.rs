#![forbid(unsafe_code)]

use rand::Rng;
use std::usize;

pub struct BoolGrid {
    width: usize,
    height: usize,
    board: Vec<Vec<bool>>,
}

impl BoolGrid {
    pub fn available(&self, used: &mut Vec<Vec<bool>>, x: usize, y: usize) -> Vec<(usize, usize)> {
        let mut points: Vec<(usize, usize)> = Vec::new();
        if x + 1 < self.width() && !self.get(x + 1, y) && !used[x + 1][y] {
            points.push((x + 1, y));
        }
        if x >= 1 && !self.get(x - 1, y) && !used[x - 1][y] {
            points.push((x - 1, y));
        }
        if y < self.height() && !self.get(x, y + 1) && !used[x][y + 1] {
            points.push((x, y + 1));
        }
        if y >= 1 && !self.get(x, y - 1) && !used[x][y - 1] {
            points.push((x, y - 1));
        }
        points
    }

    fn ocupied(vacancy: f64) -> bool {
        let mut rng = rand::thread_rng();
        let rnd = rng.gen_range(0.0..1.0);
        !(rnd < vacancy)
    }

    pub fn new(width: usize, height: usize) -> Self {
        let board_init = vec![vec![false; height]; width];
        BoolGrid {
            width,
            height,
            board: board_init,
        }
    }

    pub fn random(width: usize, height: usize, vacancy: f64) -> Self {
        let mut board_init: Vec<Vec<bool>> = Vec::with_capacity(width);
        for _ in 0..width {
            let mut tmp: Vec<bool> = Vec::with_capacity(height);
            for _ in 0..height {
                tmp.push(BoolGrid::ocupied(vacancy));
            }
            board_init.push(tmp);
        }
        BoolGrid {
            width,
            height,
            board: board_init,
        }
    }

    pub fn width(&self) -> usize {
        self.width
    }

    pub fn height(&self) -> usize {
        self.height
    }

    pub fn get(&self, x: usize, y: usize) -> bool {
        self.board[x][y]
    }

    pub fn set(&mut self, x: usize, y: usize, value: bool) {
        self.board[x][y] = value;
    }
}

pub fn dfs(v: (usize, usize), used: &mut Vec<Vec<bool>>, grid: &BoolGrid) -> bool {
    if v.1 == grid.height() - 1 {
        return true;
    }
    if !used[v.0][v.1] {
        used[v.0][v.1] = true;
    }
    let available = grid.available(used, v.0, v.1);
    for p in available {
        if !used[p.0][p.1] && dfs(p, used, grid) {
            return true;
        }
    }
    false
}

pub fn percolates(grid: &BoolGrid) -> bool {
    if grid.height() == 0 || grid.width() == 0 {
        return true;
    }
    for xs in 0..grid.width() {
        let mut used = vec![vec![false; grid.height()]; grid.width()];
        if grid.get(xs, 0) {
            continue;
        }

        let found_path = dfs((xs, 0), &mut used, grid);
        if found_path {
            return true;
        }
    }
    false
}

const N_TRIALS: u64 = 10000;

pub fn evaluate_probability(width: usize, height: usize, vacancy: f64) -> f64 {
    let mut perc_count = 0;
    for _ in 0..N_TRIALS {
        let grid = BoolGrid::random(width, height, vacancy);
        if percolates(&grid) {
            perc_count += 1;
        }
    }
    perc_count as f64 / N_TRIALS as f64
}
