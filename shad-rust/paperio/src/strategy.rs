use std::cmp::{max, min};
use std::collections::HashSet;

use crate::data::{Cell, Direction, World};

#[derive(Debug)]
pub struct Strategy {
    cur_cell: Option<Cell>,
    last_cell: Option<Cell>,
    taken: HashSet<Cell>,
    in_territory: bool,
    cur_best_perimeter: Vec<Cell>,
}

impl Default for Strategy {
    fn default() -> Self {
        Self::new()
    }
}

fn get_cells_in_rect(cell1: &Cell, cell2: &Cell) -> Vec<Cell> {
    let mut cells = Vec::new();
    for x in min(cell1.0, cell2.0)..=max(cell1.0, cell2.0) {
        for y in min(cell1.1, cell2.1)..=max(cell1.1, cell2.1) {
            cells.push(Cell(x, y));
        }
    }
    cells
}

fn get_cells_in_perimeter(cell1: &Cell, cell2: &Cell) -> Vec<Cell> {
    let mut cells = Vec::new();
    for x in min(cell1.0, cell2.0)..=max(cell1.0, cell2.0) {
        cells.push(Cell(x, cell1.1));
        cells.push(Cell(x, cell2.1));
    }
    for y in min(cell1.1, cell2.1)..=max(cell1.1, cell2.1) {
        cells.push(Cell(cell1.0, y));
        cells.push(Cell(cell2.0, y));
    }
    cells
}

impl Strategy {
    fn follow_perimeter(&mut self) -> Direction {
        let all_possible_cells = self.cur_cell.unwrap().iter_neighbors();
        let mut possible_cells = Vec::new();

        for cell in all_possible_cells {
            if self.cur_best_perimeter.contains(&cell) {
                possible_cells.push(cell);
            }
        }

        let mut next_cell = Cell(0, 0);
        for cell in possible_cells {
            if Option::is_some(&self.last_cell) && cell == self.last_cell.unwrap() {
                continue;
            }
            next_cell = cell;
            break;
        }

        self.cur_cell.unwrap().direction_to(next_cell)
    }

    fn find_best_cell(&mut self, world: &World) -> Cell {
        let cells = world.iter_cells();
        let mut max_score = i32::MIN;
        let mut best_cell = Cell(0, 0);
        for cell in cells {
            let new_perimeter = get_cells_in_perimeter(&cell, &self.cur_cell.unwrap());
            let rect = get_cells_in_rect(&cell, &self.cur_cell.unwrap());
            let cells_score = self.calculate_score(&rect);
            let danger =
                (new_perimeter.len() as i32) - self.calculate_danger(&new_perimeter, world);
            let rectangle_score = cells_score * 10 - (danger * (danger - 10));
            if rectangle_score >= max_score {
                max_score = rectangle_score;
                best_cell = cell;
            }
        }
        best_cell
    }

    fn calculate_danger(&mut self, perimeter: &[Cell], world: &World) -> i32 {
        let enemy_pos: Vec<Cell> = world
            .players
            .values()
            .map(|player| player.position.to_cell())
            .collect();

        let mut min_from_enemies = i32::MAX;
        for cell in &enemy_pos {
            let min_dist = perimeter
                .iter()
                .map(|p_cell| p_cell.distance_to(*cell))
                .min()
                .unwrap();
            if min_dist < min_from_enemies {
                min_from_enemies = min_dist
            }
        }
        min_from_enemies
    }

    fn calculate_score(&mut self, rectangle: &Vec<Cell>) -> i32 {
        let mut score = 0;
        for cell in rectangle {
            if self.taken.contains(cell) {
                score += 5;
            } else {
                score += 1;
            }
        }
        score
    }

    fn fill_taken(&mut self, world: &World) {
        self.taken.clear();
        world.players.values().for_each(|player| {
            player.territory.iter().for_each(|point| {
                self.taken.insert(point.to_cell());
            })
        });
    }

    pub fn new() -> Self {
        Strategy {
            cur_cell: None,
            last_cell: None,
            taken: HashSet::new(),
            in_territory: true,
            cur_best_perimeter: Vec::new(),
        }
    }

    pub fn on_tick(&mut self, world: World) -> Direction {
        self.cur_cell = Some(world.me().position.to_cell());
        self.fill_taken(&world);

        self.in_territory = world.me().territory.contains(&world.me().position);

        if self.in_territory {
            let best_cell = Strategy::find_best_cell(self, &world);
            self.cur_best_perimeter = get_cells_in_perimeter(&self.cur_cell.unwrap(), &best_cell);
        }

        let dir = Strategy::follow_perimeter(self);
        self.last_cell = self.cur_cell;
        dir
    }
}
