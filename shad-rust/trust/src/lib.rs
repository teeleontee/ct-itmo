#![forbid(unsafe_code)]

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RoundOutcome {
    BothCooperated,
    LeftCheated,
    RightCheated,
    BothCheated,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GameMove {
    Cheat,
    Cooperate,
}

pub trait Player {
    fn make_move(&self, ctx: &Context) -> GameMove;
}

pub struct Context {
    last_move: Option<GameMove>,
    has_cheated: bool,
    rounds: i32,
}

impl Default for Context {
    fn default() -> Self {
        Context::new()
    }
}

impl Context {
    pub fn new() -> Self {
        Context {
            last_move: None,
            has_cheated: false,
            rounds: 0,
        }
    }
}

struct GameContext {
    left_score: i32,
    right_score: i32,
}

impl Default for GameContext {
    fn default() -> Self {
        GameContext::new()
    }
}

impl GameContext {
    pub fn new() -> Self {
        GameContext {
            left_score: 0,
            right_score: 0,
        }
    }
}

pub struct Game {
    player1: Box<dyn Player>,
    player2: Box<dyn Player>,
    player1_ctx: Context,
    player2_ctx: Context,
    ctx: GameContext,
}

impl Game {
    pub fn new(left: Box<dyn Player>, right: Box<dyn Player>) -> Self {
        Game {
            player1: left,
            player2: right,
            player1_ctx: Context::new(),
            player2_ctx: Context::new(),
            ctx: Default::default(),
        }
    }

    pub fn left_score(&self) -> i32 {
        self.ctx.left_score
    }

    pub fn right_score(&self) -> i32 {
        self.ctx.right_score
    }

    pub fn play_round(&mut self) -> RoundOutcome {
        let move1 = self.player1.make_move(&self.player1_ctx);
        let move2 = self.player2.make_move(&self.player2_ctx);
        self.update_context(move1, move2);
        self.get_result(move1, move2)
    }

    fn update_context(&mut self, player1_move: GameMove, player2_move: GameMove) {
        match player1_move {
            GameMove::Cooperate => {}
            GameMove::Cheat => self.player2_ctx.has_cheated = true,
        };
        match player2_move {
            GameMove::Cooperate => {}
            GameMove::Cheat => self.player1_ctx.has_cheated = true,
        };
        self.player2_ctx.rounds += 1;
        self.player1_ctx.rounds += 1;
        self.player2_ctx.last_move = Some(player1_move);
        self.player1_ctx.last_move = Some(player2_move);
    }

    fn get_result(&mut self, player1_move: GameMove, player2_move: GameMove) -> RoundOutcome {
        match player1_move {
            GameMove::Cooperate => match player2_move {
                GameMove::Cooperate => {
                    self.ctx.left_score += 2;
                    self.ctx.right_score += 2;
                    RoundOutcome::BothCooperated
                }
                GameMove::Cheat => {
                    self.ctx.left_score -= 1;
                    self.ctx.right_score += 3;
                    RoundOutcome::RightCheated
                }
            },
            GameMove::Cheat => match player2_move {
                GameMove::Cooperate => {
                    self.ctx.left_score += 3;
                    self.ctx.right_score -= 1;
                    RoundOutcome::LeftCheated
                }
                GameMove::Cheat => RoundOutcome::BothCheated,
            },
        }
    }
}

/* CheatingAgent */

pub struct CheatingAgent {}

impl Default for CheatingAgent {
    fn default() -> Self {
        CheatingAgent::new()
    }
}

impl Player for CheatingAgent {
    fn make_move(&self, _ctx: &Context) -> GameMove {
        GameMove::Cheat
    }
}

impl CheatingAgent {
    pub fn new() -> Self {
        CheatingAgent {}
    }
}

/* CooperatingAgent */

pub struct CooperatingAgent {}

impl Default for CooperatingAgent {
    fn default() -> Self {
        CooperatingAgent::new()
    }
}

impl Player for CooperatingAgent {
    fn make_move(&self, _ctx: &Context) -> GameMove {
        GameMove::Cooperate
    }
}

impl CooperatingAgent {
    pub fn new() -> Self {
        CooperatingAgent {}
    }
}

/* GrudgerAgent */

pub struct GrudgerAgent {}

impl GrudgerAgent {
    pub fn new() -> Self {
        GrudgerAgent {}
    }
}

impl Default for GrudgerAgent {
    fn default() -> Self {
        GrudgerAgent::new()
    }
}

impl Player for GrudgerAgent {
    fn make_move(&self, ctx: &Context) -> GameMove {
        match ctx.has_cheated {
            true => GameMove::Cheat,
            false => GameMove::Cooperate,
        }
    }
}

/* CopycatAgent */

pub struct CopycatAgent {}

impl Player for CopycatAgent {
    fn make_move(&self, ctx: &Context) -> GameMove {
        match ctx.last_move {
            None => GameMove::Cooperate,
            Some(choice) => choice,
        }
    }
}

impl CopycatAgent {
    pub fn new() -> Self {
        CopycatAgent {}
    }
}

impl Default for CopycatAgent {
    fn default() -> Self {
        CopycatAgent::new()
    }
}

/* DetectiveAgent */

pub struct DetectiveAgent {}

impl Default for DetectiveAgent {
    fn default() -> Self {
        DetectiveAgent::new()
    }
}

impl Player for DetectiveAgent {
    fn make_move(&self, ctx: &Context) -> GameMove {
        match ctx.rounds {
            0 => GameMove::Cooperate,
            1 => GameMove::Cheat,
            2 => GameMove::Cooperate,
            3 => GameMove::Cooperate,
            _ => match ctx.has_cheated {
                true => ctx.last_move.unwrap(),
                false => GameMove::Cheat,
            },
        }
    }
}

impl DetectiveAgent {
    pub fn new() -> Self {
        DetectiveAgent {}
    }
}
