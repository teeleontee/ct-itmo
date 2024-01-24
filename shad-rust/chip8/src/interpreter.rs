use std::{collections::VecDeque, usize};

use crate::{
    data::{Address, Nibble, OpCode, RegisterIndex, Word},
    image::Image,
    platform::{Platform, Point, Sprite},
    Error, Offset, Result,
};

////////////////////////////////////////////////////////////////////////////////

pub const SCREEN_WIDTH: usize = 64;
pub const SCREEN_HEIGHT: usize = 32;

////////////////////////////////////////////////////////////////////////////////

pub struct Interpreter<P: Platform> {
    platform: P,
    registers: [Word; 16],
    call_stack: VecDeque<Address>,
    memory: [Word; 4096],
    program_counter: Address,
    index_register: Address,
    is_pressed: bool,
}

impl<P: Platform> Interpreter<P> {
    pub const LAST_VF: usize = 15;

    pub fn new(image: impl Image, platform: P) -> Self {
        let program_counter = image.entry_point();
        let mut memory: [u8; Address::DOMAIN_SIZE] = [0u8; Address::DOMAIN_SIZE];
        image.load_into_memory(&mut memory);
        let call_stack = VecDeque::new();
        let registers = [0u8; 16];
        let index_register = Address::new(0);
        Self {
            platform,
            registers,
            call_stack,
            memory,
            program_counter,
            index_register,
            is_pressed: false,
        }
    }

    pub fn platform(&self) -> &P {
        &self.platform
    }

    pub fn platform_mut(&mut self) -> &mut P {
        &mut self.platform
    }

    pub fn run_next_instruction(&mut self) -> Result<()> {
        let big_part = self.memory[self.program_counter.as_usize()];
        let little_part = self.memory[self.program_counter.as_usize() + 1];
        self.program_counter += 2;
        let code = OpCode::from_bytes(big_part, little_part);
        let operation = Operation::try_from(code);
        if operation.is_err() {
            return Err(Error::UnknownOpCode(code));
        }

        if let Operation::WaitForKey(vx) = operation.unwrap() {
            println!("actually got here ");
            let key = self.platform_mut().consume_key_press();
            if let Some(val) = key {
                println!("hui hui hui");
                self.registers[vx.as_usize()] = val.as_u8();
                self.is_pressed = true;
            } else {
                self.program_counter += -2 as Offset;
            }
            return Ok(());
        }

        if self.is_pressed {
            self.program_counter += -2 as Offset;
            if !self.platform().get_pressed_key() {
                self.is_pressed = false;
            }
            return Ok(());
        }

        match operation.unwrap() {
            Operation::ClearScreen => self.platform.clear_screen(),
            Operation::SetRegister(reg, word) => {
                self.registers[reg.as_usize()] = word;
            }
            Operation::SetIndexRegister(addr) => {
                self.index_register = addr;
            }
            Operation::Jump(addr) => {
                self.program_counter = addr;
            }
            Operation::Draw(vx, vy, n) => {
                let p = Point {
                    x: self.registers[vx.as_usize()],
                    y: self.registers[vy.as_usize()],
                };
                let slice = &self.memory
                    [self.index_register.as_usize()..self.index_register.as_usize() + n.as_usize()];
                let flipped = self.platform.draw_sprite(p, Sprite::new(slice));
                self.registers[15] = flipped as u8
            }
            Operation::AddValue(reg, word) => {
                self.registers[reg.as_usize()] = self.registers[reg.as_usize()].wrapping_add(word);
            }
            Operation::SkipIfEqual(vx, word) => {
                if self.registers[vx.as_usize()] == word {
                    self.program_counter += 2;
                }
            }
            Operation::SkipIfNotEqual(vx, word) => {
                if self.registers[vx.as_usize()] != word {
                    self.program_counter += 2;
                }
            }
            Operation::SkipIfRegistersEqual(vx, vy) => {
                if self.registers[vx.as_usize()] == self.registers[vy.as_usize()] {
                    self.program_counter += 2;
                }
            }
            Operation::SkipIfRegistersNotEqual(vx, vy) => {
                if self.registers[vx.as_usize()] != self.registers[vy.as_usize()] {
                    self.program_counter += 2;
                }
            }
            Operation::Call(addr) => {
                self.call_stack.push_back(self.program_counter);
                self.program_counter = addr;
            }
            Operation::Return => {
                self.program_counter = self.call_stack.pop_back().unwrap();
            }
            Operation::SetToRegister(vx, vy) => {
                self.registers[vx.as_usize()] = self.registers[vy.as_usize()];
            }
            Operation::Or(vx, vy) => {
                self.registers[vx.as_usize()] |= self.registers[vy.as_usize()];
                self.registers[15] = 0;
            }
            Operation::And(vx, vy) => {
                self.registers[vx.as_usize()] &= self.registers[vy.as_usize()];
                self.registers[15] = 0;
            }
            Operation::Xor(vx, vy) => {
                self.registers[vx.as_usize()] ^= self.registers[vy.as_usize()];
                self.registers[15] = 0;
            }
            Operation::AddRegister(vx, vy) => {
                let result =
                    self.registers[vx.as_usize()].checked_add(self.registers[vy.as_usize()]);
                match result {
                    None => {
                        self.registers[vx.as_usize()] = self.registers[vx.as_usize()]
                            .wrapping_add(self.registers[vy.as_usize()]);
                        self.registers[Self::LAST_VF] = 1;
                    }
                    Some(val) => {
                        self.registers[vx.as_usize()] = val;
                        self.registers[Self::LAST_VF] = 0;
                    }
                }
            }
            Operation::SubRegister(vx, vy) => {
                let result =
                    self.registers[vx.as_usize()].checked_sub(self.registers[vy.as_usize()]);
                match result {
                    None => {
                        self.registers[vx.as_usize()] = self.registers[vx.as_usize()]
                            .wrapping_sub(self.registers[vy.as_usize()]);
                        self.registers[Self::LAST_VF] = 0;
                    }
                    Some(val) => {
                        self.registers[vx.as_usize()] = val;
                        self.registers[Self::LAST_VF] = 1;
                    }
                }
            }
            Operation::SubRegisterReversed(vx, vy) => {
                let result =
                    self.registers[vy.as_usize()].checked_sub(self.registers[vx.as_usize()]);
                match result {
                    None => {
                        self.registers[vx.as_usize()] = self.registers[vy.as_usize()]
                            .wrapping_sub(self.registers[vx.as_usize()]);
                        self.registers[Self::LAST_VF] = 0;
                    }
                    Some(val) => {
                        self.registers[vx.as_usize()] = val;
                        self.registers[Self::LAST_VF] = 1;
                    }
                }
            }
            Operation::ShiftRight(vx, vy) => {
                let orig = self.registers[vy.as_usize()];
                let suspended = ((orig >> 1) << 1) != orig;
                let (val, _overflow) = self.registers[vy.as_usize()].overflowing_shr(1);
                self.registers[vx.as_usize()] = val;
                self.registers[Self::LAST_VF] = suspended as u8;
            }
            Operation::ShiftLeft(vx, vy) => {
                let orig = self.registers[vy.as_usize()];
                let suspended = ((orig << 1) >> 1) != orig;
                let (val, _overflow) = self.registers[vy.as_usize()].overflowing_shl(1);
                self.registers[vx.as_usize()] = val;
                self.registers[Self::LAST_VF] = suspended as u8;
            }
            Operation::ReadMemory(vx) => {
                for (i, reg) in &mut self.registers[0..vx.as_usize() + 1].iter_mut().enumerate() {
                    *reg = self.memory[self.index_register.as_usize() + i];
                }
                self.index_register += self.registers[vx.as_usize()] as Offset;
            }
            Operation::WriteMemory(vx) => {
                for (i, reg) in &mut self.registers[0..vx.as_usize() + 1].iter_mut().enumerate() {
                    self.memory[self.index_register.as_usize() + i] = *reg;
                }
                self.index_register += self.registers[vx.as_usize()] as Offset;
            }
            Operation::ToDecimal(vx) => {
                let reg_val = self.registers[vx.as_usize()];
                self.memory[self.index_register.as_usize()] = reg_val / 100;
                self.memory[self.index_register.as_usize() + 1] = reg_val % 100 / 10;
                self.memory[self.index_register.as_usize() + 2] = reg_val % 10;
            }
            Operation::IncrementIndexRegister(vx) => {
                self.index_register += self.registers[vx.as_usize()] as i16;
            }
            Operation::SkipIfKeyDown(vx) => {
                let tmp = self.registers[vx.as_usize()];
                if self.platform.is_key_down(Nibble::try_from(tmp).unwrap()) {
                    self.program_counter += 2;
                }
            }
            Operation::SkipIfKeyUp(vx) => {
                let tmp = self.registers[vx.as_usize()];
                if !self.platform.is_key_down(Nibble::try_from(tmp).unwrap()) {
                    self.program_counter += 2;
                }
            }
            Operation::GetDelayTimer(vx) => {
                self.registers[vx.as_usize()] = self.platform_mut().get_delay_timer();
            }
            Operation::SetDelayTimer(vx) => {
                self.platform.set_delay_timer(self.registers[vx.as_usize()]);
            }
            Operation::JumpV0(addr) => {
                self.program_counter = addr;
                self.program_counter += self.registers[0] as Offset;
            }
            _ => {
                unimplemented!()
            }
        }
        Ok(())
    }
}

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, Copy)]
pub enum Operation {
    ClearScreen,
    Return,
    Jump(Address),
    Call(Address),
    SkipIfEqual(RegisterIndex, Word),
    SkipIfNotEqual(RegisterIndex, Word),
    SkipIfRegistersEqual(RegisterIndex, RegisterIndex),
    SetRegister(RegisterIndex, Word),
    AddValue(RegisterIndex, Word),
    SetToRegister(RegisterIndex, RegisterIndex),
    Or(RegisterIndex, RegisterIndex),
    And(RegisterIndex, RegisterIndex),
    Xor(RegisterIndex, RegisterIndex),
    AddRegister(RegisterIndex, RegisterIndex),
    SubRegister(RegisterIndex, RegisterIndex),
    ShiftRight(RegisterIndex, RegisterIndex),
    SubRegisterReversed(RegisterIndex, RegisterIndex),
    ShiftLeft(RegisterIndex, RegisterIndex),
    SkipIfRegistersNotEqual(RegisterIndex, RegisterIndex),
    SetIndexRegister(Address),
    JumpV0(Address),
    SetToRandom(RegisterIndex, Word),
    Draw(RegisterIndex, RegisterIndex, Nibble),
    SkipIfKeyDown(RegisterIndex),
    SkipIfKeyUp(RegisterIndex),
    GetDelayTimer(RegisterIndex),
    WaitForKey(RegisterIndex),
    SetDelayTimer(RegisterIndex),
    SetSoundTimer(RegisterIndex),
    IncrementIndexRegister(RegisterIndex),
    SetIndexRegisterToSprite(Nibble),
    ToDecimal(RegisterIndex),
    WriteMemory(Nibble),
    ReadMemory(Nibble),
}

impl TryFrom<OpCode> for Operation {
    type Error = ();

    fn try_from(code: OpCode) -> std::result::Result<Self, ()> {
        let op = match code.as_u8s() {
            [0x0, 0x0, 0xE, 0x0] => Operation::ClearScreen,
            [0x0, 0x0, 0xE, 0xE] => Operation::Return,
            [0x6, _, _, _] => Operation::SetRegister(code.extract_nibble(2), code.extract_word(0)),
            [0xA, _, _, _] => Operation::SetIndexRegister(code.extract_address()),
            [0x1, _, _, _] => Operation::Jump(code.extract_address()),
            [0x2, _, _, _] => Operation::Call(code.extract_address()),
            [0xD, _, _, _] => Operation::Draw(
                code.extract_nibble(2),
                code.extract_nibble(1),
                code.extract_nibble(0),
            ),
            [0x7, _, _, _] => Operation::AddValue(code.extract_nibble(2), code.extract_word(0)),
            [0x3, _, _, _] => Operation::SkipIfEqual(code.extract_nibble(2), code.extract_word(0)),
            [0x4, _, _, _] => {
                Operation::SkipIfNotEqual(code.extract_nibble(2), code.extract_word(0))
            }
            [0x5, _, _, 0x0] => {
                Operation::SkipIfRegistersEqual(code.extract_nibble(2), code.extract_nibble(1))
            }
            [0x8, _, _, 0x0] => {
                Operation::SetToRegister(code.extract_nibble(2), code.extract_nibble(1))
            }
            [0x8, _, _, 0x1] => Operation::Or(code.extract_nibble(2), code.extract_nibble(1)),
            [0x8, _, _, 0x2] => Operation::And(code.extract_nibble(2), code.extract_nibble(1)),
            [0x8, _, _, 0x3] => Operation::Xor(code.extract_nibble(2), code.extract_nibble(1)),
            [0x8, _, _, 0x4] => {
                Operation::AddRegister(code.extract_nibble(2), code.extract_nibble(1))
            }
            [0x8, _, _, 0x5] => {
                Operation::SubRegister(code.extract_nibble(2), code.extract_nibble(1))
            }
            [0x8, _, _, 0x6] => {
                Operation::ShiftRight(code.extract_nibble(2), code.extract_nibble(1))
            }
            [0x8, _, _, 0x7] => {
                Operation::SubRegisterReversed(code.extract_nibble(2), code.extract_nibble(1))
            }
            [0x9, _, _, 0x0] => {
                Operation::SkipIfRegistersNotEqual(code.extract_nibble(2), code.extract_nibble(1))
            }
            [0x8, _, _, 0xE] => {
                Operation::ShiftLeft(code.extract_nibble(2), code.extract_nibble(1))
            }

            [0xB, _, _, _] => Operation::JumpV0(code.extract_address()),

            [0xE, _, 0x9, 0xE] => Operation::SkipIfKeyDown(code.extract_nibble(2)),
            [0xE, _, 0xA, 0x1] => Operation::SkipIfKeyUp(code.extract_nibble(2)),
            [0xF, _, 0x0, 0xA] => Operation::WaitForKey(code.extract_nibble(2)),
            [0xF, _, 0x1, 0xE] => Operation::IncrementIndexRegister(code.extract_nibble(2)),

            [0xF, _, 0x0, 0x7] => Operation::GetDelayTimer(code.extract_nibble(2)),
            [0xF, _, 0x1, 0x5] => Operation::SetDelayTimer(code.extract_nibble(2)),

            [0xF, _, 0x3, 0x3] => Operation::ToDecimal(code.extract_nibble(2)),
            [0xF, _, 0x5, 0x5] => Operation::WriteMemory(code.extract_nibble(2)),
            [0xF, _, 0x6, 0x5] => Operation::ReadMemory(code.extract_nibble(2)),
            _ => return Err(()),
        };
        Ok(op)
    }
}

////////////////////////////////////////////////////////////////////////////////
