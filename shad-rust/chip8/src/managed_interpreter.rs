use crate::{
    data::Word,
    error::Result,
    image::Image,
    interpreter::{Interpreter, SCREEN_HEIGHT, SCREEN_WIDTH},
    platform::{Key, Platform, Point, Sprite},
};

use core::time::Duration;

////////////////////////////////////////////////////////////////////////////////

pub struct FrameBuffer([[bool; SCREEN_WIDTH]; SCREEN_HEIGHT]);

impl Default for FrameBuffer {
    fn default() -> Self {
        Self([[false; SCREEN_WIDTH]; SCREEN_HEIGHT])
    }
}

impl FrameBuffer {
    pub fn iter_rows(&self) -> impl Iterator<Item = &[bool; SCREEN_WIDTH]> {
        self.0.iter()
    }
}

////////////////////////////////////////////////////////////////////////////////

pub trait RandomNumberGenerator: FnMut() -> Word {}

impl<R: FnMut() -> Word> RandomNumberGenerator for R {}

////////////////////////////////////////////////////////////////////////////////

#[derive(Default)]
struct ManagedPlatform<R: RandomNumberGenerator> {
    rand: R,
    frame_buffer: FrameBuffer,
    delay_timer: Word,
    sound_timer: Word,
    keys: [bool; 16],
}

impl<R: RandomNumberGenerator> Platform for ManagedPlatform<R> {
    fn draw_sprite(&mut self, pos: Point, sprite: Sprite) -> bool {
        let mut res = false;
        let buf = &mut self.frame_buffer.0;
        sprite.iter_pixels().for_each(|pixel| {
            let y = pos.y as usize % 32 + pixel.y as usize;
            let x = pos.x as usize % 64 + pixel.x as usize;
            if !(y >= 32 || x >= 64) {
                res |= buf[y][x];
                buf[y][x] ^= true;
            }
        });
        res
    }

    fn get_pressed_key(&self) -> bool {
        for i in 0..self.keys.len() {
            if self.keys[i] {
                return true;
            }
        }
        false
    }

    fn clear_screen(&mut self) {
        self.frame_buffer = FrameBuffer::default();
    }

    fn get_delay_timer(&self) -> Word {
        self.delay_timer
    }

    fn set_delay_timer(&mut self, value: Word) {
        self.delay_timer = value;
    }

    fn set_sound_timer(&mut self, value: Word) {
        self.sound_timer = value;
    }

    fn is_key_down(&self, key: Key) -> bool {
        self.keys[key.as_usize()]
    }

    fn consume_key_press(&mut self) -> Option<Key> {
        for i in 0..self.keys.len() {
            if self.keys[i] {
                return Some(Key::try_from(i as u8).unwrap());
            }
        }
        None
    }

    fn get_random_word(&mut self) -> Word {
        (self.rand)()
    }
}

impl<R: RandomNumberGenerator> ManagedPlatform<R> {
    fn new(rand: R) -> Self {
        Self {
            rand,
            frame_buffer: Default::default(),
            delay_timer: 0,
            sound_timer: 0,
            keys: [false; 16],
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

pub struct ManagedInterpreter<R: RandomNumberGenerator> {
    inner: Interpreter<ManagedPlatform<R>>,
}

impl<R: RandomNumberGenerator> ManagedInterpreter<R> {
    pub fn new(image: impl Image, rand: R) -> Self {
        Self::new_with_durations(image, rand)
    }

    pub fn new_with_durations(image: impl Image, rand: R) -> Self {
        Self {
            inner: Interpreter::new(image, ManagedPlatform::new(rand)),
        }
    }

    pub fn simulate_one_instruction(&mut self) -> Result<()> {
        self.inner.run_next_instruction()
    }

    pub fn simulate_duration(&mut self, duration: Duration) -> Result<()> {
        for i in 0..duration.as_secs() * 500 {
            let _ = self.simulate_one_instruction();
            if i % 8 == 0 {
                let current_time = self.inner.platform().get_delay_timer();
                if current_time > 0 {
                    self.inner.platform_mut().set_delay_timer(current_time - 1);
                }
            }
        }
        Ok(())
    }

    pub fn frame_buffer(&self) -> &FrameBuffer {
        &self.inner.platform().frame_buffer
    }

    pub fn set_key_down(&mut self, key: Key, is_down: bool) {
        self.inner.platform_mut().keys[key.as_usize()] = is_down;
    }
}
