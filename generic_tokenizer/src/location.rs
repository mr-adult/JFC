use std::fmt::Display;

use crate::non_zero::NonZero;

#[derive(Clone, Debug)]
pub struct Location {
    byte_index: usize,
    line: NonZero,
    col: NonZero,
}

impl Location {
    pub fn new(byte_index: usize, line: NonZero, col: NonZero) -> Self {
        Self {
            byte_index,
            line,
            col,
        }
    }

    pub fn byte_index(&self) -> usize {
        self.byte_index
    }

    pub fn line(&self) -> NonZero {
        self.line
    }

    pub fn col(&self) -> NonZero {
        self.col
    }

    pub fn increment(&mut self) {
        self.byte_index += 1;
        self.col.increment();
    }

    pub fn decrement(&mut self) {
        self.byte_index -= 1;
        unsafe {
            self.col.decrement();
        };
    }
}

impl Default for Location {
    fn default() -> Self {
        Self {
            byte_index: 0,
            line: unsafe { NonZero::new_unchecked(1) },
            col: unsafe { NonZero::new_unchecked(1) },
        }
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let line: usize = self.line.into();
        let col: usize = self.col.into();
        write!(f, "line: {0}, col: {1}", line, col)
    }
}
