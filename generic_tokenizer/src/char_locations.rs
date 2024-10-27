use std::iter::Peekable;

use crate::{Location, NonZero};

pub struct CharLocations<CharIndices>
where
    CharIndices: Iterator<Item = (usize, char)>,
{
    done: bool,
    source: Peekable<CharIndices>,
    line: usize,
    col: usize,
    previous_was_new_line: bool,
}

impl<CharIndices> CharLocations<CharIndices>
where
    CharIndices: Iterator<Item = (usize, char)>,
{
    pub fn new(source: CharIndices) -> Self {
        Self {
            done: false,
            source: source.peekable(),
            line: 1,
            col: 0, // we will increment on every loop, so start at 0
            previous_was_new_line: false,
        }
    }

    pub fn line(&self) -> NonZero {
        unsafe { NonZero::new_unchecked(self.line) }
    }

    pub fn col(&self) -> NonZero {
        if self.col == 0 {
            if self.line == 1 {
                unsafe { NonZero::new_unchecked(1) }
            } else {
                unsafe { NonZero::new_unchecked(0) }
            }
        } else {
            unsafe { NonZero::new_unchecked(self.col) }
        }
    }

    fn attach_current_location(&self, byte_index: usize) -> Location {
        let line = unsafe { NonZero::new_unchecked(self.line) };
        let col = unsafe { NonZero::new_unchecked(self.col) };
        Location::new(byte_index, line, col)
    }
}

impl<CharIndices> Iterator for CharLocations<CharIndices>
where
    CharIndices: Iterator<Item = (usize, char)>,
{
    type Item = (Location, char);

    fn next(&mut self) -> Option<Self::Item> {
        // Safety check. Don't overrun the end of any buffers
        if self.done {
            return None;
        }

        let next = match self.source.next() {
            None => {
                self.done = true;
                return None;
            }
            Some(next) => next,
        };

        let location = if self.previous_was_new_line {
            self.line += 1;
            self.col = 1;
            self.attach_current_location(next.0)
        } else {
            self.col += 1;
            self.attach_current_location(next.0)
        };

        self.previous_was_new_line = next.1 == '\n';
        Some((location, next.1))
    }
}
