use std::{collections::VecDeque, str::CharIndices};

use crate::{CharLocations, Location, Span};

pub struct Tokenizer<'source> {
    source: &'source str,
    char_indices: CharLocations<CharIndices<'source>>,
    pub lookaheads: VecDeque<(Location, char)>,
}

impl<'source> Tokenizer<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            source,
            char_indices: CharLocations::new(source.char_indices()),
            lookaheads: VecDeque::new(),
        }
    }

    pub fn match_char(&mut self, ch: char) -> Option<(Location, char)> {
        self.match_char_if(|next_ch| next_ch == ch)
    }

    pub fn match_char_if<F: FnOnce(char) -> bool>(
        &mut self,
        predicate: F,
    ) -> Option<(Location, char)> {
        let peeked = self.peek()?;
        if predicate(peeked.1) {
            let next = self.next_ch_index();
            next
        } else {
            None
        }
    }

    pub fn match_chars_while<F: FnMut(char) -> bool>(&mut self, mut predicate: F) -> Option<Span> {
        let mut start = None;
        loop {
            let met_predicate = match self.peek() {
                None => false,
                Some(peeked) => predicate(peeked.1),
            };

            if !met_predicate {
                if let Some(start) = start {
                    return Some(Span::new(start, self.peek_location()));
                } else {
                    return None;
                }
            }

            let next = self.next_ch_index().unwrap().0;
            if start.is_none() {
                start = Some(next);
            }
        }
    }

    pub fn match_str_exact(&mut self, str_to_match: &str) -> Option<Span> {
        assert!(!str_to_match.is_empty());
        let start = self.peek_location();

        let mut lookaheads = VecDeque::new();

        for ch in str_to_match.chars() {
            if let Some(ch_index) = self.next_ch_index() {
                let matches = ch_index.1 == ch;
                lookaheads.push_back(ch_index);

                if matches {
                    continue;
                } else {
                    self.lookaheads = lookaheads;
                    return None;
                }
            }
        }

        return Some(Span::new(start, self.peek_location()));
    }

    pub fn next_ch_index(&mut self) -> Option<(Location, char)> {
        if !self.lookaheads.is_empty() {
            self.lookaheads.pop_front()
        } else {
            self.char_indices.next()
        }
    }

    /// Peeks the next character location combination from
    /// this tokenizer
    pub fn peek(&mut self) -> Option<&(Location, char)> {
        if self.lookaheads.is_empty() {
            if let Some(next) = self.char_indices.next() {
                self.lookaheads.push_back(next);
            }
        }

        self.lookaheads.iter().next()
    }

    /// Peeks the location of the next character in the string.
    /// This will return a value even if the entire string has
    /// been consumed.
    pub fn peek_location(&mut self) -> Location {
        match self.peek() {
            None => {
                let mut col = self.char_indices.col();
                col.increment();
                Location::new(self.source.len(), self.char_indices.line(), col)
            }
            Some(peeked) => peeked.0.clone(),
        }
    }
}
