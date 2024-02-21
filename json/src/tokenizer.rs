use std::{
    collections::VecDeque, error::Error, fmt::Display, iter::Peekable, ops::Range, str::CharIndices,
};

use crate::JsonParseState;

pub(crate) struct JsonTokenizer<'json> {
    source_len: usize,
    chars: Peekable<CharIndices<'json>>,
    states: Vec<JsonParseState>,
    lookahead: Option<JsonToken>,
    unicode_escape_errs: VecDeque<JsonParseErr>,
    current_position: Option<Position>,
}

impl<'json> JsonTokenizer<'json> {
    pub(crate) fn new(source: &'json str) -> Self {
        let states = vec![JsonParseState::Value];
        Self {
            source_len: source.len(),
            chars: source.char_indices().peekable(),
            states,
            lookahead: None,
            unicode_escape_errs: VecDeque::with_capacity(0),
            current_position: None,
        }
    }

    fn match_whitespace(&mut self) {
        self.match_char_while(|ch| matches!(ch, ' ' | '\n' | '\r' | '\t'));
    }

    fn match_string(&mut self) -> Result<JsonToken, JsonParseErr> {
        let start = self.peek_position();
        match self.chars.peek() {
            None => {
                self.states.clear();
                self.lookahead = Some(JsonToken {
                    span: Span {
                        start: start,
                        end: self.peek_position(),
                    },
                    kind: JsonTokenKind::String,
                });
                return Err(JsonParseErr::UnexpectedEOF);
            }
            Some(_) => {
                if !self.match_char('"') {
                    return Err(JsonParseErr::UnexpectedCharacters(
                        self.recover_in_panic_mode(),
                    ));
                }
            }
        };

        loop {
            match self.next_char() {
                None => {
                    self.states.clear();
                    self.lookahead = Some(JsonToken {
                        span: Span {
                            start,
                            end: self.peek_position(),
                        },
                        kind: JsonTokenKind::String,
                    });
                    return Err(JsonParseErr::UnexpectedEOF);
                }
                Some(ch) => {
                    match ch.1 {
                        '"' => {
                            return Ok(JsonToken {
                                span: Span {
                                    start,
                                    end: self.peek_position(),
                                },
                                kind: JsonTokenKind::String,
                            });
                        }
                        '\\' => {
                            if self.match_char('u') {
                                for i in 0..4 {
                                    if !self.match_char_if(|ch| ch.is_ascii_hexdigit()) {
                                        let peeked = self.peek_position();
                                        self.unicode_escape_errs.push_back(
                                            JsonParseErr::InvalidUnicodeEscapeSequence(Span {
                                                start: self.get_current_position().minus(i + 1),
                                                end: peeked,
                                            }),
                                        );
                                        break;
                                    }
                                }

                                if !self.unicode_escape_errs.is_empty() {
                                    continue;
                                }
                            }

                            // we're just tokenizing, not interpreting the value's escape sequences.
                            // no need to handle unicode escape sequences.
                            self.match_char_if(|ch| {
                                matches!(ch, '"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't')
                            });
                        }
                        '\n' | '\t' => {
                            // label these as errors because they are very rare in strings
                            // and aid in fault tolerance/recovery
                            self.lookahead = Some(JsonToken {
                                span: Span {
                                    start,
                                    end: self.peek_position(),
                                },
                                kind: JsonTokenKind::String,
                            });
                            return Err(JsonParseErr::UnclosedString(self.peek_position()));
                        }
                        _ => {} // just continue
                    }
                }
            }
        }
    }

    fn match_number(&mut self) -> Result<JsonToken, JsonParseErr> {
        let start = self.peek_position();
        self.match_char('-');

        let mut leading_0_err = None;
        if self.match_char('0') {
            if self.match_char_if(|ch| ch.is_ascii_digit()) {
                leading_0_err = Some(JsonParseErr::IllegalLeading0(
                    self.get_current_position().minus(1),
                ));
            }
        } else if !self.match_char_if(|ch| ch.is_ascii_digit()) {
            // We found only a dash... need to panic.
            let mut span = self.recover_in_panic_mode();
            span.start = start;
            return Err(JsonParseErr::UnexpectedCharacters(span));
        }

        self.match_char_while(|ch| ch.is_ascii_digit());
        if self.match_char('.') {
            self.match_char_while(|ch| ch.is_ascii_digit());
        }

        if self.match_char_if(|ch| ch == 'e' || ch == 'E') {
            self.match_char_if(|ch| ch == '-' || ch == '+');
            self.match_char_while(|ch| ch.is_ascii_digit());
        }

        let token = JsonToken {
            span: Span {
                start,
                end: self.peek_position(),
            },
            kind: JsonTokenKind::Number,
        };

        if let Some(err) = leading_0_err {
            self.lookahead = Some(token);
            return Err(err);
        }

        Ok(token)
    }

    fn match_literal(&mut self, str: &str) -> bool {
        for char in str.chars() {
            if !self.match_char(char) {
                return false;
            }
        }
        true
    }

    fn match_char(&mut self, char: char) -> bool {
        self.match_char_if(|ch| ch == char)
    }

    fn match_char_while<P: FnMut(char) -> bool>(&mut self, mut predicate: P) {
        loop {
            if !self.match_char_if(&mut |ch| predicate(ch)) {
                break;
            }
        }
    }

    fn match_char_if<P: FnMut(char) -> bool>(&mut self, mut predicate: P) -> bool {
        match self.chars.peek() {
            None => false,
            Some(char) => {
                if predicate(char.1) {
                    self.next_char();
                    true
                } else {
                    false
                }
            }
        }
    }

    fn next_char(&mut self) -> Option<(usize, char)> {
        if let Some((i, ch)) = self.chars.next() {
            if self.current_position.is_none() {
                self.current_position = Some(Position::default());
            }

            if ch == '\n' {
                self.current_position.as_mut().unwrap().line += 1;
                self.current_position.as_mut().unwrap().col = 1;
            } else {
                self.current_position.as_mut().unwrap().col += 1;
            }
            self.current_position.as_mut().unwrap().raw = i;
            Some((i, ch))
        } else {
            None
        }
    }

    fn peek_position(&mut self) -> Position {
        let mut result = self.get_current_position();
        if let Some((i, ch)) = self.chars.peek() {
            if *ch == '\n' {
                result.line += 1;
                result.col = 1;
            } else {
                result.col += 1;
            }
            result.raw = *i;
            result
        } else {
            result.raw = self.source_len;
            result
        }
    }

    /// Tries to recover from an unexpected character using panic mode.
    /// We resynchronize on ']', '}', and ',' as these 3 are points of known, recoverable states.
    fn recover_in_panic_mode(&mut self) -> Span {
        let start = self.peek_position();

        loop {
            match self.chars.peek() {
                None => {
                    break;
                }
                Some(char) => {
                    match char.1 {
                        '[' | '{' => {
                            // These are valid tokens, so don't match it and just pass control back
                            // ot the regular tokenizer
                            break;
                        }
                        ']' => {
                            // Found array end. Resynchronize
                            if !self
                                .states
                                .iter()
                                .rev()
                                .any(|state| *state == JsonParseState::Array)
                            {
                                // consume the character, but we didn't have any arrays
                                // so we need to keep panicking.
                                self.next_char();
                                continue;
                            } else {
                                while let Some(state) = self.states.pop() {
                                    if let JsonParseState::Array = state {
                                        break;
                                    }
                                }

                                self.next_char();
                                self.lookahead = Some(JsonToken {
                                    span: Span {
                                        start: self.get_current_position(),
                                        end: self.peek_position(),
                                    },
                                    kind: JsonTokenKind::ArrayEnd,
                                });

                                return Span {
                                    start,
                                    end: self.get_current_position(),
                                };
                            }
                        }
                        '}' => {
                            // Found object end. Resynchronize
                            if !self
                                .states
                                .iter()
                                .rev()
                                .any(|state| *state == JsonParseState::Object)
                            {
                                // consume the character, but we didn't have any arrays
                                // so we need to keep panicking.
                                self.next_char();
                                continue;
                            } else {
                                while let Some(state) = self.states.pop() {
                                    if let JsonParseState::Object = state {
                                        break;
                                    }
                                }
                                self.next_char();
                                self.lookahead = Some(JsonToken {
                                    span: Span {
                                        start: self.get_current_position(),
                                        end: self.peek_position(),
                                    },
                                    kind: JsonTokenKind::ObjectEnd,
                                });

                                return Span {
                                    start,
                                    end: self.get_current_position(),
                                };
                            }
                        }
                        ',' => {
                            if !self.states.iter().any(|state| {
                                matches!(state, JsonParseState::Array | JsonParseState::Object)
                            }) {
                                self.next_char();
                                continue;
                            }

                            while let Some(state) = self.states.pop() {
                                match state {
                                    JsonParseState::Array => {
                                        self.states.push(JsonParseState::Array);
                                        break;
                                    }
                                    JsonParseState::Object => {
                                        self.states.push(JsonParseState::Object);
                                        self.states.push(JsonParseState::Value);
                                        self.states.push(JsonParseState::KeyValuePairColon);
                                        self.states.push(JsonParseState::KeyValuePairKey);
                                        break;
                                    }
                                    _ => {}
                                }
                            }

                            // always match the comma.
                            self.next_char();
                            self.lookahead = Some(JsonToken {
                                span: Span {
                                    start: self.get_current_position(),
                                    end: self.peek_position(),
                                },
                                kind: JsonTokenKind::Comma,
                            });

                            return Span {
                                start,
                                end: self.get_current_position(),
                            };
                        }
                        _ => {
                            self.next_char();
                            continue;
                        }
                    };
                }
            }
        }

        Span {
            start,
            end: self.peek_position(),
        }
    }

    fn get_current_position(&self) -> Position {
        match &self.current_position {
            None => Position::default(),
            Some(pos) => pos.clone(),
        }
    }
}

impl<'i> Iterator for JsonTokenizer<'i> {
    type Item = Result<JsonToken, JsonParseErr>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(unicode_err) = self.unicode_escape_errs.pop_front() {
            return Some(Err(unicode_err));
        }

        if let Some(lookahead) = std::mem::take(&mut self.lookahead) {
            return Some(Ok(lookahead));
        }

        loop {
            match self.states.pop() {
                None => {
                    self.match_whitespace();
                    if self.match_char(',') {
                        // trailing commas are common. Make sure we don't choke on them.
                        return Some(Err(JsonParseErr::TrailingComma(
                            self.get_current_position(),
                        )));
                    }
                    self.match_whitespace();
                    self.states.push(JsonParseState::Value);
                }
                Some(state) => {
                    match state {
                        JsonParseState::Value => {
                            self.match_whitespace();
                            match self.chars.peek() {
                                None => {
                                    if self.states.is_empty() {
                                        return None;
                                    } else {
                                        self.states.clear();
                                        return Some(Err(JsonParseErr::UnexpectedEOF));
                                    }
                                }
                                Some(ch) => {
                                    if !self.states.is_empty() {
                                        self.states.push(JsonParseState::AfterValue);
                                    }
                                    match ch.1 {
                                        '{' => {
                                            self.states.push(JsonParseState::Object);
                                            self.next_char();
                                            let span = Span {
                                                start: self.get_current_position(),
                                                end: self.peek_position(),
                                            };
                                            return Some(Ok(JsonToken {
                                                span,
                                                kind: JsonTokenKind::ObjectStart,
                                            }));
                                        }
                                        '[' => {
                                            self.states.push(JsonParseState::Array);
                                            self.next_char();
                                            let start = self.get_current_position();
                                            let span = Span {
                                                start,
                                                end: self.peek_position(),
                                            };
                                            return Some(Ok(JsonToken {
                                                span,
                                                kind: JsonTokenKind::ArrayStart,
                                            }));
                                        }
                                        '"' => {
                                            let token_result = self.match_string();
                                            let token = match token_result {
                                                Err(err) => return Some(Err(err)),
                                                Ok(token) => token,
                                            };

                                            if let Some(err) = self.unicode_escape_errs.pop_front()
                                            {
                                                self.lookahead = Some(token);
                                                return Some(Err(err));
                                            }
                                            return Some(Ok(token));
                                        }
                                        '-' | '0'..='9' => {
                                            return Some(self.match_number());
                                        }
                                        _ => {
                                            let current_position = if self.current_position.is_none()
                                            {
                                                Position::default()
                                            } else {
                                                self.peek_position()
                                            };

                                            if self.match_literal("true") {
                                                return Some(Ok(JsonToken {
                                                    span: Span {
                                                        start: current_position,
                                                        end: self.peek_position(),
                                                    },
                                                    kind: JsonTokenKind::True,
                                                }));
                                            }

                                            if self.match_literal("false") {
                                                return Some(Ok(JsonToken {
                                                    span: Span {
                                                        start: current_position,
                                                        end: self.peek_position(),
                                                    },
                                                    kind: JsonTokenKind::False,
                                                }));
                                            }

                                            if self.match_literal("null") {
                                                return Some(Ok(JsonToken {
                                                    span: Span {
                                                        start: current_position,
                                                        end: self.peek_position(),
                                                    },
                                                    kind: JsonTokenKind::Null,
                                                }));
                                            }

                                            // re-push value back onto the stack, removing AfterValue
                                            self.states.pop();
                                            self.states.push(JsonParseState::Value);
                                            let mut span = self.recover_in_panic_mode();
                                            span.start = current_position;
                                            return Some(Err(JsonParseErr::UnexpectedCharacters(
                                                span,
                                            )));
                                        }
                                    }
                                }
                            }
                        }
                        JsonParseState::Object => {
                            self.match_whitespace();
                            if self.match_char('}') {
                                return Some(Ok(JsonToken {
                                    span: Span {
                                        start: self.get_current_position(),
                                        end: self.peek_position(),
                                    },
                                    kind: JsonTokenKind::ObjectEnd,
                                }));
                            }

                            // need to find the colon + value next. Put them on the stack!
                            self.states.push(JsonParseState::Object);
                            self.states.push(JsonParseState::Value);
                            self.states.push(JsonParseState::KeyValuePairColon);
                            self.states.push(JsonParseState::KeyValuePairKey);
                            continue;
                        }
                        JsonParseState::KeyValuePairColon => {
                            self.match_whitespace();
                            if self.match_char(':') {
                                return Some(Ok(JsonToken {
                                    span: Span {
                                        start: self.get_current_position(),
                                        end: self.peek_position(),
                                    },
                                    kind: JsonTokenKind::Colon,
                                }));
                            } else {
                                return Some(Err(JsonParseErr::UnexpectedCharacters(
                                    self.recover_in_panic_mode(),
                                )));
                            }
                        }
                        JsonParseState::KeyValuePairKey => {
                            self.match_whitespace();

                            let token_result = self.match_string();
                            let token = match token_result {
                                Err(err) => return Some(Err(err)),
                                Ok(token) => token,
                            };

                            if let Some(unicode_err) = self.unicode_escape_errs.pop_front() {
                                self.lookahead = Some(token);
                                return Some(Err(unicode_err));
                            }

                            return Some(Ok(token));
                        }
                        JsonParseState::AfterValue => {
                            self.match_whitespace();
                            let start = self.get_current_position();
                            if self.match_char(',') {
                                let comma_position = self.get_current_position();
                                self.match_whitespace();
                                if let Some((_, '}' | ']')) = self.chars.peek() {
                                    return Some(Err(
                                        // trailing commas are common. Make sure we don't choke on them.
                                        JsonParseErr::TrailingComma(comma_position),
                                    ));
                                }

                                match self
                                    .states
                                    .last()
                                    .expect("BUG: States should include at least 1 value")
                                {
                                    JsonParseState::Object => {
                                        self.states.push(JsonParseState::Value);
                                        self.states.push(JsonParseState::KeyValuePairColon);
                                        self.states.push(JsonParseState::KeyValuePairKey);
                                        return Some(Ok(JsonToken {
                                            span: Span {
                                                start: self.get_current_position(),
                                                end: self.peek_position(),
                                            },
                                            kind: JsonTokenKind::Comma,
                                        }));
                                    }
                                    JsonParseState::Array => {
                                        self.states.push(JsonParseState::Value);
                                        return Some(Ok(JsonToken {
                                            span: Span {
                                                start,
                                                end: self.get_current_position(),
                                            },
                                            kind: JsonTokenKind::Comma,
                                        }));
                                    }
                                    JsonParseState::Value
                                    | JsonParseState::AfterValue
                                    | JsonParseState::KeyValuePairColon
                                    | JsonParseState::KeyValuePairKey => {
                                        return Some(Err(JsonParseErr::UnexpectedCharacters(
                                            self.recover_in_panic_mode(),
                                        )));
                                    }
                                }
                            }
                        }
                        JsonParseState::Array => {
                            self.match_whitespace();
                            if self.match_char(']') {
                                return Some(Ok(JsonToken {
                                    span: Span {
                                        start: self.get_current_position(),
                                        end: self.peek_position(),
                                    },
                                    kind: JsonTokenKind::ArrayEnd,
                                }));
                            }

                            self.states.push(JsonParseState::Array);
                            self.states.push(JsonParseState::Value);
                        }
                    }
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct JsonToken {
    pub(crate) span: Span,
    pub(crate) kind: JsonTokenKind,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct Span {
    pub(crate) start: Position,
    end: Position,
}

impl Span {
    pub(crate) fn as_range(&self) -> Range<usize> {
        self.start.raw..self.end.raw
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Position {
    line: usize,
    col: usize,
    raw: usize,
}

impl Position {
    pub(crate) fn as_index(&self) -> usize {
        self.raw
    }

    /// this function assumes that you know the
    /// value is on the same line. It will panic
    /// if you subtract more than the number of
    /// columns in the line so far.
    fn minus(&self, amount: usize) -> Self {
        Self {
            line: self.line,
            col: self.col - amount,
            raw: self.raw - amount,
        }
    }
}

impl Default for Position {
    fn default() -> Self {
        Self {
            line: 1,
            col: 1,
            raw: 0,
        }
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line: {} column: {}", self.line, self.col)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum JsonTokenKind {
    ObjectStart,
    ObjectEnd,
    ArrayStart,
    ArrayEnd,
    Colon,
    Comma,
    String,
    Number,
    True,
    False,
    Null,
}

#[derive(Debug, Clone)]
pub(crate) enum JsonParseErr {
    UnexpectedEOF,
    IllegalLeading0(Position),
    UnexpectedCharacters(Span),
    TrailingComma(Position),
    InvalidUnicodeEscapeSequence(Span),
    UnclosedString(Position),
    DuplicateObjectKeys(Position, Position),
}

impl Error for JsonParseErr {}
impl Display for JsonParseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = String::new();
        match self {
            JsonParseErr::UnexpectedEOF => {
                result.push_str("Unexpected EOF.");
            }
            JsonParseErr::IllegalLeading0(location) => {
                result.push_str(&format!("found illegal leading 0 at {}", location));
            }
            JsonParseErr::UnexpectedCharacters(span) => {
                result.push_str(&format!(
                    "Found unexpected character at {}. Entered panic mode, resynchronizing at {}",
                    span.start, span.end
                ));
            }
            JsonParseErr::TrailingComma(position) => {
                result.push_str("Found illegal trailing comma at ");
                result.push_str(&format!("{}", position));
            }
            JsonParseErr::InvalidUnicodeEscapeSequence(span) => {
                result.push_str("Found invalid unicode escape sequence at ");
                result.push_str(&format!("{}", span.start));
            }
            JsonParseErr::UnclosedString(position) => {
                result.push_str("Found unclosed string at ");
                result.push_str(&format!("{}", position));
            }
            JsonParseErr::DuplicateObjectKeys(start1, start2) => {
                result.push_str("Found duplicate object keys at ");
                result.push_str(&format!("{}", start1));
                result.push_str(" and ");
                result.push_str(&format!("{}", start2));
            }
        }
        f.write_str(&result)?;
        Ok(())
    }
}
