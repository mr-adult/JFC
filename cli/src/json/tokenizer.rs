use std::{
    collections::VecDeque, error::Error, fmt::Display, iter::Peekable, ops::Range, str::CharIndices,
};

use generic_tokenizer::{CharLocations, Location};

use super::JsonParseState;

pub(crate) struct JsonTokenizer<'json> {
    source_len: usize,
    chars: Peekable<CharLocations<CharIndices<'json>>>,
    last_position: Location,
    states: Vec<JsonParseState>,
    lookahead: Option<JsonToken>,
    unicode_escape_errs: VecDeque<JsonParseErr>,
}

impl<'json> JsonTokenizer<'json> {
    pub fn new(source: &'json str) -> Self {
        let states = vec![JsonParseState::Value];
        Self {
            source_len: source.len(),
            chars: CharLocations::new(source.char_indices()).peekable(),
            last_position: Location::default(),
            states,
            lookahead: None,
            unicode_escape_errs: VecDeque::with_capacity(0),
        }
    }

    fn match_whitespace(&mut self) {
        self.match_char_while(|ch| matches!(ch, ' ' | '\n' | '\r' | '\t'));
    }

    fn match_string(&mut self) -> Result<JsonToken, JsonParseErr> {
        let start = self.peek_position();
        match self.peek_char() {
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
                            let end = self.peek_position();
                            return Ok(JsonToken {
                                span: Span { start, end },
                                kind: JsonTokenKind::String,
                            });
                        }
                        '\\' => {
                            if self.match_char('u') {
                                for _ in 0..4 {
                                    if !self.match_char_if(|ch| ch.is_ascii_hexdigit()) {
                                        let peeked = self.peek_position();
                                        self.unicode_escape_errs.push_back(
                                            JsonParseErr::InvalidUnicodeEscapeSequence(Span {
                                                start: ch.0,
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
        let leading_zero_pos = self.peek_position();
        if self.match_char('0') {
            if self.match_char_if(|ch| ch.is_ascii_digit()) {
                leading_0_err = Some(JsonParseErr::IllegalLeading0(leading_zero_pos));
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
        match self.peek_char() {
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

    fn next_char(&mut self) -> Option<(Location, char)> {
        let return_val = self.chars.next();
        if let Some((loc, _)) = &return_val {
            self.last_position = loc.clone();
        } else if self.last_position.byte_index() != self.source_len {
            self.last_position.increment();
        }
        return_val
    }

    fn peek_char(&mut self) -> Option<&(Location, char)> {
        let return_val = self.chars.peek();
        if let Some((loc, _)) = &return_val {
            self.last_position = loc.clone();
        } else if self.last_position.byte_index() != self.source_len {
            self.last_position.increment();
        }
        return_val
    }

    fn peek_position(&mut self) -> Location {
        self.peek_char()
            .and_then(|char_loc| Some(char_loc.0.clone()))
            .unwrap_or_else(|| self.last_position.clone())
    }

    /// Tries to recover from an unexpected character using panic mode.
    /// We resynchronize on ']', '}', and ',' as these 3 are points of known, recoverable states.
    fn recover_in_panic_mode(&mut self) -> Span {
        let start = self.peek_position();

        loop {
            match self.peek_char() {
                None => {
                    self.next_char();
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

                                let bracket_start = self.peek_position();
                                self.next_char();
                                self.lookahead = Some(JsonToken {
                                    span: Span {
                                        start: bracket_start.clone(),
                                        end: self.peek_position(),
                                    },
                                    kind: JsonTokenKind::ArrayEnd,
                                });

                                return Span {
                                    start,
                                    end: bracket_start,
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

                                let bracket_start = self.peek_position();
                                self.next_char();
                                self.lookahead = Some(JsonToken {
                                    span: Span {
                                        start: bracket_start.clone(),
                                        end: self.peek_position(),
                                    },
                                    kind: JsonTokenKind::ObjectEnd,
                                });

                                return Span {
                                    start,
                                    end: bracket_start,
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

                            let before_comma = self.peek_position();
                            // always match the comma.
                            self.next_char();
                            self.lookahead = Some(JsonToken {
                                span: Span {
                                    start: before_comma.clone(),
                                    end: self.peek_position(),
                                },
                                kind: JsonTokenKind::Comma,
                            });

                            return Span {
                                start,
                                end: before_comma,
                            };
                        }
                        ':' => {
                            // Found object end. Resynchronize
                            if !self
                                .states
                                .iter()
                                .rev()
                                .any(|state| *state == JsonParseState::KeyValuePairColon)
                            {
                                // consume the character, but we didn't have any arrays
                                // so we need to keep panicking.
                                self.next_char();
                                continue;
                            } else {
                                while let Some(state) = self.states.pop() {
                                    if let JsonParseState::KeyValuePairColon = state {
                                        break;
                                    }
                                }

                                let colon_start = self.peek_position();
                                self.next_char();
                                self.lookahead = Some(JsonToken {
                                    span: Span {
                                        start: colon_start.clone(),
                                        end: self.peek_position(),
                                    },
                                    kind: JsonTokenKind::ObjectEnd,
                                });

                                return Span {
                                    start,
                                    end: colon_start,
                                };
                            }
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
                    let before_comma = self.peek_position();
                    if self.match_char(',') {
                        // trailing commas are common. Make sure we don't choke on them.
                        return Some(Err(JsonParseErr::TrailingComma(before_comma)));
                    }
                    self.match_whitespace();
                    self.states.push(JsonParseState::Value);
                }
                Some(state) => {
                    match state {
                        JsonParseState::Value => {
                            self.match_whitespace();
                            match self.peek_char().cloned() {
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

                                            let before_bracket = self.peek_position();
                                            self.next_char();
                                            let span = Span {
                                                start: before_bracket,
                                                end: self.peek_position(),
                                            };
                                            return Some(Ok(JsonToken {
                                                span,
                                                kind: JsonTokenKind::ObjectStart,
                                            }));
                                        }
                                        '[' => {
                                            self.states.push(JsonParseState::Array);
                                            let before_bracket = self.peek_position();
                                            self.next_char();
                                            let start = before_bracket;
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
                                            let before_match = self.peek_position();

                                            if self.match_literal("true") {
                                                return Some(Ok(JsonToken {
                                                    span: Span {
                                                        start: before_match,
                                                        end: self.peek_position(),
                                                    },
                                                    kind: JsonTokenKind::True,
                                                }));
                                            }

                                            if self.match_literal("false") {
                                                return Some(Ok(JsonToken {
                                                    span: Span {
                                                        start: before_match,
                                                        end: self.peek_position(),
                                                    },
                                                    kind: JsonTokenKind::False,
                                                }));
                                            }

                                            if self.match_literal("null") {
                                                return Some(Ok(JsonToken {
                                                    span: Span {
                                                        start: before_match,
                                                        end: self.peek_position(),
                                                    },
                                                    kind: JsonTokenKind::Null,
                                                }));
                                            }

                                            // re-push value back onto the stack, removing AfterValue
                                            self.states.pop();
                                            self.states.push(JsonParseState::Value);
                                            let mut span = self.recover_in_panic_mode();
                                            span.start = before_match;
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
                            let before_bracket = self.peek_position();
                            if self.match_char('}') {
                                return Some(Ok(JsonToken {
                                    span: Span {
                                        start: before_bracket,
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
                            let before_colon = self.peek_position();
                            if self.match_char(':') {
                                return Some(Ok(JsonToken {
                                    span: Span {
                                        start: before_colon,
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
                            let start = self.peek_position();
                            if self.match_char(',') {
                                let after_comma = self.peek_position();
                                self.match_whitespace();
                                if let Some((_, '}' | ']')) = self.peek_char() {
                                    return Some(Err(
                                        // trailing commas are common. Make sure we don't choke on them.
                                        JsonParseErr::TrailingComma(start),
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
                                                start: start,
                                                end: after_comma,
                                            },
                                            kind: JsonTokenKind::Comma,
                                        }));
                                    }
                                    JsonParseState::Array => {
                                        self.states.push(JsonParseState::Value);
                                        return Some(Ok(JsonToken {
                                            span: Span {
                                                start,
                                                end: after_comma,
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
                            let before_bracket = self.peek_position();
                            if self.match_char(']') {
                                return Some(Ok(JsonToken {
                                    span: Span {
                                        start: before_bracket,
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
pub struct JsonToken {
    pub(crate) span: Span,
    pub(crate) kind: JsonTokenKind,
}

impl JsonToken {
    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn kind(&self) -> JsonTokenKind {
        self.kind
    }
}

#[derive(Clone, Debug, Default)]
pub struct Span {
    pub(crate) start: Location,
    pub(crate) end: Location,
}

impl Span {
    pub fn as_range(&self) -> Range<usize> {
        self.start.byte_index()..self.end.byte_index()
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum JsonTokenKind {
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
    IllegalLeading0(Location),
    UnexpectedCharacters(Span),
    TrailingComma(Location),
    InvalidUnicodeEscapeSequence(Span),
    UnclosedString(Location),
    DuplicateObjectKeys(Span, Span),
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
            JsonParseErr::DuplicateObjectKeys(span1, span2) => {
                result.push_str("Found duplicate object keys at ");
                result.push_str(&format!("{}", span1.start));
                result.push_str(" and ");
                result.push_str(&format!("{}", span2.start));
            }
        }
        f.write_str(&result)?;
        Ok(())
    }
}
