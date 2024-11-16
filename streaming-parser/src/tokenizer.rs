use std::iter::Peekable;

use crate::{char_locations::CharLocations, Location};

pub(crate) struct Tokenizer<CharsStream, PassThroughError>
where
    CharsStream: IntoIterator<Item = Result<char, PassThroughError>>,
    PassThroughError: std::fmt::Display,
{
    chars: Peekable<CharLocations<CharsStream::IntoIter, PassThroughError>>,
    last_position: Location,
    done: bool,
    states: Vec<JsonParseState>,
    lookahead: Option<Token>,
}

impl<CharsStream, PassThroughError> Tokenizer<CharsStream, PassThroughError>
where
    CharsStream: IntoIterator<Item = Result<char, PassThroughError>>,
    PassThroughError: std::fmt::Display,
{
    pub(crate) fn new(source: CharsStream) -> Self
    where
        CharsStream: IntoIterator<Item = Result<char, PassThroughError>>,
    {
        let states = vec![JsonParseState::Value];
        Self {
            chars: CharLocations::new(source.into_iter()).peekable(),
            last_position: Location::default(),
            done: false,
            states,
            lookahead: None,
        }
    }

    fn match_whitespace(&mut self) {
        self.match_char_while(|ch| matches!(ch, ' ' | '\n' | '\r' | '\t'));
    }

    fn match_string(&mut self) -> Result<Token, Error<PassThroughError>> {
        let start = self.peek_location();
        match self.peek_char() {
            None => {
                self.done = true;
                return Err(Error {
                    location: self.peek_location(),
                    kind: ErrKind::UnexpectedEOF,
                });
            }
            Some(_) => {
                if !self.match_char('"') {
                    self.done = true;

                    match self.peek_char().unwrap().1 {
                        Err(_) => {
                            let next_err = self.next_char().unwrap();
                            return Err(Error {
                                location: next_err.0,
                                kind: if let Err(err) = next_err.1 {
                                    ErrKind::PassThrough(err)
                                } else {
                                    unreachable!()
                                },
                            });
                        }
                        Ok(ch_loc) => {
                            return Err(Error {
                                location: self.next_char().unwrap().0,
                                kind: ErrKind::UnexpectedChar {
                                    expected: vec!['"'],
                                    actual: Some(ch_loc),
                                },
                            });
                        }
                    }
                }
            }
        };

        let mut result = String::new();

        loop {
            match self.next_char() {
                None => {
                    self.done = true;
                    return Err(Error {
                        location: self.peek_location(),
                        kind: ErrKind::UnclosedString,
                    });
                }
                Some(ch) => {
                    let ch = match ch.1 {
                        Ok(ch) => ch,
                        Err(err) => {
                            return Err(Error {
                                location: ch.0,
                                kind: ErrKind::PassThrough(err),
                            });
                        }
                    };

                    match ch {
                        '"' => {
                            let end = self.peek_location();
                            return Ok(Token {
                                string: result,
                                span: Span { start, end },
                                kind: TokenKind::String,
                            });
                        }
                        '\\' => {
                            if self.match_char('u') {
                                let mut buf = String::with_capacity(4);
                                let unicode_location = self.peek_location();

                                for _ in 0..4 {
                                    if let Some(ascii_digit) =
                                        self.match_char_if(|ch| ch.is_ascii_digit())
                                    {
                                        buf.push(ascii_digit.1);
                                    } else {
                                        self.done = true;
                                        return Err(Error {
                                            location: self.peek_location(),
                                            kind: ErrKind::InvalidUnicodeEscapeSequence,
                                        });
                                    }
                                }

                                let code = buf.parse::<u32>().unwrap();
                                if let Some(ch) = char::from_u32(code) {
                                    result.push(ch);
                                } else {
                                    self.done = true;
                                    return Err(Error {
                                        location: unicode_location,
                                        kind: ErrKind::InvalidUnicodeEscapeSequence,
                                    });
                                }
                            }

                            // we're just tokenizing, not interpreting the value's escape sequences.
                            // no need to handle unicode escape sequences.
                            self.match_char_if(|ch| {
                                match ch {
                                    '"' | '\\' | '/' => {
                                        result.push(ch);
                                        true
                                    }
                                    'b' => {
                                        result.push('\u{0008}'); // backspace (0x08)
                                        true
                                    }
                                    'f' => {
                                        result.push('\u{000C}'); // form feed (0x0C)
                                        true
                                    }
                                    'n' => {
                                        result.push('\n');
                                        true
                                    }
                                    'r' => {
                                        result.push('\r');
                                        true
                                    }
                                    't' => {
                                        result.push('\t');
                                        true
                                    }
                                    _ => false,
                                }
                            });
                        }
                        '\n' | '\t' => {
                            self.done = true;
                            return Err(Error {
                                location: self.peek_location(),
                                kind: ErrKind::UnclosedString,
                            });
                        }
                        ch => {
                            result.push(ch);
                        } // just continue
                    }
                }
            }
        }
    }

    fn match_number(&mut self) -> Result<Token, Error<PassThroughError>> {
        let start = self.peek_location();
        let mut buf = String::new();
        if self.match_char('-') {
            buf.push('-');
        }

        let leading_zero_pos = self.peek_location();
        if self.match_char('0') {
            if self.match_char_if(|ch| ch.is_ascii_digit()).is_some() {
                self.done = true;
                return Err(Error {
                    location: leading_zero_pos,
                    kind: ErrKind::IllegalLeading0,
                });
            }
            buf.push('0');
        } else if let Some(ch) = self.match_char_if(|ch| ch.is_ascii_digit()) {
            buf.push(ch.1);
        } else {
            // We found only a dash... Error
            self.done = true;

            if let Some(peeked) = self.next_char() {
                match peeked.1 {
                    Ok(ch) => {
                        return Err(Error {
                            location: peeked.0,
                            kind: ErrKind::UnexpectedChar {
                                expected: vec!['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'],
                                actual: Some(ch),
                            },
                        });
                    }
                    Err(err) => {
                        return Err(Error {
                            location: peeked.0,
                            kind: ErrKind::PassThrough(err),
                        });
                    }
                }
            } else {
                return Err(Error {
                    location: self.peek_location(),
                    kind: ErrKind::UnexpectedChar {
                        expected: vec!['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'],
                        actual: None,
                    },
                });
            }
        }

        buf.push_str(&self.match_char_while(|ch| ch.is_ascii_digit()).1);
        if self.match_char('.') {
            buf.push('.');
            buf.push_str(&self.match_char_while(|ch| ch.is_ascii_digit()).1);
        }

        if let Some((_, ch)) = self.match_char_if(|ch| matches!(ch, 'e' | 'E')) {
            buf.push(ch);
            if let Some((_, ch)) = self.match_char_if(|ch| matches!(ch, '+' | '-')) {
                buf.push(ch);
            }

            buf.push_str(&self.match_char_while(|ch| ch.is_ascii_digit()).1);
        }

        let token = Token {
            string: buf,
            span: Span {
                start,
                end: self.peek_location(),
            },
            kind: TokenKind::Number,
        };

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
        self.match_char_if(|ch| ch == char).is_some()
    }

    fn match_char_while<P: FnMut(char) -> bool>(&mut self, mut predicate: P) -> (Span, String) {
        let mut buf = String::new();

        let start_loc = if let Some(ch) = self.match_char_if(&mut |ch| predicate(ch)) {
            buf.push(ch.1);
            ch.0
        } else {
            return (
                Span {
                    start: self.peek_location(),
                    end: self.peek_location(),
                },
                buf,
            );
        };

        loop {
            if let Some(ch) = self.match_char_if(&mut |ch| predicate(ch)) {
                buf.push(ch.1);
            } else {
                return (
                    Span {
                        start: start_loc,
                        end: self.peek_location(),
                    },
                    buf,
                );
            }
        }
    }

    fn match_char_if<P: FnMut(char) -> bool>(
        &mut self,
        mut predicate: P,
    ) -> Option<(Location, char)> {
        match self.peek_char() {
            None => None,
            Some((_, char)) => {
                if let Ok(char) = char {
                    if predicate(*char) {
                        let next = self.next_char();
                        let loc_result = next.unwrap();
                        if let Ok(char) = loc_result.1 {
                            return Some((loc_result.0, char));
                        } else {
                            unreachable!("We already proved that this was an Ok() value.")
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        }
    }

    fn next_char(&mut self) -> Option<(Location, Result<char, PassThroughError>)> {
        let return_val = self.chars.next();
        if let Some((loc, _)) = &return_val {
            self.last_position = loc.clone();
        }
        return_val
    }

    fn peek_char(&mut self) -> Option<&(Location, Result<char, PassThroughError>)> {
        let return_val = self.chars.peek();
        if let Some((loc, _)) = &return_val {
            self.last_position = loc.clone();
        }
        return_val
    }

    pub(crate) fn peek_location(&mut self) -> Location {
        self.peek_char()
            .and_then(|char_loc| Some(char_loc.0.clone()))
            .unwrap_or_else(|| self.last_position.incremented())
    }
}

impl<CharsStream, PassThroughError> Iterator for Tokenizer<CharsStream, PassThroughError>
where
    CharsStream: IntoIterator<Item = Result<char, PassThroughError>>,
    PassThroughError: std::fmt::Display,
{
    type Item = Result<Token, Error<PassThroughError>>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(lookahead) = std::mem::take(&mut self.lookahead) {
            return Some(Ok(lookahead));
        }

        loop {
            match self.states.pop() {
                None => {
                    self.match_whitespace();
                    let before_comma = self.peek_location();
                    if self.match_char(',') {
                        // trailing commas are common. Make sure we don't choke on them.
                        return Some(Err(Error {
                            location: before_comma,
                            kind: ErrKind::TrailingComma,
                        }));
                    }
                    self.match_whitespace();
                    self.states.push(JsonParseState::Value);
                }
                Some(state) => {
                    match state {
                        JsonParseState::Value => {
                            self.match_whitespace();
                            match self.peek_char() {
                                None => {
                                    if self.states.is_empty() {
                                        return None;
                                    } else {
                                        self.done = true;
                                        return Some(Err(Error {
                                            location: self.peek_location(),
                                            kind: ErrKind::UnexpectedEOF,
                                        }));
                                    }
                                }
                                Some(_) => {
                                    if !self.states.is_empty() {
                                        self.states.push(JsonParseState::AfterValue);
                                    }

                                    let ch = match self.peek_char().unwrap().1 {
                                        Ok(ch) => ch,
                                        Err(_) => {
                                            self.done = true;

                                            let next_ch_location = self.next_char().unwrap();
                                            let err = if let Err(err) = next_ch_location.1 {
                                                err
                                            } else {
                                                unreachable!();
                                            };

                                            return Some(Err(Error {
                                                location: next_ch_location.0,
                                                kind: ErrKind::PassThrough(err),
                                            }));
                                        }
                                    };

                                    match ch {
                                        '{' => {
                                            self.states.push(JsonParseState::Object);

                                            let before_bracket = self.peek_location();
                                            self.next_char();
                                            let span = Span {
                                                start: before_bracket,
                                                end: self.peek_location(),
                                            };
                                            return Some(Ok(Token {
                                                string: "{".to_string(),
                                                span,
                                                kind: TokenKind::ObjectStart,
                                            }));
                                        }
                                        '[' => {
                                            self.states.push(JsonParseState::Array);
                                            let before_bracket = self.peek_location();
                                            self.next_char();
                                            let start = before_bracket;
                                            let span = Span {
                                                start,
                                                end: self.peek_location(),
                                            };
                                            return Some(Ok(Token {
                                                string: "[".to_string(),
                                                span,
                                                kind: TokenKind::ArrayStart,
                                            }));
                                        }
                                        '"' => {
                                            let token_result = self.match_string();
                                            let token = match token_result {
                                                Err(err) => {
                                                    self.done = true;
                                                    return Some(Err(err));
                                                }
                                                Ok(token) => token,
                                            };

                                            return Some(Ok(token));
                                        }
                                        '-' | '0'..='9' => {
                                            return Some(self.match_number());
                                        }
                                        _ => {
                                            let before_match = self.peek_location();
                                            let actual = if let Some(peeked) = self.peek_char() {
                                                match &peeked.1 {
                                                    Ok(ch) => Some(*ch),
                                                    Err(_) => {
                                                        self.done = true;

                                                        let (loc, err) = self.next_char().unwrap();
                                                        let err = if let Err(err) = err {
                                                            err
                                                        } else {
                                                            unreachable!();
                                                        };

                                                        return Some(Err(Error {
                                                            location: loc,
                                                            kind: ErrKind::PassThrough(err),
                                                        }));
                                                    }
                                                }
                                            } else {
                                                None
                                            };

                                            if self.match_literal("true") {
                                                return Some(Ok(Token {
                                                    string: "true".to_string(),
                                                    span: Span {
                                                        start: before_match,
                                                        end: self.peek_location(),
                                                    },
                                                    kind: TokenKind::True,
                                                }));
                                            }

                                            if self.match_literal("false") {
                                                return Some(Ok(Token {
                                                    string: "false".to_string(),
                                                    span: Span {
                                                        start: before_match,
                                                        end: self.peek_location(),
                                                    },
                                                    kind: TokenKind::False,
                                                }));
                                            }

                                            if self.match_literal("null") {
                                                return Some(Ok(Token {
                                                    string: "null".to_string(),
                                                    span: Span {
                                                        start: before_match,
                                                        end: self.peek_location(),
                                                    },
                                                    kind: TokenKind::Null,
                                                }));
                                            }

                                            // re-push value back onto the stack, removing AfterValue
                                            self.states.pop();
                                            self.states.push(JsonParseState::Value);
                                            self.done = true;
                                            return Some(Err(Error {
                                                location: before_match,
                                                kind: ErrKind::UnexpectedChar {
                                                    expected: vec!['t', 'f', 'n'],
                                                    actual,
                                                },
                                            }));
                                        }
                                    }
                                }
                            }
                        }
                        JsonParseState::Object => {
                            self.match_whitespace();
                            let before_bracket = self.peek_location();
                            if self.match_char('}') {
                                return Some(Ok(Token {
                                    string: "}".to_string(),
                                    span: Span {
                                        start: before_bracket,
                                        end: self.peek_location(),
                                    },
                                    kind: TokenKind::ObjectEnd,
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
                            let before_colon = self.peek_location();
                            if self.match_char(':') {
                                return Some(Ok(Token {
                                    string: ":".to_string(),
                                    span: Span {
                                        start: before_colon,
                                        end: self.peek_location(),
                                    },
                                    kind: TokenKind::Colon,
                                }));
                            } else {
                                let actual = match self.next_char() {
                                    None => None,
                                    Some(result) => match result.1 {
                                        Ok(ch) => Some(ch),
                                        Err(err) => {
                                            self.done = true;

                                            return Some(Err(Error {
                                                location: result.0,
                                                kind: ErrKind::PassThrough(err),
                                            }));
                                        }
                                    },
                                };

                                return Some(Err(Error {
                                    location: self.peek_location(),
                                    kind: ErrKind::UnexpectedChar {
                                        expected: vec![':'],
                                        actual,
                                    },
                                }));
                            }
                        }
                        JsonParseState::KeyValuePairKey => {
                            self.match_whitespace();

                            let token_result = self.match_string();
                            let token = match token_result {
                                Err(err) => {
                                    self.done = true;
                                    return Some(Err(err));
                                }
                                Ok(token) => token,
                            };

                            return Some(Ok(token));
                        }
                        JsonParseState::AfterValue => {
                            self.match_whitespace();
                            let start = self.peek_location();
                            if self.match_char(',') {
                                let after_comma = self.peek_location();
                                self.match_whitespace();
                                if let Some((_, Ok('}' | ']'))) = self.peek_char() {
                                    return Some(Err(Error {
                                        location: after_comma,
                                        // trailing commas are common. Make sure we don't choke on them.
                                        kind: ErrKind::TrailingComma,
                                    }));
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
                                        return Some(Ok(Token {
                                            string: ",".to_string(),
                                            span: Span {
                                                start: start,
                                                end: after_comma,
                                            },
                                            kind: TokenKind::Comma,
                                        }));
                                    }
                                    JsonParseState::Array => {
                                        self.states.push(JsonParseState::Value);
                                        return Some(Ok(Token {
                                            string: ",".to_string(),
                                            span: Span {
                                                start,
                                                end: after_comma,
                                            },
                                            kind: TokenKind::Comma,
                                        }));
                                    }
                                    JsonParseState::Value => {
                                        self.done = true;
                                        return Some(Err(Error {
                                            location: self.peek_location(),
                                            kind: ErrKind::UnexpectedChar {
                                                expected: vec!['t', 'f', 'n', '"', '{', '['],
                                                actual: Some(','),
                                            },
                                        }));
                                    }
                                    JsonParseState::AfterValue => {
                                        self.done = true;
                                        return Some(Err(Error {
                                            location: self.peek_location(),
                                            kind: ErrKind::UnexpectedChar {
                                                expected: vec!['}', ']'],
                                                actual: Some(','),
                                            },
                                        }));
                                    }
                                    JsonParseState::KeyValuePairColon => {
                                        self.done = true;
                                        return Some(Err(Error {
                                            location: self.peek_location(),
                                            kind: ErrKind::UnexpectedChar {
                                                expected: vec![':'],
                                                actual: Some(','),
                                            },
                                        }));
                                    }
                                    JsonParseState::KeyValuePairKey => {
                                        self.done = true;
                                        return Some(Err(Error {
                                            location: self.peek_location(),
                                            kind: ErrKind::UnexpectedChar {
                                                expected: vec!['"'],
                                                actual: Some(','),
                                            },
                                        }));
                                    }
                                }
                            }
                        }
                        JsonParseState::Array => {
                            self.match_whitespace();
                            let before_bracket = self.peek_location();
                            if self.match_char(']') {
                                return Some(Ok(Token {
                                    string: "]".to_string(),
                                    span: Span {
                                        start: before_bracket,
                                        end: self.peek_location(),
                                    },
                                    kind: TokenKind::ArrayEnd,
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
pub(crate) struct Token {
    pub(crate) string: String,
    pub(crate) span: Span,
    pub(crate) kind: TokenKind,
}

#[derive(Clone, Debug)]
pub(crate) struct Span {
    pub(crate) start: Location,
    pub(crate) end: Location,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum TokenKind {
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

#[derive(Debug)]
pub struct Error<PassThroughErr> {
    pub(crate) location: Location,
    pub kind: ErrKind<PassThroughErr>,
}

#[derive(Debug)]
pub enum ErrKind<PassThroughErr> {
    PassThrough(PassThroughErr),
    UnexpectedEOF,
    IllegalLeading0,
    TrailingComma,
    InvalidUnicodeEscapeSequence,
    UnclosedString,
    UnexpectedChar {
        expected: Vec<char>,
        actual: Option<char>,
    },
    UnexpectedToken {
        expected: Vec<TokenKind>,
        actual: Option<TokenKind>,
    },
    IntegerOverflow(String),
    StringFailedCastToChar(String),
    Custom(String),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub(crate) enum JsonParseState {
    Object,
    Array,
    KeyValuePairColon,
    KeyValuePairKey,
    Value,
    AfterValue,
}
