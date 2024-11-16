use std::marker::PhantomData;

use serde::Deserialize;

use crate::{
    de::CharsDeserializer,
    tokenizer::{ErrKind, Error, TokenKind},
};

/// A struct for lazily deserializing a series of key value pairs
/// from a JSON character stream.
pub struct KeyValuePairStream<T, CharsStream, PassThroughError>
where
    T: for<'de> Deserialize<'de>,
    CharsStream: IntoIterator<Item = Result<char, PassThroughError>>,
    PassThroughError: std::error::Error,
{
    phantom: PhantomData<T>,
    stream: CharsDeserializer<CharsStream, PassThroughError>,
    state: KeyValuePairState,
}

enum KeyValuePairState {
    Start,
    Key,
    Colon,
    Value,
    AfterValue,
    End,
}

impl<T, CharsStream, PassThroughError> KeyValuePairStream<T, CharsStream, PassThroughError>
where
    T: for<'de> Deserialize<'de>,
    CharsStream: IntoIterator<Item = Result<char, PassThroughError>>,
    PassThroughError: std::error::Error,
{
    pub(crate) fn new(stream: CharsStream) -> Self {
        Self {
            phantom: PhantomData::default(),
            stream: CharsDeserializer::from_stream(stream),
            state: KeyValuePairState::Start,
        }
    }
}

impl<T, CharsStream, PassThroughError> Iterator
    for KeyValuePairStream<T, CharsStream, PassThroughError>
where
    T: for<'de> Deserialize<'de>,
    CharsStream: IntoIterator<Item = Result<char, PassThroughError>>,
    PassThroughError: std::error::Error,
{
    type Item = Result<(String, T), Error<PassThroughError>>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut key = String::with_capacity(0);
        loop {
            match self.state {
                KeyValuePairState::Start => {
                    let token_result = self
                        .stream
                        .next()
                        .unwrap_or_else(|| Err(self.stream.unexpected_eof()));

                    let token = match token_result {
                        Err(err) => {
                            self.state = KeyValuePairState::End;
                            return Some(Err(err));
                        }
                        Ok(token) => token,
                    };

                    if let TokenKind::ObjectStart = token.kind {
                        self.state = KeyValuePairState::Key;
                    } else {
                        self.state = KeyValuePairState::End;
                        return Some(Err(Error {
                            location: token.span.start,
                            kind: ErrKind::UnexpectedToken {
                                expected: vec![TokenKind::ObjectStart],
                                actual: Some(token.kind),
                            },
                        }));
                    }
                }
                KeyValuePairState::Key => {
                    let token_result = self
                        .stream
                        .next()
                        .unwrap_or_else(|| Err(self.stream.unexpected_eof()));

                    let token = match token_result {
                        Err(err) => {
                            self.state = KeyValuePairState::End;
                            return Some(Err(err));
                        }
                        Ok(token) => token,
                    };

                    if let TokenKind::String = token.kind {
                        key = token.string;
                        self.state = KeyValuePairState::Colon;
                    } else {
                        self.state = KeyValuePairState::End;
                        return Some(Err(Error {
                            location: token.span.start,
                            kind: ErrKind::UnexpectedToken {
                                expected: vec![TokenKind::String],
                                actual: Some(token.kind),
                            },
                        }));
                    }
                }
                KeyValuePairState::Colon => {
                    let token_result = self
                        .stream
                        .next()
                        .unwrap_or_else(|| Err(self.stream.unexpected_eof()));

                    let token = match token_result {
                        Err(err) => {
                            self.state = KeyValuePairState::End;
                            return Some(Err(err));
                        }
                        Ok(token) => token,
                    };

                    if let TokenKind::Colon = token.kind {
                        self.state = KeyValuePairState::Value;
                    } else {
                        self.state = KeyValuePairState::End;
                        return Some(Err(Error {
                            location: token.span.start,
                            kind: ErrKind::UnexpectedToken {
                                expected: vec![TokenKind::Colon],
                                actual: Some(token.kind),
                            },
                        }));
                    }
                }
                KeyValuePairState::Value => {
                    let result = T::deserialize(&mut self.stream);
                    match result {
                        Err(err) => {
                            self.state = KeyValuePairState::End;
                            return Some(Err(err));
                        }
                        Ok(value) => {
                            self.state = KeyValuePairState::AfterValue;
                            return Some(Ok((key, value)));
                        }
                    }
                }
                KeyValuePairState::AfterValue => {
                    let token_result = self
                        .stream
                        .next()
                        .unwrap_or_else(|| Err(self.stream.unexpected_eof()));

                    let token = match token_result {
                        Err(err) => {
                            self.state = KeyValuePairState::End;
                            return Some(Err(err));
                        }
                        Ok(token) => token,
                    };

                    if let TokenKind::ObjectEnd = token.kind {
                        return None;
                    }

                    if let TokenKind::Comma = token.kind {
                        self.state = KeyValuePairState::Key;
                    } else {
                        self.state = KeyValuePairState::End;
                        return Some(Err(Error {
                            location: token.span.start,
                            kind: ErrKind::UnexpectedToken {
                                expected: vec![TokenKind::Comma],
                                actual: Some(token.kind),
                            },
                        }));
                    }
                }
                KeyValuePairState::End => {
                    return None;
                }
            }
        }
    }
}
