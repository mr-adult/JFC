use std::marker::PhantomData;

use serde::Deserialize;

use crate::{
    de::CharsDeserializer,
    tokenizer::{ErrKind, Error, TokenKind},
};

pub struct ValueStream<T, CharsStream, PassThroughError>
where
    T: for<'de> Deserialize<'de>,
    CharsStream: IntoIterator<Item = Result<char, PassThroughError>>,
    PassThroughError: std::error::Error,
{
    phantom: PhantomData<T>,
    state: ValueStreamState,
    stream: CharsDeserializer<CharsStream, PassThroughError>,
}

enum ValueStreamState {
    Start,
    BeforeComma,
    AfterComma,
    End,
}

impl<T, CharsStream, PassThroughError> ValueStream<T, CharsStream, PassThroughError>
where
    T: for<'de> Deserialize<'de>,
    CharsStream: IntoIterator<Item = Result<char, PassThroughError>>,
    PassThroughError: std::error::Error,
{
    pub(crate) fn new(stream: CharsStream) -> Self {
        Self {
            phantom: PhantomData::default(),
            state: ValueStreamState::Start,
            stream: CharsDeserializer::from_stream(stream),
        }
    }
}

impl<T, CharsStream, PassThroughError> Iterator for ValueStream<T, CharsStream, PassThroughError>
where
    T: for<'de> Deserialize<'de>,
    CharsStream: IntoIterator<Item = Result<char, PassThroughError>>,
    PassThroughError: std::error::Error,
{
    type Item = Result<T, Error<PassThroughError>>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.state {
                ValueStreamState::Start => {
                    let token_result = self
                        .stream
                        .next()
                        .unwrap_or_else(|| Err(self.stream.unexpected_eof()));

                    let token = match token_result {
                        Err(err) => {
                            self.state = ValueStreamState::End;
                            return Some(Err(err));
                        }
                        Ok(token) => token,
                    };

                    if let TokenKind::ArrayStart = token.kind {
                        self.state = ValueStreamState::AfterComma;
                    } else {
                        self.state = ValueStreamState::End;
                        return Some(Err(Error {
                            location: token.span.start,
                            kind: ErrKind::UnexpectedToken {
                                expected: vec![TokenKind::ArrayStart],
                                actual: Some(token.kind),
                            },
                        }));
                    }
                }
                ValueStreamState::BeforeComma => {
                    let token_result = self
                        .stream
                        .next()
                        .unwrap_or_else(|| Err(self.stream.unexpected_eof()));

                    let token = match token_result {
                        Err(err) => {
                            self.state = ValueStreamState::End;
                            return Some(Err(err));
                        }
                        Ok(token) => token,
                    };

                    if let TokenKind::ArrayEnd = token.kind {
                        return None;
                    }

                    if let TokenKind::Comma = token.kind {
                        self.state = ValueStreamState::AfterComma;
                    } else {
                        self.state = ValueStreamState::End;
                        return Some(Err(Error {
                            location: token.span.start,
                            kind: ErrKind::UnexpectedToken {
                                expected: vec![TokenKind::Comma],
                                actual: Some(token.kind),
                            },
                        }));
                    }
                }
                ValueStreamState::AfterComma => {
                    let result = T::deserialize(&mut self.stream);
                    match result {
                        Err(err) => {
                            self.state = ValueStreamState::End;
                            return Some(Err(err));
                        }
                        Ok(value) => {
                            self.state = ValueStreamState::BeforeComma;
                            return Some(Ok(value));
                        }
                    }
                }

                ValueStreamState::End => {
                    return None;
                }
            }
        }
    }
}
