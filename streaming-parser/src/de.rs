use crate::tokenizer::{ErrKind, Error, Token, TokenKind};
use serde::de::{
    self, DeserializeSeed, EnumAccess, IntoDeserializer, MapAccess, SeqAccess, VariantAccess,
    Visitor,
};

use crate::tokenizer::Tokenizer;
use crate::Location;

pub(crate) struct CharsDeserializer<CharsStream, PassThroughError>
where
    CharsStream: IntoIterator<Item = Result<char, PassThroughError>>,
    PassThroughError: std::fmt::Display,
{
    pub(crate) input: Tokenizer<CharsStream, PassThroughError>,
    pub(crate) peeked: Option<Token>,
}

impl<CharsStream, PassThroughError> CharsDeserializer<CharsStream, PassThroughError>
where
    CharsStream: IntoIterator<Item = Result<char, PassThroughError>>,
    PassThroughError: std::error::Error,
{
    pub(crate) fn from_stream(input: CharsStream) -> Self {
        Self {
            input: Tokenizer::new(input),
            peeked: None,
        }
    }

    pub(crate) fn next(&mut self) -> Option<Result<Token, Error<PassThroughError>>> {
        if let Some(peeked) = std::mem::take(&mut self.peeked) {
            Some(Ok(peeked))
        } else {
            self.input.next()
        }
    }

    pub(crate) fn unexpected_eof(&mut self) -> Error<PassThroughError> {
        Error {
            location: self.input.peek_location(),
            kind: ErrKind::UnexpectedEOF,
        }
    }
}

impl<'de, 'a, CharsStream, PassThroughError> de::Deserializer<'de>
    for &'a mut CharsDeserializer<CharsStream, PassThroughError>
where
    CharsStream: IntoIterator<Item = Result<char, PassThroughError>>,
    PassThroughError: std::error::Error,
{
    type Error = Error<PassThroughError>;

    // Look at the input data to decide what Serde data model type to
    // deserialize as. Not all data formats are able to support this operation.
    // Formats that support `deserialize_any` are known as self-describing.
    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_any");
        let token = self.next().unwrap_or_else(|| Err(self.unexpected_eof()))?;

        match token.kind {
            TokenKind::ObjectStart => {
                self.peeked = Some(token);
                self.deserialize_map(visitor)
            }
            TokenKind::ArrayStart => {
                self.peeked = Some(token);
                self.deserialize_seq(visitor)
            }
            TokenKind::String => visitor.visit_string(token.string),
            TokenKind::Number => {
                if token.string.starts_with('-') {
                    let val = token.string.parse::<i64>().map_err(|_| Error {
                        location: token.span.start,
                        kind: ErrKind::IntegerOverflow(token.string),
                    })?;
                    visitor.visit_i64(val)
                } else {
                    let val = token.string.parse::<u64>().map_err(|_| Error {
                        location: token.span.start,
                        kind: ErrKind::IntegerOverflow(token.string),
                    })?;
                    visitor.visit_u64(val)
                }
            }
            TokenKind::True => visitor.visit_bool(true),
            TokenKind::False => visitor.visit_bool(false),
            TokenKind::Null => {
                self.peeked = Some(token);
                self.deserialize_unit(visitor)
            }
            TokenKind::Comma | TokenKind::Colon | TokenKind::ObjectEnd | TokenKind::ArrayEnd => {
                Err(Error {
                    location: token.span.start,
                    kind: ErrKind::UnexpectedToken {
                        expected: vec![
                            TokenKind::ObjectStart,
                            TokenKind::ArrayStart,
                            TokenKind::String,
                            TokenKind::Number,
                            TokenKind::True,
                            TokenKind::False,
                            TokenKind::Null,
                        ],
                        actual: Some(token.kind),
                    },
                })
            }
        }
    }

    // Uses the `parse_bool` parsing function defined above to read the JSON
    // identifier `true` or `false` from the input.
    //
    // Parsing refers to looking at the input and deciding that it contains the
    // JSON value `true` or `false`.
    //
    // Deserialization refers to mapping that JSON value into Serde's data
    // model by invoking one of the `Visitor` methods. In the case of JSON and
    // bool that mapping is straightforward so the distinction may seem silly,
    // but in other cases Deserializers sometimes perform non-obvious mappings.
    // For example the TOML format has a Datetime type and Serde's data model
    // does not. In the `toml` crate, a Datetime in the input is deserialized by
    // mapping it to a Serde data model "struct" type with a special name and a
    // single field containing the Datetime represented as a string.
    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_bool");
        let token = self.next().unwrap_or_else(|| Err(self.unexpected_eof()))?;
        match token.kind {
            TokenKind::True => visitor.visit_bool(true),
            TokenKind::False => visitor.visit_bool(false),
            _ => Err(Error {
                location: token.span.start,
                kind: ErrKind::UnexpectedToken {
                    expected: vec![TokenKind::True, TokenKind::False],
                    actual: Some(token.kind),
                },
            }),
        }
    }

    // The `parse_signed` function is generic over the integer type `T` so here
    // it is invoked with `T=i8`. The next 8 methods are similar.
    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_i8");
        let token = self.next().unwrap_or_else(|| Err(self.unexpected_eof()))?;

        if let TokenKind::Number = token.kind {
            let val = token.string.parse::<i8>().map_err(|_| Error {
                location: token.span.start,
                kind: ErrKind::IntegerOverflow(token.string),
            })?;
            visitor.visit_i8(val)
        } else {
            return Err(Error {
                location: token.span.start,
                kind: ErrKind::UnexpectedToken {
                    expected: vec![TokenKind::Number],
                    actual: Some(token.kind),
                },
            });
        }
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_i16");
        let token = self.next().unwrap_or_else(|| Err(self.unexpected_eof()))?;

        if let TokenKind::Number = token.kind {
            let val = token.string.parse::<i16>().map_err(|_| Error {
                location: token.span.start,
                kind: ErrKind::IntegerOverflow(token.string),
            })?;
            visitor.visit_i16(val)
        } else {
            return Err(Error {
                location: token.span.start,
                kind: ErrKind::UnexpectedToken {
                    expected: vec![TokenKind::Number],
                    actual: Some(token.kind),
                },
            });
        }
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_i32");
        let token = self.next().unwrap_or_else(|| Err(self.unexpected_eof()))?;
        if let TokenKind::Number = token.kind {
            let val = token.string.parse::<i32>().map_err(|_| Error {
                location: token.span.start,
                kind: ErrKind::IntegerOverflow(token.string),
            })?;
            visitor.visit_i32(val)
        } else {
            return Err(Error {
                location: token.span.start,
                kind: ErrKind::UnexpectedToken {
                    expected: vec![TokenKind::Number],
                    actual: Some(token.kind),
                },
            });
        }
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_i64");
        let token = self.next().unwrap_or_else(|| Err(self.unexpected_eof()))?;

        if let TokenKind::Number = token.kind {
            let val = token.string.parse::<i64>().map_err(|_| Error {
                location: token.span.start,
                kind: ErrKind::IntegerOverflow(token.string),
            })?;
            visitor.visit_i64(val)
        } else {
            return Err(Error {
                location: token.span.start,
                kind: ErrKind::UnexpectedToken {
                    expected: vec![TokenKind::Number],
                    actual: Some(token.kind),
                },
            });
        }
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_u8");
        let token = self.next().unwrap_or_else(|| Err(self.unexpected_eof()))?;

        if let TokenKind::Number = token.kind {
            let val = token.string.parse::<u8>().map_err(|_| Error {
                location: token.span.start,
                kind: ErrKind::IntegerOverflow(token.string),
            })?;
            visitor.visit_u8(val)
        } else {
            return Err(Error {
                location: token.span.start,
                kind: ErrKind::UnexpectedToken {
                    expected: vec![TokenKind::Number],
                    actual: Some(token.kind),
                },
            });
        }
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_u16");
        let token = self.next().unwrap_or_else(|| Err(self.unexpected_eof()))?;

        if let TokenKind::Number = token.kind {
            let val = token.string.parse::<u16>().map_err(|_| Error {
                location: token.span.start,
                kind: ErrKind::IntegerOverflow(token.string),
            })?;
            visitor.visit_u16(val)
        } else {
            return Err(Error {
                location: token.span.start,
                kind: ErrKind::UnexpectedToken {
                    expected: vec![TokenKind::Number],
                    actual: Some(token.kind),
                },
            });
        }
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_u32");
        let token = self.next().unwrap_or_else(|| Err(self.unexpected_eof()))?;

        if let TokenKind::Number = token.kind {
            let val = token.string.parse::<u32>().map_err(|_| Error {
                location: token.span.start,
                kind: ErrKind::IntegerOverflow(token.string),
            })?;
            visitor.visit_u32(val)
        } else {
            return Err(Error {
                location: token.span.start,
                kind: ErrKind::UnexpectedToken {
                    expected: vec![TokenKind::Number],
                    actual: Some(token.kind),
                },
            });
        }
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_u64");
        let token = self.next().unwrap_or_else(|| Err(self.unexpected_eof()))?;

        if let TokenKind::Number = token.kind {
            let val = token.string.parse::<u64>().map_err(|_| Error {
                location: token.span.start,
                kind: ErrKind::IntegerOverflow(token.string),
            })?;
            visitor.visit_u64(val)
        } else {
            return Err(Error {
                location: token.span.start,
                kind: ErrKind::UnexpectedToken {
                    expected: vec![TokenKind::Number],
                    actual: Some(token.kind),
                },
            });
        }
    }

    // Float parsing is stupidly hard.
    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_f32");
        let token = self.next().unwrap_or_else(|| Err(self.unexpected_eof()))?;

        if let TokenKind::Number = token.kind {
            let val = token.string.parse::<f32>().map_err(|_| Error {
                location: token.span.start,
                kind: ErrKind::IntegerOverflow(token.string),
            })?;
            visitor.visit_f32(val)
        } else {
            return Err(Error {
                location: token.span.start,
                kind: ErrKind::UnexpectedToken {
                    expected: vec![TokenKind::Number],
                    actual: Some(token.kind),
                },
            });
        }
    }

    // Float parsing is stupidly hard.
    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_f64");
        let token = self.next().unwrap_or_else(|| Err(self.unexpected_eof()))?;

        if let TokenKind::Number = token.kind {
            let val = token.string.parse::<f64>().map_err(|_| Error {
                location: token.span.start,
                kind: ErrKind::IntegerOverflow(token.string),
            })?;
            visitor.visit_f64(val)
        } else {
            return Err(Error {
                location: token.span.start,
                kind: ErrKind::UnexpectedToken {
                    expected: vec![TokenKind::Number],
                    actual: Some(token.kind),
                },
            });
        }
    }

    // The `Serializer` implementation on the previous page serialized chars as
    // single-character strings so handle that representation here.
    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_char");
        let token = self.next().unwrap_or_else(|| Err(self.unexpected_eof()))?;
        if let TokenKind::String = token.kind {
            let mut chars = token.string.chars();
            if let Some(ch) = chars.next() {
                if let None = chars.next() {
                    return visitor.visit_char(ch);
                }
            }
            return Err(Error {
                location: token.span.start,
                kind: ErrKind::StringFailedCastToChar(token.string),
            });
        } else {
            Err(Error {
                location: token.span.start,
                kind: ErrKind::UnexpectedToken {
                    expected: vec![TokenKind::String],
                    actual: Some(token.kind),
                },
            })
        }
    }

    // Refer to the "Understanding deserializer lifetimes" page for information
    // about the three deserialization flavors of strings in Serde.
    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_str");
        let token = self.next().unwrap_or_else(|| Err(self.unexpected_eof()))?;
        if let TokenKind::String = token.kind {
            visitor.visit_string(token.string)
        } else {
            Err(Error {
                location: token.span.start,
                kind: ErrKind::UnexpectedToken {
                    expected: vec![TokenKind::String],
                    actual: Some(token.kind),
                },
            })
        }
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_string");
        let token = self.next().unwrap_or_else(|| Err(self.unexpected_eof()))?;
        if let TokenKind::String = token.kind {
            visitor.visit_string(token.string)
        } else {
            Err(Error {
                location: token.span.start,
                kind: ErrKind::UnexpectedToken {
                    expected: vec![TokenKind::String],
                    actual: Some(token.kind),
                },
            })
        }
    }

    // The `Serializer` implementation on the previous page serialized byte
    // arrays as JSON arrays of bytes. Handle that representation here.
    fn deserialize_bytes<V>(self, _visitor: V) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_bytes");
        unimplemented!()
    }

    fn deserialize_byte_buf<V>(self, _visitor: V) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_byte_buf");
        unimplemented!()
    }

    // An absent optional is represented as the JSON `null` and a present
    // optional is represented as just the contained value.
    //
    // As commented in `Serializer` implementation, this is a lossy
    // representation. For example the values `Some(())` and `None` both
    // serialize as just `null`. Unfortunately this is typically what people
    // expect when working with JSON. Other formats are encouraged to behave
    // more intelligently if possible.
    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_option");
        let token = self.next().unwrap_or_else(|| Err(self.unexpected_eof()))?;
        match token.kind {
            TokenKind::Null => visitor.visit_none(),
            TokenKind::ObjectStart
            | TokenKind::ArrayStart
            | TokenKind::String
            | TokenKind::Number
            | TokenKind::True
            | TokenKind::False => visitor.visit_some(self),
            TokenKind::ObjectEnd | TokenKind::ArrayEnd | TokenKind::Colon | TokenKind::Comma => {
                Err(Error {
                    location: token.span.start,
                    kind: ErrKind::UnexpectedToken {
                        expected: vec![
                            TokenKind::ObjectStart,
                            TokenKind::ArrayStart,
                            TokenKind::String,
                            TokenKind::Number,
                            TokenKind::True,
                            TokenKind::False,
                            TokenKind::Null,
                        ],
                        actual: Some(token.kind),
                    },
                })
            }
        }
    }

    // In Serde, unit means an anonymous value containing no data.
    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_unit");
        let token = self.next().unwrap_or_else(|| Err(self.unexpected_eof()))?;
        if let TokenKind::Null = token.kind {
            visitor.visit_unit()
        } else {
            Err(Error {
                location: token.span.start,
                kind: ErrKind::UnexpectedToken {
                    expected: vec![TokenKind::Null],
                    actual: Some(token.kind),
                },
            })
        }
    }

    // Unit struct means a named value containing no data.
    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_unit_struct");
        self.deserialize_unit(visitor)
    }

    // As is done here, serializers are encouraged to treat newtype structs as
    // insignificant wrappers around the data they contain. That means not
    // parsing anything other than the contained value.
    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_newtype_struct");
        visitor.visit_newtype_struct(self)
    }

    // Deserialization of compound types like sequences and maps happens by
    // passing the visitor an "Access" object that gives it the ability to
    // iterate through the data contained in the sequence.
    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_seq");
        let token = self.next().unwrap_or_else(|| Err(self.unexpected_eof()))?;

        if let TokenKind::ArrayStart = token.kind {
            let value = visitor.visit_seq(CommaSeparated::new(self))?;
            let expected_arr_end = self.next().unwrap_or_else(|| Err(self.unexpected_eof()))?;

            if let TokenKind::ArrayEnd = expected_arr_end.kind {
                Ok(value)
            } else {
                Err(Error {
                    location: expected_arr_end.span.start,
                    kind: ErrKind::UnexpectedToken {
                        expected: vec![TokenKind::ArrayEnd],
                        actual: Some(token.kind),
                    },
                })
            }
        } else {
            Err(Error {
                location: token.span.start,
                kind: ErrKind::UnexpectedToken {
                    expected: vec![TokenKind::ArrayStart],
                    actual: Some(token.kind),
                },
            })
        }
    }

    // Tuples look just like sequences in JSON. Some formats may be able to
    // represent tuples more efficiently.
    //
    // As indicated by the length parameter, the `Deserialize` implementation
    // for a tuple in the Serde data model is required to know the length of the
    // tuple before even looking at the input data.
    fn deserialize_tuple<V>(
        self,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_tuple");
        self.deserialize_seq(visitor)
    }

    // Tuple structs look just like sequences in JSON.
    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_tuple_struct");
        self.deserialize_seq(visitor)
    }

    // Much like `deserialize_seq` but calls the visitors `visit_map` method
    // with a `MapAccess` implementation, rather than the visitor's `visit_seq`
    // method with a `SeqAccess` implementation.
    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_map");
        let token = self.next().unwrap_or_else(|| Err(self.unexpected_eof()))?;

        if let TokenKind::ObjectStart = token.kind {
            let value = visitor.visit_map(CommaSeparated::new(self))?;

            let expected_obj_end = self.next().unwrap_or_else(|| Err(self.unexpected_eof()))?;

            if let TokenKind::ObjectEnd = expected_obj_end.kind {
                Ok(value)
            } else {
                Err(Error {
                    location: expected_obj_end.span.start,
                    kind: ErrKind::UnexpectedToken {
                        expected: vec![TokenKind::ObjectEnd],
                        actual: Some(expected_obj_end.kind),
                    },
                })
            }
        } else {
            Err(Error {
                location: token.span.start,
                kind: ErrKind::UnexpectedToken {
                    expected: vec![TokenKind::ObjectStart],
                    actual: Some(token.kind),
                },
            })
        }
    }

    // Structs look just like maps in JSON.
    //
    // Notice the `fields` parameter - a "struct" in the Serde data model means
    // that the `Deserialize` implementation is required to know what the fields
    // are before even looking at the input data. Any key-value pairing in which
    // the fields cannot be known ahead of time is probably a map.
    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_struct");
        self.deserialize_map(visitor)
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_enum");
        let token = self.next().unwrap_or_else(|| Err(self.unexpected_eof()))?;

        match token.kind {
            TokenKind::String => visitor.visit_enum(token.string.into_deserializer()),
            TokenKind::ObjectStart => {
                let value = visitor.visit_enum(Enum::new(self))?;

                let expected_obj_end = self.next().unwrap_or_else(|| Err(self.unexpected_eof()))?;
                if let TokenKind::ObjectEnd = expected_obj_end.kind {
                    Ok(value)
                } else {
                    Err(Error {
                        location: expected_obj_end.span.start,
                        kind: ErrKind::UnexpectedToken {
                            expected: vec![TokenKind::ObjectEnd],
                            actual: Some(expected_obj_end.kind),
                        },
                    })
                }
            }
            _ => Err(Error {
                location: token.span.start,
                kind: ErrKind::UnexpectedToken {
                    expected: vec![TokenKind::String, TokenKind::ObjectStart],
                    actual: Some(token.kind),
                },
            }),
        }
    }

    // An identifier in Serde is the type that identifies a field of a struct or
    // the variant of an enum. In JSON, struct fields and enum variants are
    // represented as strings. In other formats they may be represented as
    // numeric indices.
    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_identifier");
        self.deserialize_string(visitor)
    }

    // Like `deserialize_any` but indicates to the `Deserializer` that it makes
    // no difference which `Visitor` method is called because the data is
    // ignored.
    //
    // Some deserializers are able to implement this more efficiently than
    // `deserialize_any`, for example by rapidly skipping over matched
    // delimiters without paying close attention to the data in between.
    //
    // Some formats are not able to implement this at all. Formats that can
    // implement `deserialize_any` and `deserialize_ignored_any` are known as
    // self-describing.
    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("deserialize_ignored_any");
        self.deserialize_any(visitor)
    }
}

// In order to handle commas correctly when deserializing a JSON array or map,
// we need to track whether we are on the first element or past the first
// element.
struct CommaSeparated<'a, CharsStream, PassThroughError>
where
    CharsStream: IntoIterator<Item = Result<char, PassThroughError>>,
    PassThroughError: std::error::Error,
{
    de: &'a mut CharsDeserializer<CharsStream, PassThroughError>,
    first: bool,
}

impl<'a, CharsStream, PassThroughError> CommaSeparated<'a, CharsStream, PassThroughError>
where
    CharsStream: IntoIterator<Item = Result<char, PassThroughError>>,
    PassThroughError: std::error::Error,
{
    fn new(de: &'a mut CharsDeserializer<CharsStream, PassThroughError>) -> Self {
        CommaSeparated { de, first: true }
    }
}

// `SeqAccess` is provided to the `Visitor` to give it the ability to iterate
// through elements of the sequence.
impl<'de, 'a, CharsStream, PassThroughError> SeqAccess<'de>
    for CommaSeparated<'a, CharsStream, PassThroughError>
where
    CharsStream: IntoIterator<Item = Result<char, PassThroughError>>,
    PassThroughError: std::error::Error,
{
    type Error = Error<PassThroughError>;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Error<PassThroughError>>
    where
        T: DeserializeSeed<'de>,
    {
        #[cfg(feature = "debug")]
        println!("next_element_seed");
        let token = self
            .de
            .next()
            .unwrap_or_else(|| Err(self.de.unexpected_eof()))?;

        if let TokenKind::ArrayEnd | TokenKind::ObjectEnd = token.kind {
            self.de.peeked = Some(token);
            return Ok(None);
        }

        if self.first {
            self.first = false;
            self.de.peeked = Some(token);
            seed.deserialize(&mut *self.de).map(Some)
        } else {
            if let TokenKind::Comma = token.kind {
                seed.deserialize(&mut *self.de).map(Some)
            } else {
                Err(Error {
                    location: token.span.start,
                    kind: ErrKind::UnexpectedToken {
                        expected: vec![TokenKind::Comma],
                        actual: Some(token.kind),
                    },
                })
            }
        }
    }
}

// `MapAccess` is provided to the `Visitor` to give it the ability to iterate
// through entries of the map.
impl<'de, 'a, CharsStream, PassThroughError> MapAccess<'de>
    for CommaSeparated<'a, CharsStream, PassThroughError>
where
    CharsStream: IntoIterator<Item = Result<char, PassThroughError>>,
    PassThroughError: std::error::Error,
{
    type Error = Error<PassThroughError>;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Error<PassThroughError>>
    where
        K: DeserializeSeed<'de>,
    {
        #[cfg(feature = "debug")]
        println!("next_key_seed");
        let token = self
            .de
            .next()
            .unwrap_or_else(|| Err(self.de.unexpected_eof()))?;

        if let TokenKind::ObjectEnd | TokenKind::ArrayEnd = token.kind {
            self.de.peeked = Some(token);
            return Ok(None);
        }

        if self.first {
            self.first = false;
            self.de.peeked = Some(token);
            seed.deserialize(&mut *self.de).map(Some)
        } else {
            if let TokenKind::Comma = token.kind {
                seed.deserialize(&mut *self.de).map(Some)
            } else {
                Err(Error {
                    location: token.span.start,
                    kind: ErrKind::UnexpectedToken {
                        expected: vec![TokenKind::Comma],
                        actual: Some(token.kind),
                    },
                })
            }
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Error<PassThroughError>>
    where
        V: DeserializeSeed<'de>,
    {
        #[cfg(feature = "debug")]
        println!("next_value_seed");
        let token = self
            .de
            .next()
            .unwrap_or_else(|| Err(self.de.unexpected_eof()))?;

        if let TokenKind::Colon = token.kind {
            seed.deserialize(&mut *self.de)
        } else {
            Err(Error {
                location: token.span.start,
                kind: ErrKind::UnexpectedToken {
                    expected: vec![TokenKind::Colon],
                    actual: Some(token.kind),
                },
            })
        }
    }
}

struct Enum<'a, CharsStream, PassThroughError>
where
    CharsStream: IntoIterator<Item = Result<char, PassThroughError>>,
    PassThroughError: std::error::Error,
{
    de: &'a mut CharsDeserializer<CharsStream, PassThroughError>,
}

impl<'a, CharsStream, PassThroughError> Enum<'a, CharsStream, PassThroughError>
where
    CharsStream: IntoIterator<Item = Result<char, PassThroughError>>,
    PassThroughError: std::error::Error,
{
    fn new(de: &'a mut CharsDeserializer<CharsStream, PassThroughError>) -> Self {
        Enum { de }
    }
}

// `EnumAccess` is provided to the `Visitor` to give it the ability to determine
// which variant of the enum is supposed to be deserialized.
//
// Note that all enum deserialization methods in Serde refer exclusively to the
// "externally tagged" enum representation.
impl<'de, 'a, CharsStream, PassThroughError> EnumAccess<'de>
    for Enum<'a, CharsStream, PassThroughError>
where
    CharsStream: IntoIterator<Item = Result<char, PassThroughError>>,
    PassThroughError: std::error::Error,
{
    type Error = Error<PassThroughError>;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Error<PassThroughError>>
    where
        V: DeserializeSeed<'de>,
    {
        #[cfg(feature = "debug")]
        println!("variant_seed");
        // The `deserialize_enum` method parsed a `{` character so we are
        // currently inside of a map. The seed will be deserializing itself from
        // the key of the map.
        let val = seed.deserialize(&mut *self.de)?;
        // Parse the colon separating map key from value.
        let token = self
            .de
            .next()
            .unwrap_or_else(|| Err(self.de.unexpected_eof()))?;

        if let TokenKind::Colon = token.kind {
            Ok((val, self))
        } else {
            Err(Error {
                location: token.span.start,
                kind: ErrKind::UnexpectedToken {
                    expected: vec![TokenKind::Colon],
                    actual: Some(token.kind),
                },
            })
        }
    }
}

// `VariantAccess` is provided to the `Visitor` to give it the ability to see
// the content of the single variant that it decided to deserialize.
impl<'de, 'a, CharsStream, PassThroughError> VariantAccess<'de>
    for Enum<'a, CharsStream, PassThroughError>
where
    CharsStream: IntoIterator<Item = Result<char, PassThroughError>>,
    PassThroughError: std::error::Error,
{
    type Error = Error<PassThroughError>;

    // If the `Visitor` expected this variant to be a unit variant, the input
    // should have been the plain string case handled in `deserialize_enum`.
    fn unit_variant(self) -> Result<(), Error<PassThroughError>> {
        #[cfg(feature = "debug")]
        println!("unit_variant");
        let next_token = if let Some(peeked) = &self.de.peeked {
            Some(peeked.clone().kind)
        } else {
            let next = self.de.next();
            if let Some(next) = next {
                self.de.peeked = Some(next?);
            } else {
                self.de.peeked = None;
            }
            self.de.peeked.clone().and_then(|token| Some(token.kind))
        };
        Err(Error {
            location: self.de.input.peek_location(),
            kind: ErrKind::UnexpectedToken {
                expected: vec![TokenKind::String],
                actual: next_token,
            },
        })
    }

    // Newtype variants are represented in JSON as `{ NAME: VALUE }` so
    // deserialize the value here.
    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Error<PassThroughError>>
    where
        T: DeserializeSeed<'de>,
    {
        #[cfg(feature = "debug")]
        println!("newtype_variant_seed");
        seed.deserialize(self.de)
    }

    // Tuple variants are represented in JSON as `{ NAME: [DATA...] }` so
    // deserialize the sequence of data here.
    fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("tuple_variant");
        de::Deserializer::deserialize_seq(self.de, visitor)
    }

    // Struct variants are represented in JSON as `{ NAME: { K: V, ... } }` so
    // deserialize the inner map here.
    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Error<PassThroughError>>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "debug")]
        println!("struct_variant");
        de::Deserializer::deserialize_map(self.de, visitor)
    }
}

////////////////////////////////////////////////////////////////////////////////

#[test]
fn test_struct() {
    use std::convert::Infallible;
    use crate::from_value_stream;
    use serde_derive::Deserialize;
    #[derive(Deserialize, PartialEq, Debug)]
    struct Test {
        int: u32,
        seq: Vec<String>,
    }

    let j = r#"[{"int":1,"seq":["a","b"]},{"int":1,"seq":["a","b"]},{}]"#;
    let expected = Test {
        int: 1,
        seq: vec!["a".to_owned(), "b".to_owned()],
    };

    let stream = j.chars().map(|ch| Ok::<_, Infallible>(ch));
    let mut result_list = from_value_stream::<Test, _, _>(stream);
    let result = result_list.next().unwrap().unwrap();
    assert_eq!(expected, result);
    let result = result_list.next().unwrap().unwrap();
    assert_eq!(expected, result);
    assert!(result_list.next().unwrap().is_err());
}

impl<PassThroughError> serde::ser::Error for Error<PassThroughError>
where
    PassThroughError: std::error::Error,
{
    fn custom<T>(msg: T) -> Self
    where
        T: std::fmt::Display,
    {
        Error {
            location: Location::default(),
            kind: ErrKind::Custom(format!("{msg}")),
        }
    }
}

impl<PassThroughError> serde::de::Error for Error<PassThroughError>
where
    PassThroughError: std::error::Error,
{
    fn custom<T>(msg: T) -> Self
    where
        T: std::fmt::Display,
    {
        Error {
            location: Location::default(),
            kind: ErrKind::Custom(format!("{msg}")),
        }
    }
}

impl<PassThroughError> std::fmt::Display for Error<PassThroughError>
where
    PassThroughError: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut output = String::new();
        output.push_str("Line: ");
        output.push_str(&self.location.line().to_string());
        output.push_str("Column: ");
        output.push_str(&self.location.col().to_string());
        output.push(' ');

        match &self.kind {
            ErrKind::UnexpectedEOF => {
                output.push_str("Unexpected End of File.");
            }
            ErrKind::IllegalLeading0 => {
                output.push_str("Illegal leading 0.");
            }
            ErrKind::TrailingComma => {
                output.push_str("Trailing comma.");
            }
            ErrKind::InvalidUnicodeEscapeSequence => {
                output.push_str("Invalid Unicode escape sequence.");
            }
            ErrKind::UnclosedString => {
                output.push_str("Unclosed string.");
            }
            ErrKind::UnexpectedChar { expected, actual } => {
                output.push_str(&format!(
                    "Expected one of [{}], but found {}.",
                    expected
                        .into_iter()
                        .map(|ch| ch.to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                    actual
                        .and_then(|ch| Some(ch.to_string()))
                        .unwrap_or("None".to_string())
                ));
            }
            ErrKind::UnexpectedToken { expected, actual } => {
                output.push_str(&format!(
                    "Expected one of [{}], but found {}.",
                    expected
                        .into_iter()
                        .map(|token| format!("{token:?}"))
                        .collect::<Vec<_>>()
                        .join(", "),
                    actual
                        .and_then(|token| Some(format!("{token:?}")))
                        .unwrap_or("None".to_string())
                ));
            }
            ErrKind::IntegerOverflow(str) => {
                output.push_str(&format!(
                    "The string '{str}' overflowed its target integer type."
                ));
            }
            ErrKind::StringFailedCastToChar(str) => {
                output.push_str(&format!("The string '{str}' could not be cast to a char."));
            }
            ErrKind::Custom(custom) => {
                return write!(f, "{custom}");
            }
            ErrKind::PassThrough(pass_through) => {
                output.push_str(&format!("{}", pass_through));
            }
        }

        write!(f, "{}", output)
    }
}

impl<PassThroughError> std::error::Error for Error<PassThroughError> where
    PassThroughError: std::error::Error
{
}
