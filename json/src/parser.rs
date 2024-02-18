use std::{borrow::Cow, collections::HashMap, str::FromStr};

use crate::{
    tokenizer::{JsonParseErr, JsonToken, JsonTokenKind, JsonTokenizer},
    JsonParseState,
};

pub fn parse(json: &str) -> Value {
    for result in JsonTokenizer::new(json) {
        match result {
            Ok(token) => match token.kind {
                JsonTokenKind::ObjectStart => todo!(),
                JsonTokenKind::ObjectEnd => todo!(),
                JsonTokenKind::ArrayStart => todo!(),
                JsonTokenKind::ArrayEnd => todo!(),
                JsonTokenKind::Colon => todo!(),
                JsonTokenKind::Comma => todo!(),
                JsonTokenKind::String => todo!(),
                JsonTokenKind::Number => todo!(),
                JsonTokenKind::True => todo!(),
                JsonTokenKind::False => todo!(),
                JsonTokenKind::Null => todo!(),
            },
            Err(err) => {
                match err {
                    JsonParseErr::IllegalLeading0(_) => {} // Do nothing. The number parser will handle this.
                    JsonParseErr::InvalidUnicodeEscapeSequence(_) => {} // Do nothing. The string parser will handle this.
                    JsonParseErr::TrailingComma(_) => {} // Just ignore trailing commas.
                    JsonParseErr::UnclosedString(_) => {} // Do nothing. The string parser will handle this.
                    JsonParseErr::UnexpectedCharacters(span) => {
                        todo!();
                    }
                    JsonParseErr::UnexpectedEOF => {
                        todo!();
                    }
                }
            }
        }
    }

    Value::Null
}

struct JsonParser<'json> {
    tokenizer: JsonTokenizer<'json>,
    json: &'json str,
    lookahead: Option<JsonToken>,
    values_being_built: Vec<Value<'json>>,
}

impl<'json> JsonParser<'json> {
    fn new(json: &'json str) -> Self {
        Self {
            tokenizer: JsonTokenizer::new(json),
            json,
            lookahead: None,
            values_being_built: Vec::new(),
        }
    }

    fn parse(json: &'json str) -> Value {
        Self::new(json).parse_internal()
    }

    fn parse_internal(mut self) -> Value<'json> {
        self.parse_value();
        loop {
            if let Some(result) = self.tokenizer.next() {
                match result {
                    Ok(token) => match token.kind {
                        JsonTokenKind::ObjectStart => {
                            self.values_being_built.push(Value::Object(HashMap::new()));
                        }
                        JsonTokenKind::ObjectEnd => {
                            self.pop_object();
                        }
                        JsonTokenKind::ArrayStart => {
                            self.values_being_built.push(Value::Array(Vec::new()))
                        }
                        JsonTokenKind::ArrayEnd => {
                            self.pop_array();
                        }
                        JsonTokenKind::Colon => {}
                        JsonTokenKind::Comma => {}
                        JsonTokenKind::String => todo!(),
                        JsonTokenKind::Number => todo!(),
                        JsonTokenKind::True => todo!(),
                        JsonTokenKind::False => todo!(),
                        JsonTokenKind::Null => todo!(),
                    },
                    Err(parse_err) => {
                        match parse_err {
                            JsonParseErr::IllegalLeading0(_) => {} // Do nothing. The number parser will handle this.
                            JsonParseErr::InvalidUnicodeEscapeSequence(_) => {} // Do nothing. The string parser will handle this.
                            JsonParseErr::TrailingComma(_) => {} // Just ignore trailing commas.
                            JsonParseErr::UnclosedString(_) => {} // Do nothing. The string parser will handle this.
                            JsonParseErr::UnexpectedCharacters(span) => {
                                todo!();
                            }
                            JsonParseErr::UnexpectedEOF => {
                                todo!();
                            }
                        }
                    }
                }
            } else {
                break;
            }
        }
        Value::Null
    }

    fn parse_value(&mut self) -> Option<Value<'json>> {
        match self.next_token() {
            None => return None,
            Some(token) => match token.kind {
                JsonTokenKind::String => {
                    return Some(Value::String(JsonString::new(
                        &self.json[token.span.as_range()],
                    )))
                }
                JsonTokenKind::Number => {
                    return Some(Value::Number(JsonNumber::new(
                        &self.json[token.span.as_range()],
                    )))
                }
                JsonTokenKind::ObjectStart => {
                    let result = self.parse_object();
                    let is_err = result.is_err();

                    let obj = match result {
                        Ok(map) | Err(map) => Value::Object(map),
                    };

                    if is_err {
                        self.recover_in_panic_mode();
                        todo!();
                    }

                    return Some(obj);
                }
                JsonTokenKind::ArrayStart => {
                    let result = self.parse_array();
                    let is_err = result.is_err();

                    let arr = match result {
                        Ok(vec) | Err(vec) => Value::Array(vec),
                    };

                    if is_err {
                        self.recover_in_panic_mode();
                        todo!()
                    }

                    return Some(arr);
                }
                JsonTokenKind::True => return Some(Value::Bool(true)),
                JsonTokenKind::False => return Some(Value::Bool(false)),
                JsonTokenKind::Null => return Some(Value::Null),
                JsonTokenKind::ArrayEnd => todo!(),
                JsonTokenKind::ObjectEnd => todo!(),
                JsonTokenKind::Colon => todo!(), // ignore probably?
                JsonTokenKind::Comma => todo!(), // ignore probably?
            },
        }
    }

    fn parse_object(
        &mut self,
    ) -> Result<HashMap<&'json str, Value<'json>>, HashMap<&'json str, Value<'json>>> {
        todo!();
    }

    fn parse_array(&mut self) -> Result<Vec<Value<'json>>, Vec<Value<'json>>> {
        let result = Vec::new();
        loop {
            match self.next_token() {
                None => return Ok(result),
                Some(token) => {}
            }
        }
    }

    fn pop_object(&mut self) -> Option<Value<'json>> {
        if !self
            .values_being_built
            .iter()
            .rev()
            .any(|val| matches!(val, Value::Object(_)))
        {
            return None;
        }

        todo!()
    }

    fn pop_array(&mut self) -> Option<Value<'json>> {
        if !self
            .values_being_built
            .iter()
            .rev()
            .any(|val| matches!(val, Value::Object(_)))
        {
            return None;
        }

        todo!()
    }

    fn recover_in_panic_mode(&mut self) {
        todo!();
    }

    fn next_token(&mut self) -> Option<JsonToken> {
        if let Some(token) = std::mem::take(&mut self.lookahead) {
            return Some(token);
        }

        loop {
            match self.tokenizer.next() {
                None => return None,
                Some(result) => match result {
                    Ok(token) => return Some(token),
                    Err(err) => match err {
                        // if there's more tokens we want them, so continue
                        JsonParseErr::UnexpectedEOF => {}
                        // not meaningful to parser
                        JsonParseErr::TrailingComma(_) => {}
                        // defer to string parser
                        JsonParseErr::UnclosedString(_) => {}
                        // defer to string parser
                        JsonParseErr::InvalidUnicodeEscapeSequence(_) => {}
                        // defer to number parser
                        JsonParseErr::IllegalLeading0(_) => {}
                        JsonParseErr::UnexpectedCharacters(span) => {
                            return Some(JsonToken {
                                kind: JsonTokenKind::String,
                                span,
                            });
                        }
                    },
                },
            }
        }
    }
}

pub enum Value<'json> {
    Null,
    Bool(bool),
    Number(JsonNumber<'json>),
    String(JsonString<'json>),
    Array(Vec<Value<'json>>),
    Object(HashMap<&'json str, Value<'json>>),
}

#[derive(Clone, Debug)]
pub struct JsonNumber<'json> {
    source: &'json str,
}

impl<'json> JsonNumber<'json> {
    pub(crate) fn new(source: &'json str) -> Self {
        Self { source }
    }

    pub fn parse<T>(&self) -> Result<T, <T as FromStr>::Err>
    where
        T: FromStr,
    {
        self.source.parse::<T>()
    }
}

#[derive(Clone, Debug)]
pub struct JsonString<'json> {
    source: &'json str,
    cow: Option<Cow<'json, str>>,
}

impl<'json> JsonString<'json> {
    pub(crate) fn new(source: &'json str) -> Self {
        Self {
            source,
            cow: Self::escape(source),
        }
    }

    pub fn raw(&self) -> &str {
        self.source
    }

    pub fn parsed(&self) -> &Option<Cow<'json, str>> {
        &self.cow
    }

    /// Handles escaping characters from the string. If the string
    /// is not a valid JSON string, returns None. If the string is
    /// parsed without issue, returns Some() with the Cow containing
    /// the escaped string.
    pub fn escape(source: &str) -> Option<Cow<'_, str>> {
        let mut cow = Cow::Borrowed(source);

        let mut chars = source.char_indices().peekable();
        loop {
            let ch = chars.next();
            match ch {
                None => return Some(cow),
                Some((i, ch)) => {
                    if ch == '\\' {
                        let mut string = match cow {
                            Cow::Borrowed(_) => source[..i].to_string(),
                            Cow::Owned(string) => string,
                        };

                        match chars.next() {
                            None => return None,
                            Some((_, next_ch)) => {
                                let ch_to_add = match next_ch {
                                    '"' => '"',
                                    '\\' => '\\',
                                    '/' => '/',
                                    'b' => '\u{0008}',
                                    'f' => '\u{000c}',
                                    'n' => '\n',
                                    'r' => '\r',
                                    't' => '\t',
                                    'u' => {
                                        let mut code = String::with_capacity(4);
                                        let mut is_valid_unicode_escape = true;

                                        for _ in 0..4 {
                                            match chars.peek() {
                                                Some((_, ch)) => {
                                                    if ch.is_ascii_hexdigit() {
                                                        code.push(chars.next().unwrap().1);
                                                    } else {
                                                        is_valid_unicode_escape = false;
                                                        string.push_str("\\u");
                                                        string.push_str(&code);
                                                        break;
                                                    }
                                                }
                                                _ => {
                                                    is_valid_unicode_escape = false;
                                                    string.push_str("\\u");
                                                    string.push_str(&code);
                                                    break;
                                                }
                                            }
                                        }

                                        if is_valid_unicode_escape {
                                            match u32::from_str_radix(&code, 16) {
                                                Ok(parsed) => match char::from_u32(parsed) {
                                                    Some(ch) => string.push(ch),
                                                    None => {
                                                        string.push_str("\\u");
                                                        string.push_str(&code);
                                                    }
                                                },
                                                Err(_) => {
                                                    string.push_str("\\u");
                                                    string.push_str(&code);
                                                }
                                            }
                                        }

                                        // We're doing custom additions to the string, so no
                                        // need to pass a character to the outer loop
                                        cow = Cow::Owned(string);
                                        continue;
                                    }
                                    _ => return None,
                                };
                                string.push(ch_to_add);
                                cow = Cow::Owned(string)
                            }
                        }
                    } else if ch.is_control() {
                        return None;
                    } else {
                        match cow {
                            Cow::Borrowed(_) => {}
                            Cow::Owned(mut string) => {
                                string.push(ch);
                                cow = Cow::Owned(string);
                            }
                        }
                    }
                }
            }
        }
    }
}

