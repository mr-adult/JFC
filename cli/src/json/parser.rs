use std::{borrow::Cow, collections::HashMap, slice::Iter};

use generic_tokenizer::Location;

use super::{
    tokenizer::{JsonParseErr, JsonToken, JsonTokenKind, JsonTokenizer, Span},
    JsonParseState,
};

static DEFAULT_KEY: &'static str = "unknown_key";
static DEFAULT_KEY_COW: Cow<'static, str> = Cow::Borrowed(&DEFAULT_KEY);

pub(crate) struct JsonParser<'json> {
    tokenizer: JsonTokenizer<'json>,
    json: &'json str,
    lookahead: Option<JsonToken>,
    states: Vec<JsonParseState>,
    values_being_built: Vec<ValueInProgress<'json>>,
    errs: Vec<JsonParseErr>,
}

impl<'json> JsonParser<'json> {
    fn new(json: &'json str) -> Self {
        Self {
            tokenizer: JsonTokenizer::new(json),
            json,
            lookahead: None,
            states: vec![JsonParseState::Value],
            values_being_built: Vec::new(),
            errs: Vec::new(),
        }
    }

    pub(crate) fn parse(json: &'json str) -> (Value<'json>, Vec<JsonParseErr>) {
        Self::new(json).parse_internal()
    }

    fn parse_internal(mut self) -> (Value<'json>, Vec<JsonParseErr>) {
        loop {
            match self.states.pop() {
                None => {
                    self.match_token(JsonTokenKind::Comma);
                    if self.states.is_empty() {
                        match self.next_token() {
                            None => {
                                return (
                                    self.unwind_full_value_stack()
                                        .unwrap_or_else(|| Value::Null),
                                    self.errs,
                                )
                            }
                            Some(token) => {
                                self.lookahead = Some(token);
                                self.states.push(JsonParseState::Value);
                            }
                        }
                    }
                }
                Some(state) => {
                    match state {
                        JsonParseState::Value => {
                            match self.next_token() {
                                None => {
                                    return (
                                        self.unwind_full_value_stack()
                                            .unwrap_or_else(|| Value::Null),
                                        self.errs,
                                    );
                                }
                                Some(token) => {
                                    if !self.states.is_empty() {
                                        self.states.push(JsonParseState::AfterValue);
                                    }
                                    match &token.kind {
                                        JsonTokenKind::ObjectStart => {
                                            self.values_being_built.push(ValueInProgress::Object(
                                                ObjectInProgress::new(),
                                            ));

                                            self.states.push(JsonParseState::Object);
                                        }
                                        JsonTokenKind::ArrayStart => {
                                            self.values_being_built
                                                .push(ValueInProgress::Array(Vec::new()));
                                            self.states.push(JsonParseState::Array);
                                        }
                                        JsonTokenKind::String => {
                                            self.values_being_built.push(ValueInProgress::String(
                                                JsonString::new(self.json, token.span),
                                            ));
                                            // We just pushed a value on, so this should never fail
                                            assert!(self.unwind_value_stack_once(), "BUG: Expected value to be on the stack since one was just added.");
                                        }
                                        JsonTokenKind::Number => {
                                            self.values_being_built.push(ValueInProgress::Number(
                                                JsonNumber::new(&self.json[token.span.as_range()]),
                                            ));
                                            assert!(self.unwind_value_stack_once(), "BUG: Expected value to be on the stack since one was just added.");
                                        }
                                        JsonTokenKind::True => {
                                            self.values_being_built
                                                .push(ValueInProgress::Bool(true));
                                            assert!(self.unwind_value_stack_once(),"BUG: Expected value to be on the stack since one was just added.");
                                        }
                                        JsonTokenKind::False => {
                                            self.values_being_built
                                                .push(ValueInProgress::Bool(false));
                                            assert!(self.unwind_value_stack_once(), "BUG: Expected value to be on the stack since one was just added.");
                                        }
                                        JsonTokenKind::Null => {
                                            self.values_being_built.push(ValueInProgress::Null);
                                            assert!(self.unwind_value_stack_once(), "BUG: Expected value to be on the stack since one was just added.");
                                        }
                                        // Explicitly specifying all kinds to make refactoring
                                        // easier later.
                                        JsonTokenKind::ArrayEnd
                                        | JsonTokenKind::ObjectEnd
                                        | JsonTokenKind::Colon
                                        | JsonTokenKind::Comma => {
                                            self.lookahead = Some(token);
                                            self.states.push(JsonParseState::Value);
                                            self.recover_in_panic_mode();
                                        }
                                    }
                                }
                            }
                        }
                        JsonParseState::Object => {
                            if self.match_token(JsonTokenKind::ObjectEnd).is_some() {
                                self.pop_object();
                                continue;
                            }

                            self.states.push(JsonParseState::Object);
                            self.states.push(JsonParseState::Value);
                            self.states.push(JsonParseState::KeyValuePairColon);
                            self.states.push(JsonParseState::KeyValuePairKey);
                            continue;
                        }
                        JsonParseState::KeyValuePairColon => {
                            if self.match_token(JsonTokenKind::Colon).is_some() {
                                continue;
                            } else {
                                self.recover_in_panic_mode();
                            }
                        }
                        JsonParseState::KeyValuePairKey => {
                            if let Some(str_token) = self.match_token(JsonTokenKind::String) {
                                match self.values_being_built.pop() {
                                    Some(ValueInProgress::Object(mut obj)) => {
                                        obj.active_key =
                                            Some(JsonString::new(self.json, str_token.span));
                                        self.values_being_built.push(ValueInProgress::Object(obj));
                                    }
                                    None
                                    | Some(ValueInProgress::Null)
                                    | Some(ValueInProgress::Bool(_))
                                    | Some(ValueInProgress::Number(_))
                                    | Some(ValueInProgress::String(_))
                                    | Some(ValueInProgress::Array(_)) => {
                                        self.recover_in_panic_mode();
                                    }
                                }
                            }
                        }
                        JsonParseState::AfterValue => {
                            if self.match_token(JsonTokenKind::Comma).is_none() {
                                continue;
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
                                }
                                JsonParseState::Array => {
                                    self.states.push(JsonParseState::Value);
                                }
                                JsonParseState::KeyValuePairColon
                                | JsonParseState::KeyValuePairKey
                                | JsonParseState::Value
                                | JsonParseState::AfterValue => {
                                    self.recover_in_panic_mode();
                                }
                            }
                        }
                        JsonParseState::Array => {
                            if let Some(_) = self.match_token(JsonTokenKind::ArrayEnd) {
                                self.pop_array();
                                continue;
                            }

                            self.states.push(JsonParseState::Array);
                            self.states.push(JsonParseState::Value);
                        }
                    }
                }
            }
        }
    }

    fn pop_object(&mut self) {
        if !self
            .values_being_built
            .iter()
            .rev()
            .any(|val| matches!(val, ValueInProgress::Object(_)))
        {
            return;
        }

        loop {
            match self.values_being_built.pop() {
                None => break,
                Some(ValueInProgress::Object(obj)) => {
                    self.values_being_built.push(ValueInProgress::Object(obj));
                    self.unwind_value_stack_once();
                    break;
                }
                Some(other_val) => {
                    self.values_being_built.push(other_val);
                    self.unwind_value_stack_once();
                }
            }
        }
    }

    fn pop_array(&mut self) {
        if !self
            .values_being_built
            .iter()
            .rev()
            .any(|val| matches!(val, ValueInProgress::Array(_)))
        {
            return;
        }

        loop {
            match self.values_being_built.pop() {
                None => break,
                Some(ValueInProgress::Array(vec)) => {
                    self.values_being_built.push(ValueInProgress::Array(vec));
                    self.unwind_value_stack_once();
                    break;
                }
                Some(other) => {
                    self.values_being_built.push(other);
                    self.unwind_value_stack_once();
                }
            }
        }
    }

    fn unwind_full_value_stack(&mut self) -> Option<Value<'json>> {
        loop {
            self.unwind_value_stack_once();
            if self.values_being_built.len() <= 1 {
                break;
            }
        }

        match self.values_being_built.pop() {
            None => None,
            Some(val) => Some(val.into()),
        }
    }

    /// Unwinds one value from the value stack.
    ///
    /// Returns false if no value was able to be
    /// unwound, true otherwise
    fn unwind_value_stack_once(&mut self) -> bool {
        if let Some(top) = self.values_being_built.pop() {
            match self.values_being_built.pop() {
                None => {
                    self.values_being_built.push(top);
                    return true;
                }
                Some(new_top) => {
                    match new_top {
                        ValueInProgress::Array(mut vec) => {
                            vec.push(top.into());
                            self.values_being_built.push(ValueInProgress::Array(vec));
                            return true;
                        }
                        ValueInProgress::Object(mut obj) => {
                            let key = match std::mem::take(&mut obj.active_key) {
                                None => JsonString::default(),
                                Some(obj_key) => obj_key,
                            };

                            let mut key_for_map = key;
                            if obj.map.contains_key(&key_for_map) {
                                if key_for_map.span.is_some() {
                                    self.errs.push(JsonParseErr::DuplicateObjectKeys(
                                        match obj
                                            .map
                                            .get_key_value(&key_for_map)
                                            .unwrap()
                                            .0
                                            .span
                                            .as_ref()
                                        {
                                            None => Span::default(),
                                            Some(span) => span.clone(),
                                        },
                                        match key_for_map.span.as_ref() {
                                            None => Span::default(),
                                            Some(span) => span.clone(),
                                        },
                                    ));
                                }

                                let mut counter: u64 = 0;
                                // most of the time this should take < 10 iterations,
                                // so only allocate space for 1 additional ascii digit.
                                let mut sanitized =
                                    String::with_capacity(key_for_map.parsed.as_ref().len() + 1);
                                sanitized.push_str(&key_for_map.sanitized);
                                let mut parsed =
                                    String::with_capacity(key_for_map.parsed.len() + 1);
                                parsed.push_str(&key_for_map.parsed);
                                key_for_map = JsonString {
                                    span: None,
                                    sanitized: Cow::Owned(sanitized),
                                    parsed: Cow::Owned(parsed),
                                };

                                loop {
                                    let counter_str = counter.to_string();
                                    for cow in [&mut key_for_map.sanitized, &mut key_for_map.parsed]
                                    {
                                        if let Cow::Owned(string) = cow {
                                            string.push_str(&counter_str);
                                        } else {
                                            unreachable!();
                                        }
                                    }

                                    if !obj.map.contains_key(&key_for_map) {
                                        break;
                                    }

                                    for cow in [&mut key_for_map.parsed, &mut key_for_map.sanitized]
                                    {
                                        if let Cow::Owned(string) = cow {
                                            for _ in counter_str.chars() {
                                                string.pop();
                                            }
                                        } else {
                                            unreachable!();
                                        }
                                    }
                                    counter += 1;
                                }
                            }

                            obj.insert(key_for_map, top.into());
                            self.values_being_built.push(ValueInProgress::Object(obj));
                            return true;
                        }
                        ValueInProgress::Null
                        | ValueInProgress::Bool(_)
                        | ValueInProgress::String(_)
                        | ValueInProgress::Number(_) => {
                            self.values_being_built
                                .push(ValueInProgress::Array(vec![new_top.into(), top.into()]));
                            return true;
                        }
                    }
                }
            }
        } else {
            return false;
        }
    }

    fn recover_in_panic_mode(&mut self) {
        loop {
            match self.next_token() {
                None => break,
                Some(token) => match token.kind {
                    JsonTokenKind::ArrayStart | JsonTokenKind::ObjectStart => {
                        self.lookahead = Some(token);
                        break;
                    }
                    JsonTokenKind::ArrayEnd => {
                        if !self
                            .states
                            .iter()
                            .rev()
                            .any(|state| *state == JsonParseState::Array)
                        {
                            continue;
                        }

                        while let Some(state) = self.states.pop() {
                            if let JsonParseState::Array = state {
                                break;
                            }
                        }

                        self.lookahead = Some(token);
                        break;
                    }
                    JsonTokenKind::ObjectEnd => {
                        if !self
                            .states
                            .iter()
                            .rev()
                            .any(|state| *state == JsonParseState::Array)
                        {
                            continue;
                        }

                        while let Some(state) = self.states.pop() {
                            if let JsonParseState::Object = state {
                                break;
                            }
                        }

                        self.lookahead = Some(token);
                        break;
                    }
                    JsonTokenKind::Comma => {
                        if !self.states.iter().any(|state| {
                            matches!(state, JsonParseState::Array | JsonParseState::Object)
                        }) {
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
                    }
                    JsonTokenKind::Colon => {
                        if !self.states.iter().any(|state| {
                            matches!(state, JsonParseState::KeyValuePairColon)
                        }) {
                            continue;
                        }
                        break;
                    }
                    JsonTokenKind::Null => {
                        self.values_being_built.push(ValueInProgress::Null);
                        self.unwind_value_stack_once();
                        break;
                    }
                    JsonTokenKind::Number => {
                        self.values_being_built
                            .push(ValueInProgress::Number(JsonNumber::new(
                                &self.json[token.span.as_range()],
                            )));
                        self.unwind_value_stack_once();
                        break;
                    }
                    JsonTokenKind::String => {
                        self.values_being_built
                            .push(ValueInProgress::String(JsonString::new(
                                self.json, token.span,
                            )));
                        self.unwind_value_stack_once();
                        break;
                    }
                    JsonTokenKind::True => {
                        self.values_being_built.push(ValueInProgress::Bool(true));
                        self.unwind_value_stack_once();
                        break;
                    }
                    JsonTokenKind::False => {
                        self.values_being_built.push(ValueInProgress::Bool(false));
                        self.unwind_value_stack_once();
                        break;
                    }
                    JsonTokenKind::Colon => {}
                },
            }
        }
    }

    fn match_token(&mut self, kind: JsonTokenKind) -> Option<JsonToken> {
        match self.next_token() {
            None => return None,
            Some(token) => {
                if token.kind == kind {
                    return Some(token);
                }
                self.lookahead = Some(token);
                return None;
            }
        }
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
                    Err(err) => match &err {
                        // if there are more tokens we want them, so continue
                        JsonParseErr::UnexpectedEOF
                        // not meaningful to parser, so skip
                        | JsonParseErr::TrailingComma(_)
                        // defer to string parser to handle
                        | JsonParseErr::UnclosedString(_)
                        // defer to string parser to handle
                        | JsonParseErr::InvalidUnicodeEscapeSequence(_)
                        // defer to number parser to handle
                        | JsonParseErr::IllegalLeading0(_)
                        | JsonParseErr::DuplicateObjectKeys(_, _) => {
                            self.errs.push(err);
                        }
                        // We want these sequences to show up in the output,
                        // so label them as strings.
                        JsonParseErr::UnexpectedCharacters(span) => {
                            let span = span.clone();
                            self.errs.push(err);
                            if span.end.byte_index() != span.start.byte_index() {
                                return Some(JsonToken {
                                    kind: JsonTokenKind::String,
                                    span,
                                });
                            }
                        }
                    },
                },
            }
        }
    }
}

enum ValueInProgress<'json> {
    Null,
    Bool(bool),
    Number(JsonNumber<'json>),
    String(JsonString<'json>),
    Array(Vec<Value<'json>>),
    Object(ObjectInProgress<'json>),
}

impl<'json> Into<Value<'json>> for ValueInProgress<'json> {
    fn into(self) -> Value<'json> {
        match self {
            ValueInProgress::Null => Value::Null,
            ValueInProgress::Bool(bool) => Value::Bool(bool),
            ValueInProgress::Number(num) => Value::Number(num),
            ValueInProgress::String(str) => Value::String(str),
            ValueInProgress::Array(arr) => Value::Array(arr),
            ValueInProgress::Object(obj) => Value::Object(obj.into()),
        }
    }
}

struct ObjectInProgress<'json> {
    active_key: Option<JsonString<'json>>,
    keys_in_found_order: Vec<Cow<'json, str>>,
    map: HashMap<JsonString<'json>, Value<'json>>,
}

impl<'json> Into<Object<'json>> for ObjectInProgress<'json> {
    fn into(mut self) -> Object<'json> {
        if let Some(active_key) = std::mem::take(&mut self.active_key) {
            self.insert(active_key, Value::Null);
        }

        Object {
            map: self.map,
            keys_in_found_order: self.keys_in_found_order,
        }
    }
}

impl<'json> ObjectInProgress<'json> {
    fn new() -> Self {
        Self {
            active_key: None,
            keys_in_found_order: Vec::new(),
            map: HashMap::new(),
        }
    }

    fn insert(&mut self, key: JsonString<'json>, value: Value<'json>) {
        self.keys_in_found_order.push(key.parsed.to_owned());
        if self.map.contains_key(&key) {
            let mut new_keys = Vec::with_capacity(self.keys_in_found_order.capacity());
            for found in self.keys_in_found_order.iter() {
                if found.as_ref() == key.parsed.as_ref() {
                    continue;
                }
                new_keys.push(found);
            }
        }
        self.map.insert(key, value);
    }
}

#[derive(Debug)]
pub enum Value<'json> {
    Null,
    Bool(bool),
    Number(JsonNumber<'json>),
    String(JsonString<'json>),
    Array(Vec<Value<'json>>),
    Object(Object<'json>),
}

impl<'json> Value<'json> {
    /// Retrieves all values from within this value recursively.
    pub fn get_all_leaves_iter_mut(&mut self) -> impl Iterator<Item = &mut Value<'json>> {
        let mut result = Vec::new();
        match self {
            Value::Null | Value::Bool(_) | Value::Number(_) | Value::String(_) => result.push(self),
            Value::Array(arr) => {
                for item in arr.iter_mut() {
                    for sub_value in item.get_all_leaves_iter_mut() {
                        result.push(sub_value);
                    }
                }
            }
            Value::Object(obj) => {
                for kvp in obj.map.iter_mut() {
                    for sub_value in kvp.1.get_all_leaves_iter_mut() {
                        result.push(sub_value);
                    }
                }
            }
        }
        result.into_iter()
    }
}

#[derive(Debug)]
pub struct Object<'json> {
    map: HashMap<JsonString<'json>, Value<'json>>,
    keys_in_found_order: Vec<Cow<'json, str>>,
}

impl<'json> Object<'json> {
    pub fn keys_in_order_found<'a>(&'a self) -> Iter<'a, Cow<'json, str>> {
        self.keys_in_found_order.iter()
    }

    pub fn get_map(&self) -> &HashMap<JsonString<'json>, Value<'json>> {
        &self.map
    }
}

impl<'json> Value<'json> {
    pub fn to_string(&self) -> String {
        let mut result = String::new();
        self.to_string_helper(&mut result, false, 0, "  ");
        result
    }

    pub fn to_string_pretty(&self) -> String {
        let mut result = String::new();
        self.to_string_helper(&mut result, true, 0, "  ");
        result
    }

    pub fn to_string_pretty_with_indent_str(&self, indent_str: &str) -> String {
        let mut result = String::new();
        self.to_string_helper(&mut result, true, 0, indent_str);
        result
    }

    fn to_string_helper(
        &self,
        buf: &mut String,
        pretty: bool,
        indent_level: usize,
        indent_str: &str,
    ) {
        match self {
            Self::Null => {
                buf.push_str("null");
            }
            Self::Bool(bool) => {
                if *bool {
                    buf.push_str("true");
                } else {
                    buf.push_str("false");
                }
            }
            Self::Number(num) => {
                buf.push_str(num.sanitized);
            }
            Self::String(str) => {
                Self::to_string_for_string(str, buf);
            }
            Self::Array(vec) => {
                buf.push('[');

                for (i, item) in vec.iter().enumerate() {
                    if i != 0 {
                        buf.push(',');
                    }
                    if pretty {
                        buf.push('\n');
                        for _ in 0..indent_level + 1 {
                            buf.push_str(indent_str);
                        }
                    }
                    item.to_string_helper(buf, pretty, indent_level + 1, indent_str);
                }

                if pretty && vec.len() > 0 {
                    buf.push('\n');
                    for _ in 0..indent_level {
                        buf.push_str(indent_str);
                    }
                }

                buf.push(']');
            }
            Self::Object(obj) => {
                buf.push('{');

                for (i, cow) in obj.keys_in_found_order.iter().enumerate() {
                    if i != 0 {
                        buf.push(',');
                    }
                    if pretty {
                        buf.push('\n');
                        for _ in 0..indent_level + 1 {
                            buf.push_str(indent_str);
                        }
                    }
                    let key = obj
                        .map
                        .get_key_value(&JsonString {
                            span: None,
                            parsed: cow.clone(),
                            sanitized: Cow::Borrowed(""),
                        })
                        .expect("BUG: key to be in the object")
                        .0;
                    Self::to_string_for_string(key, buf);
                    buf.push(':');
                    if pretty {
                        buf.push(' ');
                    }

                    let value = obj.map.get(&JsonString {
                        span: None,
                        parsed: cow.clone(), // hash is only determined by the parsed value
                        sanitized: Cow::Borrowed(""),
                    }).expect("BUG: values in the keys in found order vec should always be in the object hashmap as well.");
                    value.to_string_helper(buf, pretty, indent_level + 1, indent_str);
                }

                if pretty && obj.keys_in_found_order.len() > 0 {
                    buf.push('\n');
                    for _ in 0..indent_level {
                        buf.push_str(indent_str);
                    }
                }

                buf.push('}')
            }
        }
    }

    fn to_string_for_string(str: &JsonString, buf: &mut String) {
        buf.push('"');
        buf.push_str(&str.sanitized);
        buf.push('"');
    }
}

#[derive(Clone, Debug)]
pub struct JsonNumber<'json> {
    #[allow(dead_code)]
    source: &'json str,
    sanitized: &'json str,
}

impl<'json> JsonNumber<'json> {
    pub(crate) fn new(source: &'json str) -> Self {
        Self {
            source,
            sanitized: Self::sanitize(source),
        }
    }

    pub fn sanitized(&self) -> &'json str {
        self.sanitized
    }

    fn sanitize(source: &str) -> &str {
        let mut chars = source.chars().peekable();
        let mut num_matched_zeroes = 0;
        while let Some('0') = chars.peek() {
            chars.next();
            num_matched_zeroes += 1;
        }

        if num_matched_zeroes == 0 {
            return source;
        }

        match chars.peek() {
            Some('.' | 'e' | 'E') => {
                return &source[num_matched_zeroes - 1..];
            }
            Some('0'..='9') => {
                return &source[num_matched_zeroes..];
            }
            _ => {
                return source;
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct JsonString<'json> {
    pub(crate) span: Option<Span>,
    pub(crate) parsed: Cow<'json, str>,
    pub(crate) sanitized: Cow<'json, str>,
}

impl<'json> From<Cow<'json, str>> for JsonString<'json> {
    fn from(value: Cow<'json, str>) -> Self {
        Self {
            span: None,
            parsed: value,
            sanitized: Cow::Borrowed(""),
        }
    }
}

impl<'json> JsonString<'json> {
    pub(crate) fn new(original_json: &'json str, span: Span) -> Self {
        let range = span.as_range();
        Self {
            span: Some(span),
            parsed: Self::unescape(&original_json[range.clone()]),
            sanitized: Self::sanitize(&original_json[range]),
        }
    }

    pub fn sanitized(&self) -> &str {
        self.sanitized.as_ref()
    }

    pub fn unescape(source: &str) -> Cow<'_, str> {
        let result = Self::parse(source, true);
        #[cfg(debug_assertions)]
        println!("\n\n\nUnescaped: {source}\n\n\nResult: {result}\n\n\n");
        result
    }

    fn sanitize(source: &str) -> Cow<'_, str> {
        let result = Self::parse(source, false);
        #[cfg(debug_assertions)]
        println!("\n\n\nUnescaped: {source}\n\n\nSanitized: {result}\n\n\n");
        result
    }

    fn parse(mut source: &str, replace_escape_chars: bool) -> Cow<'_, str> {
        // Remove the quote at the beginning (if there is one)
        if let Some('"') = source.chars().next() {
            source = &source[1..];
        }

        let mut chars = source.char_indices().peekable();
        let mut cow = Cow::Borrowed(source);

        loop {
            let ch = chars.next();
            match ch {
                None => break,
                Some((i, ch)) => {
                    match ch {
                        '\\' => {
                            let mut string = match cow {
                                Cow::Borrowed(_) => source[..i].to_string(),
                                Cow::Owned(string) => string,
                            };

                            if !replace_escape_chars {
                                string.push('\\');
                            }

                            match chars.next() {
                                None => {
                                    string.push('\\');
                                    cow = Cow::Owned(string);
                                    break;
                                }
                                Some((_, next_ch)) => {
                                    let ch_to_add = match next_ch {
                                        '"' => '"',
                                        '\\' => '\\',
                                        '/' => '/',
                                        'b' => {
                                            if replace_escape_chars {
                                                '\u{0008}'
                                            } else {
                                                'b'
                                            }
                                        }
                                        'f' => {
                                            if replace_escape_chars {
                                                '\u{000c}'
                                            } else {
                                                'f'
                                            }
                                        }
                                        'n' => {
                                            if replace_escape_chars {
                                                '\n'
                                            } else {
                                                'n'
                                            }
                                        }
                                        'r' => {
                                            if replace_escape_chars {
                                                '\r'
                                            } else {
                                                'r'
                                            }
                                        }
                                        't' => {
                                            if replace_escape_chars {
                                                '\t'
                                            } else {
                                                't'
                                            }
                                        }
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
                                                        Some(ch) => {
                                                            if replace_escape_chars {
                                                                string.push(ch);
                                                            } else {
                                                                string.push('u');
                                                                string.push_str(&code);
                                                            }
                                                        }
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
                                        ch => {
                                            // a '\\' gets added if we're not replacing escape chars for
                                            // all the other code paths, so we need to account for that.
                                            if replace_escape_chars {
                                                string.push('\\');
                                            }
                                            string.push('\\');
                                            string.push(ch);
                                            cow = Cow::Owned(string);
                                            continue;
                                        }
                                    };

                                    string.push(ch_to_add);
                                    cow = Cow::Owned(string)
                                }
                            }
                        }
                        '"' => {
                            // don't escape the ending quote. Quotes are ascii, so -1 is safe
                            if i == source.len() - 1 {
                                cow = match cow {
                                    Cow::Owned(string) => Cow::Owned(string),
                                    Cow::Borrowed(str) => Cow::Borrowed(&str[..str.len() - 1]),
                                };
                                continue;
                            }

                            let mut string = match cow {
                                Cow::Borrowed(_) => source[..i].to_string(),
                                Cow::Owned(string) => string,
                            };

                            string.push_str("\\\"");
                            cow = Cow::Owned(string);
                        }
                        '\n' | '\u{0008}' | '\u{000c}' | '\r' | '\t' => {
                            if replace_escape_chars {
                                cow = match cow {
                                    Cow::Borrowed(_) => Cow::Borrowed(&source[..i]),
                                    Cow::Owned(mut string) => {
                                        string.push(ch);
                                        Cow::Owned(string)
                                    }
                                };
                                continue;
                            }

                            let mut string = match cow {
                                Cow::Borrowed(_) => source[..i].to_string(),
                                Cow::Owned(string) => string,
                            };

                            string.push('\\');
                            string.push(match ch {
                                '\u{0008}' => 'b',
                                '\u{000c}' => 'f',
                                '\n' => 'n',
                                '\r' => 'r',
                                '\t' => 't',
                                _ => unreachable!(),
                            });
                            cow = Cow::Owned(string);
                        }
                        ch => {
                            if ch.is_control() {
                                if replace_escape_chars {
                                    cow = match cow {
                                        Cow::Borrowed(_) => Cow::Owned(source[..i].to_string()),
                                        Cow::Owned(mut string) => {
                                            string.push(ch);
                                            Cow::Owned(string)
                                        }
                                    };
                                    continue;
                                }

                                cow = match cow {
                                    Cow::Owned(mut string) => {
                                        string.push_str("\\u");
                                        let num_string = (ch as u32).to_string();
                                        for _ in 0..(4 - num_string.len()) {
                                            string.push('0');
                                        }
                                        string.push_str(&num_string);
                                        Cow::Owned(string)
                                    }
                                    Cow::Borrowed(str) => {
                                        let mut string = str.to_string();
                                        string.push_str("\\u");
                                        let num_string = (ch as u32).to_string();
                                        for _ in 0..(4 - num_string.len()) {
                                            string.push('0');
                                        }
                                        string.push_str(&num_string);
                                        Cow::Owned(string)
                                    }
                                };
                                continue;
                            }

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

        cow
    }
}

impl<'json> std::hash::Hash for JsonString<'json> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match &self.parsed {
            Cow::Borrowed(str) => str.hash(state),
            Cow::Owned(string) => string.hash(state),
        }
    }
}

impl<'json> PartialEq for JsonString<'json> {
    fn eq(&self, other: &Self) -> bool {
        self.parsed.eq(&other.parsed)
    }
}

impl<'json> Eq for JsonString<'json> {}

impl<'json> Default for JsonString<'json> {
    fn default() -> Self {
        Self {
            span: None,
            parsed: DEFAULT_KEY_COW.clone(),
            sanitized: DEFAULT_KEY_COW.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn obj_keys_with_escapes_work() {
        let input = "{\"key\\\\ \\t\\b\\n\\a\":10\"}";
        let (output, _) = crate::parse(input);

        println!("{}", output.to_string());
        println!("{}", output.to_string_pretty());
    }
}
