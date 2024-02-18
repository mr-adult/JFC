use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    marker::PhantomData,
    str::FromStr,
};

use crate::{
    tokenizer::{JsonParseErr, JsonToken, JsonTokenKind, JsonTokenizer},
    JsonParseState,
};

static DEFAULT_KEY: &'static str = "unknown_key";
static DEFAULT_KEY_COW: Cow<'static, str> = Cow::Borrowed(&DEFAULT_KEY);

pub(crate) struct JsonParser<'json> {
    tokenizer: JsonTokenizer<'json>,
    string_store: &'json mut StringStore<'json>,
    json: &'json str,
    lookahead: Option<JsonToken>,
    states: Vec<JsonParseState>,
    values_being_built: Vec<ValueInProgress<'json>>,
    errs: Vec<JsonParseErr>,
}

impl<'json> JsonParser<'json> {
    fn new(json: &'json str, string_store: &'json mut StringStore<'json>) -> Self {
        Self {
            tokenizer: JsonTokenizer::new(json),
            string_store,
            json,
            lookahead: None,
            states: Vec::new(),
            values_being_built: Vec::new(),
            errs: Vec::new(),
        }
    }

    pub(crate) fn parse(
        json: &'json str,
        string_store: &'json mut StringStore<'json>,
    ) -> (Value<'json>, Vec<JsonParseErr>) {
        Self::new(json, string_store).parse_internal()
    }

    fn parse_internal(mut self) -> (Value<'json>, Vec<JsonParseErr>) {
        loop {
            match self.states.pop() {
                None => {
                    self.match_token(JsonTokenKind::Comma);
                    self.states.push(JsonParseState::Value);
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
                                        }
                                        JsonTokenKind::ArrayStart => {
                                            self.values_being_built
                                                .push(ValueInProgress::Array(Vec::new()));
                                        }
                                        JsonTokenKind::String => {
                                            self.values_being_built.push(ValueInProgress::String(
                                                JsonString::new(&self.json[token.span.as_range()]),
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
                                        obj.active_key = Some(JsonString::new(
                                            &self.json[str_token.span.as_range()],
                                        ));
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

    fn pop_object(&mut self) -> Option<Value<'json>> {
        if !self
            .values_being_built
            .iter()
            .rev()
            .any(|val| matches!(val, ValueInProgress::Object(_)))
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
            .any(|val| matches!(val, ValueInProgress::Object(_)))
        {
            return None;
        }

        todo!()
    }

    fn unwind_full_value_stack(&mut self) -> Option<Value<'json>> {
        loop {
            if !self.unwind_value_stack_once() {
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
                    self.values_being_built
                        .push(ValueInProgress::Array(vec![top.into()]));
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
                                let mut counter: u64 = 0;
                                // most of the time this should take < 10 iterations,
                                // so only allocate space for 1 ascii digit.
                                let mut ident =
                                    String::with_capacity(key_for_map.cow.as_ref().len() + 1);
                                loop {
                                    let counter_str = counter.to_string();
                                    ident.push_str(&counter_str);
                                    if !obj.map.contains_key(&JsonString::new(&ident)) {
                                        key_for_map =
                                            JsonString::new(self.string_store.push(ident));
                                        break;
                                    }

                                    for _ in counter_str.chars() {
                                        ident.pop();
                                    }
                                    counter += 1;
                                }
                            }

                            obj.map.insert(key_for_map, top.into());
                            self.values_being_built.push(ValueInProgress::Object(obj));
                            return true;
                        }
                        ValueInProgress::Null => {
                            todo!("Right now, I've implemented the simple form of this. Ideally, I would like to unwind the stack here.");
                            self.values_being_built
                                .push(ValueInProgress::Array(vec![top.into(), Value::Null]));
                            return true;
                        }
                        ValueInProgress::Bool(bool) => {
                            todo!("Right now, I've implemented the simple form of this. Ideally, I would like to unwind the stack here.");
                            self.values_being_built
                                .push(ValueInProgress::Array(vec![top.into(), Value::Bool(bool)]));
                            return true;
                        }
                        ValueInProgress::String(str) => {
                            todo!("Right now, I've implemented the simple form of this. Ideally, I would like to unwind the stack here.");
                            self.values_being_built
                                .push(ValueInProgress::Array(vec![top.into(), Value::String(str)]));
                            return true;
                        }
                        ValueInProgress::Number(num) => {
                            todo!("Right now, I've implemented the simple form of this. Ideally, I would like to unwind the stack here.");
                            self.values_being_built
                                .push(ValueInProgress::Array(vec![top.into(), Value::Number(num)]));
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
                    JsonTokenKind::Null
                    | JsonTokenKind::Number
                    | JsonTokenKind::String
                    | JsonTokenKind::True
                    | JsonTokenKind::False
                    | JsonTokenKind::Colon => {}
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
                        | JsonParseErr::IllegalLeading0(_) => {
                            self.errs.push(err);
                        }
                        // We want these sequences to show up in the output,
                        // so label them as strings.
                        JsonParseErr::UnexpectedCharacters(span) => {
                            let span = span.clone();
                            self.errs.push(err);
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

pub struct StringStore<'s> {
    strings: Option<*mut StoredString>,
    all_strings: HashSet<&'s String>,
    _phantom: PhantomData<&'s ()>,
}

impl<'s> StringStore<'s> {
    pub fn new() -> Self {
        Self {
            strings: None,
            all_strings: HashSet::new(),
            _phantom: PhantomData,
        }
    }

    fn push(&mut self, str: String) -> &'s String {
        let ptr = &mut StoredString {
            str,
            next: match self.strings {
                None => None,
                Some(ptr) => Some(unsafe { Box::from_raw(ptr) }),
            },
        } as *mut StoredString;

        self.all_strings
            .insert(unsafe { &(*ptr).str } as &'s String);
        self.strings = Some(ptr);
        unsafe { &(*ptr).str }
    }
}

impl<'s> Drop for StringStore<'s> {
    fn drop(&mut self) {
        match self.strings {
            None => {}
            Some(ptr) => {
                unsafe { drop(Box::from_raw(ptr)) };
            }
        };
    }
}

struct StoredString {
    str: String,
    next: Option<Box<StoredString>>,
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
    map: HashMap<JsonString<'json>, Value<'json>>,
}

impl<'json> Into<HashMap<JsonString<'json>, Value<'json>>> for ObjectInProgress<'json> {
    fn into(self) -> HashMap<JsonString<'json>, Value<'json>> {
        self.map
    }
}

impl<'json> ObjectInProgress<'json> {
    fn new() -> Self {
        Self {
            active_key: None,
            map: HashMap::new(),
        }
    }
}

pub enum Value<'json> {
    Null,
    Bool(bool),
    Number(JsonNumber<'json>),
    String(JsonString<'json>),
    Array(Vec<Value<'json>>),
    Object(HashMap<JsonString<'json>, Value<'json>>),
}

impl<'json> Value<'json> {
    pub fn to_string(&self) -> String {
        let mut result = String::new();
        self.to_string_helper(&mut result, false, 0);
        result
    }

    pub fn to_string_pretty(&self) -> String {
        let mut result = String::new();
        self.to_string_helper(&mut result, true, 0);
        result
    }

    fn to_string_helper(&self, buf: &mut String, pretty: bool, indent_level: usize) {
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
                buf.push_str(num.source);
            }
            Self::String(str) => {
                buf.push_str(&str.cow);
            }
            Self::Array(vec) => {
                if pretty {
                    buf.push('\n');
                    for _ in 0..indent_level {
                        buf.push(' ');
                        buf.push(' ');
                    }
                }
                buf.push('[');

                for item in vec {
                    if pretty {
                        for _ in 0..indent_level + 1 {
                            buf.push(' ');
                            buf.push(' ');
                        }
                    }
                    item.to_string_helper(buf, pretty, indent_level + 1);
                    buf.push(',');
                    if pretty {
                        buf.push('\n');
                    }
                }
                // pop that last ','
                buf.pop();
                if pretty {
                    buf.pop();
                    buf.push('\n');
                    for _ in 0..indent_level {
                        buf.push(' ');
                        buf.push(' ');
                    }
                }

                buf.push(']');
            }
            Self::Object(map) => {
                if pretty {
                    buf.push('\n');
                    for _ in 0..indent_level {
                        buf.push(' ');
                        buf.push(' ');
                    }
                }
                buf.push('{');
                for kvp in map {
                    if pretty {
                        for _ in 0..indent_level {
                            buf.push(' ');
                            buf.push(' ');
                        }
                    }
                    buf.push_str(&kvp.0.cow);
                    buf.push(':');
                    if pretty {
                        buf.push(' ');
                    }
                    kvp.1.to_string_helper(buf, pretty, indent_level + 1);
                    buf.push(',');
                    if pretty {
                        buf.push('\n');
                    }
                }
                // pop that last ','
                buf.pop();
                if pretty {
                    buf.pop();
                    buf.push('\n');
                    for _ in 0..indent_level {
                        buf.push(' ');
                        buf.push(' ');
                    }
                }
                buf.push('}')
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct JsonNumber<'json> {
    source: &'json str,
}

impl<'json> JsonNumber<'json> {
    pub(crate) fn new(source: &'json str) -> Self {
        Self { source }
    }

    pub(crate) fn parse<T>(&self) -> Result<T, <T as FromStr>::Err>
    where
        T: FromStr,
    {
        self.source.parse::<T>()
    }
}

#[derive(Clone, Debug)]
pub struct JsonString<'json> {
    source: &'json str,
    cow: Cow<'json, str>,
}

impl<'json> JsonString<'json> {
    pub(crate) fn new(source: &'json str) -> Self {
        Self {
            source,
            cow: Self::escape(source),
        }
    }

    fn new_unchecked(source: &'json str) -> Self {
        Self {
            source,
            cow: Cow::Borrowed(source),
        }
    }

    pub(crate) fn raw(&self) -> &str {
        self.source
    }

    pub(crate) fn parsed(&self) -> &Cow<'json, str> {
        &self.cow
    }

    /// Handles escaping characters from the string. If the string
    /// is not a valid JSON string, returns None. If the string is
    /// parsed without issue, returns Some() with the Cow containing
    /// the escaped string.
    pub(crate) fn escape(source: &str) -> Cow<'_, str> {
        let mut cow = Cow::Borrowed(source);

        let mut chars = source.char_indices().peekable();
        loop {
            let ch = chars.next();
            match ch {
                None => return cow,
                Some((i, ch)) => {
                    if ch == '\\' {
                        let mut string = match cow {
                            Cow::Borrowed(_) => source[..i].to_string(),
                            Cow::Owned(string) => string,
                        };

                        match chars.next() {
                            None => {
                                string.push('\\');
                                return Cow::Owned(string);
                            }
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
                                    _ => {
                                        string.push('\\');
                                        return Cow::Owned(string);
                                    }
                                };
                                string.push(ch_to_add);
                                cow = Cow::Owned(string)
                            }
                        }
                    } else if ch.is_control() {
                        continue;
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

impl<'json> std::hash::Hash for JsonString<'json> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.cow.hash(state);
    }
}

impl<'json> PartialEq for JsonString<'json> {
    fn eq(&self, other: &Self) -> bool {
        self.cow.eq(&other.cow)
    }
}

impl<'json> Eq for JsonString<'json> {}

impl<'json> Default for JsonString<'json> {
    fn default() -> Self {
        Self {
            source: DEFAULT_KEY,
            cow: DEFAULT_KEY_COW.clone(),
        }
    }
}
