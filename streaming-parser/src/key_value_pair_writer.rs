use std::collections::VecDeque;

use serde::Serialize;

pub struct KeyValuePairWriter<T, Iter>
where
    Iter: IntoIterator<Item = (String, T)>,
    T: Serialize,
{
    state: ValueWriterState,
    iter: Iter::IntoIter,
    queued: VecDeque<String>,
    value: Option<T>,
}

enum ValueWriterState {
    Start,
    FirstKey,
    Key,
    Colon,
    Value,
    EndingBracket,
    End,
}

impl<T, Iter> KeyValuePairWriter<T, Iter>
where
    Iter: IntoIterator<Item = (String, T)>,
    T: Serialize,
{
    pub(crate) fn new(values: Iter) -> Self {
        Self {
            state: ValueWriterState::Start,
            iter: values.into_iter(),
            queued: VecDeque::with_capacity(3),
            value: None,
        }
    }
}

impl<T, Iter> Iterator for KeyValuePairWriter<T, Iter>
where
    Iter: IntoIterator<Item = (String, T)>,
    T: Serialize,
{
    type Item = Result<String, serde_json::Error>;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(queued) = self.queued.pop_front() {
            return Some(Ok(queued));
        }

        loop {
            match self.state {
                ValueWriterState::Start => {
                    self.state = ValueWriterState::FirstKey;
                    return Some(Ok("{".to_string()));
                }
                ValueWriterState::FirstKey => {
                    if let Some(next) = self.iter.next() {
                        self.state = ValueWriterState::Colon;
                        self.value = Some(next.1);
                        self.queued.push_back(next.0);
                        self.queued.push_back("\"".to_string());
                        return Some(Ok("\"".to_string()));
                    }

                    self.state = ValueWriterState::EndingBracket;
                }
                ValueWriterState::Key => {
                    if let Some(next) = self.iter.next() {
                        self.state = ValueWriterState::Colon;
                        self.value = Some(next.1);

                        let string = match serde_json::to_string(&next.0) {
                            Err(err) => return Some(Err(err)),
                            Ok(val) => val,
                        };

                        self.queued.push_back("\"".to_string());
                        self.queued.push_back(string);
                        self.queued.push_back("\"".to_string());
                        return Some(Ok(",".to_string()));
                    }

                    self.state = ValueWriterState::EndingBracket;
                }
                ValueWriterState::Colon => {
                    self.state = ValueWriterState::Value;
                    return Some(Ok(":".to_string()));
                }
                ValueWriterState::Value => {
                    if let Some(next) = std::mem::take(&mut self.value) {
                        self.state = ValueWriterState::Key;
                        return Some(serde_json::to_string(&next));
                    }

                    self.state = ValueWriterState::EndingBracket;
                }
                ValueWriterState::EndingBracket => {
                    self.state = ValueWriterState::End;
                    return Some(Ok("}".to_string()));
                }
                ValueWriterState::End => {
                    return None;
                }
            }
        }
    }
}
