use serde::Serialize;

pub struct ValueWriter<Iter>
where
    Iter: IntoIterator,
    Iter::Item: Serialize,
{
    state: ValueWriterState,
    iter: Iter::IntoIter,
    queued: Option<Result<String, serde_json::Error>>,
}

enum ValueWriterState {
    Start,
    FirstValue,
    Value,
    EndingBracket,
    End,
}

impl<Iter> ValueWriter<Iter>
where
    Iter: IntoIterator,
    Iter::Item: Serialize,
{
    pub(crate) fn new(values: Iter) -> ValueWriter<Iter> {
        Self {
            state: ValueWriterState::Start,
            iter: values.into_iter(),
            queued: None,
        }
    }
}

impl<Iter> Iterator for ValueWriter<Iter>
where
    Iter: IntoIterator,
    Iter::Item: Serialize,
{
    type Item = Result<String, serde_json::Error>;
    fn next(&mut self) -> Option<Self::Item> {
        let queued = std::mem::take(&mut self.queued);
        if queued.is_some() {
            return queued;
        }

        loop {
            match self.state {
                ValueWriterState::Start => {
                    self.state = ValueWriterState::FirstValue;
                    return Some(Ok("[".to_string()));
                }
                ValueWriterState::FirstValue => {
                    if let Some(next) = self.iter.next() {
                        self.state = ValueWriterState::Value;
                        return Some(serde_json::to_string(&next));
                    }

                    self.state = ValueWriterState::EndingBracket;
                }
                ValueWriterState::Value => {
                    if let Some(next) = self.iter.next() {
                        self.queued = Some(serde_json::to_string(&next));
                        return Some(Ok(",".to_string()));
                    }

                    self.state = ValueWriterState::EndingBracket;
                }
                ValueWriterState::EndingBracket => {
                    self.state = ValueWriterState::End;
                    return Some(Ok("]".to_string()));
                }
                ValueWriterState::End => {
                    return None;
                }
            }
        }
    }
}
