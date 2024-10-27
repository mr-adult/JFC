use std::ops::Index;

use crate::location::Location;

#[derive(Clone, Debug)]
pub struct Span {
    pub start: Location,
    pub end: Location,
}

impl Default for Span {
    fn default() -> Self {
        use crate::NonZero;

        let location =
            unsafe { Location::new(0, NonZero::new_unchecked(1), NonZero::new_unchecked(1)) };

        Self {
            start: location.clone(),
            end: location,
        }
    }
}

impl Span {
    pub fn new(start: Location, end: Location) -> Self {
        Self { start, end }
    }

    pub fn start(&self) -> &Location {
        &self.start
    }

    pub fn end(&self) -> &Location {
        &self.end
    }
}

impl Index<&Span> for String {
    type Output = str;
    fn index(&self, index: &Span) -> &Self::Output {
        &self[index.start.byte_index()..index.end.byte_index()]
    }
}

impl Index<&Span> for str {
    type Output = str;
    fn index(&self, index: &Span) -> &Self::Output {
        &self[index.start.byte_index()..index.end.byte_index()]
    }
}
