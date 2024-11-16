use crate::Location;

pub(crate) struct CharLocations<CharIndices, PassThroughErr>
where
    CharIndices: Iterator<Item = Result<char, PassThroughErr>>,
{
    done: bool,
    source: CharIndices,
    line: usize,
    col: usize,
    previous_was_new_line: bool,
}

impl<CharIndices, PassThroughErr> CharLocations<CharIndices, PassThroughErr>
where
    CharIndices: Iterator<Item = Result<char, PassThroughErr>>,
{
    pub(crate) fn new(source: CharIndices) -> Self {
        Self {
            done: false,
            source: source,
            line: 0,
            col: 0,
            previous_was_new_line: false,
        }
    }

    fn get_current_location(&self) -> Location {
        Location::new(self.line, self.col)
    }
}

impl<CharIndices, PassThroughErr> Iterator for CharLocations<CharIndices, PassThroughErr>
where
    CharIndices: Iterator<Item = Result<char, PassThroughErr>>,
{
    type Item = (Location, Result<char, PassThroughErr>);

    fn next(&mut self) -> Option<Self::Item> {
        // Safety check. Don't overrun the end of any buffers
        if self.done {
            return None;
        }

        let next = match self.source.next() {
            None => {
                self.done = true;
                return None;
            }
            Some(next) => next,
        };

        if self.previous_was_new_line {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }

        match next {
            Ok(next) => {
                self.previous_was_new_line = next == '\n';
                Some((self.get_current_location(), Ok(next)))
            }
            Err(err) => Some((self.get_current_location(), Err(err))),
        }
    }
}
