use std::fmt::Display;

#[derive(Clone, Debug)]
pub(crate) struct Location {
    line: usize,
    col: usize,
}

impl Location {
    pub(crate) fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }

    pub(crate) fn line(&self) -> usize {
        self.line
    }

    pub(crate) fn col(&self) -> usize {
        self.col
    }

    pub(crate) fn incremented(&self) -> Self {
        Self::new(self.line, self.col + 1)
    }
}

impl Default for Location {
    fn default() -> Self {
        Self { line: 0, col: 0 }
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let line: usize = self.line.into();
        let col: usize = self.col.into();
        write!(f, "line: {0}, col: {1}", line, col)
    }
}
