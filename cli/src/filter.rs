pub(crate) struct Filter {}

impl Filter {
    pub(crate) fn new(raw_filter: &str) -> Result<Self, FilterSyntaxErr> {
        Ok(Self {})
    }
}

impl Iterator for Filter {
    type Item = Operation;

    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}

pub(crate) enum Operation {
    /// The simplest filter: return stdin on stdout
    Identity,
    /// A simple index on a key. Ex .hello indexes into the object on
    /// the "hello" key. Indexes are also placed into this category.
    Identifier(Box<Identifier>),
    /// Represents the operation of wrapping a series of values into a
    /// list together
    WrapInList,
}

pub(crate) struct Identifier {
    name: String,
    optional: bool,
}

pub(crate) enum FilterSyntaxErr {}
