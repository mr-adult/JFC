#[derive(Debug, Copy, Clone)]
/// A wrapper around usize guaranteeing that this value is nonzero
pub struct NonZero(usize);

impl NonZero {
    pub unsafe fn new_unchecked(x: usize) -> NonZero {
        debug_assert!(x != 0);
        NonZero(x)
    }

    pub unsafe fn decrement(&mut self) {
        assert!(self.0 != 1);
        self.0 -= 1;
    }

    pub fn increment(&mut self) {
        self.0 += 1;
    }
}

impl Into<usize> for NonZero {
    fn into(self) -> usize {
        self.0
    }
}
