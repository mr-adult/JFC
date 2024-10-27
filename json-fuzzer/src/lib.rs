use std::iter::Filter;

use rand::{
    distributions::{DistIter, Standard},
    rngs::ThreadRng,
    thread_rng, Rng, RngCore,
};

pub fn fuzz() -> impl Iterator<Item = String> {
    JsonFuzzer(
        thread_rng(),
        thread_rng()
            .sample_iter(Standard)
            .filter(|ch| (*ch as u32) < 0x10FFFF),
    )
}

struct JsonFuzzer<F: FnMut(&char) -> bool>(
    ThreadRng,
    Filter<DistIter<Standard, ThreadRng, char>, F>,
);

impl<F: FnMut(&char) -> bool> JsonFuzzer<F> {
    fn next_escape_sequence(&mut self) -> String {
        match self.0.next_u32() % 16 {
            0 => "\\\"".to_string(),
            1 => "\\\\".to_string(),
            2 => "\\/".to_string(),
            3 => "\\b".to_string(),
            4 => "\\f".to_string(),
            5 => "\\n".to_string(),
            6 => "\\r".to_string(),
            7 => "\\t".to_string(),
            8..=15 => {
                let mut result = String::with_capacity(6);
                result.push_str("\\u");
                let mut code = self.0.next_u32();
                let mut chars = Vec::with_capacity(10);
                while code > 0 {
                    let next_digit = code % 16;
                    let ch = match next_digit {
                        0..=9 => ('0' as u8 + next_digit as u8) as char,
                        10 => match self.0.next_u32() % 2 {
                            0 => 'a',
                            1 => 'A',
                            _ => unreachable!(),
                        },
                        11 => match self.0.next_u32() % 2 {
                            0 => 'b',
                            1 => 'B',
                            _ => unreachable!(),
                        },
                        12 => match self.0.next_u32() % 2 {
                            0 => 'c',
                            1 => 'C',
                            _ => unreachable!(),
                        },
                        13 => match self.0.next_u32() % 2 {
                            0 => 'd',
                            1 => 'D',
                            _ => unreachable!(),
                        },
                        14 => match self.0.next_u32() % 2 {
                            0 => 'e',
                            1 => 'E',
                            _ => unreachable!(),
                        },
                        15 => match self.0.next_u32() % 2 {
                            0 => 'f',
                            1 => 'F',
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };
                    chars.push(ch);
                    code = code / 16;
                }

                while let Some(ch) = chars.pop() {
                    result.push(ch);
                }

                result
            }
            _ => unreachable!(),
        }
    }
}

impl<F: FnMut(&char) -> bool> Iterator for JsonFuzzer<F> {
    type Item = String;
    fn next(&mut self) -> Option<Self::Item> {
        let mut string = String::new();
        for _ in 0..10_000 {
            match self.0.next_u32() % 24 {
                0 => {
                    string.push('\n');
                }
                1 => {
                    string.push('{');
                }
                2 => {
                    string.push('}');
                }
                3 => {
                    string.push('[');
                }
                4 => {
                    string.push(']');
                }
                5 => {
                    string.push(',');
                }
                6 => {
                    string.push(':');
                }
                7 => {
                    string.push_str(&self.next_escape_sequence());
                }
                8 => {
                    string.push('"');

                    let mut i = 0;
                    let end = self.0.next_u32() % 100;
                    while i < end {
                        match self.1.next() {
                            None => unreachable!("char producer should always produce a char"),
                            Some(ch) => match ch {
                                '"' => string.push_str("\\\""),
                                '\\' => {
                                    string.push('\\');
                                    string.push_str(&self.next_escape_sequence());
                                }
                                _ => string.push(ch),
                            },
                        }

                        i += 1;
                    }

                    string.push('"');
                }
                9 => {
                    let num_type = self.0.next_u32() % 3;
                    for _ in 0..(self.0.next_u32() % 100) {
                        let digit = self.0.next_u32() % 10;
                        let ch = ('0' as u8 + digit as u8) as char;
                        string.push(ch);
                    }

                    if num_type > 0 {
                        string.push('.');
                        for _ in 0..(self.0.next_u32() % 100) {
                            let digit = self.0.next_u32() % 10;
                            let ch = ('0' as u8 + digit as u8) as char;
                            string.push(ch);
                        }

                        if self.0.next_u32() % 10 == 1 {
                            for _ in 0..(self.0.next_u32() % 10) {
                                string.push(self.1.next().unwrap());
                            }
                        }
                    }

                    if num_type > 1 {
                        if self.0.next_u32() % 10 == 1 {
                            for _ in 0..(self.0.next_u32() % 10) {
                                string.push(self.1.next().unwrap());
                            }
                        }

                        match self.0.next_u32() % 2 {
                            0 => string.push('e'),
                            1 => string.push('E'),
                            _ => unreachable!(),
                        }

                        if self.0.next_u32() % 10 == 1 {
                            for _ in 0..(self.0.next_u32() % 10) {
                                string.push(self.1.next().unwrap());
                            }
                        }
                    }
                }
                10 => {
                    string.push_str("null");
                }
                11 => {
                    string.push_str("true");
                }
                12 => {
                    string.push_str("false");
                }
                13.. => {
                    let ws_char = match self.0.next_u32() % 4 {
                        0 => ' ',
                        1 => '\n',
                        2 => '\r',
                        3 => '\t',
                        _ => unreachable!(),
                    };
                    string.push(ws_char);
                }
            }
        }

        Some(string)
    }
}

#[cfg(test)]
mod tests {
    use crate::fuzz;

    #[test]
    fn test() {
        for str in fuzz() {}
    }
}
