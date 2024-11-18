#![doc = include_str!("../README.md")]

mod char_locations;
mod de;
mod key_value_pair_stream;
mod key_value_pair_writer;
mod location;
#[cfg(test)]
mod tests;
mod tokenizer;
mod value_stream;
mod value_writer;

use serde::{Deserialize, Serialize};

use crate::location::Location;
pub use key_value_pair_stream::KeyValuePairStream;
pub use key_value_pair_writer::KeyValuePairWriter;
pub use tokenizer::{ErrKind, Error};
pub use value_stream::ValueStream;
pub use value_writer::ValueWriter;

/// Lazily parses the JSON from the given stream of characters. If the stream of characters
/// does not represent a valid stream of JSON, then this stream will continue parsing until
/// failure.
///
/// For example, if given the stream `[1,2,3,4,5,"a non-i32 value"]` and the target type of i32,
/// the ValueStream would yield 1, 2, 3, 4, and 5 before failing on the string value and yielding
/// an error.
pub fn from_value_stream<T, CharsStream, PassThroughError>(
    stream: CharsStream,
) -> ValueStream<T, CharsStream, PassThroughError>
where
    T: for<'de> Deserialize<'de>,
    CharsStream: IntoIterator<Item = Result<char, PassThroughError>>,
    PassThroughError: std::error::Error,
{
    ValueStream::new(stream)
}

/// Lazily parses the JSON from the given stream of characters. If the stream of characters
/// does not represent a valid stream of JSON object key-value pairs, then this stream will
/// continue parsing until failure.
///
/// For example, if given the stream `{"1": 1, "2": 2, "3": 3, "4": 4, "5": 5,"6": "a non-i32 value"]`
/// and the target type of i32, the ValueStream would yield the key value pairs for 1, 2, 3,
/// 4, and 5 before failing on the string value and yielding
/// an error.
pub fn from_key_value_pair_stream<T, CharsStream, PassThroughError>(
    stream: CharsStream,
) -> KeyValuePairStream<T, CharsStream, PassThroughError>
where
    T: for<'de> Deserialize<'de>,
    CharsStream: IntoIterator<Item = Result<char, PassThroughError>>,
    PassThroughError: std::error::Error,
{
    KeyValuePairStream::new(stream)
}

/// Lazily serializes the given values into a series of JSON array segments.
/// The ValueWriter is an Iterator which will succeed in creating the JSON
/// unless the `Iter::Item` type's serialize implementation can fail. JSON
/// is created on-demand, so the caller can flush it to an output target
/// incrementally.
pub fn values_to_json_stream<Iter>(values: Iter) -> ValueWriter<Iter>
where
    Iter: IntoIterator,
    Iter::Item: Serialize,
{
    ValueWriter::new(values)
}

/// Lazily serializes the given key value pairs into a series of JSON object segments.
/// The ValueWriter is an sIterator which will ucceed in creating the JSON
/// unless the `Iter::Item` type's serialize implementation can fail. JSON is created
/// on-demand, so the caller can flush it to an output target incrementally.
pub fn key_value_pairs_to_json_stream<T, Iter>(values: Iter) -> KeyValuePairWriter<T, Iter>
where
    Iter: IntoIterator<Item = (String, T)>,
    T: Serialize,
{
    KeyValuePairWriter::new(values)
}
