# Streaming Serde JSON

This project is still experimental and not intended for production use. It has
not been optimized to use any hardware capabilities like SIMD. The main
serde_json crate is a better fit for all non-streaming use cases.

## Goal

This library aims to solve a very simple to understand problem. JSON files can
be larger than the memory limitations of the machine that is processing the
data. As an example, if I have a file with 300 GB of JSON u64 values as input
from a hardware device and am working on a machine with only 64 GB of RAM, that
will not fit into my machine's memory. Assuming that JSON is well-formed and is
simply in an extremely large array or dictionary, using the standard serde_json
crate will not suffice. The standard serde_json crate will eagerly parse all 300
GB of values. This crate provides methods and iterator implementations for
lazy-parsing JSON.

## The Public API

This crate only exposes 4 functions. Each returns an iterator that performs the
desired conversion (either character stream to value stream or value stream to
JSON stream).

Functions:

1. from_key_value_pair_stream - takes in a stream of characters and parses it as
   a single JSON object. The key value pairs will be returned one at a time from
   the returned iterator.
1. from_value_stream - takes in a stream of characters and parses it as a single
   JSON array. The values will be returned one at a time from the returned
   iterator.
1. key_value_pairs_to_json_stream - takes in an iterator of serializable values
   and returns an iterator of JSON string segments. These segments can then be
   incrementally written to an output target (probably a file or database in
   most cases).
1. values_to_json_stream - takes in an iterator of key value pairs and returns
   an iterator of JSON string segments. These segments can then be incrementally
   written to an output target (probably a file or database in most cases).

## Examples

### Values API

The simplest example is reading from a file, aggregating, and writing to a file.

Let's say I have a 300 GB stream of object values where each object holds the
min/max and average over a 5 minute window of data. This is placed in
`myFile.json`, which might look something like this:

```json
[
    {
        "avg": 100,
        "min": 2,
        "max": 178
    },
    {
        "avg": 102,
        "min": 5,
        "max": 198
    },
    "...etc"
]
```

If I want to find the global minimum and maximum values in this stream, I could
do that with the following code:

```rust
use std::fs::{File, OpenOptions};
use std::io::{BufReader, BufWriter, Write};

use utf8_chars::BufReadCharsExt;
use serde_derive::{Serialize, Deserialize};
use streaming_serde_json::{from_value_stream, values_to_json_stream};

#[derive(Deserialize)]
struct InputData {
    avg: i32,
    min: i32,
    max: i32,
}

#[derive(Serialize)]
struct OutputData {
    min: i32,
    max: i32,
}

let mut reader = 
    BufReader::new(
        File::open("./myFile.json")
            .unwrap());

let chars = reader.chars();

// The value stream has a PassThroughError type parameter since most
// sources with data large enough to need this will be fallible to
// read from.
let values = from_value_stream::<InputData, _, std::io::Error>(chars);

let mut global_min = i32::MAX;
let mut global_max = i32::MIN;
for result in values {
    // Your error handling goes here. Since this is a 
    // demonstration, I'm not worried about panics.
    let input_data_value: InputData = result.unwrap(); 

    if input_data_value.min < global_min {
        global_min = input_data_value.min;
    }
    if input_data_value.max > global_max {
        global_max = input_data_value.max;
    }
}

let output_data = OutputData {
    min: global_min,
    max: global_max,
};

// Since the output data will be only one item, using the 
// buffered writers and streaming JSON output functions 
// is overkill, but I am using them to demonstrate their 
// usage.
let output_file = OpenOptions::new()
            .read(false)
            .write(true)
            .create(true)
            .truncate(true)
            .open("./myResultJson.json")
            .unwrap();

let mut writer = BufWriter::new(output_file);

// I am using an array, but the values_to_json_stream
// function will accept any type that implements IntoIterator
// and has an Item type that implements Serialize.
let iter = [output_data];
for str in values_to_json_stream(iter) {
    writer.write_all(str.unwrap().as_bytes()).unwrap();
}
writer.flush().unwrap();
```

### Key Value Pairs API

The simplest example is again, reading from a file, making some modification,
and writing to a file.

Let's say I have a 300 GB stream of key value pairs where each value holds the
population of a city in the world. This is placed in `worldPopulations.json`,
which might look something like this:

```json
{
    "Seattle": 755078,
    "New York": 19469200,
    "...etc"
}
```

If I want to find the global average population of a city, I could do that with 
the following code:

```rust
use std::fs::{File, OpenOptions};
use std::io::{BufReader, BufWriter, Write};

use utf8_chars::BufReadCharsExt;
use serde_derive::{Serialize, Deserialize};
use streaming_serde_json::{from_key_value_pair_stream, key_value_pairs_to_json_stream};

let mut reader = 
    BufReader::new(
        File::open("./worldPopulations.json")
            .unwrap());

let chars = reader.chars();

// The value stream has a PassThroughError type parameter since most
// sources with data large enough to need this API will be fallible to
// read from.
let values = from_key_value_pair_stream::<u32, _, std::io::Error>(chars);

let mut global_total = 0_u128;
let mut city_count = 0_u32;
for result in values {
    // Your error handling goes here. Since this is a 
    // demonstration, I'm not worried about panics.
    let input_data_value: (String, u32) = result.unwrap(); 

    global_total += input_data_value.1 as u128;
    city_count += 1;
}

let global_average = global_total / city_count as u128;

// Since the output data will be only one item, using the 
// buffered writers and streaming JSON output functions 
// is overkill, but I am using them to demonstrate their 
// usage.
let output_file = OpenOptions::new()
            .read(false)
            .write(true)
            .create(true)
            .truncate(true)
            .open("./globalAvgPop.json")
            .unwrap();

let mut writer = BufWriter::new(output_file);

// I am using an array, but the values_to_json_stream
// function will accept any type that implements IntoIterator
// and has an Item type that implements Serialize.
let iter = [("global total".to_string(), global_average)];
for str in key_value_pairs_to_json_stream(iter) {
    writer.write_all(str.unwrap().as_bytes()).unwrap();
}
writer.flush().unwrap();
```
