# Data Types

Because this crate is attempting to unify the CLI for handling many different file formats, it is important to have a unified set of structures that they can all be represented and queried with.

## JSON
JSON is a very standard data storage format which can be represented by the following:
```text
Null,
Bool(bool),
Number(Number),
String(String),
Array(Vec<Value>),
Object(Map<String, Value>),
```

## XML/HTML
XML and HTML are also very widely used data formats. They can be represented by the following:
```
enum XMLNode {
    Text(String),
    Standard(Node),
}
Node {
    attributes: {
        name: String,
        value: String,
    }[],
    children: Node[]
}
```

Note: this should allow querying using list structure if a single child tag is repeated several times.

## TOML
TOML can be represented in a very similar way to JSON.