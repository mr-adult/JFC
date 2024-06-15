# toy-json-formatter

This library is a JSON parsing libary with the following goals:
1. Adhere to the standard JSON specification and parse all valid inputs correctly.
2. Have extreme fault-tolerance. Any input should still yield a formatted or parsed JSON value regardless of how malformed it is.
3. All malformed JSON inputs should yield an error to signal that the input has been transformed.

This library reserves the right to change how malformed JSON is transformed back into valid JSON at any time. Calling code should not depend on the implementation details of error recovery. Changes to error recovery strategies will not be considered breaking changes under semver.
