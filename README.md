# Kaba Language

The repository for Kaba programming language.

The languages I took for the inspirations of this project are Java and PHP, but I want to make it with better features, such as no null reference, better type system than PHP, etc.

## Build

Make sure that Rust and its toolchains (such as Cargo) are installed (see [https://www.rust-lang.org/tools/install](https://www.rust-lang.org/tools/install) for installation instructions).

If you want to try using the language instantly, use:
```bash
cargo run -- run <file-name>
```

If you want to build the binary first in release mode, use:
```bash
# Compile once
cargo build --release

./target/release/kaba run <file-name>
```

## Usage

Usage instructions:

1. Create a source code file (the extension **must be** `.kaba`).

2. Run:
  ```bash
  kaba run <file-name>
  ```

## Features

As this is a really new project, it only has limited features for now:

1. Variable creation
  ```text
  var x = 15;
  ```

2. Support for integer and float numbers
  ```text
  var a = 5;
  var b = 10.7;
  var c = -(999);
  ```

3. Value re-assign
  ```text
  var x = 20;
  x = 999;
  ```

4. Support for type notation
  ```text
  var x: Float = 5;
  ```

5. Basic arithmetic operation (Division, Multiplication, Addition, Subtraction)
  ```text
  23 + 5 * 30 / 2 - 9
  ```

6. Grouped expression
  ```text
  52 * (2 + 3) / 3
  ```

7. `print` function (there is no other function, and it is not yet supported to create a new one)
  ```text
  var x = 101;

  print(x);
  ```

## Example

Program for variable swapping:
```text
var x = 10;
var y = 20;

print(x);
print(y);

var temp = x;
x = y;
y = temp;

print(x);
print(y);
```

## Next Goals

Current priority:
1. Control flow support (such as `if-else` and loop).
2. Support for other data types, such as boolean and string.

## Attention

1. All statements must be terminated with semicolon (`;`).
2. There is no garbage collector yet, so don't create too many variables for now🤣.

## License

```text
Copyright 2023 Hafidh Muqsithanova Sukarno

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```
