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

If you want to run all tests, use:
```bash
cargo test --workspace
```

## Usage

Usage instructions:

0. Follow the build step above.

1. Create a source code file (the extension **must be** `.kaba`).

2. Run:
  ```bash
  kaba run <file-name>
  ```

## Features

As this is a really new project, it only has limited features for now:

0. Comments
  ```text
  // This is a single line comment

  /*
    This
    is
    a
    multi-
    line
    comment
  */
  ```

1. Variable creation
  ```text
  var x = 15;
  ```

2. Support for integer, float and boolean
  ```text
  var a = 5;
  var b = 10.7;
  var c = -(999);
  var d = true;
  ```

3. Value re-assign
  ```text
  var x = 20;
  x = 999;

  // This will trigger error due to the types are being incompatible:
  x = 5.0;
  ```

4. Support for type notation
  ```text
  var x: Float = 5;

  // This will trigger error
  var i: Int = 5.0;
  ```

5. Basic math operation (division, multiplication, addition, subtraction, and modulo)
  ```text
  23 + 5 * 30 / 2 - 9;

  5 % 2;
  ```

6. Equality and comparison operation
  ```text
  50 == 50;
  50 != 10;
  43 > 2;
  2.5 >= 2.5;
  100 < 101;
  7.3 <= 10;
  ```

7. Logical operation
  ```text
  false || true;  // true
  false && false; // false
  !false;         // true
  ```

8. Grouped expression
  ```text
  52 * (2 + 3) / 3;
  ```

9. `print` function (there is no other function, and it is not yet supported to create a new one)
  ```text
  var x = 101;

  print(x);
  ```

10. Conditional branch
  ```text
  var condition = 50 > 10;
  var condition2 = 50 > 20;

  if condition {
    print(1);
    if condition2 {
      print(2);
    }
  } else {
    print(0);
  }
  ```

11. Loop
  ```
  var i = 0;

  while i < 10 {
    if i % 2 == 0 {
      print(i);
    }
    i = i + 1;
  }

  // `Break` and `continue` statements are also supported
  while true {
    break;
  }
  ```

## Example

0. Program for variable swapping:
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

1. Program to print all odd numbers below 10:
  ```text
  var i = 0;

  while i < 10 {
    if i % 2 == 1 {
      print(i);
    }
    i = i + 1;
  }
  ```

## Next Goals

Current priorities:

0. Better type system.
1. Support for other data types, such as array and string.

## Attention

* All statements must be terminated with either a semicolon (`;`) or right brace (`}`) (in case of conditional branch and loop block).

## License

```text
Copyright 2023-2024 Hafidh Muqsithanova Sukarno

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

## Acknowledgements

> Standing on the shoulders of giants

This project can be made thanks to the help of amazing works done by the others. You can read [ACKNOWLEDGEMENTS.md](ACKNOWLEDGEMENTS.md) for the list of the projects that Kaba depends on.

p.s. the list is not exhaustive as Kaba may depends on another projects not listed above.
