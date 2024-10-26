# Kaba Language

The repository for Kaba programming language.

The languages I took for the inspirations of this project are Java and PHP, but I want to make it with better features, such as no null reference, better type system than PHP, etc.

## 📦 Install

You can download the specific version you want to use from our [releases page](https://github.com/snaztoz/kaba/releases), and then unzip it.

## 🛠️ Build

If you want to build the executable from source, follow the following build instructions.

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

## 🚀 Usage

Usage instructions:

1. Install or build Kaba.

2. Create a source code file (the extension **must be** `.kaba`). Let's say that we name it `count.kaba`.

3. Run:
  ```bash
  kaba run count.kaba
  ```

## ❓ Features

Currently, Kaba only has the limited features for now:

* Defining functions
  ```text
  fn main() do
    debug return_two();
  end

  fn return_two(): Int do
    return 2;
  end
  ```

(The next examples are assumed to be reside inside `main()` function)

* Comments
  ```text
  # This is a comment.
  #
  # It will be ignored.
  ```

* Variable creation
  ```text
  var x = 15;
  ```

* Support for integer, float and boolean
  ```text
  var a = 5;
  var b = 10.7;
  var c = -(999);
  var d = true;
  ```

* Value assignments
  ```text
  var x = 20;
  x = 999;

  # Shorthands are also supported:
  x += 1;
  x -= 0;
  x *= 2;
  x /= 4;
  x %= 2;

  # This will trigger error due to the types are being incompatible:
  var i = 5.0;
  i = 7;
  ```

* Support for type notation
  ```text
  var x: Float = 5;

  # This will trigger error
  var i: Int = 5.0;
  ```

* Support for function type notation
  ```text
  # Let's say that `foo` is something like:
  #
  #   fn foo(): Int do
  #     return 20;
  #   end
  #

  var x: () -> Int = foo;
  debug x();

  # Or, it can be automatically inferred as well:

  var y = foo;
  debug y();
  ```

* Basic math operation (division, multiplication, addition, subtraction, and modulo)
  ```text
  23 + 5 * 30 / 2 - 9;

  5 % 2;
  ```

* Equality and comparison operation
  ```text
  50 == 50;
  50 != 10;
  43 > 2;
  2.5 >= 2.5;
  100 < 101;
  7 <= 10;
  ```

* Logical operation
  ```text
  false || true;  # true
  false && false; # false
  !false;         # true
  ```

* Grouped expression
  ```text
  52 * (2 + 3) / 3;
  ```

* `debug` statement to output value to console
  ```text
  var x = 101;

  debug x;
  ```

* Conditional branch
  ```text
  var condition = 50 > 10;
  var condition2 = 50 > 20;

  if condition do
    debug 1;
    if condition2 do
      debug 2;
    end
  else do
    debug 0;
  end
  ```

* Loop
  ```
  var i = 0;

  while i < 10 do
    if i % 2 == 0 do
      debug i;
    end
    i += 1;
  end

  # `Break` and `continue` statements are also supported
  while true do
    break;
  end
  ```

## 🤔 Example

* Program for variable swapping:
  ```text
  fn main() do
    var x = 10;
    var y = 20;

    debug x;
    debug y;

    var temp = x;
    x = y;
    y = temp;

    debug x;
    debug y;
  end
  ```

* Program to print all odd numbers below 10:
  ```text
  fn main() do
    var i = 0;

    while i < 10 do
      if i % 2 == 1 do
        debug i;
      end
      i += 1;
    end
  end
  ```

* Simple fibonacci
  ```text
  fn main() do
    debug fibonacci(6); # This should print "8"
  end

  fn fibonacci(n: Int): Int do
    if n <= 0 do
      return 0;
    else if n == 1 || n == 2 do
      return 1;
    end
    return fibonacci(n-1) + fibonacci(n-2);
  end
  ```

## 🎯 Next Goals

Current priorities:

* Support for other data types, such as array and string.

## ⚠️ Attention

* All statements must be terminated with either a semicolon (`;`) or `end` keyword (in case of body blocks).

## 📃 License

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

## 🙌 Acknowledgements

> Standing on the shoulders of giants

This project can be made thanks to the help of amazing works done by the others. You can read [ACKNOWLEDGEMENTS.md](ACKNOWLEDGEMENTS.md) for the list of the projects that Kaba depends on.

p.s. the list is not exhaustive as Kaba may depends on another projects not listed above.
