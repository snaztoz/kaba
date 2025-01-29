# Kaba Programming Language Features

As Kaba is still actively under development, this file is considered as a living document that will be updated as needed.

## Overview

Kaba is a:

1. Statically typed language, where every type violations will be caught before the program is actually run.

    ```text
    var x: int = 5;

    x = false;  // ERROR
    ```

2. Strongly typed language, where operations between different types can't be done without going through certain procedure.

    ```text
    5 + 5.0;    // ERROR
    ```

Kaba is mainly consisted of 2 parts:

1. [Compiler](../kabac/), where the architecture is briefly explained at the [README](../kabac/README.md) file.
2. [Runtime](../src/runtime.rs), which currently is still a prototype.

To run a Kaba program, the `run` command may be used:

```bash
kaba run my-program.kaba
```

## Program file

Kaba program files **must** have `.kaba` extension. For example, `math.kaba`.

The file also must contains a [main function](#main-function), which acts as the entrypoint to the program.

> There is a [known bug](https://github.com/snaztoz/kaba/issues/28) that will make Kaba display a relatively *unreadable* error message if the program file does not contains `main()` function.

## Comments

Kaba only supports single-line comment, which is prefixed by the `//` symbol:

```text
// This is a comment
```

## Defining functions

To define functions, use the `fn` keyword:

```text
fn foo() {
    // do nothing
}
```

From the example above, `foo()` return type is `void` (not returning anything).

To return a value from functions, use the `return` keyword (and don't forget to specify the type notation as well):

```text
fn yield_five(): int {
    return 5;
}
```

## `main()` function

The entry point to a Kaba program is a function called `main()`:

```text
fn main() {
    do_nothing();
}

fn do_nothing() {}
```

## Creating variables

To create variables, use the `var` keyword:

```text
fn main() {
    var x = 5;
}
```

The value must always be specified, while the variable's type can be inferred from it.

If you want to specify the type manually, use the following syntax:

```text
fn main() {
    var x: int = 5;
}
```

If value type is incompatible with the variable, the compiler will throw an error:

```text
fn main() {
    var x: int = 10.0;  // ERROR
}
```

## Value assignments

Kaba is a strongly-typed language, so the type of operands in various operations must be compatible.

For example, the following program will results in compilation error:

```text
fn main() {
    var x = 5;

    x = 10.0;   // ERROR!
}
```

## Shorthand assignments

Shorthand assignments are also supported:

```text
fn main() {
    var x = 10;

    x += 1;
    x -= 0;
    x *= 2;
    x /= 4;
    x %= 2;
}
```

## Displaying value with `debug` statement

To display value to `stdout`, use the `debug` statement:

```text
fn main() {
    var x = 101;

    debug x;
}
```

Note that the compiler will reject if the expression evaluates to `void` type:

```text
fn main() {
    debug my_void_fn();  // ERROR
}

fn my_void_fn() {}
```

## Data types

Currently, Kaba only support these (non-`void`) data types:

1. Signed integers

    | No | Type    | Name        | Minimum         | Maximum            |
    |----|---------|-------------|-----------------|--------------------|
    | 1  | `sbyte` | Signed byte | -2<sup>7</sup>  | 2<sup>7</sup> - 1  |
    | 2  | `short` | Short       | -2<sup>15</sup> | 2<sup>15</sup> - 1 |
    | 3  | `int`   | Integer     | -2<sup>31</sup> | 2<sup>31</sup> - 1 |
    | 4  | `long`  | Long        | -2<sup>63</sup> | 2<sup>63</sup> - 1 |

2. Floating-point numbers

    | No | Type     | Name   | Minimum                  | Maximum                 |
    |----|----------|--------|--------------------------|-------------------------|
    | 1  | `float`  | Float  | -3.40282347E+38          | 3.40282347E+38f32       |
    | 2  | `double` | Double | -1.7976931348623157E+308 | 1.7976931348623157E+308 |

3. Boolean (`bool`)
4. Char (`char`)
5. String (`string`)
5. Callable
6. Array

(... more to come!)

```text
fn main() {
    var a: int = 10;

    var b: float = 5.0;

    var c: bool = false;

    var d: char = 'A';

    var e: string = "Hello!";

    var f: () -> void = foo;

    var g: []int = [int 99, 101];
}

fn foo() {}
```

### About `char` type

Currently, it supports:

1. Basic characters, such as `'A'`, `'0'`, etc.
2. Escape characters, such as `'\n'`.
3. Hex-based characters, such as `'\x41'` (which basically the same as `'A'`).

Non-ASCII characters are not yet supported at the moment.

### About `string` type

Currently, it supports:

1. Basic characters, such as `'A'`, `'0'`, etc.
2. Escape characters, such as `'\n'`.
3. Hex-based characters, such as `'\x41'` (which basically the same as `'A'`).

Strings in Kaba currently are immutable, and no operation that can be used for them yet (aside from printing, assigning, and equality checkings, of course).

### About array type

Even though the size is not specified in the type notation (`[]T`), array has a **fixed** size, so that it can't grow or shrink after it was created.

Because the size is not included in the type notation, an integer array like `[1, 2, 3]` is considered to have the same type as `[1]`.

Kaba can infer the type of an array:

```text
fn main() {
    var arr = [bool false, true, true];

    // The type of `arr` is `[]bool`

    debug arr[1];   // `true`
}
```

More complex scenarios are also supported:

```text
fn main() {
    var arr = [[]int [int], [int 4, 5]];
    foo(arr);

    arr[1][1] = 10;
    foo(arr);
}

fn foo(arr: [][]int) {
    debug arr[1][1];
}
```

## Expressions

### Math expressions

Basic math operations such as addition, subtraction, etc. are supported:

```text
fn main() {
    debug 23 + 5 * 30 / (2 - 9);

    debug 5 % 2;
}
```

### Equality and comparison expressions

Operations like "less than", "equal", etc. are supported:

```text
fn main() {
    debug 50 == 50;
    debug 50 != 10;
    debug 43 > 2;
    debug 2.5 >= 2.5;
    debug 100 < 101;
    debug 7 <= 10;
}
```

### Logical boolean expressions

Logical "or", "and", and "not" are supported:

```text
fn main() {
    debug false || true;
    debug false && false;
    debug !false;
}
```

## Conditional branches

Kaba also support "if... else..." statement:

```text
fn main() {
    var condition = 50 > 10;
    var condition2 = 50 > 20;

    if condition {
        debug 1;
        if condition2 {
            debug 2;
        }
    } else {
        debug 0;
    }
}
```

## Looping with `while` statement

To looping over while a condition is met, use the `while` statement:

```text
fn main() {
    var i = 0;

    while i < 10 {
        if i % 2 == 0 {
            debug i;
        }
        i += 1;
    }
}
```

To exit from a loop early, use the `break` statement:

```text
fn main() {
    var i = 0;

    while i < 10 {
        if i == 5 {
            break;  // exit from loop
        }
        debug i;
        i += 1;
    }
}
```

To skip an iteration, use the `continue` statement:

```text
fn main() {
    var i = 0;

    while i < 10 {
        i += 1;
        if i == 5 {
            continue;  // "5" won't be printed
        }
        debug i;
    }
}
```

## Looping with `each` statement

To simplify looping over elements of an iterable, use the `each` loop statement:

```text
fn main() {
    each n in [int 1, 2, 3, 4] {
        debug n * 2;
    }
}
```

Similar to the `while` statement, we can also use `continue` and `break` statements in inside of it:

```text
fn main() {
    each n in [int 1, 2, 3, 4, 5, 6] {
        if n == 3 {
            continue;
        }

        if n == 5 {
            break;
        }

        debug n;
    }
}
```
