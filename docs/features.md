# Kaba Programming Language Features

As Kaba is still actively under development, this file is considered as a living document that will be updated as needed.

## Overview

Kaba is a:

1. Statically typed language, where every type violations will be caught before the program is actually run.

    ```text
    var x: Int = 5;

    x = false;  # ERROR
    ```

2. Strongly typed language, where operations between different types can't be done without going through certain procedure.

    ```text
    5 + 5.0;    # ERROR
    ```

Kaba is mainly consisted of 2 parts:

1. [Compiler](../compiler/), where the architecture is briefly explained at the [README](../compiler/README.md) file.
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

Kaba only supports single-line comment, which is prefixed by the `#` symbol:

```text
# This is a comment
```

## Defining functions

To define functions, use the `fn` keyword:

```text
fn foo() do
    # do nothing
end
```

From the example above, `foo()` return type is `Void` (not returning anything).

To return a value from functions, use the `return` keyword (and don't forget to specify the type notation as well):

```text
fn yield_five(): Int do
    return 5;
end
```

## `main()` function

The entry point to a Kaba program is a function called `main()`:

```text
fn main() do
    do_nothing();
end

fn do_nothing() do
end
```

## Creating variables

To create variables, use the `var` keyword:

```text
fn main() do
    var x = 5;
end
```

The value must always be specified, while the variable's type can be inferred from it.

If you want to specify the type manually, use the following syntax:

```text
fn main() do
    var x: Int = 5;
end
```

If value type is incompatible with the variable, the compiler will throw an error:

```text
fn main() do
    var x: Int = 10.0;  # ERROR
end
```

## Displaying value with `debug` statement

To display value to `stdout`, use the `debug` statement:

```text
fn main() do
    var x = 101;

    debug x;
end
```

Note that the compiler will reject if the expression evaluates to `Void` type:

```text
fn main() do
    debug my_void_fn();  # ERROR
end

fn my_void_fn() do
end
```

## Data types

Currently, Kaba only support these (non-`Void`) data types:

1. Integer (`Int`)
2. Float (`Float`)
3. Boolean (`Bool`)
4. Callable
5. Array

(... more to come!)

```text
fn main() do
    var a: Int = 10;

    var b: Float = 5.0;

    var c: Bool = false;

    var d: () -> Void = foo;

    var e: [2]Int = [99, 101];
end

fn foo() do
end
```

### More on array type

Array has a **fixed** size, so that it can't be changed after it was created.

If two arrays have different size, even though their elements are the same type, they will still be considered as different types.

For example, `[2]Int` is not the same as `[1]Int`.

Kaba can infer the type of an array:

```text
fn main do
    var arr = [false, true, true];

    # The type of `arr` is `[3]Bool`

    debug arr[1];   // `true`
end
```

Kaba can also infer the array size automatically:

```text
fn main do
    var arr: [_]Int = [1, 4, 6, 7];

    # The type of `arr` is `[4]Int`
end
```

More complex scenarios are also supported:

```text
fn main() do
    var arr = [[4, 5]];
    foo(arr);

    arr[0][1] = 10;
    foo(arr);
end

fn foo(arr: [_][_]Int) do
    debug arr[0][1];
end
```

## Value assignments

Kaba is a strongly-typed language, so the type of operands in various operations must be compatible.

For example, the following program will results in compilation error:

```text
fn main() do
    var x = 5;

    x = 10.0;   # ERROR!
end
```

## Shorthand assignments

Shorthand assignments are also supported:

```text
fn main() do
    var x = 10;

    x += 1;
    x -= 0;
    x *= 2;
    x /= 4;
    x %= 2;
end
```

## Expressions

### Math expressions

Basic math operations such as addition, subtraction, etc. are supported:

```text
fn main() do
    debug 23 + 5 * 30 / (2 - 9);

    debug 5 % 2;
end
```

### Equality and comparison expressions

Operations like "less than", "equal", etc. are supported:

```text
fn main() do
    debug 50 == 50;
    debug 50 != 10;
    debug 43 > 2;
    debug 2.5 >= 2.5;
    debug 100 < 101;
    debug 7 <= 10;
end
```

### Logical boolean expressions

Logical "or", "and", and "not" are supported:

```text
fn main() do
    debug false || true;
    debug false && false;
    debug !false;
end
```

## Conditional branches

Kaba also support "if... else..." statement:

```text
fn main() do
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
end
```

## Loop

To loop over while a condition is met, use the `while` statement:

```text
fn main() do
    var i = 0;

    while i < 10 do
        if i % 2 == 0 do
            debug i;
        end
        i += 1;
    end
end
```

To exit from a loop early, use the `break` statement:

```text
fn main() do
    var i = 0;

    while i < 10 do
        if i == 5 do
            break;  # exit from loop
        end
        debug i;
        i += 1;
    end
end
```

While to skip an iteration, use the `continue` statement:

```text
fn main() do
    var i = 0;

    while i < 10 do
        i += 1;
        if i == 5 do
            continue;  # "5" won't be printed
        end
        debug i;
    end
end
```
