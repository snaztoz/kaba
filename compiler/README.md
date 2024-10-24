# Kaba Language Compiler

This library implements the compiler for Kaba programming language.

## How it works?

The compilation process itself undergoes multiple steps:

### Lexing / Tokenizing

Lexing is a process that turns program source code into a series of tokens.

For example, this expression:

```txt
1 + abc * 5;
```

Will be turned into:

```txt
[Integer(1), Add, Identifier("abc"), Mul, Integer(5)]
```

The compiler currently uses [Logos](https://github.com/maciejhirsz/logos) to implement lexing process.

### Parsing

Parsing stage will turns an array of tokens into an abstract-syntax tree (AST).

For example, using the previous produced tokens by the lexer, the constructed AST will resembles something like this:

```txt
Add {
    lhs: Integer(1),
    rhs: Mul {
        lhs: Identifier("abc"),
        rhs: Integer(5)
    }
}
```

or visually:

```txt
            Add
           /   \
  Integer(1)    Mul
               /   \
Identifier("abc")  Integer(5)
```

Operator precedences will also be handled during this stage.

The parser used by the compiler is implemented using a hand-written [recursive-descent parsing](https://en.wikipedia.org/wiki/Recursive_descent_parser) technique.

### Semantic Checking

This stage will catch all the *nonsenses* found in the AST.

For example, the following program:

```txt
5 + false;
```

... is valid according to the syntax of Kaba language, but it will be marked as invalid by the semantic checker as it does not make sense to add a number with a boolean (not in Kaba, at least😉).

This part is also implemented manually (hand-written).

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
