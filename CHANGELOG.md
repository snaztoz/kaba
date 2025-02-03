# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Fixed

- Integer literal limit size.
- Fix math expressions wrapping in runtime.

## [0.5.2] - 2025-01-20

### Added

- Support for JS code generation (by [@snaztoz](https://github.com/snaztoz) in [#57](https://github.com/snaztoz/kaba/pull/57))

### Changed

- (Internal) main binary is splitted from runtime prototype (by [@snaztoz](https://github.com/snaztoz) in [#55](https://github.com/snaztoz/kaba/pull/55))
- (Internal) symbol table's data structure (by [@snaztoz](https://github.com/snaztoz) in [a2ba193](https://github.com/snaztoz/kaba/pull/57/commits/a2ba193abfd2d57162fcc9d85a38c18002770f56))

### Fixed

- Prevent creating symbols using built-in type names (by [@snaztoz](https://github.com/snaztoz) in [#58](https://github.com/snaztoz/kaba/pull/58))
- Prevent function call with mismatched args length (by [@snaztoz](https://github.com/snaztoz) in [#59](https://github.com/snaztoz/kaba/pull/59))

## [0.5.1] - 2025-01-02

### Changed

- Change array literal syntax (by [@snaztoz](https://github.com/snaztoz) in [#50](https://github.com/snaztoz/kaba/pull/50))

## [0.5.0] - 2025-01-01

### Added

- Add new signed integer types: `sbyte`, `short`, and `long` (by [@snaztoz](https://github.com/snaztoz) in [7bbfd17](https://github.com/snaztoz/kaba/pull/44/commits/7bbfd17d02acd57267742376598efdb1f267d78d))
- Add a new floating-point types: `double` (by [@snaztoz](https://github.com/snaztoz) in [438e189](https://github.com/snaztoz/kaba/pull/44/commits/438e1896107bc209f9b6de00e603d4fa790c458a))
- Add a new character data type: `char` (by [@snaztoz](https://github.com/snaztoz) in [60f6c6c](https://github.com/snaztoz/kaba/pull/44/commits/60f6c6c09fd88e56864e2f877fa82e2c3d9e0d72))
- Add a new string data type: `string` (by [@snaztoz](https://github.com/snaztoz) in [#45](https://github.com/snaztoz/kaba/pull/45))
- Check unbounded integer expression value before assigning to bounded integer types (by [@snaztoz](https://github.com/snaztoz) in [1eb2b7b](https://github.com/snaztoz/kaba/pull/44/commits/1eb2b7b4f8bdf9d31cc7557fee83060226efd745))

### Changed

- Rename built-in types to lowercase version (by [@snaztoz](https://github.com/snaztoz) in [#43](https://github.com/snaztoz/kaba/pull/43))
- (Internal) replace semantic analyzer's scope stack with symbol tree (by [@snaztoz](https://github.com/snaztoz) in [#43](https://github.com/snaztoz/kaba/pull/43))
- Change array literal syntax (by [@snaztoz](https://github.com/snaztoz) in [8904c18](https://github.com/snaztoz/kaba/pull/44/commits/8904c18d1e7b721f4ee675c4a33f2dc9477c0118))
- Change single-line comment token from `#` to `//` (by [@snaztoz](https://github.com/snaztoz) in [#47](https://github.com/snaztoz/kaba/pull/47))
- Change block delimiters from `do ... end` to `{ ... }` (by [@snaztoz](https://github.com/snaztoz) in [#48](https://github.com/snaztoz/kaba/pull/48))

## [0.4.0] - 2024-12-06

### Added

- Add support for array type (by [@snaztoz](https://github.com/snaztoz) in [#35](https://github.com/snaztoz/kaba/pull/35))
- Add support for `each` loop statement (by [@snaztoz](https://github.com/snaztoz) in [#39](https://github.com/snaztoz/kaba/pull/39) and [#41](https://github.com/snaztoz/kaba/pull/41))

### Changed

- Make operations stricter on operand types (by [@snaztoz](https://github.com/snaztoz) in [#33](https://github.com/snaztoz/kaba/pull/33))

### Fixed

- Avoid cloning the entire source code on compiler errors (by [@null8626](https://github.com/null8626) in [#29](https://github.com/snaztoz/kaba/pull/29))
- Prevent non-existing types in callable parameters and return type.

## [0.3.3] - 2024-11-28

### Fixed

- Return statement's expression checking bug.

## [0.3.2] - 2024-11-28

### Fixed

- Prevent creating variable with `Void` type.
- Calling function returned from another function call.

### Removed

- Support for creating variables without providing initial value.

## [0.3.1] - 2024-10-26

### Added

- Support for function type notations.

## [0.3.0] - 2024-10-20

### Added

- Support for defining custom functions.
- Shorthand assignment operators.
- Debug statement.

### Changed

- Operators for indicating blocks are changed to `do ... end`
- Use `#` to write comments.

### Removed

- Auto-casting behaviour in number types is removed.
- `Any` type is removed.
- Multi-line comments.
- `//` operator.
- Built-in `print` function.

## [0.2.2] - 2024-01-11

### Added

- Support for comments.
- Support for logical boolean operators.
- Support for loop.
- Support for modulo (`%`) operator.

## [0.2.1] - 2024-01-08

### Added

- Support for boolean data type.
- Support for `if-else` control flow.
- Support for equality and comparison operators.

### Fixed

- Parser EOF bug.

## [0.2.0] - 2024-01-05

### Added

- Support for floating numbers.
- Support for semantic analysis.

### Changed

- Integer literal limit is changed from 64 bits to 32 bits.
- Error message displaying format

## [0.1.2] - 2023-12-27

### Added

- Support for negation (`-`) operations.

### Fixed

- Versioning.
- Group expression.
- Source file errors output.
