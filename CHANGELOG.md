# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.3.1] - 2024-10-26

### Added

- Support for function type notations.

## [0.3.0] - 2024-10-20

### Added

- Support for defining custom functions.
- Shorthand assignment operators.
- Debug statement.

### Changed

- Auto-casting behaviour in number types is disabled.
- `Any` type is removed.
- Operators for indicating blocks are changed to `do ... end`
- Use `#` to write comments.

### Removed

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

- Change integer literal limit from 64 bits to 32 bits.
- Change error message format

## [0.1.2] - 2023-12-27

### Added

- Support for negation (`-`) operations.

### Fixed

- Versioning.
- Group expression.
- Source file errors output.
