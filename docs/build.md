# Building Kaba

## Prerequisites

Make sure that Rust and its development toolchains (such as Cargo) are already installed.

See [https://www.rust-lang.org/tools/install](https://www.rust-lang.org/tools/install) for installation instructions.

## Building Kaba using Cargo

Compile the project first:

```bash
cargo build --release
```

The resulting binary can then be found in `./target/release/kaba`. Use this binary to execute Kaba programs.

For example, if the source code is saved as `./program.kaba`, run the following:

```bash
./target/release/kaba run ./program.kaba
```

Alternatively, if you already move the binary inside one of the directories included in `PATH`:

```bash
kaba run ./program.kaba
```

## Running Kaba using Cargo

To run Kaba directly (without manually compiling first), use:

```bash
cargo run -- run <path>
```

For example, if the program is saved as `./program.kaba`, run:

```bash
cargo run -- run ./program.kaba
```

## Running tests

Run the following command:

```bash
cargo test --workspace
```
