# Kaba Programming Language

Kaba is a strong and statically-typed programming language.

It is written in Rust and currently under active development.

## 📦 Install

You can download the specific version you want to use directly from our [releases page](https://github.com/snaztoz/kaba/releases), and then unzip the binary.

If you want to build Kaba from source, consult the ["Building Kaba" guide](docs/build.md).

## 🚀 Usage

First, make sure Kaba binary is already [installed](#-install).

Then create a [Kaba program file](docs/features.md) (the extension **must be** `.kaba`). For example, let's create a file called `math.kaba` with the following code:

```text
fn main() do
    debug 5 + 7;
end
```

Now we run the following shell command:

```bash
kaba run math.kaba

# Or use:
#   ./target/release/kaba run ...
#
# Or:
#   ./kaba run ...
#
# (Adjust on where the binary file is located)
```

It will print `12` in the screen🤯

## ❓ Features

See [features](docs/features.md) page to learn about all available features.

## 🤔 Example

See [docs/examples](docs/examples) directory for the examples of already-working Kaba programs.

## 🎯 Next Goals

Current priority is to add support for other data types, such as string.

## ⚒️ Contributing

Thank you for considering contributing to the Kaba programming language! The contribution guide can be found in the [CONTRIBUTING.md](CONTRIBUTING.md) file.

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
