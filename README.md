# Kaba Language

Bahasa pemrograman Kaba.

## Features

Baru sedikit (project super baru, *boy*!)

## Build

Pastikan Rust beserta toolchain-nya sudah terinstall (Lihat [https://www.rust-lang.org/tools/install](https://www.rust-lang.org/tools/install)).

Jika ingin menjalankan dengan instan, gunakan:
```bash
cargo run -- <nama-file>
```

Jika ingin melakukan build binary dengan mode *release*, gunakan:
```bash
# Compile sekali saja
cargo build --release

./target/release/kaba <nama-file>
```

## Usage

Contoh penggunaan:

1. Buat file source code (ekstensi harus `.kaba`).

2. Jalankan:
  ```
  kaba <nama-file>
  ```

## Limitations

Daripada tanya "batasannya apa aja?", untuk sekarang lebih baik diganti jadi "bisa apa aja?".

1. Pembuatan variabel. Baru bisa value integer
  ```
  var x = 15;
  ```

2. Value re-assign. (Lagi, cuma bisa integer untuk sekarang)
  ```
  var x = 20;
  x = 999;
  ```

3. Fungsi `print` (ga ada fungsi lainnya, pembuatan fungsi custom juga belum bisa)
  ```
  var x = 101;

  print(x);
  ```

Selain itu, anggap saja belum support🥲.

## Example

Program untuk swapping variabel:
```
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

Prioritas untuk sekarang:
1. Support operasi matematika.
2. Support control flow (kaya `if-else` dan loop).
3. Support tipe data lain.
4. Better error handling.

## Attention!

1. Semua statement harus diakhiri dengan tanda titik koma (`;`).
2. Belum ada fitur garbage-collection atau semacamnya, jadi jangan buat variabel kebanyakan🤣.
