# Kaba Language

Bahasa pemrograman Kaba.

## Features

Baru sedikit (project super baru, *boy*!)

## Usage

Contoh penggunaan:

1. Buat file source code (ekstensi `.kaba`, meskipun ekstensi lain juga bisa untuk sekarang).

2. Jalanin:
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

3. Fungsi `print` (ga ada fungsi lainnya, pembuatan fungsi custom juga belum)

  ```
  var x = 101;

  print(x);
  ```

Selain itu, anggap saja belum support🥲.

## Examples

Program untuk swapping variabel:
```
var x = 15;
var y = 20;

var temp = x;
x = y;
y = temp;

print(x);
print(y);
```

## Next Goals

Prioritas untuk sekarang:
1. Support control flow (kaya `if-else` dan loop).
2. Support tipe data lain.
3. Better error handling.

## Attention!

1. Semua statement harus diakhiri dengan tanda titik koma (`;`).
2. Belum ada fitur garbage-collection atau semacamnya, jadi jangan buat variabel kebanyakan🤣.
