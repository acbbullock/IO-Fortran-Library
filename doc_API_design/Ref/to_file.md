---
title: to_file
author: Austin C Bullock
---

## [interface to_file](../../interface/to_file.html)

*Description*: Subroutine for writing an array of uniform numeric,
`logical` or text data to an external file.

For writing textual data from an array `x` of rank `1`, `2` and of type
`integer`:

```fortran
call to_file(x, file [, header, delim, fmt, stat, errmsg, format])
```

* `file` is of type `character(len=*)`
* `header` is `optional` and of type `character(len=*), dimension(:)`
* `delim` is `optional` and of type `character(len=*)`
* `fmt` is `optional`, may be one of `INT_FMTS`
* `stat` is `optional` and of type `integer`
* `errmsg` is `optional` and of type `character(len=*)`

For writing textual data from an array `x` of rank `1`, `2` and of type
`real`:

```fortran
call to_file(x, file [, header, locale, delim, fmt, decimals, stat, errmsg, format])
```

* `file` is of type `character(len=*)`
* `header` is `optional` and of type `character(len=*), dimension(:)`
* `locale` is `optional`, may be one of `LOCALES`
* `delim` is `optional` and of type `character(len=*)`
* `fmt` is `optional`, may be one of `REAL_FMTS`
* `decimals` is `optional` and of type `integer`
* `stat` is `optional` and of type `integer`
* `errmsg` is `optional` and of type `character(len=*)`

For writing textual data from an array `x` of rank `1`, `2` and of type
`complex`:

```fortran
call to_file(x, file [, header, locale, delim, fmt, decimals, im, stat, errmsg, format])
```

* `file` is of type `character(len=*)`
* `header` is `optional` and of type `character(len=*), dimension(:)`
* `locale` is `optional`, may be one of `LOCALES`
* `delim` is `optional` and of type `character(len=*)`
* `fmt` is `optional`, may be one of `REAL_FMTS`
* `decimals` is `optional` and of type `integer`
* `im` is `optional` and of type `character(len=*)`
* `stat` is `optional` and of type `integer`
* `errmsg` is `optional` and of type `character(len=*)`

For writing textual data from an array `x` of rank `1`, `2` and of type
`logical`:

```fortran
call to_file(x, file [, header, delim, fmt, stat, errmsg, format])
```

* `file` is of type `character(len=*)`
* `header` is `optional` and of type `character(len=*), dimension(:)`
* `delim` is `optional` and of type `character(len=*)`
* `fmt` is `optional`, and may be `"tf"` (the default), which writes `T`
  and `F` as list-directed output does, or `"bit"`, which writes `1` and
  `0`
* `stat` is `optional` and of type `integer`
* `errmsg` is `optional` and of type `character(len=*)`

For writing textual data from an array `x` of rank `1`, `2` and of type
`String` or `character(len=*)`:

```fortran
call to_file(x, file [, header, delim, stat, errmsg, format])
```

* `file` is of type `character(len=*)`
* `header` is `optional` and of type `character(len=*), dimension(:)`
* `delim` is `optional` and of type `character(len=*)`
* `stat` is `optional` and of type `integer`
* `errmsg` is `optional` and of type `character(len=*)`

Each element is one cell, quoted when it contains the delimiter, a
quote, `CR` or `LF`, as [write_file](String-methods.html#write_file)
does. Trailing blanks of `character` elements are removed, as they are
for `header`. Text data can only be written to a text file.

For writing binary data from an array `x` of any rank `1`-`15` and of
type `integer`, `real`, `complex`, to a raw binary file or a NumPy
[.npy file](../UserInfo/file-ext.html#numpy-npy-files), or of type
`logical` to a `.npy` file:

```fortran
call to_file(x, file [, stat, errmsg, format])
```

For adding an array `x` of the same types and ranks to a NumPy
[.npz archive](../UserInfo/file-ext.html#numpy-npz-archives) under the
name `name`:

```fortran
call to_file(x, file, name [, stat, errmsg, format])
```

* `file` is of type `character(len=*)`
* `name` is of type `character(len=*)`
* `stat` is `optional` and of type `integer`
* `errmsg` is `optional` and of type `character(len=*)`

`name` is the name NumPy gives the array: `numpy.load(file)[name]`.
Trailing blanks in it are ignored. If `file` doesn't exist it is
created, and if it does, `x` is added to the arrays already in it. An
array already there under `name` is replaced, and the new one is listed
last by [npz_names](npz_names.html). `name` is required for a `.npz`
file, and is an `IOFL_ARG_ERR` for any other file:

```fortran
call to_file(x, file="data.npz", name="x")      ! Creates data.npz, or adds x to it
call to_file(y, file="data.npz", name="y")      ! Now data.npz holds x and y
```

To start a new archive, delete the old file first.

@note `logical` data can't be written to a raw binary file (`.dat`,
`.bin`), because compilers store `logical` values differently, so the
file couldn't be read back reliably. A `.npy` or `.npz` file stores each
value as one byte, `0` or `1`, which NumPy reads as `bool`.

@note `file` may be a relative path, but absolute paths are not
guaranteed to work on every platform.

@note `to_file` will always use the `IOFL_NL` line ending when writing text
files (which on most systems equates to `LF`).

### Optional Arguments

Header (default is none): `header` is a
[character array literal](../UserInfo/compilers.html). For `x` of rank
`1`, `header` may be of size `1` or `size(x)`. For `x` of rank `2`,
`header` may be of size `1` or `size(x, dim=2)`.

Locales (default is `"US"`):

```fortran
LOCALES = [ "US", "EU" ]
```

Delimiter: data separator. Default is a tab for a `.tsv` file. For
other files, it is `","` for `integer` data and for `real`/`complex`
data with `"US"` locale, and `";"` for `real`/`complex` data with
`"EU"` locale. It is always recommended to
omit the delimiter argument for default unless a custom delimiter is
really necessary. If `x` has rank `1` and `dim=1`, then the `delim`
argument is ignored.

Integer formats (default is `"i"`):

```fortran
INT_FMTS = [ "i", "z" ]
```

Real formats (default is `"e"`):

```fortran
REAL_FMTS = [ "e", "f", "z" ]
```

Decimals: `decimals` specifies the number of digits on the rhs of the
radix point, with a default determined internally based on the
[text format](../UserInfo/text-fmts.html) and precision. The value is
rounded to that number of digits.

Imaginary unit: `im` specifies the form of a complex number. By
default, `complex` numbers will be written as ordered pairs, e.g.
`(2.45,3.45)`. If `im` is specified, then the number will be written as
a sum with the specified imaginary unit, e.g. `2.45+3.45j` for `im="j"`
or `2.45+3.45*1i` for `im="*1i"`.

Format: `format` is `optional`, and may be `"text"`, `"binary"`, `"npy"` or `"npz"`. If
it is not present, the format comes from the [file extension](../UserInfo/file-ext.html).
