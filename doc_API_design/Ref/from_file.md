---
title: from_file
author: Austin C Bullock
---

## [interface from_file](../../interface/from_file.html)

*Description*: Subroutine for reading an external file of uniform
numeric, `logical` or text data into an array.

For reading textual data into an array `into` of rank `1` or `2` and of
type `integer`:

```fortran
call from_file(file, into [, header, delim, fmt, stat, errmsg, format, column_names])
```

* `file` is of type `character(len=*)`
* `header` is `optional` and of type `logical`
* `delim` is `optional` and of type `character(len=*)`
* `fmt` is `optional`, may be one of `INT_FMTS`
* `stat` is `optional` and of type `integer`
* `errmsg` is `optional` and of type `character(len=*)`
* `column_names` is `optional` and of type `type(String), allocatable, dimension(:)`

For reading textual data into an array `into` of rank `1` or `2` and of
type `real`:

```fortran
call from_file(file, into [, header, locale, delim, fmt, stat, errmsg, format, column_names])
```

* `file` is of type `character(len=*)`
* `header` is `optional` and of type `logical`
* `locale` is `optional`, may be one of `LOCALES`
* `delim` is `optional` and of type `character(len=*)`
* `fmt` is `optional`, may be one of `REAL_FMTS`
* `stat` is `optional` and of type `integer`
* `errmsg` is `optional` and of type `character(len=*)`
* `column_names` is `optional` and of type `type(String), allocatable, dimension(:)`

For reading textual data into an array `into` of rank `1` or `2` and of
type `complex`:

```fortran
call from_file(file, into [, header, locale, delim, fmt, im, stat, errmsg, format, &
                           column_names])
```

* `file` is of type `character(len=*)`
* `header` is `optional` and of type `logical`
* `locale` is `optional`, may be one of `LOCALES`
* `delim` is `optional` and of type `character(len=*)`
* `fmt` is `optional`, may be one of `REAL_FMTS`
* `im` is `optional` and of type `character(len=*)`
* `stat` is `optional` and of type `integer`
* `errmsg` is `optional` and of type `character(len=*)`
* `column_names` is `optional` and of type `type(String), allocatable, dimension(:)`

For reading textual data into an array `into` of rank `1` or `2` and of
type `logical`:

```fortran
call from_file(file, into [, header, delim, fmt, stat, errmsg, format, column_names])
```

* `file` is of type `character(len=*)`
* `header` is `optional` and of type `logical`
* `delim` is `optional` and of type `character(len=*)`
* `fmt` is `optional`, and may be `"tf"` (the default) or `"bit"`
* `stat` is `optional` and of type `integer`
* `errmsg` is `optional` and of type `character(len=*)`
* `column_names` is `optional` and of type `type(String), allocatable, dimension(:)`

With `fmt="tf"`, a cell is read as list-directed input reads a
`logical`: the value is the first letter, `T` or `F` in either case,
after any blanks and an optional period, so `T`, `.true.` and `False`
all read. With `fmt="bit"`, a cell must be `1` or `0`. Anything else,
including an empty cell, is an error that gives the cell's row and
column.

For reading textual data into an array `into` of rank `1` or `2` and of
type `String`:

```fortran
call from_file(file, into [, header, delim, stat, errmsg, format, column_names])
```

* `file` is of type `character(len=*)`
* `header` is `optional` and of type `logical`
* `delim` is `optional` and of type `character(len=*)`
* `stat` is `optional` and of type `integer`
* `errmsg` is `optional` and of type `character(len=*)`
* `column_names` is `optional` and of type `type(String), allocatable, dimension(:)`

Each cell becomes one element, unquoted as
[read_file](String-methods.html#read_file) does. Text data can only be
read from a text file, so read `character` data into a `String` array.

For reading binary data into an array `into` of any rank `1`-`15` and
of type `integer`, `real`, `complex`:

```fortran
call from_file(file, into, data_shape [, stat, errmsg, format])
```

For reading a NumPy [.npy file](../UserInfo/file-ext.html#numpy-npy-files),
which records its own shape, into an array `into` of any rank `1`-`15`
and of type `integer`, `real`, `complex` or `logical`:

```fortran
call from_file(file, into [, stat, errmsg, format])
```

`data_shape` may also be passed for a `.npy` file, and must then match
the file.

* `file` is of type `character(len=*)`
* `data_shape` is of type `integer, dimension(:)`
* `stat` is `optional` and of type `integer`
* `errmsg` is `optional` and of type `character(len=*)`

@note `file` may be a relative path, but absolute paths are not
guaranteed to work on every platform.

@warning In all cases, `into` must be `allocatable`, and will lose its
allocation status upon passing into `from_file` if already allocated.
As a result, `from_file` does not allow reading into sections of
already allocated arrays.

@note When reading raw binary data, `data_shape` must be present and its
size must equal the rank of `into`.

### Optional Arguments

Header (default is `.false.`): specifies whether a header line is
present.

Column names: returns the header line's cells, one per column, with any
quotes around a cell removed, as `read_file` does. It implies
`header=.true.`, so it may be given without `header`. Giving it with
`header=.false.`, or for a `.npy` file, which has no header, is an
`IOFL_ARG_ERR`:

```fortran
type(String), allocatable :: names(:)
real, allocatable :: x(:,:)

call from_file("data.csv", into=x, column_names=names)   ! names(j) is the name of column j
```

Locales (default is `"US"`):

```fortran
LOCALES = [ "US", "EU" ]
```

Delimiter: data separator. Default is a tab for a `.tsv` file. For
other files, it is `","` for `integer` data and for `real`/`complex`
data with `"US"` locale, and `";"` for `real`/`complex` data with
`"EU"` locale. It is always recommended to
omit the delimiter argument for default unless a custom delimiter is
really necessary. If `x` has rank `1` and the data is ordered down the
rows, then the `delim` argument is ignored.

Integer formats (default is `"i"`):

```fortran
INT_FMTS = [ "i", "z" ]
```

Real formats (default is `"e"`):

```fortran
REAL_FMTS = [ "e", "f", "z" ]
```

Imaginary unit: `im` specifies the form of a complex number. If not
present, `complex` numbers will be assumed to be written as ordered
pairs, e.g. `(2.45,3.45)`.

Empty cells: an empty or blank cell in `real` or `complex` data reads as
NaN, including a cell missing from a short row. For `integer` data, which
has no NaN, it is an error. Any other text that is not a valid number is
an error, and `errmsg` gives the text with its row and column.

Format: `format` is `optional`, and may be `"text"`, `"binary"` or `"npy"`. If it is
not present, the format comes from the [file extension](../UserInfo/file-ext.html).
