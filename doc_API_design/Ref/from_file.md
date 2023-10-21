---
title: from_file
author: Austin C Bullock
---

## [interface from_file](../../interface/from_file.html)

*Description*: Subroutine for reading an external file of uniform
numeric data type and format into an array.

For reading textual data into an array `into` of rank `1` or `2` and of
type `integer`:

```fortran
call from_file(file, into [, header, delim, fmt, stat, errmsg])
```

* `file` is of type `character(len=*)`
* `header` is `optional` and of type `logical`
* `delim` is `optional` and of type `character(len=*)`
* `fmt` is `optional`, may be one of `INT_FMTS`
* `stat` is `optional` and of type `integer`
* `errmsg` is `optional` and of type `character(len=*)`

For reading textual data into an array `into` of rank `1` or `2` and of
type `real`:

```fortran
call from_file(file, into [, header, locale, delim, fmt, stat, errmsg])
```

* `file` is of type `character(len=*)`
* `header` is `optional` and of type `logical`
* `locale` is `optional`, may be one of `LOCALES`
* `delim` is `optional` and of type `character(len=*)`
* `fmt` is `optional`, may be one of `REAL_FMTS`
* `stat` is `optional` and of type `integer`
* `errmsg` is `optional` and of type `character(len=*)`

For reading textual data into an array `into` of rank `1` or `2` and of
type `complex`:

```fortran
call from_file(file, into [, header, locale, delim, fmt, im, stat, errmsg])
```

* `file` is of type `character(len=*)`
* `header` is `optional` and of type `logical`
* `locale` is `optional`, may be one of `LOCALES`
* `delim` is `optional` and of type `character(len=*)`
* `fmt` is `optional`, may be one of `REAL_FMTS`
* `im` is `optional` and of type `character(len=*)`
* `stat` is `optional` and of type `integer`
* `errmsg` is `optional` and of type `character(len=*)`

For reading binary data into an array `into` of any rank `1`-`15` and
of type `integer`, `real`, `complex`:

```fortran
call from_file(file, into, data_shape [, stat, errmsg])
```

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

@note When reading binary data, `data_shape` must be present and its
size must equal the rank of `into`.

### Optional Arguments

Header (default is `.false.`): specifies whether a header line is
present.

Locales (default is `"US"`):

```fortran
LOCALES = [ "US", "EU" ]
```

Delimiter: data separator. Default is `","` for `integer` data and for
`real`/`complex` data with `"US"` locale, and `";"` for
`real`/`complex` data with `"EU"` locale. It is always recommended to
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
