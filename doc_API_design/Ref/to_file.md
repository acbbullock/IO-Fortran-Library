---
title: to_file
author: Austin C Bullock
---

## [interface to_file](../../interface/to_file.html)

*Description*: Subroutine for writing an array of uniform numeric data type to an external file.

For writing textual data from an array `x` of rank `1`, `2` and of type `integer`:

```fortran
call to_file(x, file [, header, delim, fmt, stat, errmsg])
```

* `file` is of type `character(len=*)`
* `header` is `optional` and of type `character(len=*), dimension(:)`
* `delim` is `optional` and of type `character(len=*)`
* `fmt` is `optional`, may be one of `INT_FMTS`
* `stat` is `optional` and of type `integer`
* `errmsg` is `optional` and of type `character(len=*)`

For writing textual data from an array `x` of rank `1`, `2` and of type `real`:

```fortran
call to_file(x, file [, header, locale, delim, fmt, decimals, stat, errmsg])
```

* `file` is of type `character(len=*)`
* `header` is `optional` and of type `character(len=*), dimension(:)`
* `locale` is `optional`, may be one of `LOCALES`
* `delim` is `optional` and of type `character(len=*)`
* `fmt` is `optional`, may be one of `REAL_FMTS`
* `decimals` is `optional` and of type `integer`
* `stat` is `optional` and of type `integer`
* `errmsg` is `optional` and of type `character(len=*)`

For writing textual data from an array `x` of rank `1`, `2` and of type `complex`:

```fortran
call to_file(x, file [, header, locale, delim, fmt, decimals, im, stat, errmsg])
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

For writing binary data from an array `x` of any rank `1`-`15` and of type `integer`, `real`, `complex`:

```fortran
call to_file(x, file [, stat, errmsg])
```

* `file` is of type `character(len=*)`
* `stat` is `optional` and of type `integer`
* `errmsg` is `optional` and of type `character(len=*)`

@note `file` may be a relative path, but absolute paths are not guaranteed to work on every platform.

@note `to_file` will always use the `NL` line ending when writing text files (which on most systems equates to `LF`).

### Optional Arguments

Header (default is none): `header` is a [character array literal](../UserInfo/compilers.html). For `x` of rank `1`, `header` may be of size `1` or `size(x)`. For `x` of rank `2`, `header` may be of size `1` or `size(x, dim=2)`.

Locales (default is `"US"`):

```fortran
LOCALES = [ "US", "EU" ]
```

Delimiter: data separator. Default is `","` for `integer` data and for `real`/`complex` data with `"US"` locale, and `";"` for `real`/`complex` data with `"EU"` locale. It is always recommended to omit the delimiter argument for default unless a custom delimiter is really necessary. If `x` has rank `1` and `dim=1`, then the `delim` argument is ignored.

Integer formats (default is `"i"`):

```fortran
INT_FMTS = [ "i", "z" ]
```

Real formats (default is `"e"`):

```fortran
REAL_FMTS = [ "e", "f", "z" ]
```

Decimals: `decimals` specifies the number of digits on the rhs of the radix point, with a default determined internally based on the [text format](../UserInfo/text-fmts.html) and precision.

Imaginary unit: `im` specifies the form of a complex number. By default, `complex` numbers will be written as ordered pairs, e.g. `(2.45,3.45)`. If `im` is specified, then the number will be written as a sum with the specified imaginary unit, e.g. `2.45+3.45j` for `im="j"` or `2.45+3.45*1i` for `im="*1i"`.
