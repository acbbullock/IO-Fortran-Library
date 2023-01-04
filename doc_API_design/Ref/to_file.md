---
title: to_file
author: Austin C Bullock
---

## [interface to_file](../../interface/to_file.html)

*Description*: Subroutine for writing an array of uniform data type to an external file.

For `x` an array of rank `1`, `2` and of type `integer`:

```fortran
call to_file(x, file_name, header, dim, delim, fmt)
```

```fortran
call to_file(x, file_name, header, delim, fmt)
```

* `file_name` is of type `character(len=*)`
* `header` is `optional` and of type `character(len=*), dimension(:)`
* `dim` is `optional` and of type `integer`
* `delim` is `optional` and of type `character(len=*)`
* `fmt` is `optional`, may be one of `int_fmts`

For `x` an array of rank `1`, `2` and of type `real`:

```fortran
call to_file(x, file_name, header, dim, locale, delim, fmt, decimals)
```

```fortran
call to_file(x, file_name, header, locale, delim, fmt, decimals)
```

* `file_name` is of type `character(len=*)`
* `header` is `optional` and of type `character(len=*), dimension(:)`
* `dim` is `optional` and of type `integer`
* `locale` is `optional`, may be one of `locales`
* `delim` is `optional` and of type `character(len=*)`
* `fmt` is `optional`, may be one of `real_fmts`
* `decimals` is `optional` and of type `integer`

For `x` an array of rank `1`, `2` and of type of type `complex`:

```fortran
call to_file(x, file_name, header, dim, locale, delim, fmt, decimals, im)
```

```fortran
call to_file(x, file_name, header, locale, delim, fmt, decimals, im)
```

* `file_name` is of type `character(len=*)`
* `header` is `optional` and of type `character(len=*), dimension(:)`
* `dim` is `optional` and of type `integer`
* `locale` is `optional`, may be one of `locales`
* `delim` is `optional` and of type `character(len=*)`
* `fmt` is `optional`, may be one of `real_fmts`
* `decimals` is `optional` and of type `integer`
* `im` is `optional` and of type `character(len=*)`

For `x` an array of any rank `3`-`15` and of type `integer`, `real`, `complex`:

```fortran
call to_file(x, file_name)
```

* `file_name` is of type `character(len=*)`

@warning `file_name` may be a relative path, but absolute paths are not guaranteed to work on every platform.

### Optional Arguments

Header (default is none): `header` is a [character array literal](../UserInfo/compilers.html). For `x` of rank `1`, `header` may be of size `1` or `size(x)`. For `x` of rank `2`, `header` may be of size `1` or `size(x, dim=2)`.

Dimension: `dim` specifies whether to write along the rows (`dim=1`) or along the columns (`dim=2`), choosing the former by default unless `size(header)` is `size(x)`.

Locales (default is `'US'`):

```fortran
locales = [ 'US', 'EU' ]
```

Delimiter (default is `','`): data separator. Default is `','` for `'US'` locale and `';'` for `'EU'` locale. It is always recommended to omit the delimiter argument for default unless a custom delimiter is really necessary.

Integer formats (default is `'i'`):

```fortran
int_fmts = [ 'i', 'z' ]
```

Real formats (default is `'e'`):

```fortran
real_fmts = [ 'e', 'f', 'z' ]
```

Decimals: `decimals` specifies the number of digits on the rhs of the radix point, with a default determined internally based on the [text format](../UserInfo/text-fmts.html) and precision.

Imaginary unit: `im` specifies the form of a complex number. By default, `complex` numbers will be written as ordered pairs, e.g. `(2.45,3.45)`. If `im` is specified, then the number will be written as a sum with the specified imaginary unit, e.g. `2.45+3.45j` for `im='j'` or `2.45+3.45*1i` for `im='*1i'`.
