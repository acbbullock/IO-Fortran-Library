---
title: str
author: Austin C Bullock
---

## [interface str](../../interface/str.html)

For `x` a scalar of type `integer`:

```fortran
result = str(x, fmt)
```

* `fmt` is `optional`, may be one of `int_fmts`

For `x` a scalar of type `real`:

```fortran
result = str(x, locale, fmt, decimals)
```

* `locale` is `optional`, may be one of `locales`
* `fmt` is `optional`, may be one of `real_fmts`
* `decimals` is `optional` and of type `integer`

For `x` a scalar of type `complex`:

```fortran
result = str(x, locale, fmt, decimals, im)
```

* `locale` is `optional`, may be one of `locales`
* `fmt` is `optional`, may be one of `real_fmts`
* `decimals` is `optional` and of type `integer`
* `im` is `optional` and of type `character(len=*)`

@note Note that `str` is restricted to operate on scalars only since `character` arrays of non-identical length elements are not defined by the Fortran 2018 standard. For this functionality, see [String](string.html).

### Optional Arguments

Integer formats (default is `'i'`):

```fortran
int_fmts = [ 'i', 'z' ]
```

Real formats (default is `'e'`):

```fortran
real_fmts = [ 'e', 'f', 'z' ]
```

Locales (default is `'US'`):

```fortran
locales = [ 'US', 'EU' ]
```

Decimals: `decimals` specifies the number of digits on the rhs of the radix point, with a default determined internally based on the [text format](../UserInfo/text-fmts.html) and precision.

Imaginary unit: `im` specifies the form of a complex number. By default, `complex` numbers will be written as ordered pairs, e.g. `(2.45,3.45)`. If `im` is specified, then the number will be written as a sum with the specified imaginary unit, e.g. `2.45+3.45j` for `im='j'` or `2.45+3.45*1i` for `im='*1i'`.
