---
title: str
author: Austin C Bullock
---

## [interface str](../../interface/str.html)

If `x` is `integer`:

```fortran
result = str(x, fmt)
```

* `fmt` is `optional`, may be one of `int_fmts`

If `x` is `real`:

```fortran
result = str(x, locale, fmt, decimals)
```

* `locale` is `optional`, may be one of `locales`
* `fmt` is `optional`, may be one of `real_fmts`
* `decimals` is `optional` and of type `integer`

If `x` is `complex`:

```fortran
result = str(x, locale, fmt, decimals, im)
```

* `locale` is `optional`, may be one of `locales`
* `fmt` is `optional`, may be one of `real_fmts`
* `decimals` is `optional` and of type `integer`
* `im` is `optional` and of type `character(len=*)`

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

Decimals: `decimals` specifies the number of digits on the rhs of the radix point, with a default determined internally based on the [text format](../user-info.html) and precision.

Imaginary unit: `im` specifies the form of a complex number. By default, `complex` numbers will be written as ordered pairs, e.g. `(2.45,3.45)`. If `im` is specified, then the number will be written as a sum with the specified imaginary unit, e.g. `2.45+3.45j` for `im='j'` or `2.45+3.45*1i` for `im='*1i'`.
