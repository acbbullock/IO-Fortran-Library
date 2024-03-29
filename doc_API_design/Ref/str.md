---
title: str
author: Austin C Bullock
---

## [interface str](../../interface/str.html)

*Description*: Function for returning a `character` representation of a
number.

To return the
[empty string](../../module/io_fortran_lib.html#variable-empty_str),
use no arguments:

```fortran
result = str()
```

For `x` a scalar of type `String`:

```fortran
result = str(x)
```

This is for scalar `String` to `character` conversion.

For `x` a scalar of type `integer`:

```fortran
result = str(x [, fmt])
```

* `fmt` is `optional`, may be one of `INT_FMTS`

For `x` a scalar of type `real`:

```fortran
result = str(x [, locale, fmt, decimals])
```

* `locale` is `optional`, may be one of `LOCALES`
* `fmt` is `optional`, may be one of `REAL_FMTS`
* `decimals` is `optional` and of type `integer`

For `x` a scalar of type `complex`:

```fortran
result = str(x [, locale, fmt, decimals, im])
```

* `locale` is `optional`, may be one of `LOCALES`
* `fmt` is `optional`, may be one of `REAL_FMTS`
* `decimals` is `optional` and of type `integer`
* `im` is `optional` and of type `character(len=*)`

@note Note that `str` operates on scalars only. For elemental
functionality, see [String](String.html).

### Optional Arguments

Integer formats (default is `"i"`):

```fortran
INT_FMTS = [ "i", "z" ]
```

Real formats (default is `"e"`):

```fortran
REAL_FMTS = [ "e", "f", "z" ]
```

Locales (default is `"US"`):

```fortran
LOCALES = [ "US", "EU" ]
```

Decimals: `decimals` specifies the number of digits on the rhs of the
radix point, with a default determined internally based on the
[text format](../UserInfo/text-fmts.html) and precision.

Imaginary unit: `im` specifies the form of a complex number. By
default, `complex` numbers will be written as ordered pairs, e.g.
`(2.45,3.45)`. If `im` is specified, then the number will be written as
a sum with the specified imaginary unit, e.g. `2.45+3.45j` for `im="j"`
or `2.45+3.45*1i` for `im="*1i"`.
