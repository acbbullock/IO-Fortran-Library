---
title: String
author: Austin C Bullock
---

## [interface String](../../interface/string.html)

*Description*: Function for returning a
[String](../../type/string.html) representation of numbers.

To return the empty `String`, use no arguments:

```fortran
result = String()
```

For `x` a scalar or array of any rank and of type `character`:

```fortran
result = String(x)
```

This is for `character` to `String` conversion.

For `x` a scalar or array of any rank and of type `integer`:

```fortran
result = String(x [, fmt])
```

* `fmt` is `optional`, may be one of `INT_FMTS`

For `x` a scalar or array of any rank and of type `real`:

```fortran
result = String(x [, locale, fmt, decimals])
```

* `locale` is `optional`, may be one of `LOCALES`
* `fmt` is `optional`, may be one of `REAL_FMTS`
* `decimals` is `optional` and of type `integer`

For `x` a scalar or array of any rank and of type `complex`:

```fortran
result = String(x [, locale, fmt, decimals, im])
```

* `locale` is `optional`, may be one of `LOCALES`
* `fmt` is `optional`, may be one of `REAL_FMTS`
* `decimals` is `optional` and of type `integer`
* `im` is `optional` and of type `character(len=*)`

@note Unlike [str](str.html), which takes scalar arguments only and
returns a `character`, `String` operates elementally and returns a
[String](../../type/string.html).

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
