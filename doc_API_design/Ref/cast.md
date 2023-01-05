---
title: cast
author: Austin C Bullock
---

## [interface cast](../../interface/cast.html)

*Description*: Subroutine for casting a scalar `character` string into a number.

For casting a scalar `character` string `chars` into a variable `into` of type `integer`:

```fortran
call cast(chars, into, fmt)
```

* `fmt` is `optional`, may be one of `INT_FMTS`

For casting a scalar `character` string `chars` into a variable `into` of type `real`:

```fortran
call cast(chars, into, locale, fmt)
```

* `locale` is `optional`, may be one of `LOCALES`
* `fmt` is `optional`, may be one of `REAL_FMTS`

For casting a scalar `character` string `chars` into a variable `into` of type `complex`:

```fortran
call cast(chars, into, locale, fmt, im)
```

* `locale` is `optional`, may be one of `LOCALES`
* `fmt` is `optional`, may be one of `REAL_FMTS`
* `im` is `optional` and of type `character(len=*)`

### Optional Arguments

Integer formats (default is `'i'`):

```fortran
INT_FMTS = [ 'i', 'z' ]
```

Real formats (default is `'e'`):

```fortran
REAL_FMTS = [ 'e', 'f', 'z' ]
```

Locales (default is `'US'`):

```fortran
LOCALES = [ 'US', 'EU' ]
```

Imaginary unit: `im` specifies the form of a complex number. If not present, `complex` numbers will be assumed to be written as ordered pairs, e.g. `(2.45,3.45)`.
