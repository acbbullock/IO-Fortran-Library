---
title: cast
author: Austin C Bullock
---

## [interface cast](../../interface/cast.html)

*Description*: Subroutine for casting a `character` or `String` into a number.

For casting `substring` of type `character` (scalar only) or `String` (any rank) into a variable `into` of compatible rank and of type `integer`:

```fortran
call cast(substring, into, fmt)
```

* `fmt` is `optional`, may be one of `INT_FMTS`

For casting `substring` of type `character` (scalar only) or `String` (any rank) into a variable `into` of compatible rank and of type `real`:

```fortran
call cast(substring, into, locale, fmt)
```

* `locale` is `optional`, may be one of `LOCALES`
* `fmt` is `optional`, may be one of `REAL_FMTS`

For casting `substring` of type `character` (scalar only) or `String` (any rank) into a variable `into` of compatible rank and of type `complex`:

```fortran
call cast(substring, into, locale, fmt, im)
```

* `locale` is `optional`, may be one of `LOCALES`
* `fmt` is `optional`, may be one of `REAL_FMTS`
* `im` is `optional` and of type `character(len=*)`

For casting a `String` variable `substring` into a variable `into` of compatible rank and of type `integer`:

```fortran
call substring%cast(into, fmt)
```

For casting a `String` variable `substring` into a variable `into` of compatible rank and of type `real`:

```fortran
call substring%cast(into, locale, fmt)
```

For casting a `String` variable `substring` into a variable `into` of compatible rank and of type `complex`:

```fortran
call substring%cast(into, locale, fmt, im)
```

@note The type-bound procedure access of the form `call substring%cast()` is valid when `substring` is a `String` variable. To cast a `String`-valued expression, the expression must be passed to `cast` by the form `call cast()`.

@warning In all cases, `into` must be pre-allocated prior to calling `cast` due to the restriction that `intent(out)` arguments of `elemental` procedures may not be `allocatable`.

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
