---
title: cast
author: Austin C Bullock
---

## [interface cast](../../interface/cast.html)

*Description*: Subroutine for casting between numeric and string data.

### Casting numbers to string variables

For casting `x` of type `integer` into a variable `into` of type `character` (scalar only) or `String` (any rank):

```fortran
call cast(x, into, fmt)
```

* `fmt` is `optional`, may be one of `INT_FMTS`

For casting `x` of type `real` into a variable `into` of type `character` (scalar only) or `String` (any rank):

```fortran
call cast(x, into, locale, fmt, decimals)
```

* `locale` is `optional`, may be one of `LOCALES`
* `fmt` is `optional`, may be one of `REAL_FMTS`
* `decimals` is `optional` and of type `integer`

For casting `x` of type `complex` into a variable `into` of type `character` (scalar only) or `String` (any rank):

```fortran
call cast(x, into, locale, fmt, decimals, im)
```

* `locale` is `optional`, may be one of `LOCALES`
* `fmt` is `optional`, may be one of `REAL_FMTS`
* `decimals` is `optional` and of type `integer`
* `im` is `optional` and of type `character(len=*)`

@note While [str](str.html) and [String](String.html) return values which may be used flexibly inside of string expressions, `cast` may be used as above to write directly to variables. When converting large amounts of data to strings, `cast` may be up to 2x faster than the functional alternatives since the total number of string allocations is reduced by at least half, all else being equal.

### Casting strings to numeric variables

For casting `substring` of type `character` (scalar only) or `String` (any rank) into a variable `into` of type `integer`:

```fortran
call cast(substring, into, fmt)
```

```fortran
call substring%cast(into, fmt)
```

* `fmt` is `optional`, may be one of `INT_FMTS`

For casting `substring` of type `character` (scalar only) or `String` (any rank) into a variable `into` of type `real`:

```fortran
call cast(substring, into, locale, fmt)
```

```fortran
call substring%cast(into, locale, fmt)
```

* `locale` is `optional`, may be one of `LOCALES`
* `fmt` is `optional`, may be one of `REAL_FMTS`

For casting `substring` of type `character` (scalar only) or `String` (any rank) into a variable `into` of type `complex`:

```fortran
call cast(substring, into, locale, fmt, im)
```

```fortran
call substring%cast(into, locale, fmt, im)
```

* `locale` is `optional`, may be one of `LOCALES`
* `fmt` is `optional`, may be one of `REAL_FMTS`
* `im` is `optional` and of type `character(len=*)`

@warning The arguments `x` and `substring` must always be of the same rank and shape as `into`, which must be pre-allocated prior to calling `cast` due to the restrictions on `intent(out)` arguments of `elemental` procedures.

@note The type-bound procedure access of the form `call substring%cast()` is valid when `substring` is a `String` variable. To cast a `String`-valued expression, the expression must be passed to `cast` by the form `call cast()`.

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

Decimals: `decimals` specifies the number of digits on the rhs of the radix point, with a default determined internally based on the [text format](../UserInfo/text-fmts.html) and precision.

Imaginary unit: `im` specifies the form of a complex number. If not present, `complex` numbers will be assumed to be written as ordered pairs, e.g. `(2.45,3.45)`.
