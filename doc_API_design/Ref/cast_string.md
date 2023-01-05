---
title: cast_string
author: Austin C Bullock
---

## [interface cast_string](../../interface/cast_string.html)

*Description*: Subroutine for casting a `String` type into a number.

For casting a `String` type `self` into a variable `into` of compatible rank and of type `integer`:

```fortran
call cast_string(self, into, fmt)
```

```fortran
call self%cast(into, fmt)
```

* `fmt` is `optional`, may be one of `INT_FMTS`

For casting a `String` type `self` into a variable `into` of compatible rank and of type `real`:

```fortran
call cast_string(self, into, locale, fmt)
```

```fortran
call self%cast(into, locale, fmt)
```

* `locale` is `optional`, may be one of `LOCALES`
* `fmt` is `optional`, may be one of `REAL_FMTS`

For casting a `String` type `self` into a variable `into` of compatible rank and of type `complex`:

```fortran
call cast_string(self, into, locale, fmt, im)
```

```fortran
call self%cast(into, locale, fmt, im)
```

* `locale` is `optional`, may be one of `LOCALES`
* `fmt` is `optional`, may be one of `REAL_FMTS`
* `im` is `optional` and of type `character(len=*)`

@note The type-bound procedure access of the form `call self%cast()` is valid when `self` is a `String` variable. To cast a `String` expression, the expression must be passed to `cast_string` by the form `call cast_string()`.

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
