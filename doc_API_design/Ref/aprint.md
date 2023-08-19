---
title: aprint
author: Austin C Bullock
---

## [interface aprint](../../interface/aprint.html)

*Description*: Subroutine for printing arrays and array sections to stdout.

For `x` an array of rank `1` or `2` and of type `character` or `String`:

```fortran
call aprint(x)
```

For `x` an array of rank `1` or `2` and of type `integer`:

```fortran
call aprint(x [, fmt])
```

* `fmt` is `optional`, may be one of `INT_FMTS`

For `x` an array of rank `1` or `2` and of type `real`:

```fortran
call aprint(x [, fmt, decimals])
```

* `fmt` is `optional`, may be one of `REAL_FMTS`
* `decimals` is `optional` and of type `integer`

For `x` an array of rank `1` or `2` and of type `complex`:

```fortran
call aprint(x [, fmt, decimals, im])
```

* `fmt` is `optional`, may be one of `REAL_FMTS`
* `decimals` is `optional` and of type `integer`
* `im` is `optional` and of type `character(len=*)`

### Optional Arguments

Integer formats (default is `"i"`):

```fortran
INT_FMTS = [ "i", "z" ]
```

Real formats (default is `"f"`):

```fortran
REAL_FMTS = [ "e", "f", "z" ]
```

Decimals (default is `2`): `decimals` specifies the number of digits on the rhs of the radix point.

Imaginary unit (default is `"j"`): `im` specifies the form of a complex number.

@note The optional arguments for `aprint` are different than elsewhere, and better suited for easy viewing of array sections.
