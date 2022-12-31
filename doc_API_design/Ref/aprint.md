---
title: aprint
author: Austin C Bullock
---

## [interface aprint](../../interface/aprint.html)

For `x` of rank `1` or `2` and of type `character`, [`String`](../../type/string.html):

```fortran
call aprint(x)
```

For `x` of rank `1` or `2` and of type `integer`:

```fortran
call aprint(x, fmt)
```

* `fmt` is `optional`, may be one of `int_fmts`

For `x` of rank `1` or `2` and of type `real`:

```fortran
call aprint(x, fmt, decimals)
```

* `fmt` is `optional`, may be one of `real_fmts`
* `decimals` is `optional` and of type `integer`

For `x` of rank `1` or `2` and of type `complex`:

```fortran
call aprint(x, fmt, decimals, im)
```

* `fmt` is `optional`, may be one of `real_fmts`
* `decimals` is `optional` and of type `integer`
* `im` is `optional` and of type `character(len=*)`

### Optional Arguments

Integer formats (default is `'i'`):

```fortran
int_fmts = [ 'i', 'z' ]
```

Real formats (default is `'f'`):

```fortran
real_fmts = [ 'e', 'f', 'z' ]
```

Decimals (default is `2`): `decimals` specifies the number of digits on the rhs of the radix point.

Imaginary unit (default is `'j'`): `im` specifies the form of a complex number.

@note The optional arguments for `aprint` are different than for `str` and `to_file`, and better suited for easy viewing of array sections.
