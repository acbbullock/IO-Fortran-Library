---
title: Numeric text formats
author: Austin C Bullock
---

When writing `integer` data as strings with [str](../Ref/str.html), [String](../Ref/string.html), or [to_file](../Ref/to_file.html), any of the following text formats may be used:

```fortran
INT_FMTS = [ 'i', 'z' ]
```

* `'i'`: integer format (default), e.g. `123456`
* `'z'`: hexadecimal format, e.g. `1E240`

When writing floating point numbers of type `real` or `complex` as strings with [str](../Ref/str.html), [String](../Ref/string.html), or [to_file](../Ref/to_file.html), any of the following text formats may be used:

```fortran
REAL_FMTS = [ 'e', 'f', 'z' ]
```

* `'e'`: normalized exponential format (default), e.g. `1.2345678901234567E+005`
* `'f'`: decimal format, e.g. `123456.78901234567`
* `'z'`: hexadecimal format, e.g. `40FE240C9FCB68CD`

@note The `'z'` hexadecimal format is an integer format and may be used for `integer`, `real`, or `complex` data. Floating point numbers are interpreted bit-wise as integers when written with the `'z'` format, preventing any loss of precision in a round-trip conversion. This format is preferred for portable data transfers for which precision losses are intolerable.

When moving data in the opposite direction with the complementary procedures [cast](../Ref/cast.html), [cast_string](../Ref/cast_string.html), or [from_file](../Ref/from_file.html), the same `fmt` is required to properly cast the data. Specifying a `fmt` that is different from what is actually present may result in an I/O syntax error.

@note In round-trip conversions for type `real` and `complex` of the form `numeric -> string -> numeric`, strings will be written by default with a number of significant digits required in the worst case for a lossless round-trip conversion. In general, one may expect the `f` format to produce losses in round-trip conversion. However, the `e` format will rarely produce any losses, and the `z` format will never produce losses as it involves direct bit transfer (no base-10 ⇆ base-2 conversions are involved).
