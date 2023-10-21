---
title: Numeric text formats
author: Austin C Bullock
---

When writing `integer` data as strings with
[String](../Ref/String.html), [str](../Ref/str.html), or
[to_file](../Ref/to_file.html), any of the following text formats may
be used:

```fortran
INT_FMTS = [ "i", "z" ]
```

* `"i"`: integer format (default), e.g. `123456`
* `"z"`: hexadecimal format, e.g. `0x1e240`

When writing floating point numbers of type `real` or `complex` as
strings with [String](../Ref/String.html), [str](../Ref/str.html), or
[to_file](../Ref/to_file.html), any of the following text formats may
be used:

```fortran
REAL_FMTS = [ "e", "f", "z" ]
```

* `"e"`: normalized exponential format (default), e.g.
  `1.23456789012345675e+005`
* `"f"`: decimal format, e.g. `123456.789012345674`
* `"z"`: hexadecimal format, e.g. `0x40fe240c9fcb68cd`

@note The `"z"` hexadecimal format is an unsigned integer format and
may be used for `integer`, `real`, or `complex` data. Floating point
numbers are interpreted bit-wise as unsigned integers when written with
the `"z"` format, preventing any loss of precision in a round-trip
conversion. This format is preferred in data transfers for which
precision losses are intolerable. The `"z"` format may also be
preferred for faster read/write times and more compact storage.

When moving data in the opposite direction with the complementary
procedures [cast](../Ref/cast.html) or
[from_file](../Ref/from_file.html), the same `fmt` is required to
properly cast the data. Specifying a `fmt` that is different from what
is actually present may result in an I/O syntax error.

@note By default, `real` and `complex` data will be written with a
number of significant digits required for a lossless round-trip
conversion of the form `numeric -> string -> numeric`. In general, one
may expect the `f` format to produce losses in round-trip conversion of
up to a few `epsilon` in as many as a fifth of transfers. However, the
`e` format is not expected to produce any losses, and the `z` format
will never produce losses as it involves direct bit transfer (no
base-10 ⇆ base-2 conversions are involved).

@note For negative integers, the hexadecimal format `"z"` produces
ambiguities across storage sizes. For instance, the integer `0xff`
equates to `-1` in `int8` storage but equates to `255` in larger
storage sizes. Similarly, the integer `0xffff` equates to `-1` in
`int16` storage but equates to `65535` in larger storage sizes, and so
on. It is the responsibility of the programmer to cast hexadecimal
strings into variables with the proper storage size.
