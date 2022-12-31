---
title: Numeric text formats
author: Austin C Bullock
---

When writing `integer` data as strings with [str](../Ref/str.html) or [to_file](../Ref/to_file.html), any of the following text formats may be used:

```fortran
int_fmts = [ 'i', 'z' ]
```

* `'i'`: integer format (default), e.g. `123456`
* `'z'`: hexadecimal format, e.g. `1E240`

When writing floating point numbers of type `real` or `complex` as strings with [str](../Ref/str.html) or [to_file](../Ref/to_file.html), any of the following text formats may be used:

```fortran
real_fmts = [ 'e', 'f', 'z' ]
```

* `'e'`: normalized exponential format (default), e.g. `1.2345678901234567E+005`
* `'f'`: decimal format, e.g. `123456.78901234567`
* `'z'`: hexadecimal format, e.g. `40FE240C9FCB68CD`

@note The `'z'` hexadecimal format is an integer format and may be used for `integer`, `real`, or `complex` data. Floating point numbers are interpreted bit-wise as integers when written with the `'z'` format, preventing any loss of precision in a round-trip conversion. This format is preferred for portable data transfers for which precision losses are intolerable.
