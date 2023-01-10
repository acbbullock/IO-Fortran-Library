---
title: Locales
author: Austin C Bullock
---

When writing floating point numbers of type `real` or `complex` as strings with [String](../Ref/string.html), [str](../Ref/str.html), or [to_file](../Ref/to_file.html), any of the following locales may be used:

```fortran
LOCALES = [ 'US', 'EU' ]
```

* `'US'`: US decimal with default comma delimiter (default), e.g. `1.23456789,0.12345678`
* `'EU'`: EU decimal with default semicolon delimiter, e.g. `1,23456789;0,12345678`

When moving data in the opposite direction with the complementary procedures [cast](../Ref/cast.html) or [from_file](../Ref/from_file.html), the same `locale` is required to properly read the decimals. Specifying a `locale` that is different from what is actually present may result in an I/O syntax error.
