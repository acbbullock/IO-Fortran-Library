---
title: Locales
author: Austin C Bullock
---

When writing floating point numbers of type `real` or `complex` as
strings with [String](../Ref/String.html), [str](../Ref/str.html), or
[to_file](../Ref/to_file.html), any of the following locales may be
used:

```fortran
character(len=2), parameter :: LOCALES(*) = [ "US", "EU" ] ! Allowed locale specifiers
```

* `"US"`: US decimal (default), e.g. `1.23456789`
* `"EU"`: EU decimal, e.g. `1,23456789`

With `to_file` and `from_file`, the `locale` additionally determines
the default delimiter, e.g. `1.23456789,0.12345678` for `locale="US"`
and `1,23456789;0,12345678` for `locale="EU"`.

When moving data in the opposite direction with the complementary
procedures [cast](../Ref/cast.html) or
[from_file](../Ref/from_file.html), the same `locale` is required to
properly read the decimals. Specifying a `locale` that is different
from what is actually present may result in an I/O syntax error.
