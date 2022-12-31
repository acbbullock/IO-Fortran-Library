---
title: Locales
author: Austin C Bullock
---

When writing floating point numbers of type `real` or `complex` as strings with [str](../Ref/str.html) or [to_file](../Ref/to_file.html), any of the following locales may be used:

```fortran
locales = [ 'US', 'EU' ]
```

* `'US'`: US decimal with default comma delimiter (default), e.g. `1.23456789,0.12345678`
* `'EU'`: EU decimal with default semicolon delimiter, e.g. `1,23456789;0,12345678`
