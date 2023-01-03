---
title: Operators
author: Austin C Bullock
---

## [Operator interfaces](../../lists/procedures.html)

### Concatenation ([//](../../interface/operator%28SLASHSLASH%29.html) and [+](../../interface/operator%28%2B%29.html))

For `x` and `y` scalars or arrays of any compatible rank, and of any combination of type `character`, `String`:

```fortran
result = x // y
```

```fortran
result = x + y
```

@note Concatenation of mixed type will return a `String`.

### Excision ([-](../../interface/operator%28-%29.html))

For `x` and `y` scalars or arrays of any compatible rank, and of any combination of type `character`, `String`:

```fortran
result = x - y
```

@note Excision always returns a `String` even when both arguments are of type `character`.

### Repetition ([**](../../interface/operator%28ASTERISKASTERISK%29.html))

For `x` a scalar or array of any rank, and of type `character`, `String`:

```fortran
result = x**exponent
```

* `exponent` is of type `integer`

@note The `**` operator is a wrapper for the [repeat](https://gcc.gnu.org/onlinedocs/gfortran/REPEAT.html) intrinsic.

### Equivalence ([==](../../interface/operator%28%3D%3D%29.html))

For `x` and `y` scalars or arrays of any compatible rank, and of any combination of type `character`, `String`:

```fortran
result = (x == y)
```
