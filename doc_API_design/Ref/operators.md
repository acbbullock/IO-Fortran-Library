---
title: Operators
author: Austin C Bullock
---

## [Operator interfaces](../../lists/procedures.html)

*Description*: Extended operators for convenient string manipulations.

### Concatenation ([//](../../interface/operator%28SLASHSLASH%29.html) and [+](../../interface/operator%28%2B%29.html))

For `x` and `y` scalars or arrays of any compatible rank, and of any
combination of type `character` and `String`:

```fortran
result = x // y
```

```fortran
result = x + y
```

@note Concatenation of mixed type will return a `String`.

### Excision ([-](../../interface/operator%28-%29.html))

For `x` and `y` scalars or arrays of any compatible rank, and of any
combination of type `character` and `String`:

```fortran
result = x - y
```

@note Excision always returns a `String` value even when both arguments
are of type `character`. This ensures that excision can be performed
elementally even for `character` values, which would not be
well-defined with a return type of `character`. For two scalar
`character` values, one may simply perform the conversion
`result = str(x - y)` to return a scalar `character`.

@note
String arithmetic is not associative, commutative, or distributive in
general:

* (Associative) `(x + y) + z == x + (y + z)` and `x + (y - z) /= (x +
  y) - z` are both `.true.` in general.
* (Commutative) `x + y /= y + x` and `x + y - z /= x - z + y` are both
  `.true.` in general.
* (Distributive) `x - (y + z) /= x - y - z` is `.true.` in general.
@endnote

### Repetition ([**](../../interface/operator%28ASTERISKASTERISK%29.html))

For `x` a scalar or array of any rank, and of type `character` or
`String`:

```fortran
result = x**ncopies
```

* `ncopies` is of type `integer`

@note The `**` operator is a wrapper for the
[repeat](https://gcc.gnu.org/onlinedocs/gfortran/REPEAT.html)
intrinsic, and extended for type `String`.

### Equivalence ([==](../../interface/operator%28%3D%3D%29.html))

For `x` and `y` scalars or arrays of any compatible rank, and of any
combination of type `character` and `String`:

```fortran
result = (x == y)
```

```fortran
result = (x .eq. y)
```

### Non-equivalence ([/=](../../interface/operator%28SLASH%3D%29.html))

For `x` and `y` scalars or arrays of any compatible rank, and of any
combination of type `character` and `String`:

```fortran
result = (x /= y)
```

```fortran
result = (x .ne. y)
```
