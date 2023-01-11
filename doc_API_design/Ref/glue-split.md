---
title: glue and split
author: Austin C Bullock
---

## [interface glue](../../interface/glue.html)

*Description*: Function for gluing a vector of `tokens` into a scalar `character` or `String`.

To glue a one-dimensional array `tokens` of type `character` or `String`:

```fortran
result = glue(tokens, separator)
```

* `separator` is `optional` and of type `character(len=*)`

For a subroutine version of `glue`, see [glue](string-methods.html#glue).

@note The return type of `glue` is the same as the type of `tokens`.

## [interface split](../../interface/split.html)

*Description*: Function for splitting a scalar `character` or `String` into a vector of `tokens`.

For `substring` a scalar `character` or `String`:

```fortran
result = split(substring, separator)
```

* `separator` is `optional` and of type `character(len=*)`

For `substring` a scalar variable of type `String`:

```fortran
result = substring%split(separator)
```

@note The type-bound procedure access of the form `substring%split()` is valid when `substring` is a `String` variable. To split a `String`-valued expression, the expression must be passed to `split` by the form `split(substring)`.

@note The return type of `split` is always `String`.

### Optional Arguments

Separator (default is `SPACE`): the separator to use when gluing or splitting.
