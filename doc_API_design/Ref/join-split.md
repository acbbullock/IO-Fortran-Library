---
title: join and split
author: Austin C Bullock
---

## [interface join](../../interface/join.html)

*Description*: Function for joining a vector of `tokens` into a scalar
`character` or `String`.

To join a one-dimensional array `tokens` of type `character` or
`String`:

```fortran
result = join(tokens [, separator])
```

* `separator` is `optional` and of type `character(len=*)`

For a subroutine version of `join`, see
[join](String-methods.html#join).

@note The return type of `join` is the same as the type of `tokens`.

## [interface split](../../interface/split.html)

*Description*: Function for splitting a scalar `character` or `String`
into a vector of `tokens`.

For `substring` a scalar `character` or `String`:

```fortran
result = split(substring [, separator])
```

* `separator` is `optional` and of type `character(len=*)`

For `substring` a scalar variable of type `String`:

```fortran
result = substring%split([separator])
```

* `separator` is `optional` and of type `character(len=*)`

@note The type-bound procedure access of the form `substring%split()`
is valid when `substring` is a `String` variable. To split a
`String`-valued expression, the expression must be passed to `split` by
the form `split(substring)`.

@note The return type of `split` is always `String`.

### Optional Arguments

Separator (default is `SPACE`): the separator to use when joining or
splitting.
