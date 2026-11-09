---
title: io_strings
author: Austin C Bullock
---

## [module io_strings](../../module/io_strings.html)

*Description*: Functions on `character` and `String` values: case
conversion, prefix and suffix tests, searching, and removing given
characters from both ends. The names and arguments follow the
[Fortran standard library](https://stdlib.fortran-lang.org), so calls
written for stdlib, including keyword arguments, work here too.

They are in a module of their own, separate from `io_fortran_lib`, so
that adding them couldn't break a program that uses `io_fortran_lib`
without an `only` list and defines a procedure with one of these names.
Use it alongside `io_fortran_lib`:

```fortran
use io_fortran_lib, only: String
use io_strings, only: to_upper, starts_with, find

print "(a)", to_upper("abc")                     ! ABC
print *, starts_with("data.csv", "data")         ! T
print *, find("one two one", "one", occurrence=2) ! 9
```

Each function is also a [String method](String-methods.html), such as
`s%to_upper()`, which needs no extra `use`.

### to_upper

```fortran
    result = to_upper(string)
```

* `string` is of type `character(len=*)` or `String`

*Description*: Converts the ASCII letters `a` to `z` to upper case,
elementally. Other characters, including bytes outside ASCII, are
unchanged. For `character`, the result has the same length as
`string`; for `String`, it is a `String`.

### to_lower

```fortran
    result = to_lower(string)
```

* `string` is of type `character(len=*)` or `String`

*Description*: Converts the ASCII letters `A` to `Z` to lower case,
elementally, in the same way as `to_upper`.

### starts_with

```fortran
    result = starts_with(string, substring)
```

* `string` and `substring` are each of type `character(len=*)` or
  `String`

*Description*: Returns `.true.` where `string` starts with `substring`,
elementally. Every string starts with the empty string. Trailing blanks
count, so `starts_with("ab", "ab ")` is `.false.`.

### ends_with

```fortran
    result = ends_with(string, substring)
```

* `string` and `substring` are each of type `character(len=*)` or
  `String`

*Description*: Returns `.true.` where `string` ends with `substring`,
elementally, in the same way as `starts_with`.

### find

```fortran
    result = find(string, pattern [, occurrence, consider_overlapping])
```

* `string` and `pattern` are each of type `character(len=*)` or `String`
* `occurrence` is `optional` and of type `integer` (default is `1`)
* `consider_overlapping` is `optional` and of type `logical` (default
  is `.true.`)

*Description*: Returns the position where the `occurrence`th occurrence
of `pattern` starts in `string`, elementally, or `0` if there is none.
Occurrences may overlap unless `consider_overlapping` is `.false.`:
`find("aaaa", "aa", occurrence=2)` is `2`, and `3` without overlap. An
empty `pattern`, or an `occurrence` below `1`, gives `0`. The result is
a default `integer`, like the intrinsic `index`.

### trim

```fortran
    result = trim(string, chars)
    result = trim(string [, chars])   ! For string of type String
```

* `string` is of type `character(len=*)` or `String`
* `chars` is of type `character(len=*)`, and optional for `String`

*Description*: Removes every character in `chars` from both ends of
`string`: `trim("--name--", "-")` is `"name"`. `chars` is a set, so
`trim("-*a*-", "*-")` is `"a"`. For a `String` without `chars`, it
removes whitespace, as [String%trim](String-methods.html#trim) does.

`trim` extends the intrinsic: `trim(string)` with one `character`
argument is still the intrinsic `trim`, which removes trailing blanks.
For `character`, `trim` is not elemental, like the intrinsic, since the
length of the result depends on the value of `string`.
