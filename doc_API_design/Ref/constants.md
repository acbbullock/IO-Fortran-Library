---
title: CONSTANTS
author: Austin C Bullock
---

The [[io_fortran_lib]] module provides access to a handful of
`parameter` [constants](../../module/io_fortran_lib.html#variable-iofl_nl)
for general use:

* `IOFL_NL`: The newline character (system agnostic).
* `IOFL_SPACE`: The space character.
* `IOFL_CR`: The carriage return character.
* `IOFL_LF`: The line feed character.
* `IOFL_TAB`: The horizontal tab character.
* `IOFL_FF`: The form feed character.
* `IOFL_VT`: The vertical tab character.
* `IOFL_BELL`: The bell/alert character.
* `IOFL_NUL`: The null character, which is also the C string terminator
  `c_null_char`.

The module also exports the [error codes](../UserInfo/error-codes.html)
`IOFL_READ_ERR`, `IOFL_WRITE_ERR`, `IOFL_ALLOC_ERR` and `IOFL_ARG_ERR`.

### Names before version 2.1

Before version 2.1 the constants had short names, which are kept so that
existing programs compile unchanged:

| Name | Same as |
|---|---|
| `NL` | `IOFL_NL` |
| `SPACE` | `IOFL_SPACE` |
| `CR` | `IOFL_CR` |
| `LF` | `IOFL_LF` |
| `TAB`, `HT` | `IOFL_TAB` |
| `FF` | `IOFL_FF` |
| `VT` | `IOFL_VT` |
| `BELL` | `IOFL_BELL` |
| `NUL`, `CNUL` | `IOFL_NUL` |

Prefer the `IOFL_` names in new code. Fortran names are not case
sensitive, so a program that uses the module without an `only` list
cannot declare a variable called `cr`, `lf` or `tab`. To keep a short
name in your own code, import only what you need, renaming if you like:

```fortran
use io_fortran_lib, only: String, lf => IOFL_LF
```
