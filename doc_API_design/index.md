---
title: API Design
author: Austin C Bullock
ordered_subpage: UserInfo
ordered_subpage: Ref
ordered_subpage: Examples
---

The API is made available for use through the following statement:

```fortran
use io_fortran_lib
```

which may be placed at the start of any compilation unit, immediately following the `program`, `module`, `function`, or `subroutine` statement, and before any `implicit` statement.

The functionality provided by the API is distributed via a handful of [generic interfaces](../lists/procedures.html) and a derived type [String](../type/string.html). The corresponding routines operate on numeric and character data, and take a minimal number of arguments with optional arguments to customize output for particular use cases. This ensures ease of use and flexibility for the end-user, without the need for any specific knowledge regarding the internal implementations. For advanced character handling, the `String` type provides extensive functionality for internal character manipulations through [type-bound procedures](Ref/string-methods.html).

For convenience, a constant `parameter` is also provided:

* [nl](../module/io_fortran_lib.html#variable-nl)

@note All external I/O (both text and binary) is conducted as unformatted, stream-access reads and writes as introduced in Fortran 2003, which is much more flexible and typically must faster than record-based I/O.
