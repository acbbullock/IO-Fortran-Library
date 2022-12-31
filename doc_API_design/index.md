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

The functionality provided by the API is distributed via a handful of [generic interfaces](../lists/procedures.html) for routines that operate on numeric data and which take a minimal number of arguments with optional arguments to customize output for particular use cases. This ensures ease of use and flexibility for the end-user, without the need for any specific knowledge regarding the internal implementations. Object-oriented designs for file I/O are avoided, as these incur additional overhead for both users and programs, without much additional functionality.

For convenience, two simple data structures are also provided:

* [String](../type/string.html)
* [nl](../module/io_fortran_lib.html#variable-nl)

These data structures are not required to use any of the functionality provided by the interfaces.

@note All external I/O (both text and binary) is conducted as unformatted, stream-access reads and writes as introduced in Fortran 2003, which is much more flexible and typically must faster than record-based I/O.
