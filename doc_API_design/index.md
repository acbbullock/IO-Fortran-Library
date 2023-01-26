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

The functionality provided by the API is distributed via a handful of [generic interfaces](../lists/procedures.html) and a derived type [String](../type/string.html). The corresponding routines operate on numeric and string data, and take a minimal number of arguments with optional arguments to customize output for particular use cases. This ensures ease of use and flexibility for the end-user, without the need for any specific knowledge regarding the internal implementations. For advanced character handling, the `String` type provides extensive functionality through [type-bound procedures](Ref/String-methods.html). For convenience, a list of [constants](../module/io_fortran_lib.html#variable-nl) are also provided.

The design of [[io_fortran_lib]] is dualistic in nature. That is, for each operation represented by an interface, there exists a transpose operation with a corresponding interface, such as `String ⇆ cast`, `str ⇆ cast`, `to_file ⇆ from_file`, `read_file ⇆ write_file`, `join ⇆ split`, and so on. Each pair of interfaces are designed to mirror each other in their arguments and assumptions of optional arguments.

@note All file I/O (both text and binary) is conducted via unformatted, stream-access reads and writes as introduced in Fortran 2003.
