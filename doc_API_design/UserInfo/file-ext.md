---
title: File extensions
author: Austin C Bullock
---

When writing to file or reading from file with [echo](../Ref/echo.html), [to_file](../Ref/to_file.html), [from_file](../Ref/from_file.html), or [read_file](../Ref/string-methods.html#read_file), a valid file extension must be present.

The following are valid text file extensions:

```fortran
text_ext = [ 'csv', 'txt', 'ods', 'odf', 'odm', 'odt', 'xls', 'doc', 'log', 'rtf', 'org', 'dbf' ]
```

The following are valid binary file extensions:

```fortran
binary_ext = [ 'dat', 'bin' ]
```

The routines [to_file](../Ref/to_file.html) and [from_file](../Ref/from_file.html) will detect the file extension used and direct whether to write/read a text file or a binary file.
