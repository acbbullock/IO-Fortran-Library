---
title: File extensions
author: Austin C Bullock
---

When writing to file or reading from file, a valid file extension must
be present.

The following are valid text file extensions:

```fortran
character(len=3), parameter :: TEXT_EXT(*) = [ "csv", "txt", & ! Allowed text extensions
                                               "log", "rtf", &
                                               "odm", "odt", &
                                               "ods", "odf", &
                                               "xls", "doc", &
                                               "org", "dbf", &
                                               "bed", "gff", &
                                               "gtf"         ]
```

The following are valid binary file extensions:

```fortran
character(len=3), parameter :: BINARY_EXT(*) = [ "dat", "bin" ] ! Allowed binary extensions
```

The routines [to_file](../Ref/to_file.html) and
[from_file](../Ref/from_file.html) will detect the file extension used
and direct whether to write/read a text file or a binary file. The
routines [echo](../Ref/echo.html),
[write_file](../Ref/String-methods.html#write_file), and
[read_file](../Ref/String-methods.html#read_file) accept only text
extensions. Other file extensions may be eligible for addition.
