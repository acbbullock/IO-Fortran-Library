---
title: Thread safety
author: Austin C Bullock
---

The IO-Fortran-Library promotes thread-safety by explicitly enforcing
recursion with the `recursive` keyword on all module procedures.
However, performing I/O in parallel regions has the tendency to result
in unexpected behavior. For instance, inspect the output of the
following program with multiple coarray images:

```fortran
program main
    use io_fortran_lib
    implicit none (type, external)

    call echo("Hello from image "//str(this_image()), file="hello.txt")
end program main
```

This program will result in conflicts as multiple images attempt to
write to the same file concurrently. The proper way to compose this
program is by nesting `echo` inside a `critical` block to enforce
strict thread-safety in the region:

```fortran
program main
    use io_fortran_lib
    implicit none (type, external)

    critical
        call echo("Hello from image "//str(this_image()), file="hello.txt")
    end critical
end program main
```

Another common scenario involves performing I/O on a single image,
which is thread-safe:

```fortran
program main
    use io_fortran_lib
    implicit none (type, external)

    if ( this_image() == 1 ) then
        call echo("Hello from image "//str(this_image()), file="hello.txt")
    end if
end program main
```
