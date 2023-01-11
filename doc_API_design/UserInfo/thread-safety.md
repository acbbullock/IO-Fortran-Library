---
title: Thread safety
author: Austin C Bullock
---

The IO-Fortran-Library explicitly enforces recursion with the `recursive` keyword on all module procedures. However, even with recursion enforced, some programs may not operate as expected when performing I/O in parallel regions. For instance, inspect the output of the following program with multiple coarray images:

```fortran
program main
    use io_fortran_lib
    implicit none (type,external)

    call echo('Hello from image '//str(this_image()), file_name='hello.txt')
end program main
```

@warning Even with recursion, the above program will likely not behave as expected.

The proper way to compose the above program is by nesting `echo` inside a `critical` block to enforce strict thread-safety in the region:

```fortran
program main
    use io_fortran_lib
    implicit none (type,external)

    critical
        call echo('Hello from image '//str(this_image()), file_name='hello.txt')
    end critical
end program main
```

Another common scenario involves performing I/O on a single image, which is thread-safe:

```fortran
program main
    use io_fortran_lib
    implicit none (type,external)

    if ( this_image() == 1 ) then
        call echo('Hello from image '//str(this_image()), file_name='hello.txt')
    end if
end program main
```
