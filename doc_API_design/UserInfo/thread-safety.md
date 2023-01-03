---
title: Thread safety
author: Austin C Bullock
---

As of Fortran 2018, all functions and subroutines are recursive by default without having to specify the `recursive` keyword. However, at the time of writing, the implementation of this behavior remains unfinished by major compilers, and this may result in catastrophic race conditions when multiple threads attempt to write to the same file in a parallel region. The [IO Fortran Library](../../index.html) avoids this by explicitly enforcing recursion with the `recursive` keyword on all module procedures. However, even with recursion enforced, some programs may not operate as expected when performing I/O in parallel regions. For instance, inspect the output of the following program with multiple coarray images:

```fortran
program main
    use io_fortran_lib
    implicit none (type,external)

    call echo(string='Hello from image '//str(this_image()), file_name='hello.txt')
end program main
```

@warning Even with recursion, the above program will likely not behave as expected.

The proper way to compose the above program is by nesting `echo` inside a `critical` block to enforce strict thread-safety in the region:

```fortran
program main
    use io_fortran_lib
    implicit none (type,external)

    critical
        call echo(string='Hello from image '//str(this_image()), file_name='hello.txt')
    end critical
end program main
```

Another common scenario involves performing I/O on a single image, which is thread-safe:

```fortran
program main
    use io_fortran_lib
    implicit none (type,external)

    if ( this_image() == 1 ) then
        call echo(string='Hello from image '//str(this_image()), file_name='hello.txt')
    end if
end program main
```
