---
title: csv file IO
author: Austin C Bullock
---

The following program demonstrates the use of [to_file](../Ref/to_file.html) and [from_file](../Ref/from_file.html) for writing `real` data to a `.csv` file in each possible [text format](../UserInfo/text-fmts.html), reading each file back into the program, and testing for exact equality to ensure that there has been no loss in precision:

```fortran
program main
    use io_fortran_lib, only: to_file, from_file
    implicit none (type,external)

    real, dimension(1000,20) :: x
    real, allocatable, dimension(:,:) :: x_e, x_f, x_z

    call random_number(x)

    call to_file(x, file_name='x_e.csv', header=['x'], fmt='e')
    call to_file(x, file_name='x_f.csv', header=['x'], fmt='f')
    call to_file(x, file_name='x_z.csv', header=['x'], fmt='z')

    call from_file('x_e.csv', into=x_e, header=.true., fmt='e')
    call from_file('x_f.csv', into=x_f, header=.true., fmt='f')
    call from_file('x_z.csv', into=x_z, header=.true., fmt='z')

    write(*,*) 'x == x_e : ', all(x == x_e)
    write(*,*) 'x == x_f : ', all(x == x_f)
    write(*,*) 'x == x_z : ', all(x == x_z)
end program main
```

@warning Default text format for writing and reading is `'e'` for data of type `real` or `complex`, and `'i'` for data of type `integer`. Attempting to read data with a format that does not correspond to the format in the file will result in an I/O syntax error.

@warning Default value for `header` when reading is `.false.`. If a header is actually present, the output array will have an extra row with default initialized values.
