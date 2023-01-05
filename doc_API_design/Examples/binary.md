---
title: binary file I/O
author: Austin C Bullock
---

The following program demonstrates the use of [to_file](../Ref/to_file.html) and [from_file](../Ref/from_file.html) for writing `real` data to a `.dat` file, reading the file back into the program, and testing for exact equality to ensure that there has been no loss in precision:

```fortran
program main
    use io_fortran_lib, only: to_file, from_file
    implicit none (type,external)

    real, dimension(1000,20) :: x
    real, allocatable, dimension(:,:) :: x_dat

    call random_number(x)

    call to_file(x, file_name='x.dat')

    call from_file('x.dat', into=x_dat, data_shape=shape(x))

    write(*,*) 'x == x_dat : ', all(x == x_dat)
end program main
```

@warning It's crucial that the kind of the array read into has the same `kind` as the array that was written, or else the data will appear as garbage.

@note Tip: The `shape` of an array may be written to file as a `.csv` so that the value of `data_shape` can be read into the program before reading in the main array with the corresponding value.

The following program demonstrates the above tip:

```fortran
program main
    use io_fortran_lib, only: to_file, from_file
    implicit none (type,external)

    real, dimension(1000,20) :: x
    real, allocatable, dimension(:,:) :: x_dat
    integer, allocatable, dimension(:) :: x_shape

    call random_number(x)

    call to_file(x, file_name='x.dat')
    call to_file(shape(x), file_name='x_shape.csv')

    call from_file('x_shape.csv', into=x_shape)
    call from_file('x.dat', into=x_dat, data_shape=x_shape)

    write(*,*) 'x == x_dat : ', all(x == x_dat)
end program main
```
