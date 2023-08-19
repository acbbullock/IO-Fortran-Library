---
title: Handling dat files
author: Austin C Bullock
---

## Binary file I/O

The routines [to_file](../Ref/to_file.html) and [from_file](../Ref/from_file.html) are used for writing numeric arrays to binary files with the extension `.dat` or `.bin`.

The following program demonstrates the use of `to_file` and `from_file` for writing `real` data of rank `5` to a `.dat` file, reading the file back into the program, and testing for exact equality to ensure that there has been no loss in precision:

```fortran
program main
    use io_fortran_lib, only: to_file, from_file
    implicit none (type,external)

    real, dimension(20,20,20,20,20) :: x
    real, allocatable, dimension(:,:,:,:,:) :: x_dat

    call random_number(x)

    call to_file(x, file="x.dat")

    call from_file("x.dat", into=x_dat, data_shape=shape(x))

    write(*,*) "x == x_dat : ", all(x == x_dat)
end program main
```

@warning Reading into arrays of a different `kind` than the array that was written will invalidate the data. Always make sure the `kind` is matching for binary I/O.

TIP: The shape of an array may be written to a csv file so that the value of `data_shape` can be read into the program before reading in the main array with the corresponding value. The following program demonstrates the above tip:

```fortran
program main
    use io_fortran_lib, only: to_file, from_file
    implicit none (type,external)

    real, dimension(20,20,20,20,20) :: x
    real, allocatable, dimension(:,:,:,:,:) :: x_dat
    integer, allocatable, dimension(:) :: x_shape

    call random_number(x)

    call to_file(x, file="x.dat")
    call to_file(shape(x), file="x_shape.csv")

    call from_file("x_shape.csv", into=x_shape)
    call from_file("x.dat", into=x_dat, data_shape=x_shape)

    write(*,*) "x == x_dat : ", all(x == x_dat)
end program main
```
