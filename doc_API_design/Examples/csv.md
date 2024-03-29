---
title: Handling csv files
author: Austin C Bullock
---

## Basic csv file I/O

The routines [to_file](../Ref/to_file.html) and
[from_file](../Ref/from_file.html) are the preferred method for
handling I/O for numeric data of uniform type and format. Typical use
cases involve writing whole arrays to file and reading files of uniform
type and format directly into an array of numeric type.

The following program demonstrates the use of `to_file` and `from_file`
for writing an array of `real` data to a csv file in each possible
[text format](../UserInfo/text-fmts.html), reading each file back into
the program, and testing for exact equality to ensure that there has
been no loss in precision:

```fortran
program main
    use io_fortran_lib, only: to_file, from_file
    implicit none (type, external)

    real :: x(1000,20)
    real, allocatable :: x_e(:,:), x_f(:,:), x_z(:,:)

    call random_number(x)

    call to_file(x, file="x_e.csv", header=["x"], fmt="e")
    call to_file(x, file="x_f.csv", header=["x"], fmt="f")
    call to_file(x, file="x_z.csv", header=["x"], fmt="z")

    call from_file("x_e.csv", into=x_e, header=.true., fmt="e")
    call from_file("x_f.csv", into=x_f, header=.true., fmt="f")
    call from_file("x_z.csv", into=x_z, header=.true., fmt="z")

    write(*,*) "x == x_e : ", all(x == x_e)
    write(*,*) "x == x_f : ", all(x == x_f)
    write(*,*) "x == x_z : ", all(x == x_z)
end program main
```

Here we use the simple header `header=["x"]`, which produces a header
of the form:

```text
x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20
```

Note that the default value for `header` when reading is `.false.`. If
a header is actually present, the output array will have an extra row
with default initialized values.

@warning Default text format for writing and reading is `"e"` for data
of type `real` or `complex`, and `"i"` for data of type `integer`.
Attempting to read data with a format that does not correspond to the
format in the file may result in an I/O syntax error.

@note Reading into arrays of a different `kind` than the array that was
written is a type conversion, and will fail the equality test.

## Advanced csv file I/O

The routines [write_file](../Ref/String-methods.html#write_file) and
[read_file](../Ref/String-methods.html#read_file) are the preferred
method for handling I/O for general text files. Typical use cases
involve writing cell arrays of type `String` to delimited files, and
reading delimited files into a cell array. For reading and writing
non-delimited text files, one would use `read_file` without the
`cell_array` argument and [echo](../Ref/echo.html).

The same program as above can be recast in an object-oriented fashion
that is generalizable for processing data of mixed type:

```fortran
program main
    use io_fortran_lib, only: String, str, cast
    implicit none (type, external)

    type(String) :: csv
    type(String), allocatable :: cells(:,:)

    real :: x(1000,20)
    real, allocatable :: x_e(:,:), x_f(:,:), x_z(:,:)
    integer :: i

    call random_number(x); allocate( cells(1001,20) ); cells(1,:) = [(String("x"//str(i)), i = 1, 20)]

    call cast(x, into=cells(2:,:), fmt="e"); call csv%write_file(cells, file="x_e.csv")
    call cast(x, into=cells(2:,:), fmt="f"); call csv%write_file(cells, file="x_f.csv")
    call cast(x, into=cells(2:,:), fmt="z"); call csv%write_file(cells, file="x_z.csv")

    allocate( x_e, x_f, x_z, mold=x )
    call csv%read_file("x_e.csv", cell_array=cells); call cells(2:,:)%cast(into=x_e, fmt="e")
    call csv%read_file("x_f.csv", cell_array=cells); call cells(2:,:)%cast(into=x_f, fmt="f")
    call csv%read_file("x_z.csv", cell_array=cells); call cells(2:,:)%cast(into=x_z, fmt="z")

    write(*,*) "x == x_e : ", all(x == x_e)
    write(*,*) "x == x_f : ", all(x == x_f)
    write(*,*) "x == x_z : ", all(x == x_z)
end program main
```

Here, we construct the same header as before with the implicit loop

```fortran
cells(1,:) = [(String("x"//str(i)), i = 1, 20)]
```

and then construct the remainder of the cell array `cells` with an
elemental cast `call cast(x, into=cells(2:,:), fmt)` before writing the
array to a csv file. We then read the files back into `csv` and output
the cells into `cells` (which is reallocated internally). Note that
when casting the cell data into numeric arrays, we must pre-allocate
the output arrays due to the restrictions on `intent(out)` arguments
of `elemental` procedures.

@note One may optionally specify the arguments of `row_separator` and
`column_separator` when writing and reading text files with
[write_file](../Ref/String-methods.html#write_file) and
[read_file](../Ref/String-methods.html#read_file). The default
`row_separator` is `LF`, and the default `column_separator` is `","`.

@warning When reading files with `CRLF` line endings, be sure to
specify `row_separator=CR//LF` or pre-process the file to `LF`. Trying
to cast data with a hidden `CR` character may result in an I/O syntax
error.

For a slightly more advanced example, consider the following program
to read in and cast the data of mixed type contained in the example
data `/data/ancestry_comp.csv`:

```fortran
program main
    use, intrinsic :: iso_fortran_env, only: int8, int64
    use io_fortran_lib, only: String, cast, CR, LF, operator(+), operator(-)
    implicit none (type, external)

    type(String) :: csv
    type(String), allocatable :: cells(:,:)

    integer(int8), allocatable :: copy(:), chromosome(:)
    integer(int64), allocatable :: start_point(:), end_point(:)
    integer :: nrows

    call csv%read_file("./data/ancestry_comp.csv", cell_array=cells, row_separator=CR+LF)
    write(*,*) csv

    nrows = size(cells, dim=1) - 1

    allocate( copy(nrows), chromosome(nrows), start_point(nrows), end_point(nrows) )

    call cells(2:,2)%cast(into=copy)
    call cast(cells(2:,3)%replace("X","0") - "chr", into=chromosome)
    call cells(2:,4)%cast(into=start_point)
    call cells(2:,5)%cast(into=end_point)
end program main
```

Here, `file` is a relative path, and we use the extended operator `+`
for [concatenation](../Ref/operators.html#concatenation) in the
`character` expression `CR+LF`. We then allocate data arrays and cast
each column into respective arrays. Note that we must use
[cast](../Ref/cast.html) as a standalone subroutine to accept the
`String`-valued expression

```fortran
cells(2:,3)%replace("X","0") - "chr"
```

which first calls [replace](../Ref/String-methods.html#replace) to
return an elemental copy of the given cells in which all instances of
`X` have been replaced with `0`, and then calls the
[excision operator](../Ref/operators.html#excision) `-` to remove all
instances of `"chr"` elementally. The output of the `String` expression
contains numeric characters only, which are then casted to the array
`chromosome`.

@note In general, for other
[text file extensions](../UserInfo/file-ext.html), one would specify
the `column_separator` associated with the given file. For instance,
one would specify `column_separator=TAB` for the file formats `.bed`,
`.gff`, and `.gtf`.
