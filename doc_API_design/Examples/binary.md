---
title: Handling binary files
author: Austin C Bullock
---

## Binary file I/O

The routines [to_file](../Ref/to_file.html) and
[from_file](../Ref/from_file.html) are used for writing numeric arrays
to binary files with the extension `.dat` or `.bin`.

The following program demonstrates the use of `to_file` and `from_file`
for writing `real` data of rank `5` to a `.dat` file, reading the file
back into the program, and testing for exact equality to ensure that
there has been no loss in precision:

```fortran
{!example/binary_dat.f90!}
```

@warning Reading into arrays of a different `kind` than the array that
was written will invalidate the data. Always make sure the `kind` is
matching for binary I/O.

TIP: The shape of an array may be written to a csv file so that the
value of `data_shape` can be read into the program before reading in
the main array with the corresponding value. The following program
demonstrates the above tip:

```fortran
{!example/binary_shape_file.f90!}
```

## NumPy `.npy` files

A `.npy` file records the type, shape and memory order of its array, so
reading it needs no `data_shape`, and reading it into an array of the
wrong kind or rank is an error rather than garbage. The same program
with a `.npy` file:

```fortran
{!example/binary_npy.f90!}
```

In Python, `numpy.load("x.npy")` gives the same array, with element
`x(i,j,k,l,m)` at `x[i-1, j-1, k-1, l-1, m-1]`. See
[File extensions](../UserInfo/file-ext.html) for the details.

## NumPy `.npz` archives

A `.npz` archive holds several arrays, each under a name. Writing an
array adds it to the archive, and `npz_names` lists what is there:

```fortran
{!example/binary_npz.f90!}
```

In Python, `numpy.load("arrays.npz")` gives a dictionary-like object
with `d["x"]` and `d["counts"]`, and running the program again replaces
both arrays. See [File extensions](../UserInfo/file-ext.html#numpy-npz-archives)
for the details.
