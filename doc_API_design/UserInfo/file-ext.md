---
title: File extensions
author: Austin C Bullock
---

When writing to file or reading from file, the file's format comes from
the `format` argument if present, and otherwise from its extension.
Extensions are not case sensitive.

The `format` argument may be `"text"`, `"binary"`, `"npy"` or `"npz"`, and allows
a file to have any name, as in:

```fortran
call to_file(x, file="data.out", format="text")
```

Without `format`, a valid file extension must be present.

The following are valid text file extensions:

```fortran
character(len=3), parameter :: TEXT_EXT(*) = [ "csv", "tsv", & ! Allowed text extensions
                                               "txt", "log", &
                                               "rtf", "odm", &
                                               "odt", "ods", &
                                               "odf", "xls", &
                                               "doc", "org", &
                                               "dbf", "bed", &
                                               "gff", "gtf"  ]
```

The following are valid binary file extensions:

```fortran
character(len=3), parameter :: BINARY_EXT(*) = [ "dat", "bin" ] ! Allowed binary extensions
```

The `npy` extension, or `format="npy"`, selects a NumPy `.npy` file, and
the `npz` extension, or `format="npz"`, a NumPy `.npz` archive.

### Delimiters

Without `delim` (or `column_separator` for `read_file` and `write_file`),
a `.tsv` file is separated by tabs. Other text files are separated by
commas, or by semicolons in the `"EU"` locale, whose decimal mark is a
comma. An explicit delimiter always wins:

```fortran
call to_file(x, file="x.tsv")                ! Tab-separated
call to_file(x, file="x.csv")                ! Comma-separated
call to_file(x, file="x.csv", locale="EU")   ! Semicolon-separated, with decimal commas
call to_file(x, file="x.tsv", delim=";")     ! Semicolon-separated
```

### NumPy `.npy` files

A `.npy` file records the type, shape and memory order of its array in a
header before the data, so it can be read without `data_shape`, and
reading it into an array of the wrong kind or rank is an error rather
than garbage. NumPy reads it with `numpy.load`, and `numpy.save` writes
files this library reads:

```fortran
call to_file(x, file="x.npy")       ! In Python: x = numpy.load("x.npy")
call from_file("y.npy", into=y)     ! From numpy.save("y.npy", y)
```

`to_file` writes version 1.0 of the format, in Fortran order and the
machine's byte order. `from_file` reads versions 1.0 to 3.0, in Fortran
or C order (NumPy's default) and in either byte order. Element `y(i,j)`
is `y[i-1, j-1]` in NumPy either way.

The supported types are `integer` of kinds `int8` to `int64`, `real` of
kinds `real32` and `real64`, `complex` of the same kinds, and `logical`,
which is stored as NumPy's one-byte `bool`. NumPy has
no type for `real128`: its `float128` is 80-bit extended precision on
x86, so `real128` and `complex(real128)` data give an `IOFL_ARG_ERR`.

### NumPy `.npz` archives

A `.npz` file is a ZIP archive of `.npy` files, one for each array, as
`numpy.savez` writes it. `to_file` and `from_file` take the array's name
in `name`, which is the name NumPy uses:

```fortran
call to_file(x, file="data.npz", name="x")      ! Creates data.npz, or adds x to it
call to_file(y, file="data.npz", name="y")      ! In Python: d = numpy.load("data.npz"); d["x"], d["y"]
call from_file("data.npz", into=x, name="x")    ! From numpy.savez("data.npz", x=x, y=y)
call npz_names("data.npz", names)               ! names is ["x", "y"], as a String array
```

Each array is a `.npy` file, with the same types and orders as above.
Writing an array adds it to the archive, and writing a name that is
already there replaces that array, so the archive is never rewritten as
a whole. To start a new archive, delete the old file first. A file that
exists but isn't a ZIP archive is left unchanged, with an
`IOFL_WRITE_ERR`. As with appending to any ZIP archive, an error partway
through writing, such as a full disk, can leave the archive damaged.
Archives of 4 GiB and more use ZIP64 records, as NumPy's do.

Only uncompressed archives can be read: an array in an archive from
`numpy.savez_compressed` gives an `IOFL_READ_ERR` that says so, though
[npz_names](../Ref/npz_names.html) can still list it.

A raw binary file (`.dat`, `.bin`) holds only the values, so reading it
needs `data_shape` and the right kind. `.npy` and `.npz` are the better
choice for data that leaves the program.

### Which routines accept which formats

The routines [to_file](../Ref/to_file.html) and
[from_file](../Ref/from_file.html) will detect the file extension used
and direct whether to write/read a text file, a binary file, a `.npy`
file or a `.npz` archive. The routines [echo](../Ref/echo.html),
[write_file](../Ref/String-methods.html#write_file), and
[read_file](../Ref/String-methods.html#read_file) accept only text
extensions. Other file extensions may be eligible for addition.
