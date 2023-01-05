---
title: from_file
author: Austin C Bullock
---

## [interface from_file](../../interface/from_file.html)

*Description*: Subroutine for reading an external file of uniform data type into an array.

For reading textual data into an array `into` of rank `1` or `2` and of type `integer`:

```fortran
call from_file(file_name, into, header, fmt)
```

* `file_name` is of type `character(len=*)`
* `header` is `optional` and of type `logical`
* `fmt` is `optional`, may be one of `INT_FMTS`

For reading textual data into an array `into` of rank `1` or `2` and of type `real`:

```fortran
call from_file(file_name, into, header, locale, fmt)
```

* `file_name` is of type `character(len=*)`
* `header` is `optional` and of type `logical`
* `locale` is `optional`, may be one of `LOCALES`
* `fmt` is `optional`, may be one of `REAL_FMTS`

For reading textual data into an array `into` of rank `1` or `2` and of type `complex`:

```fortran
call from_file(file_name, into, header, locale, fmt, im)
```

* `file_name` is of type `character(len=*)`
* `header` is `optional` and of type `logical`
* `locale` is `optional`, may be one of `LOCALES`
* `fmt` is `optional`, may be one of `REAL_FMTS`
* `im` is `optional` and of type `character(len=*)`

For reading binary data into an array `into` of any rank `1`-`15` and of type `integer`, `real`, `complex`:

```fortran
call from_file(file_name, into, data_shape)
```

* `file_name` is of type `character(len=*)`
* `data_shape` is of type `integer, dimension(:)`

@warning `file_name` may be a relative path, but absolute paths are not guaranteed to work on every platform. If `file_name` does not exist, `from_file` will issue an `error stop`.

@warning The actual argument of `into` must be `allocatable`, and will lose its allocation status upon passing into `from_file` if already allocated. As a result, `from_file` does not allow reading into sections of already allocated arrays.

@warning When reading binary data, `data_shape` must be present and its size must equal the rank of `into` for the read to be valid, or else `from_file` will issue an `error stop`.

### Optional Arguments

Header (default is `.false.`): specifies whether a header line is present.

Locales (default is `'US'`):

```fortran
LOCALES = [ 'US', 'EU' ]
```

Integer formats (default is `'i'`):

```fortran
INT_FMTS = [ 'i', 'z' ]
```

Real formats (default is `'e'`):

```fortran
REAL_FMTS = [ 'e', 'f', 'z' ]
```

Imaginary unit: `im` specifies the form of a complex number. If not present, `complex` numbers will be assumed to be written as ordered pairs, e.g. `(2.45,3.45)`.
