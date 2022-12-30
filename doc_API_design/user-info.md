---
title: Important User Information
author: Austin C Bullock
---

## Text formats for numeric data

When writing `integer` data as strings with `str` or `to_file`, any of the following text formats may be used:

```fortran
int_fmts = [ 'i', 'z' ]
```

* `'i'`: integer format (default), e.g. `123456`
* `'z'`: hexadecimal format, e.g. `1E240`

When writing floating point numbers of type `real` or `complex` as strings with `str` or `to_file`, any of the following text formats may be used:

```fortran
real_fmts = [ 'e', 'f', 'z' ]
```

* `'e'`: normalized exponential format (default), e.g. `1.2345678901234567E+005`
* `'f'`: decimal format, e.g. `123456.78901234567`
* `'z'`: hexadecimal format, e.g. `40FE240C9FCB68CD`

@note The `'z'` hexadecimal format is an integer format and may be used for `integer`, `real`, or `complex` data. Floating point numbers are interpreted bit-wise as integers when written with the `'z'` format, preventing any loss of precision in a round-trip conversion. This format is preferred for portable data transfers for which precision losses are intolerable.

## Locale formats for numeric data

When writing floating point numbers of type `real` or `complex` as strings with `str` or `to_file`, any of the following locales may be used:

```fortran
locales = [ 'US', 'EU' ]
```

* `'US'`: US decimal with default comma delimiter (default), e.g. `1.23456789,0.12345678`
* `'EU'`: EU decimal with default semicolon delimiter, e.g. `1,23456789;0,12345678`

## File extensions

When writing to file or reading from file, a valid file extension must be present. The following are valid text file extensions for strings and arrays of rank `1` or `2`:

```fortran
text_ext = [ 'csv', 'txt', 'ods', 'odf', 'odm', 'odt', 'xls', 'doc', 'log', 'rtf', 'org', 'dbf' ]
```

For arrays of any rank `1`-`15`, the following binary file extensions may be used:

```fortran
binary_ext = [ 'dat', 'bin' ]
```

The routines `to_file` and `from_file` will detect the file extension used and direct whether to write/read a text file or a binary file.

## Compiler-dependent behavior

When writing text files, it's important to note that some compilers implement extensions to the Fortran standard by default with regards to character array literals. For example, the array literal

```fortran
header = [ 'firstcol', 'secondcol' ]
```

is not standard Fortran 2018 since the strings in the array do not have identical length. Some compilers will accept this and others will not. If required, simply add padding spaces to the left or right of each string to match the length of the longest element. These padding spaces will not be present in the output file.

@note Some compilers may allocate strings dynamically on the stack. When reading very large text files, this may result in a stack overflow or segmentation fault unless the compiler is directed to allocate everything on the heap. For example, one would specify `-heap-arrays 0` for the Intel Fortran compiler on Linux (`/heap-arrays:0` on Windows).
