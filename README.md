# API Specification

The IO Fortran Library is a Fortran module `io_fortran_lib` which provides high level convenience routines for doing internal and external IO. In particular, the module provides a handful of generic interfaces for performing string-based and array-based IO that are useful for recording program data, reading data into programs, and for writing formatted logs and output. Simply include the source file `io_fortran_lib.f90` into the source directory of your Fortran project and `use` the module in your program units to access the routines.

The module is fully self-contained, with no external dependencies, and is written to be portable and compliant to the Fortran 2018 standard such that no special extensions or compiler options should be required. All generic procedures which take numeric arguments support all of the standard kinds provided by the intrinsic `iso_fortran_env` module, including `int8`, `int16`, `int32`, `int64`, `real32`, `real64`, and `real128`. All array-based routines additionally support up to rank 15.

## String IO

1. `interface str`: A function for representing a number as a string.
2. `interface echo`: A subroutine for recording a string to an external file.

### `pure function str`

For `x` of type `integer`:

```fortran
result = str(x, fmt)
```

* `fmt` is `optional`, may be one of `['i', 'z']` where `'i'` is default integer format and `'z'` is hexadecimal format.

For `x` of type `real`:

```fortran
result = str(x, locale, fmt, decimals)
```

* `locale` is `optional`, may be one of `['US', 'EU']` where `'US'` is default.
* `fmt` is `optional`, may be one of `['e', 'f', 'z']` where `'e'` is default exponential format, `'f'` is decimal format, and `'z'` is hexadecimal format.
* `decimals` is `optional` and of type `integer(int32)`, which prescribes the number of decimals places to write.

For `x` of type `complex`:

```fortran
result = str(x, locale, fmt, decimals, im)
```

* `locale` is `optional`, may be one of `['US', 'EU']` where `'US'` is default.
* `fmt` is `optional`, may be one of `['e', 'f', 'z']` where `'e'` is default exponential format, `'f'` is decimal format, and `'z'` is hexadecimal format.
* `decimals` is `optional` and of type `integer(int32)`, which prescribes the number of decimals places to write.
* `im` is `optional` and of type `character(len=*)`, which prescribes the imaginary unit to write.

By default, the function `str` will write a real or complex number using a number of significant digits required in the worst case for a lossless round-trip conversion starting with the internal model representation of `x`. The `decimals` argument can be used to reduce the number of digits to the right of the radix point for use in cases where not all digits are needed, such as in prints or logging. The number of digits cannot be reduced to the left of the radix point, nor can the number of digits be extended beyond that given by precision. Precision losses are most likely to occur with the `'f'` format, unlikely with the `'e'` format, and will not happen with the hexadecimal format `'z'`.

The `locale` determines the form of the decimal, such as `2.45` or `2,45` produced by the write. The imaginary unit `im` determines the form of a complex number. By default complex numbers will be written as ordered pairs, e.g. `(2.45,3.45)`. If `im='j'`, then `2.45+3.45j` will be written. Any string is allowed for the complex unit to allow for compatibility with other languages, such as `2.45+3.45*1i` with `im='*1i'` for MATLAB.

### `impure subroutine echo`

```fortran
call echo(string, file_name, append)
```

* `string` is of type `character(len=*)`, which prescribes the string to write.
* `file_name` is of type `character(len=*)`, which prescribes the file path to write to.
* `append` is `optional` and of type `logical`, which is `.true.` by default and prescribes whether to append or to overwrite the file.
