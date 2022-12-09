# IO Fortran Library

The IO Fortran Library is a Fortran module `io_fortran_lib` which provides high level routines for doing internal and external IO. In particular, the module provides a handful of generic interfaces for performing string-based and array-based IO that are useful for recording program data, reading data into programs, and for writing formatted logs and output. To use `io_fortran_lib` with your [fpm](https://github.com/fortran-lang/fpm) project, add the following lines to your `fpm.toml` file and `use` the module in your program units to access the routines:

```toml
[dependencies]
IO-Fortran-Library = { git="https://github.com/acbbullock/IO-Fortran-Library", branch="main" }
```

The module is fully self-contained, with no external dependencies, and is written to be portable and compliant to the Fortran 2018 standard such that no special extensions or compiler options should be required. All generic procedures which take numeric arguments support all of the standard kinds provided by the intrinsic `iso_fortran_env` module, including `int8`, `int16`, `int32`, `int64`, `real32`, `real64`, and `real128`. All array-based routines additionally support up to rank 15.

The following is a complete list of publicly accessible interfaces:

1. [`interface str`](https://github.com/acbbullock/IO-Fortran-Library#pure-function-str): A function for representing a number as a string.
2. [`interface echo`](https://github.com/acbbullock/IO-Fortran-Library#impure-subroutine-echo): A subroutine for recording a string to an external file.
3. [`interface to_file`](https://github.com/acbbullock/IO-Fortran-Library#impure-subroutine-to_file): A subroutine for recording an array to an external file.
4. [`interface from_file`](https://github.com/acbbullock/IO-Fortran-Library#impure-subroutine-from_file): A subroutine for reading an external file into an array.
5. [`interface aprint`](https://github.com/acbbullock/IO-Fortran-Library#impure-subroutine-aprint): A subroutine for printing array sections to stdout.
6. [`type String`](https://github.com/acbbullock/IO-Fortran-Library#type-string): A wrapper type for an allocatable string.
7. [`parameter nl`](https://github.com/acbbullock/IO-Fortran-Library#parameter-nl): The new line character constant.

### **IMPORTANT**

When writing or reading an array, a valid file extension must be present. For arrays of rank `1` or `2`, any of the following text file extensions may be used:

```fortran
text_ext = [ 'csv', 'txt', 'ods', 'odf', 'odm', 'odt', 'xls', 'doc', 'log', 'rtf', 'org', 'dbf' ]
```

For arrays of any rank, any of the following binary file extensions may be used:

```fortran
binary_ext = [ 'dat', 'bin' ]
```

The routines `to_file` and `from_file` will detect the file extension used and direct whether to write/read a text file or a binary file.

Some compilers implement extensions to the Fortran standard by default with regards to character array literals. For example, the array literal

```fortran
header = [ 'firstcol', 'secondcol' ]
```

is not standard Fortran 2018 since the strings in the array do not have identical length. Some compilers will accept this and others will not. If required, simply add padding spaces to the left or right of each string to match the length of the longest element. These padding spaces will not be present in the output file.

## `pure function str`

For `x` of type `integer`, `real`, `complex`:

```fortran
result = str(x, fmt)
```

```fortran
result = str(x, locale, fmt, decimals)
```

```fortran
result = str(x, locale, fmt, decimals, im)
```

* `locale` is `optional`, may be one of `['US', 'EU']` where `'US'` is default, which prescribes the form of the decimal.
* `fmt` is `optional`, may be one of `['i', 'z']` if `x` is `integer` or one of `['e', 'f', 'z']` if `x` is `real` or `complex`, which prescribes the text format to write to.
* `decimals` is `optional` and of type `int32`, which prescribes the number of decimals places to write.
* `im` is `optional` and of type `character(len=*)`, which prescribes the imaginary unit to write.

By default, the function `str` will write a real or complex number using a number of significant digits required in the worst case for a lossless round-trip conversion starting with the internal model representation of `x`. The `decimals` argument can be used to reduce the number of digits to the right of the radix point for use in cases where not all digits are needed, such as in prints or logging. The number of digits cannot be reduced to the left of the radix point, nor can the number of digits be extended beyond that allowed by precision. Precision losses are most likely to occur with the `'f'` decimal format, unlikely with the `'e'` exponential format, and will not happen with the `'z'` hexadecimal format. By default, integer strings will use the `'i'` format and real or complex strings will use the `'e'` format.

The `locale` determines the form of the decimal, such as `2.45` or `2,45` produced by the write, choosing the former by default. The imaginary unit `im` determines the form of a complex number. By default complex numbers will be written as ordered pairs, e.g. `(2.45,3.45)`. If `im='j'`, then `2.45+3.45j` will be written. Any string is allowed for the complex unit to allow for compatibility with other languages, such as `2.45+3.45*1i` with `im='*1i'` for MATLAB.

## `impure subroutine echo`

```fortran
call echo(string, file_name, append)
```

* `string` is of type `character(len=*)`, which prescribes the string to write.
* `file_name` is of type `character(len=*)`, which prescribes the file path to write to.
* `append` is `optional` and of type `logical`, which is `.true.` by default and prescribes whether to append or to overwrite the file.

By default, `echo` will insert a new line at the end of the input string, but `string` itself may contain new line characters on the part of the programmer. Thus the `string` will be written to file literally, with no modifications.

## `impure subroutine to_file`

For `x` of rank `1` and of type `integer`, `real`, `complex`:

```fortran
call to_file(x, file_name, header, dim, delim, fmt)
```

```fortran
call to_file(x, file_name, header, dim, locale, delim, fmt, decimals)
```

```fortran
call to_file(x, file_name, header, dim, locale, delim, fmt, decimals, im)
```

* `file_name` is of type `character(len=*)`, which prescribes the file path to write to.
* `header` is `optional` and of type `character(len=*), dimension(:)`, which prescribes the header of a text file.
* `dim` is `optional` and of type `int32`, which prescribes whether to write the data as a column (`dim=1`) or as a row (`dim=2`) of a text file.
* `locale` is `optional`, may be one of `['US', 'EU']` where `'US'` is default, which prescribes the form of the decimal.
* `delim` is `optional` and of type `character(len=*)`, which prescribes the delimiter to use for a text file.
* `fmt` is `optional`, may be one of `['i', 'z']` if `x` is `integer` or one of `['e', 'f', 'z']` if `x` is `real` or `complex`, which prescribes the text format to write to.
* `decimals` is `optional` and of type `int32`, which prescribes the number of decimals places to write for a text file and qualifying format.
* `im` is `optional` and of type `character(len=*)`, which prescribes the imaginary unit to write to a text file if `x` is `complex`.

If writing to binary, all optional arguments are ignored. The delimiter argument is honored only if `dim=2`, and is not applicable if `dim=1`. It is always recommended to omit the delimiter argument for default unless a custom delimiter is really necessary. The `header` is in the form of a character array literal, and must be of size `1` or `size(x)`, or will be ignored. If the `header` has size `size(x)` then `dim=2` is the only possibility. The `locale`, `fmt`, `decimals`, and `im` arguments are precisely as they are for the `str` function, and are honored in precisely the same ways. If `locale='US'`, the default delimiter is `','`. If `locale='EU'`, the default delimiter is `';'`.

For `x` of rank `2` and of type `integer`, `real`, `complex`:

```fortran
call to_file(x, file_name, header, delim, fmt)
```

```fortran
call to_file(x, file_name, header, locale, delim, fmt, decimals)
```

```fortran
call to_file(x, file_name, header, locale, delim, fmt, decimals, im)
```

* `file_name` is of type `character(len=*)`, which prescribes the file path to write to.
* `header` is `optional` and of type `character(len=*), dimension(:)`, which prescribes the header of a text file.
* `locale` is `optional`, may be one of `['US', 'EU']` where `'US'` is default, which prescribes the form of the decimal.
* `delim` is `optional` and of type `character(len=*)`, which prescribes the delimiter to use for a text file.
* `fmt` is `optional`, may be one of `['i', 'z']` if `x` is `integer` or one of `['e', 'f', 'z']` if `x` is `real` or `complex`, which prescribes the text format to write to.
* `decimals` is `optional` and of type `int32`, which prescribes the number of decimals places to write for a text file and qualifying format.
* `im` is `optional` and of type `character(len=*)`, which prescribes the imaginary unit to write to a text file if `x` is `complex`.

If writing to binary, all optional arguments are ignored. It is always recommended to omit the delimiter argument for default unless a custom delimiter is really necessary. The `header` is in the form of a character array literal, and must be of size `1` or `size(x, dim=2)`, or will be ignored. The `locale`, `fmt`, `decimals`, and `im` arguments are precisely as they are for the `str` function, and are honored in precisely the same ways. If `locale='US'`, the default delimiter is `','`. If `locale='EU'`, the default delimiter is `';'`.

For `x` of rank `3`-`15` and of type `integer`, `real`, `complex`:

```fortran
call to_file(x, file_name)
```

* `file_name` is of type `character(len=*)`, which prescribes the file path to write to.

## `impure subroutine from_file`

For reading text data into an `allocatable` array `into` of rank `1` or `2` and of type `integer`, `real`, `complex`:

```fortran
call from_file(file_name, into, header, fmt)
```

```fortran
call from_file(file_name, into, header, locale, fmt)
```

```fortran
call from_file(file_name, into, header, locale, fmt, im)
```

* `file_name` is of type `character(len=*)`, which prescribes the file path to read from.
* `into` is an `allocatable` array of type `integer`, `real`, or `complex`, and has attribute `dimension(:)` or `dimension(:,:)`, which is the array for storing the data read from file.
* `header` is `optional` and of type `logical`, which is `.false.` by default and specifies whether a header is present in the text file.
* `locale` is `optional`, may be one of `['US', 'EU']` where `'US'` is default, which specifies the form of the decimal.
* `fmt` is `optional`, may be one of `['i', 'z']` for `integer` data or one of `['e', 'f', 'z']` for `real` or `complex` data, which specifies the text format of the data.
* `im` is `optional` and of type `character(len=*)`, which specifies the imaginary unit present in a text file of `complex` data.

The corresponding actual argument of `into` must also be `allocatable`, and will lose its allocation status if already allocated upon passing into `from_file`. Thus `from_file` does not allow reading into sections of already allocated arrays. By default, integer data will be assumed to be in format `'i'` and `real` or `complex` data will be assumed to be in format `'e'`. If `im` is not present for complex data, the data will be assumed to be in ordered pair form.

For reading binary data into an `allocatable` array `into` of any rank and of type `integer`, `real`, `complex`:

```fortran
call from_file(file_name, into, data_shape)
```

* `file_name` is of type `character(len=*)`, which prescribes the file path to read from.
* `into` is an `allocatable` array of type `integer`, `real`, or `complex`, which is the array for storing the data read from file.
* `data_shape` is an `int32` array of rank `1` which specifies the shape of the data in the file.

Note that `data_shape` must be present when reading from binary files, and that the size of `data_shape` must match the rank of `into` for the read to be valid.

## `impure subroutine aprint`

For `x` of rank `1` or `2` and of type `character`, `integer`, `real`, `complex`:

```fortran
call aprint(x)
```

```fortran
call aprint(x, fmt)
```

```fortran
call aprint(x, fmt, decimals)
```

```fortran
call aprint(x, fmt, decimals, im)
```

* `fmt` is `optional`, may be one of `['i', 'z']` for `integer` data or one of `['e', 'f', 'z']` for `real` or `complex` data, which specifies the text format of the data.
* `decimals` is `optional` and of type `int32`, which prescribes the number of decimals places to display.
* `im` is `optional` and of type `character(len=*)`, which specifies the imaginary unit to display.

For printing arrays, the default values of the optional arguments are different than for `str` and `to_file`. For instance, `decimals` will default to `2` and `fmt` will default to `f` for `real` and `complex` data. Additionally, `im` will default to `'j'` for complex data for readability.

## `type String`

```fortran
type String
    character(len=:), allocatable :: s
end type String
```

This type is a simple wrapper for an allocatable string `s`, provided for the purpose of declaring arrays of varying-length strings whenever such an object is desired over an array of identical length strings.

## `parameter nl`

```fortran
character(len=1), parameter :: nl = new_line('a')
```

This is the new line character, provided for the purpose of inserting new lines in formatted output without needing to call the `new_line` intrinsic each time.
