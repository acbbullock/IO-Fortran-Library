---
title: Character sets and kinds
author: Austin C Bullock
---

The IO Fortran Library officially supports the standard Fortran character set, based on the [US-ASCII](https://en.wikipedia.org/wiki/ASCII) character set with default character `kind`. On most systems, the default `kind` will consist of precisely one byte per character, equivalent to `selected_char_kind('ascii')`. Since US-ASCII is a 7-bit character set and most systems use 8-bit characters, it must be noted that only the first 128 characters are system-independent, i.e. `achar(0)` is portable but `achar(255)` is not. To maximize portability across systems and compilers, the IO Fortran Library does not reference any other character sets or kinds other than the 128-character US-ASCII with default `kind`.

Even without official support, the user may employ any number of special characters in strings and string expressions (including many Unicode symbols) and these will tend to behave as expected as long as the characters can fit comfortably into one byte and the output unit supports UTF-8 encoding. For instance, inspect the output of the following program:

```fortran
program main
    use io_fortran_lib
    implicit none (type,external)

    type(String) :: emojis

    emojis = String('😂🙈😊🤣') + '😍' - '😂' + '👌'**5
    call emojis%echo('emojis.txt')
    write(*,*) emojis
end program main
```

The expected result is `🙈😊🤣😍👌👌👌👌👌`, which will be displayed properly in any terminal or text file with UTF-8 encoding.

@note The Fortran standard permits compilers to support character sets and kinds other than US-ASCII with one-byte `kind`, and acknowledges the extended four-byte [UCS-4](https://en.wikipedia.org/wiki/Universal_Coded_Character_Set) character set defined by ISO 10646, but such support is highly inconsistent across compilers at the time of writing.
