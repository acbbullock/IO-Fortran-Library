! Generated from fypp/modules/io_fortran_lib_mod.fypp by fypp/generate.sh. Edit that file, not this one.
#include "iofl_config.inc"
module io_fortran_lib
  !---------------------------------------------------------------------------------------------------------------------
  !! This module provides common I/O routines for data of `integer`, `real`, `complex`, and `character` type, and
  !! a derived type `String` for advanced character handling and text file I/O. This module is F2018 compliant, has
  !! no external dependencies, and has a max line length of 120.
  !---------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: r128=>real128, r64=>real64, r32=>real32,      & ! ISO standard real kinds
                                           i64=>int64, i32=>int32, i16=>int16, i8=>int8, & ! ISO standard int kinds
                                           input_unit, output_unit,                      & ! I/O units
                                           compiler_version                                ! Compiler inquiry function
  use, intrinsic :: iso_c_binding,   only: c_null_char                                     ! C null character
  implicit none (type, external)                                                           ! Nothing implicit
  private                                                                                  ! Default private access

  ! Public API list ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  public :: aprint, to_file, from_file                                                       ! Array I/O
  public :: String, str, cast, join, split, echo                                             ! String I/O
  public :: IOFL_NL, IOFL_SPACE, IOFL_CR, IOFL_LF, IOFL_TAB, IOFL_FF, IOFL_VT, IOFL_BELL, IOFL_NUL ! Constants
  public :: IOFL_READ_ERR, IOFL_WRITE_ERR, IOFL_ALLOC_ERR, IOFL_ARG_ERR                     ! Error codes
  public :: NL, SPACE, CR, FF, VT, LF, TAB, HT, BELL, NUL, CNUL                              ! Constants, old names
  public :: operator(//), operator(+), operator(-), operator(**), operator(==), operator(/=) ! Operators
  public :: operator(<), operator(<=), operator(>), operator(>=)                             ! Operators

  ! Definitions and Interfaces ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=1), parameter :: IOFL_NL    = new_line("a") !! The newline character (system agnostic)
  character(len=1), parameter :: IOFL_SPACE = achar(32)     !! The space character
  character(len=1), parameter :: IOFL_CR    = achar(13)     !! The carriage return character
  character(len=1), parameter :: IOFL_LF    = achar(10)     !! The line feed character
  character(len=1), parameter :: IOFL_TAB   = achar(9)      !! The horizontal tab character
  character(len=1), parameter :: IOFL_FF    = achar(12)     !! The form feed character
  character(len=1), parameter :: IOFL_VT    = achar(11)     !! The vertical tab character
  character(len=1), parameter :: IOFL_BELL  = achar(7)      !! The bell/alert character
  character(len=1), parameter :: IOFL_NUL   = c_null_char   !! The null character, which is also C's string terminator

  ! The names before 2.1, kept for compatibility. Prefer the `IOFL_` names, which don't clash with variable names.
  character(len=1), parameter :: NL        = IOFL_NL       !! Same as `IOFL_NL`
  character(len=1), parameter :: SPACE     = IOFL_SPACE    !! Same as `IOFL_SPACE`
  character(len=1), parameter :: CR        = IOFL_CR       !! Same as `IOFL_CR`
  character(len=1), parameter :: FF        = IOFL_FF       !! Same as `IOFL_FF`
  character(len=1), parameter :: VT        = IOFL_VT       !! Same as `IOFL_VT`
  character(len=1), parameter :: LF        = IOFL_LF       !! Same as `IOFL_LF`
  character(len=1), parameter :: TAB       = IOFL_TAB      !! Same as `IOFL_TAB`
  character(len=1), parameter :: HT        = IOFL_TAB      !! Same as `IOFL_TAB`
  character(len=1), parameter :: BELL      = IOFL_BELL     !! Same as `IOFL_BELL`
  character(len=1), parameter :: NUL       = achar(0)      !! Same as `IOFL_NUL`
  character(len=1), parameter :: CNUL      = c_null_char   !! Same as `IOFL_NUL`

  character(len=*), parameter :: COMPILER  = compiler_version() ! Compiler version
  character(len=0), parameter :: EMPTY_STR = ""                 ! The empty string
  character(len=1), parameter :: SEMICOLON = achar(59)          ! Semicolon
  character(len=1), parameter :: POINT     = achar(46)          ! Full stop
  character(len=1), parameter :: COMMA     = achar(44)          ! Comma
  character(len=1), parameter :: QQUOTE    = achar(34)          ! Double quote

  character(len=1), parameter :: INT_FMTS(*)  = [ "i", "z"      ] ! Allowed formats for integers
  character(len=1), parameter :: REAL_FMTS(*) = [ "e", "f", "z" ] ! Allowed formats for floats
  character(len=2), parameter :: LOCALES(*)   = [ "US", "EU"    ] ! Allowed locale specifiers

  character(len=3), parameter :: BINARY_EXT(*) = [ "dat", "bin"  ] ! Allowed binary extensions
  character(len=3), parameter :: TEXT_EXT(*)   = [ "csv", "tsv", & ! Allowed text extensions
                                                   "txt", "log", &
                                                   "rtf", "odm", &
                                                   "odt", "ods", &
                                                   "odf", "xls", &
                                                   "doc", "org", &
                                                   "dbf", "bed", &
                                                   "gff", "gtf"  ]

  integer, parameter :: IOFL_READ_ERR  = 1 !! `stat` after an error reading, inquiring about or closing a file
  integer, parameter :: IOFL_WRITE_ERR = 2 !! `stat` after an error writing or closing a file
  integer, parameter :: IOFL_ALLOC_ERR = 3 !! `stat` after a failed allocation
  integer, parameter :: IOFL_ARG_ERR   = 4 !! `stat` after an invalid argument

  integer, parameter :: READ_ERR  = IOFL_READ_ERR  ! Internal names of the error codes
  integer, parameter :: WRITE_ERR = IOFL_WRITE_ERR
  integer, parameter :: ALLOC_ERR = IOFL_ALLOC_ERR
  integer, parameter :: ARG_ERR   = IOFL_ARG_ERR

  type String
    !-------------------------------------------------------------------------------------------------------------------
    !! A growable string type for advanced character handling and text I/O.
    !!
    !! For a user reference, see [String](../page/Ref/String.html),
    !! [String methods](../page/Ref/String-methods.html), and [Operators](../page/Ref/operators.html).
    !!
    !! @note TECHNICAL NOTE: The `String` type is memory safe. The user will never need to be concerned about
    !! accessing invalid memory when using the `String` type. Any operation defined in this documentation for the
    !! `String` type which may involve a `String` with an unallocated component, or arrays of `String`s in which
    !! some of the elements may have unallocated components, is well-defined. In all such cases, the component is
    !! treated as the [empty string](../module/io_fortran_lib.html#variable-empty_str).
    !-------------------------------------------------------------------------------------------------------------------
    private
    character(len=:), allocatable :: buf     !! Text in `buf(:n)`, with any room to grow after it
    integer(i64)                  :: n = 0_i64 !! Length of the text
    contains
      private

      ! Generics ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ifdef IOFL_HAS_R128
      generic, public :: cast             => cast_string_to_c128, cast_string_to_r128
#endif
      generic, public :: cast             => cast_string_to_c64, cast_string_to_c32, &
                                             cast_string_to_r64, cast_string_to_r32, &
                                             cast_string_to_i64, cast_string_to_i32, cast_string_to_i16, &
                                             cast_string_to_i8
      generic, public :: count            => count_substring_chars, count_substring_string
      generic, public :: count64          => count64_chars, count64_string
      generic, public :: echo             => echo_string
      generic, public :: push             => push_chars, push_string
      generic, public :: replace          => replace_ch_copy, replace_st_copy, replace_chst_copy, &
                                             replace_stch_copy
      generic, public :: replace_inplace  => replace_ch_inplace, replace_st_inplace, replace_chst_inplace, &
                                             replace_stch_inplace
      generic, public :: split            => split_string
      generic, public :: write(formatted) => write_string

      ! Specifics ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      procedure, pass(self), public :: as_str
#ifdef IOFL_HAS_R128
      procedure, pass(substring)    :: cast_string_to_c128, cast_string_to_r128
#endif
      procedure, pass(substring)    :: cast_string_to_c64, cast_string_to_c32, &
                                       cast_string_to_r64, cast_string_to_r32, &
                                       cast_string_to_i64, cast_string_to_i32, cast_string_to_i16, &
                                       cast_string_to_i8
      procedure, pass(self)         :: count_substring_chars, count_substring_string
      procedure, pass(self)         :: count64_chars, count64_string
      procedure, pass(substring)    :: echo_string
      procedure, pass(self), public :: empty
      procedure, pass(self), public :: join => join_into_self
      procedure, pass(self)         :: join_base
      procedure, pass(self), public :: len => length
      procedure, pass(self), public :: len64 => length64
      procedure, pass(self)         :: push_chars, push_string
      procedure, pass(self), public :: read_file
      procedure, pass(self)         :: replace_ch_copy, replace_st_copy, replace_chst_copy, &
                                       replace_stch_copy, replace_ch_inplace, replace_st_inplace, &
                                       replace_chst_inplace, replace_stch_inplace
      procedure, pass(substring)    :: split_string
      procedure, pass(self), public :: trim => trim_copy
      procedure, pass(self), public :: trim_inplace
      procedure, pass(self), public :: write_file
      procedure, pass(substring)    :: write_string
      procedure, pass(self), public :: read_line
  end type String

  interface                                                                                   ! Submodule io_string_methods
    !-------------------------------------------------------------------------------------------------------------------
    !! Methods for the `String` type.
    !-------------------------------------------------------------------------------------------------------------------
    pure recursive module function as_str(self) result(string_slice)
      !-----------------------------------------------------------------------------------------------------------------
      !! Returns a copy of the string slice component of a scalar `String`.
      !!
      !! For a user reference, see [as_str](../page/Ref/String-methods.html#as_str).
      !-----------------------------------------------------------------------------------------------------------------
      class(String),    intent(in)  :: self
      character(len=:), allocatable :: string_slice
    end function as_str

    elemental recursive integer(i64) module function count64_chars(self, match) result(occurrences)
      !-----------------------------------------------------------------------------------------------------------------
      !! Returns number of non-overlapping occurrences of a substring elementally, as a 64-bit integer.
      !-----------------------------------------------------------------------------------------------------------------
      class(String),    intent(in) :: self
      character(len=*), intent(in) :: match
    end function count64_chars

    elemental recursive integer(i64) module function count64_string(self, match) result(occurrences)
      !-----------------------------------------------------------------------------------------------------------------
      !! Returns number of non-overlapping occurrences of a substring elementally, as a 64-bit integer.
      !-----------------------------------------------------------------------------------------------------------------
      class(String), intent(in) :: self
      type(String),  intent(in) :: match
    end function count64_string

    elemental recursive integer module function count_substring_chars(self, match) result(occurrences)
      !-----------------------------------------------------------------------------------------------------------------
      !! Returns number of non-overlapping occurrences of a substring elementally.
      !-----------------------------------------------------------------------------------------------------------------
      class(String),    intent(in) :: self
      character(len=*), intent(in) :: match
    end function count_substring_chars

    elemental recursive integer module function count_substring_string(self, match) result(occurrences)
      !-----------------------------------------------------------------------------------------------------------------
      !! Returns number of non-overlapping occurrences of a substring elementally.
      !-----------------------------------------------------------------------------------------------------------------
      class(String), intent(in) :: self
      type(String),  intent(in) :: match
    end function count_substring_string

    elemental recursive module subroutine empty(self)
      !-----------------------------------------------------------------------------------------------------------------
      !! Sets the string slice component to the empty string elementally. This procedure is identical in function
      !! to the assignment `self = String()`.
      !!
      !! For a user reference, see [empty](../page/Ref/String-methods.html#empty).
      !-----------------------------------------------------------------------------------------------------------------
      class(String), intent(inout) :: self
    end subroutine empty

    pure recursive module subroutine join_into_self(self, tokens, separator)
      !-----------------------------------------------------------------------------------------------------------------
      !! Joins a `String` vector `tokens` into `self` with given separator. Default separator is SPACE. The
      !! string slice component will be replaced if already allocated.
      !!
      !! For a user reference, see [join](../page/Ref/String-methods.html#join).
      !-----------------------------------------------------------------------------------------------------------------
      class(String),    intent(inout)        :: self
      type(String),     intent(in)           :: tokens(:)
      character(len=*), intent(in), optional :: separator
    end subroutine join_into_self

    pure recursive module subroutine join_base(self, tokens, separator)
      !-----------------------------------------------------------------------------------------------------------------
      !! Tail recursion routine for `join_string` and `join_into_self`.
      !-----------------------------------------------------------------------------------------------------------------
      class(String),    intent(inout) :: self
      type(String),     intent(in)    :: tokens(:)
      character(len=*), intent(in)    :: separator
    end subroutine join_base

    elemental recursive integer module function length(self) result(self_len)
      !-----------------------------------------------------------------------------------------------------------------
      !! Returns the length of the string slice component elementally. Unallocated components return `-1`.
      !!
      !! For a user reference, see [len](../page/Ref/String-methods.html#len).
      !-----------------------------------------------------------------------------------------------------------------
      class(String), intent(in) :: self
    end function length

    elemental recursive integer(i64) module function length64(self) result(self_len)
      !-----------------------------------------------------------------------------------------------------------------
      !! Returns the length of the string slice component elementally. Unallocated components return `-1`. This
      !! function is identical to `len` for strings of 2,147,483,647 bytes or smaller.
      !!
      !! For a user reference, see [len](../page/Ref/String-methods.html#len).
      !-----------------------------------------------------------------------------------------------------------------
      class(String), intent(in) :: self
    end function length64

    elemental recursive module subroutine push_chars(self, substring)
      !-----------------------------------------------------------------------------------------------------------------
      !! Appends characters to the string slice component elementally in place.
      !-----------------------------------------------------------------------------------------------------------------
      class(String),    intent(inout) :: self
      character(len=*), intent(in)    :: substring
    end subroutine push_chars

    elemental recursive module subroutine push_string(self, substring)
      !-----------------------------------------------------------------------------------------------------------------
      !! Appends string to the string slice component elementally in place.
      !-----------------------------------------------------------------------------------------------------------------
      class(String), intent(inout) :: self
      type(String),  intent(in)    :: substring
    end subroutine push_string

    recursive module subroutine read_file(self, file, cell_array, row_separator, column_separator, stat, &
                                          errmsg, format)
      !-----------------------------------------------------------------------------------------------------------------
      !! Reads raw text file contents into `self` and optionally populates a cell array using the designated
      !! `row_separator` and `column_separator` whose default values are `LF`, and a tab for a `.tsv` file or `COMMA`
      !! otherwise.
      !!
      !! For a user reference, see [read_file](../page/Ref/String-methods.html#read_file).
      !-----------------------------------------------------------------------------------------------------------------
      class(String),    intent(inout)                      :: self
      character(len=*), intent(in)                         :: file
      type(String),     intent(out), optional, allocatable :: cell_array(:,:)
      character(len=*), intent(in),  optional      :: row_separator, column_separator
      integer,          intent(out), optional, target      :: stat
      character(len=*), intent(out), optional, target      :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine read_file

    elemental recursive type(String) module function replace_ch_copy(self, match, substring, back) result(new)
      !-----------------------------------------------------------------------------------------------------------------
      !! Matches and replaces all occurrences of a substring elementally.
      !-----------------------------------------------------------------------------------------------------------------
      class(String),    intent(in)           :: self
      character(len=*), intent(in)           :: match, substring
      logical,          intent(in), optional :: back
    end function replace_ch_copy

    elemental recursive type(String) module function replace_st_copy(self, match, substring, back) result(new)
      !-----------------------------------------------------------------------------------------------------------------
      !! Matches and replaces all occurrences of a substring elementally.
      !-----------------------------------------------------------------------------------------------------------------
      class(String), intent(in)           :: self
      type(String),  intent(in)           :: match, substring
      logical,       intent(in), optional :: back
    end function replace_st_copy

    elemental recursive type(String) module function replace_chst_copy(self, match, substring, back) result(new)
      !-----------------------------------------------------------------------------------------------------------------
      !! Matches and replaces all occurrences of a substring elementally.
      !-----------------------------------------------------------------------------------------------------------------
      class(String),    intent(in)           :: self
      character(len=*), intent(in)           :: match
      type(String),     intent(in)           :: substring
      logical,          intent(in), optional :: back
    end function replace_chst_copy

    elemental recursive type(String) module function replace_stch_copy(self, match, substring, back) result(new)
      !-----------------------------------------------------------------------------------------------------------------
      !! Matches and replaces all occurrences of a substring elementally.
      !-----------------------------------------------------------------------------------------------------------------
      class(String),    intent(in)           :: self
      type(String),     intent(in)           :: match
      character(len=*), intent(in)           :: substring
      logical,          intent(in), optional :: back
    end function replace_stch_copy

    elemental recursive module subroutine replace_ch_inplace(self, match, substring, back)
      !-----------------------------------------------------------------------------------------------------------------
      !! Matches and replaces all occurrences of a substring elementally in place.
      !-----------------------------------------------------------------------------------------------------------------
      class(String),    intent(inout)        :: self
      character(len=*), intent(in)           :: match, substring
      logical,          intent(in), optional :: back
    end subroutine replace_ch_inplace

    elemental recursive module subroutine replace_st_inplace(self, match, substring, back)
      !-----------------------------------------------------------------------------------------------------------------
      !! Matches and replaces all occurrences of a substring elementally in place.
      !-----------------------------------------------------------------------------------------------------------------
      class(String), intent(inout)        :: self
      type(String),  intent(in)           :: match, substring
      logical,       intent(in), optional :: back
    end subroutine replace_st_inplace

    elemental recursive module subroutine replace_chst_inplace(self, match, substring, back)
      !-----------------------------------------------------------------------------------------------------------------
      !! Matches and replaces all occurrences of a substring elementally in place.
      !-----------------------------------------------------------------------------------------------------------------
      class(String),    intent(inout)        :: self
      character(len=*), intent(in)           :: match
      type(String),     intent(in)           :: substring
      logical,          intent(in), optional :: back
    end subroutine replace_chst_inplace

    elemental recursive module subroutine replace_stch_inplace(self, match, substring, back)
      !-----------------------------------------------------------------------------------------------------------------
      !! Matches and replaces all occurrences of a substring elementally in place.
      !-----------------------------------------------------------------------------------------------------------------
      class(String),    intent(inout)        :: self
      type(String),     intent(in)           :: match
      character(len=*), intent(in)           :: substring
      logical,          intent(in), optional :: back
    end subroutine replace_stch_inplace

    elemental recursive type(String) module function trim_copy(self) result(new)
      !-----------------------------------------------------------------------------------------------------------------
      !! Returns a copy of a `String` elementally in which each string slice component has been trimmed of any
      !! leading or trailing whitespace.
      !!
      !! For a user reference, see [trim](../page/Ref/String-methods.html#trim).
      !-----------------------------------------------------------------------------------------------------------------
      class(String), intent(in) :: self
    end function trim_copy

    elemental recursive module subroutine trim_inplace(self)
      !-----------------------------------------------------------------------------------------------------------------
      !! Removes any leading or trailing whitespace of the string slice component of a `String` elementally and
      !! in place.
      !!
      !! For a user reference, see [trim_inplace](../page/Ref/String-methods.html#trim_inplace).
      !-----------------------------------------------------------------------------------------------------------------
      class(String), intent(inout) :: self
    end subroutine trim_inplace

    recursive module subroutine write_file(self, cell_array, file, row_separator, column_separator, append, &
                                           stat, errmsg, format)
      !-----------------------------------------------------------------------------------------------------------------
      !! Writes the content of a cell array to a text file. The cell array's entire contents are populated into
      !! `self` and then streamed to an external text file using the designated `row_separator` and
      !! `column_separator` whose default values are `LF`, and a tab for a `.tsv` file or `COMMA` otherwise.
      !!
      !! For a user reference, see [write_file](../page/Ref/String-methods.html#write_file).
      !-----------------------------------------------------------------------------------------------------------------
      class(String),    intent(inout)                 :: self
      type(String),     intent(in)                    :: cell_array(:,:)
      character(len=*), intent(in)                    :: file
      character(len=*), intent(in),  optional :: row_separator, column_separator
      logical,          intent(in),  optional :: append
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine write_file

    recursive module subroutine read_cells(self, file, pairs, cell_array, row_separator, column_separator, &
                                           stat, errmsg, format)
      class(String),    intent(inout)                      :: self
      character(len=*), intent(in)                         :: file
      logical,          intent(in)                         :: pairs
      type(String),     intent(out), optional, allocatable :: cell_array(:,:)
      character(len=*), intent(in),  optional      :: row_separator, column_separator
      integer,          intent(out), optional, target      :: stat
      character(len=*), intent(out), optional, target      :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine read_cells

    recursive module subroutine write_cells(self, pairs, cell_array, file, row_separator, column_separator, &
                                            append, &
                                           stat, errmsg, format)
      class(String),    intent(inout)                 :: self
      logical,          intent(in)                    :: pairs
      type(String),     intent(in)                    :: cell_array(:,:)
      character(len=*), intent(in)                    :: file
      character(len=*), intent(in),  optional :: row_separator, column_separator
      logical,          intent(in),  optional :: append
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine write_cells

    module subroutine stop_if_unhandled(stat, errmsg, handled)
      !-----------------------------------------------------------------------------------------------------------------
      !! Ends the program on an error when the caller did not pass `stat`, so that an error can't go unnoticed. This is
      !! not `pure` because at `-O0`, ifx drops the `error stop` in a pure procedure with only `intent(in)` arguments.
      !-----------------------------------------------------------------------------------------------------------------
      integer,          intent(in) :: stat
      character(len=*), intent(in) :: errmsg
      logical,          intent(in) :: handled
    end subroutine stop_if_unhandled

    module subroutine close_quietly(unit)
      !-----------------------------------------------------------------------------------------------------------------
      !! Closes a file while handling another error, which is the one to report.
      !-----------------------------------------------------------------------------------------------------------------
      integer, intent(in) :: unit
    end subroutine close_quietly

    recursive module subroutine read_line(self, unit, stat, errmsg)
      !-----------------------------------------------------------------------------------------------------------------
      !! Reads the next record from a formatted, connected `unit` into `self`. At the end of the file, `stat` satisfies
      !! `is_iostat_end`. If `stat` is not present, the end of the file or any other error ends the program.
      !!
      !! This is a method rather than `read(formatted)` because in a child read at the end of a record, gfortran 15 and
      !! 16 skip the next record and ifx reports an error to the parent statement.
      !!
      !! For a user reference, see [read_line](../page/Ref/String-methods.html#read_line).
      !-----------------------------------------------------------------------------------------------------------------
      class(String),    intent(inout)         :: self
      integer,          intent(in)            :: unit
      integer,          intent(out), optional :: stat
      character(len=*), intent(out), optional :: errmsg
    end subroutine read_line

    recursive module subroutine write_string(substring, unit, iotype, v_list, iostat, iomsg)
      !-----------------------------------------------------------------------------------------------------------------
      !! Formatted write DTIO procedure for type `String`.
      !-----------------------------------------------------------------------------------------------------------------
      class(String),    intent(in)    :: substring
      integer,          intent(in)    :: unit
      character(len=*), intent(in)    :: iotype
      integer,          intent(in)    :: v_list(:)
      integer,          intent(out)   :: iostat
      character(len=*), intent(inout) :: iomsg
    end subroutine write_string

  end interface

  interface optval
    !-------------------------------------------------------------------------------------------------------------------
    !! Returns the value of an optional argument, or `default` if it is not present.
    !-------------------------------------------------------------------------------------------------------------------
    pure module function optval_chars(x, default) result(value)
      character(len=*), intent(in), optional :: x
      character(len=*), intent(in)           :: default
      character(len=:), allocatable          :: value
    end function optval_chars

    pure module function optval_int(x, default) result(value)
      integer, intent(in), optional :: x
      integer, intent(in)           :: default
      integer                       :: value
    end function optval_int

    pure module function optval_logical(x, default) result(value)
      logical, intent(in), optional :: x
      logical, intent(in)           :: default
      logical                       :: value
    end function optval_logical
  end interface optval

  interface operator(//)                                                                           ! Submodule io_operators
    !-------------------------------------------------------------------------------------------------------------------
    !! Concatenation operator for `character` and `String`, lifted from `character`. Mixed type concatenation of
    !! `character` and `String` is explicitly defined.
    !!
    !! For a user reference, see [Concatenation](../page/Ref/operators.html#concatenation).
    !-------------------------------------------------------------------------------------------------------------------
    elemental recursive type(String) module function string_concatenation(Stringl, Stringr) result(new)
      class(String), intent(in) :: Stringl, Stringr
    end function string_concatenation

    elemental recursive type(String) module function string_char_concatenation(Stringl, charsr) result(new)
      class(String),    intent(in) :: Stringl
      character(len=*), intent(in) :: charsr
    end function string_char_concatenation

    elemental recursive type(String) module function char_string_concatenation(charsl, Stringr) result(new)
      character(len=*), intent(in) :: charsl
      class(String),    intent(in) :: Stringr
    end function char_string_concatenation
  end interface operator(//)

  interface operator(<) ! Submodule io_operators
    !-------------------------------------------------------------------------------------------------------------------
    !! Less than for `String`, and mixed `character` and `String`, by comparing bytes. A shorter string that
    !! matches the start of a longer one comes first, and an unallocated `String` acts as empty, in agreement with `==`.
    !!
    !! For a user reference, see [Ordering](../page/Ref/operators.html#ordering).
    !-------------------------------------------------------------------------------------------------------------------
    elemental recursive logical module function string_lt(Stringl, Stringr) result(res)
      class(String), intent(in) :: Stringl, Stringr
    end function string_lt

    elemental recursive logical module function string_char_lt(Stringl, charsr) result(res)
      class(String),    intent(in) :: Stringl
      character(len=*), intent(in) :: charsr
    end function string_char_lt

    elemental recursive logical module function char_string_lt(charsl, Stringr) result(res)
      character(len=*), intent(in) :: charsl
      class(String),    intent(in) :: Stringr
    end function char_string_lt
  end interface operator(<)

  interface operator(<=) ! Submodule io_operators
    !-------------------------------------------------------------------------------------------------------------------
    !! Less than or equal for `String`, and mixed `character` and `String`, by comparing bytes. A shorter string that
    !! matches the start of a longer one comes first, and an unallocated `String` acts as empty, in agreement with `==`.
    !!
    !! For a user reference, see [Ordering](../page/Ref/operators.html#ordering).
    !-------------------------------------------------------------------------------------------------------------------
    elemental recursive logical module function string_le(Stringl, Stringr) result(res)
      class(String), intent(in) :: Stringl, Stringr
    end function string_le

    elemental recursive logical module function string_char_le(Stringl, charsr) result(res)
      class(String),    intent(in) :: Stringl
      character(len=*), intent(in) :: charsr
    end function string_char_le

    elemental recursive logical module function char_string_le(charsl, Stringr) result(res)
      character(len=*), intent(in) :: charsl
      class(String),    intent(in) :: Stringr
    end function char_string_le
  end interface operator(<=)

  interface operator(>) ! Submodule io_operators
    !-------------------------------------------------------------------------------------------------------------------
    !! Greater than for `String`, and mixed `character` and `String`, by comparing bytes. A shorter string that
    !! matches the start of a longer one comes first, and an unallocated `String` acts as empty, in agreement with `==`.
    !!
    !! For a user reference, see [Ordering](../page/Ref/operators.html#ordering).
    !-------------------------------------------------------------------------------------------------------------------
    elemental recursive logical module function string_gt(Stringl, Stringr) result(res)
      class(String), intent(in) :: Stringl, Stringr
    end function string_gt

    elemental recursive logical module function string_char_gt(Stringl, charsr) result(res)
      class(String),    intent(in) :: Stringl
      character(len=*), intent(in) :: charsr
    end function string_char_gt

    elemental recursive logical module function char_string_gt(charsl, Stringr) result(res)
      character(len=*), intent(in) :: charsl
      class(String),    intent(in) :: Stringr
    end function char_string_gt
  end interface operator(>)

  interface operator(>=) ! Submodule io_operators
    !-------------------------------------------------------------------------------------------------------------------
    !! Greater than or equal for `String`, and mixed `character` and `String`, by comparing bytes. A shorter string that
    !! matches the start of a longer one comes first, and an unallocated `String` acts as empty, in agreement with `==`.
    !!
    !! For a user reference, see [Ordering](../page/Ref/operators.html#ordering).
    !-------------------------------------------------------------------------------------------------------------------
    elemental recursive logical module function string_ge(Stringl, Stringr) result(res)
      class(String), intent(in) :: Stringl, Stringr
    end function string_ge

    elemental recursive logical module function string_char_ge(Stringl, charsr) result(res)
      class(String),    intent(in) :: Stringl
      character(len=*), intent(in) :: charsr
    end function string_char_ge

    elemental recursive logical module function char_string_ge(charsl, Stringr) result(res)
      character(len=*), intent(in) :: charsl
      class(String),    intent(in) :: Stringr
    end function char_string_ge
  end interface operator(>=)

  interface operator(+)                                                                            ! Submodule io_operators
    !-------------------------------------------------------------------------------------------------------------------
    !! Concatenation operator for `character` and `String` (as addition). Mixed type concatenation of
    !! `character` and `String` is explicitly defined.
    !!
    !! For a user reference, see [Concatenation](../page/Ref/operators.html#concatenation).
    !-------------------------------------------------------------------------------------------------------------------
    module procedure char_concat_plus

    elemental recursive type(String) module function string_concat_plus(Stringl, Stringr) result(new)
      class(String), intent(in) :: Stringl, Stringr
    end function string_concat_plus

    elemental recursive type(String) module function string_char_concat_plus(Stringl, charsr) result(new)
      class(String),    intent(in) :: Stringl
      character(len=*), intent(in) :: charsr
    end function string_char_concat_plus

    elemental recursive type(String) module function char_string_concat_plus(charsl, Stringr) result(new)
      character(len=*), intent(in) :: charsl
      class(String),    intent(in) :: Stringr
    end function char_string_concat_plus
  end interface operator(+)

  interface operator(-)                                                                            ! Submodule io_operators
    !-------------------------------------------------------------------------------------------------------------------
    !! Excision operator for `character` and `String` (as subtraction). Mixed type excision of `character` and
    !! `String` is explicitly defined.
    !!
    !! For a user reference, see [Excision](../page/Ref/operators.html#excision).
    !-------------------------------------------------------------------------------------------------------------------
    elemental recursive type(String) module function char_excision(charsl, charsr) result(new)
      character(len=*), intent(in) :: charsl, charsr
    end function char_excision

    elemental recursive type(String) module function string_excision(Stringl, Stringr) result(new)
      class(String), intent(in) :: Stringl, Stringr
    end function string_excision

    elemental recursive type(String) module function string_char_excision(Stringl, charsr) result(new)
      class(String),    intent(in) :: Stringl
      character(len=*), intent(in) :: charsr
    end function string_char_excision

    elemental recursive type(String) module function char_string_excision(charsl, Stringr) result(new)
      character(len=*), intent(in) :: charsl
      class(String),    intent(in) :: Stringr
    end function char_string_excision
  end interface operator(-)

  interface operator(**)                                                                           ! Submodule io_operators
    !-------------------------------------------------------------------------------------------------------------------
    !! Repetition operator for `character` and `String` (as exponentiation).
    !!
    !! For a user reference, see [Repetition](../page/Ref/operators.html#repetition).
    !-------------------------------------------------------------------------------------------------------------------
    pure module function repeat_chars(char_base, ncopies) result(new)
      character(len=*), intent(in)          :: char_base
      integer,          intent(in)          :: ncopies
      character(len=len(char_base)*ncopies) :: new
    end function repeat_chars

    pure module function repeat_chars_1d(char_base, ncopies) result(new)
      character(len=*), intent(in)          :: char_base(:)
      integer,          intent(in)          :: ncopies
      character(len=len(char_base)*ncopies) :: new(size(char_base,1))
    end function repeat_chars_1d

    pure module function repeat_chars_2d(char_base, ncopies) result(new)
      character(len=*), intent(in)          :: char_base(:,:)
      integer,          intent(in)          :: ncopies
      character(len=len(char_base)*ncopies) :: new(size(char_base,1),size(char_base,2))
    end function repeat_chars_2d

    pure module function repeat_chars_3d(char_base, ncopies) result(new)
      character(len=*), intent(in)          :: char_base(:,:,:)
      integer,          intent(in)          :: ncopies
      character(len=len(char_base)*ncopies) :: new(size(char_base,1),size(char_base,2),size(char_base,3))
    end function repeat_chars_3d

    pure module function repeat_chars_4d(char_base, ncopies) result(new)
      character(len=*), intent(in)          :: char_base(:,:,:,:)
      integer,          intent(in)          :: ncopies
      character(len=len(char_base)*ncopies) :: new(size(char_base,1),size(char_base,2),size(char_base,3), &
                                                   size(char_base,4))
    end function repeat_chars_4d

    pure module function repeat_chars_5d(char_base, ncopies) result(new)
      character(len=*), intent(in)          :: char_base(:,:,:,:,:)
      integer,          intent(in)          :: ncopies
      character(len=len(char_base)*ncopies) :: new(size(char_base,1),size(char_base,2),size(char_base,3), &
                                                   size(char_base,4),size(char_base,5))
    end function repeat_chars_5d

    pure module function repeat_chars_6d(char_base, ncopies) result(new)
      character(len=*), intent(in)          :: char_base(:,:,:,:,:,:)
      integer,          intent(in)          :: ncopies
      character(len=len(char_base)*ncopies) :: new(size(char_base,1),size(char_base,2),size(char_base,3), &
                                                   size(char_base,4),size(char_base,5),size(char_base,6))
    end function repeat_chars_6d

    pure module function repeat_chars_7d(char_base, ncopies) result(new)
      character(len=*), intent(in)          :: char_base(:,:,:,:,:,:,:)
      integer,          intent(in)          :: ncopies
      character(len=len(char_base)*ncopies) :: new(size(char_base,1),size(char_base,2),size(char_base,3), &
                                                   size(char_base,4),size(char_base,5),size(char_base,6), &
                                                   size(char_base,7))
    end function repeat_chars_7d

    pure module function repeat_chars_8d(char_base, ncopies) result(new)
      character(len=*), intent(in)          :: char_base(:,:,:,:,:,:,:,:)
      integer,          intent(in)          :: ncopies
      character(len=len(char_base)*ncopies) :: new(size(char_base,1),size(char_base,2),size(char_base,3), &
                                                   size(char_base,4),size(char_base,5),size(char_base,6), &
                                                   size(char_base,7),size(char_base,8))
    end function repeat_chars_8d

    pure module function repeat_chars_9d(char_base, ncopies) result(new)
      character(len=*), intent(in)          :: char_base(:,:,:,:,:,:,:,:,:)
      integer,          intent(in)          :: ncopies
      character(len=len(char_base)*ncopies) :: new(size(char_base,1),size(char_base,2),size(char_base,3), &
                                                   size(char_base,4),size(char_base,5),size(char_base,6), &
                                                   size(char_base,7),size(char_base,8),size(char_base,9))
    end function repeat_chars_9d

    pure module function repeat_chars_10d(char_base, ncopies) result(new)
      character(len=*), intent(in)          :: char_base(:,:,:,:,:,:,:,:,:,:)
      integer,          intent(in)          :: ncopies
      character(len=len(char_base)*ncopies) :: new(size(char_base,1),size(char_base,2),size(char_base,3), &
                                                   size(char_base,4),size(char_base,5),size(char_base,6), &
                                                   size(char_base,7),size(char_base,8),size(char_base,9), &
                                                   size(char_base,10))
    end function repeat_chars_10d

    pure module function repeat_chars_11d(char_base, ncopies) result(new)
      character(len=*), intent(in)          :: char_base(:,:,:,:,:,:,:,:,:,:,:)
      integer,          intent(in)          :: ncopies
      character(len=len(char_base)*ncopies) :: new(size(char_base,1),size(char_base,2),size(char_base,3), &
                                                   size(char_base,4),size(char_base,5),size(char_base,6), &
                                                   size(char_base,7),size(char_base,8),size(char_base,9), &
                                                   size(char_base,10),size(char_base,11))
    end function repeat_chars_11d

    pure module function repeat_chars_12d(char_base, ncopies) result(new)
      character(len=*), intent(in)          :: char_base(:,:,:,:,:,:,:,:,:,:,:,:)
      integer,          intent(in)          :: ncopies
      character(len=len(char_base)*ncopies) :: new(size(char_base,1),size(char_base,2),size(char_base,3), &
                                                   size(char_base,4),size(char_base,5),size(char_base,6), &
                                                   size(char_base,7),size(char_base,8),size(char_base,9), &
                                                   size(char_base,10),size(char_base,11),size(char_base,12))
    end function repeat_chars_12d

    pure module function repeat_chars_13d(char_base, ncopies) result(new)
      character(len=*), intent(in)          :: char_base(:,:,:,:,:,:,:,:,:,:,:,:,:)
      integer,          intent(in)          :: ncopies
      character(len=len(char_base)*ncopies) :: new(size(char_base,1),size(char_base,2),size(char_base,3), &
                                                   size(char_base,4),size(char_base,5),size(char_base,6), &
                                                   size(char_base,7),size(char_base,8),size(char_base,9), &
                                                   size(char_base,10),size(char_base,11),size(char_base,12), &
                                                   size(char_base,13))
    end function repeat_chars_13d

    pure module function repeat_chars_14d(char_base, ncopies) result(new)
      character(len=*), intent(in)          :: char_base(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
      integer,          intent(in)          :: ncopies
      character(len=len(char_base)*ncopies) :: new(size(char_base,1),size(char_base,2),size(char_base,3), &
                                                   size(char_base,4),size(char_base,5),size(char_base,6), &
                                                   size(char_base,7),size(char_base,8),size(char_base,9), &
                                                   size(char_base,10),size(char_base,11),size(char_base,12), &
                                                   size(char_base,13),size(char_base,14))
    end function repeat_chars_14d

    pure module function repeat_chars_15d(char_base, ncopies) result(new)
      character(len=*), intent(in)          :: char_base(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
      integer,          intent(in)          :: ncopies
      character(len=len(char_base)*ncopies) :: new(size(char_base,1),size(char_base,2),size(char_base,3), &
                                                   size(char_base,4),size(char_base,5),size(char_base,6), &
                                                   size(char_base,7),size(char_base,8),size(char_base,9), &
                                                   size(char_base,10),size(char_base,11),size(char_base,12), &
                                                   size(char_base,13),size(char_base,14),size(char_base,15))
    end function repeat_chars_15d

    elemental recursive type(String) module function repeat_String(String_base, ncopies) result(new)
      class(String), intent(in) :: String_base
      integer,       intent(in) :: ncopies
    end function repeat_String
  end interface operator(**)

  interface operator(==)                                                                           ! Submodule io_operators
    !-------------------------------------------------------------------------------------------------------------------
    !! Equivalence operator for `character` and `String`. Mixed type equivalence of `character` and `String` is
    !! explicitly defined.
    !!
    !! For a user reference, see [Equivalence](../page/Ref/operators.html#equivalence).
    !!
    !! @note The equivalence operator `==` is interchangeable with `.eq.`.
    !-------------------------------------------------------------------------------------------------------------------
    elemental recursive logical module function string_equivalence(Stringl, Stringr) result(equal)
      class(String), intent(in) :: Stringl, Stringr
    end function string_equivalence

    elemental recursive logical module function string_char_equivalence(Stringl, charsr) result(equal)
      class(String),    intent(in) :: Stringl
      character(len=*), intent(in) :: charsr
    end function string_char_equivalence

    elemental recursive logical module function char_string_equivalence(charsl, Stringr) result(equal)
      character(len=*), intent(in) :: charsl
      class(String),    intent(in) :: Stringr
    end function char_string_equivalence
  end interface operator(==)

  interface operator(/=)                                                                           ! Submodule io_operators
    !-------------------------------------------------------------------------------------------------------------------
    !! Non-equivalence operator for `character` and `String`. Mixed type non-equivalence of `character` and
    !! `String` is explicitly defined.
    !!
    !! For a user reference, see [Non-equivalence](../page/Ref/operators.html#non-equivalence).
    !!
    !! @note The non-equivalence operator `/=` is interchangeable with `.ne.`.
    !-------------------------------------------------------------------------------------------------------------------
    elemental recursive logical module function string_nonequivalence(Stringl, Stringr) result(unequal)
      class(String), intent(in) :: Stringl, Stringr
    end function string_nonequivalence

    elemental recursive logical module function string_char_nonequivalence(Stringl, charsr) result(unequal)
      class(String),    intent(in) :: Stringl
      character(len=*), intent(in) :: charsr
    end function string_char_nonequivalence

    elemental recursive logical module function char_string_nonequivalence(charsl, Stringr) result(unequal)
      character(len=*), intent(in) :: charsl
      class(String),    intent(in) :: Stringr
    end function char_string_nonequivalence
  end interface operator(/=)

  interface String                                                                               ! Submodule io_internal
    !-------------------------------------------------------------------------------------------------------------------
    !! Function for returning a [String](../type/string.html) representation of numbers.
    !!
    !! For a user reference, see [String](../page/Ref/String.html),
    !! [String methods](../page/Ref/String-methods.html), and [Operators](../page/Ref/operators.html).
    !-------------------------------------------------------------------------------------------------------------------
#ifdef IOFL_HAS_R128
    elemental recursive type(String) module function new_string_from_c128(x, locale, fmt, decimals, im) result(new)
      complex(r128),    intent(in)           :: x
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
    end function new_string_from_c128
#endif

    elemental recursive type(String) module function new_string_from_c64(x, locale, fmt, decimals, im) result(new)
      complex(r64),    intent(in)           :: x
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
    end function new_string_from_c64

    elemental recursive type(String) module function new_string_from_c32(x, locale, fmt, decimals, im) result(new)
      complex(r32),    intent(in)           :: x
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
    end function new_string_from_c32


#ifdef IOFL_HAS_R128
    elemental recursive type(String) module function new_string_from_r128(x, locale, fmt, decimals) result(new)
      real(r128),       intent(in)           :: x
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
    end function new_string_from_r128
#endif

    elemental recursive type(String) module function new_string_from_r64(x, locale, fmt, decimals) result(new)
      real(r64),       intent(in)           :: x
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
    end function new_string_from_r64

    elemental recursive type(String) module function new_string_from_r32(x, locale, fmt, decimals) result(new)
      real(r32),       intent(in)           :: x
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
    end function new_string_from_r32


    elemental recursive type(String) module function new_string_from_i64(x, fmt) result(new)
      integer(i64),     intent(in)           :: x
      character(len=*), intent(in), optional :: fmt
    end function new_string_from_i64

    elemental recursive type(String) module function new_string_from_i32(x, fmt) result(new)
      integer(i32),     intent(in)           :: x
      character(len=*), intent(in), optional :: fmt
    end function new_string_from_i32

    elemental recursive type(String) module function new_string_from_i16(x, fmt) result(new)
      integer(i16),     intent(in)           :: x
      character(len=*), intent(in), optional :: fmt
    end function new_string_from_i16

    elemental recursive type(String) module function new_string_from_i8(x, fmt) result(new)
      integer(i8),     intent(in)           :: x
      character(len=*), intent(in), optional :: fmt
    end function new_string_from_i8


    elemental recursive type(String) module function new_string_from_string(x) result(new)
      class(String), intent(in) :: x
    end function new_string_from_string
    elemental recursive type(String) module function new_string_from_char(x) result(new)
      character(len=*), intent(in) :: x
    end function new_string_from_char
    elemental recursive type(String) module function new_string_from_empty() result(new)
      ! No arguments
    end function new_string_from_empty
  end interface String

  interface str                                                                                  ! Submodule io_internal
    !-------------------------------------------------------------------------------------------------------------------
    !! Function for returning a `character` representation of a number.
    !!
    !! For a user reference, see [str](../page/Ref/str.html).
    !-------------------------------------------------------------------------------------------------------------------
#ifdef IOFL_HAS_R128
    pure recursive module function str_from_c128(x, locale, fmt, decimals, im) result(x_str)
      complex(r128),    intent(in)           :: x
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
      character(len=:), allocatable          :: x_str
    end function str_from_c128
#endif

    pure recursive module function str_from_c64(x, locale, fmt, decimals, im) result(x_str)
      complex(r64),    intent(in)           :: x
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
      character(len=:), allocatable          :: x_str
    end function str_from_c64

    pure recursive module function str_from_c32(x, locale, fmt, decimals, im) result(x_str)
      complex(r32),    intent(in)           :: x
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
      character(len=:), allocatable          :: x_str
    end function str_from_c32


#ifdef IOFL_HAS_R128
    pure recursive module function str_from_r128(x, locale, fmt, decimals) result(x_str)
      real(r128),       intent(in)           :: x
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=:), allocatable          :: x_str
    end function str_from_r128
#endif

    pure recursive module function str_from_r64(x, locale, fmt, decimals) result(x_str)
      real(r64),       intent(in)           :: x
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=:), allocatable          :: x_str
    end function str_from_r64

    pure recursive module function str_from_r32(x, locale, fmt, decimals) result(x_str)
      real(r32),       intent(in)           :: x
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=:), allocatable          :: x_str
    end function str_from_r32


    pure recursive module function str_from_i64(x, fmt) result(x_str)
      integer(i64),     intent(in)           :: x
      character(len=*), intent(in), optional :: fmt
      character(len=:), allocatable          :: x_str
    end function str_from_i64

    pure recursive module function str_from_i32(x, fmt) result(x_str)
      integer(i32),     intent(in)           :: x
      character(len=*), intent(in), optional :: fmt
      character(len=:), allocatable          :: x_str
    end function str_from_i32

    pure recursive module function str_from_i16(x, fmt) result(x_str)
      integer(i16),     intent(in)           :: x
      character(len=*), intent(in), optional :: fmt
      character(len=:), allocatable          :: x_str
    end function str_from_i16

    pure recursive module function str_from_i8(x, fmt) result(x_str)
      integer(i8),     intent(in)           :: x
      character(len=*), intent(in), optional :: fmt
      character(len=:), allocatable          :: x_str
    end function str_from_i8


    pure recursive module function str_from_string(x) result(x_str)
      class(String),    intent(in)  :: x
      character(len=:), allocatable :: x_str
    end function str_from_string
    pure recursive module function str_from_char(x) result(x_str)
      character(len=*), intent(in)  :: x
      character(len=:), allocatable :: x_str
    end function str_from_char
    pure recursive module function str_from_empty() result(x_str)
      character(len=:), allocatable :: x_str
    end function str_from_empty
  end interface str

  interface cast                                                                                 ! Submodule io_internal
    !-------------------------------------------------------------------------------------------------------------------
    !! Subroutine for casting between numeric and string data.
    !!
    !! For a user reference, see [cast](../page/Ref/cast.html).
    !-------------------------------------------------------------------------------------------------------------------
#ifdef IOFL_HAS_R128
    elemental recursive module subroutine cast_c128_to_string(x, into, locale, fmt, decimals, im)
      complex(r128),    intent(in)           :: x
      type(String),     intent(inout)        :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
    end subroutine cast_c128_to_string
#endif

    elemental recursive module subroutine cast_c64_to_string(x, into, locale, fmt, decimals, im)
      complex(r64),    intent(in)           :: x
      type(String),     intent(inout)        :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
    end subroutine cast_c64_to_string

    elemental recursive module subroutine cast_c32_to_string(x, into, locale, fmt, decimals, im)
      complex(r32),    intent(in)           :: x
      type(String),     intent(inout)        :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
    end subroutine cast_c32_to_string


#ifdef IOFL_HAS_R128
    elemental recursive module subroutine cast_r128_to_string(x, into, locale, fmt, decimals)
      real(r128),       intent(in)           :: x
      type(String),     intent(inout)        :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
    end subroutine cast_r128_to_string
#endif

    elemental recursive module subroutine cast_r64_to_string(x, into, locale, fmt, decimals)
      real(r64),       intent(in)           :: x
      type(String),     intent(inout)        :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
    end subroutine cast_r64_to_string

    elemental recursive module subroutine cast_r32_to_string(x, into, locale, fmt, decimals)
      real(r32),       intent(in)           :: x
      type(String),     intent(inout)        :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
    end subroutine cast_r32_to_string


    elemental recursive module subroutine cast_i64_to_string(x, into, fmt)
      integer(i64),     intent(in)           :: x
      type(String),     intent(inout)        :: into
      character(len=*), intent(in), optional :: fmt
    end subroutine cast_i64_to_string

    elemental recursive module subroutine cast_i32_to_string(x, into, fmt)
      integer(i32),     intent(in)           :: x
      type(String),     intent(inout)        :: into
      character(len=*), intent(in), optional :: fmt
    end subroutine cast_i32_to_string

    elemental recursive module subroutine cast_i16_to_string(x, into, fmt)
      integer(i16),     intent(in)           :: x
      type(String),     intent(inout)        :: into
      character(len=*), intent(in), optional :: fmt
    end subroutine cast_i16_to_string

    elemental recursive module subroutine cast_i8_to_string(x, into, fmt)
      integer(i8),     intent(in)           :: x
      type(String),     intent(inout)        :: into
      character(len=*), intent(in), optional :: fmt
    end subroutine cast_i8_to_string


#ifdef IOFL_HAS_R128
    pure recursive module subroutine cast_c128_to_char(x, into, locale, fmt, decimals, im)
      complex(r128),    intent(in)                 :: x
      character(len=:), intent(inout), allocatable :: into
      character(len=*), intent(in),    optional    :: locale
      character(len=*), intent(in),    optional    :: fmt
      integer,          intent(in),    optional    :: decimals
      character(len=*), intent(in),    optional    :: im
    end subroutine cast_c128_to_char
#endif

    pure recursive module subroutine cast_c64_to_char(x, into, locale, fmt, decimals, im)
      complex(r64),    intent(in)                 :: x
      character(len=:), intent(inout), allocatable :: into
      character(len=*), intent(in),    optional    :: locale
      character(len=*), intent(in),    optional    :: fmt
      integer,          intent(in),    optional    :: decimals
      character(len=*), intent(in),    optional    :: im
    end subroutine cast_c64_to_char

    pure recursive module subroutine cast_c32_to_char(x, into, locale, fmt, decimals, im)
      complex(r32),    intent(in)                 :: x
      character(len=:), intent(inout), allocatable :: into
      character(len=*), intent(in),    optional    :: locale
      character(len=*), intent(in),    optional    :: fmt
      integer,          intent(in),    optional    :: decimals
      character(len=*), intent(in),    optional    :: im
    end subroutine cast_c32_to_char


#ifdef IOFL_HAS_R128
    pure recursive module subroutine cast_r128_to_char(x, into, locale, fmt, decimals)
      real(r128),       intent(in)                 :: x
      character(len=:), intent(inout), allocatable :: into
      character(len=*), intent(in),    optional    :: locale
      character(len=*), intent(in),    optional    :: fmt
      integer,          intent(in),    optional    :: decimals
    end subroutine cast_r128_to_char
#endif

    pure recursive module subroutine cast_r64_to_char(x, into, locale, fmt, decimals)
      real(r64),       intent(in)                 :: x
      character(len=:), intent(inout), allocatable :: into
      character(len=*), intent(in),    optional    :: locale
      character(len=*), intent(in),    optional    :: fmt
      integer,          intent(in),    optional    :: decimals
    end subroutine cast_r64_to_char

    pure recursive module subroutine cast_r32_to_char(x, into, locale, fmt, decimals)
      real(r32),       intent(in)                 :: x
      character(len=:), intent(inout), allocatable :: into
      character(len=*), intent(in),    optional    :: locale
      character(len=*), intent(in),    optional    :: fmt
      integer,          intent(in),    optional    :: decimals
    end subroutine cast_r32_to_char


    pure recursive module subroutine cast_i64_to_char(x, into, fmt)
      integer(i64),     intent(in)                 :: x
      character(len=:), intent(inout), allocatable :: into
      character(len=*), intent(in),    optional    :: fmt
    end subroutine cast_i64_to_char

    pure recursive module subroutine cast_i32_to_char(x, into, fmt)
      integer(i32),     intent(in)                 :: x
      character(len=:), intent(inout), allocatable :: into
      character(len=*), intent(in),    optional    :: fmt
    end subroutine cast_i32_to_char

    pure recursive module subroutine cast_i16_to_char(x, into, fmt)
      integer(i16),     intent(in)                 :: x
      character(len=:), intent(inout), allocatable :: into
      character(len=*), intent(in),    optional    :: fmt
    end subroutine cast_i16_to_char

    pure recursive module subroutine cast_i8_to_char(x, into, fmt)
      integer(i8),     intent(in)                 :: x
      character(len=:), intent(inout), allocatable :: into
      character(len=*), intent(in),    optional    :: fmt
    end subroutine cast_i8_to_char


#ifdef IOFL_HAS_R128
    elemental recursive module subroutine cast_string_to_c128(substring, into, locale, fmt, im, stat)
      class(String),    intent(in)           :: substring
      complex(r128),    intent(out)          :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      character(len=*), intent(in), optional :: im
      integer,          intent(out), optional :: stat
    end subroutine cast_string_to_c128
#endif

    elemental recursive module subroutine cast_string_to_c64(substring, into, locale, fmt, im, stat)
      class(String),    intent(in)           :: substring
      complex(r64),    intent(out)          :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      character(len=*), intent(in), optional :: im
      integer,          intent(out), optional :: stat
    end subroutine cast_string_to_c64

    elemental recursive module subroutine cast_string_to_c32(substring, into, locale, fmt, im, stat)
      class(String),    intent(in)           :: substring
      complex(r32),    intent(out)          :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      character(len=*), intent(in), optional :: im
      integer,          intent(out), optional :: stat
    end subroutine cast_string_to_c32


#ifdef IOFL_HAS_R128
    elemental recursive module subroutine cast_string_to_r128(substring, into, locale, fmt, stat)
      class(String),    intent(in)           :: substring
      real(r128),       intent(out)          :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(out), optional :: stat
    end subroutine cast_string_to_r128
#endif

    elemental recursive module subroutine cast_string_to_r64(substring, into, locale, fmt, stat)
      class(String),    intent(in)           :: substring
      real(r64),       intent(out)          :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(out), optional :: stat
    end subroutine cast_string_to_r64

    elemental recursive module subroutine cast_string_to_r32(substring, into, locale, fmt, stat)
      class(String),    intent(in)           :: substring
      real(r32),       intent(out)          :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(out), optional :: stat
    end subroutine cast_string_to_r32


    elemental recursive module subroutine cast_string_to_i64(substring, into, fmt, stat)
      class(String),    intent(in)           :: substring
      integer(i64),     intent(out)          :: into
      character(len=*), intent(in), optional :: fmt
      integer,          intent(out), optional :: stat
    end subroutine cast_string_to_i64

    elemental recursive module subroutine cast_string_to_i32(substring, into, fmt, stat)
      class(String),    intent(in)           :: substring
      integer(i32),     intent(out)          :: into
      character(len=*), intent(in), optional :: fmt
      integer,          intent(out), optional :: stat
    end subroutine cast_string_to_i32

    elemental recursive module subroutine cast_string_to_i16(substring, into, fmt, stat)
      class(String),    intent(in)           :: substring
      integer(i16),     intent(out)          :: into
      character(len=*), intent(in), optional :: fmt
      integer,          intent(out), optional :: stat
    end subroutine cast_string_to_i16

    elemental recursive module subroutine cast_string_to_i8(substring, into, fmt, stat)
      class(String),    intent(in)           :: substring
      integer(i8),     intent(out)          :: into
      character(len=*), intent(in), optional :: fmt
      integer,          intent(out), optional :: stat
    end subroutine cast_string_to_i8


#ifdef IOFL_HAS_R128
    pure recursive module subroutine cast_char_to_c128(substring, into, locale, fmt, im, stat)
      character(len=*), intent(in)           :: substring
      complex(r128),    intent(out)          :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      character(len=*), intent(in), optional :: im
      integer,          intent(out), optional :: stat
    end subroutine cast_char_to_c128
#endif

    pure recursive module subroutine cast_char_to_c64(substring, into, locale, fmt, im, stat)
      character(len=*), intent(in)           :: substring
      complex(r64),    intent(out)          :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      character(len=*), intent(in), optional :: im
      integer,          intent(out), optional :: stat
    end subroutine cast_char_to_c64

    pure recursive module subroutine cast_char_to_c32(substring, into, locale, fmt, im, stat)
      character(len=*), intent(in)           :: substring
      complex(r32),    intent(out)          :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      character(len=*), intent(in), optional :: im
      integer,          intent(out), optional :: stat
    end subroutine cast_char_to_c32


#ifdef IOFL_HAS_R128
    pure recursive module subroutine cast_char_to_r128(substring, into, locale, fmt, stat)
      character(len=*), intent(in)           :: substring
      real(r128),       intent(out)          :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(out), optional :: stat
    end subroutine cast_char_to_r128
#endif

    pure recursive module subroutine cast_char_to_r64(substring, into, locale, fmt, stat)
      character(len=*), intent(in)           :: substring
      real(r64),       intent(out)          :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(out), optional :: stat
    end subroutine cast_char_to_r64

    pure recursive module subroutine cast_char_to_r32(substring, into, locale, fmt, stat)
      character(len=*), intent(in)           :: substring
      real(r32),       intent(out)          :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(out), optional :: stat
    end subroutine cast_char_to_r32


    pure recursive module subroutine cast_char_to_i64(substring, into, fmt, stat)
      character(len=*), intent(in)           :: substring
      integer(i64),     intent(out)          :: into
      character(len=*), intent(in), optional :: fmt
      integer,          intent(out), optional :: stat
    end subroutine cast_char_to_i64

    pure recursive module subroutine cast_char_to_i32(substring, into, fmt, stat)
      character(len=*), intent(in)           :: substring
      integer(i32),     intent(out)          :: into
      character(len=*), intent(in), optional :: fmt
      integer,          intent(out), optional :: stat
    end subroutine cast_char_to_i32

    pure recursive module subroutine cast_char_to_i16(substring, into, fmt, stat)
      character(len=*), intent(in)           :: substring
      integer(i16),     intent(out)          :: into
      character(len=*), intent(in), optional :: fmt
      integer,          intent(out), optional :: stat
    end subroutine cast_char_to_i16

    pure recursive module subroutine cast_char_to_i8(substring, into, fmt, stat)
      character(len=*), intent(in)           :: substring
      integer(i8),     intent(out)          :: into
      character(len=*), intent(in), optional :: fmt
      integer,          intent(out), optional :: stat
    end subroutine cast_char_to_i8

  end interface cast

  interface join                                                                                  ! Submodule io_join_split
    !-------------------------------------------------------------------------------------------------------------------
    !! Function for joining a vector of `tokens` into a scalar `character` or `String`.
    !!
    !! For a user reference, see [join](../page/Ref/join-split.html).
    !-------------------------------------------------------------------------------------------------------------------
    pure recursive module function join_char(tokens, separator) result(new)
      character(len=*), intent(in)              :: tokens(:)
      character(len=*), intent(in), optional    :: separator
      character(len=:),             allocatable :: new
    end function join_char

    pure recursive type(String) module function join_string(tokens, separator) result(new)
      type(String),     intent(in)           :: tokens(:)
      character(len=*), intent(in), optional :: separator
    end function join_string
  end interface join

  interface split                                                                                 ! Submodule io_join_split
    !-------------------------------------------------------------------------------------------------------------------
    !! Function for splitting a scalar `character` or `String` into a vector of `tokens`.
    !!
    !! For a user reference, see [split](../page/Ref/join-split.html).
    !-------------------------------------------------------------------------------------------------------------------
    pure recursive module function split_char(substring, separator) result(tokens)
      character(len=*), intent(in)              :: substring
      character(len=*), intent(in), optional    :: separator
      type(String),                 allocatable :: tokens(:)
    end function split_char

    pure recursive module function split_string(substring, separator) result(tokens)
      class(String),    intent(in)              :: substring
      character(len=*), intent(in), optional    :: separator
      type(String),                 allocatable :: tokens(:)
    end function split_string
  end interface split

  interface                                                                                          ! Submodule io_file
    !-------------------------------------------------------------------------------------------------------------------
    ! Function for parsing a file name for an extension
    !-------------------------------------------------------------------------------------------------------------------
    pure recursive module function ext_of(file) result(ext)
      character(len=*), intent(in)  :: file
      character(len=:), allocatable :: ext
    end function ext_of

    pure recursive module function format_of(file, format) result(file_format)
      !-----------------------------------------------------------------------------------------------------------------
      !! Returns "text", "binary" or "npy" from `format` if present, or else from the extension of `file`, or "" if
      !! neither is valid.
      !-----------------------------------------------------------------------------------------------------------------
      character(len=*), intent(in)           :: file
      character(len=*), intent(in), optional :: format
      character(len=:), allocatable          :: file_format
    end function format_of

    pure recursive module function default_delim(file, locale) result(delim)
      !-----------------------------------------------------------------------------------------------------------------
      !! The delimiter for a text file when none is given: a tab for a `.tsv` file, and otherwise a comma, or a
      !! semicolon for the `"EU"` locale, whose decimal mark is a comma.
      !-----------------------------------------------------------------------------------------------------------------
      character(len=*), intent(in)  :: file, locale
      character(len=:), allocatable :: delim
    end function default_delim
  end interface

  interface to_file                                                                                  ! Submodule io_file
    !-------------------------------------------------------------------------------------------------------------------
    !! Subroutine for writing an array of uniform numeric data type to an external file.
    !!
    !! For a user reference, see [to_file](../page/Ref/to_file.html).
    !-------------------------------------------------------------------------------------------------------------------
#ifdef IOFL_HAS_R128
    recursive module subroutine to_file_c128(x, file, header, locale, delim, fmt, decimals, im, stat, errmsg, &
                                              format)
      complex(r128),    intent(in)                    :: x(..)
      character(len=*), intent(in)                    :: file
      character(len=*), intent(in),  optional :: header(:)
      character(len=*), intent(in),  optional :: locale
      character(len=*), intent(in),  optional :: delim
      character(len=*), intent(in),  optional :: fmt
      integer,          intent(in),  optional :: decimals
      character(len=*), intent(in),  optional :: im
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine to_file_c128
#endif

    recursive module subroutine to_file_c64(x, file, header, locale, delim, fmt, decimals, im, stat, errmsg, &
                                              format)
      complex(r64),    intent(in)                    :: x(..)
      character(len=*), intent(in)                    :: file
      character(len=*), intent(in),  optional :: header(:)
      character(len=*), intent(in),  optional :: locale
      character(len=*), intent(in),  optional :: delim
      character(len=*), intent(in),  optional :: fmt
      integer,          intent(in),  optional :: decimals
      character(len=*), intent(in),  optional :: im
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine to_file_c64

    recursive module subroutine to_file_c32(x, file, header, locale, delim, fmt, decimals, im, stat, errmsg, &
                                              format)
      complex(r32),    intent(in)                    :: x(..)
      character(len=*), intent(in)                    :: file
      character(len=*), intent(in),  optional :: header(:)
      character(len=*), intent(in),  optional :: locale
      character(len=*), intent(in),  optional :: delim
      character(len=*), intent(in),  optional :: fmt
      integer,          intent(in),  optional :: decimals
      character(len=*), intent(in),  optional :: im
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine to_file_c32


#ifdef IOFL_HAS_R128
    recursive module subroutine to_file_r128(x, file, header, locale, delim, fmt, decimals, stat, errmsg, format)
      real(r128),       intent(in)                    :: x(..)
      character(len=*), intent(in)                    :: file
      character(len=*), intent(in),  optional :: header(:)
      character(len=*), intent(in),  optional :: locale
      character(len=*), intent(in),  optional :: delim
      character(len=*), intent(in),  optional :: fmt
      integer,          intent(in),  optional :: decimals
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine to_file_r128
#endif

    recursive module subroutine to_file_r64(x, file, header, locale, delim, fmt, decimals, stat, errmsg, format)
      real(r64),       intent(in)                    :: x(..)
      character(len=*), intent(in)                    :: file
      character(len=*), intent(in),  optional :: header(:)
      character(len=*), intent(in),  optional :: locale
      character(len=*), intent(in),  optional :: delim
      character(len=*), intent(in),  optional :: fmt
      integer,          intent(in),  optional :: decimals
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine to_file_r64

    recursive module subroutine to_file_r32(x, file, header, locale, delim, fmt, decimals, stat, errmsg, format)
      real(r32),       intent(in)                    :: x(..)
      character(len=*), intent(in)                    :: file
      character(len=*), intent(in),  optional :: header(:)
      character(len=*), intent(in),  optional :: locale
      character(len=*), intent(in),  optional :: delim
      character(len=*), intent(in),  optional :: fmt
      integer,          intent(in),  optional :: decimals
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine to_file_r32


    recursive module subroutine to_file_i64(x, file, header, delim, fmt, stat, errmsg, format)
      integer(i64),     intent(in)                    :: x(..)
      character(len=*), intent(in)                    :: file
      character(len=*), intent(in),  optional :: header(:)
      character(len=*), intent(in),  optional :: delim
      character(len=*), intent(in),  optional :: fmt
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine to_file_i64

    recursive module subroutine to_file_i32(x, file, header, delim, fmt, stat, errmsg, format)
      integer(i32),     intent(in)                    :: x(..)
      character(len=*), intent(in)                    :: file
      character(len=*), intent(in),  optional :: header(:)
      character(len=*), intent(in),  optional :: delim
      character(len=*), intent(in),  optional :: fmt
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine to_file_i32

    recursive module subroutine to_file_i16(x, file, header, delim, fmt, stat, errmsg, format)
      integer(i16),     intent(in)                    :: x(..)
      character(len=*), intent(in)                    :: file
      character(len=*), intent(in),  optional :: header(:)
      character(len=*), intent(in),  optional :: delim
      character(len=*), intent(in),  optional :: fmt
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine to_file_i16

    recursive module subroutine to_file_i8(x, file, header, delim, fmt, stat, errmsg, format)
      integer(i8),     intent(in)                    :: x(..)
      character(len=*), intent(in)                    :: file
      character(len=*), intent(in),  optional :: header(:)
      character(len=*), intent(in),  optional :: delim
      character(len=*), intent(in),  optional :: fmt
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine to_file_i8

  end interface to_file

  interface from_file                                                                                ! Submodule io_file
    !-------------------------------------------------------------------------------------------------------------------
    !! Subroutine for reading an external file of uniform numeric data type and format into an array.
    !!
    !! For a user reference, see [from_file](../page/Ref/from_file.html).
    !-------------------------------------------------------------------------------------------------------------------
#ifdef IOFL_HAS_R128
    recursive module subroutine from_textfile_c128(file, into, header, locale, delim, fmt, im, stat, errmsg, &
                                              format)
      character(len=*), intent(in)                    :: file
      complex(r128),    intent(out), allocatable      :: into(..)
      logical,          intent(in),  optional :: header
      character(len=*), intent(in),  optional :: locale
      character(len=*), intent(in),  optional :: delim
      character(len=*), intent(in),  optional :: fmt
      character(len=*), intent(in),  optional :: im
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine from_textfile_c128
#endif

    recursive module subroutine from_textfile_c64(file, into, header, locale, delim, fmt, im, stat, errmsg, &
                                              format)
      character(len=*), intent(in)                    :: file
      complex(r64),    intent(out), allocatable      :: into(..)
      logical,          intent(in),  optional :: header
      character(len=*), intent(in),  optional :: locale
      character(len=*), intent(in),  optional :: delim
      character(len=*), intent(in),  optional :: fmt
      character(len=*), intent(in),  optional :: im
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine from_textfile_c64

    recursive module subroutine from_textfile_c32(file, into, header, locale, delim, fmt, im, stat, errmsg, &
                                              format)
      character(len=*), intent(in)                    :: file
      complex(r32),    intent(out), allocatable      :: into(..)
      logical,          intent(in),  optional :: header
      character(len=*), intent(in),  optional :: locale
      character(len=*), intent(in),  optional :: delim
      character(len=*), intent(in),  optional :: fmt
      character(len=*), intent(in),  optional :: im
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine from_textfile_c32


#ifdef IOFL_HAS_R128
    recursive module subroutine from_binaryfile_c128(file, into, data_shape, stat, errmsg, format)
      character(len=*), intent(in)                    :: file
      complex(r128),    intent(out), allocatable      :: into(..)
      integer,          intent(in)                    :: data_shape(:)
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine from_binaryfile_c128
#endif

    recursive module subroutine from_binaryfile_c64(file, into, data_shape, stat, errmsg, format)
      character(len=*), intent(in)                    :: file
      complex(r64),    intent(out), allocatable      :: into(..)
      integer,          intent(in)                    :: data_shape(:)
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine from_binaryfile_c64

    recursive module subroutine from_binaryfile_c32(file, into, data_shape, stat, errmsg, format)
      character(len=*), intent(in)                    :: file
      complex(r32),    intent(out), allocatable      :: into(..)
      integer,          intent(in)                    :: data_shape(:)
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine from_binaryfile_c32


#ifdef IOFL_HAS_R128
    recursive module subroutine from_textfile_r128(file, into, header, locale, delim, fmt, stat, errmsg, format)
      character(len=*), intent(in)                    :: file
      real(r128),       intent(out), allocatable      :: into(..)
      logical,          intent(in),  optional :: header
      character(len=*), intent(in),  optional :: locale
      character(len=*), intent(in),  optional :: delim
      character(len=*), intent(in),  optional :: fmt
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine from_textfile_r128
#endif

    recursive module subroutine from_textfile_r64(file, into, header, locale, delim, fmt, stat, errmsg, format)
      character(len=*), intent(in)                    :: file
      real(r64),       intent(out), allocatable      :: into(..)
      logical,          intent(in),  optional :: header
      character(len=*), intent(in),  optional :: locale
      character(len=*), intent(in),  optional :: delim
      character(len=*), intent(in),  optional :: fmt
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine from_textfile_r64

    recursive module subroutine from_textfile_r32(file, into, header, locale, delim, fmt, stat, errmsg, format)
      character(len=*), intent(in)                    :: file
      real(r32),       intent(out), allocatable      :: into(..)
      logical,          intent(in),  optional :: header
      character(len=*), intent(in),  optional :: locale
      character(len=*), intent(in),  optional :: delim
      character(len=*), intent(in),  optional :: fmt
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine from_textfile_r32


#ifdef IOFL_HAS_R128
    recursive module subroutine from_binaryfile_r128(file, into, data_shape, stat, errmsg, format)
      character(len=*), intent(in)                    :: file
      real(r128),       intent(out), allocatable      :: into(..)
      integer,          intent(in)                    :: data_shape(:)
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine from_binaryfile_r128
#endif

    recursive module subroutine from_binaryfile_r64(file, into, data_shape, stat, errmsg, format)
      character(len=*), intent(in)                    :: file
      real(r64),       intent(out), allocatable      :: into(..)
      integer,          intent(in)                    :: data_shape(:)
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine from_binaryfile_r64

    recursive module subroutine from_binaryfile_r32(file, into, data_shape, stat, errmsg, format)
      character(len=*), intent(in)                    :: file
      real(r32),       intent(out), allocatable      :: into(..)
      integer,          intent(in)                    :: data_shape(:)
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine from_binaryfile_r32


    recursive module subroutine from_textfile_i64(file, into, header, delim, fmt, stat, errmsg, format)
      character(len=*), intent(in)                    :: file
      integer(i64),     intent(out), allocatable      :: into(..)
      logical,          intent(in),  optional :: header
      character(len=*), intent(in),  optional :: delim
      character(len=*), intent(in),  optional :: fmt
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine from_textfile_i64

    recursive module subroutine from_textfile_i32(file, into, header, delim, fmt, stat, errmsg, format)
      character(len=*), intent(in)                    :: file
      integer(i32),     intent(out), allocatable      :: into(..)
      logical,          intent(in),  optional :: header
      character(len=*), intent(in),  optional :: delim
      character(len=*), intent(in),  optional :: fmt
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine from_textfile_i32

    recursive module subroutine from_textfile_i16(file, into, header, delim, fmt, stat, errmsg, format)
      character(len=*), intent(in)                    :: file
      integer(i16),     intent(out), allocatable      :: into(..)
      logical,          intent(in),  optional :: header
      character(len=*), intent(in),  optional :: delim
      character(len=*), intent(in),  optional :: fmt
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine from_textfile_i16

    recursive module subroutine from_textfile_i8(file, into, header, delim, fmt, stat, errmsg, format)
      character(len=*), intent(in)                    :: file
      integer(i8),     intent(out), allocatable      :: into(..)
      logical,          intent(in),  optional :: header
      character(len=*), intent(in),  optional :: delim
      character(len=*), intent(in),  optional :: fmt
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine from_textfile_i8


    recursive module subroutine from_binaryfile_i64(file, into, data_shape, stat, errmsg, format)
      character(len=*), intent(in)                    :: file
      integer(i64),     intent(out), allocatable      :: into(..)
      integer,          intent(in)                    :: data_shape(:)
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine from_binaryfile_i64

    recursive module subroutine from_binaryfile_i32(file, into, data_shape, stat, errmsg, format)
      character(len=*), intent(in)                    :: file
      integer(i32),     intent(out), allocatable      :: into(..)
      integer,          intent(in)                    :: data_shape(:)
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine from_binaryfile_i32

    recursive module subroutine from_binaryfile_i16(file, into, data_shape, stat, errmsg, format)
      character(len=*), intent(in)                    :: file
      integer(i16),     intent(out), allocatable      :: into(..)
      integer,          intent(in)                    :: data_shape(:)
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine from_binaryfile_i16

    recursive module subroutine from_binaryfile_i8(file, into, data_shape, stat, errmsg, format)
      character(len=*), intent(in)                    :: file
      integer(i8),     intent(out), allocatable      :: into(..)
      integer,          intent(in)                    :: data_shape(:)
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine from_binaryfile_i8

  end interface from_file

  interface echo                                                                                     ! Submodule io_text
    !-------------------------------------------------------------------------------------------------------------------
    !! Subroutine for writing a scalar `character` or `String` to an external text file.
    !!
    !! For a user reference, see [echo](../page/Ref/echo.html).
    !-------------------------------------------------------------------------------------------------------------------
    recursive module subroutine echo_chars(substring, file, append, terminator, stat, errmsg, format)
      character(len=*), intent(in)                    :: substring
      character(len=*), intent(in)                    :: file
      logical,          intent(in),  optional :: append
      character(len=*), intent(in),  optional :: terminator
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine echo_chars

    recursive module subroutine echo_string(substring, file, append, terminator, stat, errmsg, format)
      class(String),    intent(in)                    :: substring
      character(len=*), intent(in)                    :: file
      logical,          intent(in),  optional :: append
      character(len=*), intent(in),  optional :: terminator
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
      character(len=*), intent(in), optional :: format
    end subroutine echo_string
  end interface echo

  interface to_text                                                                                  ! Submodule io_text
    !-------------------------------------------------------------------------------------------------------------------
    !! Private interface for writing an array to an external text file.
    !-------------------------------------------------------------------------------------------------------------------
#ifdef IOFL_HAS_R128
    recursive module subroutine to_text_c128(x, file, header, locale, delim, fmt, decimals, im, stat, errmsg)
      complex(r128),    intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      character(len=*), intent(in), optional  :: header(:)
      character(len=*), intent(in)  :: locale
      character(len=*), intent(in)  :: delim
      character(len=*), intent(in)  :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in)  :: im
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_text_c128
#endif

    recursive module subroutine to_text_c64(x, file, header, locale, delim, fmt, decimals, im, stat, errmsg)
      complex(r64),    intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      character(len=*), intent(in), optional  :: header(:)
      character(len=*), intent(in)  :: locale
      character(len=*), intent(in)  :: delim
      character(len=*), intent(in)  :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in)  :: im
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_text_c64

    recursive module subroutine to_text_c32(x, file, header, locale, delim, fmt, decimals, im, stat, errmsg)
      complex(r32),    intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      character(len=*), intent(in), optional  :: header(:)
      character(len=*), intent(in)  :: locale
      character(len=*), intent(in)  :: delim
      character(len=*), intent(in)  :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in)  :: im
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_text_c32


#ifdef IOFL_HAS_R128
    recursive module subroutine to_text_r128(x, file, header, locale, delim, fmt, decimals, stat, errmsg)
      real(r128),       intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      character(len=*), intent(in), optional  :: header(:)
      character(len=*), intent(in)  :: locale
      character(len=*), intent(in)  :: delim
      character(len=*), intent(in)  :: fmt
      integer,          intent(in), optional :: decimals
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_text_r128
#endif

    recursive module subroutine to_text_r64(x, file, header, locale, delim, fmt, decimals, stat, errmsg)
      real(r64),       intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      character(len=*), intent(in), optional  :: header(:)
      character(len=*), intent(in)  :: locale
      character(len=*), intent(in)  :: delim
      character(len=*), intent(in)  :: fmt
      integer,          intent(in), optional :: decimals
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_text_r64

    recursive module subroutine to_text_r32(x, file, header, locale, delim, fmt, decimals, stat, errmsg)
      real(r32),       intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      character(len=*), intent(in), optional  :: header(:)
      character(len=*), intent(in)  :: locale
      character(len=*), intent(in)  :: delim
      character(len=*), intent(in)  :: fmt
      integer,          intent(in), optional :: decimals
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_text_r32


    recursive module subroutine to_text_i64(x, file, header, delim, fmt, stat, errmsg)
      integer(i64),     intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      character(len=*), intent(in), optional  :: header(:)
      character(len=*), intent(in)  :: delim
      character(len=*), intent(in)  :: fmt
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_text_i64

    recursive module subroutine to_text_i32(x, file, header, delim, fmt, stat, errmsg)
      integer(i32),     intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      character(len=*), intent(in), optional  :: header(:)
      character(len=*), intent(in)  :: delim
      character(len=*), intent(in)  :: fmt
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_text_i32

    recursive module subroutine to_text_i16(x, file, header, delim, fmt, stat, errmsg)
      integer(i16),     intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      character(len=*), intent(in), optional  :: header(:)
      character(len=*), intent(in)  :: delim
      character(len=*), intent(in)  :: fmt
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_text_i16

    recursive module subroutine to_text_i8(x, file, header, delim, fmt, stat, errmsg)
      integer(i8),     intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      character(len=*), intent(in), optional  :: header(:)
      character(len=*), intent(in)  :: delim
      character(len=*), intent(in)  :: fmt
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_text_i8

  end interface to_text

  interface from_text                                                                                ! Submodule io_text
    !-------------------------------------------------------------------------------------------------------------------
    !! Private interface for reading an external text file into an array.
    !-------------------------------------------------------------------------------------------------------------------
#ifdef IOFL_HAS_R128
    recursive module subroutine from_text_c128(file, into, header, locale, delim, fmt, im, stat, errmsg)
      character(len=*), intent(in)               :: file
      complex(r128),    intent(out), allocatable :: into(..)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: locale
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
      character(len=*), intent(in)               :: im
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_text_c128
#endif

    recursive module subroutine from_text_c64(file, into, header, locale, delim, fmt, im, stat, errmsg)
      character(len=*), intent(in)               :: file
      complex(r64),    intent(out), allocatable :: into(..)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: locale
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
      character(len=*), intent(in)               :: im
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_text_c64

    recursive module subroutine from_text_c32(file, into, header, locale, delim, fmt, im, stat, errmsg)
      character(len=*), intent(in)               :: file
      complex(r32),    intent(out), allocatable :: into(..)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: locale
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
      character(len=*), intent(in)               :: im
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_text_c32


#ifdef IOFL_HAS_R128
    recursive module subroutine from_text_r128(file, into, header, locale, delim, fmt, stat, errmsg)
      character(len=*), intent(in)               :: file
      real(r128),       intent(out), allocatable :: into(..)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: locale
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_text_r128
#endif

    recursive module subroutine from_text_r64(file, into, header, locale, delim, fmt, stat, errmsg)
      character(len=*), intent(in)               :: file
      real(r64),       intent(out), allocatable :: into(..)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: locale
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_text_r64

    recursive module subroutine from_text_r32(file, into, header, locale, delim, fmt, stat, errmsg)
      character(len=*), intent(in)               :: file
      real(r32),       intent(out), allocatable :: into(..)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: locale
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_text_r32


    recursive module subroutine from_text_i64(file, into, header, delim, fmt, stat, errmsg)
      character(len=*), intent(in)               :: file
      integer(i64),     intent(out), allocatable :: into(..)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_text_i64

    recursive module subroutine from_text_i32(file, into, header, delim, fmt, stat, errmsg)
      character(len=*), intent(in)               :: file
      integer(i32),     intent(out), allocatable :: into(..)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_text_i32

    recursive module subroutine from_text_i16(file, into, header, delim, fmt, stat, errmsg)
      character(len=*), intent(in)               :: file
      integer(i16),     intent(out), allocatable :: into(..)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_text_i16

    recursive module subroutine from_text_i8(file, into, header, delim, fmt, stat, errmsg)
      character(len=*), intent(in)               :: file
      integer(i8),     intent(out), allocatable :: into(..)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_text_i8

  end interface from_text

  interface to_binary                                                                              ! Submodule io_binary
    !-------------------------------------------------------------------------------------------------------------------
    !! Private interface for writing an array to an external binary file.
    !-------------------------------------------------------------------------------------------------------------------
#ifdef IOFL_HAS_R128
    recursive module subroutine to_binary_c128(x, file, stat, errmsg)
      complex(r128),    intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_binary_c128
#endif

    recursive module subroutine to_binary_c64(x, file, stat, errmsg)
      complex(r64),    intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_binary_c64

    recursive module subroutine to_binary_c32(x, file, stat, errmsg)
      complex(r32),    intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_binary_c32


#ifdef IOFL_HAS_R128
    recursive module subroutine to_binary_r128(x, file, stat, errmsg)
      real(r128),       intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_binary_r128
#endif

    recursive module subroutine to_binary_r64(x, file, stat, errmsg)
      real(r64),       intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_binary_r64

    recursive module subroutine to_binary_r32(x, file, stat, errmsg)
      real(r32),       intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_binary_r32


    recursive module subroutine to_binary_i64(x, file, stat, errmsg)
      integer(i64),     intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_binary_i64

    recursive module subroutine to_binary_i32(x, file, stat, errmsg)
      integer(i32),     intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_binary_i32

    recursive module subroutine to_binary_i16(x, file, stat, errmsg)
      integer(i16),     intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_binary_i16

    recursive module subroutine to_binary_i8(x, file, stat, errmsg)
      integer(i8),     intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_binary_i8

  end interface to_binary

  interface from_binary                                                                            ! Submodule io_binary
    !-------------------------------------------------------------------------------------------------------------------
    !! Private interface for reading an external binary file into an array.
    !-------------------------------------------------------------------------------------------------------------------
#ifdef IOFL_HAS_R128
    recursive module subroutine from_binary_c128(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)               :: file
      complex(r128),    intent(out), allocatable :: into(..)
      integer,          intent(in)               :: data_shape(:)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_binary_c128
#endif

    recursive module subroutine from_binary_c64(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)               :: file
      complex(r64),    intent(out), allocatable :: into(..)
      integer,          intent(in)               :: data_shape(:)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_binary_c64

    recursive module subroutine from_binary_c32(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)               :: file
      complex(r32),    intent(out), allocatable :: into(..)
      integer,          intent(in)               :: data_shape(:)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_binary_c32


#ifdef IOFL_HAS_R128
    recursive module subroutine from_binary_r128(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)               :: file
      real(r128),       intent(out), allocatable :: into(..)
      integer,          intent(in)               :: data_shape(:)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_binary_r128
#endif

    recursive module subroutine from_binary_r64(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)               :: file
      real(r64),       intent(out), allocatable :: into(..)
      integer,          intent(in)               :: data_shape(:)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_binary_r64

    recursive module subroutine from_binary_r32(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)               :: file
      real(r32),       intent(out), allocatable :: into(..)
      integer,          intent(in)               :: data_shape(:)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_binary_r32


    recursive module subroutine from_binary_i64(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)               :: file
      integer(i64),     intent(out), allocatable :: into(..)
      integer,          intent(in)               :: data_shape(:)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_binary_i64

    recursive module subroutine from_binary_i32(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)               :: file
      integer(i32),     intent(out), allocatable :: into(..)
      integer,          intent(in)               :: data_shape(:)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_binary_i32

    recursive module subroutine from_binary_i16(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)               :: file
      integer(i16),     intent(out), allocatable :: into(..)
      integer,          intent(in)               :: data_shape(:)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_binary_i16

    recursive module subroutine from_binary_i8(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)               :: file
      integer(i8),     intent(out), allocatable :: into(..)
      integer,          intent(in)               :: data_shape(:)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_binary_i8

  end interface from_binary

  interface to_npy                                                                                    ! Submodule io_npy
    !-------------------------------------------------------------------------------------------------------------------
    !! Private interface for writing an array to a NumPy `.npy` file.
    !-------------------------------------------------------------------------------------------------------------------
    recursive module subroutine to_npy_c64(x, file, stat, errmsg)
      complex(r64), intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_npy_c64

    recursive module subroutine to_npy_c32(x, file, stat, errmsg)
      complex(r32), intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_npy_c32

    recursive module subroutine to_npy_r64(x, file, stat, errmsg)
      real(r64), intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_npy_r64

    recursive module subroutine to_npy_r32(x, file, stat, errmsg)
      real(r32), intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_npy_r32

    recursive module subroutine to_npy_i64(x, file, stat, errmsg)
      integer(i64), intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_npy_i64

    recursive module subroutine to_npy_i32(x, file, stat, errmsg)
      integer(i32), intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_npy_i32

    recursive module subroutine to_npy_i16(x, file, stat, errmsg)
      integer(i16), intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_npy_i16

    recursive module subroutine to_npy_i8(x, file, stat, errmsg)
      integer(i8), intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_npy_i8

  end interface to_npy

  interface from_npy                                                                                  ! Submodule io_npy
    !-------------------------------------------------------------------------------------------------------------------
    !! Private interface for reading a NumPy `.npy` file into an array, which takes its shape from the file.
    !-------------------------------------------------------------------------------------------------------------------
    recursive module subroutine from_npy_c64(file, into, stat, errmsg)
      character(len=*), intent(in)               :: file
      complex(r64), intent(out), allocatable :: into(..)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_npy_c64

    recursive module subroutine from_npy_c32(file, into, stat, errmsg)
      character(len=*), intent(in)               :: file
      complex(r32), intent(out), allocatable :: into(..)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_npy_c32

    recursive module subroutine from_npy_r64(file, into, stat, errmsg)
      character(len=*), intent(in)               :: file
      real(r64), intent(out), allocatable :: into(..)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_npy_r64

    recursive module subroutine from_npy_r32(file, into, stat, errmsg)
      character(len=*), intent(in)               :: file
      real(r32), intent(out), allocatable :: into(..)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_npy_r32

    recursive module subroutine from_npy_i64(file, into, stat, errmsg)
      character(len=*), intent(in)               :: file
      integer(i64), intent(out), allocatable :: into(..)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_npy_i64

    recursive module subroutine from_npy_i32(file, into, stat, errmsg)
      character(len=*), intent(in)               :: file
      integer(i32), intent(out), allocatable :: into(..)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_npy_i32

    recursive module subroutine from_npy_i16(file, into, stat, errmsg)
      character(len=*), intent(in)               :: file
      integer(i16), intent(out), allocatable :: into(..)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_npy_i16

    recursive module subroutine from_npy_i8(file, into, stat, errmsg)
      character(len=*), intent(in)               :: file
      integer(i8), intent(out), allocatable :: into(..)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_npy_i8

  end interface from_npy

  interface aprint                                                                            ! Submodule io_array_printing
    !-------------------------------------------------------------------------------------------------------------------
    !! Subroutine for printing arrays and array sections to stdout.
    !!
    !! For a user reference, see [aprint](../page/Ref/aprint.html).
    !-------------------------------------------------------------------------------------------------------------------
#ifdef IOFL_HAS_R128
    recursive module subroutine aprint_1dc128(x, fmt, decimals, im, unit)
      complex(r128),    intent(in)           :: x(:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
      integer,          intent(in), optional :: unit
    end subroutine aprint_1dc128
#endif

    recursive module subroutine aprint_1dc64(x, fmt, decimals, im, unit)
      complex(r64),    intent(in)           :: x(:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
      integer,          intent(in), optional :: unit
    end subroutine aprint_1dc64

    recursive module subroutine aprint_1dc32(x, fmt, decimals, im, unit)
      complex(r32),    intent(in)           :: x(:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
      integer,          intent(in), optional :: unit
    end subroutine aprint_1dc32


#ifdef IOFL_HAS_R128
    recursive module subroutine aprint_2dc128(x, fmt, decimals, im, unit)
      complex(r128),    intent(in)           :: x(:,:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
      integer,          intent(in), optional :: unit
    end subroutine aprint_2dc128
#endif

    recursive module subroutine aprint_2dc64(x, fmt, decimals, im, unit)
      complex(r64),    intent(in)           :: x(:,:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
      integer,          intent(in), optional :: unit
    end subroutine aprint_2dc64

    recursive module subroutine aprint_2dc32(x, fmt, decimals, im, unit)
      complex(r32),    intent(in)           :: x(:,:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
      integer,          intent(in), optional :: unit
    end subroutine aprint_2dc32


#ifdef IOFL_HAS_R128
    recursive module subroutine aprint_1dr128(x, fmt, decimals, unit)
      real(r128),       intent(in)           :: x(:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      integer,          intent(in), optional :: unit
    end subroutine aprint_1dr128
#endif

    recursive module subroutine aprint_1dr64(x, fmt, decimals, unit)
      real(r64),       intent(in)           :: x(:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      integer,          intent(in), optional :: unit
    end subroutine aprint_1dr64

    recursive module subroutine aprint_1dr32(x, fmt, decimals, unit)
      real(r32),       intent(in)           :: x(:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      integer,          intent(in), optional :: unit
    end subroutine aprint_1dr32


#ifdef IOFL_HAS_R128
    recursive module subroutine aprint_2dr128(x, fmt, decimals, unit)
      real(r128),       intent(in)           :: x(:,:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      integer,          intent(in), optional :: unit
    end subroutine aprint_2dr128
#endif

    recursive module subroutine aprint_2dr64(x, fmt, decimals, unit)
      real(r64),       intent(in)           :: x(:,:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      integer,          intent(in), optional :: unit
    end subroutine aprint_2dr64

    recursive module subroutine aprint_2dr32(x, fmt, decimals, unit)
      real(r32),       intent(in)           :: x(:,:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      integer,          intent(in), optional :: unit
    end subroutine aprint_2dr32


    recursive module subroutine aprint_1di64(x, fmt, unit)
      integer(i64),     intent(in)           :: x(:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: unit
    end subroutine aprint_1di64

    recursive module subroutine aprint_1di32(x, fmt, unit)
      integer(i32),     intent(in)           :: x(:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: unit
    end subroutine aprint_1di32

    recursive module subroutine aprint_1di16(x, fmt, unit)
      integer(i16),     intent(in)           :: x(:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: unit
    end subroutine aprint_1di16

    recursive module subroutine aprint_1di8(x, fmt, unit)
      integer(i8),     intent(in)           :: x(:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: unit
    end subroutine aprint_1di8


    recursive module subroutine aprint_2di64(x, fmt, unit)
      integer(i64),     intent(in)           :: x(:,:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: unit
    end subroutine aprint_2di64

    recursive module subroutine aprint_2di32(x, fmt, unit)
      integer(i32),     intent(in)           :: x(:,:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: unit
    end subroutine aprint_2di32

    recursive module subroutine aprint_2di16(x, fmt, unit)
      integer(i16),     intent(in)           :: x(:,:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: unit
    end subroutine aprint_2di16

    recursive module subroutine aprint_2di8(x, fmt, unit)
      integer(i8),     intent(in)           :: x(:,:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: unit
    end subroutine aprint_2di8


    recursive module subroutine aprint_1dchar(x, unit)
      character(len=*), intent(in) :: x(:)
      integer,          intent(in), optional :: unit
    end subroutine aprint_1dchar

    recursive module subroutine aprint_2dchar(x, unit)
      character(len=*), intent(in) :: x(:,:)
      integer,          intent(in), optional :: unit
    end subroutine aprint_2dchar

    recursive module subroutine aprint_1dString(x, unit)
      class(String), intent(in) :: x(:)
      integer,          intent(in), optional :: unit
    end subroutine aprint_1dString

    recursive module subroutine aprint_2dString(x, unit)
      class(String), intent(in) :: x(:,:)
      integer,          intent(in), optional :: unit
    end subroutine aprint_2dString
  end interface aprint

contains
  elemental function char_concat_plus(charsl, charsr) result(new)
    !-------------------------------------------------------------------------------------------------------------------
    !! Defined here rather than in submodule `operators` because ifx 2026.2 rejects this result length in the interface
    !! body of an elemental procedure (error #6598), while accepting it in an ordinary module procedure.
    !-------------------------------------------------------------------------------------------------------------------
    character(len=*), intent(in)           :: charsl, charsr
    character(len=len(charsl)+len(charsr)) :: new

    new = charsl//charsr
  end function char_concat_plus
end module io_fortran_lib
