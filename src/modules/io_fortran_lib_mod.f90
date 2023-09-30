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
  public :: NL, SPACE, CR, FF, VT, LF, TAB, HT, BELL, NUL, CNUL                              ! Constants
  public :: operator(//), operator(+), operator(-), operator(**), operator(==), operator(/=) ! Operators

  ! Definitions and Interfaces ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=1), parameter :: NL        = new_line("a") !! The newline character (system agnostic)
  character(len=1), parameter :: SPACE     = achar(32)     !! The space character
  character(len=1), parameter :: CR        = achar(13)     !! The carriage return character
  character(len=1), parameter :: FF        = achar(12)     !! The form feed character
  character(len=1), parameter :: VT        = achar(11)     !! The vertical tab character
  character(len=1), parameter :: LF        = achar(10)     !! The line feed character
  character(len=1), parameter :: TAB       = achar(9)      !! The horizontal tab character
  character(len=1), parameter :: HT        = achar(9)      !! The horizontal tab character (alternate name)
  character(len=1), parameter :: BELL      = achar(7)      !! The bell/alert character
  character(len=1), parameter :: NUL       = achar(0)      !! The null character
  character(len=1), parameter :: CNUL      = c_null_char   !! The C null character re-exported from iso_c_binding

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
  character(len=3), parameter :: TEXT_EXT(*)   = [ "csv", "txt", & ! Allowed text extensions
                                                   "log", "rtf", &
                                                   "odm", "odt", &
                                                   "ods", "odf", &
                                                   "xls", "doc", &
                                                   "org", "dbf", &
                                                   "bed", "gff", &
                                                   "gtf"         ]

  integer, parameter :: READ_ERR  = 1 ! Read error code
  integer, parameter :: WRITE_ERR = 2 ! Write error code
  integer, parameter :: ALLOC_ERR = 3 ! Allocation error code
  integer, parameter :: ARG_ERR   = 4 ! Argument error code


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
    character(len=:), allocatable :: s !! Component is an allocatable string
    contains
      private

      ! Generics ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      generic, public :: cast             => cast_string_to_c128, cast_string_to_c64, cast_string_to_c32, &
                                             cast_string_to_r128, cast_string_to_r64, cast_string_to_r32, &
                                             cast_string_to_i64, cast_string_to_i32, cast_string_to_i16, &
                                             cast_string_to_i8
      generic, public :: count            => count_substring_chars, count_substring_string
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
      procedure, pass(substring)    :: cast_string_to_c128, cast_string_to_c64, cast_string_to_c32, &
                                       cast_string_to_r128, cast_string_to_r64, cast_string_to_r32, &
                                       cast_string_to_i64, cast_string_to_i32, cast_string_to_i16, &
                                       cast_string_to_i8
      procedure, pass(self)         :: count_substring_chars, count_substring_string
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
      final                         :: scrub
  end type String

  interface                                                                                   ! Submodule string_methods
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

    pure elemental recursive integer module function count_substring_chars(self, match) result(occurrences)
      !-----------------------------------------------------------------------------------------------------------------
      !! Returns number of non-overlapping occurrences of a substring elementally.
      !-----------------------------------------------------------------------------------------------------------------
      class(String),    intent(in) :: self
      character(len=*), intent(in) :: match
    end function count_substring_chars

    pure elemental recursive integer module function count_substring_string(self, match) result(occurrences)
      !-----------------------------------------------------------------------------------------------------------------
      !! Returns number of non-overlapping occurrences of a substring elementally.
      !-----------------------------------------------------------------------------------------------------------------
      class(String), intent(in) :: self
      type(String),  intent(in) :: match
    end function count_substring_string

    pure elemental recursive module subroutine empty(self)
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

    pure elemental recursive integer module function length(self) result(self_len)
      !-----------------------------------------------------------------------------------------------------------------
      !! Returns the length of the string slice component elementally. Unallocated components return `-1`.
      !!
      !! For a user reference, see [len](../page/Ref/String-methods.html#len).
      !-----------------------------------------------------------------------------------------------------------------
      class(String), intent(in) :: self
    end function length

    pure elemental recursive integer(i64) module function length64(self) result(self_len)
      !-----------------------------------------------------------------------------------------------------------------
      !! Returns the length of the string slice component elementally. Unallocated components return `-1`. This
      !! function is identical to `len` for strings of 2,147,483,647 bytes or smaller.
      !!
      !! For a user reference, see [len](../page/Ref/String-methods.html#len).
      !-----------------------------------------------------------------------------------------------------------------
      class(String), intent(in) :: self
    end function length64

    pure elemental recursive module subroutine push_chars(self, substring)
      !-----------------------------------------------------------------------------------------------------------------
      !! Appends characters to the string slice component elementally in place.
      !-----------------------------------------------------------------------------------------------------------------
      class(String),    intent(inout) :: self
      character(len=*), intent(in)    :: substring
    end subroutine push_chars

    pure elemental recursive module subroutine push_string(self, substring)
      !-----------------------------------------------------------------------------------------------------------------
      !! Appends string to the string slice component elementally in place.
      !-----------------------------------------------------------------------------------------------------------------
      class(String), intent(inout) :: self
      type(String),  intent(in)    :: substring
    end subroutine push_string

    impure recursive module subroutine read_file(self, file, cell_array, row_separator, column_separator)
      !-----------------------------------------------------------------------------------------------------------------
      !! Reads raw text file contents into `self` and optionally populates a cell array using the designated
      !! `row_separator` and `column_separator` whose default values are `LF` and `COMMA` respectively.
      !!
      !! For a user reference, see [read_file](../page/Ref/String-methods.html#read_file).
      !-----------------------------------------------------------------------------------------------------------------
      class(String),    intent(inout)                      :: self
      character(len=*), intent(in)                         :: file
      type(String),     intent(out), allocatable, optional :: cell_array(:,:)
      character(len=*), intent(in),               optional :: row_separator, column_separator
    end subroutine read_file

    pure elemental recursive type(String) module function replace_ch_copy(self, match, substring, back) result(new)
      !-----------------------------------------------------------------------------------------------------------------
      !! Matches and replaces all occurrences of a substring elementally.
      !-----------------------------------------------------------------------------------------------------------------
      class(String),    intent(in)           :: self
      character(len=*), intent(in)           :: match, substring
      logical,          intent(in), optional :: back
    end function replace_ch_copy

    pure elemental recursive type(String) module function replace_st_copy(self, match, substring, back) result(new)
      !-----------------------------------------------------------------------------------------------------------------
      !! Matches and replaces all occurrences of a substring elementally.
      !-----------------------------------------------------------------------------------------------------------------
      class(String), intent(in)           :: self
      type(String),  intent(in)           :: match, substring
      logical,       intent(in), optional :: back
    end function replace_st_copy

    pure elemental recursive type(String) module function replace_chst_copy(self, match, substring, back) result(new)
      !-----------------------------------------------------------------------------------------------------------------
      !! Matches and replaces all occurrences of a substring elementally.
      !-----------------------------------------------------------------------------------------------------------------
      class(String),    intent(in)           :: self
      character(len=*), intent(in)           :: match
      type(String),     intent(in)           :: substring
      logical,          intent(in), optional :: back
    end function replace_chst_copy

    pure elemental recursive type(String) module function replace_stch_copy(self, match, substring, back) result(new)
      !-----------------------------------------------------------------------------------------------------------------
      !! Matches and replaces all occurrences of a substring elementally.
      !-----------------------------------------------------------------------------------------------------------------
      class(String),    intent(in)           :: self
      type(String),     intent(in)           :: match
      character(len=*), intent(in)           :: substring
      logical,          intent(in), optional :: back
    end function replace_stch_copy

    pure elemental recursive module subroutine replace_ch_inplace(self, match, substring, back)
      !-----------------------------------------------------------------------------------------------------------------
      !! Matches and replaces all occurrences of a substring elementally in place.
      !-----------------------------------------------------------------------------------------------------------------
      class(String),    intent(inout)        :: self
      character(len=*), intent(in)           :: match, substring
      logical,          intent(in), optional :: back
    end subroutine replace_ch_inplace

    pure elemental recursive module subroutine replace_st_inplace(self, match, substring, back)
      !-----------------------------------------------------------------------------------------------------------------
      !! Matches and replaces all occurrences of a substring elementally in place.
      !-----------------------------------------------------------------------------------------------------------------
      class(String), intent(inout)        :: self
      type(String),  intent(in)           :: match, substring
      logical,       intent(in), optional :: back
    end subroutine replace_st_inplace

    pure elemental recursive module subroutine replace_chst_inplace(self, match, substring, back)
      !-----------------------------------------------------------------------------------------------------------------
      !! Matches and replaces all occurrences of a substring elementally in place.
      !-----------------------------------------------------------------------------------------------------------------
      class(String),    intent(inout)        :: self
      character(len=*), intent(in)           :: match
      type(String),     intent(in)           :: substring
      logical,          intent(in), optional :: back
    end subroutine replace_chst_inplace

    pure elemental recursive module subroutine replace_stch_inplace(self, match, substring, back)
      !-----------------------------------------------------------------------------------------------------------------
      !! Matches and replaces all occurrences of a substring elementally in place.
      !-----------------------------------------------------------------------------------------------------------------
      class(String),    intent(inout)        :: self
      type(String),     intent(in)           :: match
      character(len=*), intent(in)           :: substring
      logical,          intent(in), optional :: back
    end subroutine replace_stch_inplace

    pure elemental recursive type(String) module function trim_copy(self) result(new)
      !-----------------------------------------------------------------------------------------------------------------
      !! Returns a copy of a `String` elementally in which each string slice component has been trimmed of any
      !! leading or trailing whitespace.
      !!
      !! For a user reference, see [trim](../page/Ref/String-methods.html#trim).
      !-----------------------------------------------------------------------------------------------------------------
      class(String), intent(in) :: self
    end function trim_copy

    pure elemental recursive module subroutine trim_inplace(self)
      !-----------------------------------------------------------------------------------------------------------------
      !! Removes any leading or trailing whitespace of the string slice component of a `String` elementally and
      !! in place.
      !!
      !! For a user reference, see [trim_inplace](../page/Ref/String-methods.html#trim_inplace).
      !-----------------------------------------------------------------------------------------------------------------
      class(String), intent(inout) :: self
    end subroutine trim_inplace

    impure recursive module subroutine write_file(self, cell_array, file, row_separator, column_separator, append, &
                                                  stat, errmsg)
      !-----------------------------------------------------------------------------------------------------------------
      !! Writes the content of a cell array to a text file. The cell array's entire contents are populated into
      !! `self` and then streamed to an external text file using the designated `row_separator` and
      !! `column_separator` whose default values are `LF` and `COMMA` respectively.
      !!
      !! For a user reference, see [write_file](../page/Ref/String-methods.html#write_file).
      !-----------------------------------------------------------------------------------------------------------------
      class(String),    intent(inout)                 :: self
      type(String),     intent(in)                    :: cell_array(:,:)
      character(len=*), intent(in)                    :: file
      character(len=*), intent(in),  optional, target :: row_separator, column_separator
      logical,          intent(in),  optional, target :: append
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine write_file

    impure recursive module subroutine write_string(substring, unit, iotype, v_list, iostat, iomsg)
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

    pure elemental recursive module subroutine scrub(self)
      !-----------------------------------------------------------------------------------------------------------------
      !! Finalization procedure for type `String`.
      !-----------------------------------------------------------------------------------------------------------------
      type(String), intent(inout) :: self
    end subroutine scrub
  end interface

  interface operator(//)                                                                           ! Submodule operators
    !-------------------------------------------------------------------------------------------------------------------
    !! Concatenation operator for `character` and `String`, lifted from `character`. Mixed type concatenation of
    !! `character` and `String` is explicitly defined.
    !!
    !! For a user reference, see [Concatenation](../page/Ref/operators.html#concatenation).
    !-------------------------------------------------------------------------------------------------------------------
    pure elemental recursive type(String) module function string_concatenation(Stringl, Stringr) result(new)
      class(String), intent(in) :: Stringl, Stringr
    end function string_concatenation

    pure elemental recursive type(String) module function string_char_concatenation(Stringl, charsr) result(new)
      class(String),    intent(in) :: Stringl
      character(len=*), intent(in) :: charsr
    end function string_char_concatenation

    pure elemental recursive type(String) module function char_string_concatenation(charsl, Stringr) result(new)
      character(len=*), intent(in) :: charsl
      class(String),    intent(in) :: Stringr
    end function char_string_concatenation
  end interface operator(//)

  interface operator(+)                                                                            ! Submodule operators
    !-------------------------------------------------------------------------------------------------------------------
    !! Concatenation operator for `character` and `String` (as addition). Mixed type concatenation of
    !! `character` and `String` is explicitly defined.
    !!
    !! For a user reference, see [Concatenation](../page/Ref/operators.html#concatenation).
    !-------------------------------------------------------------------------------------------------------------------
    pure elemental recursive module function char_concat_plus(charsl, charsr) result(new)
      character(len=*), intent(in)           :: charsl, charsr
      character(len=len(charsl)+len(charsr)) :: new
    end function char_concat_plus

    pure elemental recursive type(String) module function string_concat_plus(Stringl, Stringr) result(new)
      class(String), intent(in) :: Stringl, Stringr
    end function string_concat_plus

    pure elemental recursive type(String) module function string_char_concat_plus(Stringl, charsr) result(new)
      class(String),    intent(in) :: Stringl
      character(len=*), intent(in) :: charsr
    end function string_char_concat_plus

    pure elemental recursive type(String) module function char_string_concat_plus(charsl, Stringr) result(new)
      character(len=*), intent(in) :: charsl
      class(String),    intent(in) :: Stringr
    end function char_string_concat_plus
  end interface operator(+)

  interface operator(-)                                                                            ! Submodule operators
    !-------------------------------------------------------------------------------------------------------------------
    !! Excision operator for `character` and `String` (as subtraction). Mixed type excision of `character` and
    !! `String` is explicitly defined.
    !!
    !! For a user reference, see [Excision](../page/Ref/operators.html#excision).
    !-------------------------------------------------------------------------------------------------------------------
    pure elemental recursive type(String) module function char_excision(charsl, charsr) result(new)
      character(len=*), intent(in) :: charsl, charsr
    end function char_excision

    pure elemental recursive type(String) module function string_excision(Stringl, Stringr) result(new)
      class(String), intent(in) :: Stringl, Stringr
    end function string_excision

    pure elemental recursive type(String) module function string_char_excision(Stringl, charsr) result(new)
      class(String),    intent(in) :: Stringl
      character(len=*), intent(in) :: charsr
    end function string_char_excision

    pure elemental recursive type(String) module function char_string_excision(charsl, Stringr) result(new)
      character(len=*), intent(in) :: charsl
      class(String),    intent(in) :: Stringr
    end function char_string_excision
  end interface operator(-)

  interface operator(**)                                                                           ! Submodule operators
    !-------------------------------------------------------------------------------------------------------------------
    !! Repetition operator for `character` and `String` (as exponentiation).
    !!
    !! For a user reference, see [Repetition](../page/Ref/operators.html#repetition).
    !-------------------------------------------------------------------------------------------------------------------
    pure elemental recursive module function repeat_chars(char_base, ncopies) result(new)
      character(len=*), intent(in)          :: char_base
      integer,          intent(in)          :: ncopies
      character(len=len(char_base)*ncopies) :: new
    end function repeat_chars

    pure elemental recursive type(String) module function repeat_String(String_base, ncopies) result(new)
      class(String), intent(in) :: String_base
      integer,       intent(in) :: ncopies
    end function repeat_String
  end interface operator(**)

  interface operator(==)                                                                           ! Submodule operators
    !-------------------------------------------------------------------------------------------------------------------
    !! Equivalence operator for `character` and `String`. Mixed type equivalence of `character` and `String` is
    !! explicitly defined.
    !!
    !! For a user reference, see [Equivalence](../page/Ref/operators.html#equivalence).
    !!
    !! @note The equivalence operator `==` is interchangeable with `.eq.`.
    !-------------------------------------------------------------------------------------------------------------------
    pure elemental recursive logical module function string_equivalence(Stringl, Stringr) result(equal)
      class(String), intent(in) :: Stringl, Stringr
    end function string_equivalence

    pure elemental recursive logical module function string_char_equivalence(Stringl, charsr) result(equal)
      class(String),    intent(in) :: Stringl
      character(len=*), intent(in) :: charsr
    end function string_char_equivalence

    pure elemental recursive logical module function char_string_equivalence(charsl, Stringr) result(equal)
      character(len=*), intent(in) :: charsl
      class(String),    intent(in) :: Stringr
    end function char_string_equivalence
  end interface operator(==)

  interface operator(/=)                                                                           ! Submodule operators
    !-------------------------------------------------------------------------------------------------------------------
    !! Non-equivalence operator for `character` and `String`. Mixed type non-equivalence of `character` and
    !! `String` is explicitly defined.
    !!
    !! For a user reference, see [Non-equivalence](../page/Ref/operators.html#non-equivalence).
    !!
    !! @note The non-equivalence operator `/=` is interchangeable with `.ne.`.
    !-------------------------------------------------------------------------------------------------------------------
    pure elemental recursive logical module function string_nonequivalence(Stringl, Stringr) result(unequal)
      class(String), intent(in) :: Stringl, Stringr
    end function string_nonequivalence

    pure elemental recursive logical module function string_char_nonequivalence(Stringl, charsr) result(unequal)
      class(String),    intent(in) :: Stringl
      character(len=*), intent(in) :: charsr
    end function string_char_nonequivalence

    pure elemental recursive logical module function char_string_nonequivalence(charsl, Stringr) result(unequal)
      character(len=*), intent(in) :: charsl
      class(String),    intent(in) :: Stringr
    end function char_string_nonequivalence
  end interface operator(/=)

  interface String                                                                               ! Submodule internal_io
    !-------------------------------------------------------------------------------------------------------------------
    !! Function for returning a [String](../type/string.html) representation of numbers.
    !!
    !! For a user reference, see [String](../page/Ref/String.html),
    !! [String methods](../page/Ref/String-methods.html), and [Operators](../page/Ref/operators.html).
    !-------------------------------------------------------------------------------------------------------------------
    pure elemental recursive type(String) module function new_string_from_c128(x, locale, fmt, decimals, im) result(new)
      complex(r128),    intent(in)           :: x
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
    end function new_string_from_c128
    pure elemental recursive type(String) module function new_string_from_c64(x, locale, fmt, decimals, im) result(new)
      complex(r64),     intent(in)           :: x
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
    end function new_string_from_c64
    pure elemental recursive type(String) module function new_string_from_c32(x, locale, fmt, decimals, im) result(new)
      complex(r32),     intent(in)           :: x
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
    end function new_string_from_c32

    pure elemental recursive type(String) module function new_string_from_r128(x, locale, fmt, decimals) result(new)
      real(r128),       intent(in)           :: x
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
    end function new_string_from_r128
    pure elemental recursive type(String) module function new_string_from_r64(x, locale, fmt, decimals) result(new)
      real(r64),        intent(in)           :: x
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
    end function new_string_from_r64
    pure elemental recursive type(String) module function new_string_from_r32(x, locale, fmt, decimals) result(new)
      real(r32),        intent(in)           :: x
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
    end function new_string_from_r32

    pure elemental recursive type(String) module function new_string_from_i64(x, fmt) result(new)
      integer(i64),     intent(in)           :: x
      character(len=*), intent(in), optional :: fmt
    end function new_string_from_i64
    pure elemental recursive type(String) module function new_string_from_i32(x, fmt) result(new)
      integer(i32),     intent(in)           :: x
      character(len=*), intent(in), optional :: fmt
    end function new_string_from_i32
    pure elemental recursive type(String) module function new_string_from_i16(x, fmt) result(new)
      integer(i16),     intent(in)           :: x
      character(len=*), intent(in), optional :: fmt
    end function new_string_from_i16
    pure elemental recursive type(String) module function new_string_from_i8(x, fmt) result(new)
      integer(i8),      intent(in)           :: x
      character(len=*), intent(in), optional :: fmt
    end function new_string_from_i8

    pure elemental recursive type(String) module function new_string_from_string(x) result(new)
      class(String), intent(in) :: x
    end function new_string_from_string
    pure elemental recursive type(String) module function new_string_from_char(x) result(new)
      character(len=*), intent(in) :: x
    end function new_string_from_char
    pure elemental recursive type(String) module function new_string_from_empty() result(new)
      ! No arguments
    end function new_string_from_empty
  end interface String

  interface str                                                                                  ! Submodule internal_io
    !-------------------------------------------------------------------------------------------------------------------
    !! Function for returning a `character` representation of a number.
    !!
    !! For a user reference, see [str](../page/Ref/str.html).
    !-------------------------------------------------------------------------------------------------------------------
    pure recursive module function str_from_c128(x, locale, fmt, decimals, im) result(x_str)
      complex(r128),    intent(in)           :: x
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
      character(len=:), allocatable          :: x_str
    end function str_from_c128
    pure recursive module function str_from_c64(x, locale, fmt, decimals, im) result(x_str)
      complex(r64),     intent(in)           :: x
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
      character(len=:), allocatable          :: x_str
    end function str_from_c64
    pure recursive module function str_from_c32(x, locale, fmt, decimals, im) result(x_str)
      complex(r32),     intent(in)           :: x
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
      character(len=:), allocatable          :: x_str
    end function str_from_c32

    pure recursive module function str_from_r128(x, locale, fmt, decimals) result(x_str)
      real(r128),       intent(in)           :: x
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=:), allocatable          :: x_str
    end function str_from_r128
    pure recursive module function str_from_r64(x, locale, fmt, decimals) result(x_str)
      real(r64),        intent(in)           :: x
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=:), allocatable          :: x_str
    end function str_from_r64
    pure recursive module function str_from_r32(x, locale, fmt, decimals) result(x_str)
      real(r32),        intent(in)           :: x
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
      integer(i8),      intent(in)           :: x
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

  interface cast                                                                                 ! Submodule internal_io
    !-------------------------------------------------------------------------------------------------------------------
    !! Subroutine for casting between numeric and string data.
    !!
    !! For a user reference, see [cast](../page/Ref/cast.html).
    !-------------------------------------------------------------------------------------------------------------------
    pure elemental recursive module subroutine cast_c128_to_string(x, into, locale, fmt, decimals, im)
      complex(r128),    intent(in)           :: x
      type(String),     intent(inout)        :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
    end subroutine cast_c128_to_string
    pure elemental recursive module subroutine cast_c64_to_string(x, into, locale, fmt, decimals, im)
      complex(r64),     intent(in)           :: x
      type(String),     intent(inout)        :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
    end subroutine cast_c64_to_string
    pure elemental recursive module subroutine cast_c32_to_string(x, into, locale, fmt, decimals, im)
      complex(r32),     intent(in)           :: x
      type(String),     intent(inout)        :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
    end subroutine cast_c32_to_string

    pure elemental recursive module subroutine cast_r128_to_string(x, into, locale, fmt, decimals)
      real(r128),       intent(in)           :: x
      type(String),     intent(inout)        :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
    end subroutine cast_r128_to_string
    pure elemental recursive module subroutine cast_r64_to_string(x, into, locale, fmt, decimals)
      real(r64),        intent(in)           :: x
      type(String),     intent(inout)        :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
    end subroutine cast_r64_to_string
    pure elemental recursive module subroutine cast_r32_to_string(x, into, locale, fmt, decimals)
      real(r32),        intent(in)           :: x
      type(String),     intent(inout)        :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
    end subroutine cast_r32_to_string

    pure elemental recursive module subroutine cast_i64_to_string(x, into, fmt)
      integer(i64),     intent(in)           :: x
      type(String),     intent(inout)        :: into
      character(len=*), intent(in), optional :: fmt
    end subroutine cast_i64_to_string
    pure elemental recursive module subroutine cast_i32_to_string(x, into, fmt)
      integer(i32),     intent(in)           :: x
      type(String),     intent(inout)        :: into
      character(len=*), intent(in), optional :: fmt
    end subroutine cast_i32_to_string
    pure elemental recursive module subroutine cast_i16_to_string(x, into, fmt)
      integer(i16),     intent(in)           :: x
      type(String),     intent(inout)        :: into
      character(len=*), intent(in), optional :: fmt
    end subroutine cast_i16_to_string
    pure elemental recursive module subroutine cast_i8_to_string(x, into, fmt)
      integer(i8),      intent(in)           :: x
      type(String),     intent(inout)        :: into
      character(len=*), intent(in), optional :: fmt
    end subroutine cast_i8_to_string

    pure recursive module subroutine cast_c128_to_char(x, into, locale, fmt, decimals, im)
      complex(r128),    intent(in)                 :: x
      character(len=:), intent(inout), allocatable :: into
      character(len=*), intent(in),    optional    :: locale
      character(len=*), intent(in),    optional    :: fmt
      integer,          intent(in),    optional    :: decimals
      character(len=*), intent(in),    optional    :: im
    end subroutine cast_c128_to_char
    pure recursive module subroutine cast_c64_to_char(x, into, locale, fmt, decimals, im)
      complex(r64),     intent(in)                 :: x
      character(len=:), intent(inout), allocatable :: into
      character(len=*), intent(in), optional       :: locale
      character(len=*), intent(in), optional       :: fmt
      integer,          intent(in), optional       :: decimals
      character(len=*), intent(in), optional       :: im
    end subroutine cast_c64_to_char
    pure recursive module subroutine cast_c32_to_char(x, into, locale, fmt, decimals, im)
      complex(r32),     intent(in)                 :: x
      character(len=:), intent(inout), allocatable :: into
      character(len=*), intent(in),    optional    :: locale
      character(len=*), intent(in),    optional    :: fmt
      integer,          intent(in),    optional    :: decimals
      character(len=*), intent(in),    optional    :: im
    end subroutine cast_c32_to_char

    pure recursive module subroutine cast_r128_to_char(x, into, locale, fmt, decimals)
      real(r128),       intent(in)                 :: x
      character(len=:), intent(inout), allocatable :: into
      character(len=*), intent(in),    optional    :: locale
      character(len=*), intent(in),    optional    :: fmt
      integer,          intent(in),    optional    :: decimals
    end subroutine cast_r128_to_char
    pure recursive module subroutine cast_r64_to_char(x, into, locale, fmt, decimals)
      real(r64),        intent(in)                 :: x
      character(len=:), intent(inout), allocatable :: into
      character(len=*), intent(in),    optional    :: locale
      character(len=*), intent(in),    optional    :: fmt
      integer,          intent(in),    optional    :: decimals
    end subroutine cast_r64_to_char
    pure recursive module subroutine cast_r32_to_char(x, into, locale, fmt, decimals)
      real(r32),        intent(in)                 :: x
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
      integer(i8),      intent(in)                 :: x
      character(len=:), intent(inout), allocatable :: into
      character(len=*), intent(in),    optional    :: fmt
    end subroutine cast_i8_to_char

    pure elemental recursive module subroutine cast_string_to_c128(substring, into, locale, fmt, im)
      class(String),    intent(in)           :: substring
      complex(r128),    intent(out)          :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      character(len=*), intent(in), optional :: im
    end subroutine cast_string_to_c128
    pure elemental recursive module subroutine cast_string_to_c64(substring, into, locale, fmt, im)
      class(String),    intent(in)           :: substring
      complex(r64),     intent(out)          :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      character(len=*), intent(in), optional :: im
    end subroutine cast_string_to_c64
    pure elemental recursive module subroutine cast_string_to_c32(substring, into, locale, fmt, im)
      class(String),    intent(in)           :: substring
      complex(r32),     intent(out)          :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      character(len=*), intent(in), optional :: im
    end subroutine cast_string_to_c32

    pure elemental recursive module subroutine cast_string_to_r128(substring, into, locale, fmt)
      class(String),    intent(in)           :: substring
      real(r128),       intent(out)          :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
    end subroutine cast_string_to_r128
    pure elemental recursive module subroutine cast_string_to_r64(substring, into, locale, fmt)
      class(String),    intent(in)           :: substring
      real(r64),        intent(out)          :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
    end subroutine cast_string_to_r64
    pure elemental recursive module subroutine cast_string_to_r32(substring, into, locale, fmt)
      class(String),    intent(in)           :: substring
      real(r32),        intent(out)          :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
    end subroutine cast_string_to_r32

    pure elemental recursive module subroutine cast_string_to_i64(substring, into, fmt)
      class(String),    intent(in)           :: substring
      integer(i64),     intent(out)          :: into
      character(len=*), intent(in), optional :: fmt
    end subroutine cast_string_to_i64
    pure elemental recursive module subroutine cast_string_to_i32(substring, into, fmt)
      class(String),    intent(in)           :: substring
      integer(i32),     intent(out)          :: into
      character(len=*), intent(in), optional :: fmt
    end subroutine cast_string_to_i32
    pure elemental recursive module subroutine cast_string_to_i16(substring, into, fmt)
      class(String),    intent(in)           :: substring
      integer(i16),     intent(out)          :: into
      character(len=*), intent(in), optional :: fmt
    end subroutine cast_string_to_i16
    pure elemental recursive module subroutine cast_string_to_i8(substring, into, fmt)
      class(String),    intent(in)           :: substring
      integer(i8),      intent(out)          :: into
      character(len=*), intent(in), optional :: fmt
    end subroutine cast_string_to_i8

    pure recursive module subroutine cast_char_to_c128(substring, into, locale, fmt, im)
      character(len=*), intent(in)           :: substring
      complex(r128),    intent(out)          :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      character(len=*), intent(in), optional :: im
    end subroutine cast_char_to_c128
    pure recursive module subroutine cast_char_to_c64(substring, into, locale, fmt, im)
      character(len=*), intent(in)           :: substring
      complex(r64),     intent(out)          :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      character(len=*), intent(in), optional :: im
    end subroutine cast_char_to_c64
    pure recursive module subroutine cast_char_to_c32(substring, into, locale, fmt, im)
      character(len=*), intent(in)           :: substring
      complex(r32),     intent(out)          :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
      character(len=*), intent(in), optional :: im
    end subroutine cast_char_to_c32

    pure recursive module subroutine cast_char_to_r128(substring, into, locale, fmt)
      character(len=*), intent(in)           :: substring
      real(r128),       intent(out)          :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
    end subroutine cast_char_to_r128
    pure recursive module subroutine cast_char_to_r64(substring, into, locale, fmt)
      character(len=*), intent(in)           :: substring
      real(r64),        intent(out)          :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
    end subroutine cast_char_to_r64
    pure recursive module subroutine cast_char_to_r32(substring, into, locale, fmt)
      character(len=*), intent(in)           :: substring
      real(r32),        intent(out)          :: into
      character(len=*), intent(in), optional :: locale
      character(len=*), intent(in), optional :: fmt
    end subroutine cast_char_to_r32

    pure recursive module subroutine cast_char_to_i64(substring, into, fmt)
      character(len=*), intent(in)           :: substring
      integer(i64),     intent(out)          :: into
      character(len=*), intent(in), optional :: fmt
    end subroutine cast_char_to_i64
    pure recursive module subroutine cast_char_to_i32(substring, into, fmt)
      character(len=*), intent(in)           :: substring
      integer(i32),     intent(out)          :: into
      character(len=*), intent(in), optional :: fmt
    end subroutine cast_char_to_i32
    pure recursive module subroutine cast_char_to_i16(substring, into, fmt)
      character(len=*), intent(in)           :: substring
      integer(i16),     intent(out)          :: into
      character(len=*), intent(in), optional :: fmt
    end subroutine cast_char_to_i16
    pure recursive module subroutine cast_char_to_i8(substring, into, fmt)
      character(len=*), intent(in)           :: substring
      integer(i8),      intent(out)          :: into
      character(len=*), intent(in), optional :: fmt
    end subroutine cast_char_to_i8
  end interface cast

  interface join                                                                                  ! Submodule join_split
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

  interface split                                                                                 ! Submodule join_split
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

  interface                                                                                          ! Submodule file_io
    !-------------------------------------------------------------------------------------------------------------------
    ! Function for parsing a file name for an extension
    !-------------------------------------------------------------------------------------------------------------------
    pure recursive module function ext_of(file) result(ext)
      character(len=*), intent(in)  :: file
      character(len=:), allocatable :: ext
    end function ext_of
  end interface

  interface to_file                                                                                  ! Submodule file_io
    !-------------------------------------------------------------------------------------------------------------------
    !! Subroutine for writing an array of uniform numeric data type to an external file.
    !!
    !! For a user reference, see [to_file](../page/Ref/to_file.html).
    !-------------------------------------------------------------------------------------------------------------------
    impure recursive module subroutine to_file_c128(x, file, header, locale, delim, fmt, decimals, im, stat, errmsg)
      complex(r128),    intent(in)                    :: x(..)
      character(len=*), intent(in)                    :: file
      character(len=*), intent(in),  optional, target :: header(:)
      character(len=*), intent(in),  optional, target :: locale
      character(len=*), intent(in),  optional, target :: delim
      character(len=*), intent(in),  optional, target :: fmt
      integer,          intent(in),  optional, target :: decimals
      character(len=*), intent(in),  optional, target :: im
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine to_textfile_c128
    impure recursive module subroutine to_file_c64(x, file, header, locale, delim, fmt, decimals, im, stat, errmsg)
      complex(r64),     intent(in)                    :: x(..)
      character(len=*), intent(in)                    :: file
      character(len=*), intent(in),  optional, target :: header(:)
      character(len=*), intent(in),  optional, target :: locale
      character(len=*), intent(in),  optional, target :: delim
      character(len=*), intent(in),  optional, target :: fmt
      integer,          intent(in),  optional, target :: decimals
      character(len=*), intent(in),  optional, target :: im
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine to_textfile_c64
    impure recursive module subroutine to_file_c32(x, file, header, locale, delim, fmt, decimals, im, stat, errmsg)
      complex(r32),     intent(in)                    :: x(..)
      character(len=*), intent(in)                    :: file
      character(len=*), intent(in),  optional, target :: header(:)
      character(len=*), intent(in),  optional, target :: locale
      character(len=*), intent(in),  optional, target :: delim
      character(len=*), intent(in),  optional, target :: fmt
      integer,          intent(in),  optional, target :: decimals
      character(len=*), intent(in),  optional, target :: im
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine to_file_c32

    impure recursive module subroutine to_file_r128(x, file, header, locale, delim, fmt, decimals, stat, errmsg)
      real(r128),       intent(in)                    :: x(..)
      character(len=*), intent(in)                    :: file
      character(len=*), intent(in),  optional, target :: header(:)
      character(len=*), intent(in),  optional, target :: locale
      character(len=*), intent(in),  optional, target :: delim
      character(len=*), intent(in),  optional, target :: fmt
      integer,          intent(in),  optional, target :: decimals
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine to_file_r128
    impure recursive module subroutine to_file_r64(x, file, header, locale, delim, fmt, decimals, stat, errmsg)
      real(r64),        intent(in)                    :: x(..)
      character(len=*), intent(in)                    :: file
      character(len=*), intent(in),  optional, target :: header(:)
      character(len=*), intent(in),  optional, target :: locale
      character(len=*), intent(in),  optional, target :: delim
      character(len=*), intent(in),  optional, target :: fmt
      integer,          intent(in),  optional, target :: decimals
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine to_file_r64
    impure recursive module subroutine to_file_r32(x, file, header, locale, delim, fmt, decimals, stat, errmsg)
      real(r32),        intent(in)                    :: x(..)
      character(len=*), intent(in)                    :: file
      character(len=*), intent(in),  optional, target :: header(:)
      character(len=*), intent(in),  optional, target :: locale
      character(len=*), intent(in),  optional, target :: delim
      character(len=*), intent(in),  optional, target :: fmt
      integer,          intent(in),  optional, target :: decimals
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine to_file_r32

    impure recursive module subroutine to_file_i64(x, file, header, delim, fmt, stat, errmsg)
      integer(i64),     intent(in)                    :: x(..)
      character(len=*), intent(in)                    :: file
      character(len=*), intent(in),  optional, target :: header(:)
      character(len=*), intent(in),  optional, target :: delim
      character(len=*), intent(in),  optional, target :: fmt
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine to_file_i64
    impure recursive module subroutine to_file_i32(x, file, header, delim, fmt, stat, errmsg)
      integer(i32),     intent(in)                    :: x(..)
      character(len=*), intent(in)                    :: file
      character(len=*), intent(in),  optional, target :: header(:)
      character(len=*), intent(in),  optional, target :: delim
      character(len=*), intent(in),  optional, target :: fmt
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine to_file_i32
    impure recursive module subroutine to_file_i16(x, file, header, delim, fmt, stat, errmsg)
      integer(i16),     intent(in)                    :: x(..)
      character(len=*), intent(in)                    :: file
      character(len=*), intent(in),  optional, target :: header(:)
      character(len=*), intent(in),  optional, target :: delim
      character(len=*), intent(in),  optional, target :: fmt
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine to_file_i16
    impure recursive module subroutine to_file_i8(x, file, header, delim, fmt, stat, errmsg)
      integer(i8),      intent(in)                    :: x(..)
      character(len=*), intent(in)                    :: file
      character(len=*), intent(in),  optional, target :: header(:)
      character(len=*), intent(in),  optional, target :: delim
      character(len=*), intent(in),  optional, target :: fmt
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine to_file_i8
  end interface to_file

  interface from_file                                                                                ! Submodule file_io
    !-------------------------------------------------------------------------------------------------------------------
    !! Subroutine for reading an external file of uniform numeric data type and format into an array.
    !!
    !! For a user reference, see [from_file](../page/Ref/from_file.html).
    !-------------------------------------------------------------------------------------------------------------------
    impure recursive module subroutine from_textfile_c128(file, into, header, locale, delim, fmt, im, stat, errmsg)
      character(len=*), intent(in)                    :: file
      complex(r128),    intent(out), allocatable      :: into(..)
      logical,          intent(in),  optional, target :: header
      character(len=*), intent(in),  optional, target :: locale
      character(len=*), intent(in),  optional, target :: delim
      character(len=*), intent(in),  optional, target :: fmt
      character(len=*), intent(in),  optional, target :: im
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine from_textfile_c128
    impure recursive module subroutine from_textfile_c64(file, into, header, locale, delim, fmt, im, stat, errmsg)
      character(len=*), intent(in)                    :: file
      complex(r64),     intent(out), allocatable      :: into(..)
      logical,          intent(in),  optional, target :: header
      character(len=*), intent(in),  optional, target :: locale
      character(len=*), intent(in),  optional, target :: delim
      character(len=*), intent(in),  optional, target :: fmt
      character(len=*), intent(in),  optional, target :: im
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine from_textfile_c64
    impure recursive module subroutine from_textfile_c32(file, into, header, locale, delim, fmt, im, stat, errmsg)
      character(len=*), intent(in)                    :: file
      complex(r32),     intent(out), allocatable      :: into(..)
      logical,          intent(in),  optional, target :: header
      character(len=*), intent(in),  optional, target :: locale
      character(len=*), intent(in),  optional, target :: delim
      character(len=*), intent(in),  optional, target :: fmt
      character(len=*), intent(in),  optional, target :: im
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine from_textfile_c32

    impure recursive module subroutine from_binaryfile_c128(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)                    :: file
      complex(r128),    intent(out), allocatable      :: into(..)
      integer,          intent(in)                    :: data_shape(:)
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine from_binaryfile_c128
    impure recursive module subroutine from_binaryfile_c64(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)                    :: file
      complex(r64),     intent(out), allocatable      :: into(..)
      integer,          intent(in)                    :: data_shape(:)
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine from_binaryfile_c64
    impure recursive module subroutine from_binaryfile_c32(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)                    :: file
      complex(r32),     intent(out), allocatable      :: into(..)
      integer,          intent(in)                    :: data_shape(:)
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine from_binaryfile_c32

    impure recursive module subroutine from_textfile_r128(file, into, header, locale, delim, fmt, stat, errmsg)
      character(len=*), intent(in)                    :: file
      real(r128),       intent(out), allocatable      :: into(..)
      logical,          intent(in),  optional, target :: header
      character(len=*), intent(in),  optional, target :: locale
      character(len=*), intent(in),  optional, target :: delim
      character(len=*), intent(in),  optional, target :: fmt
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine from_textfile_r128
    impure recursive module subroutine from_textfile_r64(file, into, header, locale, delim, fmt, stat, errmsg)
      character(len=*), intent(in)                    :: file
      real(r64),        intent(out), allocatable      :: into(..)
      logical,          intent(in),  optional, target :: header
      character(len=*), intent(in),  optional, target :: locale
      character(len=*), intent(in),  optional, target :: delim
      character(len=*), intent(in),  optional, target :: fmt
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine from_textfile_r64
    impure recursive module subroutine from_textfile_r32(file, into, header, locale, delim, fmt, stat, errmsg)
      character(len=*), intent(in)                    :: file
      real(r32),        intent(out), allocatable      :: into(..)
      logical,          intent(in),  optional, target :: header
      character(len=*), intent(in),  optional, target :: locale
      character(len=*), intent(in),  optional, target :: delim
      character(len=*), intent(in),  optional, target :: fmt
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine from_textfile_r32

    impure recursive module subroutine from_binaryfile_r128(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)                    :: file
      real(r128),       intent(out), allocatable      :: into(..)
      integer,          intent(in)                    :: data_shape(:)
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine from_binaryfile_r128
    impure recursive module subroutine from_binaryfile_r64(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)                    :: file
      real(r64),        intent(out), allocatable      :: into(..)
      integer,          intent(in)                    :: data_shape(:)
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine from_binaryfile_r64
    impure recursive module subroutine from_binaryfile_r32(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)                    :: file
      real(r32),        intent(out), allocatable      :: into(..)
      integer,          intent(in)                    :: data_shape(:)
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine from_binaryfile_r32

    impure recursive module subroutine from_textfile_i64(file, into, header, delim, fmt, stat, errmsg)
      character(len=*), intent(in)                    :: file
      integer(i64),     intent(out), allocatable      :: into(..)
      logical,          intent(in),  optional, target :: header
      character(len=*), intent(in),  optional, target :: delim
      character(len=*), intent(in),  optional, target :: fmt
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine from_textfile_i64
    impure recursive module subroutine from_textfile_i32(file, into, header, delim, fmt, stat, errmsg)
      character(len=*), intent(in)                    :: file
      integer(i32),     intent(out), allocatable      :: into(..)
      logical,          intent(in),  optional, target :: header
      character(len=*), intent(in),  optional, target :: delim
      character(len=*), intent(in),  optional, target :: fmt
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine from_textfile_i32
    impure recursive module subroutine from_textfile_i16(file, into, header, delim, fmt, stat, errmsg)
      character(len=*), intent(in)                    :: file
      integer(i16),     intent(out), allocatable      :: into(..)
      logical,          intent(in),  optional, target :: header
      character(len=*), intent(in),  optional, target :: delim
      character(len=*), intent(in),  optional, target :: fmt
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine from_textfile_i16
    impure recursive module subroutine from_textfile_i8(file, into, header, delim, fmt, stat, errmsg)
      character(len=*), intent(in)                    :: file
      integer(i8),      intent(out), allocatable      :: into(..)
      logical,          intent(in),  optional, target :: header
      character(len=*), intent(in),  optional, target :: delim
      character(len=*), intent(in),  optional, target :: fmt
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine from_textfile_i8

    impure recursive module subroutine from_binaryfile_i64(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)                    :: file
      integer(i64),     intent(out), allocatable      :: into(..)
      integer,          intent(in)                    :: data_shape(:)
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine from_binaryfile_i64
    impure recursive module subroutine from_binaryfile_i32(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)                    :: file
      integer(i32),     intent(out), allocatable      :: into(..)
      integer,          intent(in)                    :: data_shape(:)
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine from_binaryfile_i32
    impure recursive module subroutine from_binaryfile_i16(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)                    :: file
      integer(i16),     intent(out), allocatable      :: into(..)
      integer,          intent(in)                    :: data_shape(:)
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine from_binaryfile_i16
    impure recursive module subroutine from_binaryfile_i8(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)                    :: file
      integer(i8),      intent(out), allocatable      :: into(..)
      integer,          intent(in)                    :: data_shape(:)
      integer,          intent(out), optional, target :: stat
      character(len=*), intent(out), optional, target :: errmsg
    end subroutine from_binaryfile_i8
  end interface from_file

  interface echo                                                                                     ! Submodule text_io
    !-------------------------------------------------------------------------------------------------------------------
    !! Subroutine for writing a scalar `character` or `String` to an external text file.
    !!
    !! For a user reference, see [echo](../page/Ref/echo.html).
    !-------------------------------------------------------------------------------------------------------------------
    impure recursive module subroutine echo_chars(substring, file, append, terminator)
      character(len=*), intent(in)           :: substring
      character(len=*), intent(in)           :: file
      logical,          intent(in), optional :: append
      character(len=*), intent(in), optional :: terminator
    end subroutine echo_chars

    impure recursive module subroutine echo_string(substring, file, append, terminator)
      class(String),    intent(in)           :: substring
      character(len=*), intent(in)           :: file
      logical,          intent(in), optional :: append
      character(len=*), intent(in), optional :: terminator
    end subroutine echo_string
  end interface echo

  interface to_text                                                                                  ! Submodule text_io
    !-------------------------------------------------------------------------------------------------------------------
    !! Private interface for writing an array to an external text file.
    !-------------------------------------------------------------------------------------------------------------------
    impure recursive module subroutine to_text_c128(x, file, header, locale, delim, fmt, decimals, im, stat, errmsg)
      complex(r128),    intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      character(len=*), intent(in)  :: header(:)
      character(len=*), intent(in)  :: locale
      character(len=*), intent(in)  :: delim
      character(len=*), intent(in)  :: fmt
      integer,          intent(in)  :: decimals
      character(len=*), intent(in)  :: im
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_text_c128
    impure recursive module subroutine to_text_c64(x, file, header, locale, delim, fmt, decimals, im, stat, errmsg)
      complex(r64),     intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      character(len=*), intent(in)  :: header(:)
      character(len=*), intent(in)  :: locale
      character(len=*), intent(in)  :: delim
      character(len=*), intent(in)  :: fmt
      integer,          intent(in)  :: decimals
      character(len=*), intent(in)  :: im
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_text_c64
    impure recursive module subroutine to_text_c32(x, file, header, locale, delim, fmt, decimals, im, stat, errmsg)
      complex(r32),     intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      character(len=*), intent(in)  :: header(:)
      character(len=*), intent(in)  :: locale
      character(len=*), intent(in)  :: delim
      character(len=*), intent(in)  :: fmt
      integer,          intent(in)  :: decimals
      character(len=*), intent(in)  :: im
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_text_c32

    impure recursive module subroutine to_text_r128(x, file, header, locale, delim, fmt, decimals, stat, errmsg)
      real(r128),       intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      character(len=*), intent(in)  :: header(:)
      character(len=*), intent(in)  :: locale
      character(len=*), intent(in)  :: delim
      character(len=*), intent(in)  :: fmt
      integer,          intent(in)  :: decimals
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_text_r128
    impure recursive module subroutine to_text_r64(x, file, header, locale, delim, fmt, decimals, stat, errmsg)
      real(r64),        intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      character(len=*), intent(in)  :: header(:)
      character(len=*), intent(in)  :: locale
      character(len=*), intent(in)  :: delim
      character(len=*), intent(in)  :: fmt
      integer,          intent(in)  :: decimals
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_text_r64
    impure recursive module subroutine to_text_r32(x, file, header, locale, delim, fmt, decimals, stat, errmsg)
      real(r32),        intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      character(len=*), intent(in)  :: header(:)
      character(len=*), intent(in)  :: locale
      character(len=*), intent(in)  :: delim
      character(len=*), intent(in)  :: fmt
      integer,          intent(in)  :: decimals
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_text_r32

    impure recursive module subroutine to_text_i64(x, file, header, delim, fmt, stat, errmsg)
      integer(i64),     intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      character(len=*), intent(in)  :: header(:)
      character(len=*), intent(in)  :: delim
      character(len=*), intent(in)  :: fmt
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_text_i64
    impure recursive module subroutine to_text_i32(x, file, header, delim, fmt, stat, errmsg)
      integer(i32),     intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      character(len=*), intent(in)  :: header(:)
      character(len=*), intent(in)  :: delim
      character(len=*), intent(in)  :: fmt
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_text_i32
    impure recursive module subroutine to_text_i16(x, file, header, delim, fmt, stat, errmsg)
      integer(i16),     intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      character(len=*), intent(in)  :: header(:)
      character(len=*), intent(in)  :: delim
      character(len=*), intent(in)  :: fmt
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_text_i16
    impure recursive module subroutine to_text_i8(x, file, header, delim, fmt, stat, errmsg)
      integer(i8),      intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      character(len=*), intent(in)  :: header(:)
      character(len=*), intent(in)  :: delim
      character(len=*), intent(in)  :: fmt
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_text_i8
  end interface to_text

  interface from_text                                                                                ! Submodule text_io
    !-------------------------------------------------------------------------------------------------------------------
    !! Private interface for reading an external text file into an array.
    !-------------------------------------------------------------------------------------------------------------------
    impure recursive module subroutine from_text_1dc128(file, into, header, locale, delim, fmt, im)
      character(len=*), intent(in)               :: file
      complex(r128),    intent(out), allocatable :: into(:)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: locale
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
      character(len=*), intent(in)               :: im
    end subroutine from_text_1dc128
    impure recursive module subroutine from_text_1dc64(file, into, header, locale, delim, fmt, im)
      character(len=*), intent(in)               :: file
      complex(r64),     intent(out), allocatable :: into(:)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: locale
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
      character(len=*), intent(in)               :: im
    end subroutine from_text_1dc64
    impure recursive module subroutine from_text_1dc32(file, into, header, locale, delim, fmt, im)
      character(len=*), intent(in)               :: file
      complex(r32),     intent(out), allocatable :: into(:)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: locale
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
      character(len=*), intent(in)               :: im
    end subroutine from_text_1dc32

    impure recursive module subroutine from_text_2dc128(file, into, header, locale, delim, fmt, im)
      character(len=*), intent(in)               :: file
      complex(r128),    intent(out), allocatable :: into(:,:)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: locale
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
      character(len=*), intent(in)               :: im
    end subroutine from_text_2dc128
    impure recursive module subroutine from_text_2dc64(file, into, header, locale, delim, fmt, im)
      character(len=*), intent(in)               :: file
      complex(r64),     intent(out), allocatable :: into(:,:)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: locale
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
      character(len=*), intent(in)               :: im
    end subroutine from_text_2dc64
    impure recursive module subroutine from_text_2dc32(file, into, header, locale, delim, fmt, im)
      character(len=*), intent(in)               :: file
      complex(r32),     intent(out), allocatable :: into(:,:)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: locale
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
      character(len=*), intent(in)               :: im
    end subroutine from_text_2dc32

    impure recursive module subroutine from_text_1dr128(file, into, header, locale, delim, fmt)
      character(len=*), intent(in)               :: file
      real(r128),       intent(out), allocatable :: into(:)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: locale
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
    end subroutine from_text_1dr128
    impure recursive module subroutine from_text_1dr64(file, into, header, locale, delim, fmt)
      character(len=*), intent(in)               :: file
      real(r64),        intent(out), allocatable :: into(:)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: locale
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
    end subroutine from_text_1dr64
    impure recursive module subroutine from_text_1dr32(file, into, header, locale, delim, fmt)
      character(len=*), intent(in)               :: file
      real(r32),        intent(out), allocatable :: into(:)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: locale
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
    end subroutine from_text_1dr32

    impure recursive module subroutine from_text_2dr128(file, into, header, locale, delim, fmt)
      character(len=*), intent(in)               :: file
      real(r128),       intent(out), allocatable :: into(:,:)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: locale
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
    end subroutine from_text_2dr128
    impure recursive module subroutine from_text_2dr64(file, into, header, locale, delim, fmt)
      character(len=*), intent(in)               :: file
      real(r64),        intent(out), allocatable :: into(:,:)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: locale
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
    end subroutine from_text_2dr64
    impure recursive module subroutine from_text_2dr32(file, into, header, locale, delim, fmt)
      character(len=*), intent(in)               :: file
      real(r32),        intent(out), allocatable :: into(:,:)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: locale
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
    end subroutine from_text_2dr32

    impure recursive module subroutine from_text_1di64(file, into, header, delim, fmt)
      character(len=*), intent(in)               :: file
      integer(i64),     intent(out), allocatable :: into(:)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
    end subroutine from_text_1di64
    impure recursive module subroutine from_text_1di32(file, into, header, delim, fmt)
      character(len=*), intent(in)               :: file
      integer(i32),     intent(out), allocatable :: into(:)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
    end subroutine from_text_1di32
    impure recursive module subroutine from_text_1di16(file, into, header, delim, fmt)
      character(len=*), intent(in)               :: file
      integer(i16),     intent(out), allocatable :: into(:)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
    end subroutine from_text_1di16
    impure recursive module subroutine from_text_1di8(file, into, header, delim, fmt)
      character(len=*), intent(in)               :: file
      integer(i8),      intent(out), allocatable :: into(:)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
    end subroutine from_text_1di8

    impure recursive module subroutine from_text_2di64(file, into, header, delim, fmt)
      character(len=*), intent(in)               :: file
      integer(i64),     intent(out), allocatable :: into(:,:)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
    end subroutine from_text_2di64
    impure recursive module subroutine from_text_2di32(file, into, header, delim, fmt)
      character(len=*), intent(in)               :: file
      integer(i32),     intent(out), allocatable :: into(:,:)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
    end subroutine from_text_2di32
    impure recursive module subroutine from_text_2di16(file, into, header, delim, fmt)
      character(len=*), intent(in)               :: file
      integer(i16),     intent(out), allocatable :: into(:,:)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
    end subroutine from_text_2di16
    impure recursive module subroutine from_text_2di8(file, into, header, delim, fmt)
      character(len=*), intent(in)               :: file
      integer(i8),      intent(out), allocatable :: into(:,:)
      logical,          intent(in)               :: header
      character(len=*), intent(in)               :: delim
      character(len=*), intent(in)               :: fmt
    end subroutine from_text_2di8
  end interface from_text

  interface to_binary                                                                              ! Submodule binary_io
    !-------------------------------------------------------------------------------------------------------------------
    !! Private interface for writing an array to an external binary file.
    !-------------------------------------------------------------------------------------------------------------------
    impure recursive module subroutine to_binary_c128(x, file, stat, errmsg)
      complex(r128),    intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_binary_c128
    impure recursive module subroutine to_binary_c64(x, file, stat, errmsg)
      complex(r64),     intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_binary_c64
    impure recursive module subroutine to_binary_c32(x, file, stat, errmsg)
      complex(r32),     intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_binary_c32

    impure recursive module subroutine to_binary_r128(x, file, stat, errmsg)
      real(r128),       intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_binary_r128
    impure recursive module subroutine to_binary_r64(x, file, stat, errmsg)
      real(r64),        intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_binary_r64
    impure recursive module subroutine to_binary_r32(x, file, stat, errmsg)
      real(r32),        intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_binary_r32

    impure recursive module subroutine to_binary_i64(x, file, stat, errmsg)
      integer(i64),     intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_binary_i64
    impure recursive module subroutine to_binary_i32(x, file, stat, errmsg)
      integer(i32),     intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_binary_i32
    impure recursive module subroutine to_binary_i16(x, file, stat, errmsg)
      integer(i16),     intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_binary_i16
    impure recursive module subroutine to_binary_i8(x, file, stat, errmsg)
      integer(i8),      intent(in)  :: x(..)
      character(len=*), intent(in)  :: file
      integer,          intent(out) :: stat
      character(len=*), intent(out) :: errmsg
    end subroutine to_binary_i8
  end interface to_binary

  interface from_binary                                                                            ! Submodule binary_io
    !-------------------------------------------------------------------------------------------------------------------
    !! Private interface for reading an external binary file into an array.
    !-------------------------------------------------------------------------------------------------------------------
    impure recursive module subroutine from_binary_c128(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)               :: file
      complex(r128),    intent(out), allocatable :: into(..)
      integer,          intent(in)               :: data_shape(:)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_binary_c128
    impure recursive module subroutine from_binary_c64(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)               :: file
      complex(r64),     intent(out), allocatable :: into(..)
      integer,          intent(in)               :: data_shape(:)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_binary_c64
    impure recursive module subroutine from_binary_c32(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)               :: file
      complex(r32),     intent(out), allocatable :: into(..)
      integer,          intent(in)               :: data_shape(:)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_binary_c32

    impure recursive module subroutine from_binary_r128(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)               :: file
      real(r128),       intent(out), allocatable :: into(..)
      integer,          intent(in)               :: data_shape(:)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_binary_r128
    impure recursive module subroutine from_binary_r64(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)               :: file
      real(r64),        intent(out), allocatable :: into(..)
      integer,          intent(in)               :: data_shape(:)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_binary_r64
    impure recursive module subroutine from_binary_r32(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)               :: file
      real(r32),        intent(out), allocatable :: into(..)
      integer,          intent(in)               :: data_shape(:)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_binary_r32

    impure recursive module subroutine from_binary_i64(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)               :: file
      integer(i64),     intent(out), allocatable :: into(..)
      integer,          intent(in)               :: data_shape(:)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_binary_i64
    impure recursive module subroutine from_binary_i32(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)               :: file
      integer(i32),     intent(out), allocatable :: into(..)
      integer,          intent(in)               :: data_shape(:)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_binary_i32
    impure recursive module subroutine from_binary_i16(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)               :: file
      integer(i16),     intent(out), allocatable :: into(..)
      integer,          intent(in)               :: data_shape(:)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_binary_i16
    impure recursive module subroutine from_binary_i8(file, into, data_shape, stat, errmsg)
      character(len=*), intent(in)               :: file
      integer(i8),      intent(out), allocatable :: into(..)
      integer,          intent(in)               :: data_shape(:)
      integer,          intent(out)              :: stat
      character(len=*), intent(out)              :: errmsg
    end subroutine from_binary_i8
  end interface from_binary

  interface aprint                                                                            ! Submodule array_printing
    !-------------------------------------------------------------------------------------------------------------------
    !! Subroutine for printing arrays and array sections to stdout.
    !!
    !! For a user reference, see [aprint](../page/Ref/aprint.html).
    !-------------------------------------------------------------------------------------------------------------------
    impure recursive module subroutine aprint_1dc128(x, fmt, decimals, im)
      complex(r128),    intent(in)           :: x(:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
    end subroutine aprint_1dc128
    impure recursive module subroutine aprint_1dc64(x, fmt, decimals, im)
      complex(r64),     intent(in)           :: x(:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
    end subroutine aprint_1dc64
    impure recursive module subroutine aprint_1dc32(x, fmt, decimals, im)
      complex(r32),     intent(in)           :: x(:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
    end subroutine aprint_1dc32

    impure recursive module subroutine aprint_2dc128(x, fmt, decimals, im)
      complex(r128),    intent(in)           :: x(:,:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
    end subroutine aprint_2dc128
    impure recursive module subroutine aprint_2dc64(x, fmt, decimals, im)
      complex(r64),     intent(in)           :: x(:,:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
    end subroutine aprint_2dc64
    impure recursive module subroutine aprint_2dc32(x, fmt, decimals, im)
      complex(r32),     intent(in)           :: x(:,:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
      character(len=*), intent(in), optional :: im
    end subroutine aprint_2dc32

    impure recursive module subroutine aprint_1dr128(x, fmt, decimals)
      real(r128),       intent(in)           :: x(:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
    end subroutine aprint_1dr128
    impure recursive module subroutine aprint_1dr64(x, fmt, decimals)
      real(r64),        intent(in)           :: x(:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
    end subroutine aprint_1dr64
    impure recursive module subroutine aprint_1dr32(x, fmt, decimals)
      real(r32),        intent(in)           :: x(:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
    end subroutine aprint_1dr32

    impure recursive module subroutine aprint_2dr128(x, fmt, decimals)
      real(r128),       intent(in)           :: x(:,:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
    end subroutine aprint_2dr128
    impure recursive module subroutine aprint_2dr64(x, fmt, decimals)
      real(r64),        intent(in)           :: x(:,:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
    end subroutine aprint_2dr64
    impure recursive module subroutine aprint_2dr32(x, fmt, decimals)
      real(r32),        intent(in)           :: x(:,:)
      character(len=*), intent(in), optional :: fmt
      integer,          intent(in), optional :: decimals
    end subroutine aprint_2dr32

    impure recursive module subroutine aprint_1di64(x, fmt)
      integer(i64),     intent(in)           :: x(:)
      character(len=*), intent(in), optional :: fmt
    end subroutine aprint_1di64
    impure recursive module subroutine aprint_1di32(x, fmt)
      integer(i32),     intent(in)           :: x(:)
      character(len=*), intent(in), optional :: fmt
    end subroutine aprint_1di32
    impure recursive module subroutine aprint_1di16(x, fmt)
      integer(i16),     intent(in)           :: x(:)
      character(len=*), intent(in), optional :: fmt
    end subroutine aprint_1di16
    impure recursive module subroutine aprint_1di8(x, fmt)
      integer(i8),      intent(in)           :: x(:)
      character(len=*), intent(in), optional :: fmt
    end subroutine aprint_1di8

    impure recursive module subroutine aprint_2di64(x, fmt)
      integer(i64),     intent(in)           :: x(:,:)
      character(len=*), intent(in), optional :: fmt
    end subroutine aprint_2di64
    impure recursive module subroutine aprint_2di32(x, fmt)
      integer(i32),     intent(in)           :: x(:,:)
      character(len=*), intent(in), optional :: fmt
    end subroutine aprint_2di32
    impure recursive module subroutine aprint_2di16(x, fmt)
      integer(i16),     intent(in)           :: x(:,:)
      character(len=*), intent(in), optional :: fmt
    end subroutine aprint_2di16
    impure recursive module subroutine aprint_2di8(x, fmt)
      integer(i8),      intent(in)           :: x(:,:)
      character(len=*), intent(in), optional :: fmt
    end subroutine aprint_2di8

    impure recursive module subroutine aprint_1dchar(x)
      character(len=*), intent(in) :: x(:)
    end subroutine aprint_1dchar

    impure recursive module subroutine aprint_2dchar(x)
      character(len=*), intent(in) :: x(:,:)
    end subroutine aprint_2dchar

    impure recursive module subroutine aprint_1dString(x)
      class(String), intent(in) :: x(:)
    end subroutine aprint_1dString

    impure recursive module subroutine aprint_2dString(x)
      class(String), intent(in) :: x(:,:)
    end subroutine aprint_2dString
  end interface aprint

end module io_fortran_lib

!=======================================================================================================================
!  List of workarounds for compiler bugs in ifx 2023.0.0 :
!  -------------------------------------------------------
!  1.  In join_into_self (line 4808), the recursive call to join_into_self at line 4836 induces a run-time
!      segmentation fault in the program contained in benchmark.f90 not seen with the following compilers: ifort
!      2021.8.0, gfortran 11.3.0, gfortran 11.2.0. From investigation, the segmentation fault seems due to the passing
!      of the array of derived type. The fault occurs in a majority of runs, but not in every run. To avoid the fault,
!      the array to be passed must be constructed element by element and passed as in the "else" section of the "if"
!      block. The fault again seems to be induced only when "-heap-arrays 0" is specified and only with ifx 2023.0.0.
!=======================================================================================================================
