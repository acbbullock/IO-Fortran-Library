module io_fortran_lib
	!------------------------------------------------------------------------------------------------------------------
	!! This module provides common I/O routines for data of `integer`, `real`, `complex`, and `character` type, and
	!! a derived type `String` for advanced character handling and text file I/O. This module is F2018 compliant, has
	!! no external dependencies, and has a max line length of 120.
	!------------------------------------------------------------------------------------------------------------------
	use, intrinsic :: iso_fortran_env, only: real128, real64, real32, int64, int32, int16, int8, &	   ! Standard kinds
											 input_unit, output_unit				  ! Standard input and output units
	use, intrinsic :: iso_c_binding, only: c_null_char											 ! The C null character
	implicit none (type,external)													  ! No implicit types or interfaces
	private

	! Public API list ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	public :: aprint, to_file, from_file																	! Array I/O
	public :: String, str, cast, glue, split, echo														   ! String I/O
	public :: NL, SPACE, CR, FF, VT, LF, TAB, HT, BELL, NUL, CNUL, EMPTY_STR								! Constants
	public :: operator(//), operator(+), operator(-), operator(**), operator(==), operator(/=)				! Operators

	! Definitions and Interfaces ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	character(len=1), parameter :: NL    = new_line('a')					!! The newline character (system agnostic).
	character(len=1), parameter :: SPACE = achar(32)											!! The space character.
	character(len=1), parameter :: CR    = achar(13)								  !! The carriage return character.
	character(len=1), parameter :: FF    = achar(12)										!! The form feed character.
	character(len=1), parameter :: VT    = achar(11)									 !! The vertical tab character.
	character(len=1), parameter :: LF    = achar(10)										!! The line feed character.
	character(len=1), parameter :: TAB   = achar(9)									   !! The horizontal tab character.
	character(len=1), parameter :: HT    = achar(9)					  !! The horizontal tab character (alternate name).
	character(len=1), parameter :: BELL  = achar(7)										   !! The bell/alert character.
	character(len=1), parameter :: NUL   = achar(0)												 !! The null character.
	character(len=1), parameter :: CNUL  = c_null_char			!! The C null character re-exported from iso_c_binding.
	character(len=0), parameter :: EMPTY_STR = ''												   !! The empty string.

	character(len=1),				parameter :: SEMICOLON	= achar(59)										! Semicolon
	character(len=1),				parameter :: POINT		= achar(46)										! Full stop
	character(len=1),				parameter :: COMMA		= achar(44)											! Comma
	character(len=1),				parameter :: QQUOTE		= achar(34)									 ! Double quote
	character(len=*), dimension(*), parameter :: INT_FMTS   = [ 'i', 'z' ]				 ! Allowed formats for integers
	character(len=*), dimension(*), parameter :: REAL_FMTS  = [ 'e', 'f', 'z' ]			   ! Allowed formats for floats
	character(len=*), dimension(*), parameter :: LOCALES    = [ 'US', 'EU' ]				! Allowed locale specifiers
	character(len=*), dimension(*), parameter :: BINARY_EXT = [ 'dat', 'bin' ]				! Allowed binary extensions
	character(len=*), dimension(*), parameter :: TEXT_EXT   = [ 'csv', 'txt', 'log', 'rtf', & ! Allowed text extensions
																'odm', 'odt', 'ods', 'odf', 'xls', &
																'doc', 'org', 'dbf', 'bed', 'gff', 'gtf' ]

	type String
		!--------------------------------------------------------------------------------------------------------------
		!! A growable string type for advanced character handling and text file I/O.
		!!
		!! For a user reference, see [String](../page/Ref/string.html),
		!! [String methods](../page/Ref/string-methods.html), and [Operators](../page/Ref/operators.html).
		!!
		!! @note TECHNICAL NOTE: The `String` type is memory safe. The user will never need to be concerned about
		!! accessing invalid memory when using the `String` type. Any operation defined in this documentation for the
		!! `String` type which may involve a `String` with an unallocated component, or arrays of `String`s in which
		!! some of the elements may have unallocated components, is well-defined. In all such cases, the component is
		!! treated as the [empty string](../module/io_fortran_lib.html#variable-empty_str).
		!--------------------------------------------------------------------------------------------------------------
		private
		character(len=:), allocatable :: s                                               !! Component is a string slice
		contains
			private
			! Generics ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
			generic, public :: cast  			=>	cast_string_c128, cast_string_c64, cast_string_c32, &
													cast_string_r128, cast_string_r64, cast_string_r32, &
													cast_string_i64, cast_string_i32, cast_string_i16, cast_string_i8
			generic, public :: count 			=>	count_substring_chars, count_substring_string
			generic, public :: echo				=>	echo_string
			generic, public :: push  			=>	push_chars, push_string
			generic, public :: replace 			=>	replace_ch_copy, replace_st_copy, replace_chst_copy, &
													replace_stch_copy
			generic, public :: replace_inplace 	=>	replace_ch_inplace, replace_st_inplace, replace_chst_inplace, &
													replace_stch_inplace
			generic, public :: split			=>	split_string
			generic, public :: write(formatted) =>	write_string

			! Specifics ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
			procedure, pass(self), public	::	as_str
			procedure, pass(substring)		::	cast_string_c128, cast_string_c64, cast_string_c32, &
												cast_string_r128, cast_string_r64, cast_string_r32, &
												cast_string_i64, cast_string_i32, cast_string_i16, cast_string_i8
			procedure, pass(self)			::	count_substring_chars, count_substring_string
			procedure, pass(substring)		::	echo_string
			procedure, pass(self), public	::	empty
			procedure, pass(self), public	::	glue => glue_into_self
			procedure, pass(self), public	::	len => length
			procedure, pass(self)			::	push_chars, push_string
			procedure, pass(self), public	::	read_file
			procedure, pass(self)			::	replace_ch_copy, replace_st_copy, replace_chst_copy, &
												replace_stch_copy, replace_ch_inplace, replace_st_inplace, &
												replace_chst_inplace, replace_stch_inplace
			procedure, pass(substring)		::	split_string
			procedure, pass(self), public	::	trim => trim_copy
			procedure, pass(self), public	::	trim_inplace
			procedure, pass(self), public	::	write_file
			procedure, pass(substring)		::	write_string
	end type String

	interface                                                                             ! Submodule String_procedures
		!--------------------------------------------------------------------------------------------------------------
		!! Methods for the `String` type.
		!--------------------------------------------------------------------------------------------------------------
		pure recursive module function as_str(self) result(string_slice)
			!----------------------------------------------------------------------------------------------------------
			!! Returns a copy of the string slice component of a scalar `String`.
			!!
			!! For a user reference, see [as_str](../page/Ref/string-methods.html#as_str).
			!----------------------------------------------------------------------------------------------------------
			class(String), intent(in) :: self
			character(len=:), allocatable :: string_slice
		end function as_str

		pure elemental recursive integer module function count_substring_chars(self, match) result(occurrences)
			!----------------------------------------------------------------------------------------------------------
			!! Returns number of non-overlapping occurrences of a substring elementally.
			!----------------------------------------------------------------------------------------------------------
			class(String), intent(in) :: self
			character(len=*), intent(in) :: match
		end function count_substring_chars

		pure elemental recursive integer module function count_substring_string(self, match) result(occurrences)
			!----------------------------------------------------------------------------------------------------------
			!! Returns number of non-overlapping occurrences of a substring elementally.
			!----------------------------------------------------------------------------------------------------------
			class(String), intent(in) :: self
			type(String), intent(in) :: match
		end function count_substring_string

		pure elemental recursive module subroutine empty(self)
			!----------------------------------------------------------------------------------------------------------
			!! Sets the string slice component to the empty string elementally. This procedure is identical in function
			!! to the assignment `self = String()`.
			!!
			!! For a user reference, see [empty](../page/Ref/string-methods.html#empty).
			!----------------------------------------------------------------------------------------------------------
			class(String), intent(inout) :: self
		end subroutine empty

		pure recursive module subroutine glue_into_self(self, tokens, separator)
			!----------------------------------------------------------------------------------------------------------
			!! Glues a `String` vector `tokens` into `self` with given separator. Default separator is SPACE. The
			!! string slice component will be replaced if already allocated.
			!!
			!! For a user reference, see [glue](../page/Ref/string-methods.html#glue).
			!----------------------------------------------------------------------------------------------------------
			class(String), intent(inout) :: self
			type(String), dimension(:), intent(in) :: tokens
			character(len=*), intent(in), optional :: separator
		end subroutine glue_into_self

		pure elemental recursive integer module function length(self) result(self_len)
			!----------------------------------------------------------------------------------------------------------
			!! Returns the length of the string slice component elementally. Unallocated components return `-1`.
			!!
			!! For a user reference, see [len](../page/Ref/string-methods.html#len).
			!----------------------------------------------------------------------------------------------------------
			class(String), intent(in) :: self
		end function length

		pure elemental recursive module subroutine push_chars(self, substring)
			!----------------------------------------------------------------------------------------------------------
			!! Appends characters to the string slice component elementally in place.
			!----------------------------------------------------------------------------------------------------------
			class(String), intent(inout) :: self
			character(len=*), intent(in) :: substring
		end subroutine push_chars

		pure elemental recursive module subroutine push_string(self, substring)
			!----------------------------------------------------------------------------------------------------------
			!! Appends string to the string slice component elementally in place.
			!----------------------------------------------------------------------------------------------------------
			class(String), intent(inout) :: self
			type(String), intent(in) :: substring
		end subroutine push_string

		impure recursive module subroutine read_file(self, file_name, cell_array, row_separator, column_separator)
			!----------------------------------------------------------------------------------------------------------
			!! Reads an external text file into `self` and optionally populates a cell array using the designated
			!! `row_separator` and `column_separator` whose default values are `LF` and `COMMA` respectively.
			!!
			!! For a user reference, see [read_file](../page/Ref/string-methods.html#read_file).
			!----------------------------------------------------------------------------------------------------------
			class(String), intent(inout) :: self
			character(len=*), intent(in) :: file_name
			type(String), allocatable, dimension(:,:), intent(out), optional :: cell_array
			character(len=*), intent(in), optional :: row_separator, column_separator
		end subroutine read_file

		pure elemental recursive type(String) module function replace_ch_copy(self, match, substring, back) result(new)
			!----------------------------------------------------------------------------------------------------------
			!! Matches and replaces all occurrences of a substring elementally.
			!----------------------------------------------------------------------------------------------------------
			class(String), intent(in) :: self
			character(len=*), intent(in) :: match, substring
			logical, intent(in), optional :: back
		end function replace_ch_copy

		pure elemental recursive type(String) module function replace_st_copy(self, match, substring, back) result(new)
			!----------------------------------------------------------------------------------------------------------
			!! Matches and replaces all occurrences of a substring elementally.
			!----------------------------------------------------------------------------------------------------------
			class(String), intent(in) :: self
			type(String), intent(in) :: match, substring
			logical, intent(in), optional :: back
		end function replace_st_copy

		pure elemental recursive type(String) module function replace_chst_copy(self, match,substring,back) result(new)
			!----------------------------------------------------------------------------------------------------------
			!! Matches and replaces all occurrences of a substring elementally.
			!----------------------------------------------------------------------------------------------------------
			class(String), intent(in) :: self
			character(len=*), intent(in) :: match
			type(String), intent(in) :: substring
			logical, intent(in), optional :: back
		end function replace_chst_copy

		pure elemental recursive type(String) module function replace_stch_copy(self, match,substring,back) result(new)
			!----------------------------------------------------------------------------------------------------------
			!! Matches and replaces all occurrences of a substring elementally.
			!----------------------------------------------------------------------------------------------------------
			class(String), intent(in) :: self
			type(String), intent(in) :: match
			character(len=*), intent(in) :: substring
			logical, intent(in), optional :: back
		end function replace_stch_copy

		pure elemental recursive module subroutine replace_ch_inplace(self, match, substring, back)
			!----------------------------------------------------------------------------------------------------------
			!! Matches and replaces all occurrences of a substring elementally in place.
			!----------------------------------------------------------------------------------------------------------
			class(String), intent(inout) :: self
			character(len=*), intent(in) :: match, substring
			logical, intent(in), optional :: back
		end subroutine replace_ch_inplace

		pure elemental recursive module subroutine replace_st_inplace(self, match, substring, back)
			!----------------------------------------------------------------------------------------------------------
			!! Matches and replaces all occurrences of a substring elementally in place.
			!----------------------------------------------------------------------------------------------------------
			class(String), intent(inout) :: self
			type(String), intent(in) :: match, substring
			logical, intent(in), optional :: back
		end subroutine replace_st_inplace

		pure elemental recursive module subroutine replace_chst_inplace(self, match, substring, back)
			!----------------------------------------------------------------------------------------------------------
			!! Matches and replaces all occurrences of a substring elementally in place.
			!----------------------------------------------------------------------------------------------------------
			class(String), intent(inout) :: self
			character(len=*), intent(in) :: match
			type(String), intent(in) :: substring
			logical, intent(in), optional :: back
		end subroutine replace_chst_inplace

		pure elemental recursive module subroutine replace_stch_inplace(self, match, substring, back)
			!----------------------------------------------------------------------------------------------------------
			!! Matches and replaces all occurrences of a substring elementally in place.
			!----------------------------------------------------------------------------------------------------------
			class(String), intent(inout) :: self
			type(String), intent(in) :: match
			character(len=*), intent(in) :: substring
			logical, intent(in), optional :: back
		end subroutine replace_stch_inplace

		pure elemental recursive type(String) module function trim_copy(self) result(new)
			!----------------------------------------------------------------------------------------------------------
			!! Returns a copy of a `String` elementally in which each string slice component has been trimmed of any
			!! leading or trailing whitespace.
			!!
			!! For a user reference, see [trim](../page/Ref/string-methods.html#trim).
			!----------------------------------------------------------------------------------------------------------
			class(String), intent(in) :: self
		end function trim_copy

		pure elemental recursive module subroutine trim_inplace(self)
			!----------------------------------------------------------------------------------------------------------
			!! Removes any leading or trailing whitespace of the string slice component of a `String` elementally and
			!! in place.
			!!
			!! For a user reference, see [trim_inplace](../page/Ref/string-methods.html#trim_inplace).
			!----------------------------------------------------------------------------------------------------------
			class(String), intent(inout) :: self
		end subroutine trim_inplace

		impure recursive module subroutine write_file(self, cell_array, file_name, row_separator, column_separator)
			!----------------------------------------------------------------------------------------------------------
			!! Writes the content of a cell array to a text file. The cell array's entire contents are populated into
			!! `self` and then streamed to an external text file using the designated `row_separator` and
			!! `column_separator` whose default values are `LF` and `COMMA` respectively.
			!!
			!! For a user reference, see [write_file](../page/Ref/string-methods.html#write_file).
			!----------------------------------------------------------------------------------------------------------
			class(String), intent(inout) :: self
			type(String), dimension(:,:), intent(in) :: cell_array
			character(len=*), intent(in) :: file_name
			character(len=*), intent(in), optional :: row_separator, column_separator
		end subroutine write_file

		impure recursive module subroutine write_string(substring, unit, iotype, v_list, iostat, iomsg)
			!----------------------------------------------------------------------------------------------------------
			!! Formatted write DTIO procedure for type `String`.
			!----------------------------------------------------------------------------------------------------------
			class(String), intent(in) :: substring
			integer, intent(in) :: unit
			character(len=*), intent(in) :: iotype
			integer, dimension(:), intent(in) :: v_list
			integer, intent(out) :: iostat
			character(len=*), intent(inout) :: iomsg
		end subroutine write_string
	end interface

	interface operator(//)                                                                        ! Submodule operators
		!--------------------------------------------------------------------------------------------------------------
		!! Concatenation operator for `character` and `String`, lifted from `character`. Mixed type concatenation of
		!! `character` and `String` is explicitly defined.
		!!
		!! For a user reference, see [Concatenation](../page/Ref/operators.html#concatenation).
		!--------------------------------------------------------------------------------------------------------------
		pure elemental recursive type(String) module function string_concatenation(Stringl, Stringr) result(new)
			class(String), intent(in) :: Stringl, Stringr
		end function string_concatenation

		pure elemental recursive type(String) module function string_char_concatenation(Stringl, charsr) result(new)
			class(String), intent(in) :: Stringl
			character(len=*), intent(in) :: charsr
		end function string_char_concatenation

		pure elemental recursive type(String) module function char_string_concatenation(charsl, Stringr) result(new)
			character(len=*), intent(in) :: charsl
			class(String), intent(in) :: Stringr
		end function char_string_concatenation
	end interface

	interface operator(+)                                                                         ! Submodule operators
		!--------------------------------------------------------------------------------------------------------------
		!! Concatenation operator for `character` and `String` (as addition). Mixed type concatenation of
		!! `character` and `String` is explicitly defined.
		!!
		!! For a user reference, see [Concatenation](../page/Ref/operators.html#concatenation).
		!--------------------------------------------------------------------------------------------------------------
		pure elemental recursive module function char_concat_plus(charsl, charsr) result(new)
			character(len=*), intent(in) :: charsl, charsr
			character(len=len(charsl)+len(charsr)) :: new
		end function char_concat_plus

		pure elemental recursive type(String) module function string_concat_plus(Stringl, Stringr) result(new)
			class(String), intent(in) :: Stringl, Stringr
		end function string_concat_plus

		pure elemental recursive type(String) module function string_char_concat_plus(Stringl, charsr) result(new)
			class(String), intent(in) :: Stringl
			character(len=*), intent(in) :: charsr
		end function string_char_concat_plus

		pure elemental recursive type(String) module function char_string_concat_plus(charsl, Stringr) result(new)
			character(len=*), intent(in) :: charsl
			class(String), intent(in) :: Stringr
		end function char_string_concat_plus
	end interface

	interface operator(-)                                                                         ! Submodule operators
		!--------------------------------------------------------------------------------------------------------------
		!! Excision operator for `character` and `String` (as subtraction). Mixed type excision of `character` and
		!! `String` is explicitly defined.
		!!
		!! For a user reference, see [Excision](../page/Ref/operators.html#excision).
		!--------------------------------------------------------------------------------------------------------------
		pure elemental recursive type(String) module function char_excision(charsl, charsr) result(new)
			character(len=*), intent(in) :: charsl, charsr
		end function char_excision

		pure elemental recursive type(String) module function string_excision(Stringl, Stringr) result(new)
			class(String), intent(in) :: Stringl, Stringr
		end function string_excision

		pure elemental recursive type(String) module function string_char_excision(Stringl, charsr) result(new)
			class(String), intent(in) :: Stringl
			character(len=*), intent(in) :: charsr
		end function string_char_excision

		pure elemental recursive type(String) module function char_string_excision(charsl, Stringr) result(new)
			character(len=*), intent(in) :: charsl
			class(String), intent(in) :: Stringr
		end function char_string_excision
	end interface

	interface operator(**)                                                                        ! Submodule operators
		!--------------------------------------------------------------------------------------------------------------
		!! Repetition operator for `character` and `String` (as exponentiation).
		!!
		!! For a user reference, see [Repetition](../page/Ref/operators.html#repetition).
		!--------------------------------------------------------------------------------------------------------------
		pure elemental recursive module function repeat_chars(char_base, ncopies) result(new)
			character(len=*), intent(in) :: char_base
			integer, intent(in) :: ncopies
			character(len=len(char_base)*ncopies) :: new
		end function repeat_chars

		pure elemental recursive type(String) module function repeat_String(String_base, ncopies) result(new)
			class(String), intent(in) :: String_base
			integer, intent(in) :: ncopies
		end function repeat_String
	end interface

	interface operator(==)                                                                        ! Submodule operators
		!--------------------------------------------------------------------------------------------------------------
		!! Equivalence operator for `character` and `String`. Mixed type equivalence of `character` and `String` is
		!! explicitly defined.
		!!
		!! For a user reference, see [Equivalence](../page/Ref/operators.html#equivalence).
		!!
		!! @note The equivalence operator `==` is interchangeable with `.eq.`.
		!--------------------------------------------------------------------------------------------------------------
		pure elemental recursive logical module function string_equivalence(Stringl, Stringr) result(equal)
			class(String), intent(in) :: Stringl, Stringr
		end function string_equivalence

		pure elemental recursive logical module function string_char_equivalence(Stringl, charsr) result(equal)
			class(String), intent(in) :: Stringl
			character(len=*), intent(in) :: charsr
		end function string_char_equivalence

		pure elemental recursive logical module function char_string_equivalence(charsl, Stringr) result(equal)
			character(len=*), intent(in) :: charsl
			class(String), intent(in) :: Stringr
		end function char_string_equivalence
	end interface

	interface operator(/=)                                                                        ! Submodule operators
		!--------------------------------------------------------------------------------------------------------------
		!! Non-equivalence operator for `character` and `String`. Mixed type non-equivalence of `character` and
		!! `String` is explicitly defined.
		!!
		!! For a user reference, see [Non-equivalence](../page/Ref/operators.html#non-equivalence).
		!!
		!! @note The non-equivalence operator `/=` is interchangeable with `.ne.`.
		!--------------------------------------------------------------------------------------------------------------
		pure elemental recursive logical module function string_nonequivalence(Stringl, Stringr) result(unequal)
			class(String), intent(in) :: Stringl, Stringr
		end function string_nonequivalence

		pure elemental recursive logical module function string_char_nonequivalence(Stringl, charsr) result(unequal)
			class(String), intent(in) :: Stringl
			character(len=*), intent(in) :: charsr
		end function string_char_nonequivalence

		pure elemental recursive logical module function char_string_nonequivalence(charsl, Stringr) result(unequal)
			character(len=*), intent(in) :: charsl
			class(String), intent(in) :: Stringr
		end function char_string_nonequivalence
	end interface

	interface String                                                                            ! Submodule internal_io
		!--------------------------------------------------------------------------------------------------------------
		!! Function for transforming numeric or `character` data into a [String](../type/string.html) type.
		!!
		!! The interface for `String` is identical to that of `str` but with a return type of `String`, allowing for
		!! elemental assignments and access to the various `String` methods for advanced character handling. For the
		!! complement of `String`, see [cast](../page/Ref/cast.html).
		!!
		!! For a user reference, see [String](../page/Ref/string.html),
		!! [String methods](../page/Ref/string-methods.html), and [Operators](../page/Ref/operators.html).
		!--------------------------------------------------------------------------------------------------------------
		pure elemental recursive type(String) module function new_Str_c128(x, locale, fmt, decimals, im) result(new)
			complex(real128), intent(in) :: x
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
			character(len=*), intent(in), optional :: im
		end function new_Str_c128
		pure elemental recursive type(String) module function new_Str_c64(x, locale, fmt, decimals, im) result(new)
			complex(real64), intent(in) :: x
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
			character(len=*), intent(in), optional :: im
		end function new_Str_c64
		pure elemental recursive type(String) module function new_Str_c32(x, locale, fmt, decimals, im) result(new)
			complex(real32), intent(in) :: x
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
			character(len=*), intent(in), optional :: im
		end function new_Str_c32

		pure elemental recursive type(String) module function new_Str_r128(x, locale, fmt, decimals) result(new)
			real(real128), intent(in) :: x
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
		end function new_Str_r128
		pure elemental recursive type(String) module function new_Str_r64(x, locale, fmt, decimals) result(new)
			real(real64), intent(in) :: x
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
		end function new_Str_r64
		pure elemental recursive type(String) module function new_Str_r32(x, locale, fmt, decimals) result(new)
			real(real32), intent(in) :: x
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
		end function new_Str_r32

		pure elemental recursive type(String) module function new_Str_i64(x, fmt) result(new)
			integer(int64), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
		end function new_Str_i64
		pure elemental recursive type(String) module function new_Str_i32(x, fmt) result(new)
			integer(int32), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
		end function new_Str_i32
		pure elemental recursive type(String) module function new_Str_i16(x, fmt) result(new)
			integer(int16), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
		end function new_Str_i16
		pure elemental recursive type(String) module function new_Str_i8(x, fmt) result(new)
			integer(int8), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
		end function new_Str_i8

		pure elemental recursive type(String) module function new_Str_string(x) result(new)
			class(String), intent(in) :: x
		end function new_Str_string

		pure elemental recursive type(String) module function new_Str_char(x) result(new)
			character(len=*), intent(in) :: x
		end function new_Str_char

		pure elemental recursive type(String) module function new_Str_empty() result(new)
			! No arguments
		end function new_Str_empty
	end interface

	interface str                                                                               ! Submodule internal_io
		!--------------------------------------------------------------------------------------------------------------
		!! Function for representing a scalar number as a `character` string.
		!!
		!! By default behavior, `str` will write a `real` or `complex` number using a number of significant digits
		!! required in the worst case for a lossless round-trip conversion starting with the internal model
		!! representation of `x`. For the complement of `str`, see [cast](../page/Ref/cast.html).
		!!
		!! For a user reference, see [str](../page/Ref/str.html).
		!--------------------------------------------------------------------------------------------------------------
		pure recursive module function str_c128(x, locale, fmt, decimals, im) result(x_str)
			complex(real128), intent(in) :: x
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
			character(len=*), intent(in), optional :: im
			character(len=:), allocatable :: x_str
		end function str_c128
		pure recursive module function str_c64(x, locale, fmt, decimals, im) result(x_str)
			complex(real64), intent(in) :: x
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
			character(len=*), intent(in), optional :: im
			character(len=:), allocatable :: x_str
		end function str_c64
		pure recursive module function str_c32(x, locale, fmt, decimals, im) result(x_str)
			complex(real32), intent(in) :: x
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
			character(len=*), intent(in), optional :: im
			character(len=:), allocatable :: x_str
		end function str_c32

		pure recursive module function str_r128(x, locale, fmt, decimals) result(x_str)
			real(real128), intent(in) :: x
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
			character(len=:), allocatable :: x_str
		end function str_r128
		pure recursive module function str_r64(x, locale, fmt, decimals) result(x_str)
			real(real64), intent(in) :: x
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
			character(len=:), allocatable :: x_str
		end function str_r64
		pure recursive module function str_r32(x, locale, fmt, decimals) result(x_str)
			real(real32), intent(in) :: x
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
			character(len=:), allocatable :: x_str
		end function str_r32

		pure recursive module function str_i64(x, fmt) result(x_str)
			integer(int64), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
			character(len=:), allocatable :: x_str
		end function str_i64
		pure recursive module function str_i32(x, fmt) result(x_str)
			integer(int32), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
			character(len=:), allocatable :: x_str
		end function str_i32
		pure recursive module function str_i16(x, fmt) result(x_str)
			integer(int16), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
			character(len=:), allocatable :: x_str
		end function str_i16
		pure recursive module function str_i8(x, fmt) result(x_str)
			integer(int8), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
			character(len=:), allocatable :: x_str
		end function str_i8

		pure recursive module function str_string(x) result(x_str)
			class(String), intent(in) :: x
			character(len=:), allocatable :: x_str
		end function str_string

		pure recursive module function str_char(x) result(x_str)
			character(len=*), intent(in) :: x
			character(len=:), allocatable :: x_str
		end function str_char

		pure recursive module function str_empty() result(x_str)
			character(len=:), allocatable :: x_str
		end function str_empty
	end interface

	interface cast                                                                              ! Submodule internal_io
		!--------------------------------------------------------------------------------------------------------------
		!! Subroutine for casting a `character` or `String` into a number.
		!!
		!! For the complement of `cast`, see [String](../page/Ref/string.html) and [str](../page/Ref/str.html).
		!!
		!! For a user reference, see [cast](../page/Ref/cast.html).
		!--------------------------------------------------------------------------------------------------------------
		pure elemental recursive module subroutine cast_string_c128(substring, into, locale, fmt, im)
			class(String), intent(in) :: substring
			complex(real128), intent(out) :: into
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
			character(len=*), intent(in), optional :: im
		end subroutine cast_string_c128
		pure elemental recursive module subroutine cast_string_c64(substring, into, locale, fmt, im)
			class(String), intent(in) :: substring
			complex(real64), intent(out) :: into
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
			character(len=*), intent(in), optional :: im
		end subroutine cast_string_c64
		pure elemental recursive module subroutine cast_string_c32(substring, into, locale, fmt, im)
			class(String), intent(in) :: substring
			complex(real32), intent(out) :: into
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
			character(len=*), intent(in), optional :: im
		end subroutine cast_string_c32

		pure elemental recursive module subroutine cast_string_r128(substring, into, locale, fmt)
			class(String), intent(in) :: substring
			real(real128), intent(out) :: into
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
		end subroutine cast_string_r128
		pure elemental recursive module subroutine cast_string_r64(substring, into, locale, fmt)
			class(String), intent(in) :: substring
			real(real64), intent(out) :: into
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
		end subroutine cast_string_r64
		pure elemental recursive module subroutine cast_string_r32(substring, into, locale, fmt)
			class(String), intent(in) :: substring
			real(real32), intent(out) :: into
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
		end subroutine cast_string_r32

		pure elemental recursive module subroutine cast_string_i64(substring, into, fmt)
			class(String), intent(in) :: substring
			integer(int64), intent(out) :: into
			character(len=*), intent(in), optional :: fmt
		end subroutine cast_string_i64
		pure elemental recursive module subroutine cast_string_i32(substring, into, fmt)
			class(String), intent(in) :: substring
			integer(int32), intent(out) :: into
			character(len=*), intent(in), optional :: fmt
		end subroutine cast_string_i32
		pure elemental recursive module subroutine cast_string_i16(substring, into, fmt)
			class(String), intent(in) :: substring
			integer(int16), intent(out) :: into
			character(len=*), intent(in), optional :: fmt
		end subroutine cast_string_i16
		pure elemental recursive module subroutine cast_string_i8(substring, into, fmt)
			class(String), intent(in) :: substring
			integer(int8), intent(out) :: into
			character(len=*), intent(in), optional :: fmt
		end subroutine cast_string_i8

		pure recursive module subroutine cast_c128(substring, into, locale, fmt, im)
			character(len=*), intent(in) :: substring
			complex(real128), intent(out) :: into
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
			character(len=*), intent(in), optional :: im
		end subroutine cast_c128
		pure recursive module subroutine cast_c64(substring, into, locale, fmt, im)
			character(len=*), intent(in) :: substring
			complex(real64), intent(out) :: into
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
			character(len=*), intent(in), optional :: im
		end subroutine cast_c64
		pure recursive module subroutine cast_c32(substring, into, locale, fmt, im)
			character(len=*), intent(in) :: substring
			complex(real32), intent(out) :: into
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
			character(len=*), intent(in), optional :: im
		end subroutine cast_c32

		pure recursive module subroutine cast_r128(substring, into, locale, fmt)
			character(len=*), intent(in) :: substring
			real(real128), intent(out) :: into
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
		end subroutine cast_r128
		pure recursive module subroutine cast_r64(substring, into, locale, fmt)
			character(len=*), intent(in) :: substring
			real(real64), intent(out) :: into
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
		end subroutine cast_r64
		pure recursive module subroutine cast_r32(substring, into, locale, fmt)
			character(len=*), intent(in) :: substring
			real(real32), intent(out) :: into
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
		end subroutine cast_r32

		pure recursive module subroutine cast_i64(substring, into, fmt)
			character(len=*), intent(in) :: substring
			integer(int64), intent(out) :: into
			character(len=*), intent(in), optional :: fmt
		end subroutine cast_i64
		pure recursive module subroutine cast_i32(substring, into, fmt)
			character(len=*), intent(in) :: substring
			integer(int32), intent(out) :: into
			character(len=*), intent(in), optional :: fmt
		end subroutine cast_i32
		pure recursive module subroutine cast_i16(substring, into, fmt)
			character(len=*), intent(in) :: substring
			integer(int16), intent(out) :: into
			character(len=*), intent(in), optional :: fmt
		end subroutine cast_i16
		pure recursive module subroutine cast_i8(substring, into, fmt)
			character(len=*), intent(in) :: substring
			integer(int8), intent(out) :: into
			character(len=*), intent(in), optional :: fmt
		end subroutine cast_i8
	end interface

	interface glue																				 ! Submodule glue_split
		!--------------------------------------------------------------------------------------------------------------
		!! Function for gluing a vector of `tokens` into a scalar `character` or `String`.
		!!
		!! For the complement of `glue`, see [split](../page/Ref/glue-split.html).
		!!
		!! For a user reference, see [glue](../page/Ref/glue-split.html).
		!--------------------------------------------------------------------------------------------------------------
		pure recursive module function glue_char(tokens, separator) result(new)
			character(len=*), dimension(:), intent(in) :: tokens
			character(len=*), intent(in), optional :: separator
			character(len=:), allocatable :: new
		end function glue_char

		pure recursive type(String) module function glue_string(tokens, separator) result(new)
			type(String), dimension(:), intent(in) :: tokens
			character(len=*), intent(in), optional :: separator
		end function glue_string
	end interface

	interface split																				 ! Submodule glue_split
		!--------------------------------------------------------------------------------------------------------------
		!! Function for splitting a scalar `character` or `String` into a vector of `tokens`.
		!!
		!! For the complement of `split`, see [glue](../page/Ref/glue-split.html).
		!!
		!! For a user reference, see [split](../page/Ref/glue-split.html).
		!--------------------------------------------------------------------------------------------------------------
		pure recursive module function split_char(substring, separator) result(tokens)
			character(len=*), intent(in) :: substring
			character(len=*), intent(in), optional :: separator
			type(String), allocatable, dimension(:) :: tokens
		end function split_char

		pure recursive module function split_string(substring, separator) result(tokens)
			class(String), intent(in) :: substring
			character(len=*), intent(in), optional :: separator
			type(String), allocatable, dimension(:) :: tokens
		end function split_string
	end interface

	interface to_file                                                                               ! Submodule file_io
		!--------------------------------------------------------------------------------------------------------------
		!! Subroutine for writing an array of uniform numeric data type to an external file.
		!!
		!! The file `file_name` will be created if it does not already exist and will be overwritten if it does exist.
		!! Writing to text is allowed for arrays of rank `1` or `2`, and writing to binary is allowed for arrays of any
		!! rank `1`-`15`. Any invalid actual arguments will be ignored, defaults will be assumed, and a warning message
		!! will be issued on stdout.
		!!
		!! For a user reference, see [to_file](../page/Ref/to_file.html).
		!--------------------------------------------------------------------------------------------------------------
		impure recursive module subroutine to_file_1dc128(x, file_name, header, dim, locale, delim, fmt, decimals, im)
			complex(real128), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in), optional :: header
			integer, intent(in), optional :: dim
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: delim
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
			character(len=*), intent(in), optional :: im
		end subroutine to_file_1dc128
		impure recursive module subroutine to_file_1dc64(x, file_name, header, dim, locale, delim, fmt, decimals, im)
			complex(real64), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in), optional :: header
			integer, intent(in), optional :: dim
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: delim
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
			character(len=*), intent(in), optional :: im
		end subroutine to_file_1dc64
		impure recursive module subroutine to_file_1dc32(x, file_name, header, dim, locale, delim, fmt, decimals, im)
			complex(real32), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in), optional :: header
			integer, intent(in), optional :: dim
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: delim
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
			character(len=*), intent(in), optional :: im
		end subroutine to_file_1dc32

		impure recursive module subroutine to_file_2dc128(x, file_name, header, locale, delim, fmt, decimals, im)
			complex(real128), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in), optional :: header
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: delim
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
			character(len=*), intent(in), optional :: im
		end subroutine to_file_2dc128
		impure recursive module subroutine to_file_2dc64(x, file_name, header, locale, delim, fmt, decimals, im)
			complex(real64), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in), optional :: header
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: delim
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
			character(len=*), intent(in), optional :: im
		end subroutine to_file_2dc64
		impure recursive module subroutine to_file_2dc32(x, file_name, header, locale, delim, fmt, decimals, im)
			complex(real32), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in), optional :: header
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: delim
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
			character(len=*), intent(in), optional :: im
		end subroutine to_file_2dc32

		impure recursive module subroutine to_file_3dc128(x, file_name)
			complex(real128), dimension(:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_3dc128
		impure recursive module subroutine to_file_3dc64(x, file_name)
			complex(real64), dimension(:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_3dc64
		impure recursive module subroutine to_file_3dc32(x, file_name)
			complex(real32), dimension(:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_3dc32

		impure recursive module subroutine to_file_4dc128(x, file_name)
			complex(real128), dimension(:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_4dc128
		impure recursive module subroutine to_file_4dc64(x, file_name)
			complex(real64), dimension(:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_4dc64
		impure recursive module subroutine to_file_4dc32(x, file_name)
			complex(real32), dimension(:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_4dc32

		impure recursive module subroutine to_file_5dc128(x, file_name)
			complex(real128), dimension(:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_5dc128
		impure recursive module subroutine to_file_5dc64(x, file_name)
			complex(real64), dimension(:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_5dc64
		impure recursive module subroutine to_file_5dc32(x, file_name)
			complex(real32), dimension(:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_5dc32

		impure recursive module subroutine to_file_6dc128(x, file_name)
			complex(real128), dimension(:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_6dc128
		impure recursive module subroutine to_file_6dc64(x, file_name)
			complex(real64), dimension(:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_6dc64
		impure recursive module subroutine to_file_6dc32(x, file_name)
			complex(real32), dimension(:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_6dc32

		impure recursive module subroutine to_file_7dc128(x, file_name)
			complex(real128), dimension(:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_7dc128
		impure recursive module subroutine to_file_7dc64(x, file_name)
			complex(real64), dimension(:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_7dc64
		impure recursive module subroutine to_file_7dc32(x, file_name)
			complex(real32), dimension(:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_7dc32

		impure recursive module subroutine to_file_8dc128(x, file_name)
			complex(real128), dimension(:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_8dc128
		impure recursive module subroutine to_file_8dc64(x, file_name)
			complex(real64), dimension(:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_8dc64
		impure recursive module subroutine to_file_8dc32(x, file_name)
			complex(real32), dimension(:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_8dc32

		impure recursive module subroutine to_file_9dc128(x, file_name)
			complex(real128), dimension(:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_9dc128
		impure recursive module subroutine to_file_9dc64(x, file_name)
			complex(real64), dimension(:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_9dc64
		impure recursive module subroutine to_file_9dc32(x, file_name)
			complex(real32), dimension(:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_9dc32

		impure recursive module subroutine to_file_10dc128(x, file_name)
			complex(real128), dimension(:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_10dc128
		impure recursive module subroutine to_file_10dc64(x, file_name)
			complex(real64), dimension(:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_10dc64
		impure recursive module subroutine to_file_10dc32(x, file_name)
			complex(real32), dimension(:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_10dc32

		impure recursive module subroutine to_file_11dc128(x, file_name)
			complex(real128), dimension(:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_11dc128
		impure recursive module subroutine to_file_11dc64(x, file_name)
			complex(real64), dimension(:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_11dc64
		impure recursive module subroutine to_file_11dc32(x, file_name)
			complex(real32), dimension(:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_11dc32

		impure recursive module subroutine to_file_12dc128(x, file_name)
			complex(real128), dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_12dc128
		impure recursive module subroutine to_file_12dc64(x, file_name)
			complex(real64), dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_12dc64
		impure recursive module subroutine to_file_12dc32(x, file_name)
			complex(real32), dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_12dc32

		impure recursive module subroutine to_file_13dc128(x, file_name)
			complex(real128), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_13dc128
		impure recursive module subroutine to_file_13dc64(x, file_name)
			complex(real64), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_13dc64
		impure recursive module subroutine to_file_13dc32(x, file_name)
			complex(real32), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_13dc32

		impure recursive module subroutine to_file_14dc128(x, file_name)
			complex(real128), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_14dc128
		impure recursive module subroutine to_file_14dc64(x, file_name)
			complex(real64), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_14dc64
		impure recursive module subroutine to_file_14dc32(x, file_name)
			complex(real32), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_14dc32

		impure recursive module subroutine to_file_15dc128(x, file_name)
			complex(real128), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_15dc128
		impure recursive module subroutine to_file_15dc64(x, file_name)
			complex(real64), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_15dc64
		impure recursive module subroutine to_file_15dc32(x, file_name)
			complex(real32), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_15dc32

		impure recursive module subroutine to_file_1dr128(x, file_name, header, dim, locale, delim, fmt, decimals)
			real(real128), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in), optional :: header
			integer, intent(in), optional :: dim
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: delim
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
		end subroutine to_file_1dr128
		impure recursive module subroutine to_file_1dr64(x, file_name, header, dim, locale, delim, fmt, decimals)
			real(real64), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in), optional :: header
			integer, intent(in), optional :: dim
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: delim
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
		end subroutine to_file_1dr64
		impure recursive module subroutine to_file_1dr32(x, file_name, header, dim, locale, delim, fmt, decimals)
			real(real32), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in), optional :: header
			integer, intent(in), optional :: dim
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: delim
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
		end subroutine to_file_1dr32

		impure recursive module subroutine to_file_2dr128(x, file_name, header, locale, delim, fmt, decimals)
			real(real128), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in), optional :: header
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: delim
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
		end subroutine to_file_2dr128
		impure recursive module subroutine to_file_2dr64(x, file_name, header, locale, delim, fmt, decimals)
			real(real64), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in), optional :: header
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: delim
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
		end subroutine to_file_2dr64
		impure recursive module subroutine to_file_2dr32(x, file_name, header, locale, delim, fmt, decimals)
			real(real32), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in), optional :: header
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: delim
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
		end subroutine to_file_2dr32

		impure recursive module subroutine to_file_3dr128(x, file_name)
			real(real128), dimension(:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_3dr128
		impure recursive module subroutine to_file_3dr64(x, file_name)
			real(real64), dimension(:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_3dr64
		impure recursive module subroutine to_file_3dr32(x, file_name)
			real(real32), dimension(:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_3dr32

		impure recursive module subroutine to_file_4dr128(x, file_name)
			real(real128), dimension(:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_4dr128
		impure recursive module subroutine to_file_4dr64(x, file_name)
			real(real64), dimension(:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_4dr64
		impure recursive module subroutine to_file_4dr32(x, file_name)
			real(real32), dimension(:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_4dr32

		impure recursive module subroutine to_file_5dr128(x, file_name)
			real(real128), dimension(:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_5dr128
		impure recursive module subroutine to_file_5dr64(x, file_name)
			real(real64), dimension(:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_5dr64
		impure recursive module subroutine to_file_5dr32(x, file_name)
			real(real32), dimension(:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_5dr32

		impure recursive module subroutine to_file_6dr128(x, file_name)
			real(real128), dimension(:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_6dr128
		impure recursive module subroutine to_file_6dr64(x, file_name)
			real(real64), dimension(:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_6dr64
		impure recursive module subroutine to_file_6dr32(x, file_name)
			real(real32), dimension(:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_6dr32

		impure recursive module subroutine to_file_7dr128(x, file_name)
			real(real128), dimension(:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_7dr128
		impure recursive module subroutine to_file_7dr64(x, file_name)
			real(real64), dimension(:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_7dr64
		impure recursive module subroutine to_file_7dr32(x, file_name)
			real(real32), dimension(:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_7dr32

		impure recursive module subroutine to_file_8dr128(x, file_name)
			real(real128), dimension(:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_8dr128
		impure recursive module subroutine to_file_8dr64(x, file_name)
			real(real64), dimension(:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_8dr64
		impure recursive module subroutine to_file_8dr32(x, file_name)
			real(real32), dimension(:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_8dr32

		impure recursive module subroutine to_file_9dr128(x, file_name)
			real(real128), dimension(:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_9dr128
		impure recursive module subroutine to_file_9dr64(x, file_name)
			real(real64), dimension(:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_9dr64
		impure recursive module subroutine to_file_9dr32(x, file_name)
			real(real32), dimension(:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_9dr32

		impure recursive module subroutine to_file_10dr128(x, file_name)
			real(real128), dimension(:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_10dr128
		impure recursive module subroutine to_file_10dr64(x, file_name)
			real(real64), dimension(:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_10dr64
		impure recursive module subroutine to_file_10dr32(x, file_name)
			real(real32), dimension(:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_10dr32

		impure recursive module subroutine to_file_11dr128(x, file_name)
			real(real128), dimension(:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_11dr128
		impure recursive module subroutine to_file_11dr64(x, file_name)
			real(real64), dimension(:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_11dr64
		impure recursive module subroutine to_file_11dr32(x, file_name)
			real(real32), dimension(:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_11dr32

		impure recursive module subroutine to_file_12dr128(x, file_name)
			real(real128), dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_12dr128
		impure recursive module subroutine to_file_12dr64(x, file_name)
			real(real64), dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_12dr64
		impure recursive module subroutine to_file_12dr32(x, file_name)
			real(real32), dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_12dr32

		impure recursive module subroutine to_file_13dr128(x, file_name)
			real(real128), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_13dr128
		impure recursive module subroutine to_file_13dr64(x, file_name)
			real(real64), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_13dr64
		impure recursive module subroutine to_file_13dr32(x, file_name)
			real(real32), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_13dr32

		impure recursive module subroutine to_file_14dr128(x, file_name)
			real(real128), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_14dr128
		impure recursive module subroutine to_file_14dr64(x, file_name)
			real(real64), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_14dr64
		impure recursive module subroutine to_file_14dr32(x, file_name)
			real(real32), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_14dr32

		impure recursive module subroutine to_file_15dr128(x, file_name)
			real(real128), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_15dr128
		impure recursive module subroutine to_file_15dr64(x, file_name)
			real(real64), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_15dr64
		impure recursive module subroutine to_file_15dr32(x, file_name)
			real(real32), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_15dr32

		impure recursive module subroutine to_file_1di64(x, file_name, header, dim, delim, fmt)
			integer(int64), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in), optional :: header
			integer, intent(in), optional :: dim
			character(len=*), intent(in), optional :: delim
			character(len=*), intent(in), optional :: fmt
		end subroutine to_file_1di64
		impure recursive module subroutine to_file_1di32(x, file_name, header, dim, delim, fmt)
			integer(int32), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in), optional :: header
			integer, intent(in), optional :: dim
			character(len=*), intent(in), optional :: delim
			character(len=*), intent(in), optional :: fmt
		end subroutine to_file_1di32
		impure recursive module subroutine to_file_1di16(x, file_name, header, dim, delim, fmt)
			integer(int16), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in), optional :: header
			integer, intent(in), optional :: dim
			character(len=*), intent(in), optional :: delim
			character(len=*), intent(in), optional :: fmt
		end subroutine to_file_1di16
		impure recursive module subroutine to_file_1di8(x, file_name, header, dim, delim, fmt)
			integer(int8), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in), optional :: header
			integer, intent(in), optional :: dim
			character(len=*), intent(in), optional :: delim
			character(len=*), intent(in), optional :: fmt
		end subroutine to_file_1di8

		impure recursive module subroutine to_file_2di64(x, file_name, header, delim, fmt)
			integer(int64), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in), optional :: header
			character(len=*), intent(in), optional :: delim
			character(len=*), intent(in), optional :: fmt
		end subroutine to_file_2di64
		impure recursive module subroutine to_file_2di32(x, file_name, header, delim, fmt)
			integer(int32), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in), optional :: header
			character(len=*), intent(in), optional :: delim
			character(len=*), intent(in), optional :: fmt
		end subroutine to_file_2di32
		impure recursive module subroutine to_file_2di16(x, file_name, header, delim, fmt)
			integer(int16), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in), optional :: header
			character(len=*), intent(in), optional :: delim
			character(len=*), intent(in), optional :: fmt
		end subroutine to_file_2di16
		impure recursive module subroutine to_file_2di8(x, file_name, header, delim, fmt)
			integer(int8), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in), optional :: header
			character(len=*), intent(in), optional :: delim
			character(len=*), intent(in), optional :: fmt
		end subroutine to_file_2di8

		impure recursive module subroutine to_file_3di64(x, file_name)
			integer(int64), dimension(:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_3di64
		impure recursive module subroutine to_file_3di32(x, file_name)
			integer(int32), dimension(:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_3di32
		impure recursive module subroutine to_file_3di16(x, file_name)
			integer(int16), dimension(:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_3di16
		impure recursive module subroutine to_file_3di8(x, file_name)
			integer(int8), dimension(:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_3di8

		impure recursive module subroutine to_file_4di64(x, file_name)
			integer(int64), dimension(:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_4di64
		impure recursive module subroutine to_file_4di32(x, file_name)
			integer(int32), dimension(:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_4di32
		impure recursive module subroutine to_file_4di16(x, file_name)
			integer(int16), dimension(:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_4di16
		impure recursive module subroutine to_file_4di8(x, file_name)
			integer(int8), dimension(:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_4di8

		impure recursive module subroutine to_file_5di64(x, file_name)
			integer(int64), dimension(:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_5di64
		impure recursive module subroutine to_file_5di32(x, file_name)
			integer(int32), dimension(:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_5di32
		impure recursive module subroutine to_file_5di16(x, file_name)
			integer(int16), dimension(:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_5di16
		impure recursive module subroutine to_file_5di8(x, file_name)
			integer(int8), dimension(:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_5di8

		impure recursive module subroutine to_file_6di64(x, file_name)
			integer(int64), dimension(:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_6di64
		impure recursive module subroutine to_file_6di32(x, file_name)
			integer(int32), dimension(:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_6di32
		impure recursive module subroutine to_file_6di16(x, file_name)
			integer(int16), dimension(:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_6di16
		impure recursive module subroutine to_file_6di8(x, file_name)
			integer(int8), dimension(:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_6di8

		impure recursive module subroutine to_file_7di64(x, file_name)
			integer(int64), dimension(:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_7di64
		impure recursive module subroutine to_file_7di32(x, file_name)
			integer(int32), dimension(:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_7di32
		impure recursive module subroutine to_file_7di16(x, file_name)
			integer(int16), dimension(:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_7di16
		impure recursive module subroutine to_file_7di8(x, file_name)
			integer(int8), dimension(:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_7di8

		impure recursive module subroutine to_file_8di64(x, file_name)
			integer(int64), dimension(:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_8di64
		impure recursive module subroutine to_file_8di32(x, file_name)
			integer(int32), dimension(:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_8di32
		impure recursive module subroutine to_file_8di16(x, file_name)
			integer(int16), dimension(:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_8di16
		impure recursive module subroutine to_file_8di8(x, file_name)
			integer(int8), dimension(:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_8di8

		impure recursive module subroutine to_file_9di64(x, file_name)
			integer(int64), dimension(:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_9di64
		impure recursive module subroutine to_file_9di32(x, file_name)
			integer(int32), dimension(:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_9di32
		impure recursive module subroutine to_file_9di16(x, file_name)
			integer(int16), dimension(:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_9di16
		impure recursive module subroutine to_file_9di8(x, file_name)
			integer(int8), dimension(:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_9di8

		impure recursive module subroutine to_file_10di64(x, file_name)
			integer(int64), dimension(:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_10di64
		impure recursive module subroutine to_file_10di32(x, file_name)
			integer(int32), dimension(:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_10di32
		impure recursive module subroutine to_file_10di16(x, file_name)
			integer(int16), dimension(:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_10di16
		impure recursive module subroutine to_file_10di8(x, file_name)
			integer(int8), dimension(:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_10di8

		impure recursive module subroutine to_file_11di64(x, file_name)
			integer(int64), dimension(:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_11di64
		impure recursive module subroutine to_file_11di32(x, file_name)
			integer(int32), dimension(:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_11di32
		impure recursive module subroutine to_file_11di16(x, file_name)
			integer(int16), dimension(:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_11di16
		impure recursive module subroutine to_file_11di8(x, file_name)
			integer(int8), dimension(:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_11di8

		impure recursive module subroutine to_file_12di64(x, file_name)
			integer(int64), dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_12di64
		impure recursive module subroutine to_file_12di32(x, file_name)
			integer(int32), dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_12di32
		impure recursive module subroutine to_file_12di16(x, file_name)
			integer(int16), dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_12di16
		impure recursive module subroutine to_file_12di8(x, file_name)
			integer(int8), dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_12di8

		impure recursive module subroutine to_file_13di64(x, file_name)
			integer(int64), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_13di64
		impure recursive module subroutine to_file_13di32(x, file_name)
			integer(int32), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_13di32
		impure recursive module subroutine to_file_13di16(x, file_name)
			integer(int16), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_13di16
		impure recursive module subroutine to_file_13di8(x, file_name)
			integer(int8), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_13di8

		impure recursive module subroutine to_file_14di64(x, file_name)
			integer(int64), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_14di64
		impure recursive module subroutine to_file_14di32(x, file_name)
			integer(int32), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_14di32
		impure recursive module subroutine to_file_14di16(x, file_name)
			integer(int16), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_14di16
		impure recursive module subroutine to_file_14di8(x, file_name)
			integer(int8), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_14di8

		impure recursive module subroutine to_file_15di64(x, file_name)
			integer(int64), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_15di64
		impure recursive module subroutine to_file_15di32(x, file_name)
			integer(int32), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_15di32
		impure recursive module subroutine to_file_15di16(x, file_name)
			integer(int16), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_15di16
		impure recursive module subroutine to_file_15di8(x, file_name)
			integer(int8), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_file_15di8
	end interface

	interface from_file                                                                             ! Submodule file_io
		!--------------------------------------------------------------------------------------------------------------
		!! Subroutine for reading an external file of uniform numeric data type and format into an array.
		!!
		!! In the event that any actual arguments provided to `from_file` are invalid, the subprogram will not allow
		!! progression of execution of the caller and will issue an `error stop`. This is due to the critical nature of
		!! reads and the fact that the procedure may not be able to make the proper assumptions about the data being
		!! read.
		!!
		!! For a user reference, see [from_file](../page/Ref/from_file.html).
		!--------------------------------------------------------------------------------------------------------------
		impure recursive module subroutine from_textfile_1dc128(file_name, into, header, locale, fmt, im)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:), intent(out) :: into
			logical, intent(in), optional :: header
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
			character(len=*), intent(in), optional :: im
		end subroutine from_textfile_1dc128
		impure recursive module subroutine from_binaryfile_1dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binaryfile_1dc128
		impure recursive module subroutine from_textfile_1dc64(file_name, into, header, locale, fmt, im)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:), intent(out) :: into
			logical, intent(in), optional :: header
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
			character(len=*), intent(in), optional :: im
		end subroutine from_textfile_1dc64
		impure recursive module subroutine from_binaryfile_1dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binaryfile_1dc64
		impure recursive module subroutine from_textfile_1dc32(file_name, into, header, locale, fmt, im)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:), intent(out) :: into
			logical, intent(in), optional :: header
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
			character(len=*), intent(in), optional :: im
		end subroutine from_textfile_1dc32
		impure recursive module subroutine from_binaryfile_1dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binaryfile_1dc32

		impure recursive module subroutine from_textfile_2dc128(file_name, into, header, locale, fmt, im)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:), intent(out) :: into
			logical, intent(in), optional :: header
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
			character(len=*), intent(in), optional :: im
		end subroutine from_textfile_2dc128
		impure recursive module subroutine from_binaryfile_2dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binaryfile_2dc128
		impure recursive module subroutine from_textfile_2dc64(file_name, into, header, locale, fmt, im)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:), intent(out) :: into
			logical, intent(in), optional :: header
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
			character(len=*), intent(in), optional :: im
		end subroutine from_textfile_2dc64
		impure recursive module subroutine from_binaryfile_2dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binaryfile_2dc64
		impure recursive module subroutine from_textfile_2dc32(file_name, into, header, locale, fmt, im)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:), intent(out) :: into
			logical, intent(in), optional :: header
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
			character(len=*), intent(in), optional :: im
		end subroutine from_textfile_2dc32
		impure recursive module subroutine from_binaryfile_2dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binaryfile_2dc32

		impure recursive module subroutine from_file_3dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_3dc128
		impure recursive module subroutine from_file_3dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_3dc64
		impure recursive module subroutine from_file_3dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_3dc32

		impure recursive module subroutine from_file_4dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_4dc128
		impure recursive module subroutine from_file_4dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_4dc64
		impure recursive module subroutine from_file_4dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_4dc32

		impure recursive module subroutine from_file_5dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_5dc128
		impure recursive module subroutine from_file_5dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_5dc64
		impure recursive module subroutine from_file_5dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_5dc32

		impure recursive module subroutine from_file_6dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_6dc128
		impure recursive module subroutine from_file_6dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_6dc64
		impure recursive module subroutine from_file_6dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_6dc32

		impure recursive module subroutine from_file_7dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_7dc128
		impure recursive module subroutine from_file_7dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_7dc64
		impure recursive module subroutine from_file_7dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_7dc32

		impure recursive module subroutine from_file_8dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_8dc128
		impure recursive module subroutine from_file_8dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_8dc64
		impure recursive module subroutine from_file_8dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_8dc32

		impure recursive module subroutine from_file_9dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_9dc128
		impure recursive module subroutine from_file_9dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_9dc64
		impure recursive module subroutine from_file_9dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_9dc32

		impure recursive module subroutine from_file_10dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_10dc128
		impure recursive module subroutine from_file_10dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_10dc64
		impure recursive module subroutine from_file_10dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_10dc32

		impure recursive module subroutine from_file_11dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_11dc128
		impure recursive module subroutine from_file_11dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_11dc64
		impure recursive module subroutine from_file_11dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_11dc32

		impure recursive module subroutine from_file_12dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_12dc128
		impure recursive module subroutine from_file_12dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_12dc64
		impure recursive module subroutine from_file_12dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_12dc32

		impure recursive module subroutine from_file_13dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_13dc128
		impure recursive module subroutine from_file_13dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_13dc64
		impure recursive module subroutine from_file_13dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_13dc32

		impure recursive module subroutine from_file_14dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_14dc128
		impure recursive module subroutine from_file_14dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_14dc64
		impure recursive module subroutine from_file_14dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_14dc32

		impure recursive module subroutine from_file_15dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_15dc128
		impure recursive module subroutine from_file_15dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_15dc64
		impure recursive module subroutine from_file_15dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_15dc32

		impure recursive module subroutine from_textfile_1dr128(file_name, into, header, locale, fmt)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:), intent(out) :: into
			logical, intent(in), optional :: header
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
		end subroutine from_textfile_1dr128
		impure recursive module subroutine from_binaryfile_1dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binaryfile_1dr128
		impure recursive module subroutine from_textfile_1dr64(file_name, into, header, locale, fmt)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:), intent(out) :: into
			logical, intent(in), optional :: header
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
		end subroutine from_textfile_1dr64
		impure recursive module subroutine from_binaryfile_1dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binaryfile_1dr64
		impure recursive module subroutine from_textfile_1dr32(file_name, into, header, locale, fmt)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:), intent(out) :: into
			logical, intent(in), optional :: header
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
		end subroutine from_textfile_1dr32
		impure recursive module subroutine from_binaryfile_1dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binaryfile_1dr32

		impure recursive module subroutine from_textfile_2dr128(file_name, into, header, locale, fmt)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:), intent(out) :: into
			logical, intent(in), optional :: header
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
		end subroutine from_textfile_2dr128
		impure recursive module subroutine from_binaryfile_2dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binaryfile_2dr128
		impure recursive module subroutine from_textfile_2dr64(file_name, into, header, locale, fmt)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:), intent(out) :: into
			logical, intent(in), optional :: header
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
		end subroutine from_textfile_2dr64
		impure recursive module subroutine from_binaryfile_2dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binaryfile_2dr64
		impure recursive module subroutine from_textfile_2dr32(file_name, into, header, locale, fmt)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:), intent(out) :: into
			logical, intent(in), optional :: header
			character(len=*), intent(in), optional :: locale
			character(len=*), intent(in), optional :: fmt
		end subroutine from_textfile_2dr32
		impure recursive module subroutine from_binaryfile_2dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binaryfile_2dr32

		impure recursive module subroutine from_file_3dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_3dr128
		impure recursive module subroutine from_file_3dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_3dr64
		impure recursive module subroutine from_file_3dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_3dr32

		impure recursive module subroutine from_file_4dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_4dr128
		impure recursive module subroutine from_file_4dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_4dr64
		impure recursive module subroutine from_file_4dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_4dr32

		impure recursive module subroutine from_file_5dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_5dr128
		impure recursive module subroutine from_file_5dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_5dr64
		impure recursive module subroutine from_file_5dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_5dr32

		impure recursive module subroutine from_file_6dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_6dr128
		impure recursive module subroutine from_file_6dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_6dr64
		impure recursive module subroutine from_file_6dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_6dr32

		impure recursive module subroutine from_file_7dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_7dr128
		impure recursive module subroutine from_file_7dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_7dr64
		impure recursive module subroutine from_file_7dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_7dr32

		impure recursive module subroutine from_file_8dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_8dr128
		impure recursive module subroutine from_file_8dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_8dr64
		impure recursive module subroutine from_file_8dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_8dr32

		impure recursive module subroutine from_file_9dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_9dr128
		impure recursive module subroutine from_file_9dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_9dr64
		impure recursive module subroutine from_file_9dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_9dr32

		impure recursive module subroutine from_file_10dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_10dr128
		impure recursive module subroutine from_file_10dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_10dr64
		impure recursive module subroutine from_file_10dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_10dr32

		impure recursive module subroutine from_file_11dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_11dr128
		impure recursive module subroutine from_file_11dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_11dr64
		impure recursive module subroutine from_file_11dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_11dr32

		impure recursive module subroutine from_file_12dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_12dr128
		impure recursive module subroutine from_file_12dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_12dr64
		impure recursive module subroutine from_file_12dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_12dr32

		impure recursive module subroutine from_file_13dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_13dr128
		impure recursive module subroutine from_file_13dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_13dr64
		impure recursive module subroutine from_file_13dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_13dr32

		impure recursive module subroutine from_file_14dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_14dr128
		impure recursive module subroutine from_file_14dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_14dr64
		impure recursive module subroutine from_file_14dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_14dr32

		impure recursive module subroutine from_file_15dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_15dr128
		impure recursive module subroutine from_file_15dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_15dr64
		impure recursive module subroutine from_file_15dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_15dr32

		impure recursive module subroutine from_textfile_1di64(file_name, into, header, fmt)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:), intent(out) :: into
			logical, intent(in), optional :: header
			character(len=*), intent(in), optional :: fmt
		end subroutine from_textfile_1di64
		impure recursive module subroutine from_binaryfile_1di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binaryfile_1di64
		impure recursive module subroutine from_textfile_1di32(file_name, into, header, fmt)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:), intent(out) :: into
			logical, intent(in), optional :: header
			character(len=*), intent(in), optional :: fmt
		end subroutine from_textfile_1di32
		impure recursive module subroutine from_binaryfile_1di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binaryfile_1di32
		impure recursive module subroutine from_textfile_1di16(file_name, into, header, fmt)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:), intent(out) :: into
			logical, intent(in), optional :: header
			character(len=*), intent(in), optional :: fmt
		end subroutine from_textfile_1di16
		impure recursive module subroutine from_binaryfile_1di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binaryfile_1di16
		impure recursive module subroutine from_textfile_1di8(file_name, into, header, fmt)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:), intent(out) :: into
			logical, intent(in), optional :: header
			character(len=*), intent(in), optional :: fmt
		end subroutine from_textfile_1di8
		impure recursive module subroutine from_binaryfile_1di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binaryfile_1di8

		impure recursive module subroutine from_textfile_2di64(file_name, into, header, fmt)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:), intent(out) :: into
			logical, intent(in), optional :: header
			character(len=*), intent(in), optional :: fmt
		end subroutine from_textfile_2di64
		impure recursive module subroutine from_binaryfile_2di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binaryfile_2di64
		impure recursive module subroutine from_textfile_2di32(file_name, into, header, fmt)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:), intent(out) :: into
			logical, intent(in), optional :: header
			character(len=*), intent(in), optional :: fmt
		end subroutine from_textfile_2di32
		impure recursive module subroutine from_binaryfile_2di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binaryfile_2di32
		impure recursive module subroutine from_textfile_2di16(file_name, into, header, fmt)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:), intent(out) :: into
			logical, intent(in), optional :: header
			character(len=*), intent(in), optional :: fmt
		end subroutine from_textfile_2di16
		impure recursive module subroutine from_binaryfile_2di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binaryfile_2di16
		impure recursive module subroutine from_textfile_2di8(file_name, into, header, fmt)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:), intent(out) :: into
			logical, intent(in), optional :: header
			character(len=*), intent(in), optional :: fmt
		end subroutine from_textfile_2di8
		impure recursive module subroutine from_binaryfile_2di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binaryfile_2di8

		impure recursive module subroutine from_file_3di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_3di64
		impure recursive module subroutine from_file_3di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_3di32
		impure recursive module subroutine from_file_3di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_3di16
		impure recursive module subroutine from_file_3di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_3di8

		impure recursive module subroutine from_file_4di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_4di64
		impure recursive module subroutine from_file_4di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_4di32
		impure recursive module subroutine from_file_4di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_4di16
		impure recursive module subroutine from_file_4di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_4di8

		impure recursive module subroutine from_file_5di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_5di64
		impure recursive module subroutine from_file_5di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_5di32
		impure recursive module subroutine from_file_5di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_5di16
		impure recursive module subroutine from_file_5di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_5di8

		impure recursive module subroutine from_file_6di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_6di64
		impure recursive module subroutine from_file_6di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_6di32
		impure recursive module subroutine from_file_6di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_6di16
		impure recursive module subroutine from_file_6di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_6di8

		impure recursive module subroutine from_file_7di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_7di64
		impure recursive module subroutine from_file_7di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_7di32
		impure recursive module subroutine from_file_7di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_7di16
		impure recursive module subroutine from_file_7di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_7di8

		impure recursive module subroutine from_file_8di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_8di64
		impure recursive module subroutine from_file_8di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_8di32
		impure recursive module subroutine from_file_8di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_8di16
		impure recursive module subroutine from_file_8di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_8di8

		impure recursive module subroutine from_file_9di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_9di64
		impure recursive module subroutine from_file_9di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_9di32
		impure recursive module subroutine from_file_9di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_9di16
		impure recursive module subroutine from_file_9di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_9di8

		impure recursive module subroutine from_file_10di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_10di64
		impure recursive module subroutine from_file_10di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_10di32
		impure recursive module subroutine from_file_10di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_10di16
		impure recursive module subroutine from_file_10di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_10di8

		impure recursive module subroutine from_file_11di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_11di64
		impure recursive module subroutine from_file_11di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_11di32
		impure recursive module subroutine from_file_11di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_11di16
		impure recursive module subroutine from_file_11di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_11di8

		impure recursive module subroutine from_file_12di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_12di64
		impure recursive module subroutine from_file_12di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_12di32
		impure recursive module subroutine from_file_12di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_12di16
		impure recursive module subroutine from_file_12di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_12di8

		impure recursive module subroutine from_file_13di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_13di64
		impure recursive module subroutine from_file_13di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_13di32
		impure recursive module subroutine from_file_13di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_13di16
		impure recursive module subroutine from_file_13di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_13di8

		impure recursive module subroutine from_file_14di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_14di64
		impure recursive module subroutine from_file_14di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_14di32
		impure recursive module subroutine from_file_14di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_14di16
		impure recursive module subroutine from_file_14di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_14di8

		impure recursive module subroutine from_file_15di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_15di64
		impure recursive module subroutine from_file_15di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_15di32
		impure recursive module subroutine from_file_15di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_15di16
		impure recursive module subroutine from_file_15di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_file_15di8
	end interface

	interface echo                                                                                  ! Submodule text_io
		!--------------------------------------------------------------------------------------------------------------
		!! Subroutine for writing a scalar `character` or `String` to an external text file.
		!!
		!! The file `file_name` will be created if it does not already exist and will be overwritten if `append` is
		!! `.false.` (if it already exists). The default terminator is `LF` (line feed).
		!!
		!! For a user reference, see [echo](../page/Ref/echo.html).
		!--------------------------------------------------------------------------------------------------------------
		impure recursive module subroutine echo_chars(substring, file_name, append, terminator)
			character(len=*), intent(in) :: substring
			character(len=*), intent(in) :: file_name
			logical, intent(in), optional :: append
			character(len=*), intent(in), optional :: terminator
		end subroutine echo_chars

		impure recursive module subroutine echo_string(substring, file_name, append, terminator)
			class(String), intent(in) :: substring
			character(len=*), intent(in) :: file_name
			logical, intent(in), optional :: append
			character(len=*), intent(in), optional :: terminator
		end subroutine echo_string
	end interface

	interface to_text                                                                               ! Submodule text_io
		!! Private interface for writing an array to an external text file.
		impure recursive module subroutine to_text_1dc128(x, file_name, header, dim, locale, delim, fmt, decimals, im)
			complex(real128), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in) :: header
			integer, intent(in) :: dim
			character(len=*), intent(in) :: locale
			character(len=*), intent(in) :: delim
			character(len=*), intent(in) :: fmt
			integer, intent(in) :: decimals
			character(len=*), intent(in) :: im
		end subroutine to_text_1dc128
		impure recursive module subroutine to_text_1dc64(x, file_name, header, dim, locale, delim, fmt, decimals, im)
			complex(real64), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in) :: header
			integer, intent(in) :: dim
			character(len=*), intent(in) :: locale
			character(len=*), intent(in) :: delim
			character(len=*), intent(in) :: fmt
			integer, intent(in) :: decimals
			character(len=*), intent(in) :: im
		end subroutine to_text_1dc64
		impure recursive module subroutine to_text_1dc32(x, file_name, header, dim, locale, delim, fmt, decimals, im)
			complex(real32), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in) :: header
			integer, intent(in) :: dim
			character(len=*), intent(in) :: locale
			character(len=*), intent(in) :: delim
			character(len=*), intent(in) :: fmt
			integer, intent(in) :: decimals
			character(len=*), intent(in) :: im
		end subroutine to_text_1dc32

		impure recursive module subroutine to_text_2dc128(x, file_name, header, locale, delim, fmt, decimals, im)
			complex(real128), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in) :: header
			character(len=*), intent(in) :: locale
			character(len=*), intent(in) :: delim
			character(len=*), intent(in) :: fmt
			integer, intent(in) :: decimals
			character(len=*), intent(in) :: im
		end subroutine to_text_2dc128
		impure recursive module subroutine to_text_2dc64(x, file_name, header, locale, delim, fmt, decimals, im)
			complex(real64), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in) :: header
			character(len=*), intent(in) :: locale
			character(len=*), intent(in) :: delim
			character(len=*), intent(in) :: fmt
			integer, intent(in) :: decimals
			character(len=*), intent(in) :: im
		end subroutine to_text_2dc64
		impure recursive module subroutine to_text_2dc32(x, file_name, header, locale, delim, fmt, decimals, im)
			complex(real32), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in) :: header
			character(len=*), intent(in) :: locale
			character(len=*), intent(in) :: delim
			character(len=*), intent(in) :: fmt
			integer, intent(in) :: decimals
			character(len=*), intent(in) :: im
		end subroutine to_text_2dc32

		impure recursive module subroutine to_text_1dr128(x, file_name, header, dim, locale, delim, fmt, decimals)
			real(real128), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in) :: header
			integer, intent(in) :: dim
			character(len=*), intent(in) :: locale
			character(len=*), intent(in) :: delim
			character(len=*), intent(in) :: fmt
			integer, intent(in) :: decimals
		end subroutine to_text_1dr128
		impure recursive module subroutine to_text_1dr64(x, file_name, header, dim, locale, delim, fmt, decimals)
			real(real64), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in) :: header
			integer, intent(in) :: dim
			character(len=*), intent(in) :: locale
			character(len=*), intent(in) :: delim
			character(len=*), intent(in) :: fmt
			integer, intent(in) :: decimals
		end subroutine to_text_1dr64
		impure recursive module subroutine to_text_1dr32(x, file_name, header, dim, locale, delim, fmt, decimals)
			real(real32), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in) :: header
			integer, intent(in) :: dim
			character(len=*), intent(in) :: locale
			character(len=*), intent(in) :: delim
			character(len=*), intent(in) :: fmt
			integer, intent(in) :: decimals
		end subroutine to_text_1dr32

		impure recursive module subroutine to_text_2dr128(x, file_name, header, locale, delim, fmt, decimals)
			real(real128), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in) :: header
			character(len=*), intent(in) :: locale
			character(len=*), intent(in) :: delim
			character(len=*), intent(in) :: fmt
			integer, intent(in) :: decimals
		end subroutine to_text_2dr128
		impure recursive module subroutine to_text_2dr64(x, file_name, header, locale, delim, fmt, decimals)
			real(real64), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in) :: header
			character(len=*), intent(in) :: locale
			character(len=*), intent(in) :: delim
			character(len=*), intent(in) :: fmt
			integer, intent(in) :: decimals
		end subroutine to_text_2dr64
		impure recursive module subroutine to_text_2dr32(x, file_name, header, locale, delim, fmt, decimals)
			real(real32), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in) :: header
			character(len=*), intent(in) :: locale
			character(len=*), intent(in) :: delim
			character(len=*), intent(in) :: fmt
			integer, intent(in) :: decimals
		end subroutine to_text_2dr32

		impure recursive module subroutine to_text_1di64(x, file_name, header, dim, delim, fmt)
			integer(int64), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in) :: header
			integer, intent(in) :: dim
			character(len=*), intent(in) :: delim
			character(len=*), intent(in) :: fmt
		end subroutine to_text_1di64
		impure recursive module subroutine to_text_1di32(x, file_name, header, dim, delim, fmt)
			integer(int32), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in) :: header
			integer, intent(in) :: dim
			character(len=*), intent(in) :: delim
			character(len=*), intent(in) :: fmt
		end subroutine to_text_1di32
		impure recursive module subroutine to_text_1di16(x, file_name, header, dim, delim, fmt)
			integer(int16), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in) :: header
			integer, intent(in) :: dim
			character(len=*), intent(in) :: delim
			character(len=*), intent(in) :: fmt
		end subroutine to_text_1di16
		impure recursive module subroutine to_text_1di8(x, file_name, header, dim, delim, fmt)
			integer(int8), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in) :: header
			integer, intent(in) :: dim
			character(len=*), intent(in) :: delim
			character(len=*), intent(in) :: fmt
		end subroutine to_text_1di8

		impure recursive module subroutine to_text_2di64(x, file_name, header, delim, fmt)
			integer(int64), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in) :: header
			character(len=*), intent(in) :: delim
			character(len=*), intent(in) :: fmt
		end subroutine to_text_2di64
		impure recursive module subroutine to_text_2di32(x, file_name, header, delim, fmt)
			integer(int32), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in) :: header
			character(len=*), intent(in) :: delim
			character(len=*), intent(in) :: fmt
		end subroutine to_text_2di32
		impure recursive module subroutine to_text_2di16(x, file_name, header, delim, fmt)
			integer(int16), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in) :: header
			character(len=*), intent(in) :: delim
			character(len=*), intent(in) :: fmt
		end subroutine to_text_2di16
		impure recursive module subroutine to_text_2di8(x, file_name, header, delim, fmt)
			integer(int8), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
			character(len=*), dimension(:), intent(in) :: header
			character(len=*), intent(in) :: delim
			character(len=*), intent(in) :: fmt
		end subroutine to_text_2di8
	end interface

	interface from_text                                                                             ! Submodule text_io
		!! Private interface for reading an external text file into an array.
		impure recursive module subroutine from_text_1dc128(file_name, into, header, locale, fmt, im)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:), intent(out) :: into
			logical, intent(in) :: header
			character(len=*), intent(in) :: locale
			character(len=*), intent(in) :: fmt
			character(len=*), intent(in) :: im
		end subroutine from_text_1dc128
		impure recursive module subroutine from_text_1dc64(file_name, into, header, locale, fmt, im)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:), intent(out) :: into
			logical, intent(in) :: header
			character(len=*), intent(in) :: locale
			character(len=*), intent(in) :: fmt
			character(len=*), intent(in) :: im
		end subroutine from_text_1dc64
		impure recursive module subroutine from_text_1dc32(file_name, into, header, locale, fmt, im)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:), intent(out) :: into
			logical, intent(in) :: header
			character(len=*), intent(in) :: locale
			character(len=*), intent(in) :: fmt
			character(len=*), intent(in) :: im
		end subroutine from_text_1dc32

		impure recursive module subroutine from_text_2dc128(file_name, into, header, locale, fmt, im)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:), intent(out) :: into
			logical, intent(in) :: header
			character(len=*), intent(in) :: locale
			character(len=*), intent(in) :: fmt
			character(len=*), intent(in) :: im
		end subroutine from_text_2dc128
		impure recursive module subroutine from_text_2dc64(file_name, into, header, locale, fmt, im)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:), intent(out) :: into
			logical, intent(in) :: header
			character(len=*), intent(in) :: locale
			character(len=*), intent(in) :: fmt
			character(len=*), intent(in) :: im
		end subroutine from_text_2dc64
		impure recursive module subroutine from_text_2dc32(file_name, into, header, locale, fmt, im)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:), intent(out) :: into
			logical, intent(in) :: header
			character(len=*), intent(in) :: locale
			character(len=*), intent(in) :: fmt
			character(len=*), intent(in) :: im
		end subroutine from_text_2dc32

		impure recursive module subroutine from_text_1dr128(file_name, into, header, locale, fmt)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:), intent(out) :: into
			logical, intent(in) :: header
			character(len=*), intent(in) :: locale
			character(len=*), intent(in) :: fmt
		end subroutine from_text_1dr128
		impure recursive module subroutine from_text_1dr64(file_name, into, header, locale, fmt)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:), intent(out) :: into
			logical, intent(in) :: header
			character(len=*), intent(in) :: locale
			character(len=*), intent(in) :: fmt
		end subroutine from_text_1dr64
		impure recursive module subroutine from_text_1dr32(file_name, into, header, locale, fmt)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:), intent(out) :: into
			logical, intent(in) :: header
			character(len=*), intent(in) :: locale
			character(len=*), intent(in) :: fmt
		end subroutine from_text_1dr32

		impure recursive module subroutine from_text_2dr128(file_name, into, header, locale, fmt)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:), intent(out) :: into
			logical, intent(in) :: header
			character(len=*), intent(in) :: locale
			character(len=*), intent(in) :: fmt
		end subroutine from_text_2dr128
		impure recursive module subroutine from_text_2dr64(file_name, into, header, locale, fmt)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:), intent(out) :: into
			logical, intent(in) :: header
			character(len=*), intent(in) :: locale
			character(len=*), intent(in) :: fmt
		end subroutine from_text_2dr64
		impure recursive module subroutine from_text_2dr32(file_name, into, header, locale, fmt)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:), intent(out) :: into
			logical, intent(in) :: header
			character(len=*), intent(in) :: locale
			character(len=*), intent(in) :: fmt
		end subroutine from_text_2dr32

		impure recursive module subroutine from_text_1di64(file_name, into, header, fmt)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:), intent(out) :: into
			logical, intent(in) :: header
			character(len=*), intent(in) :: fmt
		end subroutine from_text_1di64
		impure recursive module subroutine from_text_1di32(file_name, into, header, fmt)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:), intent(out) :: into
			logical, intent(in) :: header
			character(len=*), intent(in) :: fmt
		end subroutine from_text_1di32
		impure recursive module subroutine from_text_1di16(file_name, into, header, fmt)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:), intent(out) :: into
			logical, intent(in) :: header
			character(len=*), intent(in) :: fmt
		end subroutine from_text_1di16
		impure recursive module subroutine from_text_1di8(file_name, into, header, fmt)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:), intent(out) :: into
			logical, intent(in) :: header
			character(len=*), intent(in) :: fmt
		end subroutine from_text_1di8

		impure recursive module subroutine from_text_2di64(file_name, into, header, fmt)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:), intent(out) :: into
			logical, intent(in) :: header
			character(len=*), intent(in) :: fmt
		end subroutine from_text_2di64
		impure recursive module subroutine from_text_2di32(file_name, into, header, fmt)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:), intent(out) :: into
			logical, intent(in) :: header
			character(len=*), intent(in) :: fmt
		end subroutine from_text_2di32
		impure recursive module subroutine from_text_2di16(file_name, into, header, fmt)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:), intent(out) :: into
			logical, intent(in) :: header
			character(len=*), intent(in) :: fmt
		end subroutine from_text_2di16
		impure recursive module subroutine from_text_2di8(file_name, into, header, fmt)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:), intent(out) :: into
			logical, intent(in) :: header
			character(len=*), intent(in) :: fmt
		end subroutine from_text_2di8
	end interface

	interface to_binary                                                                           ! Submodule binary_io
		!! Private interface for writing an array to an external binary file.
		impure recursive module subroutine to_binary_1dc128(x, file_name)
			complex(real128), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_1dc128
		impure recursive module subroutine to_binary_1dc64(x, file_name)
			complex(real64), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_1dc64
		impure recursive module subroutine to_binary_1dc32(x, file_name)
			complex(real32), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_1dc32

		impure recursive module subroutine to_binary_2dc128(x, file_name)
			complex(real128), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_2dc128
		impure recursive module subroutine to_binary_2dc64(x, file_name)
			complex(real64), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_2dc64
		impure recursive module subroutine to_binary_2dc32(x, file_name)
			complex(real32), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_2dc32

		impure recursive module subroutine to_binary_3dc128(x, file_name)
			complex(real128), dimension(:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_3dc128
		impure recursive module subroutine to_binary_3dc64(x, file_name)
			complex(real64), dimension(:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_3dc64
		impure recursive module subroutine to_binary_3dc32(x, file_name)
			complex(real32), dimension(:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_3dc32

		impure recursive module subroutine to_binary_4dc128(x, file_name)
			complex(real128), dimension(:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_4dc128
		impure recursive module subroutine to_binary_4dc64(x, file_name)
			complex(real64), dimension(:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_4dc64
		impure recursive module subroutine to_binary_4dc32(x, file_name)
			complex(real32), dimension(:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_4dc32

		impure recursive module subroutine to_binary_5dc128(x, file_name)
			complex(real128), dimension(:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_5dc128
		impure recursive module subroutine to_binary_5dc64(x, file_name)
			complex(real64), dimension(:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_5dc64
		impure recursive module subroutine to_binary_5dc32(x, file_name)
			complex(real32), dimension(:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_5dc32

		impure recursive module subroutine to_binary_6dc128(x, file_name)
			complex(real128), dimension(:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_6dc128
		impure recursive module subroutine to_binary_6dc64(x, file_name)
			complex(real64), dimension(:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_6dc64
		impure recursive module subroutine to_binary_6dc32(x, file_name)
			complex(real32), dimension(:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_6dc32

		impure recursive module subroutine to_binary_7dc128(x, file_name)
			complex(real128), dimension(:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_7dc128
		impure recursive module subroutine to_binary_7dc64(x, file_name)
			complex(real64), dimension(:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_7dc64
		impure recursive module subroutine to_binary_7dc32(x, file_name)
			complex(real32), dimension(:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_7dc32

		impure recursive module subroutine to_binary_8dc128(x, file_name)
			complex(real128), dimension(:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_8dc128
		impure recursive module subroutine to_binary_8dc64(x, file_name)
			complex(real64), dimension(:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_8dc64
		impure recursive module subroutine to_binary_8dc32(x, file_name)
			complex(real32), dimension(:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_8dc32

		impure recursive module subroutine to_binary_9dc128(x, file_name)
			complex(real128), dimension(:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_9dc128
		impure recursive module subroutine to_binary_9dc64(x, file_name)
			complex(real64), dimension(:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_9dc64
		impure recursive module subroutine to_binary_9dc32(x, file_name)
			complex(real32), dimension(:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_9dc32

		impure recursive module subroutine to_binary_10dc128(x, file_name)
			complex(real128), dimension(:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_10dc128
		impure recursive module subroutine to_binary_10dc64(x, file_name)
			complex(real64), dimension(:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_10dc64
		impure recursive module subroutine to_binary_10dc32(x, file_name)
			complex(real32), dimension(:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_10dc32

		impure recursive module subroutine to_binary_11dc128(x, file_name)
			complex(real128), dimension(:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_11dc128
		impure recursive module subroutine to_binary_11dc64(x, file_name)
			complex(real64), dimension(:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_11dc64
		impure recursive module subroutine to_binary_11dc32(x, file_name)
			complex(real32), dimension(:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_11dc32

		impure recursive module subroutine to_binary_12dc128(x, file_name)
			complex(real128), dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_12dc128
		impure recursive module subroutine to_binary_12dc64(x, file_name)
			complex(real64), dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_12dc64
		impure recursive module subroutine to_binary_12dc32(x, file_name)
			complex(real32), dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_12dc32

		impure recursive module subroutine to_binary_13dc128(x, file_name)
			complex(real128), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_13dc128
		impure recursive module subroutine to_binary_13dc64(x, file_name)
			complex(real64), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_13dc64
		impure recursive module subroutine to_binary_13dc32(x, file_name)
			complex(real32), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_13dc32

		impure recursive module subroutine to_binary_14dc128(x, file_name)
			complex(real128), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_14dc128
		impure recursive module subroutine to_binary_14dc64(x, file_name)
			complex(real64), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_14dc64
		impure recursive module subroutine to_binary_14dc32(x, file_name)
			complex(real32), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_14dc32

		impure recursive module subroutine to_binary_15dc128(x, file_name)
			complex(real128), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_15dc128
		impure recursive module subroutine to_binary_15dc64(x, file_name)
			complex(real64), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_15dc64
		impure recursive module subroutine to_binary_15dc32(x, file_name)
			complex(real32), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_15dc32

		impure recursive module subroutine to_binary_1dr128(x, file_name)
			real(real128), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_1dr128
		impure recursive module subroutine to_binary_1dr64(x, file_name)
			real(real64), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_1dr64
		impure recursive module subroutine to_binary_1dr32(x, file_name)
			real(real32), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_1dr32

		impure recursive module subroutine to_binary_2dr128(x, file_name)
			real(real128), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_2dr128
		impure recursive module subroutine to_binary_2dr64(x, file_name)
			real(real64), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_2dr64
		impure recursive module subroutine to_binary_2dr32(x, file_name)
			real(real32), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_2dr32

		impure recursive module subroutine to_binary_3dr128(x, file_name)
			real(real128), dimension(:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_3dr128
		impure recursive module subroutine to_binary_3dr64(x, file_name)
			real(real64), dimension(:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_3dr64
		impure recursive module subroutine to_binary_3dr32(x, file_name)
			real(real32), dimension(:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_3dr32

		impure recursive module subroutine to_binary_4dr128(x, file_name)
			real(real128), dimension(:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_4dr128
		impure recursive module subroutine to_binary_4dr64(x, file_name)
			real(real64), dimension(:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_4dr64
		impure recursive module subroutine to_binary_4dr32(x, file_name)
			real(real32), dimension(:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_4dr32

		impure recursive module subroutine to_binary_5dr128(x, file_name)
			real(real128), dimension(:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_5dr128
		impure recursive module subroutine to_binary_5dr64(x, file_name)
			real(real64), dimension(:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_5dr64
		impure recursive module subroutine to_binary_5dr32(x, file_name)
			real(real32), dimension(:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_5dr32

		impure recursive module subroutine to_binary_6dr128(x, file_name)
			real(real128), dimension(:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_6dr128
		impure recursive module subroutine to_binary_6dr64(x, file_name)
			real(real64), dimension(:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_6dr64
		impure recursive module subroutine to_binary_6dr32(x, file_name)
			real(real32), dimension(:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_6dr32

		impure recursive module subroutine to_binary_7dr128(x, file_name)
			real(real128), dimension(:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_7dr128
		impure recursive module subroutine to_binary_7dr64(x, file_name)
			real(real64), dimension(:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_7dr64
		impure recursive module subroutine to_binary_7dr32(x, file_name)
			real(real32), dimension(:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_7dr32

		impure recursive module subroutine to_binary_8dr128(x, file_name)
			real(real128), dimension(:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_8dr128
		impure recursive module subroutine to_binary_8dr64(x, file_name)
			real(real64), dimension(:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_8dr64
		impure recursive module subroutine to_binary_8dr32(x, file_name)
			real(real32), dimension(:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_8dr32

		impure recursive module subroutine to_binary_9dr128(x, file_name)
			real(real128), dimension(:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_9dr128
		impure recursive module subroutine to_binary_9dr64(x, file_name)
			real(real64), dimension(:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_9dr64
		impure recursive module subroutine to_binary_9dr32(x, file_name)
			real(real32), dimension(:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_9dr32

		impure recursive module subroutine to_binary_10dr128(x, file_name)
			real(real128), dimension(:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_10dr128
		impure recursive module subroutine to_binary_10dr64(x, file_name)
			real(real64), dimension(:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_10dr64
		impure recursive module subroutine to_binary_10dr32(x, file_name)
			real(real32), dimension(:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_10dr32

		impure recursive module subroutine to_binary_11dr128(x, file_name)
			real(real128), dimension(:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_11dr128
		impure recursive module subroutine to_binary_11dr64(x, file_name)
			real(real64), dimension(:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_11dr64
		impure recursive module subroutine to_binary_11dr32(x, file_name)
			real(real32), dimension(:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_11dr32

		impure recursive module subroutine to_binary_12dr128(x, file_name)
			real(real128), dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_12dr128
		impure recursive module subroutine to_binary_12dr64(x, file_name)
			real(real64), dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_12dr64
		impure recursive module subroutine to_binary_12dr32(x, file_name)
			real(real32), dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_12dr32

		impure recursive module subroutine to_binary_13dr128(x, file_name)
			real(real128), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_13dr128
		impure recursive module subroutine to_binary_13dr64(x, file_name)
			real(real64), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_13dr64
		impure recursive module subroutine to_binary_13dr32(x, file_name)
			real(real32), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_13dr32

		impure recursive module subroutine to_binary_14dr128(x, file_name)
			real(real128), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_14dr128
		impure recursive module subroutine to_binary_14dr64(x, file_name)
			real(real64), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_14dr64
		impure recursive module subroutine to_binary_14dr32(x, file_name)
			real(real32), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_14dr32

		impure recursive module subroutine to_binary_15dr128(x, file_name)
			real(real128), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_15dr128
		impure recursive module subroutine to_binary_15dr64(x, file_name)
			real(real64), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_15dr64
		impure recursive module subroutine to_binary_15dr32(x, file_name)
			real(real32), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_15dr32

		impure recursive module subroutine to_binary_1di64(x, file_name)
			integer(int64), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_1di64
		impure recursive module subroutine to_binary_1di32(x, file_name)
			integer(int32), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_1di32
		impure recursive module subroutine to_binary_1di16(x, file_name)
			integer(int16), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_1di16
		impure recursive module subroutine to_binary_1di8(x, file_name)
			integer(int8), dimension(:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_1di8

		impure recursive module subroutine to_binary_2di64(x, file_name)
			integer(int64), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_2di64
		impure recursive module subroutine to_binary_2di32(x, file_name)
			integer(int32), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_2di32
		impure recursive module subroutine to_binary_2di16(x, file_name)
			integer(int16), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_2di16
		impure recursive module subroutine to_binary_2di8(x, file_name)
			integer(int8), dimension(:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_2di8

		impure recursive module subroutine to_binary_3di64(x, file_name)
			integer(int64), dimension(:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_3di64
		impure recursive module subroutine to_binary_3di32(x, file_name)
			integer(int32), dimension(:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_3di32
		impure recursive module subroutine to_binary_3di16(x, file_name)
			integer(int16), dimension(:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_3di16
		impure recursive module subroutine to_binary_3di8(x, file_name)
			integer(int8), dimension(:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_3di8

		impure recursive module subroutine to_binary_4di64(x, file_name)
			integer(int64), dimension(:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_4di64
		impure recursive module subroutine to_binary_4di32(x, file_name)
			integer(int32), dimension(:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_4di32
		impure recursive module subroutine to_binary_4di16(x, file_name)
			integer(int16), dimension(:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_4di16
		impure recursive module subroutine to_binary_4di8(x, file_name)
			integer(int8), dimension(:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_4di8

		impure recursive module subroutine to_binary_5di64(x, file_name)
			integer(int64), dimension(:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_5di64
		impure recursive module subroutine to_binary_5di32(x, file_name)
			integer(int32), dimension(:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_5di32
		impure recursive module subroutine to_binary_5di16(x, file_name)
			integer(int16), dimension(:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_5di16
		impure recursive module subroutine to_binary_5di8(x, file_name)
			integer(int8), dimension(:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_5di8

		impure recursive module subroutine to_binary_6di64(x, file_name)
			integer(int64), dimension(:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_6di64
		impure recursive module subroutine to_binary_6di32(x, file_name)
			integer(int32), dimension(:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_6di32
		impure recursive module subroutine to_binary_6di16(x, file_name)
			integer(int16), dimension(:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_6di16
		impure recursive module subroutine to_binary_6di8(x, file_name)
			integer(int8), dimension(:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_6di8

		impure recursive module subroutine to_binary_7di64(x, file_name)
			integer(int64), dimension(:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_7di64
		impure recursive module subroutine to_binary_7di32(x, file_name)
			integer(int32), dimension(:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_7di32
		impure recursive module subroutine to_binary_7di16(x, file_name)
			integer(int16), dimension(:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_7di16
		impure recursive module subroutine to_binary_7di8(x, file_name)
			integer(int8), dimension(:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_7di8

		impure recursive module subroutine to_binary_8di64(x, file_name)
			integer(int64), dimension(:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_8di64
		impure recursive module subroutine to_binary_8di32(x, file_name)
			integer(int32), dimension(:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_8di32
		impure recursive module subroutine to_binary_8di16(x, file_name)
			integer(int16), dimension(:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_8di16
		impure recursive module subroutine to_binary_8di8(x, file_name)
			integer(int8), dimension(:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_8di8

		impure recursive module subroutine to_binary_9di64(x, file_name)
			integer(int64), dimension(:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_9di64
		impure recursive module subroutine to_binary_9di32(x, file_name)
			integer(int32), dimension(:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_9di32
		impure recursive module subroutine to_binary_9di16(x, file_name)
			integer(int16), dimension(:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_9di16
		impure recursive module subroutine to_binary_9di8(x, file_name)
			integer(int8), dimension(:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_9di8

		impure recursive module subroutine to_binary_10di64(x, file_name)
			integer(int64), dimension(:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_10di64
		impure recursive module subroutine to_binary_10di32(x, file_name)
			integer(int32), dimension(:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_10di32
		impure recursive module subroutine to_binary_10di16(x, file_name)
			integer(int16), dimension(:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_10di16
		impure recursive module subroutine to_binary_10di8(x, file_name)
			integer(int8), dimension(:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_10di8

		impure recursive module subroutine to_binary_11di64(x, file_name)
			integer(int64), dimension(:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_11di64
		impure recursive module subroutine to_binary_11di32(x, file_name)
			integer(int32), dimension(:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_11di32
		impure recursive module subroutine to_binary_11di16(x, file_name)
			integer(int16), dimension(:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_11di16
		impure recursive module subroutine to_binary_11di8(x, file_name)
			integer(int8), dimension(:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_11di8

		impure recursive module subroutine to_binary_12di64(x, file_name)
			integer(int64), dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_12di64
		impure recursive module subroutine to_binary_12di32(x, file_name)
			integer(int32), dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_12di32
		impure recursive module subroutine to_binary_12di16(x, file_name)
			integer(int16), dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_12di16
		impure recursive module subroutine to_binary_12di8(x, file_name)
			integer(int8), dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_12di8

		impure recursive module subroutine to_binary_13di64(x, file_name)
			integer(int64), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_13di64
		impure recursive module subroutine to_binary_13di32(x, file_name)
			integer(int32), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_13di32
		impure recursive module subroutine to_binary_13di16(x, file_name)
			integer(int16), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_13di16
		impure recursive module subroutine to_binary_13di8(x, file_name)
			integer(int8), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_13di8

		impure recursive module subroutine to_binary_14di64(x, file_name)
			integer(int64), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_14di64
		impure recursive module subroutine to_binary_14di32(x, file_name)
			integer(int32), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_14di32
		impure recursive module subroutine to_binary_14di16(x, file_name)
			integer(int16), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_14di16
		impure recursive module subroutine to_binary_14di8(x, file_name)
			integer(int8), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_14di8

		impure recursive module subroutine to_binary_15di64(x, file_name)
			integer(int64), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_15di64
		impure recursive module subroutine to_binary_15di32(x, file_name)
			integer(int32), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_15di32
		impure recursive module subroutine to_binary_15di16(x, file_name)
			integer(int16), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_15di16
		impure recursive module subroutine to_binary_15di8(x, file_name)
			integer(int8), dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(in) :: x
			character(len=*), intent(in) :: file_name
		end subroutine to_binary_15di8
	end interface

	interface from_binary                                                                         ! Submodule binary_io
		!! Private interface for reading an external binary file into an array.
		impure recursive module subroutine from_binary_1dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_1dc128
		impure recursive module subroutine from_binary_1dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_1dc64
		impure recursive module subroutine from_binary_1dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_1dc32

		impure recursive module subroutine from_binary_2dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_2dc128
		impure recursive module subroutine from_binary_2dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_2dc64
		impure recursive module subroutine from_binary_2dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_2dc32

		impure recursive module subroutine from_binary_3dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_3dc128
		impure recursive module subroutine from_binary_3dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_3dc64
		impure recursive module subroutine from_binary_3dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_3dc32

		impure recursive module subroutine from_binary_4dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_4dc128
		impure recursive module subroutine from_binary_4dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_4dc64
		impure recursive module subroutine from_binary_4dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_4dc32

		impure recursive module subroutine from_binary_5dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_5dc128
		impure recursive module subroutine from_binary_5dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_5dc64
		impure recursive module subroutine from_binary_5dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_5dc32

		impure recursive module subroutine from_binary_6dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_6dc128
		impure recursive module subroutine from_binary_6dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_6dc64
		impure recursive module subroutine from_binary_6dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_6dc32

		impure recursive module subroutine from_binary_7dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_7dc128
		impure recursive module subroutine from_binary_7dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_7dc64
		impure recursive module subroutine from_binary_7dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_7dc32

		impure recursive module subroutine from_binary_8dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_8dc128
		impure recursive module subroutine from_binary_8dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_8dc64
		impure recursive module subroutine from_binary_8dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_8dc32

		impure recursive module subroutine from_binary_9dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_9dc128
		impure recursive module subroutine from_binary_9dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_9dc64
		impure recursive module subroutine from_binary_9dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_9dc32

		impure recursive module subroutine from_binary_10dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_10dc128
		impure recursive module subroutine from_binary_10dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_10dc64
		impure recursive module subroutine from_binary_10dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_10dc32

		impure recursive module subroutine from_binary_11dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_11dc128
		impure recursive module subroutine from_binary_11dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_11dc64
		impure recursive module subroutine from_binary_11dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_11dc32

		impure recursive module subroutine from_binary_12dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_12dc128
		impure recursive module subroutine from_binary_12dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_12dc64
		impure recursive module subroutine from_binary_12dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_12dc32

		impure recursive module subroutine from_binary_13dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_13dc128
		impure recursive module subroutine from_binary_13dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_13dc64
		impure recursive module subroutine from_binary_13dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_13dc32

		impure recursive module subroutine from_binary_14dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_14dc128
		impure recursive module subroutine from_binary_14dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_14dc64
		impure recursive module subroutine from_binary_14dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_14dc32

		impure recursive module subroutine from_binary_15dc128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_15dc128
		impure recursive module subroutine from_binary_15dc64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_15dc64
		impure recursive module subroutine from_binary_15dc32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			complex(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_15dc32

		impure recursive module subroutine from_binary_1dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_1dr128
		impure recursive module subroutine from_binary_1dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_1dr64
		impure recursive module subroutine from_binary_1dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_1dr32

		impure recursive module subroutine from_binary_2dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_2dr128
		impure recursive module subroutine from_binary_2dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_2dr64
		impure recursive module subroutine from_binary_2dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_2dr32

		impure recursive module subroutine from_binary_3dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_3dr128
		impure recursive module subroutine from_binary_3dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_3dr64
		impure recursive module subroutine from_binary_3dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_3dr32

		impure recursive module subroutine from_binary_4dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_4dr128
		impure recursive module subroutine from_binary_4dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_4dr64
		impure recursive module subroutine from_binary_4dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_4dr32

		impure recursive module subroutine from_binary_5dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_5dr128
		impure recursive module subroutine from_binary_5dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_5dr64
		impure recursive module subroutine from_binary_5dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_5dr32

		impure recursive module subroutine from_binary_6dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_6dr128
		impure recursive module subroutine from_binary_6dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_6dr64
		impure recursive module subroutine from_binary_6dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_6dr32

		impure recursive module subroutine from_binary_7dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_7dr128
		impure recursive module subroutine from_binary_7dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_7dr64
		impure recursive module subroutine from_binary_7dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_7dr32

		impure recursive module subroutine from_binary_8dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_8dr128
		impure recursive module subroutine from_binary_8dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_8dr64
		impure recursive module subroutine from_binary_8dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_8dr32

		impure recursive module subroutine from_binary_9dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_9dr128
		impure recursive module subroutine from_binary_9dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_9dr64
		impure recursive module subroutine from_binary_9dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_9dr32

		impure recursive module subroutine from_binary_10dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_10dr128
		impure recursive module subroutine from_binary_10dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_10dr64
		impure recursive module subroutine from_binary_10dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_10dr32

		impure recursive module subroutine from_binary_11dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_11dr128
		impure recursive module subroutine from_binary_11dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_11dr64
		impure recursive module subroutine from_binary_11dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_11dr32

		impure recursive module subroutine from_binary_12dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_12dr128
		impure recursive module subroutine from_binary_12dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_12dr64
		impure recursive module subroutine from_binary_12dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_12dr32

		impure recursive module subroutine from_binary_13dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_13dr128
		impure recursive module subroutine from_binary_13dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_13dr64
		impure recursive module subroutine from_binary_13dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_13dr32

		impure recursive module subroutine from_binary_14dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_14dr128
		impure recursive module subroutine from_binary_14dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_14dr64
		impure recursive module subroutine from_binary_14dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_14dr32

		impure recursive module subroutine from_binary_15dr128(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real128), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_15dr128
		impure recursive module subroutine from_binary_15dr64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_15dr64
		impure recursive module subroutine from_binary_15dr32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			real(real32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_15dr32

		impure recursive module subroutine from_binary_1di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_1di64
		impure recursive module subroutine from_binary_1di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_1di32
		impure recursive module subroutine from_binary_1di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_1di16
		impure recursive module subroutine from_binary_1di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_1di8

		impure recursive module subroutine from_binary_2di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_2di64
		impure recursive module subroutine from_binary_2di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_2di32
		impure recursive module subroutine from_binary_2di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_2di16
		impure recursive module subroutine from_binary_2di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_2di8

		impure recursive module subroutine from_binary_3di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_3di64
		impure recursive module subroutine from_binary_3di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_3di32
		impure recursive module subroutine from_binary_3di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_3di16
		impure recursive module subroutine from_binary_3di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_3di8

		impure recursive module subroutine from_binary_4di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_4di64
		impure recursive module subroutine from_binary_4di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_4di32
		impure recursive module subroutine from_binary_4di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_4di16
		impure recursive module subroutine from_binary_4di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_4di8

		impure recursive module subroutine from_binary_5di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_5di64
		impure recursive module subroutine from_binary_5di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_5di32
		impure recursive module subroutine from_binary_5di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_5di16
		impure recursive module subroutine from_binary_5di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_5di8

		impure recursive module subroutine from_binary_6di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_6di64
		impure recursive module subroutine from_binary_6di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_6di32
		impure recursive module subroutine from_binary_6di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_6di16
		impure recursive module subroutine from_binary_6di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_6di8

		impure recursive module subroutine from_binary_7di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_7di64
		impure recursive module subroutine from_binary_7di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_7di32
		impure recursive module subroutine from_binary_7di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_7di16
		impure recursive module subroutine from_binary_7di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_7di8

		impure recursive module subroutine from_binary_8di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_8di64
		impure recursive module subroutine from_binary_8di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_8di32
		impure recursive module subroutine from_binary_8di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_8di16
		impure recursive module subroutine from_binary_8di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_8di8

		impure recursive module subroutine from_binary_9di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_9di64
		impure recursive module subroutine from_binary_9di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_9di32
		impure recursive module subroutine from_binary_9di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_9di16
		impure recursive module subroutine from_binary_9di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_9di8

		impure recursive module subroutine from_binary_10di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_10di64
		impure recursive module subroutine from_binary_10di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_10di32
		impure recursive module subroutine from_binary_10di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_10di16
		impure recursive module subroutine from_binary_10di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_10di8

		impure recursive module subroutine from_binary_11di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_11di64
		impure recursive module subroutine from_binary_11di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_11di32
		impure recursive module subroutine from_binary_11di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_11di16
		impure recursive module subroutine from_binary_11di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_11di8

		impure recursive module subroutine from_binary_12di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_12di64
		impure recursive module subroutine from_binary_12di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_12di32
		impure recursive module subroutine from_binary_12di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_12di16
		impure recursive module subroutine from_binary_12di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_12di8

		impure recursive module subroutine from_binary_13di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_13di64
		impure recursive module subroutine from_binary_13di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_13di32
		impure recursive module subroutine from_binary_13di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_13di16
		impure recursive module subroutine from_binary_13di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_13di8

		impure recursive module subroutine from_binary_14di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_14di64
		impure recursive module subroutine from_binary_14di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_14di32
		impure recursive module subroutine from_binary_14di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_14di16
		impure recursive module subroutine from_binary_14di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_14di8

		impure recursive module subroutine from_binary_15di64(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int64), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_15di64
		impure recursive module subroutine from_binary_15di32(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int32), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_15di32
		impure recursive module subroutine from_binary_15di16(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int16), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_15di16
		impure recursive module subroutine from_binary_15di8(file_name, into, data_shape)
			character(len=*), intent(in) :: file_name
			integer(int8), allocatable, dimension(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), intent(out) :: into
			integer, dimension(:), intent(in) :: data_shape
		end subroutine from_binary_15di8
	end interface

	interface aprint                                                                         ! Submodule array_printing
		!--------------------------------------------------------------------------------------------------------------
		!! Subroutine for printing arrays and array sections to stdout.
		!!
		!! For a user reference, see [aprint](../page/Ref/aprint.html).
		!--------------------------------------------------------------------------------------------------------------
		impure recursive module subroutine aprint_1dc128(x, fmt, decimals, im)
			complex(real128), dimension(:), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
			character(len=*), intent(in), optional :: im
		end subroutine aprint_1dc128
		impure recursive module subroutine aprint_1dc64(x, fmt, decimals, im)
			complex(real64), dimension(:), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
			character(len=*), intent(in), optional :: im
		end subroutine aprint_1dc64
		impure recursive module subroutine aprint_1dc32(x, fmt, decimals, im)
			complex(real32), dimension(:), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
			character(len=*), intent(in), optional :: im
		end subroutine aprint_1dc32

		impure recursive module subroutine aprint_2dc128(x, fmt, decimals, im)
			complex(real128), dimension(:,:), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
			character(len=*), intent(in), optional :: im
		end subroutine aprint_2dc128
		impure recursive module subroutine aprint_2dc64(x, fmt, decimals, im)
			complex(real64), dimension(:,:), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
			character(len=*), intent(in), optional :: im
		end subroutine aprint_2dc64
		impure recursive module subroutine aprint_2dc32(x, fmt, decimals, im)
			complex(real32), dimension(:,:), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
			character(len=*), intent(in), optional :: im
		end subroutine aprint_2dc32

		impure recursive module subroutine aprint_1dr128(x, fmt, decimals)
			real(real128), dimension(:), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
		end subroutine aprint_1dr128
		impure recursive module subroutine aprint_1dr64(x, fmt, decimals)
			real(real64), dimension(:), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
		end subroutine aprint_1dr64
		impure recursive module subroutine aprint_1dr32(x, fmt, decimals)
			real(real32), dimension(:), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
		end subroutine aprint_1dr32

		impure recursive module subroutine aprint_2dr128(x, fmt, decimals)
			real(real128), dimension(:,:), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
		end subroutine aprint_2dr128
		impure recursive module subroutine aprint_2dr64(x, fmt, decimals)
			real(real64), dimension(:,:), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
		end subroutine aprint_2dr64
		impure recursive module subroutine aprint_2dr32(x, fmt, decimals)
			real(real32), dimension(:,:), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
			integer, intent(in), optional :: decimals
		end subroutine aprint_2dr32

		impure recursive module subroutine aprint_1di64(x, fmt)
			integer(int64), dimension(:), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
		end subroutine aprint_1di64
		impure recursive module subroutine aprint_1di32(x, fmt)
			integer(int32), dimension(:), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
		end subroutine aprint_1di32
		impure recursive module subroutine aprint_1di16(x, fmt)
			integer(int16), dimension(:), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
		end subroutine aprint_1di16
		impure recursive module subroutine aprint_1di8(x, fmt)
			integer(int8), dimension(:), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
		end subroutine aprint_1di8

		impure recursive module subroutine aprint_2di64(x, fmt)
			integer(int64), dimension(:,:), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
		end subroutine aprint_2di64
		impure recursive module subroutine aprint_2di32(x, fmt)
			integer(int32), dimension(:,:), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
		end subroutine aprint_2di32
		impure recursive module subroutine aprint_2di16(x, fmt)
			integer(int16), dimension(:,:), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
		end subroutine aprint_2di16
		impure recursive module subroutine aprint_2di8(x, fmt)
			integer(int8), dimension(:,:), intent(in) :: x
			character(len=*), intent(in), optional :: fmt
		end subroutine aprint_2di8

		impure recursive module subroutine aprint_1dchar(x)
			character(len=*), dimension(:), intent(in) :: x
		end subroutine aprint_1dchar

		impure recursive module subroutine aprint_2dchar(x)
			character(len=*), dimension(:,:), intent(in) :: x
		end subroutine aprint_2dchar

		impure recursive module subroutine aprint_1dString(x)
			class(String), dimension(:), intent(in) :: x
		end subroutine aprint_1dString

		impure recursive module subroutine aprint_2dString(x)
			class(String), dimension(:,:), intent(in) :: x
		end subroutine aprint_2dString
	end interface

	contains
	pure recursive function ext_of(file_name) result(ext)
		! Function for parsing a file name for an extension
		character(len=*), intent(in) :: file_name
		character(len=:), allocatable :: ext

		integer :: i, l

		l = len_trim(file_name)

		do i = l, 1, -1
			if ( file_name(i:i) == POINT ) exit
		end do

		if ( i > 0 ) then
			ext = trim(adjustl(file_name(i+1:l)))
		else
			ext = EMPTY_STR
		end if
	end function ext_of

end module io_fortran_lib

submodule (io_fortran_lib) String_procedures
	!! This submodule provides module procedure implementations for the **type-bound procedures** of type `String`.
	contains
	module procedure as_str
		if ( self%len() < 1 ) then
			string_slice = EMPTY_STR
		else
			string_slice = self%s
		end if
	end procedure as_str

	module procedure count_substring_chars
		integer :: self_len, match_len, i

		self_len = self%len()
		match_len = len(match)

		if ( self_len < 1 ) then
			if ( self_len == match_len ) then
				occurrences = 1; return
			else
				occurrences = 0; return
			end if
		end if

		if ( (match_len == 0) .or. (match_len > self_len) ) then
			occurrences = 0; return
		end if

		i = 1
		occurrences = 0

		counting: do while ( i <= self_len )
			if ( self%s(i:i) == match(1:1) ) then
				if ( i+match_len-1 > self_len ) exit counting

				if ( self%s(i:i+match_len-1) == match ) then
					occurrences = occurrences + 1
					i = i + match_len; cycle counting
				else
					i = i + 1; cycle counting
				end if
			else
				i = i + 1; cycle counting
			end if
		end do counting
	end procedure count_substring_chars

	module procedure count_substring_string
		integer :: self_len, match_len, i

		self_len = self%len()
		match_len = match%len()

		if ( self_len < 1 ) then
			if ( self_len == match_len ) then
				if ( self_len == 0 ) then
					occurrences = 1; return
				else
					occurrences = 0; return
				end if
			else
				occurrences = 0; return
			end if
		end if

		if ( (match_len < 1) .or. (match_len > self_len) ) then
			occurrences = 0; return
		end if

		i = 1
		occurrences = 0

		counting: do while ( i <= self_len )
			if ( self%s(i:i) == match%s(1:1) ) then
				if ( i+match_len-1 > self_len ) exit counting

				if ( self%s(i:i+match_len-1) == match%s ) then
					occurrences = occurrences + 1
					i = i + match_len; cycle counting
				else
					i = i + 1; cycle counting
				end if
			else
				i = i + 1; cycle counting
			end if
		end do counting
	end procedure count_substring_string

	module procedure empty
		self%s = EMPTY_STR
	end procedure empty

	module procedure glue_into_self
		character(len=:), allocatable :: separator_
		integer, allocatable, dimension(:) :: lengths
		integer :: i

		if ( .not. present(separator) ) then
			separator_ = SPACE
		else
			separator_ = separator
		end if

		lengths = tokens%len()

		self%s = EMPTY_STR
		do i = 1, size(tokens)-1
			if ( lengths(i) < 1 ) then
				self%s = self%s//separator_
			else
				self%s = self%s//tokens(i)%s//separator_
			end if
		end do

		if ( lengths(size(tokens)) < 1 ) then
			return
		else
			self%s = self%s//tokens(size(tokens))%s
		end if
	end procedure glue_into_self

	module procedure length
		if ( .not. allocated(self%s) ) then
			self_len = -1
		else
			self_len = len(self%s)
		end if
	end procedure length

	module procedure push_chars
		if ( self%len() < 1 ) then
			self%s = substring
		else
			self%s = self%s//substring
		end if
	end procedure push_chars

	module procedure push_string
		if ( self%len() < 1 ) then
			if ( substring%len() < 1 ) then
				self%s = EMPTY_STR
			else
				self%s = substring%s
			end if
		else
			if ( substring%len() < 1 ) then
				return
			else
				self%s = self%s//substring%s
			end if
		end if
	end procedure push_string

	module procedure read_file
		character(len=:), allocatable :: ext
		integer :: file_unit, file_length, iostat
		logical :: exists

		ext = ext_of(file_name)

		if ( .not. any(TEXT_EXT == ext) ) then
			if ( any(BINARY_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'" in method READ_FILE. Binary data '// &
							   'cannot be read into a String.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'" in '// &
							   'method READ_FILE.'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)
			end if
		end if

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		inquire( file=file_name, size=file_length )

		if ( file_length == 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty.'
			return
		end if

		if ( allocated(self%s) ) deallocate(self%s)

		allocate( character(len=file_length) :: self%s )
		read(unit=file_unit, iostat=iostat) self%s
		close(file_unit)

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		cell_block: block
			type(String), allocatable, dimension(:) :: rows, columns
			character(len=:), allocatable :: row_separator_, column_separator_
			integer :: n_rows, n_columns, i

			if ( .not. present(cell_array) ) then
				if ( present(row_separator) ) then
					write(*,'(a)') LF//'Row separator was specified in method READ_FILE for file "'// &
									   file_name//'" without a cell array output. To use this option, '// &
									   'provide an actual argument to cell_array.'
				end if

				if ( present(column_separator) ) then
					write(*,'(a)') LF//'Column separator was specified in method READ_FILE for file "'// &
									   file_name//'" without a cell array output. To use this option, '// &
									   'provide an actual argument to cell_array.'
				end if

				exit cell_block
			end if

			if ( .not. present(row_separator) ) then
				row_separator_ = LF
			else
				row_separator_ = row_separator
			end if

			if ( .not. present(column_separator) ) then
				column_separator_ = COMMA
			else
				column_separator_ = column_separator
			end if

			rows = self%split(separator=row_separator_)

			if ( row_separator_ == self%s(file_length-len(row_separator_)+1:) ) then
				n_rows = size(rows) - 1
			else
				n_rows = size(rows)
			end if

			call process_quotes(rows, row_separator=row_separator_, column_separator=column_separator_)

			columns = rows(1)%split(separator=column_separator_)
			n_columns = size(columns)

			allocate( cell_array(n_rows, n_columns) )

			cell_array(1,:) = columns
			deallocate(columns)

			if ( n_rows > 1 ) then
				do concurrent (i = 2:n_rows)
					cell_array(i,:) = rows(i)%split(separator=column_separator_)
				end do
			end if

			call cell_array%replace_inplace(match=row_separator_, substring=column_separator_)
		end block cell_block

		contains
		pure elemental recursive subroutine process_quotes(row, row_separator, column_separator)
			type(String), intent(inout) :: row
			character(len=*), intent(in) :: row_separator, column_separator

			type(String) :: new_row
			integer :: row_len, sep_len, diff_len, i
			logical :: in_quote

			row_len = row%len()
			sep_len = len(column_separator)

			if ( row_len < 1 ) return

			in_quote = .false.

			new_row%s = row%s
			diff_len = 0
			i = 1

			replace_sep: do while ( i <= row_len )
				if ( row%s(i:i) == QQUOTE ) then
					in_quote = ( .not. in_quote )
					i = i + 1; cycle replace_sep
				end if

				if ( in_quote ) then
					if ( row%s(i:i) == column_separator(1:1) ) then
						if ( i+sep_len-1 > row_len ) exit replace_sep

						if ( row%s(i:i+sep_len-1) == column_separator ) then
							new_row%s = new_row%s(:i-1+diff_len)//row_separator//new_row%s(i+sep_len+diff_len:)
							diff_len = diff_len + ( len(row_separator) - sep_len )
							i = i + sep_len; cycle replace_sep
						else
							i = i + 1; cycle replace_sep
						end if
					else
						i = i + 1; cycle replace_sep
					end if
				else
					i = i + 1; cycle replace_sep
				end if
			end do replace_sep

			row = new_row%replace(match=QQUOTE, substring=EMPTY_STR)
		end subroutine process_quotes
	end procedure read_file

	module procedure replace_ch_copy
		integer :: i, self_len, match_len, substring_len, diff_len
		logical :: back_

		self_len = self%len()
		match_len = len(match)
		substring_len = len(substring)

		if ( self_len < 1 ) then
			new%s = EMPTY_STR; return
		end if

		if ( (match_len < 1) .or. (match_len > self_len) ) then
			new%s = self%s; return
		end if

		if ( .not. present(back) ) then
			back_ = .false.
		else
			back_ = back
		end if

		new%s = self%s

		if ( .not. back_ ) then
			i = 1; diff_len = 0
			match_and_replace_forward: do while ( i <= self_len )
				if ( self%s(i:i) == match(1:1) ) then
					if ( i+match_len-1 > self_len ) exit match_and_replace_forward

					if ( self%s(i:i+match_len-1) == match ) then
						new%s = new%s(:i-1+diff_len)//substring//new%s(i+match_len+diff_len:)
						diff_len = diff_len + ( substring_len - match_len )
						i = i + match_len; cycle match_and_replace_forward
					else
						i = i + 1; cycle match_and_replace_forward
					end if
				else
					i = i + 1; cycle match_and_replace_forward
				end if
			end do match_and_replace_forward
		else
			i = self_len
			match_and_replace_backward: do while ( i > 0 )
				if ( self%s(i:i) == match(match_len:match_len) ) then
					if ( i-match_len+1 < 1 ) exit match_and_replace_backward

					if ( self%s(i-match_len+1:i) == match ) then
						new%s = new%s(:i-match_len)//substring//new%s(i+1:)
						i = i - match_len; cycle match_and_replace_backward
					else
						i = i - 1; cycle match_and_replace_backward
					end if
				else
					i = i - 1; cycle match_and_replace_backward
				end if
			end do match_and_replace_backward
		end if
	end procedure replace_ch_copy

	module procedure replace_st_copy
		character(len=:), allocatable :: substring_
		integer :: i, self_len, match_len, substring_len, diff_len
		logical :: back_

		self_len = self%len()
		match_len = match%len()
		substring_len = substring%len()

		if ( self_len < 1 ) then
			new%s = EMPTY_STR; return
		end if

		if ( (match_len < 1) .or. (match_len > self_len) ) then
			new%s = self%s; return
		end if

		if ( substring_len < 1 ) then
			substring_ = EMPTY_STR
		else
			substring_ = substring%s
		end if

		if ( .not. present(back) ) then
			back_ = .false.
		else
			back_ = back
		end if

		new%s = self%s

		if ( .not. back_ ) then
			i = 1; diff_len = 0
			match_and_replace_forward: do while ( i <= self_len )
				if ( self%s(i:i) == match%s(1:1) ) then
					if ( i+match_len-1 > self_len ) exit match_and_replace_forward

					if ( self%s(i:i+match_len-1) == match%s ) then
						new%s = new%s(:i-1+diff_len)//substring_//new%s(i+match_len+diff_len:)
						diff_len = diff_len + ( substring_len - match_len )
						i = i + match_len; cycle match_and_replace_forward
					else
						i = i + 1; cycle match_and_replace_forward
					end if
				else
					i = i + 1; cycle match_and_replace_forward
				end if
			end do match_and_replace_forward
		else
			i = self_len
			match_and_replace_backward: do while ( i > 0 )
				if ( self%s(i:i) == match%s(match_len:match_len) ) then
					if ( i-match_len+1 < 1 ) exit match_and_replace_backward

					if ( self%s(i-match_len+1:i) == match%s ) then
						new%s = new%s(:i-match_len)//substring_//new%s(i+1:)
						i = i - match_len; cycle match_and_replace_backward
					else
						i = i - 1; cycle match_and_replace_backward
					end if
				else
					i = i - 1; cycle match_and_replace_backward
				end if
			end do match_and_replace_backward
		end if
	end procedure replace_st_copy

	module procedure replace_chst_copy
		character(len=:), allocatable :: substring_
		integer :: i, self_len, match_len, substring_len, diff_len
		logical :: back_

		self_len = self%len()
		match_len = len(match)
		substring_len = substring%len()

		if ( self_len < 1 ) then
			new%s = EMPTY_STR; return
		end if

		if ( (match_len < 1) .or. (match_len > self_len) ) then
			new%s = self%s; return
		end if

		if ( substring_len < 1 ) then
			substring_ = EMPTY_STR
		else
			substring_ = substring%s
		end if

		if ( .not. present(back) ) then
			back_ = .false.
		else
			back_ = back
		end if

		new%s = self%s

		if ( .not. back_ ) then
			i = 1; diff_len = 0
			match_and_replace_forward: do while ( i <= self_len )
				if ( self%s(i:i) == match(1:1) ) then
					if ( i+match_len-1 > self_len ) exit match_and_replace_forward

					if ( self%s(i:i+match_len-1) == match ) then
						new%s = new%s(:i-1+diff_len)//substring_//new%s(i+match_len+diff_len:)
						diff_len = diff_len + ( substring_len - match_len )
						i = i + match_len; cycle match_and_replace_forward
					else
						i = i + 1; cycle match_and_replace_forward
					end if
				else
					i = i + 1; cycle match_and_replace_forward
				end if
			end do match_and_replace_forward
		else
			i = self_len
			match_and_replace_backward: do while ( i > 0 )
				if ( self%s(i:i) == match(match_len:match_len) ) then
					if ( i-match_len+1 < 1 ) exit match_and_replace_backward

					if ( self%s(i-match_len+1:i) == match ) then
						new%s = new%s(:i-match_len)//substring_//new%s(i+1:)
						i = i - match_len; cycle match_and_replace_backward
					else
						i = i - 1; cycle match_and_replace_backward
					end if
				else
					i = i - 1; cycle match_and_replace_backward
				end if
			end do match_and_replace_backward
		end if
	end procedure replace_chst_copy

	module procedure replace_stch_copy
		integer :: i, self_len, match_len, substring_len, diff_len
		logical :: back_

		self_len = self%len()
		match_len = match%len()
		substring_len = len(substring)

		if ( self_len < 1 ) then
			new%s = EMPTY_STR; return
		end if

		if ( (match_len < 1) .or. (match_len > self_len) ) then
			new%s = self%s; return
		end if

		if ( .not. present(back) ) then
			back_ = .false.
		else
			back_ = back
		end if

		new%s = self%s

		if ( .not. back_ ) then
			i = 1; diff_len = 0
			match_and_replace_forward: do while ( i <= self_len )
				if ( self%s(i:i) == match%s(1:1) ) then
					if ( i+match_len-1 > self_len ) exit match_and_replace_forward

					if ( self%s(i:i+match_len-1) == match%s ) then
						new%s = new%s(:i-1+diff_len)//substring//new%s(i+match_len+diff_len:)
						diff_len = diff_len + ( substring_len - match_len )
						i = i + match_len; cycle match_and_replace_forward
					else
						i = i + 1; cycle match_and_replace_forward
					end if
				else
					i = i + 1; cycle match_and_replace_forward
				end if
			end do match_and_replace_forward
		else
			i = self_len
			match_and_replace_backward: do while ( i > 0 )
				if ( self%s(i:i) == match%s(match_len:match_len) ) then
					if ( i-match_len+1 < 1 ) exit match_and_replace_backward

					if ( self%s(i-match_len+1:i) == match%s ) then
						new%s = new%s(:i-match_len)//substring//new%s(i+1:)
						i = i - match_len; cycle match_and_replace_backward
					else
						i = i - 1; cycle match_and_replace_backward
					end if
				else
					i = i - 1; cycle match_and_replace_backward
				end if
			end do match_and_replace_backward
		end if
	end procedure replace_stch_copy

	module procedure replace_ch_inplace
		type(String) :: new
		integer :: i, self_len, match_len, substring_len, diff_len
		logical :: back_

		self_len = self%len()
		match_len = len(match)
		substring_len = len(substring)

		if ( self_len < 1 ) then
			self%s = EMPTY_STR; return
		end if

		if ( (match_len < 1) .or. (match_len > self_len) ) return

		if ( .not. present(back) ) then
			back_ = .false.
		else
			back_ = back
		end if

		new%s = self%s
		
		if ( .not. back_ ) then
			i = 1; diff_len = 0
			match_and_replace_forward: do while ( i <= self_len )
				if ( self%s(i:i) == match(1:1) ) then
					if ( i+match_len-1 > self_len ) exit match_and_replace_forward

					if ( self%s(i:i+match_len-1) == match ) then
						new%s = new%s(:i-1+diff_len)//substring//new%s(i+match_len+diff_len:)
						diff_len = diff_len + ( substring_len - match_len )
						i = i + match_len; cycle match_and_replace_forward
					else
						i = i + 1; cycle match_and_replace_forward
					end if
				else
					i = i + 1; cycle match_and_replace_forward
				end if
			end do match_and_replace_forward
		else
			i = self_len
			match_and_replace_backward: do while ( i > 0 )
				if ( self%s(i:i) == match(match_len:match_len) ) then
					if ( i-match_len+1 < 1 ) exit match_and_replace_backward

					if ( self%s(i-match_len+1:i) == match ) then
						new%s = new%s(:i-match_len)//substring//new%s(i+1:)
						i = i - match_len; cycle match_and_replace_backward
					else
						i = i - 1; cycle match_and_replace_backward
					end if
				else
					i = i - 1; cycle match_and_replace_backward
				end if
			end do match_and_replace_backward
		end if

		self%s = new%s
	end procedure replace_ch_inplace

	module procedure replace_st_inplace
		type(String) :: new
		character(len=:), allocatable :: substring_
		integer :: i, self_len, match_len, substring_len, diff_len
		logical :: back_

		self_len = self%len()
		match_len = match%len()
		substring_len = substring%len()

		if ( self_len < 1 ) then
			self%s = EMPTY_STR; return
		end if

		if ( (match_len < 1) .or. (match_len > self_len) ) return

		if ( substring_len < 1 ) then
			substring_ = EMPTY_STR
		else
			substring_ = substring%s
		end if

		if ( .not. present(back) ) then
			back_ = .false.
		else
			back_ = back
		end if

		new%s = self%s
		
		if ( .not. back_ ) then
			i = 1; diff_len = 0
			match_and_replace_forward: do while ( i <= self_len )
				if ( self%s(i:i) == match%s(1:1) ) then
					if ( i+match_len-1 > self_len ) exit match_and_replace_forward

					if ( self%s(i:i+match_len-1) == match%s ) then
						new%s = new%s(:i-1+diff_len)//substring_//new%s(i+match_len+diff_len:)
						diff_len = diff_len + ( substring_len - match_len )
						i = i + match_len; cycle match_and_replace_forward
					else
						i = i + 1; cycle match_and_replace_forward
					end if
				else
					i = i + 1; cycle match_and_replace_forward
				end if
			end do match_and_replace_forward
		else
			i = self_len
			match_and_replace_backward: do while ( i > 0 )
				if ( self%s(i:i) == match%s(match_len:match_len) ) then
					if ( i-match_len+1 < 1 ) exit match_and_replace_backward

					if ( self%s(i-match_len+1:i) == match%s ) then
						new%s = new%s(:i-match_len)//substring_//new%s(i+1:)
						i = i - match_len; cycle match_and_replace_backward
					else
						i = i - 1; cycle match_and_replace_backward
					end if
				else
					i = i - 1; cycle match_and_replace_backward
				end if
			end do match_and_replace_backward
		end if

		self%s = new%s
	end procedure replace_st_inplace

	module procedure replace_chst_inplace
		type(String) :: new
		character(len=:), allocatable :: substring_
		integer :: i, self_len, match_len, substring_len, diff_len
		logical :: back_

		self_len = self%len()
		match_len = len(match)
		substring_len = substring%len()

		if ( self_len < 1 ) then
			self%s = EMPTY_STR; return
		end if

		if ( (match_len < 1) .or. (match_len > self_len) ) return

		if ( substring_len < 1 ) then
			substring_ = EMPTY_STR
		else
			substring_ = substring%s
		end if

		if ( .not. present(back) ) then
			back_ = .false.
		else
			back_ = back
		end if

		new%s = self%s
		
		if ( .not. back_ ) then
			i = 1; diff_len = 0
			match_and_replace_forward: do while ( i <= self_len )
				if ( self%s(i:i) == match(1:1) ) then
					if ( i+match_len-1 > self_len ) exit match_and_replace_forward

					if ( self%s(i:i+match_len-1) == match ) then
						new%s = new%s(:i-1+diff_len)//substring_//new%s(i+match_len+diff_len:)
						diff_len = diff_len + ( substring_len - match_len )
						i = i + match_len; cycle match_and_replace_forward
					else
						i = i + 1; cycle match_and_replace_forward
					end if
				else
					i = i + 1; cycle match_and_replace_forward
				end if
			end do match_and_replace_forward
		else
			i = self_len
			match_and_replace_backward: do while ( i > 0 )
				if ( self%s(i:i) == match(match_len:match_len) ) then
					if ( i-match_len+1 < 1 ) exit match_and_replace_backward

					if ( self%s(i-match_len+1:i) == match ) then
						new%s = new%s(:i-match_len)//substring_//new%s(i+1:)
						i = i - match_len; cycle match_and_replace_backward
					else
						i = i - 1; cycle match_and_replace_backward
					end if
				else
					i = i - 1; cycle match_and_replace_backward
				end if
			end do match_and_replace_backward
		end if

		self%s = new%s
	end procedure replace_chst_inplace

	module procedure replace_stch_inplace
		type(String) :: new
		integer :: i, self_len, match_len, substring_len, diff_len
		logical :: back_

		self_len = self%len()
		match_len = match%len()
		substring_len = len(substring)

		if ( self_len < 1 ) then
			self%s = EMPTY_STR; return
		end if

		if ( (match_len < 1) .or. (match_len > self_len) ) return

		if ( .not. present(back) ) then
			back_ = .false.
		else
			back_ = back
		end if

		new%s = self%s
		
		if ( .not. back_ ) then
			i = 1; diff_len = 0
			match_and_replace_forward: do while ( i <= self_len )
				if ( self%s(i:i) == match%s(1:1) ) then
					if ( i+match_len-1 > self_len ) exit match_and_replace_forward

					if ( self%s(i:i+match_len-1) == match%s ) then
						new%s = new%s(:i-1+diff_len)//substring//new%s(i+match_len+diff_len:)
						diff_len = diff_len + ( substring_len - match_len )
						i = i + match_len; cycle match_and_replace_forward
					else
						i = i + 1; cycle match_and_replace_forward
					end if
				else
					i = i + 1; cycle match_and_replace_forward
				end if
			end do match_and_replace_forward
		else
			i = self_len
			match_and_replace_backward: do while ( i > 0 )
				if ( self%s(i:i) == match%s(match_len:match_len) ) then
					if ( i-match_len+1 < 1 ) exit match_and_replace_backward

					if ( self%s(i-match_len+1:i) == match%s ) then
						new%s = new%s(:i-match_len)//substring//new%s(i+1:)
						i = i - match_len; cycle match_and_replace_backward
					else
						i = i - 1; cycle match_and_replace_backward
					end if
				else
					i = i - 1; cycle match_and_replace_backward
				end if
			end do match_and_replace_backward
		end if

		self%s = new%s
	end procedure replace_stch_inplace

	module procedure trim_copy
		if ( self%len() < 1 ) then
			new%s = EMPTY_STR
		else
			new%s = trim(adjustl(self%s))
		end if
	end procedure trim_copy

	module procedure trim_inplace
		if ( self%len() < 1 ) then
			self%s = EMPTY_STR
		else
			self%s = trim(adjustl(self%s))
		end if
	end procedure trim_inplace

	module procedure write_file
		type(String), allocatable, dimension(:) :: rows
		character(len=:), allocatable :: ext, row_separator_, column_separator_
		integer :: n_rows, i, file_unit
		logical :: exists

		ext = ext_of(file_name)

		if ( .not. any(TEXT_EXT == ext) ) then
			write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" in method WRITE_FILE'// &
								'due to unsupported file extension "'//ext//'".'// &
							LF//'Supported file extensions: '//glue(TEXT_EXT)
			return
		end if

		if ( .not. present(row_separator) ) then
			row_separator_ = LF
		else
			row_separator_ = row_separator
		end if

		if ( .not. present(column_separator) ) then
			column_separator_ = COMMA
		else
			column_separator_ = column_separator
		end if

		n_rows = size(cell_array, dim=1)

		allocate( rows(n_rows) )

		do concurrent (i = 1:n_rows)
			call rows(i)%glue(tokens=cell_array(i,:), separator=column_separator_)
		end do

		call rows%push(row_separator_)

		call self%glue(tokens=rows, separator=EMPTY_STR)

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write( unit=file_unit ) self%s

		close(file_unit)
	end procedure write_file

	module procedure write_string
		if ( substring%len() < 1 ) then
			write(unit=unit, fmt='(a)', iostat=iostat, iomsg=iomsg) EMPTY_STR
		else
			write(unit=unit, fmt='(a)', iostat=iostat, iomsg=iomsg) substring%s
		end if
	end procedure write_string
end submodule String_procedures

submodule (io_fortran_lib) operators
	!! This submodule provides module procedure implementations for the **public interfaces** `operator(//)`,
	!! `operator(+)`, `operator(-)`, `operator(**)`, `operator(==)`, and `operator(/=)`.
	contains
	module procedure string_concatenation
		if ( Stringl%len() < 1 ) then
			if ( Stringr%len() < 1 ) then
				new%s = EMPTY_STR; return
			else
				new%s = Stringr%s; return
			end if
		end if

		if ( Stringr%len() < 1 ) then
			new%s = Stringl%s; return
		end if

		new%s = Stringl%s//Stringr%s
	end procedure string_concatenation

	module procedure string_char_concatenation
		if ( Stringl%len() < 1 ) then
			if ( len(charsr) < 1 ) then
				new%s = EMPTY_STR; return
			else
				new%s = charsr; return
			end if
		end if

		if ( len(charsr) < 1 ) then
			new%s = Stringl%s; return
		end if

		new%s = Stringl%s//charsr
	end procedure string_char_concatenation

	module procedure char_string_concatenation
		if ( len(charsl) < 1 ) then
			if ( Stringr%len() < 1 ) then
				new%s = EMPTY_STR; return
			else
				new%s = Stringr%s; return
			end if
		end if

		if ( Stringr%len() < 1 ) then
			new%s = charsl; return
		end if

		new%s = charsl//Stringr%s
	end procedure char_string_concatenation

	module procedure char_concat_plus
		new = charsl//charsr
	end procedure char_concat_plus

	module procedure string_concat_plus
		if ( Stringl%len() < 1 ) then
			if ( Stringr%len() < 1 ) then
				new%s = EMPTY_STR; return
			else
				new%s = Stringr%s; return
			end if
		end if

		if ( Stringr%len() < 1 ) then
			new%s = Stringl%s; return
		end if

		new%s = Stringl%s//Stringr%s
	end procedure string_concat_plus

	module procedure string_char_concat_plus
		if ( Stringl%len() < 1 ) then
			if ( len(charsr) < 1 ) then
				new%s = EMPTY_STR; return
			else
				new%s = charsr; return
			end if
		end if

		if ( len(charsr) < 1 ) then
			new%s = Stringl%s; return
		end if

		new%s = Stringl%s//charsr
	end procedure string_char_concat_plus

	module procedure char_string_concat_plus
		if ( len(charsl) < 1 ) then
			if ( Stringr%len() < 1 ) then
				new%s = EMPTY_STR; return
			else
				new%s = Stringr%s; return
			end if
		end if

		if ( Stringr%len() < 1 ) then
			new%s = charsl; return
		end if

		new%s = charsl//Stringr%s
	end procedure char_string_concat_plus

	module procedure char_excision
		type(String) :: Stringl

		Stringl%s = charsl

		if ( Stringl%len() < 1 ) then
			new%s = EMPTY_STR; return
		end if

		if ( len(charsr) < 1 ) then
			new%s = Stringl%s; return
		end if

		new = Stringl%replace(match=charsr, substring=EMPTY_STR)
	end procedure char_excision

	module procedure string_excision
		if ( Stringl%len() < 1 ) then
			new%s = EMPTY_STR; return
		end if

		if ( Stringr%len() < 1 ) then
			new%s = Stringl%s; return
		end if

		new = Stringl%replace(match=Stringr%s, substring=EMPTY_STR)
	end procedure string_excision

	module procedure string_char_excision
		if ( Stringl%len() < 1 ) then
			new%s = EMPTY_STR; return
		end if

		if ( len(charsr) < 1 ) then
			new%s = Stringl%s; return
		end if

		new = Stringl%replace(match=charsr, substring=EMPTY_STR)
	end procedure string_char_excision

	module procedure char_string_excision
		type(String) :: Stringl

		Stringl%s = charsl

		if ( Stringl%len() < 1 ) then
			new%s = EMPTY_STR; return
		end if

		if ( Stringr%len() < 1 ) then
			new%s = Stringl%s; return
		end if

		new = Stringl%replace(match=Stringr%s, substring=EMPTY_STR)
	end procedure char_string_excision

	module procedure repeat_chars
		new = repeat(char_base, ncopies=ncopies)
	end procedure repeat_chars

	module procedure repeat_String
		if ( String_base%len() < 1 ) then
			new%s = EMPTY_STR; return
		end if

		new%s = repeat(String_base%s, ncopies=ncopies)
	end procedure repeat_String

	module procedure string_equivalence
		integer :: Stringl_len, Stringr_len

		Stringl_len = Stringl%len()
		Stringr_len = Stringr%len()

		if ( Stringl_len /= Stringr_len ) then
			equal = .false.; return
		end if

		if ( Stringl_len < 1 ) then
			equal = .true.; return
		end if

		equal = ( Stringl%s == Stringr%s )
	end procedure string_equivalence

	module procedure string_char_equivalence
		integer :: Stringl_len, charsr_len

		Stringl_len = Stringl%len()
		charsr_len = len(charsr)

		if ( Stringl_len /= charsr_len ) then
			equal = .false.; return
		end if

		if ( Stringl_len < 1 ) then
			equal = .true.; return
		end if

		equal = ( Stringl%s == charsr )
	end procedure string_char_equivalence

	module procedure char_string_equivalence
		integer :: charsl_len, Stringr_len

		charsl_len = len(charsl)
		Stringr_len = Stringr%len()

		if ( charsl_len /= Stringr_len ) then
			equal = .false.; return
		end if

		if ( charsl_len < 1 ) then
			equal = .true.; return
		end if

		equal = ( charsl == Stringr%s )
	end procedure char_string_equivalence

	module procedure string_nonequivalence
		integer :: Stringl_len, Stringr_len

		Stringl_len = Stringl%len()
		Stringr_len = Stringr%len()

		if ( Stringl_len /= Stringr_len ) then
			unequal = .true.; return
		end if

		if ( Stringl_len < 1 ) then
			unequal = .false.; return
		end if

		unequal = ( Stringl%s /= Stringr%s )
	end procedure string_nonequivalence

	module procedure string_char_nonequivalence
		integer :: Stringl_len, charsr_len

		Stringl_len = Stringl%len()
		charsr_len = len(charsr)

		if ( Stringl_len /= charsr_len ) then
			unequal = .true.; return
		end if

		if ( Stringl_len < 1 ) then
			unequal = .false.; return
		end if

		unequal = ( Stringl%s /= charsr )
	end procedure string_char_nonequivalence

	module procedure char_string_nonequivalence
		integer :: charsl_len, Stringr_len

		charsl_len = len(charsl)
		Stringr_len = Stringr%len()

		if ( charsl_len /= Stringr_len ) then
			unequal = .true.; return
		end if

		if ( charsl_len < 1 ) then
			unequal = .false.; return
		end if

		unequal = ( charsl /= Stringr%s )
	end procedure char_string_nonequivalence
end submodule operators

submodule (io_fortran_lib) internal_io
	!! This submodule provides module procedure implementations for the **public interfaces** `String`, `str`, and
	!! `cast`.
	contains
	module procedure new_Str_c128
		character(len=:), allocatable :: locale_, fmt_, im_
		integer :: decimals_

		if ( .not. present(locale) ) then
			locale_ = 'US'
		else
			if ( any(LOCALES == locale) ) then
				locale_ = locale
			else
				locale_ = 'US'
			end if
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'e'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				fmt_ = 'e'
			end if
		end if

		if ( .not. present(decimals) ) then
			decimals_ = 150
		else
			decimals_ = decimals
		end if

		if ( .not. present(im) ) then
			im_ = EMPTY_STR
		else
			im_ = trim(adjustl(im))
		end if

		new%s = str(x, locale=locale_, fmt=fmt_, decimals=decimals_, im=im_)
	end procedure new_Str_c128
	module procedure new_Str_c64
		character(len=:), allocatable :: locale_, fmt_, im_
		integer :: decimals_

		if ( .not. present(locale) ) then
			locale_ = 'US'
		else
			if ( any(LOCALES == locale) ) then
				locale_ = locale
			else
				locale_ = 'US'
			end if
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'e'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				fmt_ = 'e'
			end if
		end if

		if ( .not. present(decimals) ) then
			decimals_ = 150
		else
			decimals_ = decimals
		end if

		if ( .not. present(im) ) then
			im_ = EMPTY_STR
		else
			im_ = trim(adjustl(im))
		end if

		new%s = str(x, locale=locale_, fmt=fmt_, decimals=decimals_, im=im_)
	end procedure new_Str_c64
	module procedure new_Str_c32
		character(len=:), allocatable :: locale_, fmt_, im_
		integer :: decimals_

		if ( .not. present(locale) ) then
			locale_ = 'US'
		else
			if ( any(LOCALES == locale) ) then
				locale_ = locale
			else
				locale_ = 'US'
			end if
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'e'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				fmt_ = 'e'
			end if
		end if

		if ( .not. present(decimals) ) then
			decimals_ = 150
		else
			decimals_ = decimals
		end if

		if ( .not. present(im) ) then
			im_ = EMPTY_STR
		else
			im_ = trim(adjustl(im))
		end if

		new%s = str(x, locale=locale_, fmt=fmt_, decimals=decimals_, im=im_)
	end procedure new_Str_c32

	module procedure new_Str_r128
		character(len=:), allocatable :: locale_, fmt_
		integer :: decimals_

		if ( .not. present(locale) ) then
			locale_ = 'US'
		else
			if ( any(LOCALES == locale) ) then
				locale_ = locale
			else
				locale_ = 'US'
			end if
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'e'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				fmt_ = 'e'
			end if
		end if

		if ( .not. present(decimals) ) then
			decimals_ = 150
		else
			decimals_ = decimals
		end if

		new%s = str(x, locale=locale_, fmt=fmt_, decimals=decimals_)
	end procedure new_Str_r128
	module procedure new_Str_r64
		character(len=:), allocatable :: locale_, fmt_
		integer :: decimals_

		if ( .not. present(locale) ) then
			locale_ = 'US'
		else
			if ( any(LOCALES == locale) ) then
				locale_ = locale
			else
				locale_ = 'US'
			end if
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'e'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				fmt_ = 'e'
			end if
		end if

		if ( .not. present(decimals) ) then
			decimals_ = 150
		else
			decimals_ = decimals
		end if

		new%s = str(x, locale=locale_, fmt=fmt_, decimals=decimals_)
	end procedure new_Str_r64
	module procedure new_Str_r32
		character(len=:), allocatable :: locale_, fmt_
		integer :: decimals_

		if ( .not. present(locale) ) then
			locale_ = 'US'
		else
			if ( any(LOCALES == locale) ) then
				locale_ = locale
			else
				locale_ = 'US'
			end if
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'e'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				fmt_ = 'e'
			end if
		end if

		if ( .not. present(decimals) ) then
			decimals_ = 150
		else
			decimals_ = decimals
		end if

		new%s = str(x, locale=locale_, fmt=fmt_, decimals=decimals_)
	end procedure new_Str_r32

	module procedure new_Str_i64
		character(len=:), allocatable :: fmt_

		if ( .not. present(fmt) ) then
			fmt_ = 'i'
		else
			if ( any(INT_FMTS == fmt) ) then
				fmt_ = fmt
			else
				fmt_ = 'i'
			end if
		end if

		new%s = str(x, fmt=fmt_)
	end procedure new_Str_i64
	module procedure new_Str_i32
		character(len=:), allocatable :: fmt_

		if ( .not. present(fmt) ) then
			fmt_ = 'i'
		else
			if ( any(INT_FMTS == fmt) ) then
				fmt_ = fmt
			else
				fmt_ = 'i'
			end if
		end if

		new%s = str(x, fmt=fmt_)
	end procedure new_Str_i32
	module procedure new_Str_i16
		character(len=:), allocatable :: fmt_

		if ( .not. present(fmt) ) then
			fmt_ = 'i'
		else
			if ( any(INT_FMTS == fmt) ) then
				fmt_ = fmt
			else
				fmt_ = 'i'
			end if
		end if

		new%s = str(x, fmt=fmt_)
	end procedure new_Str_i16
	module procedure new_Str_i8
		character(len=:), allocatable :: fmt_

		if ( .not. present(fmt) ) then
			fmt_ = 'i'
		else
			if ( any(INT_FMTS == fmt) ) then
				fmt_ = fmt
			else
				fmt_ = 'i'
			end if
		end if

		new%s = str(x, fmt=fmt_)
	end procedure new_Str_i8

	module procedure new_Str_string
		if ( x%len() < 1 ) then
			new%s = EMPTY_STR
		else
			new%s = x%s
		end if
	end procedure new_Str_string

	module procedure new_Str_char
		new%s = x
	end procedure new_Str_char

	module procedure new_Str_empty
		new%s = EMPTY_STR
	end procedure new_Str_empty

	module procedure cast_string_c128
		character(len=:), allocatable :: locale_, fmt_, im_, substring_

		if ( substring%len() < 1 ) then
			into = (0.0_real128,0.0_real128); return
		end if

		if ( .not. present(locale) ) then
			locale_ = 'US'
		else
			if ( any(LOCALES == locale) ) then
				locale_ = locale
			else
				into = (0.0_real128,0.0_real128); return
			end if
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'e'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				into = (0.0_real128,0.0_real128); return
			end if
		end if

		if ( .not. present(im) ) then
			im_ = EMPTY_STR
		else
			im_ = trim(adjustl(im))
		end if

		substring_ = trim(adjustl(substring%s))
		call cast(substring=substring_, into=into, locale=locale_, fmt=fmt_, im=im_)
	end procedure cast_string_c128
	module procedure cast_string_c64
		character(len=:), allocatable :: locale_, fmt_, im_, substring_

		if ( substring%len() < 1 ) then
			into = (0.0_real64,0.0_real64); return
		end if

		if ( .not. present(locale) ) then
			locale_ = 'US'
		else
			if ( any(LOCALES == locale) ) then
				locale_ = locale
			else
				into = (0.0_real64,0.0_real64); return
			end if
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'e'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				into = (0.0_real64,0.0_real64); return
			end if
		end if

		if ( .not. present(im) ) then
			im_ = EMPTY_STR
		else
			im_ = trim(adjustl(im))
		end if

		substring_ = trim(adjustl(substring%s))
		call cast(substring=substring_, into=into, locale=locale_, fmt=fmt_, im=im_)
	end procedure cast_string_c64
	module procedure cast_string_c32
		character(len=:), allocatable :: locale_, fmt_, im_, substring_

		if ( substring%len() < 1 ) then
			into = (0.0_real32,0.0_real32); return
		end if

		if ( .not. present(locale) ) then
			locale_ = 'US'
		else
			if ( any(LOCALES == locale) ) then
				locale_ = locale
			else
				into = (0.0_real32,0.0_real32); return
			end if
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'e'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				into = (0.0_real32,0.0_real32); return
			end if
		end if

		if ( .not. present(im) ) then
			im_ = EMPTY_STR
		else
			im_ = trim(adjustl(im))
		end if

		substring_ = trim(adjustl(substring%s))
		call cast(substring=substring_, into=into, locale=locale_, fmt=fmt_, im=im_)
	end procedure cast_string_c32

	module procedure cast_string_r128
		character(len=:), allocatable :: locale_, fmt_, substring_

		if ( substring%len() < 1 ) then
			into = 0.0_real128; return
		end if

		if ( .not. present(locale) ) then
			locale_ = 'US'
		else
			if ( any(LOCALES == locale) ) then
				locale_ = locale
			else
				into = 0.0_real128; return
			end if
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'e'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				into = 0.0_real128; return
			end if
		end if

		substring_ = trim(adjustl(substring%s))
		call cast(substring=substring_, into=into, locale=locale_, fmt=fmt_)
	end procedure cast_string_r128
	module procedure cast_string_r64
		character(len=:), allocatable :: locale_, fmt_, substring_

		if ( substring%len() < 1 ) then
			into = 0.0_real64; return
		end if

		if ( .not. present(locale) ) then
			locale_ = 'US'
		else
			if ( any(LOCALES == locale) ) then
				locale_ = locale
			else
				into = 0.0_real64; return
			end if
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'e'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				into = 0.0_real64; return
			end if
		end if

		substring_ = trim(adjustl(substring%s))
		call cast(substring=substring_, into=into, locale=locale_, fmt=fmt_)
	end procedure cast_string_r64
	module procedure cast_string_r32
		character(len=:), allocatable :: locale_, fmt_, substring_

		if ( substring%len() < 1 ) then
			into = 0.0_real32; return
		end if

		if ( .not. present(locale) ) then
			locale_ = 'US'
		else
			if ( any(LOCALES == locale) ) then
				locale_ = locale
			else
				into = 0.0_real32; return
			end if
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'e'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				into = 0.0_real32; return
			end if
		end if

		substring_ = trim(adjustl(substring%s))
		call cast(substring=substring_, into=into, locale=locale_, fmt=fmt_)
	end procedure cast_string_r32

	module procedure cast_string_i64
		character(len=:), allocatable :: fmt_, substring_

		if ( substring%len() < 1 ) then
			into = 0_int64; return
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'i'
		else
			if ( any(INT_FMTS == fmt) ) then
				fmt_ = fmt
			else
				into = 0_int64; return
			end if
		end if

		substring_ = trim(adjustl(substring%s))
		call cast(substring=substring_, into=into, fmt=fmt_)
	end procedure cast_string_i64
	module procedure cast_string_i32
		character(len=:), allocatable :: fmt_, substring_

		if ( substring%len() < 1 ) then
			into = 0_int32; return
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'i'
		else
			if ( any(INT_FMTS == fmt) ) then
				fmt_ = fmt
			else
				into = 0_int32; return
			end if
		end if

		substring_ = trim(adjustl(substring%s))
		call cast(substring=substring_, into=into, fmt=fmt_)
	end procedure cast_string_i32
	module procedure cast_string_i16
		character(len=:), allocatable :: fmt_, substring_

		if ( substring%len() < 1 ) then
			into = 0_int16; return
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'i'
		else
			if ( any(INT_FMTS == fmt) ) then
				fmt_ = fmt
			else
				into = 0_int16; return
			end if
		end if

		substring_ = trim(adjustl(substring%s))
		call cast(substring=substring_, into=into, fmt=fmt_)
	end procedure cast_string_i16
	module procedure cast_string_i8
		character(len=:), allocatable :: fmt_, substring_

		if ( substring%len() < 1 ) then
			into = 0_int8; return
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'i'
		else
			if ( any(INT_FMTS == fmt) ) then
				fmt_ = fmt
			else
				into = 0_int8; return
			end if
		end if

		substring_ = trim(adjustl(substring%s))
		call cast(substring=substring_, into=into, fmt=fmt_)
	end procedure cast_string_i8

	module procedure str_c128
		character(len=:), allocatable :: locale_, fmt_, im_, sep
		integer :: decimals_

		if ( .not. present(locale) ) then
			locale_ = 'US'
		else
			if ( any(LOCALES == locale) ) then
				locale_ = locale
			else
				x_str = EMPTY_STR; return
			end if
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'e'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				x_str = EMPTY_STR; return
			end if
		end if

		if ( .not. present(decimals) ) then
			decimals_ = 150
		else
			decimals_ = decimals
		end if

		if ( .not. present(im) ) then
			im_ = EMPTY_STR
		else
			im_ = trim(adjustl(im))
		end if

		if ( im_ == EMPTY_STR ) then
			if ( locale_ == 'US' ) then
				sep = COMMA
			else
				sep = SEMICOLON
			end if
			x_str = '('//str(x%re, locale=locale_, fmt=fmt_, decimals=decimals_)//sep// &
						 str(x%im, locale=locale_, fmt=fmt_, decimals=decimals_)//')'
		else
			if ( fmt_ == 'z' ) then
				x_str = str(x%re, locale=locale_, fmt=fmt_, decimals=decimals_)//'+'// &
						str(x%im, locale=locale_, fmt=fmt_, decimals=decimals_)//im_
			else
				if ( x%im < 0 ) then
					x_str = str(x%re, locale=locale_, fmt=fmt_, decimals=decimals_)//'-'// &
							str(abs(x%im), locale=locale_, fmt=fmt_, decimals=decimals_)//im_
				else
					x_str = str(x%re, locale=locale_, fmt=fmt_, decimals=decimals_)//'+'// &
							str(x%im, locale=locale_, fmt=fmt_, decimals=decimals_)//im_
				end if
			end if
		end if
	end procedure str_c128
	module procedure str_c64
		character(len=:), allocatable :: locale_, fmt_, im_, sep
		integer :: decimals_

		if ( .not. present(locale) ) then
			locale_ = 'US'
		else
			if ( any(LOCALES == locale) ) then
				locale_ = locale
			else
				x_str = EMPTY_STR; return
			end if
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'e'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				x_str = EMPTY_STR; return
			end if
		end if

		if ( .not. present(decimals) ) then
			decimals_ = 150
		else
			decimals_ = decimals
		end if

		if ( .not. present(im) ) then
			im_ = EMPTY_STR
		else
			im_ = trim(adjustl(im))
		end if

		if ( im_ == EMPTY_STR ) then
			if ( locale_ == 'US' ) then
				sep = COMMA
			else
				sep = SEMICOLON
			end if
			x_str = '('//str(x%re, locale=locale_, fmt=fmt_, decimals=decimals_)//sep// &
						 str(x%im, locale=locale_, fmt=fmt_, decimals=decimals_)//')'
		else
			if ( fmt_ == 'z' ) then
				x_str = str(x%re, locale=locale_, fmt=fmt_, decimals=decimals_)//'+'// &
						str(x%im, locale=locale_, fmt=fmt_, decimals=decimals_)//im_
			else
				if ( x%im < 0 ) then
					x_str = str(x%re, locale=locale_, fmt=fmt_, decimals=decimals_)//'-'// &
							str(abs(x%im), locale=locale_, fmt=fmt_, decimals=decimals_)//im_
				else
					x_str = str(x%re, locale=locale_, fmt=fmt_, decimals=decimals_)//'+'// &
							str(x%im, locale=locale_, fmt=fmt_, decimals=decimals_)//im_
				end if
			end if
		end if
	end procedure str_c64
	module procedure str_c32
		character(len=:), allocatable :: locale_, fmt_, im_, sep
		integer :: decimals_

		if ( .not. present(locale) ) then
			locale_ = 'US'
		else
			if ( any(LOCALES == locale) ) then
				locale_ = locale
			else
				x_str = EMPTY_STR; return
			end if
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'e'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				x_str = EMPTY_STR; return
			end if
		end if

		if ( .not. present(decimals) ) then
			decimals_ = 150
		else
			decimals_ = decimals
		end if

		if ( .not. present(im) ) then
			im_ = EMPTY_STR
		else
			im_ = trim(adjustl(im))
		end if

		if ( im_ == EMPTY_STR ) then
			if ( locale_ == 'US' ) then
				sep = COMMA
			else
				sep = SEMICOLON
			end if
			x_str = '('//str(x%re, locale=locale_, fmt=fmt_, decimals=decimals_)//sep// &
						 str(x%im, locale=locale_, fmt=fmt_, decimals=decimals_)//')'
		else
			if ( fmt_ == 'z' ) then
				x_str = str(x%re, locale=locale_, fmt=fmt_, decimals=decimals_)//'+'// &
						str(x%im, locale=locale_, fmt=fmt_, decimals=decimals_)//im_
			else
				if ( x%im < 0 ) then
					x_str = str(x%re, locale=locale_, fmt=fmt_, decimals=decimals_)//'-'// &
							str(abs(x%im), locale=locale_, fmt=fmt_, decimals=decimals_)//im_
				else
					x_str = str(x%re, locale=locale_, fmt=fmt_, decimals=decimals_)//'+'// &
							str(x%im, locale=locale_, fmt=fmt_, decimals=decimals_)//im_
				end if
			end if
		end if
	end procedure str_c32

	module procedure str_r128
		character(len=:), allocatable :: decimal, fmt_, str_tmp
		integer :: i, e, max_decimals, decimals_, l, extra

		if ( .not. present(locale) ) then
			decimal = 'POINT'
		else
			if ( locale == 'US' ) then
				decimal = 'POINT'
			else if ( locale == 'EU' ) then
				decimal = 'COMMA'
			else
				x_str = EMPTY_STR; return
			end if
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'e'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				x_str = EMPTY_STR; return
			end if
		end if

		if ( fmt_ == 'e' ) then
			associate ( max_precision => ceiling( 1.0 + log10(real(radix(x)))*digits(x) ) )
				if ( .not. present(decimals) ) then
					decimals_ = max_precision - 1
				else
					if ( decimals < 0 ) then
						decimals_ = 0
					else if ( decimals > (max_precision - 1) ) then
						decimals_ = max_precision - 1
					else
						decimals_ = decimals
					end if
				end if
			end associate

			l = decimals_ + 15

			allocate( character(len=l) :: str_tmp )

			write(unit=str_tmp, fmt='(es'//str(l)//'.'//str(decimals_)//'e4)', decimal=decimal) x
			x_str = trim(adjustl(str_tmp))
		else if ( fmt_ == 'f' ) then
			if ( abs(x) /= 0.0_real128 ) then
				e = int(log10(abs(x)))
			else
				e = 0
			end if

			associate ( max_precision => ceiling( 1.0 + log10(real(radix(x)))*digits(x) ) )
				if ( e == 0 ) then
					if ( floor(x) == 0 ) then
						max_decimals = max_precision
					else
						max_decimals = max_precision - 1
						e = 1 + e
					end if
				else if ( e > 0 ) then
					max_decimals = max_precision - (1 + e)
					e = 1 + e
				else
					max_decimals = max_precision - e
				end if

				extra = e - max_precision
			end associate

			if ( max_decimals < 0 ) max_decimals = 0

			if ( .not. present(decimals) ) then
				decimals_ = max_decimals
			else
				if ( decimals < 0 ) then
					decimals_ = 0
				else if ( decimals > max_decimals ) then
					decimals_ = max_decimals
				else
					decimals_ = decimals
				end if
			end if

			if ( e > 0 ) then
				l = decimals_ + e + 10
			else
				l = decimals_ + 10
			end if

			allocate( character(len=l) :: str_tmp )

			write(unit=str_tmp, fmt='(f'//str(l)//'.'//str(decimals_)//')', decimal=decimal) x
			x_str = trim(adjustl(str_tmp))

			if ( extra > 0 ) then
				do i = len(x_str)-1, 1, -1
					x_str(i:i) = '0'
					extra = extra - 1
					if ( extra == 0 ) exit
				end do
			end if
		else if ( fmt_ == 'z' ) then
			allocate( character(len=70) :: str_tmp )

			write(unit=str_tmp, fmt='(z70)') x
			x_str = trim(adjustl(str_tmp))
		end if
	end procedure str_r128
	module procedure str_r64
		character(len=:), allocatable :: decimal, fmt_, str_tmp
		integer :: i, e, max_decimals, decimals_, l, extra

		if ( .not. present(locale) ) then
			decimal = 'POINT'
		else
			if ( locale == 'US' ) then
				decimal = 'POINT'
			else if ( locale == 'EU' ) then
				decimal = 'COMMA'
			else
				x_str = EMPTY_STR; return
			end if
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'e'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				x_str = EMPTY_STR; return
			end if
		end if

		if ( fmt_ == 'e' ) then
			associate ( max_precision => ceiling( 1.0 + log10(real(radix(x)))*digits(x) ) )
				if ( .not. present(decimals) ) then
					decimals_ = max_precision - 1
				else
					if ( decimals < 0 ) then
						decimals_ = 0
					else if ( decimals > (max_precision - 1) ) then
						decimals_ = max_precision - 1
					else
						decimals_ = decimals
					end if
				end if
			end associate

			l = decimals_ + 15

			allocate( character(len=l) :: str_tmp )

			write(unit=str_tmp, fmt='(es'//str(l)//'.'//str(decimals_)//'e3)', decimal=decimal) x
			x_str = trim(adjustl(str_tmp))
		else if ( fmt_ == 'f' ) then
			if ( abs(x) /= 0.0_real64 ) then
				e = int(log10(abs(x)))
			else
				e = 0
			end if

			associate ( max_precision => ceiling( 1.0 + log10(real(radix(x)))*digits(x) ) )
				if ( e == 0 ) then
					if ( floor(x) == 0 ) then
						max_decimals = max_precision
					else
						max_decimals = max_precision - 1
						e = 1 + e
					end if
				else if ( e > 0 ) then
					max_decimals = max_precision - (1 + e)
					e = 1 + e
				else
					max_decimals = max_precision - e
				end if

				extra = e - max_precision
			end associate

			if ( max_decimals < 0 ) max_decimals = 0

			if ( .not. present(decimals) ) then
				decimals_ = max_decimals
			else
				if ( decimals < 0 ) then
					decimals_ = 0
				else if ( decimals > max_decimals ) then
					decimals_ = max_decimals
				else
					decimals_ = decimals
				end if
			end if

			if ( e > 0 ) then
				l = decimals_ + e + 10
			else
				l = decimals_ + 10
			end if

			allocate( character(len=l) :: str_tmp )

			write(unit=str_tmp, fmt='(f'//str(l)//'.'//str(decimals_)//')', decimal=decimal) x
			x_str = trim(adjustl(str_tmp))

			if ( extra > 0 ) then
				do i = len(x_str)-1, 1, -1
					x_str(i:i) = '0'
					extra = extra - 1
					if ( extra == 0 ) exit
				end do
			end if
		else if ( fmt_ == 'z' ) then
			allocate( character(len=40) :: str_tmp )

			write(unit=str_tmp, fmt='(z40)') x
			x_str = trim(adjustl(str_tmp))
		end if
	end procedure str_r64
	module procedure str_r32
		character(len=:), allocatable :: decimal, fmt_, str_tmp
		integer :: i, e, max_decimals, decimals_, l, extra

		if ( .not. present(locale) ) then
			decimal = 'POINT'
		else
			if ( locale == 'US' ) then
				decimal = 'POINT'
			else if ( locale == 'EU' ) then
				decimal = 'COMMA'
			else
				x_str = EMPTY_STR; return
			end if
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'e'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				x_str = EMPTY_STR; return
			end if
		end if

		if ( fmt_ == 'e' ) then
			associate ( max_precision => ceiling( 1.0 + log10(real(radix(x)))*digits(x) ) )
				if ( .not. present(decimals) ) then
					decimals_ = max_precision - 1
				else
					if ( decimals < 0 ) then
						decimals_ = 0
					else if ( decimals > (max_precision - 1) ) then
						decimals_ = max_precision - 1
					else
						decimals_ = decimals
					end if
				end if
			end associate

			l = decimals_ + 15

			allocate( character(len=l) :: str_tmp )

			write(unit=str_tmp, fmt='(es'//str(l)//'.'//str(decimals_)//'e2)', decimal=decimal) x
			x_str = trim(adjustl(str_tmp))
		else if ( fmt_ == 'f' ) then
			if ( abs(x) /= 0.0_real32 ) then
				e = int(log10(abs(x)))
			else
				e = 0
			end if

			associate ( max_precision => ceiling( 1.0 + log10(real(radix(x)))*digits(x) ) )
				if ( e == 0 ) then
					if ( floor(x) == 0 ) then
						max_decimals = max_precision
					else
						max_decimals = max_precision - 1
						e = 1 + e
					end if
				else if ( e > 0 ) then
					max_decimals = max_precision - (1 + e)
					e = 1 + e
				else
					max_decimals = max_precision - e
				end if

				extra = e - max_precision
			end associate

			if ( max_decimals < 0 ) max_decimals = 0

			if ( .not. present(decimals) ) then
				decimals_ = max_decimals
			else
				if ( decimals < 0 ) then
					decimals_ = 0
				else if ( decimals > max_decimals ) then
					decimals_ = max_decimals
				else
					decimals_ = decimals
				end if
			end if

			if ( e > 0 ) then
				l = decimals_ + e + 10
			else
				l = decimals_ + 10
			end if

			allocate( character(len=l) :: str_tmp )

			write(unit=str_tmp, fmt='(f'//str(l)//'.'//str(decimals_)//')', decimal=decimal) x
			x_str = trim(adjustl(str_tmp))

			if ( extra > 0 ) then
				do i = len(x_str)-1, 1, -1
					x_str(i:i) = '0'
					extra = extra - 1
					if ( extra == 0 ) exit
				end do
			end if
		else if ( fmt_ == 'z' ) then
			allocate( character(len=25) :: str_tmp )

			write(unit=str_tmp, fmt='(z25)') x
			x_str = trim(adjustl(str_tmp))
		end if
	end procedure str_r32

	module procedure str_i64
		character(len=:), allocatable :: fmt_
		character(len=40) :: str_tmp

		if ( .not. present(fmt) ) then
			fmt_ = 'i'
		else
			if ( any(INT_FMTS == fmt) ) then
				fmt_ = fmt
			else
				x_str = EMPTY_STR; return
			end if
		end if

		if ( fmt_ == 'i' ) then
			write(unit=str_tmp, fmt='(i40)') x
		else if ( fmt_ == 'z' ) then
			write(unit=str_tmp, fmt='(z40)') x
		end if

		x_str = trim(adjustl(str_tmp))
	end procedure str_i64
	module procedure str_i32
		character(len=:), allocatable :: fmt_
		character(len=25) :: str_tmp

		if ( .not. present(fmt) ) then
			fmt_ = 'i'
		else
			if ( any(INT_FMTS == fmt) ) then
				fmt_ = fmt
			else
				x_str = EMPTY_STR; return
			end if
		end if

		if ( fmt_ == 'i' ) then
			write(unit=str_tmp, fmt='(i25)') x
		else if ( fmt_ == 'z' ) then
			write(unit=str_tmp, fmt='(z25)') x
		end if

		x_str = trim(adjustl(str_tmp))
	end procedure str_i32
	module procedure str_i16
		character(len=:), allocatable :: fmt_
		character(len=15) :: str_tmp

		if ( .not. present(fmt) ) then
			fmt_ = 'i'
		else
			if ( any(INT_FMTS == fmt) ) then
				fmt_ = fmt
			else
				x_str = EMPTY_STR; return
			end if
		end if

		if ( fmt_ == 'i' ) then
			write(unit=str_tmp, fmt='(i15)') x
		else if ( fmt_ == 'z' ) then
			write(unit=str_tmp, fmt='(z15)') x
		end if

		x_str = trim(adjustl(str_tmp))
	end procedure str_i16
	module procedure str_i8
		character(len=:), allocatable :: fmt_
		character(len=10) :: str_tmp

		if ( .not. present(fmt) ) then
			fmt_ = 'i'
		else
			if ( any(INT_FMTS == fmt) ) then
				fmt_ = fmt
			else
				x_str = EMPTY_STR; return
			end if
		end if

		if ( fmt_ == 'i' ) then
			write(unit=str_tmp, fmt='(i10)') x
		else if ( fmt_ == 'z' ) then
			write(unit=str_tmp, fmt='(z10)') x
		end if

		x_str = trim(adjustl(str_tmp))
	end procedure str_i8

	module procedure str_string
		if ( x%len() < 1 ) then
			x_str = EMPTY_STR
		else
			x_str = x%s
		end if
	end procedure str_string

	module procedure str_char
		x_str = x
	end procedure str_char

	module procedure str_empty
		x_str = EMPTY_STR
	end procedure str_empty

	module procedure cast_c128
		character(len=:), allocatable :: locale_, fmt_, im_, substring_, decimal
		character(len=:), allocatable, dimension(:) :: ignore_chars, e_chars
		character(len=1) :: current_char

		real(real128) :: z_re, z_im
		integer :: i

		if ( len(substring) == 0 ) then
			into = (0.0_real128,0.0_real128); return
		end if

		if ( .not. present(locale) ) then
			locale_ = 'US'
		else
			if ( any(LOCALES == locale) ) then
				locale_ = locale
			else
				into = (0.0_real128,0.0_real128); return
			end if
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'e'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				into = (0.0_real128,0.0_real128); return
			end if
		end if

		if ( .not. present(im) ) then
			im_ = EMPTY_STR
		else
			im_ = trim(adjustl(im))
			e_chars = ['e', 'E']
		end if

		if ( fmt_ == 'z' ) then
			ignore_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', &
							'a', 'b', 'c', 'd', 'e', 'f']
		else
			if ( locale_ == 'US' ) then
				decimal = 'POINT'
				ignore_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', 'e', 'E', '+', '-']
			else if ( locale_ == 'EU' ) then
				decimal = 'COMMA'
				ignore_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ',', 'e', 'E', '+', '-']
			end if
		end if

		substring_ = trim(adjustl(substring))

		if ( im_ == EMPTY_STR ) then
			if ( substring_(1:1) /= '(' ) then
				into = (0.0_real128,0.0_real128); return
			else
				substring_ = substring_(2:len(substring_)-1)

				do i = 1, len(substring_)
					if ( .not. any(ignore_chars == substring_(i:i)) ) then
						if ( fmt_ == 'z' ) then
							read(unit=substring_(:i-1), fmt='(z'//str(i-1)//')') z_re
							read(unit=substring_(i+1:), fmt='(z'//str(len(substring_)-i)//')') z_im
						else
							read(unit=substring_(:i-1), fmt=*, decimal=decimal) z_re
							read(unit=substring_(i+1:), fmt=*, decimal=decimal) z_im
						end if

						into = cmplx(z_re, z_im, kind=real128); return
					end if
				end do
			end if
		else
			if ( substring_(len(substring_):len(substring_)) /= im_(len(im_):len(im_)) ) then
				into = (0.0_real128,0.0_real128); return
			else
				substring_ = substring_(:len(substring_)-len(im_))

				do i = 1, len(substring_)
					current_char = substring_(i:i)

					if ( (current_char == '+') .or. (current_char == '-') ) then
						if ( i == 1 ) cycle

						if ( fmt_ == 'z' ) then
							read(unit=substring_(:i-1), fmt='(z'//str(i-1)//')') z_re
							read(unit=substring_(i+1:), fmt='(z'//str(len(substring_)-i)//')') z_im
						else
							if ( any(e_chars == substring_(i-1:i-1)) ) cycle
							read(unit=substring_(:i-1), fmt=*, decimal=decimal) z_re
							read(unit=substring_(i:), fmt=*, decimal=decimal) z_im
						end if

						into = cmplx(z_re, z_im, kind=real128); return
					end if
				end do
			end if
		end if
	end procedure cast_c128
	module procedure cast_c64
		character(len=:), allocatable :: locale_, fmt_, im_, substring_, decimal
		character(len=:), allocatable, dimension(:) :: ignore_chars, e_chars
		character(len=1) :: current_char

		real(real64) :: z_re, z_im
		integer :: i

		if ( len(substring) == 0 ) then
			into = (0.0_real64,0.0_real64); return
		end if

		if ( .not. present(locale) ) then
			locale_ = 'US'
		else
			if ( any(LOCALES == locale) ) then
				locale_ = locale
			else
				into = (0.0_real64,0.0_real64); return
			end if
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'e'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				into = (0.0_real64,0.0_real64); return
			end if
		end if

		if ( .not. present(im) ) then
			im_ = EMPTY_STR
		else
			im_ = trim(adjustl(im))
			e_chars = ['e', 'E']
		end if

		if ( fmt_ == 'z' ) then
			ignore_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', &
							'a', 'b', 'c', 'd', 'e', 'f']
		else
			if ( locale_ == 'US' ) then
				decimal = 'POINT'
				ignore_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', 'e', 'E', '+', '-']
			else if ( locale_ == 'EU' ) then
				decimal = 'COMMA'
				ignore_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ',', 'e', 'E', '+', '-']
			end if
		end if

		substring_ = trim(adjustl(substring))

		if ( im_ == EMPTY_STR ) then
			if ( substring_(1:1) /= '(' ) then
				into = (0.0_real64,0.0_real64); return
			else
				substring_ = substring_(2:len(substring_)-1)

				do i = 1, len(substring_)
					if ( .not. any(ignore_chars == substring_(i:i)) ) then
						if ( fmt_ == 'z' ) then
							read(unit=substring_(:i-1), fmt='(z'//str(i-1)//')') z_re
							read(unit=substring_(i+1:), fmt='(z'//str(len(substring_)-i)//')') z_im
						else
							read(unit=substring_(:i-1), fmt=*, decimal=decimal) z_re
							read(unit=substring_(i+1:), fmt=*, decimal=decimal) z_im
						end if

						into = cmplx(z_re, z_im, kind=real64); return
					end if
				end do
			end if
		else
			if ( substring_(len(substring_):len(substring_)) /= im_(len(im_):len(im_)) ) then
				into = (0.0_real64,0.0_real64); return
			else
				substring_ = substring_(:len(substring_)-len(im_))

				do i = 1, len(substring_)
					current_char = substring_(i:i)

					if ( (current_char == '+') .or. (current_char == '-') ) then
						if ( i == 1 ) cycle

						if ( fmt_ == 'z' ) then
							read(unit=substring_(:i-1), fmt='(z'//str(i-1)//')') z_re
							read(unit=substring_(i+1:), fmt='(z'//str(len(substring_)-i)//')') z_im
						else
							if ( any(e_chars == substring_(i-1:i-1)) ) cycle
							read(unit=substring_(:i-1), fmt=*, decimal=decimal) z_re
							read(unit=substring_(i:), fmt=*, decimal=decimal) z_im
						end if

						into = cmplx(z_re, z_im, kind=real64); return
					end if
				end do
			end if
		end if
	end procedure cast_c64
	module procedure cast_c32
		character(len=:), allocatable :: locale_, fmt_, im_, substring_, decimal
		character(len=:), allocatable, dimension(:) :: ignore_chars, e_chars
		character(len=1) :: current_char

		real(real32) :: z_re, z_im
		integer :: i

		if ( len(substring) == 0 ) then
			into = (0.0_real32,0.0_real32); return
		end if

		if ( .not. present(locale) ) then
			locale_ = 'US'
		else
			if ( any(LOCALES == locale) ) then
				locale_ = locale
			else
				into = (0.0_real32,0.0_real32); return
			end if
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'e'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				into = (0.0_real32,0.0_real32); return
			end if
		end if

		if ( .not. present(im) ) then
			im_ = EMPTY_STR
		else
			im_ = trim(adjustl(im))
			e_chars = ['e', 'E']
		end if

		if ( fmt_ == 'z' ) then
			ignore_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', &
							'a', 'b', 'c', 'd', 'e', 'f']
		else
			if ( locale_ == 'US' ) then
				decimal = 'POINT'
				ignore_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', 'e', 'E', '+', '-']
			else if ( locale_ == 'EU' ) then
				decimal = 'COMMA'
				ignore_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ',', 'e', 'E', '+', '-']
			end if
		end if

		substring_ = trim(adjustl(substring))

		if ( im_ == EMPTY_STR ) then
			if ( substring_(1:1) /= '(' ) then
				into = (0.0_real32,0.0_real32); return
			else
				substring_ = substring_(2:len(substring_)-1)

				do i = 1, len(substring_)
					if ( .not. any(ignore_chars == substring_(i:i)) ) then
						if ( fmt_ == 'z' ) then
							read(unit=substring_(:i-1), fmt='(z'//str(i-1)//')') z_re
							read(unit=substring_(i+1:), fmt='(z'//str(len(substring_)-i)//')') z_im
						else
							read(unit=substring_(:i-1), fmt=*, decimal=decimal) z_re
							read(unit=substring_(i+1:), fmt=*, decimal=decimal) z_im
						end if

						into = cmplx(z_re, z_im, kind=real32); return
					end if
				end do
			end if
		else
			if ( substring_(len(substring_):len(substring_)) /= im_(len(im_):len(im_)) ) then
				into = (0.0_real32,0.0_real32); return
			else
				substring_ = substring_(:len(substring_)-len(im_))

				do i = 1, len(substring_)
					current_char = substring_(i:i)

					if ( (current_char == '+') .or. (current_char == '-') ) then
						if ( i == 1 ) cycle

						if ( fmt_ == 'z' ) then
							read(unit=substring_(:i-1), fmt='(z'//str(i-1)//')') z_re
							read(unit=substring_(i+1:), fmt='(z'//str(len(substring_)-i)//')') z_im
						else
							if ( any(e_chars == substring_(i-1:i-1)) ) cycle
							read(unit=substring_(:i-1), fmt=*, decimal=decimal) z_re
							read(unit=substring_(i:), fmt=*, decimal=decimal) z_im
						end if

						into = cmplx(z_re, z_im, kind=real32); return
					end if
				end do
			end if
		end if
	end procedure cast_c32

	module procedure cast_r128
		character(len=:), allocatable :: locale_, fmt_, substring_, decimal

		if ( len(substring) == 0 ) then
			into = 0.0_real128; return
		end if

		if ( .not. present(locale) ) then
			locale_ = 'US'
		else
			if ( any(LOCALES == locale) ) then
				locale_ = locale
			else
				into = 0.0_real128; return
			end if
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'e'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				into = 0.0_real128; return
			end if
		end if

		substring_ = trim(adjustl(substring))

		if ( fmt_ == 'z' ) then
			read(unit=substring_, fmt='(z'//str(len(substring_))//')') into
		else
			if ( locale_ == 'US' ) then
				decimal = 'POINT'
			else if ( locale_ == 'EU' ) then
				decimal = 'COMMA'
			end if

			read(unit=substring_, fmt=*, decimal=decimal) into
		end if
	end procedure cast_r128
	module procedure cast_r64
		character(len=:), allocatable :: locale_, fmt_, substring_, decimal

		if ( len(substring) == 0 ) then
			into = 0.0_real64; return
		end if

		if ( .not. present(locale) ) then
			locale_ = 'US'
		else
			if ( any(LOCALES == locale) ) then
				locale_ = locale
			else
				into = 0.0_real64; return
			end if
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'e'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				into = 0.0_real64; return
			end if
		end if

		substring_ = trim(adjustl(substring))

		if ( fmt_ == 'z' ) then
			read(unit=substring_, fmt='(z'//str(len(substring_))//')') into
		else
			if ( locale_ == 'US' ) then
				decimal = 'POINT'
			else if ( locale_ == 'EU' ) then
				decimal = 'COMMA'
			end if

			read(unit=substring_, fmt=*, decimal=decimal) into
		end if
	end procedure cast_r64
	module procedure cast_r32
		character(len=:), allocatable :: locale_, fmt_, substring_, decimal

		if ( len(substring) == 0 ) then
			into = 0.0_real32; return
		end if

		if ( .not. present(locale) ) then
			locale_ = 'US'
		else
			if ( any(LOCALES == locale) ) then
				locale_ = locale
			else
				into = 0.0_real32; return
			end if
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'e'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				into = 0.0_real32; return
			end if
		end if

		substring_ = trim(adjustl(substring))

		if ( fmt_ == 'z' ) then
			read(unit=substring_, fmt='(z'//str(len(substring_))//')') into
		else
			if ( locale_ == 'US' ) then
				decimal = 'POINT'
			else if ( locale_ == 'EU' ) then
				decimal = 'COMMA'
			end if

			read(unit=substring_, fmt=*, decimal=decimal) into
		end if
	end procedure cast_r32

	module procedure cast_i64
		character(len=:), allocatable :: fmt_, substring_

		if ( len(substring) == 0 ) then
			into = 0_int64; return
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'i'
		else
			if ( any(INT_FMTS == fmt) ) then
				fmt_ = fmt
			else
				into = 0_int64; return
			end if
		end if

		substring_ = trim(adjustl(substring))

		if ( fmt_ == 'z' ) then
			read(unit=substring_, fmt='(z'//str(len(substring_))//')') into
		else
			read(unit=substring_, fmt=*) into
		end if
	end procedure cast_i64
	module procedure cast_i32
		character(len=:), allocatable :: fmt_, substring_

		if ( len(substring) == 0 ) then
			into = 0_int32; return
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'i'
		else
			if ( any(INT_FMTS == fmt) ) then
				fmt_ = fmt
			else
				into = 0_int32; return
			end if
		end if

		substring_ = trim(adjustl(substring))

		if ( fmt_ == 'z' ) then
			read(unit=substring_, fmt='(z'//str(len(substring_))//')') into
		else
			read(unit=substring_, fmt=*) into
		end if
	end procedure cast_i32
	module procedure cast_i16
		character(len=:), allocatable :: fmt_, substring_

		if ( len(substring) == 0 ) then
			into = 0_int16; return
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'i'
		else
			if ( any(INT_FMTS == fmt) ) then
				fmt_ = fmt
			else
				into = 0_int16; return
			end if
		end if

		substring_ = trim(adjustl(substring))

		if ( fmt_ == 'z' ) then
			read(unit=substring_, fmt='(z'//str(len(substring_))//')') into
		else
			read(unit=substring_, fmt=*) into
		end if
	end procedure cast_i16
	module procedure cast_i8
		character(len=:), allocatable :: fmt_, substring_

		if ( len(substring) == 0 ) then
			into = 0_int8; return
		end if

		if ( .not. present(fmt) ) then
			fmt_ = 'i'
		else
			if ( any(INT_FMTS == fmt) ) then
				fmt_ = fmt
			else
				into = 0_int8; return
			end if
		end if

		substring_ = trim(adjustl(substring))

		if ( fmt_ == 'z' ) then
			read(unit=substring_, fmt='(z'//str(len(substring_))//')') into
		else
			read(unit=substring_, fmt=*) into
		end if
	end procedure cast_i8
end submodule internal_io

submodule (io_fortran_lib) glue_split
	!! This submodule provides module procedure implementations for the **public interfaces** `glue` and `split`.
	contains
	module procedure glue_char
		character(len=:), allocatable :: separator_
		integer :: length, i

		if ( .not. present(separator) ) then
			separator_ = SPACE
		else
			separator_ = separator
		end if

		length = len(tokens)

		if ( length == 0 ) then
			new = EMPTY_STR; return
		end if

		new = EMPTY_STR
		do i = 1, size(tokens)-1
			new = new//trim(adjustl(tokens(i)))//separator_
		end do
		new = new//trim(adjustl(tokens(size(tokens))))
	end procedure glue_char

	module procedure glue_string
		character(len=:), allocatable :: separator_
		integer, allocatable, dimension(:) :: lengths
		integer :: i

		if ( .not. present(separator) ) then
			separator_ = SPACE
		else
			separator_ = separator
		end if

		lengths = tokens%len()

		new%s = EMPTY_STR
		do i = 1, size(tokens)-1
			if ( lengths(i) < 1 ) then
				new%s = new%s//separator_
			else
				new%s = new%s//tokens(i)%s//separator_
			end if
		end do

		if ( lengths(size(tokens)) < 1 ) then
			return
		else
			new%s = new%s//tokens(size(tokens))%s
		end if
	end procedure glue_string

	module procedure split_char
		character(len=:), allocatable :: separator_

		if ( .not. present(separator) ) then
			separator_ = SPACE
		else
			separator_ = separator
		end if

		tokens = split(String(substring), separator=separator_)
	end procedure split_char

	module procedure split_string
		type(String) :: temp_String
		character(len=:), allocatable :: separator_
		integer :: i, temp_len, sep_len, num_seps, l, current_sep

		temp_String = substring%trim()
		temp_len = temp_String%len()

		if ( temp_len < 1 ) then
			tokens = [ String(EMPTY_STR) ]; return
		end if

		if ( .not. present(separator) ) then
			separator_ = SPACE
		else
			separator_ = separator
		end if

		sep_len = len(separator_)

		if ( sep_len == 0 ) then
			allocate( tokens(temp_len) )
			do concurrent (i = 1:temp_len)
				tokens(i)%s = temp_String%s(i:i)
			end do
			return
		end if

		num_seps = 0
		i = 1

		count_seps: do
			if ( temp_String%s(i:i) == separator_(1:1) ) then
				if ( i+sep_len-1 > temp_len ) exit count_seps

				if ( temp_String%s(i:i+sep_len-1) == separator_ ) then
					num_seps = num_seps + 1
					i = i + sep_len
				else
					i = i + 1
				end if
			else
				i = i + 1
			end if

			if ( i > temp_len ) exit count_seps
		end do count_seps

		if ( num_seps == 0 ) then
			tokens = [ substring ]; return
		end if

		allocate( tokens(num_seps + 1) )
		call tokens%empty()

		i = 1
		l = 1
		current_sep = 1

		splitting: do while ( i <= temp_len-sep_len+1 )
			if ( temp_String%s(i:i+sep_len-1) == separator_ ) then
				tokens(current_sep)%s = temp_String%s(l:i-1)

				if ( current_sep == num_seps ) then
					if ( i+sep_len > temp_len ) then
						tokens(num_seps+1)%s = EMPTY_STR; exit splitting
					else
						tokens(num_seps+1)%s = temp_String%s(i+sep_len:); exit splitting
					end if
				end if

				current_sep = current_sep + 1; i = i + sep_len; l = i; cycle splitting
			else
				i = i + 1; cycle splitting
			end if
		end do splitting

		call tokens%trim_inplace()
	end procedure split_string
end submodule glue_split

submodule (io_fortran_lib) file_io
	!! This submodule provides module procedure implementations for the **public interfaces** `to_file` and
	!! `from_file`.
	contains
	! Writing Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	module procedure to_file_1dc128
		character(len=:), allocatable, dimension(:) :: header_
		character(len=:), allocatable :: ext, locale_, delim_, fmt_, im_
		integer :: decimals_, hstat, dim_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = [ EMPTY_STR ]
				hstat = 0
			else
				if ( (size(header) /= 1) .and. (size(header) /= size(x)) ) then
					header_ = [ EMPTY_STR ]
					hstat = -1
					write(*,'(a)') LF//'WARNING: Invalid header for file "'//file_name//'".'// &
								   LF//'Header for this data must have size (1) or '// &
									   '('//str(size(x))//').'
				else
					header_ = header
					if ( size(header) == 1 ) then
						hstat = 1
					else
						hstat = 2
					end if
				end if
			end if

			if ( .not. present(dim) ) then
				if ( hstat == 2 ) then
					dim_ = 2
				else
					dim_ = 1
				end if
			else
				if ( hstat == 2 ) then
					dim_ = 2
					if ( dim /= 2 ) then
						write(*,'(a)') LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
									   file_name//'" for given header... defaulting to (2).'
					end if
				else
					if ( dim == 1 ) then
						dim_ = 1
					else if ( dim == 2 ) then
						dim_ = 2
					else
						dim_ = 1
						write(*,'(a)') LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
									   file_name//'" for given header... defaulting to (1).'
					end if
				end if
			end if

			if ( .not. present(locale) ) then
				locale_ = 'US'
			else
				if ( any(LOCALES == locale) ) then
					locale_ = locale
				else
					locale_ = 'US'
					write(*,'(a)') LF//'WARNING: Invalid locale "'//locale//'" for file "'//file_name//'". '// &
									   'Defaulting to US format.'// &
								   LF//'Locale must be one of: '//glue(LOCALES)
				end if
			end if

			if ( .not. present(delim) ) then
				if ( dim_ == 1 ) then
					delim_ = EMPTY_STR
				else
					if ( locale_ == 'US' ) then
						delim_ = COMMA
					else
						delim_ = SEMICOLON
					end if
				end if
			else
				if ( dim_ == 1 ) then
					delim_ = EMPTY_STR
				else
					delim_ = delim
				end if
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'e'
			else
				if ( any(REAL_FMTS == fmt) ) then
					fmt_ = fmt
				else
					fmt_ = 'e'
					write(*,'(a)') LF//'WARNING: Invalid format "'//fmt//'" for file "'//file_name//'". '// &
									   'Defaulting to exponential format.'// &
								   LF//'Format must be one of: '//glue(REAL_FMTS)
				end if
			end if

			if ( .not. present(decimals) ) then
				decimals_ = 150
			else
				decimals_ = decimals
			end if

			if ( .not. present(im) ) then
				im_ = EMPTY_STR
			else
				im_ = trim(adjustl(im))
			end if

			call to_text( x=x, file_name=file_name, header=header_, dim=dim_, locale=locale_, delim=delim_, &
						  fmt=fmt_, decimals=decimals_, im=im_ )
		else if ( any(BINARY_EXT == ext) ) then
			if ( present(header) )   write(*,'(a)') LF//'WARNING: header not supported for file type "'//ext//'".'
			if ( present(dim) )      write(*,'(a)') LF//'WARNING: dim not supported for file type "'//ext//'".'
			if ( present(locale) )   write(*,'(a)') LF//'WARNING: locale not supported for file type "'//ext//'".'
			if ( present(delim) )    write(*,'(a)') LF//'WARNING: delim not supported for file type "'//ext//'".'
			if ( present(fmt) )      write(*,'(a)') LF//'WARNING: fmt not supported for file type "'//ext//'".'
			if ( present(decimals) ) write(*,'(a)') LF//'WARNING: decimals not supported for file type "'//ext//'".'
			if ( present(im) )       write(*,'(a)') LF//'WARNING: im not supported for file type "'//ext//'".'

			call to_binary(x=x, file_name=file_name)
		else
			write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
								'due to unsupported file extension "'//ext//'".'// &
							LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
								glue(BINARY_EXT)
		end if
	end procedure to_file_1dc128
	module procedure to_file_1dc64
		character(len=:), allocatable, dimension(:) :: header_
		character(len=:), allocatable :: ext, locale_, delim_, fmt_, im_
		integer :: decimals_, hstat, dim_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = [ EMPTY_STR ]
				hstat = 0
			else
				if ( (size(header) /= 1) .and. (size(header) /= size(x)) ) then
					header_ = [ EMPTY_STR ]
					hstat = -1
					write(*,'(a)') LF//'WARNING: Invalid header for file "'//file_name//'".'// &
								   LF//'Header for this data must have size (1) or '// &
									   '('//str(size(x))//').'
				else
					header_ = header
					if ( size(header) == 1 ) then
						hstat = 1
					else
						hstat = 2
					end if
				end if
			end if

			if ( .not. present(dim) ) then
				if ( hstat == 2 ) then
					dim_ = 2
				else
					dim_ = 1
				end if
			else
				if ( hstat == 2 ) then
					dim_ = 2
					if ( dim /= 2 ) then
						write(*,'(a)') LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
									   file_name//'" for given header... defaulting to (2).'
					end if
				else
					if ( dim == 1 ) then
						dim_ = 1
					else if ( dim == 2 ) then
						dim_ = 2
					else
						dim_ = 1
						write(*,'(a)') LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
									   file_name//'" for given header... defaulting to (1).'
					end if
				end if
			end if

			if ( .not. present(locale) ) then
				locale_ = 'US'
			else
				if ( any(LOCALES == locale) ) then
					locale_ = locale
				else
					locale_ = 'US'
					write(*,'(a)') LF//'WARNING: Invalid locale "'//locale//'" for file "'//file_name//'". '// &
									   'Defaulting to US format.'// &
								   LF//'Locale must be one of: '//glue(LOCALES)
				end if
			end if

			if ( .not. present(delim) ) then
				if ( dim_ == 1 ) then
					delim_ = EMPTY_STR
				else
					if ( locale_ == 'US' ) then
						delim_ = COMMA
					else
						delim_ = SEMICOLON
					end if
				end if
			else
				if ( dim_ == 1 ) then
					delim_ = EMPTY_STR
				else
					delim_ = delim
				end if
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'e'
			else
				if ( any(REAL_FMTS == fmt) ) then
					fmt_ = fmt
				else
					fmt_ = 'e'
					write(*,'(a)') LF//'WARNING: Invalid format "'//fmt//'" for file "'//file_name//'". '// &
									   'Defaulting to exponential format.'// &
								   LF//'Format must be one of: '//glue(REAL_FMTS)
				end if
			end if

			if ( .not. present(decimals) ) then
				decimals_ = 150
			else
				decimals_ = decimals
			end if

			if ( .not. present(im) ) then
				im_ = EMPTY_STR
			else
				im_ = trim(adjustl(im))
			end if

			call to_text( x=x, file_name=file_name, header=header_, dim=dim_, locale=locale_, delim=delim_, &
						  fmt=fmt_, decimals=decimals_, im=im_ )
		else if ( any(BINARY_EXT == ext) ) then
			if ( present(header) )   write(*,'(a)') LF//'WARNING: header not supported for file type "'//ext//'".'
			if ( present(dim) )      write(*,'(a)') LF//'WARNING: dim not supported for file type "'//ext//'".'
			if ( present(locale) )   write(*,'(a)') LF//'WARNING: locale not supported for file type "'//ext//'".'
			if ( present(delim) )    write(*,'(a)') LF//'WARNING: delim not supported for file type "'//ext//'".'
			if ( present(fmt) )      write(*,'(a)') LF//'WARNING: fmt not supported for file type "'//ext//'".'
			if ( present(decimals) ) write(*,'(a)') LF//'WARNING: decimals not supported for file type "'//ext//'".'
			if ( present(im) )       write(*,'(a)') LF//'WARNING: im not supported for file type "'//ext//'".'

			call to_binary(x=x, file_name=file_name)
		else
			write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
								'due to unsupported file extension "'//ext//'".'// &
							LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
								glue(BINARY_EXT)
		end if
	end procedure to_file_1dc64
	module procedure to_file_1dc32
		character(len=:), allocatable, dimension(:) :: header_
		character(len=:), allocatable :: ext, locale_, delim_, fmt_, im_
		integer :: decimals_, hstat, dim_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = [ EMPTY_STR ]
				hstat = 0
			else
				if ( (size(header) /= 1) .and. (size(header) /= size(x)) ) then
					header_ = [ EMPTY_STR ]
					hstat = -1
					write(*,'(a)') LF//'WARNING: Invalid header for file "'//file_name//'".'// &
								   LF//'Header for this data must have size (1) or '// &
									   '('//str(size(x))//').'
				else
					header_ = header
					if ( size(header) == 1 ) then
						hstat = 1
					else
						hstat = 2
					end if
				end if
			end if

			if ( .not. present(dim) ) then
				if ( hstat == 2 ) then
					dim_ = 2
				else
					dim_ = 1
				end if
			else
				if ( hstat == 2 ) then
					dim_ = 2
					if ( dim /= 2 ) then
						write(*,'(a)') LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
									   file_name//'" for given header... defaulting to (2).'
					end if
				else
					if ( dim == 1 ) then
						dim_ = 1
					else if ( dim == 2 ) then
						dim_ = 2
					else
						dim_ = 1
						write(*,'(a)') LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
									   file_name//'" for given header... defaulting to (1).'
					end if
				end if
			end if

			if ( .not. present(locale) ) then
				locale_ = 'US'
			else
				if ( any(LOCALES == locale) ) then
					locale_ = locale
				else
					locale_ = 'US'
					write(*,'(a)') LF//'WARNING: Invalid locale "'//locale//'" for file "'//file_name//'". '// &
									   'Defaulting to US format.'// &
								   LF//'Locale must be one of: '//glue(LOCALES)
				end if
			end if

			if ( .not. present(delim) ) then
				if ( dim_ == 1 ) then
					delim_ = EMPTY_STR
				else
					if ( locale_ == 'US' ) then
						delim_ = COMMA
					else
						delim_ = SEMICOLON
					end if
				end if
			else
				if ( dim_ == 1 ) then
					delim_ = EMPTY_STR
				else
					delim_ = delim
				end if
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'e'
			else
				if ( any(REAL_FMTS == fmt) ) then
					fmt_ = fmt
				else
					fmt_ = 'e'
					write(*,'(a)') LF//'WARNING: Invalid format "'//fmt//'" for file "'//file_name//'". '// &
									   'Defaulting to exponential format.'// &
								   LF//'Format must be one of: '//glue(REAL_FMTS)
				end if
			end if

			if ( .not. present(decimals) ) then
				decimals_ = 150
			else
				decimals_ = decimals
			end if

			if ( .not. present(im) ) then
				im_ = EMPTY_STR
			else
				im_ = trim(adjustl(im))
			end if

			call to_text( x=x, file_name=file_name, header=header_, dim=dim_, locale=locale_, delim=delim_, &
						  fmt=fmt_, decimals=decimals_, im=im_ )
		else if ( any(BINARY_EXT == ext) ) then
			if ( present(header) )   write(*,'(a)') LF//'WARNING: header not supported for file type "'//ext//'".'
			if ( present(dim) )      write(*,'(a)') LF//'WARNING: dim not supported for file type "'//ext//'".'
			if ( present(locale) )   write(*,'(a)') LF//'WARNING: locale not supported for file type "'//ext//'".'
			if ( present(delim) )    write(*,'(a)') LF//'WARNING: delim not supported for file type "'//ext//'".'
			if ( present(fmt) )      write(*,'(a)') LF//'WARNING: fmt not supported for file type "'//ext//'".'
			if ( present(decimals) ) write(*,'(a)') LF//'WARNING: decimals not supported for file type "'//ext//'".'
			if ( present(im) )       write(*,'(a)') LF//'WARNING: im not supported for file type "'//ext//'".'

			call to_binary(x=x, file_name=file_name)
		else
			write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
								'due to unsupported file extension "'//ext//'".'// &
							LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
								glue(BINARY_EXT)
		end if
	end procedure to_file_1dc32

	module procedure to_file_2dc128
		character(len=:), allocatable, dimension(:) :: header_
		character(len=:), allocatable :: ext, locale_, delim_, fmt_, im_
		integer :: decimals_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = [ EMPTY_STR ]
			else
				if ( (size(header) /= 1) .and. (size(header) /= size(x, dim=2)) ) then
					header_ = [ EMPTY_STR ]
					write(*,'(a)') LF//'WARNING: Invalid header for file "'//file_name//'".'// &
								   LF//'Header for this data must have size (1) or '// &
									   '('//str(size(x, dim=2))//').'
				else
					header_ = header
				end if
			end if

			if ( .not. present(locale) ) then
				locale_ = 'US'
			else
				if ( any(LOCALES == locale) ) then
					locale_ = locale
				else
					locale_ = 'US'
					write(*,'(a)') LF//'WARNING: Invalid locale "'//locale//'" for file "'//file_name//'". '// &
									   'Defaulting to US format.'// &
								   LF//'Locale must be one of: '//glue(LOCALES)
				end if
			end if

			if ( .not. present(delim) ) then
				if ( locale_ == 'US' ) then
					delim_ = COMMA
				else
					delim_ = SEMICOLON
				end if
			else
				delim_ = delim
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'e'
			else
				if ( any(REAL_FMTS == fmt) ) then
					fmt_ = fmt
				else
					fmt_ = 'e'
					write(*,'(a)') LF//'WARNING: Invalid format "'//fmt//'" for file "'//file_name//'". '// &
									   'Defaulting to exponential format.'// &
								   LF//'Format must be one of: '//glue(REAL_FMTS)
				end if
			end if

			if ( .not. present(decimals) ) then
				decimals_ = 150
			else
				decimals_ = decimals
			end if

			if ( .not. present(im) ) then
				im_ = EMPTY_STR
			else
				im_ = trim(adjustl(im))
			end if

			call to_text( x=x, file_name=file_name, header=header_, locale=locale_, delim=delim_, &
						  fmt=fmt_, decimals=decimals_, im=im_ )
		else if ( any(BINARY_EXT == ext) ) then
			if ( present(header) )   write(*,'(a)') LF//'WARNING: header not supported for file type "'//ext//'".'
			if ( present(locale) )   write(*,'(a)') LF//'WARNING: locale not supported for file type "'//ext//'".'
			if ( present(delim) )    write(*,'(a)') LF//'WARNING: delim not supported for file type "'//ext//'".'
			if ( present(fmt) )      write(*,'(a)') LF//'WARNING: fmt not supported for file type "'//ext//'".'
			if ( present(decimals) ) write(*,'(a)') LF//'WARNING: decimals not supported for file type "'//ext//'".'
			if ( present(im) )       write(*,'(a)') LF//'WARNING: im not supported for file type "'//ext//'".'

			call to_binary(x=x, file_name=file_name)
		else
			write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
								'due to unsupported file extension "'//ext//'".'// &
							LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
								glue(BINARY_EXT)
		end if
	end procedure to_file_2dc128
	module procedure to_file_2dc64
		character(len=:), allocatable, dimension(:) :: header_
		character(len=:), allocatable :: ext, locale_, delim_, fmt_, im_
		integer :: decimals_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = [ EMPTY_STR ]
			else
				if ( (size(header) /= 1) .and. (size(header) /= size(x, dim=2)) ) then
					header_ = [ EMPTY_STR ]
					write(*,'(a)') LF//'WARNING: Invalid header for file "'//file_name//'".'// &
								   LF//'Header for this data must have size (1) or '// &
									   '('//str(size(x, dim=2))//').'
				else
					header_ = header
				end if
			end if

			if ( .not. present(locale) ) then
				locale_ = 'US'
			else
				if ( any(LOCALES == locale) ) then
					locale_ = locale
				else
					locale_ = 'US'
					write(*,'(a)') LF//'WARNING: Invalid locale "'//locale//'" for file "'//file_name//'". '// &
									   'Defaulting to US format.'// &
								   LF//'Locale must be one of: '//glue(LOCALES)
				end if
			end if

			if ( .not. present(delim) ) then
				if ( locale_ == 'US' ) then
					delim_ = COMMA
				else
					delim_ = SEMICOLON
				end if
			else
				delim_ = delim
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'e'
			else
				if ( any(REAL_FMTS == fmt) ) then
					fmt_ = fmt
				else
					fmt_ = 'e'
					write(*,'(a)') LF//'WARNING: Invalid format "'//fmt//'" for file "'//file_name//'". '// &
									   'Defaulting to exponential format.'// &
								   LF//'Format must be one of: '//glue(REAL_FMTS)
				end if
			end if

			if ( .not. present(decimals) ) then
				decimals_ = 150
			else
				decimals_ = decimals
			end if

			if ( .not. present(im) ) then
				im_ = EMPTY_STR
			else
				im_ = trim(adjustl(im))
			end if

			call to_text( x=x, file_name=file_name, header=header_, locale=locale_, delim=delim_, &
						  fmt=fmt_, decimals=decimals_, im=im_ )
		else if ( any(BINARY_EXT == ext) ) then
			if ( present(header) )   write(*,'(a)') LF//'WARNING: header not supported for file type "'//ext//'".'
			if ( present(locale) )   write(*,'(a)') LF//'WARNING: locale not supported for file type "'//ext//'".'
			if ( present(delim) )    write(*,'(a)') LF//'WARNING: delim not supported for file type "'//ext//'".'
			if ( present(fmt) )      write(*,'(a)') LF//'WARNING: fmt not supported for file type "'//ext//'".'
			if ( present(decimals) ) write(*,'(a)') LF//'WARNING: decimals not supported for file type "'//ext//'".'
			if ( present(im) )       write(*,'(a)') LF//'WARNING: im not supported for file type "'//ext//'".'

			call to_binary(x=x, file_name=file_name)
		else
			write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
								'due to unsupported file extension "'//ext//'".'// &
							LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
								glue(BINARY_EXT)
		end if
	end procedure to_file_2dc64
	module procedure to_file_2dc32
		character(len=:), allocatable, dimension(:) :: header_
		character(len=:), allocatable :: ext, locale_, delim_, fmt_, im_
		integer :: decimals_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = [ EMPTY_STR ]
			else
				if ( (size(header) /= 1) .and. (size(header) /= size(x, dim=2)) ) then
					header_ = [ EMPTY_STR ]
					write(*,'(a)') LF//'WARNING: Invalid header for file "'//file_name//'".'// &
								   LF//'Header for this data must have size (1) or '// &
									   '('//str(size(x, dim=2))//').'
				else
					header_ = header
				end if
			end if

			if ( .not. present(locale) ) then
				locale_ = 'US'
			else
				if ( any(LOCALES == locale) ) then
					locale_ = locale
				else
					locale_ = 'US'
					write(*,'(a)') LF//'WARNING: Invalid locale "'//locale//'" for file "'//file_name//'". '// &
									   'Defaulting to US format.'// &
								   LF//'Locale must be one of: '//glue(LOCALES)
				end if
			end if

			if ( .not. present(delim) ) then
				if ( locale_ == 'US' ) then
					delim_ = COMMA
				else
					delim_ = SEMICOLON
				end if
			else
				delim_ = delim
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'e'
			else
				if ( any(REAL_FMTS == fmt) ) then
					fmt_ = fmt
				else
					fmt_ = 'e'
					write(*,'(a)') LF//'WARNING: Invalid format "'//fmt//'" for file "'//file_name//'". '// &
									   'Defaulting to exponential format.'// &
								   LF//'Format must be one of: '//glue(REAL_FMTS)
				end if
			end if

			if ( .not. present(decimals) ) then
				decimals_ = 150
			else
				decimals_ = decimals
			end if

			if ( .not. present(im) ) then
				im_ = EMPTY_STR
			else
				im_ = trim(adjustl(im))
			end if

			call to_text( x=x, file_name=file_name, header=header_, locale=locale_, delim=delim_, &
						  fmt=fmt_, decimals=decimals_, im=im_ )
		else if ( any(BINARY_EXT == ext) ) then
			if ( present(header) )   write(*,'(a)') LF//'WARNING: header not supported for file type "'//ext//'".'
			if ( present(locale) )   write(*,'(a)') LF//'WARNING: locale not supported for file type "'//ext//'".'
			if ( present(delim) )    write(*,'(a)') LF//'WARNING: delim not supported for file type "'//ext//'".'
			if ( present(fmt) )      write(*,'(a)') LF//'WARNING: fmt not supported for file type "'//ext//'".'
			if ( present(decimals) ) write(*,'(a)') LF//'WARNING: decimals not supported for file type "'//ext//'".'
			if ( present(im) )       write(*,'(a)') LF//'WARNING: im not supported for file type "'//ext//'".'

			call to_binary(x=x, file_name=file_name)
		else
			write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
								'due to unsupported file extension "'//ext//'".'// &
							LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
								glue(BINARY_EXT)
		end if
	end procedure to_file_2dc32

	module procedure to_file_3dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_3dc128
	module procedure to_file_3dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_3dc64
	module procedure to_file_3dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_3dc32

	module procedure to_file_4dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_4dc128
	module procedure to_file_4dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_4dc64
	module procedure to_file_4dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_4dc32

	module procedure to_file_5dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_5dc128
	module procedure to_file_5dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_5dc64
	module procedure to_file_5dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_5dc32

	module procedure to_file_6dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_6dc128
	module procedure to_file_6dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_6dc64
	module procedure to_file_6dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_6dc32

	module procedure to_file_7dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_7dc128
	module procedure to_file_7dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_7dc64
	module procedure to_file_7dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_7dc32

	module procedure to_file_8dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_8dc128
	module procedure to_file_8dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_8dc64
	module procedure to_file_8dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_8dc32

	module procedure to_file_9dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_9dc128
	module procedure to_file_9dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_9dc64
	module procedure to_file_9dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_9dc32

	module procedure to_file_10dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_10dc128
	module procedure to_file_10dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_10dc64
	module procedure to_file_10dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_10dc32

	module procedure to_file_11dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_11dc128
	module procedure to_file_11dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_11dc64
	module procedure to_file_11dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_11dc32

	module procedure to_file_12dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_12dc128
	module procedure to_file_12dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_12dc64
	module procedure to_file_12dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_12dc32

	module procedure to_file_13dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_13dc128
	module procedure to_file_13dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_13dc64
	module procedure to_file_13dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_13dc32

	module procedure to_file_14dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_14dc128
	module procedure to_file_14dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_14dc64
	module procedure to_file_14dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_14dc32

	module procedure to_file_15dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_15dc128
	module procedure to_file_15dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_15dc64
	module procedure to_file_15dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_15dc32

	module procedure to_file_1dr128
		character(len=:), allocatable, dimension(:) :: header_
		character(len=:), allocatable :: ext, locale_, delim_, fmt_
		integer :: decimals_, hstat, dim_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = [ EMPTY_STR ]
				hstat = 0
			else
				if ( (size(header) /= 1) .and. (size(header) /= size(x)) ) then
					header_ = [ EMPTY_STR ]
					hstat = -1
					write(*,'(a)') LF//'WARNING: Invalid header for file "'//file_name//'".'// &
								   LF//'Header for this data must have size (1) or '// &
									   '('//str(size(x))//').'
				else
					header_ = header
					if ( size(header) == 1 ) then
						hstat = 1
					else
						hstat = 2
					end if
				end if
			end if

			if ( .not. present(dim) ) then
				if ( hstat == 2 ) then
					dim_ = 2
				else
					dim_ = 1
				end if
			else
				if ( hstat == 2 ) then
					dim_ = 2
					if ( dim /= 2 ) then
						write(*,'(a)') LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
									   file_name//'" for given header... defaulting to (2).'
					end if
				else
					if ( dim == 1 ) then
						dim_ = 1
					else if ( dim == 2 ) then
						dim_ = 2
					else
						dim_ = 1
						write(*,'(a)') LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
									   file_name//'" for given header... defaulting to (1).'
					end if
				end if
			end if

			if ( .not. present(locale) ) then
				locale_ = 'US'
			else
				if ( any(LOCALES == locale) ) then
					locale_ = locale
				else
					locale_ = 'US'
					write(*,'(a)') LF//'WARNING: Invalid locale "'//locale//'" for file "'//file_name//'". '// &
									   'Defaulting to US format.'// &
								   LF//'Locale must be one of: '//glue(LOCALES)
				end if
			end if

			if ( .not. present(delim) ) then
				if ( dim_ == 1 ) then
					delim_ = EMPTY_STR
				else
					if ( locale_ == 'US' ) then
						delim_ = COMMA
					else
						delim_ = SEMICOLON
					end if
				end if
			else
				if ( dim_ == 1 ) then
					delim_ = EMPTY_STR
				else
					delim_ = delim
				end if
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'e'
			else
				if ( any(REAL_FMTS == fmt) ) then
					fmt_ = fmt
				else
					fmt_ = 'e'
					write(*,'(a)') LF//'WARNING: Invalid format "'//fmt//'" for file "'//file_name//'". '// &
									   'Defaulting to exponential format.'// &
								   LF//'Format must be one of: '//glue(REAL_FMTS)
				end if
			end if

			if ( .not. present(decimals) ) then
				decimals_ = 150
			else
				decimals_ = decimals
			end if

			call to_text( x=x, file_name=file_name, header=header_, dim=dim_, locale=locale_, delim=delim_, &
						  fmt=fmt_, decimals=decimals_ )
		else if ( any(BINARY_EXT == ext) ) then
			if ( present(header) )   write(*,'(a)') LF//'WARNING: header not supported for file type "'//ext//'".'
			if ( present(dim) )      write(*,'(a)') LF//'WARNING: dim not supported for file type "'//ext//'".'
			if ( present(locale) )   write(*,'(a)') LF//'WARNING: locale not supported for file type "'//ext//'".'
			if ( present(delim) )    write(*,'(a)') LF//'WARNING: delim not supported for file type "'//ext//'".'
			if ( present(fmt) )      write(*,'(a)') LF//'WARNING: fmt not supported for file type "'//ext//'".'
			if ( present(decimals) ) write(*,'(a)') LF//'WARNING: decimals not supported for file type "'//ext//'".'

			call to_binary(x=x, file_name=file_name)
		else
			write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
								'due to unsupported file extension "'//ext//'".'// &
							LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
								glue(BINARY_EXT)
		end if
	end procedure to_file_1dr128
	module procedure to_file_1dr64
		character(len=:), allocatable, dimension(:) :: header_
		character(len=:), allocatable :: ext, locale_, delim_, fmt_
		integer :: decimals_, hstat, dim_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = [ EMPTY_STR ]
				hstat = 0
			else
				if ( (size(header) /= 1) .and. (size(header) /= size(x)) ) then
					header_ = [ EMPTY_STR ]
					hstat = -1
					write(*,'(a)') LF//'WARNING: Invalid header for file "'//file_name//'".'// &
								   LF//'Header for this data must have size (1) or '// &
									   '('//str(size(x))//').'
				else
					header_ = header
					if ( size(header) == 1 ) then
						hstat = 1
					else
						hstat = 2
					end if
				end if
			end if

			if ( .not. present(dim) ) then
				if ( hstat == 2 ) then
					dim_ = 2
				else
					dim_ = 1
				end if
			else
				if ( hstat == 2 ) then
					dim_ = 2
					if ( dim /= 2 ) then
						write(*,'(a)') LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
									   file_name//'" for given header... defaulting to (2).'
					end if
				else
					if ( dim == 1 ) then
						dim_ = 1
					else if ( dim == 2 ) then
						dim_ = 2
					else
						dim_ = 1
						write(*,'(a)') LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
									   file_name//'" for given header... defaulting to (1).'
					end if
				end if
			end if

			if ( .not. present(locale) ) then
				locale_ = 'US'
			else
				if ( any(LOCALES == locale) ) then
					locale_ = locale
				else
					locale_ = 'US'
					write(*,'(a)') LF//'WARNING: Invalid locale "'//locale//'" for file "'//file_name//'". '// &
									   'Defaulting to US format.'// &
								   LF//'Locale must be one of: '//glue(LOCALES)
				end if
			end if

			if ( .not. present(delim) ) then
				if ( dim_ == 1 ) then
					delim_ = EMPTY_STR
				else
					if ( locale_ == 'US' ) then
						delim_ = COMMA
					else
						delim_ = SEMICOLON
					end if
				end if
			else
				if ( dim_ == 1 ) then
					delim_ = EMPTY_STR
				else
					delim_ = delim
				end if
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'e'
			else
				if ( any(REAL_FMTS == fmt) ) then
					fmt_ = fmt
				else
					fmt_ = 'e'
					write(*,'(a)') LF//'WARNING: Invalid format "'//fmt//'" for file "'//file_name//'". '// &
									   'Defaulting to exponential format.'// &
								   LF//'Format must be one of: '//glue(REAL_FMTS)
				end if
			end if

			if ( .not. present(decimals) ) then
				decimals_ = 150
			else
				decimals_ = decimals
			end if

			call to_text( x=x, file_name=file_name, header=header_, dim=dim_, locale=locale_, delim=delim_, &
						  fmt=fmt_, decimals=decimals_ )
		else if ( any(BINARY_EXT == ext) ) then
			if ( present(header) )   write(*,'(a)') LF//'WARNING: header not supported for file type "'//ext//'".'
			if ( present(dim) )      write(*,'(a)') LF//'WARNING: dim not supported for file type "'//ext//'".'
			if ( present(locale) )   write(*,'(a)') LF//'WARNING: locale not supported for file type "'//ext//'".'
			if ( present(delim) )    write(*,'(a)') LF//'WARNING: delim not supported for file type "'//ext//'".'
			if ( present(fmt) )      write(*,'(a)') LF//'WARNING: fmt not supported for file type "'//ext//'".'
			if ( present(decimals) ) write(*,'(a)') LF//'WARNING: decimals not supported for file type "'//ext//'".'

			call to_binary(x=x, file_name=file_name)
		else
			write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
								'due to unsupported file extension "'//ext//'".'// &
							LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
								glue(BINARY_EXT)
		end if
	end procedure to_file_1dr64
	module procedure to_file_1dr32
		character(len=:), allocatable, dimension(:) :: header_
		character(len=:), allocatable :: ext, locale_, delim_, fmt_
		integer :: decimals_, hstat, dim_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = [ EMPTY_STR ]
				hstat = 0
			else
				if ( (size(header) /= 1) .and. (size(header) /= size(x)) ) then
					header_ = [ EMPTY_STR ]
					hstat = -1
					write(*,'(a)') LF//'WARNING: Invalid header for file "'//file_name//'".'// &
								   LF//'Header for this data must have size (1) or '// &
									   '('//str(size(x))//').'
				else
					header_ = header
					if ( size(header) == 1 ) then
						hstat = 1
					else
						hstat = 2
					end if
				end if
			end if

			if ( .not. present(dim) ) then
				if ( hstat == 2 ) then
					dim_ = 2
				else
					dim_ = 1
				end if
			else
				if ( hstat == 2 ) then
					dim_ = 2
					if ( dim /= 2 ) then
						write(*,'(a)') LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
									   file_name//'" for given header... defaulting to (2).'
					end if
				else
					if ( dim == 1 ) then
						dim_ = 1
					else if ( dim == 2 ) then
						dim_ = 2
					else
						dim_ = 1
						write(*,'(a)') LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
									   file_name//'" for given header... defaulting to (1).'
					end if
				end if
			end if

			if ( .not. present(locale) ) then
				locale_ = 'US'
			else
				if ( any(LOCALES == locale) ) then
					locale_ = locale
				else
					locale_ = 'US'
					write(*,'(a)') LF//'WARNING: Invalid locale "'//locale//'" for file "'//file_name//'". '// &
									   'Defaulting to US format.'// &
								   LF//'Locale must be one of: '//glue(LOCALES)
				end if
			end if

			if ( .not. present(delim) ) then
				if ( dim_ == 1 ) then
					delim_ = EMPTY_STR
				else
					if ( locale_ == 'US' ) then
						delim_ = COMMA
					else
						delim_ = SEMICOLON
					end if
				end if
			else
				if ( dim_ == 1 ) then
					delim_ = EMPTY_STR
				else
					delim_ = delim
				end if
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'e'
			else
				if ( any(REAL_FMTS == fmt) ) then
					fmt_ = fmt
				else
					fmt_ = 'e'
					write(*,'(a)') LF//'WARNING: Invalid format "'//fmt//'" for file "'//file_name//'". '// &
									   'Defaulting to exponential format.'// &
								   LF//'Format must be one of: '//glue(REAL_FMTS)
				end if
			end if

			if ( .not. present(decimals) ) then
				decimals_ = 150
			else
				decimals_ = decimals
			end if

			call to_text( x=x, file_name=file_name, header=header_, dim=dim_, locale=locale_, delim=delim_, &
						  fmt=fmt_, decimals=decimals_ )
		else if ( any(BINARY_EXT == ext) ) then
			if ( present(header) )   write(*,'(a)') LF//'WARNING: header not supported for file type "'//ext//'".'
			if ( present(dim) )      write(*,'(a)') LF//'WARNING: dim not supported for file type "'//ext//'".'
			if ( present(locale) )   write(*,'(a)') LF//'WARNING: locale not supported for file type "'//ext//'".'
			if ( present(delim) )    write(*,'(a)') LF//'WARNING: delim not supported for file type "'//ext//'".'
			if ( present(fmt) )      write(*,'(a)') LF//'WARNING: fmt not supported for file type "'//ext//'".'
			if ( present(decimals) ) write(*,'(a)') LF//'WARNING: decimals not supported for file type "'//ext//'".'

			call to_binary(x=x, file_name=file_name)
		else
			write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
								'due to unsupported file extension "'//ext//'".'// &
							LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
								glue(BINARY_EXT)
		end if
	end procedure to_file_1dr32

	module procedure to_file_2dr128
		character(len=:), allocatable, dimension(:) :: header_
		character(len=:), allocatable :: ext, locale_, delim_, fmt_
		integer :: decimals_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = [ EMPTY_STR ]
			else
				if ( (size(header) /= 1) .and. (size(header) /= size(x, dim=2)) ) then
					header_ = [ EMPTY_STR ]
					write(*,'(a)') LF//'WARNING: Invalid header for file "'//file_name//'".'// &
								   LF//'Header for this data must have size (1) or '// &
									   '('//str(size(x, dim=2))//').'
				else
					header_ = header
				end if
			end if

			if ( .not. present(locale) ) then
				locale_ = 'US'
			else
				if ( any(LOCALES == locale) ) then
					locale_ = locale
				else
					locale_ = 'US'
					write(*,'(a)') LF//'WARNING: Invalid locale "'//locale//'" for file "'//file_name//'". '// &
									   'Defaulting to US format.'// &
								   LF//'Locale must be one of: '//glue(LOCALES)
				end if
			end if

			if ( .not. present(delim) ) then
				if ( locale_ == 'US' ) then
					delim_ = COMMA
				else
					delim_ = SEMICOLON
				end if
			else
				delim_ = delim
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'e'
			else
				if ( any(REAL_FMTS == fmt) ) then
					fmt_ = fmt
				else
					fmt_ = 'e'
					write(*,'(a)') LF//'WARNING: Invalid format "'//fmt//'" for file "'//file_name//'". '// &
									   'Defaulting to exponential format.'// &
								   LF//'Format must be one of: '//glue(REAL_FMTS)
				end if
			end if

			if ( .not. present(decimals) ) then
				decimals_ = 150
			else
				decimals_ = decimals
			end if

			call to_text( x=x, file_name=file_name, header=header_, locale=locale_, delim=delim_, &
						  fmt=fmt_, decimals=decimals_ )
		else if ( any(BINARY_EXT == ext) ) then
			if ( present(header) )   write(*,'(a)') LF//'WARNING: header not supported for file type "'//ext//'".'
			if ( present(locale) )   write(*,'(a)') LF//'WARNING: locale not supported for file type "'//ext//'".'
			if ( present(delim) )    write(*,'(a)') LF//'WARNING: delim not supported for file type "'//ext//'".'
			if ( present(fmt) )      write(*,'(a)') LF//'WARNING: fmt not supported for file type "'//ext//'".'
			if ( present(decimals) ) write(*,'(a)') LF//'WARNING: decimals not supported for file type "'//ext//'".'

			call to_binary(x=x, file_name=file_name)
		else
			write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
								'due to unsupported file extension "'//ext//'".'// &
							LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
								glue(BINARY_EXT)
		end if
	end procedure to_file_2dr128
	module procedure to_file_2dr64
		character(len=:), allocatable, dimension(:) :: header_
		character(len=:), allocatable :: ext, locale_, delim_, fmt_
		integer :: decimals_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = [ EMPTY_STR ]
			else
				if ( (size(header) /= 1) .and. (size(header) /= size(x, dim=2)) ) then
					header_ = [ EMPTY_STR ]
					write(*,'(a)') LF//'WARNING: Invalid header for file "'//file_name//'".'// &
								   LF//'Header for this data must have size (1) or '// &
									   '('//str(size(x, dim=2))//').'
				else
					header_ = header
				end if
			end if

			if ( .not. present(locale) ) then
				locale_ = 'US'
			else
				if ( any(LOCALES == locale) ) then
					locale_ = locale
				else
					locale_ = 'US'
					write(*,'(a)') LF//'WARNING: Invalid locale "'//locale//'" for file "'//file_name//'". '// &
									   'Defaulting to US format.'// &
								   LF//'Locale must be one of: '//glue(LOCALES)
				end if
			end if

			if ( .not. present(delim) ) then
				if ( locale_ == 'US' ) then
					delim_ = COMMA
				else
					delim_ = SEMICOLON
				end if
			else
				delim_ = delim
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'e'
			else
				if ( any(REAL_FMTS == fmt) ) then
					fmt_ = fmt
				else
					fmt_ = 'e'
					write(*,'(a)') LF//'WARNING: Invalid format "'//fmt//'" for file "'//file_name//'". '// &
									   'Defaulting to exponential format.'// &
								   LF//'Format must be one of: '//glue(REAL_FMTS)
				end if
			end if

			if ( .not. present(decimals) ) then
				decimals_ = 150
			else
				decimals_ = decimals
			end if

			call to_text( x=x, file_name=file_name, header=header_, locale=locale_, delim=delim_, &
						  fmt=fmt_, decimals=decimals_ )
		else if ( any(BINARY_EXT == ext) ) then
			if ( present(header) )   write(*,'(a)') LF//'WARNING: header not supported for file type "'//ext//'".'
			if ( present(locale) )   write(*,'(a)') LF//'WARNING: locale not supported for file type "'//ext//'".'
			if ( present(delim) )    write(*,'(a)') LF//'WARNING: delim not supported for file type "'//ext//'".'
			if ( present(fmt) )      write(*,'(a)') LF//'WARNING: fmt not supported for file type "'//ext//'".'
			if ( present(decimals) ) write(*,'(a)') LF//'WARNING: decimals not supported for file type "'//ext//'".'

			call to_binary(x=x, file_name=file_name)
		else
			write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
								'due to unsupported file extension "'//ext//'".'// &
							LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
								glue(BINARY_EXT)
		end if
	end procedure to_file_2dr64
	module procedure to_file_2dr32
		character(len=:), allocatable, dimension(:) :: header_
		character(len=:), allocatable :: ext, locale_, delim_, fmt_
		integer :: decimals_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = [ EMPTY_STR ]
			else
				if ( (size(header) /= 1) .and. (size(header) /= size(x, dim=2)) ) then
					header_ = [ EMPTY_STR ]
					write(*,'(a)') LF//'WARNING: Invalid header for file "'//file_name//'".'// &
								   LF//'Header for this data must have size (1) or '// &
									   '('//str(size(x, dim=2))//').'
				else
					header_ = header
				end if
			end if

			if ( .not. present(locale) ) then
				locale_ = 'US'
			else
				if ( any(LOCALES == locale) ) then
					locale_ = locale
				else
					locale_ = 'US'
					write(*,'(a)') LF//'WARNING: Invalid locale "'//locale//'" for file "'//file_name//'". '// &
									   'Defaulting to US format.'// &
								   LF//'Locale must be one of: '//glue(LOCALES)
				end if
			end if

			if ( .not. present(delim) ) then
				if ( locale_ == 'US' ) then
					delim_ = COMMA
				else
					delim_ = SEMICOLON
				end if
			else
				delim_ = delim
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'e'
			else
				if ( any(REAL_FMTS == fmt) ) then
					fmt_ = fmt
				else
					fmt_ = 'e'
					write(*,'(a)') LF//'WARNING: Invalid format "'//fmt//'" for file "'//file_name//'". '// &
									   'Defaulting to exponential format.'// &
								   LF//'Format must be one of: '//glue(REAL_FMTS)
				end if
			end if

			if ( .not. present(decimals) ) then
				decimals_ = 150
			else
				decimals_ = decimals
			end if

			call to_text( x=x, file_name=file_name, header=header_, locale=locale_, delim=delim_, &
						  fmt=fmt_, decimals=decimals_ )
		else if ( any(BINARY_EXT == ext) ) then
			if ( present(header) )   write(*,'(a)') LF//'WARNING: header not supported for file type "'//ext//'".'
			if ( present(locale) )   write(*,'(a)') LF//'WARNING: locale not supported for file type "'//ext//'".'
			if ( present(delim) )    write(*,'(a)') LF//'WARNING: delim not supported for file type "'//ext//'".'
			if ( present(fmt) )      write(*,'(a)') LF//'WARNING: fmt not supported for file type "'//ext//'".'
			if ( present(decimals) ) write(*,'(a)') LF//'WARNING: decimals not supported for file type "'//ext//'".'

			call to_binary(x=x, file_name=file_name)
		else
			write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
								'due to unsupported file extension "'//ext//'".'// &
							LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
								glue(BINARY_EXT)
		end if
	end procedure to_file_2dr32

	module procedure to_file_3dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_3dr128
	module procedure to_file_3dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_3dr64
	module procedure to_file_3dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_3dr32

	module procedure to_file_4dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_4dr128
	module procedure to_file_4dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_4dr64
	module procedure to_file_4dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_4dr32

	module procedure to_file_5dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_5dr128
	module procedure to_file_5dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_5dr64
	module procedure to_file_5dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_5dr32

	module procedure to_file_6dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_6dr128
	module procedure to_file_6dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_6dr64
	module procedure to_file_6dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_6dr32

	module procedure to_file_7dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_7dr128
	module procedure to_file_7dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_7dr64
	module procedure to_file_7dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_7dr32

	module procedure to_file_8dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_8dr128
	module procedure to_file_8dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_8dr64
	module procedure to_file_8dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_8dr32

	module procedure to_file_9dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_9dr128
	module procedure to_file_9dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_9dr64
	module procedure to_file_9dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_9dr32

	module procedure to_file_10dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_10dr128
	module procedure to_file_10dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_10dr64
	module procedure to_file_10dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_10dr32

	module procedure to_file_11dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_11dr128
	module procedure to_file_11dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_11dr64
	module procedure to_file_11dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_11dr32

	module procedure to_file_12dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_12dr128
	module procedure to_file_12dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_12dr64
	module procedure to_file_12dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_12dr32

	module procedure to_file_13dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_13dr128
	module procedure to_file_13dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_13dr64
	module procedure to_file_13dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_13dr32

	module procedure to_file_14dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_14dr128
	module procedure to_file_14dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_14dr64
	module procedure to_file_14dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_14dr32

	module procedure to_file_15dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_15dr128
	module procedure to_file_15dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_15dr64
	module procedure to_file_15dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_15dr32

	module procedure to_file_1di64
		character(len=:), allocatable :: ext, delim_, fmt_
		character(len=:), allocatable, dimension(:) :: header_
		integer :: hstat, dim_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = [ EMPTY_STR ]
				hstat = 0
			else
				if ( (size(header) /= 1) .and. (size(header) /= size(x)) ) then
					header_ = [ EMPTY_STR ]
					hstat = -1
					write(*,'(a)') LF//'WARNING: Invalid header for file "'//file_name//'".'// &
								   LF//'Header for this data must have size (1) or '// &
									   '('//str(size(x))//').'
				else
					header_ = header
					if ( size(header) == 1 ) then
						hstat = 1
					else
						hstat = 2
					end if
				end if
			end if

			if ( .not. present(dim) ) then
				if ( hstat == 2 ) then
					dim_ = 2
				else
					dim_ = 1
				end if
			else
				if ( hstat == 2 ) then
					dim_ = 2
					if ( dim /= 2 ) then
						write(*,'(a)') LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
									   file_name//'" for given header... defaulting to (2).'
					end if
				else
					if ( dim == 1 ) then
						dim_ = 1
					else if ( dim == 2 ) then
						dim_ = 2
					else
						dim_ = 1
						write(*,'(a)') LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
									   file_name//'" for given header... defaulting to (1).'
					end if
				end if
			end if

			if ( .not. present(delim) ) then
				if ( dim_ == 1 ) then
					delim_ = EMPTY_STR
				else
					delim_ = COMMA
				end if
			else
				if ( dim_ == 1 ) then
					delim_ = EMPTY_STR
				else
					delim_ = delim
				end if
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'i'
			else
				if ( any(INT_FMTS == fmt) ) then
					fmt_ = fmt
				else
					fmt_ = 'i'
					write(*,'(a)') LF//'WARNING: Invalid format "'//fmt//'" for file "'//file_name//'". '// &
									   'Defaulting to integer format.'// &
								   LF//'Format must be one of: '//glue(INT_FMTS)
				end if
			end if

			call to_text(x=x, file_name=file_name, header=header_, dim=dim_, delim=delim_, fmt=fmt_)
		else if ( any(BINARY_EXT == ext) ) then
			if ( present(header) ) write(*,'(a)') LF//'WARNING: header not supported for file type "'//ext//'".'
			if ( present(dim) )    write(*,'(a)') LF//'WARNING: dim not supported for file type "'//ext//'".'
			if ( present(delim) )  write(*,'(a)') LF//'WARNING: delim not supported for file type "'//ext//'".'
			if ( present(fmt) )    write(*,'(a)') LF//'WARNING: fmt not supported for file type "'//ext//'".'

			call to_binary(x=x, file_name=file_name)
		else
			write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
								'due to unsupported file extension "'//ext//'".'// &
							LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
								glue(BINARY_EXT)
		end if
	end procedure to_file_1di64
	module procedure to_file_1di32
		character(len=:), allocatable :: ext, delim_, fmt_
		character(len=:), allocatable, dimension(:) :: header_
		integer :: hstat, dim_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = [ EMPTY_STR ]
				hstat = 0
			else
				if ( (size(header) /= 1) .and. (size(header) /= size(x)) ) then
					header_ = [ EMPTY_STR ]
					hstat = -1
					write(*,'(a)') LF//'WARNING: Invalid header for file "'//file_name//'".'// &
								   LF//'Header for this data must have size (1) or '// &
									   '('//str(size(x))//').'
				else
					header_ = header
					if ( size(header) == 1 ) then
						hstat = 1
					else
						hstat = 2
					end if
				end if
			end if

			if ( .not. present(dim) ) then
				if ( hstat == 2 ) then
					dim_ = 2
				else
					dim_ = 1
				end if
			else
				if ( hstat == 2 ) then
					dim_ = 2
					if ( dim /= 2 ) then
						write(*,'(a)') LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
									   file_name//'" for given header... defaulting to (2).'
					end if
				else
					if ( dim == 1 ) then
						dim_ = 1
					else if ( dim == 2 ) then
						dim_ = 2
					else
						dim_ = 1
						write(*,'(a)') LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
									   file_name//'" for given header... defaulting to (1).'
					end if
				end if
			end if

			if ( .not. present(delim) ) then
				if ( dim_ == 1 ) then
					delim_ = EMPTY_STR
				else
					delim_ = COMMA
				end if
			else
				if ( dim_ == 1 ) then
					delim_ = EMPTY_STR
				else
					delim_ = delim
				end if
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'i'
			else
				if ( any(INT_FMTS == fmt) ) then
					fmt_ = fmt
				else
					fmt_ = 'i'
					write(*,'(a)') LF//'WARNING: Invalid format "'//fmt//'" for file "'//file_name//'". '// &
									   'Defaulting to integer format.'// &
								   LF//'Format must be one of: '//glue(INT_FMTS)
				end if
			end if

			call to_text(x=x, file_name=file_name, header=header_, dim=dim_, delim=delim_, fmt=fmt_)
		else if ( any(BINARY_EXT == ext) ) then
			if ( present(header) ) write(*,'(a)') LF//'WARNING: header not supported for file type "'//ext//'".'
			if ( present(dim) )    write(*,'(a)') LF//'WARNING: dim not supported for file type "'//ext//'".'
			if ( present(delim) )  write(*,'(a)') LF//'WARNING: delim not supported for file type "'//ext//'".'
			if ( present(fmt) )    write(*,'(a)') LF//'WARNING: fmt not supported for file type "'//ext//'".'

			call to_binary(x=x, file_name=file_name)
		else
			write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
								'due to unsupported file extension "'//ext//'".'// &
							LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
								glue(BINARY_EXT)
		end if
	end procedure to_file_1di32
	module procedure to_file_1di16
		character(len=:), allocatable :: ext, delim_, fmt_
		character(len=:), allocatable, dimension(:) :: header_
		integer :: hstat, dim_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = [ EMPTY_STR ]
				hstat = 0
			else
				if ( (size(header) /= 1) .and. (size(header) /= size(x)) ) then
					header_ = [ EMPTY_STR ]
					hstat = -1
					write(*,'(a)') LF//'WARNING: Invalid header for file "'//file_name//'".'// &
								   LF//'Header for this data must have size (1) or '// &
									   '('//str(size(x))//').'
				else
					header_ = header
					if ( size(header) == 1 ) then
						hstat = 1
					else
						hstat = 2
					end if
				end if
			end if

			if ( .not. present(dim) ) then
				if ( hstat == 2 ) then
					dim_ = 2
				else
					dim_ = 1
				end if
			else
				if ( hstat == 2 ) then
					dim_ = 2
					if ( dim /= 2 ) then
						write(*,'(a)') LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
									   file_name//'" for given header... defaulting to (2).'
					end if
				else
					if ( dim == 1 ) then
						dim_ = 1
					else if ( dim == 2 ) then
						dim_ = 2
					else
						dim_ = 1
						write(*,'(a)') LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
									   file_name//'" for given header... defaulting to (1).'
					end if
				end if
			end if

			if ( .not. present(delim) ) then
				if ( dim_ == 1 ) then
					delim_ = EMPTY_STR
				else
					delim_ = COMMA
				end if
			else
				if ( dim_ == 1 ) then
					delim_ = EMPTY_STR
				else
					delim_ = delim
				end if
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'i'
			else
				if ( any(INT_FMTS == fmt) ) then
					fmt_ = fmt
				else
					fmt_ = 'i'
					write(*,'(a)') LF//'WARNING: Invalid format "'//fmt//'" for file "'//file_name//'". '// &
									   'Defaulting to integer format.'// &
								   LF//'Format must be one of: '//glue(INT_FMTS)
				end if
			end if

			call to_text(x=x, file_name=file_name, header=header_, dim=dim_, delim=delim_, fmt=fmt_)
		else if ( any(BINARY_EXT == ext) ) then
			if ( present(header) ) write(*,'(a)') LF//'WARNING: header not supported for file type "'//ext//'".'
			if ( present(dim) )    write(*,'(a)') LF//'WARNING: dim not supported for file type "'//ext//'".'
			if ( present(delim) )  write(*,'(a)') LF//'WARNING: delim not supported for file type "'//ext//'".'
			if ( present(fmt) )    write(*,'(a)') LF//'WARNING: fmt not supported for file type "'//ext//'".'

			call to_binary(x=x, file_name=file_name)
		else
			write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
								'due to unsupported file extension "'//ext//'".'// &
							LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
								glue(BINARY_EXT)
		end if
	end procedure to_file_1di16
	module procedure to_file_1di8
		character(len=:), allocatable :: ext, delim_, fmt_
		character(len=:), allocatable, dimension(:) :: header_
		integer :: hstat, dim_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = [ EMPTY_STR ]
				hstat = 0
			else
				if ( (size(header) /= 1) .and. (size(header) /= size(x)) ) then
					header_ = [ EMPTY_STR ]
					hstat = -1
					write(*,'(a)') LF//'WARNING: Invalid header for file "'//file_name//'".'// &
								   LF//'Header for this data must have size (1) or '// &
									   '('//str(size(x))//').'
				else
					header_ = header
					if ( size(header) == 1 ) then
						hstat = 1
					else
						hstat = 2
					end if
				end if
			end if

			if ( .not. present(dim) ) then
				if ( hstat == 2 ) then
					dim_ = 2
				else
					dim_ = 1
				end if
			else
				if ( hstat == 2 ) then
					dim_ = 2
					if ( dim /= 2 ) then
						write(*,'(a)') LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
									   file_name//'" for given header... defaulting to (2).'
					end if
				else
					if ( dim == 1 ) then
						dim_ = 1
					else if ( dim == 2 ) then
						dim_ = 2
					else
						dim_ = 1
						write(*,'(a)') LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
									   file_name//'" for given header... defaulting to (1).'
					end if
				end if
			end if

			if ( .not. present(delim) ) then
				if ( dim_ == 1 ) then
					delim_ = EMPTY_STR
				else
					delim_ = COMMA
				end if
			else
				if ( dim_ == 1 ) then
					delim_ = EMPTY_STR
				else
					delim_ = delim
				end if
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'i'
			else
				if ( any(INT_FMTS == fmt) ) then
					fmt_ = fmt
				else
					fmt_ = 'i'
					write(*,'(a)') LF//'WARNING: Invalid format "'//fmt//'" for file "'//file_name//'". '// &
									   'Defaulting to integer format.'// &
								   LF//'Format must be one of: '//glue(INT_FMTS)
				end if
			end if

			call to_text(x=x, file_name=file_name, header=header_, dim=dim_, delim=delim_, fmt=fmt_)
		else if ( any(BINARY_EXT == ext) ) then
			if ( present(header) ) write(*,'(a)') LF//'WARNING: header not supported for file type "'//ext//'".'
			if ( present(dim) )    write(*,'(a)') LF//'WARNING: dim not supported for file type "'//ext//'".'
			if ( present(delim) )  write(*,'(a)') LF//'WARNING: delim not supported for file type "'//ext//'".'
			if ( present(fmt) )    write(*,'(a)') LF//'WARNING: fmt not supported for file type "'//ext//'".'

			call to_binary(x=x, file_name=file_name)
		else
			write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
								'due to unsupported file extension "'//ext//'".'// &
							LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
								glue(BINARY_EXT)
		end if
	end procedure to_file_1di8

	module procedure to_file_2di64
		character(len=:), allocatable :: ext, delim_, fmt_
		character(len=:), allocatable, dimension(:) :: header_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = [ EMPTY_STR ]
			else
				if ( (size(header) /= 1) .and. (size(header) /= size(x, dim=2)) ) then
					header_ = [ EMPTY_STR ]
					write(*,'(a)') LF//'WARNING: Invalid header for file "'//file_name//'".'// &
								   LF//'Header for this data must have size (1) or '// &
									   '('//str(size(x, dim=2))//').'
				else
					header_ = header
				end if
			end if

			if ( .not. present(delim) ) then
				delim_ = COMMA
			else
				delim_ = delim
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'i'
			else
				if ( any(INT_FMTS == fmt) ) then
					fmt_ = fmt
				else
					fmt_ = 'i'
					write(*,'(a)') LF//'WARNING: Invalid format "'//fmt//'" for file "'//file_name//'". '// &
									   'Defaulting to integer format.'// &
								   LF//'Format must be one of: '//glue(INT_FMTS)
				end if
			end if

			call to_text(x=x, file_name=file_name, header=header_, delim=delim_, fmt=fmt_)
		else if ( any(BINARY_EXT == ext) ) then
			if ( present(header) ) write(*,'(a)') LF//'WARNING: header not supported for file type "'//ext//'".'
			if ( present(delim) )  write(*,'(a)') LF//'WARNING: delim not supported for file type "'//ext//'".'
			if ( present(fmt) )    write(*,'(a)') LF//'WARNING: fmt not supported for file type "'//ext//'".'

			call to_binary(x=x, file_name=file_name)
		else
			write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
								'due to unsupported file extension "'//ext//'".'// &
							LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
								glue(BINARY_EXT)
		end if
	end procedure to_file_2di64
	module procedure to_file_2di32
		character(len=:), allocatable :: ext, delim_, fmt_
		character(len=:), allocatable, dimension(:) :: header_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = [ EMPTY_STR ]
			else
				if ( (size(header) /= 1) .and. (size(header) /= size(x, dim=2)) ) then
					header_ = [ EMPTY_STR ]
					write(*,'(a)') LF//'WARNING: Invalid header for file "'//file_name//'".'// &
								   LF//'Header for this data must have size (1) or '// &
									   '('//str(size(x, dim=2))//').'
				else
					header_ = header
				end if
			end if

			if ( .not. present(delim) ) then
				delim_ = COMMA
			else
				delim_ = delim
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'i'
			else
				if ( any(INT_FMTS == fmt) ) then
					fmt_ = fmt
				else
					fmt_ = 'i'
					write(*,'(a)') LF//'WARNING: Invalid format "'//fmt//'" for file "'//file_name//'". '// &
									   'Defaulting to integer format.'// &
								   LF//'Format must be one of: '//glue(INT_FMTS)
				end if
			end if

			call to_text(x=x, file_name=file_name, header=header_, delim=delim_, fmt=fmt_)
		else if ( any(BINARY_EXT == ext) ) then
			if ( present(header) ) write(*,'(a)') LF//'WARNING: header not supported for file type "'//ext//'".'
			if ( present(delim) )  write(*,'(a)') LF//'WARNING: delim not supported for file type "'//ext//'".'
			if ( present(fmt) )    write(*,'(a)') LF//'WARNING: fmt not supported for file type "'//ext//'".'

			call to_binary(x=x, file_name=file_name)
		else
			write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
								'due to unsupported file extension "'//ext//'".'// &
							LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
								glue(BINARY_EXT)
		end if
	end procedure to_file_2di32
	module procedure to_file_2di16
		character(len=:), allocatable :: ext, delim_, fmt_
		character(len=:), allocatable, dimension(:) :: header_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = [ EMPTY_STR ]
			else
				if ( (size(header) /= 1) .and. (size(header) /= size(x, dim=2)) ) then
					header_ = [ EMPTY_STR ]
					write(*,'(a)') LF//'WARNING: Invalid header for file "'//file_name//'".'// &
								   LF//'Header for this data must have size (1) or '// &
									   '('//str(size(x, dim=2))//').'
				else
					header_ = header
				end if
			end if

			if ( .not. present(delim) ) then
				delim_ = COMMA
			else
				delim_ = delim
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'i'
			else
				if ( any(INT_FMTS == fmt) ) then
					fmt_ = fmt
				else
					fmt_ = 'i'
					write(*,'(a)') LF//'WARNING: Invalid format "'//fmt//'" for file "'//file_name//'". '// &
									   'Defaulting to integer format.'// &
								   LF//'Format must be one of: '//glue(INT_FMTS)
				end if
			end if

			call to_text(x=x, file_name=file_name, header=header_, delim=delim_, fmt=fmt_)
		else if ( any(BINARY_EXT == ext) ) then
			if ( present(header) ) write(*,'(a)') LF//'WARNING: header not supported for file type "'//ext//'".'
			if ( present(delim) )  write(*,'(a)') LF//'WARNING: delim not supported for file type "'//ext//'".'
			if ( present(fmt) )    write(*,'(a)') LF//'WARNING: fmt not supported for file type "'//ext//'".'

			call to_binary(x=x, file_name=file_name)
		else
			write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
								'due to unsupported file extension "'//ext//'".'// &
							LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
								glue(BINARY_EXT)
		end if
	end procedure to_file_2di16
	module procedure to_file_2di8
		character(len=:), allocatable :: ext, delim_, fmt_
		character(len=:), allocatable, dimension(:) :: header_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = [ EMPTY_STR ]
			else
				if ( (size(header) /= 1) .and. (size(header) /= size(x, dim=2)) ) then
					header_ = [ EMPTY_STR ]
					write(*,'(a)') LF//'WARNING: Invalid header for file "'//file_name//'".'// &
								   LF//'Header for this data must have size (1) or '// &
									   '('//str(size(x, dim=2))//').'
				else
					header_ = header
				end if
			end if

			if ( .not. present(delim) ) then
				delim_ = COMMA
			else
				delim_ = delim
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'i'
			else
				if ( any(INT_FMTS == fmt) ) then
					fmt_ = fmt
				else
					fmt_ = 'i'
					write(*,'(a)') LF//'WARNING: Invalid format "'//fmt//'" for file "'//file_name//'". '// &
									   'Defaulting to integer format.'// &
								   LF//'Format must be one of: '//glue(INT_FMTS)
				end if
			end if

			call to_text(x=x, file_name=file_name, header=header_, delim=delim_, fmt=fmt_)
		else if ( any(BINARY_EXT == ext) ) then
			if ( present(header) ) write(*,'(a)') LF//'WARNING: header not supported for file type "'//ext//'".'
			if ( present(delim) )  write(*,'(a)') LF//'WARNING: delim not supported for file type "'//ext//'".'
			if ( present(fmt) )    write(*,'(a)') LF//'WARNING: fmt not supported for file type "'//ext//'".'

			call to_binary(x=x, file_name=file_name)
		else
			write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
								'due to unsupported file extension "'//ext//'".'// &
							LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
								glue(BINARY_EXT)
		end if
	end procedure to_file_2di8

	module procedure to_file_3di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_3di64
	module procedure to_file_3di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_3di32
	module procedure to_file_3di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_3di16
	module procedure to_file_3di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_3di8

	module procedure to_file_4di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_4di64
	module procedure to_file_4di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_4di32
	module procedure to_file_4di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_4di16
	module procedure to_file_4di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_4di8

	module procedure to_file_5di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_5di64
	module procedure to_file_5di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_5di32
	module procedure to_file_5di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_5di16
	module procedure to_file_5di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_5di8

	module procedure to_file_6di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_6di64
	module procedure to_file_6di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_6di32
	module procedure to_file_6di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_6di16
	module procedure to_file_6di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_6di8

	module procedure to_file_7di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_7di64
	module procedure to_file_7di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_7di32
	module procedure to_file_7di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_7di16
	module procedure to_file_7di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_7di8

	module procedure to_file_8di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_8di64
	module procedure to_file_8di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_8di32
	module procedure to_file_8di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_8di16
	module procedure to_file_8di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_8di8

	module procedure to_file_9di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_9di64
	module procedure to_file_9di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_9di32
	module procedure to_file_9di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_9di16
	module procedure to_file_9di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_9di8

	module procedure to_file_10di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_10di64
	module procedure to_file_10di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_10di32
	module procedure to_file_10di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_10di16
	module procedure to_file_10di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_10di8

	module procedure to_file_11di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_11di64
	module procedure to_file_11di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_11di32
	module procedure to_file_11di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_11di16
	module procedure to_file_11di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_11di8

	module procedure to_file_12di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_12di64
	module procedure to_file_12di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_12di32
	module procedure to_file_12di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_12di16
	module procedure to_file_12di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_12di8

	module procedure to_file_13di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_13di64
	module procedure to_file_13di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_13di32
	module procedure to_file_13di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_13di16
	module procedure to_file_13di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_13di8

	module procedure to_file_14di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_14di64
	module procedure to_file_14di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_14di32
	module procedure to_file_14di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_14di16
	module procedure to_file_14di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_14di8

	module procedure to_file_15di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_15di64
	module procedure to_file_15di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_15di32
	module procedure to_file_15di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_15di16
	module procedure to_file_15di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			call to_binary(x=x, file_name=file_name)
		else
			if ( any(TEXT_EXT == ext) ) then
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
									'dimension ('//str(rank(x))//') to text.'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			else
				write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
									'due to unsupported file extension "'//ext//'".'// &
								LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure to_file_15di8

	! Reading Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	module procedure from_textfile_1dc128
		character(len=:), allocatable :: ext, locale_, fmt_, im_
		logical :: header_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = .false.
			else
				header_ = header
			end if

			if ( .not. present(locale) ) then
				locale_ = 'US'
			else
				if ( any(LOCALES == locale) ) then
					locale_ = locale
				else
					error stop LF//'FATAL: Invalid locale "'//locale//'" for read of file "'//file_name//'".'// &
							   LF//'Locale must be one of: '//glue(LOCALES)
				end if
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'e'
			else
				if ( any(REAL_FMTS == fmt) ) then
					fmt_ = fmt
				else
					error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file_name//'" '// &
								   'into complex array.'// &
							   LF//'Format must be one of: '//glue(REAL_FMTS)
				end if
			end if

			if ( .not. present(im) ) then
				im_ = EMPTY_STR
			else
				im_ = im
			end if

			call from_text(file_name=file_name, into=into, header=header_, locale=locale_, fmt=fmt_, im=im_)
		else
			if ( any(BINARY_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
							   'for binary data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_textfile_1dc128
	module procedure from_binaryfile_1dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
							   'for textual data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_binaryfile_1dc128
	module procedure from_textfile_1dc64
		character(len=:), allocatable :: ext, locale_, fmt_, im_
		logical :: header_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = .false.
			else
				header_ = header
			end if

			if ( .not. present(locale) ) then
				locale_ = 'US'
			else
				if ( any(LOCALES == locale) ) then
					locale_ = locale
				else
					error stop LF//'FATAL: Invalid locale "'//locale//'" for read of file "'//file_name//'".'// &
							   LF//'Locale must be one of: '//glue(LOCALES)
				end if
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'e'
			else
				if ( any(REAL_FMTS == fmt) ) then
					fmt_ = fmt
				else
					error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file_name//'" '// &
								   'into complex array.'// &
							   LF//'Format must be one of: '//glue(REAL_FMTS)
				end if
			end if

			if ( .not. present(im) ) then
				im_ = EMPTY_STR
			else
				im_ = im
			end if

			call from_text(file_name=file_name, into=into, header=header_, locale=locale_, fmt=fmt_, im=im_)
		else
			if ( any(BINARY_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
							   'for binary data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_textfile_1dc64
	module procedure from_binaryfile_1dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
							   'for textual data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_binaryfile_1dc64
	module procedure from_textfile_1dc32
		character(len=:), allocatable :: ext, locale_, fmt_, im_
		logical :: header_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = .false.
			else
				header_ = header
			end if

			if ( .not. present(locale) ) then
				locale_ = 'US'
			else
				if ( any(LOCALES == locale) ) then
					locale_ = locale
				else
					error stop LF//'FATAL: Invalid locale "'//locale//'" for read of file "'//file_name//'".'// &
							   LF//'Locale must be one of: '//glue(LOCALES)
				end if
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'e'
			else
				if ( any(REAL_FMTS == fmt) ) then
					fmt_ = fmt
				else
					error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file_name//'" '// &
								   'into complex array.'// &
							   LF//'Format must be one of: '//glue(REAL_FMTS)
				end if
			end if

			if ( .not. present(im) ) then
				im_ = EMPTY_STR
			else
				im_ = im
			end if

			call from_text(file_name=file_name, into=into, header=header_, locale=locale_, fmt=fmt_, im=im_)
		else
			if ( any(BINARY_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
							   'for binary data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_textfile_1dc32
	module procedure from_binaryfile_1dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
							   'for textual data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_binaryfile_1dc32

	module procedure from_textfile_2dc128
		character(len=:), allocatable :: ext, locale_, fmt_, im_
		logical :: header_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = .false.
			else
				header_ = header
			end if

			if ( .not. present(locale) ) then
				locale_ = 'US'
			else
				if ( any(LOCALES == locale) ) then
					locale_ = locale
				else
					error stop LF//'FATAL: Invalid locale "'//locale//'" for read of file "'//file_name//'".'// &
							   LF//'Locale must be one of: '//glue(LOCALES)
				end if
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'e'
			else
				if ( any(REAL_FMTS == fmt) ) then
					fmt_ = fmt
				else
					error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file_name//'" '// &
								   'into complex array.'// &
							   LF//'Format must be one of: '//glue(REAL_FMTS)
				end if
			end if

			if ( .not. present(im) ) then
				im_ = EMPTY_STR
			else
				im_ = im
			end if

			call from_text(file_name=file_name, into=into, header=header_, locale=locale_, fmt=fmt_, im=im_)
		else
			if ( any(BINARY_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
							   'for binary data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_textfile_2dc128
	module procedure from_binaryfile_2dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
							   'for textual data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_binaryfile_2dc128
	module procedure from_textfile_2dc64
		character(len=:), allocatable :: ext, locale_, fmt_, im_
		logical :: header_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = .false.
			else
				header_ = header
			end if

			if ( .not. present(locale) ) then
				locale_ = 'US'
			else
				if ( any(LOCALES == locale) ) then
					locale_ = locale
				else
					error stop LF//'FATAL: Invalid locale "'//locale//'" for read of file "'//file_name//'".'// &
							   LF//'Locale must be one of: '//glue(LOCALES)
				end if
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'e'
			else
				if ( any(REAL_FMTS == fmt) ) then
					fmt_ = fmt
				else
					error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file_name//'" '// &
								   'into complex array.'// &
							   LF//'Format must be one of: '//glue(REAL_FMTS)
				end if
			end if

			if ( .not. present(im) ) then
				im_ = EMPTY_STR
			else
				im_ = im
			end if

			call from_text(file_name=file_name, into=into, header=header_, locale=locale_, fmt=fmt_, im=im_)
		else
			if ( any(BINARY_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
							   'for binary data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_textfile_2dc64
	module procedure from_binaryfile_2dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
							   'for textual data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_binaryfile_2dc64
	module procedure from_textfile_2dc32
		character(len=:), allocatable :: ext, locale_, fmt_, im_
		logical :: header_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = .false.
			else
				header_ = header
			end if

			if ( .not. present(locale) ) then
				locale_ = 'US'
			else
				if ( any(LOCALES == locale) ) then
					locale_ = locale
				else
					error stop LF//'FATAL: Invalid locale "'//locale//'" for read of file "'//file_name//'".'// &
							   LF//'Locale must be one of: '//glue(LOCALES)
				end if
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'e'
			else
				if ( any(REAL_FMTS == fmt) ) then
					fmt_ = fmt
				else
					error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file_name//'" '// &
								   'into complex array.'// &
							   LF//'Format must be one of: '//glue(REAL_FMTS)
				end if
			end if

			if ( .not. present(im) ) then
				im_ = EMPTY_STR
			else
				im_ = im
			end if

			call from_text(file_name=file_name, into=into, header=header_, locale=locale_, fmt=fmt_, im=im_)
		else
			if ( any(BINARY_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
							   'for binary data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_textfile_2dc32
	module procedure from_binaryfile_2dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
							   'for textual data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_binaryfile_2dc32

	module procedure from_file_3dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_3dc128
	module procedure from_file_3dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_3dc64
	module procedure from_file_3dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_3dc32

	module procedure from_file_4dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_4dc128
	module procedure from_file_4dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_4dc64
	module procedure from_file_4dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_4dc32

	module procedure from_file_5dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_5dc128
	module procedure from_file_5dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_5dc64
	module procedure from_file_5dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_5dc32

	module procedure from_file_6dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_6dc128
	module procedure from_file_6dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_6dc64
	module procedure from_file_6dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_6dc32

	module procedure from_file_7dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_7dc128
	module procedure from_file_7dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_7dc64
	module procedure from_file_7dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_7dc32

	module procedure from_file_8dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_8dc128
	module procedure from_file_8dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_8dc64
	module procedure from_file_8dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_8dc32

	module procedure from_file_9dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_9dc128
	module procedure from_file_9dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_9dc64
	module procedure from_file_9dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_9dc32

	module procedure from_file_10dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_10dc128
	module procedure from_file_10dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_10dc64
	module procedure from_file_10dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_10dc32

	module procedure from_file_11dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_11dc128
	module procedure from_file_11dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_11dc64
	module procedure from_file_11dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_11dc32

	module procedure from_file_12dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_12dc128
	module procedure from_file_12dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_12dc64
	module procedure from_file_12dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_12dc32

	module procedure from_file_13dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_13dc128
	module procedure from_file_13dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_13dc64
	module procedure from_file_13dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_13dc32

	module procedure from_file_14dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_14dc128
	module procedure from_file_14dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_14dc64
	module procedure from_file_14dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_14dc32

	module procedure from_file_15dc128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_15dc128
	module procedure from_file_15dc64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_15dc64
	module procedure from_file_15dc32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_15dc32

	module procedure from_textfile_1dr128
		character(len=:), allocatable :: ext, locale_, fmt_
		logical :: header_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = .false.
			else
				header_ = header
			end if

			if ( .not. present(locale) ) then
				locale_ = 'US'
			else
				if ( any(LOCALES == locale) ) then
					locale_ = locale
				else
					error stop LF//'FATAL: Invalid locale "'//locale//'" for read of file "'//file_name//'".'// &
							   LF//'Locale must be one of: '//glue(LOCALES)
				end if
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'e'
			else
				if ( any(REAL_FMTS == fmt) ) then
					fmt_ = fmt
				else
					error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file_name//'" '// &
								   'into real array.'// &
							   LF//'Format must be one of: '//glue(REAL_FMTS)
				end if
			end if

			call from_text(file_name=file_name, into=into, header=header_, locale=locale_, fmt=fmt_)
		else
			if ( any(BINARY_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
							   'for binary data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_textfile_1dr128
	module procedure from_binaryfile_1dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
							   'for textual data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_binaryfile_1dr128
	module procedure from_textfile_1dr64
		character(len=:), allocatable :: ext, locale_, fmt_
		logical :: header_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = .false.
			else
				header_ = header
			end if

			if ( .not. present(locale) ) then
				locale_ = 'US'
			else
				if ( any(LOCALES == locale) ) then
					locale_ = locale
				else
					error stop LF//'FATAL: Invalid locale "'//locale//'" for read of file "'//file_name//'".'// &
							   LF//'Locale must be one of: '//glue(LOCALES)
				end if
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'e'
			else
				if ( any(REAL_FMTS == fmt) ) then
					fmt_ = fmt
				else
					error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file_name//'" '// &
								   'into real array.'// &
							   LF//'Format must be one of: '//glue(REAL_FMTS)
				end if
			end if

			call from_text(file_name=file_name, into=into, header=header_, locale=locale_, fmt=fmt_)
		else
			if ( any(BINARY_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
							   'for binary data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_textfile_1dr64
	module procedure from_binaryfile_1dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
							   'for textual data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_binaryfile_1dr64
	module procedure from_textfile_1dr32
		character(len=:), allocatable :: ext, locale_, fmt_
		logical :: header_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = .false.
			else
				header_ = header
			end if

			if ( .not. present(locale) ) then
				locale_ = 'US'
			else
				if ( any(LOCALES == locale) ) then
					locale_ = locale
				else
					error stop LF//'FATAL: Invalid locale "'//locale//'" for read of file "'//file_name//'".'// &
							   LF//'Locale must be one of: '//glue(LOCALES)
				end if
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'e'
			else
				if ( any(REAL_FMTS == fmt) ) then
					fmt_ = fmt
				else
					error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file_name//'" '// &
								   'into real array.'// &
							   LF//'Format must be one of: '//glue(REAL_FMTS)
				end if
			end if

			call from_text(file_name=file_name, into=into, header=header_, locale=locale_, fmt=fmt_)
		else
			if ( any(BINARY_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
							   'for binary data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_textfile_1dr32
	module procedure from_binaryfile_1dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
							   'for textual data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_binaryfile_1dr32

	module procedure from_textfile_2dr128
		character(len=:), allocatable :: ext, locale_, fmt_
		logical :: header_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = .false.
			else
				header_ = header
			end if

			if ( .not. present(locale) ) then
				locale_ = 'US'
			else
				if ( any(LOCALES == locale) ) then
					locale_ = locale
				else
					error stop LF//'FATAL: Invalid locale "'//locale//'" for read of file "'//file_name//'".'// &
							   LF//'Locale must be one of: '//glue(LOCALES)
				end if
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'e'
			else
				if ( any(REAL_FMTS == fmt) ) then
					fmt_ = fmt
				else
					error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file_name//'" '// &
								   'into real array.'// &
							   LF//'Format must be one of: '//glue(REAL_FMTS)
				end if
			end if

			call from_text(file_name=file_name, into=into, header=header_, locale=locale_, fmt=fmt_)
		else
			if ( any(BINARY_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
							   'for binary data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_textfile_2dr128
	module procedure from_binaryfile_2dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
							   'for textual data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_binaryfile_2dr128
	module procedure from_textfile_2dr64
		character(len=:), allocatable :: ext, locale_, fmt_
		logical :: header_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = .false.
			else
				header_ = header
			end if

			if ( .not. present(locale) ) then
				locale_ = 'US'
			else
				if ( any(LOCALES == locale) ) then
					locale_ = locale
				else
					error stop LF//'FATAL: Invalid locale "'//locale//'" for read of file "'//file_name//'".'// &
							   LF//'Locale must be one of: '//glue(LOCALES)
				end if
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'e'
			else
				if ( any(REAL_FMTS == fmt) ) then
					fmt_ = fmt
				else
					error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file_name//'" '// &
								   'into real array.'// &
							   LF//'Format must be one of: '//glue(REAL_FMTS)
				end if
			end if

			call from_text(file_name=file_name, into=into, header=header_, locale=locale_, fmt=fmt_)
		else
			if ( any(BINARY_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
							   'for binary data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_textfile_2dr64
	module procedure from_binaryfile_2dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
							   'for textual data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_binaryfile_2dr64
	module procedure from_textfile_2dr32
		character(len=:), allocatable :: ext, locale_, fmt_
		logical :: header_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = .false.
			else
				header_ = header
			end if

			if ( .not. present(locale) ) then
				locale_ = 'US'
			else
				if ( any(LOCALES == locale) ) then
					locale_ = locale
				else
					error stop LF//'FATAL: Invalid locale "'//locale//'" for read of file "'//file_name//'".'// &
							   LF//'Locale must be one of: '//glue(LOCALES)
				end if
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'e'
			else
				if ( any(REAL_FMTS == fmt) ) then
					fmt_ = fmt
				else
					error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file_name//'" '// &
								   'into real array.'// &
							   LF//'Format must be one of: '//glue(REAL_FMTS)
				end if
			end if

			call from_text(file_name=file_name, into=into, header=header_, locale=locale_, fmt=fmt_)
		else
			if ( any(BINARY_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
							   'for binary data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_textfile_2dr32
	module procedure from_binaryfile_2dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
							   'for textual data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_binaryfile_2dr32

	module procedure from_file_3dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_3dr128
	module procedure from_file_3dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_3dr64
	module procedure from_file_3dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_3dr32

	module procedure from_file_4dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_4dr128
	module procedure from_file_4dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_4dr64
	module procedure from_file_4dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_4dr32

	module procedure from_file_5dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_5dr128
	module procedure from_file_5dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_5dr64
	module procedure from_file_5dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_5dr32

	module procedure from_file_6dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_6dr128
	module procedure from_file_6dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_6dr64
	module procedure from_file_6dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_6dr32

	module procedure from_file_7dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_7dr128
	module procedure from_file_7dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_7dr64
	module procedure from_file_7dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_7dr32

	module procedure from_file_8dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_8dr128
	module procedure from_file_8dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_8dr64
	module procedure from_file_8dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_8dr32

	module procedure from_file_9dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_9dr128
	module procedure from_file_9dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_9dr64
	module procedure from_file_9dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_9dr32

	module procedure from_file_10dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_10dr128
	module procedure from_file_10dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_10dr64
	module procedure from_file_10dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_10dr32

	module procedure from_file_11dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_11dr128
	module procedure from_file_11dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_11dr64
	module procedure from_file_11dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_11dr32

	module procedure from_file_12dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_12dr128
	module procedure from_file_12dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_12dr64
	module procedure from_file_12dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_12dr32

	module procedure from_file_13dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_13dr128
	module procedure from_file_13dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_13dr64
	module procedure from_file_13dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_13dr32

	module procedure from_file_14dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_14dr128
	module procedure from_file_14dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_14dr64
	module procedure from_file_14dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_14dr32

	module procedure from_file_15dr128
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_15dr128
	module procedure from_file_15dr64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_15dr64
	module procedure from_file_15dr32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_15dr32

	module procedure from_textfile_1di64
		character(len=:), allocatable :: ext, fmt_
		logical :: header_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = .false.
			else
				header_ = header
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'i'
			else
				if ( any(INT_FMTS == fmt) ) then
					fmt_ = fmt
				else
					error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file_name//'" '// &
								   'into integer array.'// &
							   LF//'Format must be one of: '//glue(INT_FMTS)
				end if
			end if

			call from_text(file_name=file_name, into=into, header=header_, fmt=fmt_)
		else
			if ( any(BINARY_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
							   'for binary data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_textfile_1di64
	module procedure from_binaryfile_1di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
							   'for textual data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_binaryfile_1di64
	module procedure from_textfile_1di32
		character(len=:), allocatable :: ext, fmt_
		logical :: header_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = .false.
			else
				header_ = header
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'i'
			else
				if ( any(INT_FMTS == fmt) ) then
					fmt_ = fmt
				else
					error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file_name//'" '// &
								   'into integer array.'// &
							   LF//'Format must be one of: '//glue(INT_FMTS)
				end if
			end if

			call from_text(file_name=file_name, into=into, header=header_, fmt=fmt_)
		else
			if ( any(BINARY_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
							   'for binary data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_textfile_1di32
	module procedure from_binaryfile_1di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
							   'for textual data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_binaryfile_1di32
	module procedure from_textfile_1di16
		character(len=:), allocatable :: ext, fmt_
		logical :: header_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = .false.
			else
				header_ = header
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'i'
			else
				if ( any(INT_FMTS == fmt) ) then
					fmt_ = fmt
				else
					error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file_name//'" '// &
								   'into integer array.'// &
							   LF//'Format must be one of: '//glue(INT_FMTS)
				end if
			end if

			call from_text(file_name=file_name, into=into, header=header_, fmt=fmt_)
		else
			if ( any(BINARY_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
							   'for binary data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_textfile_1di16
	module procedure from_binaryfile_1di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
							   'for textual data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_binaryfile_1di16
	module procedure from_textfile_1di8
		character(len=:), allocatable :: ext, fmt_
		logical :: header_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = .false.
			else
				header_ = header
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'i'
			else
				if ( any(INT_FMTS == fmt) ) then
					fmt_ = fmt
				else
					error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file_name//'" '// &
								   'into integer array.'// &
							   LF//'Format must be one of: '//glue(INT_FMTS)
				end if
			end if

			call from_text(file_name=file_name, into=into, header=header_, fmt=fmt_)
		else
			if ( any(BINARY_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
							   'for binary data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_textfile_1di8
	module procedure from_binaryfile_1di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
							   'for textual data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_binaryfile_1di8

	module procedure from_textfile_2di64
		character(len=:), allocatable :: ext, fmt_
		logical :: header_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = .false.
			else
				header_ = header
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'i'
			else
				if ( any(INT_FMTS == fmt) ) then
					fmt_ = fmt
				else
					error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file_name//'" '// &
								   'into integer array.'// &
							   LF//'Format must be one of: '//glue(INT_FMTS)
				end if
			end if

			call from_text(file_name=file_name, into=into, header=header_, fmt=fmt_)
		else
			if ( any(BINARY_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
							   'for binary data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_textfile_2di64
	module procedure from_binaryfile_2di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
							   'for textual data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_binaryfile_2di64
	module procedure from_textfile_2di32
		character(len=:), allocatable :: ext, fmt_
		logical :: header_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = .false.
			else
				header_ = header
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'i'
			else
				if ( any(INT_FMTS == fmt) ) then
					fmt_ = fmt
				else
					error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file_name//'" '// &
								   'into integer array.'// &
							   LF//'Format must be one of: '//glue(INT_FMTS)
				end if
			end if

			call from_text(file_name=file_name, into=into, header=header_, fmt=fmt_)
		else
			if ( any(BINARY_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
							   'for binary data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_textfile_2di32
	module procedure from_binaryfile_2di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
							   'for textual data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_binaryfile_2di32
	module procedure from_textfile_2di16
		character(len=:), allocatable :: ext, fmt_
		logical :: header_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = .false.
			else
				header_ = header
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'i'
			else
				if ( any(INT_FMTS == fmt) ) then
					fmt_ = fmt
				else
					error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file_name//'" '// &
								   'into integer array.'// &
							   LF//'Format must be one of: '//glue(INT_FMTS)
				end if
			end if

			call from_text(file_name=file_name, into=into, header=header_, fmt=fmt_)
		else
			if ( any(BINARY_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
							   'for binary data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_textfile_2di16
	module procedure from_binaryfile_2di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
							   'for textual data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_binaryfile_2di16
	module procedure from_textfile_2di8
		character(len=:), allocatable :: ext, fmt_
		logical :: header_

		ext = ext_of(file_name)

		if ( any(TEXT_EXT == ext) ) then
			if ( .not. present(header) ) then
				header_ = .false.
			else
				header_ = header
			end if

			if ( .not. present(fmt) ) then
				fmt_ = 'i'
			else
				if ( any(INT_FMTS == fmt) ) then
					fmt_ = fmt
				else
					error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file_name//'" '// &
								   'into integer array.'// &
							   LF//'Format must be one of: '//glue(INT_FMTS)
				end if
			end if

			call from_text(file_name=file_name, into=into, header=header_, fmt=fmt_)
		else
			if ( any(BINARY_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
							   'for binary data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_textfile_2di8
	module procedure from_binaryfile_2di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
							   'for textual data.'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(TEXT_EXT)//SPACE// &
						   glue(BINARY_EXT)
			end if
		end if
	end procedure from_binaryfile_2di8

	module procedure from_file_3di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_3di64
	module procedure from_file_3di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_3di32
	module procedure from_file_3di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_3di16
	module procedure from_file_3di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_3di8

	module procedure from_file_4di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_4di64
	module procedure from_file_4di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_4di32
	module procedure from_file_4di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_4di16
	module procedure from_file_4di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_4di8

	module procedure from_file_5di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_5di64
	module procedure from_file_5di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_5di32
	module procedure from_file_5di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_5di16
	module procedure from_file_5di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_5di8

	module procedure from_file_6di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_6di64
	module procedure from_file_6di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_6di32
	module procedure from_file_6di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_6di16
	module procedure from_file_6di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_6di8

	module procedure from_file_7di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_7di64
	module procedure from_file_7di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_7di32
	module procedure from_file_7di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_7di16
	module procedure from_file_7di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_7di8

	module procedure from_file_8di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_8di64
	module procedure from_file_8di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_8di32
	module procedure from_file_8di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_8di16
	module procedure from_file_8di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_8di8

	module procedure from_file_9di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_9di64
	module procedure from_file_9di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_9di32
	module procedure from_file_9di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_9di16
	module procedure from_file_9di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_9di8

	module procedure from_file_10di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_10di64
	module procedure from_file_10di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_10di32
	module procedure from_file_10di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_10di16
	module procedure from_file_10di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_10di8

	module procedure from_file_11di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_11di64
	module procedure from_file_11di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_11di32
	module procedure from_file_11di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_11di16
	module procedure from_file_11di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_11di8

	module procedure from_file_12di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_12di64
	module procedure from_file_12di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_12di32
	module procedure from_file_12di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_12di16
	module procedure from_file_12di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_12di8

	module procedure from_file_13di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_13di64
	module procedure from_file_13di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_13di32
	module procedure from_file_13di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_13di16
	module procedure from_file_13di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_13di8

	module procedure from_file_14di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_14di64
	module procedure from_file_14di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_14di32
	module procedure from_file_14di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_14di16
	module procedure from_file_14di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_14di8

	module procedure from_file_15di64
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_15di64
	module procedure from_file_15di32
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_15di32
	module procedure from_file_15di16
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_15di16
	module procedure from_file_15di8
		character(len=:), allocatable :: ext

		ext = ext_of(file_name)

		if ( any(BINARY_EXT == ext) ) then
			if ( size(data_shape) /= rank(into) ) then
				error stop LF//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
						   LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
							 //str(size(data_shape))//'). These must match.'
			end if

			call from_binary(file_name=file_name, into=into, data_shape=data_shape)
		else
			if ( any(TEXT_EXT == ext) ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
							   'arrays of dimension greater than (2).'
			else
				error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
						   LF//'Supported file extensions: '//glue(BINARY_EXT)
			end if
		end if
	end procedure from_file_15di8
end submodule file_io

submodule (io_fortran_lib) text_io
	!! This submodule provides module procedure implementations for the **public interface** `echo` and the **private
	!! interfaces** `to_text` and `from_text`.
	contains
	! Writing Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	module procedure echo_chars
		character(len=:), allocatable :: ext, terminator_
		logical :: exists, append_
		integer :: file_unit

		ext = ext_of(file_name)

		if ( .not. any(TEXT_EXT == ext) ) then
			write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
								'due to unsupported file extension "'//ext//'".'// &
							LF//'Supported file extensions: '//glue(TEXT_EXT)
			return
		end if

		if ( len(substring) == 0 ) then
			write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". '// &
								'String to write is empty.'
			return
		end if

		if ( .not. present(append) ) then
			append_ = .true.
		else
			append_ = append
		end if

		if ( .not. present(terminator) ) then
			terminator_ = LF
		else
			terminator_ = terminator
		end if

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream', position='rewind' )
		else
			if ( .not. append_ ) then
				open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
					  action='write', access='stream', position='rewind' )
			else
				open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
					  action='write', access='stream', position='append' )
			end if
		end if

		write( unit=file_unit ) substring//terminator_

		close(file_unit)
	end procedure echo_chars

	module procedure echo_string
		character(len=:), allocatable :: ext, terminator_
		logical :: exists, append_
		integer :: file_unit

		ext = ext_of(file_name)

		if ( .not. any(TEXT_EXT == ext) ) then
			write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'" '// &
								'due to unsupported file extension "'//ext//'".'// &
							LF//'Supported file extensions: '//glue(TEXT_EXT)
			return
		end if

		if ( substring%len() < 1 ) then
			write(*,'(a)')  LF//'WARNING: Skipping write to "'//file_name//'". '// &
								'String to write is empty.'
			return
		end if

		if ( .not. present(append) ) then
			append_ = .true.
		else
			append_ = append
		end if

		if ( .not. present(terminator) ) then
			terminator_ = LF
		else
			terminator_ = terminator
		end if

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream', position='rewind' )
		else
			if ( .not. append_ ) then
				open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
					  action='write', access='stream', position='rewind' )
			else
				open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
					  action='write', access='stream', position='append' )
			end if
		end if

		write( unit=file_unit ) substring%s//terminator_

		close(file_unit)
	end procedure echo_string

	module procedure to_text_1dc128
		type(String) :: text_file
		type(String), allocatable, dimension(:,:) :: cells
		character(len=:), allocatable :: label
		integer :: nx, j
		logical :: header_present

		nx = size(x)
		header_present = .false.

		if ( size(header) == 1 ) then
			if ( all(header == EMPTY_STR) ) then
				if ( dim == 1 ) then
					allocate( cells(nx,1) )
				else
					allocate( cells(1,nx) )
				end if
			else
				header_present = .true.
				if ( dim == 1 ) then
					allocate( cells(nx+1,1) )
					cells(1,1) = String( trim(adjustl(header(1))) )
				else
					allocate( cells(2,nx) )
					label = trim(adjustl(header(1)))
					do concurrent (j = lbound(x, dim=1):ubound(x, dim=1))
						cells(1,j) = String(label//str(j))
					end do
				end if
			end if
		else
			header_present = .true.
			allocate( cells(2,nx) )
			cells(1,:) = String(header)
		end if

		if ( header_present ) then
			if ( dim == 1 ) then
				cells(2:,1) = String(x, locale=locale, fmt=fmt, decimals=decimals, im=im)
			else
				cells(2,:) = String(x, locale=locale, fmt=fmt, decimals=decimals, im=im)
			end if
		else
			if ( dim == 1 ) then
				cells(:,1) = String(x, locale=locale, fmt=fmt, decimals=decimals, im=im)
			else
				cells(1,:) = String(x, locale=locale, fmt=fmt, decimals=decimals, im=im)
			end if
		end if

		call text_file%write_file(cells, file_name=file_name, row_separator=NL, column_separator=delim)
	end procedure to_text_1dc128
	module procedure to_text_1dc64
		type(String) :: text_file
		type(String), allocatable, dimension(:,:) :: cells
		character(len=:), allocatable :: label
		integer :: nx, j
		logical :: header_present

		nx = size(x)
		header_present = .false.

		if ( size(header) == 1 ) then
			if ( all(header == EMPTY_STR) ) then
				if ( dim == 1 ) then
					allocate( cells(nx,1) )
				else
					allocate( cells(1,nx) )
				end if
			else
				header_present = .true.
				if ( dim == 1 ) then
					allocate( cells(nx+1,1) )
					cells(1,1) = String( trim(adjustl(header(1))) )
				else
					allocate( cells(2,nx) )
					label = trim(adjustl(header(1)))
					do concurrent (j = lbound(x, dim=1):ubound(x, dim=1))
						cells(1,j) = String(label//str(j))
					end do
				end if
			end if
		else
			header_present = .true.
			allocate( cells(2,nx) )
			cells(1,:) = String(header)
		end if

		if ( header_present ) then
			if ( dim == 1 ) then
				cells(2:,1) = String(x, locale=locale, fmt=fmt, decimals=decimals, im=im)
			else
				cells(2,:) = String(x, locale=locale, fmt=fmt, decimals=decimals, im=im)
			end if
		else
			if ( dim == 1 ) then
				cells(:,1) = String(x, locale=locale, fmt=fmt, decimals=decimals, im=im)
			else
				cells(1,:) = String(x, locale=locale, fmt=fmt, decimals=decimals, im=im)
			end if
		end if

		call text_file%write_file(cells, file_name=file_name, row_separator=NL, column_separator=delim)
	end procedure to_text_1dc64
	module procedure to_text_1dc32
		type(String) :: text_file
		type(String), allocatable, dimension(:,:) :: cells
		character(len=:), allocatable :: label
		integer :: nx, j
		logical :: header_present

		nx = size(x)
		header_present = .false.

		if ( size(header) == 1 ) then
			if ( all(header == EMPTY_STR) ) then
				if ( dim == 1 ) then
					allocate( cells(nx,1) )
				else
					allocate( cells(1,nx) )
				end if
			else
				header_present = .true.
				if ( dim == 1 ) then
					allocate( cells(nx+1,1) )
					cells(1,1) = String( trim(adjustl(header(1))) )
				else
					allocate( cells(2,nx) )
					label = trim(adjustl(header(1)))
					do concurrent (j = lbound(x, dim=1):ubound(x, dim=1))
						cells(1,j) = String(label//str(j))
					end do
				end if
			end if
		else
			header_present = .true.
			allocate( cells(2,nx) )
			cells(1,:) = String(header)
		end if

		if ( header_present ) then
			if ( dim == 1 ) then
				cells(2:,1) = String(x, locale=locale, fmt=fmt, decimals=decimals, im=im)
			else
				cells(2,:) = String(x, locale=locale, fmt=fmt, decimals=decimals, im=im)
			end if
		else
			if ( dim == 1 ) then
				cells(:,1) = String(x, locale=locale, fmt=fmt, decimals=decimals, im=im)
			else
				cells(1,:) = String(x, locale=locale, fmt=fmt, decimals=decimals, im=im)
			end if
		end if

		call text_file%write_file(cells, file_name=file_name, row_separator=NL, column_separator=delim)
	end procedure to_text_1dc32

	module procedure to_text_2dc128
		type(String) :: text_file
		type(String), allocatable, dimension(:,:) :: cells
		character(len=:), allocatable :: label
		integer :: nrows, ncols, j
		logical :: header_present

		nrows = size(x, dim=1)
		ncols = size(x, dim=2)
		header_present = .false.

		if ( size(header) == 1 ) then
			if ( all(header == EMPTY_STR) ) then
				allocate( cells(nrows,ncols) )
			else
				header_present = .true.
				allocate( cells(nrows+1,ncols) )
				label = trim(adjustl(header(1)))
				do concurrent (j = lbound(x, dim=2):ubound(x, dim=2))
					cells(1,j) = String(label//str(j))
				end do
			end if
		else
			header_present = .true.
			allocate( cells(nrows+1,ncols) )
			cells(1,:) = String(header)
		end if

		if ( header_present ) then
			cells(2:,:) = String(x, locale=locale, fmt=fmt, decimals=decimals, im=im)
		else
			cells = String(x, locale=locale, fmt=fmt, decimals=decimals, im=im)
		end if

		call text_file%write_file(cells, file_name=file_name, row_separator=NL, column_separator=delim)
	end procedure to_text_2dc128
	module procedure to_text_2dc64
		type(String) :: text_file
		type(String), allocatable, dimension(:,:) :: cells
		character(len=:), allocatable :: label
		integer :: nrows, ncols, j
		logical :: header_present

		nrows = size(x, dim=1)
		ncols = size(x, dim=2)
		header_present = .false.

		if ( size(header) == 1 ) then
			if ( all(header == EMPTY_STR) ) then
				allocate( cells(nrows,ncols) )
			else
				header_present = .true.
				allocate( cells(nrows+1,ncols) )
				label = trim(adjustl(header(1)))
				do concurrent (j = lbound(x, dim=2):ubound(x, dim=2))
					cells(1,j) = String(label//str(j))
				end do
			end if
		else
			header_present = .true.
			allocate( cells(nrows+1,ncols) )
			cells(1,:) = String(header)
		end if

		if ( header_present ) then
			cells(2:,:) = String(x, locale=locale, fmt=fmt, decimals=decimals, im=im)
		else
			cells = String(x, locale=locale, fmt=fmt, decimals=decimals, im=im)
		end if

		call text_file%write_file(cells, file_name=file_name, row_separator=NL, column_separator=delim)
	end procedure to_text_2dc64
	module procedure to_text_2dc32
		type(String) :: text_file
		type(String), allocatable, dimension(:,:) :: cells
		character(len=:), allocatable :: label
		integer :: nrows, ncols, j
		logical :: header_present

		nrows = size(x, dim=1)
		ncols = size(x, dim=2)
		header_present = .false.

		if ( size(header) == 1 ) then
			if ( all(header == EMPTY_STR) ) then
				allocate( cells(nrows,ncols) )
			else
				header_present = .true.
				allocate( cells(nrows+1,ncols) )
				label = trim(adjustl(header(1)))
				do concurrent (j = lbound(x, dim=2):ubound(x, dim=2))
					cells(1,j) = String(label//str(j))
				end do
			end if
		else
			header_present = .true.
			allocate( cells(nrows+1,ncols) )
			cells(1,:) = String(header)
		end if

		if ( header_present ) then
			cells(2:,:) = String(x, locale=locale, fmt=fmt, decimals=decimals, im=im)
		else
			cells = String(x, locale=locale, fmt=fmt, decimals=decimals, im=im)
		end if

		call text_file%write_file(cells, file_name=file_name, row_separator=NL, column_separator=delim)
	end procedure to_text_2dc32

	module procedure to_text_1dr128
		type(String) :: text_file
		type(String), allocatable, dimension(:,:) :: cells
		character(len=:), allocatable :: label
		integer :: nx, j
		logical :: header_present

		nx = size(x)
		header_present = .false.

		if ( size(header) == 1 ) then
			if ( all(header == EMPTY_STR) ) then
				if ( dim == 1 ) then
					allocate( cells(nx,1) )
				else
					allocate( cells(1,nx) )
				end if
			else
				header_present = .true.
				if ( dim == 1 ) then
					allocate( cells(nx+1,1) )
					cells(1,1) = String( trim(adjustl(header(1))) )
				else
					allocate( cells(2,nx) )
					label = trim(adjustl(header(1)))
					do concurrent (j = lbound(x, dim=1):ubound(x, dim=1))
						cells(1,j) = String(label//str(j))
					end do
				end if
			end if
		else
			header_present = .true.
			allocate( cells(2,nx) )
			cells(1,:) = String(header)
		end if

		if ( header_present ) then
			if ( dim == 1 ) then
				cells(2:,1) = String(x, locale=locale, fmt=fmt, decimals=decimals)
			else
				cells(2,:) = String(x, locale=locale, fmt=fmt, decimals=decimals)
			end if
		else
			if ( dim == 1 ) then
				cells(:,1) = String(x, locale=locale, fmt=fmt, decimals=decimals)
			else
				cells(1,:) = String(x, locale=locale, fmt=fmt, decimals=decimals)
			end if
		end if

		call text_file%write_file(cells, file_name=file_name, row_separator=NL, column_separator=delim)
	end procedure to_text_1dr128
	module procedure to_text_1dr64
		type(String) :: text_file
		type(String), allocatable, dimension(:,:) :: cells
		character(len=:), allocatable :: label
		integer :: nx, j
		logical :: header_present

		nx = size(x)
		header_present = .false.

		if ( size(header) == 1 ) then
			if ( all(header == EMPTY_STR) ) then
				if ( dim == 1 ) then
					allocate( cells(nx,1) )
				else
					allocate( cells(1,nx) )
				end if
			else
				header_present = .true.
				if ( dim == 1 ) then
					allocate( cells(nx+1,1) )
					cells(1,1) = String( trim(adjustl(header(1))) )
				else
					allocate( cells(2,nx) )
					label = trim(adjustl(header(1)))
					do concurrent (j = lbound(x, dim=1):ubound(x, dim=1))
						cells(1,j) = String(label//str(j))
					end do
				end if
			end if
		else
			header_present = .true.
			allocate( cells(2,nx) )
			cells(1,:) = String(header)
		end if

		if ( header_present ) then
			if ( dim == 1 ) then
				cells(2:,1) = String(x, locale=locale, fmt=fmt, decimals=decimals)
			else
				cells(2,:) = String(x, locale=locale, fmt=fmt, decimals=decimals)
			end if
		else
			if ( dim == 1 ) then
				cells(:,1) = String(x, locale=locale, fmt=fmt, decimals=decimals)
			else
				cells(1,:) = String(x, locale=locale, fmt=fmt, decimals=decimals)
			end if
		end if

		call text_file%write_file(cells, file_name=file_name, row_separator=NL, column_separator=delim)
	end procedure to_text_1dr64
	module procedure to_text_1dr32
		type(String) :: text_file
		type(String), allocatable, dimension(:,:) :: cells
		character(len=:), allocatable :: label
		integer :: nx, j
		logical :: header_present

		nx = size(x)
		header_present = .false.

		if ( size(header) == 1 ) then
			if ( all(header == EMPTY_STR) ) then
				if ( dim == 1 ) then
					allocate( cells(nx,1) )
				else
					allocate( cells(1,nx) )
				end if
			else
				header_present = .true.
				if ( dim == 1 ) then
					allocate( cells(nx+1,1) )
					cells(1,1) = String( trim(adjustl(header(1))) )
				else
					allocate( cells(2,nx) )
					label = trim(adjustl(header(1)))
					do concurrent (j = lbound(x, dim=1):ubound(x, dim=1))
						cells(1,j) = String(label//str(j))
					end do
				end if
			end if
		else
			header_present = .true.
			allocate( cells(2,nx) )
			cells(1,:) = String(header)
		end if

		if ( header_present ) then
			if ( dim == 1 ) then
				cells(2:,1) = String(x, locale=locale, fmt=fmt, decimals=decimals)
			else
				cells(2,:) = String(x, locale=locale, fmt=fmt, decimals=decimals)
			end if
		else
			if ( dim == 1 ) then
				cells(:,1) = String(x, locale=locale, fmt=fmt, decimals=decimals)
			else
				cells(1,:) = String(x, locale=locale, fmt=fmt, decimals=decimals)
			end if
		end if

		call text_file%write_file(cells, file_name=file_name, row_separator=NL, column_separator=delim)
	end procedure to_text_1dr32

	module procedure to_text_2dr128
		type(String) :: text_file
		type(String), allocatable, dimension(:,:) :: cells
		character(len=:), allocatable :: label
		integer :: nrows, ncols, j
		logical :: header_present

		nrows = size(x, dim=1)
		ncols = size(x, dim=2)
		header_present = .false.

		if ( size(header) == 1 ) then
			if ( all(header == EMPTY_STR) ) then
				allocate( cells(nrows,ncols) )
			else
				header_present = .true.
				allocate( cells(nrows+1,ncols) )
				label = trim(adjustl(header(1)))
				do concurrent (j = lbound(x, dim=2):ubound(x, dim=2))
					cells(1,j) = String(label//str(j))
				end do
			end if
		else
			header_present = .true.
			allocate( cells(nrows+1,ncols) )
			cells(1,:) = String(header)
		end if

		if ( header_present ) then
			cells(2:,:) = String(x, locale=locale, fmt=fmt, decimals=decimals)
		else
			cells = String(x, locale=locale, fmt=fmt, decimals=decimals)
		end if

		call text_file%write_file(cells, file_name=file_name, row_separator=NL, column_separator=delim)
	end procedure to_text_2dr128
	module procedure to_text_2dr64
		type(String) :: text_file
		type(String), allocatable, dimension(:,:) :: cells
		character(len=:), allocatable :: label
		integer :: nrows, ncols, j
		logical :: header_present

		nrows = size(x, dim=1)
		ncols = size(x, dim=2)
		header_present = .false.

		if ( size(header) == 1 ) then
			if ( all(header == EMPTY_STR) ) then
				allocate( cells(nrows,ncols) )
			else
				header_present = .true.
				allocate( cells(nrows+1,ncols) )
				label = trim(adjustl(header(1)))
				do concurrent (j = lbound(x, dim=2):ubound(x, dim=2))
					cells(1,j) = String(label//str(j))
				end do
			end if
		else
			header_present = .true.
			allocate( cells(nrows+1,ncols) )
			cells(1,:) = String(header)
		end if

		if ( header_present ) then
			cells(2:,:) = String(x, locale=locale, fmt=fmt, decimals=decimals)
		else
			cells = String(x, locale=locale, fmt=fmt, decimals=decimals)
		end if

		call text_file%write_file(cells, file_name=file_name, row_separator=NL, column_separator=delim)
	end procedure to_text_2dr64
	module procedure to_text_2dr32
		type(String) :: text_file
		type(String), allocatable, dimension(:,:) :: cells
		character(len=:), allocatable :: label
		integer :: nrows, ncols, j
		logical :: header_present

		nrows = size(x, dim=1)
		ncols = size(x, dim=2)
		header_present = .false.

		if ( size(header) == 1 ) then
			if ( all(header == EMPTY_STR) ) then
				allocate( cells(nrows,ncols) )
			else
				header_present = .true.
				allocate( cells(nrows+1,ncols) )
				label = trim(adjustl(header(1)))
				do concurrent (j = lbound(x, dim=2):ubound(x, dim=2))
					cells(1,j) = String(label//str(j))
				end do
			end if
		else
			header_present = .true.
			allocate( cells(nrows+1,ncols) )
			cells(1,:) = String(header)
		end if

		if ( header_present ) then
			cells(2:,:) = String(x, locale=locale, fmt=fmt, decimals=decimals)
		else
			cells = String(x, locale=locale, fmt=fmt, decimals=decimals)
		end if

		call text_file%write_file(cells, file_name=file_name, row_separator=NL, column_separator=delim)
	end procedure to_text_2dr32

	module procedure to_text_1di64
		type(String) :: text_file
		type(String), allocatable, dimension(:,:) :: cells
		character(len=:), allocatable :: label
		integer :: nx, j
		logical :: header_present

		nx = size(x)
		header_present = .false.

		if ( size(header) == 1 ) then
			if ( all(header == EMPTY_STR) ) then
				if ( dim == 1 ) then
					allocate( cells(nx,1) )
				else
					allocate( cells(1,nx) )
				end if
			else
				header_present = .true.
				if ( dim == 1 ) then
					allocate( cells(nx+1,1) )
					cells(1,1) = String( trim(adjustl(header(1))) )
				else
					allocate( cells(2,nx) )
					label = trim(adjustl(header(1)))
					do concurrent (j = lbound(x, dim=1):ubound(x, dim=1))
						cells(1,j) = String(label//str(j))
					end do
				end if
			end if
		else
			header_present = .true.
			allocate( cells(2,nx) )
			cells(1,:) = String(header)
		end if

		if ( header_present ) then
			if ( dim == 1 ) then
				cells(2:,1) = String(x, fmt=fmt)
			else
				cells(2,:) = String(x, fmt=fmt)
			end if
		else
			if ( dim == 1 ) then
				cells(:,1) = String(x, fmt=fmt)
			else
				cells(1,:) = String(x, fmt=fmt)
			end if
		end if

		call text_file%write_file(cells, file_name=file_name, row_separator=NL, column_separator=delim)
	end procedure to_text_1di64
	module procedure to_text_1di32
		type(String) :: text_file
		type(String), allocatable, dimension(:,:) :: cells
		character(len=:), allocatable :: label
		integer :: nx, j
		logical :: header_present

		nx = size(x)
		header_present = .false.

		if ( size(header) == 1 ) then
			if ( all(header == EMPTY_STR) ) then
				if ( dim == 1 ) then
					allocate( cells(nx,1) )
				else
					allocate( cells(1,nx) )
				end if
			else
				header_present = .true.
				if ( dim == 1 ) then
					allocate( cells(nx+1,1) )
					cells(1,1) = String( trim(adjustl(header(1))) )
				else
					allocate( cells(2,nx) )
					label = trim(adjustl(header(1)))
					do concurrent (j = lbound(x, dim=1):ubound(x, dim=1))
						cells(1,j) = String(label//str(j))
					end do
				end if
			end if
		else
			header_present = .true.
			allocate( cells(2,nx) )
			cells(1,:) = String(header)
		end if

		if ( header_present ) then
			if ( dim == 1 ) then
				cells(2:,1) = String(x, fmt=fmt)
			else
				cells(2,:) = String(x, fmt=fmt)
			end if
		else
			if ( dim == 1 ) then
				cells(:,1) = String(x, fmt=fmt)
			else
				cells(1,:) = String(x, fmt=fmt)
			end if
		end if

		call text_file%write_file(cells, file_name=file_name, row_separator=NL, column_separator=delim)
	end procedure to_text_1di32
	module procedure to_text_1di16
		type(String) :: text_file
		type(String), allocatable, dimension(:,:) :: cells
		character(len=:), allocatable :: label
		integer :: nx, j
		logical :: header_present

		nx = size(x)
		header_present = .false.

		if ( size(header) == 1 ) then
			if ( all(header == EMPTY_STR) ) then
				if ( dim == 1 ) then
					allocate( cells(nx,1) )
				else
					allocate( cells(1,nx) )
				end if
			else
				header_present = .true.
				if ( dim == 1 ) then
					allocate( cells(nx+1,1) )
					cells(1,1) = String( trim(adjustl(header(1))) )
				else
					allocate( cells(2,nx) )
					label = trim(adjustl(header(1)))
					do concurrent (j = lbound(x, dim=1):ubound(x, dim=1))
						cells(1,j) = String(label//str(j))
					end do
				end if
			end if
		else
			header_present = .true.
			allocate( cells(2,nx) )
			cells(1,:) = String(header)
		end if

		if ( header_present ) then
			if ( dim == 1 ) then
				cells(2:,1) = String(x, fmt=fmt)
			else
				cells(2,:) = String(x, fmt=fmt)
			end if
		else
			if ( dim == 1 ) then
				cells(:,1) = String(x, fmt=fmt)
			else
				cells(1,:) = String(x, fmt=fmt)
			end if
		end if

		call text_file%write_file(cells, file_name=file_name, row_separator=NL, column_separator=delim)
	end procedure to_text_1di16
	module procedure to_text_1di8
		type(String) :: text_file
		type(String), allocatable, dimension(:,:) :: cells
		character(len=:), allocatable :: label
		integer :: nx, j
		logical :: header_present

		nx = size(x)
		header_present = .false.

		if ( size(header) == 1 ) then
			if ( all(header == EMPTY_STR) ) then
				if ( dim == 1 ) then
					allocate( cells(nx,1) )
				else
					allocate( cells(1,nx) )
				end if
			else
				header_present = .true.
				if ( dim == 1 ) then
					allocate( cells(nx+1,1) )
					cells(1,1) = String( trim(adjustl(header(1))) )
				else
					allocate( cells(2,nx) )
					label = trim(adjustl(header(1)))
					do concurrent (j = lbound(x, dim=1):ubound(x, dim=1))
						cells(1,j) = String(label//str(j))
					end do
				end if
			end if
		else
			header_present = .true.
			allocate( cells(2,nx) )
			cells(1,:) = String(header)
		end if

		if ( header_present ) then
			if ( dim == 1 ) then
				cells(2:,1) = String(x, fmt=fmt)
			else
				cells(2,:) = String(x, fmt=fmt)
			end if
		else
			if ( dim == 1 ) then
				cells(:,1) = String(x, fmt=fmt)
			else
				cells(1,:) = String(x, fmt=fmt)
			end if
		end if

		call text_file%write_file(cells, file_name=file_name, row_separator=NL, column_separator=delim)
	end procedure to_text_1di8

	module procedure to_text_2di64
		type(String) :: text_file
		type(String), allocatable, dimension(:,:) :: cells
		character(len=:), allocatable :: label
		integer :: nrows, ncols, j
		logical :: header_present

		nrows = size(x, dim=1)
		ncols = size(x, dim=2)
		header_present = .false.

		if ( size(header) == 1 ) then
			if ( all(header == EMPTY_STR) ) then
				allocate( cells(nrows,ncols) )
			else
				header_present = .true.
				allocate( cells(nrows+1,ncols) )
				label = trim(adjustl(header(1)))
				do concurrent (j = lbound(x, dim=2):ubound(x, dim=2))
					cells(1,j) = String(label//str(j))
				end do
			end if
		else
			header_present = .true.
			allocate( cells(nrows+1,ncols) )
			cells(1,:) = String(header)
		end if

		if ( header_present ) then
			cells(2:,:) = String(x, fmt=fmt)
		else
			cells = String(x, fmt=fmt)
		end if

		call text_file%write_file(cells, file_name=file_name, row_separator=NL, column_separator=delim)
	end procedure to_text_2di64
	module procedure to_text_2di32
		type(String) :: text_file
		type(String), allocatable, dimension(:,:) :: cells
		character(len=:), allocatable :: label
		integer :: nrows, ncols, j
		logical :: header_present

		nrows = size(x, dim=1)
		ncols = size(x, dim=2)
		header_present = .false.

		if ( size(header) == 1 ) then
			if ( all(header == EMPTY_STR) ) then
				allocate( cells(nrows,ncols) )
			else
				header_present = .true.
				allocate( cells(nrows+1,ncols) )
				label = trim(adjustl(header(1)))
				do concurrent (j = lbound(x, dim=2):ubound(x, dim=2))
					cells(1,j) = String(label//str(j))
				end do
			end if
		else
			header_present = .true.
			allocate( cells(nrows+1,ncols) )
			cells(1,:) = String(header)
		end if

		if ( header_present ) then
			cells(2:,:) = String(x, fmt=fmt)
		else
			cells = String(x, fmt=fmt)
		end if

		call text_file%write_file(cells, file_name=file_name, row_separator=NL, column_separator=delim)
	end procedure to_text_2di32
	module procedure to_text_2di16
		type(String) :: text_file
		type(String), allocatable, dimension(:,:) :: cells
		character(len=:), allocatable :: label
		integer :: nrows, ncols, j
		logical :: header_present

		nrows = size(x, dim=1)
		ncols = size(x, dim=2)
		header_present = .false.

		if ( size(header) == 1 ) then
			if ( all(header == EMPTY_STR) ) then
				allocate( cells(nrows,ncols) )
			else
				header_present = .true.
				allocate( cells(nrows+1,ncols) )
				label = trim(adjustl(header(1)))
				do concurrent (j = lbound(x, dim=2):ubound(x, dim=2))
					cells(1,j) = String(label//str(j))
				end do
			end if
		else
			header_present = .true.
			allocate( cells(nrows+1,ncols) )
			cells(1,:) = String(header)
		end if

		if ( header_present ) then
			cells(2:,:) = String(x, fmt=fmt)
		else
			cells = String(x, fmt=fmt)
		end if

		call text_file%write_file(cells, file_name=file_name, row_separator=NL, column_separator=delim)
	end procedure to_text_2di16
	module procedure to_text_2di8
		type(String) :: text_file
		type(String), allocatable, dimension(:,:) :: cells
		character(len=:), allocatable :: label
		integer :: nrows, ncols, j
		logical :: header_present

		nrows = size(x, dim=1)
		ncols = size(x, dim=2)
		header_present = .false.

		if ( size(header) == 1 ) then
			if ( all(header == EMPTY_STR) ) then
				allocate( cells(nrows,ncols) )
			else
				header_present = .true.
				allocate( cells(nrows+1,ncols) )
				label = trim(adjustl(header(1)))
				do concurrent (j = lbound(x, dim=2):ubound(x, dim=2))
					cells(1,j) = String(label//str(j))
				end do
			end if
		else
			header_present = .true.
			allocate( cells(nrows+1,ncols) )
			cells(1,:) = String(header)
		end if

		if ( header_present ) then
			cells(2:,:) = String(x, fmt=fmt)
		else
			cells = String(x, fmt=fmt)
		end if

		call text_file%write_file(cells, file_name=file_name, row_separator=NL, column_separator=delim)
	end procedure to_text_2di8

	! Reading Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	module procedure from_text_1dc128
		logical :: exists, ignore_sep
		integer :: file_unit, iostat
		integer :: n_rows, n_columns
		integer :: i, j, ind, l1, l2, sep_pos

		complex(real128) :: c
		character(len=:), allocatable, dimension(:) :: im_chars, non_separating_chars
		character(len=:), allocatable :: file, decimal, sep, number
		character(len=1) :: prev_char, current_char
		integer(int64) :: file_length

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		inquire( file=file_name, size=file_length )

		if ( file_length == 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty.'
			return
		end if

		allocate( character(len=file_length) :: file )
		read(unit=file_unit, iostat=iostat) file
		close(file_unit)

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		if ( header ) then
			do i = 1, file_length
				if ( file(i:i) == NL ) then
					file = file(i+1:)
					file_length = len(file)
					exit
				else if ( i == file_length ) then
					file = file//LF
					file_length = file_length + 1
					write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
									file_name//'". File has one line.'
				end if
			end do

			if ( file_length == 0 ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
				return
			end if
		end if

		n_rows = 0

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				n_rows = n_rows + 1
			else if ( i == file_length ) then
				file = file//LF
				file_length = file_length + 1
				n_rows = n_rows + 1
			end if
		end do

		if ( len(im) > 0 ) then
			allocate( character(len=1) :: im_chars(len(im)) )
			do i = 1, len(im)
				im_chars(i) = im(i:i)
			end do
		else
			im_chars = [ EMPTY_STR ]
		end if

		if ( fmt == 'z' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', &
									'a', 'b', 'c', 'd', 'e', 'f', '(', ')', '+']
		else
			if ( locale == 'US' ) then
				decimal = 'POINT'
				non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', 'e', 'E', '+', '-', &
										'(', ')']
			else if ( locale == 'EU' ) then
				decimal = 'COMMA'
				non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ',', 'e', 'E', '+', '-', &
										'(', ')']
			end if
		end if

		if ( locale == 'US' ) then
			sep = COMMA
		else if ( locale == 'EU' ) then
			sep = SEMICOLON
		end if

		ignore_sep = .false.
		prev_char = '0'
		n_columns = 0

		do i = 1, file_length
			current_char = file(i:i)
			if ( current_char == '(' ) then
				ignore_sep = .true.
			else if ( current_char == ')' ) then
				ignore_sep = .false.
			end if

			if ( any(non_separating_chars == current_char) .or. any(im_chars == current_char) ) then
				prev_char = current_char
			else
				if ( ignore_sep ) then
					prev_char = current_char
					cycle
				end if

				if ( any(non_separating_chars == prev_char) .or. any(im_chars == prev_char) ) then
					prev_char = current_char
					n_columns = n_columns + 1
				else
					prev_char = current_char
				end if
			end if

			if ( current_char == NL ) exit
		end do

		if ( (n_rows > 1) .and. (n_columns > 1) ) then
			error stop LF//'Error reading file "'//file_name//'". File data cannot fit into one-dimensional array.'
			return
		else if ( n_columns == 1 ) then
			allocate( into(n_rows) )
		else if ( n_rows == 1 ) then
			allocate( into(n_columns) )
		end if

		ignore_sep = .false.
		ind = 1
		l1 = 1
		l2 = 1
		i = 1

		read_into: do while ( i <= file_length )
			current_char = file(i:i)
			if ( current_char == '(' ) then
				ignore_sep = .true.
			else if ( current_char == ')' ) then
				ignore_sep = .false.
			end if

			if ( any(non_separating_chars == current_char) ) then
				if ( .not. any(non_separating_chars == file(l2:l2)) ) l1 = i
				l2 = i
			else if ( current_char == im_chars(1) ) then
				number = file(l1:i-1)
				do j = 2, len(number)
					if ( (number(j:j) == '+') .or. (number(j:j) == '-') ) then
						if ( fmt == 'e' ) then
							if ( (number(j-1:j-1) /= 'E') .and. (number(j-1:j-1) /= 'e') ) then
								sep_pos = j
								exit
							end if
						else
							sep_pos = j
							exit
						end if
					end if
				end do
				if ( fmt == 'z' ) then
					read(unit=number(:sep_pos-1), fmt='(z'//str(len(number(:sep_pos-1)))//')') c%re
					read(unit=number(sep_pos+1:), fmt='(z'//str(len(number(sep_pos+1:)))//')') c%im
				else
					read(unit=number(:sep_pos-1), fmt=*, decimal=decimal) c%re
					read(unit=number(sep_pos:), fmt=*, decimal=decimal) c%im
				end if
				into(ind) = c
				if ( ind /= size(into) ) then
					ind = ind + 1
				else
					exit read_into
				end if
				l2 = i
				i = i + size(im_chars)
				cycle read_into
			else
				if ( ignore_sep ) then
					l2 = i + 1
					i = i + 1
					cycle read_into
				end if

				if ( any(non_separating_chars == file(l2:l2)) ) then
					number = file(l1+1:l2-1)
					do j = 1, len(number)
						if ( number(j:j) == sep ) then
							sep_pos = j
							exit
						end if
					end do
					if ( fmt == 'z' ) then
						read(unit=number(:sep_pos-1), fmt='(z'//str(len(number(:sep_pos-1)))//')') c%re
						read(unit=number(sep_pos+1:), fmt='(z'//str(len(number(sep_pos+1:)))//')') c%im
					else
						read(unit=number(:sep_pos-1), fmt=*, decimal=decimal) c%re
						read(unit=number(sep_pos+1:), fmt=*, decimal=decimal) c%im
					end if
					into(ind) = c
					if ( ind /= size(into) ) then
						ind = ind + 1
					else
						exit read_into
					end if
					l2 = i
				else
					l2 = i
				end if
			end if
			i = i + 1
		end do read_into
	end procedure from_text_1dc128
	module procedure from_text_1dc64
		logical :: exists, ignore_sep
		integer :: file_unit, iostat
		integer :: n_rows, n_columns
		integer :: i, j, ind, l1, l2, sep_pos

		complex(real64) :: c
		character(len=:), allocatable, dimension(:) :: im_chars, non_separating_chars
		character(len=:), allocatable :: file, decimal, sep, number
		character(len=1) :: prev_char, current_char
		integer(int64) :: file_length

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		inquire( file=file_name, size=file_length )

		if ( file_length == 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty.'
			return
		end if

		allocate( character(len=file_length) :: file )
		read(unit=file_unit, iostat=iostat) file
		close(file_unit)

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		if ( header ) then
			do i = 1, file_length
				if ( file(i:i) == NL ) then
					file = file(i+1:)
					file_length = len(file)
					exit
				else if ( i == file_length ) then
					file = file//LF
					file_length = file_length + 1
					write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
									file_name//'". File has one line.'
				end if
			end do

			if ( file_length == 0 ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
				return
			end if
		end if

		n_rows = 0

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				n_rows = n_rows + 1
			else if ( i == file_length ) then
				file = file//LF
				file_length = file_length + 1
				n_rows = n_rows + 1
			end if
		end do

		if ( len(im) > 0 ) then
			allocate( character(len=1) :: im_chars(len(im)) )
			do i = 1, len(im)
				im_chars(i) = im(i:i)
			end do
		else
			im_chars = [ EMPTY_STR ]
		end if

		if ( fmt == 'z' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', &
									'a', 'b', 'c', 'd', 'e', 'f', '(', ')', '+']
		else
			if ( locale == 'US' ) then
				decimal = 'POINT'
				non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', 'e', 'E', '+', '-', &
										'(', ')']
			else if ( locale == 'EU' ) then
				decimal = 'COMMA'
				non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ',', 'e', 'E', '+', '-', &
										'(', ')']
			end if
		end if

		if ( locale == 'US' ) then
			sep = COMMA
		else if ( locale == 'EU' ) then
			sep = SEMICOLON
		end if

		ignore_sep = .false.
		prev_char = '0'
		n_columns = 0

		do i = 1, file_length
			current_char = file(i:i)
			if ( current_char == '(' ) then
				ignore_sep = .true.
			else if ( current_char == ')' ) then
				ignore_sep = .false.
			end if

			if ( any(non_separating_chars == current_char) .or. any(im_chars == current_char) ) then
				prev_char = current_char
			else
				if ( ignore_sep ) then
					prev_char = current_char
					cycle
				end if

				if ( any(non_separating_chars == prev_char) .or. any(im_chars == prev_char) ) then
					prev_char = current_char
					n_columns = n_columns + 1
				else
					prev_char = current_char
				end if
			end if

			if ( current_char == NL ) exit
		end do

		if ( (n_rows > 1) .and. (n_columns > 1) ) then
			error stop LF//'Error reading file "'//file_name//'". File data cannot fit into one-dimensional array.'
			return
		else if ( n_columns == 1 ) then
			allocate( into(n_rows) )
		else if ( n_rows == 1 ) then
			allocate( into(n_columns) )
		end if

		ignore_sep = .false.
		ind = 1
		l1 = 1
		l2 = 1
		i = 1

		read_into: do while ( i <= file_length )
			current_char = file(i:i)
			if ( current_char == '(' ) then
				ignore_sep = .true.
			else if ( current_char == ')' ) then
				ignore_sep = .false.
			end if

			if ( any(non_separating_chars == current_char) ) then
				if ( .not. any(non_separating_chars == file(l2:l2)) ) l1 = i
				l2 = i
			else if ( current_char == im_chars(1) ) then
				number = file(l1:i-1)
				do j = 2, len(number)
					if ( (number(j:j) == '+') .or. (number(j:j) == '-') ) then
						if ( fmt == 'e' ) then
							if ( (number(j-1:j-1) /= 'E') .and. (number(j-1:j-1) /= 'e') ) then
								sep_pos = j
								exit
							end if
						else
							sep_pos = j
							exit
						end if
					end if
				end do
				if ( fmt == 'z' ) then
					read(unit=number(:sep_pos-1), fmt='(z'//str(len(number(:sep_pos-1)))//')') c%re
					read(unit=number(sep_pos+1:), fmt='(z'//str(len(number(sep_pos+1:)))//')') c%im
				else
					read(unit=number(:sep_pos-1), fmt=*, decimal=decimal) c%re
					read(unit=number(sep_pos:), fmt=*, decimal=decimal) c%im
				end if
				into(ind) = c
				if ( ind /= size(into) ) then
					ind = ind + 1
				else
					exit read_into
				end if
				l2 = i
				i = i + size(im_chars)
				cycle read_into
			else
				if ( ignore_sep ) then
					l2 = i + 1
					i = i + 1
					cycle read_into
				end if

				if ( any(non_separating_chars == file(l2:l2)) ) then
					number = file(l1+1:l2-1)
					do j = 1, len(number)
						if ( number(j:j) == sep ) then
							sep_pos = j
							exit
						end if
					end do
					if ( fmt == 'z' ) then
						read(unit=number(:sep_pos-1), fmt='(z'//str(len(number(:sep_pos-1)))//')') c%re
						read(unit=number(sep_pos+1:), fmt='(z'//str(len(number(sep_pos+1:)))//')') c%im
					else
						read(unit=number(:sep_pos-1), fmt=*, decimal=decimal) c%re
						read(unit=number(sep_pos+1:), fmt=*, decimal=decimal) c%im
					end if
					into(ind) = c
					if ( ind /= size(into) ) then
						ind = ind + 1
					else
						exit read_into
					end if
					l2 = i
				else
					l2 = i
				end if
			end if
			i = i + 1
		end do read_into
	end procedure from_text_1dc64
	module procedure from_text_1dc32
		logical :: exists, ignore_sep
		integer :: file_unit, iostat
		integer :: n_rows, n_columns
		integer :: i, j, ind, l1, l2, sep_pos

		complex(real32) :: c
		character(len=:), allocatable, dimension(:) :: im_chars, non_separating_chars
		character(len=:), allocatable :: file, decimal, sep, number
		character(len=1) :: prev_char, current_char
		integer(int64) :: file_length

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		inquire( file=file_name, size=file_length )

		if ( file_length == 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty.'
			return
		end if

		allocate( character(len=file_length) :: file )
		read(unit=file_unit, iostat=iostat) file
		close(file_unit)

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		if ( header ) then
			do i = 1, file_length
				if ( file(i:i) == NL ) then
					file = file(i+1:)
					file_length = len(file)
					exit
				else if ( i == file_length ) then
					file = file//LF
					file_length = file_length + 1
					write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
									file_name//'". File has one line.'
				end if
			end do

			if ( file_length == 0 ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
				return
			end if
		end if

		n_rows = 0

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				n_rows = n_rows + 1
			else if ( i == file_length ) then
				file = file//LF
				file_length = file_length + 1
				n_rows = n_rows + 1
			end if
		end do

		if ( len(im) > 0 ) then
			allocate( character(len=1) :: im_chars(len(im)) )
			do i = 1, len(im)
				im_chars(i) = im(i:i)
			end do
		else
			im_chars = [ EMPTY_STR ]
		end if

		if ( fmt == 'z' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', &
									'a', 'b', 'c', 'd', 'e', 'f', '(', ')', '+']
		else
			if ( locale == 'US' ) then
				decimal = 'POINT'
				non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', 'e', 'E', '+', '-', &
										'(', ')']
			else if ( locale == 'EU' ) then
				decimal = 'COMMA'
				non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ',', 'e', 'E', '+', '-', &
										'(', ')']
			end if
		end if

		if ( locale == 'US' ) then
			sep = COMMA
		else if ( locale == 'EU' ) then
			sep = SEMICOLON
		end if

		ignore_sep = .false.
		prev_char = '0'
		n_columns = 0

		do i = 1, file_length
			current_char = file(i:i)
			if ( current_char == '(' ) then
				ignore_sep = .true.
			else if ( current_char == ')' ) then
				ignore_sep = .false.
			end if

			if ( any(non_separating_chars == current_char) .or. any(im_chars == current_char) ) then
				prev_char = current_char
			else
				if ( ignore_sep ) then
					prev_char = current_char
					cycle
				end if

				if ( any(non_separating_chars == prev_char) .or. any(im_chars == prev_char) ) then
					prev_char = current_char
					n_columns = n_columns + 1
				else
					prev_char = current_char
				end if
			end if

			if ( current_char == NL ) exit
		end do

		if ( (n_rows > 1) .and. (n_columns > 1) ) then
			error stop LF//'Error reading file "'//file_name//'". File data cannot fit into one-dimensional array.'
			return
		else if ( n_columns == 1 ) then
			allocate( into(n_rows) )
		else if ( n_rows == 1 ) then
			allocate( into(n_columns) )
		end if

		ignore_sep = .false.
		ind = 1
		l1 = 1
		l2 = 1
		i = 1

		read_into: do while ( i <= file_length )
			current_char = file(i:i)
			if ( current_char == '(' ) then
				ignore_sep = .true.
			else if ( current_char == ')' ) then
				ignore_sep = .false.
			end if

			if ( any(non_separating_chars == current_char) ) then
				if ( .not. any(non_separating_chars == file(l2:l2)) ) l1 = i
				l2 = i
			else if ( current_char == im_chars(1) ) then
				number = file(l1:i-1)
				do j = 2, len(number)
					if ( (number(j:j) == '+') .or. (number(j:j) == '-') ) then
						if ( fmt == 'e' ) then
							if ( (number(j-1:j-1) /= 'E') .and. (number(j-1:j-1) /= 'e') ) then
								sep_pos = j
								exit
							end if
						else
							sep_pos = j
							exit
						end if
					end if
				end do
				if ( fmt == 'z' ) then
					read(unit=number(:sep_pos-1), fmt='(z'//str(len(number(:sep_pos-1)))//')') c%re
					read(unit=number(sep_pos+1:), fmt='(z'//str(len(number(sep_pos+1:)))//')') c%im
				else
					read(unit=number(:sep_pos-1), fmt=*, decimal=decimal) c%re
					read(unit=number(sep_pos:), fmt=*, decimal=decimal) c%im
				end if
				into(ind) = c
				if ( ind /= size(into) ) then
					ind = ind + 1
				else
					exit read_into
				end if
				l2 = i
				i = i + size(im_chars)
				cycle read_into
			else
				if ( ignore_sep ) then
					l2 = i + 1
					i = i + 1
					cycle read_into
				end if

				if ( any(non_separating_chars == file(l2:l2)) ) then
					number = file(l1+1:l2-1)
					do j = 1, len(number)
						if ( number(j:j) == sep ) then
							sep_pos = j
							exit
						end if
					end do
					if ( fmt == 'z' ) then
						read(unit=number(:sep_pos-1), fmt='(z'//str(len(number(:sep_pos-1)))//')') c%re
						read(unit=number(sep_pos+1:), fmt='(z'//str(len(number(sep_pos+1:)))//')') c%im
					else
						read(unit=number(:sep_pos-1), fmt=*, decimal=decimal) c%re
						read(unit=number(sep_pos+1:), fmt=*, decimal=decimal) c%im
					end if
					into(ind) = c
					if ( ind /= size(into) ) then
						ind = ind + 1
					else
						exit read_into
					end if
					l2 = i
				else
					l2 = i
				end if
			end if
			i = i + 1
		end do read_into
	end procedure from_text_1dc32

	module procedure from_text_2dc128
		logical :: exists, ignore_sep
		integer :: file_unit, iostat
		integer :: n_rows, n_columns
		integer :: i, j, row, column, l1, l2, sep_pos

		complex(real128) :: c
		type(String), allocatable, dimension(:) :: lines
		character(len=:), allocatable, dimension(:) :: im_chars, non_separating_chars
		character(len=:), allocatable :: file, decimal, sep, number
		character(len=1) :: prev_char, current_char
		integer(int64) :: file_length

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		inquire( file=file_name, size=file_length )

		if ( file_length == 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty.'
			return
		end if

		allocate( character(len=file_length) :: file )
		read(unit=file_unit, iostat=iostat) file
		close(file_unit)

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		if ( header ) then
			do i = 1, file_length
				if ( file(i:i) == NL ) then
					file = file(i+1:)
					file_length = len(file)
					exit
				else if ( i == file_length ) then
					file = file//LF
					file_length = file_length + 1
					write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
									file_name//'". File has one line.'
				end if
			end do

			if ( file_length == 0 ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
				return
			end if
		end if

		n_rows = 0

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				n_rows = n_rows + 1
			else if ( i == file_length ) then
				file = file//LF
				file_length = file_length + 1
				n_rows = n_rows + 1
			end if
		end do

		if ( len(im) > 0 ) then
			allocate( character(len=1) :: im_chars(len(im)) )
			do i = 1, len(im)
				im_chars(i) = im(i:i)
			end do
		else
			im_chars = [ EMPTY_STR ]
		end if

		if ( fmt == 'z' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', &
									'a', 'b', 'c', 'd', 'e', 'f', '(', ')', '+']
		else
			if ( locale == 'US' ) then
				decimal = 'POINT'
				non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', 'e', 'E', '+', '-', &
										'(', ')']
			else if ( locale == 'EU' ) then
				decimal = 'COMMA'
				non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ',', 'e', 'E', '+', '-', &
										'(', ')']
			end if
		end if

		if ( locale == 'US' ) then
			sep = COMMA
		else if ( locale == 'EU' ) then
			sep = SEMICOLON
		end if

		ignore_sep = .false.
		prev_char = '0'
		n_columns = 0

		do i = 1, file_length
			current_char = file(i:i)
			if ( current_char == '(' ) then
				ignore_sep = .true.
			else if ( current_char == ')' ) then
				ignore_sep = .false.
			end if

			if ( any(non_separating_chars == current_char) .or. any(im_chars == current_char) ) then
				prev_char = current_char
			else
				if ( ignore_sep ) then
					prev_char = current_char
					cycle
				end if

				if ( any(non_separating_chars == prev_char) .or. any(im_chars == prev_char) ) then
					prev_char = current_char
					n_columns = n_columns + 1
				else
					prev_char = current_char
				end if
			end if

			if ( current_char == NL ) exit
		end do

		allocate( lines(n_rows) )

		row = 1
		l1 = 1

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				lines(row)%s = file(l1:i)
				if ( row /= n_rows ) then
					row = row + 1
					l1 = i + 1
				else
					exit
				end if
			end if
		end do

		deallocate(file)

		allocate( into(n_rows, n_columns) )

		do concurrent (row = 1:n_rows)
			ignore_sep = .false.
			column = 1
			l1 = 1
			l2 = 1
			i = 1
			read_into: do while ( i <= len(lines(row)%s) )
				current_char = lines(row)%s(i:i)
				if ( current_char == '(' ) then
					ignore_sep = .true.
				else if ( current_char == ')' ) then
					ignore_sep = .false.
				end if

				if ( any(non_separating_chars == current_char) ) then
					if ( .not. any(non_separating_chars == lines(row)%s(l2:l2)) ) l1 = i
					l2 = i
				else if ( current_char == im_chars(1) ) then
					number = lines(row)%s(l1:i-1)
					do j = 2, len(number)
						if ( (number(j:j) == '+') .or. (number(j:j) == '-') ) then
							if ( fmt == 'e' ) then
								if ( (number(j-1:j-1) /= 'E') .and. (number(j-1:j-1) /= 'e') ) then
									sep_pos = j
									exit
								end if
							else
								sep_pos = j
								exit
							end if
						end if
					end do
					if ( fmt == 'z' ) then
						read(unit=number(:sep_pos-1), fmt='(z'//str(len(number(:sep_pos-1)))//')') c%re
						read(unit=number(sep_pos+1:), fmt='(z'//str(len(number(sep_pos+1:)))//')') c%im
					else
						read(unit=number(:sep_pos-1), fmt=*, decimal=decimal) c%re
						read(unit=number(sep_pos:), fmt=*, decimal=decimal) c%im
					end if
					into(row, column) = c
					if ( column /= n_columns ) then
						column = column + 1
					else
						exit read_into
					end if
					l2 = i
					i = i + size(im_chars)
					cycle read_into
				else
					if ( ignore_sep ) then
						l2 = i + 1
						i = i + 1
						cycle read_into
					end if

					if ( any(non_separating_chars == lines(row)%s(l2:l2)) ) then
						number = lines(row)%s(l1+1:l2-1)
						do j = 1, len(number)
							if ( number(j:j) == sep ) then
								sep_pos = j
								exit
							end if
						end do
						if ( fmt == 'z' ) then
							read(unit=number(:sep_pos-1), fmt='(z'//str(len(number(:sep_pos-1)))//')') c%re
							read(unit=number(sep_pos+1:), fmt='(z'//str(len(number(sep_pos+1:)))//')') c%im
						else
							read(unit=number(:sep_pos-1), fmt=*, decimal=decimal) c%re
							read(unit=number(sep_pos+1:), fmt=*, decimal=decimal) c%im
						end if
						into(row, column) = c
						if ( column /= n_columns ) then
							column = column + 1
						else
							exit read_into
						end if
						l2 = i
					else
						l2 = i
					end if
				end if
				i = i + 1
			end do read_into
		end do
	end procedure from_text_2dc128
	module procedure from_text_2dc64
		logical :: exists, ignore_sep
		integer :: file_unit, iostat
		integer :: n_rows, n_columns
		integer :: i, j, row, column, l1, l2, sep_pos

		complex(real64) :: c
		type(String), allocatable, dimension(:) :: lines
		character(len=:), allocatable, dimension(:) :: im_chars, non_separating_chars
		character(len=:), allocatable :: file, decimal, sep, number
		character(len=1) :: prev_char, current_char
		integer(int64) :: file_length

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		inquire( file=file_name, size=file_length )

		if ( file_length == 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty.'
			return
		end if

		allocate( character(len=file_length) :: file )
		read(unit=file_unit, iostat=iostat) file
		close(file_unit)

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		if ( header ) then
			do i = 1, file_length
				if ( file(i:i) == NL ) then
					file = file(i+1:)
					file_length = len(file)
					exit
				else if ( i == file_length ) then
					file = file//LF
					file_length = file_length + 1
					write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
									file_name//'". File has one line.'
				end if
			end do

			if ( file_length == 0 ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
				return
			end if
		end if

		n_rows = 0

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				n_rows = n_rows + 1
			else if ( i == file_length ) then
				file = file//LF
				file_length = file_length + 1
				n_rows = n_rows + 1
			end if
		end do

		if ( len(im) > 0 ) then
			allocate( character(len=1) :: im_chars(len(im)) )
			do i = 1, len(im)
				im_chars(i) = im(i:i)
			end do
		else
			im_chars = [ EMPTY_STR ]
		end if

		if ( fmt == 'z' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', &
									'a', 'b', 'c', 'd', 'e', 'f', '(', ')', '+']
		else
			if ( locale == 'US' ) then
				decimal = 'POINT'
				non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', 'e', 'E', '+', '-', &
										'(', ')']
			else if ( locale == 'EU' ) then
				decimal = 'COMMA'
				non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ',', 'e', 'E', '+', '-', &
										'(', ')']
			end if
		end if

		if ( locale == 'US' ) then
			sep = COMMA
		else if ( locale == 'EU' ) then
			sep = SEMICOLON
		end if

		ignore_sep = .false.
		prev_char = '0'
		n_columns = 0

		do i = 1, file_length
			current_char = file(i:i)
			if ( current_char == '(' ) then
				ignore_sep = .true.
			else if ( current_char == ')' ) then
				ignore_sep = .false.
			end if

			if ( any(non_separating_chars == current_char) .or. any(im_chars == current_char) ) then
				prev_char = current_char
			else
				if ( ignore_sep ) then
					prev_char = current_char
					cycle
				end if

				if ( any(non_separating_chars == prev_char) .or. any(im_chars == prev_char) ) then
					prev_char = current_char
					n_columns = n_columns + 1
				else
					prev_char = current_char
				end if
			end if

			if ( current_char == NL ) exit
		end do

		allocate( lines(n_rows) )

		row = 1
		l1 = 1

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				lines(row)%s = file(l1:i)
				if ( row /= n_rows ) then
					row = row + 1
					l1 = i + 1
				else
					exit
				end if
			end if
		end do

		deallocate(file)

		allocate( into(n_rows, n_columns) )

		do concurrent (row = 1:n_rows)
			ignore_sep = .false.
			column = 1
			l1 = 1
			l2 = 1
			i = 1
			read_into: do while ( i <= len(lines(row)%s) )
				current_char = lines(row)%s(i:i)
				if ( current_char == '(' ) then
					ignore_sep = .true.
				else if ( current_char == ')' ) then
					ignore_sep = .false.
				end if

				if ( any(non_separating_chars == current_char) ) then
					if ( .not. any(non_separating_chars == lines(row)%s(l2:l2)) ) l1 = i
					l2 = i
				else if ( current_char == im_chars(1) ) then
					number = lines(row)%s(l1:i-1)
					do j = 2, len(number)
						if ( (number(j:j) == '+') .or. (number(j:j) == '-') ) then
							if ( fmt == 'e' ) then
								if ( (number(j-1:j-1) /= 'E') .and. (number(j-1:j-1) /= 'e') ) then
									sep_pos = j
									exit
								end if
							else
								sep_pos = j
								exit
							end if
						end if
					end do
					if ( fmt == 'z' ) then
						read(unit=number(:sep_pos-1), fmt='(z'//str(len(number(:sep_pos-1)))//')') c%re
						read(unit=number(sep_pos+1:), fmt='(z'//str(len(number(sep_pos+1:)))//')') c%im
					else
						read(unit=number(:sep_pos-1), fmt=*, decimal=decimal) c%re
						read(unit=number(sep_pos:), fmt=*, decimal=decimal) c%im
					end if
					into(row, column) = c
					if ( column /= n_columns ) then
						column = column + 1
					else
						exit read_into
					end if
					l2 = i
					i = i + size(im_chars)
					cycle read_into
				else
					if ( ignore_sep ) then
						l2 = i + 1
						i = i + 1
						cycle read_into
					end if

					if ( any(non_separating_chars == lines(row)%s(l2:l2)) ) then
						number = lines(row)%s(l1+1:l2-1)
						do j = 1, len(number)
							if ( number(j:j) == sep ) then
								sep_pos = j
								exit
							end if
						end do
						if ( fmt == 'z' ) then
							read(unit=number(:sep_pos-1), fmt='(z'//str(len(number(:sep_pos-1)))//')') c%re
							read(unit=number(sep_pos+1:), fmt='(z'//str(len(number(sep_pos+1:)))//')') c%im
						else
							read(unit=number(:sep_pos-1), fmt=*, decimal=decimal) c%re
							read(unit=number(sep_pos+1:), fmt=*, decimal=decimal) c%im
						end if
						into(row, column) = c
						if ( column /= n_columns ) then
							column = column + 1
						else
							exit read_into
						end if
						l2 = i
					else
						l2 = i
					end if
				end if
				i = i + 1
			end do read_into
		end do
	end procedure from_text_2dc64
	module procedure from_text_2dc32
		logical :: exists, ignore_sep
		integer :: file_unit, iostat
		integer :: n_rows, n_columns
		integer :: i, j, row, column, l1, l2, sep_pos

		complex(real32) :: c
		type(String), allocatable, dimension(:) :: lines
		character(len=:), allocatable, dimension(:) :: im_chars, non_separating_chars
		character(len=:), allocatable :: file, decimal, sep, number
		character(len=1) :: prev_char, current_char
		integer(int64) :: file_length

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		inquire( file=file_name, size=file_length )

		if ( file_length == 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty.'
			return
		end if

		allocate( character(len=file_length) :: file )
		read(unit=file_unit, iostat=iostat) file
		close(file_unit)

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		if ( header ) then
			do i = 1, file_length
				if ( file(i:i) == NL ) then
					file = file(i+1:)
					file_length = len(file)
					exit
				else if ( i == file_length ) then
					file = file//LF
					file_length = file_length + 1
					write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
									file_name//'". File has one line.'
				end if
			end do

			if ( file_length == 0 ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
				return
			end if
		end if

		n_rows = 0

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				n_rows = n_rows + 1
			else if ( i == file_length ) then
				file = file//LF
				file_length = file_length + 1
				n_rows = n_rows + 1
			end if
		end do

		if ( len(im) > 0 ) then
			allocate( character(len=1) :: im_chars(len(im)) )
			do i = 1, len(im)
				im_chars(i) = im(i:i)
			end do
		else
			im_chars = [ EMPTY_STR ]
		end if

		if ( fmt == 'z' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', &
									'a', 'b', 'c', 'd', 'e', 'f', '(', ')', '+']
		else
			if ( locale == 'US' ) then
				decimal = 'POINT'
				non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', 'e', 'E', '+', '-', &
										'(', ')']
			else if ( locale == 'EU' ) then
				decimal = 'COMMA'
				non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ',', 'e', 'E', '+', '-', &
										'(', ')']
			end if
		end if

		if ( locale == 'US' ) then
			sep = COMMA
		else if ( locale == 'EU' ) then
			sep = SEMICOLON
		end if

		ignore_sep = .false.
		prev_char = '0'
		n_columns = 0

		do i = 1, file_length
			current_char = file(i:i)
			if ( current_char == '(' ) then
				ignore_sep = .true.
			else if ( current_char == ')' ) then
				ignore_sep = .false.
			end if

			if ( any(non_separating_chars == current_char) .or. any(im_chars == current_char) ) then
				prev_char = current_char
			else
				if ( ignore_sep ) then
					prev_char = current_char
					cycle
				end if

				if ( any(non_separating_chars == prev_char) .or. any(im_chars == prev_char) ) then
					prev_char = current_char
					n_columns = n_columns + 1
				else
					prev_char = current_char
				end if
			end if

			if ( current_char == NL ) exit
		end do

		allocate( lines(n_rows) )

		row = 1
		l1 = 1

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				lines(row)%s = file(l1:i)
				if ( row /= n_rows ) then
					row = row + 1
					l1 = i + 1
				else
					exit
				end if
			end if
		end do

		deallocate(file)

		allocate( into(n_rows, n_columns) )

		do concurrent (row = 1:n_rows)
			ignore_sep = .false.
			column = 1
			l1 = 1
			l2 = 1
			i = 1
			read_into: do while ( i <= len(lines(row)%s) )
				current_char = lines(row)%s(i:i)
				if ( current_char == '(' ) then
					ignore_sep = .true.
				else if ( current_char == ')' ) then
					ignore_sep = .false.
				end if

				if ( any(non_separating_chars == current_char) ) then
					if ( .not. any(non_separating_chars == lines(row)%s(l2:l2)) ) l1 = i
					l2 = i
				else if ( current_char == im_chars(1) ) then
					number = lines(row)%s(l1:i-1)
					do j = 2, len(number)
						if ( (number(j:j) == '+') .or. (number(j:j) == '-') ) then
							if ( fmt == 'e' ) then
								if ( (number(j-1:j-1) /= 'E') .and. (number(j-1:j-1) /= 'e') ) then
									sep_pos = j
									exit
								end if
							else
								sep_pos = j
								exit
							end if
						end if
					end do
					if ( fmt == 'z' ) then
						read(unit=number(:sep_pos-1), fmt='(z'//str(len(number(:sep_pos-1)))//')') c%re
						read(unit=number(sep_pos+1:), fmt='(z'//str(len(number(sep_pos+1:)))//')') c%im
					else
						read(unit=number(:sep_pos-1), fmt=*, decimal=decimal) c%re
						read(unit=number(sep_pos:), fmt=*, decimal=decimal) c%im
					end if
					into(row, column) = c
					if ( column /= n_columns ) then
						column = column + 1
					else
						exit read_into
					end if
					l2 = i
					i = i + size(im_chars)
					cycle read_into
				else
					if ( ignore_sep ) then
						l2 = i + 1
						i = i + 1
						cycle read_into
					end if

					if ( any(non_separating_chars == lines(row)%s(l2:l2)) ) then
						number = lines(row)%s(l1+1:l2-1)
						do j = 1, len(number)
							if ( number(j:j) == sep ) then
								sep_pos = j
								exit
							end if
						end do
						if ( fmt == 'z' ) then
							read(unit=number(:sep_pos-1), fmt='(z'//str(len(number(:sep_pos-1)))//')') c%re
							read(unit=number(sep_pos+1:), fmt='(z'//str(len(number(sep_pos+1:)))//')') c%im
						else
							read(unit=number(:sep_pos-1), fmt=*, decimal=decimal) c%re
							read(unit=number(sep_pos+1:), fmt=*, decimal=decimal) c%im
						end if
						into(row, column) = c
						if ( column /= n_columns ) then
							column = column + 1
						else
							exit read_into
						end if
						l2 = i
					else
						l2 = i
					end if
				end if
				i = i + 1
			end do read_into
		end do
	end procedure from_text_2dc32

	module procedure from_text_1dr128
		logical :: exists
		integer :: file_unit, iostat
		integer :: n_rows, n_columns
		integer :: i, ind, l1, l2

		character(len=:), allocatable, dimension(:) :: non_separating_chars
		character(len=:), allocatable :: file, decimal
		character(len=1) :: prev_char, current_char
		integer(int64) :: file_length

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		inquire( file=file_name, size=file_length )

		if ( file_length == 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty.'
			return
		end if

		allocate( character(len=file_length) :: file )
		read(unit=file_unit, iostat=iostat) file
		close(file_unit)

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		if ( header ) then
			do i = 1, file_length
				if ( file(i:i) == NL ) then
					file = file(i+1:)
					file_length = len(file)
					exit
				else if ( i == file_length ) then
					file = file//LF
					file_length = file_length + 1
					write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
									file_name//'". File has one line.'
				end if
			end do

			if ( file_length == 0 ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
				return
			end if
		end if

		n_rows = 0

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				n_rows = n_rows + 1
			else if ( i == file_length ) then
				file = file//LF
				file_length = file_length + 1
				n_rows = n_rows + 1
			end if
		end do

		if ( fmt == 'z' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', &
									'a', 'b', 'c', 'd', 'e', 'f']
		else
			if ( locale == 'US' ) then
				decimal = 'POINT'
				non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', 'e', 'E', '+', '-']
			else if ( locale == 'EU' ) then
				decimal = 'COMMA'
				non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ',', 'e', 'E', '+', '-']
			end if
		end if

		prev_char = '0'
		n_columns = 0

		do i = 1, file_length
			current_char = file(i:i)

			if ( any(non_separating_chars == current_char) ) then
				prev_char = current_char
			else
				if ( any(non_separating_chars == prev_char) ) then
					prev_char = current_char
					n_columns = n_columns + 1
				else
					prev_char = current_char
				end if
			end if

			if ( current_char == NL ) exit
		end do

		if ( (n_rows > 1) .and. (n_columns > 1) ) then
			error stop LF//'Error reading file "'//file_name//'". File data cannot fit into one-dimensional array.'
			return
		else if ( n_columns == 1 ) then
			allocate( into(n_rows) )
		else if ( n_rows == 1 ) then
			allocate( into(n_columns) )
		end if

		ind = 1
		l1 = 1
		l2 = 1

		read_into: do i = 1, file_length
			if ( any(non_separating_chars == file(i:i)) ) then
				if ( .not. any(non_separating_chars == file(l2:l2)) ) l1 = i
				l2 = i
			else
				if ( any(non_separating_chars == file(l2:l2)) ) then
					if ( fmt == 'z' ) then
						read(unit=file(l1:l2), fmt='(z'//str(l2-l1+1)//')') into(ind)
					else
						read(unit=file(l1:l2), fmt=*, decimal=decimal) into(ind)
					end if
					if ( ind /= size(into) ) then
						ind = ind + 1
					else
						exit read_into
					end if
					l2 = i
				else
					l2 = i
				end if
			end if
		end do read_into
	end procedure from_text_1dr128
	module procedure from_text_1dr64
		logical :: exists
		integer :: file_unit, iostat
		integer :: n_rows, n_columns
		integer :: i, ind, l1, l2

		character(len=:), allocatable, dimension(:) :: non_separating_chars
		character(len=:), allocatable :: file, decimal
		character(len=1) :: prev_char, current_char
		integer(int64) :: file_length

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		inquire( file=file_name, size=file_length )

		if ( file_length == 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty.'
			return
		end if

		allocate( character(len=file_length) :: file )
		read(unit=file_unit, iostat=iostat) file
		close(file_unit)

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		if ( header ) then
			do i = 1, file_length
				if ( file(i:i) == NL ) then
					file = file(i+1:)
					file_length = len(file)
					exit
				else if ( i == file_length ) then
					file = file//LF
					file_length = file_length + 1
					write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
									file_name//'". File has one line.'
				end if
			end do

			if ( file_length == 0 ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
				return
			end if
		end if

		n_rows = 0

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				n_rows = n_rows + 1
			else if ( i == file_length ) then
				file = file//LF
				file_length = file_length + 1
				n_rows = n_rows + 1
			end if
		end do

		if ( fmt == 'z' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', &
									'a', 'b', 'c', 'd', 'e', 'f']
		else
			if ( locale == 'US' ) then
				decimal = 'POINT'
				non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', 'e', 'E', '+', '-']
			else if ( locale == 'EU' ) then
				decimal = 'COMMA'
				non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ',', 'e', 'E', '+', '-']
			end if
		end if

		prev_char = '0'
		n_columns = 0

		do i = 1, file_length
			current_char = file(i:i)

			if ( any(non_separating_chars == current_char) ) then
				prev_char = current_char
			else
				if ( any(non_separating_chars == prev_char) ) then
					prev_char = current_char
					n_columns = n_columns + 1
				else
					prev_char = current_char
				end if
			end if

			if ( current_char == NL ) exit
		end do

		if ( (n_rows > 1) .and. (n_columns > 1) ) then
			error stop LF//'Error reading file "'//file_name//'". File data cannot fit into one-dimensional array.'
			return
		else if ( n_columns == 1 ) then
			allocate( into(n_rows) )
		else if ( n_rows == 1 ) then
			allocate( into(n_columns) )
		end if

		ind = 1
		l1 = 1
		l2 = 1

		read_into: do i = 1, file_length
			if ( any(non_separating_chars == file(i:i)) ) then
				if ( .not. any(non_separating_chars == file(l2:l2)) ) l1 = i
				l2 = i
			else
				if ( any(non_separating_chars == file(l2:l2)) ) then
					if ( fmt == 'z' ) then
						read(unit=file(l1:l2), fmt='(z'//str(l2-l1+1)//')') into(ind)
					else
						read(unit=file(l1:l2), fmt=*, decimal=decimal) into(ind)
					end if
					if ( ind /= size(into) ) then
						ind = ind + 1
					else
						exit read_into
					end if
					l2 = i
				else
					l2 = i
				end if
			end if
		end do read_into
	end procedure from_text_1dr64
	module procedure from_text_1dr32
		logical :: exists
		integer :: file_unit, iostat
		integer :: n_rows, n_columns
		integer :: i, ind, l1, l2

		character(len=:), allocatable, dimension(:) :: non_separating_chars
		character(len=:), allocatable :: file, decimal
		character(len=1) :: prev_char, current_char
		integer(int64) :: file_length

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		inquire( file=file_name, size=file_length )

		if ( file_length == 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty.'
			return
		end if

		allocate( character(len=file_length) :: file )
		read(unit=file_unit, iostat=iostat) file
		close(file_unit)

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		if ( header ) then
			do i = 1, file_length
				if ( file(i:i) == NL ) then
					file = file(i+1:)
					file_length = len(file)
					exit
				else if ( i == file_length ) then
					file = file//LF
					file_length = file_length + 1
					write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
									file_name//'". File has one line.'
				end if
			end do

			if ( file_length == 0 ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
				return
			end if
		end if

		n_rows = 0

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				n_rows = n_rows + 1
			else if ( i == file_length ) then
				file = file//LF
				file_length = file_length + 1
				n_rows = n_rows + 1
			end if
		end do

		if ( fmt == 'z' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', &
									'a', 'b', 'c', 'd', 'e', 'f']
		else
			if ( locale == 'US' ) then
				decimal = 'POINT'
				non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', 'e', 'E', '+', '-']
			else if ( locale == 'EU' ) then
				decimal = 'COMMA'
				non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ',', 'e', 'E', '+', '-']
			end if
		end if

		prev_char = '0'
		n_columns = 0

		do i = 1, file_length
			current_char = file(i:i)

			if ( any(non_separating_chars == current_char) ) then
				prev_char = current_char
			else
				if ( any(non_separating_chars == prev_char) ) then
					prev_char = current_char
					n_columns = n_columns + 1
				else
					prev_char = current_char
				end if
			end if

			if ( current_char == NL ) exit
		end do

		if ( (n_rows > 1) .and. (n_columns > 1) ) then
			error stop LF//'Error reading file "'//file_name//'". File data cannot fit into one-dimensional array.'
			return
		else if ( n_columns == 1 ) then
			allocate( into(n_rows) )
		else if ( n_rows == 1 ) then
			allocate( into(n_columns) )
		end if

		ind = 1
		l1 = 1
		l2 = 1

		read_into: do i = 1, file_length
			if ( any(non_separating_chars == file(i:i)) ) then
				if ( .not. any(non_separating_chars == file(l2:l2)) ) l1 = i
				l2 = i
			else
				if ( any(non_separating_chars == file(l2:l2)) ) then
					if ( fmt == 'z' ) then
						read(unit=file(l1:l2), fmt='(z'//str(l2-l1+1)//')') into(ind)
					else
						read(unit=file(l1:l2), fmt=*, decimal=decimal) into(ind)
					end if
					if ( ind /= size(into) ) then
						ind = ind + 1
					else
						exit read_into
					end if
					l2 = i
				else
					l2 = i
				end if
			end if
		end do read_into
	end procedure from_text_1dr32

	module procedure from_text_2dr128
		logical :: exists
		integer :: file_unit, iostat
		integer :: n_rows, n_columns
		integer :: i, row, column, l1, l2

		type(String), allocatable, dimension(:) :: lines
		character(len=:), allocatable, dimension(:) :: non_separating_chars
		character(len=:), allocatable :: file, decimal
		character(len=1) :: prev_char, current_char
		integer(int64) :: file_length

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		inquire( file=file_name, size=file_length )

		if ( file_length == 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty.'
			return
		end if

		allocate( character(len=file_length) :: file )
		read(unit=file_unit, iostat=iostat) file
		close(file_unit)

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		if ( header ) then
			do i = 1, file_length
				if ( file(i:i) == NL ) then
					file = file(i+1:)
					file_length = len(file)
					exit
				else if ( i == file_length ) then
					file = file//LF
					file_length = file_length + 1
					write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
									file_name//'". File has one line.'
				end if
			end do

			if ( file_length == 0 ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
				return
			end if
		end if

		n_rows = 0

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				n_rows = n_rows + 1
			else if ( i == file_length ) then
				file = file//LF
				file_length = file_length + 1
				n_rows = n_rows + 1
			end if
		end do

		if ( fmt == 'z' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', &
									'a', 'b', 'c', 'd', 'e', 'f']
		else
			if ( locale == 'US' ) then
				decimal = 'POINT'
				non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', 'e', 'E', '+', '-']
			else if ( locale == 'EU' ) then
				decimal = 'COMMA'
				non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ',', 'e', 'E', '+', '-']
			end if
		end if

		prev_char = '0'
		n_columns = 0

		do i = 1, file_length
			current_char = file(i:i)

			if ( any(non_separating_chars == current_char) ) then
				prev_char = current_char
			else
				if ( any(non_separating_chars == prev_char) ) then
					prev_char = current_char
					n_columns = n_columns + 1
				else
					prev_char = current_char
				end if
			end if

			if ( current_char == NL ) exit
		end do

		allocate( lines(n_rows) )

		row = 1
		l1 = 1

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				lines(row)%s = file(l1:i)
				if ( row /= n_rows ) then
					row = row + 1
					l1 = i + 1
				else
					exit
				end if
			end if
		end do

		deallocate(file)

		allocate( into(n_rows, n_columns) )

		do concurrent (row = 1:n_rows)
			column = 1
			l1 = 1
			l2 = 1
			read_into: do i = 1, len(lines(row)%s)
				if ( any(non_separating_chars == lines(row)%s(i:i)) ) then
					if ( .not. any(non_separating_chars == lines(row)%s(l2:l2)) ) l1 = i
					l2 = i
				else
					if ( any(non_separating_chars == lines(row)%s(l2:l2)) ) then
						if ( fmt == 'z' ) then
							read(unit=lines(row)%s(l1:l2), fmt='(z'//str(l2-l1+1)//')') into(row, column)
						else
							read(unit=lines(row)%s(l1:l2), fmt=*, decimal=decimal) into(row, column)
						end if
						if ( column /= n_columns ) then
							column = column + 1
						else
							exit read_into
						end if
						l2 = i
					else
						l2 = i
					end if
				end if
			end do read_into
		end do
	end procedure from_text_2dr128
	module procedure from_text_2dr64
		logical :: exists
		integer :: file_unit, iostat
		integer :: n_rows, n_columns
		integer :: i, row, column, l1, l2

		type(String), allocatable, dimension(:) :: lines
		character(len=:), allocatable, dimension(:) :: non_separating_chars
		character(len=:), allocatable :: file, decimal
		character(len=1) :: prev_char, current_char
		integer(int64) :: file_length

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		inquire( file=file_name, size=file_length )

		if ( file_length == 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty.'
			return
		end if

		allocate( character(len=file_length) :: file )
		read(unit=file_unit, iostat=iostat) file
		close(file_unit)

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		if ( header ) then
			do i = 1, file_length
				if ( file(i:i) == NL ) then
					file = file(i+1:)
					file_length = len(file)
					exit
				else if ( i == file_length ) then
					file = file//LF
					file_length = file_length + 1
					write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
									file_name//'". File has one line.'
				end if
			end do

			if ( file_length == 0 ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
				return
			end if
		end if

		n_rows = 0

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				n_rows = n_rows + 1
			else if ( i == file_length ) then
				file = file//LF
				file_length = file_length + 1
				n_rows = n_rows + 1
			end if
		end do

		if ( fmt == 'z' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', &
									'a', 'b', 'c', 'd', 'e', 'f']
		else
			if ( locale == 'US' ) then
				decimal = 'POINT'
				non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', 'e', 'E', '+', '-']
			else if ( locale == 'EU' ) then
				decimal = 'COMMA'
				non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ',', 'e', 'E', '+', '-']
			end if
		end if

		prev_char = '0'
		n_columns = 0

		do i = 1, file_length
			current_char = file(i:i)

			if ( any(non_separating_chars == current_char) ) then
				prev_char = current_char
			else
				if ( any(non_separating_chars == prev_char) ) then
					prev_char = current_char
					n_columns = n_columns + 1
				else
					prev_char = current_char
				end if
			end if

			if ( current_char == NL ) exit
		end do

		allocate( lines(n_rows) )

		row = 1
		l1 = 1

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				lines(row)%s = file(l1:i)
				if ( row /= n_rows ) then
					row = row + 1
					l1 = i + 1
				else
					exit
				end if
			end if
		end do

		deallocate(file)

		allocate( into(n_rows, n_columns) )

		do concurrent (row = 1:n_rows)
			column = 1
			l1 = 1
			l2 = 1
			read_into: do i = 1, len(lines(row)%s)
				if ( any(non_separating_chars == lines(row)%s(i:i)) ) then
					if ( .not. any(non_separating_chars == lines(row)%s(l2:l2)) ) l1 = i
					l2 = i
				else
					if ( any(non_separating_chars == lines(row)%s(l2:l2)) ) then
						if ( fmt == 'z' ) then
							read(unit=lines(row)%s(l1:l2), fmt='(z'//str(l2-l1+1)//')') into(row, column)
						else
							read(unit=lines(row)%s(l1:l2), fmt=*, decimal=decimal) into(row, column)
						end if
						if ( column /= n_columns ) then
							column = column + 1
						else
							exit read_into
						end if
						l2 = i
					else
						l2 = i
					end if
				end if
			end do read_into
		end do
	end procedure from_text_2dr64
	module procedure from_text_2dr32
		logical :: exists
		integer :: file_unit, iostat
		integer :: n_rows, n_columns
		integer :: i, row, column, l1, l2

		type(String), allocatable, dimension(:) :: lines
		character(len=:), allocatable, dimension(:) :: non_separating_chars
		character(len=:), allocatable :: file, decimal
		character(len=1) :: prev_char, current_char
		integer(int64) :: file_length

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		inquire( file=file_name, size=file_length )

		if ( file_length == 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty.'
			return
		end if

		allocate( character(len=file_length) :: file )
		read(unit=file_unit, iostat=iostat) file
		close(file_unit)

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		if ( header ) then
			do i = 1, file_length
				if ( file(i:i) == NL ) then
					file = file(i+1:)
					file_length = len(file)
					exit
				else if ( i == file_length ) then
					file = file//LF
					file_length = file_length + 1
					write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
									file_name//'". File has one line.'
				end if
			end do

			if ( file_length == 0 ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
				return
			end if
		end if

		n_rows = 0

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				n_rows = n_rows + 1
			else if ( i == file_length ) then
				file = file//LF
				file_length = file_length + 1
				n_rows = n_rows + 1
			end if
		end do

		if ( fmt == 'z' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', &
									'a', 'b', 'c', 'd', 'e', 'f']
		else
			if ( locale == 'US' ) then
				decimal = 'POINT'
				non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', 'e', 'E', '+', '-']
			else if ( locale == 'EU' ) then
				decimal = 'COMMA'
				non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ',', 'e', 'E', '+', '-']
			end if
		end if

		prev_char = '0'
		n_columns = 0

		do i = 1, file_length
			current_char = file(i:i)

			if ( any(non_separating_chars == current_char) ) then
				prev_char = current_char
			else
				if ( any(non_separating_chars == prev_char) ) then
					prev_char = current_char
					n_columns = n_columns + 1
				else
					prev_char = current_char
				end if
			end if

			if ( current_char == NL ) exit
		end do

		allocate( lines(n_rows) )

		row = 1
		l1 = 1

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				lines(row)%s = file(l1:i)
				if ( row /= n_rows ) then
					row = row + 1
					l1 = i + 1
				else
					exit
				end if
			end if
		end do

		deallocate(file)

		allocate( into(n_rows, n_columns) )

		do concurrent (row = 1:n_rows)
			column = 1
			l1 = 1
			l2 = 1
			read_into: do i = 1, len(lines(row)%s)
				if ( any(non_separating_chars == lines(row)%s(i:i)) ) then
					if ( .not. any(non_separating_chars == lines(row)%s(l2:l2)) ) l1 = i
					l2 = i
				else
					if ( any(non_separating_chars == lines(row)%s(l2:l2)) ) then
						if ( fmt == 'z' ) then
							read(unit=lines(row)%s(l1:l2), fmt='(z'//str(l2-l1+1)//')') into(row, column)
						else
							read(unit=lines(row)%s(l1:l2), fmt=*, decimal=decimal) into(row, column)
						end if
						if ( column /= n_columns ) then
							column = column + 1
						else
							exit read_into
						end if
						l2 = i
					else
						l2 = i
					end if
				end if
			end do read_into
		end do
	end procedure from_text_2dr32

	module procedure from_text_1di64
		logical :: exists
		integer :: file_unit, iostat
		integer :: n_rows, n_columns
		integer :: i, ind, l1, l2

		character(len=:), allocatable, dimension(:) :: non_separating_chars
		character(len=:), allocatable :: file
		character(len=1) :: prev_char, current_char
		integer(int64) :: file_length

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		inquire( file=file_name, size=file_length )

		if ( file_length == 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty.'
			return
		end if

		allocate( character(len=file_length) :: file )
		read(unit=file_unit, iostat=iostat) file
		close(file_unit)

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		if ( header ) then
			do i = 1, file_length
				if ( file(i:i) == NL ) then
					file = file(i+1:)
					file_length = len(file)
					exit
				else if ( i == file_length ) then
					file = file//LF
					file_length = file_length + 1
					write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
									file_name//'". File has one line.'
				end if
			end do

			if ( file_length == 0 ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
				return
			end if
		end if

		n_rows = 0

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				n_rows = n_rows + 1
			else if ( i == file_length ) then
				file = file//LF
				file_length = file_length + 1
				n_rows = n_rows + 1
			end if
		end do

		if ( fmt == 'i' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '-']
		else if ( fmt == 'z' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', &
									'a', 'b', 'c', 'd', 'e', 'f']
		end if

		prev_char = '0'
		n_columns = 0

		do i = 1, file_length
			current_char = file(i:i)

			if ( any(non_separating_chars == current_char) ) then
				prev_char = current_char
			else
				if ( any(non_separating_chars == prev_char) ) then
					prev_char = current_char
					n_columns = n_columns + 1
				else
					prev_char = current_char
				end if
			end if

			if ( current_char == NL ) exit
		end do

		if ( (n_rows > 1) .and. (n_columns > 1) ) then
			error stop LF//'Error reading file "'//file_name//'". File data cannot fit into one-dimensional array.'
			return
		else if ( n_columns == 1 ) then
			allocate( into(n_rows) )
		else if ( n_rows == 1 ) then
			allocate( into(n_columns) )
		end if

		ind = 1
		l1 = 1
		l2 = 1

		read_into: do i = 1, file_length
			if ( any(non_separating_chars == file(i:i)) ) then
				if ( .not. any(non_separating_chars == file(l2:l2)) ) l1 = i
				l2 = i
			else
				if ( any(non_separating_chars == file(l2:l2)) ) then
					if ( fmt == 'z' ) then
						read(unit=file(l1:l2), fmt='(z'//str(l2-l1+1)//')') into(ind)
					else
						read(unit=file(l1:l2), fmt=*) into(ind)
					end if
					if ( ind /= size(into) ) then
						ind = ind + 1
					else
						exit read_into
					end if
					l2 = i
				else
					l2 = i
				end if
			end if
		end do read_into
	end procedure from_text_1di64
	module procedure from_text_1di32
		logical :: exists
		integer :: file_unit, iostat
		integer :: n_rows, n_columns
		integer :: i, ind, l1, l2

		character(len=:), allocatable, dimension(:) :: non_separating_chars
		character(len=:), allocatable :: file
		character(len=1) :: prev_char, current_char
		integer(int64) :: file_length

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		inquire( file=file_name, size=file_length )

		if ( file_length == 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty.'
			return
		end if

		allocate( character(len=file_length) :: file )
		read(unit=file_unit, iostat=iostat) file
		close(file_unit)

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		if ( header ) then
			do i = 1, file_length
				if ( file(i:i) == NL ) then
					file = file(i+1:)
					file_length = len(file)
					exit
				else if ( i == file_length ) then
					file = file//LF
					file_length = file_length + 1
					write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
									file_name//'". File has one line.'
				end if
			end do

			if ( file_length == 0 ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
				return
			end if
		end if

		n_rows = 0

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				n_rows = n_rows + 1
			else if ( i == file_length ) then
				file = file//LF
				file_length = file_length + 1
				n_rows = n_rows + 1
			end if
		end do

		if ( fmt == 'i' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '-']
		else if ( fmt == 'z' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', &
									'a', 'b', 'c', 'd', 'e', 'f']
		end if

		prev_char = '0'
		n_columns = 0

		do i = 1, file_length
			current_char = file(i:i)

			if ( any(non_separating_chars == current_char) ) then
				prev_char = current_char
			else
				if ( any(non_separating_chars == prev_char) ) then
					prev_char = current_char
					n_columns = n_columns + 1
				else
					prev_char = current_char
				end if
			end if

			if ( current_char == NL ) exit
		end do

		if ( (n_rows > 1) .and. (n_columns > 1) ) then
			error stop LF//'Error reading file "'//file_name//'". File data cannot fit into one-dimensional array.'
			return
		else if ( n_columns == 1 ) then
			allocate( into(n_rows) )
		else if ( n_rows == 1 ) then
			allocate( into(n_columns) )
		end if

		ind = 1
		l1 = 1
		l2 = 1

		read_into: do i = 1, file_length
			if ( any(non_separating_chars == file(i:i)) ) then
				if ( .not. any(non_separating_chars == file(l2:l2)) ) l1 = i
				l2 = i
			else
				if ( any(non_separating_chars == file(l2:l2)) ) then
					if ( fmt == 'z' ) then
						read(unit=file(l1:l2), fmt='(z'//str(l2-l1+1)//')') into(ind)
					else
						read(unit=file(l1:l2), fmt=*) into(ind)
					end if
					if ( ind /= size(into) ) then
						ind = ind + 1
					else
						exit read_into
					end if
					l2 = i
				else
					l2 = i
				end if
			end if
		end do read_into
	end procedure from_text_1di32
	module procedure from_text_1di16
		logical :: exists
		integer :: file_unit, iostat
		integer :: n_rows, n_columns
		integer :: i, ind, l1, l2

		character(len=:), allocatable, dimension(:) :: non_separating_chars
		character(len=:), allocatable :: file
		character(len=1) :: prev_char, current_char
		integer(int64) :: file_length

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		inquire( file=file_name, size=file_length )

		if ( file_length == 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty.'
			return
		end if

		allocate( character(len=file_length) :: file )
		read(unit=file_unit, iostat=iostat) file
		close(file_unit)

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		if ( header ) then
			do i = 1, file_length
				if ( file(i:i) == NL ) then
					file = file(i+1:)
					file_length = len(file)
					exit
				else if ( i == file_length ) then
					file = file//LF
					file_length = file_length + 1
					write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
									file_name//'". File has one line.'
				end if
			end do

			if ( file_length == 0 ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
				return
			end if
		end if

		n_rows = 0

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				n_rows = n_rows + 1
			else if ( i == file_length ) then
				file = file//LF
				file_length = file_length + 1
				n_rows = n_rows + 1
			end if
		end do

		if ( fmt == 'i' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '-']
		else if ( fmt == 'z' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', &
									'a', 'b', 'c', 'd', 'e', 'f']
		end if

		prev_char = '0'
		n_columns = 0

		do i = 1, file_length
			current_char = file(i:i)

			if ( any(non_separating_chars == current_char) ) then
				prev_char = current_char
			else
				if ( any(non_separating_chars == prev_char) ) then
					prev_char = current_char
					n_columns = n_columns + 1
				else
					prev_char = current_char
				end if
			end if

			if ( current_char == NL ) exit
		end do

		if ( (n_rows > 1) .and. (n_columns > 1) ) then
			error stop LF//'Error reading file "'//file_name//'". File data cannot fit into one-dimensional array.'
			return
		else if ( n_columns == 1 ) then
			allocate( into(n_rows) )
		else if ( n_rows == 1 ) then
			allocate( into(n_columns) )
		end if

		ind = 1
		l1 = 1
		l2 = 1

		read_into: do i = 1, file_length
			if ( any(non_separating_chars == file(i:i)) ) then
				if ( .not. any(non_separating_chars == file(l2:l2)) ) l1 = i
				l2 = i
			else
				if ( any(non_separating_chars == file(l2:l2)) ) then
					if ( fmt == 'z' ) then
						read(unit=file(l1:l2), fmt='(z'//str(l2-l1+1)//')') into(ind)
					else
						read(unit=file(l1:l2), fmt=*) into(ind)
					end if
					if ( ind /= size(into) ) then
						ind = ind + 1
					else
						exit read_into
					end if
					l2 = i
				else
					l2 = i
				end if
			end if
		end do read_into
	end procedure from_text_1di16
	module procedure from_text_1di8
		logical :: exists
		integer :: file_unit, iostat
		integer :: n_rows, n_columns
		integer :: i, ind, l1, l2

		character(len=:), allocatable, dimension(:) :: non_separating_chars
		character(len=:), allocatable :: file
		character(len=1) :: prev_char, current_char
		integer(int64) :: file_length

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		inquire( file=file_name, size=file_length )

		if ( file_length == 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty.'
			return
		end if

		allocate( character(len=file_length) :: file )
		read(unit=file_unit, iostat=iostat) file
		close(file_unit)

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		if ( header ) then
			do i = 1, file_length
				if ( file(i:i) == NL ) then
					file = file(i+1:)
					file_length = len(file)
					exit
				else if ( i == file_length ) then
					file = file//LF
					file_length = file_length + 1
					write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
									file_name//'". File has one line.'
				end if
			end do

			if ( file_length == 0 ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
				return
			end if
		end if

		n_rows = 0

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				n_rows = n_rows + 1
			else if ( i == file_length ) then
				file = file//LF
				file_length = file_length + 1
				n_rows = n_rows + 1
			end if
		end do

		if ( fmt == 'i' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '-']
		else if ( fmt == 'z' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', &
									'a', 'b', 'c', 'd', 'e', 'f']
		end if

		prev_char = '0'
		n_columns = 0

		do i = 1, file_length
			current_char = file(i:i)

			if ( any(non_separating_chars == current_char) ) then
				prev_char = current_char
			else
				if ( any(non_separating_chars == prev_char) ) then
					prev_char = current_char
					n_columns = n_columns + 1
				else
					prev_char = current_char
				end if
			end if

			if ( current_char == NL ) exit
		end do

		if ( (n_rows > 1) .and. (n_columns > 1) ) then
			error stop LF//'Error reading file "'//file_name//'". File data cannot fit into one-dimensional array.'
			return
		else if ( n_columns == 1 ) then
			allocate( into(n_rows) )
		else if ( n_rows == 1 ) then
			allocate( into(n_columns) )
		end if

		ind = 1
		l1 = 1
		l2 = 1

		read_into: do i = 1, file_length
			if ( any(non_separating_chars == file(i:i)) ) then
				if ( .not. any(non_separating_chars == file(l2:l2)) ) l1 = i
				l2 = i
			else
				if ( any(non_separating_chars == file(l2:l2)) ) then
					if ( fmt == 'z' ) then
						read(unit=file(l1:l2), fmt='(z'//str(l2-l1+1)//')') into(ind)
					else
						read(unit=file(l1:l2), fmt=*) into(ind)
					end if
					if ( ind /= size(into) ) then
						ind = ind + 1
					else
						exit read_into
					end if
					l2 = i
				else
					l2 = i
				end if
			end if
		end do read_into
	end procedure from_text_1di8

	module procedure from_text_2di64
		logical :: exists
		integer :: file_unit, iostat
		integer :: n_rows, n_columns
		integer :: i, row, column, l1, l2

		type(String), allocatable, dimension(:) :: lines
		character(len=:), allocatable, dimension(:) :: non_separating_chars
		character(len=:), allocatable :: file
		character(len=1) :: prev_char, current_char
		integer(int64) :: file_length

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		inquire( file=file_name, size=file_length )

		if ( file_length == 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty.'
			return
		end if

		allocate( character(len=file_length) :: file )
		read(unit=file_unit, iostat=iostat) file
		close(file_unit)

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		if ( header ) then
			do i = 1, file_length
				if ( file(i:i) == NL ) then
					file = file(i+1:)
					file_length = len(file)
					exit
				else if ( i == file_length ) then
					file = file//LF
					file_length = file_length + 1
					write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
									file_name//'". File has one line.'
				end if
			end do

			if ( file_length == 0 ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
				return
			end if
		end if

		n_rows = 0

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				n_rows = n_rows + 1
			else if ( i == file_length ) then
				file = file//LF
				file_length = file_length + 1
				n_rows = n_rows + 1
			end if
		end do

		if ( fmt == 'i' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '-']
		else if ( fmt == 'z' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', &
									'a', 'b', 'c', 'd', 'e', 'f']
		end if

		prev_char = '0'
		n_columns = 0

		do i = 1, file_length
			current_char = file(i:i)

			if ( any(non_separating_chars == current_char) ) then
				prev_char = current_char
			else
				if ( any(non_separating_chars == prev_char) ) then
					prev_char = current_char
					n_columns = n_columns + 1
				else
					prev_char = current_char
				end if
			end if

			if ( current_char == NL ) exit
		end do

		allocate( lines(n_rows) )

		row = 1
		l1 = 1

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				lines(row)%s = file(l1:i)
				if ( row /= n_rows ) then
					row = row + 1
					l1 = i + 1
				else
					exit
				end if
			end if
		end do

		deallocate(file)

		allocate( into(n_rows, n_columns) )

		do concurrent (row = 1:n_rows)
			column = 1
			l1 = 1
			l2 = 1
			read_into: do i = 1, len(lines(row)%s)
				if ( any(non_separating_chars == lines(row)%s(i:i)) ) then
					if ( .not. any(non_separating_chars == lines(row)%s(l2:l2)) ) l1 = i
					l2 = i
				else
					if ( any(non_separating_chars == lines(row)%s(l2:l2)) ) then
						if ( fmt == 'z' ) then
							read(unit=lines(row)%s(l1:l2), fmt='(z'//str(l2-l1+1)//')') into(row, column)
						else
							read(unit=lines(row)%s(l1:l2), fmt=*) into(row, column)
						end if
						if ( column /= n_columns ) then
							column = column + 1
						else
							exit read_into
						end if
						l2 = i
					else
						l2 = i
					end if
				end if
			end do read_into
		end do
	end procedure from_text_2di64
	module procedure from_text_2di32
		logical :: exists
		integer :: file_unit, iostat
		integer :: n_rows, n_columns
		integer :: i, row, column, l1, l2

		type(String), allocatable, dimension(:) :: lines
		character(len=:), allocatable, dimension(:) :: non_separating_chars
		character(len=:), allocatable :: file
		character(len=1) :: prev_char, current_char
		integer(int64) :: file_length

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		inquire( file=file_name, size=file_length )

		if ( file_length == 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty.'
			return
		end if

		allocate( character(len=file_length) :: file )
		read(unit=file_unit, iostat=iostat) file
		close(file_unit)

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		if ( header ) then
			do i = 1, file_length
				if ( file(i:i) == NL ) then
					file = file(i+1:)
					file_length = len(file)
					exit
				else if ( i == file_length ) then
					file = file//LF
					file_length = file_length + 1
					write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
									file_name//'". File has one line.'
				end if
			end do

			if ( file_length == 0 ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
				return
			end if
		end if

		n_rows = 0

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				n_rows = n_rows + 1
			else if ( i == file_length ) then
				file = file//LF
				file_length = file_length + 1
				n_rows = n_rows + 1
			end if
		end do

		if ( fmt == 'i' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '-']
		else if ( fmt == 'z' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', &
									'a', 'b', 'c', 'd', 'e', 'f']
		end if

		prev_char = '0'
		n_columns = 0

		do i = 1, file_length
			current_char = file(i:i)

			if ( any(non_separating_chars == current_char) ) then
				prev_char = current_char
			else
				if ( any(non_separating_chars == prev_char) ) then
					prev_char = current_char
					n_columns = n_columns + 1
				else
					prev_char = current_char
				end if
			end if

			if ( current_char == NL ) exit
		end do

		allocate( lines(n_rows) )

		row = 1
		l1 = 1

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				lines(row)%s = file(l1:i)
				if ( row /= n_rows ) then
					row = row + 1
					l1 = i + 1
				else
					exit
				end if
			end if
		end do

		deallocate(file)

		allocate( into(n_rows, n_columns) )

		do concurrent (row = 1:n_rows)
			column = 1
			l1 = 1
			l2 = 1
			read_into: do i = 1, len(lines(row)%s)
				if ( any(non_separating_chars == lines(row)%s(i:i)) ) then
					if ( .not. any(non_separating_chars == lines(row)%s(l2:l2)) ) l1 = i
					l2 = i
				else
					if ( any(non_separating_chars == lines(row)%s(l2:l2)) ) then
						if ( fmt == 'z' ) then
							read(unit=lines(row)%s(l1:l2), fmt='(z'//str(l2-l1+1)//')') into(row, column)
						else
							read(unit=lines(row)%s(l1:l2), fmt=*) into(row, column)
						end if
						if ( column /= n_columns ) then
							column = column + 1
						else
							exit read_into
						end if
						l2 = i
					else
						l2 = i
					end if
				end if
			end do read_into
		end do
	end procedure from_text_2di32
	module procedure from_text_2di16
		logical :: exists
		integer :: file_unit, iostat
		integer :: n_rows, n_columns
		integer :: i, row, column, l1, l2

		type(String), allocatable, dimension(:) :: lines
		character(len=:), allocatable, dimension(:) :: non_separating_chars
		character(len=:), allocatable :: file
		character(len=1) :: prev_char, current_char
		integer(int64) :: file_length

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		inquire( file=file_name, size=file_length )

		if ( file_length == 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty.'
			return
		end if

		allocate( character(len=file_length) :: file )
		read(unit=file_unit, iostat=iostat) file
		close(file_unit)

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		if ( header ) then
			do i = 1, file_length
				if ( file(i:i) == NL ) then
					file = file(i+1:)
					file_length = len(file)
					exit
				else if ( i == file_length ) then
					file = file//LF
					file_length = file_length + 1
					write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
									file_name//'". File has one line.'
				end if
			end do

			if ( file_length == 0 ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
				return
			end if
		end if

		n_rows = 0

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				n_rows = n_rows + 1
			else if ( i == file_length ) then
				file = file//LF
				file_length = file_length + 1
				n_rows = n_rows + 1
			end if
		end do

		if ( fmt == 'i' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '-']
		else if ( fmt == 'z' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', &
									'a', 'b', 'c', 'd', 'e', 'f']
		end if

		prev_char = '0'
		n_columns = 0

		do i = 1, file_length
			current_char = file(i:i)

			if ( any(non_separating_chars == current_char) ) then
				prev_char = current_char
			else
				if ( any(non_separating_chars == prev_char) ) then
					prev_char = current_char
					n_columns = n_columns + 1
				else
					prev_char = current_char
				end if
			end if

			if ( current_char == NL ) exit
		end do

		allocate( lines(n_rows) )

		row = 1
		l1 = 1

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				lines(row)%s = file(l1:i)
				if ( row /= n_rows ) then
					row = row + 1
					l1 = i + 1
				else
					exit
				end if
			end if
		end do

		deallocate(file)

		allocate( into(n_rows, n_columns) )

		do concurrent (row = 1:n_rows)
			column = 1
			l1 = 1
			l2 = 1
			read_into: do i = 1, len(lines(row)%s)
				if ( any(non_separating_chars == lines(row)%s(i:i)) ) then
					if ( .not. any(non_separating_chars == lines(row)%s(l2:l2)) ) l1 = i
					l2 = i
				else
					if ( any(non_separating_chars == lines(row)%s(l2:l2)) ) then
						if ( fmt == 'z' ) then
							read(unit=lines(row)%s(l1:l2), fmt='(z'//str(l2-l1+1)//')') into(row, column)
						else
							read(unit=lines(row)%s(l1:l2), fmt=*) into(row, column)
						end if
						if ( column /= n_columns ) then
							column = column + 1
						else
							exit read_into
						end if
						l2 = i
					else
						l2 = i
					end if
				end if
			end do read_into
		end do
	end procedure from_text_2di16
	module procedure from_text_2di8
		logical :: exists
		integer :: file_unit, iostat
		integer :: n_rows, n_columns
		integer :: i, row, column, l1, l2

		type(String), allocatable, dimension(:) :: lines
		character(len=:), allocatable, dimension(:) :: non_separating_chars
		character(len=:), allocatable :: file
		character(len=1) :: prev_char, current_char
		integer(int64) :: file_length

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		inquire( file=file_name, size=file_length )

		if ( file_length == 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty.'
			return
		end if

		allocate( character(len=file_length) :: file )
		read(unit=file_unit, iostat=iostat) file
		close(file_unit)

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		if ( header ) then
			do i = 1, file_length
				if ( file(i:i) == NL ) then
					file = file(i+1:)
					file_length = len(file)
					exit
				else if ( i == file_length ) then
					file = file//LF
					file_length = file_length + 1
					write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
									file_name//'". File has one line.'
				end if
			end do

			if ( file_length == 0 ) then
				error stop LF//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
				return
			end if
		end if

		n_rows = 0

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				n_rows = n_rows + 1
			else if ( i == file_length ) then
				file = file//LF
				file_length = file_length + 1
				n_rows = n_rows + 1
			end if
		end do

		if ( fmt == 'i' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '-']
		else if ( fmt == 'z' ) then
			non_separating_chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', &
									'a', 'b', 'c', 'd', 'e', 'f']
		end if

		prev_char = '0'
		n_columns = 0

		do i = 1, file_length
			current_char = file(i:i)

			if ( any(non_separating_chars == current_char) ) then
				prev_char = current_char
			else
				if ( any(non_separating_chars == prev_char) ) then
					prev_char = current_char
					n_columns = n_columns + 1
				else
					prev_char = current_char
				end if
			end if

			if ( current_char == NL ) exit
		end do

		allocate( lines(n_rows) )

		row = 1
		l1 = 1

		do i = 1, file_length
			if ( file(i:i) == NL ) then
				lines(row)%s = file(l1:i)
				if ( row /= n_rows ) then
					row = row + 1
					l1 = i + 1
				else
					exit
				end if
			end if
		end do

		deallocate(file)

		allocate( into(n_rows, n_columns) )

		do concurrent (row = 1:n_rows)
			column = 1
			l1 = 1
			l2 = 1
			read_into: do i = 1, len(lines(row)%s)
				if ( any(non_separating_chars == lines(row)%s(i:i)) ) then
					if ( .not. any(non_separating_chars == lines(row)%s(l2:l2)) ) l1 = i
					l2 = i
				else
					if ( any(non_separating_chars == lines(row)%s(l2:l2)) ) then
						if ( fmt == 'z' ) then
							read(unit=lines(row)%s(l1:l2), fmt='(z'//str(l2-l1+1)//')') into(row, column)
						else
							read(unit=lines(row)%s(l1:l2), fmt=*) into(row, column)
						end if
						if ( column /= n_columns ) then
							column = column + 1
						else
							exit read_into
						end if
						l2 = i
					else
						l2 = i
					end if
				end if
			end do read_into
		end do
	end procedure from_text_2di8
end submodule text_io

submodule (io_fortran_lib) binary_io
	!! This submodule provides module procedure implementations for the **private interfaces** `to_binary` and
	!! `from_binary`.
	contains
	! Writing Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	module procedure to_binary_1dc128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_1dc128
	module procedure to_binary_1dc64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_1dc64
	module procedure to_binary_1dc32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_1dc32

	module procedure to_binary_2dc128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_2dc128
	module procedure to_binary_2dc64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_2dc64
	module procedure to_binary_2dc32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_2dc32

	module procedure to_binary_3dc128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_3dc128
	module procedure to_binary_3dc64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_3dc64
	module procedure to_binary_3dc32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_3dc32

	module procedure to_binary_4dc128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_4dc128
	module procedure to_binary_4dc64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_4dc64
	module procedure to_binary_4dc32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_4dc32

	module procedure to_binary_5dc128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_5dc128
	module procedure to_binary_5dc64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_5dc64
	module procedure to_binary_5dc32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_5dc32

	module procedure to_binary_6dc128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_6dc128
	module procedure to_binary_6dc64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_6dc64
	module procedure to_binary_6dc32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_6dc32

	module procedure to_binary_7dc128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_7dc128
	module procedure to_binary_7dc64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_7dc64
	module procedure to_binary_7dc32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_7dc32

	module procedure to_binary_8dc128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_8dc128
	module procedure to_binary_8dc64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_8dc64
	module procedure to_binary_8dc32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_8dc32

	module procedure to_binary_9dc128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_9dc128
	module procedure to_binary_9dc64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_9dc64
	module procedure to_binary_9dc32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_9dc32

	module procedure to_binary_10dc128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_10dc128
	module procedure to_binary_10dc64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_10dc64
	module procedure to_binary_10dc32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_10dc32

	module procedure to_binary_11dc128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_11dc128
	module procedure to_binary_11dc64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_11dc64
	module procedure to_binary_11dc32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_11dc32

	module procedure to_binary_12dc128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_12dc128
	module procedure to_binary_12dc64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_12dc64
	module procedure to_binary_12dc32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_12dc32

	module procedure to_binary_13dc128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_13dc128
	module procedure to_binary_13dc64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_13dc64
	module procedure to_binary_13dc32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_13dc32

	module procedure to_binary_14dc128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_14dc128
	module procedure to_binary_14dc64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_14dc64
	module procedure to_binary_14dc32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_14dc32

	module procedure to_binary_15dc128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_15dc128
	module procedure to_binary_15dc64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_15dc64
	module procedure to_binary_15dc32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_15dc32

	module procedure to_binary_1dr128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_1dr128
	module procedure to_binary_1dr64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_1dr64
	module procedure to_binary_1dr32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_1dr32

	module procedure to_binary_2dr128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_2dr128
	module procedure to_binary_2dr64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_2dr64
	module procedure to_binary_2dr32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_2dr32

	module procedure to_binary_3dr128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_3dr128
	module procedure to_binary_3dr64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_3dr64
	module procedure to_binary_3dr32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_3dr32

	module procedure to_binary_4dr128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_4dr128
	module procedure to_binary_4dr64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_4dr64
	module procedure to_binary_4dr32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_4dr32

	module procedure to_binary_5dr128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_5dr128
	module procedure to_binary_5dr64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_5dr64
	module procedure to_binary_5dr32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_5dr32

	module procedure to_binary_6dr128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_6dr128
	module procedure to_binary_6dr64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_6dr64
	module procedure to_binary_6dr32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_6dr32

	module procedure to_binary_7dr128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_7dr128
	module procedure to_binary_7dr64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_7dr64
	module procedure to_binary_7dr32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_7dr32

	module procedure to_binary_8dr128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_8dr128
	module procedure to_binary_8dr64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_8dr64
	module procedure to_binary_8dr32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_8dr32

	module procedure to_binary_9dr128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_9dr128
	module procedure to_binary_9dr64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_9dr64
	module procedure to_binary_9dr32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_9dr32

	module procedure to_binary_10dr128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_10dr128
	module procedure to_binary_10dr64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_10dr64
	module procedure to_binary_10dr32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_10dr32

	module procedure to_binary_11dr128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_11dr128
	module procedure to_binary_11dr64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_11dr64
	module procedure to_binary_11dr32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_11dr32

	module procedure to_binary_12dr128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_12dr128
	module procedure to_binary_12dr64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_12dr64
	module procedure to_binary_12dr32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_12dr32

	module procedure to_binary_13dr128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_13dr128
	module procedure to_binary_13dr64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_13dr64
	module procedure to_binary_13dr32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_13dr32

	module procedure to_binary_14dr128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_14dr128
	module procedure to_binary_14dr64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_14dr64
	module procedure to_binary_14dr32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_14dr32

	module procedure to_binary_15dr128
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_15dr128
	module procedure to_binary_15dr64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_15dr64
	module procedure to_binary_15dr32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_15dr32

	module procedure to_binary_1di64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_1di64
	module procedure to_binary_1di32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_1di32
	module procedure to_binary_1di16
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_1di16
	module procedure to_binary_1di8
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_1di8

	module procedure to_binary_2di64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_2di64
	module procedure to_binary_2di32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_2di32
	module procedure to_binary_2di16
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_2di16
	module procedure to_binary_2di8
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_2di8

	module procedure to_binary_3di64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_3di64
	module procedure to_binary_3di32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_3di32
	module procedure to_binary_3di16
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_3di16
	module procedure to_binary_3di8
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_3di8

	module procedure to_binary_4di64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_4di64
	module procedure to_binary_4di32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_4di32
	module procedure to_binary_4di16
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_4di16
	module procedure to_binary_4di8
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_4di8

	module procedure to_binary_5di64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_5di64
	module procedure to_binary_5di32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_5di32
	module procedure to_binary_5di16
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_5di16
	module procedure to_binary_5di8
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_5di8

	module procedure to_binary_6di64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_6di64
	module procedure to_binary_6di32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_6di32
	module procedure to_binary_6di16
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_6di16
	module procedure to_binary_6di8
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_6di8

	module procedure to_binary_7di64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_7di64
	module procedure to_binary_7di32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_7di32
	module procedure to_binary_7di16
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_7di16
	module procedure to_binary_7di8
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_7di8

	module procedure to_binary_8di64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_8di64
	module procedure to_binary_8di32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_8di32
	module procedure to_binary_8di16
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_8di16
	module procedure to_binary_8di8
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_8di8

	module procedure to_binary_9di64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_9di64
	module procedure to_binary_9di32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_9di32
	module procedure to_binary_9di16
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_9di16
	module procedure to_binary_9di8
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_9di8

	module procedure to_binary_10di64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_10di64
	module procedure to_binary_10di32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_10di32
	module procedure to_binary_10di16
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_10di16
	module procedure to_binary_10di8
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_10di8

	module procedure to_binary_11di64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_11di64
	module procedure to_binary_11di32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_11di32
	module procedure to_binary_11di16
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_11di16
	module procedure to_binary_11di8
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_11di8

	module procedure to_binary_12di64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_12di64
	module procedure to_binary_12di32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_12di32
	module procedure to_binary_12di16
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_12di16
	module procedure to_binary_12di8
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_12di8

	module procedure to_binary_13di64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_13di64
	module procedure to_binary_13di32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_13di32
	module procedure to_binary_13di16
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_13di16
	module procedure to_binary_13di8
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_13di8

	module procedure to_binary_14di64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_14di64
	module procedure to_binary_14di32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_14di32
	module procedure to_binary_14di16
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_14di16
	module procedure to_binary_14di8
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_14di8

	module procedure to_binary_15di64
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_15di64
	module procedure to_binary_15di32
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_15di32
	module procedure to_binary_15di16
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_15di16
	module procedure to_binary_15di8
		logical :: exists
		integer :: file_unit

		inquire( file=file_name, exist=exists )

		file_unit = output_unit

		if ( .not. exists ) then
			open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
				  action='write', access='stream' )
		else
			open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
				  action='write', access='stream' )
		end if

		write(unit=file_unit) x

		close(file_unit)
	end procedure to_binary_15di8

	! Reading Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	module procedure from_binary_1dc128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_1dc128
	module procedure from_binary_1dc64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_1dc64
	module procedure from_binary_1dc32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_1dc32

	module procedure from_binary_2dc128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_2dc128
	module procedure from_binary_2dc64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_2dc64
	module procedure from_binary_2dc32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_2dc32

	module procedure from_binary_3dc128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_3dc128
	module procedure from_binary_3dc64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_3dc64
	module procedure from_binary_3dc32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_3dc32

	module procedure from_binary_4dc128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_4dc128
	module procedure from_binary_4dc64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_4dc64
	module procedure from_binary_4dc32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_4dc32

	module procedure from_binary_5dc128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_5dc128
	module procedure from_binary_5dc64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_5dc64
	module procedure from_binary_5dc32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_5dc32

	module procedure from_binary_6dc128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_6dc128
	module procedure from_binary_6dc64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_6dc64
	module procedure from_binary_6dc32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_6dc32

	module procedure from_binary_7dc128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_7dc128
	module procedure from_binary_7dc64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_7dc64
	module procedure from_binary_7dc32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_7dc32

	module procedure from_binary_8dc128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_8dc128
	module procedure from_binary_8dc64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_8dc64
	module procedure from_binary_8dc32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_8dc32

	module procedure from_binary_9dc128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_9dc128
	module procedure from_binary_9dc64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_9dc64
	module procedure from_binary_9dc32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_9dc32

	module procedure from_binary_10dc128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_10dc128
	module procedure from_binary_10dc64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_10dc64
	module procedure from_binary_10dc32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_10dc32

	module procedure from_binary_11dc128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_11dc128
	module procedure from_binary_11dc64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_11dc64
	module procedure from_binary_11dc32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_11dc32

	module procedure from_binary_12dc128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_12dc128
	module procedure from_binary_12dc64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_12dc64
	module procedure from_binary_12dc32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_12dc32

	module procedure from_binary_13dc128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_13dc128
	module procedure from_binary_13dc64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_13dc64
	module procedure from_binary_13dc32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_13dc32

	module procedure from_binary_14dc128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13), data_shape(14)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_14dc128
	module procedure from_binary_14dc64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13), data_shape(14)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_14dc64
	module procedure from_binary_14dc32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13), data_shape(14)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_14dc32

	module procedure from_binary_15dc128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13), data_shape(14), data_shape(15)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_15dc128
	module procedure from_binary_15dc64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13), data_shape(14), data_shape(15)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_15dc64
	module procedure from_binary_15dc32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13), data_shape(14), data_shape(15)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_15dc32

	module procedure from_binary_1dr128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_1dr128
	module procedure from_binary_1dr64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_1dr64
	module procedure from_binary_1dr32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_1dr32

	module procedure from_binary_2dr128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_2dr128
	module procedure from_binary_2dr64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_2dr64
	module procedure from_binary_2dr32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_2dr32

	module procedure from_binary_3dr128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_3dr128
	module procedure from_binary_3dr64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_3dr64
	module procedure from_binary_3dr32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_3dr32

	module procedure from_binary_4dr128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_4dr128
	module procedure from_binary_4dr64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_4dr64
	module procedure from_binary_4dr32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_4dr32

	module procedure from_binary_5dr128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_5dr128
	module procedure from_binary_5dr64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_5dr64
	module procedure from_binary_5dr32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_5dr32

	module procedure from_binary_6dr128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_6dr128
	module procedure from_binary_6dr64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_6dr64
	module procedure from_binary_6dr32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_6dr32

	module procedure from_binary_7dr128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_7dr128
	module procedure from_binary_7dr64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_7dr64
	module procedure from_binary_7dr32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_7dr32

	module procedure from_binary_8dr128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_8dr128
	module procedure from_binary_8dr64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_8dr64
	module procedure from_binary_8dr32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_8dr32

	module procedure from_binary_9dr128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_9dr128
	module procedure from_binary_9dr64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_9dr64
	module procedure from_binary_9dr32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_9dr32

	module procedure from_binary_10dr128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_10dr128
	module procedure from_binary_10dr64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_10dr64
	module procedure from_binary_10dr32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_10dr32

	module procedure from_binary_11dr128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_11dr128
	module procedure from_binary_11dr64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_11dr64
	module procedure from_binary_11dr32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_11dr32

	module procedure from_binary_12dr128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_12dr128
	module procedure from_binary_12dr64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_12dr64
	module procedure from_binary_12dr32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_12dr32

	module procedure from_binary_13dr128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_13dr128
	module procedure from_binary_13dr64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_13dr64
	module procedure from_binary_13dr32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_13dr32

	module procedure from_binary_14dr128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13), data_shape(14)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_14dr128
	module procedure from_binary_14dr64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13), data_shape(14)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_14dr64
	module procedure from_binary_14dr32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13), data_shape(14)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_14dr32

	module procedure from_binary_15dr128
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13), data_shape(14), data_shape(15)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_15dr128
	module procedure from_binary_15dr64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13), data_shape(14), data_shape(15)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_15dr64
	module procedure from_binary_15dr32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13), data_shape(14), data_shape(15)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_15dr32

	module procedure from_binary_1di64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_1di64
	module procedure from_binary_1di32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_1di32
	module procedure from_binary_1di16
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_1di16
	module procedure from_binary_1di8
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_1di8

	module procedure from_binary_2di64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_2di64
	module procedure from_binary_2di32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_2di32
	module procedure from_binary_2di16
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_2di16
	module procedure from_binary_2di8
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_2di8

	module procedure from_binary_3di64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_3di64
	module procedure from_binary_3di32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_3di32
	module procedure from_binary_3di16
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_3di16
	module procedure from_binary_3di8
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_3di8

	module procedure from_binary_4di64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_4di64
	module procedure from_binary_4di32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_4di32
	module procedure from_binary_4di16
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_4di16
	module procedure from_binary_4di8
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_4di8

	module procedure from_binary_5di64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_5di64
	module procedure from_binary_5di32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_5di32
	module procedure from_binary_5di16
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_5di16
	module procedure from_binary_5di8
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_5di8

	module procedure from_binary_6di64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_6di64
	module procedure from_binary_6di32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_6di32
	module procedure from_binary_6di16
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_6di16
	module procedure from_binary_6di8
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_6di8

	module procedure from_binary_7di64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_7di64
	module procedure from_binary_7di32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_7di32
	module procedure from_binary_7di16
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_7di16
	module procedure from_binary_7di8
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_7di8

	module procedure from_binary_8di64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_8di64
	module procedure from_binary_8di32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_8di32
	module procedure from_binary_8di16
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_8di16
	module procedure from_binary_8di8
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_8di8

	module procedure from_binary_9di64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_9di64
	module procedure from_binary_9di32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_9di32
	module procedure from_binary_9di16
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_9di16
	module procedure from_binary_9di8
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_9di8

	module procedure from_binary_10di64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_10di64
	module procedure from_binary_10di32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_10di32
	module procedure from_binary_10di16
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_10di16
	module procedure from_binary_10di8
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_10di8

	module procedure from_binary_11di64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_11di64
	module procedure from_binary_11di32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_11di32
	module procedure from_binary_11di16
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_11di16
	module procedure from_binary_11di8
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_11di8

	module procedure from_binary_12di64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_12di64
	module procedure from_binary_12di32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_12di32
	module procedure from_binary_12di16
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_12di16
	module procedure from_binary_12di8
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_12di8

	module procedure from_binary_13di64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_13di64
	module procedure from_binary_13di32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_13di32
	module procedure from_binary_13di16
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_13di16
	module procedure from_binary_13di8
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_13di8

	module procedure from_binary_14di64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13), data_shape(14)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_14di64
	module procedure from_binary_14di32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13), data_shape(14)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_14di32
	module procedure from_binary_14di16
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13), data_shape(14)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_14di16
	module procedure from_binary_14di8
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13), data_shape(14)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_14di8

	module procedure from_binary_15di64
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13), data_shape(14), data_shape(15)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_15di64
	module procedure from_binary_15di32
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13), data_shape(14), data_shape(15)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_15di32
	module procedure from_binary_15di16
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13), data_shape(14), data_shape(15)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_15di16
	module procedure from_binary_15di8
		logical :: exists
		integer :: file_unit, iostat

		inquire( file=file_name, exist=exists )

		file_unit = input_unit

		if ( exists ) then
			open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
				  action='read', access='stream', position='rewind' )
		else
			error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
			return
		end if

		allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
					   data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
					   data_shape(13), data_shape(14), data_shape(15)) )
		read(unit=file_unit, iostat=iostat) into

		if ( iostat > 0 ) then
			error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
			return
		end if

		close(file_unit)
	end procedure from_binary_15di8
end submodule binary_io

submodule (io_fortran_lib) array_printing
	!! This submodule provides module procedure implementations for the **public interface** `aprint`.
	contains
	module procedure aprint_1dc128
		character(len=:), allocatable, dimension(:) :: x_str
		character(len=:), allocatable :: fmt_, im_, xre_max_str, xre_min_str, xim_max_str, xim_min_str
		integer :: i, decimals_, l

		if ( .not. present(fmt) ) then
			fmt_ = 'f'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				write(*,'(a)') LF//'WARNING: Unknown format "'//fmt//'" for printing real array. Aborting...'
				return
			end if
		end if

		if ( .not. present(decimals) ) then
			decimals_ = 2
		else
			decimals_ = decimals
		end if

		if ( .not. present(im) ) then
			im_ = 'j'
		else
			im_ = trim(adjustl(im))
		end if

		if ( len(im_) > 0 ) then
			l = len(im_)
		else
			l = 3
		end if

		xre_max_str = str(maxval(x%re), fmt=fmt_, decimals=decimals_)
		xre_min_str = str(minval(x%re), fmt=fmt_, decimals=decimals_)
		xim_max_str = str(maxval(x%im), fmt=fmt_, decimals=decimals_)
		xim_min_str = str(minval(x%im), fmt=fmt_, decimals=decimals_)

		if ( len(xre_max_str) > len(xre_min_str) ) then
			l = l + len(xre_max_str)
		else
			l = l + len(xre_min_str)
		end if

		if ( len(xim_max_str) > len(xim_min_str) ) then
			l = l + len(xim_max_str)
		else
			l = l + len(xim_min_str)
		end if

		allocate( character(len=l) :: x_str(lbound(x, dim=1):ubound(x, dim=1)) )

		do concurrent (i = lbound(x, dim=1):ubound(x, dim=1))
			x_str(i) = str(x(i), fmt=fmt_, decimals=decimals_, im=im_)
		end do

		call aprint(x_str)
	end procedure aprint_1dc128
	module procedure aprint_1dc64
		character(len=:), allocatable, dimension(:) :: x_str
		character(len=:), allocatable :: fmt_, im_, xre_max_str, xre_min_str, xim_max_str, xim_min_str
		integer :: i, decimals_, l

		if ( .not. present(fmt) ) then
			fmt_ = 'f'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				write(*,'(a)') LF//'WARNING: Unknown format "'//fmt//'" for printing real array. Aborting...'
				return
			end if
		end if

		if ( .not. present(decimals) ) then
			decimals_ = 2
		else
			decimals_ = decimals
		end if

		if ( .not. present(im) ) then
			im_ = 'j'
		else
			im_ = trim(adjustl(im))
		end if

		if ( len(im_) > 0 ) then
			l = len(im_)
		else
			l = 3
		end if

		xre_max_str = str(maxval(x%re), fmt=fmt_, decimals=decimals_)
		xre_min_str = str(minval(x%re), fmt=fmt_, decimals=decimals_)
		xim_max_str = str(maxval(x%im), fmt=fmt_, decimals=decimals_)
		xim_min_str = str(minval(x%im), fmt=fmt_, decimals=decimals_)

		if ( len(xre_max_str) > len(xre_min_str) ) then
			l = l + len(xre_max_str)
		else
			l = l + len(xre_min_str)
		end if

		if ( len(xim_max_str) > len(xim_min_str) ) then
			l = l + len(xim_max_str)
		else
			l = l + len(xim_min_str)
		end if

		allocate( character(len=l) :: x_str(lbound(x, dim=1):ubound(x, dim=1)) )

		do concurrent (i = lbound(x, dim=1):ubound(x, dim=1))
			x_str(i) = str(x(i), fmt=fmt_, decimals=decimals_, im=im_)
		end do

		call aprint(x_str)
	end procedure aprint_1dc64
	module procedure aprint_1dc32
		character(len=:), allocatable, dimension(:) :: x_str
		character(len=:), allocatable :: fmt_, im_, xre_max_str, xre_min_str, xim_max_str, xim_min_str
		integer :: i, decimals_, l

		if ( .not. present(fmt) ) then
			fmt_ = 'f'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				write(*,'(a)') LF//'WARNING: Unknown format "'//fmt//'" for printing real array. Aborting...'
				return
			end if
		end if

		if ( .not. present(decimals) ) then
			decimals_ = 2
		else
			decimals_ = decimals
		end if

		if ( .not. present(im) ) then
			im_ = 'j'
		else
			im_ = trim(adjustl(im))
		end if

		if ( len(im_) > 0 ) then
			l = len(im_)
		else
			l = 3
		end if

		xre_max_str = str(maxval(x%re), fmt=fmt_, decimals=decimals_)
		xre_min_str = str(minval(x%re), fmt=fmt_, decimals=decimals_)
		xim_max_str = str(maxval(x%im), fmt=fmt_, decimals=decimals_)
		xim_min_str = str(minval(x%im), fmt=fmt_, decimals=decimals_)

		if ( len(xre_max_str) > len(xre_min_str) ) then
			l = l + len(xre_max_str)
		else
			l = l + len(xre_min_str)
		end if

		if ( len(xim_max_str) > len(xim_min_str) ) then
			l = l + len(xim_max_str)
		else
			l = l + len(xim_min_str)
		end if

		allocate( character(len=l) :: x_str(lbound(x, dim=1):ubound(x, dim=1)) )

		do concurrent (i = lbound(x, dim=1):ubound(x, dim=1))
			x_str(i) = str(x(i), fmt=fmt_, decimals=decimals_, im=im_)
		end do

		call aprint(x_str)
	end procedure aprint_1dc32

	module procedure aprint_2dc128
		character(len=:), allocatable, dimension(:,:) :: x_str
		character(len=:), allocatable :: fmt_, im_, xre_max_str, xre_min_str, xim_max_str, xim_min_str
		integer :: i, j, decimals_, l

		if ( .not. present(fmt) ) then
			fmt_ = 'f'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				write(*,'(a)') LF//'WARNING: Unknown format "'//fmt//'" for printing real array. Aborting...'
				return
			end if
		end if

		if ( .not. present(decimals) ) then
			decimals_ = 2
		else
			decimals_ = decimals
		end if

		if ( .not. present(im) ) then
			im_ = 'j'
		else
			im_ = trim(adjustl(im))
		end if

		if ( len(im_) > 0 ) then
			l = len(im_)
		else
			l = 3
		end if

		xre_max_str = str(maxval(x%re), fmt=fmt_, decimals=decimals_)
		xre_min_str = str(minval(x%re), fmt=fmt_, decimals=decimals_)
		xim_max_str = str(maxval(x%im), fmt=fmt_, decimals=decimals_)
		xim_min_str = str(minval(x%im), fmt=fmt_, decimals=decimals_)

		if ( len(xre_max_str) > len(xre_min_str) ) then
			l = l + len(xre_max_str)
		else
			l = l + len(xre_min_str)
		end if

		if ( len(xim_max_str) > len(xim_min_str) ) then
			l = l + len(xim_max_str)
		else
			l = l + len(xim_min_str)
		end if

		allocate( character(len=l) :: x_str(lbound(x, dim=1):ubound(x, dim=1), lbound(x, dim=2):ubound(x, dim=2)) )

		do concurrent (j = lbound(x, dim=2):ubound(x, dim=2), i = lbound(x, dim=1):ubound(x, dim=1))
			x_str(i,j) = str(x(i,j), fmt=fmt_, decimals=decimals_, im=im_)
		end do

		call aprint(x_str)
	end procedure aprint_2dc128
	module procedure aprint_2dc64
		character(len=:), allocatable, dimension(:,:) :: x_str
		character(len=:), allocatable :: fmt_, im_, xre_max_str, xre_min_str, xim_max_str, xim_min_str
		integer :: i, j, decimals_, l

		if ( .not. present(fmt) ) then
			fmt_ = 'f'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				write(*,'(a)') LF//'WARNING: Unknown format "'//fmt//'" for printing real array. Aborting...'
				return
			end if
		end if

		if ( .not. present(decimals) ) then
			decimals_ = 2
		else
			decimals_ = decimals
		end if

		if ( .not. present(im) ) then
			im_ = 'j'
		else
			im_ = trim(adjustl(im))
		end if

		if ( len(im_) > 0 ) then
			l = len(im_)
		else
			l = 3
		end if

		xre_max_str = str(maxval(x%re), fmt=fmt_, decimals=decimals_)
		xre_min_str = str(minval(x%re), fmt=fmt_, decimals=decimals_)
		xim_max_str = str(maxval(x%im), fmt=fmt_, decimals=decimals_)
		xim_min_str = str(minval(x%im), fmt=fmt_, decimals=decimals_)

		if ( len(xre_max_str) > len(xre_min_str) ) then
			l = l + len(xre_max_str)
		else
			l = l + len(xre_min_str)
		end if

		if ( len(xim_max_str) > len(xim_min_str) ) then
			l = l + len(xim_max_str)
		else
			l = l + len(xim_min_str)
		end if

		allocate( character(len=l) :: x_str(lbound(x, dim=1):ubound(x, dim=1), lbound(x, dim=2):ubound(x, dim=2)) )

		do concurrent (j = lbound(x, dim=2):ubound(x, dim=2), i = lbound(x, dim=1):ubound(x, dim=1))
			x_str(i,j) = str(x(i,j), fmt=fmt_, decimals=decimals_, im=im_)
		end do

		call aprint(x_str)
	end procedure aprint_2dc64
	module procedure aprint_2dc32
		character(len=:), allocatable, dimension(:,:) :: x_str
		character(len=:), allocatable :: fmt_, im_, xre_max_str, xre_min_str, xim_max_str, xim_min_str
		integer :: i, j, decimals_, l

		if ( .not. present(fmt) ) then
			fmt_ = 'f'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				write(*,'(a)') LF//'WARNING: Unknown format "'//fmt//'" for printing real array. Aborting...'
				return
			end if
		end if

		if ( .not. present(decimals) ) then
			decimals_ = 2
		else
			decimals_ = decimals
		end if

		if ( .not. present(im) ) then
			im_ = 'j'
		else
			im_ = trim(adjustl(im))
		end if

		if ( len(im_) > 0 ) then
			l = len(im_)
		else
			l = 3
		end if

		xre_max_str = str(maxval(x%re), fmt=fmt_, decimals=decimals_)
		xre_min_str = str(minval(x%re), fmt=fmt_, decimals=decimals_)
		xim_max_str = str(maxval(x%im), fmt=fmt_, decimals=decimals_)
		xim_min_str = str(minval(x%im), fmt=fmt_, decimals=decimals_)

		if ( len(xre_max_str) > len(xre_min_str) ) then
			l = l + len(xre_max_str)
		else
			l = l + len(xre_min_str)
		end if

		if ( len(xim_max_str) > len(xim_min_str) ) then
			l = l + len(xim_max_str)
		else
			l = l + len(xim_min_str)
		end if

		allocate( character(len=l) :: x_str(lbound(x, dim=1):ubound(x, dim=1), lbound(x, dim=2):ubound(x, dim=2)) )

		do concurrent (j = lbound(x, dim=2):ubound(x, dim=2), i = lbound(x, dim=1):ubound(x, dim=1))
			x_str(i,j) = str(x(i,j), fmt=fmt_, decimals=decimals_, im=im_)
		end do

		call aprint(x_str)
	end procedure aprint_2dc32

	module procedure aprint_1dr128
		character(len=:), allocatable, dimension(:) :: x_str
		character(len=:), allocatable :: fmt_, x_max_str, x_min_str, x_abs_min_str, source
		integer :: i, decimals_

		if ( .not. present(fmt) ) then
			fmt_ = 'f'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				write(*,'(a)') LF//'WARNING: Unknown format "'//fmt//'" for printing real array. Aborting...'
				return
			end if
		end if

		if ( .not. present(decimals) ) then
			decimals_ = 2
		else
			decimals_ = decimals
		end if

		x_max_str = str(maxval(x), fmt=fmt_, decimals=decimals_)
		x_min_str = str(minval(x), fmt=fmt_, decimals=decimals_)
		x_abs_min_str = str(minval(abs(x)), fmt=fmt_, decimals=decimals_)

		if ( len(x_max_str) > len(x_min_str) ) then
			source = x_max_str
		else
			source = x_min_str
		end if

		if ( len(x_abs_min_str) > len(source) ) source = x_abs_min_str

		allocate( x_str(lbound(x, dim=1):ubound(x, dim=1)), source=source )

		do concurrent (i = lbound(x, dim=1):ubound(x, dim=1))
			x_str(i) = str(x(i), fmt=fmt_, decimals=decimals_)
		end do

		call aprint(x_str)
	end procedure aprint_1dr128
	module procedure aprint_1dr64
		character(len=:), allocatable, dimension(:) :: x_str
		character(len=:), allocatable :: fmt_, x_max_str, x_min_str, x_abs_min_str, source
		integer :: i, decimals_

		if ( .not. present(fmt) ) then
			fmt_ = 'f'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				write(*,'(a)') LF//'WARNING: Unknown format "'//fmt//'" for printing real array. Aborting...'
				return
			end if
		end if

		if ( .not. present(decimals) ) then
			decimals_ = 2
		else
			decimals_ = decimals
		end if

		x_max_str = str(maxval(x), fmt=fmt_, decimals=decimals_)
		x_min_str = str(minval(x), fmt=fmt_, decimals=decimals_)
		x_abs_min_str = str(minval(abs(x)), fmt=fmt_, decimals=decimals_)

		if ( len(x_max_str) > len(x_min_str) ) then
			source = x_max_str
		else
			source = x_min_str
		end if

		if ( len(x_abs_min_str) > len(source) ) source = x_abs_min_str

		allocate( x_str(lbound(x, dim=1):ubound(x, dim=1)), source=source )

		do concurrent (i = lbound(x, dim=1):ubound(x, dim=1))
			x_str(i) = str(x(i), fmt=fmt_, decimals=decimals_)
		end do

		call aprint(x_str)
	end procedure aprint_1dr64
	module procedure aprint_1dr32
		character(len=:), allocatable, dimension(:) :: x_str
		character(len=:), allocatable :: fmt_, x_max_str, x_min_str, x_abs_min_str, source
		integer :: i, decimals_

		if ( .not. present(fmt) ) then
			fmt_ = 'f'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				write(*,'(a)') LF//'WARNING: Unknown format "'//fmt//'" for printing real array. Aborting...'
				return
			end if
		end if

		if ( .not. present(decimals) ) then
			decimals_ = 2
		else
			decimals_ = decimals
		end if

		x_max_str = str(maxval(x), fmt=fmt_, decimals=decimals_)
		x_min_str = str(minval(x), fmt=fmt_, decimals=decimals_)
		x_abs_min_str = str(minval(abs(x)), fmt=fmt_, decimals=decimals_)

		if ( len(x_max_str) > len(x_min_str) ) then
			source = x_max_str
		else
			source = x_min_str
		end if

		if ( len(x_abs_min_str) > len(source) ) source = x_abs_min_str

		allocate( x_str(lbound(x, dim=1):ubound(x, dim=1)), source=source )

		do concurrent (i = lbound(x, dim=1):ubound(x, dim=1))
			x_str(i) = str(x(i), fmt=fmt_, decimals=decimals_)
		end do

		call aprint(x_str)
	end procedure aprint_1dr32

	module procedure aprint_2dr128
		character(len=:), allocatable, dimension(:,:) :: x_str
		character(len=:), allocatable :: fmt_, x_max_str, x_min_str, x_abs_min_str, source
		integer :: i, j, decimals_

		if ( .not. present(fmt) ) then
			fmt_ = 'f'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				write(*,'(a)') LF//'WARNING: Unknown format "'//fmt//'" for printing real array. Aborting...'
				return
			end if
		end if

		if ( .not. present(decimals) ) then
			decimals_ = 2
		else
			decimals_ = decimals
		end if

		x_max_str = str(maxval(x), fmt=fmt_, decimals=decimals_)
		x_min_str = str(minval(x), fmt=fmt_, decimals=decimals_)
		x_abs_min_str = str(minval(abs(x)), fmt=fmt_, decimals=decimals_)

		if ( len(x_max_str) > len(x_min_str) ) then
			source = x_max_str
		else
			source = x_min_str
		end if

		if ( len(x_abs_min_str) > len(source) ) source = x_abs_min_str

		allocate( x_str(lbound(x, dim=1):ubound(x, dim=1), lbound(x, dim=2):ubound(x, dim=2)), source=source )

		do concurrent (j = lbound(x, dim=2):ubound(x, dim=2), i = lbound(x, dim=1):ubound(x, dim=1))
			x_str(i,j) = str(x(i,j), fmt=fmt_, decimals=decimals_)
		end do

		call aprint(x_str)
	end procedure aprint_2dr128
	module procedure aprint_2dr64
		character(len=:), allocatable, dimension(:,:) :: x_str
		character(len=:), allocatable :: fmt_, x_max_str, x_min_str, x_abs_min_str, source
		integer :: i, j, decimals_

		if ( .not. present(fmt) ) then
			fmt_ = 'f'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				write(*,'(a)') LF//'WARNING: Unknown format "'//fmt//'" for printing real array. Aborting...'
				return
			end if
		end if

		if ( .not. present(decimals) ) then
			decimals_ = 2
		else
			decimals_ = decimals
		end if

		x_max_str = str(maxval(x), fmt=fmt_, decimals=decimals_)
		x_min_str = str(minval(x), fmt=fmt_, decimals=decimals_)
		x_abs_min_str = str(minval(abs(x)), fmt=fmt_, decimals=decimals_)

		if ( len(x_max_str) > len(x_min_str) ) then
			source = x_max_str
		else
			source = x_min_str
		end if

		if ( len(x_abs_min_str) > len(source) ) source = x_abs_min_str

		allocate( x_str(lbound(x, dim=1):ubound(x, dim=1), lbound(x, dim=2):ubound(x, dim=2)), source=source )

		do concurrent (j = lbound(x, dim=2):ubound(x, dim=2), i = lbound(x, dim=1):ubound(x, dim=1))
			x_str(i,j) = str(x(i,j), fmt=fmt_, decimals=decimals_)
		end do

		call aprint(x_str)
	end procedure aprint_2dr64
	module procedure aprint_2dr32
		character(len=:), allocatable, dimension(:,:) :: x_str
		character(len=:), allocatable :: fmt_, x_max_str, x_min_str, x_abs_min_str, source
		integer :: i, j, decimals_

		if ( .not. present(fmt) ) then
			fmt_ = 'f'
		else
			if ( any(REAL_FMTS == fmt) ) then
				fmt_ = fmt
			else
				write(*,'(a)') LF//'WARNING: Unknown format "'//fmt//'" for printing real array. Aborting...'
				return
			end if
		end if

		if ( .not. present(decimals) ) then
			decimals_ = 2
		else
			decimals_ = decimals
		end if

		x_max_str = str(maxval(x), fmt=fmt_, decimals=decimals_)
		x_min_str = str(minval(x), fmt=fmt_, decimals=decimals_)
		x_abs_min_str = str(minval(abs(x)), fmt=fmt_, decimals=decimals_)

		if ( len(x_max_str) > len(x_min_str) ) then
			source = x_max_str
		else
			source = x_min_str
		end if

		if ( len(x_abs_min_str) > len(source) ) source = x_abs_min_str

		allocate( x_str(lbound(x, dim=1):ubound(x, dim=1), lbound(x, dim=2):ubound(x, dim=2)), source=source )

		do concurrent (j = lbound(x, dim=2):ubound(x, dim=2), i = lbound(x, dim=1):ubound(x, dim=1))
			x_str(i,j) = str(x(i,j), fmt=fmt_, decimals=decimals_)
		end do

		call aprint(x_str)
	end procedure aprint_2dr32

	module procedure aprint_1di64
		character(len=:), allocatable, dimension(:) :: x_str
		character(len=:), allocatable :: fmt_, x_max_str, x_min_str, source
		integer :: i

		if ( .not. present(fmt) ) then
			fmt_ = 'i'
		else
			if ( any(INT_FMTS == fmt) ) then
				fmt_ = fmt
			else
				write(*,'(a)') LF//'WARNING: Unknown format "'//fmt//'" for printing integer array. Aborting...'
				return
			end if
		end if

		x_max_str = str(maxval(x), fmt=fmt_)
		x_min_str = str(minval(x), fmt=fmt_)

		if ( len(x_max_str) > len(x_min_str) ) then
			source = x_max_str
		else
			source = x_min_str
		end if

		allocate( x_str(lbound(x, dim=1):ubound(x, dim=1)), source=source )

		do concurrent (i = lbound(x, dim=1):ubound(x, dim=1))
			x_str(i) = str(x(i), fmt=fmt_)
		end do

		call aprint(x_str)
	end procedure aprint_1di64
	module procedure aprint_1di32
		character(len=:), allocatable, dimension(:) :: x_str
		character(len=:), allocatable :: fmt_, x_max_str, x_min_str, source
		integer :: i

		if ( .not. present(fmt) ) then
			fmt_ = 'i'
		else
			if ( any(INT_FMTS == fmt) ) then
				fmt_ = fmt
			else
				write(*,'(a)') LF//'WARNING: Unknown format "'//fmt//'" for printing integer array. Aborting...'
				return
			end if
		end if

		x_max_str = str(maxval(x), fmt=fmt_)
		x_min_str = str(minval(x), fmt=fmt_)

		if ( len(x_max_str) > len(x_min_str) ) then
			source = x_max_str
		else
			source = x_min_str
		end if

		allocate( x_str(lbound(x, dim=1):ubound(x, dim=1)), source=source )

		do concurrent (i = lbound(x, dim=1):ubound(x, dim=1))
			x_str(i) = str(x(i), fmt=fmt_)
		end do

		call aprint(x_str)
	end procedure aprint_1di32
	module procedure aprint_1di16
		character(len=:), allocatable, dimension(:) :: x_str
		character(len=:), allocatable :: fmt_, x_max_str, x_min_str, source
		integer :: i

		if ( .not. present(fmt) ) then
			fmt_ = 'i'
		else
			if ( any(INT_FMTS == fmt) ) then
				fmt_ = fmt
			else
				write(*,'(a)') LF//'WARNING: Unknown format "'//fmt//'" for printing integer array. Aborting...'
				return
			end if
		end if

		x_max_str = str(maxval(x), fmt=fmt_)
		x_min_str = str(minval(x), fmt=fmt_)

		if ( len(x_max_str) > len(x_min_str) ) then
			source = x_max_str
		else
			source = x_min_str
		end if

		allocate( x_str(lbound(x, dim=1):ubound(x, dim=1)), source=source )

		do concurrent (i = lbound(x, dim=1):ubound(x, dim=1))
			x_str(i) = str(x(i), fmt=fmt_)
		end do

		call aprint(x_str)
	end procedure aprint_1di16
	module procedure aprint_1di8
		character(len=:), allocatable, dimension(:) :: x_str
		character(len=:), allocatable :: fmt_, x_max_str, x_min_str, source
		integer :: i

		if ( .not. present(fmt) ) then
			fmt_ = 'i'
		else
			if ( any(INT_FMTS == fmt) ) then
				fmt_ = fmt
			else
				write(*,'(a)') LF//'WARNING: Unknown format "'//fmt//'" for printing integer array. Aborting...'
				return
			end if
		end if

		x_max_str = str(maxval(x), fmt=fmt_)
		x_min_str = str(minval(x), fmt=fmt_)

		if ( len(x_max_str) > len(x_min_str) ) then
			source = x_max_str
		else
			source = x_min_str
		end if

		allocate( x_str(lbound(x, dim=1):ubound(x, dim=1)), source=source )

		do concurrent (i = lbound(x, dim=1):ubound(x, dim=1))
			x_str(i) = str(x(i), fmt=fmt_)
		end do

		call aprint(x_str)
	end procedure aprint_1di8

	module procedure aprint_2di64
		character(len=:), allocatable, dimension(:,:) :: x_str
		character(len=:), allocatable :: fmt_, x_max_str, x_min_str, source, str_tmp
		integer :: i, j

		if ( .not. present(fmt) ) then
			fmt_ = 'i'
		else
			if ( any(INT_FMTS == fmt) ) then
				fmt_ = fmt
			else
				write(*,'(a)') LF//'WARNING: Unknown format "'//fmt//'" for printing integer array. Aborting...'
				return
			end if
		end if

		x_max_str = str(maxval(x), fmt=fmt_)
		x_min_str = str(minval(x), fmt=fmt_)

		if ( len(x_max_str) > len(x_min_str) ) then
			source = x_max_str
		else
			source = x_min_str
		end if

		allocate( x_str(lbound(x, dim=1):ubound(x, dim=1), lbound(x, dim=2):ubound(x, dim=2)), source=source )

		do concurrent (j = lbound(x, dim=2):ubound(x, dim=2), i = lbound(x, dim=1):ubound(x, dim=1))
			x_str(i,j) = str(x(i,j), fmt=fmt_)
		end do

		call aprint(x_str)
	end procedure aprint_2di64
	module procedure aprint_2di32
		character(len=:), allocatable, dimension(:,:) :: x_str
		character(len=:), allocatable :: fmt_, x_max_str, x_min_str, source, str_tmp
		integer :: i, j

		if ( .not. present(fmt) ) then
			fmt_ = 'i'
		else
			if ( any(INT_FMTS == fmt) ) then
				fmt_ = fmt
			else
				write(*,'(a)') LF//'WARNING: Unknown format "'//fmt//'" for printing integer array. Aborting...'
				return
			end if
		end if

		x_max_str = str(maxval(x), fmt=fmt_)
		x_min_str = str(minval(x), fmt=fmt_)

		if ( len(x_max_str) > len(x_min_str) ) then
			source = x_max_str
		else
			source = x_min_str
		end if

		allocate( x_str(lbound(x, dim=1):ubound(x, dim=1), lbound(x, dim=2):ubound(x, dim=2)), source=source )

		do concurrent (j = lbound(x, dim=2):ubound(x, dim=2), i = lbound(x, dim=1):ubound(x, dim=1))
			x_str(i,j) = str(x(i,j), fmt=fmt_)
		end do

		call aprint(x_str)
	end procedure aprint_2di32
	module procedure aprint_2di16
		character(len=:), allocatable, dimension(:,:) :: x_str
		character(len=:), allocatable :: fmt_, x_max_str, x_min_str, source, str_tmp
		integer :: i, j

		if ( .not. present(fmt) ) then
			fmt_ = 'i'
		else
			if ( any(INT_FMTS == fmt) ) then
				fmt_ = fmt
			else
				write(*,'(a)') LF//'WARNING: Unknown format "'//fmt//'" for printing integer array. Aborting...'
				return
			end if
		end if

		x_max_str = str(maxval(x), fmt=fmt_)
		x_min_str = str(minval(x), fmt=fmt_)

		if ( len(x_max_str) > len(x_min_str) ) then
			source = x_max_str
		else
			source = x_min_str
		end if

		allocate( x_str(lbound(x, dim=1):ubound(x, dim=1), lbound(x, dim=2):ubound(x, dim=2)), source=source )

		do concurrent (j = lbound(x, dim=2):ubound(x, dim=2), i = lbound(x, dim=1):ubound(x, dim=1))
			x_str(i,j) = str(x(i,j), fmt=fmt_)
		end do

		call aprint(x_str)
	end procedure aprint_2di16
	module procedure aprint_2di8
		character(len=:), allocatable, dimension(:,:) :: x_str
		character(len=:), allocatable :: fmt_, x_max_str, x_min_str, source, str_tmp
		integer :: i, j

		if ( .not. present(fmt) ) then
			fmt_ = 'i'
		else
			if ( any(INT_FMTS == fmt) ) then
				fmt_ = fmt
			else
				write(*,'(a)') LF//'WARNING: Unknown format "'//fmt//'" for printing integer array. Aborting...'
				return
			end if
		end if

		x_max_str = str(maxval(x), fmt=fmt_)
		x_min_str = str(minval(x), fmt=fmt_)

		if ( len(x_max_str) > len(x_min_str) ) then
			source = x_max_str
		else
			source = x_min_str
		end if

		allocate( x_str(lbound(x, dim=1):ubound(x, dim=1), lbound(x, dim=2):ubound(x, dim=2)), source=source )

		do concurrent (j = lbound(x, dim=2):ubound(x, dim=2), i = lbound(x, dim=1):ubound(x, dim=1))
			x_str(i,j) = str(x(i,j), fmt=fmt_)
		end do

		call aprint(x_str)
	end procedure aprint_2di8

	module procedure aprint_1dchar
		integer :: i

		write(unit=*, fmt='(a)') LF//'     '//adjustr(x(lbound(x, dim=1)))

		if ( size(x) == 1 ) return

		if ( size(x) > 2 ) then
			do i = lbound(x, dim=1) + 1, ubound(x, dim=1) - 1
				write(unit=*, fmt='(a)') '     '//adjustr(x(i))
			end do
		end if

		write(unit=*, fmt='(a)') '     '//adjustr(x(ubound(x, dim=1)))//LF
	end procedure aprint_1dchar

	module procedure aprint_2dchar
		integer :: i

		write(unit=*, fmt='(a)') LF//'    '//accum(x(lbound(x, dim=1),:))

		if ( size(x, dim=1) == 1 ) return

		if ( size(x, dim=1) > 2 ) then
			do i = lbound(x, dim=1) + 1, ubound(x, dim=1) - 1
				write(unit=*, fmt='(a)') '    '//accum(x(i,:))
			end do
		end if

		write(unit=*, fmt='(a)') '    '//accum(x(ubound(x, dim=1),:))//LF

		contains
		pure recursive function accum(x) result(x_str)
			character(len=*), dimension(:), intent(in) :: x
			character(len=:), allocatable :: x_str

			integer :: i

			x_str = EMPTY_STR
			do i = 1, size(x)-1
				x_str = x_str//adjustr(x(i))//SPACE
			end do
			x_str = x_str//adjustr(x(size(x)))
		end function accum
	end procedure aprint_2dchar

	module procedure aprint_1dString
		character(len=:), allocatable, dimension(:) :: char_arr
		integer, allocatable, dimension(:) :: lengths
		integer :: i, max_length

		lengths = x%len()
		max_length = maxval(lengths)

		allocate( character(len=max_length) :: char_arr(lbound(x, dim=1):ubound(x, dim=1)) )

		do concurrent (i = lbound(x, dim=1):ubound(x, dim=1))
			if ( lengths(i) < 1 ) then
				char_arr(i) = EMPTY_STR
			else
				char_arr(i) = x(i)%s
			end if
		end do

		call aprint(char_arr)
	end procedure aprint_1dString

	module procedure aprint_2dString
		character(len=:), allocatable, dimension(:,:) :: char_arr
		integer, allocatable, dimension(:,:) :: lengths
		integer :: i, j, max_length

		lengths = x%len()
		max_length = maxval(lengths)

		allocate( character(len=max_length) :: &
				  char_arr(lbound(x, dim=1):ubound(x, dim=1), lbound(x, dim=2):ubound(x, dim=2)) )

		do concurrent (j = lbound(x, dim=2):ubound(x, dim=2), i = lbound(x, dim=1):ubound(x, dim=1))
			if ( lengths(i,j) < 1 ) then
				char_arr(i,j) = EMPTY_STR
			else
				char_arr(i,j) = x(i,j)%s
			end if
		end do

		call aprint(char_arr)
	end procedure aprint_2dString
end submodule array_printing
