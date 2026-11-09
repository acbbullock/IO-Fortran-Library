module io_strings
  !---------------------------------------------------------------------------------------------------------------------
  !! Functions on `character` and `String` values: case conversion, prefix and suffix tests, searching, and removing
  !! given characters from both ends. The names and arguments follow the Fortran standard library (stdlib).
  !!
  !! These are in a module of their own, separate from `io_fortran_lib`, so that adding them can't clash with a
  !! procedure of the same name in a program that uses `io_fortran_lib` without an `only` list. Use it alongside:
  !!
  !!     use io_fortran_lib, only: String
  !!     use io_strings, only: to_upper, find
  !!
  !! The same operations are type-bound procedures of `String`, such as `s%to_upper()`.
  !!
  !! For a user reference, see [io_strings](../page/Ref/io_strings.html).
  !---------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: i64 => int64
  use io_fortran_lib, only: string_t => String ! Renamed here, since stdlib calls the first argument string
  implicit none (type, external)
  private

  public :: to_upper, to_lower, starts_with, ends_with, find, trim

  interface to_upper
    !! Converts the ASCII letters `a` to `z` to upper case. Other characters are unchanged.
    module procedure to_upper_chars, to_upper_string
  end interface to_upper

  interface to_lower
    !! Converts the ASCII letters `A` to `Z` to lower case. Other characters are unchanged.
    module procedure to_lower_chars, to_lower_string
  end interface to_lower

  interface starts_with
    !! Whether `string` starts with `substring`. Every string starts with the empty string.
    module procedure starts_with_cc, starts_with_cs, starts_with_sc, starts_with_ss
  end interface starts_with

  interface ends_with
    !! Whether `string` ends with `substring`. Every string ends with the empty string.
    module procedure ends_with_cc, ends_with_cs, ends_with_sc, ends_with_ss
  end interface ends_with

  interface find
    !! The position where the `occurrence`th occurrence (default 1) of `pattern` starts in `string`, or 0 if there is
    !! none. Occurrences may overlap unless `consider_overlapping` is `.false.`. An empty `pattern` gives 0.
    module procedure find_cc, find_cs, find_sc, find_ss
  end interface find

  interface trim
    !! Removes every character in `chars` from both ends of `string`. For a `String`, `chars` is optional and defaults
    !! to whitespace. `trim(string)` with one `character` argument is still the intrinsic `trim`.
    module procedure trim_chars, trim_string
  end interface trim

  contains ! Procedure bodies <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

  elemental recursive function to_upper_chars(string) result(res)
    character(len=*), intent(in) :: string
    character(len=len(string))   :: res

    integer(i64) :: i

    res = string
    do i = 1_i64, len(string, kind=i64)
      if ( (string(i:i) >= "a") .and. (string(i:i) <= "z") ) res(i:i) = achar(iachar(string(i:i)) - 32)
    end do
  end function to_upper_chars

  elemental recursive function to_lower_chars(string) result(res)
    character(len=*), intent(in) :: string
    character(len=len(string))   :: res

    integer(i64) :: i

    res = string
    do i = 1_i64, len(string, kind=i64)
      if ( (string(i:i) >= "A") .and. (string(i:i) <= "Z") ) res(i:i) = achar(iachar(string(i:i)) + 32)
    end do
  end function to_lower_chars

  elemental recursive type(string_t) function to_upper_string(string) result(res)
    type(string_t), intent(in) :: string

    res = string%to_upper()
  end function to_upper_string

  elemental recursive type(string_t) function to_lower_string(string) result(res)
    type(string_t), intent(in) :: string

    res = string%to_lower()
  end function to_lower_string

  elemental recursive logical function starts_with_cc(string, substring) result(res)
    character(len=*), intent(in) :: string, substring

    res = .false.
    if ( len(substring, kind=i64) > len(string, kind=i64) ) return
    res = ( string(:len(substring, kind=i64)) == substring )
  end function starts_with_cc

  elemental recursive logical function starts_with_cs(string, substring) result(res)
    character(len=*), intent(in) :: string
    type(string_t),     intent(in) :: substring

    res = starts_with_cc(string, substring%as_str())
  end function starts_with_cs

  elemental recursive logical function starts_with_sc(string, substring) result(res)
    type(string_t),     intent(in) :: string
    character(len=*), intent(in) :: substring

    res = string%starts_with(substring)
  end function starts_with_sc

  elemental recursive logical function starts_with_ss(string, substring) result(res)
    type(string_t), intent(in) :: string, substring

    res = string%starts_with(substring)
  end function starts_with_ss

  elemental recursive logical function ends_with_cc(string, substring) result(res)
    character(len=*), intent(in) :: string, substring

    integer(i64) :: ls, lt

    ls = len(string, kind=i64); lt = len(substring, kind=i64)
    res = .false.
    if ( lt > ls ) return
    res = ( string(ls-lt+1_i64:) == substring )
  end function ends_with_cc

  elemental recursive logical function ends_with_cs(string, substring) result(res)
    character(len=*), intent(in) :: string
    type(string_t),     intent(in) :: substring

    res = ends_with_cc(string, substring%as_str())
  end function ends_with_cs

  elemental recursive logical function ends_with_sc(string, substring) result(res)
    type(string_t),     intent(in) :: string
    character(len=*), intent(in) :: substring

    res = string%ends_with(substring)
  end function ends_with_sc

  elemental recursive logical function ends_with_ss(string, substring) result(res)
    type(string_t), intent(in) :: string, substring

    res = string%ends_with(substring)
  end function ends_with_ss

  elemental recursive integer function find_cc(string, pattern, occurrence, consider_overlapping) result(position)
    character(len=*), intent(in)           :: string, pattern
    integer,          intent(in), optional :: occurrence
    logical,          intent(in), optional :: consider_overlapping

    integer(i64) :: start, i, step, last
    integer      :: wanted, found

    position = 0

    wanted = 1
    if ( present(occurrence) ) wanted = occurrence
    if ( (wanted < 1) .or. (len(pattern) == 0) ) return

    step = 1_i64
    if ( present(consider_overlapping) ) then
      if ( .not. consider_overlapping ) step = len(pattern, kind=i64)
    end if

    last  = len(string, kind=i64) - len(pattern, kind=i64) + 1_i64
    found = 0; start = 1_i64

    do while ( start <= last )
      i = index(string(start:), pattern, kind=i64)
      if ( i == 0_i64 ) return
      found = found + 1
      if ( found == wanted ) then
        position = int(start + i - 1_i64)
        return
      end if
      start = start + i - 1_i64 + step
    end do
  end function find_cc

  elemental recursive integer function find_cs(string, pattern, occurrence, consider_overlapping) result(position)
    character(len=*), intent(in)           :: string
    type(string_t),     intent(in)           :: pattern
    integer,          intent(in), optional :: occurrence
    logical,          intent(in), optional :: consider_overlapping

    position = find_cc(string, pattern%as_str(), occurrence, consider_overlapping)
  end function find_cs

  elemental recursive integer function find_sc(string, pattern, occurrence, consider_overlapping) result(position)
    type(string_t),     intent(in)           :: string
    character(len=*), intent(in)           :: pattern
    integer,          intent(in), optional :: occurrence
    logical,          intent(in), optional :: consider_overlapping

    position = string%find(pattern, occurrence, consider_overlapping)
  end function find_sc

  elemental recursive integer function find_ss(string, pattern, occurrence, consider_overlapping) result(position)
    type(string_t),     intent(in)           :: string, pattern
    integer,          intent(in), optional :: occurrence
    logical,          intent(in), optional :: consider_overlapping

    position = string%find(pattern, occurrence, consider_overlapping)
  end function find_ss

  pure recursive function trim_chars(string, chars) result(res)
    !! Not elemental, like the intrinsic `trim`, since the length of the result depends on the value of `string`.
    character(len=*), intent(in)  :: string, chars
    character(len=:), allocatable :: res

    integer(i64) :: first

    first = verify(string, chars, kind=i64)

    if ( first == 0_i64 ) then
      res = ""
    else
      res = string(first:verify(string, chars, back=.true., kind=i64))
    end if
  end function trim_chars

  elemental recursive type(string_t) function trim_string(string, chars) result(res)
    type(string_t),     intent(in)           :: string
    character(len=*), intent(in), optional :: chars

    res = string%trim(chars)
  end function trim_string
end module io_strings
