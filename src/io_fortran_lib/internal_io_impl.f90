! Generated from fypp/io_fortran_lib/internal_io_impl.fypp by fypp/generate.sh. Edit that file, not this one.
#include "iofl_config.inc"
submodule (io_fortran_lib) io_internal
  !---------------------------------------------------------------------------------------------------------------------
  !! This submodule provides module procedure implementations for the **public interfaces** `String`, `str`, and
  !! `cast`.
  !---------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_is_finite, ieee_is_negative
  implicit none (type, external)

  integer, parameter :: MAX_F_DECIMALS = 200 ! Most decimals to request from an `f` edit descriptor

  interface real_text
#ifdef IOFL_HAS_R128
    module procedure real_text_r128
#endif
    module procedure real_text_r64, real_text_r32
  end interface real_text

  interface complex_text
#ifdef IOFL_HAS_R128
    module procedure complex_text_c128
#endif
    module procedure complex_text_c64, complex_text_c32
  end interface complex_text

  ! Definitions and interfaces ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=1), parameter :: DIGITS_A(0:15) = ["0","1","2","3","4","5","6","7","8","9","a","b","c","d","e","f"]

  contains ! Procedure bodies for module subprograms <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

  ! String ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ifdef IOFL_HAS_R128
  module procedure new_string_from_c128
    new%buf = complex_text(x, decimal_of(locale), real_fmt_of(fmt), decimals, im)
    new%n = len(new%buf, kind=i64)
  end procedure new_string_from_c128
#endif

  module procedure new_string_from_c64
    new%buf = complex_text(x, decimal_of(locale), real_fmt_of(fmt), decimals, im)
    new%n = len(new%buf, kind=i64)
  end procedure new_string_from_c64

  module procedure new_string_from_c32
    new%buf = complex_text(x, decimal_of(locale), real_fmt_of(fmt), decimals, im)
    new%n = len(new%buf, kind=i64)
  end procedure new_string_from_c32


#ifdef IOFL_HAS_R128
  module procedure new_string_from_r128
    new%buf = real_text(x, decimal_of(locale), real_fmt_of(fmt), decimals)
    new%n = len(new%buf, kind=i64)
  end procedure new_string_from_r128
#endif

  module procedure new_string_from_r64
    new%buf = real_text(x, decimal_of(locale), real_fmt_of(fmt), decimals)
    new%n = len(new%buf, kind=i64)
  end procedure new_string_from_r64

  module procedure new_string_from_r32
    new%buf = real_text(x, decimal_of(locale), real_fmt_of(fmt), decimals)
    new%n = len(new%buf, kind=i64)
  end procedure new_string_from_r32


  module procedure new_string_from_i64
    new%buf = int_text(int(x, kind=i64), int(bit_size(x)), int_fmt_of(fmt))
    new%n = len(new%buf, kind=i64)
  end procedure new_string_from_i64

  module procedure new_string_from_i32
    new%buf = int_text(int(x, kind=i64), int(bit_size(x)), int_fmt_of(fmt))
    new%n = len(new%buf, kind=i64)
  end procedure new_string_from_i32

  module procedure new_string_from_i16
    new%buf = int_text(int(x, kind=i64), int(bit_size(x)), int_fmt_of(fmt))
    new%n = len(new%buf, kind=i64)
  end procedure new_string_from_i16

  module procedure new_string_from_i8
    new%buf = int_text(int(x, kind=i64), int(bit_size(x)), int_fmt_of(fmt))
    new%n = len(new%buf, kind=i64)
  end procedure new_string_from_i8


  module procedure new_string_from_string
    if ( x%len64() < 1_i64 ) then
      new%buf = EMPTY_STR; new%n = len(new%buf, kind=i64)
    else
      new%buf = x%buf(:x%n); new%n = len(new%buf, kind=i64)
    end if
  end procedure new_string_from_string
  module procedure new_string_from_char
    new%buf = x; new%n = len(new%buf, kind=i64)
  end procedure new_string_from_char
  module procedure new_string_from_empty
    new%buf = EMPTY_STR; new%n = len(new%buf, kind=i64)
  end procedure new_string_from_empty

  ! str ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ifdef IOFL_HAS_R128
  module procedure str_from_c128
    x_str = complex_text(x, decimal_of(locale), real_fmt_of(fmt), decimals, im)
  end procedure str_from_c128
#endif

  module procedure str_from_c64
    x_str = complex_text(x, decimal_of(locale), real_fmt_of(fmt), decimals, im)
  end procedure str_from_c64

  module procedure str_from_c32
    x_str = complex_text(x, decimal_of(locale), real_fmt_of(fmt), decimals, im)
  end procedure str_from_c32


#ifdef IOFL_HAS_R128
  module procedure str_from_r128
    x_str = real_text(x, decimal_of(locale), real_fmt_of(fmt), decimals)
  end procedure str_from_r128
#endif

  module procedure str_from_r64
    x_str = real_text(x, decimal_of(locale), real_fmt_of(fmt), decimals)
  end procedure str_from_r64

  module procedure str_from_r32
    x_str = real_text(x, decimal_of(locale), real_fmt_of(fmt), decimals)
  end procedure str_from_r32


  module procedure str_from_i64
    x_str = int_text(int(x, kind=i64), int(bit_size(x)), int_fmt_of(fmt))
  end procedure str_from_i64

  module procedure str_from_i32
    x_str = int_text(int(x, kind=i64), int(bit_size(x)), int_fmt_of(fmt))
  end procedure str_from_i32

  module procedure str_from_i16
    x_str = int_text(int(x, kind=i64), int(bit_size(x)), int_fmt_of(fmt))
  end procedure str_from_i16

  module procedure str_from_i8
    x_str = int_text(int(x, kind=i64), int(bit_size(x)), int_fmt_of(fmt))
  end procedure str_from_i8


  module procedure str_from_string
    if ( x%len64() < 1_i64 ) then
      x_str = EMPTY_STR
    else
      x_str = x%buf(:x%n)
    end if
  end procedure str_from_string
  module procedure str_from_char
    x_str = x
  end procedure str_from_char
  module procedure str_from_empty
    x_str = EMPTY_STR
  end procedure str_from_empty

  ! cast ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ifdef IOFL_HAS_R128
  module procedure cast_c128_to_string
    into%buf = complex_text(x, decimal_of(locale), real_fmt_of(fmt), decimals, im)
    into%n = len(into%buf, kind=i64)
  end procedure cast_c128_to_string

  module procedure cast_column_c128
    type(String), allocatable :: re(:), im_part(:)
    real(r128),    allocatable :: parts(:,:)
    character(len=5)          :: decimal
    integer                   :: i

    decimal = decimal_of(locale)

    if ( (real_fmt_of(fmt) /= "e") .or. (size(x) == 0) ) then
      call cast(x, into, locale, fmt, decimals, im); return
    end if

    allocate( re(size(x)), im_part(size(x)), parts(size(x),2) )

    do i = 1, size(x)                    ! Copied, as ifx 2025.3 rejects x%re of an intent(in) x as an argument
      parts(i,1) = real(x(i), kind=r128); parts(i,2) = aimag(x(i))
    end do

    call cast_column(parts(:,1), re, locale, fmt, decimals)
    call cast_column(parts(:,2), im_part, locale, fmt, decimals)

    do i = 1, size(x)
      into(i)%buf = join_complex(re(i)%buf, im_part(i)%buf, ieee_is_negative(x(i)%im), decimal, "e", im)
      into(i)%n = len(into(i)%buf, kind=i64)
    end do
  end procedure cast_column_c128
#endif

  module procedure cast_c64_to_string
    into%buf = complex_text(x, decimal_of(locale), real_fmt_of(fmt), decimals, im)
    into%n = len(into%buf, kind=i64)
  end procedure cast_c64_to_string

  module procedure cast_column_c64
    type(String), allocatable :: re(:), im_part(:)
    real(r64),    allocatable :: parts(:,:)
    character(len=5)          :: decimal
    integer                   :: i

    decimal = decimal_of(locale)

    if ( (real_fmt_of(fmt) /= "e") .or. (size(x) == 0) ) then
      call cast(x, into, locale, fmt, decimals, im); return
    end if

    allocate( re(size(x)), im_part(size(x)), parts(size(x),2) )

    do i = 1, size(x)                    ! Copied, as ifx 2025.3 rejects x%re of an intent(in) x as an argument
      parts(i,1) = real(x(i), kind=r64); parts(i,2) = aimag(x(i))
    end do

    call cast_column(parts(:,1), re, locale, fmt, decimals)
    call cast_column(parts(:,2), im_part, locale, fmt, decimals)

    do i = 1, size(x)
      into(i)%buf = join_complex(re(i)%buf, im_part(i)%buf, ieee_is_negative(x(i)%im), decimal, "e", im)
      into(i)%n = len(into(i)%buf, kind=i64)
    end do
  end procedure cast_column_c64

  module procedure cast_c32_to_string
    into%buf = complex_text(x, decimal_of(locale), real_fmt_of(fmt), decimals, im)
    into%n = len(into%buf, kind=i64)
  end procedure cast_c32_to_string

  module procedure cast_column_c32
    type(String), allocatable :: re(:), im_part(:)
    real(r32),    allocatable :: parts(:,:)
    character(len=5)          :: decimal
    integer                   :: i

    decimal = decimal_of(locale)

    if ( (real_fmt_of(fmt) /= "e") .or. (size(x) == 0) ) then
      call cast(x, into, locale, fmt, decimals, im); return
    end if

    allocate( re(size(x)), im_part(size(x)), parts(size(x),2) )

    do i = 1, size(x)                    ! Copied, as ifx 2025.3 rejects x%re of an intent(in) x as an argument
      parts(i,1) = real(x(i), kind=r32); parts(i,2) = aimag(x(i))
    end do

    call cast_column(parts(:,1), re, locale, fmt, decimals)
    call cast_column(parts(:,2), im_part, locale, fmt, decimals)

    do i = 1, size(x)
      into(i)%buf = join_complex(re(i)%buf, im_part(i)%buf, ieee_is_negative(x(i)%im), decimal, "e", im)
      into(i)%n = len(into(i)%buf, kind=i64)
    end do
  end procedure cast_column_c32


#ifdef IOFL_HAS_R128
  module procedure cast_r128_to_string
    into%buf = real_text(x, decimal_of(locale), real_fmt_of(fmt), decimals)
    into%n = len(into%buf, kind=i64)
  end procedure cast_r128_to_string

  module procedure cast_column_r128
    integer, parameter :: E_DECIMALS = 35, E_DIGITS = 4, CHUNK = 1024

    character(len=:), allocatable :: chunk_text
    character(len=32)             :: edit
    character(len=5)              :: decimal
    integer                       :: d, w, i, j, n

    decimal = decimal_of(locale)

    if ( (real_fmt_of(fmt) /= "e") .or. (size(x) == 0) ) then
      call cast(x, into, locale, fmt, decimals); return
    end if

    d = E_DECIMALS
    if ( present(decimals) ) d = max(min(decimals, E_DECIMALS), 0)

    w    = d + E_DIGITS + 8
    edit = "(*"//trim(e_edit(w, d, E_DIGITS))//")"

    allocate( character(len=w*min(size(x), CHUNK)) :: chunk_text )

    do j = 0, size(x) - 1, CHUNK                       ! One `write` formats a chunk of numbers, each in a w-wide field
      n = min(CHUNK, size(x) - j)
      write(unit=chunk_text, fmt=edit, decimal=decimal) x(j+1:j+n)

      do i = 1, n
        if ( ieee_is_finite(x(j+i)) .and. (x(j+i) /= 0e0_r128) ) then
          into(j+i)%buf = e_field_text(chunk_text((i-1)*w+1:i*w))
        else
          into(j+i)%buf = real_text(x(j+i), decimal, "e", decimals)
        end if

        into(j+i)%n = len(into(j+i)%buf, kind=i64)
      end do
    end do
  end procedure cast_column_r128
#endif

  module procedure cast_r64_to_string
    into%buf = real_text(x, decimal_of(locale), real_fmt_of(fmt), decimals)
    into%n = len(into%buf, kind=i64)
  end procedure cast_r64_to_string

  module procedure cast_column_r64
    integer, parameter :: E_DECIMALS = 17, E_DIGITS = 3, CHUNK = 1024

    character(len=:), allocatable :: chunk_text
    character(len=32)             :: edit
    character(len=5)              :: decimal
    integer                       :: d, w, i, j, n

    decimal = decimal_of(locale)

    if ( (real_fmt_of(fmt) /= "e") .or. (size(x) == 0) ) then
      call cast(x, into, locale, fmt, decimals); return
    end if

    d = E_DECIMALS
    if ( present(decimals) ) d = max(min(decimals, E_DECIMALS), 0)

    w    = d + E_DIGITS + 8
    edit = "(*"//trim(e_edit(w, d, E_DIGITS))//")"

    allocate( character(len=w*min(size(x), CHUNK)) :: chunk_text )

    do j = 0, size(x) - 1, CHUNK                       ! One `write` formats a chunk of numbers, each in a w-wide field
      n = min(CHUNK, size(x) - j)
      write(unit=chunk_text, fmt=edit, decimal=decimal) x(j+1:j+n)

      do i = 1, n
        if ( ieee_is_finite(x(j+i)) .and. (x(j+i) /= 0e0_r64) ) then
          into(j+i)%buf = e_field_text(chunk_text((i-1)*w+1:i*w))
        else
          into(j+i)%buf = real_text(x(j+i), decimal, "e", decimals)
        end if

        into(j+i)%n = len(into(j+i)%buf, kind=i64)
      end do
    end do
  end procedure cast_column_r64

  module procedure cast_r32_to_string
    into%buf = real_text(x, decimal_of(locale), real_fmt_of(fmt), decimals)
    into%n = len(into%buf, kind=i64)
  end procedure cast_r32_to_string

  module procedure cast_column_r32
    integer, parameter :: E_DECIMALS = 8, E_DIGITS = 2, CHUNK = 1024

    character(len=:), allocatable :: chunk_text
    character(len=32)             :: edit
    character(len=5)              :: decimal
    integer                       :: d, w, i, j, n

    decimal = decimal_of(locale)

    if ( (real_fmt_of(fmt) /= "e") .or. (size(x) == 0) ) then
      call cast(x, into, locale, fmt, decimals); return
    end if

    d = E_DECIMALS
    if ( present(decimals) ) d = max(min(decimals, E_DECIMALS), 0)

    w    = d + E_DIGITS + 8
    edit = "(*"//trim(e_edit(w, d, E_DIGITS))//")"

    allocate( character(len=w*min(size(x), CHUNK)) :: chunk_text )

    do j = 0, size(x) - 1, CHUNK                       ! One `write` formats a chunk of numbers, each in a w-wide field
      n = min(CHUNK, size(x) - j)
      write(unit=chunk_text, fmt=edit, decimal=decimal) x(j+1:j+n)

      do i = 1, n
        if ( ieee_is_finite(x(j+i)) .and. (x(j+i) /= 0e0_r32) ) then
          into(j+i)%buf = e_field_text(chunk_text((i-1)*w+1:i*w))
        else
          into(j+i)%buf = real_text(x(j+i), decimal, "e", decimals)
        end if

        into(j+i)%n = len(into(j+i)%buf, kind=i64)
      end do
    end do
  end procedure cast_column_r32


  module procedure cast_i64_to_string
    into%buf = int_text(int(x, kind=i64), int(bit_size(x)), int_fmt_of(fmt))
    into%n = len(into%buf, kind=i64)
  end procedure cast_i64_to_string

  module procedure cast_i32_to_string
    into%buf = int_text(int(x, kind=i64), int(bit_size(x)), int_fmt_of(fmt))
    into%n = len(into%buf, kind=i64)
  end procedure cast_i32_to_string

  module procedure cast_i16_to_string
    into%buf = int_text(int(x, kind=i64), int(bit_size(x)), int_fmt_of(fmt))
    into%n = len(into%buf, kind=i64)
  end procedure cast_i16_to_string

  module procedure cast_i8_to_string
    into%buf = int_text(int(x, kind=i64), int(bit_size(x)), int_fmt_of(fmt))
    into%n = len(into%buf, kind=i64)
  end procedure cast_i8_to_string


#ifdef IOFL_HAS_R128
  module procedure cast_c128_to_char
    into = complex_text(x, decimal_of(locale), real_fmt_of(fmt), decimals, im)
  end procedure cast_c128_to_char
#endif

  module procedure cast_c64_to_char
    into = complex_text(x, decimal_of(locale), real_fmt_of(fmt), decimals, im)
  end procedure cast_c64_to_char

  module procedure cast_c32_to_char
    into = complex_text(x, decimal_of(locale), real_fmt_of(fmt), decimals, im)
  end procedure cast_c32_to_char


#ifdef IOFL_HAS_R128
  module procedure cast_r128_to_char
    into = real_text(x, decimal_of(locale), real_fmt_of(fmt), decimals)
  end procedure cast_r128_to_char
#endif

  module procedure cast_r64_to_char
    into = real_text(x, decimal_of(locale), real_fmt_of(fmt), decimals)
  end procedure cast_r64_to_char

  module procedure cast_r32_to_char
    into = real_text(x, decimal_of(locale), real_fmt_of(fmt), decimals)
  end procedure cast_r32_to_char


  module procedure cast_i64_to_char
    into = int_text(int(x, kind=i64), int(bit_size(x)), int_fmt_of(fmt))
  end procedure cast_i64_to_char

  module procedure cast_i32_to_char
    into = int_text(int(x, kind=i64), int(bit_size(x)), int_fmt_of(fmt))
  end procedure cast_i32_to_char

  module procedure cast_i16_to_char
    into = int_text(int(x, kind=i64), int(bit_size(x)), int_fmt_of(fmt))
  end procedure cast_i16_to_char

  module procedure cast_i8_to_char
    into = int_text(int(x, kind=i64), int(bit_size(x)), int_fmt_of(fmt))
  end procedure cast_i8_to_char


#ifdef IOFL_HAS_R128
  module procedure cast_string_to_c128
    if ( allocated(substring%buf) ) then
      call cast_char_to_c128(substring%buf(:substring%n), into, locale, fmt, im, stat)
    else
      into = (0e0_r128,0e0_r128)
      call report_cast(READ_ERR, EMPTY_STR, stat)
    end if
  end procedure cast_string_to_c128
#endif

  module procedure cast_string_to_c64
    if ( allocated(substring%buf) ) then
      call cast_char_to_c64(substring%buf(:substring%n), into, locale, fmt, im, stat)
    else
      into = (0e0_r64,0e0_r64)
      call report_cast(READ_ERR, EMPTY_STR, stat)
    end if
  end procedure cast_string_to_c64

  module procedure cast_string_to_c32
    if ( allocated(substring%buf) ) then
      call cast_char_to_c32(substring%buf(:substring%n), into, locale, fmt, im, stat)
    else
      into = (0e0_r32,0e0_r32)
      call report_cast(READ_ERR, EMPTY_STR, stat)
    end if
  end procedure cast_string_to_c32


#ifdef IOFL_HAS_R128
  module procedure cast_string_to_r128
    if ( allocated(substring%buf) ) then
      call cast_char_to_r128(substring%buf(:substring%n), into, locale, fmt, stat)
    else
      into = 0e0_r128
      call report_cast(READ_ERR, EMPTY_STR, stat)
    end if
  end procedure cast_string_to_r128
#endif

  module procedure cast_string_to_r64
    if ( allocated(substring%buf) ) then
      call cast_char_to_r64(substring%buf(:substring%n), into, locale, fmt, stat)
    else
      into = 0e0_r64
      call report_cast(READ_ERR, EMPTY_STR, stat)
    end if
  end procedure cast_string_to_r64

  module procedure cast_string_to_r32
    if ( allocated(substring%buf) ) then
      call cast_char_to_r32(substring%buf(:substring%n), into, locale, fmt, stat)
    else
      into = 0e0_r32
      call report_cast(READ_ERR, EMPTY_STR, stat)
    end if
  end procedure cast_string_to_r32


  module procedure cast_string_to_i64
    if ( allocated(substring%buf) ) then
      call cast_char_to_i64(substring%buf(:substring%n), into, fmt, stat)
    else
      into = 0_i64
      call report_cast(READ_ERR, EMPTY_STR, stat)
    end if
  end procedure cast_string_to_i64

  module procedure cast_string_to_i32
    if ( allocated(substring%buf) ) then
      call cast_char_to_i32(substring%buf(:substring%n), into, fmt, stat)
    else
      into = 0_i32
      call report_cast(READ_ERR, EMPTY_STR, stat)
    end if
  end procedure cast_string_to_i32

  module procedure cast_string_to_i16
    if ( allocated(substring%buf) ) then
      call cast_char_to_i16(substring%buf(:substring%n), into, fmt, stat)
    else
      into = 0_i16
      call report_cast(READ_ERR, EMPTY_STR, stat)
    end if
  end procedure cast_string_to_i16

  module procedure cast_string_to_i8
    if ( allocated(substring%buf) ) then
      call cast_char_to_i8(substring%buf(:substring%n), into, fmt, stat)
    else
      into = 0_i8
      call report_cast(READ_ERR, EMPTY_STR, stat)
    end if
  end procedure cast_string_to_i8


#ifdef IOFL_HAS_R128
  module procedure cast_char_to_c128
    character(len=1) :: fmt_
    character(len=5) :: decimal
    integer          :: status

    into = (0e0_r128,0e0_r128)

    call cast_args(REAL_FMTS, "e", fmt, locale, fmt_, decimal, status)
    if ( status == 0 ) call parse_complex_c128(substring, decimal, fmt_, im, into, status)

    call report_cast(status, substring, stat)
  end procedure cast_char_to_c128
#endif

  module procedure cast_char_to_c64
    character(len=1) :: fmt_
    character(len=5) :: decimal
    integer          :: status

    into = (0e0_r64,0e0_r64)

    call cast_args(REAL_FMTS, "e", fmt, locale, fmt_, decimal, status)
    if ( status == 0 ) call parse_complex_c64(substring, decimal, fmt_, im, into, status)

    call report_cast(status, substring, stat)
  end procedure cast_char_to_c64

  module procedure cast_char_to_c32
    character(len=1) :: fmt_
    character(len=5) :: decimal
    integer          :: status

    into = (0e0_r32,0e0_r32)

    call cast_args(REAL_FMTS, "e", fmt, locale, fmt_, decimal, status)
    if ( status == 0 ) call parse_complex_c32(substring, decimal, fmt_, im, into, status)

    call report_cast(status, substring, stat)
  end procedure cast_char_to_c32


#ifdef IOFL_HAS_R128
  module procedure cast_char_to_r128
    character(len=1) :: fmt_
    character(len=5) :: decimal
    integer          :: status

    into = 0e0_r128

    call cast_args(REAL_FMTS, "e", fmt, locale, fmt_, decimal, status)
    if ( status == 0 ) call parse_real_r128(substring, decimal, fmt_, into, status)

    call report_cast(status, substring, stat)
  end procedure cast_char_to_r128
#endif

  module procedure cast_char_to_r64
    character(len=1) :: fmt_
    character(len=5) :: decimal
    integer          :: status

    into = 0e0_r64

    call cast_args(REAL_FMTS, "e", fmt, locale, fmt_, decimal, status)
    if ( status == 0 ) call parse_real_r64(substring, decimal, fmt_, into, status)

    call report_cast(status, substring, stat)
  end procedure cast_char_to_r64

  module procedure cast_char_to_r32
    character(len=1) :: fmt_
    character(len=5) :: decimal
    integer          :: status

    into = 0e0_r32

    call cast_args(REAL_FMTS, "e", fmt, locale, fmt_, decimal, status)
    if ( status == 0 ) call parse_real_r32(substring, decimal, fmt_, into, status)

    call report_cast(status, substring, stat)
  end procedure cast_char_to_r32


  module procedure cast_char_to_i64
    character(len=1) :: fmt_
    character(len=5) :: decimal
    integer          :: status

    into = 0_i64

    call cast_args(INT_FMTS, "i", fmt=fmt, fmt_=fmt_, decimal=decimal, status=status)
    if ( status == 0 ) call parse_int_i64(substring, fmt_, into, status)

    call report_cast(status, substring, stat)
  end procedure cast_char_to_i64

  module procedure cast_char_to_i32
    character(len=1) :: fmt_
    character(len=5) :: decimal
    integer          :: status

    into = 0_i32

    call cast_args(INT_FMTS, "i", fmt=fmt, fmt_=fmt_, decimal=decimal, status=status)
    if ( status == 0 ) call parse_int_i32(substring, fmt_, into, status)

    call report_cast(status, substring, stat)
  end procedure cast_char_to_i32

  module procedure cast_char_to_i16
    character(len=1) :: fmt_
    character(len=5) :: decimal
    integer          :: status

    into = 0_i16

    call cast_args(INT_FMTS, "i", fmt=fmt, fmt_=fmt_, decimal=decimal, status=status)
    if ( status == 0 ) call parse_int_i16(substring, fmt_, into, status)

    call report_cast(status, substring, stat)
  end procedure cast_char_to_i16

  module procedure cast_char_to_i8
    character(len=1) :: fmt_
    character(len=5) :: decimal
    integer          :: status

    into = 0_i8

    call cast_args(INT_FMTS, "i", fmt=fmt, fmt_=fmt_, decimal=decimal, status=status)
    if ( status == 0 ) call parse_int_i8(substring, fmt_, into, status)

    call report_cast(status, substring, stat)
  end procedure cast_char_to_i8


  ! Number formatting, shared by `String`, `str` and `cast` ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pure function int_fmt_of(fmt) result(fmt_)
    character(len=*), intent(in), optional :: fmt
    character(len=1)                       :: fmt_

    fmt_ = "i"
    if ( .not. present(fmt) ) return

    if ( .not. any(INT_FMTS == fmt) ) error stop 'Invalid fmt "'//fmt//'": must be one of the INT_FMTS.'
    fmt_ = fmt
  end function int_fmt_of

  pure function int_text(x, bits, fmt) result(text)
    !-------------------------------------------------------------------------------------------------------------------
    !! Returns the text of an integer of `bits` bits. A negative number in `fmt="z"` is its full bit pattern, and digits
    !! are found from a negative number in `fmt="i"`, since the most negative integer has no positive counterpart.
    !-------------------------------------------------------------------------------------------------------------------
    integer(i64),     intent(in)  :: x
    integer,          intent(in)  :: bits
    character(len=*), intent(in)  :: fmt
    character(len=:), allocatable :: text

    character(len=20) :: buffer
    integer(i64)      :: num
    integer           :: i, digit, width

    i = len(buffer)

    if ( fmt == "i" ) then
      num = x
      if ( x > 0_i64 ) num = -x

      do
        buffer(i:i) = achar(iachar("0") - int(mod(num, 10_i64)))
        num = num/10_i64
        if ( num == 0_i64 ) exit
        i = i - 1
      end do

      if ( x < 0_i64 ) then
        i = i - 1; buffer(i:i) = "-"
      end if
    else
      width = bits/4
      num   = x
      if ( bits < 64 ) num = ibits(x, 0, bits)

      do digit = 0, width - 1
        if ( (x >= 0_i64) .and. (digit > 0) .and. (ishft(num, -4*digit) == 0_i64) ) exit
        buffer(i-digit:i-digit) = DIGITS_A(ibits(num, 4*digit, 4))
      end do

      i = i - digit + 1
      i = i - 2; buffer(i:i+1) = "0x"
    end if

    text = buffer(i:)
  end function int_text

  pure function decimal_of(locale) result(decimal)
    character(len=*), intent(in), optional :: locale
    character(len=5)                       :: decimal

    decimal = "POINT"
    if ( .not. present(locale) ) return

    if ( locale == "EU" ) then
      decimal = "COMMA"
    else if ( locale /= "US" ) then
      error stop 'Invalid locale "'//locale//'": must be one of the LOCALES.'
    end if
  end function decimal_of

  pure function real_fmt_of(fmt) result(fmt_)
    character(len=*), intent(in), optional :: fmt
    character(len=1)                       :: fmt_

    fmt_ = "e"
    if ( .not. present(fmt) ) return

    if ( .not. any(REAL_FMTS == fmt) ) error stop 'Invalid fmt "'//fmt//'": must be one of the REAL_FMTS.'
    fmt_ = fmt
  end function real_fmt_of

  pure function zero_text(decimal, fmt, e_digits) result(text)
    character(len=*), intent(in)  :: decimal, fmt
    integer,          intent(in)  :: e_digits
    character(len=:), allocatable :: text

    character(len=1) :: mark

    mark = POINT
    if ( decimal == "COMMA" ) mark = COMMA

    text = "0"//mark//"0"
    if ( fmt == "e" ) text = text//"e+"//repeat("0", e_digits)
  end function zero_text

  pure function e_edit(width, d, e_digits) result(edit)
    !-------------------------------------------------------------------------------------------------------------------
    !! Returns the format `(esW.DeE)`. The edit descriptors are joined from digits, not written by an internal `write`,
    !! which would cost as much as writing the number itself.
    !-------------------------------------------------------------------------------------------------------------------
    integer, intent(in) :: width, d, e_digits
    character(len=32)   :: edit

    edit = "(es"//digits(width)//"."//digits(d)//"e"//digits(e_digits)//")"
  end function e_edit

  pure function f_edit(width, d) result(edit)
    integer, intent(in) :: width, d
    character(len=32)   :: edit

    edit = "(f"//digits(width)//"."//digits(d)//")"
  end function f_edit

  pure function e_field_text(field) result(text)
    !-------------------------------------------------------------------------------------------------------------------
    !! Returns a nonzero finite number written right-justified by an `es` edit descriptor, as the library writes it:
    !! without the leading blanks, and with a lowercase exponent letter.
    !-------------------------------------------------------------------------------------------------------------------
    character(len=*), intent(in)  :: field
    character(len=:), allocatable :: text

    integer :: i

    text = field(verify(field, SPACE):)
    i = index(text, "E"); text(i:i) = "e"
  end function e_field_text

  pure function digits(n) result(text)
    integer, intent(in)           :: n
    character(len=:), allocatable :: text

    text = int_text(int(n, kind=i64), 64, "i")
  end function digits

  pure function small_f_text(e_text, d) result(text)
    !-------------------------------------------------------------------------------------------------------------------
    !! Returns `e_text`, a number in `es` form with a negative exponent, in `f` form with `d` decimals. Very small
    !! numbers go through this because ifx writes only zeros past about 255 decimals with an `f` edit descriptor.
    !-------------------------------------------------------------------------------------------------------------------
    character(len=*), intent(in)  :: e_text
    integer,          intent(in)  :: d
    character(len=:), allocatable :: text

    integer :: first, i_e, power, target_len

    first = 1
    if ( e_text(1:1) == "-" ) first = 2

    i_e = index(e_text, "E")
    read(unit=e_text(i_e+1:), fmt=*) power

    text = e_text(:first-1)//"0"//e_text(first+1:first+1)//repeat("0", -power-1)// &
           e_text(first:first)//e_text(first+2:i_e-1)

    target_len = first + 1 + d

    if ( len(text) < target_len ) then
      text = text//repeat("0", target_len-len(text))
    else
      text = text(:target_len)
    end if
  end function small_f_text

  pure function join_complex(re, im_part, negative, decimal, fmt, im) result(text)
    character(len=*), intent(in)           :: re, im_part, decimal, fmt
    logical,          intent(in)           :: negative
    character(len=*), intent(in), optional :: im
    character(len=:), allocatable          :: text

    character(len=:), allocatable :: im_

    im_ = EMPTY_STR
    if ( present(im) ) im_ = im

    if ( im_ == EMPTY_STR ) then
      if ( decimal == "POINT" ) then
        text = "("//re//COMMA//im_part//")"
      else
        text = "("//re//SEMICOLON//im_part//")"
      end if
    else if ( negative .and. (fmt /= "z") ) then
      text = re//im_part//im_
    else
      text = re//"+"//im_part//im_
    end if
  end function join_complex

#ifdef IOFL_HAS_R128
  pure function real_text_r128(x, decimal, fmt, decimals) result(text)
    real(r128),        intent(in)           :: x
    character(len=*), intent(in)           :: decimal, fmt
    integer,          intent(in), optional :: decimals
    character(len=:), allocatable          :: text

    integer,          parameter :: E_DECIMALS = 35, E_DIGITS = 4, F_DIGITS = 36
    character(len=*), parameter :: E_FMT = "(es47.35e4)"

    character(len=64) :: field                                         ! Wider than any `es` text, at most 47 characters
    integer           :: d, e, i, n

    if ( fmt == "z" ) then
      if ( (x == 0e0_r128) .and. (.not. ieee_is_negative(x)) ) then
        text = "0x0"; return
      end if

      allocate( character(len=34) :: text )
      text(1:2) = "0x"
      write(unit=text(3:), fmt="(z32.32)") x

      do i = 3, 34
        if ( (text(i:i) >= "A") .and. (text(i:i) <= "F") ) text(i:i) = achar(iachar(text(i:i)) + 32)
      end do

      return
    end if

    if ( ieee_is_nan(x) ) then
      text = "NaN"; return
    else if ( .not. ieee_is_finite(x) ) then
      text = "Inf"
      if ( x < 0e0_r128 ) text = "-Inf"
      return
    end if

    if ( x == 0e0_r128 ) then
      text = zero_text(decimal, fmt, E_DIGITS)
      if ( ieee_is_negative(x) ) text = "-"//text
      return
    end if

    if ( fmt == "e" ) then
      d = E_DECIMALS
      if ( present(decimals) ) d = max(min(decimals, E_DECIMALS), 0)

      n = d + E_DIGITS + 8

      if ( d == E_DECIMALS ) then                            ! A constant format, which compilers can parse in advance
        write(unit=field(:n), fmt=E_FMT, decimal=decimal) x
      else
        write(unit=field(:n), fmt=e_edit(n, d, E_DIGITS), decimal=decimal) x
      end if

      text = e_field_text(field(:n))
    else
      if ( abs(x) >= 1e0_r128 ) then
        e = 1 + floor(log10(abs(x)))
      else
        e = int(log10(abs(x)))
      end if

      d = max(F_DIGITS - e, 0)
      if ( present(decimals) ) d = max(min(decimals, d), 0)

      if ( d > MAX_F_DECIMALS ) then
        n = max(d + floor(log10(abs(x))) + 1, 1)

        allocate( character(len=n+E_DIGITS+8) :: text )
        write(unit=text, fmt=e_edit(len(text), n-1, E_DIGITS), decimal=decimal) x
        text = small_f_text(trim(adjustl(text)), d); return
      end if

      allocate( character(len=max(e,1)+d+4) :: text )
      write(unit=text, fmt=f_edit(len(text), d), decimal=decimal) x
      text = trim(adjustl(text))
    end if
  end function real_text_r128
#endif

  pure function real_text_r64(x, decimal, fmt, decimals) result(text)
    real(r64),        intent(in)           :: x
    character(len=*), intent(in)           :: decimal, fmt
    integer,          intent(in), optional :: decimals
    character(len=:), allocatable          :: text

    integer,          parameter :: E_DECIMALS = 17, E_DIGITS = 3, F_DIGITS = 18
    character(len=*), parameter :: E_FMT = "(es28.17e3)"

    character(len=64) :: field                                         ! Wider than any `es` text, at most 47 characters
    integer           :: d, e, i, n

    if ( fmt == "z" ) then
      if ( (x == 0e0_r64) .and. (.not. ieee_is_negative(x)) ) then
        text = "0x0"; return
      end if

      call cast(transfer(source=x, mold=1_i64), into=text, fmt="z"); return
    end if

    if ( ieee_is_nan(x) ) then
      text = "NaN"; return
    else if ( .not. ieee_is_finite(x) ) then
      text = "Inf"
      if ( x < 0e0_r64 ) text = "-Inf"
      return
    end if

    if ( x == 0e0_r64 ) then
      text = zero_text(decimal, fmt, E_DIGITS)
      if ( ieee_is_negative(x) ) text = "-"//text
      return
    end if

    if ( fmt == "e" ) then
      d = E_DECIMALS
      if ( present(decimals) ) d = max(min(decimals, E_DECIMALS), 0)

      n = d + E_DIGITS + 8

      if ( d == E_DECIMALS ) then                            ! A constant format, which compilers can parse in advance
        write(unit=field(:n), fmt=E_FMT, decimal=decimal) x
      else
        write(unit=field(:n), fmt=e_edit(n, d, E_DIGITS), decimal=decimal) x
      end if

      text = e_field_text(field(:n))
    else
      if ( abs(x) >= 1e0_r64 ) then
        e = 1 + floor(log10(abs(x)))
      else
        e = int(log10(abs(x)))
      end if

      d = max(F_DIGITS - e, 0)
      if ( present(decimals) ) d = max(min(decimals, d), 0)

      if ( d > MAX_F_DECIMALS ) then
        n = max(d + floor(log10(abs(x))) + 1, 1)

        allocate( character(len=n+E_DIGITS+8) :: text )
        write(unit=text, fmt=e_edit(len(text), n-1, E_DIGITS), decimal=decimal) x
        text = small_f_text(trim(adjustl(text)), d); return
      end if

      allocate( character(len=max(e,1)+d+4) :: text )
      write(unit=text, fmt=f_edit(len(text), d), decimal=decimal) x
      text = trim(adjustl(text))
    end if
  end function real_text_r64

  pure function real_text_r32(x, decimal, fmt, decimals) result(text)
    real(r32),        intent(in)           :: x
    character(len=*), intent(in)           :: decimal, fmt
    integer,          intent(in), optional :: decimals
    character(len=:), allocatable          :: text

    integer,          parameter :: E_DECIMALS = 8, E_DIGITS = 2, F_DIGITS = 9
    character(len=*), parameter :: E_FMT = "(es18.8e2)"

    character(len=64) :: field                                         ! Wider than any `es` text, at most 47 characters
    integer           :: d, e, i, n

    if ( fmt == "z" ) then
      if ( (x == 0e0_r32) .and. (.not. ieee_is_negative(x)) ) then
        text = "0x0"; return
      end if

      call cast(transfer(source=x, mold=1_i32), into=text, fmt="z"); return
    end if

    if ( ieee_is_nan(x) ) then
      text = "NaN"; return
    else if ( .not. ieee_is_finite(x) ) then
      text = "Inf"
      if ( x < 0e0_r32 ) text = "-Inf"
      return
    end if

    if ( x == 0e0_r32 ) then
      text = zero_text(decimal, fmt, E_DIGITS)
      if ( ieee_is_negative(x) ) text = "-"//text
      return
    end if

    if ( fmt == "e" ) then
      d = E_DECIMALS
      if ( present(decimals) ) d = max(min(decimals, E_DECIMALS), 0)

      n = d + E_DIGITS + 8

      if ( d == E_DECIMALS ) then                            ! A constant format, which compilers can parse in advance
        write(unit=field(:n), fmt=E_FMT, decimal=decimal) x
      else
        write(unit=field(:n), fmt=e_edit(n, d, E_DIGITS), decimal=decimal) x
      end if

      text = e_field_text(field(:n))
    else
      if ( abs(x) >= 1e0_r32 ) then
        e = 1 + floor(log10(abs(x)))
      else
        e = int(log10(abs(x)))
      end if

      d = max(F_DIGITS - e, 0)
      if ( present(decimals) ) d = max(min(decimals, d), 0)

      if ( d > MAX_F_DECIMALS ) then
        n = max(d + floor(log10(abs(x))) + 1, 1)

        allocate( character(len=n+E_DIGITS+8) :: text )
        write(unit=text, fmt=e_edit(len(text), n-1, E_DIGITS), decimal=decimal) x
        text = small_f_text(trim(adjustl(text)), d); return
      end if

      allocate( character(len=max(e,1)+d+4) :: text )
      write(unit=text, fmt=f_edit(len(text), d), decimal=decimal) x
      text = trim(adjustl(text))
    end if
  end function real_text_r32

#ifdef IOFL_HAS_R128
  pure function complex_text_c128(x, decimal, fmt, decimals, im) result(text)
    complex(r128),     intent(in)           :: x
    character(len=*), intent(in)           :: decimal, fmt
    integer,          intent(in), optional :: decimals
    character(len=*), intent(in), optional :: im
    character(len=:), allocatable          :: text

    text = join_complex( real_text_r128(x%re, decimal, fmt, decimals), &
                         real_text_r128(x%im, decimal, fmt, decimals), &
                         ieee_is_negative(x%im), decimal, fmt, im )
  end function complex_text_c128
#endif

  pure function complex_text_c64(x, decimal, fmt, decimals, im) result(text)
    complex(r64),     intent(in)           :: x
    character(len=*), intent(in)           :: decimal, fmt
    integer,          intent(in), optional :: decimals
    character(len=*), intent(in), optional :: im
    character(len=:), allocatable          :: text

    text = join_complex( real_text_r64(x%re, decimal, fmt, decimals), &
                         real_text_r64(x%im, decimal, fmt, decimals), &
                         ieee_is_negative(x%im), decimal, fmt, im )
  end function complex_text_c64

  pure function complex_text_c32(x, decimal, fmt, decimals, im) result(text)
    complex(r32),     intent(in)           :: x
    character(len=*), intent(in)           :: decimal, fmt
    integer,          intent(in), optional :: decimals
    character(len=*), intent(in), optional :: im
    character(len=:), allocatable          :: text

    text = join_complex( real_text_r32(x%re, decimal, fmt, decimals), &
                         real_text_r32(x%im, decimal, fmt, decimals), &
                         ieee_is_negative(x%im), decimal, fmt, im )
  end function complex_text_c32


  ! Number parsing, shared by `cast` for `character` and `String` ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pure subroutine cast_args(fmts, default_fmt, fmt, locale, fmt_, decimal, status)
    character(len=*), intent(in)           :: fmts(:), default_fmt
    character(len=*), intent(in), optional :: fmt, locale
    character(len=1), intent(out)          :: fmt_
    character(len=5), intent(out)          :: decimal
    integer,          intent(out)          :: status

    status = 0; fmt_ = default_fmt; decimal = "POINT"

    if ( present(fmt) ) then
      if ( any(fmts == fmt) ) then
        fmt_ = fmt
      else
        status = ARG_ERR
      end if
    end if

    if ( present(locale) ) then
      if ( locale == "EU" ) then
        decimal = "COMMA"
      else if ( locale /= "US" ) then
        status = ARG_ERR
      end if
    end if
  end subroutine cast_args

  pure subroutine report_cast(status, text, stat)
    integer,          intent(in)            :: status
    character(len=*), intent(in)            :: text
    integer,          intent(out), optional :: stat

    if ( present(stat) ) then
      stat = status
    else if ( status == ARG_ERR ) then
      error stop "Invalid fmt or locale in cast."
    else if ( status /= 0 ) then
      error stop 'Invalid number in cast: "'//text(:min(len(text),64))//'"'
    end if
  end subroutine report_cast

  pure subroutine trimmed_bounds(text, l, r)
    character(len=*), intent(in)  :: text
    integer,          intent(out) :: l, r

    l = 1; r = len(text)

    do while ( l <= r )
      if ( .not. is_space(text(l:l)) ) exit
      l = l + 1
    end do

    do while ( r >= l )
      if ( .not. is_space(text(r:r)) ) exit
      r = r - 1
    end do
  end subroutine trimmed_bounds

  elemental function is_space(c) result(blank)
    character(len=1), intent(in) :: c
    logical                      :: blank

    blank = (c == SPACE) .or. (c == TAB) .or. (c == CR) .or. (c == LF)
  end function is_space

  elemental function digit_value(c, base) result(digit)
    character(len=1), intent(in) :: c
    integer,          intent(in) :: base
    integer                      :: digit

    digit = -1

    if ( (c >= "0") .and. (c <= "9") ) then
      digit = iachar(c) - iachar("0")
    else if ( (base == 16) .and. (c >= "a") .and. (c <= "f") ) then
      digit = iachar(c) - iachar("a") + 10
    else if ( (base == 16) .and. (c >= "A") .and. (c <= "F") ) then
      digit = iachar(c) - iachar("A") + 10
    end if
  end function digit_value

  pure subroutine hex_bounds(text, max_digits, l, r, status)
    !-------------------------------------------------------------------------------------------------------------------
    !! Finds the hexadecimal digits in `text`, after any "0x". Leading zeros do not count toward `max_digits`.
    !-------------------------------------------------------------------------------------------------------------------
    character(len=*), intent(in)  :: text
    integer,          intent(in)  :: max_digits
    integer,          intent(out) :: l, r, status

    integer :: i, significant

    status = READ_ERR
    call trimmed_bounds(text, l, r)

    if ( r - l >= 1 ) then
      if ( (text(l:l) == "0") .and. ((text(l+1:l+1) == "x") .or. (text(l+1:l+1) == "X")) ) l = l + 2
    end if

    if ( l > r ) return

    significant = 0

    do i = l, r
      if ( digit_value(text(i:i), 16) < 0 ) return
      if ( (significant > 0) .or. (text(i:i) /= "0") ) significant = significant + 1
    end do

    if ( significant <= max_digits ) status = 0
  end subroutine hex_bounds

  pure subroutine parse_integer(text, fmt, bits, value, status)
    !-------------------------------------------------------------------------------------------------------------------
    !! Parses an integer that fits in `bits` bits. Decimal digits accumulate as a negative number, since the most
    !! negative integer has no positive counterpart. Hexadecimal digits are the bit pattern, so "0xff" is -1 for 8 bits.
    !-------------------------------------------------------------------------------------------------------------------
    character(len=*), intent(in)  :: text, fmt
    integer,          intent(in)  :: bits
    integer(i64),     intent(out) :: value
    integer,          intent(out) :: status

    integer(i64) :: smallest
    integer      :: i, l, r, digit
    logical      :: negative

    value = 0_i64

    if ( fmt == "z" ) then
      call hex_bounds(text, bits/4, l, r, status)
      if ( status /= 0 ) return

      do i = l, r
        value = ior(ishft(value, 4), int(digit_value(text(i:i), 16), i64))
      end do

      if ( bits < 64 ) then
        if ( btest(value, bits-1) ) value = ior(value, ishft(-1_i64, bits))
      end if

      return
    end if

    status = READ_ERR
    call trimmed_bounds(text, l, r)
    if ( l > r ) return

    negative = (text(l:l) == "-")
    if ( negative .or. (text(l:l) == "+") ) l = l + 1
    if ( l > r ) return

    smallest = ishft(-1_i64, bits-1)

    do i = l, r
      digit = digit_value(text(i:i), 10)

      if ( digit < 0 ) then
        value = 0_i64; return
      end if

      if ( value < (smallest + digit)/10_i64 ) then
        value = 0_i64; return
      end if

      value = 10_i64*value - digit
    end do

    if ( .not. negative ) then
      if ( value == smallest ) then
        value = 0_i64; return
      end if

      value = -value
    end if

    status = 0
  end subroutine parse_integer

  pure function lowercase(text) result(lower)
    character(len=*), intent(in) :: text
    character(len=len(text))     :: lower

    integer :: i

    do i = 1, len(text)
      lower(i:i) = text(i:i)
      if ( (text(i:i) >= "A") .and. (text(i:i) <= "Z") ) lower(i:i) = achar(iachar(text(i:i)) + 32)
    end do
  end function lowercase

  pure function is_real_text(text, decimal) result(valid)
    !-------------------------------------------------------------------------------------------------------------------
    !! Checks for an optional sign, then NaN, Inf, Infinity, or digits with an optional decimal mark and exponent. This
    !! comes before list-directed input, which would otherwise accept forms such as "2*3.5", "1 2", "7,junk" and "/".
    !-------------------------------------------------------------------------------------------------------------------
    character(len=*), intent(in) :: text, decimal
    logical                      :: valid

    character(len=1) :: mark
    integer          :: i, n, digits

    valid = .false.; n = len(text); i = 1
    if ( n < 1 ) return

    mark = POINT
    if ( decimal == "COMMA" ) mark = COMMA

    if ( (text(1:1) == "+") .or. (text(1:1) == "-") ) i = 2
    if ( i > n ) return

    if ( index("nNiI", text(i:i)) > 0 ) then
      valid = (lowercase(text(i:)) == "nan") .or. (lowercase(text(i:)) == "inf") .or. &
              (lowercase(text(i:)) == "infinity")
      return
    end if

    digits = 0

    do while ( i <= n )
      if ( digit_value(text(i:i), 10) < 0 ) exit
      digits = digits + 1; i = i + 1
    end do

    if ( i <= n ) then
      if ( text(i:i) == mark ) then
        i = i + 1

        do while ( i <= n )
          if ( digit_value(text(i:i), 10) < 0 ) exit
          digits = digits + 1; i = i + 1
        end do
      end if
    end if

    if ( digits == 0 ) return

    if ( i > n ) then
      valid = .true.; return
    end if

    if ( index("eEdDqQ", text(i:i)) == 0 ) return
    i = i + 1
    if ( i > n ) return

    if ( (text(i:i) == "+") .or. (text(i:i) == "-") ) i = i + 1
    if ( i > n ) return

    do while ( i <= n )
      if ( digit_value(text(i:i), 10) < 0 ) return
      i = i + 1
    end do

    valid = .true.
  end function is_real_text

  pure subroutine split_complex(text, decimal, fmt, im, re_l, re_r, im_l, im_r, status)
    !-------------------------------------------------------------------------------------------------------------------
    !! Finds the real and imaginary parts of `text`, either "(re,im)" or "(re;im)" by locale, or "re+imj" with `im` as
    !! the imaginary unit. Only the bounds of the parts are checked here, as they are parsed as real numbers afterward.
    !-------------------------------------------------------------------------------------------------------------------
    character(len=*), intent(in)           :: text, decimal, fmt
    character(len=*), intent(in), optional :: im
    integer,          intent(out)          :: re_l, re_r, im_l, im_r, status

    character(len=1) :: separator
    integer          :: i, l, r, im_len

    re_l = 1; re_r = 0; im_l = 1; im_r = 0
    status = READ_ERR

    call trimmed_bounds(text, l, r)
    if ( l > r ) return

    im_len = 0
    if ( present(im) ) im_len = len(im)

    if ( im_len == 0 ) then
      if ( (r - l < 2) .or. (text(l:l) /= "(") .or. (text(r:r) /= ")") ) return

      separator = COMMA
      if ( decimal == "COMMA" ) separator = SEMICOLON

      i = index(text(l+1:r-1), separator)
      if ( i == 0 ) return

      i = l + i
      re_l = l + 1; re_r = i - 1; im_l = i + 1; im_r = r - 1
      status = 0; return
    end if

    if ( r - l + 1 <= im_len ) return
    if ( text(r-im_len+1:r) /= im ) return

    r = r - im_len

    do i = l + 1, r
      if ( (text(i:i) /= "+") .and. ((fmt == "z") .or. (text(i:i) /= "-")) ) cycle
      if ( (fmt /= "z") .and. (index("eEdDqQ", text(i-1:i-1)) /= 0) ) cycle

      re_l = l; re_r = i - 1; im_r = r

      im_l = i
      if ( (fmt == "z") .or. (text(i:i) == "+") ) im_l = i + 1

      status = 0; return
    end do
  end subroutine split_complex

  pure subroutine parse_int_i64(text, fmt, value, status)
    character(len=*), intent(in)  :: text, fmt
    integer(i64),   intent(out) :: value
    integer,          intent(out) :: status

    integer(i64) :: number

    call parse_integer(text, fmt, int(bit_size(value)), number, status)
    value = int(number, i64)
  end subroutine parse_int_i64

  pure subroutine parse_int_i32(text, fmt, value, status)
    character(len=*), intent(in)  :: text, fmt
    integer(i32),   intent(out) :: value
    integer,          intent(out) :: status

    integer(i64) :: number

    call parse_integer(text, fmt, int(bit_size(value)), number, status)
    value = int(number, i32)
  end subroutine parse_int_i32

  pure subroutine parse_int_i16(text, fmt, value, status)
    character(len=*), intent(in)  :: text, fmt
    integer(i16),   intent(out) :: value
    integer,          intent(out) :: status

    integer(i64) :: number

    call parse_integer(text, fmt, int(bit_size(value)), number, status)
    value = int(number, i16)
  end subroutine parse_int_i16

  pure subroutine parse_int_i8(text, fmt, value, status)
    character(len=*), intent(in)  :: text, fmt
    integer(i8),   intent(out) :: value
    integer,          intent(out) :: status

    integer(i64) :: number

    call parse_integer(text, fmt, int(bit_size(value)), number, status)
    value = int(number, i8)
  end subroutine parse_int_i8


#ifdef IOFL_HAS_R128
  pure subroutine parse_real_r128(text, decimal, fmt, value, status)
    character(len=*), intent(in)  :: text, decimal, fmt
    real(r128),        intent(out) :: value
    integer,          intent(out) :: status

    integer :: l, r

    value = 0e0_r128

    if ( fmt == "z" ) then
      call hex_bounds(text, 32, l, r, status)
      if ( status /= 0 ) return

      read(unit=text(l:r), fmt="(z100)", iostat=status) value

      if ( status /= 0 ) then
        value = 0e0_r128; status = READ_ERR
      end if

      return
    end if

    call trimmed_bounds(text, l, r)

    if ( .not. is_real_text(text(l:r), decimal) ) then
      status = READ_ERR; return
    end if

    read(unit=text(l:r), fmt=*, decimal=decimal, iostat=status) value

    if ( status /= 0 ) then
      value = 0e0_r128; status = READ_ERR
    end if
  end subroutine parse_real_r128
#endif

  pure subroutine parse_real_r64(text, decimal, fmt, value, status)
    character(len=*), intent(in)  :: text, decimal, fmt
    real(r64),        intent(out) :: value
    integer,          intent(out) :: status

    integer(i64) :: number
    integer :: l, r

    value = 0e0_r64

    if ( fmt == "z" ) then
      call parse_int_i64(text, "z", number, status)
      if ( status == 0 ) value = transfer(source=number, mold=value)

      return
    end if

    call trimmed_bounds(text, l, r)

    if ( .not. is_real_text(text(l:r), decimal) ) then
      status = READ_ERR; return
    end if

    read(unit=text(l:r), fmt=*, decimal=decimal, iostat=status) value

    if ( status /= 0 ) then
      value = 0e0_r64; status = READ_ERR
    end if
  end subroutine parse_real_r64

  pure subroutine parse_real_r32(text, decimal, fmt, value, status)
    character(len=*), intent(in)  :: text, decimal, fmt
    real(r32),        intent(out) :: value
    integer,          intent(out) :: status

    integer(i32) :: number
    integer :: l, r

    value = 0e0_r32

    if ( fmt == "z" ) then
      call parse_int_i32(text, "z", number, status)
      if ( status == 0 ) value = transfer(source=number, mold=value)

      return
    end if

    call trimmed_bounds(text, l, r)

    if ( .not. is_real_text(text(l:r), decimal) ) then
      status = READ_ERR; return
    end if

    read(unit=text(l:r), fmt=*, decimal=decimal, iostat=status) value

    if ( status /= 0 ) then
      value = 0e0_r32; status = READ_ERR
    end if
  end subroutine parse_real_r32

#ifdef IOFL_HAS_R128
  pure subroutine parse_complex_c128(text, decimal, fmt, im, value, status)
    character(len=*), intent(in)           :: text, decimal, fmt
    character(len=*), intent(in), optional :: im
    complex(r128),     intent(out)          :: value
    integer,          intent(out)          :: status

    real(r128) :: re_part, im_part
    integer   :: re_l, re_r, im_l, im_r

    value = (0e0_r128,0e0_r128)

    call split_complex(text, decimal, fmt, im, re_l, re_r, im_l, im_r, status)
    if ( status /= 0 ) return

    call parse_real_r128(text(re_l:re_r), decimal, fmt, re_part, status)
    if ( status /= 0 ) return

    call parse_real_r128(text(im_l:im_r), decimal, fmt, im_part, status)
    if ( status == 0 ) value = cmplx(re_part, im_part, kind=r128)
  end subroutine parse_complex_c128
#endif

  pure subroutine parse_complex_c64(text, decimal, fmt, im, value, status)
    character(len=*), intent(in)           :: text, decimal, fmt
    character(len=*), intent(in), optional :: im
    complex(r64),     intent(out)          :: value
    integer,          intent(out)          :: status

    real(r64) :: re_part, im_part
    integer   :: re_l, re_r, im_l, im_r

    value = (0e0_r64,0e0_r64)

    call split_complex(text, decimal, fmt, im, re_l, re_r, im_l, im_r, status)
    if ( status /= 0 ) return

    call parse_real_r64(text(re_l:re_r), decimal, fmt, re_part, status)
    if ( status /= 0 ) return

    call parse_real_r64(text(im_l:im_r), decimal, fmt, im_part, status)
    if ( status == 0 ) value = cmplx(re_part, im_part, kind=r64)
  end subroutine parse_complex_c64

  pure subroutine parse_complex_c32(text, decimal, fmt, im, value, status)
    character(len=*), intent(in)           :: text, decimal, fmt
    character(len=*), intent(in), optional :: im
    complex(r32),     intent(out)          :: value
    integer,          intent(out)          :: status

    real(r32) :: re_part, im_part
    integer   :: re_l, re_r, im_l, im_r

    value = (0e0_r32,0e0_r32)

    call split_complex(text, decimal, fmt, im, re_l, re_r, im_l, im_r, status)
    if ( status /= 0 ) return

    call parse_real_r32(text(re_l:re_r), decimal, fmt, re_part, status)
    if ( status /= 0 ) return

    call parse_real_r32(text(im_l:im_r), decimal, fmt, im_part, status)
    if ( status == 0 ) value = cmplx(re_part, im_part, kind=r32)
  end subroutine parse_complex_c32

end submodule io_internal
