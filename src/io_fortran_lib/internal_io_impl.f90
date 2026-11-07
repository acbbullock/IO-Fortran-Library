#include "iofl_config.inc"
submodule (io_fortran_lib) internal_io
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
  integer(i64), parameter :: largest_i64  =   huge(1_i64)         ! Largest 64-bit signed integer
  integer(i32), parameter :: largest_i32  =   huge(1_i32)         ! Largest 32-bit signed integer
  integer(i16), parameter :: largest_i16  =   huge(1_i16)         ! Largest 16-bit signed integer
  integer(i8),  parameter :: largest_i8   =   huge(1_i8)          ! Largest 8-bit signed integer
  integer(i64), parameter :: smallest_i64 = - huge(1_i64) - 1_i64 ! Smallest 64-bit signed integer
  integer(i32), parameter :: smallest_i32 = - huge(1_i32) - 1_i32 ! Smallest 32-bit signed integer
  integer(i16), parameter :: smallest_i16 = - huge(1_i16) - 1_i16 ! Smallest 16-bit signed integer
  integer(i8),  parameter :: smallest_i8  = - huge(1_i8)  - 1_i8  ! Smallest 8-bit signed integer

  ! Define powers of 10 for each storage size:
  integer(i64), parameter :: TENS_i64(0:18) = int([ 1e0_r64,  1e1_r64,  1e2_r64,  1e3_r64,  &
                                                    1e4_r64,  1e5_r64,  1e6_r64,  1e7_r64,  &
                                                    1e8_r64,  1e9_r64,  1e10_r64, 1e11_r64, &
                                                    1e12_r64, 1e13_r64, 1e14_r64, 1e15_r64, &
                                                    1e16_r64, 1e17_r64, 1e18_r64 ],         &
                                                    kind=i64                                )
  integer(i32), parameter :: TENS_i32(0:9)  = int([ 1e0_r32, 1e1_r32, 1e2_r32, 1e3_r32,     &
                                                    1e4_r32, 1e5_r32, 1e6_r32, 1e7_r32,     &
                                                    1e8_r32, 1e9_r32 ],                     &
                                                    kind=i32                                )
  integer(i16), parameter :: TENS_i16(0:4)  = int([ 1e0_r32, 1e1_r32, 1e2_r32, 1e3_r32,     &
                                                    1e4_r32 ],                              &
                                                    kind=i16                                )
  integer(i8),  parameter :: TENS_i8(0:2)   = int([ 1e0_r32, 1e1_r32, 1e2_r32 ],            &
                                                    kind=i8                                 )

  ! Define powers of 16 for each storage size:
  integer(i64), parameter :: SIXTEENS_i64(0:15) = [ 16_i64**0,  16_i64**1,  16_i64**2,  16_i64**3,  &
                                                    16_i64**4,  16_i64**5,  16_i64**6,  16_i64**7,  &
                                                    16_i64**8,  16_i64**9,  16_i64**10, 16_i64**11, &
                                                    16_i64**12, 16_i64**13, 16_i64**14, 16_i64**15  ]
  integer(i32), parameter :: SIXTEENS_i32(0:7)  = [ 16_i32**0,  16_i32**1,  16_i32**2,  16_i32**3,  &
                                                    16_i32**4,  16_i32**5,  16_i32**6,  16_i32**7   ]
  integer(i16), parameter :: SIXTEENS_i16(0:3)  = [ 16_i16**0,  16_i16**1,  16_i16**2,  16_i16**3   ]
  integer(i8),  parameter :: SIXTEENS_i8(0:1)   = [ 16_i8**0,   16_i8**1                            ]

  ! Define hex string digits:
  character(len=1), parameter :: DIGITS_A(0:15) = ["0","1","2","3","4","5","6","7","8","9","a","b","c","d","e","f"]

  contains ! Procedure bodies for module subprograms <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

  ! String ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ifdef IOFL_HAS_R128
  module procedure new_string_from_c128
    new%s = complex_text(x, decimal_of(locale), real_fmt_of(fmt), decimals, im)
  end procedure new_string_from_c128
#endif
  module procedure new_string_from_c64
    new%s = complex_text(x, decimal_of(locale), real_fmt_of(fmt), decimals, im)
  end procedure new_string_from_c64
  module procedure new_string_from_c32
    new%s = complex_text(x, decimal_of(locale), real_fmt_of(fmt), decimals, im)
  end procedure new_string_from_c32

#ifdef IOFL_HAS_R128
  module procedure new_string_from_r128
    new%s = real_text(x, decimal_of(locale), real_fmt_of(fmt), decimals)
  end procedure new_string_from_r128
#endif
  module procedure new_string_from_r64
    new%s = real_text(x, decimal_of(locale), real_fmt_of(fmt), decimals)
  end procedure new_string_from_r64
  module procedure new_string_from_r32
    new%s = real_text(x, decimal_of(locale), real_fmt_of(fmt), decimals)
  end procedure new_string_from_r32

  module procedure new_string_from_i64
    character(len=1)  :: fmt_
    character(len=20) :: buffer
    integer(i64)      :: num, next
    integer           :: ascii_code, i
    logical           :: negative

    num=0_i64; next=0_i64; ascii_code=0; i=0; negative=.false.

    if ( .not. present(fmt) ) then
      fmt_ = "i"
    else
      if ( any(INT_FMTS == fmt) ) then
        fmt_ = fmt
      else
        error stop 'Invalid fmt "'//fmt//'": must be one of the INT_FMTS.'
      end if
    end if

    if ( fmt_ == "i" ) then
      if ( x < 0_i64 ) then
        if ( x == smallest_i64 ) then
          new%s = "-9223372036854775808"; return
        end if
        num = -x; negative = .true.
      else
        num = x; negative = .false.
      end if

      i = len(buffer); extract_digits: do
        next = num/10_i64; buffer(i:i) = achar(num - 10_i64*next + 48_i64); if ( next == 0_i64 ) exit
        num = next; i = i - 1; cycle
      end do extract_digits

      if ( negative ) then
        buffer(i-1:i-1) = "-"; new%s = buffer(i-1:); return
      else
        new%s = buffer(i:); return
      end if
    else
      if ( x < 0_i64 ) then
        num = (x + 1_i64) + largest_i64; negative = .true.; buffer(3:) = "0x0000000000000000"
      else
        num = x; negative = .false.
      end if

      i = len(buffer); extract_hex_digits: do
        next = num/16_i64; buffer(i:i) = DIGITS_A(num - 16_i64*next); if ( next == 0_i64 ) exit
        num = next; i = i - 1; cycle
      end do extract_hex_digits

      if ( negative ) then
        ascii_code = iachar(buffer(5:5))
        if ( ascii_code < 50 ) then
          buffer(5:5) = achar(ascii_code + 8)
        else
          buffer(5:5) = achar(ascii_code + 47)
        end if
        new%s = buffer(3:); return
      else
        buffer(i-2:i-1) = "0x"; new%s = buffer(i-2:); return
      end if
    end if
  end procedure new_string_from_i64
  module procedure new_string_from_i32
    character(len=1)  :: fmt_
    character(len=11) :: buffer
    integer           :: num, next, ascii_code, i
    logical           :: negative

    num=0; next=0; ascii_code=0; i=0; negative=.false.

    if ( .not. present(fmt) ) then
      fmt_ = "i"
    else
      if ( any(INT_FMTS == fmt) ) then
        fmt_ = fmt
      else
        error stop 'Invalid fmt "'//fmt//'": must be one of the INT_FMTS.'
      end if
    end if

    if ( fmt_ == "i" ) then
      if ( x < 0 ) then
        if ( x == smallest_i32 ) then
          new%s = "-2147483648"; return
        end if
        num = -x; negative = .true.
      else
        num = x; negative = .false.
      end if

      i = len(buffer); extract_digits: do
        next = num/10; buffer(i:i) = achar(num - 10*next + 48); if ( next == 0 ) exit
        num = next; i = i - 1; cycle
      end do extract_digits

      if ( negative ) then
        buffer(i-1:i-1) = "-"; new%s = buffer(i-1:); return
      else
        new%s = buffer(i:); return
      end if
    else
      if ( x < 0 ) then
        num = (x + 1) + largest_i32; negative = .true.; buffer(2:) = "0x00000000"
      else
        num = x; negative = .false.
      end if

      i = len(buffer); extract_hex_digits: do
        next = num/16; buffer(i:i) = DIGITS_A(num - 16*next); if ( next == 0 ) exit
        num = next; i = i - 1; cycle
      end do extract_hex_digits

      if ( negative ) then
        ascii_code = iachar(buffer(4:4))
        if ( ascii_code < 50 ) then
          buffer(4:4) = achar(ascii_code + 8)
        else
          buffer(4:4) = achar(ascii_code + 47)
        end if
        new%s = buffer(2:); return
      else
        buffer(i-2:i-1) = "0x"; new%s = buffer(i-2:); return
      end if
    end if
  end procedure new_string_from_i32
  module procedure new_string_from_i16
    character(len=1) :: fmt_
    character(len=6) :: buffer
    integer          :: num, next, ascii_code, i
    logical          :: negative

    if ( .not. present(fmt) ) then
      fmt_ = "i"
    else
      if ( any(INT_FMTS == fmt) ) then
        fmt_ = fmt
      else
        error stop 'Invalid fmt "'//fmt//'": must be one of the INT_FMTS.'
      end if
    end if

    if ( fmt_ == "i" ) then
      if ( x < 0_i16 ) then
        if ( x == smallest_i16 ) then
          new%s = "-32768"; return
        end if
        num = int(-x); negative = .true.
      else
        num = int(x); negative = .false.
      end if

      i = len(buffer); extract_digits: do
        next = num/10; buffer(i:i) = achar(num - 10*next + 48); if ( next == 0 ) exit
        num = next; i = i - 1; cycle
      end do extract_digits

      if ( negative ) then
        buffer(i-1:i-1) = "-"; new%s = buffer(i-1:); return
      else
        new%s = buffer(i:); return
      end if
    else
      if ( x < 0_i16 ) then
        num = int((x + 1_i16) + largest_i16); negative = .true.; buffer(1:) = "0x0000"
      else
        num = int(x); negative = .false.
      end if

      i = len(buffer); extract_hex_digits: do
        next = num/16; buffer(i:i) = DIGITS_A(num - 16*next); if ( next == 0 ) exit
        num = next; i = i - 1; cycle
      end do extract_hex_digits

      if ( negative ) then
        ascii_code = iachar(buffer(3:3))
        if ( ascii_code < 50 ) then
          buffer(3:3) = achar(ascii_code + 8)
        else
          buffer(3:3) = achar(ascii_code + 47)
        end if
        new%s = buffer(1:); return
      else
        buffer(i-2:i-1) = "0x"; new%s = buffer(i-2:); return
      end if
    end if
  end procedure new_string_from_i16
  module procedure new_string_from_i8
    character(len=1) :: fmt_
    character(len=4) :: buffer
    integer          :: num, next, ascii_code, i
    logical          :: negative

    if ( .not. present(fmt) ) then
      fmt_ = "i"
    else
      if ( any(INT_FMTS == fmt) ) then
        fmt_ = fmt
      else
        error stop 'Invalid fmt "'//fmt//'": must be one of the INT_FMTS.'
      end if
    end if

    if ( fmt_ == "i" ) then
      if ( x < 0_i8 ) then
        if ( x == smallest_i8 ) then
          new%s = "-128"; return
        end if
        num = int(-x); negative = .true.
      else
        num = int(x); negative = .false.
      end if

      i = len(buffer); extract_digits: do
        next = num/10; buffer(i:i) = achar(num - 10*next + 48); if ( next == 0 ) exit
        num = next; i = i - 1; cycle
      end do extract_digits

      if ( negative ) then
        buffer(i-1:i-1) = "-"; new%s = buffer(i-1:); return
      else
        new%s = buffer(i:); return
      end if
    else
      if ( x < 0_i8 ) then
        num = int((x + 1_i8) + largest_i8); negative = .true.; buffer(1:) = "0x00"
      else
        num = int(x); negative = .false.
      end if

      i = len(buffer); extract_hex_digits: do
        next = num/16; buffer(i:i) = DIGITS_A(num - 16*next); if ( next == 0 ) exit
        num = next; i = i - 1; cycle
      end do extract_hex_digits

      if ( negative ) then
        ascii_code = iachar(buffer(3:3))
        if ( ascii_code < 50 ) then
          buffer(3:3) = achar(ascii_code + 8)
        else
          buffer(3:3) = achar(ascii_code + 47)
        end if
        new%s = buffer(1:); return
      else
        buffer(i-2:i-1) = "0x"; new%s = buffer(i-2:); return
      end if
    end if
  end procedure new_string_from_i8

  module procedure new_string_from_string
    if ( x%len() < 1 ) then
      new%s = EMPTY_STR
    else
      new%s = x%s
    end if
  end procedure new_string_from_string
  module procedure new_string_from_char
    new%s = x
  end procedure new_string_from_char
  module procedure new_string_from_empty
    new%s = EMPTY_STR
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
    character(len=1)  :: fmt_
    character(len=20) :: buffer
    integer(i64)      :: num, next
    integer           :: ascii_code, i
    logical           :: negative

    num=0_i64; next=0_i64; ascii_code=0; i=0; negative=.false.

    if ( .not. present(fmt) ) then
      fmt_ = "i"
    else
      if ( any(INT_FMTS == fmt) ) then
        fmt_ = fmt
      else
        error stop 'Invalid fmt "'//fmt//'": must be one of the INT_FMTS.'
      end if
    end if

    if ( fmt_ == "i" ) then
      if ( x < 0_i64 ) then
        if ( x == smallest_i64 ) then
          x_str = "-9223372036854775808"; return
        end if
        num = -x; negative = .true.
      else
        num = x; negative = .false.
      end if

      i = len(buffer); extract_digits: do
        next = num/10_i64; buffer(i:i) = achar(num - 10_i64*next + 48_i64); if ( next == 0_i64 ) exit
        num = next; i = i - 1; cycle
      end do extract_digits

      if ( negative ) then
        buffer(i-1:i-1) = "-"; x_str = buffer(i-1:); return
      else
        x_str = buffer(i:); return
      end if
    else
      if ( x < 0_i64 ) then
        num = (x + 1_i64) + largest_i64; negative = .true.; buffer(3:) = "0x0000000000000000"
      else
        num = x; negative = .false.
      end if

      i = len(buffer); extract_hex_digits: do
        next = num/16_i64; buffer(i:i) = DIGITS_A(num - 16_i64*next); if ( next == 0_i64 ) exit
        num = next; i = i - 1; cycle
      end do extract_hex_digits

      if ( negative ) then
        ascii_code = iachar(buffer(5:5))
        if ( ascii_code < 50 ) then
          buffer(5:5) = achar(ascii_code + 8)
        else
          buffer(5:5) = achar(ascii_code + 47)
        end if
        x_str = buffer(3:); return
      else
        buffer(i-2:i-1) = "0x"; x_str = buffer(i-2:); return
      end if
    end if
  end procedure str_from_i64
  module procedure str_from_i32
    character(len=1)  :: fmt_
    character(len=11) :: buffer
    integer           :: num, next, ascii_code, i
    logical           :: negative

    num=0; next=0; ascii_code=0; i=0; negative=.false.

    if ( .not. present(fmt) ) then
      fmt_ = "i"
    else
      if ( (any(INT_FMTS == fmt)) ) then
        fmt_ = fmt
      else
        error stop 'Invalid fmt "'//fmt//'": must be one of the INT_FMTS.'
      end if
    end if

    if ( fmt_ == "i" ) then
      if ( x < 0 ) then
        if ( x == smallest_i32 ) then
          x_str = "-2147483648"; return
        end if
        num = -x; negative = .true.
      else
        num = x; negative = .false.
      end if

      i = len(buffer); extract_digits: do
        next = num/10; buffer(i:i) = achar(num - 10*next + 48); if ( next == 0 ) exit
        num = next; i = i - 1; cycle
      end do extract_digits

      if ( negative ) then
        buffer(i-1:i-1) = "-"; x_str = buffer(i-1:); return
      else
        x_str = buffer(i:); return
      end if
    else
      if ( x < 0 ) then
        num = (x + 1) + largest_i32; negative = .true.; buffer(2:) = "0x00000000"
      else
        num = x; negative = .false.
      end if

      i = len(buffer); extract_hex_digits: do
        next = num/16; buffer(i:i) = DIGITS_A(num - 16*next); if ( next == 0 ) exit
        num = next; i = i - 1; cycle
      end do extract_hex_digits

      if ( negative ) then
        ascii_code = iachar(buffer(4:4))
        if ( ascii_code < 50 ) then
          buffer(4:4) = achar(ascii_code + 8)
        else
          buffer(4:4) = achar(ascii_code + 47)
        end if
        x_str = buffer(2:); return
      else
        buffer(i-2:i-1) = "0x"; x_str = buffer(i-2:); return
      end if
    end if
  end procedure str_from_i32
  module procedure str_from_i16
    character(len=1) :: fmt_
    character(len=6) :: buffer
    integer          :: num, next, ascii_code, i
    logical          :: negative

    if ( .not. present(fmt) ) then
      fmt_ = "i"
    else
      if ( any(INT_FMTS == fmt) ) then
        fmt_ = fmt
      else
        error stop 'Invalid fmt "'//fmt//'": must be one of the INT_FMTS.'
      end if
    end if

    if ( fmt_ == "i" ) then
      if ( x < 0_i16 ) then
        if ( x == smallest_i16 ) then
          x_str = "-32768"; return
        end if
        num = int(-x); negative = .true.
      else
        num = int(x); negative = .false.
      end if

      i = len(buffer); extract_digits: do
        next = num/10; buffer(i:i) = achar(num - 10*next + 48); if ( next == 0 ) exit
        num = next; i = i - 1; cycle
      end do extract_digits

      if ( negative ) then
        buffer(i-1:i-1) = "-"; x_str = buffer(i-1:); return
      else
        x_str = buffer(i:); return
      end if
    else
      if ( x < 0_i16 ) then
        num = int((x + 1_i16) + largest_i16); negative = .true.; buffer(1:) = "0x0000"
      else
        num = int(x); negative = .false.
      end if

      i = len(buffer); extract_hex_digits: do
        next = num/16; buffer(i:i) = DIGITS_A(num - 16*next); if ( next == 0 ) exit
        num = next; i = i - 1; cycle
      end do extract_hex_digits

      if ( negative ) then
        ascii_code = iachar(buffer(3:3))
        if ( ascii_code < 50 ) then
          buffer(3:3) = achar(ascii_code + 8)
        else
          buffer(3:3) = achar(ascii_code + 47)
        end if
        x_str = buffer(1:); return
      else
        buffer(i-2:i-1) = "0x"; x_str = buffer(i-2:); return
      end if
    end if
  end procedure str_from_i16
  module procedure str_from_i8
    character(len=1) :: fmt_
    character(len=4) :: buffer
    integer          :: num, next, ascii_code, i
    logical          :: negative

    if ( .not. present(fmt) ) then
      fmt_ = "i"
    else
      if ( any(INT_FMTS == fmt) ) then
        fmt_ = fmt
      else
        error stop 'Invalid fmt "'//fmt//'": must be one of the INT_FMTS.'
      end if
    end if

    if ( fmt_ == "i" ) then
      if ( x < 0_i8 ) then
        if ( x == smallest_i8 ) then
          x_str = "-128"; return
        end if
        num = int(-x); negative = .true.
      else
        num = int(x); negative = .false.
      end if

      i = len(buffer); extract_digits: do
        next = num/10; buffer(i:i) = achar(num - 10*next + 48); if ( next == 0 ) exit
        num = next; i = i - 1; cycle
      end do extract_digits

      if ( negative ) then
        buffer(i-1:i-1) = "-"; x_str = buffer(i-1:); return
      else
        x_str = buffer(i:); return
      end if
    else
      if ( x < 0_i8 ) then
        num = int((x + 1_i8) + largest_i8); negative = .true.; buffer(1:) = "0x00"
      else
        num = int(x); negative = .false.
      end if

      i = len(buffer); extract_hex_digits: do
        next = num/16; buffer(i:i) = DIGITS_A(num - 16*next); if ( next == 0 ) exit
        num = next; i = i - 1; cycle
      end do extract_hex_digits

      if ( negative ) then
        ascii_code = iachar(buffer(3:3))
        if ( ascii_code < 50 ) then
          buffer(3:3) = achar(ascii_code + 8)
        else
          buffer(3:3) = achar(ascii_code + 47)
        end if
        x_str = buffer(1:); return
      else
        buffer(i-2:i-1) = "0x"; x_str = buffer(i-2:); return
      end if
    end if
  end procedure str_from_i8

  module procedure str_from_string
    if ( x%len() < 1 ) then
      x_str = EMPTY_STR
    else
      x_str = x%s
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
    into%s = complex_text(x, decimal_of(locale), real_fmt_of(fmt), decimals, im)
  end procedure cast_c128_to_string
#endif
  module procedure cast_c64_to_string
    into%s = complex_text(x, decimal_of(locale), real_fmt_of(fmt), decimals, im)
  end procedure cast_c64_to_string
  module procedure cast_c32_to_string
    into%s = complex_text(x, decimal_of(locale), real_fmt_of(fmt), decimals, im)
  end procedure cast_c32_to_string

#ifdef IOFL_HAS_R128
  module procedure cast_r128_to_string
    into%s = real_text(x, decimal_of(locale), real_fmt_of(fmt), decimals)
  end procedure cast_r128_to_string
#endif
  module procedure cast_r64_to_string
    into%s = real_text(x, decimal_of(locale), real_fmt_of(fmt), decimals)
  end procedure cast_r64_to_string
  module procedure cast_r32_to_string
    into%s = real_text(x, decimal_of(locale), real_fmt_of(fmt), decimals)
  end procedure cast_r32_to_string

  module procedure cast_i64_to_string
    character(len=1)  :: fmt_
    character(len=20) :: buffer
    integer(i64)      :: num, next
    integer           :: ascii_code, i
    logical           :: negative

    num=0_i64; next=0_i64; ascii_code=0; i=0; negative=.false.

    if ( .not. present(fmt) ) then
      fmt_ = "i"
    else
      if ( any(INT_FMTS == fmt) ) then
        fmt_ = fmt
      else
        error stop 'Invalid fmt "'//fmt//'": must be one of the INT_FMTS.'
      end if
    end if

    if ( fmt_ == "i" ) then
      if ( x < 0_i64 ) then
        if ( x == smallest_i64 ) then
          into%s = "-9223372036854775808"; return
        end if
        num = -x; negative = .true.
      else
        num = x; negative = .false.
      end if

      i = len(buffer); extract_digits: do
        next = num/10_i64; buffer(i:i) = achar(num - 10_i64*next + 48_i64); if ( next == 0_i64 ) exit
        num = next; i = i - 1; cycle
      end do extract_digits

      if ( negative ) then
        buffer(i-1:i-1) = "-"; into%s = buffer(i-1:); return
      else
        into%s = buffer(i:); return
      end if
    else
      if ( x < 0_i64 ) then
        num = (x + 1_i64) + largest_i64; negative = .true.; buffer(3:) = "0x0000000000000000"
      else
        num = x; negative = .false.
      end if

      i = len(buffer); extract_hex_digits: do
        next = num/16_i64; buffer(i:i) = DIGITS_A(num - 16_i64*next); if ( next == 0_i64 ) exit
        num = next; i = i - 1; cycle
      end do extract_hex_digits

      if ( negative ) then
        ascii_code = iachar(buffer(5:5))
        if ( ascii_code < 50 ) then
          buffer(5:5) = achar(ascii_code + 8)
        else
          buffer(5:5) = achar(ascii_code + 47)
        end if
        into%s = buffer(3:); return
      else
        buffer(i-2:i-1) = "0x"; into%s = buffer(i-2:); return
      end if
    end if
  end procedure cast_i64_to_string
  module procedure cast_i32_to_string
    character(len=1)  :: fmt_
    character(len=11) :: buffer
    integer           :: num, next, ascii_code, i
    logical           :: negative

    num=0; next=0; ascii_code=0; i=0; negative=.false.

    if ( .not. present(fmt) ) then
      fmt_ = "i"
    else
      if ( any(INT_FMTS == fmt) ) then
        fmt_ = fmt
      else
        error stop 'Invalid fmt "'//fmt//'": must be one of the INT_FMTS.'
      end if
    end if

    if ( fmt_ == "i" ) then
      if ( x < 0_i32 ) then
        if ( x == smallest_i32 ) then
          into%s = "-2147483648"; return
        end if
        num = -x; negative = .true.
      else
        num = x; negative = .false.
      end if

      i = len(buffer); extract_digits: do
        next = num/10; buffer(i:i) = achar(num - 10*next + 48); if ( next == 0 ) exit
        num = next; i = i - 1; cycle
      end do extract_digits

      if ( negative ) then
        buffer(i-1:i-1) = "-"; into%s = buffer(i-1:); return
      else
        into%s = buffer(i:); return
      end if
    else
      if ( x < 0_i32 ) then
        num = (x + 1) + largest_i32; negative = .true.; buffer(2:) = "0x00000000"
      else
        num = x; negative = .false.
      end if

      i = len(buffer); extract_hex_digits: do
        next = num/16; buffer(i:i) = DIGITS_A(num - 16*next); if ( next == 0 ) exit
        num = next; i = i - 1; cycle
      end do extract_hex_digits

      if ( negative ) then
        ascii_code = iachar(buffer(4:4))
        if ( ascii_code < 50 ) then
          buffer(4:4) = achar(ascii_code + 8)
        else
          buffer(4:4) = achar(ascii_code + 47)
        end if
        into%s = buffer(2:); return
      else
        buffer(i-2:i-1) = "0x"; into%s = buffer(i-2:); return
      end if
    end if
  end procedure cast_i32_to_string
  module procedure cast_i16_to_string
    character(len=1) :: fmt_
    character(len=6) :: buffer
    integer          :: num, next, ascii_code, i
    logical          :: negative

    if ( .not. present(fmt) ) then
      fmt_ = "i"
    else
      if ( any(INT_FMTS == fmt) ) then
        fmt_ = fmt
      else
        error stop 'Invalid fmt "'//fmt//'": must be one of the INT_FMTS.'
      end if
    end if

    if ( fmt_ == "i" ) then
      if ( x < 0_i16 ) then
        if ( x == smallest_i16 ) then
          into%s = "-32768"; return
        end if
        num = int(-x); negative = .true.
      else
        num = int(x); negative = .false.
      end if

      i = len(buffer); extract_digits: do
        next = num/10; buffer(i:i) = achar(num - 10*next + 48); if ( next == 0 ) exit
        num = next; i = i - 1; cycle
      end do extract_digits

      if ( negative ) then
        buffer(i-1:i-1) = "-"; into%s = buffer(i-1:); return
      else
        into%s = buffer(i:); return
      end if
    else
      if ( x < 0_i16 ) then
        num = int((x + 1_i16) + largest_i16); negative = .true.; buffer(1:) = "0x0000"
      else
        num = int(x); negative = .false.
      end if

      i = len(buffer); extract_hex_digits: do
        next = num/16; buffer(i:i) = DIGITS_A(num - 16*next); if ( next == 0 ) exit
        num = next; i = i - 1; cycle
      end do extract_hex_digits

      if ( negative ) then
        ascii_code = iachar(buffer(3:3))
        if ( ascii_code < 50 ) then
          buffer(3:3) = achar(ascii_code + 8)
        else
          buffer(3:3) = achar(ascii_code + 47)
        end if
        into%s = buffer(1:); return
      else
        buffer(i-2:i-1) = "0x"; into%s = buffer(i-2:); return
      end if
    end if
  end procedure cast_i16_to_string
  module procedure cast_i8_to_string
    character(len=1) :: fmt_
    character(len=4) :: buffer
    integer          :: num, next, ascii_code, i
    logical          :: negative

    if ( .not. present(fmt) ) then
      fmt_ = "i"
    else
      if ( any(INT_FMTS == fmt) ) then
        fmt_ = fmt
      else
        error stop 'Invalid fmt "'//fmt//'": must be one of the INT_FMTS.'
      end if
    end if

    if ( fmt_ == "i" ) then
      if ( x < 0_i8 ) then
        if ( x == smallest_i8 ) then
          into%s = "-128"; return
        end if
        num = int(-x); negative = .true.
      else
        num = int(x); negative = .false.
      end if

      i = len(buffer); extract_digits: do
        next = num/10; buffer(i:i) = achar(num - 10*next + 48); if ( next == 0 ) exit
        num = next; i = i - 1; cycle
      end do extract_digits

      if ( negative ) then
        buffer(i-1:i-1) = "-"; into%s = buffer(i-1:); return
      else
        into%s = buffer(i:); return
      end if
    else
      if ( x < 0_i8 ) then
        num = int((x + 1_i8) + largest_i8); negative = .true.; buffer(1:) = "0x00"
      else
        num = int(x); negative = .false.
      end if

      i = len(buffer); extract_hex_digits: do
        next = num/16; buffer(i:i) = DIGITS_A(num - 16*next); if ( next == 0 ) exit
        num = next; i = i - 1; cycle
      end do extract_hex_digits

      if ( negative ) then
        ascii_code = iachar(buffer(3:3))
        if ( ascii_code < 50 ) then
          buffer(3:3) = achar(ascii_code + 8)
        else
          buffer(3:3) = achar(ascii_code + 47)
        end if
        into%s = buffer(1:); return
      else
        buffer(i-2:i-1) = "0x"; into%s = buffer(i-2:); return
      end if
    end if
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
    character(len=1)  :: fmt_
    character(len=20) :: buffer
    integer(i64)      :: num, next
    integer           :: ascii_code, i
    logical           :: negative

    num=0_i64; next=0_i64; ascii_code=0; i=0; negative=.false.

    if ( .not. present(fmt) ) then
      fmt_ = "i"
    else
      if ( any(INT_FMTS == fmt) ) then
        fmt_ = fmt
      else
        error stop 'Invalid fmt "'//fmt//'": must be one of the INT_FMTS.'
      end if
    end if

    if ( fmt_ == "i" ) then
      if ( x < 0_i64 ) then
        if ( x == smallest_i64 ) then
          into = "-9223372036854775808"; return
        end if
        num = -x; negative = .true.
      else
        num = x; negative = .false.
      end if

      i = len(buffer); extract_digits: do
        next = num/10_i64; buffer(i:i) = achar(num - 10_i64*next + 48_i64); if ( next == 0_i64 ) exit
        num = next; i = i - 1; cycle
      end do extract_digits

      if ( negative ) then
        buffer(i-1:i-1) = "-"; into = buffer(i-1:); return
      else
        into = buffer(i:); return
      end if
    else
      if ( x < 0_i64 ) then
        num = (x + 1_i64) + largest_i64; negative = .true.; buffer(3:) = "0x0000000000000000"
      else
        num = x; negative = .false.
      end if

      i = len(buffer); extract_hex_digits: do
        next = num/16_i64; buffer(i:i) = DIGITS_A(num - 16_i64*next); if ( next == 0_i64 ) exit
        num = next; i = i - 1; cycle
      end do extract_hex_digits

      if ( negative ) then
        ascii_code = iachar(buffer(5:5))
        if ( ascii_code < 50 ) then
          buffer(5:5) = achar(ascii_code + 8)
        else
          buffer(5:5) = achar(ascii_code + 47)
        end if
        into = buffer(3:); return
      else
        buffer(i-2:i-1) = "0x"; into = buffer(i-2:); return
      end if
    end if
  end procedure cast_i64_to_char
  module procedure cast_i32_to_char
    character(len=1)  :: fmt_
    character(len=11) :: buffer
    integer           :: num, next, ascii_code, i
    logical           :: negative

    num=0; next=0; ascii_code=0; i=0; negative=.false.

    if ( .not. present(fmt) ) then
      fmt_ = "i"
    else
      if ( any(INT_FMTS == fmt) ) then
        fmt_ = fmt
      else
        error stop 'Invalid fmt "'//fmt//'": must be one of the INT_FMTS.'
      end if
    end if

    if ( fmt_ == "i" ) then
      if ( x < 0_i32 ) then
        if ( x == smallest_i32 ) then
          into = "-2147483648"; return
        end if
        num = -x; negative = .true.
      else
        num = x; negative = .false.
      end if

      i = len(buffer); extract_digits: do
        next = num/10; buffer(i:i) = achar(num - 10*next + 48); if ( next == 0 ) exit
        num = next; i = i - 1; cycle
      end do extract_digits

      if ( negative ) then
        buffer(i-1:i-1) = "-"; into = buffer(i-1:); return
      else
        into = buffer(i:); return
      end if
    else
      if ( x < 0_i32 ) then
        num = (x + 1) + largest_i32; negative = .true.; buffer(2:) = "0x00000000"
      else
        num = x; negative = .false.
      end if

      i = len(buffer); extract_hex_digits: do
        next = num/16; buffer(i:i) = DIGITS_A(num - 16*next); if ( next == 0 ) exit
        num = next; i = i - 1; cycle
      end do extract_hex_digits

      if ( negative ) then
        ascii_code = iachar(buffer(4:4))
        if ( ascii_code < 50 ) then
          buffer(4:4) = achar(ascii_code + 8)
        else
          buffer(4:4) = achar(ascii_code + 47)
        end if
        into = buffer(2:); return
      else
        buffer(i-2:i-1) = "0x"; into = buffer(i-2:); return
      end if
    end if
  end procedure cast_i32_to_char
  module procedure cast_i16_to_char
    character(len=1) :: fmt_
    character(len=6) :: buffer
    integer          :: num, next, ascii_code, i
    logical          :: negative

    num=0; next=0; ascii_code=0; i=0; negative=.false.

    if ( .not. present(fmt) ) then
      fmt_ = "i"
    else
      if ( any(INT_FMTS == fmt) ) then
        fmt_ = fmt
      else
        error stop 'Invalid fmt "'//fmt//'": must be one of the INT_FMTS.'
      end if
    end if

    if ( fmt_ == "i" ) then
      if ( x < 0_i16 ) then
        if ( x == smallest_i16 ) then
          into = "-32768"; return
        end if
        num = int(-x); negative = .true.
      else
        num = int(x); negative = .false.
      end if

      i = len(buffer); extract_digits: do
        next = num/10; buffer(i:i) = achar(num - 10*next + 48); if ( next == 0 ) exit
        num = next; i = i - 1; cycle
      end do extract_digits

      if ( negative ) then
        buffer(i-1:i-1) = "-"; into = buffer(i-1:); return
      else
        into = buffer(i:); return
      end if
    else
      if ( x < 0_i16 ) then
        num = int((x + 1_i16) + largest_i16); negative = .true.; buffer(1:) = "0x0000"
      else
        num = int(x); negative = .false.
      end if

      i = len(buffer); extract_hex_digits: do
        next = num/16; buffer(i:i) = DIGITS_A(num - 16*next); if ( next == 0 ) exit
        num = next; i = i - 1; cycle
      end do extract_hex_digits

      if ( negative ) then
        ascii_code = iachar(buffer(3:3))
        if ( ascii_code < 50 ) then
          buffer(3:3) = achar(ascii_code + 8)
        else
          buffer(3:3) = achar(ascii_code + 47)
        end if
        into = buffer(1:); return
      else
        buffer(i-2:i-1) = "0x"; into = buffer(i-2:); return
      end if
    end if
  end procedure cast_i16_to_char
  module procedure cast_i8_to_char
    character(len=1) :: fmt_
    character(len=4) :: buffer
    integer          :: num, next, ascii_code, i
    logical          :: negative

    num=0; next=0; ascii_code=0; i=0; negative=.false.

    if ( .not. present(fmt) ) then
      fmt_ = "i"
    else
      if ( any(INT_FMTS == fmt) ) then
        fmt_ = fmt
      else
        error stop 'Invalid fmt "'//fmt//'": must be one of the INT_FMTS.'
      end if
    end if

    if ( fmt_ == "i" ) then
      if ( x < 0_i8 ) then
        if ( x == smallest_i8 ) then
          into = "-128"; return
        end if
        num = int(-x); negative = .true.
      else
        num = int(x); negative = .false.
      end if

      i = len(buffer); extract_digits: do
        next = num/10; buffer(i:i) = achar(num - 10*next + 48); if ( next == 0 ) exit
        num = next; i = i - 1; cycle
      end do extract_digits

      if ( negative ) then
        buffer(i-1:i-1) = "-"; into = buffer(i-1:); return
      else
        into = buffer(i:); return
      end if
    else
      if ( x < 0_i8 ) then
        num = int((x + 1_i8) + largest_i8); negative = .true.; buffer(1:) = "0x00"
      else
        num = int(x); negative = .false.
      end if

      i = len(buffer); extract_hex_digits: do
        next = num/16; buffer(i:i) = DIGITS_A(num - 16*next); if ( next == 0 ) exit
        num = next; i = i - 1; cycle
      end do extract_hex_digits

      if ( negative ) then
        ascii_code = iachar(buffer(3:3))
        if ( ascii_code < 50 ) then
          buffer(3:3) = achar(ascii_code + 8)
        else
          buffer(3:3) = achar(ascii_code + 47)
        end if
        into = buffer(1:); return
      else
        buffer(i-2:i-1) = "0x"; into = buffer(i-2:); return
      end if
    end if
  end procedure cast_i8_to_char

#ifdef IOFL_HAS_R128
  module procedure cast_string_to_c128
    character(len=1)              :: fmt_
    character(len=5)              :: decimal
    character(len=:), allocatable :: im_

    real(r128) :: z_re, z_im
    integer    :: substring_len, l, r, i, sep_code, e_code, im_len

    z_re=0e0_r128; z_im=0e0_r128; substring_len=0; l=0; r=0; i=0; sep_code=0; e_code=0; im_len=0

    substring_len = substring%len()

    if ( substring_len < 1 ) then
      into = (0e0_r128,0e0_r128); return
    end if

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into = (0e0_r128,0e0_r128); return
      end if
    end if

    if ( .not. present(im) ) then
      im_ = EMPTY_STR
    else
      im_ = im
    end if

    if ( .not. present(locale) ) then
      decimal = "POINT"
    else
      if ( locale == "US" ) then
        decimal = "POINT"
      else if ( locale == "EU" ) then
        decimal = "COMMA"
      else
        into = (0e0_r128,0e0_r128); return
      end if
    end if

    if ( len(im_) == 0 ) then
      if ( decimal == "POINT" ) then
        sep_code = iachar(COMMA)
      else
        sep_code = iachar(SEMICOLON)
      end if

      l = 1; do
        if ( iachar(substring%s(l:l)) == 40 ) exit
        l = l + 1; cycle
      end do

      r = substring_len; do
        if ( iachar(substring%s(r:r)) == 41 ) exit
        r = r - 1; cycle
      end do

      i = l+1; do
        if ( iachar(substring%s(i:i)) == sep_code ) exit
        i = i + 1; cycle
      end do

      if ( fmt_ == "z" ) then
        if ( i-l-1 > 2 ) then
          if ( substring%s(l+1:l+2) == "0x" ) then
            read(unit=substring%s(l+3:i-1), fmt="(z100)") z_re
          else
            read(unit=substring%s(l+1:i-1), fmt="(z100)") z_re
          end if
        else
          read(unit=substring%s(l+1:i-1), fmt="(z100)") z_re
        end if

        if ( r-i-1 > 2 ) then
          if ( substring%s(i+1:i+2) == "0x" ) then
            read(unit=substring%s(i+3:r-1), fmt="(z100)") z_im
          else
            read(unit=substring%s(i+1:r-1), fmt="(z100)") z_im
          end if
        else
          read(unit=substring%s(i+1:r-1), fmt="(z100)") z_im
        end if

        into = cmplx(z_re, z_im, kind=r128); return
      else
        read(unit=substring%s(l+1:i-1), fmt=*, decimal=decimal) z_re
        read(unit=substring%s(i+1:r-1), fmt=*, decimal=decimal) z_im
        into = cmplx(z_re, z_im, kind=r128); return
      end if
    end if

    sep_code = iachar("+"); e_code = iachar("e"); im_len = len(im_)

    l = 1; do
      if ( iachar(substring%s(l:l)) > sep_code ) exit
      l = l + 1; cycle
    end do

    r = substring_len-im_len+1; do
      if ( substring%s(r:r+im_len-1) == im_ ) exit
      r = r - 1; cycle
    end do

    if ( fmt_ == "z" ) then
      i = l+1; do
        if ( iachar(substring%s(i:i)) == sep_code ) exit
        i = i + 1; cycle
      end do
    else
      i = l+1; do
        if ( (iachar(substring%s(i:i)) == sep_code) .or. (iachar(substring%s(i:i)) == sep_code+2) ) then
          if ( (iachar(substring%s(i-1:i-1)) == e_code).or.(iachar(substring%s(i-1:i-1)) == e_code-32) ) then
            i = i + 1; cycle
          else
            exit
          end if
        end if
        i = i + 1; cycle
      end do
    end if

    if ( fmt_ == "z" ) then
      if ( i-l > 2 ) then
        if ( substring%s(l:l+1) == "0x" ) then
          read(unit=substring%s(l+2:i-1), fmt="(z100)") z_re
        else
          read(unit=substring%s(l:i-1), fmt="(z100)") z_re
        end if
      else
        read(unit=substring%s(l:i-1), fmt="(z100)") z_re
      end if

      if ( r-i-1 > 2 ) then
        if ( substring%s(i+1:i+2) == "0x" ) then
          read(unit=substring%s(i+3:r-1), fmt="(z100)") z_im
        else
          read(unit=substring%s(i+1:r-1), fmt="(z100)") z_im
        end if
      else
        read(unit=substring%s(i+1:r-1), fmt="(z100)") z_im
      end if

      into = cmplx(z_re, z_im, kind=r128); return
    else
      read(unit=substring%s(l:i-1), fmt=*, decimal=decimal) z_re
      read(unit=substring%s(i:r-1), fmt=*, decimal=decimal) z_im
      into = cmplx(z_re, z_im, kind=r128); return
    end if
  end procedure cast_string_to_c128
#endif
  module procedure cast_string_to_c64
    character(len=1)              :: fmt_
    character(len=5)              :: decimal
    character(len=:), allocatable :: im_

    real(r64) :: z_re, z_im
    integer :: substring_len, l, r, i, sep_code, e_code, im_len

    z_re=0e0_r64; z_im=0e0_r64; substring_len=0; l=0; r=0; i=0; sep_code=0; e_code=0; im_len=0

    substring_len = substring%len()

    if ( substring_len < 1 ) then
      into = (0e0_r64,0e0_r64); return
    end if

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into = (0e0_r64,0e0_r64); return
      end if
    end if

    if ( .not. present(im) ) then
      im_ = EMPTY_STR
    else
      im_ = im
    end if

    if ( .not. present(locale) ) then
      decimal = "POINT"
    else
      if ( locale == "US" ) then
        decimal = "POINT"
      else if ( locale == "EU" ) then
        decimal = "COMMA"
      else
        into = (0e0_r64,0e0_r64); return
      end if
    end if

    if ( len(im_) == 0 ) then
      if ( decimal == "POINT" ) then
        sep_code = iachar(COMMA)
      else
        sep_code = iachar(SEMICOLON)
      end if

      l = 1; do
        if ( iachar(substring%s(l:l)) == 40 ) exit
        l = l + 1; cycle
      end do

      r = substring_len; do
        if ( iachar(substring%s(r:r)) == 41 ) exit
        r = r - 1; cycle
      end do

      i = l+1; do
        if ( iachar(substring%s(i:i)) == sep_code ) exit
        i = i + 1; cycle
      end do

      if ( fmt_ == "z" ) then
        block; integer(i64) :: num; character(len=i-l-1) :: hex_str_re; character(len=r-i-1) :: hex_str_im
          hex_str_re = substring%s(l+1:i-1); hex_str_im = substring%s(i+1:r-1)
          call cast(hex_str_re, into=num, fmt="z"); z_re = transfer(source=num, mold=z_re)
          call cast(hex_str_im, into=num, fmt="z"); z_im = transfer(source=num, mold=z_im)
          into = cmplx(z_re, z_im, kind=r64); return
        end block
      else
        read(unit=substring%s(l+1:i-1), fmt=*, decimal=decimal) z_re
        read(unit=substring%s(i+1:r-1), fmt=*, decimal=decimal) z_im
        into = cmplx(z_re, z_im, kind=r64); return
      end if
    end if

    sep_code = iachar("+"); e_code = iachar("e"); im_len = len(im_)

    l = 1; do
      if ( iachar(substring%s(l:l)) > sep_code ) exit
      l = l + 1; cycle
    end do

    r = substring_len-im_len+1; do
      if ( substring%s(r:r+im_len-1) == im_ ) exit
      r = r - 1; cycle
    end do

    if ( fmt_ == "z" ) then
      i = l+1; do
        if ( iachar(substring%s(i:i)) == sep_code ) exit
        i = i + 1; cycle
      end do
    else
      i = l+1; do
        if ( (iachar(substring%s(i:i)) == sep_code) .or. (iachar(substring%s(i:i)) == sep_code+2) ) then
          if ( (iachar(substring%s(i-1:i-1)) == e_code).or.(iachar(substring%s(i-1:i-1)) == e_code-32) ) then
            i = i + 1; cycle
          else
            exit
          end if
        end if
        i = i + 1; cycle
      end do
    end if

    if ( fmt_ == "z" ) then
      block; integer(i64) :: num; character(len=i-l) :: hex_str_re; character(len=r-i-1) :: hex_str_im
        hex_str_re = substring%s(l:i-1); hex_str_im = substring%s(i+1:r-1)
        call cast(hex_str_re, into=num, fmt="z"); z_re = transfer(source=num, mold=z_re)
        call cast(hex_str_im, into=num, fmt="z"); z_im = transfer(source=num, mold=z_im)
        into = cmplx(z_re, z_im, kind=r64); return
      end block
    else
      read(unit=substring%s(l:i-1), fmt=*, decimal=decimal) z_re
      read(unit=substring%s(i:r-1), fmt=*, decimal=decimal) z_im
      into = cmplx(z_re, z_im, kind=r64); return
    end if
  end procedure cast_string_to_c64
  module procedure cast_string_to_c32
    character(len=1)              :: fmt_
    character(len=5)              :: decimal
    character(len=:), allocatable :: im_

    real(r32) :: z_re, z_im
    integer :: substring_len, l, r, i, sep_code, e_code, im_len

    z_re=0e0_r32; z_im=0e0_r32; substring_len=0; l=0; r=0; i=0; sep_code=0; e_code=0; im_len=0

    substring_len = substring%len()

    if ( substring_len < 1 ) then
      into = (0e0_r32,0e0_r32); return
    end if

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into = (0e0_r32,0e0_r32); return
      end if
    end if

    if ( .not. present(im) ) then
      im_ = EMPTY_STR
    else
      im_ = im
    end if

    if ( .not. present(locale) ) then
      decimal = "POINT"
    else
      if ( locale == "US" ) then
        decimal = "POINT"
      else if ( locale == "EU" ) then
        decimal = "COMMA"
      else
        into = (0e0_r32,0e0_r32); return
      end if
    end if

    if ( len(im_) == 0 ) then
      if ( decimal == "POINT" ) then
        sep_code = iachar(COMMA)
      else
        sep_code = iachar(SEMICOLON)
      end if

      l = 1; do
        if ( iachar(substring%s(l:l)) == 40 ) exit
        l = l + 1; cycle
      end do

      r = substring_len; do
        if ( iachar(substring%s(r:r)) == 41 ) exit
        r = r - 1; cycle
      end do

      i = l+1; do
        if ( iachar(substring%s(i:i)) == sep_code ) exit
        i = i + 1; cycle
      end do

      if ( fmt_ == "z" ) then
        block; integer :: num; character(len=i-l-1) :: hex_str_re; character(len=r-i-1) :: hex_str_im
          hex_str_re = substring%s(l+1:i-1); hex_str_im = substring%s(i+1:r-1)
          call cast(hex_str_re, into=num, fmt="z"); z_re = transfer(source=num, mold=z_re)
          call cast(hex_str_im, into=num, fmt="z"); z_im = transfer(source=num, mold=z_im)
          into = cmplx(z_re, z_im, kind=r32); return
        end block
      else
        read(unit=substring%s(l+1:i-1), fmt=*, decimal=decimal) z_re
        read(unit=substring%s(i+1:r-1), fmt=*, decimal=decimal) z_im
        into = cmplx(z_re, z_im, kind=r32); return
      end if
    end if

    sep_code = iachar("+"); e_code = iachar("e"); im_len = len(im_)

    l = 1; do
      if ( iachar(substring%s(l:l)) > sep_code ) exit
      l = l + 1; cycle
    end do

    r = substring_len-im_len+1; do
      if ( substring%s(r:r+im_len-1) == im_ ) exit
      r = r - 1; cycle
    end do

    if ( fmt_ == "z" ) then
      i = l+1; do
        if ( iachar(substring%s(i:i)) == sep_code ) exit
        i = i + 1; cycle
      end do
    else
      i = l+1; do
        if ( (iachar(substring%s(i:i)) == sep_code) .or. (iachar(substring%s(i:i)) == sep_code+2) ) then
          if ( (iachar(substring%s(i-1:i-1)) == e_code).or.(iachar(substring%s(i-1:i-1)) == e_code-32) ) then
            i = i + 1; cycle
          else
            exit
          end if
        end if
        i = i + 1; cycle
      end do
    end if

    if ( fmt_ == "z" ) then
      block; integer :: num; character(len=i-l) :: hex_str_re; character(len=r-i-1) :: hex_str_im
        hex_str_re = substring%s(l:i-1); hex_str_im = substring%s(i+1:r-1)
        call cast(hex_str_re, into=num, fmt="z"); z_re = transfer(source=num, mold=z_re)
        call cast(hex_str_im, into=num, fmt="z"); z_im = transfer(source=num, mold=z_im)
        into = cmplx(z_re, z_im, kind=r32); return
      end block
    else
      read(unit=substring%s(l:i-1), fmt=*, decimal=decimal) z_re
      read(unit=substring%s(i:r-1), fmt=*, decimal=decimal) z_im
      into = cmplx(z_re, z_im, kind=r32); return
    end if
  end procedure cast_string_to_c32

#ifdef IOFL_HAS_R128
  module procedure cast_string_to_r128
    character(len=1) :: fmt_
    character(len=5) :: decimal

    if ( substring%len() < 1 ) then
      into = 0e0_r128; return
    end if

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into = 0e0_r128; return
      end if
    end if

    if ( fmt_ == "z" ) then
      if ( substring%len() > 2 ) then
        if ( substring%s(1:2) == "0x" ) then
          read(unit=substring%s(3:), fmt="(z100)") into; return
        else
          read(unit=substring%s, fmt="(z100)") into; return
        end if
      else
        read(unit=substring%s, fmt="(z100)") into; return
      end if
    end if

    if ( .not. present(locale) ) then
      decimal = "POINT"
    else
      if ( locale == "US" ) then
        decimal = "POINT"
      else if ( locale == "EU" ) then
        decimal = "COMMA"
      else
        into = 0e0_r128; return
      end if
    end if

    read(unit=substring%s, fmt=*, decimal=decimal) into
  end procedure cast_string_to_r128
#endif
  module procedure cast_string_to_r64
    character(len=1) :: fmt_
    character(len=5) :: decimal

    if ( substring%len() < 1 ) then
      into = 0e0_r64; return
    end if

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into = 0e0_r64; return
      end if
    end if

    if ( fmt_ == "z" ) then
      inline_cast: block; integer(i64) :: num; integer :: substring_len, r, l, i, digit; logical ::negative
        substring_len = substring%len()

        r = substring_len; do
          if ( (iachar(substring%s(r:r)) > 47) .or. (r == 1) ) exit
          r = r - 1; cycle
        end do

        l = 1; do
          if ( (iachar(substring%s(l:l)) > 47) .or. (l == substring_len) ) then
            if ( r-l+1 > 2 ) then
              if ( iachar(substring%s(l+1:l+1)) == 120 ) l = l + 2
            end if
            exit
          end if
          l = l + 1; cycle
        end do

        if ( (r-l+1 == 16) .and. (iachar(substring%s(l:l)) > 55) ) then
          negative = .true.
        else
          negative = .false.
        end if

        num = 0_i64

        do i = 0, ubound(SIXTEENS_i64, dim=1)
          digit = iachar(substring%s(r:r)) - 48

          if ( digit > 16 ) then
            if ( digit < 23 ) then
              digit = digit - 7
            else
              digit = digit - 39
            end if
          end if

          if ( r > l ) then
            num = num + int(digit,i64)*SIXTEENS_i64(i); r = r - 1; cycle
          else
            if ( negative ) then
              digit = digit - 8; num = num + int(digit,i64)*SIXTEENS_i64(i)
              num = (num - 1_i64) - largest_i64
              into = transfer(source=num, mold=into); return
            else
              num = num + int(digit,i64)*SIXTEENS_i64(i)
              into = transfer(source=num, mold=into); return
            end if
          end if
        end do
      end block inline_cast
    end if

    if ( .not. present(locale) ) then
      decimal = "POINT"
    else
      if ( locale == "US" ) then
        decimal = "POINT"
      else if ( locale == "EU" ) then
        decimal = "COMMA"
      else
        into = 0e0_r64; return
      end if
    end if

    read(unit=substring%s, fmt=*, decimal=decimal) into
  end procedure cast_string_to_r64
  module procedure cast_string_to_r32
    character(len=1) :: fmt_
    character(len=5) :: decimal

    if ( substring%len() < 1 ) then
      into = 0e0_r32; return
    end if

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into = 0e0_r32; return
      end if
    end if

    if ( fmt_ == "z" ) then
      inline_cast: block; integer :: num, substring_len, r, l, i, digit; logical :: negative
        substring_len = substring%len()

        r = substring_len; do
          if ( (iachar(substring%s(r:r)) > 47) .or. (r == 1) ) exit
          r = r - 1; cycle
        end do

        l = 1; do
          if ( (iachar(substring%s(l:l)) > 47) .or. (l == substring_len) ) then
            if ( r-l+1 > 2 ) then
              if ( iachar(substring%s(l+1:l+1)) == 120 ) l = l + 2
            end if
            exit
          end if
          l = l + 1; cycle
        end do

        if ( (r-l+1 == 8) .and. (iachar(substring%s(l:l)) > 55) ) then
          negative = .true.
        else
          negative = .false.
        end if

        num = 0

        do i = 0, ubound(SIXTEENS_i32, dim=1)
          digit = iachar(substring%s(r:r)) - 48

          if ( digit > 16 ) then
            if ( digit < 23 ) then
              digit = digit - 7
            else
              digit = digit - 39
            end if
          end if

          if ( r > l ) then
            num = num + digit*SIXTEENS_i32(i); r = r - 1; cycle
          else
            if ( negative ) then
              digit = digit - 8; num = num + digit*SIXTEENS_i32(i)
              num = (num - 1) - largest_i32
              into = transfer(source=num, mold=into); return
            else
              num = num + digit*SIXTEENS_i32(i)
              into = transfer(source=num, mold=into); return
            end if
          end if
        end do
      end block inline_cast
    end if

    if ( .not. present(locale) ) then
      decimal = "POINT"
    else
      if ( locale == "US" ) then
        decimal = "POINT"
      else if ( locale == "EU" ) then
        decimal = "COMMA"
      else
        into = 0e0_r32; return
      end if
    end if

    read(unit=substring%s, fmt=*, decimal=decimal) into
  end procedure cast_string_to_r32

  module procedure cast_string_to_i64
    character(len=1) :: fmt_
    integer          :: substring_len, r, l, i, digit
    logical          :: negative

    substring_len=0; r=0; l=0; i=0; digit=0; negative=.false.

    substring_len = substring%len()

    if ( substring_len < 1 ) then
      into = 0_i64; return
    end if

    if ( .not. present(fmt) ) then
      fmt_ = "i"
    else
      if ( any(INT_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into = 0_i64; return
      end if
    end if

    if ( fmt_ == "i" ) then
      r = substring_len; do
        if ( (iachar(substring%s(r:r)) > 44) .or. (r == 1) ) exit
        r = r - 1; cycle
      end do

      l = 1; do
        if ( (iachar(substring%s(l:l)) > 44) .or. (l == substring_len) ) exit
        l = l + 1; cycle
      end do

      if ( iachar(substring%s(l:l)) == 45 ) then
        negative = .true.; l = l + 1
      else
        negative = .false.
      end if

      into = 0_i64

      do i = 0, ubound(TENS_i64, dim=1)
        into = into + int(iachar(substring%s(r:r)) - 48,i64)*TENS_i64(i); if ( r == l ) exit
        r = r - 1; cycle
      end do

      if ( .not. negative ) return

      into = -into; return
    else
      r = substring_len; do
        if ( (iachar(substring%s(r:r)) > 47) .or. (r == 1) ) exit
        r = r - 1; cycle
      end do

      l = 1; do
        if ( (iachar(substring%s(l:l)) > 47) .or. (l == substring_len) ) then
          if ( r-l+1 > 2 ) then
            if ( iachar(substring%s(l+1:l+1)) == 120 ) l = l + 2
          end if
          exit
        end if
        l = l + 1; cycle
      end do

      if ( (r-l+1 == 16) .and. (iachar(substring%s(l:l)) > 55) ) then
        negative = .true.
      else
        negative = .false.
      end if

      into = 0_i64

      do i = 0, ubound(SIXTEENS_i64, dim=1)
        digit = iachar(substring%s(r:r)) - 48

        if ( digit > 16 ) then
          if ( digit < 23 ) then
            digit = digit - 7
          else
            digit = digit - 39
          end if
        end if

        if ( r > l ) then
          into = into + int(digit,i64)*SIXTEENS_i64(i); r = r - 1; cycle
        else
          if ( negative ) then
            digit = digit - 8; into = into + int(digit,i64)*SIXTEENS_i64(i)
            into = (into - 1_i64) - largest_i64; return
          else
            into = into + int(digit,i64)*SIXTEENS_i64(i); return
          end if
        end if
      end do
    end if
  end procedure cast_string_to_i64
  module procedure cast_string_to_i32
    character(len=1) :: fmt_
    integer          :: substring_len, r, l, i, digit
    logical          :: negative

    substring_len=0; r=0; l=0; i=0; digit=0; negative=.false.

    substring_len = substring%len()

    if ( substring_len < 1 ) then
      into = 0_i32; return
    end if

    if ( .not. present(fmt) ) then
      fmt_ = "i"
    else
      if ( any(INT_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into = 0_i32; return
      end if
    end if

    if ( fmt_ == "i" ) then
      r = substring_len; do
        if ( (iachar(substring%s(r:r)) > 44) .or. (r == 1) ) exit
        r = r - 1; cycle
      end do

      l = 1; do
        if ( (iachar(substring%s(l:l)) > 44) .or. (l == substring_len) ) exit
        l = l + 1; cycle
      end do

      if ( iachar(substring%s(l:l)) == 45 ) then
        negative = .true.; l = l + 1
      else
        negative = .false.
      end if

      into = 0_i32

      do i = 0, ubound(TENS_i32, dim=1)
        into = into + (iachar(substring%s(r:r)) - 48)*TENS_i32(i); if ( r == l ) exit
        r = r - 1; cycle
      end do

      if ( .not. negative ) return

      into = -into; return
    else
      r = substring_len; do
        if ( (iachar(substring%s(r:r)) > 47) .or. (r == 1) ) exit
        r = r - 1; cycle
      end do

      l = 1; do
        if ( (iachar(substring%s(l:l)) > 47) .or. (l == substring_len) ) then
          if ( r-l+1 > 2 ) then
            if ( iachar(substring%s(l+1:l+1)) == 120 ) l = l + 2
          end if
          exit
        end if
        l = l + 1; cycle
      end do

      if ( (r-l+1 == 8) .and. (iachar(substring%s(l:l)) > 55) ) then
        negative = .true.
      else
        negative = .false.
      end if

      into = 0_i32

      do i = 0, ubound(SIXTEENS_i32, dim=1)
        digit = iachar(substring%s(r:r)) - 48

        if ( digit > 16 ) then
          if ( digit < 23 ) then
            digit = digit - 7
          else
            digit = digit - 39
          end if
        end if

        if ( r > l ) then
          into = into + digit*SIXTEENS_i32(i); r = r - 1; cycle
        else
          if ( negative ) then
            digit = digit - 8; into = into + digit*SIXTEENS_i32(i)
            into = (into - 1) - largest_i32; return
          else
            into = into + digit*SIXTEENS_i32(i); return
          end if
        end if
      end do
    end if
  end procedure cast_string_to_i32
  module procedure cast_string_to_i16
    character(len=1) :: fmt_
    integer          :: substring_len, r, l, i, digit
    logical          :: negative

    substring_len=0; r=0; l=0; i=0; digit=0; negative=.false.

    substring_len = substring%len()

    if ( substring_len < 1 ) then
      into = 0_i16; return
    end if

    if ( .not. present(fmt) ) then
      fmt_ = "i"
    else
      if ( any(INT_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into = 0_i16; return
      end if
    end if

    if ( fmt_ == "i" ) then
      r = substring_len; do
        if ( (iachar(substring%s(r:r)) > 44) .or. (r == 1) ) exit
        r = r - 1; cycle
      end do

      l = 1; do
        if ( (iachar(substring%s(l:l)) > 44) .or. (l == substring_len) ) exit
        l = l + 1; cycle
      end do

      if ( iachar(substring%s(l:l)) == 45 ) then
        negative = .true.; l = l + 1
      else
        negative = .false.
      end if

      into = 0_i16

      do i = 0, ubound(TENS_i16, dim=1)
        into = into + int(iachar(substring%s(r:r)) - 48,i16)*TENS_i16(i); if ( r == l ) exit
        r = r - 1; cycle
      end do

      if ( .not. negative ) return

      into = -into; return
    else
      r = substring_len; do
        if ( (iachar(substring%s(r:r)) > 47) .or. (r == 1) ) exit
        r = r - 1; cycle
      end do

      l = 1; do
        if ( (iachar(substring%s(l:l)) > 47) .or. (l == substring_len) ) then
          if ( r-l+1 > 2 ) then
            if ( iachar(substring%s(l+1:l+1)) == 120 ) l = l + 2
          end if
          exit
        end if
        l = l + 1; cycle
      end do

      if ( (r-l+1 == 4) .and. (iachar(substring%s(l:l)) > 55) ) then
        negative = .true.
      else
        negative = .false.
      end if

      into = 0_i16

      do i = 0, ubound(SIXTEENS_i16, dim=1)
        digit = iachar(substring%s(r:r)) - 48

        if ( digit > 16 ) then
          if ( digit < 23 ) then
            digit = digit - 7
          else
            digit = digit - 39
          end if
        end if

        if ( r > l ) then
          into = into + int(digit,i16)*SIXTEENS_i16(i); r = r - 1; cycle
        else
          if ( negative ) then
            digit = digit - 8; into = into + int(digit,i16)*SIXTEENS_i16(i)
            into = (into - 1_i16) - largest_i16; return
          else
            into = into + int(digit,i16)*SIXTEENS_i16(i); return
          end if
        end if
      end do
    end if
  end procedure cast_string_to_i16
  module procedure cast_string_to_i8
    character(len=1) :: fmt_
    integer          :: substring_len, r, l, i, digit
    logical          :: negative

    substring_len=0; r=0; l=0; i=0; digit=0; negative=.false.

    substring_len = substring%len()

    if ( substring_len < 1 ) then
      into = 0_i8; return
    end if

    if ( .not. present(fmt) ) then
      fmt_ = "i"
    else
      if ( any(INT_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into = 0_i8; return
      end if
    end if

    if ( fmt_ == "i" ) then
      r = substring_len; do
        if ( (iachar(substring%s(r:r)) > 44) .or. (r == 1) ) exit
        r = r - 1; cycle
      end do

      l = 1; do
        if ( (iachar(substring%s(l:l)) > 44) .or. (l == substring_len) ) exit
        l = l + 1; cycle
      end do

      if ( iachar(substring%s(l:l)) == 45 ) then
        negative = .true.; l = l + 1
      else
        negative = .false.
      end if

      into = 0_i8

      do i = 0, ubound(TENS_i16, dim=1)
        into = into + int(iachar(substring%s(r:r)) - 48,i16)*TENS_i16(i); if ( r == l ) exit
        r = r - 1; cycle
      end do

      if ( .not. negative ) return

      into = -into; return
    else
      r = substring_len; do
        if ( (iachar(substring%s(r:r)) > 47) .or. (r == 1) ) exit
        r = r - 1; cycle
      end do

      l = 1; do
        if ( (iachar(substring%s(l:l)) > 47) .or. (l == substring_len) ) then
          if ( r-l+1 > 2 ) then
            if ( iachar(substring%s(l+1:l+1)) == 120 ) l = l + 2
          end if
          exit
        end if
        l = l + 1; cycle
      end do

      if ( (r-l+1 == 2) .and. (iachar(substring%s(l:l)) > 55) ) then
        negative = .true.
      else
        negative = .false.
      end if

      into = 0_i8

      do i = 0, ubound(SIXTEENS_i8, dim=1)
        digit = iachar(substring%s(r:r)) - 48

        if ( digit > 16 ) then
          if ( digit < 23 ) then
            digit = digit - 7
          else
            digit = digit - 39
          end if
        end if

        if ( r > l ) then
          into = into + int(digit,i8)*SIXTEENS_i8(i); r = r - 1; cycle
        else
          if ( negative ) then
            digit = digit - 8; into = into + int(digit,i8)*SIXTEENS_i8(i)
            into = (into - 1_i8) - largest_i8; return
          else
            into = into + int(digit,i8)*SIXTEENS_i8(i); return
          end if
        end if
      end do
    end if
  end procedure cast_string_to_i8

#ifdef IOFL_HAS_R128
  module procedure cast_char_to_c128
    character(len=1)              :: fmt_
    character(len=5)              :: decimal
    character(len=:), allocatable :: im_

    real(r128) :: z_re, z_im
    integer    :: substring_len, l, r, i, sep_code, e_code, im_len

    z_re=0e0_r128; z_im=0e0_r128; substring_len=0; l=0; r=0; i=0; sep_code=0; e_code=0; im_len=0

    substring_len = len(substring)

    if ( substring_len < 1 ) then
      into = (0e0_r128,0e0_r128); return
    end if

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into = (0e0_r128,0e0_r128); return
      end if
    end if

    if ( .not. present(im) ) then
      im_ = EMPTY_STR
    else
      im_ = im
    end if

    if ( .not. present(locale) ) then
      decimal = "POINT"
    else
      if ( locale == "US" ) then
        decimal = "POINT"
      else if ( locale == "EU" ) then
        decimal = "COMMA"
      else
        into = (0e0_r128,0e0_r128); return
      end if
    end if

    if ( len(im_) == 0 ) then
      if ( decimal == "POINT" ) then
        sep_code = iachar(COMMA)
      else
        sep_code = iachar(SEMICOLON)
      end if

      l = 1; do
        if ( iachar(substring(l:l)) == 40 ) exit
        l = l + 1; cycle
      end do

      r = substring_len; do
        if ( iachar(substring(r:r)) == 41 ) exit
        r = r - 1; cycle
      end do

      i = l+1; do
        if ( iachar(substring(i:i)) == sep_code ) exit
        i = i + 1; cycle
      end do

      if ( fmt_ == "z" ) then
        if ( i-l-1 > 2 ) then
          if ( substring(l+1:l+2) == "0x" ) then
            read(unit=substring(l+3:i-1), fmt="(z100)") z_re
          else
            read(unit=substring(l+1:i-1), fmt="(z100)") z_re
          end if
        else
          read(unit=substring(l+1:i-1), fmt="(z100)") z_re
        end if

        if ( r-i-1 > 2 ) then
          if ( substring(i+1:i+2) == "0x" ) then
            read(unit=substring(i+3:r-1), fmt="(z100)") z_im
          else
            read(unit=substring(i+1:r-1), fmt="(z100)") z_im
          end if
        else
          read(unit=substring(i+1:r-1), fmt="(z100)") z_im
        end if

        into = cmplx(z_re, z_im, kind=r128); return
      else
        read(unit=substring(l+1:i-1), fmt=*, decimal=decimal) z_re
        read(unit=substring(i+1:r-1), fmt=*, decimal=decimal) z_im
        into = cmplx(z_re, z_im, kind=r128); return
      end if
    end if

    sep_code = iachar("+"); e_code = iachar("e"); im_len = len(im_)

    l = 1; do
      if ( iachar(substring(l:l)) > sep_code ) exit
      l = l + 1; cycle
    end do

    r = substring_len-im_len+1; do
      if ( substring(r:r+im_len-1) == im_ ) exit
      r = r - 1; cycle
    end do

    if ( fmt_ == "z" ) then
      i = l+1; do
        if ( iachar(substring(i:i)) == sep_code ) exit
        i = i + 1; cycle
      end do
    else
      i = l+1; do
        if ( (iachar(substring(i:i)) == sep_code) .or. (iachar(substring(i:i)) == sep_code+2) ) then
          if ( (iachar(substring(i-1:i-1)) == e_code).or.(iachar(substring(i-1:i-1)) == e_code-32) ) then
            i = i + 1; cycle
          else
            exit
          end if
        end if
        i = i + 1; cycle
      end do
    end if

    if ( fmt_ == "z" ) then
      if ( i-l > 2 ) then
        if ( substring(l:l+1) == "0x" ) then
          read(unit=substring(l+2:i-1), fmt="(z100)") z_re
        else
          read(unit=substring(l:i-1), fmt="(z100)") z_re
        end if
      else
        read(unit=substring(l:i-1), fmt="(z100)") z_re
      end if

      if ( r-i-1 > 2 ) then
        if ( substring(i+1:i+2) == "0x" ) then
          read(unit=substring(i+3:r-1), fmt="(z100)") z_im
        else
          read(unit=substring(i+1:r-1), fmt="(z100)") z_im
        end if
      else
        read(unit=substring(i+1:r-1), fmt="(z100)") z_im
      end if

      into = cmplx(z_re, z_im, kind=r128); return
    else
      read(unit=substring(l:i-1), fmt=*, decimal=decimal) z_re
      read(unit=substring(i:r-1), fmt=*, decimal=decimal) z_im
      into = cmplx(z_re, z_im, kind=r128); return
    end if
  end procedure cast_char_to_c128
#endif
  module procedure cast_char_to_c64
    character(len=1)              :: fmt_
    character(len=5)              :: decimal
    character(len=:), allocatable :: im_

    real(r64) :: z_re, z_im
    integer   :: substring_len, l, r, i, sep_code, e_code, im_len

    z_re=0e0_r64; z_im=0e0_r64; substring_len=0; l=0; r=0; i=0; sep_code=0; e_code=0; im_len=0

    substring_len = len(substring)

    if ( substring_len < 1 ) then
      into = (0e0_r64,0e0_r64); return
    end if

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into = (0e0_r64,0e0_r64); return
      end if
    end if

    if ( .not. present(im) ) then
      im_ = EMPTY_STR
    else
      im_ = im
    end if

    if ( .not. present(locale) ) then
      decimal = "POINT"
    else
      if ( locale == "US" ) then
        decimal = "POINT"
      else if ( locale == "EU" ) then
        decimal = "COMMA"
      else
        into = (0e0_r64,0e0_r64); return
      end if
    end if

    if ( len(im_) == 0 ) then
      if ( decimal == "POINT" ) then
        sep_code = iachar(COMMA)
      else
        sep_code = iachar(SEMICOLON)
      end if

      l = 1; do
        if ( iachar(substring(l:l)) == 40 ) exit
        l = l + 1; cycle
      end do

      r = substring_len; do
        if ( iachar(substring(r:r)) == 41 ) exit
        r = r - 1; cycle
      end do

      i = l+1; do
        if ( iachar(substring(i:i)) == sep_code ) exit
        i = i + 1; cycle
      end do

      if ( fmt_ == "z" ) then
        block; integer(i64) :: num; character(len=i-l-1) :: hex_str_re; character(len=r-i-1) :: hex_str_im
          hex_str_re = substring(l+1:i-1); hex_str_im = substring(i+1:r-1)
          call cast(hex_str_re, into=num, fmt="z"); z_re = transfer(source=num, mold=z_re)
          call cast(hex_str_im, into=num, fmt="z"); z_im = transfer(source=num, mold=z_im)
          into = cmplx(z_re, z_im, kind=r64); return
        end block
      else
        read(unit=substring(l+1:i-1), fmt=*, decimal=decimal) z_re
        read(unit=substring(i+1:r-1), fmt=*, decimal=decimal) z_im
        into = cmplx(z_re, z_im, kind=r64); return
      end if
    end if

    sep_code = iachar("+"); e_code = iachar("e"); im_len = len(im_)

    l = 1; do
      if ( iachar(substring(l:l)) > sep_code ) exit
      l = l + 1; cycle
    end do

    r = substring_len-im_len+1; do
      if ( substring(r:r+im_len-1) == im_ ) exit
      r = r - 1; cycle
    end do

    if ( fmt_ == "z" ) then
      i = l+1; do
        if ( iachar(substring(i:i)) == sep_code ) exit
        i = i + 1; cycle
      end do
    else
      i = l+1; do
        if ( (iachar(substring(i:i)) == sep_code) .or. (iachar(substring(i:i)) == sep_code+2) ) then
          if ( (iachar(substring(i-1:i-1)) == e_code).or.(iachar(substring(i-1:i-1)) == e_code-32) ) then
            i = i + 1; cycle
          else
            exit
          end if
        end if
        i = i + 1; cycle
      end do
    end if

    if ( fmt_ == "z" ) then
      block; integer(i64) :: num; character(len=i-l) :: hex_str_re; character(len=r-i-1) :: hex_str_im
        hex_str_re = substring(l:i-1); hex_str_im = substring(i+1:r-1)
        call cast(hex_str_re, into=num, fmt="z"); z_re = transfer(source=num, mold=z_re)
        call cast(hex_str_im, into=num, fmt="z"); z_im = transfer(source=num, mold=z_im)
        into = cmplx(z_re, z_im, kind=r64); return
      end block
    else
      read(unit=substring(l:i-1), fmt=*, decimal=decimal) z_re
      read(unit=substring(i:r-1), fmt=*, decimal=decimal) z_im
      into = cmplx(z_re, z_im, kind=r64); return
    end if
  end procedure cast_char_to_c64
  module procedure cast_char_to_c32
    character(len=1)              :: fmt_
    character(len=5)              :: decimal
    character(len=:), allocatable :: im_

    real(r32) :: z_re, z_im
    integer   :: substring_len, l, r, i, sep_code, e_code, im_len

    z_re=0e0_r32; z_im=0e0_r32; substring_len=0; l=0; r=0; i=0; sep_code=0; e_code=0; im_len=0

    substring_len = len(substring)

    if ( substring_len < 1 ) then
      into = (0e0_r32,0e0_r32); return
    end if

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into = (0e0_r32,0e0_r32); return
      end if
    end if

    if ( .not. present(im) ) then
      im_ = EMPTY_STR
    else
      im_ = im
    end if

    if ( .not. present(locale) ) then
      decimal = "POINT"
    else
      if ( locale == "US" ) then
        decimal = "POINT"
      else if ( locale == "EU" ) then
        decimal = "COMMA"
      else
        into = (0e0_r32,0e0_r32); return
      end if
    end if

    if ( len(im_) == 0 ) then
      if ( decimal == "POINT" ) then
        sep_code = iachar(COMMA)
      else
        sep_code = iachar(SEMICOLON)
      end if

      l = 1; do
        if ( iachar(substring(l:l)) == 40 ) exit
        l = l + 1; cycle
      end do

      r = substring_len; do
        if ( iachar(substring(r:r)) == 41 ) exit
        r = r - 1; cycle
      end do

      i = l+1; do
        if ( iachar(substring(i:i)) == sep_code ) exit
        i = i + 1; cycle
      end do

      if ( fmt_ == "z" ) then
        block; integer :: num; character(len=i-l-1) :: hex_str_re; character(len=r-i-1) :: hex_str_im
          hex_str_re = substring(l+1:i-1); hex_str_im = substring(i+1:r-1)
          call cast(hex_str_re, into=num, fmt="z"); z_re = transfer(source=num, mold=z_re)
          call cast(hex_str_im, into=num, fmt="z"); z_im = transfer(source=num, mold=z_im)
          into = cmplx(z_re, z_im, kind=r32); return
        end block
      else
        read(unit=substring(l+1:i-1), fmt=*, decimal=decimal) z_re
        read(unit=substring(i+1:r-1), fmt=*, decimal=decimal) z_im
        into = cmplx(z_re, z_im, kind=r32); return
      end if
    end if

    sep_code = iachar("+"); e_code = iachar("e"); im_len = len(im_)

    l = 1; do
      if ( iachar(substring(l:l)) > sep_code ) exit
      l = l + 1; cycle
    end do

    r = substring_len-im_len+1; do
      if ( substring(r:r+im_len-1) == im_ ) exit
      r = r - 1; cycle
    end do

    if ( fmt_ == "z" ) then
      i = l+1; do
        if ( iachar(substring(i:i)) == sep_code ) exit
        i = i + 1; cycle
      end do
    else
      i = l+1; do
        if ( (iachar(substring(i:i)) == sep_code) .or. (iachar(substring(i:i)) == sep_code+2) ) then
          if ( (iachar(substring(i-1:i-1)) == e_code).or.(iachar(substring(i-1:i-1)) == e_code-32) ) then
            i = i + 1; cycle
          else
            exit
          end if
        end if
        i = i + 1; cycle
      end do
    end if

    if ( fmt_ == "z" ) then
      block; integer :: num; character(len=i-l) :: hex_str_re; character(len=r-i-1) :: hex_str_im
        hex_str_re = substring(l:i-1); hex_str_im = substring(i+1:r-1)
        call cast(hex_str_re, into=num, fmt="z"); z_re = transfer(source=num, mold=z_re)
        call cast(hex_str_im, into=num, fmt="z"); z_im = transfer(source=num, mold=z_im)
        into = cmplx(z_re, z_im, kind=r32); return
      end block
    else
      read(unit=substring(l:i-1), fmt=*, decimal=decimal) z_re
      read(unit=substring(i:r-1), fmt=*, decimal=decimal) z_im
      into = cmplx(z_re, z_im, kind=r32); return
    end if
  end procedure cast_char_to_c32

#ifdef IOFL_HAS_R128
  module procedure cast_char_to_r128
    character(len=1) :: fmt_
    character(len=5) :: decimal

    if ( len(substring) < 1 ) then
      into = 0e0_r128; return
    end if

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into = 0e0_r128; return
      end if
    end if

    if ( fmt_ == "z" ) then
      if ( len(substring) > 2 ) then
        if ( substring(1:2) == "0x" ) then
          read(unit=substring(3:), fmt="(z100)") into; return
        else
          read(unit=substring, fmt="(z100)") into; return
        end if
      else
        read(unit=substring, fmt="(z100)") into; return
      end if
    end if

    if ( .not. present(locale) ) then
      decimal = "POINT"
    else
      if ( locale == "US" ) then
        decimal = "POINT"
      else if ( locale == "EU" ) then
        decimal = "COMMA"
      else
        into = 0e0_r128; return
      end if
    end if

    read(unit=substring, fmt=*, decimal=decimal) into
  end procedure cast_char_to_r128
#endif
  module procedure cast_char_to_r64
    character(len=1) :: fmt_
    character(len=5) :: decimal
    integer(i64)     :: num

    if ( len(substring) < 1 ) then
      into = 0e0_r64; return
    end if

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into = 0e0_r64; return
      end if
    end if

    if ( fmt_ == "z" ) then
      call cast(substring, into=num, fmt="z")
      into = transfer(source=num, mold=into)
      return
    end if

    if ( .not. present(locale) ) then
      decimal = "POINT"
    else
      if ( locale == "US" ) then
        decimal = "POINT"
      else if ( locale == "EU" ) then
        decimal = "COMMA"
      else
        into = 0e0_r64; return
      end if
    end if

    read(unit=substring, fmt=*, decimal=decimal) into
  end procedure cast_char_to_r64
  module procedure cast_char_to_r32
    character(len=1) :: fmt_
    character(len=5) :: decimal
    integer(i32)     :: num

    if ( len(substring) < 1 ) then
      into = 0e0_r32; return
    end if

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into = 0e0_r32; return
      end if
    end if

    if ( fmt_ == "z" ) then
      call cast(substring, into=num, fmt="z")
      into = transfer(source=num, mold=into)
      return
    end if

    if ( .not. present(locale) ) then
      decimal = "POINT"
    else
      if ( locale == "US" ) then
        decimal = "POINT"
      else if ( locale == "EU" ) then
        decimal = "COMMA"
      else
        into = 0e0_r32; return
      end if
    end if

    read(unit=substring, fmt=*, decimal=decimal) into
  end procedure cast_char_to_r32

  module procedure cast_char_to_i64
    character(len=1) :: fmt_
    integer          :: substring_len, r, l, i, digit
    logical          :: negative

    substring_len=0; r=0; l=0; i=0; digit=0; negative=.false.

    substring_len = len(substring)

    if ( substring_len < 1 ) then
      into = 0_i64; return
    end if

    if ( .not. present(fmt) ) then
      fmt_ = "i"
    else
      if ( any(INT_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into = 0_i64; return
      end if
    end if

    if ( fmt_ == "i" ) then
      r = substring_len; do
        if ( (iachar(substring(r:r)) > 44) .or. (r == 1) ) exit
        r = r - 1; cycle
      end do

      l = 1; do
        if ( (iachar(substring(l:l)) > 44) .or. (l == substring_len) ) exit
        l = l + 1; cycle
      end do

      if ( iachar(substring(l:l)) == 45 ) then
        negative = .true.; l = l + 1
      else
        negative = .false.
      end if

      into = 0_i64

      do i = 0, ubound(TENS_i64, dim=1)
        into = into + int(iachar(substring(r:r)) - 48,i64)*TENS_i64(i); if ( r == l ) exit
        r = r - 1; cycle
      end do

      if ( .not. negative ) return

      into = -into; return
    else
      r = substring_len; do
        if ( (iachar(substring(r:r)) > 47) .or. (r == 1) ) exit
        r = r - 1; cycle
      end do

      l = 1; do
        if ( (iachar(substring(l:l)) > 47) .or. (l == substring_len) ) then
          if ( r-l+1 > 2 ) then
            if ( iachar(substring(l+1:l+1)) == 120 ) l = l + 2
          end if
          exit
        end if
        l = l + 1; cycle
      end do

      if ( (r-l+1 == 16) .and. (iachar(substring(l:l)) > 55) ) then
        negative = .true.
      else
        negative = .false.
      end if

      into = 0_i64

      do i = 0, ubound(SIXTEENS_i64, dim=1)
        digit = iachar(substring(r:r)) - 48

        if ( digit > 16 ) then
          if ( digit < 23 ) then
            digit = digit - 7
          else
            digit = digit - 39
          end if
        end if

        if ( r > l ) then
          into = into + int(digit,i64)*SIXTEENS_i64(i); r = r - 1; cycle
        else
          if ( negative ) then
            digit = digit - 8; into = into + int(digit,i64)*SIXTEENS_i64(i)
            into = (into - 1_i64) - largest_i64; return
          else
            into = into + int(digit,i64)*SIXTEENS_i64(i); return
          end if
        end if
      end do
    end if
  end procedure cast_char_to_i64
  module procedure cast_char_to_i32
    character(len=1) :: fmt_
    integer          :: substring_len, r, l, i, digit
    logical          :: negative

    substring_len=0; r=0; l=0; i=0; digit=0; negative=.false.

    substring_len = len(substring)

    if ( substring_len < 1 ) then
      into = 0_i32; return
    end if

    if ( .not. present(fmt) ) then
      fmt_ = "i"
    else
      if ( any(INT_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into = 0_i32; return
      end if
    end if

    if ( fmt_ == "i" ) then
      r = substring_len; do
        if ( (iachar(substring(r:r)) > 44) .or. (r == 1) ) exit
        r = r - 1; cycle
      end do

      l = 1; do
        if ( (iachar(substring(l:l)) > 44) .or. (l == substring_len) ) exit
        l = l + 1; cycle
      end do

      if ( iachar(substring(l:l)) == 45 ) then
        negative = .true.; l = l + 1
      else
        negative = .false.
      end if

      into = 0_i32

      do i = 0, ubound(TENS_i32, dim=1)
        into = into + (iachar(substring(r:r)) - 48)*TENS_i32(i); if ( r == l ) exit
        r = r - 1; cycle
      end do

      if ( .not. negative ) return

      into = -into; return
    else
      r = substring_len; do
        if ( (iachar(substring(r:r)) > 47) .or. (r == 1) ) exit
        r = r - 1; cycle
      end do

      l = 1; do
        if ( (iachar(substring(l:l)) > 47) .or. (l == substring_len) ) then
          if ( r-l+1 > 2 ) then
            if ( iachar(substring(l+1:l+1)) == 120 ) l = l + 2
          end if
          exit
        end if
        l = l + 1; cycle
      end do

      if ( (r-l+1 == 8) .and. (iachar(substring(l:l)) > 55) ) then
        negative = .true.
      else
        negative = .false.
      end if

      into = 0_i32

      do i = 0, ubound(SIXTEENS_i32, dim=1)
        digit = iachar(substring(r:r)) - 48

        if ( digit > 16 ) then
          if ( digit < 23 ) then
            digit = digit - 7
          else
            digit = digit - 39
          end if
        end if

        if ( r > l ) then
          into = into + digit*SIXTEENS_i32(i); r = r - 1; cycle
        else
          if ( negative ) then
            digit = digit - 8; into = into + digit*SIXTEENS_i32(i)
            into = (into - 1) - largest_i32; return
          else
            into = into + digit*SIXTEENS_i32(i); return
          end if
        end if
      end do
    end if
  end procedure cast_char_to_i32
  module procedure cast_char_to_i16
    character(len=1) :: fmt_
    integer          :: substring_len, r, l, i, digit
    logical          :: negative

    substring_len=0; r=0; l=0; i=0; digit=0; negative=.false.

    substring_len = len(substring)

    if ( substring_len < 1 ) then
      into = 0_i16; return
    end if

    if ( .not. present(fmt) ) then
      fmt_ = "i"
    else
      if ( any(INT_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into = 0_i16; return
      end if
    end if

    if ( fmt_ == "i" ) then
      r = substring_len; do
        if ( (iachar(substring(r:r)) > 44) .or. (r == 1) ) exit
        r = r - 1; cycle
      end do

      l = 1; do
        if ( (iachar(substring(l:l)) > 44) .or. (l == substring_len) ) exit
        l = l + 1; cycle
      end do

      if ( iachar(substring(l:l)) == 45 ) then
        negative = .true.; l = l + 1
      else
        negative = .false.
      end if

      into = 0_i16

      do i = 0, ubound(TENS_i16, dim=1)
        into = into + int(iachar(substring(r:r)) - 48,i16)*TENS_i16(i); if ( r == l ) exit
        r = r - 1; cycle
      end do

      if ( .not. negative ) return

      into = -into; return
    else
      r = substring_len; do
        if ( (iachar(substring(r:r)) > 47) .or. (r == 1) ) exit
        r = r - 1; cycle
      end do

      l = 1; do
        if ( (iachar(substring(l:l)) > 47) .or. (l == substring_len) ) then
          if ( r-l+1 > 2 ) then
            if ( iachar(substring(l+1:l+1)) == 120 ) l = l + 2
          end if
          exit
        end if
        l = l + 1; cycle
      end do

      if ( (r-l+1 == 4) .and. (iachar(substring(l:l)) > 55) ) then
        negative = .true.
      else
        negative = .false.
      end if

      into = 0_i16

      do i = 0, ubound(SIXTEENS_i16, dim=1)
        digit = iachar(substring(r:r)) - 48

        if ( digit > 16 ) then
          if ( digit < 23 ) then
            digit = digit - 7
          else
            digit = digit - 39
          end if
        end if

        if ( r > l ) then
          into = into + int(digit,i16)*SIXTEENS_i16(i); r = r - 1; cycle
        else
          if ( negative ) then
            digit = digit - 8; into = into + int(digit,i16)*SIXTEENS_i16(i)
            into = (into - 1_i16) - largest_i16; return
          else
            into = into + int(digit,i16)*SIXTEENS_i16(i); return
          end if
        end if
      end do
    end if
  end procedure cast_char_to_i16
  module procedure cast_char_to_i8
    character(len=1) :: fmt_
    integer          :: substring_len, r, l, i, digit
    logical          :: negative

    substring_len=0; r=0; l=0; i=0; digit=0; negative=.false.

    substring_len = len(substring)

    if ( substring_len < 1 ) then
      into = 0_i8; return
    end if

    if ( .not. present(fmt) ) then
      fmt_ = "i"
    else
      if ( any(INT_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into = 0_i8; return
      end if
    end if

    if ( fmt_ == "i" ) then
      r = substring_len; do
        if ( (iachar(substring(r:r)) > 44) .or. (r == 1) ) exit
        r = r - 1; cycle
      end do

      l = 1; do
        if ( (iachar(substring(l:l)) > 44) .or. (l == substring_len) ) exit
        l = l + 1; cycle
      end do

      if ( iachar(substring(l:l)) == 45 ) then
        negative = .true.; l = l + 1
      else
        negative = .false.
      end if

      into = 0_i8

      do i = 0, ubound(TENS_i16, dim=1)
        into = into + int(iachar(substring(r:r)) - 48,i16)*TENS_i16(i); if ( r == l ) exit
        r = r - 1; cycle
      end do

      if ( .not. negative ) return

      into = -into; return
    else
      r = substring_len; do
        if ( (iachar(substring(r:r)) > 47) .or. (r == 1) ) exit
        r = r - 1; cycle
      end do

      l = 1; do
        if ( (iachar(substring(l:l)) > 47) .or. (l == substring_len) ) then
          if ( r-l+1 > 2 ) then
            if ( iachar(substring(l+1:l+1)) == 120 ) l = l + 2
          end if
          exit
        end if
        l = l + 1; cycle
      end do

      if ( (r-l+1 == 2) .and. (iachar(substring(l:l)) > 55) ) then
        negative = .true.
      else
        negative = .false.
      end if

      into = 0_i8

      do i = 0, ubound(SIXTEENS_i8, dim=1)
        digit = iachar(substring(r:r)) - 48

        if ( digit > 16 ) then
          if ( digit < 23 ) then
            digit = digit - 7
          else
            digit = digit - 39
          end if
        end if

        if ( r > l ) then
          into = into + int(digit,i8)*SIXTEENS_i8(i); r = r - 1; cycle
        else
          if ( negative ) then
            digit = digit - 8; into = into + int(digit,i8)*SIXTEENS_i8(i)
            into = (into - 1_i8) - largest_i8; return
          else
            into = into + int(digit,i8)*SIXTEENS_i8(i); return
          end if
        end if
      end do
    end if
  end procedure cast_char_to_i8

  ! Number formatting, shared by `String`, `str` and `cast` ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
    integer, intent(in) :: width, d, e_digits
    character(len=32)   :: edit

    write(unit=edit, fmt="(a,i0,a,i0,a,i0,a)") "(es", width, ".", d, "e", e_digits, ")"
  end function e_edit

  pure function f_edit(width, d) result(edit)
    integer, intent(in) :: width, d
    character(len=32)   :: edit

    write(unit=edit, fmt="(a,i0,a,i0,a)") "(f", width, ".", d, ")"
  end function f_edit

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

    integer, parameter :: E_DECIMALS = 35, E_DIGITS = 4, F_DIGITS = 36
    integer            :: d, e, i, n

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
      text = zero_text(decimal, fmt, E_DIGITS); return
    end if

    if ( fmt == "e" ) then
      d = E_DECIMALS
      if ( present(decimals) ) d = max(min(decimals, E_DECIMALS), 0)

      allocate( character(len=d+E_DIGITS+8) :: text )
      write(unit=text, fmt=e_edit(len(text), d, E_DIGITS), decimal=decimal) x
      text = trim(adjustl(text))

      i = index(text, "E"); text(i:i) = "e"
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

  pure function complex_text_c128(x, decimal, fmt, decimals, im) result(text)
    complex(r128),     intent(in)           :: x
    character(len=*), intent(in)           :: decimal, fmt
    integer,          intent(in), optional :: decimals
    character(len=*), intent(in), optional :: im
    character(len=:), allocatable          :: text

    text = join_complex( real_text_r128(x%re, decimal, fmt, decimals), &
                         real_text_r128(x%im, decimal, fmt, decimals), &
                         x%im < 0e0_r128, decimal, fmt, im )
  end function complex_text_c128
#endif

  pure function real_text_r64(x, decimal, fmt, decimals) result(text)
    real(r64),        intent(in)           :: x
    character(len=*), intent(in)           :: decimal, fmt
    integer,          intent(in), optional :: decimals
    character(len=:), allocatable          :: text

    integer, parameter :: E_DECIMALS = 17, E_DIGITS = 3, F_DIGITS = 18
    integer            :: d, e, i, n

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
      text = zero_text(decimal, fmt, E_DIGITS); return
    end if

    if ( fmt == "e" ) then
      d = E_DECIMALS
      if ( present(decimals) ) d = max(min(decimals, E_DECIMALS), 0)

      allocate( character(len=d+E_DIGITS+8) :: text )
      write(unit=text, fmt=e_edit(len(text), d, E_DIGITS), decimal=decimal) x
      text = trim(adjustl(text))

      i = index(text, "E"); text(i:i) = "e"
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

  pure function complex_text_c64(x, decimal, fmt, decimals, im) result(text)
    complex(r64),     intent(in)           :: x
    character(len=*), intent(in)           :: decimal, fmt
    integer,          intent(in), optional :: decimals
    character(len=*), intent(in), optional :: im
    character(len=:), allocatable          :: text

    text = join_complex( real_text_r64(x%re, decimal, fmt, decimals), &
                         real_text_r64(x%im, decimal, fmt, decimals), &
                         x%im < 0e0_r64, decimal, fmt, im )
  end function complex_text_c64

  pure function real_text_r32(x, decimal, fmt, decimals) result(text)
    real(r32),        intent(in)           :: x
    character(len=*), intent(in)           :: decimal, fmt
    integer,          intent(in), optional :: decimals
    character(len=:), allocatable          :: text

    integer, parameter :: E_DECIMALS = 8, E_DIGITS = 2, F_DIGITS = 9
    integer            :: d, e, i, n

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
      text = zero_text(decimal, fmt, E_DIGITS); return
    end if

    if ( fmt == "e" ) then
      d = E_DECIMALS
      if ( present(decimals) ) d = max(min(decimals, E_DECIMALS), 0)

      allocate( character(len=d+E_DIGITS+8) :: text )
      write(unit=text, fmt=e_edit(len(text), d, E_DIGITS), decimal=decimal) x
      text = trim(adjustl(text))

      i = index(text, "E"); text(i:i) = "e"
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

  pure function complex_text_c32(x, decimal, fmt, decimals, im) result(text)
    complex(r32),     intent(in)           :: x
    character(len=*), intent(in)           :: decimal, fmt
    integer,          intent(in), optional :: decimals
    character(len=*), intent(in), optional :: im
    character(len=:), allocatable          :: text

    text = join_complex( real_text_r32(x%re, decimal, fmt, decimals), &
                         real_text_r32(x%im, decimal, fmt, decimals), &
                         x%im < 0e0_r32, decimal, fmt, im )
  end function complex_text_c32
end submodule internal_io
