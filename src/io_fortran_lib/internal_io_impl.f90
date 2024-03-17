submodule (io_fortran_lib) internal_io
  !---------------------------------------------------------------------------------------------------------------------
  !! This submodule provides module procedure implementations for the **public interfaces** `String`, `str`, and
  !! `cast`.
  !---------------------------------------------------------------------------------------------------------------------
  implicit none (type, external)

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
  module procedure new_string_from_c128
    character(len=1)              :: fmt_
    character(len=5)              :: decimal
    character(len=:), allocatable :: xre_str, xim_str, im_
    integer                       :: e, decimals_, i

    e=0; decimals_=0; i=0

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        new%s = EMPTY_STR; return
      end if
    end if

    if_z_re: if ( fmt_ == "z" ) then
      if ( x%re /= 0e0_r128 ) then
        xre_str = "0x00000000000000000000000000000000"
      else
        xre_str = "0x0"; exit if_z_re
      end if

      write(unit=xre_str(3:), fmt="(z32)") x%re

      do i = 3, 34
        if ( (xre_str(i:i) >= "A") .and. (xre_str(i:i) <= "F") ) xre_str(i:i) = achar(iachar(xre_str(i:i))+32)
      end do
    end if if_z_re
    if_z_im: if ( fmt_ == "z" ) then
      if ( x%im /= 0e0_r128 ) then
        xim_str = "0x00000000000000000000000000000000"
      else
        xim_str = "0x0"; exit if_z_im
      end if

      write(unit=xim_str(3:), fmt="(z32)") x%im

      do i = 3, 34
        if ( (xim_str(i:i) >= "A") .and. (xim_str(i:i) <= "F") ) xim_str(i:i) = achar(iachar(xim_str(i:i))+32)
      end do
    end if if_z_im

    if ( .not. present(locale) ) then
      decimal = "POINT"
    else
      if ( locale == "US" ) then
        decimal = "POINT"
      else if ( locale == "EU" ) then
        decimal = "COMMA"
      else
        new%s = EMPTY_STR; return
      end if
    end if

    if_eorf_re: if ( fmt_ == "e" ) then
      if ( x%re == 0e0_r128 ) then
        xre_str = "0.0e+0000"; exit if_eorf_re
      end if

      if ( x%re < 0e0_r128 ) then
        xre_str = "00000000000000000000000000000000000000000000"
        write(unit=xre_str, fmt="(es44.35e4)", decimal=decimal) x%re
        xre_str(39:39) = "e"
      else
        xre_str = "0000000000000000000000000000000000000000000"
        write(unit=xre_str, fmt="(es43.35e4)", decimal=decimal) x%re
        xre_str(38:38) = "e"
      end if

      if ( .not. present(decimals) ) exit if_eorf_re

      if ( decimals >= 35 ) exit if_eorf_re

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (xre_str(i:i) == POINT) .or. (xre_str(i:i) == COMMA) ) then
          xre_str = xre_str(:i+decimals_)//xre_str(i+36:); exit if_eorf_re
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x%re) /= 0e0_r128 ) then
        e = int(log10(abs(x%re)))
      else
        xre_str = "0.0"; exit if_eorf_re
      end if

      if ( e == 0 ) then
        if ( floor(x%re) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=125) :: xre_str )

      if ( e > 0 ) then
        write(unit=xre_str, fmt="(f0.36)", decimal=decimal) x%re
      else
        write(unit=xre_str, fmt="(f0.100)", decimal=decimal) x%re
      end if

      i = 1; do
        if ( (xre_str(i:i) == POINT) .or. (xre_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (xre_str(1:1) == "-") ) ) then
        xre_str(i+1:125) = xre_str(i:124); xre_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 36 ) then
        xre_str = xre_str(:i); exit if_eorf_re
      end if

      if ( .not. present(decimals) ) then
        xre_str = xre_str(:i+36-e); exit if_eorf_re
      end if

      if ( decimals <= 0 ) then
        xre_str = xre_str(:i); exit if_eorf_re
      end if

      if ( decimals >= 36-e ) then
        xre_str = xre_str(:i+36-e); exit if_eorf_re
      end if

      xre_str = xre_str(:i+decimals); exit if_eorf_re
    end if if_eorf_re
    if_eorf_im: if ( fmt_ == "e" ) then
      if ( x%im == 0e0_r128 ) then
        xim_str = "0.0e+0000"; exit if_eorf_im
      end if

      if ( x%im < 0e0_r128 ) then
        xim_str = "00000000000000000000000000000000000000000000"
        write(unit=xim_str, fmt="(es44.35e4)", decimal=decimal) x%im
        xim_str(39:39) = "e"
      else
        xim_str = "0000000000000000000000000000000000000000000"
        write(unit=xim_str, fmt="(es43.35e4)", decimal=decimal) x%im
        xim_str(38:38) = "e"
      end if

      if ( .not. present(decimals) ) exit if_eorf_im

      if ( decimals >= 35 ) exit if_eorf_im

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (xim_str(i:i) == POINT) .or. (xim_str(i:i) == COMMA) ) then
          xim_str = xim_str(:i+decimals_)//xim_str(i+36:); exit if_eorf_im
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x%im) /= 0e0_r128 ) then
        e = int(log10(abs(x%im)))
      else
        xim_str = "0.0"; exit if_eorf_im
      end if

      if ( e == 0 ) then
        if ( floor(x%im) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=125) :: xim_str )

      if ( e > 0 ) then
        write(unit=xim_str, fmt="(f0.36)", decimal=decimal) x%im
      else
        write(unit=xim_str, fmt="(f0.100)", decimal=decimal) x%im
      end if

      i = 1; do
        if ( (xim_str(i:i) == POINT) .or. (xim_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (xim_str(1:1) == "-") ) ) then
        xim_str(i+1:125) = xim_str(i:124); xim_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 36 ) then
        xim_str = xim_str(:i); exit if_eorf_im
      end if

      if ( .not. present(decimals) ) then
        xim_str = xim_str(:i+36-e); exit if_eorf_im
      end if

      if ( decimals <= 0 ) then
        xim_str = xim_str(:i); exit if_eorf_im
      end if

      if ( decimals >= 36-e ) then
        xim_str = xim_str(:i+36-e); exit if_eorf_im
      end if

      xim_str = xim_str(:i+decimals); exit if_eorf_im
    end if if_eorf_im

    if ( .not. present(im) ) then
      im_ = EMPTY_STR
    else
      im_ = im
    end if

    if ( im_ == EMPTY_STR ) then
      if ( decimal == "POINT" ) then
        new%s = "("//xre_str//COMMA//xim_str//")"; return
      else
        new%s = "("//xre_str//SEMICOLON//xim_str//")"; return
      end if
    end if

    if ( fmt_ == "z" ) then
      new%s = xre_str//"+"//xim_str//im_; return
    end if

    if ( x%im < 0e0_r128 ) then
      new%s = xre_str//xim_str//im_
    else
      new%s = xre_str//"+"//xim_str//im_
    end if
  end procedure new_string_from_c128
  module procedure new_string_from_c64
    character(len=1)              :: fmt_
    character(len=5)              :: decimal
    character(len=:), allocatable :: xre_str, xim_str, im_
    integer                       :: e, decimals_, i

    e=0; decimals_=0; i=0

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        new%s = EMPTY_STR; return
      end if
    end if

    if_z_re: if ( fmt_ == "z" ) then
      if ( x%re == 0e0_r64 ) then
        xre_str = "0x0"; exit if_z_re
      end if

      call cast(transfer(source=x%re, mold=1_i64), into=xre_str, fmt="z"); exit if_z_re
    end if if_z_re
    if_z_im: if ( fmt_ == "z" ) then
      if ( x%im == 0e0_r64 ) then
        xim_str = "0x0"; exit if_z_im
      end if

      call cast(transfer(source=x%im, mold=1_i64), into=xim_str, fmt="z"); exit if_z_im
    end if if_z_im

    if ( .not. present(locale) ) then
      decimal = "POINT"
    else
      if ( locale == "US" ) then
        decimal = "POINT"
      else if ( locale == "EU" ) then
        decimal = "COMMA"
      else
        new%s = EMPTY_STR; return
      end if
    end if

    if_eorf_re: if ( fmt_ == "e" ) then
      if ( x%re == 0e0_r64 ) then
        xre_str = "0.0e+000"; exit if_eorf_re
      end if

      if ( x%re < 0e0_r64 ) then
        xre_str = "0000000000000000000000000"
        write(unit=xre_str, fmt="(es25.17e3)", decimal=decimal) x%re
        xre_str(21:21) = "e"
      else
        xre_str = "000000000000000000000000"
        write(unit=xre_str, fmt="(es24.17e3)", decimal=decimal) x%re
        xre_str(20:20) = "e"
      end if

      if ( .not. present(decimals) ) exit if_eorf_re

      if ( decimals >= 17 ) exit if_eorf_re

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (xre_str(i:i) == POINT) .or. (xre_str(i:i) == COMMA) ) then
          xre_str = xre_str(:i+decimals_)//xre_str(i+18:); exit if_eorf_re
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x%re) /= 0e0_r64 ) then
        e = int(log10(abs(x%re)))
      else
        xre_str = "0.0"; return
      end if

      if ( e == 0 ) then
        if ( floor(x%re) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=100) :: xre_str )

      if ( e > 0 ) then
        write(unit=xre_str, fmt="(f0.18)", decimal=decimal) x%re
      else
        write(unit=xre_str, fmt="(f0.80)", decimal=decimal) x%re
      end if

      i = 1; do
        if ( (xre_str(i:i) == POINT) .or. (xre_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (xre_str(1:1) == "-") ) ) then
        xre_str(i+1:100) = xre_str(i:99); xre_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 18 ) then
        xre_str = xre_str(:i); exit if_eorf_re
      end if

      if ( .not. present(decimals) ) then
        xre_str = xre_str(:i+18-e); exit if_eorf_re
      end if

      if ( decimals <= 0 ) then
        xre_str = xre_str(:i); exit if_eorf_re
      end if

      if ( decimals >= 18-e ) then
        xre_str = xre_str(:i+18-e); exit if_eorf_re
      end if

      xre_str = xre_str(:i+decimals); exit if_eorf_re
    end if if_eorf_re
    if_eorf_im: if ( fmt_ == "e" ) then
      if ( x%im == 0e0_r64 ) then
        xim_str = "0.0e+000"; exit if_eorf_im
      end if

      if ( x%im < 0e0_r64 ) then
        xim_str = "0000000000000000000000000"
        write(unit=xim_str, fmt="(es25.17e3)", decimal=decimal) x%im
        xim_str(21:21) = "e"
      else
        xim_str = "000000000000000000000000"
        write(unit=xim_str, fmt="(es24.17e3)", decimal=decimal) x%im
        xim_str(20:20) = "e"
      end if

      if ( .not. present(decimals) ) exit if_eorf_im

      if ( decimals >= 17 ) exit if_eorf_im

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (xim_str(i:i) == POINT) .or. (xim_str(i:i) == COMMA) ) then
          xim_str = xim_str(:i+decimals_)//xim_str(i+18:); exit if_eorf_im
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x%im) /= 0e0_r64 ) then
        e = int(log10(abs(x%im)))
      else
        xim_str = "0.0"; return
      end if

      if ( e == 0 ) then
        if ( floor(x%im) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=100) :: xim_str )

      if ( e > 0 ) then
        write(unit=xim_str, fmt="(f0.18)", decimal=decimal) x%im
      else
        write(unit=xim_str, fmt="(f0.80)", decimal=decimal) x%im
      end if

      i = 1; do
        if ( (xim_str(i:i) == POINT) .or. (xim_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (xim_str(1:1) == "-") ) ) then
        xim_str(i+1:100) = xim_str(i:99); xim_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 18 ) then
        xim_str = xim_str(:i); exit if_eorf_im
      end if

      if ( .not. present(decimals) ) then
        xim_str = xim_str(:i+18-e); exit if_eorf_im
      end if

      if ( decimals <= 0 ) then
        xim_str = xim_str(:i); exit if_eorf_im
      end if

      if ( decimals >= 18-e ) then
        xim_str = xim_str(:i+18-e); exit if_eorf_im
      end if

      xim_str = xim_str(:i+decimals); exit if_eorf_im
    end if if_eorf_im

    if ( .not. present(im) ) then
      im_ = EMPTY_STR
    else
      im_ = im
    end if

    if ( im_ == EMPTY_STR ) then
      if ( decimal == "POINT" ) then
        new%s = "("//xre_str//COMMA//xim_str//")"; return
      else
        new%s = "("//xre_str//SEMICOLON//xim_str//")"; return
      end if
    end if

    if ( fmt_ == "z" ) then
      new%s = xre_str//"+"//xim_str//im_; return
    end if

    if ( x%im < 0e0_r64 ) then
      new%s = xre_str//xim_str//im_
    else
      new%s = xre_str//"+"//xim_str//im_
    end if
  end procedure new_string_from_c64
  module procedure new_string_from_c32
    character(len=1)              :: fmt_
    character(len=5)              :: decimal
    character(len=:), allocatable :: xre_str, xim_str, im_
    integer                       :: e, decimals_, i

    e=0; decimals_=0; i=0

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        new%s = EMPTY_STR; return
      end if
    end if

    if_z_re: if ( fmt_ == "z" ) then
      if ( x%re == 0e0_r32 ) then
        xre_str = "0x0"; exit if_z_re
      end if

      call cast(transfer(source=x%re, mold=1_i32), into=xre_str, fmt="z"); exit if_z_re
    end if if_z_re
    if_z_im: if ( fmt_ == "z" ) then
      if ( x%im == 0e0_r32 ) then
        xim_str = "0x0"; exit if_z_im
      end if

      call cast(transfer(source=x%im, mold=1_i32), into=xim_str, fmt="z"); exit if_z_im
    end if if_z_im

    if ( .not. present(locale) ) then
      decimal = "POINT"
    else
      if ( locale == "US" ) then
        decimal = "POINT"
      else if ( locale == "EU" ) then
        decimal = "COMMA"
      else
        new%s = EMPTY_STR; return
      end if
    end if

    if_eorf_re: if ( fmt_ == "e" ) then
      if ( x%re == 0e0_r32 ) then
        xre_str = "0.0e+00"; exit if_eorf_re
      end if

      if ( x%re < 0e0_r32 ) then
        xre_str = "000000000000000"
        write(unit=xre_str, fmt="(es15.8e2)", decimal=decimal) x%re
        xre_str(12:12) = "e"
      else
        xre_str = "00000000000000"
        write(unit=xre_str, fmt="(es14.8e2)", decimal=decimal) x%re
        xre_str(11:11) = "e"
      end if

      if ( .not. present(decimals) ) exit if_eorf_re

      if ( decimals >= 8 ) exit if_eorf_re

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (xre_str(i:i) == POINT) .or. (xre_str(i:i) == COMMA) ) then
          xre_str = xre_str(:i+decimals_)//xre_str(i+9:); exit if_eorf_re
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x%re) /= 0e0_r32 ) then
        e = int(log10(abs(x%re)))
      else
        xre_str = "0.0"; exit if_eorf_re
      end if

      if ( e == 0 ) then
        if ( floor(x%re) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=75) :: xre_str )

      if ( e > 0 ) then
        write(unit=xre_str, fmt="(f0.9)", decimal=decimal) x%re
      else
        write(unit=xre_str, fmt="(f0.70)", decimal=decimal) x%re
      end if

      i = 1; do
        if ( (xre_str(i:i) == POINT) .or. (xre_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (xre_str(1:1) == "-") ) ) then
        xre_str(i+1:75) = xre_str(i:74); xre_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 9 ) then
        xre_str = xre_str(:i); exit if_eorf_re
      end if

      if ( .not. present(decimals) ) then
        xre_str = xre_str(:i+9-e); exit if_eorf_re
      end if

      if ( decimals <= 0 ) then
        xre_str = xre_str(:i); exit if_eorf_re
      end if

      if ( decimals >= 9-e ) then
        xre_str = xre_str(:i+9-e); exit if_eorf_re
      end if

      xre_str = xre_str(:i+decimals); exit if_eorf_re
    end if if_eorf_re
    if_eorf_im: if ( fmt_ == "e" ) then
      if ( x%im == 0e0_r32 ) then
        xim_str = "0.0e+00"; exit if_eorf_im
      end if

      if ( x%im < 0e0_r32 ) then
        xim_str = "000000000000000"
        write(unit=xim_str, fmt="(es15.8e2)", decimal=decimal) x%im
        xim_str(12:12) = "e"
      else
        xim_str = "00000000000000"
        write(unit=xim_str, fmt="(es14.8e2)", decimal=decimal) x%im
        xim_str(11:11) = "e"
      end if

      if ( .not. present(decimals) ) exit if_eorf_im

      if ( decimals >= 8 ) exit if_eorf_im

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (xim_str(i:i) == POINT) .or. (xim_str(i:i) == COMMA) ) then
          xim_str = xim_str(:i+decimals_)//xim_str(i+9:); exit if_eorf_im
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x%im) /= 0e0_r32 ) then
        e = int(log10(abs(x%im)))
      else
        xim_str = "0.0"; exit if_eorf_im
      end if

      if ( e == 0 ) then
        if ( floor(x%im) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=75) :: xim_str )

      if ( e > 0 ) then
        write(unit=xim_str, fmt="(f0.9)", decimal=decimal) x%im
      else
        write(unit=xim_str, fmt="(f0.70)", decimal=decimal) x%im
      end if

      i = 1; do
        if ( (xim_str(i:i) == POINT) .or. (xim_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (xim_str(1:1) == "-") ) ) then
        xim_str(i+1:75) = xim_str(i:74); xim_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 9 ) then
        xim_str = xim_str(:i); exit if_eorf_im
      end if

      if ( .not. present(decimals) ) then
        xim_str = xim_str(:i+9-e); exit if_eorf_im
      end if

      if ( decimals <= 0 ) then
        xim_str = xim_str(:i); exit if_eorf_im
      end if

      if ( decimals >= 9-e ) then
        xim_str = xim_str(:i+9-e); exit if_eorf_im
      end if

      xim_str = xim_str(:i+decimals); exit if_eorf_im
    end if if_eorf_im

    if ( .not. present(im) ) then
      im_ = EMPTY_STR
    else
      im_ = im
    end if

    if ( im_ == EMPTY_STR ) then
      if ( decimal == "POINT" ) then
        new%s = "("//xre_str//COMMA//xim_str//")"; return
      else
        new%s = "("//xre_str//SEMICOLON//xim_str//")"; return
      end if
    end if

    if ( fmt_ == "z" ) then
      new%s = xre_str//"+"//xim_str//im_; return
    end if

    if ( x%im < 0e0_r32 ) then
      new%s = xre_str//xim_str//im_
    else
      new%s = xre_str//"+"//xim_str//im_
    end if
  end procedure new_string_from_c32

  module procedure new_string_from_r128
    character(len=1) :: fmt_
    character(len=5) :: decimal
    integer          :: e, decimals_, i

    e=0; decimals_=0; i=0

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        new%s = EMPTY_STR; return
      end if
    end if

    if ( fmt_ == "z" ) then
      if ( x /= 0e0_r128 ) then
        new%s = "0x00000000000000000000000000000000"
      else
        new%s = "0x0"; return
      end if

      write(unit=new%s(3:), fmt="(z32)") x

      do i = 3, 34
        if ( (new%s(i:i) >= "A") .and. (new%s(i:i) <= "F") ) new%s(i:i) = achar(iachar(new%s(i:i)) + 32)
      end do

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
        new%s = EMPTY_STR; return
      end if
    end if

    if ( fmt_ == "e" ) then
      if ( x == 0e0_r128 ) then
        new%s = "0.0e+0000"; return
      end if

      if ( x < 0e0_r128 ) then
        new%s = "00000000000000000000000000000000000000000000"
        write(unit=new%s, fmt="(es44.35e4)", decimal=decimal) x
        new%s(39:39) = "e"
      else
        new%s = "0000000000000000000000000000000000000000000"
        write(unit=new%s, fmt="(es43.35e4)", decimal=decimal) x
        new%s(38:38) = "e"
      end if

      if ( .not. present(decimals) ) return

      if ( decimals >= 35 ) return

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (new%s(i:i) == POINT) .or. (new%s(i:i) == COMMA) ) then
          new%s = new%s(:i+decimals_)//new%s(i+36:); return
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x) /= 0e0_r128 ) then
        e = int(log10(abs(x)))
      else
        new%s = "0.0"; return
      end if

      if ( e == 0 ) then
        if ( floor(x) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=125) :: new%s )

      if ( e > 0 ) then
        write(unit=new%s, fmt="(f0.36)", decimal=decimal) x
      else
        write(unit=new%s, fmt="(f0.100)", decimal=decimal) x
      end if

      i = 1; do
        if ( (new%s(i:i) == POINT) .or. (new%s(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (new%s(1:1) == "-") ) ) then
        new%s(i+1:125) = new%s(i:124); new%s(i:i) = "0"; i = i + 1
      end if

      if ( i > 36 ) then
        new%s = new%s(:i); return
      end if

      if ( .not. present(decimals) ) then
        new%s = new%s(:i+36-e); return
      end if

      if ( decimals <= 0 ) then
        new%s = new%s(:i); return
      end if

      if ( decimals >= 36-e ) then
        new%s = new%s(:i+36-e); return
      end if

      new%s = new%s(:i+decimals); return
    end if
  end procedure new_string_from_r128
  module procedure new_string_from_r64
    character(len=1) :: fmt_
    character(len=5) :: decimal
    integer          :: e, decimals_, i

    e=0; decimals_=0; i=0

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        new%s = EMPTY_STR; return
      end if
    end if

    if ( fmt_ == "z" ) then
      if ( x == 0e0_r64 ) then
        new%s = "0x0"; return
      end if

      inline_cast: block
        integer(i64) :: x_int, num, next; character(len=18) :: buffer; integer :: ascii_code
        logical :: negative

        x_int = transfer(source=x, mold=x_int)

        if ( x_int < 0_i64 ) then
          num = (x_int + 1_i64) + largest_i64; negative = .true.; buffer(1:) = "0x0000000000000000"
        else
          num = x_int; negative = .false.
        end if

        i = len(buffer); extract_hex_digits: do
          next = num/16_i64; buffer(i:i) = DIGITS_A(num - 16_i64*next); if ( next == 0_i64 ) exit
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
        new%s = EMPTY_STR; return
      end if
    end if

    if ( fmt_ == "e" ) then
      if ( x == 0e0_r64 ) then
        new%s = "0.0e+000"; return
      end if

      if ( x < 0e0_r64 ) then
        new%s = "0000000000000000000000000"
        write(unit=new%s, fmt="(es25.17e3)", decimal=decimal) x
        new%s(21:21) = "e"
      else
        new%s = "000000000000000000000000"
        write(unit=new%s, fmt="(es24.17e3)", decimal=decimal) x
        new%s(20:20) = "e"
      end if

      if ( .not. present(decimals) ) return

      if ( decimals >= 17 ) return

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (new%s(i:i) == POINT) .or. (new%s(i:i) == COMMA) ) then
          new%s = new%s(:i+decimals_)//new%s(i+18:); return
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x) /= 0e0_r64 ) then
        e = int(log10(abs(x)))
      else
        new%s = "0.0"; return
      end if

      if ( e == 0 ) then
        if ( floor(x) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=100) :: new%s )

      if ( e > 0 ) then
        write(unit=new%s, fmt="(f0.18)", decimal=decimal) x
      else
        write(unit=new%s, fmt="(f0.80)", decimal=decimal) x
      end if

      i = 1; do
        if ( (new%s(i:i) == POINT) .or. (new%s(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (new%s(1:1) == "-") ) ) then
        new%s(i+1:100) = new%s(i:99); new%s(i:i) = "0"; i = i + 1
      end if

      if ( i > 18 ) then
        new%s = new%s(:i); return
      end if

      if ( .not. present(decimals) ) then
        new%s = new%s(:i+18-e); return
      end if

      if ( decimals <= 0 ) then
        new%s = new%s(:i); return
      end if

      if ( decimals >= 18-e ) then
        new%s = new%s(:i+18-e); return
      end if

      new%s = new%s(:i+decimals); return
    end if
  end procedure new_string_from_r64
  module procedure new_string_from_r32
    character(len=1) :: fmt_
    character(len=5) :: decimal
    integer          :: e, decimals_, i

    e=0; decimals_=0; i=0

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        new%s = EMPTY_STR; return
      end if
    end if

    if ( fmt_ == "z" ) then
      if ( x == 0e0_r32 ) then
        new%s = "0x0"; return
      end if

      inline_cast: block
        integer :: x_int, num, next; character(len=10) :: buffer; integer :: ascii_code; logical :: negative

        x_int = transfer(source=x, mold=x_int)

        if ( x_int < 0 ) then
          num = (x_int + 1) + largest_i32; negative = .true.; buffer(1:) = "0x00000000"
        else
          num = x_int; negative = .false.
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
        new%s = EMPTY_STR; return
      end if
    end if

    if ( fmt_ == "e" ) then
      if ( x == 0e0_r32 ) then
        new%s = "0.0e+00"; return
      end if

      if ( x < 0e0_r32 ) then
        new%s = "000000000000000"
        write(unit=new%s, fmt="(es15.8e2)", decimal=decimal) x
        new%s(12:12) = "e"
      else
        new%s = "00000000000000"
        write(unit=new%s, fmt="(es14.8e2)", decimal=decimal) x
        new%s(11:11) = "e"
      end if

      if ( .not. present(decimals) ) return

      if ( decimals >= 8 ) return

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (new%s(i:i) == POINT) .or. (new%s(i:i) == COMMA) ) then
          new%s = new%s(:i+decimals_)//new%s(i+9:); return
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x) /= 0e0_r32 ) then
        e = int(log10(abs(x)))
      else
        new%s = "0.0"; return
      end if

      if ( e == 0 ) then
        if ( floor(x) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=75) :: new%s )

      if ( e > 0 ) then
        write(unit=new%s, fmt="(f0.9)", decimal=decimal) x
      else
        write(unit=new%s, fmt="(f0.70)", decimal=decimal) x
      end if

      i = 1; do
        if ( (new%s(i:i) == POINT) .or. (new%s(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (new%s(1:1) == "-") ) ) then
        new%s(i+1:75) = new%s(i:74); new%s(i:i) = "0"; i = i + 1
      end if

      if ( i > 9 ) then
        new%s = new%s(:i); return
      end if

      if ( .not. present(decimals) ) then
        new%s = new%s(:i+9-e); return
      end if

      if ( decimals <= 0 ) then
        new%s = new%s(:i); return
      end if

      if ( decimals >= 9-e ) then
        new%s = new%s(:i+9-e); return
      end if

      new%s = new%s(:i+decimals); return
    end if
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
        new%s = EMPTY_STR; return
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
        new%s = EMPTY_STR; return
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
        new%s = EMPTY_STR; return
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
        new%s = EMPTY_STR; return
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
  module procedure str_from_c128
    character(len=1)              :: fmt_
    character(len=5)              :: decimal
    character(len=:), allocatable :: xre_str, xim_str, im_
    integer                       :: e, decimals_, i

    e=0; decimals_=0; i=0

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        x_str = EMPTY_STR; return
      end if
    end if

    if_z_re: if ( fmt_ == "z" ) then
      if ( x%re /= 0e0_r128 ) then
        xre_str = "0x00000000000000000000000000000000"
      else
        xre_str = "0x0"; exit if_z_re
      end if

      write(unit=xre_str(3:), fmt="(z32)") x%re

      do i = 3, 34
        if ( (xre_str(i:i) >= "A") .and. (xre_str(i:i) <= "F") ) xre_str(i:i) = achar(iachar(xre_str(i:i))+32)
      end do
    end if if_z_re
    if_z_im: if ( fmt_ == "z" ) then
      if ( x%im /= 0e0_r128 ) then
        xim_str = "0x00000000000000000000000000000000"
      else
        xim_str = "0x0"; exit if_z_im
      end if

      write(unit=xim_str(3:), fmt="(z32)") x%im

      do i = 3, 34
        if ( (xim_str(i:i) >= "A") .and. (xim_str(i:i) <= "F") ) xim_str(i:i) = achar(iachar(xim_str(i:i))+32)
      end do
    end if if_z_im

    if ( .not. present(locale) ) then
      decimal = "POINT"
    else
      if ( locale == "US" ) then
        decimal = "POINT"
      else if ( locale == "EU" ) then
        decimal = "COMMA"
      else
        x_str = EMPTY_STR; return
      end if
    end if

    if_eorf_re: if ( fmt_ == "e" ) then
      if ( x%re == 0e0_r128 ) then
        xre_str = "0.0e+0000"; exit if_eorf_re
      end if

      if ( x%re < 0e0_r128 ) then
        xre_str = "00000000000000000000000000000000000000000000"
        write(unit=xre_str, fmt="(es44.35e4)", decimal=decimal) x%re
        xre_str(39:39) = "e"
      else
        xre_str = "0000000000000000000000000000000000000000000"
        write(unit=xre_str, fmt="(es43.35e4)", decimal=decimal) x%re
        xre_str(38:38) = "e"
      end if

      if ( .not. present(decimals) ) exit if_eorf_re

      if ( decimals >= 35 ) exit if_eorf_re

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (xre_str(i:i) == POINT) .or. (xre_str(i:i) == COMMA) ) then
          xre_str = xre_str(:i+decimals_)//xre_str(i+36:); exit if_eorf_re
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x%re) /= 0e0_r128 ) then
        e = int(log10(abs(x%re)))
      else
        xre_str = "0.0"; exit if_eorf_re
      end if

      if ( e == 0 ) then
        if ( floor(x%re) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=125) :: xre_str )

      if ( e > 0 ) then
        write(unit=xre_str, fmt="(f0.36)", decimal=decimal) x%re
      else
        write(unit=xre_str, fmt="(f0.100)", decimal=decimal) x%re
      end if

      i = 1; do
        if ( (xre_str(i:i) == POINT) .or. (xre_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (xre_str(1:1) == "-") ) ) then
        xre_str(i+1:125) = xre_str(i:124); xre_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 36 ) then
        xre_str = xre_str(:i); exit if_eorf_re
      end if

      if ( .not. present(decimals) ) then
        xre_str = xre_str(:i+36-e); exit if_eorf_re
      end if

      if ( decimals <= 0 ) then
        xre_str = xre_str(:i); exit if_eorf_re
      end if

      if ( decimals >= 36-e ) then
        xre_str = xre_str(:i+36-e); exit if_eorf_re
      end if

      xre_str = xre_str(:i+decimals); exit if_eorf_re
    end if if_eorf_re
    if_eorf_im: if ( fmt_ == "e" ) then
      if ( x%im == 0e0_r128 ) then
        xim_str = "0.0e+0000"; exit if_eorf_im
      end if

      if ( x%im < 0e0_r128 ) then
        xim_str = "00000000000000000000000000000000000000000000"
        write(unit=xim_str, fmt="(es44.35e4)", decimal=decimal) x%im
        xim_str(39:39) = "e"
      else
        xim_str = "0000000000000000000000000000000000000000000"
        write(unit=xim_str, fmt="(es43.35e4)", decimal=decimal) x%im
        xim_str(38:38) = "e"
      end if

      if ( .not. present(decimals) ) exit if_eorf_im

      if ( decimals >= 35 ) exit if_eorf_im

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (xim_str(i:i) == POINT) .or. (xim_str(i:i) == COMMA) ) then
          xim_str = xim_str(:i+decimals_)//xim_str(i+36:); exit if_eorf_im
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x%im) /= 0e0_r128 ) then
        e = int(log10(abs(x%im)))
      else
        xim_str = "0.0"; exit if_eorf_im
      end if

      if ( e == 0 ) then
        if ( floor(x%im) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=125) :: xim_str )

      if ( e > 0 ) then
        write(unit=xim_str, fmt="(f0.36)", decimal=decimal) x%im
      else
        write(unit=xim_str, fmt="(f0.100)", decimal=decimal) x%im
      end if

      i = 1; do
        if ( (xim_str(i:i) == POINT) .or. (xim_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (xim_str(1:1) == "-") ) ) then
        xim_str(i+1:125) = xim_str(i:124); xim_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 36 ) then
        xim_str = xim_str(:i); exit if_eorf_im
      end if

      if ( .not. present(decimals) ) then
        xim_str = xim_str(:i+36-e); exit if_eorf_im
      end if

      if ( decimals <= 0 ) then
        xim_str = xim_str(:i); exit if_eorf_im
      end if

      if ( decimals >= 36-e ) then
        xim_str = xim_str(:i+36-e); exit if_eorf_im
      end if

      xim_str = xim_str(:i+decimals); exit if_eorf_im
    end if if_eorf_im

    if ( .not. present(im) ) then
      im_ = EMPTY_STR
    else
      im_ = im
    end if

    if ( im_ == EMPTY_STR ) then
      if ( decimal == "POINT" ) then
        x_str = "("//xre_str//COMMA//xim_str//")"; return
      else
        x_str = "("//xre_str//SEMICOLON//xim_str//")"; return
      end if
    end if

    if ( fmt_ == "z" ) then
      x_str = xre_str//"+"//xim_str//im_; return
    end if

    if ( x%im < 0e0_r128 ) then
      x_str = xre_str//xim_str//im_
    else
      x_str = xre_str//"+"//xim_str//im_
    end if
  end procedure str_from_c128
  module procedure str_from_c64
    character(len=1)              :: fmt_
    character(len=5)              :: decimal
    character(len=:), allocatable :: xre_str, xim_str, im_
    integer                       :: e, decimals_, i

    e=0; decimals_=0; i=0

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        x_str = EMPTY_STR; return
      end if
    end if

    if_z_re: if ( fmt_ == "z" ) then
      if ( x%re == 0e0_r64 ) then
        xre_str = "0x0"; exit if_z_re
      end if

      call cast(transfer(source=x%re, mold=1_i64), into=xre_str, fmt="z"); exit if_z_re
    end if if_z_re
    if_z_im: if ( fmt_ == "z" ) then
      if ( x%im == 0e0_r64 ) then
        xim_str = "0x0"; exit if_z_im
      end if

      call cast(transfer(source=x%im, mold=1_i64), into=xim_str, fmt="z"); exit if_z_im
    end if if_z_im

    if ( .not. present(locale) ) then
      decimal = "POINT"
    else
      if ( locale == "US" ) then
        decimal = "POINT"
      else if ( locale == "EU" ) then
        decimal = "COMMA"
      else
        x_str = EMPTY_STR; return
      end if
    end if

    if_eorf_re: if ( fmt_ == "e" ) then
      if ( x%re == 0e0_r64 ) then
        xre_str = "0.0e+000"; exit if_eorf_re
      end if

      if ( x%re < 0e0_r64 ) then
        xre_str = "0000000000000000000000000"
        write(unit=xre_str, fmt="(es25.17e3)", decimal=decimal) x%re
        xre_str(21:21) = "e"
      else
        xre_str = "000000000000000000000000"
        write(unit=xre_str, fmt="(es24.17e3)", decimal=decimal) x%re
        xre_str(20:20) = "e"
      end if

      if ( .not. present(decimals) ) exit if_eorf_re

      if ( decimals >= 17 ) exit if_eorf_re

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (xre_str(i:i) == POINT) .or. (xre_str(i:i) == COMMA) ) then
          xre_str = xre_str(:i+decimals_)//xre_str(i+18:); exit if_eorf_re
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x%re) /= 0e0_r64 ) then
        e = int(log10(abs(x%re)))
      else
        xre_str = "0.0"; return
      end if

      if ( e == 0 ) then
        if ( floor(x%re) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=100) :: xre_str )

      if ( e > 0 ) then
        write(unit=xre_str, fmt="(f0.18)", decimal=decimal) x%re
      else
        write(unit=xre_str, fmt="(f0.80)", decimal=decimal) x%re
      end if

      i = 1; do
        if ( (xre_str(i:i) == POINT) .or. (xre_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (xre_str(1:1) == "-") ) ) then
        xre_str(i+1:100) = xre_str(i:99); xre_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 18 ) then
        xre_str = xre_str(:i); exit if_eorf_re
      end if

      if ( .not. present(decimals) ) then
        xre_str = xre_str(:i+18-e); exit if_eorf_re
      end if

      if ( decimals <= 0 ) then
        xre_str = xre_str(:i); exit if_eorf_re
      end if

      if ( decimals >= 18-e ) then
        xre_str = xre_str(:i+18-e); exit if_eorf_re
      end if

      xre_str = xre_str(:i+decimals); exit if_eorf_re
    end if if_eorf_re
    if_eorf_im: if ( fmt_ == "e" ) then
      if ( x%im == 0e0_r64 ) then
        xim_str = "0.0e+000"; exit if_eorf_im
      end if

      if ( x%im < 0e0_r64 ) then
        xim_str = "0000000000000000000000000"
        write(unit=xim_str, fmt="(es25.17e3)", decimal=decimal) x%im
        xim_str(21:21) = "e"
      else
        xim_str = "000000000000000000000000"
        write(unit=xim_str, fmt="(es24.17e3)", decimal=decimal) x%im
        xim_str(20:20) = "e"
      end if

      if ( .not. present(decimals) ) exit if_eorf_im

      if ( decimals >= 17 ) exit if_eorf_im

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (xim_str(i:i) == POINT) .or. (xim_str(i:i) == COMMA) ) then
          xim_str = xim_str(:i+decimals_)//xim_str(i+18:); exit if_eorf_im
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x%im) /= 0e0_r64 ) then
        e = int(log10(abs(x%im)))
      else
        xim_str = "0.0"; return
      end if

      if ( e == 0 ) then
        if ( floor(x%im) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=100) :: xim_str )

      if ( e > 0 ) then
        write(unit=xim_str, fmt="(f0.18)", decimal=decimal) x%im
      else
        write(unit=xim_str, fmt="(f0.80)", decimal=decimal) x%im
      end if

      i = 1; do
        if ( (xim_str(i:i) == POINT) .or. (xim_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (xim_str(1:1) == "-") ) ) then
        xim_str(i+1:100) = xim_str(i:99); xim_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 18 ) then
        xim_str = xim_str(:i); exit if_eorf_im
      end if

      if ( .not. present(decimals) ) then
        xim_str = xim_str(:i+18-e); exit if_eorf_im
      end if

      if ( decimals <= 0 ) then
        xim_str = xim_str(:i); exit if_eorf_im
      end if

      if ( decimals >= 18-e ) then
        xim_str = xim_str(:i+18-e); exit if_eorf_im
      end if

      xim_str = xim_str(:i+decimals); exit if_eorf_im
    end if if_eorf_im

    if ( .not. present(im) ) then
      im_ = EMPTY_STR
    else
      im_ = im
    end if

    if ( im_ == EMPTY_STR ) then
      if ( decimal == "POINT" ) then
        x_str = "("//xre_str//COMMA//xim_str//")"; return
      else
        x_str = "("//xre_str//SEMICOLON//xim_str//")"; return
      end if
    end if

    if ( fmt_ == "z" ) then
      x_str = xre_str//"+"//xim_str//im_; return
    end if

    if ( x%im < 0e0_r64 ) then
      x_str = xre_str//xim_str//im_
    else
      x_str = xre_str//"+"//xim_str//im_
    end if
  end procedure str_from_c64
  module procedure str_from_c32
    character(len=1)              :: fmt_
    character(len=5)              :: decimal
    character(len=:), allocatable :: xre_str, xim_str, im_
    integer                       :: e, decimals_, i

    e=0; decimals_=0; i=0

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        x_str = EMPTY_STR; return
      end if
    end if

    if_z_re: if ( fmt_ == "z" ) then
      if ( x%re == 0e0_r32 ) then
        xre_str = "0x0"; exit if_z_re
      end if

      call cast(transfer(source=x%re, mold=1_i32), into=xre_str, fmt="z"); exit if_z_re
    end if if_z_re
    if_z_im: if ( fmt_ == "z" ) then
      if ( x%im == 0e0_r32 ) then
        xim_str = "0x0"; exit if_z_im
      end if

      call cast(transfer(source=x%im, mold=1_i32), into=xim_str, fmt="z"); exit if_z_im
    end if if_z_im

    if ( .not. present(locale) ) then
      decimal = "POINT"
    else
      if ( locale == "US" ) then
        decimal = "POINT"
      else if ( locale == "EU" ) then
        decimal = "COMMA"
      else
        x_str = EMPTY_STR; return
      end if
    end if

    if_eorf_re: if ( fmt_ == "e" ) then
      if ( x%re == 0e0_r32 ) then
        xre_str = "0.0e+00"; exit if_eorf_re
      end if

      if ( x%re < 0e0_r32 ) then
        xre_str = "000000000000000"
        write(unit=xre_str, fmt="(es15.8e2)", decimal=decimal) x%re
        xre_str(12:12) = "e"
      else
        xre_str = "00000000000000"
        write(unit=xre_str, fmt="(es14.8e2)", decimal=decimal) x%re
        xre_str(11:11) = "e"
      end if

      if ( .not. present(decimals) ) exit if_eorf_re

      if ( decimals >= 8 ) exit if_eorf_re

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (xre_str(i:i) == POINT) .or. (xre_str(i:i) == COMMA) ) then
          xre_str = xre_str(:i+decimals_)//xre_str(i+9:); exit if_eorf_re
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x%re) /= 0e0_r32 ) then
        e = int(log10(abs(x%re)))
      else
        xre_str = "0.0"; exit if_eorf_re
      end if

      if ( e == 0 ) then
        if ( floor(x%re) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=75) :: xre_str )

      if ( e > 0 ) then
        write(unit=xre_str, fmt="(f0.9)", decimal=decimal) x%re
      else
        write(unit=xre_str, fmt="(f0.70)", decimal=decimal) x%re
      end if

      i = 1; do
        if ( (xre_str(i:i) == POINT) .or. (xre_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (xre_str(1:1) == "-") ) ) then
        xre_str(i+1:75) = xre_str(i:74); xre_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 9 ) then
        xre_str = xre_str(:i); exit if_eorf_re
      end if

      if ( .not. present(decimals) ) then
        xre_str = xre_str(:i+9-e); exit if_eorf_re
      end if

      if ( decimals <= 0 ) then
        xre_str = xre_str(:i); exit if_eorf_re
      end if

      if ( decimals >= 9-e ) then
        xre_str = xre_str(:i+9-e); exit if_eorf_re
      end if

      xre_str = xre_str(:i+decimals); exit if_eorf_re
    end if if_eorf_re
    if_eorf_im: if ( fmt_ == "e" ) then
      if ( x%im == 0e0_r32 ) then
        xim_str = "0.0e+00"; exit if_eorf_im
      end if

      if ( x%im < 0e0_r32 ) then
        xim_str = "000000000000000"
        write(unit=xim_str, fmt="(es15.8e2)", decimal=decimal) x%im
        xim_str(12:12) = "e"
      else
        xim_str = "00000000000000"
        write(unit=xim_str, fmt="(es14.8e2)", decimal=decimal) x%im
        xim_str(11:11) = "e"
      end if

      if ( .not. present(decimals) ) exit if_eorf_im

      if ( decimals >= 8 ) exit if_eorf_im

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (xim_str(i:i) == POINT) .or. (xim_str(i:i) == COMMA) ) then
          xim_str = xim_str(:i+decimals_)//xim_str(i+9:); exit if_eorf_im
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x%im) /= 0e0_r32 ) then
        e = int(log10(abs(x%im)))
      else
        xim_str = "0.0"; exit if_eorf_im
      end if

      if ( e == 0 ) then
        if ( floor(x%im) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=75) :: xim_str )

      if ( e > 0 ) then
        write(unit=xim_str, fmt="(f0.9)", decimal=decimal) x%im
      else
        write(unit=xim_str, fmt="(f0.70)", decimal=decimal) x%im
      end if

      i = 1; do
        if ( (xim_str(i:i) == POINT) .or. (xim_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (xim_str(1:1) == "-") ) ) then
        xim_str(i+1:75) = xim_str(i:74); xim_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 9 ) then
        xim_str = xim_str(:i); exit if_eorf_im
      end if

      if ( .not. present(decimals) ) then
        xim_str = xim_str(:i+9-e); exit if_eorf_im
      end if

      if ( decimals <= 0 ) then
        xim_str = xim_str(:i); exit if_eorf_im
      end if

      if ( decimals >= 9-e ) then
        xim_str = xim_str(:i+9-e); exit if_eorf_im
      end if

      xim_str = xim_str(:i+decimals); exit if_eorf_im
    end if if_eorf_im

    if ( .not. present(im) ) then
      im_ = EMPTY_STR
    else
      im_ = im
    end if

    if ( im_ == EMPTY_STR ) then
      if ( decimal == "POINT" ) then
        x_str = "("//xre_str//COMMA//xim_str//")"; return
      else
        x_str = "("//xre_str//SEMICOLON//xim_str//")"; return
      end if
    end if

    if ( fmt_ == "z" ) then
      x_str = xre_str//"+"//xim_str//im_; return
    end if

    if ( x%im < 0e0_r32 ) then
      x_str = xre_str//xim_str//im_
    else
      x_str = xre_str//"+"//xim_str//im_
    end if
  end procedure str_from_c32

  module procedure str_from_r128
    character(len=1) :: fmt_
    character(len=5) :: decimal
    integer          :: e, decimals_, i

    e=0; decimals_=0; i=0

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        x_str = EMPTY_STR; return
      end if
    end if

    if ( fmt_ == "z" ) then
      if ( x /= 0e0_r128 ) then
        x_str = "0x00000000000000000000000000000000"
      else
        x_str = "0x0"; return
      end if

      write(unit=x_str(3:), fmt="(z32)") x

      do i = 3, 34
        if ( (x_str(i:i) >= "A") .and. (x_str(i:i) <= "F") ) x_str(i:i) = achar(iachar(x_str(i:i)) + 32)
      end do

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
        x_str = EMPTY_STR; return
      end if
    end if

    if ( fmt_ == "e" ) then
      if ( x == 0e0_r128 ) then
        x_str = "0.0e+0000"; return
      end if

      if ( x < 0e0_r128 ) then
        x_str = "00000000000000000000000000000000000000000000"
        write(unit=x_str, fmt="(es44.35e4)", decimal=decimal) x
        x_str(39:39) = "e"
      else
        x_str = "0000000000000000000000000000000000000000000"
        write(unit=x_str, fmt="(es43.35e4)", decimal=decimal) x
        x_str(38:38) = "e"
      end if

      if ( .not. present(decimals) ) return

      if ( decimals >= 35 ) return

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (x_str(i:i) == POINT) .or. (x_str(i:i) == COMMA) ) then
          x_str = x_str(:i+decimals_)//x_str(i+36:); return
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x) /= 0e0_r128 ) then
        e = int(log10(abs(x)))
      else
        x_str = "0.0"; return
      end if

      if ( e == 0 ) then
        if ( floor(x) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=125) :: x_str )

      if ( e > 0 ) then
        write(unit=x_str, fmt="(f0.36)", decimal=decimal) x
      else
        write(unit=x_str, fmt="(f0.100)", decimal=decimal) x
      end if

      i = 1; do
        if ( (x_str(i:i) == POINT) .or. (x_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (x_str(1:1) == "-") ) ) then
        x_str(i+1:125) = x_str(i:124); x_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 36 ) then
        x_str = x_str(:i); return
      end if

      if ( .not. present(decimals) ) then
        x_str = x_str(:i+36-e); return
      end if

      if ( decimals <= 0 ) then
        x_str = x_str(:i); return
      end if

      if ( decimals >= 36-e ) then
        x_str = x_str(:i+36-e); return
      end if

      x_str = x_str(:i+decimals); return
    end if
  end procedure str_from_r128
  module procedure str_from_r64
    character(len=1) :: fmt_
    character(len=5) :: decimal
    integer          :: e, decimals_, i

    e=0; decimals_=0; i=0

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        x_str = EMPTY_STR; return
      end if
    end if

    if ( fmt_ == "z" ) then
      if ( x == 0e0_r64 ) then
        x_str = "0x0"; return
      end if

      call cast(transfer(source=x, mold=1_i64), into=x_str, fmt="z"); return
    end if

    if ( .not. present(locale) ) then
      decimal = "POINT"
    else
      if ( locale == "US" ) then
        decimal = "POINT"
      else if ( locale == "EU" ) then
        decimal = "COMMA"
      else
        x_str = EMPTY_STR; return
      end if
    end if

    if ( fmt_ == "e" ) then
      if ( x == 0e0_r64 ) then
        x_str = "0.0e+000"; return
      end if

      if ( x < 0e0_r64 ) then
        x_str = "0000000000000000000000000"
        write(unit=x_str, fmt="(es25.17e3)", decimal=decimal) x
        x_str(21:21) = "e"
      else
        x_str = "000000000000000000000000"
        write(unit=x_str, fmt="(es24.17e3)", decimal=decimal) x
        x_str(20:20) = "e"
      end if

      if ( .not. present(decimals) ) return

      if ( decimals >= 17 ) return

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (x_str(i:i) == POINT) .or. (x_str(i:i) == COMMA) ) then
          x_str = x_str(:i+decimals_)//x_str(i+18:); return
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x) /= 0e0_r64 ) then
        e = int(log10(abs(x)))
      else
        x_str = "0.0"; return
      end if

      if ( e == 0 ) then
        if ( floor(x) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=100) :: x_str )

      if ( e > 0 ) then
        write(unit=x_str, fmt="(f0.18)", decimal=decimal) x
      else
        write(unit=x_str, fmt="(f0.80)", decimal=decimal) x
      end if

      i = 1; do
        if ( (x_str(i:i) == POINT) .or. (x_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (x_str(1:1) == "-") ) ) then
        x_str(i+1:100) = x_str(i:99); x_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 18 ) then
        x_str = x_str(:i); return
      end if

      if ( .not. present(decimals) ) then
        x_str = x_str(:i+18-e); return
      end if

      if ( decimals <= 0 ) then
        x_str = x_str(:i); return
      end if

      if ( decimals >= 18-e ) then
        x_str = x_str(:i+18-e); return
      end if

      x_str = x_str(:i+decimals); return
    end if
  end procedure str_from_r64
  module procedure str_from_r32
    character(len=1) :: fmt_
    character(len=5) :: decimal
    integer          :: e, decimals_, i

    e=0; decimals_=0; i=0

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        x_str = EMPTY_STR; return
      end if
    end if

    if ( fmt_ == "z" ) then
      if ( x == 0e0_r32 ) then
        x_str = "0x0"; return
      end if

      call cast(transfer(source=x, mold=1_i32), into=x_str, fmt="z"); return
    end if

    if ( .not. present(locale) ) then
      decimal = "POINT"
    else
      if ( locale == "US" ) then
        decimal = "POINT"
      else if ( locale == "EU" ) then
        decimal = "COMMA"
      else
        x_str = EMPTY_STR; return
      end if
    end if

    if ( fmt_ == "e" ) then
      if ( x == 0e0_r32 ) then
        x_str = "0.0e+00"; return
      end if

      if ( x < 0e0_r32 ) then
        x_str = "000000000000000"
        write(unit=x_str, fmt="(es15.8e2)", decimal=decimal) x
        x_str(12:12) = "e"
      else
        x_str = "00000000000000"
        write(unit=x_str, fmt="(es14.8e2)", decimal=decimal) x
        x_str(11:11) = "e"
      end if

      if ( .not. present(decimals) ) return

      if ( decimals >= 8 ) return

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (x_str(i:i) == POINT) .or. (x_str(i:i) == COMMA) ) then
          x_str = x_str(:i+decimals_)//x_str(i+9:); return
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x) /= 0e0_r32 ) then
        e = int(log10(abs(x)))
      else
        x_str = "0.0"; return
      end if

      if ( e == 0 ) then
        if ( floor(x) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=75) :: x_str )

      if ( e > 0 ) then
        write(unit=x_str, fmt="(f0.9)", decimal=decimal) x
      else
        write(unit=x_str, fmt="(f0.70)", decimal=decimal) x
      end if

      i = 1; do
        if ( (x_str(i:i) == POINT) .or. (x_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (x_str(1:1) == "-") ) ) then
        x_str(i+1:75) = x_str(i:74); x_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 9 ) then
        x_str = x_str(:i); return
      end if

      if ( .not. present(decimals) ) then
        x_str = x_str(:i+9-e); return
      end if

      if ( decimals <= 0 ) then
        x_str = x_str(:i); return
      end if

      if ( decimals >= 9-e ) then
        x_str = x_str(:i+9-e); return
      end if

      x_str = x_str(:i+decimals); return
    end if
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
        x_str = EMPTY_STR; return
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
        x_str = EMPTY_STR; return
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
        x_str = EMPTY_STR; return
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
        x_str = EMPTY_STR; return
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
  module procedure cast_c128_to_string
    character(len=1)              :: fmt_
    character(len=5)              :: decimal
    character(len=:), allocatable :: xre_str, xim_str, im_
    integer                       :: e, decimals_, i

    e=0; decimals_=0; i=0

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into%s = EMPTY_STR; return
      end if
    end if

    if_z_re: if ( fmt_ == "z" ) then
      if ( x%re /= 0e0_r128 ) then
        xre_str = "0x00000000000000000000000000000000"
      else
        xre_str = "0x0"; exit if_z_re
      end if

      write(unit=xre_str(3:), fmt="(z32)") x%re

      do i = 3, 34
        if ( (xre_str(i:i) >= "A") .and. (xre_str(i:i) <= "F") ) xre_str(i:i) = achar(iachar(xre_str(i:i))+32)
      end do
    end if if_z_re
    if_z_im: if ( fmt_ == "z" ) then
      if ( x%im /= 0e0_r128 ) then
        xim_str = "0x00000000000000000000000000000000"
      else
        xim_str = "0x0"; exit if_z_im
      end if

      write(unit=xim_str(3:), fmt="(z32)") x%im

      do i = 3, 34
        if ( (xim_str(i:i) >= "A") .and. (xim_str(i:i) <= "F") ) xim_str(i:i) = achar(iachar(xim_str(i:i))+32)
      end do
    end if if_z_im

    if ( .not. present(locale) ) then
      decimal = "POINT"
    else
      if ( locale == "US" ) then
        decimal = "POINT"
      else if ( locale == "EU" ) then
        decimal = "COMMA"
      else
        into%s = EMPTY_STR; return
      end if
    end if

    if_eorf_re: if ( fmt_ == "e" ) then
      if ( x%re == 0e0_r128 ) then
        xre_str = "0.0e+0000"; exit if_eorf_re
      end if

      if ( x%re < 0e0_r128 ) then
        xre_str = "00000000000000000000000000000000000000000000"
        write(unit=xre_str, fmt="(es44.35e4)", decimal=decimal) x%re
        xre_str(39:39) = "e"
      else
        xre_str = "0000000000000000000000000000000000000000000"
        write(unit=xre_str, fmt="(es43.35e4)", decimal=decimal) x%re
        xre_str(38:38) = "e"
      end if

      if ( .not. present(decimals) ) exit if_eorf_re

      if ( decimals >= 35 ) exit if_eorf_re

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (xre_str(i:i) == POINT) .or. (xre_str(i:i) == COMMA) ) then
          xre_str = xre_str(:i+decimals_)//xre_str(i+36:); exit if_eorf_re
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x%re) /= 0e0_r128 ) then
        e = int(log10(abs(x%re)))
      else
        xre_str = "0.0"; exit if_eorf_re
      end if

      if ( e == 0 ) then
        if ( floor(x%re) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=125) :: xre_str )

      if ( e > 0 ) then
        write(unit=xre_str, fmt="(f0.36)", decimal=decimal) x%re
      else
        write(unit=xre_str, fmt="(f0.100)", decimal=decimal) x%re
      end if

      i = 1; do
        if ( (xre_str(i:i) == POINT) .or. (xre_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (xre_str(1:1) == "-") ) ) then
        xre_str(i+1:125) = xre_str(i:124); xre_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 36 ) then
        xre_str = xre_str(:i); exit if_eorf_re
      end if

      if ( .not. present(decimals) ) then
        xre_str = xre_str(:i+36-e); exit if_eorf_re
      end if

      if ( decimals <= 0 ) then
        xre_str = xre_str(:i); exit if_eorf_re
      end if

      if ( decimals >= 36-e ) then
        xre_str = xre_str(:i+36-e); exit if_eorf_re
      end if

      xre_str = xre_str(:i+decimals); exit if_eorf_re
    end if if_eorf_re
    if_eorf_im: if ( fmt_ == "e" ) then
      if ( x%im == 0e0_r128 ) then
        xim_str = "0.0e+0000"; exit if_eorf_im
      end if

      if ( x%im < 0e0_r128 ) then
        xim_str = "00000000000000000000000000000000000000000000"
        write(unit=xim_str, fmt="(es44.35e4)", decimal=decimal) x%im
        xim_str(39:39) = "e"
      else
        xim_str = "0000000000000000000000000000000000000000000"
        write(unit=xim_str, fmt="(es43.35e4)", decimal=decimal) x%im
        xim_str(38:38) = "e"
      end if

      if ( .not. present(decimals) ) exit if_eorf_im

      if ( decimals >= 35 ) exit if_eorf_im

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (xim_str(i:i) == POINT) .or. (xim_str(i:i) == COMMA) ) then
          xim_str = xim_str(:i+decimals_)//xim_str(i+36:); exit if_eorf_im
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x%im) /= 0e0_r128 ) then
        e = int(log10(abs(x%im)))
      else
        xim_str = "0.0"; exit if_eorf_im
      end if

      if ( e == 0 ) then
        if ( floor(x%im) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=125) :: xim_str )

      if ( e > 0 ) then
        write(unit=xim_str, fmt="(f0.36)", decimal=decimal) x%im
      else
        write(unit=xim_str, fmt="(f0.100)", decimal=decimal) x%im
      end if

      i = 1; do
        if ( (xim_str(i:i) == POINT) .or. (xim_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (xim_str(1:1) == "-") ) ) then
        xim_str(i+1:125) = xim_str(i:124); xim_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 36 ) then
        xim_str = xim_str(:i); exit if_eorf_im
      end if

      if ( .not. present(decimals) ) then
        xim_str = xim_str(:i+36-e); exit if_eorf_im
      end if

      if ( decimals <= 0 ) then
        xim_str = xim_str(:i); exit if_eorf_im
      end if

      if ( decimals >= 36-e ) then
        xim_str = xim_str(:i+36-e); exit if_eorf_im
      end if

      xim_str = xim_str(:i+decimals); exit if_eorf_im
    end if if_eorf_im

    if ( .not. present(im) ) then
      im_ = EMPTY_STR
    else
      im_ = im
    end if

    if ( im_ == EMPTY_STR ) then
      if ( decimal == "POINT" ) then
        into%s = "("//xre_str//COMMA//xim_str//")"; return
      else
        into%s = "("//xre_str//SEMICOLON//xim_str//")"; return
      end if
    end if

    if ( fmt_ == "z" ) then
      into%s = xre_str//"+"//xim_str//im_; return
    end if

    if ( x%im < 0e0_r128 ) then
      into%s = xre_str//xim_str//im_
    else
      into%s = xre_str//"+"//xim_str//im_
    end if
  end procedure cast_c128_to_string
  module procedure cast_c64_to_string
    character(len=1)              :: fmt_
    character(len=5)              :: decimal
    character(len=:), allocatable :: xre_str, xim_str, im_
    integer                       :: e, decimals_, i

    e=0; decimals_=0; i=0

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into%s = EMPTY_STR; return
      end if
    end if

    if_z_re: if ( fmt_ == "z" ) then
      if ( x%re == 0e0_r64 ) then
        xre_str = "0x0"; exit if_z_re
      end if

      call cast(transfer(source=x%re, mold=1_i64), into=xre_str, fmt="z"); exit if_z_re
    end if if_z_re
    if_z_im: if ( fmt_ == "z" ) then
      if ( x%im == 0e0_r64 ) then
        xim_str = "0x0"; exit if_z_im
      end if

      call cast(transfer(source=x%im, mold=1_i64), into=xim_str, fmt="z"); exit if_z_im
    end if if_z_im

    if ( .not. present(locale) ) then
      decimal = "POINT"
    else
      if ( locale == "US" ) then
        decimal = "POINT"
      else if ( locale == "EU" ) then
        decimal = "COMMA"
      else
        into%s = EMPTY_STR; return
      end if
    end if

    if_eorf_re: if ( fmt_ == "e" ) then
      if ( x%re == 0e0_r64 ) then
        xre_str = "0.0e+000"; exit if_eorf_re
      end if

      if ( x%re < 0e0_r64 ) then
        xre_str = "0000000000000000000000000"
        write(unit=xre_str, fmt="(es25.17e3)", decimal=decimal) x%re
        xre_str(21:21) = "e"
      else
        xre_str = "000000000000000000000000"
        write(unit=xre_str, fmt="(es24.17e3)", decimal=decimal) x%re
        xre_str(20:20) = "e"
      end if

      if ( .not. present(decimals) ) exit if_eorf_re

      if ( decimals >= 17 ) exit if_eorf_re

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (xre_str(i:i) == POINT) .or. (xre_str(i:i) == COMMA) ) then
          xre_str = xre_str(:i+decimals_)//xre_str(i+18:); exit if_eorf_re
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x%re) /= 0e0_r64 ) then
        e = int(log10(abs(x%re)))
      else
        xre_str = "0.0"; return
      end if

      if ( e == 0 ) then
        if ( floor(x%re) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=100) :: xre_str )

      if ( e > 0 ) then
        write(unit=xre_str, fmt="(f0.18)", decimal=decimal) x%re
      else
        write(unit=xre_str, fmt="(f0.80)", decimal=decimal) x%re
      end if

      i = 1; do
        if ( (xre_str(i:i) == POINT) .or. (xre_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (xre_str(1:1) == "-") ) ) then
        xre_str(i+1:100) = xre_str(i:99); xre_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 18 ) then
        xre_str = xre_str(:i); exit if_eorf_re
      end if

      if ( .not. present(decimals) ) then
        xre_str = xre_str(:i+18-e); exit if_eorf_re
      end if

      if ( decimals <= 0 ) then
        xre_str = xre_str(:i); exit if_eorf_re
      end if

      if ( decimals >= 18-e ) then
        xre_str = xre_str(:i+18-e); exit if_eorf_re
      end if

      xre_str = xre_str(:i+decimals); exit if_eorf_re
    end if if_eorf_re
    if_eorf_im: if ( fmt_ == "e" ) then
      if ( x%im == 0e0_r64 ) then
        xim_str = "0.0e+000"; exit if_eorf_im
      end if

      if ( x%im < 0e0_r64 ) then
        xim_str = "0000000000000000000000000"
        write(unit=xim_str, fmt="(es25.17e3)", decimal=decimal) x%im
        xim_str(21:21) = "e"
      else
        xim_str = "000000000000000000000000"
        write(unit=xim_str, fmt="(es24.17e3)", decimal=decimal) x%im
        xim_str(20:20) = "e"
      end if

      if ( .not. present(decimals) ) exit if_eorf_im

      if ( decimals >= 17 ) exit if_eorf_im

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (xim_str(i:i) == POINT) .or. (xim_str(i:i) == COMMA) ) then
          xim_str = xim_str(:i+decimals_)//xim_str(i+18:); exit if_eorf_im
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x%im) /= 0e0_r64 ) then
        e = int(log10(abs(x%im)))
      else
        xim_str = "0.0"; return
      end if

      if ( e == 0 ) then
        if ( floor(x%im) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=100) :: xim_str )

      if ( e > 0 ) then
        write(unit=xim_str, fmt="(f0.18)", decimal=decimal) x%im
      else
        write(unit=xim_str, fmt="(f0.80)", decimal=decimal) x%im
      end if

      i = 1; do
        if ( (xim_str(i:i) == POINT) .or. (xim_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (xim_str(1:1) == "-") ) ) then
        xim_str(i+1:100) = xim_str(i:99); xim_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 18 ) then
        xim_str = xim_str(:i); exit if_eorf_im
      end if

      if ( .not. present(decimals) ) then
        xim_str = xim_str(:i+18-e); exit if_eorf_im
      end if

      if ( decimals <= 0 ) then
        xim_str = xim_str(:i); exit if_eorf_im
      end if

      if ( decimals >= 18-e ) then
        xim_str = xim_str(:i+18-e); exit if_eorf_im
      end if

      xim_str = xim_str(:i+decimals); exit if_eorf_im
    end if if_eorf_im

    if ( .not. present(im) ) then
      im_ = EMPTY_STR
    else
      im_ = im
    end if

    if ( im_ == EMPTY_STR ) then
      if ( decimal == "POINT" ) then
        into%s = "("//xre_str//COMMA//xim_str//")"; return
      else
        into%s = "("//xre_str//SEMICOLON//xim_str//")"; return
      end if
    end if

    if ( fmt_ == "z" ) then
      into%s = xre_str//"+"//xim_str//im_; return
    end if

    if ( x%im < 0e0_r64 ) then
      into%s = xre_str//xim_str//im_
    else
      into%s = xre_str//"+"//xim_str//im_
    end if
  end procedure cast_c64_to_string
  module procedure cast_c32_to_string
    character(len=1)              :: fmt_
    character(len=5)              :: decimal
    character(len=:), allocatable :: xre_str, xim_str, im_
    integer                       :: e, decimals_, i

    e=0; decimals_=0; i=0

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into%s = EMPTY_STR; return
      end if
    end if

    if_z_re: if ( fmt_ == "z" ) then
      if ( x%re == 0e0_r32 ) then
        xre_str = "0x0"; exit if_z_re
      end if

      call cast(transfer(source=x%re, mold=1_i32), into=xre_str, fmt="z"); exit if_z_re
    end if if_z_re
    if_z_im: if ( fmt_ == "z" ) then
      if ( x%im == 0e0_r32 ) then
        xim_str = "0x0"; exit if_z_im
      end if

      call cast(transfer(source=x%im, mold=1_i32), into=xim_str, fmt="z"); exit if_z_im
    end if if_z_im

    if ( .not. present(locale) ) then
      decimal = "POINT"
    else
      if ( locale == "US" ) then
        decimal = "POINT"
      else if ( locale == "EU" ) then
        decimal = "COMMA"
      else
        into%s = EMPTY_STR; return
      end if
    end if

    if_eorf_re: if ( fmt_ == "e" ) then
      if ( x%re == 0e0_r32 ) then
        xre_str = "0.0e+00"; exit if_eorf_re
      end if

      if ( x%re < 0e0_r32 ) then
        xre_str = "000000000000000"
        write(unit=xre_str, fmt="(es15.8e2)", decimal=decimal) x%re
        xre_str(12:12) = "e"
      else
        xre_str = "00000000000000"
        write(unit=xre_str, fmt="(es14.8e2)", decimal=decimal) x%re
        xre_str(11:11) = "e"
      end if

      if ( .not. present(decimals) ) exit if_eorf_re

      if ( decimals >= 8 ) exit if_eorf_re

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (xre_str(i:i) == POINT) .or. (xre_str(i:i) == COMMA) ) then
          xre_str = xre_str(:i+decimals_)//xre_str(i+9:); exit if_eorf_re
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x%re) /= 0e0_r32 ) then
        e = int(log10(abs(x%re)))
      else
        xre_str = "0.0"; exit if_eorf_re
      end if

      if ( e == 0 ) then
        if ( floor(x%re) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=75) :: xre_str )

      if ( e > 0 ) then
        write(unit=xre_str, fmt="(f0.9)", decimal=decimal) x%re
      else
        write(unit=xre_str, fmt="(f0.70)", decimal=decimal) x%re
      end if

      i = 1; do
        if ( (xre_str(i:i) == POINT) .or. (xre_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (xre_str(1:1) == "-") ) ) then
        xre_str(i+1:75) = xre_str(i:74); xre_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 9 ) then
        xre_str = xre_str(:i); exit if_eorf_re
      end if

      if ( .not. present(decimals) ) then
        xre_str = xre_str(:i+9-e); exit if_eorf_re
      end if

      if ( decimals <= 0 ) then
        xre_str = xre_str(:i); exit if_eorf_re
      end if

      if ( decimals >= 9-e ) then
        xre_str = xre_str(:i+9-e); exit if_eorf_re
      end if

      xre_str = xre_str(:i+decimals); exit if_eorf_re
    end if if_eorf_re
    if_eorf_im: if ( fmt_ == "e" ) then
      if ( x%im == 0e0_r32 ) then
        xim_str = "0.0e+00"; exit if_eorf_im
      end if

      if ( x%im < 0e0_r32 ) then
        xim_str = "000000000000000"
        write(unit=xim_str, fmt="(es15.8e2)", decimal=decimal) x%im
        xim_str(12:12) = "e"
      else
        xim_str = "00000000000000"
        write(unit=xim_str, fmt="(es14.8e2)", decimal=decimal) x%im
        xim_str(11:11) = "e"
      end if

      if ( .not. present(decimals) ) exit if_eorf_im

      if ( decimals >= 8 ) exit if_eorf_im

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (xim_str(i:i) == POINT) .or. (xim_str(i:i) == COMMA) ) then
          xim_str = xim_str(:i+decimals_)//xim_str(i+9:); exit if_eorf_im
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x%im) /= 0e0_r32 ) then
        e = int(log10(abs(x%im)))
      else
        xim_str = "0.0"; exit if_eorf_im
      end if

      if ( e == 0 ) then
        if ( floor(x%im) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=75) :: xim_str )

      if ( e > 0 ) then
        write(unit=xim_str, fmt="(f0.9)", decimal=decimal) x%im
      else
        write(unit=xim_str, fmt="(f0.70)", decimal=decimal) x%im
      end if

      i = 1; do
        if ( (xim_str(i:i) == POINT) .or. (xim_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (xim_str(1:1) == "-") ) ) then
        xim_str(i+1:75) = xim_str(i:74); xim_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 9 ) then
        xim_str = xim_str(:i); exit if_eorf_im
      end if

      if ( .not. present(decimals) ) then
        xim_str = xim_str(:i+9-e); exit if_eorf_im
      end if

      if ( decimals <= 0 ) then
        xim_str = xim_str(:i); exit if_eorf_im
      end if

      if ( decimals >= 9-e ) then
        xim_str = xim_str(:i+9-e); exit if_eorf_im
      end if

      xim_str = xim_str(:i+decimals); exit if_eorf_im
    end if if_eorf_im

    if ( .not. present(im) ) then
      im_ = EMPTY_STR
    else
      im_ = im
    end if

    if ( im_ == EMPTY_STR ) then
      if ( decimal == "POINT" ) then
        into%s = "("//xre_str//COMMA//xim_str//")"; return
      else
        into%s = "("//xre_str//SEMICOLON//xim_str//")"; return
      end if
    end if

    if ( fmt_ == "z" ) then
      into%s = xre_str//"+"//xim_str//im_; return
    end if

    if ( x%im < 0e0_r32 ) then
      into%s = xre_str//xim_str//im_
    else
      into%s = xre_str//"+"//xim_str//im_
    end if
  end procedure cast_c32_to_string

  module procedure cast_r128_to_string
    character(len=1) :: fmt_
    character(len=5) :: decimal
    integer          :: e, decimals_, i

    e=0; decimals_=0; i=0

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into%s = EMPTY_STR; return
      end if
    end if

    if ( fmt_ == "z" ) then
      if ( x /= 0e0_r128 ) then
        into%s = "0x00000000000000000000000000000000"
      else
        into%s = "0x0"; return
      end if

      write(unit=into%s(3:), fmt="(z32)") x

      do i = 3, 34
        if ( (into%s(i:i) >= "A") .and. (into%s(i:i) <= "F") ) into%s(i:i) = achar(iachar(into%s(i:i)) + 32)
      end do

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
        into%s = EMPTY_STR; return
      end if
    end if

    if ( fmt_ == "e" ) then
      if ( x == 0e0_r128 ) then
        into%s = "0.0e+0000"; return
      end if

      if ( x < 0e0_r128 ) then
        into%s = "00000000000000000000000000000000000000000000"
        write(unit=into%s, fmt="(es44.35e4)", decimal=decimal) x
        into%s(39:39) = "e"
      else
        into%s = "0000000000000000000000000000000000000000000"
        write(unit=into%s, fmt="(es43.35e4)", decimal=decimal) x
        into%s(38:38) = "e"
      end if

      if ( .not. present(decimals) ) return

      if ( decimals >= 35 ) return

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (into%s(i:i) == POINT) .or. (into%s(i:i) == COMMA) ) then
          into%s = into%s(:i+decimals_)//into%s(i+36:); return
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x) /= 0e0_r128 ) then
        e = int(log10(abs(x)))
      else
        into%s = "0.0"; return
      end if

      if ( e == 0 ) then
        if ( floor(x) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      if ( allocated(into%s) ) deallocate(into%s)
      allocate( character(len=125) :: into%s )

      if ( e > 0 ) then
        write(unit=into%s, fmt="(f0.36)", decimal=decimal) x
      else
        write(unit=into%s, fmt="(f0.100)", decimal=decimal) x
      end if

      i = 1; do
        if ( (into%s(i:i) == POINT) .or. (into%s(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (into%s(1:1) == "-") ) ) then
        into%s(i+1:125) = into%s(i:124); into%s(i:i) = "0"; i = i + 1
      end if

      if ( i > 36 ) then
        into%s = into%s(:i); return
      end if

      if ( .not. present(decimals) ) then
        into%s = into%s(:i+36-e); return
      end if

      if ( decimals <= 0 ) then
        into%s = into%s(:i); return
      end if

      if ( decimals >= 36-e ) then
        into%s = into%s(:i+36-e); return
      end if

      into%s = into%s(:i+decimals); return
    end if
  end procedure cast_r128_to_string
  module procedure cast_r64_to_string
    character(len=1) :: fmt_
    character(len=5) :: decimal
    integer          :: e, decimals_, i

    e=0; decimals_=0; i=0

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into%s = EMPTY_STR; return
      end if
    end if

    if ( fmt_ == "z" ) then
      if ( x == 0e0_r64 ) then
        into%s = "0x0"; return
      end if

      inline_cast: block
        integer(i64) :: x_int, num, next; character(len=18) :: buffer; integer :: ascii_code
        logical :: negative

        x_int = transfer(source=x, mold=x_int)

        if ( x_int < 0_i64 ) then
          num = (x_int + 1_i64) + largest_i64; negative = .true.; buffer(1:) = "0x0000000000000000"
        else
          num = x_int; negative = .false.
        end if

        i = len(buffer); extract_hex_digits: do
          next = num/16_i64; buffer(i:i) = DIGITS_A(num - 16_i64*next); if ( next == 0_i64 ) exit
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
        into%s = EMPTY_STR; return
      end if
    end if

    if ( fmt_ == "e" ) then
      if ( x == 0e0_r64 ) then
        into%s = "0.0e+000"; return
      end if

      if ( x < 0e0_r64 ) then
        into%s = "0000000000000000000000000"
        write(unit=into%s, fmt="(es25.17e3)", decimal=decimal) x
        into%s(21:21) = "e"
      else
        into%s = "000000000000000000000000"
        write(unit=into%s, fmt="(es24.17e3)", decimal=decimal) x
        into%s(20:20) = "e"
      end if

      if ( .not. present(decimals) ) return

      if ( decimals >= 17 ) return

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (into%s(i:i) == POINT) .or. (into%s(i:i) == COMMA) ) then
          into%s = into%s(:i+decimals_)//into%s(i+18:); return
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x) /= 0e0_r64 ) then
        e = int(log10(abs(x)))
      else
        into%s = "0.0"; return
      end if

      if ( e == 0 ) then
        if ( floor(x) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      if ( allocated(into%s) ) deallocate(into%s)
      allocate( character(len=100) :: into%s )

      if ( e > 0 ) then
        write(unit=into%s, fmt="(f0.18)", decimal=decimal) x
      else
        write(unit=into%s, fmt="(f0.80)", decimal=decimal) x
      end if

      i = 1; do
        if ( (into%s(i:i) == POINT) .or. (into%s(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (into%s(1:1) == "-") ) ) then
        into%s(i+1:100) = into%s(i:99); into%s(i:i) = "0"; i = i + 1
      end if

      if ( i > 18 ) then
        into%s = into%s(:i); return
      end if

      if ( .not. present(decimals) ) then
        into%s = into%s(:i+18-e); return
      end if

      if ( decimals <= 0 ) then
        into%s = into%s(:i); return
      end if

      if ( decimals >= 18-e ) then
        into%s = into%s(:i+18-e); return
      end if

      into%s = into%s(:i+decimals); return
    end if
  end procedure cast_r64_to_string
  module procedure cast_r32_to_string
    character(len=1) :: fmt_
    character(len=5) :: decimal
    integer          :: e, decimals_, i

    e=0; decimals_=0; i=0

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into%s = EMPTY_STR; return
      end if
    end if

    if ( fmt_ == "z" ) then
      if ( x == 0e0_r32 ) then
        into%s = "0x0"; return
      end if

      inline_cast: block
        integer :: x_int, num, next; character(len=10) :: buffer; integer :: ascii_code; logical :: negative

        x_int = transfer(source=x, mold=x_int)

        if ( x_int < 0 ) then
          num = (x_int + 1) + largest_i32; negative = .true.; buffer(1:) = "0x00000000"
        else
          num = x_int; negative = .false.
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
        into%s = EMPTY_STR; return
      end if
    end if

    if ( fmt_ == "e" ) then
      if ( x == 0e0_r32 ) then
        into%s = "0.0e+00"; return
      end if

      if ( x < 0e0_r32 ) then
        into%s = "000000000000000"
        write(unit=into%s, fmt="(es15.8e2)", decimal=decimal) x
        into%s(12:12) = "e"
      else
        into%s = "00000000000000"
        write(unit=into%s, fmt="(es14.8e2)", decimal=decimal) x
        into%s(11:11) = "e"
      end if

      if ( .not. present(decimals) ) return

      if ( decimals >= 8 ) return

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (into%s(i:i) == POINT) .or. (into%s(i:i) == COMMA) ) then
          into%s = into%s(:i+decimals_)//into%s(i+9:); return
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x) /= 0e0_r32 ) then
        e = int(log10(abs(x)))
      else
        into%s = "0.0"; return
      end if

      if ( e == 0 ) then
        if ( floor(x) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      if ( allocated(into%s) ) deallocate(into%s)
      allocate( character(len=75) :: into%s )

      if ( e > 0 ) then
        write(unit=into%s, fmt="(f0.9)", decimal=decimal) x
      else
        write(unit=into%s, fmt="(f0.70)", decimal=decimal) x
      end if

      i = 1; do
        if ( (into%s(i:i) == POINT) .or. (into%s(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (into%s(1:1) == "-") ) ) then
        into%s(i+1:75) = into%s(i:74); into%s(i:i) = "0"; i = i + 1
      end if

      if ( i > 9 ) then
        into%s = into%s(:i); return
      end if

      if ( .not. present(decimals) ) then
        into%s = into%s(:i+9-e); return
      end if

      if ( decimals <= 0 ) then
        into%s = into%s(:i); return
      end if

      if ( decimals >= 9-e ) then
        into%s = into%s(:i+9-e); return
      end if

      into%s = into%s(:i+decimals); return
    end if
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
        into%s = EMPTY_STR; return
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
        into%s = EMPTY_STR; return
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
        into%s = EMPTY_STR; return
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
        into%s = EMPTY_STR; return
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

  module procedure cast_c128_to_char
    character(len=1)              :: fmt_
    character(len=5)              :: decimal
    character(len=:), allocatable :: xre_str, xim_str, im_
    integer                       :: e, decimals_, i

    e=0; decimals_=0; i=0

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into = EMPTY_STR; return
      end if
    end if

    if_z_re: if ( fmt_ == "z" ) then
      if ( x%re /= 0e0_r128 ) then
        xre_str = "0x00000000000000000000000000000000"
      else
        xre_str = "0x0"; exit if_z_re
      end if

      write(unit=xre_str(3:), fmt="(z32)") x%re

      do i = 3, 34
        if ( (xre_str(i:i) >= "A") .and. (xre_str(i:i) <= "F") ) xre_str(i:i) = achar(iachar(xre_str(i:i))+32)
      end do
    end if if_z_re
    if_z_im: if ( fmt_ == "z" ) then
      if ( x%im /= 0e0_r128 ) then
        xim_str = "0x00000000000000000000000000000000"
      else
        xim_str = "0x0"; exit if_z_im
      end if

      write(unit=xim_str(3:), fmt="(z32)") x%im

      do i = 3, 34
        if ( (xim_str(i:i) >= "A") .and. (xim_str(i:i) <= "F") ) xim_str(i:i) = achar(iachar(xim_str(i:i))+32)
      end do
    end if if_z_im

    if ( .not. present(locale) ) then
      decimal = "POINT"
    else
      if ( locale == "US" ) then
        decimal = "POINT"
      else if ( locale == "EU" ) then
        decimal = "COMMA"
      else
        into = EMPTY_STR; return
      end if
    end if

    if_eorf_re: if ( fmt_ == "e" ) then
      if ( x%re == 0e0_r128 ) then
        xre_str = "0.0e+0000"; exit if_eorf_re
      end if

      if ( x%re < 0e0_r128 ) then
        xre_str = "00000000000000000000000000000000000000000000"
        write(unit=xre_str, fmt="(es44.35e4)", decimal=decimal) x%re
        xre_str(39:39) = "e"
      else
        xre_str = "0000000000000000000000000000000000000000000"
        write(unit=xre_str, fmt="(es43.35e4)", decimal=decimal) x%re
        xre_str(38:38) = "e"
      end if

      if ( .not. present(decimals) ) exit if_eorf_re

      if ( decimals >= 35 ) exit if_eorf_re

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (xre_str(i:i) == POINT) .or. (xre_str(i:i) == COMMA) ) then
          xre_str = xre_str(:i+decimals_)//xre_str(i+36:); exit if_eorf_re
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x%re) /= 0e0_r128 ) then
        e = int(log10(abs(x%re)))
      else
        xre_str = "0.0"; exit if_eorf_re
      end if

      if ( e == 0 ) then
        if ( floor(x%re) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=125) :: xre_str )

      if ( e > 0 ) then
        write(unit=xre_str, fmt="(f0.36)", decimal=decimal) x%re
      else
        write(unit=xre_str, fmt="(f0.100)", decimal=decimal) x%re
      end if

      i = 1; do
        if ( (xre_str(i:i) == POINT) .or. (xre_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (xre_str(1:1) == "-") ) ) then
        xre_str(i+1:125) = xre_str(i:124); xre_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 36 ) then
        xre_str = xre_str(:i); exit if_eorf_re
      end if

      if ( .not. present(decimals) ) then
        xre_str = xre_str(:i+36-e); exit if_eorf_re
      end if

      if ( decimals <= 0 ) then
        xre_str = xre_str(:i); exit if_eorf_re
      end if

      if ( decimals >= 36-e ) then
        xre_str = xre_str(:i+36-e); exit if_eorf_re
      end if

      xre_str = xre_str(:i+decimals); exit if_eorf_re
    end if if_eorf_re
    if_eorf_im: if ( fmt_ == "e" ) then
      if ( x%im == 0e0_r128 ) then
        xim_str = "0.0e+0000"; exit if_eorf_im
      end if

      if ( x%im < 0e0_r128 ) then
        xim_str = "00000000000000000000000000000000000000000000"
        write(unit=xim_str, fmt="(es44.35e4)", decimal=decimal) x%im
        xim_str(39:39) = "e"
      else
        xim_str = "0000000000000000000000000000000000000000000"
        write(unit=xim_str, fmt="(es43.35e4)", decimal=decimal) x%im
        xim_str(38:38) = "e"
      end if

      if ( .not. present(decimals) ) exit if_eorf_im

      if ( decimals >= 35 ) exit if_eorf_im

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (xim_str(i:i) == POINT) .or. (xim_str(i:i) == COMMA) ) then
          xim_str = xim_str(:i+decimals_)//xim_str(i+36:); exit if_eorf_im
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x%im) /= 0e0_r128 ) then
        e = int(log10(abs(x%im)))
      else
        xim_str = "0.0"; exit if_eorf_im
      end if

      if ( e == 0 ) then
        if ( floor(x%im) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=125) :: xim_str )

      if ( e > 0 ) then
        write(unit=xim_str, fmt="(f0.36)", decimal=decimal) x%im
      else
        write(unit=xim_str, fmt="(f0.100)", decimal=decimal) x%im
      end if

      i = 1; do
        if ( (xim_str(i:i) == POINT) .or. (xim_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (xim_str(1:1) == "-") ) ) then
        xim_str(i+1:125) = xim_str(i:124); xim_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 36 ) then
        xim_str = xim_str(:i); exit if_eorf_im
      end if

      if ( .not. present(decimals) ) then
        xim_str = xim_str(:i+36-e); exit if_eorf_im
      end if

      if ( decimals <= 0 ) then
        xim_str = xim_str(:i); exit if_eorf_im
      end if

      if ( decimals >= 36-e ) then
        xim_str = xim_str(:i+36-e); exit if_eorf_im
      end if

      xim_str = xim_str(:i+decimals); exit if_eorf_im
    end if if_eorf_im

    if ( .not. present(im) ) then
      im_ = EMPTY_STR
    else
      im_ = im
    end if

    if ( im_ == EMPTY_STR ) then
      if ( decimal == "POINT" ) then
        into = "("//xre_str//COMMA//xim_str//")"; return
      else
        into = "("//xre_str//SEMICOLON//xim_str//")"; return
      end if
    end if

    if ( fmt_ == "z" ) then
      into = xre_str//"+"//xim_str//im_; return
    end if

    if ( x%im < 0e0_r128 ) then
      into = xre_str//xim_str//im_
    else
      into = xre_str//"+"//xim_str//im_
    end if
  end procedure cast_c128_to_char
  module procedure cast_c64_to_char
    character(len=1)              :: fmt_
    character(len=5)              :: decimal
    character(len=:), allocatable :: xre_str, xim_str, im_
    integer                       :: e, decimals_, i

    e=0; decimals_=0; i=0

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into = EMPTY_STR; return
      end if
    end if

    if_z_re: if ( fmt_ == "z" ) then
      if ( x%re == 0e0_r64 ) then
        xre_str = "0x0"; exit if_z_re
      end if

      call cast(transfer(source=x%re, mold=1_i64), into=xre_str, fmt="z"); exit if_z_re
    end if if_z_re
    if_z_im: if ( fmt_ == "z" ) then
      if ( x%im == 0e0_r64 ) then
        xim_str = "0x0"; exit if_z_im
      end if

      call cast(transfer(source=x%im, mold=1_i64), into=xim_str, fmt="z"); exit if_z_im
    end if if_z_im

    if ( .not. present(locale) ) then
      decimal = "POINT"
    else
      if ( locale == "US" ) then
        decimal = "POINT"
      else if ( locale == "EU" ) then
        decimal = "COMMA"
      else
        into = EMPTY_STR; return
      end if
    end if

    if_eorf_re: if ( fmt_ == "e" ) then
      if ( x%re == 0e0_r64 ) then
        xre_str = "0.0e+000"; exit if_eorf_re
      end if

      if ( x%re < 0e0_r64 ) then
        xre_str = "0000000000000000000000000"
        write(unit=xre_str, fmt="(es25.17e3)", decimal=decimal) x%re
        xre_str(21:21) = "e"
      else
        xre_str = "000000000000000000000000"
        write(unit=xre_str, fmt="(es24.17e3)", decimal=decimal) x%re
        xre_str(20:20) = "e"
      end if

      if ( .not. present(decimals) ) exit if_eorf_re

      if ( decimals >= 17 ) exit if_eorf_re

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (xre_str(i:i) == POINT) .or. (xre_str(i:i) == COMMA) ) then
          xre_str = xre_str(:i+decimals_)//xre_str(i+18:); exit if_eorf_re
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x%re) /= 0e0_r64 ) then
        e = int(log10(abs(x%re)))
      else
        xre_str = "0.0"; return
      end if

      if ( e == 0 ) then
        if ( floor(x%re) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=100) :: xre_str )

      if ( e > 0 ) then
        write(unit=xre_str, fmt="(f0.18)", decimal=decimal) x%re
      else
        write(unit=xre_str, fmt="(f0.80)", decimal=decimal) x%re
      end if

      i = 1; do
        if ( (xre_str(i:i) == POINT) .or. (xre_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (xre_str(1:1) == "-") ) ) then
        xre_str(i+1:100) = xre_str(i:99); xre_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 18 ) then
        xre_str = xre_str(:i); exit if_eorf_re
      end if

      if ( .not. present(decimals) ) then
        xre_str = xre_str(:i+18-e); exit if_eorf_re
      end if

      if ( decimals <= 0 ) then
        xre_str = xre_str(:i); exit if_eorf_re
      end if

      if ( decimals >= 18-e ) then
        xre_str = xre_str(:i+18-e); exit if_eorf_re
      end if

      xre_str = xre_str(:i+decimals); exit if_eorf_re
    end if if_eorf_re
    if_eorf_im: if ( fmt_ == "e" ) then
      if ( x%im == 0e0_r64 ) then
        xim_str = "0.0e+000"; exit if_eorf_im
      end if

      if ( x%im < 0e0_r64 ) then
        xim_str = "0000000000000000000000000"
        write(unit=xim_str, fmt="(es25.17e3)", decimal=decimal) x%im
        xim_str(21:21) = "e"
      else
        xim_str = "000000000000000000000000"
        write(unit=xim_str, fmt="(es24.17e3)", decimal=decimal) x%im
        xim_str(20:20) = "e"
      end if

      if ( .not. present(decimals) ) exit if_eorf_im

      if ( decimals >= 17 ) exit if_eorf_im

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (xim_str(i:i) == POINT) .or. (xim_str(i:i) == COMMA) ) then
          xim_str = xim_str(:i+decimals_)//xim_str(i+18:); exit if_eorf_im
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x%im) /= 0e0_r64 ) then
        e = int(log10(abs(x%im)))
      else
        xim_str = "0.0"; return
      end if

      if ( e == 0 ) then
        if ( floor(x%im) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=100) :: xim_str )

      if ( e > 0 ) then
        write(unit=xim_str, fmt="(f0.18)", decimal=decimal) x%im
      else
        write(unit=xim_str, fmt="(f0.80)", decimal=decimal) x%im
      end if

      i = 1; do
        if ( (xim_str(i:i) == POINT) .or. (xim_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (xim_str(1:1) == "-") ) ) then
        xim_str(i+1:100) = xim_str(i:99); xim_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 18 ) then
        xim_str = xim_str(:i); exit if_eorf_im
      end if

      if ( .not. present(decimals) ) then
        xim_str = xim_str(:i+18-e); exit if_eorf_im
      end if

      if ( decimals <= 0 ) then
        xim_str = xim_str(:i); exit if_eorf_im
      end if

      if ( decimals >= 18-e ) then
        xim_str = xim_str(:i+18-e); exit if_eorf_im
      end if

      xim_str = xim_str(:i+decimals); exit if_eorf_im
    end if if_eorf_im

    if ( .not. present(im) ) then
      im_ = EMPTY_STR
    else
      im_ = im
    end if

    if ( im_ == EMPTY_STR ) then
      if ( decimal == "POINT" ) then
        into = "("//xre_str//COMMA//xim_str//")"; return
      else
        into = "("//xre_str//SEMICOLON//xim_str//")"; return
      end if
    end if

    if ( fmt_ == "z" ) then
      into = xre_str//"+"//xim_str//im_; return
    end if

    if ( x%im < 0e0_r64 ) then
      into = xre_str//xim_str//im_
    else
      into = xre_str//"+"//xim_str//im_
    end if
  end procedure cast_c64_to_char
  module procedure cast_c32_to_char
    character(len=1)              :: fmt_
    character(len=5)              :: decimal
    character(len=:), allocatable :: xre_str, xim_str, im_
    integer                       :: e, decimals_, i

    e=0; decimals_=0; i=0

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into = EMPTY_STR; return
      end if
    end if

    if_z_re: if ( fmt_ == "z" ) then
      if ( x%re == 0e0_r32 ) then
        xre_str = "0x0"; exit if_z_re
      end if

      call cast(transfer(source=x%re, mold=1_i32), into=xre_str, fmt="z"); exit if_z_re
    end if if_z_re
    if_z_im: if ( fmt_ == "z" ) then
      if ( x%im == 0e0_r32 ) then
        xim_str = "0x0"; exit if_z_im
      end if

      call cast(transfer(source=x%im, mold=1_i32), into=xim_str, fmt="z"); exit if_z_im
    end if if_z_im

    if ( .not. present(locale) ) then
      decimal = "POINT"
    else
      if ( locale == "US" ) then
        decimal = "POINT"
      else if ( locale == "EU" ) then
        decimal = "COMMA"
      else
        into = EMPTY_STR; return
      end if
    end if

    if_eorf_re: if ( fmt_ == "e" ) then
      if ( x%re == 0e0_r32 ) then
        xre_str = "0.0e+00"; exit if_eorf_re
      end if

      if ( x%re < 0e0_r32 ) then
        xre_str = "000000000000000"
        write(unit=xre_str, fmt="(es15.8e2)", decimal=decimal) x%re
        xre_str(12:12) = "e"
      else
        xre_str = "00000000000000"
        write(unit=xre_str, fmt="(es14.8e2)", decimal=decimal) x%re
        xre_str(11:11) = "e"
      end if

      if ( .not. present(decimals) ) exit if_eorf_re

      if ( decimals >= 8 ) exit if_eorf_re

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (xre_str(i:i) == POINT) .or. (xre_str(i:i) == COMMA) ) then
          xre_str = xre_str(:i+decimals_)//xre_str(i+9:); exit if_eorf_re
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x%re) /= 0e0_r32 ) then
        e = int(log10(abs(x%re)))
      else
        xre_str = "0.0"; exit if_eorf_re
      end if

      if ( e == 0 ) then
        if ( floor(x%re) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=75) :: xre_str )

      if ( e > 0 ) then
        write(unit=xre_str, fmt="(f0.9)", decimal=decimal) x%re
      else
        write(unit=xre_str, fmt="(f0.70)", decimal=decimal) x%re
      end if

      i = 1; do
        if ( (xre_str(i:i) == POINT) .or. (xre_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (xre_str(1:1) == "-") ) ) then
        xre_str(i+1:75) = xre_str(i:74); xre_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 9 ) then
        xre_str = xre_str(:i); exit if_eorf_re
      end if

      if ( .not. present(decimals) ) then
        xre_str = xre_str(:i+9-e); exit if_eorf_re
      end if

      if ( decimals <= 0 ) then
        xre_str = xre_str(:i); exit if_eorf_re
      end if

      if ( decimals >= 9-e ) then
        xre_str = xre_str(:i+9-e); exit if_eorf_re
      end if

      xre_str = xre_str(:i+decimals); exit if_eorf_re
    end if if_eorf_re
    if_eorf_im: if ( fmt_ == "e" ) then
      if ( x%im == 0e0_r32 ) then
        xim_str = "0.0e+00"; exit if_eorf_im
      end if

      if ( x%im < 0e0_r32 ) then
        xim_str = "000000000000000"
        write(unit=xim_str, fmt="(es15.8e2)", decimal=decimal) x%im
        xim_str(12:12) = "e"
      else
        xim_str = "00000000000000"
        write(unit=xim_str, fmt="(es14.8e2)", decimal=decimal) x%im
        xim_str(11:11) = "e"
      end if

      if ( .not. present(decimals) ) exit if_eorf_im

      if ( decimals >= 8 ) exit if_eorf_im

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (xim_str(i:i) == POINT) .or. (xim_str(i:i) == COMMA) ) then
          xim_str = xim_str(:i+decimals_)//xim_str(i+9:); exit if_eorf_im
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x%im) /= 0e0_r32 ) then
        e = int(log10(abs(x%im)))
      else
        xim_str = "0.0"; exit if_eorf_im
      end if

      if ( e == 0 ) then
        if ( floor(x%im) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      allocate( character(len=75) :: xim_str )

      if ( e > 0 ) then
        write(unit=xim_str, fmt="(f0.9)", decimal=decimal) x%im
      else
        write(unit=xim_str, fmt="(f0.70)", decimal=decimal) x%im
      end if

      i = 1; do
        if ( (xim_str(i:i) == POINT) .or. (xim_str(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (xim_str(1:1) == "-") ) ) then
        xim_str(i+1:75) = xim_str(i:74); xim_str(i:i) = "0"; i = i + 1
      end if

      if ( i > 9 ) then
        xim_str = xim_str(:i); exit if_eorf_im
      end if

      if ( .not. present(decimals) ) then
        xim_str = xim_str(:i+9-e); exit if_eorf_im
      end if

      if ( decimals <= 0 ) then
        xim_str = xim_str(:i); exit if_eorf_im
      end if

      if ( decimals >= 9-e ) then
        xim_str = xim_str(:i+9-e); exit if_eorf_im
      end if

      xim_str = xim_str(:i+decimals); exit if_eorf_im
    end if if_eorf_im

    if ( .not. present(im) ) then
      im_ = EMPTY_STR
    else
      im_ = im
    end if

    if ( im_ == EMPTY_STR ) then
      if ( decimal == "POINT" ) then
        into = "("//xre_str//COMMA//xim_str//")"; return
      else
        into = "("//xre_str//SEMICOLON//xim_str//")"; return
      end if
    end if

    if ( fmt_ == "z" ) then
      into = xre_str//"+"//xim_str//im_; return
    end if

    if ( x%im < 0e0_r32 ) then
      into = xre_str//xim_str//im_
    else
      into = xre_str//"+"//xim_str//im_
    end if
  end procedure cast_c32_to_char

  module procedure cast_r128_to_char
    character(len=1) :: fmt_
    character(len=5) :: decimal
    integer          :: e, decimals_, i

    e=0; decimals_=0; i=0

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into = EMPTY_STR; return
      end if
    end if

    if ( fmt_ == "z" ) then
      if ( x /= 0e0_r128 ) then
        into = "0x00000000000000000000000000000000"
      else
        into = "0x0"; return
      end if

      write(unit=into(3:), fmt="(z32)") x

      do i = 3, 34
        if ( (into(i:i) >= "A") .and. (into(i:i) <= "F") ) into(i:i) = achar(iachar(into(i:i)) + 32)
      end do

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
        into = EMPTY_STR; return
      end if
    end if

    if ( fmt_ == "e" ) then
      if ( x == 0e0_r128 ) then
        into = "0.0e+0000"; return
      end if

      if ( x < 0e0_r128 ) then
        into = "00000000000000000000000000000000000000000000"
        write(unit=into, fmt="(es44.35e4)", decimal=decimal) x
        into(39:39) = "e"
      else
        into = "0000000000000000000000000000000000000000000"
        write(unit=into, fmt="(es43.35e4)", decimal=decimal) x
        into(38:38) = "e"
      end if

      if ( .not. present(decimals) ) return

      if ( decimals >= 35 ) return

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (into(i:i) == POINT) .or. (into(i:i) == COMMA) ) then
          into = into(:i+decimals_)//into(i+36:); return
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x) /= 0e0_r128 ) then
        e = int(log10(abs(x)))
      else
        into = "0.0"; return
      end if

      if ( e == 0 ) then
        if ( floor(x) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      if ( allocated(into) ) deallocate(into)
      allocate( character(len=125) :: into )

      if ( e > 0 ) then
        write(unit=into, fmt="(f0.36)", decimal=decimal) x
      else
        write(unit=into, fmt="(f0.100)", decimal=decimal) x
      end if

      i = 1; do
        if ( (into(i:i) == POINT) .or. (into(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (into(1:1) == "-") ) ) then
        into(i+1:125) = into(i:124); into(i:i) = "0"; i = i + 1
      end if

      if ( i > 36 ) then
        into = into(:i); return
      end if

      if ( .not. present(decimals) ) then
        into = into(:i+36-e); return
      end if

      if ( decimals <= 0 ) then
        into = into(:i); return
      end if

      if ( decimals >= 36-e ) then
        into = into(:i+36-e); return
      end if

      into = into(:i+decimals); return
    end if
  end procedure cast_r128_to_char
  module procedure cast_r64_to_char
    character(len=1) :: fmt_
    character(len=5) :: decimal
    integer          :: e, decimals_, i

    e=0; decimals_=0; i=0

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into = EMPTY_STR; return
      end if
    end if

    if ( fmt_ == "z" ) then
      if ( x == 0e0_r64 ) then
        into = "0x0"; return
      end if

      inline_cast: block
        integer(i64) :: x_int, num, next; character(len=18) :: buffer; integer :: ascii_code
        logical :: negative

        x_int = transfer(source=x, mold=x_int)

        if ( x_int < 0_i64 ) then
          num = (x_int + 1_i64) + largest_i64; negative = .true.; buffer(1:) = "0x0000000000000000"
        else
          num = x_int; negative = .false.
        end if

        i = len(buffer); extract_hex_digits: do
          next = num/16_i64; buffer(i:i) = DIGITS_A(num - 16_i64*next); if ( next == 0_i64 ) exit
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
        into = EMPTY_STR; return
      end if
    end if

    if ( fmt_ == "e" ) then
      if ( x == 0e0_r64 ) then
        into = "0.0e+000"; return
      end if

      if ( x < 0e0_r64 ) then
        into = "0000000000000000000000000"
        write(unit=into, fmt="(es25.17e3)", decimal=decimal) x
        into(21:21) = "e"
      else
        into = "000000000000000000000000"
        write(unit=into, fmt="(es24.17e3)", decimal=decimal) x
        into(20:20) = "e"
      end if

      if ( .not. present(decimals) ) return

      if ( decimals >= 17 ) return

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (into(i:i) == POINT) .or. (into(i:i) == COMMA) ) then
          into = into(:i+decimals_)//into(i+18:); return
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x) /= 0e0_r64 ) then
        e = int(log10(abs(x)))
      else
        into = "0.0"; return
      end if

      if ( e == 0 ) then
        if ( floor(x) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      if ( allocated(into) ) deallocate(into)
      allocate( character(len=100) :: into )

      if ( e > 0 ) then
        write(unit=into, fmt="(f0.18)", decimal=decimal) x
      else
        write(unit=into, fmt="(f0.80)", decimal=decimal) x
      end if

      i = 1; do
        if ( (into(i:i) == POINT) .or. (into(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (into(1:1) == "-") ) ) then
        into(i+1:100) = into(i:99); into(i:i) = "0"; i = i + 1
      end if

      if ( i > 18 ) then
        into = into(:i); return
      end if

      if ( .not. present(decimals) ) then
        into = into(:i+18-e); return
      end if

      if ( decimals <= 0 ) then
        into = into(:i); return
      end if

      if ( decimals >= 18-e ) then
        into = into(:i+18-e); return
      end if

      into = into(:i+decimals); return
    end if
  end procedure cast_r64_to_char
  module procedure cast_r32_to_char
    character(len=1) :: fmt_
    character(len=5) :: decimal
    integer          :: e, decimals_, i

    e=0; decimals_=0; i=0

    if ( .not. present(fmt) ) then
      fmt_ = "e"
    else
      if ( any(REAL_FMTS == fmt) ) then
        fmt_ = fmt
      else
        into = EMPTY_STR; return
      end if
    end if

    if ( fmt_ == "z" ) then
      if ( x == 0e0_r32 ) then
        into = "0x0"; return
      end if

      inline_cast: block
        integer :: x_int, num, next; character(len=10) :: buffer; integer :: ascii_code; logical :: negative

        x_int = transfer(source=x, mold=x_int)

        if ( x_int < 0 ) then
          num = (x_int + 1) + largest_i32; negative = .true.; buffer(1:) = "0x00000000"
        else
          num = x_int; negative = .false.
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
        into = EMPTY_STR; return
      end if
    end if

    if ( fmt_ == "e" ) then
      if ( x == 0e0_r32 ) then
        into = "0.0e+00"; return
      end if

      if ( x < 0e0_r32 ) then
        into = "000000000000000"
        write(unit=into, fmt="(es15.8e2)", decimal=decimal) x
        into(12:12) = "e"
      else
        into = "00000000000000"
        write(unit=into, fmt="(es14.8e2)", decimal=decimal) x
        into(11:11) = "e"
      end if

      if ( .not. present(decimals) ) return

      if ( decimals >= 8 ) return

      if ( decimals < 0 ) then
        decimals_ = 0
      else
        decimals_ = decimals
      end if

      i = 1; do
        if ( (into(i:i) == POINT) .or. (into(i:i) == COMMA) ) then
          into = into(:i+decimals_)//into(i+9:); return
        end if
        i = i + 1; cycle
      end do
    else if ( fmt_ == "f" ) then
      if ( abs(x) /= 0e0_r32 ) then
        e = int(log10(abs(x)))
      else
        into = "0.0"; return
      end if

      if ( e == 0 ) then
        if ( floor(x) > 0 ) e = 1 + e
      else if ( e > 0 ) then
        e = 1 + e
      end if

      if ( allocated(into) ) deallocate(into)
      allocate( character(len=75) :: into )

      if ( e > 0 ) then
        write(unit=into, fmt="(f0.9)", decimal=decimal) x
      else
        write(unit=into, fmt="(f0.70)", decimal=decimal) x
      end if

      i = 1; do
        if ( (into(i:i) == POINT) .or. (into(i:i) == COMMA) ) exit
        i = i + 1; cycle
      end do

      if ( (i == 1) .or. ( (i == 2) .and. (into(1:1) == "-") ) ) then
        into(i+1:75) = into(i:74); into(i:i) = "0"; i = i + 1
      end if

      if ( i > 9 ) then
        into = into(:i); return
      end if

      if ( .not. present(decimals) ) then
        into = into(:i+9-e); return
      end if

      if ( decimals <= 0 ) then
        into = into(:i); return
      end if

      if ( decimals >= 9-e ) then
        into = into(:i+9-e); return
      end if

      into = into(:i+decimals); return
    end if
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
        into = EMPTY_STR; return
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
        into = EMPTY_STR; return
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
        into = EMPTY_STR; return
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
        into = EMPTY_STR; return
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
end submodule internal_io
