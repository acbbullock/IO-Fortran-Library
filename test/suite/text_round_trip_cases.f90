! Generated from fypp/test/suite/text_round_trip_cases.fypp by fypp/generate.sh. Edit that file, not this one.
#include "iofl_config.inc"
module text_round_trip_cases
  !---------------------------------------------------------------------------------------------------------------------
  !! Round trips through text for every kind: `str` then `cast`, `cast` into a character variable, and `String` arrays,
  !! for every locale, format and imaginary unit, on edge values and seeded random values. Values must come back
  !! identical, the sign of zero included; NaN matches NaN.
  !---------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: r128=>real128, r64=>real64, r32=>real32, &
                                           i64=>int64, i32=>int32, i16=>int16, i8=>int8
  use, intrinsic :: ieee_arithmetic, only: ieee_copy_sign
  use io_fortran_lib, only: String, str, cast, operator(==)
  use test_support,   only: test_suite, fail, failure_count, expect_equal, expect_identical
  use test_values,    only: reset_random, test_values_of
  implicit none
  private

  public :: register_text_round_trip

  character(len=2), parameter :: LOCALES(*)   = [ "US", "EU" ]
  character(len=1), parameter :: REAL_FMTS(*) = [ "e", "f", "z" ]
  character(len=1), parameter :: INT_FMTS(*)  = [ "i", "z" ]
  character(len=3), parameter :: IMS(*)       = [ "   ", "*1i", "j  " ]

  integer, parameter :: N_SCALAR = 2000                                       ! Random values for the scalar cases
  integer, parameter :: N_ARRAY  = 500                                        ! Random values for the array cases

contains

  subroutine register_text_round_trip(suite)
    type(test_suite), intent(inout) :: suite

    call suite%group("text_round_trip")
    call suite%add("str_cast_i64", str_cast_i64)
    call suite%add("string_array_i64", string_array_i64)
    call suite%add("str_cast_i32", str_cast_i32)
    call suite%add("string_array_i32", string_array_i32)
    call suite%add("str_cast_i16", str_cast_i16)
    call suite%add("string_array_i16", string_array_i16)
    call suite%add("str_cast_i8", str_cast_i8)
    call suite%add("string_array_i8", string_array_i8)
#ifdef IOFL_HAS_R128
    call suite%add("str_cast_r128", str_cast_r128)
    call suite%add("string_array_r128", string_array_r128)
    call suite%add("negative_zero_r128", negative_zero_r128)
#endif
    call suite%add("str_cast_r64", str_cast_r64)
    call suite%add("string_array_r64", string_array_r64)
    call suite%add("negative_zero_r64", negative_zero_r64)
    call suite%add("str_cast_r32", str_cast_r32)
    call suite%add("string_array_r32", string_array_r32)
    call suite%add("negative_zero_r32", negative_zero_r32)
#ifdef IOFL_HAS_R128
    call suite%add("str_cast_c128", str_cast_c128)
    call suite%add("string_array_c128", string_array_c128)
    call suite%add("negative_zero_c128", negative_zero_c128)
#endif
    call suite%add("str_cast_c64", str_cast_c64)
    call suite%add("string_array_c64", string_array_c64)
    call suite%add("negative_zero_c64", negative_zero_c64)
    call suite%add("str_cast_c32", str_cast_c32)
    call suite%add("string_array_c32", string_array_c32)
    call suite%add("negative_zero_c32", negative_zero_c32)
  end subroutine register_text_round_trip

  function context(i, fmt, locale, im) result(text)
    integer,          intent(in)           :: i
    character(len=*), intent(in)           :: fmt
    character(len=*), intent(in), optional :: locale, im
    character(len=:), allocatable          :: text

    text = " (value "//str(i)//', fmt="'//fmt//'"'
    if ( present(locale) ) text = text//', locale="'//locale//'"'
    if ( present(im) )     text = text//', im="'//trim(im)//'"'
    text = text//")"
  end function context

  subroutine str_cast_i64()
    !! `str` then `cast`, and `cast` into a character variable, for each format.
    integer(i64), allocatable     :: x(:)
    integer(i64)                  :: y
    character(len=:), allocatable :: text, other
    integer                       :: f, i, st, before

    call reset_random()
    x = test_values_of(0_i64, N_SCALAR)

    do f = 1, size(INT_FMTS)
      before = failure_count()
      do i = 1, size(x)
        text = str(x(i), fmt=INT_FMTS(f))
        call cast(text, into=y, fmt=INT_FMTS(f), stat=st)
        if ( st /= 0 ) call fail("cast should read back "//text//context(i, INT_FMTS(f)))
        call expect_equal(y, x(i), "str then cast"//context(i, INT_FMTS(f)))
        call cast(x(i), into=other, fmt=INT_FMTS(f))
        call expect_equal(other, text, "cast into character should agree with str"//context(i, INT_FMTS(f)))
        if ( failure_count() > before ) exit
      end do
    end do
  end subroutine str_cast_i64

  subroutine string_array_i64()
    !! `String` on an array then `cast` back, `cast` into a `String` array, and agreement with `str`, for each format.
    integer(i64), allocatable :: x(:), y(:)
    type(String), allocatable :: strings(:), other(:)
    integer                   :: f, i, before

    call reset_random()
    x = test_values_of(0_i64, N_ARRAY)
    allocate( y(size(x)), other(size(x)) )

    do f = 1, size(INT_FMTS)
      strings = String(x, fmt=INT_FMTS(f))
      call cast(strings, into=y, fmt=INT_FMTS(f))
      call expect_equal(y, x, "String then cast"//context(0, INT_FMTS(f)))
      call cast(x, into=other, fmt=INT_FMTS(f))
      call expect_equal(other, strings, "cast into String should agree with String"//context(0, INT_FMTS(f)))

      before = failure_count()
      do i = 1, size(x)
        call expect_equal(strings(i), str(x(i), fmt=INT_FMTS(f)), &
                          "String should agree with str"//context(i, INT_FMTS(f)))
        if ( failure_count() > before ) exit
      end do
    end do
  end subroutine string_array_i64

  subroutine str_cast_i32()
    !! `str` then `cast`, and `cast` into a character variable, for each format.
    integer(i32), allocatable     :: x(:)
    integer(i32)                  :: y
    character(len=:), allocatable :: text, other
    integer                       :: f, i, st, before

    call reset_random()
    x = test_values_of(0_i32, N_SCALAR)

    do f = 1, size(INT_FMTS)
      before = failure_count()
      do i = 1, size(x)
        text = str(x(i), fmt=INT_FMTS(f))
        call cast(text, into=y, fmt=INT_FMTS(f), stat=st)
        if ( st /= 0 ) call fail("cast should read back "//text//context(i, INT_FMTS(f)))
        call expect_equal(y, x(i), "str then cast"//context(i, INT_FMTS(f)))
        call cast(x(i), into=other, fmt=INT_FMTS(f))
        call expect_equal(other, text, "cast into character should agree with str"//context(i, INT_FMTS(f)))
        if ( failure_count() > before ) exit
      end do
    end do
  end subroutine str_cast_i32

  subroutine string_array_i32()
    !! `String` on an array then `cast` back, `cast` into a `String` array, and agreement with `str`, for each format.
    integer(i32), allocatable :: x(:), y(:)
    type(String), allocatable :: strings(:), other(:)
    integer                   :: f, i, before

    call reset_random()
    x = test_values_of(0_i32, N_ARRAY)
    allocate( y(size(x)), other(size(x)) )

    do f = 1, size(INT_FMTS)
      strings = String(x, fmt=INT_FMTS(f))
      call cast(strings, into=y, fmt=INT_FMTS(f))
      call expect_equal(y, x, "String then cast"//context(0, INT_FMTS(f)))
      call cast(x, into=other, fmt=INT_FMTS(f))
      call expect_equal(other, strings, "cast into String should agree with String"//context(0, INT_FMTS(f)))

      before = failure_count()
      do i = 1, size(x)
        call expect_equal(strings(i), str(x(i), fmt=INT_FMTS(f)), &
                          "String should agree with str"//context(i, INT_FMTS(f)))
        if ( failure_count() > before ) exit
      end do
    end do
  end subroutine string_array_i32

  subroutine str_cast_i16()
    !! `str` then `cast`, and `cast` into a character variable, for each format.
    integer(i16), allocatable     :: x(:)
    integer(i16)                  :: y
    character(len=:), allocatable :: text, other
    integer                       :: f, i, st, before

    call reset_random()
    x = test_values_of(0_i16, N_SCALAR)

    do f = 1, size(INT_FMTS)
      before = failure_count()
      do i = 1, size(x)
        text = str(x(i), fmt=INT_FMTS(f))
        call cast(text, into=y, fmt=INT_FMTS(f), stat=st)
        if ( st /= 0 ) call fail("cast should read back "//text//context(i, INT_FMTS(f)))
        call expect_equal(y, x(i), "str then cast"//context(i, INT_FMTS(f)))
        call cast(x(i), into=other, fmt=INT_FMTS(f))
        call expect_equal(other, text, "cast into character should agree with str"//context(i, INT_FMTS(f)))
        if ( failure_count() > before ) exit
      end do
    end do
  end subroutine str_cast_i16

  subroutine string_array_i16()
    !! `String` on an array then `cast` back, `cast` into a `String` array, and agreement with `str`, for each format.
    integer(i16), allocatable :: x(:), y(:)
    type(String), allocatable :: strings(:), other(:)
    integer                   :: f, i, before

    call reset_random()
    x = test_values_of(0_i16, N_ARRAY)
    allocate( y(size(x)), other(size(x)) )

    do f = 1, size(INT_FMTS)
      strings = String(x, fmt=INT_FMTS(f))
      call cast(strings, into=y, fmt=INT_FMTS(f))
      call expect_equal(y, x, "String then cast"//context(0, INT_FMTS(f)))
      call cast(x, into=other, fmt=INT_FMTS(f))
      call expect_equal(other, strings, "cast into String should agree with String"//context(0, INT_FMTS(f)))

      before = failure_count()
      do i = 1, size(x)
        call expect_equal(strings(i), str(x(i), fmt=INT_FMTS(f)), &
                          "String should agree with str"//context(i, INT_FMTS(f)))
        if ( failure_count() > before ) exit
      end do
    end do
  end subroutine string_array_i16

  subroutine str_cast_i8()
    !! `str` then `cast`, and `cast` into a character variable, for each format.
    integer(i8), allocatable     :: x(:)
    integer(i8)                  :: y
    character(len=:), allocatable :: text, other
    integer                       :: f, i, st, before

    call reset_random()
    x = test_values_of(0_i8, N_SCALAR)

    do f = 1, size(INT_FMTS)
      before = failure_count()
      do i = 1, size(x)
        text = str(x(i), fmt=INT_FMTS(f))
        call cast(text, into=y, fmt=INT_FMTS(f), stat=st)
        if ( st /= 0 ) call fail("cast should read back "//text//context(i, INT_FMTS(f)))
        call expect_equal(y, x(i), "str then cast"//context(i, INT_FMTS(f)))
        call cast(x(i), into=other, fmt=INT_FMTS(f))
        call expect_equal(other, text, "cast into character should agree with str"//context(i, INT_FMTS(f)))
        if ( failure_count() > before ) exit
      end do
    end do
  end subroutine str_cast_i8

  subroutine string_array_i8()
    !! `String` on an array then `cast` back, `cast` into a `String` array, and agreement with `str`, for each format.
    integer(i8), allocatable :: x(:), y(:)
    type(String), allocatable :: strings(:), other(:)
    integer                   :: f, i, before

    call reset_random()
    x = test_values_of(0_i8, N_ARRAY)
    allocate( y(size(x)), other(size(x)) )

    do f = 1, size(INT_FMTS)
      strings = String(x, fmt=INT_FMTS(f))
      call cast(strings, into=y, fmt=INT_FMTS(f))
      call expect_equal(y, x, "String then cast"//context(0, INT_FMTS(f)))
      call cast(x, into=other, fmt=INT_FMTS(f))
      call expect_equal(other, strings, "cast into String should agree with String"//context(0, INT_FMTS(f)))

      before = failure_count()
      do i = 1, size(x)
        call expect_equal(strings(i), str(x(i), fmt=INT_FMTS(f)), &
                          "String should agree with str"//context(i, INT_FMTS(f)))
        if ( failure_count() > before ) exit
      end do
    end do
  end subroutine string_array_i8

#ifdef IOFL_HAS_R128
  subroutine str_cast_r128()
    !! `str` then `cast`, and `cast` into a character variable, for each locale and format.
    real(r128), allocatable        :: x(:)
    real(r128)                     :: y
    character(len=:), allocatable :: text, other
    integer                       :: l, f, i, st, before

    call reset_random()
    x = test_values_of(0.0_r128, N_SCALAR)

    do l = 1, size(LOCALES)
      do f = 1, size(REAL_FMTS)
        before = failure_count()
        do i = 1, size(x)
          text = str(x(i), locale=LOCALES(l), fmt=REAL_FMTS(f))
          call cast(text, into=y, locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st)
          if ( st /= 0 ) call fail("cast should read back "//text//context(i, REAL_FMTS(f), LOCALES(l)))
          call expect_identical(y, x(i), "str then cast"//context(i, REAL_FMTS(f), LOCALES(l)))
          call cast(x(i), into=other, locale=LOCALES(l), fmt=REAL_FMTS(f))
          call expect_equal(other, text, "cast into character should agree with str"// &
                            context(i, REAL_FMTS(f), LOCALES(l)))
          if ( failure_count() > before ) exit
        end do
      end do
    end do
  end subroutine str_cast_r128

  subroutine string_array_r128()
    !! `String` on an array then `cast` back, `cast` into a `String` array, and agreement with `str`, for each locale
    !! and format.
    real(r128), allocatable    :: x(:), y(:)
    type(String), allocatable :: strings(:), other(:)
    integer                   :: l, f, i, before

    call reset_random()
    x = test_values_of(0.0_r128, N_ARRAY)
    allocate( y(size(x)), other(size(x)) )

    do l = 1, size(LOCALES)
      do f = 1, size(REAL_FMTS)
        strings = String(x, locale=LOCALES(l), fmt=REAL_FMTS(f))
        call cast(strings, into=y, locale=LOCALES(l), fmt=REAL_FMTS(f))
        call expect_identical(y, x, "String then cast"//context(0, REAL_FMTS(f), LOCALES(l)))
        call cast(x, into=other, locale=LOCALES(l), fmt=REAL_FMTS(f))
        call expect_equal(other, strings, "cast into String should agree with String"// &
                          context(0, REAL_FMTS(f), LOCALES(l)))

        before = failure_count()
        do i = 1, size(x)
          call expect_equal(strings(i), str(x(i), locale=LOCALES(l), fmt=REAL_FMTS(f)), &
                            "String should agree with str"//context(i, REAL_FMTS(f), LOCALES(l)))
          if ( failure_count() > before ) exit
        end do
      end do
    end do
  end subroutine string_array_r128

  subroutine negative_zero_r128()
    !! Negative zero keeps its sign through text in every format and locale (#60).
    real(r128) :: x, y
    integer   :: l, f

    x = ieee_copy_sign(0.0_r128, -1.0_r128)

    do l = 1, size(LOCALES)
      do f = 1, size(REAL_FMTS)
        call cast(str(x, locale=LOCALES(l), fmt=REAL_FMTS(f)), into=y, locale=LOCALES(l), fmt=REAL_FMTS(f))
        call expect_identical(y, x, "Negative zero"//context(0, REAL_FMTS(f), LOCALES(l)))
      end do
    end do
  end subroutine negative_zero_r128
#endif

  subroutine str_cast_r64()
    !! `str` then `cast`, and `cast` into a character variable, for each locale and format.
    real(r64), allocatable        :: x(:)
    real(r64)                     :: y
    character(len=:), allocatable :: text, other
    integer                       :: l, f, i, st, before

    call reset_random()
    x = test_values_of(0.0_r64, N_SCALAR)

    do l = 1, size(LOCALES)
      do f = 1, size(REAL_FMTS)
        before = failure_count()
        do i = 1, size(x)
          text = str(x(i), locale=LOCALES(l), fmt=REAL_FMTS(f))
          call cast(text, into=y, locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st)
          if ( st /= 0 ) call fail("cast should read back "//text//context(i, REAL_FMTS(f), LOCALES(l)))
          call expect_identical(y, x(i), "str then cast"//context(i, REAL_FMTS(f), LOCALES(l)))
          call cast(x(i), into=other, locale=LOCALES(l), fmt=REAL_FMTS(f))
          call expect_equal(other, text, "cast into character should agree with str"// &
                            context(i, REAL_FMTS(f), LOCALES(l)))
          if ( failure_count() > before ) exit
        end do
      end do
    end do
  end subroutine str_cast_r64

  subroutine string_array_r64()
    !! `String` on an array then `cast` back, `cast` into a `String` array, and agreement with `str`, for each locale
    !! and format.
    real(r64), allocatable    :: x(:), y(:)
    type(String), allocatable :: strings(:), other(:)
    integer                   :: l, f, i, before

    call reset_random()
    x = test_values_of(0.0_r64, N_ARRAY)
    allocate( y(size(x)), other(size(x)) )

    do l = 1, size(LOCALES)
      do f = 1, size(REAL_FMTS)
        strings = String(x, locale=LOCALES(l), fmt=REAL_FMTS(f))
        call cast(strings, into=y, locale=LOCALES(l), fmt=REAL_FMTS(f))
        call expect_identical(y, x, "String then cast"//context(0, REAL_FMTS(f), LOCALES(l)))
        call cast(x, into=other, locale=LOCALES(l), fmt=REAL_FMTS(f))
        call expect_equal(other, strings, "cast into String should agree with String"// &
                          context(0, REAL_FMTS(f), LOCALES(l)))

        before = failure_count()
        do i = 1, size(x)
          call expect_equal(strings(i), str(x(i), locale=LOCALES(l), fmt=REAL_FMTS(f)), &
                            "String should agree with str"//context(i, REAL_FMTS(f), LOCALES(l)))
          if ( failure_count() > before ) exit
        end do
      end do
    end do
  end subroutine string_array_r64

  subroutine negative_zero_r64()
    !! Negative zero keeps its sign through text in every format and locale (#60).
    real(r64) :: x, y
    integer   :: l, f

    x = ieee_copy_sign(0.0_r64, -1.0_r64)

    do l = 1, size(LOCALES)
      do f = 1, size(REAL_FMTS)
        call cast(str(x, locale=LOCALES(l), fmt=REAL_FMTS(f)), into=y, locale=LOCALES(l), fmt=REAL_FMTS(f))
        call expect_identical(y, x, "Negative zero"//context(0, REAL_FMTS(f), LOCALES(l)))
      end do
    end do
  end subroutine negative_zero_r64

  subroutine str_cast_r32()
    !! `str` then `cast`, and `cast` into a character variable, for each locale and format.
    real(r32), allocatable        :: x(:)
    real(r32)                     :: y
    character(len=:), allocatable :: text, other
    integer                       :: l, f, i, st, before

    call reset_random()
    x = test_values_of(0.0_r32, N_SCALAR)

    do l = 1, size(LOCALES)
      do f = 1, size(REAL_FMTS)
        before = failure_count()
        do i = 1, size(x)
          text = str(x(i), locale=LOCALES(l), fmt=REAL_FMTS(f))
          call cast(text, into=y, locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st)
          if ( st /= 0 ) call fail("cast should read back "//text//context(i, REAL_FMTS(f), LOCALES(l)))
          call expect_identical(y, x(i), "str then cast"//context(i, REAL_FMTS(f), LOCALES(l)))
          call cast(x(i), into=other, locale=LOCALES(l), fmt=REAL_FMTS(f))
          call expect_equal(other, text, "cast into character should agree with str"// &
                            context(i, REAL_FMTS(f), LOCALES(l)))
          if ( failure_count() > before ) exit
        end do
      end do
    end do
  end subroutine str_cast_r32

  subroutine string_array_r32()
    !! `String` on an array then `cast` back, `cast` into a `String` array, and agreement with `str`, for each locale
    !! and format.
    real(r32), allocatable    :: x(:), y(:)
    type(String), allocatable :: strings(:), other(:)
    integer                   :: l, f, i, before

    call reset_random()
    x = test_values_of(0.0_r32, N_ARRAY)
    allocate( y(size(x)), other(size(x)) )

    do l = 1, size(LOCALES)
      do f = 1, size(REAL_FMTS)
        strings = String(x, locale=LOCALES(l), fmt=REAL_FMTS(f))
        call cast(strings, into=y, locale=LOCALES(l), fmt=REAL_FMTS(f))
        call expect_identical(y, x, "String then cast"//context(0, REAL_FMTS(f), LOCALES(l)))
        call cast(x, into=other, locale=LOCALES(l), fmt=REAL_FMTS(f))
        call expect_equal(other, strings, "cast into String should agree with String"// &
                          context(0, REAL_FMTS(f), LOCALES(l)))

        before = failure_count()
        do i = 1, size(x)
          call expect_equal(strings(i), str(x(i), locale=LOCALES(l), fmt=REAL_FMTS(f)), &
                            "String should agree with str"//context(i, REAL_FMTS(f), LOCALES(l)))
          if ( failure_count() > before ) exit
        end do
      end do
    end do
  end subroutine string_array_r32

  subroutine negative_zero_r32()
    !! Negative zero keeps its sign through text in every format and locale (#60).
    real(r32) :: x, y
    integer   :: l, f

    x = ieee_copy_sign(0.0_r32, -1.0_r32)

    do l = 1, size(LOCALES)
      do f = 1, size(REAL_FMTS)
        call cast(str(x, locale=LOCALES(l), fmt=REAL_FMTS(f)), into=y, locale=LOCALES(l), fmt=REAL_FMTS(f))
        call expect_identical(y, x, "Negative zero"//context(0, REAL_FMTS(f), LOCALES(l)))
      end do
    end do
  end subroutine negative_zero_r32

#ifdef IOFL_HAS_R128
  subroutine str_cast_c128()
    !! `str` then `cast`, and `cast` into a character variable, for each locale, format and imaginary unit.
    complex(r128), allocatable     :: x(:)
    complex(r128)                  :: y
    character(len=:), allocatable :: text, other
    integer                       :: l, f, m, i, st, before

    call reset_random()
    x = test_values_of((0.0_r128,0.0_r128), N_SCALAR/4)

    do m = 1, size(IMS)
      do l = 1, size(LOCALES)
        do f = 1, size(REAL_FMTS)
          before = failure_count()
          do i = 1, size(x)
            text = str(x(i), locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)))
            call cast(text, into=y, locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st)
            if ( st /= 0 ) call fail("cast should read back "//text//context(i, REAL_FMTS(f), LOCALES(l), IMS(m)))
            call expect_identical(y, x(i), "str then cast"//context(i, REAL_FMTS(f), LOCALES(l), IMS(m)))
            call cast(x(i), into=other, locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)))
            call expect_equal(other, text, "cast into character should agree with str"// &
                              context(i, REAL_FMTS(f), LOCALES(l), IMS(m)))
            if ( failure_count() > before ) exit
          end do
        end do
      end do
    end do
  end subroutine str_cast_c128

  subroutine string_array_c128()
    !! `String` on an array then `cast` back, `cast` into a `String` array, and agreement with `str`, for each locale,
    !! format and imaginary unit.
    complex(r128), allocatable :: x(:), y(:)
    type(String), allocatable :: strings(:), other(:)
    integer                   :: l, f, m, i, before

    call reset_random()
    x = test_values_of((0.0_r128,0.0_r128), N_ARRAY/4)
    allocate( y(size(x)), other(size(x)) )

    do m = 1, size(IMS)
      do l = 1, size(LOCALES)
        do f = 1, size(REAL_FMTS)
          strings = String(x, locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)))
          call cast(strings, into=y, locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)))
          call expect_identical(y, x, "String then cast"//context(0, REAL_FMTS(f), LOCALES(l), IMS(m)))
          call cast(x, into=other, locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)))
          call expect_equal(other, strings, "cast into String should agree with String"// &
                            context(0, REAL_FMTS(f), LOCALES(l), IMS(m)))

          before = failure_count()
          do i = 1, size(x)
            call expect_equal(strings(i), str(x(i), locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m))), &
                              "String should agree with str"//context(i, REAL_FMTS(f), LOCALES(l), IMS(m)))
            if ( failure_count() > before ) exit
          end do
        end do
      end do
    end do
  end subroutine string_array_c128

  subroutine negative_zero_c128()
    !! Negative zero parts keep their sign through text in every format and locale (#60).
    complex(r128) :: x, y
    real(r128)    :: nz
    integer      :: l, f

    nz = ieee_copy_sign(0.0_r128, -1.0_r128)
    x  = cmplx(nz, nz, kind=r128)

    do l = 1, size(LOCALES)
      do f = 1, size(REAL_FMTS)
        call cast(str(x, locale=LOCALES(l), fmt=REAL_FMTS(f)), into=y, locale=LOCALES(l), fmt=REAL_FMTS(f))
        call expect_identical(y, x, "Negative zero parts"//context(0, REAL_FMTS(f), LOCALES(l)))
      end do
    end do
  end subroutine negative_zero_c128
#endif

  subroutine str_cast_c64()
    !! `str` then `cast`, and `cast` into a character variable, for each locale, format and imaginary unit.
    complex(r64), allocatable     :: x(:)
    complex(r64)                  :: y
    character(len=:), allocatable :: text, other
    integer                       :: l, f, m, i, st, before

    call reset_random()
    x = test_values_of((0.0_r64,0.0_r64), N_SCALAR/4)

    do m = 1, size(IMS)
      do l = 1, size(LOCALES)
        do f = 1, size(REAL_FMTS)
          before = failure_count()
          do i = 1, size(x)
            text = str(x(i), locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)))
            call cast(text, into=y, locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st)
            if ( st /= 0 ) call fail("cast should read back "//text//context(i, REAL_FMTS(f), LOCALES(l), IMS(m)))
            call expect_identical(y, x(i), "str then cast"//context(i, REAL_FMTS(f), LOCALES(l), IMS(m)))
            call cast(x(i), into=other, locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)))
            call expect_equal(other, text, "cast into character should agree with str"// &
                              context(i, REAL_FMTS(f), LOCALES(l), IMS(m)))
            if ( failure_count() > before ) exit
          end do
        end do
      end do
    end do
  end subroutine str_cast_c64

  subroutine string_array_c64()
    !! `String` on an array then `cast` back, `cast` into a `String` array, and agreement with `str`, for each locale,
    !! format and imaginary unit.
    complex(r64), allocatable :: x(:), y(:)
    type(String), allocatable :: strings(:), other(:)
    integer                   :: l, f, m, i, before

    call reset_random()
    x = test_values_of((0.0_r64,0.0_r64), N_ARRAY/4)
    allocate( y(size(x)), other(size(x)) )

    do m = 1, size(IMS)
      do l = 1, size(LOCALES)
        do f = 1, size(REAL_FMTS)
          strings = String(x, locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)))
          call cast(strings, into=y, locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)))
          call expect_identical(y, x, "String then cast"//context(0, REAL_FMTS(f), LOCALES(l), IMS(m)))
          call cast(x, into=other, locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)))
          call expect_equal(other, strings, "cast into String should agree with String"// &
                            context(0, REAL_FMTS(f), LOCALES(l), IMS(m)))

          before = failure_count()
          do i = 1, size(x)
            call expect_equal(strings(i), str(x(i), locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m))), &
                              "String should agree with str"//context(i, REAL_FMTS(f), LOCALES(l), IMS(m)))
            if ( failure_count() > before ) exit
          end do
        end do
      end do
    end do
  end subroutine string_array_c64

  subroutine negative_zero_c64()
    !! Negative zero parts keep their sign through text in every format and locale (#60).
    complex(r64) :: x, y
    real(r64)    :: nz
    integer      :: l, f

    nz = ieee_copy_sign(0.0_r64, -1.0_r64)
    x  = cmplx(nz, nz, kind=r64)

    do l = 1, size(LOCALES)
      do f = 1, size(REAL_FMTS)
        call cast(str(x, locale=LOCALES(l), fmt=REAL_FMTS(f)), into=y, locale=LOCALES(l), fmt=REAL_FMTS(f))
        call expect_identical(y, x, "Negative zero parts"//context(0, REAL_FMTS(f), LOCALES(l)))
      end do
    end do
  end subroutine negative_zero_c64

  subroutine str_cast_c32()
    !! `str` then `cast`, and `cast` into a character variable, for each locale, format and imaginary unit.
    complex(r32), allocatable     :: x(:)
    complex(r32)                  :: y
    character(len=:), allocatable :: text, other
    integer                       :: l, f, m, i, st, before

    call reset_random()
    x = test_values_of((0.0_r32,0.0_r32), N_SCALAR/4)

    do m = 1, size(IMS)
      do l = 1, size(LOCALES)
        do f = 1, size(REAL_FMTS)
          before = failure_count()
          do i = 1, size(x)
            text = str(x(i), locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)))
            call cast(text, into=y, locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st)
            if ( st /= 0 ) call fail("cast should read back "//text//context(i, REAL_FMTS(f), LOCALES(l), IMS(m)))
            call expect_identical(y, x(i), "str then cast"//context(i, REAL_FMTS(f), LOCALES(l), IMS(m)))
            call cast(x(i), into=other, locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)))
            call expect_equal(other, text, "cast into character should agree with str"// &
                              context(i, REAL_FMTS(f), LOCALES(l), IMS(m)))
            if ( failure_count() > before ) exit
          end do
        end do
      end do
    end do
  end subroutine str_cast_c32

  subroutine string_array_c32()
    !! `String` on an array then `cast` back, `cast` into a `String` array, and agreement with `str`, for each locale,
    !! format and imaginary unit.
    complex(r32), allocatable :: x(:), y(:)
    type(String), allocatable :: strings(:), other(:)
    integer                   :: l, f, m, i, before

    call reset_random()
    x = test_values_of((0.0_r32,0.0_r32), N_ARRAY/4)
    allocate( y(size(x)), other(size(x)) )

    do m = 1, size(IMS)
      do l = 1, size(LOCALES)
        do f = 1, size(REAL_FMTS)
          strings = String(x, locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)))
          call cast(strings, into=y, locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)))
          call expect_identical(y, x, "String then cast"//context(0, REAL_FMTS(f), LOCALES(l), IMS(m)))
          call cast(x, into=other, locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)))
          call expect_equal(other, strings, "cast into String should agree with String"// &
                            context(0, REAL_FMTS(f), LOCALES(l), IMS(m)))

          before = failure_count()
          do i = 1, size(x)
            call expect_equal(strings(i), str(x(i), locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m))), &
                              "String should agree with str"//context(i, REAL_FMTS(f), LOCALES(l), IMS(m)))
            if ( failure_count() > before ) exit
          end do
        end do
      end do
    end do
  end subroutine string_array_c32

  subroutine negative_zero_c32()
    !! Negative zero parts keep their sign through text in every format and locale (#60).
    complex(r32) :: x, y
    real(r32)    :: nz
    integer      :: l, f

    nz = ieee_copy_sign(0.0_r32, -1.0_r32)
    x  = cmplx(nz, nz, kind=r32)

    do l = 1, size(LOCALES)
      do f = 1, size(REAL_FMTS)
        call cast(str(x, locale=LOCALES(l), fmt=REAL_FMTS(f)), into=y, locale=LOCALES(l), fmt=REAL_FMTS(f))
        call expect_identical(y, x, "Negative zero parts"//context(0, REAL_FMTS(f), LOCALES(l)))
      end do
    end do
  end subroutine negative_zero_c32

end module text_round_trip_cases
