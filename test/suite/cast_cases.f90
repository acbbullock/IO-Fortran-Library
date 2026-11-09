module cast_cases
  !---------------------------------------------------------------------------------------------------------------------
  !! `cast`: valid and invalid numbers, `stat`, and errors with no `stat`.
  !---------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: real32, real64, int8, int32, int64
  use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
  use io_fortran_lib, only: String, cast, str, IOFL_CR, operator(==), operator(/=), operator(<), operator(<=), &
                            operator(>)
  use test_support, only: test_suite, STOPS, check
  implicit none
  private

  public :: register_cast

contains

  subroutine register_cast(suite)
    type(test_suite), intent(inout) :: suite

    call suite%group("cast")
    call suite%add("cast_int_letters", cast_int_letters, expect=STOPS)
    call suite%add("cast_int_trailing_junk", cast_int_trailing_junk, expect=STOPS)
    call suite%add("cast_int_decimal_point", cast_int_decimal_point, expect=STOPS)
    call suite%add("cast_int32_overflow", cast_int32_overflow, expect=STOPS)
    call suite%add("cast_int32_just_over_range", cast_int32_just_over_range, expect=STOPS)
    call suite%add("cast_int8_just_over_range", cast_int8_just_over_range, expect=STOPS)
    call suite%add("cast_real_letters", cast_real_letters, expect=STOPS)
    call suite%add("cast_real_empty", cast_real_empty, expect=STOPS)
    call suite%add("cast_real_repeat_count", cast_real_repeat_count, expect=STOPS)
    call suite%add("cast_real_two_values", cast_real_two_values, expect=STOPS)
    call suite%add("cast_real_trailing_junk", cast_real_trailing_junk, expect=STOPS)
    call suite%add("cast_real_slash", cast_real_slash, expect=STOPS)
    call suite%add("cast_bad_complex", cast_bad_complex, expect=STOPS)
    call suite%add("cast_invalid_fmt", cast_invalid_fmt, expect=STOPS)
    call suite%add("cast_invalid_locale", cast_invalid_locale, expect=STOPS)
    call suite%add("cast_int_valid", cast_int_valid)
    call suite%add("cast_int_stat", cast_int_stat)
    call suite%add("cast_int_hex", cast_int_hex)
    call suite%add("cast_real_valid", cast_real_valid)
    call suite%add("cast_real_stat", cast_real_stat)
    call suite%add("cast_real_wrong_decimal_mark", cast_real_wrong_decimal_mark)
    call suite%add("cast_real_hex", cast_real_hex)
    call suite%add("cast_complex_valid", cast_complex_valid)
    call suite%add("cast_complex_stat", cast_complex_stat)
    call suite%add("cast_unallocated_stat", cast_unallocated_stat)
    call suite%add("cast_invalid_args_stat", cast_invalid_args_stat)
    call suite%add("cast_array_stat", cast_array_stat)
    call suite%add("eu_zero_cast_round_trip", eu_zero_cast_round_trip)
  end subroutine register_cast

  subroutine cast_int_letters()
    integer :: i

    call cast("abc", into=i)
  end subroutine cast_int_letters

  subroutine cast_int_trailing_junk()
    integer :: i

    call cast("12x", into=i)
  end subroutine cast_int_trailing_junk

  subroutine cast_int_decimal_point()
    integer :: i

    call cast("3.5", into=i)
  end subroutine cast_int_decimal_point

  subroutine cast_int32_overflow()
    integer :: i

    call cast("99999999999", into=i)
  end subroutine cast_int32_overflow

  subroutine cast_int32_just_over_range()
    integer :: i

    call cast("2147483648", into=i)
  end subroutine cast_int32_just_over_range

  subroutine cast_int8_just_over_range()
    integer(int8) :: i8

    call cast("128", into=i8)
  end subroutine cast_int8_just_over_range

  subroutine cast_real_letters()
    real(real64) :: x

    call cast("abc", into=x)
  end subroutine cast_real_letters

  subroutine cast_real_empty()
    real(real64) :: x

    call cast("", into=x)
  end subroutine cast_real_empty

  subroutine cast_real_repeat_count()
    real(real64) :: x

    call cast("2*3.5", into=x)
  end subroutine cast_real_repeat_count

  subroutine cast_real_two_values()
    real(real64) :: x

    call cast("1 2", into=x)
  end subroutine cast_real_two_values

  subroutine cast_real_trailing_junk()
    real(real64) :: x

    call cast("7,junk", into=x)
  end subroutine cast_real_trailing_junk

  subroutine cast_real_slash()
    real(real64) :: x

    call cast("/", into=x)
  end subroutine cast_real_slash

  subroutine cast_bad_complex()
    complex(real32) :: z

    call cast("abc", into=z)
  end subroutine cast_bad_complex

  subroutine cast_invalid_fmt()
    real(real64) :: x

    call cast("1", into=x, fmt="q")
  end subroutine cast_invalid_fmt

  subroutine cast_invalid_locale()
    real(real64) :: x

    call cast("1", into=x, locale="JP")
  end subroutine cast_invalid_locale

  subroutine cast_int_valid()
    integer        :: st
    integer(int8)  :: i8
    integer(int64) :: i64

    st = 0

    call cast_i32_is("7", 7); call cast_i32_is("+7", 7); call cast_i32_is("-7", -7); call cast_i32_is("  12 ", 12)
    call cast_i32_is("0", 0); call cast_i32_is("007", 7); call cast_i32_is("12"//IOFL_CR, 12)
    call cast_i32_is("2147483647", huge(1_int32)); call cast_i32_is("-2147483648", -huge(1_int32)-1_int32)
    call cast("127", into=i8, stat=st); call check((st == 0) .and. (i8 == 127_int8), "Largest int8.")
    call cast("-128", into=i8, stat=st); call check((st == 0) .and. (i8 == -127_int8-1_int8), "Smallest int8.")
    call cast("9223372036854775807", into=i64, stat=st)
    call check((st == 0) .and. (i64 == huge(1_int64)), "Largest int64.")
    call cast("-9223372036854775808", into=i64, stat=st)
    call check((st == 0) .and. (i64 == -huge(1_int64)-1_int64), "Smallest int64.")
  end subroutine cast_int_valid

  subroutine cast_int_stat()
    integer        :: st
    integer(int8)  :: i8
    integer(int64) :: i64

    st = 0

    call cast_i32_fails(""); call cast_i32_fails("   "); call cast_i32_fails("abc"); call cast_i32_fails("12x")
    call cast_i32_fails("3.5"); call cast_i32_fails("+"); call cast_i32_fails("-"); call cast_i32_fails("1 2")
    call cast_i32_fails("--1"); call cast_i32_fails("2147483648"); call cast_i32_fails("-2147483649")
    call cast_i32_fails("99999999999999999999999999")
    call cast("128", into=i8, stat=st); call check((st /= 0) .and. (i8 == 0_int8), "Just over int8.")
    call cast("-129", into=i8, stat=st); call check((st /= 0) .and. (i8 == 0_int8), "Just under int8.")
    call cast("9223372036854775808", into=i64, stat=st); call check(st /= 0, "Just over int64.")
    call cast("-9223372036854775809", into=i64, stat=st); call check(st /= 0, "Just under int64.")
  end subroutine cast_int_stat

  subroutine cast_int_hex()
    integer       :: st, i
    integer(int8) :: i8

    st = 0

    call cast("0xff", into=i8, fmt="z", stat=st); call check((st == 0) .and. (i8 == -1_int8), "All bits set.")
    call cast("0x7f", into=i8, fmt="z", stat=st); call check((st == 0) .and. (i8 == 127_int8), "Largest int8.")
    call cast("ff", into=i, fmt="z", stat=st); call check((st == 0) .and. (i == 255), "Without a prefix.")
    call cast("0XFF", into=i, fmt="z", stat=st); call check((st == 0) .and. (i == 255), "Uppercase.")
    call cast("0x000000ff", into=i8, fmt="z", stat=st)
    call check((st == 0) .and. (i8 == -1_int8), "Leading zeros.")
    call cast("0x", into=i, fmt="z", stat=st); call check(st /= 0, "No digits.")
    call cast("0xg", into=i, fmt="z", stat=st); call check(st /= 0, "Not a hex digit.")
    call cast("0x1ff", into=i8, fmt="z", stat=st); call check(st /= 0, "Too large for int8.")
    call cast("0x1ffffffff", into=i, fmt="z", stat=st); call check(st /= 0, "Too large for int32.")
  end subroutine cast_int_hex

  subroutine cast_real_valid()
    integer      :: st
    real(real64) :: x

    st = 0

    call cast_r64_is("1.5e3", 1500.0_real64, "US"); call cast_r64_is(".5", 0.5_real64, "US")
    call cast_r64_is("5.", 5.0_real64, "US"); call cast_r64_is("-2", -2.0_real64, "US")
    call cast_r64_is("+3.25", 3.25_real64, "US"); call cast_r64_is("1d2", 100.0_real64, "US")
    call cast_r64_is("  7 ", 7.0_real64, "US"); call cast_r64_is("1E-2", 0.01_real64, "US")
    call cast_r64_is("1,5", 1.5_real64, "EU"); call cast_r64_is("-1,5e1", -15.0_real64, "EU")
    call cast_r64_is("2.5"//IOFL_CR, 2.5_real64, "US")
    call cast("NaN", into=x, stat=st); call check((st == 0) .and. ieee_is_nan(x), "NaN.")
    call cast("Inf", into=x, stat=st); call check((st == 0) .and. (x > huge(x)), "Inf.")
    call cast("-Inf", into=x, stat=st); call check((st == 0) .and. (x < -huge(x)), "-Inf.")
    call cast("infinity", into=x, stat=st); call check((st == 0) .and. (x > huge(x)), "infinity.")
  end subroutine cast_real_valid

  subroutine cast_real_stat()
    call cast_r64_fails("", "US"); call cast_r64_fails("  ", "US"); call cast_r64_fails("abc", "US")
    call cast_r64_fails("2*3.5", "US"); call cast_r64_fails("1 2", "US"); call cast_r64_fails("7,junk", "US")
    call cast_r64_fails("/", "US"); call cast_r64_fails(".", "US"); call cast_r64_fails("e5", "US")
    call cast_r64_fails("1e", "US"); call cast_r64_fails("1e+", "US"); call cast_r64_fails("1.2.3", "US")
    call cast_r64_fails("+", "US"); call cast_r64_fails("1e5x", "US"); call cast_r64_fails("NaNN", "US")
  end subroutine cast_real_stat

  subroutine cast_real_wrong_decimal_mark()
    call cast_r64_fails("1,5", "US"); call cast_r64_fails("1.5", "EU")
  end subroutine cast_real_wrong_decimal_mark

  subroutine cast_real_hex()
    integer      :: st
    real(real32) :: x32
    real(real64) :: x

    st = 0

    call cast("0x3ff8000000000000", into=x, fmt="z", stat=st); call check((st == 0) .and. (x == 1.5_real64), "1.5.")
    call cast("0x3fc00000", into=x32, fmt="z", stat=st); call check((st == 0) .and. (x32 == 1.5), "real32 1.5.")
    call cast("0x1ffffffff", into=x32, fmt="z", stat=st); call check(st /= 0, "Too large for real32.")
    call cast("0xzz", into=x, fmt="z", stat=st); call check(st /= 0, "Not a hex digit.")
  end subroutine cast_real_hex

  subroutine cast_complex_valid()
    integer         :: st
    complex(real32) :: z

    st = 0

    call cast("(1.5,-2.5)", into=z, stat=st); call check((st == 0) .and. (z == (1.5,-2.5)), "Ordered pair.")
    call cast("(1,5;-2,5)", into=z, locale="EU", stat=st); call check((st == 0) .and. (z == (1.5,-2.5)), "EU pair.")
    call cast("1.5-2.5j", into=z, im="j", stat=st); call check((st == 0) .and. (z == (1.5,-2.5)), "Unit j.")
    call cast("-1.5+2.5*1i", into=z, im="*1i", stat=st); call check((st == 0) .and. (z == (-1.5,2.5)), "Unit *1i.")
    call cast("1.5e-1-2.5e+1j", into=z, im="j", stat=st)
    call check((st == 0) .and. (abs(z - (0.15,-25.0)) < 1e-5), "Exponents should not be split.")
    call cast("(0x3fc00000,0x40200000)", into=z, fmt="z", stat=st)
    call check((st == 0) .and. (z == (1.5,2.5)), "Hexadecimal pair.")
    call cast("0x3fc00000+0x40200000j", into=z, fmt="z", im="j", stat=st)
    call check((st == 0) .and. (z == (1.5,2.5)), "Hexadecimal with a unit.")
    call cast("(NaN,-Inf)", into=z, stat=st); call check((st == 0) .and. ieee_is_nan(z%re), "Special values.")
    call cast("NaN-Infj", into=z, im="j", stat=st); call check((st == 0) .and. (z%im < 0.0), "Special values.")
  end subroutine cast_complex_valid

  subroutine cast_complex_stat()
    call cast_c32_fails("", ""); call cast_c32_fails("abc", ""); call cast_c32_fails("(1,2", "")
    call cast_c32_fails("1,2)", ""); call cast_c32_fails("()", ""); call cast_c32_fails("(1;2)", "")
    call cast_c32_fails("(a,b)", ""); call cast_c32_fails("(1,)", ""); call cast_c32_fails("(,2)", "")
    call cast_c32_fails("(1,2,3)", ""); call cast_c32_fails("1+2", "j"); call cast_c32_fails("1.5j", "j")
    call cast_c32_fails("j", "j"); call cast_c32_fails("+j", "j"); call cast_c32_fails("abc+defj", "j")
  end subroutine cast_complex_stat

  subroutine cast_unallocated_stat()
    type(String)    :: s
    integer         :: st, i
    real(real64)    :: x
    complex(real32) :: z

    st = 0

    call s%cast(into=i, stat=st); call check((st /= 0) .and. (i == 0), "Integer.")
    call s%cast(into=x, stat=st); call check((st /= 0) .and. (x == 0.0_real64), "Real.")
    call s%cast(into=z, stat=st); call check((st /= 0) .and. (z == (0.0,0.0)), "Complex.")
  end subroutine cast_unallocated_stat

  subroutine cast_invalid_args_stat()
    integer         :: st, i
    real(real64)    :: x
    complex(real32) :: z

    st = 0

    call cast("1", into=x, fmt="q", stat=st); call check(st /= 0, "Invalid real fmt.")
    call cast("1", into=x, locale="JP", stat=st); call check(st /= 0, "Invalid locale.")
    call cast("1", into=i, fmt="q", stat=st); call check(st /= 0, "Invalid int fmt.")
    call cast("(1,2)", into=z, fmt="q", stat=st); call check(st /= 0, "Invalid complex fmt.")
  end subroutine cast_invalid_args_stat

  subroutine cast_array_stat()
    type(String) :: three(3)
    integer      :: iv(3), stv(3)

    three(1) = String("1"); three(2) = String("oops"); three(3) = String("3")
    call cast(three, into=iv, stat=stv)
    call check(all((stv /= 0) .eqv. [.false.,.true.,.false.]), "Each cell should have its own status.")
    call check(all(iv == [1,0,3]), "Each valid cell should still be cast.")
  end subroutine cast_array_stat

  subroutine eu_zero_cast_round_trip()
    type(String)    :: s
    real(real32)    :: x32
    complex(real32) :: z

    call cast(str(0.0, locale="EU"), into=x32, locale="EU")
    call check(x32 == 0.0, "Zero should survive a round trip.")
    s = String((0.0,1.0), locale="EU", fmt="e", im="j")
    call cast(s, into=z, locale="EU", fmt="e", im="j")
    call check(z == (0.0,1.0), "Zero should survive a round trip.")
  end subroutine eu_zero_cast_round_trip

  subroutine cast_i32_is(text, expected)
    character(len=*), intent(in) :: text
    integer,          intent(in) :: expected

    integer :: value, status

    call cast(text, into=value, stat=status)
    call check((status == 0) .and. (value == expected), 'Should be valid: "'//text//'"')
  end subroutine cast_i32_is

  subroutine cast_i32_fails(text)
    character(len=*), intent(in) :: text

    integer :: value, status

    call cast(text, into=value, stat=status)
    call check((status /= 0) .and. (value == 0), 'Should be rejected: "'//text//'"')
  end subroutine cast_i32_fails

  subroutine cast_r64_is(text, expected, locale)
    character(len=*), intent(in) :: text, locale
    real(real64),     intent(in) :: expected

    real(real64) :: value
    integer      :: status

    call cast(text, into=value, locale=locale, stat=status)
    call check((status == 0) .and. (abs(value - expected) <= 1e-12_real64*abs(expected)), &
               'Should be valid: "'//text//'"')
  end subroutine cast_r64_is

  subroutine cast_r64_fails(text, locale)
    character(len=*), intent(in) :: text, locale

    real(real64) :: value
    integer      :: status

    call cast(text, into=value, locale=locale, stat=status)
    call check((status /= 0) .and. (value == 0.0_real64), 'Should be rejected: "'//text//'"')
  end subroutine cast_r64_fails

  subroutine cast_c32_fails(text, im)
    character(len=*), intent(in) :: text, im

    complex(real32) :: value
    integer         :: status

    call cast(text, into=value, im=im, stat=status)
    call check((status /= 0) .and. (value == (0.0,0.0)), 'Should be rejected: "'//text//'"')
  end subroutine cast_c32_fails
end module cast_cases
