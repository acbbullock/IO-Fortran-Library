module str_cases
  !---------------------------------------------------------------------------------------------------------------------
  !! `str` and `String`: number formatting, special and extreme values, rounding, and locales.
  !---------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: real64
  use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_positive_inf, ieee_is_negative
  use io_fortran_lib, only: cast, str, operator(==), operator(+), operator(<), operator(>)
  use test_support, only: test_suite, STOPS, check, expect_equal
  implicit none
  private

  public :: register_str

contains

  subroutine register_str(suite)
    type(test_suite), intent(inout) :: suite

    call suite%group("str")
    call suite%add("str_invalid_fmt", str_invalid_fmt, expect=STOPS)
    call suite%add("str_invalid_locale", str_invalid_locale, expect=STOPS)
    call suite%add("eu_zero_str_real", eu_zero_str_real)
    call suite%add("eu_zero_str_complex", eu_zero_str_complex)
    call suite%add("nan_str", nan_str)
    call suite%add("nan_str_fmt_f", nan_str_fmt_f)
    call suite%add("inf_str_decimals", inf_str_decimals)
    call suite%add("inf_str_fmt_f", inf_str_fmt_f)
    call suite%add("huge_str_fmt_f", huge_str_fmt_f)
    call suite%add("large_str_fmt_f", large_str_fmt_f)
    call suite%add("negative_zero_hex", negative_zero_hex)
    call suite%add("tiny_str_fmt_f", tiny_str_fmt_f)
    call suite%add("decimals_round_fmt_e", decimals_round_fmt_e)
    call suite%add("decimals_round_fmt_f", decimals_round_fmt_f)
    call suite%add("digits_same_for_negative", digits_same_for_negative)
    call suite%add("complex_zero_part_fmt_f", complex_zero_part_fmt_f)
  end subroutine register_str

  subroutine str_invalid_fmt()
    write(*,"(a)") str(1.5, fmt="q")
  end subroutine str_invalid_fmt

  subroutine str_invalid_locale()
    write(*,"(a)") str(1.5, locale="JP")
  end subroutine str_invalid_locale

  subroutine eu_zero_str_real()
    call check(index(str(0.0, locale="EU"), ".") == 0, "A comma locale should not write a period.")
    call check(index(str(0.0_real64, fmt="f", locale="EU"), ".") == 0, "A comma locale should not write a period.")
  end subroutine eu_zero_str_real

  subroutine eu_zero_str_complex()
    call check(index(str((0.0,1.0), locale="EU"), ".") == 0, "A comma locale should not write a period.")
  end subroutine eu_zero_str_complex

  subroutine nan_str()
    real(real64) :: x

    x = ieee_value(x, ieee_quiet_nan)
    call check(trim(adjustl(str(x))) == "NaN", "NaN should be written as NaN.")
  end subroutine nan_str

  subroutine nan_str_fmt_f()
    real(real64) :: x

    x = ieee_value(x, ieee_quiet_nan)
    call check(trim(adjustl(str(x, fmt="f"))) == "NaN", "NaN should be written as NaN.")
  end subroutine nan_str_fmt_f

  subroutine inf_str_decimals()
    real(real64) :: x

    x = ieee_value(x, ieee_positive_inf)
    call check(index(str(x, decimals=3), "Inf") > 0, "Infinity should be written as such.")
  end subroutine inf_str_decimals

  subroutine inf_str_fmt_f()
    real(real64) :: x

    x = ieee_value(x, ieee_positive_inf)
    call check(index(str(x, fmt="f"), "Inf") > 0, "Infinity should be written as such.")
  end subroutine inf_str_fmt_f

  subroutine huge_str_fmt_f()
    real(real64) :: y

    call read_back(str(huge(1.0_real64), fmt="f"), y)
    call check(abs(y/huge(1.0_real64) - 1.0_real64) < 1e-10_real64, "The value should be written in full.")
  end subroutine huge_str_fmt_f

  subroutine large_str_fmt_f()
    real(real64) :: y

    call read_back(str(1e120_real64, fmt="f"), y)
    call check(abs(y/1e120_real64 - 1.0_real64) < 1e-10_real64, "The value should be written in full.")
  end subroutine large_str_fmt_f

  subroutine negative_zero_hex()
    real(real64) :: y

    call cast(str(-0.0_real64, fmt="z"), into=y, fmt="z")
    call check(ieee_is_negative(y), "The sign of zero should survive a hexadecimal round trip.")
  end subroutine negative_zero_hex

  subroutine tiny_str_fmt_f()
    real(real64) :: y

    call read_back(str(1e-300_real64, fmt="f"), y)
    call check(abs(y/1e-300_real64 - 1.0_real64) < 1e-10_real64, "The value should be written in full.")
    call read_back(str(-1e-300_real64, fmt="f"), y)
    call check(abs(y/1e-300_real64 + 1.0_real64) < 1e-10_real64, "The sign should be kept.")
    call check(index(str(1e-300_real64, fmt="f", locale="EU"), ",") == 2, "A comma locale should write a comma.")
    call check(len(str(1e-300_real64, fmt="f", decimals=250)) == 252, "Decimals should set the length.")
  end subroutine tiny_str_fmt_f

  subroutine decimals_round_fmt_e()
    call expect_equal(str(1.999_real64, fmt="e", decimals=2), "2.00e+000", "Decimals should round.")
    call expect_equal(str(9.999_real64, fmt="e", decimals=2), "1.00e+001", "Rounding should carry into the exponent.")
    call expect_equal(str(1.994_real64, fmt="e", decimals=2), "1.99e+000", "Decimals should round down when closer.")
  end subroutine decimals_round_fmt_e

  subroutine decimals_round_fmt_f()
    call expect_equal(str(0.999_real64, fmt="f", decimals=2), "1.00", "Decimals should round.")
    call expect_equal(str(999.999_real64, fmt="f", decimals=2), "1000.00", "Rounding should add a digit.")
    call expect_equal(str(-0.999_real64, fmt="f", decimals=2), "-1.00", "Rounding should keep the sign.")
    call expect_equal(str(0.994_real64, fmt="f", decimals=2), "0.99", "Decimals should round down when closer.")
  end subroutine decimals_round_fmt_f

  subroutine digits_same_for_negative()
    call check(str(-5.0_real64, fmt="f") == "-"//str(5.0_real64, fmt="f"), "The sign should not change the digits.")
  end subroutine digits_same_for_negative

  subroutine complex_zero_part_fmt_f()
    call check(str((1.0_real64,0.0_real64), fmt="f") == "("//str(1.0_real64, fmt="f")//",0.0)", "Zero imag part.")
    call check(str((0.0_real64,1.0_real64), fmt="f") == "(0.0,"//str(1.0_real64, fmt="f")//")", "Zero real part.")
  end subroutine complex_zero_part_fmt_f

  subroutine read_back(text, value)
    character(len=*), intent(in)  :: text
    real(real64),     intent(out) :: value

    integer :: status

    read(text, *, iostat=status) value
    call expect_equal(status, 0, "The text should be a number: "//text)
  end subroutine read_back
end module str_cases
