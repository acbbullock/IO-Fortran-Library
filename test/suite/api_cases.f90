module api_cases
  !---------------------------------------------------------------------------------------------------------------------
  !! The public names: the `IOFL_` constants and error codes, and the older constant names kept for compatibility.
  !---------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_c_binding,   only: c_null_char
  use, intrinsic :: iso_fortran_env, only: real64
  use io_fortran_lib, only: IOFL_NL, IOFL_SPACE, IOFL_CR, IOFL_LF, IOFL_TAB, IOFL_FF, IOFL_VT, IOFL_BELL, IOFL_NUL, &
                            IOFL_READ_ERR, IOFL_WRITE_ERR, IOFL_ALLOC_ERR, IOFL_ARG_ERR, &
                            NL, SPACE, CR, FF, VT, LF, TAB, HT, BELL, NUL, CNUL, to_file, from_file, cast
  use test_support, only: test_suite, expect_equal, scratch
  implicit none
  private

  public :: register_api

contains

  subroutine register_api(suite)
    type(test_suite), intent(inout) :: suite

    call suite%group("api")
    call suite%add("constant_values", constant_values)
    call suite%add("old_constant_names", old_constant_names)
    call suite%add("error_code_values", error_code_values)
    call suite%add("stat_uses_error_codes", stat_uses_error_codes)
  end subroutine register_api

  subroutine constant_values()
    call expect_equal(IOFL_NL, new_line("a"), "IOFL_NL")
    call expect_equal(IOFL_SPACE, achar(32), "IOFL_SPACE")
    call expect_equal(IOFL_CR, achar(13), "IOFL_CR")
    call expect_equal(IOFL_LF, achar(10), "IOFL_LF")
    call expect_equal(IOFL_TAB, achar(9), "IOFL_TAB")
    call expect_equal(IOFL_FF, achar(12), "IOFL_FF")
    call expect_equal(IOFL_VT, achar(11), "IOFL_VT")
    call expect_equal(IOFL_BELL, achar(7), "IOFL_BELL")
    call expect_equal(IOFL_NUL, achar(0), "IOFL_NUL")
    call expect_equal(IOFL_NUL, c_null_char, "IOFL_NUL is C's string terminator")
  end subroutine constant_values

  subroutine old_constant_names()
    !! The names from before 2.1 stay, with the same values.
    call expect_equal(NL, IOFL_NL, "NL")
    call expect_equal(SPACE, IOFL_SPACE, "SPACE")
    call expect_equal(CR, IOFL_CR, "CR")
    call expect_equal(FF, IOFL_FF, "FF")
    call expect_equal(VT, IOFL_VT, "VT")
    call expect_equal(LF, IOFL_LF, "LF")
    call expect_equal(TAB, IOFL_TAB, "TAB")
    call expect_equal(HT, IOFL_TAB, "HT")
    call expect_equal(BELL, IOFL_BELL, "BELL")
    call expect_equal(NUL, IOFL_NUL, "NUL")
    call expect_equal(CNUL, IOFL_NUL, "CNUL")
  end subroutine old_constant_names

  subroutine error_code_values()
    !! The values are documented, and callers may have compared `stat` against them.
    call expect_equal(IOFL_READ_ERR, 1, "IOFL_READ_ERR")
    call expect_equal(IOFL_WRITE_ERR, 2, "IOFL_WRITE_ERR")
    call expect_equal(IOFL_ALLOC_ERR, 3, "IOFL_ALLOC_ERR")
    call expect_equal(IOFL_ARG_ERR, 4, "IOFL_ARG_ERR")
  end subroutine error_code_values

  subroutine stat_uses_error_codes()
    real(real64), allocatable :: v(:)
    character(len=512)        :: msg
    integer                   :: st, i

    msg = ""
    call from_file(scratch()//"_missing.csv", into=v, stat=st, errmsg=msg)
    call expect_equal(st, IOFL_READ_ERR, "A missing file is a read error.")
    call to_file([1.0_real64], file=scratch()//".csv", fmt="x", stat=st, errmsg=msg)
    call expect_equal(st, IOFL_ARG_ERR, "An invalid fmt is an argument error.")
    call cast("abc", into=i, stat=st)
    call expect_equal(st, IOFL_READ_ERR, "Invalid text for cast is a read error.")
  end subroutine stat_uses_error_codes
end module api_cases
