module api_cases
  !---------------------------------------------------------------------------------------------------------------------
  !! The public names: the `IOFL_` constants and error codes, and the older constant names kept for compatibility.
  !---------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_c_binding,   only: c_null_char
  use, intrinsic :: iso_fortran_env, only: real64
  use io_fortran_lib, only: IOFL_NL, IOFL_SPACE, IOFL_CR, IOFL_LF, IOFL_TAB, IOFL_FF, IOFL_VT, IOFL_BELL, IOFL_NUL, &
                            IOFL_READ_ERR, IOFL_WRITE_ERR, IOFL_ALLOC_ERR, IOFL_ARG_ERR, &
                            NL, SPACE, CR, FF, VT, LF, TAB, HT, BELL, NUL, CNUL, to_file, from_file, cast
  use test_support, only: test_suite, check, expect_equal, scratch
  implicit none (type, external)
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
    call suite%add("string_function_names_free", string_function_names_free)
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
  subroutine string_function_names_free()
    !! io_fortran_lib exports no to_upper, to_lower, starts_with, ends_with, find or trim; they are in io_strings. So
    !! a program that uses io_fortran_lib without an only list can still define its own, as it could with 2.1 (#72).
    !! If io_fortran_lib ever exports one of these names, this case stops compiling.
    use io_fortran_lib
    implicit none (type, external)

    call check(to_upper() == 1 .and. to_lower() == 2 .and. starts_with() == 3 .and. ends_with() == 4 .and. &
               find() == 5 .and. trim() == 6, "A program's own procedures with these names are the ones called.")
  contains
    integer function to_upper()
      to_upper = 1
    end function to_upper
    integer function to_lower()
      to_lower = 2
    end function to_lower
    integer function starts_with()
      starts_with = 3
    end function starts_with
    integer function ends_with()
      ends_with = 4
    end function ends_with
    integer function find()
      find = 5
    end function find
    integer function trim()
      trim = 6
    end function trim
  end subroutine string_function_names_free
end module api_cases
