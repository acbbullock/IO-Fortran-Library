module aprint_cases
  !---------------------------------------------------------------------------------------------------------------------
  !! `aprint`.
  !---------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: real32, real64
  use io_fortran_lib, only: String, aprint, operator(==), operator(>=)
  use test_support, only: test_suite, check, expect_equal, scratch
  implicit none (type, external)
  private

  public :: register_aprint

contains

  subroutine register_aprint(suite)
    type(test_suite), intent(inout) :: suite

    call suite%group("aprint")
    call suite%add("aprint_to_unit", aprint_to_unit)
    call suite%add("aprint_complex_keeps_unit", aprint_complex_keeps_unit)
    call suite%add("aprint_empty", aprint_empty)
  end subroutine register_aprint

  subroutine aprint_to_unit()
    character(len=:), allocatable :: file
    type(String)                  :: s, strings(2)
    integer                       :: i

    file = scratch()
    strings = [String("xy"), String("z")]       ! Not passed as a constructor: gfortran leaks its components (#45)

    open(newunit=i, file=file//"_out.log", status="replace", action="write")
    call aprint([1.5_real64,2.5_real64], fmt="f", decimals=1, unit=i)
    call aprint(reshape([1,2,3,4], [2,2]), unit=i)
    call aprint([(1.0,2.0)], fmt="e", decimals=1, im="j", unit=i)
    call aprint(["ab","cd"], unit=i)
    call aprint(strings, unit=i)
    close(i)
    call s%read_file(file//"_out.log")
    call expect_equal(s%count("1.5"), 1, "The real array should be in the file.")
    call check(s%count("4") >= 1, "The integer array should be in the file.")
    call expect_equal(s%count("2.0e+00j"), 1, "The complex array should be in the file.")
    call check((s%count("ab") == 1) .and. (s%count("cd") == 1), "The character array should be in the file.")
    call expect_equal(s%count("xy"), 1, "The String array should be in the file.")
  end subroutine aprint_to_unit

  subroutine aprint_complex_keeps_unit()
    character(len=:), allocatable :: file
    type(String)                  :: s
    integer                       :: i

    file = scratch()

    open(newunit=i, file=file//"_out.log", status="replace", action="write")
    call aprint([(1.0,2.0),(3.5,4.5)], fmt="e", decimals=1, im="j", unit=i)
    close(i)
    call s%read_file(file//"_out.log")
    call expect_equal(s%count("2.0e+00j"), 1, "The imaginary unit should not be cut off.")
    call expect_equal(s%count("4.5e+00j"), 1, "On every element.")
  end subroutine aprint_complex_keeps_unit

  subroutine aprint_empty()
    real(real32), allocatable :: v32(:)

    allocate( v32(0) )
    call aprint(v32)
  end subroutine aprint_empty
end module aprint_cases
