! Generated from fypp/test/suite/test_support.fypp by fypp/generate.sh. Edit that file, not this one.
#include "iofl_config.inc"
module test_support
  !---------------------------------------------------------------------------------------------------------------------
  !! A small test harness for IO-Fortran-Library.
  !!
  !! A case is a subroutine with no arguments, registered once with `call suite%add("name", proc)`. By default every
  !! case runs in a child process, launched as this same program with `--child <name>`, and is judged by the exit
  !! status. That is how a case can check that a call ends the program with `error stop`, and why a crash in one case
  !! can't take the others down with it.
  !!
  !! Each case says what it should do (`expect=RETURNS`, the default, or `expect=STOPS`) and how far it is from doing
  !! it (`status=FIXED`, the default, `KNOWN_BUG` or `UNDEFINED`):
  !!
  !! - `KNOWN_BUG` is an expected failure. If it starts passing the suite fails, so that the marker is removed in the
  !!   same change as the fix.
  !! - `UNDEFINED` is a known bug with undefined behavior, such as an out-of-bounds access, which can appear to work at
  !!   some optimization levels, so either result is accepted.
  !! - A `STOPS` case should end with `error stop`, and its message has to be readable.
  !!
  !! Command line, after `fpm test tests --`:
  !!
  !! - no arguments: run every case.
  !! - `<pattern>`: run the cases whose `group/name` contains the pattern, such as `cast/` or `crlf`.
  !! - `--list [pattern]`: list the cases instead of running them.
  !! - `--in-process [pattern]`: run the cases in this process, for a debugger. `STOPS` cases are skipped unless
  !!   exactly one case matches, since they end the program.
  !!
  !! `expect_equal` compares values of any kind, and prints both when they differ. `expect_identical` compares reals
  !! and complex values as identical: NaN matches NaN, and the sign of zero counts unless `ignore_zero_sign` is true.
  !!
  !! Files a case writes belong under `scratch()`, which is unique to the case. Child output goes to
  !! `test/output/tests/logs/<name>.log`. Both directories are emptied at the start of each run.
  !---------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: r128=>real128, r64=>real64, r32=>real32, &
                                           i64=>int64, i32=>int32, i16=>int16, i8=>int8, output_unit
  use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_is_negative
  use io_fortran_lib,                only: String, str, operator(==)
  implicit none
  private

  public :: test_suite, RETURNS, STOPS, FIXED, KNOWN_BUG, UNDEFINED
  public :: check, fail, failure_count, expect_equal, expect_identical, scratch, write_raw, read_raw

  integer, parameter :: RETURNS = 0                                           !! Should return normally
  integer, parameter :: STOPS   = 1                                           !! Should end with `error stop`

  integer, parameter :: FIXED = 0, KNOWN_BUG = 1, UNDEFINED = 2               !! How far a case is from passing

  integer, parameter :: CHECK_FAILED = 3                                      ! Exit status of a case with failed checks

  character(len=*), parameter :: out_dir   = "test/output/tests/"
  character(len=*), parameter :: files_dir = out_dir//"files/"
  character(len=*), parameter :: logs_dir  = out_dir//"logs/"

  integer, parameter :: max_shown = 120                                       ! Longest text shown in a failure
  integer, parameter :: name_len  = 48                                        ! Longest case name

  abstract interface
    subroutine case_procedure()
    end subroutine case_procedure
  end interface

  type :: test_case
    character(len=name_len)                      :: name     = ""
    character(len=24)                            :: group    = ""
    procedure(case_procedure), pointer, nopass   :: run      => null()
    integer                                      :: expected = RETURNS
    integer                                      :: status   = FIXED
  end type test_case

  type :: test_suite
    !! A list of cases, filled by `add` and run by `run`.
    private
    type(test_case), allocatable :: cases(:)
    integer                      :: n = 0
    character(len=24)            :: current_group = ""
  contains
    procedure, public :: group
    procedure, public :: add
    procedure, public :: run
  end type test_suite

  interface expect_equal
    !! Checks that `actual` equals `expected`, and prints both if not. Reals and complex values compare exactly unless
    !! `rel_tol` is given; two NaNs count as equal, and so do the two zeros.
    module procedure :: equal_logical, equal_char, equal_string_char, equal_string, equal_string_1, &
                        equal_string_char_1, equal_char_1
    module procedure :: equal_i64, equal_i64_1
    module procedure :: equal_i32, equal_i32_1
    module procedure :: equal_i16, equal_i16_1
    module procedure :: equal_i8, equal_i8_1
#ifdef IOFL_HAS_R128
    module procedure :: equal_r128, equal_r128_1
#endif
    module procedure :: equal_r64, equal_r64_1
    module procedure :: equal_r32, equal_r32_1
#ifdef IOFL_HAS_R128
    module procedure :: equal_c128
#endif
    module procedure :: equal_c64
    module procedure :: equal_c32
  end interface expect_equal

  interface expect_identical
    !! Checks that `actual` and `expected` are the same value: NaN matches NaN, and the sign of zero counts unless
    !! `ignore_zero_sign` is true.
#ifdef IOFL_HAS_R128
    module procedure :: identical_r128, identical_r128_1
#endif
    module procedure :: identical_r64, identical_r64_1
    module procedure :: identical_r32, identical_r32_1
#ifdef IOFL_HAS_R128
    module procedure :: identical_c128, identical_c128_1
#endif
    module procedure :: identical_c64, identical_c64_1
    module procedure :: identical_c32, identical_c32_1
  end interface expect_identical

  interface shown_real
#ifdef IOFL_HAS_R128
    module procedure :: shown_real_r128
#endif
    module procedure :: shown_real_r64
    module procedure :: shown_real_r32
  end interface shown_real

  interface shown_complex
#ifdef IOFL_HAS_R128
    module procedure :: shown_complex_c128
#endif
    module procedure :: shown_complex_c64
    module procedure :: shown_complex_c32
  end interface shown_complex

  ! State of the case running in this process.
  character(len=name_len) :: current_name = ""
  integer                 :: current_expected = RETURNS
  integer                 :: failures         = 0

contains

  ! Registering and running ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  subroutine group(self, name)
    !! Sets the group for the cases added after this call.
    class(test_suite), intent(inout) :: self
    character(len=*),  intent(in)    :: name

    self%current_group = name
  end subroutine group

  subroutine add(self, name, proc, expect, status)
    !! Registers a case.
    class(test_suite), intent(inout) :: self
    character(len=*),  intent(in)    :: name
    procedure(case_procedure)        :: proc
    integer, optional, intent(in)    :: expect, status

    type(test_case), allocatable :: grown(:)
    integer                      :: i

    if ( len(name) > name_len ) error stop "Case name too long: "//name

    do i = 1, self%n
      if ( self%cases(i)%name == name ) error stop "Case registered twice: "//name
    end do

    if ( .not. allocated(self%cases) ) allocate( self%cases(64) )

    if ( self%n == size(self%cases) ) then
      allocate( grown(2*size(self%cases)) )
      grown(1:self%n) = self%cases(1:self%n)
      call move_alloc(grown, self%cases)
    end if

    self%n = self%n + 1
    self%cases(self%n)%name  = name
    self%cases(self%n)%group = self%current_group
    self%cases(self%n)%run   => proc
    if ( present(expect) ) self%cases(self%n)%expected = expect
    if ( present(status) ) self%cases(self%n)%status   = status
  end subroutine add

  subroutine run(self, title)
    !! Runs the suite as the command line asks, and ends the program with a nonzero status on any unexpected result.
    class(test_suite), intent(in) :: self
    character(len=*),  intent(in) :: title

    character(len=:), allocatable :: arg, pattern
    logical                       :: list_only, in_process
    integer                       :: i

    pattern = ""; list_only = .false.; in_process = .false.

    i = 1
    do while ( i <= command_argument_count() )
      call get_argument(i, arg)

      select case (arg)
        case ("--child")
          call get_argument(i+1, arg)
          call run_child(self, arg)
          return
        case ("--list")
          list_only = .true.
        case ("--in-process")
          in_process = .true.
        case default
          if ( arg(1:min(2,len(arg))) == "--" ) error stop "Unknown option: "//arg
          pattern = arg
      end select

      i = i + 1
    end do

    if ( list_only ) then
      call list_cases(self, pattern)
    else if ( in_process ) then
      call run_in_process(self, title, pattern)
    else
      call run_children(self, title, pattern)
    end if
  end subroutine run

  logical function matches(c, pattern)
    type(test_case),  intent(in) :: c
    character(len=*), intent(in) :: pattern

    matches = index(full_name(c), pattern) > 0
  end function matches

  function full_name(c) result(name)
    type(test_case), intent(in)   :: c
    character(len=:), allocatable :: name

    name = trim(c%group)//"/"//trim(c%name)
  end function full_name

  subroutine list_cases(self, pattern)
    type(test_suite), intent(in) :: self
    character(len=*), intent(in) :: pattern

    integer :: i

    do i = 1, self%n
      if ( .not. matches(self%cases(i), pattern) ) cycle
      write(*,"(a)") full_name(self%cases(i))//trim(markers(self%cases(i)))
    end do
  end subroutine list_cases

  function markers(c) result(text)
    type(test_case), intent(in)   :: c
    character(len=:), allocatable :: text

    text = ""
    if ( c%expected == STOPS )     text = text//"  [stops]"
    if ( c%status == KNOWN_BUG )   text = text//"  [known bug]"
    if ( c%status == UNDEFINED )   text = text//"  [undefined]"
  end function markers

  subroutine run_child(self, name)
    !! Runs one case, named exactly, and exits with status `CHECK_FAILED` if a check failed.
    type(test_suite), intent(in) :: self
    character(len=*), intent(in) :: name

    integer :: i

    do i = 1, self%n
      if ( self%cases(i)%name == name ) then
        call run_one(self%cases(i))
        if ( failures > 0 ) stop CHECK_FAILED
        return
      end if
    end do

    error stop "Unknown case: "//name
  end subroutine run_child

  subroutine run_one(c)
    type(test_case), intent(in) :: c

    current_name     = c%name
    current_expected = c%expected
    failures         = 0

    call c%run()
  end subroutine run_one

  subroutine run_children(self, title, pattern)
    type(test_suite), intent(in) :: self
    character(len=*), intent(in) :: title, pattern

    character(len=:), allocatable :: me, log, command
    integer :: i, status, cmdstat, n_run, n_passed, n_known, n_unexpected
    logical :: met

    call get_argument(0, me)
    call fresh_output()

    n_run = 0; n_passed = 0; n_known = 0; n_unexpected = 0

    do i = 1, self%n
      associate( c => self%cases(i) )
        if ( .not. matches(c, pattern) ) cycle
        n_run = n_run + 1

        log = logs_dir//trim(c%name)//".log"
        command = shell_word(me)//" --child "//trim(c%name)//" > "//shell_word(log)//" 2>&1"

        status = -1; cmdstat = 0
        call execute_command_line(command, wait=.true., exitstat=status, cmdstat=cmdstat)
        if ( cmdstat /= 0 ) error stop "Could not launch a child process for case "//trim(c%name)//"."

        if ( c%expected == STOPS ) then
          met = (status == 1) .or. (status == 128)                   ! `error stop`: 1 on gfortran and flang, 128 on ifx
          if ( met ) met = is_readable(log)                          ! The message has to be of use to someone
        else
          met = (status == 0)
        end if

        if ( met .and. (c%status == FIXED) ) then
          n_passed = n_passed + 1
        else if ( c%status == UNDEFINED ) then
          n_known = n_known + 1
        else if ( (.not. met) .and. (c%status == KNOWN_BUG) ) then
          n_known = n_known + 1
        else
          n_unexpected = n_unexpected + 1

          if ( met ) then
            write(*,"(a)") "UNEXPECTED PASS: "//full_name(c)//" (fixed? remove its KNOWN_BUG marker)"
          else
            write(*,"(a,i0,a)") "UNEXPECTED FAILURE: "//full_name(c)//" (exit status ", status, &
                                expectation(c)//"), which printed:"
            call print_file(log)
          end if
        end if
      end associate
    end do

    call summary(title, n_run, n_passed, n_known, n_unexpected)
  end subroutine run_children

  function expectation(c) result(text)
    type(test_case), intent(in)   :: c
    character(len=:), allocatable :: text

    if ( c%expected == STOPS ) then
      text = "; it should end with error stop and a readable message"
    else
      text = "; it should return normally"
    end if
  end function expectation

  subroutine run_in_process(self, title, pattern)
    type(test_suite), intent(in) :: self
    character(len=*), intent(in) :: title, pattern

    integer :: i, n_match, n_run, n_passed, n_known, n_unexpected

    call fresh_output()

    n_match = count([( matches(self%cases(i), pattern), i = 1, self%n )])
    n_run = 0; n_passed = 0; n_known = 0; n_unexpected = 0

    do i = 1, self%n
      associate( c => self%cases(i) )
        if ( .not. matches(c, pattern) ) cycle

        if ( (c%expected == STOPS) .and. (n_match > 1) ) then
          write(*,"(a)") "SKIPPED: "//full_name(c)//" (it stops the program; run it on its own)"
          cycle
        end if

        n_run = n_run + 1
        write(*,"(a)") "RUNNING: "//full_name(c)
        flush(output_unit)
        call run_one(c)

        if ( (failures == 0) .and. (c%status == FIXED) ) then
          n_passed = n_passed + 1
        else if ( (c%status == UNDEFINED) .or. ((failures > 0) .and. (c%status == KNOWN_BUG)) ) then
          n_known = n_known + 1
        else
          n_unexpected = n_unexpected + 1
          if ( failures == 0 ) write(*,"(a)") "UNEXPECTED PASS: "//full_name(c)//" (fixed? remove its KNOWN_BUG marker)"
        end if
      end associate
    end do

    call summary(title, n_run, n_passed, n_known, n_unexpected)
  end subroutine run_in_process

  subroutine summary(title, n_run, n_passed, n_known, n_unexpected)
    character(len=*), intent(in) :: title
    integer,          intent(in) :: n_run, n_passed, n_known, n_unexpected

    write(*,"(a,4(i0,a))") title//": ", n_run, " cases, ", n_passed, " passed, ", n_known, " known bugs, ", &
                           n_unexpected, " unexpected"

    if ( n_run == 0 ) error stop "No case matches the pattern."
    if ( n_unexpected > 0 ) error stop "Some cases gave unexpected results."
  end subroutine summary

  ! Checks ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  subroutine check(condition, message)
    !! Records a failure if `condition` is false. The case carries on, so that one run reports every failed check, but a
    !! case that should stop ends at once, since its `error stop` would otherwise hide the failure.
    logical,          intent(in) :: condition
    character(len=*), intent(in) :: message

    if ( condition ) return
    call fail(message)
  end subroutine check

  integer function failure_count()
    !! Returns the number of failed checks so far in the running case, so that a loop can stop at its first failure.
    failure_count = failures
  end function failure_count

  subroutine fail(message, actual, expected)
    !! Records a failure, showing `actual` and `expected` if present.
    character(len=*),           intent(in) :: message
    character(len=*), optional, intent(in) :: actual, expected

    failures = failures + 1
    write(*,"(a)") "CHECK FAILED: "//message
    if ( present(actual) )   write(*,"(a)") "    actual:   "//actual
    if ( present(expected) ) write(*,"(a)") "    expected: "//expected
    flush(output_unit)

    if ( current_expected == STOPS ) stop CHECK_FAILED
  end subroutine fail

  subroutine equal_i64(actual, expected, message)
    integer(i64),     intent(in) :: actual, expected
    character(len=*), intent(in) :: message
    if ( actual /= expected ) call fail(message, str(actual), str(expected))
  end subroutine equal_i64

  subroutine equal_i64_1(actual, expected, message)
    integer(i64),     intent(in) :: actual(:), expected(:)
    character(len=*), intent(in) :: message
    integer :: i
    if ( .not. same_size(size(actual), size(expected), message) ) return
    do i = 1, size(actual)
      if ( actual(i) /= expected(i) ) then
        call fail(message//at(i), str(actual(i)), str(expected(i)))
        return
      end if
    end do
  end subroutine equal_i64_1

  subroutine equal_i32(actual, expected, message)
    integer(i32),     intent(in) :: actual, expected
    character(len=*), intent(in) :: message
    if ( actual /= expected ) call fail(message, str(actual), str(expected))
  end subroutine equal_i32

  subroutine equal_i32_1(actual, expected, message)
    integer(i32),     intent(in) :: actual(:), expected(:)
    character(len=*), intent(in) :: message
    integer :: i
    if ( .not. same_size(size(actual), size(expected), message) ) return
    do i = 1, size(actual)
      if ( actual(i) /= expected(i) ) then
        call fail(message//at(i), str(actual(i)), str(expected(i)))
        return
      end if
    end do
  end subroutine equal_i32_1

  subroutine equal_i16(actual, expected, message)
    integer(i16),     intent(in) :: actual, expected
    character(len=*), intent(in) :: message
    if ( actual /= expected ) call fail(message, str(actual), str(expected))
  end subroutine equal_i16

  subroutine equal_i16_1(actual, expected, message)
    integer(i16),     intent(in) :: actual(:), expected(:)
    character(len=*), intent(in) :: message
    integer :: i
    if ( .not. same_size(size(actual), size(expected), message) ) return
    do i = 1, size(actual)
      if ( actual(i) /= expected(i) ) then
        call fail(message//at(i), str(actual(i)), str(expected(i)))
        return
      end if
    end do
  end subroutine equal_i16_1

  subroutine equal_i8(actual, expected, message)
    integer(i8),     intent(in) :: actual, expected
    character(len=*), intent(in) :: message
    if ( actual /= expected ) call fail(message, str(actual), str(expected))
  end subroutine equal_i8

  subroutine equal_i8_1(actual, expected, message)
    integer(i8),     intent(in) :: actual(:), expected(:)
    character(len=*), intent(in) :: message
    integer :: i
    if ( .not. same_size(size(actual), size(expected), message) ) return
    do i = 1, size(actual)
      if ( actual(i) /= expected(i) ) then
        call fail(message//at(i), str(actual(i)), str(expected(i)))
        return
      end if
    end do
  end subroutine equal_i8_1

  subroutine equal_logical(actual, expected, message)
    logical,          intent(in) :: actual, expected
    character(len=*), intent(in) :: message
    if ( actual .neqv. expected ) call fail(message, merge("true ", "false", actual), merge("true ", "false", expected))
  end subroutine equal_logical

  subroutine equal_char(actual, expected, message)
    character(len=*), intent(in) :: actual, expected
    character(len=*), intent(in) :: message
    ! Compare lengths too: intrinsic `==` pads the shorter operand with blanks.
    if ( (len(actual) /= len(expected)) .or. (actual /= expected) ) call fail(message, shown(actual), shown(expected))
  end subroutine equal_char

  subroutine equal_string_char(actual, expected, message)
    type(String),     intent(in) :: actual
    character(len=*), intent(in) :: expected
    character(len=*), intent(in) :: message
    if ( .not. (actual == expected) ) call fail(message, shown(actual%as_str()), shown(expected))
  end subroutine equal_string_char

  subroutine equal_string(actual, expected, message)
    type(String),     intent(in) :: actual, expected
    character(len=*), intent(in) :: message
    if ( .not. (actual == expected) ) call fail(message, shown(actual%as_str()), shown(expected%as_str()))
  end subroutine equal_string

  subroutine equal_string_1(actual, expected, message)
    type(String),     intent(in) :: actual(:), expected(:)
    character(len=*), intent(in) :: message
    integer :: i
    if ( .not. same_size(size(actual), size(expected), message) ) return
    do i = 1, size(actual)
      if ( .not. (actual(i) == expected(i)) ) then
        call fail(message//at(i), shown(actual(i)%as_str()), shown(expected(i)%as_str()))
        return
      end if
    end do
  end subroutine equal_string_1

  subroutine equal_string_char_1(actual, expected, message)
    type(String),     intent(in) :: actual(:)
    character(len=*), intent(in) :: expected(:)
    character(len=*), intent(in) :: message
    integer :: i
    if ( .not. same_size(size(actual), size(expected), message) ) return
    do i = 1, size(actual)
      if ( .not. (actual(i) == expected(i)) ) then
        call fail(message//at(i), shown(actual(i)%as_str()), shown(expected(i)))
        return
      end if
    end do
  end subroutine equal_string_char_1

  subroutine equal_char_1(actual, expected, message)
    character(len=*), intent(in) :: actual(:), expected(:)
    character(len=*), intent(in) :: message
    integer :: i
    if ( .not. same_size(size(actual), size(expected), message) ) return
    if ( len(actual) /= len(expected) ) then
      call fail(message//" (element lengths differ)", str(len(actual)), str(len(expected)))
      return
    end if
    do i = 1, size(actual)
      if ( actual(i) /= expected(i) ) then
        call fail(message//at(i), shown(actual(i)), shown(expected(i)))
        return
      end if
    end do
  end subroutine equal_char_1

#ifdef IOFL_HAS_R128
  subroutine equal_r128(actual, expected, message, rel_tol)
    real(r128),           intent(in) :: actual, expected
    character(len=*),    intent(in) :: message
    real(r128), optional, intent(in) :: rel_tol
    if ( .not. close_r128(actual, expected, rel_tol) ) call fail(message, shown_real(actual), shown_real(expected))
  end subroutine equal_r128

  subroutine equal_r128_1(actual, expected, message, rel_tol)
    real(r128),           intent(in) :: actual(:), expected(:)
    character(len=*),    intent(in) :: message
    real(r128), optional, intent(in) :: rel_tol
    integer :: i
    if ( .not. same_size(size(actual), size(expected), message) ) return
    do i = 1, size(actual)
      if ( .not. close_r128(actual(i), expected(i), rel_tol) ) then
        call fail(message//at(i), shown_real(actual(i)), shown_real(expected(i)))
        return
      end if
    end do
  end subroutine equal_r128_1

  subroutine identical_r128(actual, expected, message, ignore_zero_sign)
    real(r128),        intent(in) :: actual, expected
    character(len=*), intent(in) :: message
    logical,          intent(in), optional :: ignore_zero_sign
    if ( .not. same_r128(actual, expected, ignore_zero_sign) ) then
      call fail(message, shown_real(actual), shown_real(expected))
    end if
  end subroutine identical_r128

  subroutine identical_r128_1(actual, expected, message, ignore_zero_sign)
    real(r128),        intent(in) :: actual(:), expected(:)
    character(len=*), intent(in) :: message
    logical,          intent(in), optional :: ignore_zero_sign
    integer :: i
    if ( .not. same_size(size(actual), size(expected), message) ) return
    do i = 1, size(actual)
      if ( .not. same_r128(actual(i), expected(i), ignore_zero_sign) ) then
        call fail(message//at(i), shown_real(actual(i)), shown_real(expected(i)))
        return
      end if
    end do
  end subroutine identical_r128_1

  logical function close_r128(actual, expected, rel_tol)
    real(r128),           intent(in) :: actual, expected
    real(r128), optional, intent(in) :: rel_tol

    if ( ieee_is_nan(actual) .or. ieee_is_nan(expected) ) then
      close_r128 = ieee_is_nan(actual) .and. ieee_is_nan(expected)
    else if ( present(rel_tol) ) then
      close_r128 = (actual == expected) .or. (abs(actual - expected) <= rel_tol*abs(expected))
    else
      close_r128 = (actual == expected)
    end if
  end function close_r128

  logical function same_r128(actual, expected, ignore_zero_sign)
    !! Whether two values are the same: NaN matches NaN, and the sign of zero counts unless `ignore_zero_sign`.
    real(r128),        intent(in) :: actual, expected
    logical,          intent(in), optional :: ignore_zero_sign

    if ( ieee_is_nan(actual) .or. ieee_is_nan(expected) ) then
      same_r128 = ieee_is_nan(actual) .and. ieee_is_nan(expected)
    else
      same_r128 = (actual == expected)
      if ( same_r128 .and. (actual == 0.0_r128) .and. (.not. optval(ignore_zero_sign)) ) then
        same_r128 = (ieee_is_negative(actual) .eqv. ieee_is_negative(expected))
      end if
    end if
  end function same_r128

  function shown_real_r128(x) result(text)
    real(r128), intent(in)          :: x
    character(len=:), allocatable :: text

    text = str(x, fmt="e")//" ("//str(x, fmt="z")//")"
  end function shown_real_r128
#endif

  subroutine equal_r64(actual, expected, message, rel_tol)
    real(r64),           intent(in) :: actual, expected
    character(len=*),    intent(in) :: message
    real(r64), optional, intent(in) :: rel_tol
    if ( .not. close_r64(actual, expected, rel_tol) ) call fail(message, shown_real(actual), shown_real(expected))
  end subroutine equal_r64

  subroutine equal_r64_1(actual, expected, message, rel_tol)
    real(r64),           intent(in) :: actual(:), expected(:)
    character(len=*),    intent(in) :: message
    real(r64), optional, intent(in) :: rel_tol
    integer :: i
    if ( .not. same_size(size(actual), size(expected), message) ) return
    do i = 1, size(actual)
      if ( .not. close_r64(actual(i), expected(i), rel_tol) ) then
        call fail(message//at(i), shown_real(actual(i)), shown_real(expected(i)))
        return
      end if
    end do
  end subroutine equal_r64_1

  subroutine identical_r64(actual, expected, message, ignore_zero_sign)
    real(r64),        intent(in) :: actual, expected
    character(len=*), intent(in) :: message
    logical,          intent(in), optional :: ignore_zero_sign
    if ( .not. same_r64(actual, expected, ignore_zero_sign) ) then
      call fail(message, shown_real(actual), shown_real(expected))
    end if
  end subroutine identical_r64

  subroutine identical_r64_1(actual, expected, message, ignore_zero_sign)
    real(r64),        intent(in) :: actual(:), expected(:)
    character(len=*), intent(in) :: message
    logical,          intent(in), optional :: ignore_zero_sign
    integer :: i
    if ( .not. same_size(size(actual), size(expected), message) ) return
    do i = 1, size(actual)
      if ( .not. same_r64(actual(i), expected(i), ignore_zero_sign) ) then
        call fail(message//at(i), shown_real(actual(i)), shown_real(expected(i)))
        return
      end if
    end do
  end subroutine identical_r64_1

  logical function close_r64(actual, expected, rel_tol)
    real(r64),           intent(in) :: actual, expected
    real(r64), optional, intent(in) :: rel_tol

    if ( ieee_is_nan(actual) .or. ieee_is_nan(expected) ) then
      close_r64 = ieee_is_nan(actual) .and. ieee_is_nan(expected)
    else if ( present(rel_tol) ) then
      close_r64 = (actual == expected) .or. (abs(actual - expected) <= rel_tol*abs(expected))
    else
      close_r64 = (actual == expected)
    end if
  end function close_r64

  logical function same_r64(actual, expected, ignore_zero_sign)
    !! Whether two values are the same: NaN matches NaN, and the sign of zero counts unless `ignore_zero_sign`.
    real(r64),        intent(in) :: actual, expected
    logical,          intent(in), optional :: ignore_zero_sign

    if ( ieee_is_nan(actual) .or. ieee_is_nan(expected) ) then
      same_r64 = ieee_is_nan(actual) .and. ieee_is_nan(expected)
    else
      same_r64 = (actual == expected)
      if ( same_r64 .and. (actual == 0.0_r64) .and. (.not. optval(ignore_zero_sign)) ) then
        same_r64 = (ieee_is_negative(actual) .eqv. ieee_is_negative(expected))
      end if
    end if
  end function same_r64

  function shown_real_r64(x) result(text)
    real(r64), intent(in)          :: x
    character(len=:), allocatable :: text

    text = str(x, fmt="e")//" ("//str(x, fmt="z")//")"
  end function shown_real_r64

  subroutine equal_r32(actual, expected, message, rel_tol)
    real(r32),           intent(in) :: actual, expected
    character(len=*),    intent(in) :: message
    real(r32), optional, intent(in) :: rel_tol
    if ( .not. close_r32(actual, expected, rel_tol) ) call fail(message, shown_real(actual), shown_real(expected))
  end subroutine equal_r32

  subroutine equal_r32_1(actual, expected, message, rel_tol)
    real(r32),           intent(in) :: actual(:), expected(:)
    character(len=*),    intent(in) :: message
    real(r32), optional, intent(in) :: rel_tol
    integer :: i
    if ( .not. same_size(size(actual), size(expected), message) ) return
    do i = 1, size(actual)
      if ( .not. close_r32(actual(i), expected(i), rel_tol) ) then
        call fail(message//at(i), shown_real(actual(i)), shown_real(expected(i)))
        return
      end if
    end do
  end subroutine equal_r32_1

  subroutine identical_r32(actual, expected, message, ignore_zero_sign)
    real(r32),        intent(in) :: actual, expected
    character(len=*), intent(in) :: message
    logical,          intent(in), optional :: ignore_zero_sign
    if ( .not. same_r32(actual, expected, ignore_zero_sign) ) then
      call fail(message, shown_real(actual), shown_real(expected))
    end if
  end subroutine identical_r32

  subroutine identical_r32_1(actual, expected, message, ignore_zero_sign)
    real(r32),        intent(in) :: actual(:), expected(:)
    character(len=*), intent(in) :: message
    logical,          intent(in), optional :: ignore_zero_sign
    integer :: i
    if ( .not. same_size(size(actual), size(expected), message) ) return
    do i = 1, size(actual)
      if ( .not. same_r32(actual(i), expected(i), ignore_zero_sign) ) then
        call fail(message//at(i), shown_real(actual(i)), shown_real(expected(i)))
        return
      end if
    end do
  end subroutine identical_r32_1

  logical function close_r32(actual, expected, rel_tol)
    real(r32),           intent(in) :: actual, expected
    real(r32), optional, intent(in) :: rel_tol

    if ( ieee_is_nan(actual) .or. ieee_is_nan(expected) ) then
      close_r32 = ieee_is_nan(actual) .and. ieee_is_nan(expected)
    else if ( present(rel_tol) ) then
      close_r32 = (actual == expected) .or. (abs(actual - expected) <= rel_tol*abs(expected))
    else
      close_r32 = (actual == expected)
    end if
  end function close_r32

  logical function same_r32(actual, expected, ignore_zero_sign)
    !! Whether two values are the same: NaN matches NaN, and the sign of zero counts unless `ignore_zero_sign`.
    real(r32),        intent(in) :: actual, expected
    logical,          intent(in), optional :: ignore_zero_sign

    if ( ieee_is_nan(actual) .or. ieee_is_nan(expected) ) then
      same_r32 = ieee_is_nan(actual) .and. ieee_is_nan(expected)
    else
      same_r32 = (actual == expected)
      if ( same_r32 .and. (actual == 0.0_r32) .and. (.not. optval(ignore_zero_sign)) ) then
        same_r32 = (ieee_is_negative(actual) .eqv. ieee_is_negative(expected))
      end if
    end if
  end function same_r32

  function shown_real_r32(x) result(text)
    real(r32), intent(in)          :: x
    character(len=:), allocatable :: text

    text = str(x, fmt="e")//" ("//str(x, fmt="z")//")"
  end function shown_real_r32

#ifdef IOFL_HAS_R128
  subroutine equal_c128(actual, expected, message, rel_tol)
    complex(r128),        intent(in) :: actual, expected
    character(len=*),    intent(in) :: message
    real(r128), optional, intent(in) :: rel_tol
    if ( .not. (close_r128(real(actual), real(expected), rel_tol) .and. &
                close_r128(aimag(actual), aimag(expected), rel_tol)) ) then
      call fail(message, shown_complex(actual), shown_complex(expected))
    end if
  end subroutine equal_c128

  subroutine identical_c128(actual, expected, message, ignore_zero_sign)
    complex(r128),     intent(in) :: actual, expected
    character(len=*), intent(in) :: message
    logical,          intent(in), optional :: ignore_zero_sign
    if ( .not. (same_r128(real(actual), real(expected), ignore_zero_sign) .and. &
                same_r128(aimag(actual), aimag(expected), ignore_zero_sign)) ) then
      call fail(message, shown_complex(actual), shown_complex(expected))
    end if
  end subroutine identical_c128

  subroutine identical_c128_1(actual, expected, message, ignore_zero_sign)
    complex(r128),     intent(in) :: actual(:), expected(:)
    character(len=*), intent(in) :: message
    logical,          intent(in), optional :: ignore_zero_sign
    integer :: i
    if ( .not. same_size(size(actual), size(expected), message) ) return
    do i = 1, size(actual)
      if ( .not. (same_r128(real(actual(i)), real(expected(i)), ignore_zero_sign) .and. &
                  same_r128(aimag(actual(i)), aimag(expected(i)), ignore_zero_sign)) ) then
        call fail(message//at(i), shown_complex(actual(i)), shown_complex(expected(i)))
        return
      end if
    end do
  end subroutine identical_c128_1

  function shown_complex_c128(z) result(text)
    complex(r128), intent(in)       :: z
    character(len=:), allocatable :: text

    text = "("//shown_real(real(z))//", "//shown_real(aimag(z))//")"
  end function shown_complex_c128
#endif

  subroutine equal_c64(actual, expected, message, rel_tol)
    complex(r64),        intent(in) :: actual, expected
    character(len=*),    intent(in) :: message
    real(r64), optional, intent(in) :: rel_tol
    if ( .not. (close_r64(real(actual), real(expected), rel_tol) .and. &
                close_r64(aimag(actual), aimag(expected), rel_tol)) ) then
      call fail(message, shown_complex(actual), shown_complex(expected))
    end if
  end subroutine equal_c64

  subroutine identical_c64(actual, expected, message, ignore_zero_sign)
    complex(r64),     intent(in) :: actual, expected
    character(len=*), intent(in) :: message
    logical,          intent(in), optional :: ignore_zero_sign
    if ( .not. (same_r64(real(actual), real(expected), ignore_zero_sign) .and. &
                same_r64(aimag(actual), aimag(expected), ignore_zero_sign)) ) then
      call fail(message, shown_complex(actual), shown_complex(expected))
    end if
  end subroutine identical_c64

  subroutine identical_c64_1(actual, expected, message, ignore_zero_sign)
    complex(r64),     intent(in) :: actual(:), expected(:)
    character(len=*), intent(in) :: message
    logical,          intent(in), optional :: ignore_zero_sign
    integer :: i
    if ( .not. same_size(size(actual), size(expected), message) ) return
    do i = 1, size(actual)
      if ( .not. (same_r64(real(actual(i)), real(expected(i)), ignore_zero_sign) .and. &
                  same_r64(aimag(actual(i)), aimag(expected(i)), ignore_zero_sign)) ) then
        call fail(message//at(i), shown_complex(actual(i)), shown_complex(expected(i)))
        return
      end if
    end do
  end subroutine identical_c64_1

  function shown_complex_c64(z) result(text)
    complex(r64), intent(in)       :: z
    character(len=:), allocatable :: text

    text = "("//shown_real(real(z))//", "//shown_real(aimag(z))//")"
  end function shown_complex_c64

  subroutine equal_c32(actual, expected, message, rel_tol)
    complex(r32),        intent(in) :: actual, expected
    character(len=*),    intent(in) :: message
    real(r32), optional, intent(in) :: rel_tol
    if ( .not. (close_r32(real(actual), real(expected), rel_tol) .and. &
                close_r32(aimag(actual), aimag(expected), rel_tol)) ) then
      call fail(message, shown_complex(actual), shown_complex(expected))
    end if
  end subroutine equal_c32

  subroutine identical_c32(actual, expected, message, ignore_zero_sign)
    complex(r32),     intent(in) :: actual, expected
    character(len=*), intent(in) :: message
    logical,          intent(in), optional :: ignore_zero_sign
    if ( .not. (same_r32(real(actual), real(expected), ignore_zero_sign) .and. &
                same_r32(aimag(actual), aimag(expected), ignore_zero_sign)) ) then
      call fail(message, shown_complex(actual), shown_complex(expected))
    end if
  end subroutine identical_c32

  subroutine identical_c32_1(actual, expected, message, ignore_zero_sign)
    complex(r32),     intent(in) :: actual(:), expected(:)
    character(len=*), intent(in) :: message
    logical,          intent(in), optional :: ignore_zero_sign
    integer :: i
    if ( .not. same_size(size(actual), size(expected), message) ) return
    do i = 1, size(actual)
      if ( .not. (same_r32(real(actual(i)), real(expected(i)), ignore_zero_sign) .and. &
                  same_r32(aimag(actual(i)), aimag(expected(i)), ignore_zero_sign)) ) then
        call fail(message//at(i), shown_complex(actual(i)), shown_complex(expected(i)))
        return
      end if
    end do
  end subroutine identical_c32_1

  function shown_complex_c32(z) result(text)
    complex(r32), intent(in)       :: z
    character(len=:), allocatable :: text

    text = "("//shown_real(real(z))//", "//shown_real(aimag(z))//")"
  end function shown_complex_c32

  logical function same_size(n_actual, n_expected, message)
    integer,          intent(in) :: n_actual, n_expected
    character(len=*), intent(in) :: message

    same_size = (n_actual == n_expected)
    if ( .not. same_size ) call fail(message//" (sizes differ)", str(n_actual), str(n_expected))
  end function same_size

  function at(i) result(text)
    integer, intent(in)           :: i
    character(len=:), allocatable :: text

    text = " (first difference at element "//str(i)//")"
  end function at

  logical function optval(x)
    logical, intent(in), optional :: x

    optval = .false.
    if ( present(x) ) optval = x
  end function optval

  function shown(text) result(quoted)
    !! Returns `text` in quotes with control characters escaped, cut short if long, for a failure message.
    character(len=*), intent(in)  :: text
    character(len=:), allocatable :: quoted

    character(len=*), parameter :: hex = "0123456789ABCDEF"
    integer :: i, code

    quoted = '"'
    do i = 1, min(len(text), max_shown)
      code = iachar(text(i:i))
      select case (code)
        case (9)
          quoted = quoted//"\t"
        case (10)
          quoted = quoted//"\n"
        case (13)
          quoted = quoted//"\r"
        case (34)
          quoted = quoted//'\"'
        case (92)
          quoted = quoted//"\\"
        case (32:33, 35:91, 93:126)
          quoted = quoted//text(i:i)
        case default
          quoted = quoted//"\x"//hex(code/16+1:code/16+1)//hex(mod(code,16)+1:mod(code,16)+1)
      end select
    end do
    quoted = quoted//'"'

    if ( len(text) > max_shown ) quoted = quoted//"... ("//str(len(text))//" characters)"
  end function shown

  ! Files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  function scratch() result(path)
    !! Returns a path prefix unique to the running case, such as `test/output/tests/files/read_crlf`. Add a suffix and
    !! an extension to name a file: `scratch()//".csv"`, `scratch()//"_out.log"`.
    character(len=:), allocatable :: path

    path = files_dir//trim(current_name)
  end function scratch

  subroutine write_raw(file, content)
    !! Writes `content` to `file` byte for byte, replacing the file.
    character(len=*), intent(in) :: file, content

    integer :: u

    open(newunit=u, file=file, access="stream", form="unformatted", status="replace")
    write(u) content
    close(u)
  end subroutine write_raw

  function read_raw(file) result(content)
    !! Returns the bytes of `file`.
    character(len=*), intent(in)  :: file
    character(len=:), allocatable :: content

    integer :: n, u

    inquire(file=file, size=n)
    allocate( character(len=max(n,0)) :: content )
    if ( n <= 0 ) return

    open(newunit=u, file=file, access="stream", form="unformatted", status="old", action="read")
    read(u) content
    close(u)
  end function read_raw

  subroutine fresh_output()
    !! Empties the output directories, so that no case sees files from an earlier run.
    integer :: status, cmdstat

    if ( on_windows() ) then
      call execute_command_line("(if exist "//windows_path(out_dir)//" rmdir /s /q "//windows_path(out_dir)//") & " // &
                                "mkdir "//windows_path(files_dir)//" "//windows_path(logs_dir), &
                                wait=.true., exitstat=status, cmdstat=cmdstat)
    else
      call execute_command_line("rm -rf "//out_dir//" && mkdir -p "//files_dir//" "//logs_dir, &
                                wait=.true., exitstat=status, cmdstat=cmdstat)
    end if

    if ( (cmdstat /= 0) .or. (status /= 0) ) error stop "Could not create "//out_dir//"."
  end subroutine fresh_output

  logical function on_windows()
    character(len=16) :: os
    integer           :: status

    call get_environment_variable("OS", os, status=status)
    on_windows = (status == 0) .and. (os == "Windows_NT")
  end function on_windows

  function windows_path(path) result(native)
    !! Returns `path` with backslashes and no trailing separator, as cmd.exe expects.
    character(len=*), intent(in)  :: path
    character(len=:), allocatable :: native

    integer :: i

    native = path
    do i = 1, len(native)
      if ( native(i:i) == "/" ) native(i:i) = achar(92)
    end do
    if ( native(len(native):) == achar(92) ) native = native(:len(native)-1)
  end function windows_path

  function shell_word(path) result(word)
    !! Returns `path` for a command line. It is quoted only if it holds a space, since cmd.exe drops the first and last
    !! quotes of a command that starts with one. On Windows, slashes become backslashes.
    character(len=*), intent(in)  :: path
    character(len=:), allocatable :: word

    integer :: i

    word = path

    if ( on_windows() ) then
      do i = 1, len(word)
        if ( word(i:i) == "/" ) word(i:i) = achar(92)
      end do
    end if

    if ( index(word, " ") > 0 ) word = '"'//word//'"'
  end function shell_word

  subroutine print_file(file)
    character(len=*), intent(in) :: file

    write(*,"(a)") read_raw(file)
  end subroutine print_file

  logical function is_readable(file)
    !! Checks that the first line with text in a file is printable, since a compiler bug can garble a stop message.
    !! Later lines are left alone, as they may hold a backtrace.
    character(len=*), intent(in) :: file

    character(len=:), allocatable :: text
    integer :: first, i

    text = read_raw(file)

    is_readable = .false.
    first = verify(text, achar(13)//achar(10))
    if ( first == 0 ) return

    do i = first, len(text)
      if ( (text(i:i) == achar(13)) .or. (text(i:i) == achar(10)) ) exit
      if ( (iachar(text(i:i)) < 32) .or. (iachar(text(i:i)) > 126) ) return
    end do

    is_readable = .true.
  end function is_readable

  subroutine get_argument(number, value)
    integer,                       intent(in)  :: number
    character(len=:), allocatable, intent(out) :: value

    integer :: n

    call get_command_argument(number, length=n)
    allocate( character(len=n) :: value )
    if ( n > 0 ) call get_command_argument(number, value)
  end subroutine get_argument
end module test_support
