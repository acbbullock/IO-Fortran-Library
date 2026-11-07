program regression_test
  !---------------------------------------------------------------------------------------------------------------------
  !! Regression tests for known bugs, one case per bug, stating the behavior the library should have.
  !!
  !! Many of these bugs crash the process, so every case runs in a child process: with no arguments this program runs
  !! each case by launching itself with the case name, and judges the case by the exit status.
  !!
  !! A case marked `KNOWN_BUG` is an expected failure. If it starts passing the suite fails, so that the marker is
  !! removed in the same change as the fix. A case marked `UNDEFINED` is a known bug with undefined behavior, such as an
  !! out-of-bounds access, which can appear to work at some optimization levels, so either result is accepted. Replace
  !! `UNDEFINED` with `FIXED` in the change that fixes the bug. Child output is kept in
  !! `test/output/regression_<case>.log`.
  !!
  !! A case with `STOPS` should end the program with `error stop`, because an error with no `stat` argument to report it
  !! through must not go unnoticed. That behavior is planned for version 2.0.
  !---------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: real32, real64, int8, int32, compiler_version, compiler_options
  use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_positive_inf, ieee_is_nan, &
                                             ieee_is_negative
  use io_fortran_lib, only: String, cast, str, join, echo, aprint, to_file, from_file, operator(==), operator(**), &
                            operator(+), LF, CR, TAB
  implicit none (type, external)

  character(len=*), parameter :: dir = "./test/output/"

  integer, parameter :: RETURNS = 0                                           ! Should return normally: exit status 0
  integer, parameter :: STOPS = 1                                             ! Should end with `error stop`
  integer, parameter :: CHECK_FAILED = 3                                      ! Exit status of a failed check

  integer, parameter :: FIXED = 0, KNOWN_BUG = 1, UNDEFINED = 2               ! Status of a case

  type :: test_case
    character(len=32) :: name
    integer           :: expected
    integer           :: status
  end type test_case

  type(test_case), parameter :: cases(*) = [ &
    ! Harness self checks: these confirm that the exit status logic works on the current compiler.
    test_case("self_returns",                RETURNS, FIXED),     &
    test_case("self_stops",                  STOPS,   FIXED),     &
    test_case("self_failed_check",           RETURNS, KNOWN_BUG), &
    ! An error with no `stat` must not go unnoticed.
    test_case("to_file_bad_extension",       STOPS,   KNOWN_BUG), &
    test_case("from_file_missing",           STOPS,   KNOWN_BUG), &
    test_case("cast_int_letters",            STOPS,   KNOWN_BUG), &
    test_case("cast_int_trailing_junk",      STOPS,   KNOWN_BUG), &
    test_case("cast_int_decimal_point",      STOPS,   KNOWN_BUG), &
    test_case("cast_int32_overflow",         STOPS,   KNOWN_BUG), &
    test_case("cast_int32_just_over_range",  STOPS,   KNOWN_BUG), &
    test_case("cast_int8_just_over_range",   STOPS,   KNOWN_BUG), &
    test_case("cast_real_letters",           STOPS,   KNOWN_BUG), &
    test_case("cast_real_empty",             STOPS,   KNOWN_BUG), &
    test_case("cast_real_repeat_count",      STOPS,   KNOWN_BUG), &
    test_case("cast_real_two_values",        STOPS,   KNOWN_BUG), &
    test_case("cast_real_trailing_junk",     STOPS,   KNOWN_BUG), &
    test_case("cast_real_slash",             STOPS,   KNOWN_BUG), &
    test_case("str_invalid_fmt",             STOPS,   FIXED),     &
    test_case("str_invalid_locale",          STOPS,   FIXED),     &
    ! Errors must be reported through `stat`, and absent optional arguments must not be touched.
    test_case("from_file_bad_number_stat",   RETURNS, KNOWN_BUG), &
    test_case("to_file_scalar_binary",       RETURNS, UNDEFINED), &
    ! Reading and writing delimited text.
    test_case("read_no_final_newline",       RETURNS, KNOWN_BUG), &
    test_case("read_one_line_no_newline",    RETURNS, UNDEFINED), &
    test_case("read_crlf",                   RETURNS, KNOWN_BUG), &
    test_case("read_row_wider_than_first",   RETURNS, UNDEFINED), &
    test_case("read_row_narrower_than_first",RETURNS, KNOWN_BUG), &
    test_case("read_quoted_newline",         RETURNS, KNOWN_BUG), &
    test_case("read_empty_file_closes_unit", RETURNS, KNOWN_BUG), &
    test_case("write_unallocated_cell",      RETURNS, UNDEFINED), &
    test_case("csv_quotes_separator",        RETURNS, KNOWN_BUG), &
    ! Number formatting: exact zero with a comma decimal.
    test_case("eu_zero_str_real",            RETURNS, FIXED),     &
    test_case("eu_zero_str_complex",         RETURNS, FIXED),     &
    test_case("eu_zero_cast_round_trip",     RETURNS, FIXED),     &
    test_case("eu_zero_file_round_trip",     RETURNS, FIXED),     &
    test_case("us_zero_file_round_trip",     RETURNS, FIXED),     &
    ! Number formatting: special and extreme values.
    test_case("nan_str",                     RETURNS, FIXED),     &
    test_case("nan_str_fmt_f",               RETURNS, FIXED),     &
    test_case("nan_file_round_trip",         RETURNS, FIXED),     &
    test_case("inf_str_decimals",            RETURNS, FIXED),     &
    test_case("inf_str_fmt_f",               RETURNS, FIXED),     &
    test_case("huge_str_fmt_f",              RETURNS, FIXED),     &
    test_case("large_str_fmt_f",             RETURNS, FIXED),     &
    test_case("negative_zero_hex",           RETURNS, FIXED),     &
    test_case("tiny_str_fmt_f",             RETURNS, FIXED),     &
    test_case("decimals_round_fmt_e",       RETURNS, FIXED),     &
    test_case("decimals_round_fmt_f",       RETURNS, FIXED),     &
    test_case("digits_same_for_negative",   RETURNS, FIXED),     &
    test_case("complex_zero_part_fmt_f",    RETURNS, FIXED),     &
    ! The String type and operators.
    test_case("unallocated_equals_empty",    RETURNS, KNOWN_BUG), &
    test_case("count_empty_consistent",      RETURNS, KNOWN_BUG), &
    test_case("join_all_empty",              RETURNS, KNOWN_BUG), &
    test_case("trim_whitespace",             RETURNS, KNOWN_BUG), &
    test_case("echo_blank_line",             RETURNS, KNOWN_BUG), &
    test_case("repeat_negative",             RETURNS, UNDEFINED), &
    ! Behavior that works and has to stay that way.
    test_case("repeat_scalar",               RETURNS, FIXED),     &
    test_case("repeat_array",                RETURNS, FIXED),     &
    test_case("concat_plus_array",           RETURNS, FIXED),     &
    test_case("aprint_empty",                RETURNS, FIXED),     &
    test_case("binary_round_trip",           RETURNS, FIXED),     &
    test_case("binary_shape_too_large",      RETURNS, FIXED)      &
  ]

  character(len=:), allocatable :: arg

  if ( command_argument_count() == 0 ) then
    call run_all_cases()
  else
    call get_argument(1, arg)
    call run_case(trim(arg))
  end if

contains

  subroutine run_all_cases()
    character(len=:), allocatable :: self, log, command
    integer :: i, status, cmdstat, n_passed, n_known, n_unexpected
    logical :: met

    call get_argument(0, self)
    n_passed = 0; n_known = 0; n_unexpected = 0

    do i = 1, size(cases)
      log = dir//"regression_"//trim(cases(i)%name)//".log"
      command = '"'//self//'" '//trim(cases(i)%name)//' > "'//log//'" 2>&1'

      status = -1; cmdstat = 0
      call execute_command_line(command, wait=.true., exitstat=status, cmdstat=cmdstat)

      if ( cmdstat /= 0 ) then
        error stop "Could not launch a child process for case "//trim(cases(i)%name)//"."
      end if

      if ( cases(i)%expected == STOPS ) then
        met = (status == 1) .or. (status == 128)                     ! `error stop`: 1 on gfortran and flang, 128 on ifx
      else
        met = (status == 0)
      end if

      if ( met .and. (cases(i)%status == FIXED) ) then
        n_passed = n_passed + 1
      else if ( cases(i)%status == UNDEFINED ) then
        n_known = n_known + 1
      else if ( (.not. met) .and. (cases(i)%status == KNOWN_BUG) ) then
        n_known = n_known + 1
      else
        n_unexpected = n_unexpected + 1

        if ( met ) then
          write(*,"(a)") "UNEXPECTED PASS: "//trim(cases(i)%name)//" (fixed? change its marker to FIXED)"
        else
          write(*,"(a,i0,a)") "UNEXPECTED FAILURE: "//trim(cases(i)%name)//" (exit status ", status, &
                              ", see "//log//")"
        end if
      end if
    end do

    write(*,"(a,3(i0,a))") "regression-test: ", n_passed, " passed, ", n_known, " known bugs, ", &
                           n_unexpected, " unexpected"

    if ( n_unexpected > 0 ) error stop "regression-test: unexpected results."
  end subroutine run_all_cases

  subroutine run_case(name)
    character(len=*), intent(in) :: name

    character(len=:), allocatable :: file
    type(String)                  :: s, r, cell(2,2), one_row(1,2), three(3)
    type(String), allocatable     :: cells(:,:)
    character(len=512)            :: msg
    integer                       :: st, u, i
    integer(int8)                 :: i8
    logical                       :: opened, exists
    real(real32)                  :: x32
    real(real64)                  :: x, y
    real(real32), allocatable     :: v32(:)
    real(real64), allocatable     :: v(:)
    complex(real32)               :: z

    file = dir//"regression_"//name
    st = 0; msg = ""

    select case (name)
      case ("self_returns")
        continue
      case ("self_stops")
        error stop "This case stops on purpose."
      case ("self_failed_check")
        call check(.false., "This check fails on purpose.")

      case ("to_file_bad_extension")
        call to_file([1.0_real64], file=file//".tsv")
      case ("from_file_missing")
        call from_file(file//"_missing.csv", into=v)
      case ("cast_int_letters")
        call cast("abc", into=i)
      case ("cast_int_trailing_junk")
        call cast("12x", into=i)
      case ("cast_int_decimal_point")
        call cast("3.5", into=i)
      case ("cast_int32_overflow")
        call cast("99999999999", into=i)
      case ("cast_int32_just_over_range")
        call cast("2147483648", into=i)
      case ("cast_int8_just_over_range")
        call cast("128", into=i8)
      case ("cast_real_letters")
        call cast("abc", into=x)
      case ("cast_real_empty")
        call cast("", into=x)
      case ("cast_real_repeat_count")
        call cast("2*3.5", into=x)
      case ("cast_real_two_values")
        call cast("1 2", into=x)
      case ("cast_real_trailing_junk")
        call cast("7,junk", into=x)
      case ("cast_real_slash")
        call cast("/", into=x)
      case ("str_invalid_fmt")
        write(*,"(a)") str(1.5, fmt="q")
      case ("str_invalid_locale")
        write(*,"(a)") str(1.5, locale="JP")

      case ("from_file_bad_number_stat")
        call write_raw(file//".csv", "1.0"//LF//"oops"//LF)
        call from_file(file//".csv", into=v, stat=st, errmsg=msg)
        call check(st /= 0, "A bad number should be reported through stat.")
      case ("to_file_scalar_binary")
        call to_file(1.0_real64, file=file//".dat")
        inquire(file=file//".dat", exist=exists, size=i)
        call check(exists .and. (i == 8), "The file should hold one real64 value.")

      case ("read_no_final_newline")
        call write_raw(file//".csv", "a,b"//LF//"1,2"//LF//"3,4")
        call s%read_file(file//".csv", cell_array=cells)
        call check(all(shape(cells) == [3,2]), "The last row should be read.")
      case ("read_one_line_no_newline")
        call write_raw(file//".csv", "a,b")
        call s%read_file(file//".csv", cell_array=cells)
        call check(all(shape(cells) == [1,2]), "A single line should be read as one row.")
      case ("read_crlf")
        call write_raw(file//".csv", "1,2"//CR//LF//"3,4"//CR//LF)
        call s%read_file(file//".csv", cell_array=cells)
        call check(cells(1,2)%len() == 1, "The carriage return should not be part of the last cell.")
      case ("read_row_wider_than_first")
        call write_raw(file//".csv", "a,b"//LF//"1,2,3,4"//LF)
        call s%read_file(file//".csv", cell_array=cells, stat=st, errmsg=msg)
        call check(st /= 0, "A row wider than the first should be reported through stat.")
      case ("read_row_narrower_than_first")
        call write_raw(file//".csv", "a,b,c"//LF//"1"//LF//"2,3,4"//LF)
        call s%read_file(file//".csv", cell_array=cells)
        call check(cells(2,2)%len() == 0, "Missing cells should be empty, not unallocated.")
      case ("read_quoted_newline")
        call write_raw(file//".csv", "a,b"//LF//'"x'//LF//'y",2'//LF)
        call s%read_file(file//".csv", cell_array=cells)
        call check(all(shape(cells) == [2,2]), "A newline inside quotes should not end the row.")
      case ("read_empty_file_closes_unit")
        call write_raw(file//".txt", "")
        call s%read_file(file//".txt", stat=st, errmsg=msg)
        inquire(file=file//".txt", opened=opened)
        call check(.not. opened, "The file should be closed on return.")
      case ("write_unallocated_cell")
        cell(1,1) = String("a"); cell(1,2) = String("b"); cell(2,1) = String("c")
        call s%write_file(cell, file=file//".csv")
        call check(s%len() == 7, "An unallocated cell should be written as an empty cell.")
      case ("csv_quotes_separator")
        one_row(1,1) = String("a,b"); one_row(1,2) = String("x")
        call s%write_file(one_row, file=file//".csv")
        call s%read_file(file//".csv", cell_array=cells)
        call check(size(cells, dim=2) == 2, "A separator inside a cell should survive a round trip.")

      case ("eu_zero_str_real")
        call check(index(str(0.0, locale="EU"), ".") == 0, "A comma locale should not write a period.")
        call check(index(str(0.0_real64, fmt="f", locale="EU"), ".") == 0, "A comma locale should not write a period.")
      case ("eu_zero_str_complex")
        call check(index(str((0.0,1.0), locale="EU"), ".") == 0, "A comma locale should not write a period.")
      case ("eu_zero_cast_round_trip")
        call cast(str(0.0, locale="EU"), into=x32, locale="EU")
        call check(x32 == 0.0, "Zero should survive a round trip.")
        s = String((0.0,1.0), locale="EU", fmt="e", im="j")
        call cast(s, into=z, locale="EU", fmt="e", im="j")
        call check(z == (0.0,1.0), "Zero should survive a round trip.")
      case ("eu_zero_file_round_trip")
        call to_file([1.5,0.0,2.5], file=file//".csv", locale="EU", stat=st, errmsg=msg)
        call check(st == 0, "The write should succeed.")
        call from_file(file//".csv", into=v32, locale="EU", stat=st, errmsg=msg)
        call check(st == 0, "The read should succeed.")
        call check(all(v32 == [1.5,0.0,2.5]), "The data should survive a round trip.")
      case ("us_zero_file_round_trip")
        call to_file([1.5,0.0,2.5], file=file//".csv", locale="US", stat=st, errmsg=msg)
        call check(st == 0, "The write should succeed.")
        call from_file(file//".csv", into=v32, locale="US", stat=st, errmsg=msg)
        call check(st == 0, "The read should succeed.")
        call check(all(v32 == [1.5,0.0,2.5]), "The data should survive a round trip.")

      case ("nan_str")
        x = ieee_value(x, ieee_quiet_nan)
        call check(trim(adjustl(str(x))) == "NaN", "NaN should be written as NaN.")
      case ("nan_str_fmt_f")
        x = ieee_value(x, ieee_quiet_nan)
        call check(trim(adjustl(str(x, fmt="f"))) == "NaN", "NaN should be written as NaN.")
      case ("nan_file_round_trip")
        x = ieee_value(x, ieee_quiet_nan)
        call to_file([1.0_real64,x], file=file//".csv", stat=st, errmsg=msg)
        call check(st == 0, "The write should succeed.")
        call from_file(file//".csv", into=v, stat=st, errmsg=msg)
        call check(st == 0, "The read should succeed.")
        call check((v(1) == 1.0_real64) .and. ieee_is_nan(v(2)), "NaN should survive a round trip.")
      case ("inf_str_decimals")
        x = ieee_value(x, ieee_positive_inf)
        call check(index(str(x, decimals=3), "Inf") > 0, "Infinity should be written as such.")
      case ("inf_str_fmt_f")
        x = ieee_value(x, ieee_positive_inf)
        call check(index(str(x, fmt="f"), "Inf") > 0, "Infinity should be written as such.")
      case ("huge_str_fmt_f")
        call read_back(str(huge(1.0_real64), fmt="f"), y)
        call check(abs(y/huge(1.0_real64) - 1.0_real64) < 1e-10_real64, "The value should be written in full.")
      case ("large_str_fmt_f")
        call read_back(str(1e120_real64, fmt="f"), y)
        call check(abs(y/1e120_real64 - 1.0_real64) < 1e-10_real64, "The value should be written in full.")
      case ("negative_zero_hex")
        call cast(str(-0.0_real64, fmt="z"), into=y, fmt="z")
        call check(ieee_is_negative(y), "The sign of zero should survive a hexadecimal round trip.")

      case ("tiny_str_fmt_f")
        call read_back(str(1e-300_real64, fmt="f"), y)
        call check(abs(y/1e-300_real64 - 1.0_real64) < 1e-10_real64, "The value should be written in full.")
        call read_back(str(-1e-300_real64, fmt="f"), y)
        call check(abs(y/1e-300_real64 + 1.0_real64) < 1e-10_real64, "The sign should be kept.")
        call check(index(str(1e-300_real64, fmt="f", locale="EU"), ",") == 2, "A comma locale should write a comma.")
        call check(len(str(1e-300_real64, fmt="f", decimals=250)) == 252, "Decimals should set the length.")
      case ("decimals_round_fmt_e")
        call check(str(1.999_real64, fmt="e", decimals=2) == "2.00e+000", "Decimals should round.")
        call check(str(9.999_real64, fmt="e", decimals=2) == "1.00e+001", "Rounding should carry into the exponent.")
        call check(str(1.994_real64, fmt="e", decimals=2) == "1.99e+000", "Decimals should round down when closer.")
      case ("decimals_round_fmt_f")
        call check(str(0.999_real64, fmt="f", decimals=2) == "1.00", "Decimals should round.")
        call check(str(999.999_real64, fmt="f", decimals=2) == "1000.00", "Rounding should add a digit.")
        call check(str(-0.999_real64, fmt="f", decimals=2) == "-1.00", "Rounding should keep the sign.")
        call check(str(0.994_real64, fmt="f", decimals=2) == "0.99", "Decimals should round down when closer.")
      case ("digits_same_for_negative")
        call check(str(-5.0_real64, fmt="f") == "-"//str(5.0_real64, fmt="f"), "The sign should not change the digits.")
      case ("complex_zero_part_fmt_f")
        call check(str((1.0_real64,0.0_real64), fmt="f") == "("//str(1.0_real64, fmt="f")//",0.0)", "Zero imag part.")
        call check(str((0.0_real64,1.0_real64), fmt="f") == "(0.0,"//str(1.0_real64, fmt="f")//")", "Zero real part.")

      case ("unallocated_equals_empty")
        r = String("")
        call check(s == r, "An unallocated String should equal an empty String.")
        call check(s == "", "An unallocated String should equal an empty character.")
      case ("count_empty_consistent")
        r = String("")
        call check(s%count("") == r%count(""), "Unallocated and empty Strings should count alike.")
      case ("join_all_empty")
        three = String("")
        r = join(three, ",")
        call check(r%as_str() == ",,", "Joining empty tokens should keep the separators.")
      case ("trim_whitespace")
        s = String(TAB//"hi"//LF)
        r = s%trim()
        call check(r%len() == 2, "Trim should remove all leading and trailing whitespace.")
      case ("echo_blank_line")
        call echo("", file=file//".log", stat=st, errmsg=msg)
        call check(st == 0, "A blank line should be accepted.")
      case ("repeat_negative")
        call check(len("ab"**(-1)) == 0, "A negative count should give an empty result.")

      case ("repeat_scalar")
        call check("ab"**2 == "abab", "Scalar repetition.")
        call check(len("ab"**0) == 0, "Zero copies.")
      case ("repeat_array")
        call check_repeat_array()
      case ("concat_plus_array")
        call check(all(["ab","cd"] + "z" == ["abz","cdz"]), "Elemental concatenation.")
      case ("aprint_empty")
        allocate( v32(0) )
        call aprint(v32)
      case ("binary_round_trip")
        if ( ifx_checks_rank_of_unallocated() ) return
        call to_file([1.0_real64,2.0_real64], file=file//".dat", stat=st, errmsg=msg)
        call check(st == 0, "The write should succeed.")
        call from_file(file//".dat", into=v, data_shape=[2], stat=st, errmsg=msg)
        call check(st == 0, "The read should succeed.")
        call check(all(v == [1.0_real64,2.0_real64]), "The data should survive a round trip.")
      case ("binary_shape_too_large")
        if ( ifx_checks_rank_of_unallocated() ) return
        call to_file([1.0_real64,2.0_real64], file=file//".dat", stat=st, errmsg=msg)
        call check(st == 0, "The write should succeed.")
        call from_file(file//".dat", into=v, data_shape=[100], stat=st, errmsg=msg)
        call check(st /= 0, "Asking for more data than the file holds should be reported through stat.")

      case default
        error stop "Unknown case: "//name
    end select

    ! The kinds below are only referenced by some cases.
    if ( .false. ) write(*,*) kind(u), int32
  end subroutine run_case

  logical function ifx_checks_rank_of_unallocated()
    !-------------------------------------------------------------------------------------------------------------------
    !! `ifx -check pointers` reports "Attempt to fetch from allocatable variable" for `rank(x)` with `x` unallocated,
    !! which ends any binary `from_file`. `gfortran -fcheck=all` accepts it, as does ifx without that check. See #44.
    !-------------------------------------------------------------------------------------------------------------------
    ifx_checks_rank_of_unallocated = (index(compiler_version(), "Intel") > 0) .and. &
                                     (index(compiler_options(), "-check") > 0)
  end function ifx_checks_rank_of_unallocated

  subroutine check_repeat_array()
    character(len=2), parameter :: a(2)   = ["ab","cd"]
    character(len=1), parameter :: b(2,2) = reshape(["a","b","c","d"], [2,2])
    character(len=1)            :: c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

    c = "x"
    call check(len(a**3) == 6, "The result length should scale with the count.")
    call check(all(a**2 == ["abab","cdcd"]), "Rank 1 repetition.")
    call check(all(b**2 == reshape(["aa","bb","cc","dd"], [2,2])), "Rank 2 repetition, keeping the shape.")
    call check(all(c**2 == "xx") .and. (size(shape(c**2)) == 15), "Rank 15 repetition.")
  end subroutine check_repeat_array

  subroutine check(condition, message)
    logical,          intent(in) :: condition
    character(len=*), intent(in) :: message

    if ( condition ) return

    write(*,"(a)") "CHECK FAILED: "//message
    stop CHECK_FAILED
  end subroutine check

  subroutine read_back(text, value)
    character(len=*), intent(in)  :: text
    real(real64),     intent(out) :: value

    integer :: status

    read(text, *, iostat=status) value
    call check(status == 0, "The text should be a number: "//text)
  end subroutine read_back

  subroutine write_raw(file, content)
    character(len=*), intent(in) :: file, content

    integer :: u

    open(newunit=u, file=file, access="stream", form="unformatted", status="replace")
    write(u) content
    close(u)
  end subroutine write_raw

  subroutine get_argument(number, value)
    integer,                       intent(in)  :: number
    character(len=:), allocatable, intent(out) :: value

    integer :: n

    call get_command_argument(number, length=n)
    allocate( character(len=n) :: value )
    call get_command_argument(number, value)
  end subroutine get_argument
end program regression_test
