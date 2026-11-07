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
  use, intrinsic :: iso_fortran_env, only: real32, real64, int8, int32, int64
  use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_positive_inf, ieee_is_nan, &
                                             ieee_is_negative
  use io_fortran_lib, only: String, cast, str, join, echo, aprint, to_file, from_file, operator(==), operator(/=), &
                            operator(**), operator(+), LF, CR, TAB
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
    test_case("to_file_bad_extension",       STOPS,   FIXED),     &
    test_case("from_file_missing",           STOPS,   FIXED),     &
    test_case("cast_int_letters",            STOPS,   FIXED),     &
    test_case("cast_int_trailing_junk",      STOPS,   FIXED),     &
    test_case("cast_int_decimal_point",      STOPS,   FIXED),     &
    test_case("cast_int32_overflow",         STOPS,   FIXED),     &
    test_case("cast_int32_just_over_range",  STOPS,   FIXED),     &
    test_case("cast_int8_just_over_range",   STOPS,   FIXED),     &
    test_case("cast_real_letters",           STOPS,   FIXED),     &
    test_case("cast_real_empty",             STOPS,   FIXED),     &
    test_case("cast_real_repeat_count",      STOPS,   FIXED),     &
    test_case("cast_real_two_values",        STOPS,   FIXED),     &
    test_case("cast_real_trailing_junk",     STOPS,   FIXED),     &
    test_case("cast_real_slash",             STOPS,   FIXED),     &
    test_case("str_invalid_fmt",             STOPS,   FIXED),     &
    test_case("str_invalid_locale",          STOPS,   FIXED),     &
    ! Errors must be reported through `stat`, and absent optional arguments must not be touched.
    test_case("from_file_bad_number_stat",   RETURNS, FIXED),     &
    test_case("to_file_scalar_binary",       STOPS,   FIXED),     &
    test_case("read_file_missing",          STOPS,   FIXED),     &
    test_case("write_file_bad_extension",   STOPS,   FIXED),     &
    test_case("echo_bad_extension",         STOPS,   FIXED),     &
    test_case("from_file_bad_number",       STOPS,   FIXED),     &
    test_case("cast_bad_complex",           STOPS,   FIXED),     &
    test_case("cast_invalid_fmt",           STOPS,   FIXED),     &
    test_case("cast_invalid_locale",        STOPS,   FIXED),     &
    test_case("to_file_scalar_binary_stat", RETURNS, FIXED),     &
    test_case("from_file_bad_number_location",RETURNS, FIXED),     &
    test_case("from_file_bad_cell_int",     RETURNS, FIXED),     &
    test_case("cast_int_valid",             RETURNS, FIXED),     &
    test_case("cast_int_stat",              RETURNS, FIXED),     &
    test_case("cast_int_hex",               RETURNS, FIXED),     &
    test_case("cast_real_valid",            RETURNS, FIXED),     &
    test_case("cast_real_stat",             RETURNS, FIXED),     &
    test_case("cast_real_wrong_decimal_mark",RETURNS, FIXED),     &
    test_case("cast_real_hex",              RETURNS, FIXED),     &
    test_case("cast_complex_valid",         RETURNS, FIXED),     &
    test_case("cast_complex_stat",          RETURNS, FIXED),     &
    test_case("cast_unallocated_stat",      RETURNS, FIXED),     &
    test_case("cast_invalid_args_stat",     RETURNS, FIXED),     &
    test_case("cast_array_stat",            RETURNS, FIXED),     &
    ! Reading and writing delimited text.
    test_case("read_no_final_newline",       RETURNS, FIXED),     &
    test_case("read_one_line_no_newline",    RETURNS, FIXED),     &
    test_case("read_crlf",                   RETURNS, FIXED),     &
    test_case("read_row_wider_than_first",   RETURNS, FIXED),     &
    test_case("read_row_narrower_than_first",RETURNS, FIXED),     &
    test_case("read_quoted_newline",         RETURNS, FIXED),     &
    test_case("read_empty_file_closes_unit", RETURNS, FIXED),     &
    test_case("write_unallocated_cell",      RETURNS, FIXED),     &
    test_case("csv_quotes_separator",        RETURNS, FIXED),     &
    test_case("read_row_wider_stops",       STOPS,   FIXED),     &
    test_case("read_quote_not_closed_stops",STOPS,   FIXED),     &
    test_case("read_quote_not_closed_stat", RETURNS, FIXED),     &
    test_case("read_doubled_quotes",        RETURNS, FIXED),     &
    test_case("read_text_after_closing_quote",RETURNS, FIXED),     &
    test_case("read_quote_inside_cell",     RETURNS, FIXED),     &
    test_case("read_cr_inside_quotes",      RETURNS, FIXED),     &
    test_case("read_trailing_blank_lines",  RETURNS, FIXED),     &
    test_case("read_blank_line_inside",     RETURNS, FIXED),     &
    test_case("read_only_newlines",         RETURNS, FIXED),     &
    test_case("read_multichar_separators",  RETURNS, FIXED),     &
    test_case("read_tab_separator",         RETURNS, FIXED),     &
    test_case("read_empty_last_cell",       RETURNS, FIXED),     &
    test_case("read_narrow_rows_all_allocated",RETURNS, FIXED),     &
    test_case("read_error_closes_binary",   RETURNS, FIXED),     &
    test_case("write_quotes_only_when_needed",RETURNS, FIXED),     &
    test_case("write_doubles_quotes",       RETURNS, FIXED),     &
    test_case("write_single_column_empty_last",RETURNS, FIXED),     &
    test_case("write_empty_array",          RETURNS, FIXED),     &
    test_case("string_array_constructors",   RETURNS, FIXED),     &
    test_case("round_trip_all_special_cells",RETURNS, FIXED),     &
    test_case("round_trip_custom_separators",RETURNS, FIXED),     &
    test_case("tutorial_ancestry_round_trip",RETURNS, FIXED),     &
    test_case("tutorial_hg38_round_trip",   RETURNS, FIXED),     &
    test_case("from_file_quoted_numbers",   RETURNS, FIXED),     &
    test_case("from_file_crlf",             RETURNS, FIXED),     &
    test_case("from_file_no_final_newline", RETURNS, FIXED),     &
    test_case("from_file_empty_cell",       RETURNS, FIXED),     &
    test_case("from_file_empty_cell_header",RETURNS, FIXED),     &
    test_case("from_file_empty_cell_hex",   RETURNS, FIXED),     &
    test_case("from_file_empty_cell_complex",RETURNS, FIXED),     &
    test_case("from_file_empty_cell_integer",RETURNS, FIXED),     &
    test_case("from_file_pairs_no_final_newline",RETURNS, FIXED),     &
    test_case("from_file_pairs_crlf",       RETURNS, FIXED),     &
    test_case("from_file_pairs_wider_row",  RETURNS, FIXED),     &
    test_case("from_file_pairs_not_closed", RETURNS, FIXED),     &
    test_case("from_file_pairs_header_quoted",RETURNS, FIXED),     &
    test_case("write_file_quotes_text_pairs",RETURNS, FIXED),     &
    test_case("from_file_uneven_rows",      RETURNS, FIXED),     &
    test_case("to_file_complex_round_trip",RETURNS, FIXED),     &
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
    test_case("unallocated_equals_empty",    RETURNS, FIXED),     &
    test_case("count_empty_consistent",      RETURNS, FIXED),     &
    test_case("join_all_empty",              RETURNS, FIXED),     &
    test_case("join_many_empty",            RETURNS, FIXED),     &
    test_case("join_many_tokens",            RETURNS, FIXED),     &
    test_case("join_into_token",             RETURNS, FIXED),     &
    test_case("join_no_tokens",             RETURNS, FIXED),     &
    test_case("join_unallocated_tokens",    RETURNS, FIXED),     &
    test_case("trim_whitespace",             RETURNS, FIXED),     &
    test_case("echo_blank_line",             RETURNS, FIXED),     &
    test_case("echo_unallocated",            RETURNS, FIXED),     &
    test_case("repeat_negative",             RETURNS, FIXED),     &
    ! Behavior that works and has to stay that way.
    test_case("repeat_scalar",               RETURNS, FIXED),     &
    test_case("repeat_array",                RETURNS, FIXED),     &
    test_case("concat_plus_array",           RETURNS, FIXED),     &
    test_case("aprint_empty",                RETURNS, FIXED),     &
    test_case("binary_round_trip",           RETURNS, FIXED),     &
    test_case("binary_wrong_rank_stat",      RETURNS, FIXED),     &
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
        if ( met ) met = is_readable(log)                            ! The message has to be of use to someone
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
          write(*,"(a,i0,a)") "UNEXPECTED FAILURE: "//trim(cases(i)%name)//" (exit status ", status, "), which printed:"
          call print_file(log)
        end if
      end if
    end do

    write(*,"(a,3(i0,a))") "regression-test: ", n_passed, " passed, ", n_known, " known bugs, ", &
                           n_unexpected, " unexpected"

    if ( n_unexpected > 0 ) error stop "regression-test: unexpected results."
  end subroutine run_all_cases

  subroutine print_file(file)
    character(len=*), intent(in) :: file

    character(len=:), allocatable :: text
    integer :: n, u

    inquire(file=file, size=n)
    allocate( character(len=n) :: text )

    open(newunit=u, file=file, access="stream", form="unformatted", status="old")
    read(u) text
    close(u)

    write(*,"(a)") text
  end subroutine print_file

  logical function is_readable(file)
    !-------------------------------------------------------------------------------------------------------------------
    !! Checks that the first line with text in a file is printable, since a compiler bug can garble a stop message.
    !! Later lines are left alone, as they may hold a backtrace.
    !-------------------------------------------------------------------------------------------------------------------
    character(len=*), intent(in) :: file

    character(len=:), allocatable :: text
    integer :: first, i, n, u

    inquire(file=file, size=n)
    allocate( character(len=n) :: text )

    open(newunit=u, file=file, access="stream", form="unformatted", status="old")
    read(u) text
    close(u)

    is_readable = .false.
    first = verify(text, CR//LF)
    if ( first == 0 ) return

    do i = first, n
      if ( (text(i:i) == CR) .or. (text(i:i) == LF) ) exit
      if ( (ichar(text(i:i)) < 32) .or. (ichar(text(i:i)) > 126) ) return
    end do

    is_readable = .true.
  end function is_readable

  subroutine run_case(name)
    character(len=*), intent(in) :: name

    character(len=:), allocatable :: file
    type(String)                  :: s, r, cell(2,2), one_row(1,2), three(3)
    type(String), allocatable     :: cells(:,:)
    character(len=512)            :: msg
    integer                       :: st, i
    integer(int8)                 :: i8
    logical                       :: opened, exists
    real(real32)                  :: x32
    real(real64)                  :: x, y
    real(real32), allocatable     :: v32(:)
    real(real64), allocatable     :: v(:)
    complex(real32)               :: z, zv(2)
    complex(real32), allocatable  :: zw(:), mz(:,:)
    type(String)                  :: special(2,4), column(2,1)
    type(String), allocatable     :: many(:)
    integer(int64)                :: i64
    integer                       :: iv(3), stv(3)
    integer, allocatable          :: vi(:)
    real(real64), allocatable     :: m2(:,:)

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
      case ("to_file_scalar_binary_stat")
        call to_file(1.0_real64, file=file//".dat", stat=st, errmsg=msg)
        call check(st /= 0, "A scalar should be rejected through stat.")
      case ("read_file_missing")
        call s%read_file(file//"_missing.csv")
      case ("write_file_bad_extension")
        cell = String("a")
        call s%write_file(cell, file=file//".tsv")
      case ("echo_bad_extension")
        call echo("text", file=file//".tsv")
      case ("from_file_bad_number")
        call write_raw(file//".csv", "1.0"//LF//"oops"//LF)
        call from_file(file//".csv", into=v)
      case ("from_file_bad_number_location")
        call write_raw(file//".csv", "1,2"//LF//"3,oops"//LF)
        call from_file(file//".csv", into=m2, stat=st, errmsg=msg)
        call check(st /= 0, "A bad number should be reported through stat.")
        call check(index(msg, "oops") > 0, "The message should name the text.")
        call check(index(msg, "row 2, column 2") > 0, "The message should name the location.")
      case ("from_file_bad_cell_int")
        call write_raw(file//".csv", "1"//LF//"2.5"//LF)
        call from_file(file//".csv", into=vi, stat=st, errmsg=msg)
        call check(st /= 0, "A bad integer should be reported through stat.")
      case ("cast_bad_complex")
        call cast("abc", into=z)
      case ("cast_invalid_fmt")
        call cast("1", into=x, fmt="q")
      case ("cast_invalid_locale")
        call cast("1", into=x, locale="JP")
      case ("cast_int_valid")
        call cast_i32_is("7", 7); call cast_i32_is("+7", 7); call cast_i32_is("-7", -7); call cast_i32_is("  12 ", 12)
        call cast_i32_is("0", 0); call cast_i32_is("007", 7); call cast_i32_is("12"//CR, 12)
        call cast_i32_is("2147483647", huge(1_int32)); call cast_i32_is("-2147483648", -huge(1_int32)-1_int32)
        call cast("127", into=i8, stat=st); call check((st == 0) .and. (i8 == 127_int8), "Largest int8.")
        call cast("-128", into=i8, stat=st); call check((st == 0) .and. (i8 == -127_int8-1_int8), "Smallest int8.")
        call cast("9223372036854775807", into=i64, stat=st)
        call check((st == 0) .and. (i64 == huge(1_int64)), "Largest int64.")
        call cast("-9223372036854775808", into=i64, stat=st)
        call check((st == 0) .and. (i64 == -huge(1_int64)-1_int64), "Smallest int64.")
      case ("cast_int_stat")
        call cast_i32_fails(""); call cast_i32_fails("   "); call cast_i32_fails("abc"); call cast_i32_fails("12x")
        call cast_i32_fails("3.5"); call cast_i32_fails("+"); call cast_i32_fails("-"); call cast_i32_fails("1 2")
        call cast_i32_fails("--1"); call cast_i32_fails("2147483648"); call cast_i32_fails("-2147483649")
        call cast_i32_fails("99999999999999999999999999")
        call cast("128", into=i8, stat=st); call check((st /= 0) .and. (i8 == 0_int8), "Just over int8.")
        call cast("-129", into=i8, stat=st); call check((st /= 0) .and. (i8 == 0_int8), "Just under int8.")
        call cast("9223372036854775808", into=i64, stat=st); call check(st /= 0, "Just over int64.")
        call cast("-9223372036854775809", into=i64, stat=st); call check(st /= 0, "Just under int64.")
      case ("cast_int_hex")
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
      case ("cast_real_valid")
        call cast_r64_is("1.5e3", 1500.0_real64, "US"); call cast_r64_is(".5", 0.5_real64, "US")
        call cast_r64_is("5.", 5.0_real64, "US"); call cast_r64_is("-2", -2.0_real64, "US")
        call cast_r64_is("+3.25", 3.25_real64, "US"); call cast_r64_is("1d2", 100.0_real64, "US")
        call cast_r64_is("  7 ", 7.0_real64, "US"); call cast_r64_is("1E-2", 0.01_real64, "US")
        call cast_r64_is("1,5", 1.5_real64, "EU"); call cast_r64_is("-1,5e1", -15.0_real64, "EU")
        call cast_r64_is("2.5"//CR, 2.5_real64, "US")
        call cast("NaN", into=x, stat=st); call check((st == 0) .and. ieee_is_nan(x), "NaN.")
        call cast("Inf", into=x, stat=st); call check((st == 0) .and. (x > huge(x)), "Inf.")
        call cast("-Inf", into=x, stat=st); call check((st == 0) .and. (x < -huge(x)), "-Inf.")
        call cast("infinity", into=x, stat=st); call check((st == 0) .and. (x > huge(x)), "infinity.")
      case ("cast_real_stat")
        call cast_r64_fails("", "US"); call cast_r64_fails("  ", "US"); call cast_r64_fails("abc", "US")
        call cast_r64_fails("2*3.5", "US"); call cast_r64_fails("1 2", "US"); call cast_r64_fails("7,junk", "US")
        call cast_r64_fails("/", "US"); call cast_r64_fails(".", "US"); call cast_r64_fails("e5", "US")
        call cast_r64_fails("1e", "US"); call cast_r64_fails("1e+", "US"); call cast_r64_fails("1.2.3", "US")
        call cast_r64_fails("+", "US"); call cast_r64_fails("1e5x", "US"); call cast_r64_fails("NaNN", "US")
      case ("cast_real_wrong_decimal_mark")
        call cast_r64_fails("1,5", "US"); call cast_r64_fails("1.5", "EU")
      case ("cast_real_hex")
        call cast("0x3ff8000000000000", into=x, fmt="z", stat=st); call check((st == 0) .and. (x == 1.5_real64), "1.5.")
        call cast("0x3fc00000", into=x32, fmt="z", stat=st); call check((st == 0) .and. (x32 == 1.5), "real32 1.5.")
        call cast("0x1ffffffff", into=x32, fmt="z", stat=st); call check(st /= 0, "Too large for real32.")
        call cast("0xzz", into=x, fmt="z", stat=st); call check(st /= 0, "Not a hex digit.")
      case ("cast_complex_valid")
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
      case ("cast_complex_stat")
        call cast_c32_fails("", ""); call cast_c32_fails("abc", ""); call cast_c32_fails("(1,2", "")
        call cast_c32_fails("1,2)", ""); call cast_c32_fails("()", ""); call cast_c32_fails("(1;2)", "")
        call cast_c32_fails("(a,b)", ""); call cast_c32_fails("(1,)", ""); call cast_c32_fails("(,2)", "")
        call cast_c32_fails("(1,2,3)", ""); call cast_c32_fails("1+2", "j"); call cast_c32_fails("1.5j", "j")
        call cast_c32_fails("j", "j"); call cast_c32_fails("+j", "j"); call cast_c32_fails("abc+defj", "j")
      case ("cast_unallocated_stat")
        call s%cast(into=i, stat=st); call check((st /= 0) .and. (i == 0), "Integer.")
        call s%cast(into=x, stat=st); call check((st /= 0) .and. (x == 0.0_real64), "Real.")
        call s%cast(into=z, stat=st); call check((st /= 0) .and. (z == (0.0,0.0)), "Complex.")
      case ("cast_invalid_args_stat")
        call cast("1", into=x, fmt="q", stat=st); call check(st /= 0, "Invalid real fmt.")
        call cast("1", into=x, locale="JP", stat=st); call check(st /= 0, "Invalid locale.")
        call cast("1", into=i, fmt="q", stat=st); call check(st /= 0, "Invalid int fmt.")
        call cast("(1,2)", into=z, fmt="q", stat=st); call check(st /= 0, "Invalid complex fmt.")
      case ("cast_array_stat")
        three(1) = String("1"); three(2) = String("oops"); three(3) = String("3")
        call cast(three, into=iv, stat=stv)
        call check(all((stv /= 0) .eqv. [.false.,.true.,.false.]), "Each cell should have its own status.")
        call check(all(iv == [1,0,3]), "Each valid cell should still be cast.")

      case ("read_no_final_newline")
        call write_raw(file//".csv", "a,b"//LF//"1,2"//LF//"3,4")
        call s%read_file(file//".csv", cell_array=cells)
        call check(all(shape(cells) == [3,2]), "The last row should be read.")
        call check(cells(3,2)%as_str() == "4", "The last cell should be complete.")
      case ("read_one_line_no_newline")
        call write_raw(file//".csv", "a,b")
        call s%read_file(file//".csv", cell_array=cells)
        call check(all(shape(cells) == [1,2]), "A single line should be read as one row.")
      case ("read_crlf")
        call write_raw(file//".csv", "1,2"//CR//LF//"3,4"//CR//LF)
        call s%read_file(file//".csv", cell_array=cells)
        call check(all(shape(cells) == [2,2]), "CRLF should give the same shape.")
        call check(cells(1,2)%len() == 1, "The carriage return should not be part of the last cell.")
        call check(cells(2,2)%as_str() == "4", "The carriage return should not be part of the last row.")
      case ("read_row_wider_than_first")
        call write_raw(file//".csv", "a,b"//LF//"1,2,3,4"//LF)
        call s%read_file(file//".csv", cell_array=cells, stat=st, errmsg=msg)
        call check(st /= 0, "A row wider than the first should be reported through stat.")
        call check(index(msg, "Row 2 has 4 columns, but the first row has 2") > 0, "The message should name the row.")
      case ("read_row_narrower_than_first")
        call write_raw(file//".csv", "a,b,c"//LF//"1"//LF//"2,3,4"//LF)
        call s%read_file(file//".csv", cell_array=cells)
        call check(all(shape(cells) == [3,3]), "The first row sets the shape.")
        call check(cells(2,1)%as_str() == "1", "The cell should be read.")
        call check(cells(2,2)%len() == 0, "Missing cells should be empty, not unallocated.")
        call check(cells(3,3)%as_str() == "4", "Later rows should line up.")
      case ("read_quoted_newline")
        call write_raw(file//".csv", "a,b"//LF//'"x'//LF//'y",2'//LF)
        call s%read_file(file//".csv", cell_array=cells)
        call check(all(shape(cells) == [2,2]), "A newline inside quotes should not end the row.")
        call check(cells(2,1)%as_str() == "x"//LF//"y", "The newline should be kept and the quotes removed.")
        call check(cells(2,2)%as_str() == "2", "The next cell should follow.")
      case ("read_empty_file_closes_unit")
        call write_raw(file//".txt", "")
        call s%read_file(file//".txt", stat=st, errmsg=msg)
        inquire(file=file//".txt", opened=opened)
        call check(.not. opened, "The file should be closed on return.")
      case ("write_unallocated_cell")
        cell(1,1) = String("a"); cell(1,2) = String("b"); cell(2,1) = String("c")
        call s%write_file(cell, file=file//".csv")
        call check(s%as_str() == "a,b"//LF//"c,"//LF, "An unallocated cell should be written as an empty cell.")
      case ("csv_quotes_separator")
        one_row(1,1) = String("a,b"); one_row(1,2) = String("x")
        call s%write_file(one_row, file=file//".csv")
        call s%read_file(file//".csv", cell_array=cells)
        call check(size(cells, dim=2) == 2, "A separator inside a cell should survive a round trip.")
        call check(cells(1,1)%as_str() == "a,b", "The cell should come back unchanged.")
      case ("read_row_wider_stops")
        call write_raw(file//".csv", "a,b"//LF//"1,2,3"//LF)
        call s%read_file(file//".csv", cell_array=cells)
      case ("read_quote_not_closed_stops")
        call write_raw(file//".csv", 'a,"b'//LF)
        call s%read_file(file//".csv", cell_array=cells)
      case ("read_quote_not_closed_stat")
        call write_raw(file//".csv", 'a,b'//LF//'1,"2'//LF)
        call s%read_file(file//".csv", cell_array=cells, stat=st, errmsg=msg)
        call check(st /= 0, "An open quote should be reported through stat.")
        call check(index(msg, "row 2, column 2") > 0, "The message should name the location.")
      case ("read_doubled_quotes")
        call write_raw(file//".csv", '"say ""hi""",""""'//LF)
        call s%read_file(file//".csv", cell_array=cells)
        call check(cells(1,1)%as_str() == 'say "hi"', "Doubled quotes stand for one.")
        call check(cells(1,2)%as_str() == '"', "A cell can be just a quote.")
      case ("read_text_after_closing_quote")
        call write_raw(file//".csv", '"ab"c,d'//CR//LF)
        call s%read_file(file//".csv", cell_array=cells)
        call check(cells(1,1)%as_str() == "abc", "Text after a closing quote is kept.")
        call check(cells(1,2)%as_str() == "d", "The separator still ends the cell.")
      case ("read_quote_inside_cell")
        call write_raw(file//".csv", 'a"b,5"'//LF//' "c",d'//LF)
        call s%read_file(file//".csv", cell_array=cells)
        call check(cells(1,1)%as_str() == 'a"b', "A quote inside a cell is an ordinary character.")
        call check(cells(1,2)%as_str() == '5"', "A quote at the end of a cell is an ordinary character.")
        call check(cells(2,1)%as_str() == ' "c"', "Only a quote at the very start opens a quoted cell.")
      case ("read_cr_inside_quotes")
        call write_raw(file//".csv", '"a'//CR//'",b'//CR//LF)
        call s%read_file(file//".csv", cell_array=cells)
        call check(cells(1,1)%as_str() == "a"//CR, "A carriage return inside quotes is data.")
        call check(cells(1,2)%as_str() == "b", "A carriage return before the newline is not.")
      case ("read_trailing_blank_lines")
        call write_raw(file//".csv", "a,b"//LF//"1,2"//LF//LF//CR//LF)
        call s%read_file(file//".csv", cell_array=cells)
        call check(all(shape(cells) == [2,2]), "Trailing blank lines should not add rows.")
      case ("read_blank_line_inside")
        call write_raw(file//".csv", "a,b"//LF//LF//"1,2"//LF)
        call s%read_file(file//".csv", cell_array=cells)
        call check(all(shape(cells) == [3,2]), "A blank line inside the data keeps its row.")
        call check((cells(2,1)%len() == 0) .and. (cells(2,2)%len() == 0), "Its cells should be empty.")
      case ("read_only_newlines")
        call write_raw(file//".csv", LF//LF)
        call s%read_file(file//".csv", cell_array=cells, stat=st, errmsg=msg)
        call check(st /= 0, "A file with only newlines has no data.")
      case ("read_multichar_separators")
        call write_raw(file//".csv", "a||b##c|d||e##")
        call s%read_file(file//".csv", cell_array=cells, row_separator="##", column_separator="||")
        call check(all(shape(cells) == [2,2]), "Separators can be longer than one character.")
        call check(cells(2,1)%as_str() == "c|d", "A partial separator is data.")
        call check(cells(2,2)%as_str() == "e", "The last cell should be complete.")
      case ("read_tab_separator")
        call write_raw(file//".csv", "a"//TAB//"b,c"//LF)
        call s%read_file(file//".csv", cell_array=cells, column_separator=TAB)
        call check(cells(1,2)%as_str() == "b,c", "A comma is data with another separator.")
      case ("read_empty_last_cell")
        call write_raw(file//".csv", "a,b,"//LF//"1,2,")
        call s%read_file(file//".csv", cell_array=cells)
        call check(all(shape(cells) == [2,3]), "A trailing separator ends an empty cell.")
        call check(cells(2,3)%len() == 0, "The cell should be empty.")
      case ("read_narrow_rows_all_allocated")
        call write_raw(file//".csv", "a,b,c"//LF//"1"//LF//LF//"2,3"//LF)
        call s%read_file(file//".csv", cell_array=cells)
        call check(all(cells%len() >= 0), "Every cell should be allocated.")
      case ("read_error_closes_binary")
        call to_file([1.0_real64,2.0_real64], file=file//".dat", stat=st, errmsg=msg)
        call from_file(file//".dat", into=v, data_shape=[100], stat=st, errmsg=msg)
        call check(st /= 0, "The read should fail.")
        inquire(file=file//".dat", opened=opened)
        call check(.not. opened, "The file should be closed on return.")
      case ("write_quotes_only_when_needed")
        cell(1,1) = String("1.5"); cell(1,2) = String("a b"); cell(2,1) = String("a;b"); cell(2,2) = String("")
        call s%write_file(cell, file=file//".csv")
        call check(s%as_str() == "1.5,a b"//LF//"a;b,"//LF, "Plain cells should be written as they are.")
      case ("write_single_column_empty_last")
        column(1,1) = String("a"); column(2,1) = String("")
        call s%write_file(column, file=file//".csv")
        call s%read_file(file//".csv", cell_array=cells)
        call check(all(shape(cells) == [2,1]), "The empty last row should survive a round trip.")
        call check(cells(2,1)%len() == 0, "It should still be empty.")
      case ("write_doubles_quotes")
        one_row(1,1) = String('say "hi"'); one_row(1,2) = String("a"//LF//"b")
        call s%write_file(one_row, file=file//".csv")
        call check(s%as_str() == '"say ""hi""","a'//LF//'b"'//LF, "Quotes should be doubled.")
      case ("write_empty_array")
        call s%write_file(cell(1:0,:), file=file//".csv", stat=st, errmsg=msg)
        call check(st == 0, "An empty array should write an empty file.")
        call check(s%len() == 0, "The text should be empty.")
      case ("string_array_constructors")
        special(1,:) = [String("a"), String("b"), String("c"), String("d")]
        special(2,:) = [String("e"), String("f"), String("g"), String("h")]
        call check(special(1,3)%as_str() == "c", "A constructor assigned to a section.")
        call check(special(2,4)%as_str() == "h", "A constructor assigned to another section.")
        special(1,:) = [String("i"), String("j"), String("k"), String("l")]
        call check(special(1,3)%as_str() == "k", "A section assigned again.")
        call check(special(2,1)%as_str() == "e", "Other sections should keep their values.")
        three = [String("x"), String("y"), String("z")]
        call check(three(3)%as_str() == "z", "A constructor assigned to a whole array.")
      case ("round_trip_all_special_cells")
        special(1,:) = [String("a,b"), String('q"q'), String('"'), String("")]
        special(2,:) = [String("l"//LF//"f"), String("c"//CR), String(CR//LF), String(" x ")]
        call s%write_file(special, file=file//".csv")
        call s%read_file(file//".csv", cell_array=cells)
        call check(all(shape(cells) == shape(special)), "The shape should survive a round trip.")
        call check(all(cells == special), "Every cell should survive a round trip.")
      case ("round_trip_custom_separators")
        special(1,:) = [String("a|b"), String("c;d"), String("e"//LF//"f"), String("g")]
        special(2,:) = [String("##"), String("|"), String(""), String('"')]
        call s%write_file(special, file=file//".csv", row_separator="##", column_separator="|")
        call s%read_file(file//".csv", cell_array=cells, row_separator="##", column_separator="|")
        call check(all(shape(cells) == shape(special)), "The shape should survive a round trip.")
        call check(all(cells == special), "Every cell should survive a round trip.")
      case ("tutorial_ancestry_round_trip")
        call s%read_file("./data/ancestry_comp.csv", cell_array=cells, row_separator=CR+LF)
        call check(cells(2,1)%as_str() == "Arab, Egyptian & Levantine", "The quoted cell should be read.")
        call check(cells(2,5)%as_str() == "168717618", "The next column should line up.")
        call r%write_file(cells, file=file//".csv", row_separator=CR+LF)
        call check(r == s, "The file should be reproduced exactly.")
      case ("tutorial_hg38_round_trip")
        call s%read_file("./data/hg38.bed", cell_array=cells, column_separator=TAB)
        call check(size(cells, dim=1) == 192262, "The tutorial says the file has 192262 lines.")
        call r%write_file(cells, file=file//".bed", column_separator=TAB)
        call check(r == s, "The file should be reproduced exactly.")
      case ("from_file_quoted_numbers")
        call write_raw(file//".csv", '"1.5","2.5"'//LF)
        call from_file(file//".csv", into=m2, stat=st, errmsg=msg)
        call check(st == 0, "Quoted numbers should be read.")
        call check(all(m2 == reshape([1.5_real64,2.5_real64], [1,2])), "The values should be right.")
      case ("from_file_crlf")
        call write_raw(file//".csv", "1.5,2.5"//CR//LF//"3.5,4.5"//CR//LF)
        call from_file(file//".csv", into=m2, stat=st, errmsg=msg)
        call check(st == 0, "CRLF should be read.")
        call check(m2(2,2) == 4.5_real64, "The values should be right.")
      case ("from_file_no_final_newline")
        call write_raw(file//".csv", "1"//LF//"2"//LF//"3")
        call from_file(file//".csv", into=vi, stat=st, errmsg=msg)
        call check((st == 0) .and. (size(vi) == 3), "The last row should be read.")
        call check(vi(3) == 3, "The value should be right.")
      case ("from_file_empty_cell")
        call write_raw(file//".csv", "1,2"//LF//"3,"//LF//", "//LF)
        call from_file(file//".csv", into=m2, stat=st, errmsg=msg)
        call check(st == 0, "An empty cell should not be an error.")
        call check(m2(1,2) == 2.0_real64, "Other cells should be read.")
        call check(ieee_is_nan(m2(2,2)), "An empty cell should read as NaN.")
        call check(ieee_is_nan(m2(3,1)) .and. ieee_is_nan(m2(3,2)), "A blank cell should read as NaN.")
      case ("from_file_empty_cell_header")
        call write_raw(file//".csv", "a,b"//LF//"1,"//LF)
        call from_file(file//".csv", into=m2, header=.true., stat=st, errmsg=msg)
        call check(st == 0, "An empty cell should not be an error.")
        call check(all(shape(m2) == [1,2]), "The header should not count as data.")
        call check((m2(1,1) == 1.0_real64) .and. ieee_is_nan(m2(1,2)), "The NaN should land in the right cell.")
      case ("from_file_empty_cell_hex")
        call write_raw(file//".csv", "0x3ff8000000000000,"//LF)
        call from_file(file//".csv", into=m2, fmt="z", stat=st, errmsg=msg)
        call check(st == 0, "An empty cell should not be an error.")
        call check((m2(1,1) == 1.5_real64) .and. ieee_is_nan(m2(1,2)), "It should read as NaN.")
      case ("from_file_empty_cell_complex")
        call write_raw(file//".csv", "(1.5,2.5),"//LF)
        call from_file(file//".csv", into=mz, stat=st, errmsg=msg)
        call check(st == 0, "An empty cell should not be an error.")
        call check(mz(1,1) == (1.5,2.5), "The ordered pair should be read.")
        call check(ieee_is_nan(mz(1,2)%re) .and. ieee_is_nan(mz(1,2)%im), "Both parts should be NaN.")
      case ("from_file_empty_cell_integer")
        call write_raw(file//".csv", "1"//LF//LF//"3"//LF)
        call from_file(file//".csv", into=vi, stat=st, errmsg=msg)
        call check(st /= 0, "An integer has no NaN, so an empty cell is an error.")
        call check(index(msg, "row 2, column 1") > 0, "The message should name the location.")
      case ("from_file_uneven_rows")
        call write_raw(file//".csv", "1,2"//LF//"3"//LF)
        call from_file(file//".csv", into=m2, stat=st, errmsg=msg)
        call check(st == 0, "A missing cell is an empty cell.")
        call check((m2(2,1) == 3.0_real64) .and. ieee_is_nan(m2(2,2)), "It should read as NaN.")
      case ("from_file_pairs_no_final_newline")
        call write_raw(file//".csv", "(1.5,2.5),(3.5,4.5)"//LF//"(5.5,6.5),(7.5,8.5)")
        call from_file(file//".csv", into=mz, stat=st, errmsg=msg)
        call check((st == 0) .and. all(shape(mz) == [2,2]), "The last row should be read.")
        call check(mz(2,2) == (7.5,8.5), "The value should be right.")
      case ("from_file_pairs_crlf")
        call write_raw(file//".csv", "(1.5,2.5),(3.5,4.5)"//CR//LF)
        call from_file(file//".csv", into=mz, stat=st, errmsg=msg)
        call check((st == 0) .and. all(shape(mz) == [1,2]), "CRLF should be read.")
        call check(mz(1,2) == (3.5,4.5), "The value should be right.")
      case ("from_file_pairs_wider_row")
        call write_raw(file//".csv", "(1,2)"//LF//"(3,4),(5,6)"//LF)
        call from_file(file//".csv", into=mz, stat=st, errmsg=msg)
        call check(st /= 0, "A wider row should be reported through stat.")
      case ("from_file_pairs_not_closed")
        call write_raw(file//".csv", "(1,2"//LF)
        call from_file(file//".csv", into=mz, stat=st, errmsg=msg)
        call check(st /= 0, "A bad ordered pair should be reported through stat.")
      case ("from_file_pairs_header_quoted")
        call to_file(reshape([(1.5,2.5),(3.5,4.5)], [1,2]), file=file//".csv", header=["a,b","c d"], stat=st, &
                     errmsg=msg)
        call check(st == 0, "The write should succeed.")
        call from_file(file//".csv", into=mz, header=.true., stat=st, errmsg=msg)
        call check((st == 0) .and. all(shape(mz) == [1,2]), "A quoted header should not shift the columns.")
        call check(mz(1,2) == (3.5,4.5), "The value should be right.")
      case ("write_file_quotes_text_pairs")
        one_row(1,1) = String("(a,b)"); one_row(1,2) = String("c")
        call s%write_file(one_row, file=file//".csv")
        call check(s%as_str() == '"(a,b)",c'//LF, "write_file should quote text that looks like a pair.")
      case ("to_file_complex_round_trip")
        zv = [(1.5,-2.5),(0.0,1.0)]
        call to_file(zv, file=file//".csv", stat=st, errmsg=msg)
        call check(st == 0, "The write should succeed.")
        call s%read_file(file//".csv")
        call check(s%as_str() == str(zv(1))//LF//str(zv(2))//LF, "Ordered pairs should not be quoted in a file.")
        call from_file(file//".csv", into=zw, stat=st, errmsg=msg)
        call check(st == 0, "The read should succeed.")
        call check(all(zw == zv), "The default complex form should survive a round trip.")

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
        call check(r == s, "In either order.")
        call check(s == "", "An unallocated String should equal an empty character.")
        call check("" == s, "In either order.")
        call check(.not. (s /= r), "Not equal should agree.")
        call check(.not. (s /= ""), "Not equal should agree with a character.")
        call check(.not. ("" /= s), "In either order.")
        call check(s /= "a", "An unallocated String should not equal text.")
      case ("count_empty_consistent")
        r = String("")
        call check(s%count("") == r%count(""), "Unallocated and empty Strings should count alike.")
        call check(s%count(r) == r%count(s), "With a String to match as well.")
        call check(s%count("a") == 0, "An unallocated String holds no text.")
      case ("join_all_empty")
        three = String("")
        r = join(three, ",")
        call check(r%as_str() == ",,", "Joining empty tokens should keep the separators.")
        call r%join(three, ";")
        call check(r%as_str() == ";;", "The bound join should agree.")
        three(2) = String("x")
        r = join(three, ",")
        call check(r%as_str() == ",x,", "Some empty tokens.")
      case ("join_many_empty")
        allocate( many(1000) )
        many = String("")
        r = join(many, ",")
        call check(r%len() == 999, "The separators should survive the split for large arrays.")
      case ("join_many_tokens")
        allocate( many(200000) )
        many = String("ab")
        r = join(many, ",")
        call check(r%len() == 3*200000 - 1, "All tokens and separators should be present.")
        call check(r%as_str() == repeat("ab,", 199999)//"ab", "In order.")
        call r%join(many(:3), ";")
        call check(r%as_str() == "ab;ab;ab", "The bound join should agree.")
      case ("join_into_token")
        three(1) = String("a"); three(2) = String("b"); three(3) = String("c")
        call three(1)%join(three, ",")
        call check(three(1)%as_str() == "a,b,c", "Joining into one of the tokens.")
      case ("join_no_tokens")
        allocate( many(0) )
        r = join(many, ",")
        call check(r%len() == 0, "No tokens should give an empty result.")
      case ("join_unallocated_tokens")
        r = join(three, ",")
        call check(r%as_str() == ",,", "Unallocated tokens are empty tokens.")
      case ("trim_whitespace")
        s = String(TAB//"hi"//LF)
        r = s%trim()
        call check(r%as_str() == "hi", "Trim should remove all leading and trailing whitespace.")
        s = String(" "//CR//LF//TAB//"a b"//TAB//"c"//CR//LF//" ")
        r = s%trim()
        call check(r%as_str() == "a b"//TAB//"c", "Whitespace inside should stay.")
        call s%trim_inplace()
        call check(s == r, "The inplace form should agree.")
        s = String(TAB//LF//"  ")
        r = s%trim()
        call check(r%len() == 0, "All whitespace trims to empty.")
      case ("echo_blank_line")
        call write_raw(file//".log", "")
        call echo("a", file=file//".log", stat=st, errmsg=msg)
        call echo("", file=file//".log", stat=st, errmsg=msg)
        call check(st == 0, "A blank line should be accepted.")
        call echo("b", file=file//".log", stat=st, errmsg=msg)
        call s%read_file(file//".log")
        call check(s%as_str() == "a"//LF//LF//"b"//LF, "The blank line should be in the file.")
      case ("echo_unallocated")
        call write_raw(file//".log", "")
        call s%echo(file//".log", stat=st, errmsg=msg)
        call check(st == 0, "An unallocated String is an empty line.")
        call r%read_file(file//".log")
        call check(r%as_str() == LF, "It should write only the terminator.")
      case ("repeat_negative")
        call check(len("ab"**(-1)) == 0, "A negative count should give an empty result.")
        r = String("ab")**(-1)
        call check(r%len() == 0, "The same for a String.")
        call check(len(["ab","cd"]**(-2)) == 0, "The same for an array.")
      case ("binary_wrong_rank_stat")
        call to_file([1.0_real64,2.0_real64], file=file//".dat", stat=st, errmsg=msg)
        call from_file(file//".dat", into=m2, data_shape=[2], stat=st, errmsg=msg)
        call check(st /= 0, "A shape that does not match the rank should be reported through stat.")
        call from_file(file//".dat", into=m2, data_shape=[2,1], stat=st, errmsg=msg)
        call check((st == 0) .and. (m2(2,1) == 2.0_real64), "The right rank should read.")

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
        call to_file([1.0_real64,2.0_real64], file=file//".dat", stat=st, errmsg=msg)
        call check(st == 0, "The write should succeed.")
        call from_file(file//".dat", into=v, data_shape=[2], stat=st, errmsg=msg)
        call check(st == 0, "The read should succeed.")
        call check(all(v == [1.0_real64,2.0_real64]), "The data should survive a round trip.")
      case ("binary_shape_too_large")
        call to_file([1.0_real64,2.0_real64], file=file//".dat", stat=st, errmsg=msg)
        call check(st == 0, "The write should succeed.")
        call from_file(file//".dat", into=v, data_shape=[100], stat=st, errmsg=msg)
        call check(st /= 0, "Asking for more data than the file holds should be reported through stat.")

      case default
        error stop "Unknown case: "//name
    end select

  end subroutine run_case

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
