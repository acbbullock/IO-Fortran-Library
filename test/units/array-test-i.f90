program main
  use, intrinsic :: iso_fortran_env, only: compiler_version, compiler_options
  use kinds,                         only: rk, ik
  use io_fortran_lib,                only: String, cast, LF, SPACE, str, operator(+), operator(**)
  use randoms,                       only: random_gauss
  implicit none (type, external)

  integer,          parameter :: n = 2000
  character(len=*), parameter :: logfile = "./test/tests.log"

  character(len=10) :: date="", time=""
  type(String)      :: testlog, errlog
  logical           :: test_succeeded=.true., all_passing=.true.

  character(len=512) :: errmsg=""
  integer            :: stat=0

  type(String), allocatable :: string_var(:)
  real(rk),     allocatable :: x(:)
  integer(ik),  allocatable :: i(:), j(:)

  call random_init(repeatable=.false., image_distinct=.true.)
  call date_and_time(date=date, time=time)

  testlog = String("RUNNING TESTS (String/cast) | date: " + trim(adjustl(date)) + &
                   " | time: "                            + time                + &
                   " | int kind: "                        + str(ik)             )
  call testlog%push(LF + "-"**testlog%len() + LF)

  errlog = String("    ERROR LOG" + LF + "    ---------" + LF)

  allocate( string_var(n), stat=stat, errmsg=errmsg )
  if ( stat /= 0 ) error stop LF + "FATAL: Allocation failure at line " + str(__LINE__ - 1) + &
                              ' of file "' + __FILE__ + '".'
  allocate( x(n), source=0e0_rk, stat=stat, errmsg=errmsg )
  if ( stat /= 0 ) error stop LF + "FATAL: Allocation failure at line " + str(__LINE__ - 1) + &
                              ' of file "' + __FILE__ + '".'
  allocate( i(n), j(n), source=0_ik, stat=stat, errmsg=errmsg )
  if ( stat /= 0 ) error stop LF + "FATAL: Allocation failure at line " + str(__LINE__ - 1) + &
                              ' of file "' + __FILE__ + '".'

  1 continue

  call random_gauss(x,0e0_rk,1e0_rk); i = floor(huge(1_ik)*x, ik) + 1_ik
  call cast(String(i, fmt="i"), into=j, fmt="i")
  test_succeeded = all(i == j)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  2 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); i = floor(huge(1_ik)*x, ik) + 1_ik
  call cast(String(i, fmt="z"), into=j, fmt="z")
  test_succeeded = all(i == j)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  3 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); i = floor(huge(1_ik)*x, ik) + 1_ik
  call cast(i, into=string_var, fmt="i"); call cast(string_var, into=j, fmt="i")
  test_succeeded = all(i == j)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  4 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); i = floor(huge(1_ik)*x, ik) + 1_ik
  call cast(i, into=string_var, fmt="z"); call cast(string_var, into=j, fmt="z")
  test_succeeded = all(i == j)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  5 if ( .not. test_succeeded ) all_passing = .false.

  if ( all_passing ) then
    call testlog%push('All tests are "PASSING" with compiler "'  + compiler_version() + '" ' + &
                      'using compiler options "' + compiler_options() + '".' + LF)
  else
    call testlog%push('Some tests are "FAILING" with compiler "' + compiler_version() + '" ' + &
                      'using compiler options "' + compiler_options() + '".' + LF)
  end if

  call testlog%echo(logfile)
  write(*,"(DT)") testlog

  if ( .not. all_passing ) then
    call errlog%echo(logfile)
    write(*,"(DT)") errlog
  end if
end program main
