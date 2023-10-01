program main
  use, intrinsic :: iso_fortran_env, only: compiler_version, compiler_options
  use kinds,                         only: rk
  use io_fortran_lib,                only: String, cast, LF, SPACE, str, operator(+), operator(**)
  use randoms,                       only: random_gauss
  implicit none (type, external)

  real(rk),         parameter :: tol = 3.0_rk*epsilon(1e0_rk)
  integer,          parameter :: n = 2000
  character(len=*), parameter :: logfile = "./test/tests.log"

  character(len=10) :: date="", time=""
  type(String)      :: testlog, errlog
  logical           :: test_succeeded=.true., all_passing=.true.

  character(len=512) :: errmsg=""
  integer            :: stat=0

  type(String), allocatable :: string_var(:)
  real(rk),     allocatable :: x(:), y(:)
  complex(rk),  allocatable :: z1(:), z2(:)

  call random_init(repeatable=.false., image_distinct=.true.)
  call date_and_time(date=date, time=time)

  testlog = String("RUNNING TESTS (String/cast) | date: " + trim(adjustl(date)) + &
                   " | time: "                            + time                + &
                   " | complex kind: "                    + str(rk)             )
  call testlog%push(LF + "-"**testlog%len() + LF)

  errlog = String("    ERROR LOG" + LF + "    ---------" + LF)

  allocate( string_var(n), stat=stat, errmsg=errmsg )
  if ( stat /= 0 ) error stop LF + "FATAL: Allocation failure at line " + str(__LINE__ - 1) + &
                              ' of file "' + __FILE__ + '".'
  allocate( x(n), y(n), source=0e0_rk, stat=stat, errmsg=errmsg )
  if ( stat /= 0 ) error stop LF + "FATAL: Allocation failure at line " + str(__LINE__ - 1) + &
                              ' of file "' + __FILE__ + '".'
  allocate( z1(n), z2(n), source=(0e0_rk,0e0_rk), stat=stat, errmsg=errmsg )
  if ( stat /= 0 ) error stop LF + "FATAL: Allocation failure at line " + str(__LINE__ - 1) + &
                              ' of file "' + __FILE__ + '".'

  1 continue

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(String(z1, locale="US", fmt="e", im=""), into=z2, locale="US", fmt="e", im="")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  2 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(String(z1, locale="US", fmt="f", im=""), into=z2, locale="US", fmt="f", im="")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  3 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(String(z1, locale="US", fmt="z", im=""), into=z2, locale="US", fmt="z", im="")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  4 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(String(z1, locale="US", fmt="e", im="j"), into=z2, locale="US", fmt="e", im="j")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  5 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(String(z1, locale="US", fmt="f", im="j"), into=z2, locale="US", fmt="f", im="j")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  6 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(String(z1, locale="US", fmt="z", im="j"), into=z2, locale="US", fmt="z", im="j")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  7 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(String(z1, locale="US", fmt="e", im="*1i"), into=z2, locale="US", fmt="e", im="*1i")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  8 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(String(z1, locale="US", fmt="f", im="*1i"), into=z2, locale="US", fmt="f", im="*1i")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  9 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(String(z1, locale="US", fmt="z", im="*1i"), into=z2, locale="US", fmt="z", im="*1i")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  10 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(String(z1, locale="EU", fmt="e", im=""), into=z2, locale="EU", fmt="e", im="")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  11 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(String(z1, locale="EU", fmt="f", im=""), into=z2, locale="EU", fmt="f", im="")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  12 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(String(z1, locale="EU", fmt="z", im=""), into=z2, locale="EU", fmt="z", im="")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  13 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(String(z1, locale="EU", fmt="e", im="j"), into=z2, locale="EU", fmt="e", im="j")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  14 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(String(z1, locale="EU", fmt="f", im="j"), into=z2, locale="EU", fmt="f", im="j")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  15 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(String(z1, locale="EU", fmt="z", im="j"), into=z2, locale="EU", fmt="z", im="j")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  16 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(String(z1, locale="EU", fmt="e", im="*1i"), into=z2, locale="EU", fmt="e", im="*1i")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  17 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(String(z1, locale="EU", fmt="f", im="*1i"), into=z2, locale="EU", fmt="f", im="*1i")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  18 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(String(z1, locale="EU", fmt="z", im="*1i"), into=z2, locale="EU", fmt="z", im="*1i")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  19 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(z1, into=string_var, locale="US", fmt="e", im="")
  call cast(string_var, into=z2, locale="US", fmt="e", im="")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  20 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(z1, into=string_var, locale="US", fmt="f", im="")
  call cast(string_var, into=z2, locale="US", fmt="f", im="")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  21 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(z1, into=string_var, locale="US", fmt="z", im="")
  call cast(string_var, into=z2, locale="US", fmt="z", im="")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  22 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(z1, into=string_var, locale="US", fmt="e", im="j")
  call cast(string_var, into=z2, locale="US", fmt="e", im="j")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  23 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(z1, into=string_var, locale="US", fmt="f", im="j")
  call cast(string_var, into=z2, locale="US", fmt="f", im="j")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  24 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(z1, into=string_var, locale="US", fmt="z", im="j")
  call cast(string_var, into=z2, locale="US", fmt="z", im="j")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  25 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(z1, into=string_var, locale="US", fmt="e", im="*1i")
  call cast(string_var, into=z2, locale="US", fmt="e", im="*1i")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  26 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(z1, into=string_var, locale="US", fmt="f", im="*1i")
  call cast(string_var, into=z2, locale="US", fmt="f", im="*1i")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  27 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(z1, into=string_var, locale="US", fmt="z", im="*1i")
  call cast(string_var, into=z2, locale="US", fmt="z", im="*1i")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  28 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(z1, into=string_var, locale="EU", fmt="e", im="")
  call cast(string_var, into=z2, locale="EU", fmt="e", im="")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  29 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(z1, into=string_var, locale="EU", fmt="f", im="")
  call cast(string_var, into=z2, locale="EU", fmt="f", im="")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  30 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(z1, into=string_var, locale="EU", fmt="z", im="")
  call cast(string_var, into=z2, locale="EU", fmt="z", im="")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  31 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(z1, into=string_var, locale="EU", fmt="e", im="j")
  call cast(string_var, into=z2, locale="EU", fmt="e", im="j")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  32 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(z1, into=string_var, locale="EU", fmt="f", im="j")
  call cast(string_var, into=z2, locale="EU", fmt="f", im="j")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  33 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(z1, into=string_var, locale="EU", fmt="z", im="j")
  call cast(string_var, into=z2, locale="EU", fmt="z", im="j")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  34 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(z1, into=string_var, locale="EU", fmt="e", im="*1i")
  call cast(string_var, into=z2, locale="EU", fmt="e", im="*1i")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  35 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(z1, into=string_var, locale="EU", fmt="f", im="*1i")
  call cast(string_var, into=z2, locale="EU", fmt="f", im="*1i")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  36 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); z1 = cmplx(x,y,rk)
  call cast(z1, into=string_var, locale="EU", fmt="z", im="*1i")
  call cast(string_var, into=z2, locale="EU", fmt="z", im="*1i")
  test_succeeded = (maxval( abs(z1-z2)/abs(z1) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  37 if ( .not. test_succeeded ) all_passing = .false.

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
