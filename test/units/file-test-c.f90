program main
  use, intrinsic :: iso_fortran_env, only: compiler_version, compiler_options
  use kinds,                         only: rk
  use io_fortran_lib,                only: String, to_file, from_file, LF, SPACE, str, operator(+), operator(**)
  use randoms,                       only: random_gauss
  implicit none (type, external)

  real(rk),         parameter :: tol = 3.0_rk*epsilon(1e0_rk)
  integer,          parameter :: rows = 700, cols = 25
  character(len=*), parameter :: logfile = "./test/tests.log"

  character(len=10) :: date="", time=""
  type(String)      :: testlog, errlog
  logical           :: test_succeeded=.true., all_passing=.true.

  character(len=512) :: errmsg=""
  integer            :: stat=0

  real(rk),    allocatable :: u(:), v(:), x(:,:), y(:,:)
  complex(rk), allocatable :: a(:), b(:), c(:,:), d(:,:)

  call random_init(repeatable=.false., image_distinct=.true.)
  call date_and_time(date=date, time=time)

  testlog = String("RUNNING TESTS (to_file/from_file) | date: " + trim(adjustl(date)) + &
                   " | time: "                     + time                             + &
                   " | complex kind: "             + str(rk)                          )
  call testlog%push(LF + "-"**testlog%len() + LF)

  errlog = String("    ERROR LOG" + LF + "    ---------" + LF)

  allocate( u(rows), v(rows), x(rows,cols), y(rows,cols), source=0e0_rk, stat=stat, errmsg=errmsg )
  if ( stat /= 0 ) error stop LF + "FATAL: Allocation failure at line " + str(__LINE__ - 1) + &
                              ' of file "' + __FILE__ + '".'

  1 continue

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_e.csv", header=[""], locale="EU", fmt="e", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 2
  end if
  call from_file("./data/a_e.csv", into=b, header=.false., locale="EU", fmt="e", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 2
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  2 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_f.csv", header=[""], locale="EU", fmt="f", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 3
  end if
  call from_file("./data/a_f.csv", into=b, header=.false., locale="EU", fmt="f", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 3
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  3 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_z.csv", header=[""], locale="EU", fmt="z", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 4
  end if
  call from_file("./data/a_z.csv", into=b, header=.false., locale="EU", fmt="z", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 4
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  4 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_e.csv", header=[""], locale="US", fmt="e", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 5
  end if
  call from_file("./data/a_e.csv", into=b, header=.false., locale="US", fmt="e", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 5
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  5 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_f.csv", header=[""], locale="US", fmt="f", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 6
  end if
  call from_file("./data/a_f.csv", into=b, header=.false., locale="US", fmt="f", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 6
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  6 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_z.csv", header=[""], locale="US", fmt="z", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 7
  end if
  call from_file("./data/a_z.csv", into=b, header=.false., locale="US", fmt="z", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 7
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  7 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_e.csv", header=[""], locale="EU", fmt="e", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 8
  end if
  call from_file("./data/a_e.csv", into=b, header=.false., locale="EU", fmt="e", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 8
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  8 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_f.csv", header=[""], locale="EU", fmt="f", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 9
  end if
  call from_file("./data/a_f.csv", into=b, header=.false., locale="EU", fmt="f", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 9
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  9 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_z.csv", header=[""], locale="EU", fmt="z", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 10
  end if
  call from_file("./data/a_z.csv", into=b, header=.false., locale="EU", fmt="z", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 10
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  10 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_e.csv", header=[""], locale="US", fmt="e", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 11
  end if
  call from_file("./data/a_e.csv", into=b, header=.false., locale="US", fmt="e", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 11
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  11 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_f.csv", header=[""], locale="US", fmt="f", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 12
  end if
  call from_file("./data/a_f.csv", into=b, header=.false., locale="US", fmt="f", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 12
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  12 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_z.csv", header=[""], locale="US", fmt="z", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 13
  end if
  call from_file("./data/a_z.csv", into=b, header=.false., locale="US", fmt="z", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 13
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  13 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_e.csv", header=[""], locale="EU", fmt="e", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 14
  end if
  call from_file("./data/a_e.csv", into=b, header=.false., locale="EU", fmt="e", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 14
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  14 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_f.csv", header=[""], locale="EU", fmt="f", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 15
  end if
  call from_file("./data/a_f.csv", into=b, header=.false., locale="EU", fmt="f", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 15
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  15 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_z.csv", header=[""], locale="EU", fmt="z", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 16
  end if
  call from_file("./data/a_z.csv", into=b, header=.false., locale="EU", fmt="z", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 16
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  16 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_e.csv", header=[""], locale="US", fmt="e", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 17
  end if
  call from_file("./data/a_e.csv", into=b, header=.false., locale="US", fmt="e", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 17
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  17 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_f.csv", header=[""], locale="US", fmt="f", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 18
  end if
  call from_file("./data/a_f.csv", into=b, header=.false., locale="US", fmt="f", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 18
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  18 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_z.csv", header=[""], locale="US", fmt="z", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 19
  end if
  call from_file("./data/a_z.csv", into=b, header=.false., locale="US", fmt="z", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 19
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  19 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_e.csv", header=["a"], locale="EU", fmt="e", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 20
  end if
  call from_file("./data/a_e.csv", into=b, header=.true., locale="EU", fmt="e", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 20
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  20 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_f.csv", header=["a"], locale="EU", fmt="f", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 21
  end if
  call from_file("./data/a_f.csv", into=b, header=.true., locale="EU", fmt="f", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 21
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  21 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_z.csv", header=["a"], locale="EU", fmt="z", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 22
  end if
  call from_file("./data/a_z.csv", into=b, header=.true., locale="EU", fmt="z", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 22
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  22 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_e.csv", header=["a"], locale="US", fmt="e", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 23
  end if
  call from_file("./data/a_e.csv", into=b, header=.true., locale="US", fmt="e", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 23
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  23 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_f.csv", header=["a"], locale="US", fmt="f", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 24
  end if
  call from_file("./data/a_f.csv", into=b, header=.true., locale="US", fmt="f", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 24
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  24 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_z.csv", header=["a"], locale="US", fmt="z", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 25
  end if
  call from_file("./data/a_z.csv", into=b, header=.true., locale="US", fmt="z", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 25
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  25 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_e.csv", header=["a"], locale="EU", fmt="e", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 26
  end if
  call from_file("./data/a_e.csv", into=b, header=.true., locale="EU", fmt="e", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 26
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  26 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_f.csv", header=["a"], locale="EU", fmt="f", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 27
  end if
  call from_file("./data/a_f.csv", into=b, header=.true., locale="EU", fmt="f", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 27
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  27 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_z.csv", header=["a"], locale="EU", fmt="z", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 28
  end if
  call from_file("./data/a_z.csv", into=b, header=.true., locale="EU", fmt="z", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 28
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  28 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_e.csv", header=["a"], locale="US", fmt="e", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 29
  end if
  call from_file("./data/a_e.csv", into=b, header=.true., locale="US", fmt="e", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 29
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  29 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_f.csv", header=["a"], locale="US", fmt="f", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 30
  end if
  call from_file("./data/a_f.csv", into=b, header=.true., locale="US", fmt="f", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 30
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  30 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_z.csv", header=["a"], locale="US", fmt="z", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 31
  end if
  call from_file("./data/a_z.csv", into=b, header=.true., locale="US", fmt="z", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 31
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  31 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_e.csv", header=["a"], locale="EU", fmt="e", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 32
  end if
  call from_file("./data/a_e.csv", into=b, header=.true., locale="EU", fmt="e", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 32
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  32 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_f.csv", header=["a"], locale="EU", fmt="f", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 33
  end if
  call from_file("./data/a_f.csv", into=b, header=.true., locale="EU", fmt="f", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 33
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  33 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_z.csv", header=["a"], locale="EU", fmt="z", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 34
  end if
  call from_file("./data/a_z.csv", into=b, header=.true., locale="EU", fmt="z", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 34
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  34 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_e.csv", header=["a"], locale="US", fmt="e", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 35
  end if
  call from_file("./data/a_e.csv", into=b, header=.true., locale="US", fmt="e", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 35
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  35 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_f.csv", header=["a"], locale="US", fmt="f", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 36
  end if
  call from_file("./data/a_f.csv", into=b, header=.true., locale="US", fmt="f", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 36
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  36 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
  call to_file(a, file="./data/a_z.csv", header=["a"], locale="US", fmt="z", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 37
  end if
  call from_file("./data/a_z.csv", into=b, header=.true., locale="US", fmt="z", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 37
  end if
  test_succeeded = (maxval( abs(a-b)/abs(a) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  37 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_e.csv", header=[""], locale="EU", fmt="e", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 38
  end if
  call from_file("./data/c_e.csv", into=d, header=.false., locale="EU", fmt="e", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 38
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  38 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_f.csv", header=[""], locale="EU", fmt="f", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 39
  end if
  call from_file("./data/c_f.csv", into=d, header=.false., locale="EU", fmt="f", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 39
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  39 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_z.csv", header=[""], locale="EU", fmt="z", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 40
  end if
  call from_file("./data/c_z.csv", into=d, header=.false., locale="EU", fmt="z", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 40
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  40 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_e.csv", header=[""], locale="US", fmt="e", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 41
  end if
  call from_file("./data/c_e.csv", into=d, header=.false., locale="US", fmt="e", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 41
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  41 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_f.csv", header=[""], locale="US", fmt="f", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 42
  end if
  call from_file("./data/c_f.csv", into=d, header=.false., locale="US", fmt="f", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 42
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  42 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_z.csv", header=[""], locale="US", fmt="z", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 43
  end if
  call from_file("./data/c_z.csv", into=d, header=.false., locale="US", fmt="z", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 43
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  43 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_e.csv", header=[""], locale="EU", fmt="e", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 44
  end if
  call from_file("./data/c_e.csv", into=d, header=.false., locale="EU", fmt="e", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 44
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  44 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_f.csv", header=[""], locale="EU", fmt="f", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 45
  end if
  call from_file("./data/c_f.csv", into=d, header=.false., locale="EU", fmt="f", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 45
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  45 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_z.csv", header=[""], locale="EU", fmt="z", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 46
  end if
  call from_file("./data/c_z.csv", into=d, header=.false., locale="EU", fmt="z", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 46
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  46 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_e.csv", header=[""], locale="US", fmt="e", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 47
  end if
  call from_file("./data/c_e.csv", into=d, header=.false., locale="US", fmt="e", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 47
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  47 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_f.csv", header=[""], locale="US", fmt="f", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 48
  end if
  call from_file("./data/c_f.csv", into=d, header=.false., locale="US", fmt="f", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 48
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  48 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_z.csv", header=[""], locale="US", fmt="z", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 49
  end if
  call from_file("./data/c_z.csv", into=d, header=.false., locale="US", fmt="z", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 49
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  49 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_e.csv", header=[""], locale="EU", fmt="e", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 50
  end if
  call from_file("./data/c_e.csv", into=d, header=.false., locale="EU", fmt="e", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 50
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  50 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_f.csv", header=[""], locale="EU", fmt="f", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 51
  end if
  call from_file("./data/c_f.csv", into=d, header=.false., locale="EU", fmt="f", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 51
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  51 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_z.csv", header=[""], locale="EU", fmt="z", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 52
  end if
  call from_file("./data/c_z.csv", into=d, header=.false., locale="EU", fmt="z", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 52
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  52 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_e.csv", header=[""], locale="US", fmt="e", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 53
  end if
  call from_file("./data/c_e.csv", into=d, header=.false., locale="US", fmt="e", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 53
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  53 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_f.csv", header=[""], locale="US", fmt="f", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 54
  end if
  call from_file("./data/c_f.csv", into=d, header=.false., locale="US", fmt="f", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 54
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  54 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_z.csv", header=[""], locale="US", fmt="z", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 55
  end if
  call from_file("./data/c_z.csv", into=d, header=.false., locale="US", fmt="z", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 55
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  55 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_e.csv", header=["c"], locale="EU", fmt="e", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 56
  end if
  call from_file("./data/c_e.csv", into=d, header=.true., locale="EU", fmt="e", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 56
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  56 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_f.csv", header=["c"], locale="EU", fmt="f", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 57
  end if
  call from_file("./data/c_f.csv", into=d, header=.true., locale="EU", fmt="f", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 57
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  57 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_z.csv", header=["c"], locale="EU", fmt="z", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 58
  end if
  call from_file("./data/c_z.csv", into=d, header=.true., locale="EU", fmt="z", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 58
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  58 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_e.csv", header=["c"], locale="US", fmt="e", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 59
  end if
  call from_file("./data/c_e.csv", into=d, header=.true., locale="US", fmt="e", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 59
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  59 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_f.csv", header=["c"], locale="US", fmt="f", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 60
  end if
  call from_file("./data/c_f.csv", into=d, header=.true., locale="US", fmt="f", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 60
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  60 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_z.csv", header=["c"], locale="US", fmt="z", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 61
  end if
  call from_file("./data/c_z.csv", into=d, header=.true., locale="US", fmt="z", im="", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 61
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  61 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_e.csv", header=["c"], locale="EU", fmt="e", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 62
  end if
  call from_file("./data/c_e.csv", into=d, header=.true., locale="EU", fmt="e", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 62
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  62 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_f.csv", header=["c"], locale="EU", fmt="f", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 63
  end if
  call from_file("./data/c_f.csv", into=d, header=.true., locale="EU", fmt="f", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 63
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  63 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_z.csv", header=["c"], locale="EU", fmt="z", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 64
  end if
  call from_file("./data/c_z.csv", into=d, header=.true., locale="EU", fmt="z", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 64
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  64 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_e.csv", header=["c"], locale="US", fmt="e", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 65
  end if
  call from_file("./data/c_e.csv", into=d, header=.true., locale="US", fmt="e", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 65
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  65 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_f.csv", header=["c"], locale="US", fmt="f", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 66
  end if
  call from_file("./data/c_f.csv", into=d, header=.true., locale="US", fmt="f", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 66
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  66 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_z.csv", header=["c"], locale="US", fmt="z", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 67
  end if
  call from_file("./data/c_z.csv", into=d, header=.true., locale="US", fmt="z", im="*1i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 67
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  67 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_e.csv", header=["c"], locale="EU", fmt="e", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 68
  end if
  call from_file("./data/c_e.csv", into=d, header=.true., locale="EU", fmt="e", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 68
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  68 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_f.csv", header=["c"], locale="EU", fmt="f", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 69
  end if
  call from_file("./data/c_f.csv", into=d, header=.true., locale="EU", fmt="f", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 69
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  69 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_z.csv", header=["c"], locale="EU", fmt="z", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 70
  end if
  call from_file("./data/c_z.csv", into=d, header=.true., locale="EU", fmt="z", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 70
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  70 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_e.csv", header=["c"], locale="US", fmt="e", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 71
  end if
  call from_file("./data/c_e.csv", into=d, header=.true., locale="US", fmt="e", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 71
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  71 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_f.csv", header=["c"], locale="US", fmt="f", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 72
  end if
  call from_file("./data/c_f.csv", into=d, header=.true., locale="US", fmt="f", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 72
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  72 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
  call to_file(c, file="./data/c_z.csv", header=["c"], locale="US", fmt="z", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 73
  end if
  call from_file("./data/c_z.csv", into=d, header=.true., locale="US", fmt="z", im="j", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 73
  end if
  test_succeeded = (maxval( abs(c-d)/abs(c) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  73 if ( .not. test_succeeded ) all_passing = .false.

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
