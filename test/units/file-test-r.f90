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

  real(rk), allocatable :: u(:), x(:,:)
  real(rk), allocatable :: v(:), y(:,:)

  call random_init(repeatable=.false., image_distinct=.true.)
  call date_and_time(date=date, time=time)

  testlog = String("RUNNING TESTS (to_file/from_file) | date: " + trim(adjustl(date)) + &
                   " | time: "                                  + time                + &
                   " | real kind: "                             + str(rk)             )
  call testlog%push(LF + "-"**testlog%len() + LF)

  errlog = String("    ERROR LOG" + LF + "    ---------" + LF)

  allocate( u(rows), x(rows,cols), source=0e0_rk, stat=stat, errmsg=errmsg )
  if ( stat /= 0 ) error stop LF + "FATAL: Allocation failure at line " + str(__LINE__ - 1) + &
                              ' of file "' + __FILE__ + '".'

  1 continue

  call random_gauss(u,0e0_rk,1e0_rk)
  call to_file(u, file="./data/u_e.csv", header=["u"], locale="EU", fmt="e", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 2
  end if
  call from_file("./data/u_e.csv", into=v, header=.true., locale="EU", fmt="e", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 2
  end if
  test_succeeded = (maxval( abs(u-v)/abs(u) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  2 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk)
  call to_file(u, file="./data/u_f.csv", header=["u"], locale="EU", fmt="f", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 3
  end if
  call from_file("./data/u_f.csv", into=v, header=.true., locale="EU", fmt="f", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 3
  end if
  test_succeeded = (maxval( abs(u-v)/abs(u) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  3 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk)
  call to_file(u, file="./data/u_z.csv", header=["u"], locale="EU", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 4
  end if
  call from_file("./data/u_z.csv", into=v, header=.true., locale="EU", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 4
  end if
  test_succeeded = (maxval( abs(u-v)/abs(u) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  4 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk)
  call to_file(u, file="./data/u_e.csv", header=["u"], locale="US", fmt="e", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 5
  end if
  call from_file("./data/u_e.csv", into=v, header=.true., locale="US", fmt="e", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 5
  end if
  test_succeeded = (maxval( abs(u-v)/abs(u) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  5 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk)
  call to_file(u, file="./data/u_f.csv", header=["u"], locale="US", fmt="f", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 6
  end if
  call from_file("./data/u_f.csv", into=v, header=.true., locale="US", fmt="f", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 6
  end if
  test_succeeded = (maxval( abs(u-v)/abs(u) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  6 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk)
  call to_file(u, file="./data/u_z.csv", header=["u"], locale="US", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 7
  end if
  call from_file("./data/u_z.csv", into=v, header=.true., locale="US", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 7
  end if
  test_succeeded = (maxval( abs(u-v)/abs(u) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  7 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk)
  call to_file(x, file="./data/x_e.csv", header=[""], locale="EU", fmt="e", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 8
  end if
  call from_file("./data/x_e.csv", into=y, header=.false., locale="EU", fmt="e", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 8
  end if
  test_succeeded = (maxval( abs(x-y)/abs(x) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  8 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk)
  call to_file(x, file="./data/x_f.csv", header=[""], locale="EU", fmt="f", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 9
  end if
  call from_file("./data/x_f.csv", into=y, header=.false., locale="EU", fmt="f", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 9
  end if
  test_succeeded = (maxval( abs(x-y)/abs(x) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  9 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk)
  call to_file(x, file="./data/x_z.csv", header=[""], locale="EU", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 10
  end if
  call from_file("./data/x_z.csv", into=y, header=.false., locale="EU", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 10
  end if
  test_succeeded = (maxval( abs(x-y)/abs(x) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  10 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk)
  call to_file(x, file="./data/x_e.csv", header=[""], locale="US", fmt="e", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 11
  end if
  call from_file("./data/x_e.csv", into=y, header=.false., locale="US", fmt="e", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 11
  end if
  test_succeeded = (maxval( abs(x-y)/abs(x) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  11 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk)
  call to_file(x, file="./data/x_f.csv", header=[""], locale="US", fmt="f", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 12
  end if
  call from_file("./data/x_f.csv", into=y, header=.false., locale="US", fmt="f", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 12
  end if
  test_succeeded = (maxval( abs(x-y)/abs(x) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  12 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk)
  call to_file(x, file="./data/x_z.csv", header=[""], locale="US", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 13
  end if
  call from_file("./data/x_z.csv", into=y, header=.false., locale="US", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 13
  end if
  test_succeeded = (maxval( abs(x-y)/abs(x) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  13 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk)
  call to_file(x, file="./data/x_e.csv", header=["x"], locale="EU", fmt="e", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 14
  end if
  call from_file("./data/x_e.csv", into=y, header=.true., locale="EU", fmt="e", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 14
  end if
  test_succeeded = (maxval( abs(x-y)/abs(x) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  14 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk)
  call to_file(x, file="./data/x_f.csv", header=["x"], locale="EU", fmt="f", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 15
  end if
  call from_file("./data/x_f.csv", into=y, header=.true., locale="EU", fmt="f", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 15
  end if
  test_succeeded = (maxval( abs(x-y)/abs(x) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  15 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk)
  call to_file(x, file="./data/x_z.csv", header=["x"], locale="EU", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 16
  end if
  call from_file("./data/x_z.csv", into=y, header=.true., locale="EU", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 16
  end if
  test_succeeded = (maxval( abs(x-y)/abs(x) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  16 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk)
  call to_file(x, file="./data/x_e.csv", header=["x"], locale="US", fmt="e", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 17
  end if
  call from_file("./data/x_e.csv", into=y, header=.true., locale="US", fmt="e", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 17
  end if
  test_succeeded = (maxval( abs(x-y)/abs(x) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  17 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk)
  call to_file(x, file="./data/x_f.csv", header=["x"], locale="US", fmt="f", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 18
  end if
  call from_file("./data/x_f.csv", into=y, header=.true., locale="US", fmt="f", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 18
  end if
  test_succeeded = (maxval( abs(x-y)/abs(x) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  18 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk)
  call to_file(x, file="./data/x_z.csv", header=["x"], locale="US", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 19
  end if
  call from_file("./data/x_z.csv", into=y, header=.true., locale="US", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 19
  end if
  test_succeeded = (maxval( abs(x-y)/abs(x) ) < tol)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  19 if ( .not. test_succeeded ) all_passing = .false.

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
