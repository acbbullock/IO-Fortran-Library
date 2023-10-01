program main
  use, intrinsic :: iso_fortran_env, only: compiler_version, compiler_options
  use kinds,                         only: rk, ik
  use io_fortran_lib,                only: String, to_file, from_file, LF, SPACE, str, operator(+), operator(**)
  use randoms,                       only: random_gauss
  implicit none (type, external)

  integer,          parameter :: rows = 700, cols = 25
  character(len=*), parameter :: logfile = "./test/tests.log"

  character(len=10) :: date="", time=""
  type(String)      :: testlog, errlog
  logical           :: test_succeeded=.true., all_passing=.true.

  character(len=512) :: errmsg=""
  integer            :: stat=0

  real(rk),    allocatable :: u(:), x(:,:)
  integer(ik), allocatable :: i(:), k(:,:)
  integer(ik), allocatable :: j(:), l(:,:)

  call random_init(repeatable=.false., image_distinct=.true.)
  call date_and_time(date=date, time=time)

  testlog = String("RUNNING TESTS (to_file/from_file) | date: " + trim(adjustl(date)) + &
                   " | time: "                                  + time                + &
                   " | int kind: "                              + str(ik)             )
  call testlog%push(LF + "-"**testlog%len() + LF)

  errlog = String("    ERROR LOG" + LF + "    ---------" + LF)

  allocate( u(rows), x(rows,cols), source=0e0_rk, stat=stat, errmsg=errmsg )
  if ( stat /= 0 ) error stop LF + "FATAL: Allocation failure at line " + str(__LINE__ - 1) + &
                              ' of file "' + __FILE__ + '".'
  allocate( i(rows), k(rows,cols), source=0_ik, stat=stat, errmsg=errmsg )
  if ( stat /= 0 ) error stop LF + "FATAL: Allocation failure at line " + str(__LINE__ - 1) + &
                              ' of file "' + __FILE__ + '".'

  1 continue

  call random_gauss(u,0e0_rk,1e0_rk); i = floor(huge(1_ik)*u, ik) + 1_ik
  call to_file(i, file="./data/i.csv", header=[""], delim=",", fmt="i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 2
  end if
  call from_file("./data/i.csv", into=j, header=.false., delim=",", fmt="i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 2
  end if
  test_succeeded = all(i == j)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  2 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); i = floor(huge(1_ik)*u, ik) + 1_ik
  call to_file(i, file="./data/i_z.csv", header=[""], delim=",", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 3
  end if
  call from_file("./data/i_z.csv", into=j, header=.false., delim=",", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 3
  end if
  test_succeeded = all(i == j)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  3 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); i = floor(huge(1_ik)*u, ik) + 1_ik
  call to_file(i, file="./data/i.csv", header=[""], delim=";", fmt="i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 4
  end if
  call from_file("./data/i.csv", into=j, header=.false., delim=";", fmt="i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 4
  end if
  test_succeeded = all(i == j)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  4 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); i = floor(huge(1_ik)*u, ik) + 1_ik
  call to_file(i, file="./data/i_z.csv", header=[""], delim=";", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 5
  end if
  call from_file("./data/i_z.csv", into=j, header=.false., delim=";", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 5
  end if
  test_succeeded = all(i == j)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  5 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); i = floor(huge(1_ik)*u, ik) + 1_ik
  call to_file(i, file="./data/i.csv", header=["i"], delim=",", fmt="i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 6
  end if
  call from_file("./data/i.csv", into=j, header=.true., delim=",", fmt="i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 6
  end if
  test_succeeded = all(i == j)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  6 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); i = floor(huge(1_ik)*u, ik) + 1_ik
  call to_file(i, file="./data/i_z.csv", header=["i"], delim=",", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 7
  end if
  call from_file("./data/i_z.csv", into=j, header=.true., delim=",", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 7
  end if
  test_succeeded = all(i == j)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  7 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); i = floor(huge(1_ik)*u, ik) + 1_ik
  call to_file(i, file="./data/i.csv", header=["i"], delim=";", fmt="i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 8
  end if
  call from_file("./data/i.csv", into=j, header=.true., delim=";", fmt="i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 8
  end if
  test_succeeded = all(i == j)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  8 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(u,0e0_rk,1e0_rk); i = floor(huge(1_ik)*u, ik) + 1_ik
  call to_file(i, file="./data/i_z.csv", header=["i"], delim=";", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 9
  end if
  call from_file("./data/i_z.csv", into=j, header=.true., delim=";", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 9
  end if
  test_succeeded = all(i == j)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  9 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); k = floor(huge(1_ik)*x, ik) + 1_ik
  call to_file(k, file="./data/k.csv", header=[""], delim=",", fmt="i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 10
  end if
  call from_file("./data/k.csv", into=l, header=.false., delim=",", fmt="i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 10
  end if
  test_succeeded = all(k == l)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  10 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); k = floor(huge(1_ik)*x, ik) + 1_ik
  call to_file(k, file="./data/k_z.csv", header=[""], delim=",", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 11
  end if
  call from_file("./data/k_z.csv", into=l, header=.false., delim=",", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 11
  end if
  test_succeeded = all(k == l)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  11 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); k = floor(huge(1_ik)*x, ik) + 1_ik
  call to_file(k, file="./data/k.csv", header=[""], delim=";", fmt="i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 12
  end if
  call from_file("./data/k.csv", into=l, header=.false., delim=";", fmt="i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 12
  end if
  test_succeeded = all(k == l)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  12 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); k = floor(huge(1_ik)*x, ik) + 1_ik
  call to_file(k, file="./data/k_z.csv", header=[""], delim=";", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 13
  end if
  call from_file("./data/k_z.csv", into=l, header=.false., delim=";", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 13
  end if
  test_succeeded = all(k == l)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  13 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); k = floor(huge(1_ik)*x, ik) + 1_ik
  call to_file(k, file="./data/k.csv", header=["k"], delim=",", fmt="i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 14
  end if
  call from_file("./data/k.csv", into=l, header=.true., delim=",", fmt="i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 14
  end if
  test_succeeded = all(k == l)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  14 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); k = floor(huge(1_ik)*x, ik) + 1_ik
  call to_file(k, file="./data/k_z.csv", header=["k"], delim=",", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 15
  end if
  call from_file("./data/k_z.csv", into=l, header=.true., delim=",", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 15
  end if
  test_succeeded = all(k == l)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  15 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); k = floor(huge(1_ik)*x, ik) + 1_ik
  call to_file(k, file="./data/k.csv", header=["k"], delim=";", fmt="i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 16
  end if
  call from_file("./data/k.csv", into=l, header=.true., delim=";", fmt="i", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 16
  end if
  test_succeeded = all(k == l)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  16 if ( .not. test_succeeded ) all_passing = .false.

  call random_gauss(x,0e0_rk,1e0_rk); k = floor(huge(1_ik)*x, ik) + 1_ik
  call to_file(k, file="./data/k_z.csv", header=["k"], delim=";", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 17
  end if
  call from_file("./data/k_z.csv", into=l, header=.true., delim=";", fmt="z", stat=stat, errmsg=errmsg)
  if ( stat /= 0 ) then
    test_succeeded = .false.
    call errlog%push("    Test failed at line " + str(__LINE__ - 3) + ' of file "' + __FILE__ + &
                     '" with stat ' + str(stat) + " and errmsg '" + trim(adjustl(errmsg)) + "'." + LF)
    go to 17
  end if
  test_succeeded = all(k == l)
  if ( .not. test_succeeded ) then
    call errlog%push("    Test bounds failed at line " + str(__LINE__ - 2) + ' of file "' + __FILE__ + '".' + LF)
  end if

  17 if ( .not. test_succeeded ) all_passing = .false.

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
