program main
    use, intrinsic :: iso_fortran_env, only: ik=>int32, rk=>real32, &
                      compiler_version, compiler_options
    use io_fortran_lib, only: String, to_file, from_file, LF, SPACE, str, operator(+), operator(**)
    use randoms,        only: random_gauss
    implicit none (type, external)

    integer,          parameter :: rows = 700, cols = 25
    character(len=*), parameter :: logfile = "./test/tests.log"

    character(len=10) :: date = repeat(SPACE, len(date)), time = repeat(SPACE, len(time))
    type(String)      :: logmsg
    logical           :: all_passing = .true.

    real(rk),    allocatable :: u(:), x(:,:)
    integer(ik), allocatable :: i(:), k(:,:)
    integer(ik), allocatable :: j(:), l(:,:)

    allocate( u(rows), i(rows), x(rows,cols), k(rows,cols) )
    call random_init(repeatable=.false., image_distinct=.true.)
    call date_and_time(date=date, time=time)

    logmsg = String("RUNNING TESTS (array) | date: " + trim(adjustl(date)) + &
                    " | time: "                      + time                + &
                    " | int kind: "                  + str(ik)             )

    call logmsg%push(LF + "-"**logmsg%len() + LF)

    write(*,"(a)") logmsg%as_str()

    call random_gauss(u,0.0_rk,1.0_rk); i = floor(huge(1_ik)*u, ik) + 1_ik
    call to_file(i, file_name="./data/i.csv", header=[""], dim=2, delim=",", fmt="i")
    call from_file("./data/i.csv", into=j, header=.false., delim=",", fmt="i")
    if ( all(i == j) ) then
        write(*,*) "int 1: SUCCESS"
    else
        write(*,*) "int 1: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk); i = floor(huge(1_ik)*u, ik) + 1_ik
    call to_file(i, file_name="./data/i_z.csv", header=[""], dim=2, delim=",", fmt="z")
    call from_file("./data/i_z.csv", into=j, header=.false., delim=",", fmt="z")
    if ( all(i == j) ) then
        write(*,*) "int 2: SUCCESS"
    else
        write(*,*) "int 2: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk); i = floor(huge(1_ik)*u, ik) + 1_ik
    call to_file(i, file_name="./data/i.csv", header=[""], dim=2, delim=";", fmt="i")
    call from_file("./data/i.csv", into=j, header=.false., delim=";", fmt="i")
    if ( all(i == j) ) then
        write(*,*) "int 3: SUCCESS"
    else
        write(*,*) "int 3: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk); i = floor(huge(1_ik)*u, ik) + 1_ik
    call to_file(i, file_name="./data/i_z.csv", header=[""], dim=2, delim=";", fmt="z")
    call from_file("./data/i_z.csv", into=j, header=.false., delim=";", fmt="z")
    if ( all(i == j) ) then
        write(*,*) "int 4: SUCCESS"
    else
        write(*,*) "int 4: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk); i = floor(huge(1_ik)*u, ik) + 1_ik
    call to_file(i, file_name="./data/i.csv", header=[""], dim=1, delim=",", fmt="i")
    call from_file("./data/i.csv", into=j, header=.false., delim=",", fmt="i")
    if ( all(i == j) ) then
        write(*,*) "int 5: SUCCESS"
    else
        write(*,*) "int 5: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk); i = floor(huge(1_ik)*u, ik) + 1_ik
    call to_file(i, file_name="./data/i_z.csv", header=[""], dim=1, delim=",", fmt="z")
    call from_file("./data/i_z.csv", into=j, header=.false., delim=",", fmt="z")
    if ( all(i == j) ) then
        write(*,*) "int 6: SUCCESS"
    else
        write(*,*) "int 6: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk); i = floor(huge(1_ik)*u, ik) + 1_ik
    call to_file(i, file_name="./data/i.csv", header=[""], dim=1, delim=";", fmt="i")
    call from_file("./data/i.csv", into=j, header=.false., delim=";", fmt="i")
    if ( all(i == j) ) then
        write(*,*) "int 7: SUCCESS"
    else
        write(*,*) "int 7: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk); i = floor(huge(1_ik)*u, ik) + 1_ik
    call to_file(i, file_name="./data/i_z.csv", header=[""], dim=1, delim=";", fmt="z")
    call from_file("./data/i_z.csv", into=j, header=.false., delim=";", fmt="z")
    if ( all(i == j) ) then
        write(*,*) "int 8: SUCCESS"
    else
        write(*,*) "int 8: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk); i = floor(huge(1_ik)*u, ik) + 1_ik
    call to_file(i, file_name="./data/i.csv", header=["i"], dim=2, delim=",", fmt="i")
    call from_file("./data/i.csv", into=j, header=.true., delim=",", fmt="i")
    if ( all(i == j) ) then
        write(*,*) "int 9: SUCCESS"
    else
        write(*,*) "int 9: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk); i = floor(huge(1_ik)*u, ik) + 1_ik
    call to_file(i, file_name="./data/i_z.csv", header=["i"], dim=2, delim=",", fmt="z")
    call from_file("./data/i_z.csv", into=j, header=.true., delim=",", fmt="z")
    if ( all(i == j) ) then
        write(*,*) "int 10: SUCCESS"
    else
        write(*,*) "int 10: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk); i = floor(huge(1_ik)*u, ik) + 1_ik
    call to_file(i, file_name="./data/i.csv", header=["i"], dim=2, delim=";", fmt="i")
    call from_file("./data/i.csv", into=j, header=.true., delim=";", fmt="i")
    if ( all(i == j) ) then
        write(*,*) "int 11: SUCCESS"
    else
        write(*,*) "int 11: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk); i = floor(huge(1_ik)*u, ik) + 1_ik
    call to_file(i, file_name="./data/i_z.csv", header=["i"], dim=2, delim=";", fmt="z")
    call from_file("./data/i_z.csv", into=j, header=.true., delim=";", fmt="z")
    if ( all(i == j) ) then
        write(*,*) "int 12: SUCCESS"
    else
        write(*,*) "int 12: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk); i = floor(huge(1_ik)*u, ik) + 1_ik
    call to_file(i, file_name="./data/i.csv", header=["i"], dim=1, delim=",", fmt="i")
    call from_file("./data/i.csv", into=j, header=.true., delim=",", fmt="i")
    if ( all(i == j) ) then
        write(*,*) "int 13: SUCCESS"
    else
        write(*,*) "int 13: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk); i = floor(huge(1_ik)*u, ik) + 1_ik
    call to_file(i, file_name="./data/i_z.csv", header=["i"], dim=1, delim=",", fmt="z")
    call from_file("./data/i_z.csv", into=j, header=.true., delim=",", fmt="z")
    if ( all(i == j) ) then
        write(*,*) "int 14: SUCCESS"
    else
        write(*,*) "int 14: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk); i = floor(huge(1_ik)*u, ik) + 1_ik
    call to_file(i, file_name="./data/i.csv", header=["i"], dim=1, delim=";", fmt="i")
    call from_file("./data/i.csv", into=j, header=.true., delim=";", fmt="i")
    if ( all(i == j) ) then
        write(*,*) "int 15: SUCCESS"
    else
        write(*,*) "int 15: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk); i = floor(huge(1_ik)*u, ik) + 1_ik
    call to_file(i, file_name="./data/i_z.csv", header=["i"], dim=1, delim=";", fmt="z")
    call from_file("./data/i_z.csv", into=j, header=.true., delim=";", fmt="z")
    if ( all(i == j) ) then
        write(*,*) "int 16: SUCCESS"
    else
        write(*,*) "int 16: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); k = floor(huge(1_ik)*x, ik) + 1_ik
    call to_file(k, file_name="./data/k.csv", header=[""], delim=",", fmt="i")
    call from_file("./data/k.csv", into=l, header=.false., delim=",", fmt="i")
    if ( all(k == l) ) then
        write(*,*) "int 17: SUCCESS"
    else
        write(*,*) "int 17: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); k = floor(huge(1_ik)*x, ik) + 1_ik
    call to_file(k, file_name="./data/k_z.csv", header=[""], delim=",", fmt="z")
    call from_file("./data/k_z.csv", into=l, header=.false., delim=",", fmt="z")
    if ( all(k == l) ) then
        write(*,*) "int 18: SUCCESS"
    else
        write(*,*) "int 18: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); k = floor(huge(1_ik)*x, ik) + 1_ik
    call to_file(k, file_name="./data/k.csv", header=[""], delim=";", fmt="i")
    call from_file("./data/k.csv", into=l, header=.false., delim=";", fmt="i")
    if ( all(k == l) ) then
        write(*,*) "int 19: SUCCESS"
    else
        write(*,*) "int 19: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); k = floor(huge(1_ik)*x, ik) + 1_ik
    call to_file(k, file_name="./data/k_z.csv", header=[""], delim=";", fmt="z")
    call from_file("./data/k_z.csv", into=l, header=.false., delim=";", fmt="z")
    if ( all(k == l) ) then
        write(*,*) "int 20: SUCCESS"
    else
        write(*,*) "int 20: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); k = floor(huge(1_ik)*x, ik) + 1_ik
    call to_file(k, file_name="./data/k.csv", header=["k"], delim=",", fmt="i")
    call from_file("./data/k.csv", into=l, header=.true., delim=",", fmt="i")
    if ( all(k == l) ) then
        write(*,*) "int 21: SUCCESS"
    else
        write(*,*) "int 21: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); k = floor(huge(1_ik)*x, ik) + 1_ik
    call to_file(k, file_name="./data/k_z.csv", header=["k"], delim=",", fmt="z")
    call from_file("./data/k_z.csv", into=l, header=.true., delim=",", fmt="z")
    if ( all(k == l) ) then
        write(*,*) "int 22: SUCCESS"
    else
        write(*,*) "int 22: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); k = floor(huge(1_ik)*x, ik) + 1_ik
    call to_file(k, file_name="./data/k.csv", header=["k"], delim=";", fmt="i")
    call from_file("./data/k.csv", into=l, header=.true., delim=";", fmt="i")
    if ( all(k == l) ) then
        write(*,*) "int 23: SUCCESS"
    else
        write(*,*) "int 23: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); k = floor(huge(1_ik)*x, ik) + 1_ik
    call to_file(k, file_name="./data/k_z.csv", header=["k"], delim=";", fmt="z")
    call from_file("./data/k_z.csv", into=l, header=.true., delim=";", fmt="z")
    if ( all(k == l) ) then
        write(*,*) "int 24: SUCCESS"
    else
        write(*,*) "int 24: FAILURE"
        all_passing = .false.
    end if

    if ( all_passing ) then
        call logmsg%push('All tests are "PASSING" with compiler "'  + compiler_version() + '" ' + &
                         'using compiler options "' + compiler_options() + '".' + LF)
    else
        call logmsg%push('Some tests are "FAILING" with compiler "' + compiler_version() + '" ' + &
                         'using compiler options "' + compiler_options() + '".' + LF)
    end if

    call logmsg%echo(logfile)
    write(*,*)
end program main
