program main
    use, intrinsic :: iso_fortran_env, only: rk=>real32, &
                      compiler_version, compiler_options
    use io_fortran_lib, only: String, to_file, from_file, LF, str, operator(+), operator(**)
    use randoms,        only: random_gauss
    implicit none (type, external)

    real(rk),         parameter :: tol = 3.0_rk*epsilon(1.0_rk)
    integer,          parameter :: rows = 700, cols = 25
    character(len=*), parameter :: logfile = "./test/tests.log"

    character(len=10) :: date, time
    type(String)      :: logmsg
    logical           :: all_passing

    real(rk), allocatable :: u(:), x(:,:)
    real(rk), allocatable :: v(:), y(:,:)

    allocate( u(rows), x(rows,cols) )
    call random_init(repeatable=.false., image_distinct=.true.)
    call date_and_time(date=date, time=time)

    logmsg = String("RUNNING TESTS (array) | date: " + trim(adjustl(date)) + &
                    " | time: "                      + time                + &
                    " | real kind: "                 + str(rk)             )

    call logmsg%push(LF + "-"**logmsg%len() + LF)

    all_passing = .true.
    write(*,"(a)") logmsg%as_str()

    call random_gauss(u,0.0_rk,1.0_rk)
    call to_file(u, file_name="./data/u_e.csv", header=[""], dim=2, locale="EU", fmt="e")
    call from_file("./data/u_e.csv", into=v, header=.false., locale="EU", fmt="e")
    if ( maxval( abs(u-v)/abs(u) ) < tol ) then
        write(*,*) "real 1: SUCCESS"
    else
        write(*,*) "real 1: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk)
    call to_file(u, file_name="./data/u_f.csv", header=[""], dim=2, locale="EU", fmt="f")
    call from_file("./data/u_f.csv", into=v, header=.false., locale="EU", fmt="f")
    if ( maxval( abs(u-v)/abs(u) ) < tol ) then
        write(*,*) "real 2: SUCCESS"
    else
        write(*,*) "real 2: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk)
    call to_file(u, file_name="./data/u_z.csv", header=[""], dim=2, locale="EU", fmt="z")
    call from_file("./data/u_z.csv", into=v, header=.false., locale="EU", fmt="z")
    if ( maxval( abs(u-v)/abs(u) ) < tol ) then
        write(*,*) "real 3: SUCCESS"
    else
        write(*,*) "real 3: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk)
    call to_file(u, file_name="./data/u_e.csv", header=[""], dim=2, locale="US", fmt="e")
    call from_file("./data/u_e.csv", into=v, header=.false., locale="US", fmt="e")
    if ( maxval( abs(u-v)/abs(u) ) < tol ) then
        write(*,*) "real 4: SUCCESS"
    else
        write(*,*) "real 4: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk)
    call to_file(u, file_name="./data/u_f.csv", header=[""], dim=2, locale="US", fmt="f")
    call from_file("./data/u_f.csv", into=v, header=.false., locale="US", fmt="f")
    if ( maxval( abs(u-v)/abs(u) ) < tol ) then
        write(*,*) "real 5: SUCCESS"
    else
        write(*,*) "real 5: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk)
    call to_file(u, file_name="./data/u_z.csv", header=[""], dim=2, locale="US", fmt="z")
    call from_file("./data/u_z.csv", into=v, header=.false., locale="US", fmt="z")
    if ( maxval( abs(u-v)/abs(u) ) < tol ) then
        write(*,*) "real 6: SUCCESS"
    else
        write(*,*) "real 6: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk)
    call to_file(u, file_name="./data/u_e.csv", header=[""], dim=1, locale="EU", fmt="e")
    call from_file("./data/u_e.csv", into=v, header=.false., locale="EU", fmt="e")
    if ( maxval( abs(u-v)/abs(u) ) < tol ) then
        write(*,*) "real 7: SUCCESS"
    else
        write(*,*) "real 7: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk)
    call to_file(u, file_name="./data/u_f.csv", header=[""], dim=1, locale="EU", fmt="f")
    call from_file("./data/u_f.csv", into=v, header=.false., locale="EU", fmt="f")
    if ( maxval( abs(u-v)/abs(u) ) < tol ) then
        write(*,*) "real 8: SUCCESS"
    else
        write(*,*) "real 8: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk)
    call to_file(u, file_name="./data/u_z.csv", header=[""], dim=1, locale="EU", fmt="z")
    call from_file("./data/u_z.csv", into=v, header=.false., locale="EU", fmt="z")
    if ( maxval( abs(u-v)/abs(u) ) < tol ) then
        write(*,*) "real 9: SUCCESS"
    else
        write(*,*) "real 9: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk)
    call to_file(u, file_name="./data/u_e.csv", header=[""], dim=1, locale="US", fmt="e")
    call from_file("./data/u_e.csv", into=v, header=.false., locale="US", fmt="e")
    if ( maxval( abs(u-v)/abs(u) ) < tol ) then
        write(*,*) "real 10: SUCCESS"
    else
        write(*,*) "real 10: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk)
    call to_file(u, file_name="./data/u_f.csv", header=[""], dim=1, locale="US", fmt="f")
    call from_file("./data/u_f.csv", into=v, header=.false., locale="US", fmt="f")
    if ( maxval( abs(u-v)/abs(u) ) < tol ) then
        write(*,*) "real 11: SUCCESS"
    else
        write(*,*) "real 11: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk)
    call to_file(u, file_name="./data/u_z.csv", header=[""], dim=1, locale="US", fmt="z")
    call from_file("./data/u_z.csv", into=v, header=.false., locale="US", fmt="z")
    if ( maxval( abs(u-v)/abs(u) ) < tol ) then
        write(*,*) "real 12: SUCCESS"
    else
        write(*,*) "real 12: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk)
    call to_file(u, file_name="./data/u_e.csv", header=["u"], dim=2, locale="EU", fmt="e")
    call from_file("./data/u_e.csv", into=v, header=.true., locale="EU", fmt="e")
    if ( maxval( abs(u-v)/abs(u) ) < tol ) then
        write(*,*) "real 13: SUCCESS"
    else
        write(*,*) "real 13: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk)
    call to_file(u, file_name="./data/u_f.csv", header=["u"], dim=2, locale="EU", fmt="f")
    call from_file("./data/u_f.csv", into=v, header=.true., locale="EU", fmt="f")
    if ( maxval( abs(u-v)/abs(u) ) < tol ) then
        write(*,*) "real 14: SUCCESS"
    else
        write(*,*) "real 14: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk)
    call to_file(u, file_name="./data/u_z.csv", header=["u"], dim=2, locale="EU", fmt="z")
    call from_file("./data/u_z.csv", into=v, header=.true., locale="EU", fmt="z")
    if ( maxval( abs(u-v)/abs(u) ) < tol ) then
        write(*,*) "real 15: SUCCESS"
    else
        write(*,*) "real 15: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk)
    call to_file(u, file_name="./data/u_e.csv", header=["u"], dim=2, locale="US", fmt="e")
    call from_file("./data/u_e.csv", into=v, header=.true., locale="US", fmt="e")
    if ( maxval( abs(u-v)/abs(u) ) < tol ) then
        write(*,*) "real 16: SUCCESS"
    else
        write(*,*) "real 16: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk)
    call to_file(u, file_name="./data/u_f.csv", header=["u"], dim=2, locale="US", fmt="f")
    call from_file("./data/u_f.csv", into=v, header=.true., locale="US", fmt="f")
    if ( maxval( abs(u-v)/abs(u) ) < tol ) then
        write(*,*) "real 17: SUCCESS"
    else
        write(*,*) "real 17: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk)
    call to_file(u, file_name="./data/u_z.csv", header=["u"], dim=2, locale="US", fmt="z")
    call from_file("./data/u_z.csv", into=v, header=.true., locale="US", fmt="z")
    if ( maxval( abs(u-v)/abs(u) ) < tol ) then
        write(*,*) "real 18: SUCCESS"
    else
        write(*,*) "real 18: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk)
    call to_file(u, file_name="./data/u_e.csv", header=["u"], dim=1, locale="EU", fmt="e")
    call from_file("./data/u_e.csv", into=v, header=.true., locale="EU", fmt="e")
    if ( maxval( abs(u-v)/abs(u) ) < tol ) then
        write(*,*) "real 19: SUCCESS"
    else
        write(*,*) "real 19: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk)
    call to_file(u, file_name="./data/u_f.csv", header=["u"], dim=1, locale="EU", fmt="f")
    call from_file("./data/u_f.csv", into=v, header=.true., locale="EU", fmt="f")
    if ( maxval( abs(u-v)/abs(u) ) < tol ) then
        write(*,*) "real 20: SUCCESS"
    else
        write(*,*) "real 20: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk)
    call to_file(u, file_name="./data/u_z.csv", header=["u"], dim=1, locale="EU", fmt="z")
    call from_file("./data/u_z.csv", into=v, header=.true., locale="EU", fmt="z")
    if ( maxval( abs(u-v)/abs(u) ) < tol ) then
        write(*,*) "real 21: SUCCESS"
    else
        write(*,*) "real 21: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk)
    call to_file(u, file_name="./data/u_e.csv", header=["u"], dim=1, locale="US", fmt="e")
    call from_file("./data/u_e.csv", into=v, header=.true., locale="US", fmt="e")
    if ( maxval( abs(u-v)/abs(u) ) < tol ) then
        write(*,*) "real 22: SUCCESS"
    else
        write(*,*) "real 22: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk)
    call to_file(u, file_name="./data/u_f.csv", header=["u"], dim=1, locale="US", fmt="f")
    call from_file("./data/u_f.csv", into=v, header=.true., locale="US", fmt="f")
    if ( maxval( abs(u-v)/abs(u) ) < tol ) then
        write(*,*) "real 23: SUCCESS"
    else
        write(*,*) "real 23: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0.0_rk,1.0_rk)
    call to_file(u, file_name="./data/u_z.csv", header=["u"], dim=1, locale="US", fmt="z")
    call from_file("./data/u_z.csv", into=v, header=.true., locale="US", fmt="z")
    if ( maxval( abs(u-v)/abs(u) ) < tol ) then
        write(*,*) "real 24: SUCCESS"
    else
        write(*,*) "real 24: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk)
    call to_file(x, file_name="./data/x_e.csv", header=[""], locale="EU", fmt="e")
    call from_file("./data/x_e.csv", into=y, header=.false., locale="EU", fmt="e")
    if ( maxval( abs(x-y)/abs(x) ) < tol ) then
        write(*,*) "real 25: SUCCESS"
    else
        write(*,*) "real 25: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk)
    call to_file(x, file_name="./data/x_f.csv", header=[""], locale="EU", fmt="f")
    call from_file("./data/x_f.csv", into=y, header=.false., locale="EU", fmt="f")
    if ( maxval( abs(x-y)/abs(x) ) < tol ) then
        write(*,*) "real 26: SUCCESS"
    else
        write(*,*) "real 26: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk)
    call to_file(x, file_name="./data/x_z.csv", header=[""], locale="EU", fmt="z")
    call from_file("./data/x_z.csv", into=y, header=.false., locale="EU", fmt="z")
    if ( maxval( abs(x-y)/abs(x) ) < tol ) then
        write(*,*) "real 27: SUCCESS"
    else
        write(*,*) "real 27: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk)
    call to_file(x, file_name="./data/x_e.csv", header=[""], locale="US", fmt="e")
    call from_file("./data/x_e.csv", into=y, header=.false., locale="US", fmt="e")
    if ( maxval( abs(x-y)/abs(x) ) < tol ) then
        write(*,*) "real 28: SUCCESS"
    else
        write(*,*) "real 28: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk)
    call to_file(x, file_name="./data/x_f.csv", header=[""], locale="US", fmt="f")
    call from_file("./data/x_f.csv", into=y, header=.false., locale="US", fmt="f")
    if ( maxval( abs(x-y)/abs(x) ) < tol ) then
        write(*,*) "real 29: SUCCESS"
    else
        write(*,*) "real 29: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk)
    call to_file(x, file_name="./data/x_z.csv", header=[""], locale="US", fmt="z")
    call from_file("./data/x_z.csv", into=y, header=.false., locale="US", fmt="z")
    if ( maxval( abs(x-y)/abs(x) ) < tol ) then
        write(*,*) "real 30: SUCCESS"
    else
        write(*,*) "real 30: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk)
    call to_file(x, file_name="./data/x_e.csv", header=["x"], locale="EU", fmt="e")
    call from_file("./data/x_e.csv", into=y, header=.true., locale="EU", fmt="e")
    if ( maxval( abs(x-y)/abs(x) ) < tol ) then
        write(*,*) "real 31: SUCCESS"
    else
        write(*,*) "real 31: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk)
    call to_file(x, file_name="./data/x_f.csv", header=["x"], locale="EU", fmt="f")
    call from_file("./data/x_f.csv", into=y, header=.true., locale="EU", fmt="f")
    if ( maxval( abs(x-y)/abs(x) ) < tol ) then
        write(*,*) "real 32: SUCCESS"
    else
        write(*,*) "real 32: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk)
    call to_file(x, file_name="./data/x_z.csv", header=["x"], locale="EU", fmt="z")
    call from_file("./data/x_z.csv", into=y, header=.true., locale="EU", fmt="z")
    if ( maxval( abs(x-y)/abs(x) ) < tol ) then
        write(*,*) "real 33: SUCCESS"
    else
        write(*,*) "real 33: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk)
    call to_file(x, file_name="./data/x_e.csv", header=["x"], locale="US", fmt="e")
    call from_file("./data/x_e.csv", into=y, header=.true., locale="US", fmt="e")
    if ( maxval( abs(x-y)/abs(x) ) < tol ) then
        write(*,*) "real 34: SUCCESS"
    else
        write(*,*) "real 34: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk)
    call to_file(x, file_name="./data/x_f.csv", header=["x"], locale="US", fmt="f")
    call from_file("./data/x_f.csv", into=y, header=.true., locale="US", fmt="f")
    if ( maxval( abs(x-y)/abs(x) ) < tol ) then
        write(*,*) "real 35: SUCCESS"
    else
        write(*,*) "real 35: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk)
    call to_file(x, file_name="./data/x_z.csv", header=["x"], locale="US", fmt="z")
    call from_file("./data/x_z.csv", into=y, header=.true., locale="US", fmt="z")
    if ( maxval( abs(x-y)/abs(x) ) < tol ) then
        write(*,*) "real 36: SUCCESS"
    else
        write(*,*) "real 36: FAILURE"
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
end program main
