program main
    use, intrinsic :: iso_fortran_env, only: rk=>real32, compiler_version, compiler_options
    use io_fortran_lib,                only: String, to_file, from_file, LF, SPACE, str, operator(+), operator(**)
    use randoms,                       only: random_gauss
    implicit none (type, external)

    real(rk),         parameter :: tol = 3.0_rk*epsilon(1e0_rk)
    integer,          parameter :: rows = 700, cols = 25
    character(len=*), parameter :: logfile = "./test/tests.log"

    character(len=10) :: date = repeat(SPACE, len(date)), time = repeat(SPACE, len(time))
    type(String)      :: logmsg
    logical           :: all_passing = .true.

    real(rk),    allocatable :: u(:), v(:), x(:,:), y(:,:)
    complex(rk), allocatable :: a(:), b(:), c(:,:), d(:,:)

    allocate( u(rows), v(rows), x(rows,cols), y(rows,cols) )
    call random_init(repeatable=.false., image_distinct=.true.)
    call date_and_time(date=date, time=time)

    logmsg = String("RUNNING TESTS (file) | date: " + trim(adjustl(date)) + &
                    " | time: "                     + time                + &
                    " | complex kind: "             + str(rk)             )

    call logmsg%push(LF + "-"**logmsg%len() + LF)

    write(*,"(a)") logmsg%as_str()

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_e.csv", header=[""], dim=2, locale="EU", fmt="e", im="")
    call from_file("./data/a_e.csv", into=b, header=.false., locale="EU", fmt="e", im="")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 1: SUCCESS"
    else
        write(*,*) "complex 1: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_f.csv", header=[""], dim=2, locale="EU", fmt="f", im="")
    call from_file("./data/a_f.csv", into=b, header=.false., locale="EU", fmt="f", im="")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 2: SUCCESS"
    else
        write(*,*) "complex 2: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_z.csv", header=[""], dim=2, locale="EU", fmt="z", im="")
    call from_file("./data/a_z.csv", into=b, header=.false., locale="EU", fmt="z", im="")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 3: SUCCESS"
    else
        write(*,*) "complex 3: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_e.csv", header=[""], dim=2, locale="US", fmt="e", im="")
    call from_file("./data/a_e.csv", into=b, header=.false., locale="US", fmt="e", im="")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 4: SUCCESS"
    else
        write(*,*) "complex 4: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_f.csv", header=[""], dim=2, locale="US", fmt="f", im="")
    call from_file("./data/a_f.csv", into=b, header=.false., locale="US", fmt="f", im="")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 5: SUCCESS"
    else
        write(*,*) "complex 5: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_z.csv", header=[""], dim=2, locale="US", fmt="z", im="")
    call from_file("./data/a_z.csv", into=b, header=.false., locale="US", fmt="z", im="")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 6: SUCCESS"
    else
        write(*,*) "complex 6: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_e.csv", header=[""], dim=1, locale="EU", fmt="e", im="")
    call from_file("./data/a_e.csv", into=b, header=.false., locale="EU", fmt="e", im="")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 7: SUCCESS"
    else
        write(*,*) "complex 7: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_f.csv", header=[""], dim=1, locale="EU", fmt="f", im="")
    call from_file("./data/a_f.csv", into=b, header=.false., locale="EU", fmt="f", im="")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 8: SUCCESS"
    else
        write(*,*) "complex 8: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_z.csv", header=[""], dim=1, locale="EU", fmt="z", im="")
    call from_file("./data/a_z.csv", into=b, header=.false., locale="EU", fmt="z", im="")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 9: SUCCESS"
    else
        write(*,*) "complex 9: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_e.csv", header=[""], dim=1, locale="US", fmt="e", im="")
    call from_file("./data/a_e.csv", into=b, header=.false., locale="US", fmt="e", im="")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 10: SUCCESS"
    else
        write(*,*) "complex 10: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_f.csv", header=[""], dim=1, locale="US", fmt="f", im="")
    call from_file("./data/a_f.csv", into=b, header=.false., locale="US", fmt="f", im="")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 11: SUCCESS"
    else
        write(*,*) "complex 11: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_z.csv", header=[""], dim=1, locale="US", fmt="z", im="")
    call from_file("./data/a_z.csv", into=b, header=.false., locale="US", fmt="z", im="")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 12: SUCCESS"
    else
        write(*,*) "complex 12: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_e.csv", header=[""], dim=2, locale="EU", fmt="e", im="*1i")
    call from_file("./data/a_e.csv", into=b, header=.false., locale="EU", fmt="e", im="*1i")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 13: SUCCESS"
    else
        write(*,*) "complex 13: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_f.csv", header=[""], dim=2, locale="EU", fmt="f", im="*1i")
    call from_file("./data/a_f.csv", into=b, header=.false., locale="EU", fmt="f", im="*1i")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 14: SUCCESS"
    else
        write(*,*) "complex 14: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_z.csv", header=[""], dim=2, locale="EU", fmt="z", im="*1i")
    call from_file("./data/a_z.csv", into=b, header=.false., locale="EU", fmt="z", im="*1i")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 15: SUCCESS"
    else
        write(*,*) "complex 15: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_e.csv", header=[""], dim=2, locale="US", fmt="e", im="*1i")
    call from_file("./data/a_e.csv", into=b, header=.false., locale="US", fmt="e", im="*1i")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 16: SUCCESS"
    else
        write(*,*) "complex 16: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_f.csv", header=[""], dim=2, locale="US", fmt="f", im="*1i")
    call from_file("./data/a_f.csv", into=b, header=.false., locale="US", fmt="f", im="*1i")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 17: SUCCESS"
    else
        write(*,*) "complex 17: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_z.csv", header=[""], dim=2, locale="US", fmt="z", im="*1i")
    call from_file("./data/a_z.csv", into=b, header=.false., locale="US", fmt="z", im="*1i")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 18: SUCCESS"
    else
        write(*,*) "complex 18: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_e.csv", header=[""], dim=1, locale="EU", fmt="e", im="*1i")
    call from_file("./data/a_e.csv", into=b, header=.false., locale="EU", fmt="e", im="*1i")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 19: SUCCESS"
    else
        write(*,*) "complex 19: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_f.csv", header=[""], dim=1, locale="EU", fmt="f", im="*1i")
    call from_file("./data/a_f.csv", into=b, header=.false., locale="EU", fmt="f", im="*1i")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 20: SUCCESS"
    else
        write(*,*) "complex 20: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_z.csv", header=[""], dim=1, locale="EU", fmt="z", im="*1i")
    call from_file("./data/a_z.csv", into=b, header=.false., locale="EU", fmt="z", im="*1i")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 21: SUCCESS"
    else
        write(*,*) "complex 21: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_e.csv", header=[""], dim=1, locale="US", fmt="e", im="*1i")
    call from_file("./data/a_e.csv", into=b, header=.false., locale="US", fmt="e", im="*1i")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 22: SUCCESS"
    else
        write(*,*) "complex 22: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_f.csv", header=[""], dim=1, locale="US", fmt="f", im="*1i")
    call from_file("./data/a_f.csv", into=b, header=.false., locale="US", fmt="f", im="*1i")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 23: SUCCESS"
    else
        write(*,*) "complex 23: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_z.csv", header=[""], dim=1, locale="US", fmt="z", im="*1i")
    call from_file("./data/a_z.csv", into=b, header=.false., locale="US", fmt="z", im="*1i")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 24: SUCCESS"
    else
        write(*,*) "complex 24: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_e.csv", header=[""], dim=2, locale="EU", fmt="e", im="j")
    call from_file("./data/a_e.csv", into=b, header=.false., locale="EU", fmt="e", im="j")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 25: SUCCESS"
    else
        write(*,*) "complex 25: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_f.csv", header=[""], dim=2, locale="EU", fmt="f", im="j")
    call from_file("./data/a_f.csv", into=b, header=.false., locale="EU", fmt="f", im="j")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 26: SUCCESS"
    else
        write(*,*) "complex 26: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_z.csv", header=[""], dim=2, locale="EU", fmt="z", im="j")
    call from_file("./data/a_z.csv", into=b, header=.false., locale="EU", fmt="z", im="j")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 27: SUCCESS"
    else
        write(*,*) "complex 27: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_e.csv", header=[""], dim=2, locale="US", fmt="e", im="j")
    call from_file("./data/a_e.csv", into=b, header=.false., locale="US", fmt="e", im="j")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 28: SUCCESS"
    else
        write(*,*) "complex 28: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_f.csv", header=[""], dim=2, locale="US", fmt="f", im="j")
    call from_file("./data/a_f.csv", into=b, header=.false., locale="US", fmt="f", im="j")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 29: SUCCESS"
    else
        write(*,*) "complex 29: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_z.csv", header=[""], dim=2, locale="US", fmt="z", im="j")
    call from_file("./data/a_z.csv", into=b, header=.false., locale="US", fmt="z", im="j")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 30: SUCCESS"
    else
        write(*,*) "complex 30: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_e.csv", header=[""], dim=1, locale="EU", fmt="e", im="j")
    call from_file("./data/a_e.csv", into=b, header=.false., locale="EU", fmt="e", im="j")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 31: SUCCESS"
    else
        write(*,*) "complex 31: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_f.csv", header=[""], dim=1, locale="EU", fmt="f", im="j")
    call from_file("./data/a_f.csv", into=b, header=.false., locale="EU", fmt="f", im="j")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 32: SUCCESS"
    else
        write(*,*) "complex 32: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_z.csv", header=[""], dim=1, locale="EU", fmt="z", im="j")
    call from_file("./data/a_z.csv", into=b, header=.false., locale="EU", fmt="z", im="j")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 33: SUCCESS"
    else
        write(*,*) "complex 33: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_e.csv", header=[""], dim=1, locale="US", fmt="e", im="j")
    call from_file("./data/a_e.csv", into=b, header=.false., locale="US", fmt="e", im="j")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 34: SUCCESS"
    else
        write(*,*) "complex 34: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_f.csv", header=[""], dim=1, locale="US", fmt="f", im="j")
    call from_file("./data/a_f.csv", into=b, header=.false., locale="US", fmt="f", im="j")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 35: SUCCESS"
    else
        write(*,*) "complex 35: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_z.csv", header=[""], dim=1, locale="US", fmt="z", im="j")
    call from_file("./data/a_z.csv", into=b, header=.false., locale="US", fmt="z", im="j")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 36: SUCCESS"
    else
        write(*,*) "complex 36: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_e.csv", header=["a"], dim=2, locale="EU", fmt="e", im="")
    call from_file("./data/a_e.csv", into=b, header=.true., locale="EU", fmt="e", im="")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 37: SUCCESS"
    else
        write(*,*) "complex 37: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_f.csv", header=["a"], dim=2, locale="EU", fmt="f", im="")
    call from_file("./data/a_f.csv", into=b, header=.true., locale="EU", fmt="f", im="")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 38: SUCCESS"
    else
        write(*,*) "complex 38: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_z.csv", header=["a"], dim=2, locale="EU", fmt="z", im="")
    call from_file("./data/a_z.csv", into=b, header=.true., locale="EU", fmt="z", im="")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 39: SUCCESS"
    else
        write(*,*) "complex 39: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_e.csv", header=["a"], dim=2, locale="US", fmt="e", im="")
    call from_file("./data/a_e.csv", into=b, header=.true., locale="US", fmt="e", im="")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 40: SUCCESS"
    else
        write(*,*) "complex 40: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_f.csv", header=["a"], dim=2, locale="US", fmt="f", im="")
    call from_file("./data/a_f.csv", into=b, header=.true., locale="US", fmt="f", im="")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 41: SUCCESS"
    else
        write(*,*) "complex 41: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_z.csv", header=["a"], dim=2, locale="US", fmt="z", im="")
    call from_file("./data/a_z.csv", into=b, header=.true., locale="US", fmt="z", im="")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 42: SUCCESS"
    else
        write(*,*) "complex 42: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_e.csv", header=["a"], dim=1, locale="EU", fmt="e", im="")
    call from_file("./data/a_e.csv", into=b, header=.true., locale="EU", fmt="e", im="")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 43: SUCCESS"
    else
        write(*,*) "complex 43: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_f.csv", header=["a"], dim=1, locale="EU", fmt="f", im="")
    call from_file("./data/a_f.csv", into=b, header=.true., locale="EU", fmt="f", im="")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 44: SUCCESS"
    else
        write(*,*) "complex 44: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_z.csv", header=["a"], dim=1, locale="EU", fmt="z", im="")
    call from_file("./data/a_z.csv", into=b, header=.true., locale="EU", fmt="z", im="")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 45: SUCCESS"
    else
        write(*,*) "complex 45: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_e.csv", header=["a"], dim=1, locale="US", fmt="e", im="")
    call from_file("./data/a_e.csv", into=b, header=.true., locale="US", fmt="e", im="")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 46: SUCCESS"
    else
        write(*,*) "complex 46: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_f.csv", header=["a"], dim=1, locale="US", fmt="f", im="")
    call from_file("./data/a_f.csv", into=b, header=.true., locale="US", fmt="f", im="")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 47: SUCCESS"
    else
        write(*,*) "complex 47: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_z.csv", header=["a"], dim=1, locale="US", fmt="z", im="")
    call from_file("./data/a_z.csv", into=b, header=.true., locale="US", fmt="z", im="")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 48: SUCCESS"
    else
        write(*,*) "complex 48: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_e.csv", header=["a"], dim=2, locale="EU", fmt="e", im="*1i")
    call from_file("./data/a_e.csv", into=b, header=.true., locale="EU", fmt="e", im="*1i")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 49: SUCCESS"
    else
        write(*,*) "complex 49: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_f.csv", header=["a"], dim=2, locale="EU", fmt="f", im="*1i")
    call from_file("./data/a_f.csv", into=b, header=.true., locale="EU", fmt="f", im="*1i")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 50: SUCCESS"
    else
        write(*,*) "complex 50: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_z.csv", header=["a"], dim=2, locale="EU", fmt="z", im="*1i")
    call from_file("./data/a_z.csv", into=b, header=.true., locale="EU", fmt="z", im="*1i")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 51: SUCCESS"
    else
        write(*,*) "complex 51: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_e.csv", header=["a"], dim=2, locale="US", fmt="e", im="*1i")
    call from_file("./data/a_e.csv", into=b, header=.true., locale="US", fmt="e", im="*1i")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 52: SUCCESS"
    else
        write(*,*) "complex 52: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_f.csv", header=["a"], dim=2, locale="US", fmt="f", im="*1i")
    call from_file("./data/a_f.csv", into=b, header=.true., locale="US", fmt="f", im="*1i")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 53: SUCCESS"
    else
        write(*,*) "complex 53: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_z.csv", header=["a"], dim=2, locale="US", fmt="z", im="*1i")
    call from_file("./data/a_z.csv", into=b, header=.true., locale="US", fmt="z", im="*1i")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 54: SUCCESS"
    else
        write(*,*) "complex 54: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_e.csv", header=["a"], dim=1, locale="EU", fmt="e", im="*1i")
    call from_file("./data/a_e.csv", into=b, header=.true., locale="EU", fmt="e", im="*1i")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 55: SUCCESS"
    else
        write(*,*) "complex 55: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_f.csv", header=["a"], dim=1, locale="EU", fmt="f", im="*1i")
    call from_file("./data/a_f.csv", into=b, header=.true., locale="EU", fmt="f", im="*1i")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 56: SUCCESS"
    else
        write(*,*) "complex 56: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_z.csv", header=["a"], dim=1, locale="EU", fmt="z", im="*1i")
    call from_file("./data/a_z.csv", into=b, header=.true., locale="EU", fmt="z", im="*1i")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 57: SUCCESS"
    else
        write(*,*) "complex 57: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_e.csv", header=["a"], dim=1, locale="US", fmt="e", im="*1i")
    call from_file("./data/a_e.csv", into=b, header=.true., locale="US", fmt="e", im="*1i")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 58: SUCCESS"
    else
        write(*,*) "complex 58: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_f.csv", header=["a"], dim=1, locale="US", fmt="f", im="*1i")
    call from_file("./data/a_f.csv", into=b, header=.true., locale="US", fmt="f", im="*1i")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 59: SUCCESS"
    else
        write(*,*) "complex 59: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_z.csv", header=["a"], dim=1, locale="US", fmt="z", im="*1i")
    call from_file("./data/a_z.csv", into=b, header=.true., locale="US", fmt="z", im="*1i")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 60: SUCCESS"
    else
        write(*,*) "complex 60: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_e.csv", header=["a"], dim=2, locale="EU", fmt="e", im="j")
    call from_file("./data/a_e.csv", into=b, header=.true., locale="EU", fmt="e", im="j")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 61: SUCCESS"
    else
        write(*,*) "complex 61: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_f.csv", header=["a"], dim=2, locale="EU", fmt="f", im="j")
    call from_file("./data/a_f.csv", into=b, header=.true., locale="EU", fmt="f", im="j")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 62: SUCCESS"
    else
        write(*,*) "complex 62: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_z.csv", header=["a"], dim=2, locale="EU", fmt="z", im="j")
    call from_file("./data/a_z.csv", into=b, header=.true., locale="EU", fmt="z", im="j")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 63: SUCCESS"
    else
        write(*,*) "complex 63: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_e.csv", header=["a"], dim=2, locale="US", fmt="e", im="j")
    call from_file("./data/a_e.csv", into=b, header=.true., locale="US", fmt="e", im="j")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 64: SUCCESS"
    else
        write(*,*) "complex 64: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_f.csv", header=["a"], dim=2, locale="US", fmt="f", im="j")
    call from_file("./data/a_f.csv", into=b, header=.true., locale="US", fmt="f", im="j")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 65: SUCCESS"
    else
        write(*,*) "complex 65: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_z.csv", header=["a"], dim=2, locale="US", fmt="z", im="j")
    call from_file("./data/a_z.csv", into=b, header=.true., locale="US", fmt="z", im="j")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 66: SUCCESS"
    else
        write(*,*) "complex 66: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_e.csv", header=["a"], dim=1, locale="EU", fmt="e", im="j")
    call from_file("./data/a_e.csv", into=b, header=.true., locale="EU", fmt="e", im="j")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 67: SUCCESS"
    else
        write(*,*) "complex 67: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_f.csv", header=["a"], dim=1, locale="EU", fmt="f", im="j")
    call from_file("./data/a_f.csv", into=b, header=.true., locale="EU", fmt="f", im="j")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 68: SUCCESS"
    else
        write(*,*) "complex 68: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_z.csv", header=["a"], dim=1, locale="EU", fmt="z", im="j")
    call from_file("./data/a_z.csv", into=b, header=.true., locale="EU", fmt="z", im="j")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 69: SUCCESS"
    else
        write(*,*) "complex 69: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_e.csv", header=["a"], dim=1, locale="US", fmt="e", im="j")
    call from_file("./data/a_e.csv", into=b, header=.true., locale="US", fmt="e", im="j")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 70: SUCCESS"
    else
        write(*,*) "complex 70: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_f.csv", header=["a"], dim=1, locale="US", fmt="f", im="j")
    call from_file("./data/a_f.csv", into=b, header=.true., locale="US", fmt="f", im="j")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 71: SUCCESS"
    else
        write(*,*) "complex 71: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(u,0e0_rk,1e0_rk); call random_gauss(v,0e0_rk,1e0_rk); a = cmplx(u,v,rk)
    call to_file(a, file="./data/a_z.csv", header=["a"], dim=1, locale="US", fmt="z", im="j")
    call from_file("./data/a_z.csv", into=b, header=.true., locale="US", fmt="z", im="j")
    if ( maxval( abs(a-b)/abs(a) ) < tol ) then
        write(*,*) "complex 72: SUCCESS"
    else
        write(*,*) "complex 72: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_e.csv", header=[""], locale="EU", fmt="e", im="")
    call from_file("./data/c_e.csv", into=d, header=.false., locale="EU", fmt="e", im="")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 73: SUCCESS"
    else
        write(*,*) "complex 73: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_f.csv", header=[""], locale="EU", fmt="f", im="")
    call from_file("./data/c_f.csv", into=d, header=.false., locale="EU", fmt="f", im="")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 74: SUCCESS"
    else
        write(*,*) "complex 74: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_z.csv", header=[""], locale="EU", fmt="z", im="")
    call from_file("./data/c_z.csv", into=d, header=.false., locale="EU", fmt="z", im="")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 75: SUCCESS"
    else
        write(*,*) "complex 75: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_e.csv", header=[""], locale="US", fmt="e", im="")
    call from_file("./data/c_e.csv", into=d, header=.false., locale="US", fmt="e", im="")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 76: SUCCESS"
    else
        write(*,*) "complex 76: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_f.csv", header=[""], locale="US", fmt="f", im="")
    call from_file("./data/c_f.csv", into=d, header=.false., locale="US", fmt="f", im="")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 77: SUCCESS"
    else
        write(*,*) "complex 77: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_z.csv", header=[""], locale="US", fmt="z", im="")
    call from_file("./data/c_z.csv", into=d, header=.false., locale="US", fmt="z", im="")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 78: SUCCESS"
    else
        write(*,*) "complex 78: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_e.csv", header=[""], locale="EU", fmt="e", im="*1i")
    call from_file("./data/c_e.csv", into=d, header=.false., locale="EU", fmt="e", im="*1i")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 79: SUCCESS"
    else
        write(*,*) "complex 79: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_f.csv", header=[""], locale="EU", fmt="f", im="*1i")
    call from_file("./data/c_f.csv", into=d, header=.false., locale="EU", fmt="f", im="*1i")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 80: SUCCESS"
    else
        write(*,*) "complex 80: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_z.csv", header=[""], locale="EU", fmt="z", im="*1i")
    call from_file("./data/c_z.csv", into=d, header=.false., locale="EU", fmt="z", im="*1i")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 81: SUCCESS"
    else
        write(*,*) "complex 81: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_e.csv", header=[""], locale="US", fmt="e", im="*1i")
    call from_file("./data/c_e.csv", into=d, header=.false., locale="US", fmt="e", im="*1i")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 82: SUCCESS"
    else
        write(*,*) "complex 82: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_f.csv", header=[""], locale="US", fmt="f", im="*1i")
    call from_file("./data/c_f.csv", into=d, header=.false., locale="US", fmt="f", im="*1i")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 83: SUCCESS"
    else
        write(*,*) "complex 83: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_z.csv", header=[""], locale="US", fmt="z", im="*1i")
    call from_file("./data/c_z.csv", into=d, header=.false., locale="US", fmt="z", im="*1i")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 84: SUCCESS"
    else
        write(*,*) "complex 84: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_e.csv", header=[""], locale="EU", fmt="e", im="j")
    call from_file("./data/c_e.csv", into=d, header=.false., locale="EU", fmt="e", im="j")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 85: SUCCESS"
    else
        write(*,*) "complex 85: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_f.csv", header=[""], locale="EU", fmt="f", im="j")
    call from_file("./data/c_f.csv", into=d, header=.false., locale="EU", fmt="f", im="j")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 86: SUCCESS"
    else
        write(*,*) "complex 86: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_z.csv", header=[""], locale="EU", fmt="z", im="j")
    call from_file("./data/c_z.csv", into=d, header=.false., locale="EU", fmt="z", im="j")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 87: SUCCESS"
    else
        write(*,*) "complex 87: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_e.csv", header=[""], locale="US", fmt="e", im="j")
    call from_file("./data/c_e.csv", into=d, header=.false., locale="US", fmt="e", im="j")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 88: SUCCESS"
    else
        write(*,*) "complex 88: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_f.csv", header=[""], locale="US", fmt="f", im="j")
    call from_file("./data/c_f.csv", into=d, header=.false., locale="US", fmt="f", im="j")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 89: SUCCESS"
    else
        write(*,*) "complex 89: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_z.csv", header=[""], locale="US", fmt="z", im="j")
    call from_file("./data/c_z.csv", into=d, header=.false., locale="US", fmt="z", im="j")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 90: SUCCESS"
    else
        write(*,*) "complex 90: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_e.csv", header=["c"], locale="EU", fmt="e", im="")
    call from_file("./data/c_e.csv", into=d, header=.true., locale="EU", fmt="e", im="")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 91: SUCCESS"
    else
        write(*,*) "complex 91: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_f.csv", header=["c"], locale="EU", fmt="f", im="")
    call from_file("./data/c_f.csv", into=d, header=.true., locale="EU", fmt="f", im="")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 92: SUCCESS"
    else
        write(*,*) "complex 92: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_z.csv", header=["c"], locale="EU", fmt="z", im="")
    call from_file("./data/c_z.csv", into=d, header=.true., locale="EU", fmt="z", im="")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 93: SUCCESS"
    else
        write(*,*) "complex 93: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_e.csv", header=["c"], locale="US", fmt="e", im="")
    call from_file("./data/c_e.csv", into=d, header=.true., locale="US", fmt="e", im="")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 94: SUCCESS"
    else
        write(*,*) "complex 94: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_f.csv", header=["c"], locale="US", fmt="f", im="")
    call from_file("./data/c_f.csv", into=d, header=.true., locale="US", fmt="f", im="")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 95: SUCCESS"
    else
        write(*,*) "complex 95: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_z.csv", header=["c"], locale="US", fmt="z", im="")
    call from_file("./data/c_z.csv", into=d, header=.true., locale="US", fmt="z", im="")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 96: SUCCESS"
    else
        write(*,*) "complex 96: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_e.csv", header=["c"], locale="EU", fmt="e", im="*1i")
    call from_file("./data/c_e.csv", into=d, header=.true., locale="EU", fmt="e", im="*1i")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 97: SUCCESS"
    else
        write(*,*) "complex 97: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_f.csv", header=["c"], locale="EU", fmt="f", im="*1i")
    call from_file("./data/c_f.csv", into=d, header=.true., locale="EU", fmt="f", im="*1i")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 98: SUCCESS"
    else
        write(*,*) "complex 98: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_z.csv", header=["c"], locale="EU", fmt="z", im="*1i")
    call from_file("./data/c_z.csv", into=d, header=.true., locale="EU", fmt="z", im="*1i")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 99: SUCCESS"
    else
        write(*,*) "complex 99: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_e.csv", header=["c"], locale="US", fmt="e", im="*1i")
    call from_file("./data/c_e.csv", into=d, header=.true., locale="US", fmt="e", im="*1i")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 100: SUCCESS"
    else
        write(*,*) "complex 100: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_f.csv", header=["c"], locale="US", fmt="f", im="*1i")
    call from_file("./data/c_f.csv", into=d, header=.true., locale="US", fmt="f", im="*1i")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 101: SUCCESS"
    else
        write(*,*) "complex 101: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_z.csv", header=["c"], locale="US", fmt="z", im="*1i")
    call from_file("./data/c_z.csv", into=d, header=.true., locale="US", fmt="z", im="*1i")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 102: SUCCESS"
    else
        write(*,*) "complex 102: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_e.csv", header=["c"], locale="EU", fmt="e", im="j")
    call from_file("./data/c_e.csv", into=d, header=.true., locale="EU", fmt="e", im="j")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 103: SUCCESS"
    else
        write(*,*) "complex 103: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_f.csv", header=["c"], locale="EU", fmt="f", im="j")
    call from_file("./data/c_f.csv", into=d, header=.true., locale="EU", fmt="f", im="j")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 104: SUCCESS"
    else
        write(*,*) "complex 104: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_z.csv", header=["c"], locale="EU", fmt="z", im="j")
    call from_file("./data/c_z.csv", into=d, header=.true., locale="EU", fmt="z", im="j")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 105: SUCCESS"
    else
        write(*,*) "complex 105: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_e.csv", header=["c"], locale="US", fmt="e", im="j")
    call from_file("./data/c_e.csv", into=d, header=.true., locale="US", fmt="e", im="j")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 106: SUCCESS"
    else
        write(*,*) "complex 106: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_f.csv", header=["c"], locale="US", fmt="f", im="j")
    call from_file("./data/c_f.csv", into=d, header=.true., locale="US", fmt="f", im="j")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 107: SUCCESS"
    else
        write(*,*) "complex 107: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0e0_rk,1e0_rk); call random_gauss(y,0e0_rk,1e0_rk); c = cmplx(x,y,rk)
    call to_file(c, file="./data/c_z.csv", header=["c"], locale="US", fmt="z", im="j")
    call from_file("./data/c_z.csv", into=d, header=.true., locale="US", fmt="z", im="j")
    if ( maxval( abs(c-d)/abs(c) ) < tol ) then
        write(*,*) "complex 108: SUCCESS"
    else
        write(*,*) "complex 108: FAILURE"
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
