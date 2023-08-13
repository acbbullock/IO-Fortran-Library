program main
    use, intrinsic :: iso_fortran_env, only: rk=>real32, &
                      compiler_version, compiler_options
    use io_fortran_lib, only: String, cast, LF, str, operator(+), operator(**)
    use randoms,        only: random_gauss
    implicit none (type, external)

    real(rk), parameter :: tol = 3.0_rk*epsilon(1.0_rk)

    character(len=*), parameter   :: logfile = "./test/tests.log"
    character(len=:), allocatable :: char_var

    character(len=10) :: date, time
    type(String)      :: logmsg
    logical           :: all_passing
    real(rk)          :: x, y

    call random_init(repeatable=.false., image_distinct=.true.)
    call date_and_time(date=date, time=time)

    logmsg = String("RUNNING TESTS (scalar) | date: " + trim(adjustl(date)) + &
                    " | time: "                       + time                + &
                    " | real kind: "                  + str(rk)             )

    call logmsg%push(LF + "-"**logmsg%len() + LF)

    all_passing = .true.
    write(*,"(a)") logmsg%as_str()

    call random_gauss(x,0.0_rk,1.0_rk)
    call cast(str(x, locale="US", fmt="e"), into=y, locale="US", fmt="e")
    if ( abs(x-y)/abs(x) < tol ) then
        write(*,*) "real 1: SUCCESS"
    else
        write(*,*) "real 1: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk)
    call cast(str(x, locale="US", fmt="f"), into=y, locale="US", fmt="f")
    if ( abs(x-y)/abs(x) < tol ) then
        write(*,*) "real 2: SUCCESS"
    else
        write(*,*) "real 2: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk)
    call cast(str(x, locale="US", fmt="z"), into=y, locale="US", fmt="z")
    if ( abs(x-y)/abs(x) < tol ) then
        write(*,*) "real 3: SUCCESS"
    else
        write(*,*) "real 3: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk)
    call cast(str(x, locale="EU", fmt="e"), into=y, locale="EU", fmt="e")
    if ( abs(x-y)/abs(x) < tol ) then
        write(*,*) "real 4: SUCCESS"
    else
        write(*,*) "real 4: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk)
    call cast(str(x, locale="EU", fmt="f"), into=y, locale="EU", fmt="f")
    if ( abs(x-y)/abs(x) < tol ) then
        write(*,*) "real 5: SUCCESS"
    else
        write(*,*) "real 5: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk)
    call cast(str(x, locale="EU", fmt="z"), into=y, locale="EU", fmt="z")
    if ( abs(x-y)/abs(x) < tol ) then
        write(*,*) "real 6: SUCCESS"
    else
        write(*,*) "real 6: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk)
    call cast(x, into=char_var, locale="US", fmt="e"); call cast(char_var, into=y, locale="US", fmt="e")
    if ( abs(x-y)/abs(x) < tol ) then
        write(*,*) "real 7: SUCCESS"
    else
        write(*,*) "real 7: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk)
    call cast(x, into=char_var, locale="US", fmt="f"); call cast(char_var, into=y, locale="US", fmt="f")
    if ( abs(x-y)/abs(x) < tol ) then
        write(*,*) "real 8: SUCCESS"
    else
        write(*,*) "real 8: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk)
    call cast(x, into=char_var, locale="US", fmt="z"); call cast(char_var, into=y, locale="US", fmt="z")
    if ( abs(x-y)/abs(x) < tol ) then
        write(*,*) "real 9: SUCCESS"
    else
        write(*,*) "real 9: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk)
    call cast(x, into=char_var, locale="EU", fmt="e"); call cast(char_var, into=y, locale="EU", fmt="e")
    if ( abs(x-y)/abs(x) < tol ) then
        write(*,*) "real 10: SUCCESS"
    else
        write(*,*) "real 10: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk)
    call cast(x, into=char_var, locale="EU", fmt="f"); call cast(char_var, into=y, locale="EU", fmt="f")
    if ( abs(x-y)/abs(x) < tol ) then
        write(*,*) "real 11: SUCCESS"
    else
        write(*,*) "real 11: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk)
    call cast(x, into=char_var, locale="EU", fmt="z"); call cast(char_var, into=y, locale="EU", fmt="z")
    if ( abs(x-y)/abs(x) < tol ) then
        write(*,*) "real 12: SUCCESS"
    else
        write(*,*) "real 12: FAILURE"
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
