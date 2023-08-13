program main
    use, intrinsic :: iso_fortran_env, only: rk=>real32, &
                      compiler_version, compiler_options
    use io_fortran_lib, only: String, cast, LF, SPACE, str, operator(+), operator(**)
    use randoms,        only: random_gauss
    implicit none (type, external)

    real(rk),         parameter :: tol = 3.0_rk*epsilon(1.0_rk)
    integer,          parameter :: n = 2000
    character(len=*), parameter :: logfile = "./test/tests.log"

    character(len=10) :: date = repeat(SPACE, len(date)), time = repeat(SPACE, len(time))
    type(String)      :: logmsg, string_var(n)
    logical           :: all_passing = .true.
    real(rk)          :: x(n) = 0.0_rk, y(n) = 0.0_rk
    complex(rk)       :: z1(n) = (0.0_rk,0.0_rk), z2(n) = (0.0_rk,0.0_rk)

    call random_init(repeatable=.false., image_distinct=.true.)
    call date_and_time(date=date, time=time)

    logmsg = String("RUNNING TESTS (array) | date: " + trim(adjustl(date)) + &
                    " | time: "                      + time                + &
                    " | complex kind: "              + str(rk)             )

    call logmsg%push(LF + "-"**logmsg%len() + LF)

    write(*,"(a)") logmsg%as_str()

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(String(z1, locale="US", fmt="e", im=""), into=z2, locale="US", fmt="e", im="")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 1: SUCCESS"
    else
        write(*,*) "complex 1: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(String(z1, locale="US", fmt="f", im=""), into=z2, locale="US", fmt="f", im="")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 2: SUCCESS"
    else
        write(*,*) "complex 2: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(String(z1, locale="US", fmt="z", im=""), into=z2, locale="US", fmt="z", im="")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 3: SUCCESS"
    else
        write(*,*) "complex 3: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(String(z1, locale="US", fmt="e", im="j"), into=z2, locale="US", fmt="e", im="j")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 4: SUCCESS"
    else
        write(*,*) "complex 4: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(String(z1, locale="US", fmt="f", im="j"), into=z2, locale="US", fmt="f", im="j")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 5: SUCCESS"
    else
        write(*,*) "complex 5: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(String(z1, locale="US", fmt="z", im="j"), into=z2, locale="US", fmt="z", im="j")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 6: SUCCESS"
    else
        write(*,*) "complex 6: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(String(z1, locale="US", fmt="e", im="*1i"), into=z2, locale="US", fmt="e", im="*1i")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 7: SUCCESS"
    else
        write(*,*) "complex 7: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(String(z1, locale="US", fmt="f", im="*1i"), into=z2, locale="US", fmt="f", im="*1i")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 8: SUCCESS"
    else
        write(*,*) "complex 8: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(String(z1, locale="US", fmt="z", im="*1i"), into=z2, locale="US", fmt="z", im="*1i")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 9: SUCCESS"
    else
        write(*,*) "complex 9: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(String(z1, locale="EU", fmt="e", im=""), into=z2, locale="EU", fmt="e", im="")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 10: SUCCESS"
    else
        write(*,*) "complex 10: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(String(z1, locale="EU", fmt="f", im=""), into=z2, locale="EU", fmt="f", im="")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 11: SUCCESS"
    else
        write(*,*) "complex 11: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(String(z1, locale="EU", fmt="z", im=""), into=z2, locale="EU", fmt="z", im="")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 12: SUCCESS"
    else
        write(*,*) "complex 12: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(String(z1, locale="EU", fmt="e", im="j"), into=z2, locale="EU", fmt="e", im="j")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 13: SUCCESS"
    else
        write(*,*) "complex 13: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(String(z1, locale="EU", fmt="f", im="j"), into=z2, locale="EU", fmt="f", im="j")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 14: SUCCESS"
    else
        write(*,*) "complex 14: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(String(z1, locale="EU", fmt="z", im="j"), into=z2, locale="EU", fmt="z", im="j")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 15: SUCCESS"
    else
        write(*,*) "complex 15: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(String(z1, locale="EU", fmt="e", im="*1i"), into=z2, locale="EU", fmt="e", im="*1i")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 16: SUCCESS"
    else
        write(*,*) "complex 16: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(String(z1, locale="EU", fmt="f", im="*1i"), into=z2, locale="EU", fmt="f", im="*1i")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 17: SUCCESS"
    else
        write(*,*) "complex 17: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(String(z1, locale="EU", fmt="z", im="*1i"), into=z2, locale="EU", fmt="z", im="*1i")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 18: SUCCESS"
    else
        write(*,*) "complex 18: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(z1, into=string_var, locale="US", fmt="e", im="")
    call cast(string_var, into=z2, locale="US", fmt="e", im="")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 19: SUCCESS"
    else
        write(*,*) "complex 19: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(z1, into=string_var, locale="US", fmt="f", im="")
    call cast(string_var, into=z2, locale="US", fmt="f", im="")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 20: SUCCESS"
    else
        write(*,*) "complex 20: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(z1, into=string_var, locale="US", fmt="z", im="")
    call cast(string_var, into=z2, locale="US", fmt="z", im="")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 21: SUCCESS"
    else
        write(*,*) "complex 21: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(z1, into=string_var, locale="US", fmt="e", im="j")
    call cast(string_var, into=z2, locale="US", fmt="e", im="j")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 22: SUCCESS"
    else
        write(*,*) "complex 22: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(z1, into=string_var, locale="US", fmt="f", im="j")
    call cast(string_var, into=z2, locale="US", fmt="f", im="j")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 23: SUCCESS"
    else
        write(*,*) "complex 23: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(z1, into=string_var, locale="US", fmt="z", im="j")
    call cast(string_var, into=z2, locale="US", fmt="z", im="j")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 24: SUCCESS"
    else
        write(*,*) "complex 24: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(z1, into=string_var, locale="US", fmt="e", im="*1i")
    call cast(string_var, into=z2, locale="US", fmt="e", im="*1i")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 25: SUCCESS"
    else
        write(*,*) "complex 25: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(z1, into=string_var, locale="US", fmt="f", im="*1i")
    call cast(string_var, into=z2, locale="US", fmt="f", im="*1i")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 26: SUCCESS"
    else
        write(*,*) "complex 26: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(z1, into=string_var, locale="US", fmt="z", im="*1i")
    call cast(string_var, into=z2, locale="US", fmt="z", im="*1i")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 27: SUCCESS"
    else
        write(*,*) "complex 27: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(z1, into=string_var, locale="EU", fmt="e", im="")
    call cast(string_var, into=z2, locale="EU", fmt="e", im="")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 28: SUCCESS"
    else
        write(*,*) "complex 28: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(z1, into=string_var, locale="EU", fmt="f", im="")
    call cast(string_var, into=z2, locale="EU", fmt="f", im="")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 29: SUCCESS"
    else
        write(*,*) "complex 29: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(z1, into=string_var, locale="EU", fmt="z", im="")
    call cast(string_var, into=z2, locale="EU", fmt="z", im="")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 30: SUCCESS"
    else
        write(*,*) "complex 30: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(z1, into=string_var, locale="EU", fmt="e", im="j")
    call cast(string_var, into=z2, locale="EU", fmt="e", im="j")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 31: SUCCESS"
    else
        write(*,*) "complex 31: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(z1, into=string_var, locale="EU", fmt="f", im="j")
    call cast(string_var, into=z2, locale="EU", fmt="f", im="j")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 32: SUCCESS"
    else
        write(*,*) "complex 32: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(z1, into=string_var, locale="EU", fmt="z", im="j")
    call cast(string_var, into=z2, locale="EU", fmt="z", im="j")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 33: SUCCESS"
    else
        write(*,*) "complex 33: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(z1, into=string_var, locale="EU", fmt="e", im="*1i")
    call cast(string_var, into=z2, locale="EU", fmt="e", im="*1i")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 34: SUCCESS"
    else
        write(*,*) "complex 34: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(z1, into=string_var, locale="EU", fmt="f", im="*1i")
    call cast(string_var, into=z2, locale="EU", fmt="f", im="*1i")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 35: SUCCESS"
    else
        write(*,*) "complex 35: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); call random_gauss(y,0.0_rk,1.0_rk); z1 = cmplx(x,y,rk)
    call cast(z1, into=string_var, locale="EU", fmt="z", im="*1i")
    call cast(string_var, into=z2, locale="EU", fmt="z", im="*1i")
    if ( maxval( abs(z1-z2)/abs(z1) ) < tol ) then
        write(*,*) "complex 36: SUCCESS"
    else
        write(*,*) "complex 36: FAILURE"
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
