program main
    use, intrinsic :: iso_fortran_env, only: ik=>int16, rk=>real32, &
                      compiler_version, compiler_options
    use io_fortran_lib, only: String, cast, LF, str, operator(+), operator(**)
    use randoms,        only: random_gauss
    implicit none (type, external)

    integer,          parameter :: n = 2000
    character(len=*), parameter :: logfile = "./test/tests.log"

    character(len=10) :: date, time
    type(String)      :: logmsg, string_var(n)
    logical           :: all_passing
    real(rk)          :: x(n)
    integer(ik)       :: i(n), j(n)

    call random_init(repeatable=.false., image_distinct=.true.)
    call date_and_time(date=date, time=time)

    logmsg = String("RUNNING TESTS (array) | date: " + trim(adjustl(date)) + &
                    " | time: "                      + time                + &
                    " | int kind: "                  + str(ik)             )

    call logmsg%push(LF + "-"**logmsg%len() + LF)

    all_passing = .true.
    write(*,"(a)") logmsg%as_str()

    call random_gauss(x,0.0_rk,1.0_rk); i = floor(huge(1_ik)*x, ik) + 1_ik
    call cast(String(i, fmt="i"), into=j, fmt="i")
    if ( all(i == j) ) then
        write(*,*) "int 1: SUCCESS"
    else
        write(*,*) "int 1: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); i = floor(huge(1_ik)*x, ik) + 1_ik
    call cast(String(i, fmt="z"), into=j, fmt="z")
    if ( all(i == j) ) then
        write(*,*) "int 2: SUCCESS"
    else
        write(*,*) "int 2: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); i = floor(huge(1_ik)*x, ik) + 1_ik
    call cast(i, into=string_var, fmt="i"); call cast(string_var, into=j, fmt="i")
    if ( all(i == j) ) then
        write(*,*) "int 3: SUCCESS"
    else
        write(*,*) "int 3: FAILURE"
        all_passing = .false.
    end if

    call random_gauss(x,0.0_rk,1.0_rk); i = floor(huge(1_ik)*x, ik) + 1_ik
    call cast(i, into=string_var, fmt="z"); call cast(string_var, into=j, fmt="z")
    if ( all(i == j) ) then
        write(*,*) "int 4: SUCCESS"
    else
        write(*,*) "int 4: FAILURE"
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
