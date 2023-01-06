program main
    use, intrinsic :: iso_fortran_env, only: ik=>int64, rk=>real128, compiler_version, compiler_options
    use io_fortran_lib
    implicit none (type,external)

    type(String) :: logmsg
    character(len=:), allocatable :: logfile
    character(len=10) :: date, time
    logical :: all_passing

    call random_init(repeatable=.false., image_distinct=.true.)

    logfile = 'tests.log'
    call date_and_time(date=date, time=time)

    logmsg = String('RUNNING TESTS (str|cast) - date: ' + trim(adjustl(date)) + ' | time: ' + time + LF)
    call logmsg%push('-'**logmsg%len() + LF)

    all_passing = .true.
    write(*,'(a)') logmsg%as_str()

    test_int: block
        real(rk) :: x
        integer(ik) :: i, j
        character(len=:), allocatable :: i_str

        call random_number(x); i = floor(2147483647*x, ik) + 1_ik
        i_str = str(i, fmt='i')
        call cast(i_str, into=j, fmt='i')
        if ( i == j ) then
            write(*,*) 'int 1: SUCCESS'
        else
            write(*,*) 'int 1: FAILURE', i, j
            all_passing = .false.
        end if

        call random_number(x); i = floor(2147483647*x, ik) + 1_ik
        i_str = str(i, fmt='z')
        call cast(i_str, into=j, fmt='z')
        if ( i == j ) then
            write(*,*) 'int 2: SUCCESS'
        else
            write(*,*) 'int 2: FAILURE', i, j
            all_passing = .false.
        end if
    end block test_int

    test_real: block
        real(rk) :: x, y
        character(len=:), allocatable :: x_str

        call random_number(x)
        x_str = str(x, locale='US', fmt='e')
        call cast(x_str, into=y, locale='US', fmt='e')
        if ( x == y ) then
            write(*,*) 'real 1: SUCCESS'
        else
            write(*,*) 'real 1: FAILURE', x, y
            all_passing = .false.
        end if

        call random_number(x)
        x_str = str(x, locale='US', fmt='f')
        call cast(x_str, into=y, locale='US', fmt='f')
        if ( x == y ) then
            write(*,*) 'real 2: SUCCESS'
        else
            write(*,*) 'real 2: FAILURE', x, y
            all_passing = .false.
        end if

        call random_number(x)
        x_str = str(x, locale='US', fmt='z')
        call cast(x_str, into=y, locale='US', fmt='z')
        if ( x == y ) then
            write(*,*) 'real 3: SUCCESS'
        else
            write(*,*) 'real 3: FAILURE', x, y
            all_passing = .false.
        end if

        call random_number(x)
        x_str = str(x, locale='EU', fmt='e')
        call cast(x_str, into=y, locale='EU', fmt='e')
        if ( x == y ) then
            write(*,*) 'real 4: SUCCESS'
        else
            write(*,*) 'real 4: FAILURE', x, y
            all_passing = .false.
        end if

        call random_number(x)
        x_str = str(x, locale='EU', fmt='f')
        call cast(x_str, into=y, locale='EU', fmt='f')
        if ( x == y ) then
            write(*,*) 'real 5: SUCCESS'
        else
            write(*,*) 'real 5: FAILURE', x, y
            all_passing = .false.
        end if

        call random_number(x)
        x_str = str(x, locale='EU', fmt='z')
        call cast(x_str, into=y, locale='EU', fmt='z')
        if ( x == y ) then
            write(*,*) 'real 6: SUCCESS'
        else
            write(*,*) 'real 6: FAILURE', x, y
            all_passing = .false.
        end if
    end block test_real

    test_complex: block
        real(rk) :: x, y
        complex(rk) :: z1, z2
        character(len=:), allocatable :: z_str

        call random_number(x); call random_number(y)
        z1 = -cmplx(x,y,rk)
        z_str = str(z1, locale='US', fmt='e', im='')
        call cast(z_str, into=z2, locale='US', fmt='e', im='')
        if ( z1 == z2 ) then
            write(*,*) 'complex 1: SUCCESS'
        else
            write(*,*) 'complex 1: FAILURE', z1, z2
            all_passing = .false.
        end if

        call random_number(x); call random_number(y)
        z1 = -cmplx(x,y,rk)
        z_str = str(z1, locale='US', fmt='f', im='')
        call cast(z_str, into=z2, locale='US', fmt='f', im='')
        if ( z1 == z2 ) then
            write(*,*) 'complex 2: SUCCESS'
        else
            write(*,*) 'complex 2: FAILURE', z1, z2
            all_passing = .false.
        end if

        call random_number(x); call random_number(y)
        z1 = -cmplx(x,y,rk)
        z_str = str(z1, locale='US', fmt='z', im='')
        call cast(z_str, into=z2, locale='US', fmt='z', im='')
        if ( z1 == z2 ) then
            write(*,*) 'complex 3: SUCCESS'
        else
            write(*,*) 'complex 3: FAILURE', z1, z2
            all_passing = .false.
        end if

        call random_number(x); call random_number(y)
        z1 = -cmplx(x,y,rk)
        z_str = str(z1, locale='US', fmt='e', im='j')
        call cast(z_str, into=z2, locale='US', fmt='e', im='j')
        if ( z1 == z2 ) then
            write(*,*) 'complex 4: SUCCESS'
        else
            write(*,*) 'complex 4: FAILURE', z1, z2
            all_passing = .false.
        end if

        call random_number(x); call random_number(y)
        z1 = -cmplx(x,y,rk)
        z_str = str(z1, locale='US', fmt='f', im='j')
        call cast(z_str, into=z2, locale='US', fmt='f', im='j')
        if ( z1 == z2 ) then
            write(*,*) 'complex 5: SUCCESS'
        else
            write(*,*) 'complex 5: FAILURE', z1, z2
            all_passing = .false.
        end if

        call random_number(x); call random_number(y)
        z1 = -cmplx(x,y,rk)
        z_str = str(z1, locale='US', fmt='z', im='j')
        call cast(z_str, into=z2, locale='US', fmt='z', im='j')
        if ( z1 == z2 ) then
            write(*,*) 'complex 6: SUCCESS'
        else
            write(*,*) 'complex 6: FAILURE', z1, z2
            all_passing = .false.
        end if

        call random_number(x); call random_number(y)
        z1 = -cmplx(x,y,rk)
        z_str = str(z1, locale='US', fmt='e', im='*1i')
        call cast(z_str, into=z2, locale='US', fmt='e', im='*1i')
        if ( z1 == z2 ) then
            write(*,*) 'complex 7: SUCCESS'
        else
            write(*,*) 'complex 7: FAILURE', z1, z2
            all_passing = .false.
        end if

        call random_number(x); call random_number(y)
        z1 = -cmplx(x,y,rk)
        z_str = str(z1, locale='US', fmt='f', im='*1i')
        call cast(z_str, into=z2, locale='US', fmt='f', im='*1i')
        if ( z1 == z2 ) then
            write(*,*) 'complex 8: SUCCESS'
        else
            write(*,*) 'complex 8: FAILURE', z1, z2
            all_passing = .false.
        end if

        call random_number(x); call random_number(y)
        z1 = -cmplx(x,y,rk)
        z_str = str(z1, locale='US', fmt='z', im='*1i')
        call cast(z_str, into=z2, locale='US', fmt='z', im='*1i')
        if ( z1 == z2 ) then
            write(*,*) 'complex 9: SUCCESS'
        else
            write(*,*) 'complex 9: FAILURE', z1, z2
            all_passing = .false.
        end if

        call random_number(x); call random_number(y)
        z1 = -cmplx(x,y,rk)
        z_str = str(z1, locale='EU', fmt='e', im='')
        call cast(z_str, into=z2, locale='EU', fmt='e', im='')
        if ( z1 == z2 ) then
            write(*,*) 'complex 10: SUCCESS'
        else
            write(*,*) 'complex 10: FAILURE', z1, z2
            all_passing = .false.
        end if

        call random_number(x); call random_number(y)
        z1 = -cmplx(x,y,rk)
        z_str = str(z1, locale='EU', fmt='f', im='')
        call cast(z_str, into=z2, locale='EU', fmt='f', im='')
        if ( z1 == z2 ) then
            write(*,*) 'complex 11: SUCCESS'
        else
            write(*,*) 'complex 11: FAILURE', z1, z2
            all_passing = .false.
        end if

        call random_number(x); call random_number(y)
        z1 = -cmplx(x,y,rk)
        z_str = str(z1, locale='EU', fmt='z', im='')
        call cast(z_str, into=z2, locale='EU', fmt='z', im='')
        if ( z1 == z2 ) then
            write(*,*) 'complex 12: SUCCESS'
        else
            write(*,*) 'complex 12: FAILURE', z1, z2
            all_passing = .false.
        end if

        call random_number(x); call random_number(y)
        z1 = -cmplx(x,y,rk)
        z_str = str(z1, locale='EU', fmt='e', im='j')
        call cast(z_str, into=z2, locale='EU', fmt='e', im='j')
        if ( z1 == z2 ) then
            write(*,*) 'complex 13: SUCCESS'
        else
            write(*,*) 'complex 13: FAILURE', z1, z2
            all_passing = .false.
        end if

        call random_number(x); call random_number(y)
        z1 = -cmplx(x,y,rk)
        z_str = str(z1, locale='EU', fmt='f', im='j')
        call cast(z_str, into=z2, locale='EU', fmt='f', im='j')
        if ( z1 == z2 ) then
            write(*,*) 'complex 14: SUCCESS'
        else
            write(*,*) 'complex 14: FAILURE', z1, z2
            all_passing = .false.
        end if

        call random_number(x); call random_number(y)
        z1 = -cmplx(x,y,rk)
        z_str = str(z1, locale='EU', fmt='z', im='j')
        call cast(z_str, into=z2, locale='EU', fmt='z', im='j')
        if ( z1 == z2 ) then
            write(*,*) 'complex 15: SUCCESS'
        else
            write(*,*) 'complex 15: FAILURE', z1, z2
            all_passing = .false.
        end if

        call random_number(x); call random_number(y)
        z1 = -cmplx(x,y,rk)
        z_str = str(z1, locale='EU', fmt='e', im='*1i')
        call cast(z_str, into=z2, locale='EU', fmt='e', im='*1i')
        if ( z1 == z2 ) then
            write(*,*) 'complex 16: SUCCESS'
        else
            write(*,*) 'complex 16: FAILURE', z1, z2
            all_passing = .false.
        end if

        call random_number(x); call random_number(y)
        z1 = -cmplx(x,y,rk)
        z_str = str(z1, locale='EU', fmt='f', im='*1i')
        call cast(z_str, into=z2, locale='EU', fmt='f', im='*1i')
        if ( z1 == z2 ) then
            write(*,*) 'complex 17: SUCCESS'
        else
            write(*,*) 'complex 17: FAILURE', z1, z2
            all_passing = .false.
        end if

        call random_number(x); call random_number(y)
        z1 = -cmplx(x,y,rk)
        z_str = str(z1, locale='EU', fmt='z', im='*1i')
        call cast(z_str, into=z2, locale='EU', fmt='z', im='*1i')
        if ( z1 == z2 ) then
            write(*,*) 'complex 18: SUCCESS'
        else
            write(*,*) 'complex 18: FAILURE', z1, z2
            all_passing = .false.
        end if
    end block test_complex

    if ( all_passing ) then
        call logmsg%push('All tests are "PASSING" with compiler "' + compiler_version() + '" ' +  &
                         'using compiler options "' + compiler_options() + '".' + LF)
    else
        call logmsg%push('Some tests are "FAILING" with compiler "' + compiler_version() + '" ' +  &
                         'using compiler options "' + compiler_options() + '".' + LF)
    end if

    call logmsg%echo(logfile)
end program main
