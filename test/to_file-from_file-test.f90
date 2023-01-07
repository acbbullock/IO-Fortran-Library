program main
    use, intrinsic :: iso_fortran_env, only: ik=>int64, rk=>real128, compiler_version, compiler_options
    use io_fortran_lib
    implicit none (type,external)

    integer, parameter :: rows = 100
    integer, parameter :: cols = 20

    type(String) :: logmsg
    character(len=:), allocatable :: logfile
    character(len=10) :: date, time
    logical :: all_passing

    call random_init(repeatable=.false., image_distinct=.true.)

    logfile = 'tests.log'
    call date_and_time(date=date, time=time)

    logmsg = String('RUNNING TESTS (to_file|from_file) - date: ' + trim(adjustl(date)) + ' | time: ' + time + LF)
    call logmsg%push('-'**logmsg%len() + LF)

    all_passing = .true.
    write(*,'(a)') logmsg%as_str()

    test_int: block
        real(rk), allocatable :: u(:), x(:,:)
        integer(ik), allocatable :: i(:), k(:,:)
        integer(ik), allocatable :: j(:), l(:,:)

        allocate( u(rows), x(rows,cols), i(rows), k(rows,cols) )

        call random_number(u); i = floor(2147483647*u, ik) + 1_ik
        call to_file(i, file_name='./data/i.csv', header=['i'], fmt='i')
        call from_file(file_name='./data/i.csv', into=j, header=.true., fmt='i')
        if ( all(i==j) ) then
            write(*,*) 'int 1: SUCCESS'
        else
            write(*,*) 'int 1: FAILURE'
            all_passing = .false.
        end if

        call random_number(u); i = floor(2147483647*u, ik) + 1_ik
        call to_file(i, file_name='./data/i_z.csv', header=['i'], fmt='z')
        call from_file(file_name='./data/i_z.csv', into=j, header=.true., fmt='z')
        if ( all(i==j) ) then
            write(*,*) 'int 2: SUCCESS'
        else
            write(*,*) 'int 2: FAILURE'
            all_passing = .false.
        end if

        call random_number(x); k = floor(2147483647*x, ik) + 1_ik
        call to_file(k, file_name='./data/k.csv', header=['k'], fmt='i')
        call from_file(file_name='./data/k.csv', into=l, header=.true., fmt='i')
        if ( all(k==l) ) then
            write(*,*) 'int 3: SUCCESS'
        else
            write(*,*) 'int 3: FAILURE'
            all_passing = .false.
        end if

        call random_number(x); k = floor(2147483647*x, ik) + 1_ik
        call to_file(k, file_name='./data/k_z.csv', header=['k'], fmt='z')
        call from_file(file_name='./data/k_z.csv', into=l, header=.true., fmt='z')
        if ( all(k==l) ) then
            write(*,*) 'int 4: SUCCESS'
        else
            write(*,*) 'int 4: FAILURE'
            all_passing = .false.
        end if
    end block test_int

    test_real: block
        real(rk), allocatable :: u(:), x(:,:)
        real(rk), allocatable :: v(:), y(:,:)

        allocate( u(rows), x(rows,cols) )

        call random_number(u)
        call to_file(u, file_name='./data/u_e.csv', header=['u'], fmt='e')
        call from_file(file_name='./data/u_e.csv', into=v, header=.true., fmt='e')
        if ( all(u==v) ) then
            write(*,*) 'real 1: SUCCESS'
        else
            write(*,*) 'real 1: FAILURE'
            all_passing = .false.
        end if

        call random_number(u)
        call to_file(u, file_name='./data/u_f.csv', header=['u'], fmt='f')
        call from_file(file_name='./data/u_f.csv', into=v, header=.true., fmt='f')
        if ( all(u==v) ) then
            write(*,*) 'real 2: SUCCESS'
        else
            write(*,*) 'real 2: FAILURE'
            all_passing = .false.
        end if

        call random_number(u)
        call to_file(u, file_name='./data/u_z.csv', header=['u'], fmt='z')
        call from_file(file_name='./data/u_z.csv', into=v, header=.true., fmt='z')
        if ( all(u==v) ) then
            write(*,*) 'real 3: SUCCESS'
        else
            write(*,*) 'real 3: FAILURE'
            all_passing = .false.
        end if

        call random_number(x)
        call to_file(x, file_name='./data/x_e.csv', header=['x'], fmt='e')
        call from_file(file_name='./data/x_e.csv', into=y, header=.true., fmt='e')
        if ( all(x==y) ) then
            write(*,*) 'real 4: SUCCESS'
        else
            write(*,*) 'real 4: FAILURE'
            all_passing = .false.
        end if

        call random_number(x)
        call to_file(x, file_name='./data/x_f.csv', header=['x'], fmt='f')
        call from_file(file_name='./data/x_f.csv', into=y, header=.true., fmt='f')
        if ( all(x==y) ) then
            write(*,*) 'real 5: SUCCESS'
        else
            write(*,*) 'real 5: FAILURE'
            all_passing = .false.
        end if

        call random_number(x)
        call to_file(x, file_name='./data/x_z.csv', header=['x'], fmt='z')
        call from_file(file_name='./data/x_z.csv', into=y, header=.true., fmt='z')
        if ( all(x==y) ) then
            write(*,*) 'real 6: SUCCESS'
        else
            write(*,*) 'real 6: FAILURE'
            all_passing = .false.
        end if
    end block test_real

    test_complex: block
        real(rk), allocatable :: u(:), v(:), x(:,:), y(:,:)
        complex(rk), allocatable :: a(:), b(:), c(:,:), d(:,:)

        allocate( u(rows), v(rows), x(rows,cols), y(rows,cols) )

        call random_number(u); call random_number(v); a = cmplx(u,v,rk)
        call to_file(a, file_name='./data/a_e.csv', header=['a'], fmt='e', im='j')
        call from_file(file_name='./data/a_e.csv', into=b, header=.true., fmt='e', im='j')
        if ( all(a==b) ) then
            write(*,*) 'complex 1: SUCCESS'
        else
            write(*,*) 'complex 1: FAILURE'
            all_passing = .false.
        end if

        call random_number(u); call random_number(v); a = cmplx(u,v,rk)
        call to_file(a, file_name='./data/a_f.csv', header=['a'], fmt='f', im='j')
        call from_file(file_name='./data/a_f.csv', into=b, header=.true., fmt='f', im='j')
        if ( all(a==b) ) then
            write(*,*) 'complex 2: SUCCESS'
        else
            write(*,*) 'complex 2: FAILURE'
            all_passing = .false.
        end if

        call random_number(u); call random_number(v); a = cmplx(u,v,rk)
        call to_file(a, file_name='./data/a_z.csv', header=['a'], fmt='z', im='j')
        call from_file(file_name='./data/a_z.csv', into=b, header=.true., fmt='z', im='j')
        if ( all(a==b) ) then
            write(*,*) 'complex 3: SUCCESS'
        else
            write(*,*) 'complex 3: FAILURE'
            all_passing = .false.
        end if

        call random_number(x); call random_number(y); c = cmplx(x,y,rk)
        call to_file(c, file_name='./data/c_e.csv', header=['c'], fmt='e', im='j')
        call from_file(file_name='./data/c_e.csv', into=d, header=.true., fmt='e', im='j')
        if ( all(c==d) ) then
            write(*,*) 'complex 4: SUCCESS'
        else
            write(*,*) 'complex 4: FAILURE'
            all_passing = .false.
        end if

        call random_number(x); call random_number(y); c = cmplx(x,y,rk)
        call to_file(c, file_name='./data/c_f.csv', header=['c'], fmt='f', im='j')
        call from_file(file_name='./data/c_f.csv', into=d, header=.true., fmt='f', im='j')
        if ( all(c==d) ) then
            write(*,*) 'complex 5: SUCCESS'
        else
            write(*,*) 'complex 5: FAILURE'
            all_passing = .false.
        end if

        call random_number(x); call random_number(y); c = cmplx(x,y,rk)
        call to_file(c, file_name='./data/c_z.csv', header=['c'], fmt='z', im='j')
        call from_file(file_name='./data/c_z.csv', into=d, header=.true., fmt='z', im='j')
        if ( all(c==d) ) then
            write(*,*) 'complex 6: SUCCESS'
        else
            write(*,*) 'complex 6: FAILURE'
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
