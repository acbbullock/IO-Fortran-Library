program main
    !------------------------------------------------------------------------------------------------------------------
    !!  This program contains passing tests for the text processing routines of the IO Fortran Library.
    !------------------------------------------------------------------------------------------------------------------
    use, intrinsic :: iso_fortran_env, only: rk=>real64, ik=>int32, compiler_version, compiler_options
    use io_fortran_lib
    implicit none (type,external)

    integer, parameter :: rows = 500
    integer, parameter :: columns = 20

    character(len=:), allocatable :: logfile, logmsg
    character(len=10) :: date, time
    logical :: all_passing

    call random_init(repeatable=.true., image_distinct=.false.)

    logfile = 'test_results.log'
    call date_and_time(date=date, time=time)

    logmsg = 'RUNNING TESTS - date: '//trim(adjustl(date))//' | time: '//time
    call echo(string=logmsg//nl//repeat('-', ncopies=len(logmsg))//nl, file_name=logfile)

    logmsg = '    Integer kind is: '//str(ik)//nl//'    Real kind is: '//str(rk)//nl
    call echo(logmsg, logfile)

    all_passing = .true.

    test_int: block
        real(rk), allocatable, dimension(:) :: u
        real(rk), allocatable, dimension(:,:) :: x
        integer(ik), allocatable, dimension(:) :: i, j
        integer(ik), allocatable, dimension(:,:) :: k, l

        allocate( u(rows) )
        call random_number(u)
        i = floor(2147483647*u) + 1

        call to_file(i, file_name='./data/i.csv', header=['i'], dim=1, fmt='i')
        call from_file(file_name='./data/i.csv', into=j, header=.true., fmt='i')

        if ( all(i==j) ) then
            logmsg = '    i == j SUCCESS'
        else
            logmsg = '    i /= j FAILURE'
            all_passing = .false.
        end if

        call echo(logmsg, logfile)

        allocate( x(rows,columns) )
        call random_number(x)
        k = floor(2147483647*x) + 1

        call to_file(k, file_name='./data/k.csv', header=['k'], fmt='i')
        call from_file(file_name='./data/k.csv', into=l, header=.true., fmt='i')

        if ( all(k==l) ) then
            logmsg = '    k == l SUCCESS'
        else
            logmsg = '    k /= l FAILURE'
            all_passing = .false.
        end if

        call echo(logmsg, logfile)
    end block test_int

    test_real: block
        real(rk), allocatable, dimension(:) :: u, v
        real(rk), allocatable, dimension(:,:) :: x, y

        allocate( u(rows) )
        call random_number(u)

        call to_file(u, file_name='./data/u.csv', header=['u'], dim=1, fmt='e')
        call from_file(file_name='./data/u.csv', into=v, header=.true., fmt='e')

        if ( all(u==v) ) then
            logmsg = '    u == v SUCCESS'
        else
            logmsg = '    u /= v FAILURE'
            all_passing = .false.
        end if

        call echo(logmsg, logfile)

        allocate( x(rows,columns) )
        call random_number(x)

        call to_file(x, file_name='./data/x.csv', header=['x'], fmt='e')
        call from_file(file_name='./data/x.csv', into=y, header=.true., fmt='e')

        if ( all(x==y) ) then
            logmsg = '    x == y SUCCESS'
        else
            logmsg = '    x /= y FAILURE'
            all_passing = .false.
        end if

        call echo(logmsg, logfile)
    end block test_real

    test_complex: block
        real(rk), allocatable, dimension(:) :: u, v
        real(rk), allocatable, dimension(:,:) :: x, y
        complex(rk), allocatable, dimension(:) :: a, b
        complex(rk), allocatable, dimension(:,:) :: c, d

        allocate( u(rows), v(rows) )
        call random_number(u); call random_number(v)
        a = cmplx(u, v, kind=rk)

        call to_file(a, file_name='./data/a.csv', header=['a'], dim=1, fmt='e', im='j')
        call from_file(file_name='./data/a.csv', into=b, header=.true., fmt='e', im='j')

        if ( all(a==b) ) then
            logmsg = '    a == b SUCCESS'
        else
            logmsg = '    a /= b FAILURE'
            all_passing = .false.
        end if

        call echo(logmsg, logfile)

        allocate( x(rows,columns), y(rows,columns) )
        call random_number(x); call random_number(y)
        c = cmplx(x, y, kind=rk)

        call to_file(c, file_name='./data/c.csv', header=['c'], fmt='e', im='j')
        call from_file(file_name='./data/c.csv', into=d, header=.true., fmt='e', im='j')

        if ( all(c==d) ) then
            logmsg = '    c == d SUCCESS'
        else
            logmsg = '    c /= d for FAILURE'
            all_passing = .false.
        end if

        call echo(logmsg, logfile)
    end block test_complex

    if ( all_passing ) then
        logmsg = nl//'All tests are passing with compiler "'//compiler_version()//'" '// &
                     'using compiler options "'//compiler_options()//'".'//nl
    else
        logmsg = nl//'Some tests are failing with compiler "'//compiler_version()//'" '// &
                     'using compiler options "'//compiler_options()//'".'//nl
    end if

    call echo(logmsg, logfile)
end program main
