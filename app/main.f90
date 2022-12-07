!!---------------------------------------------------------------------------------------------------------------------
!!  This program contains passing tests for the text processing routines of the IO Fortran Library.
!!---------------------------------------------------------------------------------------------------------------------
program main
    use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64, qp=>real128, int8, int16, int32, int64
    use io_fortran_lib
    implicit none (type,external)

    integer(int64) :: t1, t2
    real(qp) :: rate, telapse

    integer, parameter :: rows = 500
    integer, parameter :: columns = 20

    call random_init(repeatable=.true., image_distinct=.false.)

    test_int: block
        real(dp), allocatable, dimension(:) :: u
        real(dp), allocatable, dimension(:,:) :: x
        integer(int32), allocatable, dimension(:) :: i, j
        integer(int32), allocatable, dimension(:,:) :: k, l

        allocate( u(rows) )
        call random_number(u)
        i = floor(2147483647*u) + 1

        call system_clock(t1)
        call to_file(i, file_name='./data/i.csv', header=['i'], dim=1, fmt='i')
        call system_clock(t2, count_rate=rate)
        telapse = real((t2-t1), kind=qp)/rate
        write(*,'(a)') 'time = '//str(telapse, fmt='f', decimals=3)//' seconds for i to disk'

        call system_clock(t1)
        call from_file(file_name='./data/i.csv', into=j, header=.true., fmt='i')
        call system_clock(t2, count_rate=rate)
        telapse = real((t2-t1), kind=qp)/rate
        write(*,'(a)') 'time = '//str(telapse, fmt='f', decimals=3)//' seconds for j from disk'

        if ( all(i==j) ) then
            write(*,'(a)') 'i == j SUCCESS'
        else
            write(*,'(a)') 'i /= j FAILURE'
        end if
        write(*,*)

        allocate( x(rows,columns) )
        call random_number(x)
        k = floor(2147483647*x) + 1

        call system_clock(t1)
        call to_file(k, file_name='./data/k.csv', header=['k'], fmt='i')
        call system_clock(t2, count_rate=rate)
        telapse = real((t2-t1), kind=qp)/rate
        write(*,'(a)') 'time = '//str(telapse, fmt='f', decimals=3)//' seconds for k to disk'

        call system_clock(t1)
        call from_file(file_name='./data/k.csv', into=l, header=.true., fmt='i')
        call system_clock(t2, count_rate=rate)
        telapse = real((t2-t1), kind=qp)/rate
        write(*,'(a)') 'time = '//str(telapse, fmt='f', decimals=3)//' seconds for l from disk'

        if ( all(k==l) ) then
            write(*,'(a)') 'k == l SUCCESS'
        else
            write(*,'(a)') 'k /= l FAILURE'
        end if
        write(*,*)
    end block test_int

    test_real: block
        real(dp), allocatable, dimension(:) :: u, v
        real(dp), allocatable, dimension(:,:) :: x, y

        allocate( u(rows), v(rows) )
        call random_number(u); call random_number(v)

        call system_clock(t1)
        call to_file(u, file_name='./data/u.csv', header=['u'], dim=1, fmt='f')
        call system_clock(t2, count_rate=rate)
        telapse = real((t2-t1), kind=qp)/rate
        write(*,'(a)') 'time = '//str(telapse, fmt='f', decimals=3)//' seconds for u to disk'

        call system_clock(t1)
        call from_file(file_name='./data/u.csv', into=v, header=.true., fmt='f')
        call system_clock(t2, count_rate=rate)
        telapse = real((t2-t1), kind=qp)/rate
        write(*,'(a)') 'time = '//str(telapse, fmt='f', decimals=3)//' seconds for v from disk'

        if ( all(u==v) ) then
            write(*,'(a)') 'u == v SUCCESS'
        else
            write(*,'(a)') 'u /= v FAILURE'
        end if
        write(*,*)

        allocate( x(rows,columns), y(rows,columns) )
        call random_number(x); call random_number(y)

        call system_clock(t1)
        call to_file(x, file_name='./data/x.csv', header=['x'], fmt='f')
        call system_clock(t2, count_rate=rate)
        telapse = real((t2-t1), kind=qp)/rate
        write(*,'(a)') 'time = '//str(telapse, fmt='f', decimals=3)//' seconds for x to disk'

        call system_clock(t1)
        call from_file(file_name='./data/x.csv', into=y, header=.true., fmt='f')
        call system_clock(t2, count_rate=rate)
        telapse = real((t2-t1), kind=qp)/rate
        write(*,'(a)') 'time = '//str(telapse, fmt='f', decimals=3)//' seconds for y from disk'

        if ( all(x==y) ) then
            write(*,'(a)') 'x == y SUCCESS'
        else
            write(*,'(a)') 'x /= y FAILURE'
        end if
        write(*,*)
    end block test_real

    test_complex: block
        real(dp), allocatable, dimension(:) :: u, v
        real(dp), allocatable, dimension(:,:) :: x, y
        complex(dp), allocatable, dimension(:) :: a, b
        complex(dp), allocatable, dimension(:,:) :: c, d

        allocate( u(rows), v(rows) )
        call random_number(u); call random_number(v)
        a = cmplx(u, v, kind=dp)

        call system_clock(t1)
        call to_file(a, file_name='./data/a.csv', header=['a'], dim=1, fmt='f', im='j')
        call system_clock(t2, count_rate=rate)
        telapse = real((t2-t1), kind=qp)/rate
        write(*,'(a)') 'time = '//str(telapse, fmt='f', decimals=3)//' seconds for a to disk'

        call system_clock(t1)
        call from_file(file_name='./data/a.csv', into=b, header=.true., fmt='f', im='j')
        call system_clock(t2, count_rate=rate)
        telapse = real((t2-t1), kind=qp)/rate
        write(*,'(a)') 'time = '//str(telapse, fmt='f', decimals=3)//' seconds for b from disk'

        if ( all(a==b) ) then
            write(*,'(a)') 'a == b SUCCESS'
        else
            write(*,'(a)') 'a /= b FAILURE'
        end if
        write(*,*)

        allocate( x(rows,columns), y(rows,columns) )
        call random_number(x); call random_number(y)
        c = cmplx(x, y, kind=dp)

        call system_clock(t1)
        call to_file(c, file_name='./data/c.csv', header=['c'], fmt='f', im='j')
        call system_clock(t2, count_rate=rate)
        telapse = real((t2-t1), kind=qp)/rate
        write(*,'(a)') 'time = '//str(telapse, fmt='f', decimals=3)//' seconds for c to disk'

        call system_clock(t1)
        call from_file(file_name='./data/c.csv', into=d, header=.true., fmt='f', im='j')
        call system_clock(t2, count_rate=rate)
        telapse = real((t2-t1), kind=qp)/rate
        write(*,'(a)') 'time = '//str(telapse, fmt='f', decimals=3)//' seconds for d from disk'

        if ( all(c==d) ) then
            write(*,'(a)') 'c == d SUCCESS'
        else
            write(*,'(a)') 'c /= d for FAILURE'
        end if
        write(*,*)
    end block test_complex
end program main
