program main
    use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64, qp=>real128, int8, int16, int32, int64
    use io_fortran_lib
    implicit none (type,external)

    real(sp), allocatable, dimension(:,:) :: x, y

    integer(int64) t1, t2                                                                        !! Clock variables
    real(dp) rate, telapse                                                                       !! Clock variables

    call random_init(repeatable=.true., image_distinct=.false.)

    allocate( x(5,2) )
    call random_number(x)

    call aprint(x)
    call system_clock(t1)
    call to_file(x, file_name='./data/x.csv', header=['a', 'b'], fmt='f')
    call system_clock(t2, count_rate=rate)                                                        !! Stop clock
    telapse = real((t2-t1), kind=rk)/rate                                 !! Total elapsed wall clock time in s
    write(*,*) 'time = ', telapse

    call from_file(file_name='./data/x.csv', into=y, header=.true., fmt='f')
    call aprint(y)
end program main
