program binary_npy
    use io_fortran_lib, only: to_file, from_file
    implicit none

    real :: x(20,20,20,20,20)
    real, allocatable :: x_npy(:,:,:,:,:)

    call random_number(x)

    call to_file(x, file="x.npy")

    call from_file("x.npy", into=x_npy)

    write(*,*) "x == x_npy : ", all(x == x_npy)
end program binary_npy
