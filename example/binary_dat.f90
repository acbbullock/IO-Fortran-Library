program binary_dat
    use io_fortran_lib, only: to_file, from_file
    implicit none (type, external)

    real :: x(20,20,20,20,20)
    real, allocatable :: x_dat(:,:,:,:,:)

    call random_number(x)

    call to_file(x, file="x.dat")

    call from_file("x.dat", into=x_dat, data_shape=shape(x))

    write(*,*) "x == x_dat : ", all(x == x_dat)
end program binary_dat
