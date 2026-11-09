program binary_shape_file
    use io_fortran_lib, only: to_file, from_file
    implicit none (type, external)

    real :: x(20,20,20,20,20)
    real, allocatable :: x_dat(:,:,:,:,:)
    integer, allocatable :: x_shape(:)

    call random_number(x)

    call to_file(x, file="x.dat")
    call to_file(shape(x), file="x_shape.csv")

    call from_file("x_shape.csv", into=x_shape)
    call from_file("x.dat", into=x_dat, data_shape=x_shape)

    write(*,*) "x == x_dat : ", all(x == x_dat)
end program binary_shape_file
