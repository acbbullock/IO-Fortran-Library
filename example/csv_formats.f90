program csv_formats
    use io_fortran_lib, only: to_file, from_file
    implicit none (type, external)

    real :: x(1000,20)
    real, allocatable :: x_e(:,:), x_f(:,:), x_z(:,:)

    call random_number(x)

    call to_file(x, file="x_e.csv", header=["x"], fmt="e")
    call to_file(x, file="x_f.csv", header=["x"], fmt="f")
    call to_file(x, file="x_z.csv", header=["x"], fmt="z")

    call from_file("x_e.csv", into=x_e, header=.true., fmt="e")
    call from_file("x_f.csv", into=x_f, header=.true., fmt="f")
    call from_file("x_z.csv", into=x_z, header=.true., fmt="z")

    write(*,*) "x == x_e : ", all(x == x_e)
    write(*,*) "x == x_f : ", all(x == x_f)
    write(*,*) "x == x_z : ", all(x == x_z)
end program csv_formats
