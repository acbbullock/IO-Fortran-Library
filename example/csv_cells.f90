program csv_cells
    use io_fortran_lib, only: String, str, cast
    implicit none

    type(String) :: csv
    type(String), allocatable :: cells(:,:)

    real :: x(1000,20)
    real, allocatable :: x_e(:,:), x_f(:,:), x_z(:,:)
    integer :: i

    call random_number(x); allocate( cells(1001,20) ); cells(1,:) = [(String("x"//str(i)), i = 1, 20)]

    call cast(x, into=cells(2:,:), fmt="e"); call csv%write_file(cells, file="x_e.csv")
    call cast(x, into=cells(2:,:), fmt="f"); call csv%write_file(cells, file="x_f.csv")
    call cast(x, into=cells(2:,:), fmt="z"); call csv%write_file(cells, file="x_z.csv")

    allocate( x_e, x_f, x_z, mold=x )
    call csv%read_file("x_e.csv", cell_array=cells); call cells(2:,:)%cast(into=x_e, fmt="e")
    call csv%read_file("x_f.csv", cell_array=cells); call cells(2:,:)%cast(into=x_f, fmt="f")
    call csv%read_file("x_z.csv", cell_array=cells); call cells(2:,:)%cast(into=x_z, fmt="z")

    write(*,*) "x == x_e : ", all(x == x_e)
    write(*,*) "x == x_f : ", all(x == x_f)
    write(*,*) "x == x_z : ", all(x == x_z)
end program csv_cells
