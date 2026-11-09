program quickstart
    use io_fortran_lib, only: String, str, to_file, from_file, aprint
    implicit none (type, external)

    real              :: x(3,2) = reshape([1.0, 2.5, -3.0, 4.0, 5.5, 6.25], [3,2])
    real, allocatable :: y(:,:), z(:,:)
    type(String)      :: line

    call to_file(x, file="x.csv", header=["x"])     ! A header row x1,x2, then three rows of data
    call from_file("x.csv", into=y, header=.true.)  ! y is allocated with shape [3,2]
    call aprint(y, fmt="f", decimals=2)

    call to_file(x, file="x.npy")                   ! In Python: x = numpy.load("x.npy")
    call from_file("x.npy", into=z)                 ! The shape comes from the file

    line = String("x(2,1) = "//str(x(2,1), fmt="f", decimals=2))
    write(*,"(a)") line%as_str()                     ! x(2,1) = 2.50
    write(*,*) "Read back exactly: ", all(y == x) .and. all(z == x)
end program quickstart
