program binary_npz
    use io_fortran_lib, only: String, to_file, from_file, npz_names
    implicit none (type, external)

    real :: x(30,40)
    integer :: counts(10), i
    real, allocatable :: x_npz(:,:)
    integer, allocatable :: counts_npz(:)
    type(String), allocatable :: names(:)

    call random_number(x)
    counts = [(i**2, i = 1, 10)]

    call to_file(x, file="arrays.npz", name="x")
    call to_file(counts, file="arrays.npz", name="counts")

    call npz_names("arrays.npz", names)
    write(*,*) "arrays: ", (names(i)%as_str()//" ", i = 1, size(names))

    call from_file("arrays.npz", into=x_npz, name="x")
    call from_file("arrays.npz", into=counts_npz, name="counts")

    write(*,*) "x == x_npz : ", all(x == x_npz)
    write(*,*) "counts == counts_npz : ", all(counts == counts_npz)
end program binary_npz
