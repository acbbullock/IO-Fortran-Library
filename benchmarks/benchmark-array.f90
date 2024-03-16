program main
  use, intrinsic :: iso_fortran_env, only: int64, rk=>real32, dp=>real64, compiler_version, compiler_options
  use randoms, only: random_gauss
  use io_fortran_lib, only: String, cast, str, LF, operator(+)
  implicit none (type, external)

  type(String)              :: csv
  type(String), allocatable :: cells(:,:)

  integer(int64) :: t1, t2
  real(dp)       :: wall_time, rate

  integer,  parameter   :: n = 15000
  real(rk), allocatable :: x(:,:), y(:,:)

  allocate( x(n,n), cells(n,n) ); call random_gauss(x, 0e0_rk, 1.0_rk)
  write(*,"(a)")  "Compiler version: " + compiler_version()
  write(*,"(a)")  "Compiler options: " + compiler_options() + LF

  call system_clock(t1)
  call cast(x, into=cells, fmt="z")
  call system_clock(t2, count_rate=rate); wall_time = real(t2-t1,dp)/rate

  write(*,"(a)")  "Wall time for cast: " + str(wall_time, fmt="f", decimals=3) + " s"
  write(*,"(a)")  "Number of string conversions/second: " + str(nint(size(x)/wall_time)) + LF

  call system_clock(t1)
  call csv%write_file(cells, file="bigx.csv")
  call system_clock(t2, count_rate=rate); wall_time = real(t2-t1,dp)/rate

  write(*,"(a)")  "Wall time for write_file: " + str(wall_time, fmt="f", decimals=3) + " s"
  write(*,"(a)")  "Estimated file size: " + str(csv%len64()/1e9, fmt="f", decimals=6) + " GB" + LF

  call system_clock(t1)
  call csv%read_file("bigx.csv", cell_array=cells)
  call system_clock(t2, count_rate=rate); wall_time = real(t2-t1,dp)/rate

  write(*,"(a)")  "Wall time for read_file: " + str(wall_time, fmt="f", decimals=3) + " s" + LF

  call csv%empty(); allocate( y(n,n) )

  call system_clock(t1)
  call cast(cells, into=y, fmt="z")
  call system_clock(t2, count_rate=rate); wall_time = real(t2-t1,dp)/rate

  write(*,"(a)")   "Wall time for cast: " + str(wall_time, fmt="f", decimals=3) + " s"
  write(*,"(a)")   "Number of string casts/second: " + str(nint(size(x)/wall_time))
  write(*,"(a,l)") "Data is exact match: ", all(x == y)
end program main
