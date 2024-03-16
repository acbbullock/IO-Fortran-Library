program main
  use, intrinsic :: iso_fortran_env, only: i64=>int64, dp=>real64
  use io_fortran_lib, only: String, str, operator(+)
  implicit none (type, external)

  integer, parameter :: largest = huge(0), smallest = -largest - 1, nsteps = 128
  integer, parameter :: step_size = int( (int(largest,i64) - int(smallest,i64) )/int(nsteps,i64))

  type(String) :: csv
  type(String), allocatable, target :: cells(:,:)
  type(String), pointer, contiguous :: cells_p(:,:)

  integer :: lower, upper, i, j

  integer(i64) :: t1, t2, total_length
  real(dp)     :: wall_time, rate

  write(*,"(a)") "Writing integers from " + str(smallest) + " to " + str(largest)

  allocate( cells(step_size, 2) )
  total_length = 0_i64
  call system_clock(t1)

  do j = 1, nsteps
    lower = smallest + (j-1)*step_size
    upper = lower + step_size - 1

    cells_p(lower:upper,1:2) => cells(:,:)

    do concurrent ( i = lower:upper )
      cells_p(i,1) = String(i, fmt="i")
      cells_p(i,2) = String(i, fmt="z")
    end do

    call csv%write_file(cells_p, file="int32.csv", append=.true.)
    total_length = total_length + csv%len64()

    write(*,"(a)")  "File length: " + str(total_length/1e9, fmt="f", decimals=3) + " GB in cycle " + str(j)
  end do

  lower = upper + 1; upper = largest
  nullify(cells_p); deallocate(cells); allocate( cells(lower:upper,2) )

  do concurrent ( i = lower:upper )
    cells(i,1) = String(i, fmt="i")
    cells(i,2) = String(i, fmt="z")
  end do

  call csv%write_file(cells, file="int32.csv", append=.true.)
  total_length = total_length + csv%len64()

  write(*,"(a)")  "File length: " + str(total_length/1e9, fmt="f", decimals=3) + " GB"

  call system_clock(t2, count_rate=rate); wall_time = real(t2-t1,dp)/rate
  write(*,"(a)")  "Total time for write: " + str(wall_time/60, fmt="f", decimals=3) + " minutes"
end program main
