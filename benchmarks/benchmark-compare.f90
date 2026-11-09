program benchmark_compare
  !---------------------------------------------------------------------------------------------------------------------
  !! Times the library's common operations against plain Fortran that does the same work, and prints the results as a
  !! Markdown table. Each time is the best of `REPEATS` runs, to leave out a slow first run and other programs.
  !!
  !! Run it with `fpm run benchmark-compare --flag -O2`. An optional argument scales the data, so
  !! `fpm run benchmark-compare --flag -O2 -- 0.1` is a quick run on a tenth of it. It writes its files in the
  !! current directory and deletes them at the end. The results on one machine are in the docs, under Benchmarking.
  !---------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: int64, dp => real64, compiler_version, compiler_options
  use io_fortran_lib, only: String, to_file, from_file, cast, operator(/=)
  implicit none (type, external)

  integer,          parameter :: REPEATS = 3
  character(len=*), parameter :: REAL_FMT = "(es25.17e3)"                 ! The digits the library writes
  character(len=*), parameter :: ROW_FMT  = "(*(es25.17e3,:,','))"

  type :: result_row
    character(len=:), allocatable :: operation, library, plain
    real(dp)                      :: t_library = huge(1.0_dp), t_plain = huge(1.0_dp)
  end type result_row

  type(result_row)              :: rows(9)
  real(dp),         allocatable :: x(:,:), y(:,:)          ! For text: nrows x ncols
  real(dp),         allocatable :: xb(:), yb(:)            ! For binary files, which are fast, so more data
  type(String),     allocatable :: cells(:,:)
  character(len=25), allocatable :: chars(:,:)
  type(String)                  :: text
  character(len=:), allocatable :: buffer
  character(len=32)             :: arg
  real(dp)                      :: scale
  integer                       :: nrows, ncols, nbinary, r, k

  scale = 1.0_dp
  if ( command_argument_count() > 0 ) then
    call get_command_argument(1, arg)
    read(arg, *) scale
  end if

  ncols = 50
  nrows = max(10, nint(20000*scale))
  allocate( x(nrows, ncols), y(nrows, ncols), chars(nrows, ncols), cells(nrows, ncols) )
  call random_number(x); x = 2.0_dp*x - 1.0_dp

  nbinary = max(1000, nint(33554432*scale))                         ! 2**25 values, 256 MiB
  allocate( xb(nbinary) )
  call random_number(xb)

  rows(1) = result_row("Write a CSV file", "`to_file`", "formatted `write`, a row at a time")
  rows(2) = result_row("Read a CSV file", "`from_file`", "list-directed `read`, a row at a time")
  rows(3) = result_row("Write a raw binary file", "`to_file` (`.dat`)", "unformatted stream `write`")
  rows(4) = result_row("Read a raw binary file", "`from_file` (`.dat`)", "unformatted stream `read`")
  rows(5) = result_row("Write a NumPy file", "`to_file` (`.npy`, `.npz`)", "unformatted stream `write`")
  rows(6) = result_row("Read a NumPy file", "`from_file` (`.npy`, `.npz`)", "unformatted stream `read`")
  rows(7) = result_row("Numbers to text", "`cast` into `String`", "internal `write`")
  rows(8) = result_row("Text to numbers", "`cast` from `String`", "internal `read`")
  rows(9) = result_row("Split a CSV file into cells", "`read_file`", "`read` lines, split at commas")

  do r = 1, REPEATS
    call time_library(1, write_csv_library)
    call time_plain(1, write_csv_plain)
    call time_library(2, read_csv_library)
    call time_plain(2, read_csv_plain)
    call time_library(3, write_dat_library)
    call time_plain(3, write_stream_plain)
    call time_library(4, read_dat_library)
    call time_plain(4, read_stream_plain)
    call time_library(7, cast_to_text_library)
    call time_plain(7, cast_to_text_plain)
    call time_library(8, cast_to_numbers_library)
    call time_plain(8, cast_to_numbers_plain)
    call time_library(9, split_library)
    call time_plain(9, split_plain)
  end do
  rows(5)%t_plain = rows(3)%t_plain; rows(6)%t_plain = rows(4)%t_plain

  call report()

  ! The .npy and .npz rows go in a second table, since they compare two library formats with the same plain code.
  call npy_and_npz()

  ! Building a string a piece at a time: String%push against a buffer the caller grows.
  call building()

  do k = 1, 6
    call delete_file(trim(file_name(k)))
  end do

contains

  pure function file_name(k) result(name)
    integer, intent(in)  :: k
    character(len=32)    :: name

    select case (k)
      case (1); name = "benchmark_library.csv"
      case (2); name = "benchmark_plain.csv"
      case (3); name = "benchmark_library.dat"
      case (4); name = "benchmark_plain.dat"
      case (5); name = "benchmark_library.npy"
      case default; name = "benchmark_library.npz"
    end select
  end function file_name

  real(dp) function seconds()
    integer(int64) :: count, rate

    call system_clock(count, rate)
    seconds = real(count, dp)/real(rate, dp)
  end function seconds

  subroutine time_library(i, work)
    integer, intent(in) :: i
    procedure(no_arguments) :: work

    real(dp) :: t

    t = seconds(); call work(); rows(i)%t_library = min(rows(i)%t_library, seconds() - t)
  end subroutine time_library

  subroutine time_plain(i, work)
    integer, intent(in) :: i
    procedure(no_arguments) :: work

    real(dp) :: t

    t = seconds(); call work(); rows(i)%t_plain = min(rows(i)%t_plain, seconds() - t)
  end subroutine time_plain

  subroutine no_arguments()
  end subroutine no_arguments

  ! Library ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  subroutine write_csv_library()
    call to_file(x, file=trim(file_name(1)))
  end subroutine write_csv_library

  subroutine read_csv_library()
    call from_file(trim(file_name(1)), into=y)
    call check_equal("from_file")
  end subroutine read_csv_library

  subroutine write_dat_library()
    call to_file(xb, file=trim(file_name(3)))
  end subroutine write_dat_library

  subroutine read_dat_library()
    call from_file(trim(file_name(3)), into=yb, data_shape=shape(xb))
    call check_binary("from_file .dat")
  end subroutine read_dat_library

  subroutine cast_to_text_library()
    if ( allocated(cells) ) deallocate(cells)
    allocate( cells(nrows, ncols) )
    call cast(x, into=cells, fmt="e")
  end subroutine cast_to_text_library

  subroutine cast_to_numbers_library()
    if ( allocated(y) ) deallocate(y)
    allocate( y(nrows, ncols) )
    call cast(cells, into=y, fmt="e")
    call check_equal("cast")
  end subroutine cast_to_numbers_library

  subroutine split_library()
    call text%read_file(trim(file_name(1)), cell_array=cells)
    if ( any(shape(cells) /= shape(x)) ) error stop "read_file: wrong shape"
  end subroutine split_library

  ! Plain Fortran ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  subroutine write_csv_plain()
    integer :: u, i

    open(newunit=u, file=trim(file_name(2)), status="replace", action="write")
    do i = 1, nrows
      write(u, ROW_FMT) x(i,:)
    end do
    close(u)
  end subroutine write_csv_plain

  subroutine read_csv_plain()
    integer :: u, i

    if ( allocated(y) ) deallocate(y)
    allocate( y(nrows, ncols) )
    open(newunit=u, file=trim(file_name(2)), status="old", action="read")
    do i = 1, nrows
      read(u, *) y(i,:)
    end do
    close(u)
    call check_equal("list-directed read")
  end subroutine read_csv_plain

  subroutine write_stream_plain()
    integer :: u

    open(newunit=u, file=trim(file_name(4)), status="replace", action="write", access="stream", form="unformatted")
    write(u) xb
    close(u)
  end subroutine write_stream_plain

  subroutine read_stream_plain()
    integer :: u

    if ( allocated(yb) ) deallocate(yb)
    allocate( yb(nbinary) )
    open(newunit=u, file=trim(file_name(4)), status="old", action="read", access="stream", form="unformatted")
    read(u) yb
    close(u)
    call check_binary("stream read")
  end subroutine read_stream_plain

  subroutine cast_to_text_plain()
    integer :: i, j

    do j = 1, ncols
      do i = 1, nrows
        write(chars(i,j), REAL_FMT) x(i,j)
      end do
    end do
  end subroutine cast_to_text_plain

  subroutine cast_to_numbers_plain()
    integer :: i, j

    if ( allocated(y) ) deallocate(y)
    allocate( y(nrows, ncols) )
    do j = 1, ncols
      do i = 1, nrows
        read(chars(i,j), REAL_FMT) y(i,j)
      end do
    end do
    call check_equal("internal read")
  end subroutine cast_to_numbers_plain

  subroutine split_plain()
    !! Reads each line into a buffer long enough for it and copies the text between commas into a cell.
    character(len=:), allocatable :: line
    integer                       :: u, i, j, start, comma

    allocate( character(len=27*ncols) :: line )
    open(newunit=u, file=trim(file_name(2)), status="old", action="read")
    do i = 1, nrows
      read(u, "(a)") line
      start = 1
      do j = 1, ncols
        comma = index(line(start:), ",")
        if ( comma == 0 ) then
          chars(i,j) = line(start:)
        else
          chars(i,j) = line(start:start+comma-2)
          start = start + comma
        end if
      end do
    end do
    close(u)
  end subroutine split_plain

  subroutine check_equal(what)
    !! Stops if the values read back differ, so a fast but wrong path can't go unnoticed.
    character(len=*), intent(in) :: what

    if ( .not. allocated(y) ) error stop what//": nothing read"
    if ( any(shape(y) /= shape(x)) ) error stop what//": wrong shape"
    if ( any(y /= x) ) error stop what//": values differ"
  end subroutine check_equal

  subroutine check_binary(what)
    character(len=*), intent(in) :: what

    if ( .not. allocated(yb) ) error stop what//": nothing read"
    if ( size(yb) /= size(xb) ) error stop what//": wrong size"
    if ( any(yb /= xb) ) error stop what//": values differ"
  end subroutine check_binary

  ! Output ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  subroutine report()
    integer :: i

    write(*, "(a)") "Compiler: "//compiler_version()
    write(*, "(a)") "Options:  "//compiler_options()
    write(*, "(a,i0,a,i0,a,i0,a)") "Text:     ", nrows, " x ", ncols, " real64 values (", nrows*ncols, ")"
    write(*, "(a,i0,a,i0,a)") "Binary:   ", nbinary, " real64 values (", nbinary/131072, " MiB)"
    write(*, "(a,i0,a)") "Times are the best of ", REPEATS, " runs, in milliseconds."
    write(*, "(a)") ""
    write(*, "(a)") "| Operation | Library | Plain Fortran | Library | Plain | Ratio |"
    write(*, "(a)") "|---|---|---|--:|--:|--:|"
    do i = 1, size(rows)
      if ( (i == 5) .or. (i == 6) ) cycle
      write(*, "(a)") "| "//rows(i)%operation//" | "//rows(i)%library//" | "//rows(i)%plain//" | "// &
                      ms(rows(i)%t_library)//" | "//ms(rows(i)%t_plain)//" | "// &
                      fixed(rows(i)%t_library/rows(i)%t_plain, 1)//" |"
    end do
  end subroutine report

  subroutine npy_and_npz()
    !! Writing and reading the same array as .npy and as one array of a .npz archive, against a raw stream.
    real(dp) :: t, best(4)
    integer  :: i

    best = huge(1.0_dp)
    do i = 1, REPEATS
      call delete_file(trim(file_name(6)))
      t = seconds(); call to_file(xb, file=trim(file_name(5))); best(1) = min(best(1), seconds() - t)
      t = seconds(); call from_file(trim(file_name(5)), into=yb); best(2) = min(best(2), seconds() - t)
      call check_binary(".npy")
      t = seconds(); call to_file(xb, file=trim(file_name(6)), name="x"); best(3) = min(best(3), seconds() - t)
      t = seconds(); call from_file(trim(file_name(6)), into=yb, name="x"); best(4) = min(best(4), seconds() - t)
      call check_binary(".npz")
    end do

    write(*, "(a)") ""
    write(*, "(a)") "| NumPy file | Write | Read | Write ratio | Read ratio |"
    write(*, "(a)") "|---|--:|--:|--:|--:|"
    write(*, "(a)") "| Raw stream (plain Fortran) | "//ms(rows(3)%t_plain)//" | "//ms(rows(4)%t_plain)//" | 1.0 | 1.0 |"
    write(*, "(a)") "| `.npy` | "//ms(best(1))//" | "//ms(best(2))//" | "// &
                    fixed(best(1)/rows(3)%t_plain, 1)//" | "//fixed(best(2)/rows(4)%t_plain, 1)//" |"
    write(*, "(a)") "| `.npz` | "//ms(best(3))//" | "//ms(best(4))//" | "// &
                    fixed(best(3)/rows(3)%t_plain, 1)//" | "//fixed(best(4)/rows(4)%t_plain, 1)//" |"
  end subroutine npy_and_npz

  subroutine building()
    !! Appends every number's text, with a comma, to one string.
    real(dp) :: t, best(2)
    integer  :: i, j, n, pos, rep

    best = huge(1.0_dp)
    do rep = 1, REPEATS
      t = seconds()
      call text%empty()
      do j = 1, ncols
        do i = 1, nrows
          call text%push(chars(i,j)//",")
        end do
      end do
      best(1) = min(best(1), seconds() - t)

      t = seconds()
      allocate( character(len=1024) :: buffer )
      pos = 0
      do j = 1, ncols
        do i = 1, nrows
          n = len(chars(i,j)) + 1
          if ( pos + n > len(buffer) ) call grow(buffer, pos)
          buffer(pos+1:pos+n) = chars(i,j)//","
          pos = pos + n
        end do
      end do
      best(2) = min(best(2), seconds() - t)

      ! Compared in place: buffer = buffer(:pos), or text%as_str(), would copy it, on the stack for ifx.
      if ( text /= buffer(:pos) ) error stop "push: the strings differ"
      deallocate( buffer )
    end do

    write(*, "(a)") ""
    write(*, "(a)") "| Building a string | Library | Plain | Ratio |"
    write(*, "(a)") "|---|--:|--:|--:|"
    write(*, "(a)") "| `String%push` against a buffer that doubles | "//ms(best(1))//" | "//ms(best(2))//" | "// &
                    fixed(best(1)/best(2), 1)//" |"
  end subroutine building

  subroutine grow(buffer, used)
    character(len=:), allocatable, intent(inout) :: buffer
    integer,                       intent(in)    :: used

    character(len=:), allocatable :: bigger

    allocate( character(len=2*len(buffer)) :: bigger )
    bigger(:used) = buffer(:used)
    call move_alloc(bigger, buffer)
  end subroutine grow

  function ms(seconds) result(text)
    real(dp), intent(in)          :: seconds
    character(len=:), allocatable :: text

    text = fixed(1000*seconds, 1)
  end function ms

  function fixed(value, decimals) result(text)
    real(dp), intent(in)           :: value
    integer,  intent(in), optional :: decimals
    character(len=:), allocatable  :: text

    character(len=32) :: buffer
    integer           :: d

    d = 3
    if ( present(decimals) ) d = decimals
    write(buffer, "(f0."//achar(iachar("0")+d)//")") value
    text = trim(adjustl(buffer))
    if ( text(1:1) == "." ) text = "0"//text
  end function fixed

  subroutine delete_file(name)
    character(len=*), intent(in) :: name

    integer :: u, st
    logical :: exists

    inquire(file=name, exist=exists)
    if ( .not. exists ) return
    open(newunit=u, file=name, iostat=st)
    if ( st == 0 ) close(u, status="delete", iostat=st)
  end subroutine delete_file
end program benchmark_compare
