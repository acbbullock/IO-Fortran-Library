module argument_errors_cases
  !---------------------------------------------------------------------------------------------------------------------
  !! Errors that coverage showed no case reached: arguments `to_file` and `from_file` reject, and files that can't be
  !! opened. Each check is written once in a template and repeated for every kind, so one kind per check is enough.
  !---------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: real64, int32
  use io_fortran_lib, only: String, to_file, from_file, echo, IOFL_ARG_ERR, IOFL_READ_ERR, IOFL_WRITE_ERR
  use test_support, only: test_suite, check, expect_equal, scratch, write_raw
  implicit none (type, external)
  private

  public :: register_argument_errors

  character(len=*), parameter :: NO_DIR = "test/output/no_such_directory/"   ! Never created, so opening fails

contains

  subroutine register_argument_errors(suite)
    type(test_suite), intent(inout) :: suite

    call suite%group("argument_errors")
    call suite%add("text_rank_stat", text_rank_stat)
    call suite%add("locale_stat", locale_stat)
    call suite%add("delim_is_decimal_mark_stat", delim_is_decimal_mark_stat)
    call suite%add("integer_fmt_stat", integer_fmt_stat)
    call suite%add("rank1_header_size_stat", rank1_header_size_stat)
    call suite%add("header_only_line_stat", header_only_line_stat)
    call suite%add("binary_missing_stat", binary_missing_stat)
    call suite%add("missing_directory_stat", missing_directory_stat)
  end subroutine register_argument_errors

  subroutine text_rank_stat()
    !! Text files hold arrays of rank 1 or 2.
    real(real64),     allocatable :: x(:,:,:)
    integer(int32),   allocatable :: i(:,:,:)
    complex(real64),  allocatable :: z(:,:,:)
    logical,          allocatable :: b(:,:,:)
    character(len=512)            :: msg
    integer                       :: st

    msg = ""
    call to_file(reshape([1.0_real64, 2.0_real64], [1,1,2]), file=scratch()//".csv", stat=st, errmsg=msg)
    call expect_equal(st, IOFL_ARG_ERR, "Writing a rank 3 real array to a text file.")
    call check(index(msg, "rank 3") > 0, "The message names the rank: "//trim(msg))
    call to_file(reshape([1_int32, 2_int32], [1,1,2]), file=scratch()//".csv", stat=st)
    call expect_equal(st, IOFL_ARG_ERR, "Writing a rank 3 integer array to a text file.")

    call write_raw(scratch()//"_in.csv", "1,2"//achar(10))
    call from_file(scratch()//"_in.csv", into=x, stat=st, errmsg=msg)
    call expect_equal(st, IOFL_ARG_ERR, "Reading a text file into a rank 3 real array.")
    call check(index(msg, "1-2") > 0, "The message gives the ranks allowed: "//trim(msg))
    call from_file(scratch()//"_in.csv", into=i, stat=st)
    call expect_equal(st, IOFL_ARG_ERR, "Reading a text file into a rank 3 integer array.")
    call from_file(scratch()//"_in.csv", into=z, stat=st)
    call expect_equal(st, IOFL_ARG_ERR, "Reading a text file into a rank 3 complex array.")
    call write_raw(scratch()//"_tf.csv", "T,F"//achar(10))
    call from_file(scratch()//"_tf.csv", into=b, stat=st)
    call expect_equal(st, IOFL_ARG_ERR, "Reading a text file into a rank 3 logical array.")
  end subroutine text_rank_stat

  subroutine locale_stat()
    real(real64), allocatable :: x(:)
    character(len=512)        :: msg
    integer                   :: st

    msg = ""
    call to_file([1.0_real64], file=scratch()//".csv", locale="FR", stat=st, errmsg=msg)
    call expect_equal(st, IOFL_ARG_ERR, "Writing with an unknown locale.")
    call check(index(msg, '"FR"') > 0, "The message names the locale: "//trim(msg))
    call to_file([(1.0_real64, 2.0_real64)], file=scratch()//".csv", locale="FR", stat=st)
    call expect_equal(st, IOFL_ARG_ERR, "Writing complex data with an unknown locale.")

    call write_raw(scratch()//"_in.csv", "1.0"//achar(10))
    call from_file(scratch()//"_in.csv", into=x, locale="FR", stat=st)
    call expect_equal(st, IOFL_ARG_ERR, "Reading with an unknown locale.")
  end subroutine locale_stat

  subroutine delim_is_decimal_mark_stat()
    !! The delimiter can't be the locale's decimal mark, or numbers couldn't be told apart.
    real(real64), allocatable :: x(:)
    integer                   :: st

    call write_raw(scratch()//".csv", "1.0"//achar(10))
    call from_file(scratch()//".csv", into=x, delim=".", stat=st)
    call expect_equal(st, IOFL_ARG_ERR, 'delim="." with the US locale.')
    call from_file(scratch()//".csv", into=x, locale="EU", delim=",", stat=st)
    call expect_equal(st, IOFL_ARG_ERR, 'delim="," with the EU locale.')
  end subroutine delim_is_decimal_mark_stat

  subroutine integer_fmt_stat()
    character(len=512) :: msg
    integer            :: st

    msg = ""
    call to_file([1_int32, 2_int32], file=scratch()//".csv", fmt="e", stat=st, errmsg=msg)
    call expect_equal(st, IOFL_ARG_ERR, 'fmt="e" for integer data.')
    call check(index(msg, '"e"') > 0, "The message names the format: "//trim(msg))
  end subroutine integer_fmt_stat

  subroutine rank1_header_size_stat()
    !! A rank 1 array is written as one column, so its header has one name.
    integer :: st

    call to_file([1.0_real64, 2.0_real64], file=scratch()//".csv", header=["a", "b"], stat=st)
    call expect_equal(st, IOFL_ARG_ERR, "Two header names for one column.")
    call to_file([1.0_real64, 2.0_real64], file=scratch()//".csv", header=["a"], stat=st)
    call expect_equal(st, 0, "One header name.")
  end subroutine rank1_header_size_stat

  subroutine header_only_line_stat()
    !! A file of one line, read with a header, has no data.
    real(real64), allocatable :: x(:)
    logical,      allocatable :: b(:)
    type(String), allocatable :: s(:)
    character(len=512)        :: msg
    integer                   :: st

    call write_raw(scratch()//".csv", "x"//achar(10))
    msg = ""
    call from_file(scratch()//".csv", into=x, header=.true., stat=st, errmsg=msg)
    call expect_equal(st, IOFL_ARG_ERR, "Real data.")
    call check(index(msg, "one line") > 0, "The message says so: "//trim(msg))
    call from_file(scratch()//".csv", into=b, header=.true., stat=st)
    call expect_equal(st, IOFL_ARG_ERR, "Logical data.")
    call from_file(scratch()//".csv", into=s, header=.true., stat=st)
    call expect_equal(st, IOFL_ARG_ERR, "String data.")
  end subroutine header_only_line_stat

  subroutine binary_missing_stat()
    real(real64), allocatable :: x(:)
    character(len=512)        :: msg
    integer                   :: st

    msg = ""
    call from_file(scratch()//"_missing.dat", into=x, data_shape=[2], stat=st, errmsg=msg)
    call expect_equal(st, IOFL_READ_ERR, "A missing raw binary file.")
    call check(index(msg, "No such file") > 0, "The message says so: "//trim(msg))
  end subroutine binary_missing_stat

  subroutine missing_directory_stat()
    !! A file that can't be opened for writing gives IOFL_WRITE_ERR, whatever the format.
    type(String) :: table
    integer      :: st

    call to_file([1.0_real64], file=NO_DIR//"x.csv", stat=st)
    call expect_equal(st, IOFL_WRITE_ERR, "A text file.")
    call to_file([1.0_real64], file=NO_DIR//"x.dat", stat=st)
    call expect_equal(st, IOFL_WRITE_ERR, "A raw binary file.")
    call to_file([1.0_real64], file=NO_DIR//"x.npy", stat=st)
    call expect_equal(st, IOFL_WRITE_ERR, "A .npy file.")
    call to_file([1.0_real64], file=NO_DIR//"x.npz", name="x", stat=st)
    call expect_equal(st, IOFL_WRITE_ERR, "A .npz archive.")
    call to_file([.true.], file=NO_DIR//"x.csv", stat=st)
    call expect_equal(st, IOFL_WRITE_ERR, "A text file of logical data.")
    call table%write_file(reshape([String("a")], [1,1]), file=NO_DIR//"x.csv", stat=st)
    call expect_equal(st, IOFL_WRITE_ERR, "write_file.")
    call echo("a", file=NO_DIR//"x.txt", stat=st)
    call expect_equal(st, IOFL_WRITE_ERR, "echo.")
  end subroutine missing_directory_stat
end module argument_errors_cases
