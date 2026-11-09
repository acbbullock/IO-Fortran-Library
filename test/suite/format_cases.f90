module format_cases
  !---------------------------------------------------------------------------------------------------------------------
  !! The `format` argument and file extensions.
  !---------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: real64
  use io_fortran_lib, only: String, echo, to_file, from_file, IOFL_TAB, operator(==), operator(/=)
  use test_support, only: test_suite, STOPS, check, expect_equal, scratch
  implicit none
  private

  public :: register_format

contains

  subroutine register_format(suite)
    type(test_suite), intent(inout) :: suite

    call suite%group("format")
    call suite%add("to_file_bad_extension", to_file_bad_extension, expect=STOPS)
    call suite%add("write_file_bad_extension", write_file_bad_extension, expect=STOPS)
    call suite%add("echo_bad_extension", echo_bad_extension, expect=STOPS)
    call suite%add("format_text_any_extension", format_text_any_extension)
    call suite%add("format_binary_any_extension", format_binary_any_extension)
    call suite%add("format_overrides_extension", format_overrides_extension)
    call suite%add("format_invalid_stat", format_invalid_stat)
    call suite%add("format_string_methods", format_string_methods)
    call suite%add("extension_any_case", extension_any_case)
    call suite%add("format_invalid_stops", format_invalid_stops, expect=STOPS)
  end subroutine register_format

  subroutine to_file_bad_extension()
    character(len=:), allocatable :: file

    file = scratch()

    call to_file([1.0_real64], file=file//".tsv")
  end subroutine to_file_bad_extension

  subroutine write_file_bad_extension()
    character(len=:), allocatable :: file
    type(String)                  :: s, cell(2,2)

    file = scratch()

    cell = String("a")
    call s%write_file(cell, file=file//".tsv")
  end subroutine write_file_bad_extension

  subroutine echo_bad_extension()
    character(len=:), allocatable :: file

    file = scratch()

    call echo("text", file=file//".tsv")
  end subroutine echo_bad_extension

  subroutine format_text_any_extension()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    real(real64), allocatable     :: v(:)

    file = scratch(); st = 0; msg = ""

    call to_file([1.5_real64,2.5_real64], file=file//".tsv", format="text", stat=st, errmsg=msg)
    call expect_equal(st, 0, "The write should succeed.")
    call from_file(file//".tsv", into=v, format="text", stat=st, errmsg=msg)
    call check((st == 0) .and. all(v == [1.5_real64,2.5_real64]), "The data should survive a round trip.")
    call to_file([1.5_real64], file=file//"_no_extension", format="text", stat=st, errmsg=msg)
    call expect_equal(st, 0, "No extension is needed with format.")
  end subroutine format_text_any_extension

  subroutine format_binary_any_extension()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    real(real64), allocatable     :: v(:)

    file = scratch(); st = 0; msg = ""

    call to_file([1.5_real64,2.5_real64], file=file//".xyz", format="binary", stat=st, errmsg=msg)
    call expect_equal(st, 0, "The write should succeed.")
    call from_file(file//".xyz", into=v, data_shape=[2], format="binary", stat=st, errmsg=msg)
    call check((st == 0) .and. all(v == [1.5_real64,2.5_real64]), "The data should survive a round trip.")
  end subroutine format_binary_any_extension

  subroutine format_overrides_extension()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st, i

    file = scratch(); st = 0; msg = ""

    call to_file([1.5_real64,2.5_real64], file=file//".csv", format="binary", stat=st, errmsg=msg)
    inquire(file=file//".csv", size=i)
    call check((st == 0) .and. (i == 16), "A binary format should write binary data whatever the extension.")
  end subroutine format_overrides_extension

  subroutine format_invalid_stat()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    real(real64), allocatable     :: v(:)

    file = scratch(); st = 0; msg = ""

    call to_file([1.5_real64], file=file//".csv", format="xml", stat=st, errmsg=msg)
    call check(st /= 0, "An invalid format should be reported through stat.")
    call from_file(file//".csv", into=v, format="xml", stat=st, errmsg=msg)
    call check(st /= 0, "An invalid format should be reported through stat.")
  end subroutine format_invalid_stat

  subroutine format_string_methods()
    character(len=:), allocatable :: file
    type(String)                  :: s, one_row(1,2)
    type(String), allocatable     :: cells(:,:)
    character(len=512)            :: msg
    integer                       :: st

    file = scratch(); st = 0; msg = ""

    one_row(1,1) = String("a"); one_row(1,2) = String("b")
    call s%write_file(one_row, file=file//".tsv", column_separator=IOFL_TAB, format="text")
    call s%read_file(file//".tsv", cell_array=cells, column_separator=IOFL_TAB, format="text")
    call expect_equal(cells(1,2), "b", "read_file and write_file should accept format.")
    call echo("line", file=file//".json", format="text", stat=st, errmsg=msg)
    call expect_equal(st, 0, "echo should accept format.")
    call s%read_file(file//".csv", format="binary", stat=st, errmsg=msg)
    call check(st /= 0, "A binary format is not valid for read_file.")
  end subroutine format_string_methods

  subroutine extension_any_case()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st, i
    real(real64), allocatable     :: v(:)

    file = scratch(); st = 0; msg = ""

    call to_file([1.5_real64], file=file//".CSV", stat=st, errmsg=msg)
    call expect_equal(st, 0, "An uppercase extension should work.")
    call from_file(file//".CSV", into=v, stat=st, errmsg=msg)
    call check((st == 0) .and. (v(1) == 1.5_real64), "In both directions.")
    call to_file([1.5_real64,2.5_real64], file=file//".Dat", stat=st, errmsg=msg)
    inquire(file=file//".Dat", size=i)
    call check((st == 0) .and. (i == 16), "A mixed case binary extension should write binary.")
  end subroutine extension_any_case

  subroutine format_invalid_stops()
    character(len=:), allocatable :: file

    file = scratch()

    call to_file([1.5_real64], file=file//".csv", format="xml")
  end subroutine format_invalid_stops
end module format_cases
