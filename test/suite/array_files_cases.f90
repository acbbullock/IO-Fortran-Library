module array_files_cases
  !---------------------------------------------------------------------------------------------------------------------
  !! `to_file` and `from_file` with text files: errors, empty cells, quoting, and round trips.
  !---------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: real32, real64
  use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan
  use io_fortran_lib, only: String, str, to_file, from_file, LF, CR, operator(==), operator(/=), operator(>)
  use test_support, only: test_suite, STOPS, check, expect_equal, scratch, write_raw
  implicit none (type, external)
  private

  public :: register_array_files

contains

  subroutine register_array_files(suite)
    type(test_suite), intent(inout) :: suite

    call suite%group("array_files")
    call suite%add("from_file_missing", from_file_missing, expect=STOPS)
    call suite%add("from_file_bad_number_stat", from_file_bad_number_stat)
    call suite%add("from_file_bad_number", from_file_bad_number, expect=STOPS)
    call suite%add("from_file_bad_number_location", from_file_bad_number_location)
    call suite%add("from_file_bad_cell_int", from_file_bad_cell_int)
    call suite%add("from_file_quoted_numbers", from_file_quoted_numbers)
    call suite%add("from_file_crlf", from_file_crlf)
    call suite%add("from_file_no_final_newline", from_file_no_final_newline)
    call suite%add("from_file_empty_cell", from_file_empty_cell)
    call suite%add("from_file_empty_cell_header", from_file_empty_cell_header)
    call suite%add("from_file_empty_cell_hex", from_file_empty_cell_hex)
    call suite%add("from_file_empty_cell_complex", from_file_empty_cell_complex)
    call suite%add("from_file_empty_cell_integer", from_file_empty_cell_integer)
    call suite%add("from_file_pairs_no_final_newline", from_file_pairs_no_final_newline)
    call suite%add("from_file_pairs_crlf", from_file_pairs_crlf)
    call suite%add("from_file_pairs_wider_row", from_file_pairs_wider_row)
    call suite%add("from_file_pairs_not_closed", from_file_pairs_not_closed)
    call suite%add("from_file_pairs_header_quoted", from_file_pairs_header_quoted)
    call suite%add("from_file_uneven_rows", from_file_uneven_rows)
    call suite%add("to_file_complex_round_trip", to_file_complex_round_trip)
    call suite%add("eu_zero_file_round_trip", eu_zero_file_round_trip)
    call suite%add("us_zero_file_round_trip", us_zero_file_round_trip)
    call suite%add("nan_file_round_trip", nan_file_round_trip)
  end subroutine register_array_files

  subroutine from_file_missing()
    character(len=:), allocatable :: file
    real(real64), allocatable     :: v(:)

    file = scratch()

    call from_file(file//"_missing.csv", into=v)
  end subroutine from_file_missing

  subroutine from_file_bad_number_stat()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    real(real64), allocatable     :: v(:)

    file = scratch(); st = 0; msg = ""

    call write_raw(file//".csv", "1.0"//LF//"oops"//LF)
    call from_file(file//".csv", into=v, stat=st, errmsg=msg)
    call check(st /= 0, "A bad number should be reported through stat.")
  end subroutine from_file_bad_number_stat

  subroutine from_file_bad_number()
    character(len=:), allocatable :: file
    real(real64), allocatable     :: v(:)

    file = scratch()

    call write_raw(file//".csv", "1.0"//LF//"oops"//LF)
    call from_file(file//".csv", into=v)
  end subroutine from_file_bad_number

  subroutine from_file_bad_number_location()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    real(real64), allocatable     :: m2(:,:)

    file = scratch(); st = 0; msg = ""

    call write_raw(file//".csv", "1,2"//LF//"3,oops"//LF)
    call from_file(file//".csv", into=m2, stat=st, errmsg=msg)
    call check(st /= 0, "A bad number should be reported through stat.")
    call check(index(msg, "oops") > 0, "The message should name the text.")
    call check(index(msg, "row 2, column 2") > 0, "The message should name the location.")
  end subroutine from_file_bad_number_location

  subroutine from_file_bad_cell_int()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    integer, allocatable          :: vi(:)

    file = scratch(); st = 0; msg = ""

    call write_raw(file//".csv", "1"//LF//"2.5"//LF)
    call from_file(file//".csv", into=vi, stat=st, errmsg=msg)
    call check(st /= 0, "A bad integer should be reported through stat.")
  end subroutine from_file_bad_cell_int

  subroutine from_file_quoted_numbers()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    real(real64), allocatable     :: m2(:,:)

    file = scratch(); st = 0; msg = ""

    call write_raw(file//".csv", '"1.5","2.5"'//LF)
    call from_file(file//".csv", into=m2, stat=st, errmsg=msg)
    call expect_equal(st, 0, "Quoted numbers should be read.")
    call check(all(m2 == reshape([1.5_real64,2.5_real64], [1,2])), "The values should be right.")
  end subroutine from_file_quoted_numbers

  subroutine from_file_crlf()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    real(real64), allocatable     :: m2(:,:)

    file = scratch(); st = 0; msg = ""

    call write_raw(file//".csv", "1.5,2.5"//CR//LF//"3.5,4.5"//CR//LF)
    call from_file(file//".csv", into=m2, stat=st, errmsg=msg)
    call expect_equal(st, 0, "CRLF should be read.")
    call check(m2(2,2) == 4.5_real64, "The values should be right.")
  end subroutine from_file_crlf

  subroutine from_file_no_final_newline()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    integer, allocatable          :: vi(:)

    file = scratch(); st = 0; msg = ""

    call write_raw(file//".csv", "1"//LF//"2"//LF//"3")
    call from_file(file//".csv", into=vi, stat=st, errmsg=msg)
    call check((st == 0) .and. (size(vi) == 3), "The last row should be read.")
    call expect_equal(vi(3), 3, "The value should be right.")
  end subroutine from_file_no_final_newline

  subroutine from_file_empty_cell()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    real(real64), allocatable     :: m2(:,:)

    file = scratch(); st = 0; msg = ""

    call write_raw(file//".csv", "1,2"//LF//"3,"//LF//", "//LF)
    call from_file(file//".csv", into=m2, stat=st, errmsg=msg)
    call expect_equal(st, 0, "An empty cell should not be an error.")
    call check(m2(1,2) == 2.0_real64, "Other cells should be read.")
    call check(ieee_is_nan(m2(2,2)), "An empty cell should read as NaN.")
    call check(ieee_is_nan(m2(3,1)) .and. ieee_is_nan(m2(3,2)), "A blank cell should read as NaN.")
  end subroutine from_file_empty_cell

  subroutine from_file_empty_cell_header()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    real(real64), allocatable     :: m2(:,:)

    file = scratch(); st = 0; msg = ""

    call write_raw(file//".csv", "a,b"//LF//"1,"//LF)
    call from_file(file//".csv", into=m2, header=.true., stat=st, errmsg=msg)
    call expect_equal(st, 0, "An empty cell should not be an error.")
    call check(all(shape(m2) == [1,2]), "The header should not count as data.")
    call check((m2(1,1) == 1.0_real64) .and. ieee_is_nan(m2(1,2)), "The NaN should land in the right cell.")
  end subroutine from_file_empty_cell_header

  subroutine from_file_empty_cell_hex()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    real(real64), allocatable     :: m2(:,:)

    file = scratch(); st = 0; msg = ""

    call write_raw(file//".csv", "0x3ff8000000000000,"//LF)
    call from_file(file//".csv", into=m2, fmt="z", stat=st, errmsg=msg)
    call expect_equal(st, 0, "An empty cell should not be an error.")
    call check((m2(1,1) == 1.5_real64) .and. ieee_is_nan(m2(1,2)), "It should read as NaN.")
  end subroutine from_file_empty_cell_hex

  subroutine from_file_empty_cell_complex()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    complex(real32), allocatable  :: mz(:,:)

    file = scratch(); st = 0; msg = ""

    call write_raw(file//".csv", "(1.5,2.5),"//LF)
    call from_file(file//".csv", into=mz, stat=st, errmsg=msg)
    call expect_equal(st, 0, "An empty cell should not be an error.")
    call check(mz(1,1) == (1.5,2.5), "The ordered pair should be read.")
    call check(ieee_is_nan(mz(1,2)%re) .and. ieee_is_nan(mz(1,2)%im), "Both parts should be NaN.")
  end subroutine from_file_empty_cell_complex

  subroutine from_file_empty_cell_integer()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    integer, allocatable          :: vi(:)

    file = scratch(); st = 0; msg = ""

    call write_raw(file//".csv", "1"//LF//LF//"3"//LF)
    call from_file(file//".csv", into=vi, stat=st, errmsg=msg)
    call check(st /= 0, "An integer has no NaN, so an empty cell is an error.")
    call check(index(msg, "row 2, column 1") > 0, "The message should name the location.")
  end subroutine from_file_empty_cell_integer

  subroutine from_file_pairs_no_final_newline()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    complex(real32), allocatable  :: mz(:,:)

    file = scratch(); st = 0; msg = ""

    call write_raw(file//".csv", "(1.5,2.5),(3.5,4.5)"//LF//"(5.5,6.5),(7.5,8.5)")
    call from_file(file//".csv", into=mz, stat=st, errmsg=msg)
    call check((st == 0) .and. all(shape(mz) == [2,2]), "The last row should be read.")
    call check(mz(2,2) == (7.5,8.5), "The value should be right.")
  end subroutine from_file_pairs_no_final_newline

  subroutine from_file_pairs_crlf()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    complex(real32), allocatable  :: mz(:,:)

    file = scratch(); st = 0; msg = ""

    call write_raw(file//".csv", "(1.5,2.5),(3.5,4.5)"//CR//LF)
    call from_file(file//".csv", into=mz, stat=st, errmsg=msg)
    call check((st == 0) .and. all(shape(mz) == [1,2]), "CRLF should be read.")
    call check(mz(1,2) == (3.5,4.5), "The value should be right.")
  end subroutine from_file_pairs_crlf

  subroutine from_file_pairs_wider_row()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    complex(real32), allocatable  :: mz(:,:)

    file = scratch(); st = 0; msg = ""

    call write_raw(file//".csv", "(1,2)"//LF//"(3,4),(5,6)"//LF)
    call from_file(file//".csv", into=mz, stat=st, errmsg=msg)
    call check(st /= 0, "A wider row should be reported through stat.")
  end subroutine from_file_pairs_wider_row

  subroutine from_file_pairs_not_closed()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    complex(real32), allocatable  :: mz(:,:)

    file = scratch(); st = 0; msg = ""

    call write_raw(file//".csv", "(1,2"//LF)
    call from_file(file//".csv", into=mz, stat=st, errmsg=msg)
    call check(st /= 0, "A bad ordered pair should be reported through stat.")
  end subroutine from_file_pairs_not_closed

  subroutine from_file_pairs_header_quoted()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    complex(real32), allocatable  :: mz(:,:)

    file = scratch(); st = 0; msg = ""

    call to_file(reshape([(1.5,2.5),(3.5,4.5)], [1,2]), file=file//".csv", header=["a,b","c d"], stat=st, &
                 errmsg=msg)
    call expect_equal(st, 0, "The write should succeed.")
    call from_file(file//".csv", into=mz, header=.true., stat=st, errmsg=msg)
    call check((st == 0) .and. all(shape(mz) == [1,2]), "A quoted header should not shift the columns.")
    call check(mz(1,2) == (3.5,4.5), "The value should be right.")
  end subroutine from_file_pairs_header_quoted

  subroutine from_file_uneven_rows()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    real(real64), allocatable     :: m2(:,:)

    file = scratch(); st = 0; msg = ""

    call write_raw(file//".csv", "1,2"//LF//"3"//LF)
    call from_file(file//".csv", into=m2, stat=st, errmsg=msg)
    call expect_equal(st, 0, "A missing cell is an empty cell.")
    call check((m2(2,1) == 3.0_real64) .and. ieee_is_nan(m2(2,2)), "It should read as NaN.")
  end subroutine from_file_uneven_rows

  subroutine to_file_complex_round_trip()
    character(len=:), allocatable :: file
    type(String)                  :: s
    character(len=512)            :: msg
    integer                       :: st
    complex(real32)               :: zv(2)
    complex(real32), allocatable  :: zw(:)

    file = scratch(); st = 0; msg = ""

    zv = [(1.5,-2.5),(0.0,1.0)]
    call to_file(zv, file=file//".csv", stat=st, errmsg=msg)
    call expect_equal(st, 0, "The write should succeed.")
    call s%read_file(file//".csv")
    call check(s%as_str() == str(zv(1))//LF//str(zv(2))//LF, "Ordered pairs should not be quoted in a file.")
    call from_file(file//".csv", into=zw, stat=st, errmsg=msg)
    call expect_equal(st, 0, "The read should succeed.")
    call check(all(zw == zv), "The default complex form should survive a round trip.")
  end subroutine to_file_complex_round_trip

  subroutine eu_zero_file_round_trip()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    real(real32), allocatable     :: v32(:)

    file = scratch(); st = 0; msg = ""

    call to_file([1.5,0.0,2.5], file=file//".csv", locale="EU", stat=st, errmsg=msg)
    call expect_equal(st, 0, "The write should succeed.")
    call from_file(file//".csv", into=v32, locale="EU", stat=st, errmsg=msg)
    call expect_equal(st, 0, "The read should succeed.")
    call check(all(v32 == [1.5,0.0,2.5]), "The data should survive a round trip.")
  end subroutine eu_zero_file_round_trip

  subroutine us_zero_file_round_trip()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    real(real32), allocatable     :: v32(:)

    file = scratch(); st = 0; msg = ""

    call to_file([1.5,0.0,2.5], file=file//".csv", locale="US", stat=st, errmsg=msg)
    call expect_equal(st, 0, "The write should succeed.")
    call from_file(file//".csv", into=v32, locale="US", stat=st, errmsg=msg)
    call expect_equal(st, 0, "The read should succeed.")
    call check(all(v32 == [1.5,0.0,2.5]), "The data should survive a round trip.")
  end subroutine us_zero_file_round_trip

  subroutine nan_file_round_trip()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    real(real64)                  :: x
    real(real64), allocatable     :: v(:)

    file = scratch(); st = 0; msg = ""

    x = ieee_value(x, ieee_quiet_nan)
    call to_file([1.0_real64,x], file=file//".csv", stat=st, errmsg=msg)
    call expect_equal(st, 0, "The write should succeed.")
    call from_file(file//".csv", into=v, stat=st, errmsg=msg)
    call expect_equal(st, 0, "The read should succeed.")
    call check((v(1) == 1.0_real64) .and. ieee_is_nan(v(2)), "NaN should survive a round trip.")
  end subroutine nan_file_round_trip
end module array_files_cases
