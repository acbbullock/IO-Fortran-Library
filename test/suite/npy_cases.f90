#include "iofl_config.inc"
module npy_cases
  !---------------------------------------------------------------------------------------------------------------------
  !! NumPy `.npy` files: the header this library writes, files in the layouts NumPy writes (C order, big-endian,
  !! version 2.0), and the errors for files that don't match the output array. Round trips of every kind and rank are
  !! in `file_round_trip`.
  !---------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: real64, int8, int16, int32
  use io_fortran_lib, only: to_file, from_file, IOFL_LF, IOFL_READ_ERR, IOFL_ARG_ERR
  use test_support, only: test_suite, STOPS, check, expect_equal, expect_identical, scratch, write_raw, read_raw
  implicit none (type, external)
  private

  public :: register_npy

  logical, parameter :: LITTLE_ENDIAN = (iachar(transfer(1_int16, "a")) == 1)

contains

  subroutine register_npy(suite)
    type(test_suite), intent(inout) :: suite

    call suite%group("npy")
    call suite%add("npy_header_bytes", npy_header_bytes)
    call suite%add("npy_read_c_order", npy_read_c_order)
    call suite%add("npy_read_c_order_rank3", npy_read_c_order_rank3)
    call suite%add("npy_read_other_byte_order", npy_read_other_byte_order)
    call suite%add("npy_read_version_2", npy_read_version_2)
    call suite%add("npy_wrong_type_stat", npy_wrong_type_stat)
    call suite%add("npy_wrong_rank_stat", npy_wrong_rank_stat)
    call suite%add("npy_short_data_stat", npy_short_data_stat)
    call suite%add("npy_not_npy_stat", npy_not_npy_stat)
    call suite%add("npy_wrong_type_stops", npy_wrong_type_stops, expect=STOPS)
    call suite%add("npy_format_argument", npy_format_argument)
    call suite%add("npy_data_shape", npy_data_shape)
#ifdef IOFL_HAS_R128
    call suite%add("npy_real128_stat", npy_real128_stat)
#endif
  end subroutine register_npy

  function npy_file(descr, fortran_order, dims, data, version) result(bytes)
    !! Returns the bytes of a .npy file, laid out as NumPy writes it.
    character(len=*), intent(in)           :: descr, dims, data
    logical,          intent(in)           :: fortran_order
    integer,          intent(in), optional :: version
    character(len=:), allocatable          :: bytes

    character(len=:), allocatable :: dict
    integer                       :: v, prefix

    v = 1
    if ( present(version) ) v = version
    prefix = merge(10, 12, v == 1)

    dict = "{'descr': '"//descr//"', 'fortran_order': "//trim(merge("True ", "False", fortran_order))// &
           ", 'shape': ("//dims//"), }"
    dict = dict//repeat(" ", modulo(-(prefix + len(dict) + 1), 64))//IOFL_LF

    if ( v == 1 ) then
      bytes = char(147)//"NUMPY"//achar(1)//achar(0)//achar(mod(len(dict), 256))//achar(len(dict)/256)//dict//data
    else
      bytes = char(147)//"NUMPY"//achar(v)//achar(0)//achar(mod(len(dict), 256))//achar(len(dict)/256)// &
              achar(0)//achar(0)//dict//data
    end if
  end function npy_file

  function swapped(bytes, width) result(other)
    !! Reverses each `width`-byte group of `bytes`.
    character(len=*), intent(in)  :: bytes
    integer,          intent(in)  :: width
    character(len=len(bytes))     :: other

    integer :: i, j

    do i = 1, len(bytes), width
      do j = 0, width - 1
        other(i+j:i+j) = bytes(i+width-1-j:i+width-1-j)
      end do
    end do
  end function swapped

  function native(code, size) result(descr)
    character(len=*), intent(in)  :: code
    integer,          intent(in)  :: size
    character(len=:), allocatable :: descr

    character(len=8) :: digits

    write(digits, "(i0)") size
    descr = merge("<", ">", LITTLE_ENDIAN)//code//trim(digits)
  end function native

  subroutine npy_header_bytes()
    !! The header is the one NumPy writes, and the data starts at a multiple of 64 bytes.
    character(len=:), allocatable :: bytes, dict
    integer                       :: n

    call to_file(reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64], [3,2]), &
                 file=scratch()//".npy")
    bytes = read_raw(scratch()//".npy")

    call expect_equal(bytes(1:8), char(147)//"NUMPY"//achar(1)//achar(0), "Magic string and version 1.0.")
    n = iachar(bytes(9:9)) + 256*iachar(bytes(10:10))
    call expect_equal(mod(10 + n, 64), 0, "The data should start at a multiple of 64 bytes.")
    dict = "{'descr': '"//native("f", 8)//"', 'fortran_order': True, 'shape': (3, 2), }"
    call expect_equal(bytes(11:10+len(dict)), dict, "The header dictionary.")
    call expect_equal(bytes(10+n:10+n), IOFL_LF, "The header ends with a newline.")
    call expect_equal(len(bytes) - 10 - n, 48, "Six values of eight bytes follow.")
  end subroutine npy_header_bytes

  subroutine npy_read_c_order()
    !! NumPy writes C order by default: the last index varies fastest.
    real(real64), allocatable :: x(:,:)
    real(real64)              :: c_order(6)
    integer                   :: st

    c_order = [11.0_real64, 12.0_real64, 13.0_real64, 21.0_real64, 22.0_real64, 23.0_real64]     ! x(i,j) = 10i + j
    call write_raw(scratch()//".npy", npy_file(native("f", 8), .false., "2, 3", transfer(c_order, repeat(" ", 48))))

    call from_file(scratch()//".npy", into=x, stat=st)
    call expect_equal(st, 0, "The file should be read.")
    call expect_equal(shape(x), [2, 3], "The shape comes from the header.")
    if ( all(shape(x) == [2, 3]) ) then
      call expect_identical(x(1,:), [11.0_real64, 12.0_real64, 13.0_real64], "The first row.")
      call expect_identical(x(2,:), [21.0_real64, 22.0_real64, 23.0_real64], "The second row.")
    end if
  end subroutine npy_read_c_order

  subroutine npy_read_c_order_rank3()
    integer(int32), allocatable :: x(:,:,:)
    integer(int32)              :: c_order(24)
    integer                     :: i, j, k, st

    c_order = [(((100*i + 10*j + k, k = 1, 4), j = 1, 3), i = 1, 2)]                            ! Last index fastest
    call write_raw(scratch()//".npy", npy_file(native("i", 4), .false., "2, 3, 4", transfer(c_order, repeat(" ", 96))))

    call from_file(scratch()//".npy", into=x, stat=st)
    call expect_equal(st, 0, "The file should be read.")
    call expect_equal(shape(x), [2, 3, 4], "The shape comes from the header.")
    if ( all(shape(x) == [2, 3, 4]) ) then
      call check(all([(((x(i,j,k) == 100*i + 10*j + k, i = 1, 2), j = 1, 3), k = 1, 4)]), "Every element in place.")
    end if
  end subroutine npy_read_c_order_rank3

  subroutine npy_read_other_byte_order()
    !! A file in the other byte order, in Fortran order and in C order, with real, complex and integer data.
    real(real64),    allocatable :: x(:,:)
    complex(real64), allocatable :: z(:)
    integer(int16),  allocatable :: k(:)
    character(len=1)             :: other
    character(len=32)            :: bytes
    real(real64)                 :: values(4)
    integer                      :: st

    other = merge(">", "<", LITTLE_ENDIAN)
    values = [1.5_real64, -2.25_real64, 3.0e300_real64, 4.0e-300_real64]

    bytes = swapped(transfer(values, repeat(" ", 32)), 8)

    call write_raw(scratch()//"_f.npy", npy_file(other//"f8", .true., "2, 2", bytes))
    call from_file(scratch()//"_f.npy", into=x, stat=st)
    call expect_equal(st, 0, "Fortran order should be read.")
    if ( st == 0 ) call expect_identical(reshape(x, [4]), values, "Fortran order values.")

    call write_raw(scratch()//"_c.npy", npy_file(other//"f8", .false., "2, 2", bytes))
    call from_file(scratch()//"_c.npy", into=x, stat=st)
    call expect_equal(st, 0, "C order should be read.")
    if ( st == 0 ) call expect_identical(reshape(transpose(x), [4]), values, "C order values.")

    call write_raw(scratch()//"_z.npy", npy_file(other//"c16", .true., "2,", bytes))
    call from_file(scratch()//"_z.npy", into=z, stat=st)
    call expect_equal(st, 0, "Complex data should be read.")
    if ( st == 0 ) call expect_identical(z, cmplx(values([1,3]), values([2,4]), real64), &
                                         "Each part is swapped on its own.")

    call write_raw(scratch()//"_k.npy", npy_file(other//"i2", .true., "3,", &
                   swapped(transfer([1_int16, -2_int16, 300_int16], repeat(" ", 6)), 2)))
    call from_file(scratch()//"_k.npy", into=k, stat=st)
    call expect_equal(st, 0, "Integer data should be read.")
    if ( st == 0 ) call expect_equal(k, [1_int16, -2_int16, 300_int16], "Integer values.")
  end subroutine npy_read_other_byte_order

  subroutine npy_read_version_2()
    !! Version 2.0 has a four-byte header length, which NumPy uses for headers over 65535 bytes.
    integer(int8), allocatable :: x(:)
    integer                    :: st

    call write_raw(scratch()//".npy", npy_file("|i1", .true., "3,", transfer(int([-128, 0, 127], int8), "   "), &
                                              version=2))
    call from_file(scratch()//".npy", into=x, stat=st)
    call expect_equal(st, 0, "Version 2.0 should be read.")
    if ( st == 0 ) call expect_equal(int(x), [-128, 0, 127], "The values.")
  end subroutine npy_read_version_2

  subroutine npy_wrong_type_stat()
    real(real64), allocatable :: x(:)
    character(len=512)        :: msg
    integer                   :: st

    call to_file([1_int32, 2_int32], file=scratch()//".npy")
    msg = ""
    call from_file(scratch()//".npy", into=x, stat=st, errmsg=msg)
    call expect_equal(st, IOFL_READ_ERR, "Integer data can't be read into a real array.")
    call check(index(msg, native("i", 4)) > 0, "The message names the type in the file: "//trim(msg))

    call write_raw(scratch()//"_bool.npy", npy_file("|b1", .true., "1,", achar(1)))
    call from_file(scratch()//"_bool.npy", into=x, stat=st, errmsg=msg)
    call expect_equal(st, IOFL_READ_ERR, "A NumPy type this library doesn't have.")
  end subroutine npy_wrong_type_stat

  subroutine npy_wrong_rank_stat()
    real(real64), allocatable :: x(:)
    character(len=512)        :: msg
    integer                   :: st

    call to_file(reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64], [2,2]), file=scratch()//".npy")
    msg = ""
    call from_file(scratch()//".npy", into=x, stat=st, errmsg=msg)
    call expect_equal(st, IOFL_READ_ERR, "A rank 2 file can't be read into a rank 1 array.")
    call check(index(msg, "rank 2") > 0, "The message names the rank: "//trim(msg))

    call write_raw(scratch()//"_scalar.npy", npy_file(native("f", 8), .false., "", &
                                                     transfer(1.0_real64, repeat(" ", 8))))
    call from_file(scratch()//"_scalar.npy", into=x, stat=st, errmsg=msg)
    call expect_equal(st, IOFL_READ_ERR, "A NumPy scalar has rank 0.")
  end subroutine npy_wrong_rank_stat

  subroutine npy_short_data_stat()
    real(real64), allocatable :: x(:)
    integer                   :: st

    call write_raw(scratch()//".npy", npy_file(native("f", 8), .true., "4,", transfer([1.0_real64, 2.0_real64], &
                                              repeat(" ", 16))))
    call from_file(scratch()//".npy", into=x, stat=st)
    call expect_equal(st, IOFL_READ_ERR, "Two values where the header says four.")
  end subroutine npy_short_data_stat

  subroutine npy_not_npy_stat()
    real(real64), allocatable :: x(:)
    character(len=512)        :: msg
    integer                   :: st

    call write_raw(scratch()//".npy", "1.0,2.0"//IOFL_LF)
    msg = ""
    call from_file(scratch()//".npy", into=x, stat=st, errmsg=msg)
    call expect_equal(st, IOFL_READ_ERR, "A text file named .npy.")
    call check(index(msg, "not a .npy file") > 0, "The message says so: "//trim(msg))

    call from_file(scratch()//"_missing.npy", into=x, stat=st, errmsg=msg)
    call expect_equal(st, IOFL_READ_ERR, "A missing file.")
  end subroutine npy_not_npy_stat

  subroutine npy_wrong_type_stops()
    real(real64), allocatable :: x(:)

    call to_file([1_int32, 2_int32], file=scratch()//".npy")
    call from_file(scratch()//".npy", into=x)
  end subroutine npy_wrong_type_stops

  subroutine npy_format_argument()
    !! format="npy" works with any extension, and the extension is not case sensitive.
    real(real64),     allocatable :: x(:)
    character(len=:), allocatable :: bytes
    integer                       :: st

    call to_file([1.5_real64, 2.5_real64], file=scratch()//".data", format="npy", stat=st)
    call expect_equal(st, 0, "Writing with format.")
    bytes = read_raw(scratch()//".data")
    call expect_equal(bytes(1:min(6,len(bytes))), char(147)//"NUMPY", "The file is a .npy file.")
    call from_file(scratch()//".data", into=x, format="npy", stat=st)
    call expect_equal(st, 0, "Reading with format.")
    if ( st == 0 ) call expect_identical(x, [1.5_real64, 2.5_real64], "The values.")

    call to_file([1.5_real64], file=scratch()//".NPY", stat=st)
    call expect_equal(st, 0, "An uppercase extension.")
    bytes = read_raw(scratch()//".NPY")
    call expect_equal(bytes(1:min(6,len(bytes))), char(147)//"NUMPY", "The file is a .npy file.")

    call to_file([1.5_real64, 2.5_real64], file=scratch()//"_raw.npy", format="binary", stat=st)
    call expect_equal(len(read_raw(scratch()//"_raw.npy")), 16, 'format="binary" overrides the extension.')
  end subroutine npy_format_argument

  subroutine npy_data_shape()
    !! data_shape is optional for .npy files. If given, it must match the file.
    real(real64), allocatable :: x(:,:)
    character(len=512)        :: msg
    integer                   :: st

    call to_file(reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64], [3,2]), &
                 file=scratch()//".npy")
    msg = ""
    call from_file(scratch()//".npy", into=x, data_shape=[3,2], stat=st, errmsg=msg)
    call expect_equal(st, 0, "A matching data_shape.")
    call from_file(scratch()//".npy", into=x, data_shape=[2,3], stat=st, errmsg=msg)
    call expect_equal(st, IOFL_READ_ERR, "A data_shape that differs from the file.")
  end subroutine npy_data_shape

#ifdef IOFL_HAS_R128
  subroutine npy_real128_stat()
    !! NumPy has no IEEE binary128 type: its float128 is 80-bit extended precision on x86.
    use, intrinsic :: iso_fortran_env, only: real128
    real(real128), allocatable :: x(:)
    integer                    :: st

    call to_file([1.0_real128], file=scratch()//".npy", stat=st)
    call expect_equal(st, IOFL_ARG_ERR, "real128 can't be written to a .npy file.")
    call to_file([1.0_real64], file=scratch()//".npy")
    call from_file(scratch()//".npy", into=x, stat=st)
    call expect_equal(st, IOFL_ARG_ERR, "Nor read from one.")
  end subroutine npy_real128_stat
#endif
end module npy_cases
