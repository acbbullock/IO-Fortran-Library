#include "iofl_config.inc"
module npz_cases
  !---------------------------------------------------------------------------------------------------------------------
  !! NumPy `.npz` archives: arrays written and read by name, replacing an array, archives written by NumPy (including
  !! a compressed one, which is refused), and the errors for names and files that don't fit.
  !---------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: real64, int32
  use io_fortran_lib, only: String, to_file, from_file, npz_names, IOFL_READ_ERR, IOFL_WRITE_ERR, IOFL_ARG_ERR
  use test_support, only: test_suite, STOPS, check, expect_equal, expect_identical, scratch, write_raw, read_raw
  implicit none (type, external)
  private

  public :: register_npz

  ! Written by NumPy 2.5.4: numpy.savez(f, a=numpy.arange(6, dtype="<i4").reshape(2, 3),
  !                                     flags=numpy.array([True, False, True]), x=numpy.array([1.5, -2.25]))
  character(len=*), parameter :: SAVEZ_HEX = &
    "504b03042d00000000000000210050b44d84ffffffffffffffff05001400612e6e707901001000980000000000000098" // &
    "00000000000000934e554d5059010076007b276465736372273a20273c6934272c2027666f727472616e5f6f72646572" // &
    "273a2046616c73652c20277368617065273a2028322c2033292c207d2020202020202020202020202020202020202020" // &
    "20202020202020202020202020202020202020202020202020202020202020202020202020200a000000000100000002" // &
    "000000030000000400000005000000504b03042d00000000000000210016938cbbffffffffffffffff09001400666c61" // &
    "67732e6e70790100100083000000000000008300000000000000934e554d5059010076007b276465736372273a20277c" // &
    "6231272c2027666f727472616e5f6f72646572273a2046616c73652c20277368617065273a2028332c292c207d202020" // &
    "202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020" // &
    "2020202020202020200a010001504b03042d00000000000000210032faf373ffffffffffffffff05001400782e6e7079" // &
    "0100100090000000000000009000000000000000934e554d5059010076007b276465736372273a20273c6638272c2027" // &
    "666f727472616e5f6f72646572273a2046616c73652c20277368617065273a2028322c292c207d202020202020202020" // &
    "202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020" // &
    "2020200a000000000000f83f00000000000002c0504b01022d032d00000000000000210050b44d849800000098000000" // &
    "050000000000000000000000800100000000612e6e7079504b01022d032d00000000000000210016938cbb8300000083" // &
    "0000000900000000000000000000008001cf000000666c6167732e6e7079504b01022d032d00000000000000210032fa" // &
    "f373900000009000000005000000000000000000000080018d010000782e6e7079504b050600000000030003009d0000" // &
    "00540200000000"
  ! Written by NumPy 2.5.4: numpy.savez_compressed(f, a=numpy.arange(4, dtype="<i4"))
  character(len=*), parameter :: COMPRESSED_HEX = &
    "504b03042d000000080000002100fe741ed4ffffffffffffffff05001400612e6e707901001000900000000000000057" // &
    "000000000000009bec17ea1b10c9c850c650ad9e925a9c5ca46ea5a06e9369a2aea3a09e965f54529498179f5f94920a" // &
    "12774bcc294ed551502fce482c4855b752d030d1d1d451a855a00070313030303032303030313030303330300000504b" // &
    "01022d032d000000080000002100fe741ed45700000090000000050000000000000000000000800100000000612e6e70" // &
    "79504b05060000000001000100330000008e0000000000"

contains

  subroutine register_npz(suite)
    type(test_suite), intent(inout) :: suite

    call suite%group("npz")
    call suite%add("npz_round_trip", npz_round_trip)
    call suite%add("npz_replace", npz_replace)
    call suite%add("npz_read_numpy", npz_read_numpy)
    call suite%add("npz_only_array", npz_only_array)
    call suite%add("npz_name_args_stat", npz_name_args_stat)
    call suite%add("npz_missing_array_stat", npz_missing_array_stat)
    call suite%add("npz_compressed_stat", npz_compressed_stat)
    call suite%add("npz_not_archive_stat", npz_not_archive_stat)
    call suite%add("npz_wrong_type_stat", npz_wrong_type_stat)
    call suite%add("npz_format_argument", npz_format_argument)
    call suite%add("npz_data_shape", npz_data_shape)
    call suite%add("npz_name_trailing_blanks", npz_name_trailing_blanks)
    call suite%add("npz_no_name_stops", npz_no_name_stops, expect=STOPS)
    call suite%add("npz_names_missing_stops", npz_names_missing_stops, expect=STOPS)
  end subroutine register_npz

  function from_hex(hex) result(bytes)
    !! Returns the bytes that `hex` spells out, two hexadecimal digits each.
    character(len=*), intent(in) :: hex
    character(len=len(hex)/2)    :: bytes

    integer :: i

    do i = 1, len(bytes)
      bytes(i:i) = achar(16*digit(hex(2*i-1:2*i-1)) + digit(hex(2*i:2*i)))
    end do

  contains

    integer function digit(c)
      character(len=1), intent(in) :: c

      digit = index("0123456789abcdef", c) - 1
    end function digit
  end function from_hex

  function names_of(file) result(text)
    !! Returns the names `npz_names` lists for `file`, separated by spaces, or "error" if it fails.
    character(len=*), intent(in)  :: file
    character(len=:), allocatable :: text

    type(String), allocatable :: names(:)
    integer                   :: i, st

    call npz_names(file, names, stat=st)
    if ( st /= 0 ) then
      text = "error"; return
    end if

    text = ""
    do i = 1, size(names)
      if ( i > 1 ) text = text//" "
      text = text//names(i)%as_str()
    end do
  end function names_of

  subroutine npz_round_trip()
    !! Arrays of different types and ranks in one archive, listed in the order they were added.
    real(real64),   allocatable :: x(:,:)
    integer(int32), allocatable :: i3(:,:,:)
    logical,        allocatable :: flags(:)
    integer                     :: st, k

    call to_file(reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64], [3,2]), &
                 file=scratch()//".npz", name="x", stat=st)
    call expect_equal(st, 0, "Writing x creates the archive.")
    call to_file(reshape([(k, k=1,24)], [2,3,4]), file=scratch()//".npz", name="i3", stat=st)
    call expect_equal(st, 0, "Writing i3 adds it.")
    call to_file([.true., .false., .true.], file=scratch()//".npz", name="flags", stat=st)
    call expect_equal(st, 0, "Writing flags adds it.")

    call expect_equal(names_of(scratch()//".npz"), "x i3 flags", "The names, in the order they were added.")

    call from_file(scratch()//".npz", into=x, name="x", stat=st)
    call expect_equal(st, 0, "Reading x.")
    if ( st == 0 ) call expect_identical(reshape(x, [6]), [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, &
                                                           5.0_real64, 6.0_real64], "The values of x.")
    call from_file(scratch()//".npz", into=i3, name="i3", stat=st)
    call expect_equal(st, 0, "Reading i3.")
    if ( st == 0 ) then
      call expect_equal(shape(i3), [2, 3, 4], "The shape of i3.")
      call expect_equal(reshape(i3, [24]), [(k, k=1,24)], "The values of i3.")
    end if
    call from_file(scratch()//".npz", into=flags, name="flags", stat=st)
    call expect_equal(st, 0, "Reading flags.")
    if ( st == 0 ) call check(all(flags .eqv. [.true., .false., .true.]), "The values of flags.")
  end subroutine npz_round_trip

  subroutine npz_replace()
    !! Writing a name that is already there replaces that array, which moves to the end. The other arrays are kept,
    !! and an archive that gets smaller is cut to its new length.
    real(real64), allocatable :: a(:), b(:), c(:)
    integer                   :: before, st, k

    call to_file([(real(k, real64), k=1,100)], file=scratch()//".npz", name="a")
    call to_file([10.0_real64, 20.0_real64], file=scratch()//".npz", name="b")
    call to_file([30.0_real64], file=scratch()//".npz", name="c")
    before = len(read_raw(scratch()//".npz"))

    call to_file([1.0_real64, 2.0_real64], file=scratch()//".npz", name="a", stat=st)
    call expect_equal(st, 0, "Replacing a with a smaller array.")
    call expect_equal(names_of(scratch()//".npz"), "b c a", "a moves to the end.")
    call expect_equal(before - len(read_raw(scratch()//".npz")), 98*8, "The archive is 98 values shorter.")

    call to_file([(real(k, real64), k=1,50)], file=scratch()//".npz", name="b", stat=st)
    call expect_equal(st, 0, "Replacing b with a larger array.")
    call expect_equal(names_of(scratch()//".npz"), "c a b", "b moves to the end.")

    call from_file(scratch()//".npz", into=a, name="a")
    call from_file(scratch()//".npz", into=b, name="b")
    call from_file(scratch()//".npz", into=c, name="c")
    call expect_identical(a, [1.0_real64, 2.0_real64], "The new a.")
    call expect_identical(b, [(real(k, real64), k=1,50)], "The new b.")
    call expect_identical(c, [30.0_real64], "c is unchanged.")
  end subroutine npz_replace

  subroutine npz_read_numpy()
    !! An archive as numpy.savez writes it: arrays in C order, with ZIP64 fields in each local header.
    integer(int32), allocatable :: a(:,:)
    logical,        allocatable :: flags(:)
    real(real64),   allocatable :: x(:)
    integer                     :: st

    call write_raw(scratch()//".npz", from_hex(SAVEZ_HEX))
    call expect_equal(names_of(scratch()//".npz"), "a flags x", "The names NumPy wrote.")

    call from_file(scratch()//".npz", into=a, name="a", stat=st)
    call expect_equal(st, 0, "Reading a.")
    if ( st == 0 ) then
      call expect_equal(shape(a), [2, 3], "The shape of a.")
      if ( all(shape(a) == [2, 3]) ) then
        call expect_equal(a(1,:), [0, 1, 2], "The first row of a.")
        call expect_equal(a(2,:), [3, 4, 5], "The second row of a.")
      end if
    end if

    call from_file(scratch()//".npz", into=flags, name="flags", stat=st)
    call expect_equal(st, 0, "Reading flags.")
    if ( st == 0 ) call check(all(flags .eqv. [.true., .false., .true.]), "The values of flags.")

    call from_file(scratch()//".npz", into=x, name="x.npy", stat=st)
    call expect_equal(st, 0, "A name may include .npy, as with numpy.load.")
    if ( st == 0 ) call expect_identical(x, [1.5_real64, -2.25_real64], "The values of x.")
  end subroutine npz_read_numpy

  subroutine npz_only_array()
    !! An archive of one array can be read without a name.
    real(real64), allocatable :: x(:)
    integer                   :: st

    call to_file([1.5_real64, 2.5_real64], file=scratch()//".npz", name="only")
    call from_file(scratch()//".npz", into=x, stat=st)
    call expect_equal(st, 0, "Reading the only array.")
    if ( st == 0 ) call expect_identical(x, [1.5_real64, 2.5_real64], "Its values.")
  end subroutine npz_only_array

  subroutine npz_name_args_stat()
    !! name is needed to write to a .npz file, can't be blank, and is only for .npz files. Reading an archive of
    !! several arrays needs it too.
    real(real64), allocatable :: x(:)
    character(len=512)        :: msg
    integer                   :: st

    msg = ""
    call to_file([1.0_real64], file=scratch()//".npz", stat=st, errmsg=msg)
    call expect_equal(st, IOFL_ARG_ERR, "Writing to a .npz file without a name.")
    call check(index(msg, "name") > 0, "The message says so: "//trim(msg))

    call to_file([1.0_real64], file=scratch()//".npz", name="  ", stat=st)
    call expect_equal(st, IOFL_ARG_ERR, "A blank name.")

    call to_file([1.0_real64], file=scratch()//".csv", name="x", stat=st)
    call expect_equal(st, IOFL_ARG_ERR, "A name for a .csv file.")
    call to_file([1.0_real64], file=scratch()//".npy", name="x", stat=st)
    call expect_equal(st, IOFL_ARG_ERR, "A name for a .npy file.")
    call to_file([.true.], file=scratch()//".npy", name="x", stat=st)
    call expect_equal(st, IOFL_ARG_ERR, "A name for a .npy file of logical data.")

    call to_file([1.0_real64], file=scratch()//".npy")
    call from_file(scratch()//".npy", into=x, name="x", stat=st)
    call expect_equal(st, IOFL_ARG_ERR, "A name when reading a .npy file.")

    call to_file([1.0_real64], file=scratch()//".npz", name="x")
    call to_file([2.0_real64], file=scratch()//".npz", name="y")
    msg = ""
    call from_file(scratch()//".npz", into=x, stat=st, errmsg=msg)
    call expect_equal(st, IOFL_ARG_ERR, "Reading an archive of two arrays without a name.")
    call check(index(msg, '"x", "y"') > 0, "The message lists the arrays: "//trim(msg))
  end subroutine npz_name_args_stat

  subroutine npz_missing_array_stat()
    real(real64), allocatable :: x(:)
    character(len=512)        :: msg
    integer                   :: st

    call to_file([1.0_real64], file=scratch()//".npz", name="x")
    msg = ""
    call from_file(scratch()//".npz", into=x, name="y", stat=st, errmsg=msg)
    call expect_equal(st, IOFL_READ_ERR, "An array that isn't there.")
    call check(index(msg, '"y"') > 0, "The message names it: "//trim(msg))
    call check(index(msg, '"x"') > 0, "and lists the arrays: "//trim(msg))
  end subroutine npz_missing_array_stat

  subroutine npz_compressed_stat()
    !! numpy.savez_compressed archives are refused with a message that says why.
    integer(int32), allocatable :: a(:)
    character(len=512)          :: msg
    integer                     :: st

    call write_raw(scratch()//".npz", from_hex(COMPRESSED_HEX))
    call expect_equal(names_of(scratch()//".npz"), "a", "Its names can still be listed.")
    msg = ""
    call from_file(scratch()//".npz", into=a, name="a", stat=st, errmsg=msg)
    call expect_equal(st, IOFL_READ_ERR, "A compressed array.")
    call check(index(msg, "compressed") > 0, "The message says it is compressed: "//trim(msg))
  end subroutine npz_compressed_stat

  subroutine npz_not_archive_stat()
    !! A file that isn't a ZIP archive is left alone when writing, and is an error when reading.
    real(real64),     allocatable :: x(:)
    type(String),     allocatable :: names(:)
    character(len=512)            :: msg
    integer                       :: st

    call write_raw(scratch()//".npz", "1.0,2.0"//achar(10))
    msg = ""
    call to_file([1.0_real64], file=scratch()//".npz", name="x", stat=st, errmsg=msg)
    call expect_equal(st, IOFL_WRITE_ERR, "Writing to a text file named .npz.")
    call check(index(msg, "not a .npz archive") > 0, "The message says so: "//trim(msg))
    call expect_equal(read_raw(scratch()//".npz"), "1.0,2.0"//achar(10), "The file is unchanged.")

    call from_file(scratch()//".npz", into=x, name="x", stat=st)
    call expect_equal(st, IOFL_READ_ERR, "Reading it.")
    call npz_names(scratch()//".npz", names, stat=st)
    call expect_equal(st, IOFL_READ_ERR, "Listing it.")

    call from_file(scratch()//"_missing.npz", into=x, name="x", stat=st)
    call expect_equal(st, IOFL_READ_ERR, "A missing file.")
    call npz_names(scratch()//"_missing.npz", names, stat=st)
    call expect_equal(st, IOFL_READ_ERR, "Listing a missing file.")
  end subroutine npz_not_archive_stat

  subroutine npz_wrong_type_stat()
    integer(int32), allocatable :: i(:)
    real(real64),   allocatable :: x(:,:)
    character(len=512)          :: msg
    integer                     :: st

    call to_file([1.0_real64, 2.0_real64], file=scratch()//".npz", name="x")
    msg = ""
    call from_file(scratch()//".npz", into=i, name="x", stat=st, errmsg=msg)
    call expect_equal(st, IOFL_READ_ERR, "Real data can't be read into an integer array.")
    call check(index(msg, 'array "x"') > 0, "The message names the array: "//trim(msg))
    call from_file(scratch()//".npz", into=x, name="x", stat=st)
    call expect_equal(st, IOFL_READ_ERR, "A rank 1 array can't be read into a rank 2 array.")
  end subroutine npz_wrong_type_stat

  subroutine npz_format_argument()
    !! format="npz" works with any extension.
    real(real64),     allocatable :: x(:)
    character(len=:), allocatable :: bytes
    integer                       :: st

    call to_file([1.5_real64, 2.5_real64], file=scratch()//".data", format="npz", name="x", stat=st)
    call expect_equal(st, 0, "Writing with format.")
    bytes = read_raw(scratch()//".data")
    call expect_equal(bytes(1:min(4,len(bytes))), "PK"//achar(3)//achar(4), "The file is a ZIP archive.")
    call from_file(scratch()//".data", into=x, format="npz", name="x", stat=st)
    call expect_equal(st, 0, "Reading with format.")
    if ( st == 0 ) call expect_identical(x, [1.5_real64, 2.5_real64], "The values.")
  end subroutine npz_format_argument

  subroutine npz_data_shape()
    !! data_shape is optional for .npz files. If given, it must match the array.
    real(real64), allocatable :: x(:,:)
    integer                   :: st

    call to_file(reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64], [3,2]), &
                 file=scratch()//".npz", name="x")
    call from_file(scratch()//".npz", into=x, data_shape=[3,2], name="x", stat=st)
    call expect_equal(st, 0, "A matching data_shape.")
    call from_file(scratch()//".npz", into=x, data_shape=[2,3], name="x", stat=st)
    call expect_equal(st, IOFL_READ_ERR, "A data_shape that differs from the array.")
  end subroutine npz_data_shape

  subroutine npz_name_trailing_blanks()
    !! Trailing blanks in a name are ignored, so a fixed-length variable can hold it.
    real(real64), allocatable :: x(:)
    character(len=8)          :: name
    integer                   :: st

    name = "x"
    call to_file([1.0_real64], file=scratch()//".npz", name=name)
    call expect_equal(names_of(scratch()//".npz"), "x", "The name has no blanks.")
    call from_file(scratch()//".npz", into=x, name=name, stat=st)
    call expect_equal(st, 0, "Reading by the same variable.")
  end subroutine npz_name_trailing_blanks

  subroutine npz_no_name_stops()
    call to_file([1.0_real64], file=scratch()//".npz")
  end subroutine npz_no_name_stops

  subroutine npz_names_missing_stops()
    type(String), allocatable :: names(:)

    call npz_names(scratch()//"_missing.npz", names)
  end subroutine npz_names_missing_stops
end module npz_cases
