module binary_cases
  !---------------------------------------------------------------------------------------------------------------------
  !! Binary files: round trips, shapes, and closing the unit after an error.
  !---------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: real64
  use io_fortran_lib, only: to_file, from_file, operator(==), operator(/=)
  use test_support, only: test_suite, STOPS, check, expect_equal, scratch
  implicit none (type, external)
  private

  public :: register_binary

contains

  subroutine register_binary(suite)
    type(test_suite), intent(inout) :: suite

    call suite%group("binary")
    call suite%add("to_file_scalar_binary", to_file_scalar_binary, expect=STOPS)
    call suite%add("to_file_scalar_binary_stat", to_file_scalar_binary_stat)
    call suite%add("read_error_closes_binary", read_error_closes_binary)
    call suite%add("binary_round_trip", binary_round_trip)
    call suite%add("strided_section_round_trip", strided_section_round_trip)
    call suite%add("binary_wrong_rank_stat", binary_wrong_rank_stat)
    call suite%add("binary_shape_too_large", binary_shape_too_large)
  end subroutine register_binary

  subroutine to_file_scalar_binary()
    character(len=:), allocatable :: file

    file = scratch()

    call to_file(1.0_real64, file=file//".dat")
  end subroutine to_file_scalar_binary

  subroutine to_file_scalar_binary_stat()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st

    file = scratch(); st = 0; msg = ""

    call to_file(1.0_real64, file=file//".dat", stat=st, errmsg=msg)
    call check(st /= 0, "A scalar should be rejected through stat.")
  end subroutine to_file_scalar_binary_stat

  subroutine read_error_closes_binary()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    logical                       :: opened
    real(real64), allocatable     :: v(:)

    file = scratch(); st = 0; msg = ""

    call to_file([1.0_real64,2.0_real64], file=file//".dat", stat=st, errmsg=msg)
    call from_file(file//".dat", into=v, data_shape=[100], stat=st, errmsg=msg)
    call check(st /= 0, "The read should fail.")
    inquire(file=file//".dat", opened=opened)
    call check(.not. opened, "The file should be closed on return.")
  end subroutine read_error_closes_binary

  subroutine binary_round_trip()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    real(real64), allocatable     :: v(:)

    file = scratch(); st = 0; msg = ""

    call to_file([1.0_real64,2.0_real64], file=file//".dat", stat=st, errmsg=msg)
    call expect_equal(st, 0, "The write should succeed.")
    call from_file(file//".dat", into=v, data_shape=[2], stat=st, errmsg=msg)
    call expect_equal(st, 0, "The read should succeed.")
    call check(all(v == [1.0_real64,2.0_real64]), "The data should survive a round trip.")
  end subroutine binary_round_trip

  subroutine strided_section_round_trip()
    !! A section that isn't contiguous takes a different path from a whole array, in both .dat and .npy files.
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st, i
    real(real64)                  :: a(6,5)
    real(real64), allocatable     :: v(:,:), w(:,:)

    file = scratch(); st = 0; msg = ""
    a = reshape([(real(i, kind=real64), i = 1, size(a))], shape(a))

    call to_file(a(1::2,2:5:2), file=file//".dat", stat=st, errmsg=msg)
    call expect_equal(st, 0, "The .dat write should succeed.")
    call from_file(file//".dat", into=v, data_shape=[3,2], stat=st, errmsg=msg)
    call expect_equal(st, 0, "The .dat read should succeed.")
    call check(all(v == a(1::2,2:5:2)), "The section should survive a round trip through a .dat file.")

    call to_file(a(1::2,2:5:2), file=file//".npy", stat=st, errmsg=msg)
    call expect_equal(st, 0, "The .npy write should succeed.")
    call from_file(file//".npy", into=w, stat=st, errmsg=msg)
    call expect_equal(st, 0, "The .npy read should succeed.")
    call check(all(shape(w) == [3,2]), "The .npy file should keep the section's shape.")
    call check(all(w == a(1::2,2:5:2)), "The section should survive a round trip through a .npy file.")
  end subroutine strided_section_round_trip

  subroutine binary_wrong_rank_stat()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    real(real64), allocatable     :: m2(:,:)

    file = scratch(); st = 0; msg = ""

    call to_file([1.0_real64,2.0_real64], file=file//".dat", stat=st, errmsg=msg)
    call from_file(file//".dat", into=m2, data_shape=[2], stat=st, errmsg=msg)
    call check(st /= 0, "A shape that does not match the rank should be reported through stat.")
    call from_file(file//".dat", into=m2, data_shape=[2,1], stat=st, errmsg=msg)
    call check((st == 0) .and. (m2(2,1) == 2.0_real64), "The right rank should read.")
  end subroutine binary_wrong_rank_stat

  subroutine binary_shape_too_large()
    character(len=:), allocatable :: file
    character(len=512)            :: msg
    integer                       :: st
    real(real64), allocatable     :: v(:)

    file = scratch(); st = 0; msg = ""

    call to_file([1.0_real64,2.0_real64], file=file//".dat", stat=st, errmsg=msg)
    call expect_equal(st, 0, "The write should succeed.")
    call from_file(file//".dat", into=v, data_shape=[100], stat=st, errmsg=msg)
    call check(st /= 0, "Asking for more data than the file holds should be reported through stat.")
  end subroutine binary_shape_too_large
end module binary_cases
