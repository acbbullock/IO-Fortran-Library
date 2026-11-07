! Generated from fypp/test/suite/file_round_trip_cases.fypp by fypp/generate.sh. Edit that file, not this one.
#include "iofl_config.inc"
module file_round_trip_cases
  !---------------------------------------------------------------------------------------------------------------------
  !! Round trips through files for every kind: `to_file` then `from_file` on text files of rank 1 and 2 with each form
  !! of header, for every locale, format, delimiter and imaginary unit, and on binary files of every rank. Values must
  !! come back identical, the sign of zero included; NaN matches NaN.
  !---------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: r128=>real128, r64=>real64, r32=>real32, &
                                           i64=>int64, i32=>int32, i16=>int16, i8=>int8
  use io_fortran_lib, only: str, to_file, from_file
  use test_support,   only: test_suite, fail, failure_count, expect_equal, expect_identical, scratch
  use test_values,    only: reset_random, test_values_of
  implicit none (type, external)
  private

  public :: register_file_round_trip

  character(len=2), parameter :: LOCALES(*)   = [ "US", "EU" ]
  character(len=1), parameter :: REAL_FMTS(*) = [ "e", "f", "z" ]
  character(len=1), parameter :: INT_FMTS(*)  = [ "i", "z" ]
  character(len=1), parameter :: DELIMS(*)    = [ ",", ";" ]
  character(len=3), parameter :: IMS(*)       = [ "   ", "*1i", "j  " ]

  integer,          parameter :: COLS = 6
  character(len=4), parameter :: COLUMN_NAMES(COLS) = [ character(len=4) :: "a", "b c", "d", "e,f", "g", "h" ]

  integer, parameter :: N_FILE = 280                                          ! Random values for the text cases

  ! Header forms: one name, a name per column, one empty name, and none. Rank 1 uses the first and last.
  integer,           parameter :: ONE_NAME = 1, PER_COLUMN = 2, EMPTY_NAME = 3, NO_HEADER = 4
  character(len=10), parameter :: HEADER_NAMES(4) = [ "one name  ", "per column", "empty name", "none      " ]

contains

  subroutine register_file_round_trip(suite)
    type(test_suite), intent(inout) :: suite

    call suite%group("file_round_trip")
    call suite%add("text_file_i64", text_file_i64)
    call suite%add("binary_file_i64", binary_file_i64)
    call suite%add("text_file_i32", text_file_i32)
    call suite%add("binary_file_i32", binary_file_i32)
    call suite%add("text_file_i16", text_file_i16)
    call suite%add("binary_file_i16", binary_file_i16)
    call suite%add("text_file_i8", text_file_i8)
    call suite%add("binary_file_i8", binary_file_i8)
#ifdef IOFL_HAS_R128
    call suite%add("text_file_r128", text_file_r128)
    call suite%add("binary_file_r128", binary_file_r128)
    call suite%add("small_values_fmt_f_r128", small_values_fmt_f_r128)
#endif
    call suite%add("text_file_r64", text_file_r64)
    call suite%add("binary_file_r64", binary_file_r64)
    call suite%add("small_values_fmt_f_r64", small_values_fmt_f_r64)
    call suite%add("text_file_r32", text_file_r32)
    call suite%add("binary_file_r32", binary_file_r32)
#ifdef IOFL_HAS_R128
    call suite%add("text_file_c128", text_file_c128)
    call suite%add("binary_file_c128", binary_file_c128)
    call suite%add("small_values_fmt_f_c128", small_values_fmt_f_c128)
#endif
    call suite%add("text_file_c64", text_file_c64)
    call suite%add("binary_file_c64", binary_file_c64)
    call suite%add("small_values_fmt_f_c64", small_values_fmt_f_c64)
    call suite%add("text_file_c32", text_file_c32)
    call suite%add("binary_file_c32", binary_file_c32)
  end subroutine register_file_round_trip

  logical function succeeded(stat, errmsg, what)
    !! Records a failure if `stat` is nonzero.
    integer,          intent(in) :: stat
    character(len=*), intent(in) :: errmsg, what

    succeeded = (stat == 0)
    if ( .not. succeeded ) call fail(what//" failed with stat "//str(stat)//": "//trim(errmsg))
  end function succeeded

  function settings(fmt, locale, delim, im) result(text)
    character(len=*), intent(in)           :: fmt
    character(len=*), intent(in), optional :: locale, delim, im
    character(len=:), allocatable          :: text

    text = ', fmt="'//fmt//'"'
    if ( present(locale) ) text = text//', locale="'//locale//'"'
    if ( present(delim) )  text = text//', delim="'//delim//'"'
    if ( present(im) )     text = text//', im="'//trim(im)//'"'
  end function settings

  function context(rank, header, combo) result(text)
    integer,          intent(in)  :: rank, header
    character(len=*), intent(in)  :: combo
    character(len=:), allocatable :: text

    text = " (rank "//str(rank)//", header "//trim(HEADER_NAMES(header))//combo//")"
  end function context

  subroutine text_file_i64()
    !! `to_file` then `from_file` on text files of rank 1 and 2, with each form of header.
    integer(i64), allocatable :: values(:), x1(:), y1(:), x2(:,:), y2(:,:)
    character(len=:), allocatable :: file, combo
    character(len=512)            :: msg
    integer                       :: l, f, h, st, rows

    call reset_random()
    values = test_values_of(0_i64, N_FILE)
    file = scratch()//".csv"
    msg = ""

    do l = 1, size(DELIMS)
      do f = 1, size(INT_FMTS)
          combo = settings(INT_FMTS(f), delim=DELIMS(l))

          rows = size(values)/COLS
          x1 = values
          x2 = reshape(values(1:rows*COLS), [rows, COLS])

          do h = ONE_NAME, NO_HEADER, NO_HEADER - ONE_NAME
            select case (h)
              case (ONE_NAME)
                call to_file(x1, file=file, header=["x"], &
                             delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
              case default
                call to_file(x1, file=file, &
                             delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
            end select
            if ( .not. succeeded(st, msg, "to_file"//context(1, h, combo)) ) cycle
            call from_file(file, into=y1, header=(h == ONE_NAME), &
                           delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
            if ( .not. succeeded(st, msg, "from_file"//context(1, h, combo)) ) cycle
            call expect_equal(y1, x1, "Values"//context(1, h, combo))
          end do

          do h = ONE_NAME, NO_HEADER
            select case (h)
              case (ONE_NAME)
                call to_file(x2, file=file, header=["x"], &
                             delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
              case (PER_COLUMN)
                call to_file(x2, file=file, header=COLUMN_NAMES, &
                             delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
              case (EMPTY_NAME)
                call to_file(x2, file=file, header=[""], &
                             delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
              case default
                call to_file(x2, file=file, &
                             delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
            end select
            if ( .not. succeeded(st, msg, "to_file"//context(2, h, combo)) ) cycle
            call from_file(file, into=y2, header=(h <= PER_COLUMN), &
                           delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
            if ( .not. succeeded(st, msg, "from_file"//context(2, h, combo)) ) cycle
            call expect_equal(shape(y2), shape(x2), "Shape"//context(2, h, combo))
            if ( all(shape(y2) == shape(x2)) ) then
              call expect_equal(pack(y2, .true.), pack(x2, .true.), &
                               "Values"//context(2, h, combo))
            end if
          end do
      end do
    end do
  end subroutine text_file_i64

  subroutine binary_file_i64()
    !! `to_file` then `from_file` on binary files of every rank.
    integer(i64), allocatable :: values(:), sample(:)
    integer(i64), allocatable :: a1(:), b1(:)
    integer(i64), allocatable :: a2(:,:), b2(:,:)
    integer(i64), allocatable :: a3(:,:,:), b3(:,:,:)
    integer(i64), allocatable :: a4(:,:,:,:), b4(:,:,:,:)
    integer(i64), allocatable :: a5(:,:,:,:,:), b5(:,:,:,:,:)
    integer(i64), allocatable :: a6(:,:,:,:,:,:), b6(:,:,:,:,:,:)
    integer(i64), allocatable :: a7(:,:,:,:,:,:,:), b7(:,:,:,:,:,:,:)
    integer(i64), allocatable :: a8(:,:,:,:,:,:,:,:), b8(:,:,:,:,:,:,:,:)
    integer(i64), allocatable :: a9(:,:,:,:,:,:,:,:,:), b9(:,:,:,:,:,:,:,:,:)
    integer(i64), allocatable :: a10(:,:,:,:,:,:,:,:,:,:), b10(:,:,:,:,:,:,:,:,:,:)
    integer(i64), allocatable :: a11(:,:,:,:,:,:,:,:,:,:,:), b11(:,:,:,:,:,:,:,:,:,:,:)
    integer(i64), allocatable :: a12(:,:,:,:,:,:,:,:,:,:,:,:), b12(:,:,:,:,:,:,:,:,:,:,:,:)
    integer(i64), allocatable :: a13(:,:,:,:,:,:,:,:,:,:,:,:,:), b13(:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer(i64), allocatable :: a14(:,:,:,:,:,:,:,:,:,:,:,:,:,:), b14(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer(i64), allocatable :: a15(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), b15(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    character(len=512) :: msg
    integer            :: st

    call reset_random()
    values = test_values_of(0_i64, 12)
    sample = [ values(1:12), values(size(values)-11:) ]                       ! Twelve edge and twelve random values
    msg = ""

    a1 = reshape(sample(1:3), [3])
    call to_file(a1, file=scratch()//"_rank1.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 1)") ) then
      call from_file(scratch()//"_rank1.dat", into=b1, data_shape=shape(a1), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 1)") ) then
        call expect_equal(shape(b1), shape(a1), "Shape (rank 1)")
        call expect_equal(pack(b1, .true.), pack(a1, .true.), "Values (rank 1)")
      end if
    end if

    a2 = reshape(sample(1:6), [3, 2])
    call to_file(a2, file=scratch()//"_rank2.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 2)") ) then
      call from_file(scratch()//"_rank2.dat", into=b2, data_shape=shape(a2), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 2)") ) then
        call expect_equal(shape(b2), shape(a2), "Shape (rank 2)")
        call expect_equal(pack(b2, .true.), pack(a2, .true.), "Values (rank 2)")
      end if
    end if

    a3 = reshape(sample(1:12), [3, 2, 2])
    call to_file(a3, file=scratch()//"_rank3.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 3)") ) then
      call from_file(scratch()//"_rank3.dat", into=b3, data_shape=shape(a3), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 3)") ) then
        call expect_equal(shape(b3), shape(a3), "Shape (rank 3)")
        call expect_equal(pack(b3, .true.), pack(a3, .true.), "Values (rank 3)")
      end if
    end if

    a4 = reshape(sample(1:24), [3, 2, 2, 2])
    call to_file(a4, file=scratch()//"_rank4.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 4)") ) then
      call from_file(scratch()//"_rank4.dat", into=b4, data_shape=shape(a4), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 4)") ) then
        call expect_equal(shape(b4), shape(a4), "Shape (rank 4)")
        call expect_equal(pack(b4, .true.), pack(a4, .true.), "Values (rank 4)")
      end if
    end if

    a5 = reshape(sample(1:24), [3, 2, 2, 2, 1])
    call to_file(a5, file=scratch()//"_rank5.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 5)") ) then
      call from_file(scratch()//"_rank5.dat", into=b5, data_shape=shape(a5), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 5)") ) then
        call expect_equal(shape(b5), shape(a5), "Shape (rank 5)")
        call expect_equal(pack(b5, .true.), pack(a5, .true.), "Values (rank 5)")
      end if
    end if

    a6 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1])
    call to_file(a6, file=scratch()//"_rank6.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 6)") ) then
      call from_file(scratch()//"_rank6.dat", into=b6, data_shape=shape(a6), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 6)") ) then
        call expect_equal(shape(b6), shape(a6), "Shape (rank 6)")
        call expect_equal(pack(b6, .true.), pack(a6, .true.), "Values (rank 6)")
      end if
    end if

    a7 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1])
    call to_file(a7, file=scratch()//"_rank7.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 7)") ) then
      call from_file(scratch()//"_rank7.dat", into=b7, data_shape=shape(a7), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 7)") ) then
        call expect_equal(shape(b7), shape(a7), "Shape (rank 7)")
        call expect_equal(pack(b7, .true.), pack(a7, .true.), "Values (rank 7)")
      end if
    end if

    a8 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1])
    call to_file(a8, file=scratch()//"_rank8.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 8)") ) then
      call from_file(scratch()//"_rank8.dat", into=b8, data_shape=shape(a8), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 8)") ) then
        call expect_equal(shape(b8), shape(a8), "Shape (rank 8)")
        call expect_equal(pack(b8, .true.), pack(a8, .true.), "Values (rank 8)")
      end if
    end if

    a9 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1])
    call to_file(a9, file=scratch()//"_rank9.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 9)") ) then
      call from_file(scratch()//"_rank9.dat", into=b9, data_shape=shape(a9), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 9)") ) then
        call expect_equal(shape(b9), shape(a9), "Shape (rank 9)")
        call expect_equal(pack(b9, .true.), pack(a9, .true.), "Values (rank 9)")
      end if
    end if

    a10 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1])
    call to_file(a10, file=scratch()//"_rank10.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 10)") ) then
      call from_file(scratch()//"_rank10.dat", into=b10, data_shape=shape(a10), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 10)") ) then
        call expect_equal(shape(b10), shape(a10), "Shape (rank 10)")
        call expect_equal(pack(b10, .true.), pack(a10, .true.), "Values (rank 10)")
      end if
    end if

    a11 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a11, file=scratch()//"_rank11.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 11)") ) then
      call from_file(scratch()//"_rank11.dat", into=b11, data_shape=shape(a11), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 11)") ) then
        call expect_equal(shape(b11), shape(a11), "Shape (rank 11)")
        call expect_equal(pack(b11, .true.), pack(a11, .true.), "Values (rank 11)")
      end if
    end if

    a12 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a12, file=scratch()//"_rank12.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 12)") ) then
      call from_file(scratch()//"_rank12.dat", into=b12, data_shape=shape(a12), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 12)") ) then
        call expect_equal(shape(b12), shape(a12), "Shape (rank 12)")
        call expect_equal(pack(b12, .true.), pack(a12, .true.), "Values (rank 12)")
      end if
    end if

    a13 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a13, file=scratch()//"_rank13.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 13)") ) then
      call from_file(scratch()//"_rank13.dat", into=b13, data_shape=shape(a13), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 13)") ) then
        call expect_equal(shape(b13), shape(a13), "Shape (rank 13)")
        call expect_equal(pack(b13, .true.), pack(a13, .true.), "Values (rank 13)")
      end if
    end if

    a14 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a14, file=scratch()//"_rank14.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 14)") ) then
      call from_file(scratch()//"_rank14.dat", into=b14, data_shape=shape(a14), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 14)") ) then
        call expect_equal(shape(b14), shape(a14), "Shape (rank 14)")
        call expect_equal(pack(b14, .true.), pack(a14, .true.), "Values (rank 14)")
      end if
    end if

    a15 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a15, file=scratch()//"_rank15.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 15)") ) then
      call from_file(scratch()//"_rank15.dat", into=b15, data_shape=shape(a15), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 15)") ) then
        call expect_equal(shape(b15), shape(a15), "Shape (rank 15)")
        call expect_equal(pack(b15, .true.), pack(a15, .true.), "Values (rank 15)")
      end if
    end if

  end subroutine binary_file_i64


  subroutine text_file_i32()
    !! `to_file` then `from_file` on text files of rank 1 and 2, with each form of header.
    integer(i32), allocatable :: values(:), x1(:), y1(:), x2(:,:), y2(:,:)
    character(len=:), allocatable :: file, combo
    character(len=512)            :: msg
    integer                       :: l, f, h, st, rows

    call reset_random()
    values = test_values_of(0_i32, N_FILE)
    file = scratch()//".csv"
    msg = ""

    do l = 1, size(DELIMS)
      do f = 1, size(INT_FMTS)
          combo = settings(INT_FMTS(f), delim=DELIMS(l))

          rows = size(values)/COLS
          x1 = values
          x2 = reshape(values(1:rows*COLS), [rows, COLS])

          do h = ONE_NAME, NO_HEADER, NO_HEADER - ONE_NAME
            select case (h)
              case (ONE_NAME)
                call to_file(x1, file=file, header=["x"], &
                             delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
              case default
                call to_file(x1, file=file, &
                             delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
            end select
            if ( .not. succeeded(st, msg, "to_file"//context(1, h, combo)) ) cycle
            call from_file(file, into=y1, header=(h == ONE_NAME), &
                           delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
            if ( .not. succeeded(st, msg, "from_file"//context(1, h, combo)) ) cycle
            call expect_equal(y1, x1, "Values"//context(1, h, combo))
          end do

          do h = ONE_NAME, NO_HEADER
            select case (h)
              case (ONE_NAME)
                call to_file(x2, file=file, header=["x"], &
                             delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
              case (PER_COLUMN)
                call to_file(x2, file=file, header=COLUMN_NAMES, &
                             delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
              case (EMPTY_NAME)
                call to_file(x2, file=file, header=[""], &
                             delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
              case default
                call to_file(x2, file=file, &
                             delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
            end select
            if ( .not. succeeded(st, msg, "to_file"//context(2, h, combo)) ) cycle
            call from_file(file, into=y2, header=(h <= PER_COLUMN), &
                           delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
            if ( .not. succeeded(st, msg, "from_file"//context(2, h, combo)) ) cycle
            call expect_equal(shape(y2), shape(x2), "Shape"//context(2, h, combo))
            if ( all(shape(y2) == shape(x2)) ) then
              call expect_equal(pack(y2, .true.), pack(x2, .true.), &
                               "Values"//context(2, h, combo))
            end if
          end do
      end do
    end do
  end subroutine text_file_i32

  subroutine binary_file_i32()
    !! `to_file` then `from_file` on binary files of every rank.
    integer(i32), allocatable :: values(:), sample(:)
    integer(i32), allocatable :: a1(:), b1(:)
    integer(i32), allocatable :: a2(:,:), b2(:,:)
    integer(i32), allocatable :: a3(:,:,:), b3(:,:,:)
    integer(i32), allocatable :: a4(:,:,:,:), b4(:,:,:,:)
    integer(i32), allocatable :: a5(:,:,:,:,:), b5(:,:,:,:,:)
    integer(i32), allocatable :: a6(:,:,:,:,:,:), b6(:,:,:,:,:,:)
    integer(i32), allocatable :: a7(:,:,:,:,:,:,:), b7(:,:,:,:,:,:,:)
    integer(i32), allocatable :: a8(:,:,:,:,:,:,:,:), b8(:,:,:,:,:,:,:,:)
    integer(i32), allocatable :: a9(:,:,:,:,:,:,:,:,:), b9(:,:,:,:,:,:,:,:,:)
    integer(i32), allocatable :: a10(:,:,:,:,:,:,:,:,:,:), b10(:,:,:,:,:,:,:,:,:,:)
    integer(i32), allocatable :: a11(:,:,:,:,:,:,:,:,:,:,:), b11(:,:,:,:,:,:,:,:,:,:,:)
    integer(i32), allocatable :: a12(:,:,:,:,:,:,:,:,:,:,:,:), b12(:,:,:,:,:,:,:,:,:,:,:,:)
    integer(i32), allocatable :: a13(:,:,:,:,:,:,:,:,:,:,:,:,:), b13(:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer(i32), allocatable :: a14(:,:,:,:,:,:,:,:,:,:,:,:,:,:), b14(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer(i32), allocatable :: a15(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), b15(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    character(len=512) :: msg
    integer            :: st

    call reset_random()
    values = test_values_of(0_i32, 12)
    sample = [ values(1:12), values(size(values)-11:) ]                       ! Twelve edge and twelve random values
    msg = ""

    a1 = reshape(sample(1:3), [3])
    call to_file(a1, file=scratch()//"_rank1.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 1)") ) then
      call from_file(scratch()//"_rank1.dat", into=b1, data_shape=shape(a1), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 1)") ) then
        call expect_equal(shape(b1), shape(a1), "Shape (rank 1)")
        call expect_equal(pack(b1, .true.), pack(a1, .true.), "Values (rank 1)")
      end if
    end if

    a2 = reshape(sample(1:6), [3, 2])
    call to_file(a2, file=scratch()//"_rank2.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 2)") ) then
      call from_file(scratch()//"_rank2.dat", into=b2, data_shape=shape(a2), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 2)") ) then
        call expect_equal(shape(b2), shape(a2), "Shape (rank 2)")
        call expect_equal(pack(b2, .true.), pack(a2, .true.), "Values (rank 2)")
      end if
    end if

    a3 = reshape(sample(1:12), [3, 2, 2])
    call to_file(a3, file=scratch()//"_rank3.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 3)") ) then
      call from_file(scratch()//"_rank3.dat", into=b3, data_shape=shape(a3), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 3)") ) then
        call expect_equal(shape(b3), shape(a3), "Shape (rank 3)")
        call expect_equal(pack(b3, .true.), pack(a3, .true.), "Values (rank 3)")
      end if
    end if

    a4 = reshape(sample(1:24), [3, 2, 2, 2])
    call to_file(a4, file=scratch()//"_rank4.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 4)") ) then
      call from_file(scratch()//"_rank4.dat", into=b4, data_shape=shape(a4), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 4)") ) then
        call expect_equal(shape(b4), shape(a4), "Shape (rank 4)")
        call expect_equal(pack(b4, .true.), pack(a4, .true.), "Values (rank 4)")
      end if
    end if

    a5 = reshape(sample(1:24), [3, 2, 2, 2, 1])
    call to_file(a5, file=scratch()//"_rank5.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 5)") ) then
      call from_file(scratch()//"_rank5.dat", into=b5, data_shape=shape(a5), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 5)") ) then
        call expect_equal(shape(b5), shape(a5), "Shape (rank 5)")
        call expect_equal(pack(b5, .true.), pack(a5, .true.), "Values (rank 5)")
      end if
    end if

    a6 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1])
    call to_file(a6, file=scratch()//"_rank6.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 6)") ) then
      call from_file(scratch()//"_rank6.dat", into=b6, data_shape=shape(a6), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 6)") ) then
        call expect_equal(shape(b6), shape(a6), "Shape (rank 6)")
        call expect_equal(pack(b6, .true.), pack(a6, .true.), "Values (rank 6)")
      end if
    end if

    a7 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1])
    call to_file(a7, file=scratch()//"_rank7.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 7)") ) then
      call from_file(scratch()//"_rank7.dat", into=b7, data_shape=shape(a7), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 7)") ) then
        call expect_equal(shape(b7), shape(a7), "Shape (rank 7)")
        call expect_equal(pack(b7, .true.), pack(a7, .true.), "Values (rank 7)")
      end if
    end if

    a8 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1])
    call to_file(a8, file=scratch()//"_rank8.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 8)") ) then
      call from_file(scratch()//"_rank8.dat", into=b8, data_shape=shape(a8), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 8)") ) then
        call expect_equal(shape(b8), shape(a8), "Shape (rank 8)")
        call expect_equal(pack(b8, .true.), pack(a8, .true.), "Values (rank 8)")
      end if
    end if

    a9 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1])
    call to_file(a9, file=scratch()//"_rank9.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 9)") ) then
      call from_file(scratch()//"_rank9.dat", into=b9, data_shape=shape(a9), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 9)") ) then
        call expect_equal(shape(b9), shape(a9), "Shape (rank 9)")
        call expect_equal(pack(b9, .true.), pack(a9, .true.), "Values (rank 9)")
      end if
    end if

    a10 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1])
    call to_file(a10, file=scratch()//"_rank10.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 10)") ) then
      call from_file(scratch()//"_rank10.dat", into=b10, data_shape=shape(a10), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 10)") ) then
        call expect_equal(shape(b10), shape(a10), "Shape (rank 10)")
        call expect_equal(pack(b10, .true.), pack(a10, .true.), "Values (rank 10)")
      end if
    end if

    a11 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a11, file=scratch()//"_rank11.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 11)") ) then
      call from_file(scratch()//"_rank11.dat", into=b11, data_shape=shape(a11), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 11)") ) then
        call expect_equal(shape(b11), shape(a11), "Shape (rank 11)")
        call expect_equal(pack(b11, .true.), pack(a11, .true.), "Values (rank 11)")
      end if
    end if

    a12 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a12, file=scratch()//"_rank12.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 12)") ) then
      call from_file(scratch()//"_rank12.dat", into=b12, data_shape=shape(a12), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 12)") ) then
        call expect_equal(shape(b12), shape(a12), "Shape (rank 12)")
        call expect_equal(pack(b12, .true.), pack(a12, .true.), "Values (rank 12)")
      end if
    end if

    a13 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a13, file=scratch()//"_rank13.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 13)") ) then
      call from_file(scratch()//"_rank13.dat", into=b13, data_shape=shape(a13), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 13)") ) then
        call expect_equal(shape(b13), shape(a13), "Shape (rank 13)")
        call expect_equal(pack(b13, .true.), pack(a13, .true.), "Values (rank 13)")
      end if
    end if

    a14 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a14, file=scratch()//"_rank14.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 14)") ) then
      call from_file(scratch()//"_rank14.dat", into=b14, data_shape=shape(a14), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 14)") ) then
        call expect_equal(shape(b14), shape(a14), "Shape (rank 14)")
        call expect_equal(pack(b14, .true.), pack(a14, .true.), "Values (rank 14)")
      end if
    end if

    a15 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a15, file=scratch()//"_rank15.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 15)") ) then
      call from_file(scratch()//"_rank15.dat", into=b15, data_shape=shape(a15), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 15)") ) then
        call expect_equal(shape(b15), shape(a15), "Shape (rank 15)")
        call expect_equal(pack(b15, .true.), pack(a15, .true.), "Values (rank 15)")
      end if
    end if

  end subroutine binary_file_i32


  subroutine text_file_i16()
    !! `to_file` then `from_file` on text files of rank 1 and 2, with each form of header.
    integer(i16), allocatable :: values(:), x1(:), y1(:), x2(:,:), y2(:,:)
    character(len=:), allocatable :: file, combo
    character(len=512)            :: msg
    integer                       :: l, f, h, st, rows

    call reset_random()
    values = test_values_of(0_i16, N_FILE)
    file = scratch()//".csv"
    msg = ""

    do l = 1, size(DELIMS)
      do f = 1, size(INT_FMTS)
          combo = settings(INT_FMTS(f), delim=DELIMS(l))

          rows = size(values)/COLS
          x1 = values
          x2 = reshape(values(1:rows*COLS), [rows, COLS])

          do h = ONE_NAME, NO_HEADER, NO_HEADER - ONE_NAME
            select case (h)
              case (ONE_NAME)
                call to_file(x1, file=file, header=["x"], &
                             delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
              case default
                call to_file(x1, file=file, &
                             delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
            end select
            if ( .not. succeeded(st, msg, "to_file"//context(1, h, combo)) ) cycle
            call from_file(file, into=y1, header=(h == ONE_NAME), &
                           delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
            if ( .not. succeeded(st, msg, "from_file"//context(1, h, combo)) ) cycle
            call expect_equal(y1, x1, "Values"//context(1, h, combo))
          end do

          do h = ONE_NAME, NO_HEADER
            select case (h)
              case (ONE_NAME)
                call to_file(x2, file=file, header=["x"], &
                             delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
              case (PER_COLUMN)
                call to_file(x2, file=file, header=COLUMN_NAMES, &
                             delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
              case (EMPTY_NAME)
                call to_file(x2, file=file, header=[""], &
                             delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
              case default
                call to_file(x2, file=file, &
                             delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
            end select
            if ( .not. succeeded(st, msg, "to_file"//context(2, h, combo)) ) cycle
            call from_file(file, into=y2, header=(h <= PER_COLUMN), &
                           delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
            if ( .not. succeeded(st, msg, "from_file"//context(2, h, combo)) ) cycle
            call expect_equal(shape(y2), shape(x2), "Shape"//context(2, h, combo))
            if ( all(shape(y2) == shape(x2)) ) then
              call expect_equal(pack(y2, .true.), pack(x2, .true.), &
                               "Values"//context(2, h, combo))
            end if
          end do
      end do
    end do
  end subroutine text_file_i16

  subroutine binary_file_i16()
    !! `to_file` then `from_file` on binary files of every rank.
    integer(i16), allocatable :: values(:), sample(:)
    integer(i16), allocatable :: a1(:), b1(:)
    integer(i16), allocatable :: a2(:,:), b2(:,:)
    integer(i16), allocatable :: a3(:,:,:), b3(:,:,:)
    integer(i16), allocatable :: a4(:,:,:,:), b4(:,:,:,:)
    integer(i16), allocatable :: a5(:,:,:,:,:), b5(:,:,:,:,:)
    integer(i16), allocatable :: a6(:,:,:,:,:,:), b6(:,:,:,:,:,:)
    integer(i16), allocatable :: a7(:,:,:,:,:,:,:), b7(:,:,:,:,:,:,:)
    integer(i16), allocatable :: a8(:,:,:,:,:,:,:,:), b8(:,:,:,:,:,:,:,:)
    integer(i16), allocatable :: a9(:,:,:,:,:,:,:,:,:), b9(:,:,:,:,:,:,:,:,:)
    integer(i16), allocatable :: a10(:,:,:,:,:,:,:,:,:,:), b10(:,:,:,:,:,:,:,:,:,:)
    integer(i16), allocatable :: a11(:,:,:,:,:,:,:,:,:,:,:), b11(:,:,:,:,:,:,:,:,:,:,:)
    integer(i16), allocatable :: a12(:,:,:,:,:,:,:,:,:,:,:,:), b12(:,:,:,:,:,:,:,:,:,:,:,:)
    integer(i16), allocatable :: a13(:,:,:,:,:,:,:,:,:,:,:,:,:), b13(:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer(i16), allocatable :: a14(:,:,:,:,:,:,:,:,:,:,:,:,:,:), b14(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer(i16), allocatable :: a15(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), b15(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    character(len=512) :: msg
    integer            :: st

    call reset_random()
    values = test_values_of(0_i16, 12)
    sample = [ values(1:12), values(size(values)-11:) ]                       ! Twelve edge and twelve random values
    msg = ""

    a1 = reshape(sample(1:3), [3])
    call to_file(a1, file=scratch()//"_rank1.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 1)") ) then
      call from_file(scratch()//"_rank1.dat", into=b1, data_shape=shape(a1), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 1)") ) then
        call expect_equal(shape(b1), shape(a1), "Shape (rank 1)")
        call expect_equal(pack(b1, .true.), pack(a1, .true.), "Values (rank 1)")
      end if
    end if

    a2 = reshape(sample(1:6), [3, 2])
    call to_file(a2, file=scratch()//"_rank2.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 2)") ) then
      call from_file(scratch()//"_rank2.dat", into=b2, data_shape=shape(a2), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 2)") ) then
        call expect_equal(shape(b2), shape(a2), "Shape (rank 2)")
        call expect_equal(pack(b2, .true.), pack(a2, .true.), "Values (rank 2)")
      end if
    end if

    a3 = reshape(sample(1:12), [3, 2, 2])
    call to_file(a3, file=scratch()//"_rank3.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 3)") ) then
      call from_file(scratch()//"_rank3.dat", into=b3, data_shape=shape(a3), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 3)") ) then
        call expect_equal(shape(b3), shape(a3), "Shape (rank 3)")
        call expect_equal(pack(b3, .true.), pack(a3, .true.), "Values (rank 3)")
      end if
    end if

    a4 = reshape(sample(1:24), [3, 2, 2, 2])
    call to_file(a4, file=scratch()//"_rank4.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 4)") ) then
      call from_file(scratch()//"_rank4.dat", into=b4, data_shape=shape(a4), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 4)") ) then
        call expect_equal(shape(b4), shape(a4), "Shape (rank 4)")
        call expect_equal(pack(b4, .true.), pack(a4, .true.), "Values (rank 4)")
      end if
    end if

    a5 = reshape(sample(1:24), [3, 2, 2, 2, 1])
    call to_file(a5, file=scratch()//"_rank5.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 5)") ) then
      call from_file(scratch()//"_rank5.dat", into=b5, data_shape=shape(a5), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 5)") ) then
        call expect_equal(shape(b5), shape(a5), "Shape (rank 5)")
        call expect_equal(pack(b5, .true.), pack(a5, .true.), "Values (rank 5)")
      end if
    end if

    a6 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1])
    call to_file(a6, file=scratch()//"_rank6.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 6)") ) then
      call from_file(scratch()//"_rank6.dat", into=b6, data_shape=shape(a6), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 6)") ) then
        call expect_equal(shape(b6), shape(a6), "Shape (rank 6)")
        call expect_equal(pack(b6, .true.), pack(a6, .true.), "Values (rank 6)")
      end if
    end if

    a7 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1])
    call to_file(a7, file=scratch()//"_rank7.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 7)") ) then
      call from_file(scratch()//"_rank7.dat", into=b7, data_shape=shape(a7), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 7)") ) then
        call expect_equal(shape(b7), shape(a7), "Shape (rank 7)")
        call expect_equal(pack(b7, .true.), pack(a7, .true.), "Values (rank 7)")
      end if
    end if

    a8 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1])
    call to_file(a8, file=scratch()//"_rank8.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 8)") ) then
      call from_file(scratch()//"_rank8.dat", into=b8, data_shape=shape(a8), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 8)") ) then
        call expect_equal(shape(b8), shape(a8), "Shape (rank 8)")
        call expect_equal(pack(b8, .true.), pack(a8, .true.), "Values (rank 8)")
      end if
    end if

    a9 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1])
    call to_file(a9, file=scratch()//"_rank9.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 9)") ) then
      call from_file(scratch()//"_rank9.dat", into=b9, data_shape=shape(a9), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 9)") ) then
        call expect_equal(shape(b9), shape(a9), "Shape (rank 9)")
        call expect_equal(pack(b9, .true.), pack(a9, .true.), "Values (rank 9)")
      end if
    end if

    a10 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1])
    call to_file(a10, file=scratch()//"_rank10.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 10)") ) then
      call from_file(scratch()//"_rank10.dat", into=b10, data_shape=shape(a10), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 10)") ) then
        call expect_equal(shape(b10), shape(a10), "Shape (rank 10)")
        call expect_equal(pack(b10, .true.), pack(a10, .true.), "Values (rank 10)")
      end if
    end if

    a11 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a11, file=scratch()//"_rank11.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 11)") ) then
      call from_file(scratch()//"_rank11.dat", into=b11, data_shape=shape(a11), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 11)") ) then
        call expect_equal(shape(b11), shape(a11), "Shape (rank 11)")
        call expect_equal(pack(b11, .true.), pack(a11, .true.), "Values (rank 11)")
      end if
    end if

    a12 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a12, file=scratch()//"_rank12.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 12)") ) then
      call from_file(scratch()//"_rank12.dat", into=b12, data_shape=shape(a12), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 12)") ) then
        call expect_equal(shape(b12), shape(a12), "Shape (rank 12)")
        call expect_equal(pack(b12, .true.), pack(a12, .true.), "Values (rank 12)")
      end if
    end if

    a13 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a13, file=scratch()//"_rank13.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 13)") ) then
      call from_file(scratch()//"_rank13.dat", into=b13, data_shape=shape(a13), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 13)") ) then
        call expect_equal(shape(b13), shape(a13), "Shape (rank 13)")
        call expect_equal(pack(b13, .true.), pack(a13, .true.), "Values (rank 13)")
      end if
    end if

    a14 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a14, file=scratch()//"_rank14.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 14)") ) then
      call from_file(scratch()//"_rank14.dat", into=b14, data_shape=shape(a14), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 14)") ) then
        call expect_equal(shape(b14), shape(a14), "Shape (rank 14)")
        call expect_equal(pack(b14, .true.), pack(a14, .true.), "Values (rank 14)")
      end if
    end if

    a15 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a15, file=scratch()//"_rank15.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 15)") ) then
      call from_file(scratch()//"_rank15.dat", into=b15, data_shape=shape(a15), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 15)") ) then
        call expect_equal(shape(b15), shape(a15), "Shape (rank 15)")
        call expect_equal(pack(b15, .true.), pack(a15, .true.), "Values (rank 15)")
      end if
    end if

  end subroutine binary_file_i16


  subroutine text_file_i8()
    !! `to_file` then `from_file` on text files of rank 1 and 2, with each form of header.
    integer(i8), allocatable :: values(:), x1(:), y1(:), x2(:,:), y2(:,:)
    character(len=:), allocatable :: file, combo
    character(len=512)            :: msg
    integer                       :: l, f, h, st, rows

    call reset_random()
    values = test_values_of(0_i8, N_FILE)
    file = scratch()//".csv"
    msg = ""

    do l = 1, size(DELIMS)
      do f = 1, size(INT_FMTS)
          combo = settings(INT_FMTS(f), delim=DELIMS(l))

          rows = size(values)/COLS
          x1 = values
          x2 = reshape(values(1:rows*COLS), [rows, COLS])

          do h = ONE_NAME, NO_HEADER, NO_HEADER - ONE_NAME
            select case (h)
              case (ONE_NAME)
                call to_file(x1, file=file, header=["x"], &
                             delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
              case default
                call to_file(x1, file=file, &
                             delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
            end select
            if ( .not. succeeded(st, msg, "to_file"//context(1, h, combo)) ) cycle
            call from_file(file, into=y1, header=(h == ONE_NAME), &
                           delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
            if ( .not. succeeded(st, msg, "from_file"//context(1, h, combo)) ) cycle
            call expect_equal(y1, x1, "Values"//context(1, h, combo))
          end do

          do h = ONE_NAME, NO_HEADER
            select case (h)
              case (ONE_NAME)
                call to_file(x2, file=file, header=["x"], &
                             delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
              case (PER_COLUMN)
                call to_file(x2, file=file, header=COLUMN_NAMES, &
                             delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
              case (EMPTY_NAME)
                call to_file(x2, file=file, header=[""], &
                             delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
              case default
                call to_file(x2, file=file, &
                             delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
            end select
            if ( .not. succeeded(st, msg, "to_file"//context(2, h, combo)) ) cycle
            call from_file(file, into=y2, header=(h <= PER_COLUMN), &
                           delim=DELIMS(l), fmt=INT_FMTS(f), stat=st, errmsg=msg)
            if ( .not. succeeded(st, msg, "from_file"//context(2, h, combo)) ) cycle
            call expect_equal(shape(y2), shape(x2), "Shape"//context(2, h, combo))
            if ( all(shape(y2) == shape(x2)) ) then
              call expect_equal(pack(y2, .true.), pack(x2, .true.), &
                               "Values"//context(2, h, combo))
            end if
          end do
      end do
    end do
  end subroutine text_file_i8

  subroutine binary_file_i8()
    !! `to_file` then `from_file` on binary files of every rank.
    integer(i8), allocatable :: values(:), sample(:)
    integer(i8), allocatable :: a1(:), b1(:)
    integer(i8), allocatable :: a2(:,:), b2(:,:)
    integer(i8), allocatable :: a3(:,:,:), b3(:,:,:)
    integer(i8), allocatable :: a4(:,:,:,:), b4(:,:,:,:)
    integer(i8), allocatable :: a5(:,:,:,:,:), b5(:,:,:,:,:)
    integer(i8), allocatable :: a6(:,:,:,:,:,:), b6(:,:,:,:,:,:)
    integer(i8), allocatable :: a7(:,:,:,:,:,:,:), b7(:,:,:,:,:,:,:)
    integer(i8), allocatable :: a8(:,:,:,:,:,:,:,:), b8(:,:,:,:,:,:,:,:)
    integer(i8), allocatable :: a9(:,:,:,:,:,:,:,:,:), b9(:,:,:,:,:,:,:,:,:)
    integer(i8), allocatable :: a10(:,:,:,:,:,:,:,:,:,:), b10(:,:,:,:,:,:,:,:,:,:)
    integer(i8), allocatable :: a11(:,:,:,:,:,:,:,:,:,:,:), b11(:,:,:,:,:,:,:,:,:,:,:)
    integer(i8), allocatable :: a12(:,:,:,:,:,:,:,:,:,:,:,:), b12(:,:,:,:,:,:,:,:,:,:,:,:)
    integer(i8), allocatable :: a13(:,:,:,:,:,:,:,:,:,:,:,:,:), b13(:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer(i8), allocatable :: a14(:,:,:,:,:,:,:,:,:,:,:,:,:,:), b14(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer(i8), allocatable :: a15(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), b15(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    character(len=512) :: msg
    integer            :: st

    call reset_random()
    values = test_values_of(0_i8, 12)
    sample = [ values(1:12), values(size(values)-11:) ]                       ! Twelve edge and twelve random values
    msg = ""

    a1 = reshape(sample(1:3), [3])
    call to_file(a1, file=scratch()//"_rank1.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 1)") ) then
      call from_file(scratch()//"_rank1.dat", into=b1, data_shape=shape(a1), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 1)") ) then
        call expect_equal(shape(b1), shape(a1), "Shape (rank 1)")
        call expect_equal(pack(b1, .true.), pack(a1, .true.), "Values (rank 1)")
      end if
    end if

    a2 = reshape(sample(1:6), [3, 2])
    call to_file(a2, file=scratch()//"_rank2.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 2)") ) then
      call from_file(scratch()//"_rank2.dat", into=b2, data_shape=shape(a2), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 2)") ) then
        call expect_equal(shape(b2), shape(a2), "Shape (rank 2)")
        call expect_equal(pack(b2, .true.), pack(a2, .true.), "Values (rank 2)")
      end if
    end if

    a3 = reshape(sample(1:12), [3, 2, 2])
    call to_file(a3, file=scratch()//"_rank3.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 3)") ) then
      call from_file(scratch()//"_rank3.dat", into=b3, data_shape=shape(a3), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 3)") ) then
        call expect_equal(shape(b3), shape(a3), "Shape (rank 3)")
        call expect_equal(pack(b3, .true.), pack(a3, .true.), "Values (rank 3)")
      end if
    end if

    a4 = reshape(sample(1:24), [3, 2, 2, 2])
    call to_file(a4, file=scratch()//"_rank4.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 4)") ) then
      call from_file(scratch()//"_rank4.dat", into=b4, data_shape=shape(a4), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 4)") ) then
        call expect_equal(shape(b4), shape(a4), "Shape (rank 4)")
        call expect_equal(pack(b4, .true.), pack(a4, .true.), "Values (rank 4)")
      end if
    end if

    a5 = reshape(sample(1:24), [3, 2, 2, 2, 1])
    call to_file(a5, file=scratch()//"_rank5.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 5)") ) then
      call from_file(scratch()//"_rank5.dat", into=b5, data_shape=shape(a5), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 5)") ) then
        call expect_equal(shape(b5), shape(a5), "Shape (rank 5)")
        call expect_equal(pack(b5, .true.), pack(a5, .true.), "Values (rank 5)")
      end if
    end if

    a6 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1])
    call to_file(a6, file=scratch()//"_rank6.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 6)") ) then
      call from_file(scratch()//"_rank6.dat", into=b6, data_shape=shape(a6), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 6)") ) then
        call expect_equal(shape(b6), shape(a6), "Shape (rank 6)")
        call expect_equal(pack(b6, .true.), pack(a6, .true.), "Values (rank 6)")
      end if
    end if

    a7 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1])
    call to_file(a7, file=scratch()//"_rank7.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 7)") ) then
      call from_file(scratch()//"_rank7.dat", into=b7, data_shape=shape(a7), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 7)") ) then
        call expect_equal(shape(b7), shape(a7), "Shape (rank 7)")
        call expect_equal(pack(b7, .true.), pack(a7, .true.), "Values (rank 7)")
      end if
    end if

    a8 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1])
    call to_file(a8, file=scratch()//"_rank8.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 8)") ) then
      call from_file(scratch()//"_rank8.dat", into=b8, data_shape=shape(a8), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 8)") ) then
        call expect_equal(shape(b8), shape(a8), "Shape (rank 8)")
        call expect_equal(pack(b8, .true.), pack(a8, .true.), "Values (rank 8)")
      end if
    end if

    a9 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1])
    call to_file(a9, file=scratch()//"_rank9.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 9)") ) then
      call from_file(scratch()//"_rank9.dat", into=b9, data_shape=shape(a9), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 9)") ) then
        call expect_equal(shape(b9), shape(a9), "Shape (rank 9)")
        call expect_equal(pack(b9, .true.), pack(a9, .true.), "Values (rank 9)")
      end if
    end if

    a10 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1])
    call to_file(a10, file=scratch()//"_rank10.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 10)") ) then
      call from_file(scratch()//"_rank10.dat", into=b10, data_shape=shape(a10), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 10)") ) then
        call expect_equal(shape(b10), shape(a10), "Shape (rank 10)")
        call expect_equal(pack(b10, .true.), pack(a10, .true.), "Values (rank 10)")
      end if
    end if

    a11 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a11, file=scratch()//"_rank11.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 11)") ) then
      call from_file(scratch()//"_rank11.dat", into=b11, data_shape=shape(a11), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 11)") ) then
        call expect_equal(shape(b11), shape(a11), "Shape (rank 11)")
        call expect_equal(pack(b11, .true.), pack(a11, .true.), "Values (rank 11)")
      end if
    end if

    a12 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a12, file=scratch()//"_rank12.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 12)") ) then
      call from_file(scratch()//"_rank12.dat", into=b12, data_shape=shape(a12), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 12)") ) then
        call expect_equal(shape(b12), shape(a12), "Shape (rank 12)")
        call expect_equal(pack(b12, .true.), pack(a12, .true.), "Values (rank 12)")
      end if
    end if

    a13 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a13, file=scratch()//"_rank13.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 13)") ) then
      call from_file(scratch()//"_rank13.dat", into=b13, data_shape=shape(a13), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 13)") ) then
        call expect_equal(shape(b13), shape(a13), "Shape (rank 13)")
        call expect_equal(pack(b13, .true.), pack(a13, .true.), "Values (rank 13)")
      end if
    end if

    a14 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a14, file=scratch()//"_rank14.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 14)") ) then
      call from_file(scratch()//"_rank14.dat", into=b14, data_shape=shape(a14), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 14)") ) then
        call expect_equal(shape(b14), shape(a14), "Shape (rank 14)")
        call expect_equal(pack(b14, .true.), pack(a14, .true.), "Values (rank 14)")
      end if
    end if

    a15 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a15, file=scratch()//"_rank15.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 15)") ) then
      call from_file(scratch()//"_rank15.dat", into=b15, data_shape=shape(a15), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 15)") ) then
        call expect_equal(shape(b15), shape(a15), "Shape (rank 15)")
        call expect_equal(pack(b15, .true.), pack(a15, .true.), "Values (rank 15)")
      end if
    end if

  end subroutine binary_file_i8


#ifdef IOFL_HAS_R128
  subroutine text_file_r128()
    !! `to_file` then `from_file` on text files of rank 1 and 2, with each form of header.
    real(r128), allocatable :: values(:), x1(:), y1(:), x2(:,:), y2(:,:)
    character(len=:), allocatable :: file, combo
    character(len=512)            :: msg
    integer                       :: l, f, h, st, rows

    call reset_random()
    values = test_values_of(0.0_r128, N_FILE)
    file = scratch()//".csv"
    msg = ""

    do l = 1, size(LOCALES)
      do f = 1, size(REAL_FMTS)
          combo = settings(REAL_FMTS(f), locale=LOCALES(l))

          rows = size(values)/COLS
          x1 = values
          x2 = reshape(values(1:rows*COLS), [rows, COLS])

          do h = ONE_NAME, NO_HEADER, NO_HEADER - ONE_NAME
            select case (h)
              case (ONE_NAME)
                call to_file(x1, file=file, header=["x"], &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st, errmsg=msg)
              case default
                call to_file(x1, file=file, &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st, errmsg=msg)
            end select
            if ( .not. succeeded(st, msg, "to_file"//context(1, h, combo)) ) cycle
            call from_file(file, into=y1, header=(h == ONE_NAME), &
                           locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st, errmsg=msg)
            if ( .not. succeeded(st, msg, "from_file"//context(1, h, combo)) ) cycle
            call expect_identical(y1, x1, "Values"//context(1, h, combo))
          end do

          do h = ONE_NAME, NO_HEADER
            select case (h)
              case (ONE_NAME)
                call to_file(x2, file=file, header=["x"], &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st, errmsg=msg)
              case (PER_COLUMN)
                call to_file(x2, file=file, header=COLUMN_NAMES, &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st, errmsg=msg)
              case (EMPTY_NAME)
                call to_file(x2, file=file, header=[""], &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st, errmsg=msg)
              case default
                call to_file(x2, file=file, &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st, errmsg=msg)
            end select
            if ( .not. succeeded(st, msg, "to_file"//context(2, h, combo)) ) cycle
            call from_file(file, into=y2, header=(h <= PER_COLUMN), &
                           locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st, errmsg=msg)
            if ( .not. succeeded(st, msg, "from_file"//context(2, h, combo)) ) cycle
            call expect_equal(shape(y2), shape(x2), "Shape"//context(2, h, combo))
            if ( all(shape(y2) == shape(x2)) ) then
              call expect_identical(pack(y2, .true.), pack(x2, .true.), &
                               "Values"//context(2, h, combo))
            end if
          end do
      end do
    end do
  end subroutine text_file_r128

  subroutine binary_file_r128()
    !! `to_file` then `from_file` on binary files of every rank.
    real(r128), allocatable :: values(:), sample(:)
    real(r128), allocatable :: a1(:), b1(:)
    real(r128), allocatable :: a2(:,:), b2(:,:)
    real(r128), allocatable :: a3(:,:,:), b3(:,:,:)
    real(r128), allocatable :: a4(:,:,:,:), b4(:,:,:,:)
    real(r128), allocatable :: a5(:,:,:,:,:), b5(:,:,:,:,:)
    real(r128), allocatable :: a6(:,:,:,:,:,:), b6(:,:,:,:,:,:)
    real(r128), allocatable :: a7(:,:,:,:,:,:,:), b7(:,:,:,:,:,:,:)
    real(r128), allocatable :: a8(:,:,:,:,:,:,:,:), b8(:,:,:,:,:,:,:,:)
    real(r128), allocatable :: a9(:,:,:,:,:,:,:,:,:), b9(:,:,:,:,:,:,:,:,:)
    real(r128), allocatable :: a10(:,:,:,:,:,:,:,:,:,:), b10(:,:,:,:,:,:,:,:,:,:)
    real(r128), allocatable :: a11(:,:,:,:,:,:,:,:,:,:,:), b11(:,:,:,:,:,:,:,:,:,:,:)
    real(r128), allocatable :: a12(:,:,:,:,:,:,:,:,:,:,:,:), b12(:,:,:,:,:,:,:,:,:,:,:,:)
    real(r128), allocatable :: a13(:,:,:,:,:,:,:,:,:,:,:,:,:), b13(:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(r128), allocatable :: a14(:,:,:,:,:,:,:,:,:,:,:,:,:,:), b14(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(r128), allocatable :: a15(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), b15(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    character(len=512) :: msg
    integer            :: st

    call reset_random()
    values = test_values_of(0.0_r128, 12)
    sample = [ values(1:12), values(size(values)-11:) ]                       ! Twelve edge and twelve random values
    msg = ""

    a1 = reshape(sample(1:3), [3])
    call to_file(a1, file=scratch()//"_rank1.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 1)") ) then
      call from_file(scratch()//"_rank1.dat", into=b1, data_shape=shape(a1), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 1)") ) then
        call expect_equal(shape(b1), shape(a1), "Shape (rank 1)")
        call expect_identical(pack(b1, .true.), pack(a1, .true.), "Values (rank 1)")
      end if
    end if

    a2 = reshape(sample(1:6), [3, 2])
    call to_file(a2, file=scratch()//"_rank2.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 2)") ) then
      call from_file(scratch()//"_rank2.dat", into=b2, data_shape=shape(a2), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 2)") ) then
        call expect_equal(shape(b2), shape(a2), "Shape (rank 2)")
        call expect_identical(pack(b2, .true.), pack(a2, .true.), "Values (rank 2)")
      end if
    end if

    a3 = reshape(sample(1:12), [3, 2, 2])
    call to_file(a3, file=scratch()//"_rank3.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 3)") ) then
      call from_file(scratch()//"_rank3.dat", into=b3, data_shape=shape(a3), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 3)") ) then
        call expect_equal(shape(b3), shape(a3), "Shape (rank 3)")
        call expect_identical(pack(b3, .true.), pack(a3, .true.), "Values (rank 3)")
      end if
    end if

    a4 = reshape(sample(1:24), [3, 2, 2, 2])
    call to_file(a4, file=scratch()//"_rank4.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 4)") ) then
      call from_file(scratch()//"_rank4.dat", into=b4, data_shape=shape(a4), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 4)") ) then
        call expect_equal(shape(b4), shape(a4), "Shape (rank 4)")
        call expect_identical(pack(b4, .true.), pack(a4, .true.), "Values (rank 4)")
      end if
    end if

    a5 = reshape(sample(1:24), [3, 2, 2, 2, 1])
    call to_file(a5, file=scratch()//"_rank5.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 5)") ) then
      call from_file(scratch()//"_rank5.dat", into=b5, data_shape=shape(a5), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 5)") ) then
        call expect_equal(shape(b5), shape(a5), "Shape (rank 5)")
        call expect_identical(pack(b5, .true.), pack(a5, .true.), "Values (rank 5)")
      end if
    end if

    a6 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1])
    call to_file(a6, file=scratch()//"_rank6.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 6)") ) then
      call from_file(scratch()//"_rank6.dat", into=b6, data_shape=shape(a6), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 6)") ) then
        call expect_equal(shape(b6), shape(a6), "Shape (rank 6)")
        call expect_identical(pack(b6, .true.), pack(a6, .true.), "Values (rank 6)")
      end if
    end if

    a7 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1])
    call to_file(a7, file=scratch()//"_rank7.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 7)") ) then
      call from_file(scratch()//"_rank7.dat", into=b7, data_shape=shape(a7), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 7)") ) then
        call expect_equal(shape(b7), shape(a7), "Shape (rank 7)")
        call expect_identical(pack(b7, .true.), pack(a7, .true.), "Values (rank 7)")
      end if
    end if

    a8 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1])
    call to_file(a8, file=scratch()//"_rank8.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 8)") ) then
      call from_file(scratch()//"_rank8.dat", into=b8, data_shape=shape(a8), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 8)") ) then
        call expect_equal(shape(b8), shape(a8), "Shape (rank 8)")
        call expect_identical(pack(b8, .true.), pack(a8, .true.), "Values (rank 8)")
      end if
    end if

    a9 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1])
    call to_file(a9, file=scratch()//"_rank9.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 9)") ) then
      call from_file(scratch()//"_rank9.dat", into=b9, data_shape=shape(a9), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 9)") ) then
        call expect_equal(shape(b9), shape(a9), "Shape (rank 9)")
        call expect_identical(pack(b9, .true.), pack(a9, .true.), "Values (rank 9)")
      end if
    end if

    a10 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1])
    call to_file(a10, file=scratch()//"_rank10.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 10)") ) then
      call from_file(scratch()//"_rank10.dat", into=b10, data_shape=shape(a10), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 10)") ) then
        call expect_equal(shape(b10), shape(a10), "Shape (rank 10)")
        call expect_identical(pack(b10, .true.), pack(a10, .true.), "Values (rank 10)")
      end if
    end if

    a11 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a11, file=scratch()//"_rank11.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 11)") ) then
      call from_file(scratch()//"_rank11.dat", into=b11, data_shape=shape(a11), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 11)") ) then
        call expect_equal(shape(b11), shape(a11), "Shape (rank 11)")
        call expect_identical(pack(b11, .true.), pack(a11, .true.), "Values (rank 11)")
      end if
    end if

    a12 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a12, file=scratch()//"_rank12.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 12)") ) then
      call from_file(scratch()//"_rank12.dat", into=b12, data_shape=shape(a12), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 12)") ) then
        call expect_equal(shape(b12), shape(a12), "Shape (rank 12)")
        call expect_identical(pack(b12, .true.), pack(a12, .true.), "Values (rank 12)")
      end if
    end if

    a13 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a13, file=scratch()//"_rank13.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 13)") ) then
      call from_file(scratch()//"_rank13.dat", into=b13, data_shape=shape(a13), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 13)") ) then
        call expect_equal(shape(b13), shape(a13), "Shape (rank 13)")
        call expect_identical(pack(b13, .true.), pack(a13, .true.), "Values (rank 13)")
      end if
    end if

    a14 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a14, file=scratch()//"_rank14.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 14)") ) then
      call from_file(scratch()//"_rank14.dat", into=b14, data_shape=shape(a14), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 14)") ) then
        call expect_equal(shape(b14), shape(a14), "Shape (rank 14)")
        call expect_identical(pack(b14, .true.), pack(a14, .true.), "Values (rank 14)")
      end if
    end if

    a15 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a15, file=scratch()//"_rank15.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 15)") ) then
      call from_file(scratch()//"_rank15.dat", into=b15, data_shape=shape(a15), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 15)") ) then
        call expect_equal(shape(b15), shape(a15), "Shape (rank 15)")
        call expect_identical(pack(b15, .true.), pack(a15, .true.), "Values (rank 15)")
      end if
    end if

  end subroutine binary_file_r128


  subroutine small_values_fmt_f_r128()
    !! Small values survive a text file with `fmt="f"` (#61).
    real(r128), allocatable :: x(:), y(:)
    character(len=512)     :: msg
    integer                :: st

    x = [ tiny(1.0_r128), 1.0e-200_r128, -3.0e-140_r128 ]
    msg = ""
    call to_file(x, file=scratch()//".csv", fmt="f", stat=st, errmsg=msg)
    if ( .not. succeeded(st, msg, "to_file") ) return
    call from_file(scratch()//".csv", into=y, fmt="f", stat=st, errmsg=msg)
    if ( .not. succeeded(st, msg, "from_file") ) return
    call expect_identical(y, x, "Small values")
  end subroutine small_values_fmt_f_r128
#endif

  subroutine text_file_r64()
    !! `to_file` then `from_file` on text files of rank 1 and 2, with each form of header.
    real(r64), allocatable :: values(:), x1(:), y1(:), x2(:,:), y2(:,:)
    character(len=:), allocatable :: file, combo
    character(len=512)            :: msg
    integer                       :: l, f, h, st, rows

    call reset_random()
    values = test_values_of(0.0_r64, N_FILE)
    file = scratch()//".csv"
    msg = ""

    do l = 1, size(LOCALES)
      do f = 1, size(REAL_FMTS)
          combo = settings(REAL_FMTS(f), locale=LOCALES(l))

          rows = size(values)/COLS
          x1 = values
          x2 = reshape(values(1:rows*COLS), [rows, COLS])

          do h = ONE_NAME, NO_HEADER, NO_HEADER - ONE_NAME
            select case (h)
              case (ONE_NAME)
                call to_file(x1, file=file, header=["x"], &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st, errmsg=msg)
              case default
                call to_file(x1, file=file, &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st, errmsg=msg)
            end select
            if ( .not. succeeded(st, msg, "to_file"//context(1, h, combo)) ) cycle
            call from_file(file, into=y1, header=(h == ONE_NAME), &
                           locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st, errmsg=msg)
            if ( .not. succeeded(st, msg, "from_file"//context(1, h, combo)) ) cycle
            call expect_identical(y1, x1, "Values"//context(1, h, combo))
          end do

          do h = ONE_NAME, NO_HEADER
            select case (h)
              case (ONE_NAME)
                call to_file(x2, file=file, header=["x"], &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st, errmsg=msg)
              case (PER_COLUMN)
                call to_file(x2, file=file, header=COLUMN_NAMES, &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st, errmsg=msg)
              case (EMPTY_NAME)
                call to_file(x2, file=file, header=[""], &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st, errmsg=msg)
              case default
                call to_file(x2, file=file, &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st, errmsg=msg)
            end select
            if ( .not. succeeded(st, msg, "to_file"//context(2, h, combo)) ) cycle
            call from_file(file, into=y2, header=(h <= PER_COLUMN), &
                           locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st, errmsg=msg)
            if ( .not. succeeded(st, msg, "from_file"//context(2, h, combo)) ) cycle
            call expect_equal(shape(y2), shape(x2), "Shape"//context(2, h, combo))
            if ( all(shape(y2) == shape(x2)) ) then
              call expect_identical(pack(y2, .true.), pack(x2, .true.), &
                               "Values"//context(2, h, combo))
            end if
          end do
      end do
    end do
  end subroutine text_file_r64

  subroutine binary_file_r64()
    !! `to_file` then `from_file` on binary files of every rank.
    real(r64), allocatable :: values(:), sample(:)
    real(r64), allocatable :: a1(:), b1(:)
    real(r64), allocatable :: a2(:,:), b2(:,:)
    real(r64), allocatable :: a3(:,:,:), b3(:,:,:)
    real(r64), allocatable :: a4(:,:,:,:), b4(:,:,:,:)
    real(r64), allocatable :: a5(:,:,:,:,:), b5(:,:,:,:,:)
    real(r64), allocatable :: a6(:,:,:,:,:,:), b6(:,:,:,:,:,:)
    real(r64), allocatable :: a7(:,:,:,:,:,:,:), b7(:,:,:,:,:,:,:)
    real(r64), allocatable :: a8(:,:,:,:,:,:,:,:), b8(:,:,:,:,:,:,:,:)
    real(r64), allocatable :: a9(:,:,:,:,:,:,:,:,:), b9(:,:,:,:,:,:,:,:,:)
    real(r64), allocatable :: a10(:,:,:,:,:,:,:,:,:,:), b10(:,:,:,:,:,:,:,:,:,:)
    real(r64), allocatable :: a11(:,:,:,:,:,:,:,:,:,:,:), b11(:,:,:,:,:,:,:,:,:,:,:)
    real(r64), allocatable :: a12(:,:,:,:,:,:,:,:,:,:,:,:), b12(:,:,:,:,:,:,:,:,:,:,:,:)
    real(r64), allocatable :: a13(:,:,:,:,:,:,:,:,:,:,:,:,:), b13(:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(r64), allocatable :: a14(:,:,:,:,:,:,:,:,:,:,:,:,:,:), b14(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(r64), allocatable :: a15(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), b15(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    character(len=512) :: msg
    integer            :: st

    call reset_random()
    values = test_values_of(0.0_r64, 12)
    sample = [ values(1:12), values(size(values)-11:) ]                       ! Twelve edge and twelve random values
    msg = ""

    a1 = reshape(sample(1:3), [3])
    call to_file(a1, file=scratch()//"_rank1.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 1)") ) then
      call from_file(scratch()//"_rank1.dat", into=b1, data_shape=shape(a1), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 1)") ) then
        call expect_equal(shape(b1), shape(a1), "Shape (rank 1)")
        call expect_identical(pack(b1, .true.), pack(a1, .true.), "Values (rank 1)")
      end if
    end if

    a2 = reshape(sample(1:6), [3, 2])
    call to_file(a2, file=scratch()//"_rank2.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 2)") ) then
      call from_file(scratch()//"_rank2.dat", into=b2, data_shape=shape(a2), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 2)") ) then
        call expect_equal(shape(b2), shape(a2), "Shape (rank 2)")
        call expect_identical(pack(b2, .true.), pack(a2, .true.), "Values (rank 2)")
      end if
    end if

    a3 = reshape(sample(1:12), [3, 2, 2])
    call to_file(a3, file=scratch()//"_rank3.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 3)") ) then
      call from_file(scratch()//"_rank3.dat", into=b3, data_shape=shape(a3), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 3)") ) then
        call expect_equal(shape(b3), shape(a3), "Shape (rank 3)")
        call expect_identical(pack(b3, .true.), pack(a3, .true.), "Values (rank 3)")
      end if
    end if

    a4 = reshape(sample(1:24), [3, 2, 2, 2])
    call to_file(a4, file=scratch()//"_rank4.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 4)") ) then
      call from_file(scratch()//"_rank4.dat", into=b4, data_shape=shape(a4), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 4)") ) then
        call expect_equal(shape(b4), shape(a4), "Shape (rank 4)")
        call expect_identical(pack(b4, .true.), pack(a4, .true.), "Values (rank 4)")
      end if
    end if

    a5 = reshape(sample(1:24), [3, 2, 2, 2, 1])
    call to_file(a5, file=scratch()//"_rank5.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 5)") ) then
      call from_file(scratch()//"_rank5.dat", into=b5, data_shape=shape(a5), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 5)") ) then
        call expect_equal(shape(b5), shape(a5), "Shape (rank 5)")
        call expect_identical(pack(b5, .true.), pack(a5, .true.), "Values (rank 5)")
      end if
    end if

    a6 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1])
    call to_file(a6, file=scratch()//"_rank6.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 6)") ) then
      call from_file(scratch()//"_rank6.dat", into=b6, data_shape=shape(a6), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 6)") ) then
        call expect_equal(shape(b6), shape(a6), "Shape (rank 6)")
        call expect_identical(pack(b6, .true.), pack(a6, .true.), "Values (rank 6)")
      end if
    end if

    a7 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1])
    call to_file(a7, file=scratch()//"_rank7.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 7)") ) then
      call from_file(scratch()//"_rank7.dat", into=b7, data_shape=shape(a7), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 7)") ) then
        call expect_equal(shape(b7), shape(a7), "Shape (rank 7)")
        call expect_identical(pack(b7, .true.), pack(a7, .true.), "Values (rank 7)")
      end if
    end if

    a8 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1])
    call to_file(a8, file=scratch()//"_rank8.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 8)") ) then
      call from_file(scratch()//"_rank8.dat", into=b8, data_shape=shape(a8), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 8)") ) then
        call expect_equal(shape(b8), shape(a8), "Shape (rank 8)")
        call expect_identical(pack(b8, .true.), pack(a8, .true.), "Values (rank 8)")
      end if
    end if

    a9 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1])
    call to_file(a9, file=scratch()//"_rank9.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 9)") ) then
      call from_file(scratch()//"_rank9.dat", into=b9, data_shape=shape(a9), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 9)") ) then
        call expect_equal(shape(b9), shape(a9), "Shape (rank 9)")
        call expect_identical(pack(b9, .true.), pack(a9, .true.), "Values (rank 9)")
      end if
    end if

    a10 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1])
    call to_file(a10, file=scratch()//"_rank10.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 10)") ) then
      call from_file(scratch()//"_rank10.dat", into=b10, data_shape=shape(a10), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 10)") ) then
        call expect_equal(shape(b10), shape(a10), "Shape (rank 10)")
        call expect_identical(pack(b10, .true.), pack(a10, .true.), "Values (rank 10)")
      end if
    end if

    a11 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a11, file=scratch()//"_rank11.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 11)") ) then
      call from_file(scratch()//"_rank11.dat", into=b11, data_shape=shape(a11), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 11)") ) then
        call expect_equal(shape(b11), shape(a11), "Shape (rank 11)")
        call expect_identical(pack(b11, .true.), pack(a11, .true.), "Values (rank 11)")
      end if
    end if

    a12 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a12, file=scratch()//"_rank12.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 12)") ) then
      call from_file(scratch()//"_rank12.dat", into=b12, data_shape=shape(a12), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 12)") ) then
        call expect_equal(shape(b12), shape(a12), "Shape (rank 12)")
        call expect_identical(pack(b12, .true.), pack(a12, .true.), "Values (rank 12)")
      end if
    end if

    a13 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a13, file=scratch()//"_rank13.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 13)") ) then
      call from_file(scratch()//"_rank13.dat", into=b13, data_shape=shape(a13), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 13)") ) then
        call expect_equal(shape(b13), shape(a13), "Shape (rank 13)")
        call expect_identical(pack(b13, .true.), pack(a13, .true.), "Values (rank 13)")
      end if
    end if

    a14 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a14, file=scratch()//"_rank14.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 14)") ) then
      call from_file(scratch()//"_rank14.dat", into=b14, data_shape=shape(a14), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 14)") ) then
        call expect_equal(shape(b14), shape(a14), "Shape (rank 14)")
        call expect_identical(pack(b14, .true.), pack(a14, .true.), "Values (rank 14)")
      end if
    end if

    a15 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a15, file=scratch()//"_rank15.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 15)") ) then
      call from_file(scratch()//"_rank15.dat", into=b15, data_shape=shape(a15), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 15)") ) then
        call expect_equal(shape(b15), shape(a15), "Shape (rank 15)")
        call expect_identical(pack(b15, .true.), pack(a15, .true.), "Values (rank 15)")
      end if
    end if

  end subroutine binary_file_r64


  subroutine small_values_fmt_f_r64()
    !! Small values survive a text file with `fmt="f"` (#61).
    real(r64), allocatable :: x(:), y(:)
    character(len=512)     :: msg
    integer                :: st

    x = [ tiny(1.0_r64), 1.0e-200_r64, -3.0e-140_r64 ]
    msg = ""
    call to_file(x, file=scratch()//".csv", fmt="f", stat=st, errmsg=msg)
    if ( .not. succeeded(st, msg, "to_file") ) return
    call from_file(scratch()//".csv", into=y, fmt="f", stat=st, errmsg=msg)
    if ( .not. succeeded(st, msg, "from_file") ) return
    call expect_identical(y, x, "Small values")
  end subroutine small_values_fmt_f_r64

  subroutine text_file_r32()
    !! `to_file` then `from_file` on text files of rank 1 and 2, with each form of header.
    real(r32), allocatable :: values(:), x1(:), y1(:), x2(:,:), y2(:,:)
    character(len=:), allocatable :: file, combo
    character(len=512)            :: msg
    integer                       :: l, f, h, st, rows

    call reset_random()
    values = test_values_of(0.0_r32, N_FILE)
    file = scratch()//".csv"
    msg = ""

    do l = 1, size(LOCALES)
      do f = 1, size(REAL_FMTS)
          combo = settings(REAL_FMTS(f), locale=LOCALES(l))

          rows = size(values)/COLS
          x1 = values
          x2 = reshape(values(1:rows*COLS), [rows, COLS])

          do h = ONE_NAME, NO_HEADER, NO_HEADER - ONE_NAME
            select case (h)
              case (ONE_NAME)
                call to_file(x1, file=file, header=["x"], &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st, errmsg=msg)
              case default
                call to_file(x1, file=file, &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st, errmsg=msg)
            end select
            if ( .not. succeeded(st, msg, "to_file"//context(1, h, combo)) ) cycle
            call from_file(file, into=y1, header=(h == ONE_NAME), &
                           locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st, errmsg=msg)
            if ( .not. succeeded(st, msg, "from_file"//context(1, h, combo)) ) cycle
            call expect_identical(y1, x1, "Values"//context(1, h, combo))
          end do

          do h = ONE_NAME, NO_HEADER
            select case (h)
              case (ONE_NAME)
                call to_file(x2, file=file, header=["x"], &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st, errmsg=msg)
              case (PER_COLUMN)
                call to_file(x2, file=file, header=COLUMN_NAMES, &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st, errmsg=msg)
              case (EMPTY_NAME)
                call to_file(x2, file=file, header=[""], &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st, errmsg=msg)
              case default
                call to_file(x2, file=file, &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st, errmsg=msg)
            end select
            if ( .not. succeeded(st, msg, "to_file"//context(2, h, combo)) ) cycle
            call from_file(file, into=y2, header=(h <= PER_COLUMN), &
                           locale=LOCALES(l), fmt=REAL_FMTS(f), stat=st, errmsg=msg)
            if ( .not. succeeded(st, msg, "from_file"//context(2, h, combo)) ) cycle
            call expect_equal(shape(y2), shape(x2), "Shape"//context(2, h, combo))
            if ( all(shape(y2) == shape(x2)) ) then
              call expect_identical(pack(y2, .true.), pack(x2, .true.), &
                               "Values"//context(2, h, combo))
            end if
          end do
      end do
    end do
  end subroutine text_file_r32

  subroutine binary_file_r32()
    !! `to_file` then `from_file` on binary files of every rank.
    real(r32), allocatable :: values(:), sample(:)
    real(r32), allocatable :: a1(:), b1(:)
    real(r32), allocatable :: a2(:,:), b2(:,:)
    real(r32), allocatable :: a3(:,:,:), b3(:,:,:)
    real(r32), allocatable :: a4(:,:,:,:), b4(:,:,:,:)
    real(r32), allocatable :: a5(:,:,:,:,:), b5(:,:,:,:,:)
    real(r32), allocatable :: a6(:,:,:,:,:,:), b6(:,:,:,:,:,:)
    real(r32), allocatable :: a7(:,:,:,:,:,:,:), b7(:,:,:,:,:,:,:)
    real(r32), allocatable :: a8(:,:,:,:,:,:,:,:), b8(:,:,:,:,:,:,:,:)
    real(r32), allocatable :: a9(:,:,:,:,:,:,:,:,:), b9(:,:,:,:,:,:,:,:,:)
    real(r32), allocatable :: a10(:,:,:,:,:,:,:,:,:,:), b10(:,:,:,:,:,:,:,:,:,:)
    real(r32), allocatable :: a11(:,:,:,:,:,:,:,:,:,:,:), b11(:,:,:,:,:,:,:,:,:,:,:)
    real(r32), allocatable :: a12(:,:,:,:,:,:,:,:,:,:,:,:), b12(:,:,:,:,:,:,:,:,:,:,:,:)
    real(r32), allocatable :: a13(:,:,:,:,:,:,:,:,:,:,:,:,:), b13(:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(r32), allocatable :: a14(:,:,:,:,:,:,:,:,:,:,:,:,:,:), b14(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(r32), allocatable :: a15(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), b15(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    character(len=512) :: msg
    integer            :: st

    call reset_random()
    values = test_values_of(0.0_r32, 12)
    sample = [ values(1:12), values(size(values)-11:) ]                       ! Twelve edge and twelve random values
    msg = ""

    a1 = reshape(sample(1:3), [3])
    call to_file(a1, file=scratch()//"_rank1.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 1)") ) then
      call from_file(scratch()//"_rank1.dat", into=b1, data_shape=shape(a1), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 1)") ) then
        call expect_equal(shape(b1), shape(a1), "Shape (rank 1)")
        call expect_identical(pack(b1, .true.), pack(a1, .true.), "Values (rank 1)")
      end if
    end if

    a2 = reshape(sample(1:6), [3, 2])
    call to_file(a2, file=scratch()//"_rank2.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 2)") ) then
      call from_file(scratch()//"_rank2.dat", into=b2, data_shape=shape(a2), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 2)") ) then
        call expect_equal(shape(b2), shape(a2), "Shape (rank 2)")
        call expect_identical(pack(b2, .true.), pack(a2, .true.), "Values (rank 2)")
      end if
    end if

    a3 = reshape(sample(1:12), [3, 2, 2])
    call to_file(a3, file=scratch()//"_rank3.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 3)") ) then
      call from_file(scratch()//"_rank3.dat", into=b3, data_shape=shape(a3), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 3)") ) then
        call expect_equal(shape(b3), shape(a3), "Shape (rank 3)")
        call expect_identical(pack(b3, .true.), pack(a3, .true.), "Values (rank 3)")
      end if
    end if

    a4 = reshape(sample(1:24), [3, 2, 2, 2])
    call to_file(a4, file=scratch()//"_rank4.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 4)") ) then
      call from_file(scratch()//"_rank4.dat", into=b4, data_shape=shape(a4), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 4)") ) then
        call expect_equal(shape(b4), shape(a4), "Shape (rank 4)")
        call expect_identical(pack(b4, .true.), pack(a4, .true.), "Values (rank 4)")
      end if
    end if

    a5 = reshape(sample(1:24), [3, 2, 2, 2, 1])
    call to_file(a5, file=scratch()//"_rank5.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 5)") ) then
      call from_file(scratch()//"_rank5.dat", into=b5, data_shape=shape(a5), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 5)") ) then
        call expect_equal(shape(b5), shape(a5), "Shape (rank 5)")
        call expect_identical(pack(b5, .true.), pack(a5, .true.), "Values (rank 5)")
      end if
    end if

    a6 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1])
    call to_file(a6, file=scratch()//"_rank6.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 6)") ) then
      call from_file(scratch()//"_rank6.dat", into=b6, data_shape=shape(a6), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 6)") ) then
        call expect_equal(shape(b6), shape(a6), "Shape (rank 6)")
        call expect_identical(pack(b6, .true.), pack(a6, .true.), "Values (rank 6)")
      end if
    end if

    a7 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1])
    call to_file(a7, file=scratch()//"_rank7.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 7)") ) then
      call from_file(scratch()//"_rank7.dat", into=b7, data_shape=shape(a7), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 7)") ) then
        call expect_equal(shape(b7), shape(a7), "Shape (rank 7)")
        call expect_identical(pack(b7, .true.), pack(a7, .true.), "Values (rank 7)")
      end if
    end if

    a8 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1])
    call to_file(a8, file=scratch()//"_rank8.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 8)") ) then
      call from_file(scratch()//"_rank8.dat", into=b8, data_shape=shape(a8), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 8)") ) then
        call expect_equal(shape(b8), shape(a8), "Shape (rank 8)")
        call expect_identical(pack(b8, .true.), pack(a8, .true.), "Values (rank 8)")
      end if
    end if

    a9 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1])
    call to_file(a9, file=scratch()//"_rank9.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 9)") ) then
      call from_file(scratch()//"_rank9.dat", into=b9, data_shape=shape(a9), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 9)") ) then
        call expect_equal(shape(b9), shape(a9), "Shape (rank 9)")
        call expect_identical(pack(b9, .true.), pack(a9, .true.), "Values (rank 9)")
      end if
    end if

    a10 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1])
    call to_file(a10, file=scratch()//"_rank10.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 10)") ) then
      call from_file(scratch()//"_rank10.dat", into=b10, data_shape=shape(a10), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 10)") ) then
        call expect_equal(shape(b10), shape(a10), "Shape (rank 10)")
        call expect_identical(pack(b10, .true.), pack(a10, .true.), "Values (rank 10)")
      end if
    end if

    a11 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a11, file=scratch()//"_rank11.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 11)") ) then
      call from_file(scratch()//"_rank11.dat", into=b11, data_shape=shape(a11), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 11)") ) then
        call expect_equal(shape(b11), shape(a11), "Shape (rank 11)")
        call expect_identical(pack(b11, .true.), pack(a11, .true.), "Values (rank 11)")
      end if
    end if

    a12 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a12, file=scratch()//"_rank12.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 12)") ) then
      call from_file(scratch()//"_rank12.dat", into=b12, data_shape=shape(a12), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 12)") ) then
        call expect_equal(shape(b12), shape(a12), "Shape (rank 12)")
        call expect_identical(pack(b12, .true.), pack(a12, .true.), "Values (rank 12)")
      end if
    end if

    a13 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a13, file=scratch()//"_rank13.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 13)") ) then
      call from_file(scratch()//"_rank13.dat", into=b13, data_shape=shape(a13), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 13)") ) then
        call expect_equal(shape(b13), shape(a13), "Shape (rank 13)")
        call expect_identical(pack(b13, .true.), pack(a13, .true.), "Values (rank 13)")
      end if
    end if

    a14 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a14, file=scratch()//"_rank14.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 14)") ) then
      call from_file(scratch()//"_rank14.dat", into=b14, data_shape=shape(a14), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 14)") ) then
        call expect_equal(shape(b14), shape(a14), "Shape (rank 14)")
        call expect_identical(pack(b14, .true.), pack(a14, .true.), "Values (rank 14)")
      end if
    end if

    a15 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a15, file=scratch()//"_rank15.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 15)") ) then
      call from_file(scratch()//"_rank15.dat", into=b15, data_shape=shape(a15), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 15)") ) then
        call expect_equal(shape(b15), shape(a15), "Shape (rank 15)")
        call expect_identical(pack(b15, .true.), pack(a15, .true.), "Values (rank 15)")
      end if
    end if

  end subroutine binary_file_r32


#ifdef IOFL_HAS_R128
  subroutine text_file_c128()
    !! `to_file` then `from_file` on text files of rank 1 and 2, with each form of header.
    complex(r128), allocatable :: values(:), x1(:), y1(:), x2(:,:), y2(:,:)
    character(len=:), allocatable :: file, combo
    character(len=512)            :: msg
    integer                       :: m, l, f, h, st, rows

    call reset_random()
    values = test_values_of((0.0_r128,0.0_r128), N_FILE)
    file = scratch()//".csv"
    msg = ""

    do m = 1, size(IMS)
      do l = 1, size(LOCALES)
        do f = 1, size(REAL_FMTS)
          combo = settings(REAL_FMTS(f), locale=LOCALES(l), im=IMS(m))

          rows = size(values)/COLS
          x1 = values
          x2 = reshape(values(1:rows*COLS), [rows, COLS])

          do h = ONE_NAME, NO_HEADER, NO_HEADER - ONE_NAME
            select case (h)
              case (ONE_NAME)
                call to_file(x1, file=file, header=["x"], &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st, errmsg=msg)
              case default
                call to_file(x1, file=file, &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st, errmsg=msg)
            end select
            if ( .not. succeeded(st, msg, "to_file"//context(1, h, combo)) ) cycle
            call from_file(file, into=y1, header=(h == ONE_NAME), &
                           locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st, errmsg=msg)
            if ( .not. succeeded(st, msg, "from_file"//context(1, h, combo)) ) cycle
            call expect_identical(y1, x1, "Values"//context(1, h, combo))
          end do

          do h = ONE_NAME, NO_HEADER
            select case (h)
              case (ONE_NAME)
                call to_file(x2, file=file, header=["x"], &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st, errmsg=msg)
              case (PER_COLUMN)
                call to_file(x2, file=file, header=COLUMN_NAMES, &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st, errmsg=msg)
              case (EMPTY_NAME)
                call to_file(x2, file=file, header=[""], &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st, errmsg=msg)
              case default
                call to_file(x2, file=file, &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st, errmsg=msg)
            end select
            if ( .not. succeeded(st, msg, "to_file"//context(2, h, combo)) ) cycle
            call from_file(file, into=y2, header=(h <= PER_COLUMN), &
                           locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st, errmsg=msg)
            if ( .not. succeeded(st, msg, "from_file"//context(2, h, combo)) ) cycle
            call expect_equal(shape(y2), shape(x2), "Shape"//context(2, h, combo))
            if ( all(shape(y2) == shape(x2)) ) then
              call expect_identical(pack(y2, .true.), pack(x2, .true.), &
                               "Values"//context(2, h, combo))
            end if
          end do
        end do
      end do
    end do
  end subroutine text_file_c128

  subroutine binary_file_c128()
    !! `to_file` then `from_file` on binary files of every rank.
    complex(r128), allocatable :: values(:), sample(:)
    complex(r128), allocatable :: a1(:), b1(:)
    complex(r128), allocatable :: a2(:,:), b2(:,:)
    complex(r128), allocatable :: a3(:,:,:), b3(:,:,:)
    complex(r128), allocatable :: a4(:,:,:,:), b4(:,:,:,:)
    complex(r128), allocatable :: a5(:,:,:,:,:), b5(:,:,:,:,:)
    complex(r128), allocatable :: a6(:,:,:,:,:,:), b6(:,:,:,:,:,:)
    complex(r128), allocatable :: a7(:,:,:,:,:,:,:), b7(:,:,:,:,:,:,:)
    complex(r128), allocatable :: a8(:,:,:,:,:,:,:,:), b8(:,:,:,:,:,:,:,:)
    complex(r128), allocatable :: a9(:,:,:,:,:,:,:,:,:), b9(:,:,:,:,:,:,:,:,:)
    complex(r128), allocatable :: a10(:,:,:,:,:,:,:,:,:,:), b10(:,:,:,:,:,:,:,:,:,:)
    complex(r128), allocatable :: a11(:,:,:,:,:,:,:,:,:,:,:), b11(:,:,:,:,:,:,:,:,:,:,:)
    complex(r128), allocatable :: a12(:,:,:,:,:,:,:,:,:,:,:,:), b12(:,:,:,:,:,:,:,:,:,:,:,:)
    complex(r128), allocatable :: a13(:,:,:,:,:,:,:,:,:,:,:,:,:), b13(:,:,:,:,:,:,:,:,:,:,:,:,:)
    complex(r128), allocatable :: a14(:,:,:,:,:,:,:,:,:,:,:,:,:,:), b14(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    complex(r128), allocatable :: a15(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), b15(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    character(len=512) :: msg
    integer            :: st

    call reset_random()
    values = test_values_of((0.0_r128,0.0_r128), 12)
    sample = [ values(1:12), values(size(values)-11:) ]                       ! Twelve edge and twelve random values
    msg = ""

    a1 = reshape(sample(1:3), [3])
    call to_file(a1, file=scratch()//"_rank1.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 1)") ) then
      call from_file(scratch()//"_rank1.dat", into=b1, data_shape=shape(a1), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 1)") ) then
        call expect_equal(shape(b1), shape(a1), "Shape (rank 1)")
        call expect_identical(pack(b1, .true.), pack(a1, .true.), "Values (rank 1)")
      end if
    end if

    a2 = reshape(sample(1:6), [3, 2])
    call to_file(a2, file=scratch()//"_rank2.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 2)") ) then
      call from_file(scratch()//"_rank2.dat", into=b2, data_shape=shape(a2), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 2)") ) then
        call expect_equal(shape(b2), shape(a2), "Shape (rank 2)")
        call expect_identical(pack(b2, .true.), pack(a2, .true.), "Values (rank 2)")
      end if
    end if

    a3 = reshape(sample(1:12), [3, 2, 2])
    call to_file(a3, file=scratch()//"_rank3.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 3)") ) then
      call from_file(scratch()//"_rank3.dat", into=b3, data_shape=shape(a3), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 3)") ) then
        call expect_equal(shape(b3), shape(a3), "Shape (rank 3)")
        call expect_identical(pack(b3, .true.), pack(a3, .true.), "Values (rank 3)")
      end if
    end if

    a4 = reshape(sample(1:24), [3, 2, 2, 2])
    call to_file(a4, file=scratch()//"_rank4.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 4)") ) then
      call from_file(scratch()//"_rank4.dat", into=b4, data_shape=shape(a4), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 4)") ) then
        call expect_equal(shape(b4), shape(a4), "Shape (rank 4)")
        call expect_identical(pack(b4, .true.), pack(a4, .true.), "Values (rank 4)")
      end if
    end if

    a5 = reshape(sample(1:24), [3, 2, 2, 2, 1])
    call to_file(a5, file=scratch()//"_rank5.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 5)") ) then
      call from_file(scratch()//"_rank5.dat", into=b5, data_shape=shape(a5), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 5)") ) then
        call expect_equal(shape(b5), shape(a5), "Shape (rank 5)")
        call expect_identical(pack(b5, .true.), pack(a5, .true.), "Values (rank 5)")
      end if
    end if

    a6 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1])
    call to_file(a6, file=scratch()//"_rank6.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 6)") ) then
      call from_file(scratch()//"_rank6.dat", into=b6, data_shape=shape(a6), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 6)") ) then
        call expect_equal(shape(b6), shape(a6), "Shape (rank 6)")
        call expect_identical(pack(b6, .true.), pack(a6, .true.), "Values (rank 6)")
      end if
    end if

    a7 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1])
    call to_file(a7, file=scratch()//"_rank7.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 7)") ) then
      call from_file(scratch()//"_rank7.dat", into=b7, data_shape=shape(a7), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 7)") ) then
        call expect_equal(shape(b7), shape(a7), "Shape (rank 7)")
        call expect_identical(pack(b7, .true.), pack(a7, .true.), "Values (rank 7)")
      end if
    end if

    a8 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1])
    call to_file(a8, file=scratch()//"_rank8.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 8)") ) then
      call from_file(scratch()//"_rank8.dat", into=b8, data_shape=shape(a8), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 8)") ) then
        call expect_equal(shape(b8), shape(a8), "Shape (rank 8)")
        call expect_identical(pack(b8, .true.), pack(a8, .true.), "Values (rank 8)")
      end if
    end if

    a9 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1])
    call to_file(a9, file=scratch()//"_rank9.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 9)") ) then
      call from_file(scratch()//"_rank9.dat", into=b9, data_shape=shape(a9), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 9)") ) then
        call expect_equal(shape(b9), shape(a9), "Shape (rank 9)")
        call expect_identical(pack(b9, .true.), pack(a9, .true.), "Values (rank 9)")
      end if
    end if

    a10 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1])
    call to_file(a10, file=scratch()//"_rank10.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 10)") ) then
      call from_file(scratch()//"_rank10.dat", into=b10, data_shape=shape(a10), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 10)") ) then
        call expect_equal(shape(b10), shape(a10), "Shape (rank 10)")
        call expect_identical(pack(b10, .true.), pack(a10, .true.), "Values (rank 10)")
      end if
    end if

    a11 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a11, file=scratch()//"_rank11.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 11)") ) then
      call from_file(scratch()//"_rank11.dat", into=b11, data_shape=shape(a11), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 11)") ) then
        call expect_equal(shape(b11), shape(a11), "Shape (rank 11)")
        call expect_identical(pack(b11, .true.), pack(a11, .true.), "Values (rank 11)")
      end if
    end if

    a12 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a12, file=scratch()//"_rank12.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 12)") ) then
      call from_file(scratch()//"_rank12.dat", into=b12, data_shape=shape(a12), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 12)") ) then
        call expect_equal(shape(b12), shape(a12), "Shape (rank 12)")
        call expect_identical(pack(b12, .true.), pack(a12, .true.), "Values (rank 12)")
      end if
    end if

    a13 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a13, file=scratch()//"_rank13.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 13)") ) then
      call from_file(scratch()//"_rank13.dat", into=b13, data_shape=shape(a13), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 13)") ) then
        call expect_equal(shape(b13), shape(a13), "Shape (rank 13)")
        call expect_identical(pack(b13, .true.), pack(a13, .true.), "Values (rank 13)")
      end if
    end if

    a14 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a14, file=scratch()//"_rank14.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 14)") ) then
      call from_file(scratch()//"_rank14.dat", into=b14, data_shape=shape(a14), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 14)") ) then
        call expect_equal(shape(b14), shape(a14), "Shape (rank 14)")
        call expect_identical(pack(b14, .true.), pack(a14, .true.), "Values (rank 14)")
      end if
    end if

    a15 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a15, file=scratch()//"_rank15.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 15)") ) then
      call from_file(scratch()//"_rank15.dat", into=b15, data_shape=shape(a15), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 15)") ) then
        call expect_equal(shape(b15), shape(a15), "Shape (rank 15)")
        call expect_identical(pack(b15, .true.), pack(a15, .true.), "Values (rank 15)")
      end if
    end if

  end subroutine binary_file_c128


  subroutine small_values_fmt_f_c128()
    !! Small parts survive a text file with `fmt="f"` (#61).
    complex(r128), allocatable :: x(:), y(:)
    character(len=512)        :: msg
    integer                   :: st

    x = [ cmplx(tiny(1.0_r128), 1.0e-200_r128, kind=r128), cmplx(1.0_r128, -3.0e-140_r128, kind=r128) ]
    msg = ""
    call to_file(x, file=scratch()//".csv", fmt="f", stat=st, errmsg=msg)
    if ( .not. succeeded(st, msg, "to_file") ) return
    call from_file(scratch()//".csv", into=y, fmt="f", stat=st, errmsg=msg)
    if ( .not. succeeded(st, msg, "from_file") ) return
    call expect_identical(y, x, "Small parts")
  end subroutine small_values_fmt_f_c128
#endif

  subroutine text_file_c64()
    !! `to_file` then `from_file` on text files of rank 1 and 2, with each form of header.
    complex(r64), allocatable :: values(:), x1(:), y1(:), x2(:,:), y2(:,:)
    character(len=:), allocatable :: file, combo
    character(len=512)            :: msg
    integer                       :: m, l, f, h, st, rows

    call reset_random()
    values = test_values_of((0.0_r64,0.0_r64), N_FILE)
    file = scratch()//".csv"
    msg = ""

    do m = 1, size(IMS)
      do l = 1, size(LOCALES)
        do f = 1, size(REAL_FMTS)
          combo = settings(REAL_FMTS(f), locale=LOCALES(l), im=IMS(m))

          rows = size(values)/COLS
          x1 = values
          x2 = reshape(values(1:rows*COLS), [rows, COLS])

          do h = ONE_NAME, NO_HEADER, NO_HEADER - ONE_NAME
            select case (h)
              case (ONE_NAME)
                call to_file(x1, file=file, header=["x"], &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st, errmsg=msg)
              case default
                call to_file(x1, file=file, &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st, errmsg=msg)
            end select
            if ( .not. succeeded(st, msg, "to_file"//context(1, h, combo)) ) cycle
            call from_file(file, into=y1, header=(h == ONE_NAME), &
                           locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st, errmsg=msg)
            if ( .not. succeeded(st, msg, "from_file"//context(1, h, combo)) ) cycle
            call expect_identical(y1, x1, "Values"//context(1, h, combo))
          end do

          do h = ONE_NAME, NO_HEADER
            select case (h)
              case (ONE_NAME)
                call to_file(x2, file=file, header=["x"], &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st, errmsg=msg)
              case (PER_COLUMN)
                call to_file(x2, file=file, header=COLUMN_NAMES, &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st, errmsg=msg)
              case (EMPTY_NAME)
                call to_file(x2, file=file, header=[""], &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st, errmsg=msg)
              case default
                call to_file(x2, file=file, &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st, errmsg=msg)
            end select
            if ( .not. succeeded(st, msg, "to_file"//context(2, h, combo)) ) cycle
            call from_file(file, into=y2, header=(h <= PER_COLUMN), &
                           locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st, errmsg=msg)
            if ( .not. succeeded(st, msg, "from_file"//context(2, h, combo)) ) cycle
            call expect_equal(shape(y2), shape(x2), "Shape"//context(2, h, combo))
            if ( all(shape(y2) == shape(x2)) ) then
              call expect_identical(pack(y2, .true.), pack(x2, .true.), &
                               "Values"//context(2, h, combo))
            end if
          end do
        end do
      end do
    end do
  end subroutine text_file_c64

  subroutine binary_file_c64()
    !! `to_file` then `from_file` on binary files of every rank.
    complex(r64), allocatable :: values(:), sample(:)
    complex(r64), allocatable :: a1(:), b1(:)
    complex(r64), allocatable :: a2(:,:), b2(:,:)
    complex(r64), allocatable :: a3(:,:,:), b3(:,:,:)
    complex(r64), allocatable :: a4(:,:,:,:), b4(:,:,:,:)
    complex(r64), allocatable :: a5(:,:,:,:,:), b5(:,:,:,:,:)
    complex(r64), allocatable :: a6(:,:,:,:,:,:), b6(:,:,:,:,:,:)
    complex(r64), allocatable :: a7(:,:,:,:,:,:,:), b7(:,:,:,:,:,:,:)
    complex(r64), allocatable :: a8(:,:,:,:,:,:,:,:), b8(:,:,:,:,:,:,:,:)
    complex(r64), allocatable :: a9(:,:,:,:,:,:,:,:,:), b9(:,:,:,:,:,:,:,:,:)
    complex(r64), allocatable :: a10(:,:,:,:,:,:,:,:,:,:), b10(:,:,:,:,:,:,:,:,:,:)
    complex(r64), allocatable :: a11(:,:,:,:,:,:,:,:,:,:,:), b11(:,:,:,:,:,:,:,:,:,:,:)
    complex(r64), allocatable :: a12(:,:,:,:,:,:,:,:,:,:,:,:), b12(:,:,:,:,:,:,:,:,:,:,:,:)
    complex(r64), allocatable :: a13(:,:,:,:,:,:,:,:,:,:,:,:,:), b13(:,:,:,:,:,:,:,:,:,:,:,:,:)
    complex(r64), allocatable :: a14(:,:,:,:,:,:,:,:,:,:,:,:,:,:), b14(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    complex(r64), allocatable :: a15(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), b15(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    character(len=512) :: msg
    integer            :: st

    call reset_random()
    values = test_values_of((0.0_r64,0.0_r64), 12)
    sample = [ values(1:12), values(size(values)-11:) ]                       ! Twelve edge and twelve random values
    msg = ""

    a1 = reshape(sample(1:3), [3])
    call to_file(a1, file=scratch()//"_rank1.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 1)") ) then
      call from_file(scratch()//"_rank1.dat", into=b1, data_shape=shape(a1), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 1)") ) then
        call expect_equal(shape(b1), shape(a1), "Shape (rank 1)")
        call expect_identical(pack(b1, .true.), pack(a1, .true.), "Values (rank 1)")
      end if
    end if

    a2 = reshape(sample(1:6), [3, 2])
    call to_file(a2, file=scratch()//"_rank2.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 2)") ) then
      call from_file(scratch()//"_rank2.dat", into=b2, data_shape=shape(a2), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 2)") ) then
        call expect_equal(shape(b2), shape(a2), "Shape (rank 2)")
        call expect_identical(pack(b2, .true.), pack(a2, .true.), "Values (rank 2)")
      end if
    end if

    a3 = reshape(sample(1:12), [3, 2, 2])
    call to_file(a3, file=scratch()//"_rank3.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 3)") ) then
      call from_file(scratch()//"_rank3.dat", into=b3, data_shape=shape(a3), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 3)") ) then
        call expect_equal(shape(b3), shape(a3), "Shape (rank 3)")
        call expect_identical(pack(b3, .true.), pack(a3, .true.), "Values (rank 3)")
      end if
    end if

    a4 = reshape(sample(1:24), [3, 2, 2, 2])
    call to_file(a4, file=scratch()//"_rank4.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 4)") ) then
      call from_file(scratch()//"_rank4.dat", into=b4, data_shape=shape(a4), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 4)") ) then
        call expect_equal(shape(b4), shape(a4), "Shape (rank 4)")
        call expect_identical(pack(b4, .true.), pack(a4, .true.), "Values (rank 4)")
      end if
    end if

    a5 = reshape(sample(1:24), [3, 2, 2, 2, 1])
    call to_file(a5, file=scratch()//"_rank5.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 5)") ) then
      call from_file(scratch()//"_rank5.dat", into=b5, data_shape=shape(a5), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 5)") ) then
        call expect_equal(shape(b5), shape(a5), "Shape (rank 5)")
        call expect_identical(pack(b5, .true.), pack(a5, .true.), "Values (rank 5)")
      end if
    end if

    a6 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1])
    call to_file(a6, file=scratch()//"_rank6.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 6)") ) then
      call from_file(scratch()//"_rank6.dat", into=b6, data_shape=shape(a6), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 6)") ) then
        call expect_equal(shape(b6), shape(a6), "Shape (rank 6)")
        call expect_identical(pack(b6, .true.), pack(a6, .true.), "Values (rank 6)")
      end if
    end if

    a7 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1])
    call to_file(a7, file=scratch()//"_rank7.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 7)") ) then
      call from_file(scratch()//"_rank7.dat", into=b7, data_shape=shape(a7), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 7)") ) then
        call expect_equal(shape(b7), shape(a7), "Shape (rank 7)")
        call expect_identical(pack(b7, .true.), pack(a7, .true.), "Values (rank 7)")
      end if
    end if

    a8 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1])
    call to_file(a8, file=scratch()//"_rank8.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 8)") ) then
      call from_file(scratch()//"_rank8.dat", into=b8, data_shape=shape(a8), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 8)") ) then
        call expect_equal(shape(b8), shape(a8), "Shape (rank 8)")
        call expect_identical(pack(b8, .true.), pack(a8, .true.), "Values (rank 8)")
      end if
    end if

    a9 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1])
    call to_file(a9, file=scratch()//"_rank9.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 9)") ) then
      call from_file(scratch()//"_rank9.dat", into=b9, data_shape=shape(a9), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 9)") ) then
        call expect_equal(shape(b9), shape(a9), "Shape (rank 9)")
        call expect_identical(pack(b9, .true.), pack(a9, .true.), "Values (rank 9)")
      end if
    end if

    a10 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1])
    call to_file(a10, file=scratch()//"_rank10.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 10)") ) then
      call from_file(scratch()//"_rank10.dat", into=b10, data_shape=shape(a10), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 10)") ) then
        call expect_equal(shape(b10), shape(a10), "Shape (rank 10)")
        call expect_identical(pack(b10, .true.), pack(a10, .true.), "Values (rank 10)")
      end if
    end if

    a11 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a11, file=scratch()//"_rank11.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 11)") ) then
      call from_file(scratch()//"_rank11.dat", into=b11, data_shape=shape(a11), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 11)") ) then
        call expect_equal(shape(b11), shape(a11), "Shape (rank 11)")
        call expect_identical(pack(b11, .true.), pack(a11, .true.), "Values (rank 11)")
      end if
    end if

    a12 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a12, file=scratch()//"_rank12.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 12)") ) then
      call from_file(scratch()//"_rank12.dat", into=b12, data_shape=shape(a12), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 12)") ) then
        call expect_equal(shape(b12), shape(a12), "Shape (rank 12)")
        call expect_identical(pack(b12, .true.), pack(a12, .true.), "Values (rank 12)")
      end if
    end if

    a13 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a13, file=scratch()//"_rank13.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 13)") ) then
      call from_file(scratch()//"_rank13.dat", into=b13, data_shape=shape(a13), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 13)") ) then
        call expect_equal(shape(b13), shape(a13), "Shape (rank 13)")
        call expect_identical(pack(b13, .true.), pack(a13, .true.), "Values (rank 13)")
      end if
    end if

    a14 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a14, file=scratch()//"_rank14.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 14)") ) then
      call from_file(scratch()//"_rank14.dat", into=b14, data_shape=shape(a14), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 14)") ) then
        call expect_equal(shape(b14), shape(a14), "Shape (rank 14)")
        call expect_identical(pack(b14, .true.), pack(a14, .true.), "Values (rank 14)")
      end if
    end if

    a15 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a15, file=scratch()//"_rank15.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 15)") ) then
      call from_file(scratch()//"_rank15.dat", into=b15, data_shape=shape(a15), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 15)") ) then
        call expect_equal(shape(b15), shape(a15), "Shape (rank 15)")
        call expect_identical(pack(b15, .true.), pack(a15, .true.), "Values (rank 15)")
      end if
    end if

  end subroutine binary_file_c64


  subroutine small_values_fmt_f_c64()
    !! Small parts survive a text file with `fmt="f"` (#61).
    complex(r64), allocatable :: x(:), y(:)
    character(len=512)        :: msg
    integer                   :: st

    x = [ cmplx(tiny(1.0_r64), 1.0e-200_r64, kind=r64), cmplx(1.0_r64, -3.0e-140_r64, kind=r64) ]
    msg = ""
    call to_file(x, file=scratch()//".csv", fmt="f", stat=st, errmsg=msg)
    if ( .not. succeeded(st, msg, "to_file") ) return
    call from_file(scratch()//".csv", into=y, fmt="f", stat=st, errmsg=msg)
    if ( .not. succeeded(st, msg, "from_file") ) return
    call expect_identical(y, x, "Small parts")
  end subroutine small_values_fmt_f_c64

  subroutine text_file_c32()
    !! `to_file` then `from_file` on text files of rank 1 and 2, with each form of header.
    complex(r32), allocatable :: values(:), x1(:), y1(:), x2(:,:), y2(:,:)
    character(len=:), allocatable :: file, combo
    character(len=512)            :: msg
    integer                       :: m, l, f, h, st, rows

    call reset_random()
    values = test_values_of((0.0_r32,0.0_r32), N_FILE)
    file = scratch()//".csv"
    msg = ""

    do m = 1, size(IMS)
      do l = 1, size(LOCALES)
        do f = 1, size(REAL_FMTS)
          combo = settings(REAL_FMTS(f), locale=LOCALES(l), im=IMS(m))

          rows = size(values)/COLS
          x1 = values
          x2 = reshape(values(1:rows*COLS), [rows, COLS])

          do h = ONE_NAME, NO_HEADER, NO_HEADER - ONE_NAME
            select case (h)
              case (ONE_NAME)
                call to_file(x1, file=file, header=["x"], &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st, errmsg=msg)
              case default
                call to_file(x1, file=file, &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st, errmsg=msg)
            end select
            if ( .not. succeeded(st, msg, "to_file"//context(1, h, combo)) ) cycle
            call from_file(file, into=y1, header=(h == ONE_NAME), &
                           locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st, errmsg=msg)
            if ( .not. succeeded(st, msg, "from_file"//context(1, h, combo)) ) cycle
            call expect_identical(y1, x1, "Values"//context(1, h, combo))
          end do

          do h = ONE_NAME, NO_HEADER
            select case (h)
              case (ONE_NAME)
                call to_file(x2, file=file, header=["x"], &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st, errmsg=msg)
              case (PER_COLUMN)
                call to_file(x2, file=file, header=COLUMN_NAMES, &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st, errmsg=msg)
              case (EMPTY_NAME)
                call to_file(x2, file=file, header=[""], &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st, errmsg=msg)
              case default
                call to_file(x2, file=file, &
                             locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st, errmsg=msg)
            end select
            if ( .not. succeeded(st, msg, "to_file"//context(2, h, combo)) ) cycle
            call from_file(file, into=y2, header=(h <= PER_COLUMN), &
                           locale=LOCALES(l), fmt=REAL_FMTS(f), im=trim(IMS(m)), stat=st, errmsg=msg)
            if ( .not. succeeded(st, msg, "from_file"//context(2, h, combo)) ) cycle
            call expect_equal(shape(y2), shape(x2), "Shape"//context(2, h, combo))
            if ( all(shape(y2) == shape(x2)) ) then
              call expect_identical(pack(y2, .true.), pack(x2, .true.), &
                               "Values"//context(2, h, combo))
            end if
          end do
        end do
      end do
    end do
  end subroutine text_file_c32

  subroutine binary_file_c32()
    !! `to_file` then `from_file` on binary files of every rank.
    complex(r32), allocatable :: values(:), sample(:)
    complex(r32), allocatable :: a1(:), b1(:)
    complex(r32), allocatable :: a2(:,:), b2(:,:)
    complex(r32), allocatable :: a3(:,:,:), b3(:,:,:)
    complex(r32), allocatable :: a4(:,:,:,:), b4(:,:,:,:)
    complex(r32), allocatable :: a5(:,:,:,:,:), b5(:,:,:,:,:)
    complex(r32), allocatable :: a6(:,:,:,:,:,:), b6(:,:,:,:,:,:)
    complex(r32), allocatable :: a7(:,:,:,:,:,:,:), b7(:,:,:,:,:,:,:)
    complex(r32), allocatable :: a8(:,:,:,:,:,:,:,:), b8(:,:,:,:,:,:,:,:)
    complex(r32), allocatable :: a9(:,:,:,:,:,:,:,:,:), b9(:,:,:,:,:,:,:,:,:)
    complex(r32), allocatable :: a10(:,:,:,:,:,:,:,:,:,:), b10(:,:,:,:,:,:,:,:,:,:)
    complex(r32), allocatable :: a11(:,:,:,:,:,:,:,:,:,:,:), b11(:,:,:,:,:,:,:,:,:,:,:)
    complex(r32), allocatable :: a12(:,:,:,:,:,:,:,:,:,:,:,:), b12(:,:,:,:,:,:,:,:,:,:,:,:)
    complex(r32), allocatable :: a13(:,:,:,:,:,:,:,:,:,:,:,:,:), b13(:,:,:,:,:,:,:,:,:,:,:,:,:)
    complex(r32), allocatable :: a14(:,:,:,:,:,:,:,:,:,:,:,:,:,:), b14(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    complex(r32), allocatable :: a15(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), b15(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    character(len=512) :: msg
    integer            :: st

    call reset_random()
    values = test_values_of((0.0_r32,0.0_r32), 12)
    sample = [ values(1:12), values(size(values)-11:) ]                       ! Twelve edge and twelve random values
    msg = ""

    a1 = reshape(sample(1:3), [3])
    call to_file(a1, file=scratch()//"_rank1.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 1)") ) then
      call from_file(scratch()//"_rank1.dat", into=b1, data_shape=shape(a1), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 1)") ) then
        call expect_equal(shape(b1), shape(a1), "Shape (rank 1)")
        call expect_identical(pack(b1, .true.), pack(a1, .true.), "Values (rank 1)")
      end if
    end if

    a2 = reshape(sample(1:6), [3, 2])
    call to_file(a2, file=scratch()//"_rank2.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 2)") ) then
      call from_file(scratch()//"_rank2.dat", into=b2, data_shape=shape(a2), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 2)") ) then
        call expect_equal(shape(b2), shape(a2), "Shape (rank 2)")
        call expect_identical(pack(b2, .true.), pack(a2, .true.), "Values (rank 2)")
      end if
    end if

    a3 = reshape(sample(1:12), [3, 2, 2])
    call to_file(a3, file=scratch()//"_rank3.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 3)") ) then
      call from_file(scratch()//"_rank3.dat", into=b3, data_shape=shape(a3), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 3)") ) then
        call expect_equal(shape(b3), shape(a3), "Shape (rank 3)")
        call expect_identical(pack(b3, .true.), pack(a3, .true.), "Values (rank 3)")
      end if
    end if

    a4 = reshape(sample(1:24), [3, 2, 2, 2])
    call to_file(a4, file=scratch()//"_rank4.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 4)") ) then
      call from_file(scratch()//"_rank4.dat", into=b4, data_shape=shape(a4), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 4)") ) then
        call expect_equal(shape(b4), shape(a4), "Shape (rank 4)")
        call expect_identical(pack(b4, .true.), pack(a4, .true.), "Values (rank 4)")
      end if
    end if

    a5 = reshape(sample(1:24), [3, 2, 2, 2, 1])
    call to_file(a5, file=scratch()//"_rank5.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 5)") ) then
      call from_file(scratch()//"_rank5.dat", into=b5, data_shape=shape(a5), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 5)") ) then
        call expect_equal(shape(b5), shape(a5), "Shape (rank 5)")
        call expect_identical(pack(b5, .true.), pack(a5, .true.), "Values (rank 5)")
      end if
    end if

    a6 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1])
    call to_file(a6, file=scratch()//"_rank6.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 6)") ) then
      call from_file(scratch()//"_rank6.dat", into=b6, data_shape=shape(a6), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 6)") ) then
        call expect_equal(shape(b6), shape(a6), "Shape (rank 6)")
        call expect_identical(pack(b6, .true.), pack(a6, .true.), "Values (rank 6)")
      end if
    end if

    a7 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1])
    call to_file(a7, file=scratch()//"_rank7.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 7)") ) then
      call from_file(scratch()//"_rank7.dat", into=b7, data_shape=shape(a7), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 7)") ) then
        call expect_equal(shape(b7), shape(a7), "Shape (rank 7)")
        call expect_identical(pack(b7, .true.), pack(a7, .true.), "Values (rank 7)")
      end if
    end if

    a8 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1])
    call to_file(a8, file=scratch()//"_rank8.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 8)") ) then
      call from_file(scratch()//"_rank8.dat", into=b8, data_shape=shape(a8), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 8)") ) then
        call expect_equal(shape(b8), shape(a8), "Shape (rank 8)")
        call expect_identical(pack(b8, .true.), pack(a8, .true.), "Values (rank 8)")
      end if
    end if

    a9 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1])
    call to_file(a9, file=scratch()//"_rank9.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 9)") ) then
      call from_file(scratch()//"_rank9.dat", into=b9, data_shape=shape(a9), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 9)") ) then
        call expect_equal(shape(b9), shape(a9), "Shape (rank 9)")
        call expect_identical(pack(b9, .true.), pack(a9, .true.), "Values (rank 9)")
      end if
    end if

    a10 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1])
    call to_file(a10, file=scratch()//"_rank10.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 10)") ) then
      call from_file(scratch()//"_rank10.dat", into=b10, data_shape=shape(a10), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 10)") ) then
        call expect_equal(shape(b10), shape(a10), "Shape (rank 10)")
        call expect_identical(pack(b10, .true.), pack(a10, .true.), "Values (rank 10)")
      end if
    end if

    a11 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a11, file=scratch()//"_rank11.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 11)") ) then
      call from_file(scratch()//"_rank11.dat", into=b11, data_shape=shape(a11), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 11)") ) then
        call expect_equal(shape(b11), shape(a11), "Shape (rank 11)")
        call expect_identical(pack(b11, .true.), pack(a11, .true.), "Values (rank 11)")
      end if
    end if

    a12 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a12, file=scratch()//"_rank12.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 12)") ) then
      call from_file(scratch()//"_rank12.dat", into=b12, data_shape=shape(a12), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 12)") ) then
        call expect_equal(shape(b12), shape(a12), "Shape (rank 12)")
        call expect_identical(pack(b12, .true.), pack(a12, .true.), "Values (rank 12)")
      end if
    end if

    a13 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a13, file=scratch()//"_rank13.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 13)") ) then
      call from_file(scratch()//"_rank13.dat", into=b13, data_shape=shape(a13), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 13)") ) then
        call expect_equal(shape(b13), shape(a13), "Shape (rank 13)")
        call expect_identical(pack(b13, .true.), pack(a13, .true.), "Values (rank 13)")
      end if
    end if

    a14 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a14, file=scratch()//"_rank14.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 14)") ) then
      call from_file(scratch()//"_rank14.dat", into=b14, data_shape=shape(a14), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 14)") ) then
        call expect_equal(shape(b14), shape(a14), "Shape (rank 14)")
        call expect_identical(pack(b14, .true.), pack(a14, .true.), "Values (rank 14)")
      end if
    end if

    a15 = reshape(sample(1:24), [3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1])
    call to_file(a15, file=scratch()//"_rank15.dat", stat=st, errmsg=msg)
    if ( succeeded(st, msg, "to_file (rank 15)") ) then
      call from_file(scratch()//"_rank15.dat", into=b15, data_shape=shape(a15), &
                     stat=st, errmsg=msg)
      if ( succeeded(st, msg, "from_file (rank 15)") ) then
        call expect_equal(shape(b15), shape(a15), "Shape (rank 15)")
        call expect_identical(pack(b15, .true.), pack(a15, .true.), "Values (rank 15)")
      end if
    end if

  end subroutine binary_file_c32


end module file_round_trip_cases
