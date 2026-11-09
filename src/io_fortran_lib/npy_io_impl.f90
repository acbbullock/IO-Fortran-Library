! Generated from fypp/io_fortran_lib/npy_io_impl.fypp by fypp/generate.sh. Edit that file, not this one.
#include "iofl_config.inc"
submodule (io_fortran_lib) io_npy
  !---------------------------------------------------------------------------------------------------------------------
  !! This submodule provides module procedure implementations for the **private interfaces** `to_npy` and `from_npy`,
  !! which write and read NumPy `.npy` files.
  !!
  !! A file is the magic string `\x93NUMPY`, a version, the length of a header, the header, and the data. The header is
  !! a Python dictionary literal such as `{'descr': '<f8', 'fortran_order': True, 'shape': (3, 2), }`, padded with
  !! spaces and ended with a newline so that the data starts at a multiple of 64 bytes. This library writes version 1.0
  !! in Fortran order and native byte order, and reads versions 1 to 3 in either order and either byte order.
  !---------------------------------------------------------------------------------------------------------------------
  implicit none (type, external)

  character(len=*), parameter :: NPY_MAGIC = char(147)//"NUMPY"
  logical,          parameter :: LITTLE_ENDIAN = (iachar(transfer(1_i16, "a")) == 1)   ! Byte order of this machine

  contains ! Procedure bodies for module subprograms <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

  ! Writing Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  module procedure to_npy_c64
    integer :: file_unit

    open( newunit=file_unit, file=file, status="replace", form="unformatted", action="write", access="stream", &
          iostat=stat, iomsg=errmsg )

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    write(unit=file_unit, iostat=stat, iomsg=errmsg) npy_header(descr_of("c", 16), shape(x, kind=i64))

    if ( stat == 0 ) then
      select rank(x)
        rank(1); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(2); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(3); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(4); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(5); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(6); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(7); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(8); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(9); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(10); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(11); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(12); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(13); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(14); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(15); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      end select
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR
      call close_quietly(file_unit)
      return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_npy_c64

  module procedure to_npy_c32
    integer :: file_unit

    open( newunit=file_unit, file=file, status="replace", form="unformatted", action="write", access="stream", &
          iostat=stat, iomsg=errmsg )

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    write(unit=file_unit, iostat=stat, iomsg=errmsg) npy_header(descr_of("c", 8), shape(x, kind=i64))

    if ( stat == 0 ) then
      select rank(x)
        rank(1); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(2); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(3); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(4); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(5); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(6); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(7); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(8); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(9); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(10); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(11); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(12); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(13); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(14); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(15); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      end select
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR
      call close_quietly(file_unit)
      return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_npy_c32

  module procedure to_npy_r64
    integer :: file_unit

    open( newunit=file_unit, file=file, status="replace", form="unformatted", action="write", access="stream", &
          iostat=stat, iomsg=errmsg )

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    write(unit=file_unit, iostat=stat, iomsg=errmsg) npy_header(descr_of("f", 8), shape(x, kind=i64))

    if ( stat == 0 ) then
      select rank(x)
        rank(1); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(2); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(3); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(4); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(5); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(6); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(7); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(8); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(9); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(10); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(11); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(12); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(13); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(14); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(15); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      end select
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR
      call close_quietly(file_unit)
      return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_npy_r64

  module procedure to_npy_r32
    integer :: file_unit

    open( newunit=file_unit, file=file, status="replace", form="unformatted", action="write", access="stream", &
          iostat=stat, iomsg=errmsg )

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    write(unit=file_unit, iostat=stat, iomsg=errmsg) npy_header(descr_of("f", 4), shape(x, kind=i64))

    if ( stat == 0 ) then
      select rank(x)
        rank(1); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(2); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(3); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(4); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(5); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(6); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(7); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(8); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(9); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(10); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(11); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(12); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(13); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(14); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(15); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      end select
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR
      call close_quietly(file_unit)
      return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_npy_r32

  module procedure to_npy_i64
    integer :: file_unit

    open( newunit=file_unit, file=file, status="replace", form="unformatted", action="write", access="stream", &
          iostat=stat, iomsg=errmsg )

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    write(unit=file_unit, iostat=stat, iomsg=errmsg) npy_header(descr_of("i", 8), shape(x, kind=i64))

    if ( stat == 0 ) then
      select rank(x)
        rank(1); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(2); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(3); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(4); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(5); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(6); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(7); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(8); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(9); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(10); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(11); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(12); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(13); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(14); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(15); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      end select
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR
      call close_quietly(file_unit)
      return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_npy_i64

  module procedure to_npy_i32
    integer :: file_unit

    open( newunit=file_unit, file=file, status="replace", form="unformatted", action="write", access="stream", &
          iostat=stat, iomsg=errmsg )

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    write(unit=file_unit, iostat=stat, iomsg=errmsg) npy_header(descr_of("i", 4), shape(x, kind=i64))

    if ( stat == 0 ) then
      select rank(x)
        rank(1); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(2); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(3); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(4); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(5); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(6); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(7); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(8); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(9); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(10); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(11); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(12); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(13); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(14); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(15); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      end select
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR
      call close_quietly(file_unit)
      return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_npy_i32

  module procedure to_npy_i16
    integer :: file_unit

    open( newunit=file_unit, file=file, status="replace", form="unformatted", action="write", access="stream", &
          iostat=stat, iomsg=errmsg )

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    write(unit=file_unit, iostat=stat, iomsg=errmsg) npy_header(descr_of("i", 2), shape(x, kind=i64))

    if ( stat == 0 ) then
      select rank(x)
        rank(1); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(2); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(3); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(4); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(5); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(6); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(7); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(8); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(9); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(10); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(11); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(12); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(13); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(14); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(15); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      end select
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR
      call close_quietly(file_unit)
      return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_npy_i16

  module procedure to_npy_i8
    integer :: file_unit

    open( newunit=file_unit, file=file, status="replace", form="unformatted", action="write", access="stream", &
          iostat=stat, iomsg=errmsg )

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    write(unit=file_unit, iostat=stat, iomsg=errmsg) npy_header(descr_of("i", 1), shape(x, kind=i64))

    if ( stat == 0 ) then
      select rank(x)
        rank(1); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(2); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(3); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(4); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(5); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(6); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(7); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(8); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(9); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(10); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(11); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(12); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(13); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(14); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
        rank(15); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      end select
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR
      call close_quietly(file_unit)
      return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_npy_i8

  module procedure to_npy_l
    integer :: file_unit

    open( newunit=file_unit, file=file, status="replace", form="unformatted", action="write", access="stream", &
          iostat=stat, iomsg=errmsg )

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    write(unit=file_unit, iostat=stat, iomsg=errmsg) npy_header(descr_of("b", 1), shape(x, kind=i64))

    if ( stat == 0 ) then
      select rank(x)
        rank(1); write(unit=file_unit, iostat=stat, iomsg=errmsg) merge(1_i8, 0_i8, x)   ! NumPy bool: one byte
        rank(2); write(unit=file_unit, iostat=stat, iomsg=errmsg) merge(1_i8, 0_i8, x)   ! NumPy bool: one byte
        rank(3); write(unit=file_unit, iostat=stat, iomsg=errmsg) merge(1_i8, 0_i8, x)   ! NumPy bool: one byte
        rank(4); write(unit=file_unit, iostat=stat, iomsg=errmsg) merge(1_i8, 0_i8, x)   ! NumPy bool: one byte
        rank(5); write(unit=file_unit, iostat=stat, iomsg=errmsg) merge(1_i8, 0_i8, x)   ! NumPy bool: one byte
        rank(6); write(unit=file_unit, iostat=stat, iomsg=errmsg) merge(1_i8, 0_i8, x)   ! NumPy bool: one byte
        rank(7); write(unit=file_unit, iostat=stat, iomsg=errmsg) merge(1_i8, 0_i8, x)   ! NumPy bool: one byte
        rank(8); write(unit=file_unit, iostat=stat, iomsg=errmsg) merge(1_i8, 0_i8, x)   ! NumPy bool: one byte
        rank(9); write(unit=file_unit, iostat=stat, iomsg=errmsg) merge(1_i8, 0_i8, x)   ! NumPy bool: one byte
        rank(10); write(unit=file_unit, iostat=stat, iomsg=errmsg) merge(1_i8, 0_i8, x)   ! NumPy bool: one byte
        rank(11); write(unit=file_unit, iostat=stat, iomsg=errmsg) merge(1_i8, 0_i8, x)   ! NumPy bool: one byte
        rank(12); write(unit=file_unit, iostat=stat, iomsg=errmsg) merge(1_i8, 0_i8, x)   ! NumPy bool: one byte
        rank(13); write(unit=file_unit, iostat=stat, iomsg=errmsg) merge(1_i8, 0_i8, x)   ! NumPy bool: one byte
        rank(14); write(unit=file_unit, iostat=stat, iomsg=errmsg) merge(1_i8, 0_i8, x)   ! NumPy bool: one byte
        rank(15); write(unit=file_unit, iostat=stat, iomsg=errmsg) merge(1_i8, 0_i8, x)   ! NumPy bool: one byte
      end select
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR
      call close_quietly(file_unit)
      return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_npy_l

  ! Reading Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  module procedure from_npy_c64
    character(len=:), allocatable :: descr
    integer(i64),     allocatable :: shp(:)
    integer(i8),      allocatable :: bytes(:)
    complex(r64),     allocatable :: flat(:)
    complex(r64)                      :: mold
    logical                       :: fortran_order, swap
    integer                       :: file_unit, into_rank

    select rank(into)                              ! Not the rank intrinsic: ifx rejects it on an unallocated array
      rank(1); into_rank = 1
      rank(2); into_rank = 2
      rank(3); into_rank = 3
      rank(4); into_rank = 4
      rank(5); into_rank = 5
      rank(6); into_rank = 6
      rank(7); into_rank = 7
      rank(8); into_rank = 8
      rank(9); into_rank = 9
      rank(10); into_rank = 10
      rank(11); into_rank = 11
      rank(12); into_rank = 12
      rank(13); into_rank = 13
      rank(14); into_rank = 14
      rank(15); into_rank = 15
      rank default; into_rank = -1
    end select

    call open_npy(file, file_unit, descr, fortran_order, shp, stat, errmsg)
    if ( stat /= 0 ) return

    call check_descr(descr, "c", 16, "complex(real64)", file, swap, stat, errmsg)

    if ( (stat == 0) .and. (size(shp) /= into_rank) ) then
      stat   = READ_ERR
      errmsg = 'Error reading file "'//file//'": it holds an array of rank '//int_str(size(shp, kind=i64))// &
               ", but the output array has rank "//int_str(int(into_rank, kind=i64))//"."
    end if

    if ( stat /= 0 ) then
      call close_quietly(file_unit); return
    end if

    select rank(into)
      rank(1)
        allocate( into(shp(1)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(16*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            into = flat
          end if
        end if
      rank(2)
        allocate( into(shp(1), shp(2)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(16*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[2, 1])
            end if
          end if
        end if
      rank(3)
        allocate( into(shp(1), shp(2), shp(3)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(16*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[3, 2, 1])
            end if
          end if
        end if
      rank(4)
        allocate( into(shp(1), shp(2), shp(3), shp(4)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(16*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[4, 3, 2, 1])
            end if
          end if
        end if
      rank(5)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(16*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(6)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(16*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(7)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(16*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(8)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(16*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(9)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(16*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(10)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(16*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(11)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(16*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(12)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(16*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(13)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(16*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(14)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13), shp(14)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(16*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(15)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13), shp(14), &
                       shp(15)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(16*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
    end select

    if ( stat /= 0 ) then
      stat   = READ_ERR
      errmsg = 'Error reading file "'//file//'": the data is shorter than its header says. '//trim(errmsg)
      call close_quietly(file_unit)
      return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure from_npy_c64

  module procedure from_npy_c32
    character(len=:), allocatable :: descr
    integer(i64),     allocatable :: shp(:)
    integer(i8),      allocatable :: bytes(:)
    complex(r32),     allocatable :: flat(:)
    complex(r32)                      :: mold
    logical                       :: fortran_order, swap
    integer                       :: file_unit, into_rank

    select rank(into)                              ! Not the rank intrinsic: ifx rejects it on an unallocated array
      rank(1); into_rank = 1
      rank(2); into_rank = 2
      rank(3); into_rank = 3
      rank(4); into_rank = 4
      rank(5); into_rank = 5
      rank(6); into_rank = 6
      rank(7); into_rank = 7
      rank(8); into_rank = 8
      rank(9); into_rank = 9
      rank(10); into_rank = 10
      rank(11); into_rank = 11
      rank(12); into_rank = 12
      rank(13); into_rank = 13
      rank(14); into_rank = 14
      rank(15); into_rank = 15
      rank default; into_rank = -1
    end select

    call open_npy(file, file_unit, descr, fortran_order, shp, stat, errmsg)
    if ( stat /= 0 ) return

    call check_descr(descr, "c", 8, "complex(real32)", file, swap, stat, errmsg)

    if ( (stat == 0) .and. (size(shp) /= into_rank) ) then
      stat   = READ_ERR
      errmsg = 'Error reading file "'//file//'": it holds an array of rank '//int_str(size(shp, kind=i64))// &
               ", but the output array has rank "//int_str(int(into_rank, kind=i64))//"."
    end if

    if ( stat /= 0 ) then
      call close_quietly(file_unit); return
    end if

    select rank(into)
      rank(1)
        allocate( into(shp(1)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            into = flat
          end if
        end if
      rank(2)
        allocate( into(shp(1), shp(2)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[2, 1])
            end if
          end if
        end if
      rank(3)
        allocate( into(shp(1), shp(2), shp(3)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[3, 2, 1])
            end if
          end if
        end if
      rank(4)
        allocate( into(shp(1), shp(2), shp(3), shp(4)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[4, 3, 2, 1])
            end if
          end if
        end if
      rank(5)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(6)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(7)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(8)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(9)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(10)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(11)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(12)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(13)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(14)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13), shp(14)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(15)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13), shp(14), &
                       shp(15)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
    end select

    if ( stat /= 0 ) then
      stat   = READ_ERR
      errmsg = 'Error reading file "'//file//'": the data is shorter than its header says. '//trim(errmsg)
      call close_quietly(file_unit)
      return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure from_npy_c32

  module procedure from_npy_r64
    character(len=:), allocatable :: descr
    integer(i64),     allocatable :: shp(:)
    integer(i8),      allocatable :: bytes(:)
    real(r64),     allocatable :: flat(:)
    real(r64)                      :: mold
    logical                       :: fortran_order, swap
    integer                       :: file_unit, into_rank

    select rank(into)                              ! Not the rank intrinsic: ifx rejects it on an unallocated array
      rank(1); into_rank = 1
      rank(2); into_rank = 2
      rank(3); into_rank = 3
      rank(4); into_rank = 4
      rank(5); into_rank = 5
      rank(6); into_rank = 6
      rank(7); into_rank = 7
      rank(8); into_rank = 8
      rank(9); into_rank = 9
      rank(10); into_rank = 10
      rank(11); into_rank = 11
      rank(12); into_rank = 12
      rank(13); into_rank = 13
      rank(14); into_rank = 14
      rank(15); into_rank = 15
      rank default; into_rank = -1
    end select

    call open_npy(file, file_unit, descr, fortran_order, shp, stat, errmsg)
    if ( stat /= 0 ) return

    call check_descr(descr, "f", 8, "real(real64)", file, swap, stat, errmsg)

    if ( (stat == 0) .and. (size(shp) /= into_rank) ) then
      stat   = READ_ERR
      errmsg = 'Error reading file "'//file//'": it holds an array of rank '//int_str(size(shp, kind=i64))// &
               ", but the output array has rank "//int_str(int(into_rank, kind=i64))//"."
    end if

    if ( stat /= 0 ) then
      call close_quietly(file_unit); return
    end if

    select rank(into)
      rank(1)
        allocate( into(shp(1)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            into = flat
          end if
        end if
      rank(2)
        allocate( into(shp(1), shp(2)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[2, 1])
            end if
          end if
        end if
      rank(3)
        allocate( into(shp(1), shp(2), shp(3)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[3, 2, 1])
            end if
          end if
        end if
      rank(4)
        allocate( into(shp(1), shp(2), shp(3), shp(4)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[4, 3, 2, 1])
            end if
          end if
        end if
      rank(5)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(6)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(7)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(8)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(9)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(10)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(11)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(12)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(13)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(14)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13), shp(14)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(15)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13), shp(14), &
                       shp(15)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
    end select

    if ( stat /= 0 ) then
      stat   = READ_ERR
      errmsg = 'Error reading file "'//file//'": the data is shorter than its header says. '//trim(errmsg)
      call close_quietly(file_unit)
      return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure from_npy_r64

  module procedure from_npy_r32
    character(len=:), allocatable :: descr
    integer(i64),     allocatable :: shp(:)
    integer(i8),      allocatable :: bytes(:)
    real(r32),     allocatable :: flat(:)
    real(r32)                      :: mold
    logical                       :: fortran_order, swap
    integer                       :: file_unit, into_rank

    select rank(into)                              ! Not the rank intrinsic: ifx rejects it on an unallocated array
      rank(1); into_rank = 1
      rank(2); into_rank = 2
      rank(3); into_rank = 3
      rank(4); into_rank = 4
      rank(5); into_rank = 5
      rank(6); into_rank = 6
      rank(7); into_rank = 7
      rank(8); into_rank = 8
      rank(9); into_rank = 9
      rank(10); into_rank = 10
      rank(11); into_rank = 11
      rank(12); into_rank = 12
      rank(13); into_rank = 13
      rank(14); into_rank = 14
      rank(15); into_rank = 15
      rank default; into_rank = -1
    end select

    call open_npy(file, file_unit, descr, fortran_order, shp, stat, errmsg)
    if ( stat /= 0 ) return

    call check_descr(descr, "f", 4, "real(real32)", file, swap, stat, errmsg)

    if ( (stat == 0) .and. (size(shp) /= into_rank) ) then
      stat   = READ_ERR
      errmsg = 'Error reading file "'//file//'": it holds an array of rank '//int_str(size(shp, kind=i64))// &
               ", but the output array has rank "//int_str(int(into_rank, kind=i64))//"."
    end if

    if ( stat /= 0 ) then
      call close_quietly(file_unit); return
    end if

    select rank(into)
      rank(1)
        allocate( into(shp(1)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            into = flat
          end if
        end if
      rank(2)
        allocate( into(shp(1), shp(2)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[2, 1])
            end if
          end if
        end if
      rank(3)
        allocate( into(shp(1), shp(2), shp(3)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[3, 2, 1])
            end if
          end if
        end if
      rank(4)
        allocate( into(shp(1), shp(2), shp(3), shp(4)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[4, 3, 2, 1])
            end if
          end if
        end if
      rank(5)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(6)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(7)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(8)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(9)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(10)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(11)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(12)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(13)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(14)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13), shp(14)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(15)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13), shp(14), &
                       shp(15)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
    end select

    if ( stat /= 0 ) then
      stat   = READ_ERR
      errmsg = 'Error reading file "'//file//'": the data is shorter than its header says. '//trim(errmsg)
      call close_quietly(file_unit)
      return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure from_npy_r32

  module procedure from_npy_i64
    character(len=:), allocatable :: descr
    integer(i64),     allocatable :: shp(:)
    integer(i8),      allocatable :: bytes(:)
    integer(i64),     allocatable :: flat(:)
    integer(i64)                      :: mold
    logical                       :: fortran_order, swap
    integer                       :: file_unit, into_rank

    select rank(into)                              ! Not the rank intrinsic: ifx rejects it on an unallocated array
      rank(1); into_rank = 1
      rank(2); into_rank = 2
      rank(3); into_rank = 3
      rank(4); into_rank = 4
      rank(5); into_rank = 5
      rank(6); into_rank = 6
      rank(7); into_rank = 7
      rank(8); into_rank = 8
      rank(9); into_rank = 9
      rank(10); into_rank = 10
      rank(11); into_rank = 11
      rank(12); into_rank = 12
      rank(13); into_rank = 13
      rank(14); into_rank = 14
      rank(15); into_rank = 15
      rank default; into_rank = -1
    end select

    call open_npy(file, file_unit, descr, fortran_order, shp, stat, errmsg)
    if ( stat /= 0 ) return

    call check_descr(descr, "i", 8, "integer(int64)", file, swap, stat, errmsg)

    if ( (stat == 0) .and. (size(shp) /= into_rank) ) then
      stat   = READ_ERR
      errmsg = 'Error reading file "'//file//'": it holds an array of rank '//int_str(size(shp, kind=i64))// &
               ", but the output array has rank "//int_str(int(into_rank, kind=i64))//"."
    end if

    if ( stat /= 0 ) then
      call close_quietly(file_unit); return
    end if

    select rank(into)
      rank(1)
        allocate( into(shp(1)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            into = flat
          end if
        end if
      rank(2)
        allocate( into(shp(1), shp(2)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[2, 1])
            end if
          end if
        end if
      rank(3)
        allocate( into(shp(1), shp(2), shp(3)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[3, 2, 1])
            end if
          end if
        end if
      rank(4)
        allocate( into(shp(1), shp(2), shp(3), shp(4)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[4, 3, 2, 1])
            end if
          end if
        end if
      rank(5)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(6)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(7)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(8)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(9)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(10)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(11)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(12)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(13)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(14)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13), shp(14)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(15)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13), shp(14), &
                       shp(15)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(8*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 8)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
    end select

    if ( stat /= 0 ) then
      stat   = READ_ERR
      errmsg = 'Error reading file "'//file//'": the data is shorter than its header says. '//trim(errmsg)
      call close_quietly(file_unit)
      return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure from_npy_i64

  module procedure from_npy_i32
    character(len=:), allocatable :: descr
    integer(i64),     allocatable :: shp(:)
    integer(i8),      allocatable :: bytes(:)
    integer(i32),     allocatable :: flat(:)
    integer(i32)                      :: mold
    logical                       :: fortran_order, swap
    integer                       :: file_unit, into_rank

    select rank(into)                              ! Not the rank intrinsic: ifx rejects it on an unallocated array
      rank(1); into_rank = 1
      rank(2); into_rank = 2
      rank(3); into_rank = 3
      rank(4); into_rank = 4
      rank(5); into_rank = 5
      rank(6); into_rank = 6
      rank(7); into_rank = 7
      rank(8); into_rank = 8
      rank(9); into_rank = 9
      rank(10); into_rank = 10
      rank(11); into_rank = 11
      rank(12); into_rank = 12
      rank(13); into_rank = 13
      rank(14); into_rank = 14
      rank(15); into_rank = 15
      rank default; into_rank = -1
    end select

    call open_npy(file, file_unit, descr, fortran_order, shp, stat, errmsg)
    if ( stat /= 0 ) return

    call check_descr(descr, "i", 4, "integer(int32)", file, swap, stat, errmsg)

    if ( (stat == 0) .and. (size(shp) /= into_rank) ) then
      stat   = READ_ERR
      errmsg = 'Error reading file "'//file//'": it holds an array of rank '//int_str(size(shp, kind=i64))// &
               ", but the output array has rank "//int_str(int(into_rank, kind=i64))//"."
    end if

    if ( stat /= 0 ) then
      call close_quietly(file_unit); return
    end if

    select rank(into)
      rank(1)
        allocate( into(shp(1)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            into = flat
          end if
        end if
      rank(2)
        allocate( into(shp(1), shp(2)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[2, 1])
            end if
          end if
        end if
      rank(3)
        allocate( into(shp(1), shp(2), shp(3)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[3, 2, 1])
            end if
          end if
        end if
      rank(4)
        allocate( into(shp(1), shp(2), shp(3), shp(4)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[4, 3, 2, 1])
            end if
          end if
        end if
      rank(5)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(6)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(7)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(8)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(9)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(10)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(11)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(12)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(13)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(14)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13), shp(14)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(15)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13), shp(14), &
                       shp(15)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(4*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 4)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
    end select

    if ( stat /= 0 ) then
      stat   = READ_ERR
      errmsg = 'Error reading file "'//file//'": the data is shorter than its header says. '//trim(errmsg)
      call close_quietly(file_unit)
      return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure from_npy_i32

  module procedure from_npy_i16
    character(len=:), allocatable :: descr
    integer(i64),     allocatable :: shp(:)
    integer(i8),      allocatable :: bytes(:)
    integer(i16),     allocatable :: flat(:)
    integer(i16)                      :: mold
    logical                       :: fortran_order, swap
    integer                       :: file_unit, into_rank

    select rank(into)                              ! Not the rank intrinsic: ifx rejects it on an unallocated array
      rank(1); into_rank = 1
      rank(2); into_rank = 2
      rank(3); into_rank = 3
      rank(4); into_rank = 4
      rank(5); into_rank = 5
      rank(6); into_rank = 6
      rank(7); into_rank = 7
      rank(8); into_rank = 8
      rank(9); into_rank = 9
      rank(10); into_rank = 10
      rank(11); into_rank = 11
      rank(12); into_rank = 12
      rank(13); into_rank = 13
      rank(14); into_rank = 14
      rank(15); into_rank = 15
      rank default; into_rank = -1
    end select

    call open_npy(file, file_unit, descr, fortran_order, shp, stat, errmsg)
    if ( stat /= 0 ) return

    call check_descr(descr, "i", 2, "integer(int16)", file, swap, stat, errmsg)

    if ( (stat == 0) .and. (size(shp) /= into_rank) ) then
      stat   = READ_ERR
      errmsg = 'Error reading file "'//file//'": it holds an array of rank '//int_str(size(shp, kind=i64))// &
               ", but the output array has rank "//int_str(int(into_rank, kind=i64))//"."
    end if

    if ( stat /= 0 ) then
      call close_quietly(file_unit); return
    end if

    select rank(into)
      rank(1)
        allocate( into(shp(1)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(2*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 2)
            flat = transfer(bytes, mold, size(into))
            into = flat
          end if
        end if
      rank(2)
        allocate( into(shp(1), shp(2)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(2*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 2)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[2, 1])
            end if
          end if
        end if
      rank(3)
        allocate( into(shp(1), shp(2), shp(3)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(2*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 2)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[3, 2, 1])
            end if
          end if
        end if
      rank(4)
        allocate( into(shp(1), shp(2), shp(3), shp(4)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(2*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 2)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[4, 3, 2, 1])
            end if
          end if
        end if
      rank(5)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(2*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 2)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(6)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(2*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 2)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(7)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(2*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 2)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(8)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(2*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 2)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(9)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(2*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 2)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(10)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(2*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 2)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(11)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(2*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 2)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(12)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(2*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 2)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(13)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(2*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 2)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(14)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13), shp(14)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(2*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 2)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(15)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13), shp(14), &
                       shp(15)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(2*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 2)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
    end select

    if ( stat /= 0 ) then
      stat   = READ_ERR
      errmsg = 'Error reading file "'//file//'": the data is shorter than its header says. '//trim(errmsg)
      call close_quietly(file_unit)
      return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure from_npy_i16

  module procedure from_npy_i8
    character(len=:), allocatable :: descr
    integer(i64),     allocatable :: shp(:)
    integer(i8),      allocatable :: bytes(:)
    integer(i8),     allocatable :: flat(:)
    integer(i8)                      :: mold
    logical                       :: fortran_order, swap
    integer                       :: file_unit, into_rank

    select rank(into)                              ! Not the rank intrinsic: ifx rejects it on an unallocated array
      rank(1); into_rank = 1
      rank(2); into_rank = 2
      rank(3); into_rank = 3
      rank(4); into_rank = 4
      rank(5); into_rank = 5
      rank(6); into_rank = 6
      rank(7); into_rank = 7
      rank(8); into_rank = 8
      rank(9); into_rank = 9
      rank(10); into_rank = 10
      rank(11); into_rank = 11
      rank(12); into_rank = 12
      rank(13); into_rank = 13
      rank(14); into_rank = 14
      rank(15); into_rank = 15
      rank default; into_rank = -1
    end select

    call open_npy(file, file_unit, descr, fortran_order, shp, stat, errmsg)
    if ( stat /= 0 ) return

    call check_descr(descr, "i", 1, "integer(int8)", file, swap, stat, errmsg)

    if ( (stat == 0) .and. (size(shp) /= into_rank) ) then
      stat   = READ_ERR
      errmsg = 'Error reading file "'//file//'": it holds an array of rank '//int_str(size(shp, kind=i64))// &
               ", but the output array has rank "//int_str(int(into_rank, kind=i64))//"."
    end if

    if ( stat /= 0 ) then
      call close_quietly(file_unit); return
    end if

    select rank(into)
      rank(1)
        allocate( into(shp(1)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 1)
            flat = transfer(bytes, mold, size(into))
            into = flat
          end if
        end if
      rank(2)
        allocate( into(shp(1), shp(2)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 1)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[2, 1])
            end if
          end if
        end if
      rank(3)
        allocate( into(shp(1), shp(2), shp(3)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 1)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[3, 2, 1])
            end if
          end if
        end if
      rank(4)
        allocate( into(shp(1), shp(2), shp(3), shp(4)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 1)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[4, 3, 2, 1])
            end if
          end if
        end if
      rank(5)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 1)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(6)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 1)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(7)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 1)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(8)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 1)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(9)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 1)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(10)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 1)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(11)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 1)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(12)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 1)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(13)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 1)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(14)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13), shp(14)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 1)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
      rank(15)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13), shp(14), &
                       shp(15)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        if ( fortran_order .and. (.not. swap) ) then
          read(unit=file_unit, iostat=stat, iomsg=errmsg) into
        else
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            if ( swap ) call swap_bytes(bytes, 1)
            flat = transfer(bytes, mold, size(into))
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end if
    end select

    if ( stat /= 0 ) then
      stat   = READ_ERR
      errmsg = 'Error reading file "'//file//'": the data is shorter than its header says. '//trim(errmsg)
      call close_quietly(file_unit)
      return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure from_npy_i8

  module procedure from_npy_l
    character(len=:), allocatable :: descr
    integer(i64),     allocatable :: shp(:)
    integer(i8),      allocatable :: bytes(:)
    logical,     allocatable :: flat(:)
    logical                       :: fortran_order, swap
    integer                       :: file_unit, into_rank

    select rank(into)                              ! Not the rank intrinsic: ifx rejects it on an unallocated array
      rank(1); into_rank = 1
      rank(2); into_rank = 2
      rank(3); into_rank = 3
      rank(4); into_rank = 4
      rank(5); into_rank = 5
      rank(6); into_rank = 6
      rank(7); into_rank = 7
      rank(8); into_rank = 8
      rank(9); into_rank = 9
      rank(10); into_rank = 10
      rank(11); into_rank = 11
      rank(12); into_rank = 12
      rank(13); into_rank = 13
      rank(14); into_rank = 14
      rank(15); into_rank = 15
      rank default; into_rank = -1
    end select

    call open_npy(file, file_unit, descr, fortran_order, shp, stat, errmsg)
    if ( stat /= 0 ) return

    call check_descr(descr, "b", 1, "logical", file, swap, stat, errmsg)

    if ( (stat == 0) .and. (size(shp) /= into_rank) ) then
      stat   = READ_ERR
      errmsg = 'Error reading file "'//file//'": it holds an array of rank '//int_str(size(shp, kind=i64))// &
               ", but the output array has rank "//int_str(int(into_rank, kind=i64))//"."
    end if

    if ( stat /= 0 ) then
      call close_quietly(file_unit); return
    end if

    select rank(into)
      rank(1)
        allocate( into(shp(1)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        ! A NumPy bool is one byte, which is not how a Fortran logical is stored, so it is always converted.
        block
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            flat = (bytes /= 0_i8)
            into = flat
          end if
        end block
      rank(2)
        allocate( into(shp(1), shp(2)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        ! A NumPy bool is one byte, which is not how a Fortran logical is stored, so it is always converted.
        block
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            flat = (bytes /= 0_i8)
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[2, 1])
            end if
          end if
        end block
      rank(3)
        allocate( into(shp(1), shp(2), shp(3)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        ! A NumPy bool is one byte, which is not how a Fortran logical is stored, so it is always converted.
        block
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            flat = (bytes /= 0_i8)
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[3, 2, 1])
            end if
          end if
        end block
      rank(4)
        allocate( into(shp(1), shp(2), shp(3), shp(4)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        ! A NumPy bool is one byte, which is not how a Fortran logical is stored, so it is always converted.
        block
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            flat = (bytes /= 0_i8)
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[4, 3, 2, 1])
            end if
          end if
        end block
      rank(5)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        ! A NumPy bool is one byte, which is not how a Fortran logical is stored, so it is always converted.
        block
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            flat = (bytes /= 0_i8)
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[5, 4, 3, 2, 1])
            end if
          end if
        end block
      rank(6)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        ! A NumPy bool is one byte, which is not how a Fortran logical is stored, so it is always converted.
        block
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            flat = (bytes /= 0_i8)
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[6, 5, 4, 3, 2, 1])
            end if
          end if
        end block
      rank(7)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        ! A NumPy bool is one byte, which is not how a Fortran logical is stored, so it is always converted.
        block
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            flat = (bytes /= 0_i8)
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end block
      rank(8)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        ! A NumPy bool is one byte, which is not how a Fortran logical is stored, so it is always converted.
        block
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            flat = (bytes /= 0_i8)
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end block
      rank(9)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        ! A NumPy bool is one byte, which is not how a Fortran logical is stored, so it is always converted.
        block
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            flat = (bytes /= 0_i8)
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end block
      rank(10)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        ! A NumPy bool is one byte, which is not how a Fortran logical is stored, so it is always converted.
        block
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            flat = (bytes /= 0_i8)
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end block
      rank(11)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        ! A NumPy bool is one byte, which is not how a Fortran logical is stored, so it is always converted.
        block
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            flat = (bytes /= 0_i8)
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end block
      rank(12)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        ! A NumPy bool is one byte, which is not how a Fortran logical is stored, so it is always converted.
        block
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            flat = (bytes /= 0_i8)
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end block
      rank(13)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        ! A NumPy bool is one byte, which is not how a Fortran logical is stored, so it is always converted.
        block
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            flat = (bytes /= 0_i8)
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end block
      rank(14)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13), shp(14)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        ! A NumPy bool is one byte, which is not how a Fortran logical is stored, so it is always converted.
        block
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            flat = (bytes /= 0_i8)
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end block
      rank(15)
        allocate( into(shp(1), shp(2), shp(3), shp(4), shp(5), shp(6), shp(7), &
                       shp(8), shp(9), shp(10), shp(11), shp(12), shp(13), shp(14), &
                       shp(15)), &
                  stat=stat, errmsg=errmsg )

        if ( stat /= 0 ) then
          stat = ALLOC_ERR
          call close_quietly(file_unit)
          return
        end if

        ! A NumPy bool is one byte, which is not how a Fortran logical is stored, so it is always converted.
        block
          allocate( bytes(1*size(into, kind=i64)), stat=stat, errmsg=errmsg )

          if ( stat == 0 ) read(unit=file_unit, iostat=stat, iomsg=errmsg) bytes

          if ( stat == 0 ) then
            flat = (bytes /= 0_i8)
            if ( fortran_order ) then
              into = reshape(flat, shape(into))
            else
              into = reshape(flat, shape(into), order=[15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
            end if
          end if
        end block
    end select

    if ( stat /= 0 ) then
      stat   = READ_ERR
      errmsg = 'Error reading file "'//file//'": the data is shorter than its header says. '//trim(errmsg)
      call close_quietly(file_unit)
      return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure from_npy_l

  ! Helpers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  pure function descr_of(code, size) result(descr)
    !! Returns the NumPy type string for a type `code` ("f", "c", "i" or "b") of `size` bytes in this machine's byte
    !! order.
    character(len=*), intent(in)  :: code
    integer,          intent(in)  :: size
    character(len=:), allocatable :: descr

    if ( size == 1 ) then
      descr = "|"//code//"1"
    else if ( LITTLE_ENDIAN ) then
      descr = "<"//code//int_str(int(size, kind=i64))
    else
      descr = ">"//code//int_str(int(size, kind=i64))
    end if
  end function descr_of

  pure function npy_header(descr, shp) result(header)
    !! Returns the bytes before the data of a version 1.0 `.npy` file holding `descr` data of shape `shp`.
    character(len=*), intent(in)  :: descr
    integer(i64),     intent(in)  :: shp(:)
    character(len=:), allocatable :: header

    character(len=:), allocatable :: dict, dims
    integer                       :: i, n

    dims = EMPTY_STR
    do i = 1, size(shp)
      if ( i > 1 ) dims = dims//", "
      dims = dims//int_str(shp(i))
    end do
    if ( size(shp) == 1 ) dims = dims//","

    dict = "{'descr': '"//descr//"', 'fortran_order': True, 'shape': ("//dims//"), }"

    n = len(NPY_MAGIC) + 4 + len(dict) + 1                                     ! Magic, version, length, newline
    dict = dict//repeat(SPACE, modulo(-n, 64))//LF

    header = NPY_MAGIC//achar(1)//achar(0)//achar(mod(len(dict), 256))//achar(len(dict)/256)//dict
  end function npy_header

  subroutine open_npy(file, file_unit, descr, fortran_order, shp, stat, errmsg)
    !! Opens `file`, reads its header, and leaves `file_unit` at the start of the data.
    character(len=*),              intent(in)  :: file
    integer,                       intent(out) :: file_unit
    character(len=:), allocatable, intent(out) :: descr
    logical,                       intent(out) :: fortran_order
    integer(i64),     allocatable, intent(out) :: shp(:)
    integer,                       intent(out) :: stat
    character(len=*),              intent(out) :: errmsg

    character(len=8)              :: lead
    character(len=4)              :: length
    character(len=:), allocatable :: header
    integer                       :: header_len, i
    logical                       :: exists

    allocate( shp(0) ); descr = EMPTY_STR; fortran_order = .false.; file_unit = input_unit

    inquire(file=file, exist=exists, iostat=stat, iomsg=errmsg)

    if ( (stat == 0) .and. (.not. exists) ) then
      stat   = READ_ERR
      errmsg = 'Error reading file "'//file//'". No such file exists.'
      return
    else if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    open( newunit=file_unit, file=file, status="old", form="unformatted", action="read", access="stream", &
          position="rewind", iostat=stat, iomsg=errmsg )

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    read(unit=file_unit, iostat=stat, iomsg=errmsg) lead

    if ( (stat /= 0) .or. (lead(1:6) /= NPY_MAGIC) ) then
      stat   = READ_ERR
      errmsg = 'Error reading file "'//file//'": it is not a .npy file.'
      call close_quietly(file_unit)
      return
    end if

    select case (iachar(lead(7:7)))
      case (1)
        read(unit=file_unit, iostat=stat, iomsg=errmsg) length(1:2)
        header_len = iachar(length(1:1)) + 256*iachar(length(2:2))
      case (2:3)
        read(unit=file_unit, iostat=stat, iomsg=errmsg) length
        header_len = 0
        do i = 4, 1, -1
          header_len = 256*header_len + iachar(length(i:i))
        end do
      case default
        stat   = READ_ERR
        errmsg = 'Error reading file "'//file//'": .npy version '//int_str(int(iachar(lead(7:7)), kind=i64))// &
                 " is not supported."
        call close_quietly(file_unit)
        return
    end select

    if ( stat == 0 ) then
      allocate( character(len=header_len) :: header )
      read(unit=file_unit, iostat=stat, iomsg=errmsg) header
    end if

    if ( stat == 0 ) call parse_header(header, descr, fortran_order, shp, stat)

    if ( stat /= 0 ) then
      stat   = READ_ERR
      errmsg = 'Error reading file "'//file//'": its .npy header could not be read.'
      call close_quietly(file_unit)
      return
    end if
  end subroutine open_npy

  pure subroutine parse_header(header, descr, fortran_order, shp, stat)
    !! Reads `descr`, `fortran_order` and `shape` from a header dictionary. `stat` is nonzero if one is missing.
    character(len=*),              intent(in)    :: header
    character(len=:), allocatable, intent(inout) :: descr
    logical,                       intent(inout) :: fortran_order
    integer(i64),     allocatable, intent(inout) :: shp(:)
    integer,                       intent(out)   :: stat

    character(len=:), allocatable :: value
    integer                       :: i, j
    integer(i64)                  :: n
    integer(i64),     allocatable :: dims(:)

    stat = 1

    value = value_after(header, "descr")
    if ( len(value) < 2 ) return
    if ( (value(1:1) /= "'") .and. (value(1:1) /= '"') ) return
    i = index(value(2:), value(1:1))
    if ( i == 0 ) return
    descr = value(2:i)

    value = value_after(header, "fortran_order")
    if ( value(1:min(4,len(value))) == "True" ) then
      fortran_order = .true.
    else if ( value(1:min(5,len(value))) == "False" ) then
      fortran_order = .false.
    else
      return
    end if

    value = value_after(header, "shape")
    if ( len(value) < 2 ) return
    if ( value(1:1) /= "(" ) return
    j = index(value, ")")
    if ( j == 0 ) return

    allocate( dims(0) ); n = -1
    do i = 2, j
      select case (value(i:i))
        case ("0":"9")
          if ( n < 0 ) n = 0
          n = 10*n + (iachar(value(i:i)) - iachar("0"))
        case (",", ")")
          if ( n >= 0 ) dims = [ dims, n ]
          n = -1
        case (" ")
          continue
        case default
          return
      end select
    end do

    shp = dims
    stat = 0
  end subroutine parse_header

  pure function value_after(header, key) result(value)
    !! Returns the text after `'key':` in `header`, without leading blanks, or "" if the key is missing.
    character(len=*), intent(in)  :: header, key
    character(len=:), allocatable :: value

    integer :: i

    i = index(header, "'"//key//"'")
    if ( i == 0 ) i = index(header, '"'//key//'"')

    if ( i == 0 ) then
      value = EMPTY_STR; return
    end if

    value = header(i+len(key)+2:)
    i = index(value, ":")

    if ( i == 0 ) then
      value = EMPTY_STR; return
    end if

    value = adjustl(value(i+1:))
  end function value_after

  subroutine check_descr(descr, code, size, type_name, file, swap, stat, errmsg)
    !! Checks that `descr` describes `code` data of `size` bytes, and whether its bytes need swapping.
    character(len=*), intent(in)  :: descr, code, type_name, file
    integer,          intent(in)  :: size
    logical,          intent(out) :: swap
    integer,          intent(out) :: stat
    character(len=*), intent(out) :: errmsg

    character(len=:), allocatable :: expected

    swap = .false.; stat = 0; errmsg = EMPTY_STR

    expected = code//int_str(int(size, kind=i64))

    if ( (len(descr) /= len(expected) + 1) .or. (index("<>|=", descr(1:min(1,len(descr)))) == 0) ) then
      stat = READ_ERR
    else if ( descr(2:) /= expected ) then
      stat = READ_ERR
    end if

    if ( stat /= 0 ) then
      errmsg = 'Error reading file "'//file//'": it holds NumPy data of type "'//descr//'", which cannot be read '// &
               "into "//type_name//" data."
      return
    end if

    if ( size > 1 ) swap = ((descr(1:1) == "<") .and. (.not. LITTLE_ENDIAN)) .or. &
                           ((descr(1:1) == ">") .and. LITTLE_ENDIAN)
  end subroutine check_descr

  pure subroutine swap_bytes(bytes, width)
    !! Reverses the byte order of each `width`-byte value in `bytes`.
    integer(i8), intent(inout) :: bytes(:)
    integer,     intent(in)    :: width

    integer(i64) :: i

    do i = 1, size(bytes, kind=i64), width
      bytes(i:i+width-1) = bytes(i+width-1:i:-1)
    end do
  end subroutine swap_bytes

  pure function int_str(n) result(text)
    integer(i64), intent(in)      :: n
    character(len=:), allocatable :: text

    character(len=20) :: buffer

    write(unit=buffer, fmt="(i0)") n
    text = trim(buffer)
  end function int_str
end submodule io_npy
