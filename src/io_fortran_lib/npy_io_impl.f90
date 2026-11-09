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
  !!
  !! A NumPy `.npz` file, as `numpy.savez` writes it, is a ZIP archive of `.npy` files, one per array, each stored
  !! uncompressed under the array's name with `.npy` added. An archive is a local header and the data for each file,
  !! then a central directory listing the files and where they start, then an end record giving where the directory
  !! starts. ZIP64 records extend these to sizes and offsets of 4 GiB and more. All of these numbers are little-endian
  !! on every machine. Adding an array writes it where the directory began and writes a new directory after it, and
  !! replacing one first moves the files after it down over it. Compressed archives (`numpy.savez_compressed`) are
  !! not read.
  !---------------------------------------------------------------------------------------------------------------------
  implicit none (type, external)

  character(len=*), parameter :: NPY_MAGIC = char(147)//"NUMPY"
  logical,          parameter :: LITTLE_ENDIAN = (iachar(transfer(1_i16, "a")) == 1)   ! Byte order of this machine

  character(len=*), parameter :: ZIP_LOCAL     = "PK"//achar(3)//achar(4)              ! Signatures of ZIP records
  character(len=*), parameter :: ZIP_CENTRAL   = "PK"//achar(1)//achar(2)
  character(len=*), parameter :: ZIP_END       = "PK"//achar(5)//achar(6)
  character(len=*), parameter :: ZIP64_END     = "PK"//achar(6)//achar(6)
  character(len=*), parameter :: ZIP64_LOCATOR = "PK"//achar(6)//achar(7)
  integer(i64),     parameter :: MAX_U16 = 65535_i64, MAX_U32 = 4294967295_i64          ! Largest 2- and 4-byte values
  integer(i64),     parameter :: CHUNK = 1048576_i64                                    ! Bytes moved at a time

  type zip_entry
    !! One file in a ZIP archive, as the central directory records it. Offsets count bytes from 0.
    character(len=:), allocatable :: name
    integer(i64)                  :: offset = 0_i64                                     ! Of the local header
    integer(i64)                  :: data_offset = 0_i64                                ! Of the data, when known
    integer(i64)                  :: csize = 0_i64, usize = 0_i64, crc = 0_i64
    integer(i64)                  :: method = 0_i64, flags = 0_i64, time = 0_i64, date = 0_i64
  end type zip_entry

  contains ! Procedure bodies for module subprograms <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

  ! Writing Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  module procedure to_npy_c64
    character(len=:), allocatable :: header
    type(zip_entry),  allocatable :: entries(:)
    type(zip_entry)               :: member
    integer                       :: file_unit

    header = npy_header(descr_of("c", 16), shape(x, kind=i64))

    if ( npz ) then
      if ( .not. present(name) ) then
        stat   = ARG_ERR
        errmsg = 'Error writing file "'//file//'": an array needs a name to be written to a .npz file.'
        return
      end if

      call npz_begin(file, name, len(header, kind=i64) + 16_i64*size(x, kind=i64), file_unit, entries, member, &
                     stat, errmsg)
      if ( stat /= 0 ) return
    else
      open( newunit=file_unit, file=file, status="replace", form="unformatted", action="write", access="stream", &
            iostat=stat, iomsg=errmsg )

      if ( stat /= 0 ) then
        stat = WRITE_ERR; return
      end if
    end if

    write(unit=file_unit, iostat=stat, iomsg=errmsg) header

    if ( stat == 0 ) then
      if ( is_contiguous(x) ) then                 ! One block, as gfortran moves elements one at a time in select rank
        select rank(x)
          rank(1); call write_flat_c64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(2); call write_flat_c64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(3); call write_flat_c64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(4); call write_flat_c64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(5); call write_flat_c64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(6); call write_flat_c64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(7); call write_flat_c64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(8); call write_flat_c64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(9); call write_flat_c64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(10); call write_flat_c64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(11); call write_flat_c64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(12); call write_flat_c64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(13); call write_flat_c64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(14); call write_flat_c64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(15); call write_flat_c64(file_unit, x, size(x, kind=i64), stat, errmsg)
        end select
      else
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
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR
      call close_quietly(file_unit)
      return
    end if

    if ( npz ) then
      call npz_end(file, file_unit, entries, member, stat, errmsg); return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_npy_c64

  module procedure to_npy_c32
    character(len=:), allocatable :: header
    type(zip_entry),  allocatable :: entries(:)
    type(zip_entry)               :: member
    integer                       :: file_unit

    header = npy_header(descr_of("c", 8), shape(x, kind=i64))

    if ( npz ) then
      if ( .not. present(name) ) then
        stat   = ARG_ERR
        errmsg = 'Error writing file "'//file//'": an array needs a name to be written to a .npz file.'
        return
      end if

      call npz_begin(file, name, len(header, kind=i64) + 8_i64*size(x, kind=i64), file_unit, entries, member, &
                     stat, errmsg)
      if ( stat /= 0 ) return
    else
      open( newunit=file_unit, file=file, status="replace", form="unformatted", action="write", access="stream", &
            iostat=stat, iomsg=errmsg )

      if ( stat /= 0 ) then
        stat = WRITE_ERR; return
      end if
    end if

    write(unit=file_unit, iostat=stat, iomsg=errmsg) header

    if ( stat == 0 ) then
      if ( is_contiguous(x) ) then                 ! One block, as gfortran moves elements one at a time in select rank
        select rank(x)
          rank(1); call write_flat_c32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(2); call write_flat_c32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(3); call write_flat_c32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(4); call write_flat_c32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(5); call write_flat_c32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(6); call write_flat_c32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(7); call write_flat_c32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(8); call write_flat_c32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(9); call write_flat_c32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(10); call write_flat_c32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(11); call write_flat_c32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(12); call write_flat_c32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(13); call write_flat_c32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(14); call write_flat_c32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(15); call write_flat_c32(file_unit, x, size(x, kind=i64), stat, errmsg)
        end select
      else
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
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR
      call close_quietly(file_unit)
      return
    end if

    if ( npz ) then
      call npz_end(file, file_unit, entries, member, stat, errmsg); return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_npy_c32

  module procedure to_npy_r64
    character(len=:), allocatable :: header
    type(zip_entry),  allocatable :: entries(:)
    type(zip_entry)               :: member
    integer                       :: file_unit

    header = npy_header(descr_of("f", 8), shape(x, kind=i64))

    if ( npz ) then
      if ( .not. present(name) ) then
        stat   = ARG_ERR
        errmsg = 'Error writing file "'//file//'": an array needs a name to be written to a .npz file.'
        return
      end if

      call npz_begin(file, name, len(header, kind=i64) + 8_i64*size(x, kind=i64), file_unit, entries, member, &
                     stat, errmsg)
      if ( stat /= 0 ) return
    else
      open( newunit=file_unit, file=file, status="replace", form="unformatted", action="write", access="stream", &
            iostat=stat, iomsg=errmsg )

      if ( stat /= 0 ) then
        stat = WRITE_ERR; return
      end if
    end if

    write(unit=file_unit, iostat=stat, iomsg=errmsg) header

    if ( stat == 0 ) then
      if ( is_contiguous(x) ) then                 ! One block, as gfortran moves elements one at a time in select rank
        select rank(x)
          rank(1); call write_flat_r64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(2); call write_flat_r64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(3); call write_flat_r64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(4); call write_flat_r64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(5); call write_flat_r64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(6); call write_flat_r64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(7); call write_flat_r64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(8); call write_flat_r64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(9); call write_flat_r64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(10); call write_flat_r64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(11); call write_flat_r64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(12); call write_flat_r64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(13); call write_flat_r64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(14); call write_flat_r64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(15); call write_flat_r64(file_unit, x, size(x, kind=i64), stat, errmsg)
        end select
      else
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
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR
      call close_quietly(file_unit)
      return
    end if

    if ( npz ) then
      call npz_end(file, file_unit, entries, member, stat, errmsg); return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_npy_r64

  module procedure to_npy_r32
    character(len=:), allocatable :: header
    type(zip_entry),  allocatable :: entries(:)
    type(zip_entry)               :: member
    integer                       :: file_unit

    header = npy_header(descr_of("f", 4), shape(x, kind=i64))

    if ( npz ) then
      if ( .not. present(name) ) then
        stat   = ARG_ERR
        errmsg = 'Error writing file "'//file//'": an array needs a name to be written to a .npz file.'
        return
      end if

      call npz_begin(file, name, len(header, kind=i64) + 4_i64*size(x, kind=i64), file_unit, entries, member, &
                     stat, errmsg)
      if ( stat /= 0 ) return
    else
      open( newunit=file_unit, file=file, status="replace", form="unformatted", action="write", access="stream", &
            iostat=stat, iomsg=errmsg )

      if ( stat /= 0 ) then
        stat = WRITE_ERR; return
      end if
    end if

    write(unit=file_unit, iostat=stat, iomsg=errmsg) header

    if ( stat == 0 ) then
      if ( is_contiguous(x) ) then                 ! One block, as gfortran moves elements one at a time in select rank
        select rank(x)
          rank(1); call write_flat_r32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(2); call write_flat_r32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(3); call write_flat_r32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(4); call write_flat_r32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(5); call write_flat_r32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(6); call write_flat_r32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(7); call write_flat_r32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(8); call write_flat_r32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(9); call write_flat_r32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(10); call write_flat_r32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(11); call write_flat_r32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(12); call write_flat_r32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(13); call write_flat_r32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(14); call write_flat_r32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(15); call write_flat_r32(file_unit, x, size(x, kind=i64), stat, errmsg)
        end select
      else
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
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR
      call close_quietly(file_unit)
      return
    end if

    if ( npz ) then
      call npz_end(file, file_unit, entries, member, stat, errmsg); return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_npy_r32

  module procedure to_npy_i64
    character(len=:), allocatable :: header
    type(zip_entry),  allocatable :: entries(:)
    type(zip_entry)               :: member
    integer                       :: file_unit

    header = npy_header(descr_of("i", 8), shape(x, kind=i64))

    if ( npz ) then
      if ( .not. present(name) ) then
        stat   = ARG_ERR
        errmsg = 'Error writing file "'//file//'": an array needs a name to be written to a .npz file.'
        return
      end if

      call npz_begin(file, name, len(header, kind=i64) + 8_i64*size(x, kind=i64), file_unit, entries, member, &
                     stat, errmsg)
      if ( stat /= 0 ) return
    else
      open( newunit=file_unit, file=file, status="replace", form="unformatted", action="write", access="stream", &
            iostat=stat, iomsg=errmsg )

      if ( stat /= 0 ) then
        stat = WRITE_ERR; return
      end if
    end if

    write(unit=file_unit, iostat=stat, iomsg=errmsg) header

    if ( stat == 0 ) then
      if ( is_contiguous(x) ) then                 ! One block, as gfortran moves elements one at a time in select rank
        select rank(x)
          rank(1); call write_flat_i64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(2); call write_flat_i64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(3); call write_flat_i64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(4); call write_flat_i64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(5); call write_flat_i64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(6); call write_flat_i64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(7); call write_flat_i64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(8); call write_flat_i64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(9); call write_flat_i64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(10); call write_flat_i64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(11); call write_flat_i64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(12); call write_flat_i64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(13); call write_flat_i64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(14); call write_flat_i64(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(15); call write_flat_i64(file_unit, x, size(x, kind=i64), stat, errmsg)
        end select
      else
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
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR
      call close_quietly(file_unit)
      return
    end if

    if ( npz ) then
      call npz_end(file, file_unit, entries, member, stat, errmsg); return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_npy_i64

  module procedure to_npy_i32
    character(len=:), allocatable :: header
    type(zip_entry),  allocatable :: entries(:)
    type(zip_entry)               :: member
    integer                       :: file_unit

    header = npy_header(descr_of("i", 4), shape(x, kind=i64))

    if ( npz ) then
      if ( .not. present(name) ) then
        stat   = ARG_ERR
        errmsg = 'Error writing file "'//file//'": an array needs a name to be written to a .npz file.'
        return
      end if

      call npz_begin(file, name, len(header, kind=i64) + 4_i64*size(x, kind=i64), file_unit, entries, member, &
                     stat, errmsg)
      if ( stat /= 0 ) return
    else
      open( newunit=file_unit, file=file, status="replace", form="unformatted", action="write", access="stream", &
            iostat=stat, iomsg=errmsg )

      if ( stat /= 0 ) then
        stat = WRITE_ERR; return
      end if
    end if

    write(unit=file_unit, iostat=stat, iomsg=errmsg) header

    if ( stat == 0 ) then
      if ( is_contiguous(x) ) then                 ! One block, as gfortran moves elements one at a time in select rank
        select rank(x)
          rank(1); call write_flat_i32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(2); call write_flat_i32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(3); call write_flat_i32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(4); call write_flat_i32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(5); call write_flat_i32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(6); call write_flat_i32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(7); call write_flat_i32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(8); call write_flat_i32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(9); call write_flat_i32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(10); call write_flat_i32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(11); call write_flat_i32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(12); call write_flat_i32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(13); call write_flat_i32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(14); call write_flat_i32(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(15); call write_flat_i32(file_unit, x, size(x, kind=i64), stat, errmsg)
        end select
      else
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
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR
      call close_quietly(file_unit)
      return
    end if

    if ( npz ) then
      call npz_end(file, file_unit, entries, member, stat, errmsg); return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_npy_i32

  module procedure to_npy_i16
    character(len=:), allocatable :: header
    type(zip_entry),  allocatable :: entries(:)
    type(zip_entry)               :: member
    integer                       :: file_unit

    header = npy_header(descr_of("i", 2), shape(x, kind=i64))

    if ( npz ) then
      if ( .not. present(name) ) then
        stat   = ARG_ERR
        errmsg = 'Error writing file "'//file//'": an array needs a name to be written to a .npz file.'
        return
      end if

      call npz_begin(file, name, len(header, kind=i64) + 2_i64*size(x, kind=i64), file_unit, entries, member, &
                     stat, errmsg)
      if ( stat /= 0 ) return
    else
      open( newunit=file_unit, file=file, status="replace", form="unformatted", action="write", access="stream", &
            iostat=stat, iomsg=errmsg )

      if ( stat /= 0 ) then
        stat = WRITE_ERR; return
      end if
    end if

    write(unit=file_unit, iostat=stat, iomsg=errmsg) header

    if ( stat == 0 ) then
      if ( is_contiguous(x) ) then                 ! One block, as gfortran moves elements one at a time in select rank
        select rank(x)
          rank(1); call write_flat_i16(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(2); call write_flat_i16(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(3); call write_flat_i16(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(4); call write_flat_i16(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(5); call write_flat_i16(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(6); call write_flat_i16(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(7); call write_flat_i16(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(8); call write_flat_i16(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(9); call write_flat_i16(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(10); call write_flat_i16(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(11); call write_flat_i16(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(12); call write_flat_i16(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(13); call write_flat_i16(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(14); call write_flat_i16(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(15); call write_flat_i16(file_unit, x, size(x, kind=i64), stat, errmsg)
        end select
      else
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
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR
      call close_quietly(file_unit)
      return
    end if

    if ( npz ) then
      call npz_end(file, file_unit, entries, member, stat, errmsg); return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_npy_i16

  module procedure to_npy_i8
    character(len=:), allocatable :: header
    type(zip_entry),  allocatable :: entries(:)
    type(zip_entry)               :: member
    integer                       :: file_unit

    header = npy_header(descr_of("i", 1), shape(x, kind=i64))

    if ( npz ) then
      if ( .not. present(name) ) then
        stat   = ARG_ERR
        errmsg = 'Error writing file "'//file//'": an array needs a name to be written to a .npz file.'
        return
      end if

      call npz_begin(file, name, len(header, kind=i64) + 1_i64*size(x, kind=i64), file_unit, entries, member, &
                     stat, errmsg)
      if ( stat /= 0 ) return
    else
      open( newunit=file_unit, file=file, status="replace", form="unformatted", action="write", access="stream", &
            iostat=stat, iomsg=errmsg )

      if ( stat /= 0 ) then
        stat = WRITE_ERR; return
      end if
    end if

    write(unit=file_unit, iostat=stat, iomsg=errmsg) header

    if ( stat == 0 ) then
      if ( is_contiguous(x) ) then                 ! One block, as gfortran moves elements one at a time in select rank
        select rank(x)
          rank(1); call write_flat_i8(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(2); call write_flat_i8(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(3); call write_flat_i8(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(4); call write_flat_i8(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(5); call write_flat_i8(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(6); call write_flat_i8(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(7); call write_flat_i8(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(8); call write_flat_i8(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(9); call write_flat_i8(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(10); call write_flat_i8(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(11); call write_flat_i8(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(12); call write_flat_i8(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(13); call write_flat_i8(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(14); call write_flat_i8(file_unit, x, size(x, kind=i64), stat, errmsg)
          rank(15); call write_flat_i8(file_unit, x, size(x, kind=i64), stat, errmsg)
        end select
      else
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
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR
      call close_quietly(file_unit)
      return
    end if

    if ( npz ) then
      call npz_end(file, file_unit, entries, member, stat, errmsg); return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_npy_i8

  module procedure to_npy_l
    character(len=:), allocatable :: header
    type(zip_entry),  allocatable :: entries(:)
    type(zip_entry)               :: member
    integer                       :: file_unit

    header = npy_header(descr_of("b", 1), shape(x, kind=i64))

    if ( npz ) then
      if ( .not. present(name) ) then
        stat   = ARG_ERR
        errmsg = 'Error writing file "'//file//'": an array needs a name to be written to a .npz file.'
        return
      end if

      call npz_begin(file, name, len(header, kind=i64) + 1_i64*size(x, kind=i64), file_unit, entries, member, &
                     stat, errmsg)
      if ( stat /= 0 ) return
    else
      open( newunit=file_unit, file=file, status="replace", form="unformatted", action="write", access="stream", &
            iostat=stat, iomsg=errmsg )

      if ( stat /= 0 ) then
        stat = WRITE_ERR; return
      end if
    end if

    write(unit=file_unit, iostat=stat, iomsg=errmsg) header

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

    if ( npz ) then
      call npz_end(file, file_unit, entries, member, stat, errmsg); return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_npy_l

  ! Reading Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  module procedure from_npy_c64
    character(len=:), allocatable :: descr, where
    integer(i64),     allocatable :: shp(:)
    integer(i64)                  :: data_end, data_pos
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

    call open_npy(file, npz, name, file_unit, where, data_end, descr, fortran_order, shp, stat, errmsg)
    if ( stat /= 0 ) return

    call check_descr(descr, "c", 16, "complex(real64)", where, swap, stat, errmsg)

    if ( (stat == 0) .and. (size(shp) /= into_rank) ) then
      stat   = READ_ERR
      errmsg = "Error reading "//where//": it holds an array of rank "//int_str(size(shp, kind=i64))// &
               ", but the output array has rank "//int_str(int(into_rank, kind=i64))//"."
    end if

    if ( stat == 0 ) then                          ! Checked before allocating, so a bad header can't ask for too much
      inquire(unit=file_unit, pos=data_pos, iostat=stat)

      if ( stat == 0 ) then
        if ( data_pos - 1_i64 + 16_i64*product(shp) > data_end ) stat = 1
      end if

      if ( stat /= 0 ) then
        stat   = READ_ERR
        errmsg = "Error reading "//where//": the data is shorter than its header says."
      end if
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
          call read_flat_c64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
      errmsg = "Error reading "//where//": the data is shorter than its header says. "//trim(errmsg)
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
    character(len=:), allocatable :: descr, where
    integer(i64),     allocatable :: shp(:)
    integer(i64)                  :: data_end, data_pos
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

    call open_npy(file, npz, name, file_unit, where, data_end, descr, fortran_order, shp, stat, errmsg)
    if ( stat /= 0 ) return

    call check_descr(descr, "c", 8, "complex(real32)", where, swap, stat, errmsg)

    if ( (stat == 0) .and. (size(shp) /= into_rank) ) then
      stat   = READ_ERR
      errmsg = "Error reading "//where//": it holds an array of rank "//int_str(size(shp, kind=i64))// &
               ", but the output array has rank "//int_str(int(into_rank, kind=i64))//"."
    end if

    if ( stat == 0 ) then                          ! Checked before allocating, so a bad header can't ask for too much
      inquire(unit=file_unit, pos=data_pos, iostat=stat)

      if ( stat == 0 ) then
        if ( data_pos - 1_i64 + 8_i64*product(shp) > data_end ) stat = 1
      end if

      if ( stat /= 0 ) then
        stat   = READ_ERR
        errmsg = "Error reading "//where//": the data is shorter than its header says."
      end if
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
          call read_flat_c32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_c32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
      errmsg = "Error reading "//where//": the data is shorter than its header says. "//trim(errmsg)
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
    character(len=:), allocatable :: descr, where
    integer(i64),     allocatable :: shp(:)
    integer(i64)                  :: data_end, data_pos
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

    call open_npy(file, npz, name, file_unit, where, data_end, descr, fortran_order, shp, stat, errmsg)
    if ( stat /= 0 ) return

    call check_descr(descr, "f", 8, "real(real64)", where, swap, stat, errmsg)

    if ( (stat == 0) .and. (size(shp) /= into_rank) ) then
      stat   = READ_ERR
      errmsg = "Error reading "//where//": it holds an array of rank "//int_str(size(shp, kind=i64))// &
               ", but the output array has rank "//int_str(int(into_rank, kind=i64))//"."
    end if

    if ( stat == 0 ) then                          ! Checked before allocating, so a bad header can't ask for too much
      inquire(unit=file_unit, pos=data_pos, iostat=stat)

      if ( stat == 0 ) then
        if ( data_pos - 1_i64 + 8_i64*product(shp) > data_end ) stat = 1
      end if

      if ( stat /= 0 ) then
        stat   = READ_ERR
        errmsg = "Error reading "//where//": the data is shorter than its header says."
      end if
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
          call read_flat_r64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
      errmsg = "Error reading "//where//": the data is shorter than its header says. "//trim(errmsg)
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
    character(len=:), allocatable :: descr, where
    integer(i64),     allocatable :: shp(:)
    integer(i64)                  :: data_end, data_pos
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

    call open_npy(file, npz, name, file_unit, where, data_end, descr, fortran_order, shp, stat, errmsg)
    if ( stat /= 0 ) return

    call check_descr(descr, "f", 4, "real(real32)", where, swap, stat, errmsg)

    if ( (stat == 0) .and. (size(shp) /= into_rank) ) then
      stat   = READ_ERR
      errmsg = "Error reading "//where//": it holds an array of rank "//int_str(size(shp, kind=i64))// &
               ", but the output array has rank "//int_str(int(into_rank, kind=i64))//"."
    end if

    if ( stat == 0 ) then                          ! Checked before allocating, so a bad header can't ask for too much
      inquire(unit=file_unit, pos=data_pos, iostat=stat)

      if ( stat == 0 ) then
        if ( data_pos - 1_i64 + 4_i64*product(shp) > data_end ) stat = 1
      end if

      if ( stat /= 0 ) then
        stat   = READ_ERR
        errmsg = "Error reading "//where//": the data is shorter than its header says."
      end if
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
          call read_flat_r32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_r32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
      errmsg = "Error reading "//where//": the data is shorter than its header says. "//trim(errmsg)
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
    character(len=:), allocatable :: descr, where
    integer(i64),     allocatable :: shp(:)
    integer(i64)                  :: data_end, data_pos
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

    call open_npy(file, npz, name, file_unit, where, data_end, descr, fortran_order, shp, stat, errmsg)
    if ( stat /= 0 ) return

    call check_descr(descr, "i", 8, "integer(int64)", where, swap, stat, errmsg)

    if ( (stat == 0) .and. (size(shp) /= into_rank) ) then
      stat   = READ_ERR
      errmsg = "Error reading "//where//": it holds an array of rank "//int_str(size(shp, kind=i64))// &
               ", but the output array has rank "//int_str(int(into_rank, kind=i64))//"."
    end if

    if ( stat == 0 ) then                          ! Checked before allocating, so a bad header can't ask for too much
      inquire(unit=file_unit, pos=data_pos, iostat=stat)

      if ( stat == 0 ) then
        if ( data_pos - 1_i64 + 8_i64*product(shp) > data_end ) stat = 1
      end if

      if ( stat /= 0 ) then
        stat   = READ_ERR
        errmsg = "Error reading "//where//": the data is shorter than its header says."
      end if
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
          call read_flat_i64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i64(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
      errmsg = "Error reading "//where//": the data is shorter than its header says. "//trim(errmsg)
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
    character(len=:), allocatable :: descr, where
    integer(i64),     allocatable :: shp(:)
    integer(i64)                  :: data_end, data_pos
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

    call open_npy(file, npz, name, file_unit, where, data_end, descr, fortran_order, shp, stat, errmsg)
    if ( stat /= 0 ) return

    call check_descr(descr, "i", 4, "integer(int32)", where, swap, stat, errmsg)

    if ( (stat == 0) .and. (size(shp) /= into_rank) ) then
      stat   = READ_ERR
      errmsg = "Error reading "//where//": it holds an array of rank "//int_str(size(shp, kind=i64))// &
               ", but the output array has rank "//int_str(int(into_rank, kind=i64))//"."
    end if

    if ( stat == 0 ) then                          ! Checked before allocating, so a bad header can't ask for too much
      inquire(unit=file_unit, pos=data_pos, iostat=stat)

      if ( stat == 0 ) then
        if ( data_pos - 1_i64 + 4_i64*product(shp) > data_end ) stat = 1
      end if

      if ( stat /= 0 ) then
        stat   = READ_ERR
        errmsg = "Error reading "//where//": the data is shorter than its header says."
      end if
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
          call read_flat_i32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i32(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
      errmsg = "Error reading "//where//": the data is shorter than its header says. "//trim(errmsg)
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
    character(len=:), allocatable :: descr, where
    integer(i64),     allocatable :: shp(:)
    integer(i64)                  :: data_end, data_pos
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

    call open_npy(file, npz, name, file_unit, where, data_end, descr, fortran_order, shp, stat, errmsg)
    if ( stat /= 0 ) return

    call check_descr(descr, "i", 2, "integer(int16)", where, swap, stat, errmsg)

    if ( (stat == 0) .and. (size(shp) /= into_rank) ) then
      stat   = READ_ERR
      errmsg = "Error reading "//where//": it holds an array of rank "//int_str(size(shp, kind=i64))// &
               ", but the output array has rank "//int_str(int(into_rank, kind=i64))//"."
    end if

    if ( stat == 0 ) then                          ! Checked before allocating, so a bad header can't ask for too much
      inquire(unit=file_unit, pos=data_pos, iostat=stat)

      if ( stat == 0 ) then
        if ( data_pos - 1_i64 + 2_i64*product(shp) > data_end ) stat = 1
      end if

      if ( stat /= 0 ) then
        stat   = READ_ERR
        errmsg = "Error reading "//where//": the data is shorter than its header says."
      end if
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
          call read_flat_i16(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i16(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i16(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i16(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i16(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i16(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i16(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i16(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i16(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i16(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i16(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i16(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i16(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i16(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i16(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
      errmsg = "Error reading "//where//": the data is shorter than its header says. "//trim(errmsg)
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
    character(len=:), allocatable :: descr, where
    integer(i64),     allocatable :: shp(:)
    integer(i64)                  :: data_end, data_pos
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

    call open_npy(file, npz, name, file_unit, where, data_end, descr, fortran_order, shp, stat, errmsg)
    if ( stat /= 0 ) return

    call check_descr(descr, "i", 1, "integer(int8)", where, swap, stat, errmsg)

    if ( (stat == 0) .and. (size(shp) /= into_rank) ) then
      stat   = READ_ERR
      errmsg = "Error reading "//where//": it holds an array of rank "//int_str(size(shp, kind=i64))// &
               ", but the output array has rank "//int_str(int(into_rank, kind=i64))//"."
    end if

    if ( stat == 0 ) then                          ! Checked before allocating, so a bad header can't ask for too much
      inquire(unit=file_unit, pos=data_pos, iostat=stat)

      if ( stat == 0 ) then
        if ( data_pos - 1_i64 + 1_i64*product(shp) > data_end ) stat = 1
      end if

      if ( stat /= 0 ) then
        stat   = READ_ERR
        errmsg = "Error reading "//where//": the data is shorter than its header says."
      end if
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
          call read_flat_i8(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i8(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i8(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i8(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i8(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i8(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i8(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i8(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i8(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i8(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i8(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i8(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i8(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i8(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
          call read_flat_i8(file_unit, into, size(into, kind=i64), stat, errmsg)   ! As one block
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
      errmsg = "Error reading "//where//": the data is shorter than its header says. "//trim(errmsg)
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
    character(len=:), allocatable :: descr, where
    integer(i64),     allocatable :: shp(:)
    integer(i64)                  :: data_end, data_pos
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

    call open_npy(file, npz, name, file_unit, where, data_end, descr, fortran_order, shp, stat, errmsg)
    if ( stat /= 0 ) return

    call check_descr(descr, "b", 1, "logical", where, swap, stat, errmsg)

    if ( (stat == 0) .and. (size(shp) /= into_rank) ) then
      stat   = READ_ERR
      errmsg = "Error reading "//where//": it holds an array of rank "//int_str(size(shp, kind=i64))// &
               ", but the output array has rank "//int_str(int(into_rank, kind=i64))//"."
    end if

    if ( stat == 0 ) then                          ! Checked before allocating, so a bad header can't ask for too much
      inquire(unit=file_unit, pos=data_pos, iostat=stat)

      if ( stat == 0 ) then
        if ( data_pos - 1_i64 + 1_i64*product(shp) > data_end ) stat = 1
      end if

      if ( stat /= 0 ) then
        stat   = READ_ERR
        errmsg = "Error reading "//where//": the data is shorter than its header says."
      end if
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
      errmsg = "Error reading "//where//": the data is shorter than its header says. "//trim(errmsg)
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

  subroutine open_npy(file, npz, name, file_unit, where, data_end, descr, fortran_order, shp, stat, errmsg)
    !! Opens `file` and reads the header of the `.npy` file it is, or, if `npz`, of its array `name` (or of its only
    !! array, if `name` is absent), and leaves `file_unit` at the start of the data. `where` names the file or the
    !! array for messages, and `data_end` is the position of the last byte the data may take.
    character(len=*),              intent(in)           :: file
    logical,                       intent(in)           :: npz
    character(len=*),              intent(in), optional :: name
    integer,                       intent(out)          :: file_unit
    character(len=:), allocatable, intent(out)          :: where
    integer(i64),                  intent(out)          :: data_end
    character(len=:), allocatable, intent(out)          :: descr
    logical,                       intent(out)          :: fortran_order
    integer(i64),     allocatable, intent(out)          :: shp(:)
    integer,                       intent(out)          :: stat
    character(len=*),              intent(out)          :: errmsg

    character(len=8)              :: lead
    character(len=4)              :: length
    character(len=:), allocatable :: header
    integer                       :: header_len, i
    integer(i64)                  :: start
    logical                       :: exists

    allocate( shp(0) ); descr = EMPTY_STR; fortran_order = .false.; file_unit = input_unit
    where = 'file "'//file//'"'; data_end = 0_i64; start = 1_i64

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

    inquire(unit=file_unit, size=data_end, iostat=stat, iomsg=errmsg)
    if ( stat /= 0 ) stat = READ_ERR

    if ( (stat == 0) .and. npz ) call find_member(file, file_unit, name, where, start, data_end, stat, errmsg)

    if ( stat /= 0 ) then
      call close_quietly(file_unit); return
    end if

    read(unit=file_unit, pos=start, iostat=stat, iomsg=errmsg) lead

    if ( (stat /= 0) .or. (lead(1:6) /= NPY_MAGIC) ) then
      stat   = READ_ERR
      errmsg = "Error reading "//where//": it is not a .npy file."
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
        errmsg = "Error reading "//where//": .npy version "//int_str(int(iachar(lead(7:7)), kind=i64))// &
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
      errmsg = "Error reading "//where//": its .npy header could not be read."
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

  subroutine check_descr(descr, code, size, type_name, where, swap, stat, errmsg)
    !! Checks that `descr` describes `code` data of `size` bytes, and whether its bytes need swapping.
    character(len=*), intent(in)  :: descr, code, type_name, where
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
      errmsg = "Error reading "//where//': it holds NumPy data of type "'//descr//'", which cannot be read '// &
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

  ! NumPy .npz archives ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  module procedure npz_names
    type(zip_entry),  allocatable :: entries(:)
    character(len=:), pointer     :: errmsg_
    integer,          pointer     :: stat_
    integer(i64)                  :: cd_start
    integer                       :: file_unit, i
    logical                       :: exists

    character(len=1024), target :: dummy_msg
    integer,             target :: dummy_stat

    if ( .not. present(stat) ) then
      stat_ => dummy_stat
    else
      stat_ => stat
    end if

    if ( .not. present(errmsg) ) then
      errmsg_ => dummy_msg
    else
      errmsg_ => errmsg
    end if

    stat_=0; errmsg_=EMPTY_STR
    allocate( names(0) )

    inquire(file=file, exist=exists, iostat=stat_, iomsg=errmsg_)

    if ( (stat_ == 0) .and. (.not. exists) ) then
      stat_   = READ_ERR
      errmsg_ = 'Error reading file "'//file//'". No such file exists.'
    else if ( stat_ == 0 ) then
      open( newunit=file_unit, file=file, status="old", form="unformatted", action="read", access="stream", &
            iostat=stat_, iomsg=errmsg_ )

      if ( stat_ == 0 ) then
        call read_zip_directory(file, file_unit, entries, cd_start, stat_, errmsg_)
        call close_quietly(file_unit)
      end if

      if ( stat_ == 0 ) then
        deallocate( names ); allocate( names(size(entries)) )
        do i = 1, size(entries)
          names(i)%buf = array_name(entries(i)%name); names(i)%n = len(names(i)%buf, kind=i64)
        end do
      end if
    end if

    if ( stat_ /= 0 ) stat_ = READ_ERR

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure npz_names

  subroutine find_member(file, file_unit, name, where, start, data_end, stat, errmsg)
    !! Finds the array `name` in the archive open on `file_unit`, or its only array if `name` is absent. `start` is
    !! the position of its first byte, and `data_end` of its last. `name` matches a file in the archive by its name,
    !! or by its name with `.npy` added, as `numpy.load` does.
    character(len=*),              intent(in)           :: file
    integer,                       intent(in)           :: file_unit
    character(len=*),              intent(in), optional :: name
    character(len=:), allocatable, intent(inout)        :: where
    integer(i64),                  intent(out)          :: start, data_end
    integer,                       intent(out)          :: stat
    character(len=*),              intent(out)          :: errmsg

    type(zip_entry), allocatable :: entries(:)
    character(len=30)            :: local
    integer(i64)                 :: cd_start
    integer                      :: i, k

    start = 1_i64; data_end = 0_i64

    call read_zip_directory(file, file_unit, entries, cd_start, stat, errmsg)
    if ( stat /= 0 ) return

    k = 0
    if ( present(name) ) then
      do i = 1, size(entries)
        if ( same_text(entries(i)%name, trim(name)) ) then
          k = i; exit
        end if
      end do

      if ( k == 0 ) then
        do i = 1, size(entries)
          if ( same_text(entries(i)%name, trim(name)//".npy") ) then
            k = i; exit
          end if
        end do
      end if

      if ( k == 0 ) then
        stat   = READ_ERR
        errmsg = 'Error reading file "'//file//'": it holds no array named "'//trim(name)//'". '//array_list(entries)
        return
      end if
    else if ( size(entries) /= 1 ) then
      stat   = ARG_ERR
      errmsg = 'Error reading file "'//file//'": it holds '//int_str(size(entries, kind=i64))//" arrays, so name "// &
               "must say which one to read. "//array_list(entries)
      return
    else
      k = 1
    end if

    where = 'array "'//array_name(entries(k)%name)//'" in file "'//file//'"'

    if ( btest(entries(k)%flags, 0) ) then
      stat   = READ_ERR
      errmsg = "Error reading "//where//": it is encrypted."
      return
    else if ( entries(k)%method /= 0_i64 ) then
      stat   = READ_ERR
      errmsg = "Error reading "//where//": it is compressed, as numpy.savez_compressed writes it. Only "// &
               "uncompressed archives, as numpy.savez writes them, can be read."
      return
    end if

    read(unit=file_unit, pos=entries(k)%offset+1_i64, iostat=stat, iomsg=errmsg) local

    if ( stat == 0 ) then
      if ( local(1:4) /= ZIP_LOCAL ) stat = 1
    end if

    if ( stat /= 0 ) then
      stat   = READ_ERR
      errmsg = 'Error reading file "'//file//'": it is not a .npz archive, or it is damaged.'
      return
    end if

    start    = entries(k)%offset + 31_i64 + le_int(local(27:28)) + le_int(local(29:30))
    data_end = start - 1_i64 + entries(k)%csize
  end subroutine find_member

  subroutine read_zip_directory(file, file_unit, entries, cd_start, stat, errmsg)
    !! Reads the central directory of the ZIP archive open on `file_unit` into `entries`. `cd_start` is the offset
    !! of the directory, which is where the files end.
    character(len=*),             intent(in)  :: file
    integer,                      intent(in)  :: file_unit
    type(zip_entry), allocatable, intent(out) :: entries(:)
    integer(i64),                 intent(out) :: cd_start
    integer,                      intent(out) :: stat
    character(len=*),             intent(out) :: errmsg

    character(len=:), allocatable :: tail, cd
    character(len=56)             :: end64
    character(len=20)             :: locator
    integer(i64)                  :: file_size, n, eocd, count, cd_size, i, p, q, q_end, r, id, field_len
    integer(i64)                  :: name_len, extra_len, comment_len

    allocate( entries(0) ); cd_start = 0_i64

    inquire(unit=file_unit, size=file_size, iostat=stat, iomsg=errmsg)
    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    ! The end record is the last 22 bytes, unless the archive has a comment, which is at most 65535 bytes.
    stat = 1
    if ( file_size >= 22_i64 ) then
      n = min(file_size, 22_i64 + MAX_U16)
      allocate( character(len=n) :: tail )
      read(unit=file_unit, pos=file_size-n+1_i64, iostat=stat) tail
    end if

    if ( stat == 0 ) then
      i = index(tail(:n-18_i64), ZIP_END, back=.true.)                     ! The whole record must fit
      if ( i == 0_i64 ) stat = 1
    end if

    if ( stat == 0 ) then
      eocd     = file_size - n + i                                                           ! Position of the record
      count    = le_int(tail(i+10_i64:i+11_i64))
      cd_size  = le_int(tail(i+12_i64:i+15_i64))
      cd_start = le_int(tail(i+16_i64:i+19_i64))

      if ( (count == MAX_U16) .or. (cd_size == MAX_U32) .or. (cd_start == MAX_U32) ) then
        stat = 1
        if ( eocd > 20_i64 ) read(unit=file_unit, pos=eocd-20_i64, iostat=stat) locator

        if ( stat == 0 ) then
          if ( locator(1:4) /= ZIP64_LOCATOR ) stat = 1
        end if

        if ( stat == 0 ) read(unit=file_unit, pos=le_int(locator(9:16))+1_i64, iostat=stat) end64

        if ( stat == 0 ) then
          if ( end64(1:4) /= ZIP64_END ) stat = 1
        end if

        if ( stat == 0 ) then
          count    = le_int(end64(33:40))
          cd_size  = le_int(end64(41:48))
          cd_start = le_int(end64(49:56))
        end if
      end if
    end if

    if ( stat == 0 ) then                          ! A directory record is at least 46 bytes
      if ( (cd_start + cd_size > file_size) .or. (46_i64*count > cd_size) ) stat = 1
    end if

    if ( stat == 0 ) then
      allocate( character(len=cd_size) :: cd )
      if ( cd_size > 0_i64 ) read(unit=file_unit, pos=cd_start+1_i64, iostat=stat) cd
    end if

    if ( stat == 0 ) then
      deallocate( entries ); allocate( entries(count) )
      p = 1_i64

      do i = 1_i64, count
        stat = 1
        if ( p + 45_i64 > cd_size ) exit
        if ( cd(p:p+3_i64) /= ZIP_CENTRAL ) exit

        name_len    = le_int(cd(p+28_i64:p+29_i64))
        extra_len   = le_int(cd(p+30_i64:p+31_i64))
        comment_len = le_int(cd(p+32_i64:p+33_i64))
        if ( p + 45_i64 + name_len + extra_len + comment_len > cd_size ) exit

        entries(i)%flags  = le_int(cd(p+8_i64:p+9_i64))
        entries(i)%method = le_int(cd(p+10_i64:p+11_i64))
        entries(i)%time   = le_int(cd(p+12_i64:p+13_i64))
        entries(i)%date   = le_int(cd(p+14_i64:p+15_i64))
        entries(i)%crc    = le_int(cd(p+16_i64:p+19_i64))
        entries(i)%csize  = le_int(cd(p+20_i64:p+23_i64))
        entries(i)%usize  = le_int(cd(p+24_i64:p+27_i64))
        entries(i)%offset = le_int(cd(p+42_i64:p+45_i64))
        entries(i)%name   = cd(p+46_i64:p+45_i64+name_len)

        ! A ZIP64 extra field holds, in this order, each of these that is too big for its place above.
        q = p + 46_i64 + name_len; q_end = q + extra_len - 1_i64
        do while ( q + 3_i64 <= q_end )
          id = le_int(cd(q:q+1_i64)); field_len = le_int(cd(q+2_i64:q+3_i64))

          if ( id == 1_i64 ) then
            r = q + 4_i64
            if ( (entries(i)%usize == MAX_U32) .and. (r + 7_i64 <= q_end) ) then
              entries(i)%usize = le_int(cd(r:r+7_i64)); r = r + 8_i64
            end if
            if ( (entries(i)%csize == MAX_U32) .and. (r + 7_i64 <= q_end) ) then
              entries(i)%csize = le_int(cd(r:r+7_i64)); r = r + 8_i64
            end if
            if ( (entries(i)%offset == MAX_U32) .and. (r + 7_i64 <= q_end) ) then
              entries(i)%offset = le_int(cd(r:r+7_i64))
            end if
          end if

          q = q + 4_i64 + field_len
        end do

        p = p + 46_i64 + name_len + extra_len + comment_len
        stat = 0
      end do
    end if

    if ( stat /= 0 ) then
      stat   = READ_ERR
      errmsg = 'Error reading file "'//file//'": it is not a .npz archive, or it is damaged.'
      deallocate( entries ); allocate( entries(0) )
    else
      errmsg = EMPTY_STR
    end if
  end subroutine read_zip_directory

  subroutine npz_begin(file, name, usize, file_unit, entries, member, stat, errmsg)
    !! Opens the archive `file`, or creates it, and removes any array called `name` from it. It then writes the local
    !! header of a new file for `name`, of `usize` bytes, and leaves `file_unit` where the data goes. `npz_end`
    !! finishes the archive.
    character(len=*),             intent(in)  :: file, name
    integer(i64),                 intent(in)  :: usize
    integer,                      intent(out) :: file_unit
    type(zip_entry), allocatable, intent(out) :: entries(:)
    type(zip_entry),              intent(out) :: member
    integer,                      intent(out) :: stat
    character(len=*),             intent(out) :: errmsg

    character(len=:), allocatable :: local
    integer(i64)                  :: cd_start, finish, gap
    integer                       :: values(8), i, k
    logical                       :: exists, zip64

    allocate( entries(0) ); file_unit = input_unit

    inquire(file=file, exist=exists, iostat=stat, iomsg=errmsg)

    if ( stat == 0 ) then
      if ( exists ) then
        open( newunit=file_unit, file=file, status="old", form="unformatted", action="readwrite", access="stream", &
              iostat=stat, iomsg=errmsg )
      else
        open( newunit=file_unit, file=file, status="new", form="unformatted", action="readwrite", access="stream", &
              iostat=stat, iomsg=errmsg )
      end if
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    cd_start = 0_i64
    if ( exists ) then
      call read_zip_directory(file, file_unit, entries, cd_start, stat, errmsg)

      if ( stat /= 0 ) then
        stat   = WRITE_ERR
        errmsg = 'Error writing file "'//file//'": it is not a .npz archive, or it is damaged, so it was left '// &
                 "unchanged."
        call close_quietly(file_unit)
        return
      end if
    end if

    member%name = trim(name)//".npy"                                ! Trailing blanks are ignored

    k = 0
    do i = 1, size(entries)
      if ( same_text(entries(i)%name, member%name) ) then
        k = i; exit
      end if
    end do

    if ( k > 0 ) then                              ! Move the files after it down over it
      finish = cd_start
      do i = 1, size(entries)
        if ( entries(i)%offset > entries(k)%offset ) finish = min(finish, entries(i)%offset)
      end do

      call move_bytes(file_unit, finish, cd_start, entries(k)%offset, stat, errmsg)

      if ( stat /= 0 ) then
        stat = WRITE_ERR
        call close_quietly(file_unit)
        return
      end if

      gap = finish - entries(k)%offset
      do i = 1, size(entries)
        if ( entries(i)%offset > entries(k)%offset ) entries(i)%offset = entries(i)%offset - gap
      end do
      cd_start = cd_start - gap
      entries = [ entries(:k-1), entries(k+1:) ]
    end if

    call date_and_time(values=values)

    member%offset = cd_start
    member%usize  = usize
    member%csize  = usize
    member%time   = int(values(5)*2048 + values(6)*32 + values(7)/2, kind=i64)                 ! MS-DOS time and date
    member%date   = int(max(values(1)-1980, 0)*512 + values(2)*32 + values(3), kind=i64)
    do i = 1, len(member%name)
      if ( iachar(member%name(i:i)) > 127 ) member%flags = 2048_i64                          ! The name is UTF-8
    end do

    zip64 = (usize >= MAX_U32)

    local = ZIP_LOCAL//le_bytes(merge(45_i64, 20_i64, zip64), 2)//le_bytes(member%flags, 2)//le_bytes(0_i64, 2)// &
            le_bytes(member%time, 2)//le_bytes(member%date, 2)//le_bytes(0_i64, 4)        ! The CRC is written later

    if ( zip64 ) then
      local = local//le_bytes(MAX_U32, 4)//le_bytes(MAX_U32, 4)//le_bytes(len(member%name, kind=i64), 2)// &
              le_bytes(20_i64, 2)//member%name//le_bytes(1_i64, 2)//le_bytes(16_i64, 2)//le_bytes(usize, 8)// &
              le_bytes(usize, 8)
    else
      local = local//le_bytes(usize, 4)//le_bytes(usize, 4)//le_bytes(len(member%name, kind=i64), 2)// &
              le_bytes(0_i64, 2)//member%name
    end if

    member%data_offset = cd_start + len(local, kind=i64)

    write(unit=file_unit, pos=cd_start+1_i64, iostat=stat, iomsg=errmsg) local

    if ( stat /= 0 ) then
      stat = WRITE_ERR
      call close_quietly(file_unit)
      return
    end if
  end subroutine npz_begin

  subroutine npz_end(file, file_unit, entries, member, stat, errmsg)
    !! Finishes the file `member` that `npz_begin` started: writes its CRC, then the central directory and end
    !! records after it, cuts off anything left beyond them, and closes the archive.
    character(len=*),             intent(in)    :: file
    integer,                      intent(in)    :: file_unit
    type(zip_entry), allocatable, intent(inout) :: entries(:)
    type(zip_entry),              intent(inout) :: member
    integer,                      intent(out)   :: stat
    character(len=*),             intent(out)   :: errmsg

    character(len=:), allocatable :: buffer, cd, ending
    integer(i64)                  :: table(0:255,0:7), crc, pos, left, n, cd_start, cd_size, count
    integer                       :: i

    table = crc_table()
    crc   = MAX_U32
    pos   = member%data_offset + 1_i64
    left  = member%usize
    stat  = 0

    allocate( character(len=min(max(left, 1_i64), CHUNK)) :: buffer )
    do while ( (left > 0_i64) .and. (stat == 0) )                ! Reads the data back, so as not to copy the array
      n = min(left, CHUNK)
      read(unit=file_unit, pos=pos, iostat=stat, iomsg=errmsg) buffer(:n)
      if ( stat == 0 ) call crc_update(crc, buffer(:n), table)
      pos = pos + n; left = left - n
    end do

    member%crc = ieor(crc, MAX_U32)

    if ( stat == 0 ) write(unit=file_unit, pos=member%offset+15_i64, iostat=stat, iomsg=errmsg) le_bytes(member%crc, 4)

    if ( stat == 0 ) then
      entries = [ entries, member ]

      cd = EMPTY_STR
      do i = 1, size(entries)
        cd = cd//central_record(entries(i))
      end do

      cd_start = member%data_offset + member%usize
      cd_size  = len(cd, kind=i64)
      count    = size(entries, kind=i64)

      ending = EMPTY_STR
      if ( (count >= MAX_U16) .or. (cd_size >= MAX_U32) .or. (cd_start >= MAX_U32) ) then
        ending = ZIP64_END//le_bytes(44_i64, 8)//le_bytes(45_i64, 2)//le_bytes(45_i64, 2)//le_bytes(0_i64, 4)// &
                 le_bytes(0_i64, 4)//le_bytes(count, 8)//le_bytes(count, 8)//le_bytes(cd_size, 8)// &
                 le_bytes(cd_start, 8)//ZIP64_LOCATOR//le_bytes(0_i64, 4)//le_bytes(cd_start+cd_size, 8)// &
                 le_bytes(1_i64, 4)
      end if

      ending = ending//ZIP_END//le_bytes(0_i64, 2)//le_bytes(0_i64, 2)//le_bytes(min(count, MAX_U16), 2)// &
               le_bytes(min(count, MAX_U16), 2)//le_bytes(min(cd_size, MAX_U32), 4)// &
               le_bytes(min(cd_start, MAX_U32), 4)//le_bytes(0_i64, 2)

      write(unit=file_unit, pos=cd_start+1_i64, iostat=stat, iomsg=errmsg) cd//ending
    end if

    if ( stat == 0 ) endfile(unit=file_unit, iostat=stat, iomsg=errmsg)      ! Cuts off what a longer array left

    if ( stat /= 0 ) then
      stat   = WRITE_ERR
      errmsg = 'Error writing file "'//file//'": '//trim(errmsg)
      call close_quietly(file_unit)
      return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end subroutine npz_end

  subroutine move_bytes(file_unit, from, to, dest, stat, errmsg)
    !! Moves the bytes at offsets `from` to `to` - 1 down to offset `dest`, which is less than `from`.
    integer,          intent(in)  :: file_unit
    integer(i64),     intent(in)  :: from, to, dest
    integer,          intent(out) :: stat
    character(len=*), intent(out) :: errmsg

    character(len=:), allocatable :: buffer
    integer(i64)                  :: done, n

    stat = 0; done = 0_i64
    allocate( character(len=min(max(to-from, 1_i64), CHUNK)) :: buffer )

    do while ( (done < to - from) .and. (stat == 0) )
      n = min(to - from - done, CHUNK)
      read(unit=file_unit, pos=from+done+1_i64, iostat=stat, iomsg=errmsg) buffer(:n)
      if ( stat == 0 ) write(unit=file_unit, pos=dest+done+1_i64, iostat=stat, iomsg=errmsg) buffer(:n)
      done = done + n
    end do
  end subroutine move_bytes

  pure function central_record(entry) result(record)
    !! Returns the central directory record for `entry`.
    type(zip_entry), intent(in)   :: entry
    character(len=:), allocatable :: record

    character(len=:), allocatable :: extra
    integer(i64)                  :: version

    extra = EMPTY_STR
    if ( entry%usize  >= MAX_U32 ) extra = extra//le_bytes(entry%usize, 8)
    if ( entry%csize  >= MAX_U32 ) extra = extra//le_bytes(entry%csize, 8)
    if ( entry%offset >= MAX_U32 ) extra = extra//le_bytes(entry%offset, 8)
    if ( len(extra) > 0 ) extra = le_bytes(1_i64, 2)//le_bytes(len(extra, kind=i64), 2)//extra

    version = 20_i64
    if ( len(extra) > 0 ) version = 45_i64

    record = ZIP_CENTRAL//le_bytes(45_i64, 2)//le_bytes(version, 2)//le_bytes(entry%flags, 2)// &
             le_bytes(entry%method, 2)//le_bytes(entry%time, 2)//le_bytes(entry%date, 2)//le_bytes(entry%crc, 4)// &
             le_bytes(min(entry%csize, MAX_U32), 4)//le_bytes(min(entry%usize, MAX_U32), 4)// &
             le_bytes(len(entry%name, kind=i64), 2)//le_bytes(len(extra, kind=i64), 2)//le_bytes(0_i64, 2)// &
             le_bytes(0_i64, 2)//le_bytes(0_i64, 2)//le_bytes(0_i64, 4)//le_bytes(min(entry%offset, MAX_U32), 4)// &
             entry%name//extra
  end function central_record

  pure function crc_table() result(table)
    !! Returns the tables for computing the CRC-32 that ZIP uses eight bytes at a time ("slicing by 8"). Column 0 is
    !! the usual table for one byte. Column `k` gives the effect of a byte followed by `k` zero bytes.
    integer(i64) :: table(0:255,0:7)

    integer(i64) :: c
    integer      :: i, j, k

    do i = 0, 255
      c = int(i, kind=i64)
      do j = 1, 8
        if ( btest(c, 0) ) then
          c = ieor(3988292384_i64, shiftr(c, 1))                                                ! 0xEDB88320
        else
          c = shiftr(c, 1)
        end if
      end do
      table(i,0) = c
    end do

    do k = 1, 7
      table(:,k) = ieor(shiftr(table(:,k-1), 8), table(iand(table(:,k-1), 255_i64),0))
    end do
  end function crc_table

  pure subroutine crc_update(crc, bytes, table)
    !! Updates `crc`, which starts at `MAX_U32`, with `bytes`, eight at a time and then any left over one at a time.
    !! This is about four times as fast as a byte at a time (#94).
    integer(i64),     intent(inout) :: crc
    character(len=*), intent(in)    :: bytes
    integer(i64),     intent(in)    :: table(0:255,0:7)

    integer(i64) :: i, n, b(0:7)
    integer      :: k

    n = len(bytes, kind=i64)
    i = 1_i64

    do while ( i + 7_i64 <= n )
      do k = 0, 7
        b(k) = int(ichar(bytes(i+k:i+k)), kind=i64)
      end do

      crc = ieor(crc, ior(ior(b(0), shiftl(b(1), 8)), ior(shiftl(b(2), 16), shiftl(b(3), 24))))
      crc = ieor(ieor(ieor(table(iand(crc, 255_i64),7), table(iand(shiftr(crc, 8), 255_i64),6)),   &
                      ieor(table(iand(shiftr(crc, 16), 255_i64),5), table(shiftr(crc, 24),4))),     &
                 ieor(ieor(table(b(4),3), table(b(5),2)), ieor(table(b(6),1), table(b(7),0))))
      i = i + 8_i64
    end do

    do while ( i <= n )
      crc = ieor(table(iand(ieor(crc, int(ichar(bytes(i:i)), kind=i64)), 255_i64),0), shiftr(crc, 8))
      i = i + 1_i64
    end do
  end subroutine crc_update

  pure function le_int(bytes) result(n)
    !! Returns the unsigned little-endian integer in `bytes`.
    character(len=*), intent(in) :: bytes
    integer(i64)                 :: n

    integer :: i

    n = 0_i64
    do i = len(bytes), 1, -1
      n = 256_i64*n + int(modulo(iachar(bytes(i:i)), 256), kind=i64)
    end do
  end function le_int

  pure function le_bytes(n, width) result(bytes)
    !! Returns `n`, which is not negative, as a `width`-byte little-endian integer.
    integer(i64), intent(in) :: n
    integer,      intent(in) :: width
    character(len=width)     :: bytes

    integer(i64) :: m
    integer      :: i

    m = n
    do i = 1, width
      bytes(i:i) = achar(int(modulo(m, 256_i64)))
      m = m/256_i64
    end do
  end function le_bytes

  pure logical function same_text(a, b)
    !! Whether `a` and `b` are the same, including their lengths, which `==` ignores by padding with blanks.
    character(len=*), intent(in) :: a, b

    same_text = (len(a) == len(b))
    if ( same_text ) same_text = (a == b)
  end function same_text

  pure function array_name(file_name) result(name)
    !! Returns the name of the array stored in `file_name`, which is `file_name` without a final `.npy`.
    character(len=*), intent(in)  :: file_name
    character(len=:), allocatable :: name

    name = file_name
    if ( len(name) >= 4 ) then
      if ( name(len(name)-3:) == ".npy" ) name = name(:len(name)-4)
    end if
  end function array_name

  pure function array_list(entries) result(text)
    !! Returns a sentence listing the arrays in `entries`.
    type(zip_entry), intent(in)   :: entries(:)
    character(len=:), allocatable :: text

    integer :: i

    if ( size(entries) == 0 ) then
      text = "It holds no arrays."; return
    end if

    text = "Its arrays are: "
    do i = 1, size(entries)
      if ( i > 1 ) text = text//", "
      text = text//'"'//array_name(entries(i)%name)//'"'
    end do
    text = text//"."
  end function array_list
end submodule io_npy
