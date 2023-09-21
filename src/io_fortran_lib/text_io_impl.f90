submodule (io_fortran_lib) text_io
  !---------------------------------------------------------------------------------------------------------------------
  !! This submodule provides module procedure implementations for the **public interface** `echo` and the **private
  !! interfaces** `to_text` and `from_text`.
  !---------------------------------------------------------------------------------------------------------------------
  implicit none (type, external)

  ! Definitions and interfaces ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  integer,          target :: ROW_DIM         = 1
  integer,          target :: COL_DIM         = 2

  contains ! Procedure bodies for module subprograms <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

  ! Writing Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  module procedure echo_chars
    character(len=:), allocatable :: ext, terminator_
    logical                       :: exists, append_
    integer                       :: file_unit

    exists=.false.; append_=.false.; file_unit=0

    ext = ext_of(file)

    if ( .not. any(TEXT_EXT == ext) ) then
      write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                'due to unsupported file extension "'//ext//'".'// &
              LF//'Supported file extensions: '//join(TEXT_EXT)
      return
    end if

    if ( len(substring, kind=i64) == 0_i64 ) then
      write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". '// &
                'String to write is empty.'
      return
    end if

    if ( .not. present(append) ) then
      append_ = .true.
    else
      append_ = append
    end if

    if ( .not. present(terminator) ) then
      terminator_ = LF
    else
      terminator_ = terminator
    end if

    inquire(file=file, exist=exists)

    file_unit = output_unit

    if ( .not. exists ) then
      open( newunit=file_unit, file=file, status="new", form="unformatted", &
          action="write", access="stream" )
    else
      if ( .not. append_ ) then
        open( newunit=file_unit, file=file, status="replace", form="unformatted", &
            action="write", access="stream" )
      else
        open( newunit=file_unit, file=file, status="old", form="unformatted", &
            action="write", access="stream", position="append" )
      end if
    end if

    write(unit=file_unit) substring//terminator_

    close(file_unit)
  end procedure echo_chars

  module procedure echo_string
    character(len=:), allocatable :: ext, terminator_
    logical                       :: exists, append_
    integer                       :: file_unit

    exists=.false.; append_=.false.; file_unit=0

    ext = ext_of(file)

    if ( .not. any(TEXT_EXT == ext) ) then
      write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                'due to unsupported file extension "'//ext//'".'// &
              LF//'Supported file extensions: '//join(TEXT_EXT)
      return
    end if

    if ( substring%len64() < 1_i64 ) then
      write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". '// &
                'String to write is empty.'
      return
    end if

    if ( .not. present(append) ) then
      append_ = .true.
    else
      append_ = append
    end if

    if ( .not. present(terminator) ) then
      terminator_ = LF
    else
      terminator_ = terminator
    end if

    inquire(file=file, exist=exists)

    file_unit = output_unit

    if ( .not. exists ) then
      open( newunit=file_unit, file=file, status="new", form="unformatted", &
          action="write", access="stream" )
    else
      if ( .not. append_ ) then
        open( newunit=file_unit, file=file, status="replace", form="unformatted", &
            action="write", access="stream" )
      else
        open( newunit=file_unit, file=file, status="old", form="unformatted", &
            action="write", access="stream", position="append" )
      end if
    end if

    write(unit=file_unit) substring%s//terminator_

    close(file_unit)
  end procedure echo_string

  module procedure to_text_1dc128
    type(String)                      :: text_file
    type(String), allocatable         :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nx, nrows, ncols, j
    logical :: header_present

    nx=size(x); nrows=0; ncols=0; j=0
    header_present = .false.
    if ( len(header) /= 0 ) header_present = .true.

    nrows = nx; ncols = 1
    if ( dim == COL_DIM ) then
      nrows = 1; ncols = nx
    end if
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      if ( dim == ROW_DIM ) then
        cells(1,1)%s = header(1)
      else if ( (dim == COL_DIM) .and. (size(header) == 1) ) then
        do j = 1, nx
          cells(1,j)%s = header(1)//str(j)
        end do
      else
        do j = 1, nx
          cells(1,j)%s = header(j)
        end do
      end if
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    call cast(x, numerical_data, locale, fmt, decimals, im)
    call text_file%write_file(cells, file, NL, delim, .false. stat, errmsg)
  end procedure to_text_1dc128
  module procedure to_text_1dc64
    type(String)                      :: text_file
    type(String), allocatable         :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nx, nrows, ncols, j
    logical :: header_present

    nx=size(x); nrows=0; ncols=0; j=0
    header_present = .false.
    if ( len(header) /= 0 ) header_present = .true.

    nrows = nx; ncols = 1
    if ( dim == COL_DIM ) then
      nrows = 1; ncols = nx
    end if
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      if ( dim == ROW_DIM ) then
        cells(1,1)%s = header(1)
      else if ( (dim == COL_DIM) .and. (size(header) == 1) ) then
        do j = 1, nx
          cells(1,j)%s = header(1)//str(j)
        end do
      else
        do j = 1, nx
          cells(1,j)%s = header(j)
        end do
      end if
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    call cast(x, numerical_data, locale, fmt, decimals, im)
    call text_file%write_file(cells, file, NL, delim, .false. stat, errmsg)
  end procedure to_text_1dc64
  module procedure to_text_1dc32
    type(String)                      :: text_file
    type(String), allocatable         :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nx, nrows, ncols, j
    logical :: header_present

    nx=size(x); nrows=0; ncols=0; j=0
    header_present = .false.
    if ( len(header) /= 0 ) header_present = .true.

    nrows = nx; ncols = 1
    if ( dim == COL_DIM ) then
      nrows = 1; ncols = nx
    end if
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      if ( dim == ROW_DIM ) then
        cells(1,1)%s = header(1)
      else if ( (dim == COL_DIM) .and. (size(header) == 1) ) then
        do j = 1, nx
          cells(1,j)%s = header(1)//str(j)
        end do
      else
        do j = 1, nx
          cells(1,j)%s = header(j)
        end do
      end if
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    call cast(x, numerical_data, locale, fmt, decimals, im)
    call text_file%write_file(cells, file, NL, delim, .false. stat, errmsg)
  end procedure to_text_1dc32

  module procedure to_text_2dc128
    type(String)                      :: text_file
    type(String), allocatable         :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nrows, ncols, j
    logical :: header_present

    nrows=size(x, dim=1); ncols=size(x, dim=2); j=0
    header_present = .false.
    if ( len(header) /= 0 ) header_present = .true.
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      if ( size(header) == 1 ) then
        do j = 1, ncols
          cells(1,j)%s = header(1)//str(j)
        end do
      else
        do j = 1, ncols
          cells(1,j)%s = header(j)
        end do
      end if
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    call cast(x, numerical_data, locale, fmt, decimals, im)
    call text_file%write_file(cells, file, NL, delim, .false. stat, errmsg)
  end procedure to_text_2dc128
  module procedure to_text_2dc64
    type(String)                      :: text_file
    type(String), allocatable         :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nrows, ncols, j
    logical :: header_present

    nrows=size(x, dim=1); ncols=size(x, dim=2); j=0
    header_present = .false.
    if ( len(header) /= 0 ) header_present = .true.
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      if ( size(header) == 1 ) then
        do j = 1, ncols
          cells(1,j)%s = header(1)//str(j)
        end do
      else
        do j = 1, ncols
          cells(1,j)%s = header(j)
        end do
      end if
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    call cast(x, numerical_data, locale, fmt, decimals, im)
    call text_file%write_file(cells, file, NL, delim, .false. stat, errmsg)
  end procedure to_text_2dc64
  module procedure to_text_2dc32
    type(String)                      :: text_file
    type(String), allocatable         :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nrows, ncols, j
    logical :: header_present

    nrows=size(x, dim=1); ncols=size(x, dim=2); j=0
    header_present = .false.
    if ( len(header) /= 0 ) header_present = .true.
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      if ( size(header) == 1 ) then
        do j = 1, ncols
          cells(1,j)%s = header(1)//str(j)
        end do
      else
        do j = 1, ncols
          cells(1,j)%s = header(j)
        end do
      end if
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    call cast(x, numerical_data, locale, fmt, decimals, im)
    call text_file%write_file(cells, file, NL, delim, .false. stat, errmsg)
  end procedure to_text_2dc32

  module procedure to_text_1dr128
    type(String)                      :: text_file
    type(String), allocatable         :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nx, nrows, ncols, j
    logical :: header_present

    nx=size(x); nrows=0; ncols=0; j=0
    header_present = .false.
    if ( len(header) /= 0 ) header_present = .true.

    nrows = nx; ncols = 1
    if ( dim == COL_DIM ) then
      nrows = 1; ncols = nx
    end if
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      if ( dim == ROW_DIM ) then
        cells(1,1)%s = header(1)
      else if ( (dim == COL_DIM) .and. (size(header) == 1) ) then
        do j = 1, nx
          cells(1,j)%s = header(1)//str(j)
        end do
      else
        do j = 1, nx
          cells(1,j)%s = header(j)
        end do
      end if
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    call cast(x, numerical_data, locale, fmt, decimals)
    call text_file%write_file(cells, file, NL, delim, .false. stat, errmsg)
  end procedure to_text_1dr128
  module procedure to_text_1dr64
    type(String)                      :: text_file
    type(String), allocatable         :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nx, nrows, ncols, j
    logical :: header_present

    nx=size(x); nrows=0; ncols=0; j=0
    header_present = .false.
    if ( len(header) /= 0 ) header_present = .true.

    nrows = nx; ncols = 1
    if ( dim == COL_DIM ) then
      nrows = 1; ncols = nx
    end if
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      if ( dim == ROW_DIM ) then
        cells(1,1)%s = header(1)
      else if ( (dim == COL_DIM) .and. (size(header) == 1) ) then
        do j = 1, nx
          cells(1,j)%s = header(1)//str(j)
        end do
      else
        do j = 1, nx
          cells(1,j)%s = header(j)
        end do
      end if
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    call cast(x, numerical_data, locale, fmt, decimals)
    call text_file%write_file(cells, file, NL, delim, .false. stat, errmsg)
  end procedure to_text_1dr64
  module procedure to_text_1dr32
    type(String)                      :: text_file
    type(String), allocatable         :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nx, nrows, ncols, j
    logical :: header_present

    nx=size(x); nrows=0; ncols=0; j=0
    header_present = .false.
    if ( len(header) /= 0 ) header_present = .true.

    nrows = nx; ncols = 1
    if ( dim == COL_DIM ) then
      nrows = 1; ncols = nx
    end if
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      if ( dim == ROW_DIM ) then
        cells(1,1)%s = header(1)
      else if ( (dim == COL_DIM) .and. (size(header) == 1) ) then
        do j = 1, nx
          cells(1,j)%s = header(1)//str(j)
        end do
      else
        do j = 1, nx
          cells(1,j)%s = header(j)
        end do
      end if
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    call cast(x, numerical_data, locale, fmt, decimals)
    call text_file%write_file(cells, file, NL, delim, .false. stat, errmsg)
  end procedure to_text_1dr32

  module procedure to_text_2dr128
    type(String)                      :: text_file
    type(String), allocatable         :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nrows, ncols, j
    logical :: header_present

    nrows=size(x, dim=1); ncols=size(x, dim=2); j=0
    header_present = .false.
    if ( len(header) /= 0 ) header_present = .true.
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      if ( size(header) == 1 ) then
        do j = 1, ncols
          cells(1,j)%s = header(1)//str(j)
        end do
      else
        do j = 1, ncols
          cells(1,j)%s = header(j)
        end do
      end if
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    call cast(x, numerical_data, locale, fmt, decimals)
    call text_file%write_file(cells, file, NL, delim, .false. stat, errmsg)
  end procedure to_text_2dr128
  module procedure to_text_2dr64
    type(String)                      :: text_file
    type(String), allocatable         :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nrows, ncols, j
    logical :: header_present

    nrows=size(x, dim=1); ncols=size(x, dim=2); j=0
    header_present = .false.
    if ( len(header) /= 0 ) header_present = .true.
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      if ( size(header) == 1 ) then
        do j = 1, ncols
          cells(1,j)%s = header(1)//str(j)
        end do
      else
        do j = 1, ncols
          cells(1,j)%s = header(j)
        end do
      end if
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    call cast(x, numerical_data, locale, fmt, decimals)
    call text_file%write_file(cells, file, NL, delim, .false. stat, errmsg)
  end procedure to_text_2dr64
  module procedure to_text_2dr32
    type(String)                      :: text_file
    type(String), allocatable         :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nrows, ncols, j
    logical :: header_present

    nrows=size(x, dim=1); ncols=size(x, dim=2); j=0
    header_present = .false.
    if ( len(header) /= 0 ) header_present = .true.
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      if ( size(header) == 1 ) then
        do j = 1, ncols
          cells(1,j)%s = header(1)//str(j)
        end do
      else
        do j = 1, ncols
          cells(1,j)%s = header(j)
        end do
      end if
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    call cast(x, numerical_data, locale, fmt, decimals)
    call text_file%write_file(cells, file, NL, delim, .false. stat, errmsg)
  end procedure to_text_2dr32

  module procedure to_text_1di64
    type(String)                      :: text_file
    type(String), allocatable         :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nx, nrows, ncols, j
    logical :: header_present

    nx=size(x); nrows=0; ncols=0; j=0
    header_present = .false.
    if ( len(header) /= 0 ) header_present = .true.

    nrows = nx; ncols = 1
    if ( dim == COL_DIM ) then
      nrows = 1; ncols = nx
    end if
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      if ( dim == ROW_DIM ) then
        cells(1,1)%s = header(1)
      else if ( (dim == COL_DIM) .and. (size(header) == 1) ) then
        do j = 1, nx
          cells(1,j)%s = header(1)//str(j)
        end do
      else
        do j = 1, nx
          cells(1,j)%s = header(j)
        end do
      end if
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    call cast(x, numerical_data, fmt)
    call text_file%write_file(cells, file, NL, delim, .false. stat, errmsg)
  end procedure to_text_1di64
  module procedure to_text_1di32
    type(String)                      :: text_file
    type(String), allocatable         :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nx, nrows, ncols, j
    logical :: header_present

    nx=size(x); nrows=0; ncols=0; j=0
    header_present = .false.
    if ( len(header) /= 0 ) header_present = .true.

    nrows = nx; ncols = 1
    if ( dim == COL_DIM ) then
      nrows = 1; ncols = nx
    end if
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      if ( dim == ROW_DIM ) then
        cells(1,1)%s = header(1)
      else if ( (dim == COL_DIM) .and. (size(header) == 1) ) then
        do j = 1, nx
          cells(1,j)%s = header(1)//str(j)
        end do
      else
        do j = 1, nx
          cells(1,j)%s = header(j)
        end do
      end if
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    call cast(x, numerical_data, fmt)
    call text_file%write_file(cells, file, NL, delim, .false. stat, errmsg)
  end procedure to_text_1di32
  module procedure to_text_1di16
    type(String)                      :: text_file
    type(String), allocatable         :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nx, nrows, ncols, j
    logical :: header_present

    nx=size(x); nrows=0; ncols=0; j=0
    header_present = .false.
    if ( len(header) /= 0 ) header_present = .true.

    nrows = nx; ncols = 1
    if ( dim == COL_DIM ) then
      nrows = 1; ncols = nx
    end if
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      if ( dim == ROW_DIM ) then
        cells(1,1)%s = header(1)
      else if ( (dim == COL_DIM) .and. (size(header) == 1) ) then
        do j = 1, nx
          cells(1,j)%s = header(1)//str(j)
        end do
      else
        do j = 1, nx
          cells(1,j)%s = header(j)
        end do
      end if
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    call cast(x, numerical_data, fmt)
    call text_file%write_file(cells, file, NL, delim, .false. stat, errmsg)
  end procedure to_text_1di16
  module procedure to_text_1di8
    type(String)                      :: text_file
    type(String), allocatable         :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nx, nrows, ncols, j
    logical :: header_present

    nx=size(x); nrows=0; ncols=0; j=0
    header_present = .false.
    if ( len(header) /= 0 ) header_present = .true.

    nrows = nx; ncols = 1
    if ( dim == COL_DIM ) then
      nrows = 1; ncols = nx
    end if
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      if ( dim == ROW_DIM ) then
        cells(1,1)%s = header(1)
      else if ( (dim == COL_DIM) .and. (size(header) == 1) ) then
        do j = 1, nx
          cells(1,j)%s = header(1)//str(j)
        end do
      else
        do j = 1, nx
          cells(1,j)%s = header(j)
        end do
      end if
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    call cast(x, numerical_data, fmt)
    call text_file%write_file(cells, file, NL, delim, .false. stat, errmsg)
  end procedure to_text_1di8

  module procedure to_text_2di64
    type(String)                      :: text_file
    type(String), allocatable         :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nrows, ncols, j
    logical :: header_present

    nrows=size(x, dim=1); ncols=size(x, dim=2); j=0
    header_present = .false.
    if ( len(header) /= 0 ) header_present = .true.
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      if ( size(header) == 1 ) then
        do j = 1, ncols
          cells(1,j)%s = header(1)//str(j)
        end do
      else
        do j = 1, ncols
          cells(1,j)%s = header(j)
        end do
      end if
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    call cast(x, numerical_data, fmt)
    call text_file%write_file(cells, file, NL, delim, .false. stat, errmsg)
  end procedure to_text_2di64
  module procedure to_text_2di32
    type(String)                      :: text_file
    type(String), allocatable         :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nrows, ncols, j
    logical :: header_present

    nrows=size(x, dim=1); ncols=size(x, dim=2); j=0
    header_present = .false.
    if ( len(header) /= 0 ) header_present = .true.
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      if ( size(header) == 1 ) then
        do j = 1, ncols
          cells(1,j)%s = header(1)//str(j)
        end do
      else
        do j = 1, ncols
          cells(1,j)%s = header(j)
        end do
      end if
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    call cast(x, numerical_data, fmt)
    call text_file%write_file(cells, file, NL, delim, .false. stat, errmsg)
  end procedure to_text_2di32
  module procedure to_text_2di16
    type(String)                      :: text_file
    type(String), allocatable         :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nrows, ncols, j
    logical :: header_present

    nrows=size(x, dim=1); ncols=size(x, dim=2); j=0
    header_present = .false.
    if ( len(header) /= 0 ) header_present = .true.
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      if ( size(header) == 1 ) then
        do j = 1, ncols
          cells(1,j)%s = header(1)//str(j)
        end do
      else
        do j = 1, ncols
          cells(1,j)%s = header(j)
        end do
      end if
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    call cast(x, numerical_data, fmt)
    call text_file%write_file(cells, file, NL, delim, .false. stat, errmsg)
  end procedure to_text_2di16
  module procedure to_text_2di8
    type(String)                      :: text_file
    type(String), allocatable         :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nrows, ncols, j
    logical :: header_present

    nrows=size(x, dim=1); ncols=size(x, dim=2); j=0
    header_present = .false.
    if ( len(header) /= 0 ) header_present = .true.
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      if ( size(header) == 1 ) then
        do j = 1, ncols
          cells(1,j)%s = header(1)//str(j)
        end do
      else
        do j = 1, ncols
          cells(1,j)%s = header(j)
        end do
      end if
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    call cast(x, numerical_data, fmt)
    call text_file%write_file(cells, file, NL, delim, .false. stat, errmsg)
  end procedure to_text_2di8

  ! Reading Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  module procedure from_text_1dc128
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer(i64)              :: n_rows, n_cols

    integer(i64) :: file_length, row, col, l, i
    integer      :: row_sep, col_sep, col_sep_len, open_paren, close_paren, current
    integer      :: file_unit, iostat
    logical      :: exists, in_paren

    n_rows      = 0_i64;   n_cols   = 0_i64
    file_length = 0_i64;   row      = 0_i64; col         = 0_i64; l          = 0_i64; i           = 0_i64
    row_sep     = 0;       col_sep  = 0;     col_sep_len = 0;     open_paren = 0;     close_paren = 0; current = 0
    file_unit   = 0;       iostat   = 0
    exists      = .false.; in_paren = .false.

    if ( len(im) == 0 ) then
      inquire(file=file, exist=exists)

      file_unit = input_unit

      if ( exists ) then
        open( newunit=file_unit, file=file, status="old", form="unformatted", &
            action="read", access="stream", position="rewind" )
      else
        error stop LF//'FATAL: Error reading file "'//file//'". No such file exists.'
        return
      end if

      inquire(file=file, size=file_length)

      if ( file_length == 0_i64 ) then
        error stop LF//'FATAL: Error reading file "'//file//'". File is empty.'
        return
      end if

      allocate( character(len=file_length) :: text_file%s )
      read(unit=file_unit, iostat=iostat) text_file%s
      close(file_unit)

      if ( iostat > 0 ) then
        error stop LF//'FATAL: Error reading file "'//file//'". iostat is '//str(iostat)
        return
      end if

      col_sep_len = len(delim)
      row_sep = iachar(NL); col_sep = iachar(delim(1:1))
      open_paren = iachar("("); close_paren = iachar(")"); in_paren = .false.

      n_rows = text_file%count(match=NL)

      n_cols = 1_i64; i = 1_i64; get_n_cols: do
        current = iachar(text_file%s(i:i))

        if ( (current/=open_paren) .and. (current/=close_paren) .and. (current/=col_sep) .and. &
           (current/=row_sep) ) then
          i = i + 1_i64; cycle
        end if

        if ( current == open_paren ) then
          in_paren = .true.; i = i + 1_i64; cycle
        end if

        if ( current == close_paren ) then
          in_paren = .false.; i = i + 1_i64; cycle
        end if

        if ( current == col_sep ) then
          if ( in_paren ) then
            i = i + 1_i64; cycle
          end if

          if ( col_sep_len == 1 ) then
            n_cols = n_cols + 1_i64; i = i + 1_i64; cycle
          else
            if ( text_file%s(i:i+col_sep_len-1_i64) == delim ) then
              n_cols = n_cols + 1_i64; i = i + col_sep_len; cycle
            else
              i = i + 1_i64; cycle
            end if
          end if
        end if

        if ( current == row_sep ) exit get_n_cols
      end do get_n_cols

      allocate( cells(n_rows,n_cols) )

      row = 1_i64; col = 1_i64; l = 1_i64; i = 1_i64; positional_transfers: do
        current = iachar(text_file%s(i:i))

        if ( (current/=open_paren) .and. (current/=close_paren) .and. (current/=col_sep) .and. &
           (current/=row_sep) ) then
          i = i + 1_i64; cycle
        end if

        if ( current == open_paren ) then
          in_paren = .true.; i = i + 1_i64; cycle
        end if

        if ( current == close_paren ) then
          in_paren = .false.; i = i + 1_i64; cycle
        end if

        if ( current == col_sep ) then
          if ( in_paren ) then
            i = i + 1_i64; cycle
          end if

          if ( col_sep_len == 1 ) then
            cells(row,col)%s = text_file%s(l:i-1); i = i + 1_i64; l = i
            col = col + 1_i64; cycle
          else
            if ( text_file%s(i:i+col_sep_len-1_i64) == delim ) then
              cells(row,col)%s = text_file%s(l:i-1); i = i + col_sep_len; l = i
              col = col + 1_i64; cycle
            else
              i = i + 1_i64; cycle
            end if
          end if
        end if

        if ( current == row_sep ) then
          cells(row,col)%s = text_file%s(l:i-1)
          if ( row == n_rows ) exit positional_transfers
          i = i + 1_i64; l = i; col = 1_i64; row = row + 1_i64; cycle
        end if
      end do positional_transfers
    else
      call text_file%read_file(file, cell_array=cells, row_separator=NL, column_separator=delim)

      n_rows = size(cells, dim=1, kind=i64)
      n_cols = size(cells, dim=2, kind=i64)
    end if

    if ( (n_rows > 1_i64) .and. (n_cols > 1_i64) ) then
      if ( header ) then
        if ( n_rows /= 2_i64 ) then
          error stop LF//'Error reading file "'//file//'". Data cannot fit into one-dimensional array.'
          return
        end if
      else
        error stop LF//'Error reading file "'//file//'". Data cannot fit into one-dimensional array.'// &
                 ' If there are two rows including a header row, specify "header=.true." .'
        return
      end if
    end if

    if ( n_cols == 1_i64 ) then
      if ( header ) then
        if ( .not. (n_rows > 1_i64) ) then
          error stop LF//'Error reading file "'//file//'". File is empty after header.'
          return
        end if

        allocate( into(n_rows-1_i64) )
        call cells(2_i64:,1_i64)%cast(into=into, locale=locale, fmt=fmt, im=im); return
      else
        allocate( into(n_rows) )
        call cells(:,1_i64)%cast(into=into, locale=locale, fmt=fmt, im=im); return
      end if
    end if

    if ( header ) then
      if ( .not. (n_rows > 1_i64) ) then
        error stop LF//'Error reading file "'//file//'". File is empty after header.'
        return
      end if

      allocate( into(n_cols) )
      call cells(2_i64,:)%cast(into=into, locale=locale, fmt=fmt, im=im); return
    else
      allocate( into(n_cols) )
      call cells(1_i64,:)%cast(into=into, locale=locale, fmt=fmt, im=im); return
    end if
  end procedure from_text_1dc128
  module procedure from_text_1dc64
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer(i64)              :: n_rows, n_cols

    integer(i64) :: file_length, row, col, l, i
    integer      :: row_sep, col_sep, col_sep_len, open_paren, close_paren, current
    integer      :: file_unit, iostat
    logical      :: exists, in_paren

    n_rows      = 0_i64;   n_cols   = 0_i64
    file_length = 0_i64;   row      = 0_i64; col         = 0_i64; l          = 0_i64; i           = 0_i64
    row_sep     = 0;       col_sep  = 0;     col_sep_len = 0;     open_paren = 0;     close_paren = 0; current = 0
    file_unit   = 0;       iostat   = 0
    exists      = .false.; in_paren = .false.

    if ( len(im) == 0 ) then
      inquire(file=file, exist=exists)

      file_unit = input_unit

      if ( exists ) then
        open( newunit=file_unit, file=file, status="old", form="unformatted", &
            action="read", access="stream", position="rewind" )
      else
        error stop LF//'FATAL: Error reading file "'//file//'". No such file exists.'
        return
      end if

      inquire(file=file, size=file_length)

      if ( file_length == 0_i64 ) then
        error stop LF//'FATAL: Error reading file "'//file//'". File is empty.'
        return
      end if

      allocate( character(len=file_length) :: text_file%s )
      read(unit=file_unit, iostat=iostat) text_file%s
      close(file_unit)

      if ( iostat > 0 ) then
        error stop LF//'FATAL: Error reading file "'//file//'". iostat is '//str(iostat)
        return
      end if

      col_sep_len = len(delim)
      row_sep = iachar(NL); col_sep = iachar(delim(1:1))
      open_paren = iachar("("); close_paren = iachar(")"); in_paren = .false.

      n_rows = text_file%count(match=NL)

      n_cols = 1_i64; i = 1_i64; get_n_cols: do
        current = iachar(text_file%s(i:i))

        if ( (current/=open_paren) .and. (current/=close_paren) .and. (current/=col_sep) .and. &
           (current/=row_sep) ) then
          i = i + 1_i64; cycle
        end if

        if ( current == open_paren ) then
          in_paren = .true.; i = i + 1_i64; cycle
        end if

        if ( current == close_paren ) then
          in_paren = .false.; i = i + 1_i64; cycle
        end if

        if ( current == col_sep ) then
          if ( in_paren ) then
            i = i + 1_i64; cycle
          end if

          if ( col_sep_len == 1 ) then
            n_cols = n_cols + 1_i64; i = i + 1_i64; cycle
          else
            if ( text_file%s(i:i+col_sep_len-1_i64) == delim ) then
              n_cols = n_cols + 1_i64; i = i + col_sep_len; cycle
            else
              i = i + 1_i64; cycle
            end if
          end if
        end if

        if ( current == row_sep ) exit get_n_cols
      end do get_n_cols

      allocate( cells(n_rows,n_cols) )

      row = 1_i64; col = 1_i64; l = 1_i64; i = 1_i64; positional_transfers: do
        current = iachar(text_file%s(i:i))

        if ( (current/=open_paren) .and. (current/=close_paren) .and. (current/=col_sep) .and. &
           (current/=row_sep) ) then
          i = i + 1_i64; cycle
        end if

        if ( current == open_paren ) then
          in_paren = .true.; i = i + 1_i64; cycle
        end if

        if ( current == close_paren ) then
          in_paren = .false.; i = i + 1_i64; cycle
        end if

        if ( current == col_sep ) then
          if ( in_paren ) then
            i = i + 1_i64; cycle
          end if

          if ( col_sep_len == 1 ) then
            cells(row,col)%s = text_file%s(l:i-1); i = i + 1_i64; l = i
            col = col + 1_i64; cycle
          else
            if ( text_file%s(i:i+col_sep_len-1_i64) == delim ) then
              cells(row,col)%s = text_file%s(l:i-1); i = i + col_sep_len; l = i
              col = col + 1_i64; cycle
            else
              i = i + 1_i64; cycle
            end if
          end if
        end if

        if ( current == row_sep ) then
          cells(row,col)%s = text_file%s(l:i-1)
          if ( row == n_rows ) exit positional_transfers
          i = i + 1_i64; l = i; col = 1_i64; row = row + 1_i64; cycle
        end if
      end do positional_transfers
    else
      call text_file%read_file(file, cell_array=cells, row_separator=NL, column_separator=delim)

      n_rows = size(cells, dim=1, kind=i64)
      n_cols = size(cells, dim=2, kind=i64)
    end if

    if ( (n_rows > 1_i64) .and. (n_cols > 1_i64) ) then
      if ( header ) then
        if ( n_rows /= 2_i64 ) then
          error stop LF//'Error reading file "'//file//'". Data cannot fit into one-dimensional array.'
          return
        end if
      else
        error stop LF//'Error reading file "'//file//'". Data cannot fit into one-dimensional array.'// &
                 ' If there are two rows including a header row, specify "header=.true." .'
        return
      end if
    end if

    if ( n_cols == 1_i64 ) then
      if ( header ) then
        if ( .not. (n_rows > 1_i64) ) then
          error stop LF//'Error reading file "'//file//'". File is empty after header.'
          return
        end if

        allocate( into(n_rows-1_i64) )
        call cells(2_i64:,1_i64)%cast(into=into, locale=locale, fmt=fmt, im=im); return
      else
        allocate( into(n_rows) )
        call cells(:,1_i64)%cast(into=into, locale=locale, fmt=fmt, im=im); return
      end if
    end if

    if ( header ) then
      if ( .not. (n_rows > 1_i64) ) then
        error stop LF//'Error reading file "'//file//'". File is empty after header.'
        return
      end if

      allocate( into(n_cols) )
      call cells(2_i64,:)%cast(into=into, locale=locale, fmt=fmt, im=im); return
    else
      allocate( into(n_cols) )
      call cells(1_i64,:)%cast(into=into, locale=locale, fmt=fmt, im=im); return
    end if
  end procedure from_text_1dc64
  module procedure from_text_1dc32
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer(i64)              :: n_rows, n_cols

    integer(i64) :: file_length, row, col, l, i
    integer      :: row_sep, col_sep, col_sep_len, open_paren, close_paren, current
    integer      :: file_unit, iostat
    logical      :: exists, in_paren

    n_rows      = 0_i64;   n_cols   = 0_i64
    file_length = 0_i64;   row      = 0_i64; col         = 0_i64; l          = 0_i64; i           = 0_i64
    row_sep     = 0;       col_sep  = 0;     col_sep_len = 0;     open_paren = 0;     close_paren = 0; current = 0
    file_unit   = 0;       iostat   = 0
    exists      = .false.; in_paren = .false.

    if ( len(im) == 0 ) then
      inquire(file=file, exist=exists)

      file_unit = input_unit

      if ( exists ) then
        open( newunit=file_unit, file=file, status="old", form="unformatted", &
            action="read", access="stream", position="rewind" )
      else
        error stop LF//'FATAL: Error reading file "'//file//'". No such file exists.'
        return
      end if

      inquire(file=file, size=file_length)

      if ( file_length == 0_i64 ) then
        error stop LF//'FATAL: Error reading file "'//file//'". File is empty.'
        return
      end if

      allocate( character(len=file_length) :: text_file%s )
      read(unit=file_unit, iostat=iostat) text_file%s
      close(file_unit)

      if ( iostat > 0 ) then
        error stop LF//'FATAL: Error reading file "'//file//'". iostat is '//str(iostat)
        return
      end if

      col_sep_len = len(delim)
      row_sep = iachar(NL); col_sep = iachar(delim(1:1))
      open_paren = iachar("("); close_paren = iachar(")"); in_paren = .false.

      n_rows = text_file%count(match=NL)

      n_cols = 1_i64; i = 1_i64; get_n_cols: do
        current = iachar(text_file%s(i:i))

        if ( (current/=open_paren) .and. (current/=close_paren) .and. (current/=col_sep) .and. &
           (current/=row_sep) ) then
          i = i + 1_i64; cycle
        end if

        if ( current == open_paren ) then
          in_paren = .true.; i = i + 1_i64; cycle
        end if

        if ( current == close_paren ) then
          in_paren = .false.; i = i + 1_i64; cycle
        end if

        if ( current == col_sep ) then
          if ( in_paren ) then
            i = i + 1_i64; cycle
          end if

          if ( col_sep_len == 1 ) then
            n_cols = n_cols + 1_i64; i = i + 1_i64; cycle
          else
            if ( text_file%s(i:i+col_sep_len-1_i64) == delim ) then
              n_cols = n_cols + 1_i64; i = i + col_sep_len; cycle
            else
              i = i + 1_i64; cycle
            end if
          end if
        end if

        if ( current == row_sep ) exit get_n_cols
      end do get_n_cols

      allocate( cells(n_rows,n_cols) )

      row = 1_i64; col = 1_i64; l = 1_i64; i = 1_i64; positional_transfers: do
        current = iachar(text_file%s(i:i))

        if ( (current/=open_paren) .and. (current/=close_paren) .and. (current/=col_sep) .and. &
           (current/=row_sep) ) then
          i = i + 1_i64; cycle
        end if

        if ( current == open_paren ) then
          in_paren = .true.; i = i + 1_i64; cycle
        end if

        if ( current == close_paren ) then
          in_paren = .false.; i = i + 1_i64; cycle
        end if

        if ( current == col_sep ) then
          if ( in_paren ) then
            i = i + 1_i64; cycle
          end if

          if ( col_sep_len == 1 ) then
            cells(row,col)%s = text_file%s(l:i-1); i = i + 1_i64; l = i
            col = col + 1_i64; cycle
          else
            if ( text_file%s(i:i+col_sep_len-1_i64) == delim ) then
              cells(row,col)%s = text_file%s(l:i-1); i = i + col_sep_len; l = i
              col = col + 1_i64; cycle
            else
              i = i + 1_i64; cycle
            end if
          end if
        end if

        if ( current == row_sep ) then
          cells(row,col)%s = text_file%s(l:i-1)
          if ( row == n_rows ) exit positional_transfers
          i = i + 1_i64; l = i; col = 1_i64; row = row + 1_i64; cycle
        end if
      end do positional_transfers
    else
      call text_file%read_file(file, cell_array=cells, row_separator=NL, column_separator=delim)

      n_rows = size(cells, dim=1, kind=i64)
      n_cols = size(cells, dim=2, kind=i64)
    end if

    if ( (n_rows > 1_i64) .and. (n_cols > 1_i64) ) then
      if ( header ) then
        if ( n_rows /= 2_i64 ) then
          error stop LF//'Error reading file "'//file//'". Data cannot fit into one-dimensional array.'
          return
        end if
      else
        error stop LF//'Error reading file "'//file//'". Data cannot fit into one-dimensional array.'// &
                 ' If there are two rows including a header row, specify "header=.true." .'
        return
      end if
    end if

    if ( n_cols == 1_i64 ) then
      if ( header ) then
        if ( .not. (n_rows > 1_i64) ) then
          error stop LF//'Error reading file "'//file//'". File is empty after header.'
          return
        end if

        allocate( into(n_rows-1_i64) )
        call cells(2_i64:,1_i64)%cast(into=into, locale=locale, fmt=fmt, im=im); return
      else
        allocate( into(n_rows) )
        call cells(:,1_i64)%cast(into=into, locale=locale, fmt=fmt, im=im); return
      end if
    end if

    if ( header ) then
      if ( .not. (n_rows > 1_i64) ) then
        error stop LF//'Error reading file "'//file//'". File is empty after header.'
        return
      end if

      allocate( into(n_cols) )
      call cells(2_i64,:)%cast(into=into, locale=locale, fmt=fmt, im=im); return
    else
      allocate( into(n_cols) )
      call cells(1_i64,:)%cast(into=into, locale=locale, fmt=fmt, im=im); return
    end if
  end procedure from_text_1dc32

  module procedure from_text_2dc128
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer(i64)              :: n_rows, n_cols

    integer(i64) :: file_length, row, col, l, i
    integer      :: row_sep, col_sep, col_sep_len, open_paren, close_paren, current
    integer      :: file_unit, iostat
    logical      :: exists, in_paren

    n_rows      = 0_i64;   n_cols   = 0_i64
    file_length = 0_i64;   row      = 0_i64; col         = 0_i64; l          = 0_i64; i           = 0_i64
    row_sep     = 0;       col_sep  = 0;     col_sep_len = 0;     open_paren = 0;     close_paren = 0; current = 0
    file_unit   = 0;       iostat   = 0
    exists      = .false.; in_paren = .false.

    if ( len(im) == 0 ) then
      inquire(file=file, exist=exists)

      file_unit = input_unit

      if ( exists ) then
        open( newunit=file_unit, file=file, status="old", form="unformatted", &
            action="read", access="stream", position="rewind" )
      else
        error stop LF//'FATAL: Error reading file "'//file//'". No such file exists.'
        return
      end if

      inquire(file=file, size=file_length)

      if ( file_length == 0_i64 ) then
        error stop LF//'FATAL: Error reading file "'//file//'". File is empty.'
        return
      end if

      allocate( character(len=file_length) :: text_file%s )
      read(unit=file_unit, iostat=iostat) text_file%s
      close(file_unit)

      if ( iostat > 0 ) then
        error stop LF//'FATAL: Error reading file "'//file//'". iostat is '//str(iostat)
        return
      end if

      col_sep_len = len(delim)
      row_sep = iachar(NL); col_sep = iachar(delim(1:1))
      open_paren = iachar("("); close_paren = iachar(")"); in_paren = .false.

      n_rows = text_file%count(match=NL)

      n_cols = 1_i64; i = 1_i64; get_n_cols: do
        current = iachar(text_file%s(i:i))

        if ( (current/=open_paren) .and. (current/=close_paren) .and. (current/=col_sep) .and. &
           (current/=row_sep) ) then
          i = i + 1_i64; cycle
        end if

        if ( current == open_paren ) then
          in_paren = .true.; i = i + 1_i64; cycle
        end if

        if ( current == close_paren ) then
          in_paren = .false.; i = i + 1_i64; cycle
        end if

        if ( current == col_sep ) then
          if ( in_paren ) then
            i = i + 1_i64; cycle
          end if

          if ( col_sep_len == 1 ) then
            n_cols = n_cols + 1_i64; i = i + 1_i64; cycle
          else
            if ( text_file%s(i:i+col_sep_len-1_i64) == delim ) then
              n_cols = n_cols + 1_i64; i = i + col_sep_len; cycle
            else
              i = i + 1_i64; cycle
            end if
          end if
        end if

        if ( current == row_sep ) exit get_n_cols
      end do get_n_cols

      allocate( cells(n_rows,n_cols) )

      row = 1_i64; col = 1_i64; l = 1_i64; i = 1_i64; positional_transfers: do
        current = iachar(text_file%s(i:i))

        if ( (current/=open_paren) .and. (current/=close_paren) .and. (current/=col_sep) .and. &
           (current/=row_sep) ) then
          i = i + 1_i64; cycle
        end if

        if ( current == open_paren ) then
          in_paren = .true.; i = i + 1_i64; cycle
        end if

        if ( current == close_paren ) then
          in_paren = .false.; i = i + 1_i64; cycle
        end if

        if ( current == col_sep ) then
          if ( in_paren ) then
            i = i + 1_i64; cycle
          end if

          if ( col_sep_len == 1 ) then
            cells(row,col)%s = text_file%s(l:i-1); i = i + 1_i64; l = i
            col = col + 1_i64; cycle
          else
            if ( text_file%s(i:i+col_sep_len-1_i64) == delim ) then
              cells(row,col)%s = text_file%s(l:i-1); i = i + col_sep_len; l = i
              col = col + 1_i64; cycle
            else
              i = i + 1_i64; cycle
            end if
          end if
        end if

        if ( current == row_sep ) then
          cells(row,col)%s = text_file%s(l:i-1)
          if ( row == n_rows ) exit positional_transfers
          i = i + 1_i64; l = i; col = 1_i64; row = row + 1_i64; cycle
        end if
      end do positional_transfers
    else
      call text_file%read_file(file, cell_array=cells, row_separator=NL, column_separator=delim)

      n_rows = size(cells, dim=1, kind=i64)
      n_cols = size(cells, dim=2, kind=i64)
    end if

    if ( header ) then
      if ( .not. (n_rows > 1_i64) ) then
        error stop LF//'Error reading file "'//file//'". File is empty after header.'
        return
      end if

      allocate( into(n_rows-1_i64,n_cols) )
      call cells(2_i64:,:)%cast(into=into, locale=locale, fmt=fmt, im=im); return
    else
      allocate( into(n_rows,n_cols) )
      call cells%cast(into=into, locale=locale, fmt=fmt, im=im); return
    end if
  end procedure from_text_2dc128
  module procedure from_text_2dc64
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer(i64)              :: n_rows, n_cols

    integer(i64) :: file_length, row, col, l, i
    integer      :: row_sep, col_sep, col_sep_len, open_paren, close_paren, current
    integer      :: file_unit, iostat
    logical      :: exists, in_paren

    n_rows      = 0_i64;   n_cols   = 0_i64
    file_length = 0_i64;   row      = 0_i64; col         = 0_i64; l          = 0_i64; i           = 0_i64
    row_sep     = 0;       col_sep  = 0;     col_sep_len = 0;     open_paren = 0;     close_paren = 0; current = 0
    file_unit   = 0;       iostat   = 0
    exists      = .false.; in_paren = .false.

    if ( len(im) == 0 ) then
      inquire(file=file, exist=exists)

      file_unit = input_unit

      if ( exists ) then
        open( newunit=file_unit, file=file, status="old", form="unformatted", &
            action="read", access="stream", position="rewind" )
      else
        error stop LF//'FATAL: Error reading file "'//file//'". No such file exists.'
        return
      end if

      inquire(file=file, size=file_length)

      if ( file_length == 0_i64 ) then
        error stop LF//'FATAL: Error reading file "'//file//'". File is empty.'
        return
      end if

      allocate( character(len=file_length) :: text_file%s )
      read(unit=file_unit, iostat=iostat) text_file%s
      close(file_unit)

      if ( iostat > 0 ) then
        error stop LF//'FATAL: Error reading file "'//file//'". iostat is '//str(iostat)
        return
      end if

      col_sep_len = len(delim)
      row_sep = iachar(NL); col_sep = iachar(delim(1:1))
      open_paren = iachar("("); close_paren = iachar(")"); in_paren = .false.

      n_rows = text_file%count(match=NL)

      n_cols = 1_i64; i = 1_i64; get_n_cols: do
        current = iachar(text_file%s(i:i))

        if ( (current/=open_paren) .and. (current/=close_paren) .and. (current/=col_sep) .and. &
           (current/=row_sep) ) then
          i = i + 1_i64; cycle
        end if

        if ( current == open_paren ) then
          in_paren = .true.; i = i + 1_i64; cycle
        end if

        if ( current == close_paren ) then
          in_paren = .false.; i = i + 1_i64; cycle
        end if

        if ( current == col_sep ) then
          if ( in_paren ) then
            i = i + 1_i64; cycle
          end if

          if ( col_sep_len == 1 ) then
            n_cols = n_cols + 1_i64; i = i + 1_i64; cycle
          else
            if ( text_file%s(i:i+col_sep_len-1_i64) == delim ) then
              n_cols = n_cols + 1_i64; i = i + col_sep_len; cycle
            else
              i = i + 1_i64; cycle
            end if
          end if
        end if

        if ( current == row_sep ) exit get_n_cols
      end do get_n_cols

      allocate( cells(n_rows,n_cols) )

      row = 1_i64; col = 1_i64; l = 1_i64; i = 1_i64; positional_transfers: do
        current = iachar(text_file%s(i:i))

        if ( (current/=open_paren) .and. (current/=close_paren) .and. (current/=col_sep) .and. &
           (current/=row_sep) ) then
          i = i + 1_i64; cycle
        end if

        if ( current == open_paren ) then
          in_paren = .true.; i = i + 1_i64; cycle
        end if

        if ( current == close_paren ) then
          in_paren = .false.; i = i + 1_i64; cycle
        end if

        if ( current == col_sep ) then
          if ( in_paren ) then
            i = i + 1_i64; cycle
          end if

          if ( col_sep_len == 1 ) then
            cells(row,col)%s = text_file%s(l:i-1); i = i + 1_i64; l = i
            col = col + 1_i64; cycle
          else
            if ( text_file%s(i:i+col_sep_len-1_i64) == delim ) then
              cells(row,col)%s = text_file%s(l:i-1); i = i + col_sep_len; l = i
              col = col + 1_i64; cycle
            else
              i = i + 1_i64; cycle
            end if
          end if
        end if

        if ( current == row_sep ) then
          cells(row,col)%s = text_file%s(l:i-1)
          if ( row == n_rows ) exit positional_transfers
          i = i + 1_i64; l = i; col = 1_i64; row = row + 1_i64; cycle
        end if
      end do positional_transfers
    else
      call text_file%read_file(file, cell_array=cells, row_separator=NL, column_separator=delim)

      n_rows = size(cells, dim=1, kind=i64)
      n_cols = size(cells, dim=2, kind=i64)
    end if

    if ( header ) then
      if ( .not. (n_rows > 1_i64) ) then
        error stop LF//'Error reading file "'//file//'". File is empty after header.'
        return
      end if

      allocate( into(n_rows-1_i64,n_cols) )
      call cells(2_i64:,:)%cast(into=into, locale=locale, fmt=fmt, im=im); return
    else
      allocate( into(n_rows,n_cols) )
      call cells%cast(into=into, locale=locale, fmt=fmt, im=im); return
    end if
  end procedure from_text_2dc64
  module procedure from_text_2dc32
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer(i64)              :: n_rows, n_cols

    integer(i64) :: file_length, row, col, l, i
    integer      :: row_sep, col_sep, col_sep_len, open_paren, close_paren, current
    integer      :: file_unit, iostat
    logical      :: exists, in_paren

    n_rows      = 0_i64;   n_cols   = 0_i64
    file_length = 0_i64;   row      = 0_i64; col         = 0_i64; l          = 0_i64; i           = 0_i64
    row_sep     = 0;       col_sep  = 0;     col_sep_len = 0;     open_paren = 0;     close_paren = 0; current = 0
    file_unit   = 0;       iostat   = 0
    exists      = .false.; in_paren = .false.

    if ( len(im) == 0 ) then
      inquire(file=file, exist=exists)

      file_unit = input_unit

      if ( exists ) then
        open( newunit=file_unit, file=file, status="old", form="unformatted", &
            action="read", access="stream", position="rewind" )
      else
        error stop LF//'FATAL: Error reading file "'//file//'". No such file exists.'
        return
      end if

      inquire(file=file, size=file_length)

      if ( file_length == 0_i64 ) then
        error stop LF//'FATAL: Error reading file "'//file//'". File is empty.'
        return
      end if

      allocate( character(len=file_length) :: text_file%s )
      read(unit=file_unit, iostat=iostat) text_file%s
      close(file_unit)

      if ( iostat > 0 ) then
        error stop LF//'FATAL: Error reading file "'//file//'". iostat is '//str(iostat)
        return
      end if

      col_sep_len = len(delim)
      row_sep = iachar(NL); col_sep = iachar(delim(1:1))
      open_paren = iachar("("); close_paren = iachar(")"); in_paren = .false.

      n_rows = text_file%count(match=NL)

      n_cols = 1_i64; i = 1_i64; get_n_cols: do
        current = iachar(text_file%s(i:i))

        if ( (current/=open_paren) .and. (current/=close_paren) .and. (current/=col_sep) .and. &
           (current/=row_sep) ) then
          i = i + 1_i64; cycle
        end if

        if ( current == open_paren ) then
          in_paren = .true.; i = i + 1_i64; cycle
        end if

        if ( current == close_paren ) then
          in_paren = .false.; i = i + 1_i64; cycle
        end if

        if ( current == col_sep ) then
          if ( in_paren ) then
            i = i + 1_i64; cycle
          end if

          if ( col_sep_len == 1 ) then
            n_cols = n_cols + 1_i64; i = i + 1_i64; cycle
          else
            if ( text_file%s(i:i+col_sep_len-1_i64) == delim ) then
              n_cols = n_cols + 1_i64; i = i + col_sep_len; cycle
            else
              i = i + 1_i64; cycle
            end if
          end if
        end if

        if ( current == row_sep ) exit get_n_cols
      end do get_n_cols

      allocate( cells(n_rows,n_cols) )

      row = 1_i64; col = 1_i64; l = 1_i64; i = 1_i64; positional_transfers: do
        current = iachar(text_file%s(i:i))

        if ( (current/=open_paren) .and. (current/=close_paren) .and. (current/=col_sep) .and. &
           (current/=row_sep) ) then
          i = i + 1_i64; cycle
        end if

        if ( current == open_paren ) then
          in_paren = .true.; i = i + 1_i64; cycle
        end if

        if ( current == close_paren ) then
          in_paren = .false.; i = i + 1_i64; cycle
        end if

        if ( current == col_sep ) then
          if ( in_paren ) then
            i = i + 1_i64; cycle
          end if

          if ( col_sep_len == 1 ) then
            cells(row,col)%s = text_file%s(l:i-1); i = i + 1_i64; l = i
            col = col + 1_i64; cycle
          else
            if ( text_file%s(i:i+col_sep_len-1_i64) == delim ) then
              cells(row,col)%s = text_file%s(l:i-1); i = i + col_sep_len; l = i
              col = col + 1_i64; cycle
            else
              i = i + 1_i64; cycle
            end if
          end if
        end if

        if ( current == row_sep ) then
          cells(row,col)%s = text_file%s(l:i-1)
          if ( row == n_rows ) exit positional_transfers
          i = i + 1_i64; l = i; col = 1_i64; row = row + 1_i64; cycle
        end if
      end do positional_transfers
    else
      call text_file%read_file(file, cell_array=cells, row_separator=NL, column_separator=delim)

      n_rows = size(cells, dim=1, kind=i64)
      n_cols = size(cells, dim=2, kind=i64)
    end if

    if ( header ) then
      if ( .not. (n_rows > 1_i64) ) then
        error stop LF//'Error reading file "'//file//'". File is empty after header.'
        return
      end if

      allocate( into(n_rows-1_i64,n_cols) )
      call cells(2_i64:,:)%cast(into=into, locale=locale, fmt=fmt, im=im); return
    else
      allocate( into(n_rows,n_cols) )
      call cells%cast(into=into, locale=locale, fmt=fmt, im=im); return
    end if
  end procedure from_text_2dc32

  module procedure from_text_1dr128
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer(i64)              :: n_rows, n_cols

    n_rows=0_i64; n_cols=0_i64

    call text_file%read_file(file, cell_array=cells, row_separator=NL, column_separator=delim)
    call text_file%empty()

    n_rows = size(cells, dim=1, kind=i64)
    n_cols = size(cells, dim=2, kind=i64)

    if ( (n_rows > 1_i64) .and. (n_cols > 1_i64) ) then
      if ( header ) then
        if ( n_rows /= 2_i64 ) then
          error stop LF//'Error reading file "'//file//'". Data cannot fit into one-dimensional array.'
          return
        end if
      else
        error stop LF//'Error reading file "'//file//'". Data cannot fit into one-dimensional array.'// &
                 ' If there are two rows including a header row, specify "header=.true." .'
        return
      end if
    end if

    if ( n_cols == 1_i64 ) then
      if ( header ) then
        if ( .not. (n_rows > 1_i64) ) then
          error stop LF//'Error reading file "'//file//'". File is empty after header.'
          return
        end if

        allocate( into(n_rows-1_i64) )
        call cells(2_i64:,1_i64)%cast(into=into, locale=locale, fmt=fmt); return
      else
        allocate( into(n_rows) )
        call cells(:,1_i64)%cast(into=into, locale=locale, fmt=fmt); return
      end if
    end if

    if ( header ) then
      if ( .not. (n_rows > 1_i64) ) then
        error stop LF//'Error reading file "'//file//'". File is empty after header.'
        return
      end if

      allocate( into(n_cols) )
      call cells(2_i64,:)%cast(into=into, locale=locale, fmt=fmt); return
    else
      allocate( into(n_cols) )
      call cells(1_i64,:)%cast(into=into, locale=locale, fmt=fmt); return
    end if
  end procedure from_text_1dr128
  module procedure from_text_1dr64
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer(i64)              :: n_rows, n_cols

    n_rows=0_i64; n_cols=0_i64

    call text_file%read_file(file, cell_array=cells, row_separator=NL, column_separator=delim)
    call text_file%empty()

    n_rows = size(cells, dim=1, kind=i64)
    n_cols = size(cells, dim=2, kind=i64)

    if ( (n_rows > 1_i64) .and. (n_cols > 1_i64) ) then
      if ( header ) then
        if ( n_rows /= 2_i64 ) then
          error stop LF//'Error reading file "'//file//'". Data cannot fit into one-dimensional array.'
          return
        end if
      else
        error stop LF//'Error reading file "'//file//'". Data cannot fit into one-dimensional array.'// &
                 ' If there are two rows including a header row, specify "header=.true." .'
        return
      end if
    end if

    if ( n_cols == 1_i64 ) then
      if ( header ) then
        if ( .not. (n_rows > 1_i64) ) then
          error stop LF//'Error reading file "'//file//'". File is empty after header.'
          return
        end if

        allocate( into(n_rows-1_i64) )
        call cells(2_i64:,1_i64)%cast(into=into, locale=locale, fmt=fmt); return
      else
        allocate( into(n_rows) )
        call cells(:,1_i64)%cast(into=into, locale=locale, fmt=fmt); return
      end if
    end if

    if ( header ) then
      if ( .not. (n_rows > 1_i64) ) then
        error stop LF//'Error reading file "'//file//'". File is empty after header.'
        return
      end if

      allocate( into(n_cols) )
      call cells(2_i64,:)%cast(into=into, locale=locale, fmt=fmt); return
    else
      allocate( into(n_cols) )
      call cells(1_i64,:)%cast(into=into, locale=locale, fmt=fmt); return
    end if
  end procedure from_text_1dr64
  module procedure from_text_1dr32
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer(i64)              :: n_rows, n_cols

    n_rows=0_i64; n_cols=0_i64

    call text_file%read_file(file, cell_array=cells, row_separator=NL, column_separator=delim)
    call text_file%empty()

    n_rows = size(cells, dim=1, kind=i64)
    n_cols = size(cells, dim=2, kind=i64)

    if ( (n_rows > 1_i64) .and. (n_cols > 1_i64) ) then
      if ( header ) then
        if ( n_rows /= 2_i64 ) then
          error stop LF//'Error reading file "'//file//'". Data cannot fit into one-dimensional array.'
          return
        end if
      else
        error stop LF//'Error reading file "'//file//'". Data cannot fit into one-dimensional array.'// &
                 ' If there are two rows including a header row, specify "header=.true." .'
        return
      end if
    end if

    if ( n_cols == 1_i64 ) then
      if ( header ) then
        if ( .not. (n_rows > 1_i64) ) then
          error stop LF//'Error reading file "'//file//'". File is empty after header.'
          return
        end if

        allocate( into(n_rows-1_i64) )
        call cells(2_i64:,1_i64)%cast(into=into, locale=locale, fmt=fmt); return
      else
        allocate( into(n_rows) )
        call cells(:,1_i64)%cast(into=into, locale=locale, fmt=fmt); return
      end if
    end if

    if ( header ) then
      if ( .not. (n_rows > 1_i64) ) then
        error stop LF//'Error reading file "'//file//'". File is empty after header.'
        return
      end if

      allocate( into(n_cols) )
      call cells(2_i64,:)%cast(into=into, locale=locale, fmt=fmt); return
    else
      allocate( into(n_cols) )
      call cells(1_i64,:)%cast(into=into, locale=locale, fmt=fmt); return
    end if
  end procedure from_text_1dr32

  module procedure from_text_2dr128
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer(i64)              :: n_rows, n_cols

    n_rows=0_i64; n_cols=0_i64

    call text_file%read_file(file, cell_array=cells, row_separator=NL, column_separator=delim)
    call text_file%empty()

    n_rows = size(cells, dim=1, kind=i64)
    n_cols = size(cells, dim=2, kind=i64)

    if ( header ) then
      if ( .not. (n_rows > 1_i64) ) then
        error stop LF//'Error reading file "'//file//'". File is empty after header.'
        return
      end if

      allocate( into(n_rows-1_i64,n_cols) )
      call cells(2_i64:,:)%cast(into=into, locale=locale, fmt=fmt); return
    else
      allocate( into(n_rows,n_cols) )
      call cells%cast(into=into, locale=locale, fmt=fmt); return
    end if
  end procedure from_text_2dr128
  module procedure from_text_2dr64
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer(i64)              :: n_rows, n_cols

    n_rows=0_i64; n_cols=0_i64

    call text_file%read_file(file, cell_array=cells, row_separator=NL, column_separator=delim)
    call text_file%empty()

    n_rows = size(cells, dim=1, kind=i64)
    n_cols = size(cells, dim=2, kind=i64)

    if ( header ) then
      if ( .not. (n_rows > 1_i64) ) then
        error stop LF//'Error reading file "'//file//'". File is empty after header.'
        return
      end if

      allocate( into(n_rows-1_i64,n_cols) )
      call cells(2_i64:,:)%cast(into=into, locale=locale, fmt=fmt); return
    else
      allocate( into(n_rows,n_cols) )
      call cells%cast(into=into, locale=locale, fmt=fmt); return
    end if
  end procedure from_text_2dr64
  module procedure from_text_2dr32
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer(i64)              :: n_rows, n_cols

    n_rows=0_i64; n_cols=0_i64

    call text_file%read_file(file, cell_array=cells, row_separator=NL, column_separator=delim)
    call text_file%empty()

    n_rows = size(cells, dim=1, kind=i64)
    n_cols = size(cells, dim=2, kind=i64)

    if ( header ) then
      if ( .not. (n_rows > 1_i64) ) then
        error stop LF//'Error reading file "'//file//'". File is empty after header.'
        return
      end if

      allocate( into(n_rows-1_i64,n_cols) )
      call cells(2_i64:,:)%cast(into=into, locale=locale, fmt=fmt); return
    else
      allocate( into(n_rows,n_cols) )
      call cells%cast(into=into, locale=locale, fmt=fmt); return
    end if
  end procedure from_text_2dr32

  module procedure from_text_1di64
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer(i64)              :: n_rows, n_cols

    n_rows=0_i64; n_cols=0_i64

    call text_file%read_file(file, cell_array=cells, row_separator=NL, column_separator=delim)
    call text_file%empty()

    n_rows = size(cells, dim=1, kind=i64)
    n_cols = size(cells, dim=2, kind=i64)

    if ( (n_rows > 1_i64) .and. (n_cols > 1_i64) ) then
      if ( header ) then
        if ( n_rows /= 2_i64 ) then
          error stop LF//'Error reading file "'//file//'". Data cannot fit into one-dimensional array.'
          return
        end if
      else
        error stop LF//'Error reading file "'//file//'". Data cannot fit into one-dimensional array.'// &
                 ' If there are two rows including a header row, specify "header=.true." .'
        return
      end if
    end if

    if ( n_cols == 1_i64 ) then
      if ( header ) then
        if ( .not. (n_rows > 1_i64) ) then
          error stop LF//'Error reading file "'//file//'". File is empty after header.'
          return
        end if

        allocate( into(n_rows-1_i64) )
        call cells(2_i64:,1_i64)%cast(into=into, fmt=fmt); return
      else
        allocate( into(n_rows) )
        call cells(:,1_i64)%cast(into=into, fmt=fmt); return
      end if
    end if

    if ( header ) then
      if ( .not. (n_rows > 1_i64) ) then
        error stop LF//'Error reading file "'//file//'". File is empty after header.'
        return
      end if

      allocate( into(n_cols) )
      call cells(2_i64,:)%cast(into=into, fmt=fmt); return
    else
      allocate( into(n_cols) )
      call cells(1_i64,:)%cast(into=into, fmt=fmt); return
    end if
  end procedure from_text_1di64
  module procedure from_text_1di32
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer(i64)              :: n_rows, n_cols

    n_rows=0_i64; n_cols=0_i64

    call text_file%read_file(file, cell_array=cells, row_separator=NL, column_separator=delim)
    call text_file%empty()

    n_rows = size(cells, dim=1, kind=i64)
    n_cols = size(cells, dim=2, kind=i64)

    if ( (n_rows > 1_i64) .and. (n_cols > 1_i64) ) then
      if ( header ) then
        if ( n_rows /= 2_i64 ) then
          error stop LF//'Error reading file "'//file//'". Data cannot fit into one-dimensional array.'
          return
        end if
      else
        error stop LF//'Error reading file "'//file//'". Data cannot fit into one-dimensional array.'// &
                 ' If there are two rows including a header row, specify "header=.true." .'
        return
      end if
    end if

    if ( n_cols == 1_i64 ) then
      if ( header ) then
        if ( .not. (n_rows > 1_i64) ) then
          error stop LF//'Error reading file "'//file//'". File is empty after header.'
          return
        end if

        allocate( into(n_rows-1_i64) )
        call cells(2_i64:,1_i64)%cast(into=into, fmt=fmt); return
      else
        allocate( into(n_rows) )
        call cells(:,1_i64)%cast(into=into, fmt=fmt); return
      end if
    end if

    if ( header ) then
      if ( .not. (n_rows > 1_i64) ) then
        error stop LF//'Error reading file "'//file//'". File is empty after header.'
        return
      end if

      allocate( into(n_cols) )
      call cells(2_i64,:)%cast(into=into, fmt=fmt); return
    else
      allocate( into(n_cols) )
      call cells(1_i64,:)%cast(into=into, fmt=fmt); return
    end if
  end procedure from_text_1di32
  module procedure from_text_1di16
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer(i64)              :: n_rows, n_cols

    n_rows=0_i64; n_cols=0_i64

    call text_file%read_file(file, cell_array=cells, row_separator=NL, column_separator=delim)
    call text_file%empty()

    n_rows = size(cells, dim=1, kind=i64)
    n_cols = size(cells, dim=2, kind=i64)

    if ( (n_rows > 1_i64) .and. (n_cols > 1_i64) ) then
      if ( header ) then
        if ( n_rows /= 2_i64 ) then
          error stop LF//'Error reading file "'//file//'". Data cannot fit into one-dimensional array.'
          return
        end if
      else
        error stop LF//'Error reading file "'//file//'". Data cannot fit into one-dimensional array.'// &
                 ' If there are two rows including a header row, specify "header=.true." .'
        return
      end if
    end if

    if ( n_cols == 1_i64 ) then
      if ( header ) then
        if ( .not. (n_rows > 1_i64) ) then
          error stop LF//'Error reading file "'//file//'". File is empty after header.'
          return
        end if

        allocate( into(n_rows-1_i64) )
        call cells(2_i64:,1_i64)%cast(into=into, fmt=fmt); return
      else
        allocate( into(n_rows) )
        call cells(:,1_i64)%cast(into=into, fmt=fmt); return
      end if
    end if

    if ( header ) then
      if ( .not. (n_rows > 1_i64) ) then
        error stop LF//'Error reading file "'//file//'". File is empty after header.'
        return
      end if

      allocate( into(n_cols) )
      call cells(2_i64,:)%cast(into=into, fmt=fmt); return
    else
      allocate( into(n_cols) )
      call cells(1_i64,:)%cast(into=into, fmt=fmt); return
    end if
  end procedure from_text_1di16
  module procedure from_text_1di8
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer(i64)              :: n_rows, n_cols

    n_rows=0_i64; n_cols=0_i64

    call text_file%read_file(file, cell_array=cells, row_separator=NL, column_separator=delim)
    call text_file%empty()

    n_rows = size(cells, dim=1, kind=i64)
    n_cols = size(cells, dim=2, kind=i64)

    if ( (n_rows > 1_i64) .and. (n_cols > 1_i64) ) then
      if ( header ) then
        if ( n_rows /= 2_i64 ) then
          error stop LF//'Error reading file "'//file//'". Data cannot fit into one-dimensional array.'
          return
        end if
      else
        error stop LF//'Error reading file "'//file//'". Data cannot fit into one-dimensional array.'// &
                 ' If there are two rows including a header row, specify "header=.true." .'
        return
      end if
    end if

    if ( n_cols == 1_i64 ) then
      if ( header ) then
        if ( .not. (n_rows > 1_i64) ) then
          error stop LF//'Error reading file "'//file//'". File is empty after header.'
          return
        end if

        allocate( into(n_rows-1_i64) )
        call cells(2_i64:,1_i64)%cast(into=into, fmt=fmt); return
      else
        allocate( into(n_rows) )
        call cells(:,1_i64)%cast(into=into, fmt=fmt); return
      end if
    end if

    if ( header ) then
      if ( .not. (n_rows > 1_i64) ) then
        error stop LF//'Error reading file "'//file//'". File is empty after header.'
        return
      end if

      allocate( into(n_cols) )
      call cells(2_i64,:)%cast(into=into, fmt=fmt); return
    else
      allocate( into(n_cols) )
      call cells(1_i64,:)%cast(into=into, fmt=fmt); return
    end if
  end procedure from_text_1di8

  module procedure from_text_2di64
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer(i64)              :: n_rows, n_cols

    n_rows=0_i64; n_cols=0_i64

    call text_file%read_file(file, cell_array=cells, row_separator=NL, column_separator=delim)
    call text_file%empty()

    n_rows = size(cells, dim=1, kind=i64)
    n_cols = size(cells, dim=2, kind=i64)

    if ( header ) then
      if ( .not. (n_rows > 1_i64) ) then
        error stop LF//'Error reading file "'//file//'". File is empty after header.'
        return
      end if

      allocate( into(n_rows-1_i64,n_cols) )
      call cells(2_i64:,:)%cast(into=into, fmt=fmt); return
    else
      allocate( into(n_rows,n_cols) )
      call cells%cast(into=into, fmt=fmt); return
    end if
  end procedure from_text_2di64
  module procedure from_text_2di32
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer(i64)              :: n_rows, n_cols

    n_rows=0_i64; n_cols=0_i64

    call text_file%read_file(file, cell_array=cells, row_separator=NL, column_separator=delim)
    call text_file%empty()

    n_rows = size(cells, dim=1, kind=i64)
    n_cols = size(cells, dim=2, kind=i64)

    if ( header ) then
      if ( .not. (n_rows > 1_i64) ) then
        error stop LF//'Error reading file "'//file//'". File is empty after header.'
        return
      end if

      allocate( into(n_rows-1_i64,n_cols) )
      call cells(2_i64:,:)%cast(into=into, fmt=fmt); return
    else
      allocate( into(n_rows,n_cols) )
      call cells%cast(into=into, fmt=fmt); return
    end if
  end procedure from_text_2di32
  module procedure from_text_2di16
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer(i64)              :: n_rows, n_cols

    n_rows=0_i64; n_cols=0_i64

    call text_file%read_file(file, cell_array=cells, row_separator=NL, column_separator=delim)
    call text_file%empty()

    n_rows = size(cells, dim=1, kind=i64)
    n_cols = size(cells, dim=2, kind=i64)

    if ( header ) then
      if ( .not. (n_rows > 1_i64) ) then
        error stop LF//'Error reading file "'//file//'". File is empty after header.'
        return
      end if

      allocate( into(n_rows-1_i64,n_cols) )
      call cells(2_i64:,:)%cast(into=into, fmt=fmt); return
    else
      allocate( into(n_rows,n_cols) )
      call cells%cast(into=into, fmt=fmt); return
    end if
  end procedure from_text_2di16
  module procedure from_text_2di8
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer(i64)              :: n_rows, n_cols

    n_rows=0_i64; n_cols=0_i64

    call text_file%read_file(file, cell_array=cells, row_separator=NL, column_separator=delim)
    call text_file%empty()

    n_rows = size(cells, dim=1, kind=i64)
    n_cols = size(cells, dim=2, kind=i64)

    if ( header ) then
      if ( .not. (n_rows > 1_i64) ) then
        error stop LF//'Error reading file "'//file//'". File is empty after header.'
        return
      end if

      allocate( into(n_rows-1_i64,n_cols) )
      call cells(2_i64:,:)%cast(into=into, fmt=fmt); return
    else
      allocate( into(n_rows,n_cols) )
      call cells%cast(into=into, fmt=fmt); return
    end if
  end procedure from_text_2di8
end submodule text_io
