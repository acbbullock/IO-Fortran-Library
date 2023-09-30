submodule (io_fortran_lib) text_io
  !---------------------------------------------------------------------------------------------------------------------
  !! This submodule provides module procedure implementations for the **public interface** `echo` and the **private
  !! interfaces** `to_text` and `from_text`.
  !---------------------------------------------------------------------------------------------------------------------
  implicit none (type, external)

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

  module procedure to_text_c128
    type(String)                      :: text_file
    type(String), allocatable, target :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nrows, ncols, j
    logical :: header_present

    j=0; header_present=.false.

    select rank(x)
      rank(1); nrows = size(x); ncols = 1
      rank(2); nrows = size(x, dim=1); ncols = size(x, dim=2)
    end select

    if ( len(header) /= 0 ) header_present = .true.
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      select rank(x)
        rank(1)
          cells(1,1)%s = header(1)
        rank(2)
          if ( size(header) == 1 ) then
            do j = 1, ncols
              cells(1,j)%s = header(1)//str(j)
            end do
          else
            do j = 1, ncols
              cells(1,j)%s = header(j)
            end do
          end if
      end select
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    select rank(x)
      rank(1); call cast(x, numerical_data(:,1), locale, fmt, decimals, im)
      rank(2); call cast(x, numerical_data, locale, fmt, decimals, im)
    end select

    call text_file%write_file(cells, file, NL, delim, .false., stat, errmsg)
  end procedure to_text_c128
  module procedure to_text_c64
    type(String)                      :: text_file
    type(String), allocatable, target :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nrows, ncols, j
    logical :: header_present

    j=0; header_present=.false.

    select rank(x)
      rank(1); nrows = size(x); ncols = 1
      rank(2); nrows = size(x, dim=1); ncols = size(x, dim=2)
    end select

    if ( len(header) /= 0 ) header_present = .true.
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      select rank(x)
        rank(1)
          cells(1,1)%s = header(1)
        rank(2)
          if ( size(header) == 1 ) then
            do j = 1, ncols
              cells(1,j)%s = header(1)//str(j)
            end do
          else
            do j = 1, ncols
              cells(1,j)%s = header(j)
            end do
          end if
      end select
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    select rank(x)
      rank(1); call cast(x, numerical_data(:,1), locale, fmt, decimals, im)
      rank(2); call cast(x, numerical_data, locale, fmt, decimals, im)
    end select

    call text_file%write_file(cells, file, NL, delim, .false., stat, errmsg)
  end procedure to_text_c64
  module procedure to_text_c32
    type(String)                      :: text_file
    type(String), allocatable, target :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nrows, ncols, j
    logical :: header_present

    j=0; header_present=.false.

    select rank(x)
      rank(1); nrows = size(x); ncols = 1
      rank(2); nrows = size(x, dim=1); ncols = size(x, dim=2)
    end select

    if ( len(header) /= 0 ) header_present = .true.
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      select rank(x)
        rank(1)
          cells(1,1)%s = header(1)
        rank(2)
          if ( size(header) == 1 ) then
            do j = 1, ncols
              cells(1,j)%s = header(1)//str(j)
            end do
          else
            do j = 1, ncols
              cells(1,j)%s = header(j)
            end do
          end if
      end select
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    select rank(x)
      rank(1); call cast(x, numerical_data(:,1), locale, fmt, decimals, im)
      rank(2); call cast(x, numerical_data, locale, fmt, decimals, im)
    end select

    call text_file%write_file(cells, file, NL, delim, .false., stat, errmsg)
  end procedure to_text_c32

  module procedure to_text_r128
    type(String)                      :: text_file
    type(String), allocatable, target :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nrows, ncols, j
    logical :: header_present

    j=0; header_present=.false.

    select rank(x)
      rank(1); nrows = size(x); ncols = 1
      rank(2); nrows = size(x, dim=1); ncols = size(x, dim=2)
    end select

    if ( len(header) /= 0 ) header_present = .true.
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      select rank(x)
        rank(1)
          cells(1,1)%s = header(1)
        rank(2)
          if ( size(header) == 1 ) then
            do j = 1, ncols
              cells(1,j)%s = header(1)//str(j)
            end do
          else
            do j = 1, ncols
              cells(1,j)%s = header(j)
            end do
          end if
      end select
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    select rank(x)
      rank(1); call cast(x, numerical_data(:,1), locale, fmt, decimals)
      rank(2); call cast(x, numerical_data, locale, fmt, decimals)
    end select

    call text_file%write_file(cells, file, NL, delim, .false., stat, errmsg)
  end procedure to_text_r128
  module procedure to_text_r64
    type(String)                      :: text_file
    type(String), allocatable, target :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nrows, ncols, j
    logical :: header_present

    j=0; header_present=.false.

    select rank(x)
      rank(1); nrows = size(x); ncols = 1
      rank(2); nrows = size(x, dim=1); ncols = size(x, dim=2)
    end select

    if ( len(header) /= 0 ) header_present = .true.
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      select rank(x)
        rank(1)
          cells(1,1)%s = header(1)
        rank(2)
          if ( size(header) == 1 ) then
            do j = 1, ncols
              cells(1,j)%s = header(1)//str(j)
            end do
          else
            do j = 1, ncols
              cells(1,j)%s = header(j)
            end do
          end if
      end select
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    select rank(x)
      rank(1); call cast(x, numerical_data(:,1), locale, fmt, decimals)
      rank(2); call cast(x, numerical_data, locale, fmt, decimals)
    end select

    call text_file%write_file(cells, file, NL, delim, .false., stat, errmsg)
  end procedure to_text_r64
  module procedure to_text_r32
    type(String)                      :: text_file
    type(String), allocatable, target :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nrows, ncols, j
    logical :: header_present

    j=0; header_present=.false.

    select rank(x)
      rank(1); nrows = size(x); ncols = 1
      rank(2); nrows = size(x, dim=1); ncols = size(x, dim=2)
    end select

    if ( len(header) /= 0 ) header_present = .true.
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      select rank(x)
        rank(1)
          cells(1,1)%s = header(1)
        rank(2)
          if ( size(header) == 1 ) then
            do j = 1, ncols
              cells(1,j)%s = header(1)//str(j)
            end do
          else
            do j = 1, ncols
              cells(1,j)%s = header(j)
            end do
          end if
      end select
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    select rank(x)
      rank(1); call cast(x, numerical_data(:,1), locale, fmt, decimals)
      rank(2); call cast(x, numerical_data, locale, fmt, decimals)
    end select

    call text_file%write_file(cells, file, NL, delim, .false., stat, errmsg)
  end procedure to_text_r32

  module procedure to_text_i64
    type(String)                      :: text_file
    type(String), allocatable, target :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nrows, ncols, j
    logical :: header_present

    j=0; header_present=.false.

    select rank(x)
      rank(1); nrows = size(x); ncols = 1
      rank(2); nrows = size(x, dim=1); ncols = size(x, dim=2)
    end select

    if ( len(header) /= 0 ) header_present = .true.
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      select rank(x)
        rank(1)
          cells(1,1)%s = header(1)
        rank(2)
          if ( size(header) == 1 ) then
            do j = 1, ncols
              cells(1,j)%s = header(1)//str(j)
            end do
          else
            do j = 1, ncols
              cells(1,j)%s = header(j)
            end do
          end if
      end select
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    select rank(x)
      rank(1); call cast(x, numerical_data(:,1), fmt)
      rank(2); call cast(x, numerical_data, fmt)
    end select

    call text_file%write_file(cells, file, NL, delim, .false., stat, errmsg)
  end procedure to_text_i64
  module procedure to_text_i32
    type(String)                      :: text_file
    type(String), allocatable, target :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nrows, ncols, j
    logical :: header_present

    j=0; header_present=.false.

    select rank(x)
      rank(1); nrows = size(x); ncols = 1
      rank(2); nrows = size(x, dim=1); ncols = size(x, dim=2)
    end select

    if ( len(header) /= 0 ) header_present = .true.
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      select rank(x)
        rank(1)
          cells(1,1)%s = header(1)
        rank(2)
          if ( size(header) == 1 ) then
            do j = 1, ncols
              cells(1,j)%s = header(1)//str(j)
            end do
          else
            do j = 1, ncols
              cells(1,j)%s = header(j)
            end do
          end if
      end select
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    select rank(x)
      rank(1); call cast(x, numerical_data(:,1), fmt)
      rank(2); call cast(x, numerical_data, fmt)
    end select

    call text_file%write_file(cells, file, NL, delim, .false., stat, errmsg)
  end procedure to_text_i32
  module procedure to_text_i16
    type(String)                      :: text_file
    type(String), allocatable, target :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nrows, ncols, j
    logical :: header_present

    j=0; header_present=.false.

    select rank(x)
      rank(1); nrows = size(x); ncols = 1
      rank(2); nrows = size(x, dim=1); ncols = size(x, dim=2)
    end select

    if ( len(header) /= 0 ) header_present = .true.
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      select rank(x)
        rank(1)
          cells(1,1)%s = header(1)
        rank(2)
          if ( size(header) == 1 ) then
            do j = 1, ncols
              cells(1,j)%s = header(1)//str(j)
            end do
          else
            do j = 1, ncols
              cells(1,j)%s = header(j)
            end do
          end if
      end select
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    select rank(x)
      rank(1); call cast(x, numerical_data(:,1), fmt)
      rank(2); call cast(x, numerical_data, fmt)
    end select

    call text_file%write_file(cells, file, NL, delim, .false., stat, errmsg)
  end procedure to_text_i16
  module procedure to_text_i8
    type(String)                      :: text_file
    type(String), allocatable, target :: cells(:,:)
    type(String), pointer, contiguous :: numerical_data(:,:)

    integer :: nrows, ncols, j
    logical :: header_present

    j=0; header_present=.false.

    select rank(x)
      rank(1); nrows = size(x); ncols = 1
      rank(2); nrows = size(x, dim=1); ncols = size(x, dim=2)
    end select

    if ( len(header) /= 0 ) header_present = .true.
    if ( header_present ) nrows = nrows + 1

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    if ( header_present ) then
      select rank(x)
        rank(1)
          cells(1,1)%s = header(1)
        rank(2)
          if ( size(header) == 1 ) then
            do j = 1, ncols
              cells(1,j)%s = header(1)//str(j)
            end do
          else
            do j = 1, ncols
              cells(1,j)%s = header(j)
            end do
          end if
      end select
    end if

    numerical_data => cells
    if ( header_present ) numerical_data => cells(2:,:)

    select rank(x)
      rank(1); call cast(x, numerical_data(:,1), fmt)
      rank(2); call cast(x, numerical_data, fmt)
    end select

    call text_file%write_file(cells, file, NL, delim, .false., stat, errmsg)
  end procedure to_text_i8

  ! Reading Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  module procedure from_text_c128
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer                   :: nrows, ncols

    nrows=0; ncols=0

    if ( len(im) > 0 ) then
      call text_file%read_file(file, cells, NL, delim, stat, errmsg)
    else
      call custom_read(text_file, file, cells, NL, delim, stat, errmsg)
    end if

    if ( stat /= 0 ) return

    call text_file%empty()

    nrows = size(cells, dim=1); ncols = size(cells, dim=2)

    if ( (nrows == 1) .and. header ) then
      stat   = ARG_ERR
      errmsg = 'Error reading file "'//file//'". File read with one line, but header was specified as present.'
      return
    end if

    if ( (rank(into) == 1) .and. (ncols > 1) ) then
      stat   = ARG_ERR
      errmsg = 'Error reading file "'//file//'". Data has more than one column but actual argument is a '//&
               "one-dimensional array. Try reading into a two-dimensional array instead."
      return
    end if

    select rank(into)
      rank(1)
        if ( header ) then
          allocate( into(nrows-1), stat=stat, errmsg=errmsg )
        else
          allocate( into(nrows), stat=stat, errmsg=errmsg )
        end if
      rank(2)
        if ( header ) then
          allocate( into(nrows-1,ncols), stat=stat, errmsg=errmsg )
        else
          allocate( into(nrows,ncols), stat=stat, errmsg=errmsg )
        end if
    end select

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    select rank(into)
      rank(1)
        if ( header ) then
          call cast(cells(2:,1), into, locale=locale, fmt=fmt, im=im)
        else
          call cast(cells(:,1), into, locale=locale, fmt=fmt, im=im)
        end if
      rank(2)
        if ( header ) then
          call cast(cells(2:,:), into, locale=locale, fmt=fmt, im=im)
        else
          call cast(cells, into, locale=locale, fmt=fmt, im=im)
        end if
    end select
  end procedure from_text_c128
  module procedure from_text_c64
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer                   :: nrows, ncols

    nrows=0; ncols=0

    if ( len(im) > 0 ) then
      call text_file%read_file(file, cells, NL, delim, stat, errmsg)
    else
      call custom_read(text_file, file, cells, NL, delim, stat, errmsg)
    end if

    if ( stat /= 0 ) return

    call text_file%empty()

    nrows = size(cells, dim=1); ncols = size(cells, dim=2)

    if ( (nrows == 1) .and. header ) then
      stat   = ARG_ERR
      errmsg = 'Error reading file "'//file//'". File read with one line, but header was specified as present.'
      return
    end if

    if ( (rank(into) == 1) .and. (ncols > 1) ) then
      stat   = ARG_ERR
      errmsg = 'Error reading file "'//file//'". Data has more than one column but actual argument is a '//&
               "one-dimensional array. Try reading into a two-dimensional array instead."
      return
    end if

    select rank(into)
      rank(1)
        if ( header ) then
          allocate( into(nrows-1), stat=stat, errmsg=errmsg )
        else
          allocate( into(nrows), stat=stat, errmsg=errmsg )
        end if
      rank(2)
        if ( header ) then
          allocate( into(nrows-1,ncols), stat=stat, errmsg=errmsg )
        else
          allocate( into(nrows,ncols), stat=stat, errmsg=errmsg )
        end if
    end select

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    select rank(into)
      rank(1)
        if ( header ) then
          call cast(cells(2:,1), into, locale=locale, fmt=fmt, im=im)
        else
          call cast(cells(:,1), into, locale=locale, fmt=fmt, im=im)
        end if
      rank(2)
        if ( header ) then
          call cast(cells(2:,:), into, locale=locale, fmt=fmt, im=im)
        else
          call cast(cells, into, locale=locale, fmt=fmt, im=im)
        end if
    end select
  end procedure from_text_c64
  module procedure from_text_c32
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer                   :: nrows, ncols

    nrows=0; ncols=0

    if ( len(im) > 0 ) then
      call text_file%read_file(file, cells, NL, delim, stat, errmsg)
    else
      call custom_read(text_file, file, cells, NL, delim, stat, errmsg)
    end if

    if ( stat /= 0 ) return

    call text_file%empty()

    nrows = size(cells, dim=1); ncols = size(cells, dim=2)

    if ( (nrows == 1) .and. header ) then
      stat   = ARG_ERR
      errmsg = 'Error reading file "'//file//'". File read with one line, but header was specified as present.'
      return
    end if

    if ( (rank(into) == 1) .and. (ncols > 1) ) then
      stat   = ARG_ERR
      errmsg = 'Error reading file "'//file//'". Data has more than one column but actual argument is a '//&
               "one-dimensional array. Try reading into a two-dimensional array instead."
      return
    end if

    select rank(into)
      rank(1)
        if ( header ) then
          allocate( into(nrows-1), stat=stat, errmsg=errmsg )
        else
          allocate( into(nrows), stat=stat, errmsg=errmsg )
        end if
      rank(2)
        if ( header ) then
          allocate( into(nrows-1,ncols), stat=stat, errmsg=errmsg )
        else
          allocate( into(nrows,ncols), stat=stat, errmsg=errmsg )
        end if
    end select

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    select rank(into)
      rank(1)
        if ( header ) then
          call cast(cells(2:,1), into, locale=locale, fmt=fmt, im=im)
        else
          call cast(cells(:,1), into, locale=locale, fmt=fmt, im=im)
        end if
      rank(2)
        if ( header ) then
          call cast(cells(2:,:), into, locale=locale, fmt=fmt, im=im)
        else
          call cast(cells, into, locale=locale, fmt=fmt, im=im)
        end if
    end select
  end procedure from_text_c32

  impure recursive subroutine custom_read(text_file, file, cells, row_separator, column_separator, stat, errmsg)
    type(String),     intent(inout)            :: text_file
    character(len=*), intent(in)               :: file
    type(String),     intent(out), allocatable :: cells(:,:)
    character(len=*), intent(in)               :: row_separator, column_separator
    integer,          intent(out)              :: stat
    character(len=*), intent(out)              :: errmsg

    integer(i64) :: file_length, l, i
    integer      :: nrows, ncols, row, col, row_sep, col_sep, col_sep_len, open_paren, close_paren, current, file_unit
    logical      :: exists, in_paren

    stat=0; errmsg=EMPTY_STR

    file_length=0_i64; l=0_i64; i=0_i64
    nrows=0;ncols=0;row=0;col=0;row_sep=0;col_sep=0;col_sep_len=0;open_paren=0;close_paren=0;current=0;file_unit=0
    exists=.false.; in_paren=.false.

    inquire(file=file, exist=exists, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    file_unit = input_unit

    if ( exists ) then
      open( newunit=file_unit, file=file, status="old", form="unformatted", &
            action="read", access="stream", position="rewind", iostat=stat, iomsg=errmsg )
    else
      stat   = READ_ERR
      errmsg = 'Error reading file "'//file//'". No such file exists.'
      return
    end if

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    inquire(file=file, size=file_length, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    if ( file_length == 0_i64 ) then
      stat   = READ_ERR
      errmsg = 'Error reading file "'//file//'". File is empty.'
      return
    end if

    allocate( character(len=file_length) :: text_file%s, stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    read(unit=file_unit, iostat=stat, iomsg=errmsg) text_file%s

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    col_sep_len = len(column_separator)
    row_sep = iachar(NL); col_sep = iachar(column_separator(1:1))
    open_paren = iachar("("); close_paren = iachar(")")

    nrows = text_file%count(match=NL)

    ncols = 1; i = 1_i64; get_ncols: do
      current = iachar(text_file%s(i:i))

      if ( (current/=open_paren) .and. (current/=close_paren) .and. (current/=col_sep) .and. (current/=row_sep) ) then
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
          ncols = ncols + 1; i = i + 1_i64; cycle
        else
          if ( text_file%s(i:i+col_sep_len-1_i64) == column_separator ) then
            ncols = ncols + 1; i = i + col_sep_len; cycle
          else
            i = i + 1_i64; cycle
          end if
        end if
      end if

      if ( current == row_sep ) exit get_ncols
    end do get_ncols

    allocate( cells(nrows,ncols), stat=stat, errmsg=errmsg )

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    row = 1; col = 1; l = 1_i64; i = 1_i64; positional_transfers: do
      current = iachar(text_file%s(i:i))

      if ( (current/=open_paren) .and. (current/=close_paren) .and. (current/=col_sep) .and. (current/=row_sep) ) then
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
          col = col + 1; cycle
        else
          if ( text_file%s(i:i+col_sep_len-1_i64) == column_separator ) then
            cells(row,col)%s = text_file%s(l:i-1); i = i + col_sep_len; l = i
            col = col + 1; cycle
          else
            i = i + 1_i64; cycle
          end if
        end if
      end if

      if ( current == row_sep ) then
        cells(row,col)%s = text_file%s(l:i-1)
        if ( row == nrows ) exit positional_transfers
        i = i + 1_i64; l = i; col = 1; row = row + 1; cycle
      end if
    end do positional_transfers
  end subroutine custom_read

  module procedure from_text_r128
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer                   :: nrows, ncols

    nrows=0; ncols=0

    call text_file%read_file(file, cells, NL, delim, stat, errmsg)

    if ( stat /= 0 ) return

    call text_file%empty()

    nrows = size(cells, dim=1); ncols = size(cells, dim=2)

    if ( (nrows == 1) .and. header ) then
      stat   = ARG_ERR
      errmsg = 'Error reading file "'//file//'". File read with one line, but header was specified as present.'
      return
    end if

    if ( (rank(into) == 1) .and. (ncols > 1) ) then
      stat   = ARG_ERR
      errmsg = 'Error reading file "'//file//'". Data has more than one column but actual argument is a '//&
               "one-dimensional array. Try reading into a two-dimensional array instead."
      return
    end if

    select rank(into)
      rank(1)
        if ( header ) then
          allocate( into(nrows-1), stat=stat, errmsg=errmsg )
        else
          allocate( into(nrows), stat=stat, errmsg=errmsg )
        end if
      rank(2)
        if ( header ) then
          allocate( into(nrows-1,ncols), stat=stat, errmsg=errmsg )
        else
          allocate( into(nrows,ncols), stat=stat, errmsg=errmsg )
        end if
    end select

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    select rank(into)
      rank(1)
        if ( header ) then
          call cast(cells(2:,1), into, locale=locale, fmt=fmt)
        else
          call cast(cells(:,1), into, locale=locale, fmt=fmt)
        end if
      rank(2)
        if ( header ) then
          call cast(cells(2:,:), into, locale=locale, fmt=fmt)
        else
          call cast(cells, into, locale=locale, fmt=fmt)
        end if
    end select
  end procedure from_text_r128
  module procedure from_text_r64
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer                   :: nrows, ncols

    nrows=0; ncols=0

    call text_file%read_file(file, cells, NL, delim, stat, errmsg)

    if ( stat /= 0 ) return

    call text_file%empty()

    nrows = size(cells, dim=1); ncols = size(cells, dim=2)

    if ( (nrows == 1) .and. header ) then
      stat   = ARG_ERR
      errmsg = 'Error reading file "'//file//'". File read with one line, but header was specified as present.'
      return
    end if

    if ( (rank(into) == 1) .and. (ncols > 1) ) then
      stat   = ARG_ERR
      errmsg = 'Error reading file "'//file//'". Data has more than one column but actual argument is a '//&
               "one-dimensional array. Try reading into a two-dimensional array instead."
      return
    end if

    select rank(into)
      rank(1)
        if ( header ) then
          allocate( into(nrows-1), stat=stat, errmsg=errmsg )
        else
          allocate( into(nrows), stat=stat, errmsg=errmsg )
        end if
      rank(2)
        if ( header ) then
          allocate( into(nrows-1,ncols), stat=stat, errmsg=errmsg )
        else
          allocate( into(nrows,ncols), stat=stat, errmsg=errmsg )
        end if
    end select

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    select rank(into)
      rank(1)
        if ( header ) then
          call cast(cells(2:,1), into, locale=locale, fmt=fmt)
        else
          call cast(cells(:,1), into, locale=locale, fmt=fmt)
        end if
      rank(2)
        if ( header ) then
          call cast(cells(2:,:), into, locale=locale, fmt=fmt)
        else
          call cast(cells, into, locale=locale, fmt=fmt)
        end if
    end select
  end procedure from_text_r64
  module procedure from_text_r32
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer                   :: nrows, ncols

    nrows=0; ncols=0

    call text_file%read_file(file, cells, NL, delim, stat, errmsg)

    if ( stat /= 0 ) return

    call text_file%empty()

    nrows = size(cells, dim=1); ncols = size(cells, dim=2)

    if ( (nrows == 1) .and. header ) then
      stat   = ARG_ERR
      errmsg = 'Error reading file "'//file//'". File read with one line, but header was specified as present.'
      return
    end if

    if ( (rank(into) == 1) .and. (ncols > 1) ) then
      stat   = ARG_ERR
      errmsg = 'Error reading file "'//file//'". Data has more than one column but actual argument is a '//&
               "one-dimensional array. Try reading into a two-dimensional array instead."
      return
    end if

    select rank(into)
      rank(1)
        if ( header ) then
          allocate( into(nrows-1), stat=stat, errmsg=errmsg )
        else
          allocate( into(nrows), stat=stat, errmsg=errmsg )
        end if
      rank(2)
        if ( header ) then
          allocate( into(nrows-1,ncols), stat=stat, errmsg=errmsg )
        else
          allocate( into(nrows,ncols), stat=stat, errmsg=errmsg )
        end if
    end select

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    select rank(into)
      rank(1)
        if ( header ) then
          call cast(cells(2:,1), into, locale=locale, fmt=fmt)
        else
          call cast(cells(:,1), into, locale=locale, fmt=fmt)
        end if
      rank(2)
        if ( header ) then
          call cast(cells(2:,:), into, locale=locale, fmt=fmt)
        else
          call cast(cells, into, locale=locale, fmt=fmt)
        end if
    end select
  end procedure from_text_r32

  module procedure from_text_i64
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer                   :: nrows, ncols

    nrows=0; ncols=0

    call text_file%read_file(file, cells, NL, delim, stat, errmsg)

    if ( stat /= 0 ) return

    call text_file%empty()

    nrows = size(cells, dim=1); ncols = size(cells, dim=2)

    if ( (nrows == 1) .and. header ) then
      stat   = ARG_ERR
      errmsg = 'Error reading file "'//file//'". File read with one line, but header was specified as present.'
      return
    end if

    if ( (rank(into) == 1) .and. (ncols > 1) ) then
      stat   = ARG_ERR
      errmsg = 'Error reading file "'//file//'". Data has more than one column but actual argument is a '//&
               "one-dimensional array. Try reading into a two-dimensional array instead."
      return
    end if

    select rank(into)
      rank(1)
        if ( header ) then
          allocate( into(nrows-1), stat=stat, errmsg=errmsg )
        else
          allocate( into(nrows), stat=stat, errmsg=errmsg )
        end if
      rank(2)
        if ( header ) then
          allocate( into(nrows-1,ncols), stat=stat, errmsg=errmsg )
        else
          allocate( into(nrows,ncols), stat=stat, errmsg=errmsg )
        end if
    end select

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    select rank(into)
      rank(1)
        if ( header ) then
          call cast(cells(2:,1), into, fmt)
        else
          call cast(cells(:,1), into, fmt)
        end if
      rank(2)
        if ( header ) then
          call cast(cells(2:,:), into, fmt)
        else
          call cast(cells, into, fmt)
        end if
    end select
  end procedure from_text_i64
  module procedure from_text_i32
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer                   :: nrows, ncols

    nrows=0; ncols=0

    call text_file%read_file(file, cells, NL, delim, stat, errmsg)

    if ( stat /= 0 ) return

    call text_file%empty()

    nrows = size(cells, dim=1); ncols = size(cells, dim=2)

    if ( (nrows == 1) .and. header ) then
      stat   = ARG_ERR
      errmsg = 'Error reading file "'//file//'". File read with one line, but header was specified as present.'
      return
    end if

    if ( (rank(into) == 1) .and. (ncols > 1) ) then
      stat   = ARG_ERR
      errmsg = 'Error reading file "'//file//'". Data has more than one column but actual argument is a '//&
               "one-dimensional array. Try reading into a two-dimensional array instead."
      return
    end if

    select rank(into)
      rank(1)
        if ( header ) then
          allocate( into(nrows-1), stat=stat, errmsg=errmsg )
        else
          allocate( into(nrows), stat=stat, errmsg=errmsg )
        end if
      rank(2)
        if ( header ) then
          allocate( into(nrows-1,ncols), stat=stat, errmsg=errmsg )
        else
          allocate( into(nrows,ncols), stat=stat, errmsg=errmsg )
        end if
    end select

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    select rank(into)
      rank(1)
        if ( header ) then
          call cast(cells(2:,1), into, fmt)
        else
          call cast(cells(:,1), into, fmt)
        end if
      rank(2)
        if ( header ) then
          call cast(cells(2:,:), into, fmt)
        else
          call cast(cells, into, fmt)
        end if
    end select
  end procedure from_text_i32
  module procedure from_text_i16
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer                   :: nrows, ncols

    nrows=0; ncols=0

    call text_file%read_file(file, cells, NL, delim, stat, errmsg)

    if ( stat /= 0 ) return

    call text_file%empty()

    nrows = size(cells, dim=1); ncols = size(cells, dim=2)

    if ( (nrows == 1) .and. header ) then
      stat   = ARG_ERR
      errmsg = 'Error reading file "'//file//'". File read with one line, but header was specified as present.'
      return
    end if

    if ( (rank(into) == 1) .and. (ncols > 1) ) then
      stat   = ARG_ERR
      errmsg = 'Error reading file "'//file//'". Data has more than one column but actual argument is a '//&
               "one-dimensional array. Try reading into a two-dimensional array instead."
      return
    end if

    select rank(into)
      rank(1)
        if ( header ) then
          allocate( into(nrows-1), stat=stat, errmsg=errmsg )
        else
          allocate( into(nrows), stat=stat, errmsg=errmsg )
        end if
      rank(2)
        if ( header ) then
          allocate( into(nrows-1,ncols), stat=stat, errmsg=errmsg )
        else
          allocate( into(nrows,ncols), stat=stat, errmsg=errmsg )
        end if
    end select

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    select rank(into)
      rank(1)
        if ( header ) then
          call cast(cells(2:,1), into, fmt)
        else
          call cast(cells(:,1), into, fmt)
        end if
      rank(2)
        if ( header ) then
          call cast(cells(2:,:), into, fmt)
        else
          call cast(cells, into, fmt)
        end if
    end select
  end procedure from_text_i16
  module procedure from_text_i8
    type(String)              :: text_file
    type(String), allocatable :: cells(:,:)
    integer                   :: nrows, ncols

    nrows=0; ncols=0

    call text_file%read_file(file, cells, NL, delim, stat, errmsg)

    if ( stat /= 0 ) return

    call text_file%empty()

    nrows = size(cells, dim=1); ncols = size(cells, dim=2)

    if ( (nrows == 1) .and. header ) then
      stat   = ARG_ERR
      errmsg = 'Error reading file "'//file//'". File read with one line, but header was specified as present.'
      return
    end if

    if ( (rank(into) == 1) .and. (ncols > 1) ) then
      stat   = ARG_ERR
      errmsg = 'Error reading file "'//file//'". Data has more than one column but actual argument is a '//&
               "one-dimensional array. Try reading into a two-dimensional array instead."
      return
    end if

    select rank(into)
      rank(1)
        if ( header ) then
          allocate( into(nrows-1), stat=stat, errmsg=errmsg )
        else
          allocate( into(nrows), stat=stat, errmsg=errmsg )
        end if
      rank(2)
        if ( header ) then
          allocate( into(nrows-1,ncols), stat=stat, errmsg=errmsg )
        else
          allocate( into(nrows,ncols), stat=stat, errmsg=errmsg )
        end if
    end select

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    select rank(into)
      rank(1)
        if ( header ) then
          call cast(cells(2:,1), into, fmt)
        else
          call cast(cells(:,1), into, fmt)
        end if
      rank(2)
        if ( header ) then
          call cast(cells(2:,:), into, fmt)
        else
          call cast(cells, into, fmt)
        end if
    end select
  end procedure from_text_i8
end submodule text_io
