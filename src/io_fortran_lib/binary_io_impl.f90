submodule (io_fortran_lib) binary_io
  !---------------------------------------------------------------------------------------------------------------------
  !! This submodule provides module procedure implementations for the **private interfaces** `to_binary` and
  !! `from_binary`.
  !---------------------------------------------------------------------------------------------------------------------
  implicit none (type, external)

  contains ! Procedure bodies for module subprograms <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

  ! Writing Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  module procedure to_binary_c128
    logical :: exists
    integer :: file_unit

    exists    = .false.
    file_unit = 0

    inquire(file=file, exist=exists, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    file_unit = output_unit

    if ( .not. exists ) then
      open( newunit=file_unit, file=file, status="new", form="unformatted", &
            action="write", access="stream", iostat=stat, iomsg=errmsg )
    else
      open( newunit=file_unit, file=file, status="replace", form="unformatted", &
            action="write", access="stream", iostat=stat, iomsg=errmsg )
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    select rank(x)
      rank(1);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(2);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(3);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(4);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(5);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(6);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(7);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(8);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(9);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(10); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(11); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(12); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(13); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(14); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(15); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
    end select

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_binary_c128
  module procedure to_binary_c64
    logical :: exists
    integer :: file_unit

    exists    = .false.
    file_unit = 0

    inquire(file=file, exist=exists, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    file_unit = output_unit

    if ( .not. exists ) then
      open( newunit=file_unit, file=file, status="new", form="unformatted", &
            action="write", access="stream", iostat=stat, iomsg=errmsg )
    else
      open( newunit=file_unit, file=file, status="replace", form="unformatted", &
            action="write", access="stream", iostat=stat, iomsg=errmsg )
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    select rank(x)
      rank(1);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(2);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(3);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(4);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(5);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(6);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(7);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(8);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(9);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(10); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(11); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(12); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(13); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(14); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(15); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
    end select

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_binary_c64
  module procedure to_binary_c32
    logical :: exists
    integer :: file_unit

    exists    = .false.
    file_unit = 0

    inquire(file=file, exist=exists, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    file_unit = output_unit

    if ( .not. exists ) then
      open( newunit=file_unit, file=file, status="new", form="unformatted", &
            action="write", access="stream", iostat=stat, iomsg=errmsg )
    else
      open( newunit=file_unit, file=file, status="replace", form="unformatted", &
            action="write", access="stream", iostat=stat, iomsg=errmsg )
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    select rank(x)
      rank(1);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(2);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(3);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(4);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(5);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(6);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(7);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(8);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(9);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(10); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(11); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(12); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(13); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(14); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(15); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
    end select

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_binary_c32

  module procedure to_binary_r128
    logical :: exists
    integer :: file_unit

    exists    = .false.
    file_unit = 0

    inquire(file=file, exist=exists, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    file_unit = output_unit

    if ( .not. exists ) then
      open( newunit=file_unit, file=file, status="new", form="unformatted", &
            action="write", access="stream", iostat=stat, iomsg=errmsg )
    else
      open( newunit=file_unit, file=file, status="replace", form="unformatted", &
            action="write", access="stream", iostat=stat, iomsg=errmsg )
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    select rank(x)
      rank(1);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(2);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(3);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(4);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(5);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(6);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(7);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(8);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(9);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(10); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(11); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(12); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(13); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(14); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(15); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
    end select

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_binary_r128
  module procedure to_binary_r64
    logical :: exists
    integer :: file_unit

    exists    = .false.
    file_unit = 0

    inquire(file=file, exist=exists, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    file_unit = output_unit

    if ( .not. exists ) then
      open( newunit=file_unit, file=file, status="new", form="unformatted", &
            action="write", access="stream", iostat=stat, iomsg=errmsg )
    else
      open( newunit=file_unit, file=file, status="replace", form="unformatted", &
            action="write", access="stream", iostat=stat, iomsg=errmsg )
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    select rank(x)
      rank(1);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(2);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(3);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(4);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(5);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(6);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(7);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(8);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(9);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(10); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(11); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(12); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(13); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(14); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(15); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
    end select

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_binary_r64
  module procedure to_binary_r32
    logical :: exists
    integer :: file_unit

    exists    = .false.
    file_unit = 0

    inquire(file=file, exist=exists, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    file_unit = output_unit

    if ( .not. exists ) then
      open( newunit=file_unit, file=file, status="new", form="unformatted", &
            action="write", access="stream", iostat=stat, iomsg=errmsg )
    else
      open( newunit=file_unit, file=file, status="replace", form="unformatted", &
            action="write", access="stream", iostat=stat, iomsg=errmsg )
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    select rank(x)
      rank(1);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(2);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(3);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(4);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(5);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(6);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(7);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(8);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(9);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(10); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(11); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(12); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(13); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(14); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(15); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
    end select

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_binary_r32

  module procedure to_binary_i64
    logical :: exists
    integer :: file_unit

    exists    = .false.
    file_unit = 0

    inquire(file=file, exist=exists, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    file_unit = output_unit

    if ( .not. exists ) then
      open( newunit=file_unit, file=file, status="new", form="unformatted", &
            action="write", access="stream", iostat=stat, iomsg=errmsg )
    else
      open( newunit=file_unit, file=file, status="replace", form="unformatted", &
            action="write", access="stream", iostat=stat, iomsg=errmsg )
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    select rank(x)
      rank(1);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(2);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(3);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(4);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(5);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(6);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(7);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(8);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(9);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(10); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(11); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(12); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(13); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(14); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(15); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
    end select

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_binary_i64
  module procedure to_binary_i32
    logical :: exists
    integer :: file_unit

    exists    = .false.
    file_unit = 0

    inquire(file=file, exist=exists, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    file_unit = output_unit

    if ( .not. exists ) then
      open( newunit=file_unit, file=file, status="new", form="unformatted", &
            action="write", access="stream", iostat=stat, iomsg=errmsg )
    else
      open( newunit=file_unit, file=file, status="replace", form="unformatted", &
            action="write", access="stream", iostat=stat, iomsg=errmsg )
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    select rank(x)
      rank(1);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(2);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(3);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(4);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(5);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(6);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(7);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(8);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(9);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(10); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(11); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(12); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(13); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(14); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(15); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
    end select

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_binary_i32
  module procedure to_binary_i16
    logical :: exists
    integer :: file_unit

    exists    = .false.
    file_unit = 0

    inquire(file=file, exist=exists, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    file_unit = output_unit

    if ( .not. exists ) then
      open( newunit=file_unit, file=file, status="new", form="unformatted", &
            action="write", access="stream", iostat=stat, iomsg=errmsg )
    else
      open( newunit=file_unit, file=file, status="replace", form="unformatted", &
            action="write", access="stream", iostat=stat, iomsg=errmsg )
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    select rank(x)
      rank(1);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(2);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(3);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(4);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(5);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(6);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(7);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(8);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(9);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(10); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(11); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(12); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(13); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(14); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(15); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
    end select

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_binary_i16
  module procedure to_binary_i8
    logical :: exists
    integer :: file_unit

    exists    = .false.
    file_unit = 0

    inquire(file=file, exist=exists, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    file_unit = output_unit

    if ( .not. exists ) then
      open( newunit=file_unit, file=file, status="new", form="unformatted", &
            action="write", access="stream", iostat=stat, iomsg=errmsg )
    else
      open( newunit=file_unit, file=file, status="replace", form="unformatted", &
            action="write", access="stream", iostat=stat, iomsg=errmsg )
    end if

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    select rank(x)
      rank(1);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(2);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(3);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(4);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(5);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(6);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(7);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(8);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(9);  write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(10); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(11); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(12); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(13); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(14); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
      rank(15); write(unit=file_unit, iostat=stat, iomsg=errmsg) x
    end select

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = WRITE_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure to_binary_i8

  ! Reading Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  module procedure from_binary_c128
    logical :: exists
    integer :: file_unit

    exists    = .false.
    file_unit = 0

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

    select rank(into)
      rank(1)
        allocate( into(data_shape(1)), stat=stat, errmsg=errmsg )
      rank(2)
        allocate( into(data_shape(1), data_shape(2)), stat=stat, errmsg=errmsg )
      rank(3)
        allocate( into(data_shape(1), data_shape(2), data_shape(3)), stat=stat, errmsg=errmsg )
      rank(4)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)), stat=stat, errmsg=errmsg )
      rank(5)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)), stat=stat, &
                       errmsg=errmsg )
      rank(6)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6)), stat=stat, errmsg=errmsg )
      rank(7)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7)), stat=stat, errmsg=errmsg )
      rank(8)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8)), stat=stat, errmsg=errmsg )
      rank(9)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9)), stat=stat, errmsg=errmsg )
      rank(10)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10)), stat=stat, &
                       errmsg=errmsg )
      rank(11)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11)), stat=stat, errmsg=errmsg )
      rank(12)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12)), stat=stat, errmsg=errmsg )
      rank(13)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13)), stat=stat, errmsg=errmsg )
      rank(14)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13), data_shape(14)), stat=stat, &
                       errmsg=errmsg )
      rank(15)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13), data_shape(14), data_shape(15)), &
                       stat=stat, errmsg=errmsg )
    end select

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    select rank(into)
      rank(1);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(2);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(3);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(4);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(5);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(6);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(7);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(8);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(9);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(10); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(11); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(12); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(13); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(14); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(15); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
    end select

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure from_binary_c128
  module procedure from_binary_c64
    logical :: exists
    integer :: file_unit

    exists    = .false.
    file_unit = 0

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

    select rank(into)
      rank(1)
        allocate( into(data_shape(1)), stat=stat, errmsg=errmsg )
      rank(2)
        allocate( into(data_shape(1), data_shape(2)), stat=stat, errmsg=errmsg )
      rank(3)
        allocate( into(data_shape(1), data_shape(2), data_shape(3)), stat=stat, errmsg=errmsg )
      rank(4)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)), stat=stat, errmsg=errmsg )
      rank(5)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)), stat=stat, &
                       errmsg=errmsg )
      rank(6)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6)), stat=stat, errmsg=errmsg )
      rank(7)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7)), stat=stat, errmsg=errmsg )
      rank(8)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8)), stat=stat, errmsg=errmsg )
      rank(9)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9)), stat=stat, errmsg=errmsg )
      rank(10)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10)), stat=stat, &
                       errmsg=errmsg )
      rank(11)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11)), stat=stat, errmsg=errmsg )
      rank(12)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12)), stat=stat, errmsg=errmsg )
      rank(13)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13)), stat=stat, errmsg=errmsg )
      rank(14)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13), data_shape(14)), stat=stat, &
                       errmsg=errmsg )
      rank(15)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13), data_shape(14), data_shape(15)), &
                       stat=stat, errmsg=errmsg )
    end select

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    select rank(into)
      rank(1);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(2);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(3);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(4);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(5);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(6);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(7);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(8);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(9);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(10); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(11); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(12); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(13); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(14); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(15); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
    end select

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure from_binary_c64
  module procedure from_binary_c32
    logical :: exists
    integer :: file_unit

    exists    = .false.
    file_unit = 0

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

    select rank(into)
      rank(1)
        allocate( into(data_shape(1)), stat=stat, errmsg=errmsg )
      rank(2)
        allocate( into(data_shape(1), data_shape(2)), stat=stat, errmsg=errmsg )
      rank(3)
        allocate( into(data_shape(1), data_shape(2), data_shape(3)), stat=stat, errmsg=errmsg )
      rank(4)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)), stat=stat, errmsg=errmsg )
      rank(5)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)), stat=stat, &
                       errmsg=errmsg )
      rank(6)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6)), stat=stat, errmsg=errmsg )
      rank(7)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7)), stat=stat, errmsg=errmsg )
      rank(8)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8)), stat=stat, errmsg=errmsg )
      rank(9)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9)), stat=stat, errmsg=errmsg )
      rank(10)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10)), stat=stat, &
                       errmsg=errmsg )
      rank(11)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11)), stat=stat, errmsg=errmsg )
      rank(12)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12)), stat=stat, errmsg=errmsg )
      rank(13)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13)), stat=stat, errmsg=errmsg )
      rank(14)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13), data_shape(14)), stat=stat, &
                       errmsg=errmsg )
      rank(15)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13), data_shape(14), data_shape(15)), &
                       stat=stat, errmsg=errmsg )
    end select

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    select rank(into)
      rank(1);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(2);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(3);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(4);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(5);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(6);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(7);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(8);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(9);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(10); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(11); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(12); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(13); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(14); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(15); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
    end select

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure from_binary_c32

  module procedure from_binary_r128
    logical :: exists
    integer :: file_unit

    exists    = .false.
    file_unit = 0

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

    select rank(into)
      rank(1)
        allocate( into(data_shape(1)), stat=stat, errmsg=errmsg )
      rank(2)
        allocate( into(data_shape(1), data_shape(2)), stat=stat, errmsg=errmsg )
      rank(3)
        allocate( into(data_shape(1), data_shape(2), data_shape(3)), stat=stat, errmsg=errmsg )
      rank(4)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)), stat=stat, errmsg=errmsg )
      rank(5)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)), stat=stat, &
                       errmsg=errmsg )
      rank(6)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6)), stat=stat, errmsg=errmsg )
      rank(7)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7)), stat=stat, errmsg=errmsg )
      rank(8)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8)), stat=stat, errmsg=errmsg )
      rank(9)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9)), stat=stat, errmsg=errmsg )
      rank(10)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10)), stat=stat, &
                       errmsg=errmsg )
      rank(11)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11)), stat=stat, errmsg=errmsg )
      rank(12)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12)), stat=stat, errmsg=errmsg )
      rank(13)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13)), stat=stat, errmsg=errmsg )
      rank(14)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13), data_shape(14)), stat=stat, &
                       errmsg=errmsg )
      rank(15)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13), data_shape(14), data_shape(15)), &
                       stat=stat, errmsg=errmsg )
    end select

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    select rank(into)
      rank(1);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(2);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(3);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(4);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(5);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(6);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(7);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(8);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(9);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(10); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(11); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(12); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(13); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(14); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(15); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
    end select

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure from_binary_r128
  module procedure from_binary_r64
    logical :: exists
    integer :: file_unit

    exists    = .false.
    file_unit = 0

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

    select rank(into)
      rank(1)
        allocate( into(data_shape(1)), stat=stat, errmsg=errmsg )
      rank(2)
        allocate( into(data_shape(1), data_shape(2)), stat=stat, errmsg=errmsg )
      rank(3)
        allocate( into(data_shape(1), data_shape(2), data_shape(3)), stat=stat, errmsg=errmsg )
      rank(4)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)), stat=stat, errmsg=errmsg )
      rank(5)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)), stat=stat, &
                       errmsg=errmsg )
      rank(6)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6)), stat=stat, errmsg=errmsg )
      rank(7)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7)), stat=stat, errmsg=errmsg )
      rank(8)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8)), stat=stat, errmsg=errmsg )
      rank(9)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9)), stat=stat, errmsg=errmsg )
      rank(10)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10)), stat=stat, &
                       errmsg=errmsg )
      rank(11)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11)), stat=stat, errmsg=errmsg )
      rank(12)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12)), stat=stat, errmsg=errmsg )
      rank(13)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13)), stat=stat, errmsg=errmsg )
      rank(14)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13), data_shape(14)), stat=stat, &
                       errmsg=errmsg )
      rank(15)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13), data_shape(14), data_shape(15)), &
                       stat=stat, errmsg=errmsg )
    end select

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    select rank(into)
      rank(1);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(2);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(3);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(4);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(5);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(6);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(7);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(8);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(9);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(10); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(11); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(12); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(13); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(14); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(15); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
    end select

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure from_binary_r64
  module procedure from_binary_r32
    logical :: exists
    integer :: file_unit

    exists    = .false.
    file_unit = 0

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

    select rank(into)
      rank(1)
        allocate( into(data_shape(1)), stat=stat, errmsg=errmsg )
      rank(2)
        allocate( into(data_shape(1), data_shape(2)), stat=stat, errmsg=errmsg )
      rank(3)
        allocate( into(data_shape(1), data_shape(2), data_shape(3)), stat=stat, errmsg=errmsg )
      rank(4)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)), stat=stat, errmsg=errmsg )
      rank(5)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)), stat=stat, &
                       errmsg=errmsg )
      rank(6)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6)), stat=stat, errmsg=errmsg )
      rank(7)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7)), stat=stat, errmsg=errmsg )
      rank(8)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8)), stat=stat, errmsg=errmsg )
      rank(9)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9)), stat=stat, errmsg=errmsg )
      rank(10)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10)), stat=stat, &
                       errmsg=errmsg )
      rank(11)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11)), stat=stat, errmsg=errmsg )
      rank(12)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12)), stat=stat, errmsg=errmsg )
      rank(13)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13)), stat=stat, errmsg=errmsg )
      rank(14)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13), data_shape(14)), stat=stat, &
                       errmsg=errmsg )
      rank(15)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13), data_shape(14), data_shape(15)), &
                       stat=stat, errmsg=errmsg )
    end select

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    select rank(into)
      rank(1);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(2);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(3);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(4);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(5);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(6);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(7);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(8);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(9);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(10); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(11); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(12); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(13); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(14); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(15); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
    end select

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure from_binary_r32

  module procedure from_binary_i64
    logical :: exists
    integer :: file_unit

    exists    = .false.
    file_unit = 0

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

    select rank(into)
      rank(1)
        allocate( into(data_shape(1)), stat=stat, errmsg=errmsg )
      rank(2)
        allocate( into(data_shape(1), data_shape(2)), stat=stat, errmsg=errmsg )
      rank(3)
        allocate( into(data_shape(1), data_shape(2), data_shape(3)), stat=stat, errmsg=errmsg )
      rank(4)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)), stat=stat, errmsg=errmsg )
      rank(5)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)), stat=stat, &
                       errmsg=errmsg )
      rank(6)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6)), stat=stat, errmsg=errmsg )
      rank(7)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7)), stat=stat, errmsg=errmsg )
      rank(8)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8)), stat=stat, errmsg=errmsg )
      rank(9)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9)), stat=stat, errmsg=errmsg )
      rank(10)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10)), stat=stat, &
                       errmsg=errmsg )
      rank(11)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11)), stat=stat, errmsg=errmsg )
      rank(12)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12)), stat=stat, errmsg=errmsg )
      rank(13)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13)), stat=stat, errmsg=errmsg )
      rank(14)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13), data_shape(14)), stat=stat, &
                       errmsg=errmsg )
      rank(15)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13), data_shape(14), data_shape(15)), &
                       stat=stat, errmsg=errmsg )
    end select

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    select rank(into)
      rank(1);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(2);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(3);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(4);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(5);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(6);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(7);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(8);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(9);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(10); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(11); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(12); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(13); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(14); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(15); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
    end select

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure from_binary_i64
  module procedure from_binary_i32
    logical :: exists
    integer :: file_unit

    exists    = .false.
    file_unit = 0

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

    select rank(into)
      rank(1)
        allocate( into(data_shape(1)), stat=stat, errmsg=errmsg )
      rank(2)
        allocate( into(data_shape(1), data_shape(2)), stat=stat, errmsg=errmsg )
      rank(3)
        allocate( into(data_shape(1), data_shape(2), data_shape(3)), stat=stat, errmsg=errmsg )
      rank(4)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)), stat=stat, errmsg=errmsg )
      rank(5)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)), stat=stat, &
                       errmsg=errmsg )
      rank(6)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6)), stat=stat, errmsg=errmsg )
      rank(7)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7)), stat=stat, errmsg=errmsg )
      rank(8)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8)), stat=stat, errmsg=errmsg )
      rank(9)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9)), stat=stat, errmsg=errmsg )
      rank(10)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10)), stat=stat, &
                       errmsg=errmsg )
      rank(11)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11)), stat=stat, errmsg=errmsg )
      rank(12)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12)), stat=stat, errmsg=errmsg )
      rank(13)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13)), stat=stat, errmsg=errmsg )
      rank(14)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13), data_shape(14)), stat=stat, &
                       errmsg=errmsg )
      rank(15)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13), data_shape(14), data_shape(15)), &
                       stat=stat, errmsg=errmsg )
    end select

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    select rank(into)
      rank(1);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(2);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(3);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(4);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(5);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(6);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(7);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(8);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(9);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(10); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(11); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(12); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(13); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(14); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(15); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
    end select

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure from_binary_i32
  module procedure from_binary_i16
    logical :: exists
    integer :: file_unit

    exists    = .false.
    file_unit = 0

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

    select rank(into)
      rank(1)
        allocate( into(data_shape(1)), stat=stat, errmsg=errmsg )
      rank(2)
        allocate( into(data_shape(1), data_shape(2)), stat=stat, errmsg=errmsg )
      rank(3)
        allocate( into(data_shape(1), data_shape(2), data_shape(3)), stat=stat, errmsg=errmsg )
      rank(4)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)), stat=stat, errmsg=errmsg )
      rank(5)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)), stat=stat, &
                       errmsg=errmsg )
      rank(6)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6)), stat=stat, errmsg=errmsg )
      rank(7)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7)), stat=stat, errmsg=errmsg )
      rank(8)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8)), stat=stat, errmsg=errmsg )
      rank(9)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9)), stat=stat, errmsg=errmsg )
      rank(10)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10)), stat=stat, &
                       errmsg=errmsg )
      rank(11)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11)), stat=stat, errmsg=errmsg )
      rank(12)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12)), stat=stat, errmsg=errmsg )
      rank(13)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13)), stat=stat, errmsg=errmsg )
      rank(14)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13), data_shape(14)), stat=stat, &
                       errmsg=errmsg )
      rank(15)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13), data_shape(14), data_shape(15)), &
                       stat=stat, errmsg=errmsg )
    end select

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    select rank(into)
      rank(1);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(2);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(3);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(4);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(5);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(6);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(7);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(8);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(9);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(10); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(11); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(12); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(13); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(14); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(15); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
    end select

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure from_binary_i16
  module procedure from_binary_i8
    logical :: exists
    integer :: file_unit

    exists    = .false.
    file_unit = 0

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

    select rank(into)
      rank(1)
        allocate( into(data_shape(1)), stat=stat, errmsg=errmsg )
      rank(2)
        allocate( into(data_shape(1), data_shape(2)), stat=stat, errmsg=errmsg )
      rank(3)
        allocate( into(data_shape(1), data_shape(2), data_shape(3)), stat=stat, errmsg=errmsg )
      rank(4)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)), stat=stat, errmsg=errmsg )
      rank(5)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)), stat=stat, &
                       errmsg=errmsg )
      rank(6)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6)), stat=stat, errmsg=errmsg )
      rank(7)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7)), stat=stat, errmsg=errmsg )
      rank(8)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8)), stat=stat, errmsg=errmsg )
      rank(9)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9)), stat=stat, errmsg=errmsg )
      rank(10)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10)), stat=stat, &
                       errmsg=errmsg )
      rank(11)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11)), stat=stat, errmsg=errmsg )
      rank(12)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12)), stat=stat, errmsg=errmsg )
      rank(13)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13)), stat=stat, errmsg=errmsg )
      rank(14)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13), data_shape(14)), stat=stat, &
                       errmsg=errmsg )
      rank(15)
        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), &
                       data_shape(6), data_shape(7), data_shape(8), data_shape(9), data_shape(10), &
                       data_shape(11), data_shape(12), data_shape(13), data_shape(14), data_shape(15)), &
                       stat=stat, errmsg=errmsg )
    end select

    if ( stat /= 0 ) then
      stat = ALLOC_ERR; return
    end if

    select rank(into)
      rank(1);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(2);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(3);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(4);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(5);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(6);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(7);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(8);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(9);  read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(10); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(11); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(12); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(13); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(14); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
      rank(15); read(unit=file_unit, iostat=stat, iomsg=errmsg) into
    end select

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    close(unit=file_unit, iostat=stat, iomsg=errmsg)

    if ( stat /= 0 ) then
      stat = READ_ERR; return
    end if

    stat=0; errmsg=EMPTY_STR
  end procedure from_binary_i8
end submodule binary_io
