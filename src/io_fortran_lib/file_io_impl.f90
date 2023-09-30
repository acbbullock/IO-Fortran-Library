submodule (io_fortran_lib) file_io
  !---------------------------------------------------------------------------------------------------------------------
  !! This submodule provides module procedure implementations for the **public interfaces** `to_file` and
  !! `from_file`.
  !---------------------------------------------------------------------------------------------------------------------
  implicit none (type, external)

  ! Definitions and interfaces ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=0), target :: EMPTY_STRING    = EMPTY_STR
  character(len=0), target :: EMPTY_HEADER(1) = [ EMPTY_STR ]
  character(len=1), target :: EXP_FMT         = REAL_FMTS(1)
  character(len=1), target :: INT_FMT         = INT_FMTS(1)
  character(len=1), target :: COMMA_DELIMITER = COMMA
  character(len=1), target :: SEMICOLON_DELIM = SEMICOLON
  character(len=2), target :: US_LOCALE       = LOCALES(1)
  integer,          target :: MAX_DECIMALS    = 150
  logical,          target :: ABSENT_HEADER   = .false.

  contains ! Procedure bodies for module subprograms <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

  module procedure ext_of
    integer :: i, l

    i=0; l=0

    l = len_trim(file)

    do i = l, 1, -1
      if ( file(i:i) == POINT ) exit
    end do

    if ( i > 0 ) then
      ext = trim(adjustl(file(i+1:l)))
    else
      ext = EMPTY_STR
    end if
  end procedure ext_of

  ! Writing Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  module procedure to_file_c128
    character(len=:), allocatable :: ext

    character(len=:), pointer, contiguous :: header_(:)
    character(len=:), pointer             :: locale_, delim_, fmt_, im_, errmsg_
    integer,          pointer             :: decimals_, stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( any(TEXT_EXT == ext) ) then
      select rank(x)
        rank(1); continue
        rank(2); continue
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Cannot write an array of rank "//str(rank(x))//" to text file."
          return
      end select

      if ( .not. present(header) ) then
        header_ => EMPTY_HEADER
      else
        if ( size(header) /= 1 ) then
          select rank(x)
            rank(1)
              stat_ = ARG_ERR
              errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1.'
              return
            rank(2)
              if ( size(header) /= size(x, dim=2) ) then
                stat_ = ARG_ERR
                errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1 or size '// &
                          str(size(x, dim=2))//"."
                return
              end if
          end select
        end if
        header_ => header
      end if

      if ( .not. present(locale) ) then
        locale_ => US_LOCALE
      else
        if ( any(LOCALES == locale) ) then
          locale_ => locale
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid locale "'//locale//'" for file "'//file//'". Locale must be one of: '//join(LOCALES)
          return
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == US_LOCALE ) then
          delim_ => COMMA_DELIMITER
        else
          delim_ => SEMICOLON_DELIM
        end if
      else
        delim_ => delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ => EXP_FMT
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ => fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for file "'//file//'". Format must be one of: '//join(REAL_FMTS)
          return
        end if
      end if

      if ( .not. present(decimals) ) then
        decimals_ => MAX_DECIMALS
      else
        decimals_ => decimals
      end if

      if ( .not. present(im) ) then
        im_ => EMPTY_STRING
      else
        im_ => im
      end if

      select rank(x)
        rank(1); call to_text(x, file, header_, locale_, delim_, fmt_, decimals_, im_, stat_, errmsg_)
        rank(2); call to_text(x, file, header_, locale_, delim_, fmt_, decimals_, im_, stat_, errmsg_)
      end select
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(locale  ) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'
      if ( present(decimals) ) write(*,"(a)") LF//'WARNING: decimals not supported for file type "'//ext//'".'
      if ( present(im      ) ) write(*,"(a)") LF//'WARNING: im not supported for file type "'//ext//'".'

      select rank(x)
        rank(1);  call to_binary(x, file, stat_, errmsg_)
        rank(2);  call to_binary(x, file, stat_, errmsg_)
        rank(3);  call to_binary(x, file, stat_, errmsg_)
        rank(4);  call to_binary(x, file, stat_, errmsg_)
        rank(5);  call to_binary(x, file, stat_, errmsg_)
        rank(6);  call to_binary(x, file, stat_, errmsg_)
        rank(7);  call to_binary(x, file, stat_, errmsg_)
        rank(8);  call to_binary(x, file, stat_, errmsg_)
        rank(9);  call to_binary(x, file, stat_, errmsg_)
        rank(10); call to_binary(x, file, stat_, errmsg_)
        rank(11); call to_binary(x, file, stat_, errmsg_)
        rank(12); call to_binary(x, file, stat_, errmsg_)
        rank(13); call to_binary(x, file, stat_, errmsg_)
        rank(14); call to_binary(x, file, stat_, errmsg_)
        rank(15); call to_binary(x, file, stat_, errmsg_)
        rank default
          stat   = ARG_ERR
          errmsg = "Array has invalid rank. Supported ranks: 1-15."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//SPACE//join(BINARY_EXT)
    end if
  end procedure to_file_c128
  module procedure to_file_c64
    character(len=:), allocatable :: ext

    character(len=:), pointer, contiguous :: header_(:)
    character(len=:), pointer             :: locale_, delim_, fmt_, im_, errmsg_
    integer,          pointer             :: decimals_, stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( any(TEXT_EXT == ext) ) then
      select rank(x)
        rank(1); continue
        rank(2); continue
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Cannot write an array of rank "//str(rank(x))//" to text file."
          return
      end select

      if ( .not. present(header) ) then
        header_ => EMPTY_HEADER
      else
        if ( size(header) /= 1 ) then
          select rank(x)
            rank(1)
              stat_ = ARG_ERR
              errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1.'
              return
            rank(2)
              if ( size(header) /= size(x, dim=2) ) then
                stat_ = ARG_ERR
                errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1 or size '// &
                          str(size(x, dim=2))//"."
                return
              end if
          end select
        end if
        header_ => header
      end if

      if ( .not. present(locale) ) then
        locale_ => US_LOCALE
      else
        if ( any(LOCALES == locale) ) then
          locale_ => locale
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid locale "'//locale//'" for file "'//file//'". Locale must be one of: '//join(LOCALES)
          return
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == US_LOCALE ) then
          delim_ => COMMA_DELIMITER
        else
          delim_ => SEMICOLON_DELIM
        end if
      else
        delim_ => delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ => EXP_FMT
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ => fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for file "'//file//'". Format must be one of: '//join(REAL_FMTS)
          return
        end if
      end if

      if ( .not. present(decimals) ) then
        decimals_ => MAX_DECIMALS
      else
        decimals_ => decimals
      end if

      if ( .not. present(im) ) then
        im_ => EMPTY_STRING
      else
        im_ => im
      end if

      select rank(x)
        rank(1); call to_text(x, file, header_, locale_, delim_, fmt_, decimals_, im_, stat_, errmsg_)
        rank(2); call to_text(x, file, header_, locale_, delim_, fmt_, decimals_, im_, stat_, errmsg_)
      end select
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(locale  ) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'
      if ( present(decimals) ) write(*,"(a)") LF//'WARNING: decimals not supported for file type "'//ext//'".'
      if ( present(im      ) ) write(*,"(a)") LF//'WARNING: im not supported for file type "'//ext//'".'

      select rank(x)
        rank(1);  call to_binary(x, file, stat_, errmsg_)
        rank(2);  call to_binary(x, file, stat_, errmsg_)
        rank(3);  call to_binary(x, file, stat_, errmsg_)
        rank(4);  call to_binary(x, file, stat_, errmsg_)
        rank(5);  call to_binary(x, file, stat_, errmsg_)
        rank(6);  call to_binary(x, file, stat_, errmsg_)
        rank(7);  call to_binary(x, file, stat_, errmsg_)
        rank(8);  call to_binary(x, file, stat_, errmsg_)
        rank(9);  call to_binary(x, file, stat_, errmsg_)
        rank(10); call to_binary(x, file, stat_, errmsg_)
        rank(11); call to_binary(x, file, stat_, errmsg_)
        rank(12); call to_binary(x, file, stat_, errmsg_)
        rank(13); call to_binary(x, file, stat_, errmsg_)
        rank(14); call to_binary(x, file, stat_, errmsg_)
        rank(15); call to_binary(x, file, stat_, errmsg_)
        rank default
          stat   = ARG_ERR
          errmsg = "Array has invalid rank. Supported ranks: 1-15."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//SPACE//join(BINARY_EXT)
    end if
  end procedure to_file_c64
  module procedure to_file_c32
    character(len=:), allocatable :: ext

    character(len=:), pointer, contiguous :: header_(:)
    character(len=:), pointer             :: locale_, delim_, fmt_, im_, errmsg_
    integer,          pointer             :: decimals_, stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( any(TEXT_EXT == ext) ) then
      select rank(x)
        rank(1); continue
        rank(2); continue
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Cannot write an array of rank "//str(rank(x))//" to text file."
          return
      end select

      if ( .not. present(header) ) then
        header_ => EMPTY_HEADER
      else
        if ( size(header) /= 1 ) then
          select rank(x)
            rank(1)
              stat_ = ARG_ERR
              errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1.'
              return
            rank(2)
              if ( size(header) /= size(x, dim=2) ) then
                stat_ = ARG_ERR
                errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1 or size '// &
                          str(size(x, dim=2))//"."
                return
              end if
          end select
        end if
        header_ => header
      end if

      if ( .not. present(locale) ) then
        locale_ => US_LOCALE
      else
        if ( any(LOCALES == locale) ) then
          locale_ => locale
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid locale "'//locale//'" for file "'//file//'". Locale must be one of: '//join(LOCALES)
          return
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == US_LOCALE ) then
          delim_ => COMMA_DELIMITER
        else
          delim_ => SEMICOLON_DELIM
        end if
      else
        delim_ => delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ => EXP_FMT
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ => fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for file "'//file//'". Format must be one of: '//join(REAL_FMTS)
          return
        end if
      end if

      if ( .not. present(decimals) ) then
        decimals_ => MAX_DECIMALS
      else
        decimals_ => decimals
      end if

      if ( .not. present(im) ) then
        im_ => EMPTY_STRING
      else
        im_ => im
      end if

      select rank(x)
        rank(1); call to_text(x, file, header_, locale_, delim_, fmt_, decimals_, im_, stat_, errmsg_)
        rank(2); call to_text(x, file, header_, locale_, delim_, fmt_, decimals_, im_, stat_, errmsg_)
      end select
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(locale  ) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'
      if ( present(decimals) ) write(*,"(a)") LF//'WARNING: decimals not supported for file type "'//ext//'".'
      if ( present(im      ) ) write(*,"(a)") LF//'WARNING: im not supported for file type "'//ext//'".'

      select rank(x)
        rank(1);  call to_binary(x, file, stat_, errmsg_)
        rank(2);  call to_binary(x, file, stat_, errmsg_)
        rank(3);  call to_binary(x, file, stat_, errmsg_)
        rank(4);  call to_binary(x, file, stat_, errmsg_)
        rank(5);  call to_binary(x, file, stat_, errmsg_)
        rank(6);  call to_binary(x, file, stat_, errmsg_)
        rank(7);  call to_binary(x, file, stat_, errmsg_)
        rank(8);  call to_binary(x, file, stat_, errmsg_)
        rank(9);  call to_binary(x, file, stat_, errmsg_)
        rank(10); call to_binary(x, file, stat_, errmsg_)
        rank(11); call to_binary(x, file, stat_, errmsg_)
        rank(12); call to_binary(x, file, stat_, errmsg_)
        rank(13); call to_binary(x, file, stat_, errmsg_)
        rank(14); call to_binary(x, file, stat_, errmsg_)
        rank(15); call to_binary(x, file, stat_, errmsg_)
        rank default
          stat   = ARG_ERR
          errmsg = "Array has invalid rank. Supported ranks: 1-15."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//SPACE//join(BINARY_EXT)
    end if
  end procedure to_file_c32

  module procedure to_file_r128
    character(len=:), allocatable :: ext

    character(len=:), pointer, contiguous :: header_(:)
    character(len=:), pointer             :: locale_, delim_, fmt_, errmsg_
    integer,          pointer             :: decimals_, stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( any(TEXT_EXT == ext) ) then
      select rank(x)
        rank(1); continue
        rank(2); continue
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Cannot write an array of rank "//str(rank(x))//" to text file."
          return
      end select

      if ( .not. present(header) ) then
        header_ => EMPTY_HEADER
      else
        if ( size(header) /= 1 ) then
          select rank(x)
            rank(1)
              stat_ = ARG_ERR
              errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1.'
              return
            rank(2)
              if ( size(header) /= size(x, dim=2) ) then
                stat_ = ARG_ERR
                errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1 or size '// &
                          str(size(x, dim=2))//"."
                return
              end if
          end select
        end if
        header_ => header
      end if

      if ( .not. present(locale) ) then
        locale_ => US_LOCALE
      else
        if ( any(LOCALES == locale) ) then
          locale_ => locale
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid locale "'//locale//'" for file "'//file//'". Locale must be one of: '//join(LOCALES)
          return
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == US_LOCALE ) then
          delim_ => COMMA_DELIMITER
        else
          delim_ => SEMICOLON_DELIM
        end if
      else
        delim_ => delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ => EXP_FMT
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ => fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for file "'//file//'". Format must be one of: '//join(REAL_FMTS)
          return
        end if
      end if

      if ( .not. present(decimals) ) then
        decimals_ => MAX_DECIMALS
      else
        decimals_ => decimals
      end if

      select rank(x)
        rank(1); call to_text(x, file, header_, locale_, delim_, fmt_, decimals_, stat_, errmsg_)
        rank(2); call to_text(x, file, header_, locale_, delim_, fmt_, decimals_, stat_, errmsg_)
      end select
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(locale  ) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'
      if ( present(decimals) ) write(*,"(a)") LF//'WARNING: decimals not supported for file type "'//ext//'".'

      select rank(x)
        rank(1);  call to_binary(x, file, stat_, errmsg_)
        rank(2);  call to_binary(x, file, stat_, errmsg_)
        rank(3);  call to_binary(x, file, stat_, errmsg_)
        rank(4);  call to_binary(x, file, stat_, errmsg_)
        rank(5);  call to_binary(x, file, stat_, errmsg_)
        rank(6);  call to_binary(x, file, stat_, errmsg_)
        rank(7);  call to_binary(x, file, stat_, errmsg_)
        rank(8);  call to_binary(x, file, stat_, errmsg_)
        rank(9);  call to_binary(x, file, stat_, errmsg_)
        rank(10); call to_binary(x, file, stat_, errmsg_)
        rank(11); call to_binary(x, file, stat_, errmsg_)
        rank(12); call to_binary(x, file, stat_, errmsg_)
        rank(13); call to_binary(x, file, stat_, errmsg_)
        rank(14); call to_binary(x, file, stat_, errmsg_)
        rank(15); call to_binary(x, file, stat_, errmsg_)
        rank default
          stat   = ARG_ERR
          errmsg = "Array has invalid rank. Supported ranks: 1-15."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//SPACE//join(BINARY_EXT)
    end if
  end procedure to_file_r128
  module procedure to_file_r64
    character(len=:), allocatable :: ext

    character(len=:), pointer, contiguous :: header_(:)
    character(len=:), pointer             :: locale_, delim_, fmt_, errmsg_
    integer,          pointer             :: decimals_, stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( any(TEXT_EXT == ext) ) then
      select rank(x)
        rank(1); continue
        rank(2); continue
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Cannot write an array of rank "//str(rank(x))//" to text file."
          return
      end select

      if ( .not. present(header) ) then
        header_ => EMPTY_HEADER
      else
        if ( size(header) /= 1 ) then
          select rank(x)
            rank(1)
              stat_ = ARG_ERR
              errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1.'
              return
            rank(2)
              if ( size(header) /= size(x, dim=2) ) then
                stat_ = ARG_ERR
                errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1 or size '// &
                          str(size(x, dim=2))//"."
                return
              end if
          end select
        end if
        header_ => header
      end if

      if ( .not. present(locale) ) then
        locale_ => US_LOCALE
      else
        if ( any(LOCALES == locale) ) then
          locale_ => locale
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid locale "'//locale//'" for file "'//file//'". Locale must be one of: '//join(LOCALES)
          return
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == US_LOCALE ) then
          delim_ => COMMA_DELIMITER
        else
          delim_ => SEMICOLON_DELIM
        end if
      else
        delim_ => delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ => EXP_FMT
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ => fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for file "'//file//'". Format must be one of: '//join(REAL_FMTS)
          return
        end if
      end if

      if ( .not. present(decimals) ) then
        decimals_ => MAX_DECIMALS
      else
        decimals_ => decimals
      end if

      select rank(x)
        rank(1); call to_text(x, file, header_, locale_, delim_, fmt_, decimals_, stat_, errmsg_)
        rank(2); call to_text(x, file, header_, locale_, delim_, fmt_, decimals_, stat_, errmsg_)
      end select
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(locale  ) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'
      if ( present(decimals) ) write(*,"(a)") LF//'WARNING: decimals not supported for file type "'//ext//'".'

      select rank(x)
        rank(1);  call to_binary(x, file, stat_, errmsg_)
        rank(2);  call to_binary(x, file, stat_, errmsg_)
        rank(3);  call to_binary(x, file, stat_, errmsg_)
        rank(4);  call to_binary(x, file, stat_, errmsg_)
        rank(5);  call to_binary(x, file, stat_, errmsg_)
        rank(6);  call to_binary(x, file, stat_, errmsg_)
        rank(7);  call to_binary(x, file, stat_, errmsg_)
        rank(8);  call to_binary(x, file, stat_, errmsg_)
        rank(9);  call to_binary(x, file, stat_, errmsg_)
        rank(10); call to_binary(x, file, stat_, errmsg_)
        rank(11); call to_binary(x, file, stat_, errmsg_)
        rank(12); call to_binary(x, file, stat_, errmsg_)
        rank(13); call to_binary(x, file, stat_, errmsg_)
        rank(14); call to_binary(x, file, stat_, errmsg_)
        rank(15); call to_binary(x, file, stat_, errmsg_)
        rank default
          stat   = ARG_ERR
          errmsg = "Array has invalid rank. Supported ranks: 1-15."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//SPACE//join(BINARY_EXT)
    end if
  end procedure to_file_r64
  module procedure to_file_r32
    character(len=:), allocatable :: ext

    character(len=:), pointer, contiguous :: header_(:)
    character(len=:), pointer             :: locale_, delim_, fmt_, errmsg_
    integer,          pointer             :: decimals_, stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( any(TEXT_EXT == ext) ) then
      select rank(x)
        rank(1); continue
        rank(2); continue
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Cannot write an array of rank "//str(rank(x))//" to text file."
          return
      end select

      if ( .not. present(header) ) then
        header_ => EMPTY_HEADER
      else
        if ( size(header) /= 1 ) then
          select rank(x)
            rank(1)
              stat_ = ARG_ERR
              errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1.'
              return
            rank(2)
              if ( size(header) /= size(x, dim=2) ) then
                stat_ = ARG_ERR
                errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1 or size '// &
                          str(size(x, dim=2))//"."
                return
              end if
          end select
        end if
        header_ => header
      end if

      if ( .not. present(locale) ) then
        locale_ => US_LOCALE
      else
        if ( any(LOCALES == locale) ) then
          locale_ => locale
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid locale "'//locale//'" for file "'//file//'". Locale must be one of: '//join(LOCALES)
          return
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == US_LOCALE ) then
          delim_ => COMMA_DELIMITER
        else
          delim_ => SEMICOLON_DELIM
        end if
      else
        delim_ => delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ => EXP_FMT
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ => fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for file "'//file//'". Format must be one of: '//join(REAL_FMTS)
          return
        end if
      end if

      if ( .not. present(decimals) ) then
        decimals_ => MAX_DECIMALS
      else
        decimals_ => decimals
      end if

      select rank(x)
        rank(1); call to_text(x, file, header_, locale_, delim_, fmt_, decimals_, stat_, errmsg_)
        rank(2); call to_text(x, file, header_, locale_, delim_, fmt_, decimals_, stat_, errmsg_)
      end select
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(locale  ) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'
      if ( present(decimals) ) write(*,"(a)") LF//'WARNING: decimals not supported for file type "'//ext//'".'

      select rank(x)
        rank(1);  call to_binary(x, file, stat_, errmsg_)
        rank(2);  call to_binary(x, file, stat_, errmsg_)
        rank(3);  call to_binary(x, file, stat_, errmsg_)
        rank(4);  call to_binary(x, file, stat_, errmsg_)
        rank(5);  call to_binary(x, file, stat_, errmsg_)
        rank(6);  call to_binary(x, file, stat_, errmsg_)
        rank(7);  call to_binary(x, file, stat_, errmsg_)
        rank(8);  call to_binary(x, file, stat_, errmsg_)
        rank(9);  call to_binary(x, file, stat_, errmsg_)
        rank(10); call to_binary(x, file, stat_, errmsg_)
        rank(11); call to_binary(x, file, stat_, errmsg_)
        rank(12); call to_binary(x, file, stat_, errmsg_)
        rank(13); call to_binary(x, file, stat_, errmsg_)
        rank(14); call to_binary(x, file, stat_, errmsg_)
        rank(15); call to_binary(x, file, stat_, errmsg_)
        rank default
          stat   = ARG_ERR
          errmsg = "Array has invalid rank. Supported ranks: 1-15."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//SPACE//join(BINARY_EXT)
    end if
  end procedure to_file_r32

  module procedure to_file_i64
    character(len=:), allocatable :: ext

    character(len=:), pointer, contiguous :: header_(:)
    character(len=:), pointer             :: delim_, fmt_, errmsg_
    integer,          pointer             :: stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( any(TEXT_EXT == ext) ) then
      select rank(x)
        rank(1); continue
        rank(2); continue
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Cannot write an array of rank "//str(rank(x))//" to text file."
          return
      end select

      if ( .not. present(header) ) then
        header_ => EMPTY_HEADER
      else
        if ( size(header) /= 1 ) then
          select rank(x)
            rank(1)
              stat_ = ARG_ERR
              errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1.'
              return
            rank(2)
              if ( size(header) /= size(x, dim=2) ) then
                stat_ = ARG_ERR
                errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1 or size '// &
                          str(size(x, dim=2))//"."
                return
              end if
          end select
        end if
        header_ => header
      end if

      if ( .not. present(delim) ) then
        delim_ => COMMA_DELIMITER
      else
        delim_ => delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ => INT_FMT
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ => fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for file "'//file//'". Format must be one of: '//join(INT_FMTS)
          return
        end if
      end if

      select rank(x)
        rank(1); call to_text(x, file, header_, delim_, fmt_, stat_, errmsg_)
        rank(2); call to_text(x, file, header_, delim_, fmt_, stat_, errmsg_)
      end select
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'

      select rank(x)
        rank(1);  call to_binary(x, file, stat_, errmsg_)
        rank(2);  call to_binary(x, file, stat_, errmsg_)
        rank(3);  call to_binary(x, file, stat_, errmsg_)
        rank(4);  call to_binary(x, file, stat_, errmsg_)
        rank(5);  call to_binary(x, file, stat_, errmsg_)
        rank(6);  call to_binary(x, file, stat_, errmsg_)
        rank(7);  call to_binary(x, file, stat_, errmsg_)
        rank(8);  call to_binary(x, file, stat_, errmsg_)
        rank(9);  call to_binary(x, file, stat_, errmsg_)
        rank(10); call to_binary(x, file, stat_, errmsg_)
        rank(11); call to_binary(x, file, stat_, errmsg_)
        rank(12); call to_binary(x, file, stat_, errmsg_)
        rank(13); call to_binary(x, file, stat_, errmsg_)
        rank(14); call to_binary(x, file, stat_, errmsg_)
        rank(15); call to_binary(x, file, stat_, errmsg_)
        rank default
          stat   = ARG_ERR
          errmsg = "Array has invalid rank. Supported ranks: 1-15."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//SPACE//join(BINARY_EXT)
    end if
  end procedure to_file_i64
  module procedure to_file_i32
    character(len=:), allocatable :: ext

    character(len=:), pointer, contiguous :: header_(:)
    character(len=:), pointer             :: delim_, fmt_, errmsg_
    integer,          pointer             :: stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( any(TEXT_EXT == ext) ) then
      select rank(x)
        rank(1); continue
        rank(2); continue
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Cannot write an array of rank "//str(rank(x))//" to text file."
          return
      end select

      if ( .not. present(header) ) then
        header_ => EMPTY_HEADER
      else
        if ( size(header) /= 1 ) then
          select rank(x)
            rank(1)
              stat_ = ARG_ERR
              errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1.'
              return
            rank(2)
              if ( size(header) /= size(x, dim=2) ) then
                stat_ = ARG_ERR
                errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1 or size '// &
                          str(size(x, dim=2))//"."
                return
              end if
          end select
        end if
        header_ => header
      end if

      if ( .not. present(delim) ) then
        delim_ => COMMA_DELIMITER
      else
        delim_ => delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ => INT_FMT
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ => fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for file "'//file//'". Format must be one of: '//join(INT_FMTS)
          return
        end if
      end if

      select rank(x)
        rank(1); call to_text(x, file, header_, delim_, fmt_, stat_, errmsg_)
        rank(2); call to_text(x, file, header_, delim_, fmt_, stat_, errmsg_)
      end select
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'

      select rank(x)
        rank(1);  call to_binary(x, file, stat_, errmsg_)
        rank(2);  call to_binary(x, file, stat_, errmsg_)
        rank(3);  call to_binary(x, file, stat_, errmsg_)
        rank(4);  call to_binary(x, file, stat_, errmsg_)
        rank(5);  call to_binary(x, file, stat_, errmsg_)
        rank(6);  call to_binary(x, file, stat_, errmsg_)
        rank(7);  call to_binary(x, file, stat_, errmsg_)
        rank(8);  call to_binary(x, file, stat_, errmsg_)
        rank(9);  call to_binary(x, file, stat_, errmsg_)
        rank(10); call to_binary(x, file, stat_, errmsg_)
        rank(11); call to_binary(x, file, stat_, errmsg_)
        rank(12); call to_binary(x, file, stat_, errmsg_)
        rank(13); call to_binary(x, file, stat_, errmsg_)
        rank(14); call to_binary(x, file, stat_, errmsg_)
        rank(15); call to_binary(x, file, stat_, errmsg_)
        rank default
          stat   = ARG_ERR
          errmsg = "Array has invalid rank. Supported ranks: 1-15."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//SPACE//join(BINARY_EXT)
    end if
  end procedure to_file_i32
  module procedure to_file_i16
    character(len=:), allocatable :: ext

    character(len=:), pointer, contiguous :: header_(:)
    character(len=:), pointer             :: delim_, fmt_, errmsg_
    integer,          pointer             :: stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( any(TEXT_EXT == ext) ) then
      select rank(x)
        rank(1); continue
        rank(2); continue
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Cannot write an array of rank "//str(rank(x))//" to text file."
          return
      end select

      if ( .not. present(header) ) then
        header_ => EMPTY_HEADER
      else
        if ( size(header) /= 1 ) then
          select rank(x)
            rank(1)
              stat_ = ARG_ERR
              errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1.'
              return
            rank(2)
              if ( size(header) /= size(x, dim=2) ) then
                stat_ = ARG_ERR
                errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1 or size '// &
                          str(size(x, dim=2))//"."
                return
              end if
          end select
        end if
        header_ => header
      end if

      if ( .not. present(delim) ) then
        delim_ => COMMA_DELIMITER
      else
        delim_ => delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ => INT_FMT
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ => fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for file "'//file//'". Format must be one of: '//join(INT_FMTS)
          return
        end if
      end if

      select rank(x)
        rank(1); call to_text(x, file, header_, delim_, fmt_, stat_, errmsg_)
        rank(2); call to_text(x, file, header_, delim_, fmt_, stat_, errmsg_)
      end select
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'

      select rank(x)
        rank(1);  call to_binary(x, file, stat_, errmsg_)
        rank(2);  call to_binary(x, file, stat_, errmsg_)
        rank(3);  call to_binary(x, file, stat_, errmsg_)
        rank(4);  call to_binary(x, file, stat_, errmsg_)
        rank(5);  call to_binary(x, file, stat_, errmsg_)
        rank(6);  call to_binary(x, file, stat_, errmsg_)
        rank(7);  call to_binary(x, file, stat_, errmsg_)
        rank(8);  call to_binary(x, file, stat_, errmsg_)
        rank(9);  call to_binary(x, file, stat_, errmsg_)
        rank(10); call to_binary(x, file, stat_, errmsg_)
        rank(11); call to_binary(x, file, stat_, errmsg_)
        rank(12); call to_binary(x, file, stat_, errmsg_)
        rank(13); call to_binary(x, file, stat_, errmsg_)
        rank(14); call to_binary(x, file, stat_, errmsg_)
        rank(15); call to_binary(x, file, stat_, errmsg_)
        rank default
          stat   = ARG_ERR
          errmsg = "Array has invalid rank. Supported ranks: 1-15."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//SPACE//join(BINARY_EXT)
    end if
  end procedure to_file_i16
  module procedure to_file_i8
    character(len=:), allocatable :: ext

    character(len=:), pointer, contiguous :: header_(:)
    character(len=:), pointer             :: delim_, fmt_, errmsg_
    integer,          pointer             :: stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( any(TEXT_EXT == ext) ) then
      select rank(x)
        rank(1); continue
        rank(2); continue
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Cannot write an array of rank "//str(rank(x))//" to text file."
          return
      end select

      if ( .not. present(header) ) then
        header_ => EMPTY_HEADER
      else
        if ( size(header) /= 1 ) then
          select rank(x)
            rank(1)
              stat_ = ARG_ERR
              errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1.'
              return
            rank(2)
              if ( size(header) /= size(x, dim=2) ) then
                stat_ = ARG_ERR
                errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1 or size '// &
                          str(size(x, dim=2))//"."
                return
              end if
          end select
        end if
        header_ => header
      end if

      if ( .not. present(delim) ) then
        delim_ => COMMA_DELIMITER
      else
        delim_ => delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ => INT_FMT
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ => fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for file "'//file//'". Format must be one of: '//join(INT_FMTS)
          return
        end if
      end if

      select rank(x)
        rank(1); call to_text(x, file, header_, delim_, fmt_, stat_, errmsg_)
        rank(2); call to_text(x, file, header_, delim_, fmt_, stat_, errmsg_)
      end select
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'

      select rank(x)
        rank(1);  call to_binary(x, file, stat_, errmsg_)
        rank(2);  call to_binary(x, file, stat_, errmsg_)
        rank(3);  call to_binary(x, file, stat_, errmsg_)
        rank(4);  call to_binary(x, file, stat_, errmsg_)
        rank(5);  call to_binary(x, file, stat_, errmsg_)
        rank(6);  call to_binary(x, file, stat_, errmsg_)
        rank(7);  call to_binary(x, file, stat_, errmsg_)
        rank(8);  call to_binary(x, file, stat_, errmsg_)
        rank(9);  call to_binary(x, file, stat_, errmsg_)
        rank(10); call to_binary(x, file, stat_, errmsg_)
        rank(11); call to_binary(x, file, stat_, errmsg_)
        rank(12); call to_binary(x, file, stat_, errmsg_)
        rank(13); call to_binary(x, file, stat_, errmsg_)
        rank(14); call to_binary(x, file, stat_, errmsg_)
        rank(15); call to_binary(x, file, stat_, errmsg_)
        rank default
          stat   = ARG_ERR
          errmsg = "Array has invalid rank. Supported ranks: 1-15."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//SPACE//join(BINARY_EXT)
    end if
  end procedure to_file_i8

  ! Reading Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  module procedure from_textfile_c128
    character(len=:), allocatable :: ext

    character(len=:), pointer :: locale_, delim_, fmt_, im_, errmsg_
    logical,          pointer :: header_
    integer,          pointer :: stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ => ABSENT_HEADER
      else
        header_ => header
      end if

      if ( .not. present(locale) ) then
        locale_ => US_LOCALE
      else
        if ( any(LOCALES == locale) ) then
          locale_ => locale
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid locale "'//locale//'" for file "'//file//'". Locale must be one of: '//join(LOCALES)
          return
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == US_LOCALE ) then
          delim_ => COMMA_DELIMITER
        else
          delim_ => SEMICOLON_DELIM
        end if
      else
        delim_ => delim
        if ( locale_ == US_LOCALE ) then
          if ( delim_ == POINT ) then
            stat_   = ARG_ERR
            errmsg_ = "Invalid delimiter for numbers having US style decimal."
            return
          end if
        else
          if ( delim_ == COMMA ) then
            stat_   = ARG_ERR
            errmsg_ = "Invalid delimiter for numbers having EU style decimal."
            return
          end if
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ => EXP_FMT
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ => fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                    "into complex array. Format must be one of: "//join(REAL_FMTS)
          return
        end if
      end if

      if ( .not. present(im) ) then
        im_ => EMPTY_STRING
      else
        im_ => im
      end if

      select rank(into)
        rank(1)
          call from_text(file, into, header_, locale_, delim_, fmt_, im_, stat_, errmsg_)
        rank(2)
          call from_text(file, into, header_, locale_, delim_, fmt_, im_, stat_, errmsg_)
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-2."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)
    end if
  end procedure from_textfile_c128
  module procedure from_textfile_c64
    character(len=:), allocatable :: ext

    character(len=:), pointer :: locale_, delim_, fmt_, im_, errmsg_
    logical,          pointer :: header_
    integer,          pointer :: stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ => ABSENT_HEADER
      else
        header_ => header
      end if

      if ( .not. present(locale) ) then
        locale_ => US_LOCALE
      else
        if ( any(LOCALES == locale) ) then
          locale_ => locale
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid locale "'//locale//'" for file "'//file//'". Locale must be one of: '//join(LOCALES)
          return
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == US_LOCALE ) then
          delim_ => COMMA_DELIMITER
        else
          delim_ => SEMICOLON_DELIM
        end if
      else
        delim_ => delim
        if ( locale_ == US_LOCALE ) then
          if ( delim_ == POINT ) then
            stat_   = ARG_ERR
            errmsg_ = "Invalid delimiter for numbers having US style decimal."
            return
          end if
        else
          if ( delim_ == COMMA ) then
            stat_   = ARG_ERR
            errmsg_ = "Invalid delimiter for numbers having EU style decimal."
            return
          end if
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ => EXP_FMT
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ => fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                    "into complex array. Format must be one of: "//join(REAL_FMTS)
          return
        end if
      end if

      if ( .not. present(im) ) then
        im_ => EMPTY_STRING
      else
        im_ => im
      end if

      select rank(into)
        rank(1)
          call from_text(file, into, header_, locale_, delim_, fmt_, im_, stat_, errmsg_)
        rank(2)
          call from_text(file, into, header_, locale_, delim_, fmt_, im_, stat_, errmsg_)
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-2."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)
    end if
  end procedure from_textfile_c64
  module procedure from_textfile_c32
    character(len=:), allocatable :: ext

    character(len=:), pointer :: locale_, delim_, fmt_, im_, errmsg_
    logical,          pointer :: header_
    integer,          pointer :: stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ => ABSENT_HEADER
      else
        header_ => header
      end if

      if ( .not. present(locale) ) then
        locale_ => US_LOCALE
      else
        if ( any(LOCALES == locale) ) then
          locale_ => locale
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid locale "'//locale//'" for file "'//file//'". Locale must be one of: '//join(LOCALES)
          return
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == US_LOCALE ) then
          delim_ => COMMA_DELIMITER
        else
          delim_ => SEMICOLON_DELIM
        end if
      else
        delim_ => delim
        if ( locale_ == US_LOCALE ) then
          if ( delim_ == POINT ) then
            stat_   = ARG_ERR
            errmsg_ = "Invalid delimiter for numbers having US style decimal."
            return
          end if
        else
          if ( delim_ == COMMA ) then
            stat_   = ARG_ERR
            errmsg_ = "Invalid delimiter for numbers having EU style decimal."
            return
          end if
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ => EXP_FMT
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ => fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                    "into complex array. Format must be one of: "//join(REAL_FMTS)
          return
        end if
      end if

      if ( .not. present(im) ) then
        im_ => EMPTY_STRING
      else
        im_ => im
      end if

      select rank(into)
        rank(1)
          call from_text(file, into, header_, locale_, delim_, fmt_, im_, stat_, errmsg_)
        rank(2)
          call from_text(file, into, header_, locale_, delim_, fmt_, im_, stat_, errmsg_)
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-2."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)
    end if
  end procedure from_textfile_c32

  module procedure from_binaryfile_c128
    character(len=:), allocatable :: ext

    character(len=:), pointer :: errmsg_
    integer,          pointer :: stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( size(data_shape) /= rank(into) ) then
      stat_   = ARG_ERR
      errmsg_ = "RANK/SHAPE mismatch: size of data_shape must match rank of output array."
      return
    end if

    if ( any(BINARY_EXT == ext) ) then
      select rank(into)
        rank(1)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(2)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(3)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(4)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(5)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(6)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(7)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(8)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(9)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(10)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(11)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(12)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(13)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(14)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(15)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank default
          stat   = ARG_ERR
          errmsg = "Array has invalid rank. Supported ranks: 1-15."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Error reading file "'//file//'" with extension "'//ext//'". '// &
                "If attempting to read a text file, the data_shape argument must not be specified."// &
                "If attempting to read a binary file, the supported file extensions are: "//join(BINARY_EXT)
      return
    end if
  end procedure from_binaryfile_c128
  module procedure from_binaryfile_c64
    character(len=:), allocatable :: ext

    character(len=:), pointer :: errmsg_
    integer,          pointer :: stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( size(data_shape) /= rank(into) ) then
      stat_   = ARG_ERR
      errmsg_ = "RANK/SHAPE mismatch: size of data_shape must match rank of output array."
      return
    end if

    if ( any(BINARY_EXT == ext) ) then
      select rank(into)
        rank(1)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(2)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(3)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(4)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(5)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(6)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(7)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(8)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(9)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(10)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(11)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(12)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(13)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(14)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(15)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank default
          stat   = ARG_ERR
          errmsg = "Array has invalid rank. Supported ranks: 1-15."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Error reading file "'//file//'" with extension "'//ext//'". '// &
                "If attempting to read a text file, the data_shape argument must not be specified."// &
                "If attempting to read a binary file, the supported file extensions are: "//join(BINARY_EXT)
      return
    end if
  end procedure from_binaryfile_c64
  module procedure from_binaryfile_c32
    character(len=:), allocatable :: ext

    character(len=:), pointer :: errmsg_
    integer,          pointer :: stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( size(data_shape) /= rank(into) ) then
      stat_   = ARG_ERR
      errmsg_ = "RANK/SHAPE mismatch: size of data_shape must match rank of output array."
      return
    end if

    if ( any(BINARY_EXT == ext) ) then
      select rank(into)
        rank(1)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(2)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(3)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(4)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(5)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(6)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(7)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(8)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(9)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(10)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(11)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(12)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(13)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(14)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(15)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank default
          stat   = ARG_ERR
          errmsg = "Array has invalid rank. Supported ranks: 1-15."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Error reading file "'//file//'" with extension "'//ext//'". '// &
                "If attempting to read a text file, the data_shape argument must not be specified."// &
                "If attempting to read a binary file, the supported file extensions are: "//join(BINARY_EXT)
      return
    end if
  end procedure from_binaryfile_c32

  module procedure from_textfile_r128
    character(len=:), allocatable :: ext

    character(len=:), pointer :: locale_, delim_, fmt_, errmsg_
    logical,          pointer :: header_
    integer,          pointer :: stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ => ABSENT_HEADER
      else
        header_ => header
      end if

      if ( .not. present(locale) ) then
        locale_ => US_LOCALE
      else
        if ( any(LOCALES == locale) ) then
          locale_ => locale
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid locale "'//locale//'" for file "'//file//'". Locale must be one of: '//join(LOCALES)
          return
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == US_LOCALE ) then
          delim_ => COMMA_DELIMITER
        else
          delim_ => SEMICOLON_DELIM
        end if
      else
        delim_ => delim
        if ( locale_ == US_LOCALE ) then
          if ( delim_ == POINT ) then
            stat_   = ARG_ERR
            errmsg_ = "Invalid delimiter for numbers having US style decimal."
            return
          end if
        else
          if ( delim_ == COMMA ) then
            stat_   = ARG_ERR
            errmsg_ = "Invalid delimiter for numbers having EU style decimal."
            return
          end if
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ => EXP_FMT
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ => fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                    "into real array. Format must be one of: "//join(REAL_FMTS)
          return
        end if
      end if

      select rank(into)
        rank(1)
          call from_text(file, into, header_, locale_, delim_, fmt_, stat_, errmsg_)
        rank(2)
          call from_text(file, into, header_, locale_, delim_, fmt_, stat_, errmsg_)
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-2."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)
    end if
  end procedure from_textfile_r128
  module procedure from_textfile_r64
    character(len=:), allocatable :: ext

    character(len=:), pointer :: locale_, delim_, fmt_, errmsg_
    logical,          pointer :: header_
    integer,          pointer :: stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ => ABSENT_HEADER
      else
        header_ => header
      end if

      if ( .not. present(locale) ) then
        locale_ => US_LOCALE
      else
        if ( any(LOCALES == locale) ) then
          locale_ => locale
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid locale "'//locale//'" for file "'//file//'". Locale must be one of: '//join(LOCALES)
          return
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == US_LOCALE ) then
          delim_ => COMMA_DELIMITER
        else
          delim_ => SEMICOLON_DELIM
        end if
      else
        delim_ => delim
        if ( locale_ == US_LOCALE ) then
          if ( delim_ == POINT ) then
            stat_   = ARG_ERR
            errmsg_ = "Invalid delimiter for numbers having US style decimal."
            return
          end if
        else
          if ( delim_ == COMMA ) then
            stat_   = ARG_ERR
            errmsg_ = "Invalid delimiter for numbers having EU style decimal."
            return
          end if
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ => EXP_FMT
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ => fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                    "into real array. Format must be one of: "//join(REAL_FMTS)
          return
        end if
      end if

      select rank(into)
        rank(1)
          call from_text(file, into, header_, locale_, delim_, fmt_, stat_, errmsg_)
        rank(2)
          call from_text(file, into, header_, locale_, delim_, fmt_, stat_, errmsg_)
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-2."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)
    end if
  end procedure from_textfile_r64
  module procedure from_textfile_r32
    character(len=:), allocatable :: ext

    character(len=:), pointer :: locale_, delim_, fmt_, errmsg_
    logical,          pointer :: header_
    integer,          pointer :: stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ => ABSENT_HEADER
      else
        header_ => header
      end if

      if ( .not. present(locale) ) then
        locale_ => US_LOCALE
      else
        if ( any(LOCALES == locale) ) then
          locale_ => locale
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid locale "'//locale//'" for file "'//file//'". Locale must be one of: '//join(LOCALES)
          return
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == US_LOCALE ) then
          delim_ => COMMA_DELIMITER
        else
          delim_ => SEMICOLON_DELIM
        end if
      else
        delim_ => delim
        if ( locale_ == US_LOCALE ) then
          if ( delim_ == POINT ) then
            stat_   = ARG_ERR
            errmsg_ = "Invalid delimiter for numbers having US style decimal."
            return
          end if
        else
          if ( delim_ == COMMA ) then
            stat_   = ARG_ERR
            errmsg_ = "Invalid delimiter for numbers having EU style decimal."
            return
          end if
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ => EXP_FMT
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ => fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                    "into real array. Format must be one of: "//join(REAL_FMTS)
          return
        end if
      end if

      select rank(into)
        rank(1)
          call from_text(file, into, header_, locale_, delim_, fmt_, stat_, errmsg_)
        rank(2)
          call from_text(file, into, header_, locale_, delim_, fmt_, stat_, errmsg_)
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-2."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)
    end if
  end procedure from_textfile_r32

  module procedure from_binaryfile_r128
    character(len=:), allocatable :: ext

    character(len=:), pointer :: errmsg_
    integer,          pointer :: stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( size(data_shape) /= rank(into) ) then
      stat_   = ARG_ERR
      errmsg_ = "RANK/SHAPE mismatch: size of data_shape must match rank of output array."
      return
    end if

    if ( any(BINARY_EXT == ext) ) then
      select rank(into)
        rank(1)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(2)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(3)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(4)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(5)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(6)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(7)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(8)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(9)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(10)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(11)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(12)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(13)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(14)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(15)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank default
          stat   = ARG_ERR
          errmsg = "Array has invalid rank. Supported ranks: 1-15."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Error reading file "'//file//'" with extension "'//ext//'". '// &
                "If attempting to read a text file, the data_shape argument must not be specified."// &
                "If attempting to read a binary file, the supported file extensions are: "//join(BINARY_EXT)
      return
    end if
  end procedure from_binaryfile_r128
  module procedure from_binaryfile_r64
    character(len=:), allocatable :: ext

    character(len=:), pointer :: errmsg_
    integer,          pointer :: stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( size(data_shape) /= rank(into) ) then
      stat_   = ARG_ERR
      errmsg_ = "RANK/SHAPE mismatch: size of data_shape must match rank of output array."
      return
    end if

    if ( any(BINARY_EXT == ext) ) then
      select rank(into)
        rank(1)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(2)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(3)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(4)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(5)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(6)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(7)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(8)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(9)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(10)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(11)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(12)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(13)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(14)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(15)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank default
          stat   = ARG_ERR
          errmsg = "Array has invalid rank. Supported ranks: 1-15."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Error reading file "'//file//'" with extension "'//ext//'". '// &
                "If attempting to read a text file, the data_shape argument must not be specified."// &
                "If attempting to read a binary file, the supported file extensions are: "//join(BINARY_EXT)
      return
    end if
  end procedure from_binaryfile_r64
  module procedure from_binaryfile_r32
    character(len=:), allocatable :: ext

    character(len=:), pointer :: errmsg_
    integer,          pointer :: stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( size(data_shape) /= rank(into) ) then
      stat_   = ARG_ERR
      errmsg_ = "RANK/SHAPE mismatch: size of data_shape must match rank of output array."
      return
    end if

    if ( any(BINARY_EXT == ext) ) then
      select rank(into)
        rank(1)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(2)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(3)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(4)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(5)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(6)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(7)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(8)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(9)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(10)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(11)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(12)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(13)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(14)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(15)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank default
          stat   = ARG_ERR
          errmsg = "Array has invalid rank. Supported ranks: 1-15."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Error reading file "'//file//'" with extension "'//ext//'". '// &
                "If attempting to read a text file, the data_shape argument must not be specified."// &
                "If attempting to read a binary file, the supported file extensions are: "//join(BINARY_EXT)
      return
    end if
  end procedure from_binaryfile_r32

  module procedure from_textfile_i64
    character(len=:), allocatable :: ext

    character(len=:), pointer :: delim_, fmt_, errmsg_
    logical,          pointer :: header_
    integer,          pointer :: stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ => ABSENT_HEADER
      else
        header_ => header
      end if

      if ( .not. present(delim) ) then
        delim_ => COMMA_DELIMITER
      else
        delim_ => delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ => INT_FMT
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ => fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                    "into integer array. Format must be one of: "//join(INT_FMTS)
          return
        end if
      end if

      select rank(into)
        rank(1)
          call from_text(file, into, header_, delim_, fmt_, stat_, errmsg_)
        rank(2)
          call from_text(file, into, header_, delim_, fmt_, stat_, errmsg_)
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-2."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)
    end if
  end procedure from_textfile_i64
  module procedure from_textfile_i32
    character(len=:), allocatable :: ext

    character(len=:), pointer :: delim_, fmt_, errmsg_
    logical,          pointer :: header_
    integer,          pointer :: stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ => ABSENT_HEADER
      else
        header_ => header
      end if

      if ( .not. present(delim) ) then
        delim_ => COMMA_DELIMITER
      else
        delim_ => delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ => INT_FMT
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ => fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                    "into integer array. Format must be one of: "//join(INT_FMTS)
          return
        end if
      end if

      select rank(into)
        rank(1)
          call from_text(file, into, header_, delim_, fmt_, stat_, errmsg_)
        rank(2)
          call from_text(file, into, header_, delim_, fmt_, stat_, errmsg_)
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-2."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)
    end if
  end procedure from_textfile_i32
  module procedure from_textfile_i16
    character(len=:), allocatable :: ext

    character(len=:), pointer :: delim_, fmt_, errmsg_
    logical,          pointer :: header_
    integer,          pointer :: stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ => ABSENT_HEADER
      else
        header_ => header
      end if

      if ( .not. present(delim) ) then
        delim_ => COMMA_DELIMITER
      else
        delim_ => delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ => INT_FMT
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ => fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                    "into integer array. Format must be one of: "//join(INT_FMTS)
          return
        end if
      end if

      select rank(into)
        rank(1)
          call from_text(file, into, header_, delim_, fmt_, stat_, errmsg_)
        rank(2)
          call from_text(file, into, header_, delim_, fmt_, stat_, errmsg_)
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-2."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)
    end if
  end procedure from_textfile_i16
  module procedure from_textfile_i8
    character(len=:), allocatable :: ext

    character(len=:), pointer :: delim_, fmt_, errmsg_
    logical,          pointer :: header_
    integer,          pointer :: stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ => ABSENT_HEADER
      else
        header_ => header
      end if

      if ( .not. present(delim) ) then
        delim_ => COMMA_DELIMITER
      else
        delim_ => delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ => INT_FMT
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ => fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                    "into integer array. Format must be one of: "//join(INT_FMTS)
          return
        end if
      end if

      select rank(into)
        rank(1)
          call from_text(file, into, header_, delim_, fmt_, stat_, errmsg_)
        rank(2)
          call from_text(file, into, header_, delim_, fmt_, stat_, errmsg_)
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-2."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)
    end if
  end procedure from_textfile_i8

  module procedure from_binaryfile_i64
    character(len=:), allocatable :: ext

    character(len=:), pointer :: errmsg_
    integer,          pointer :: stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( size(data_shape) /= rank(into) ) then
      stat_   = ARG_ERR
      errmsg_ = "RANK/SHAPE mismatch: size of data_shape must match rank of output array."
      return
    end if

    if ( any(BINARY_EXT == ext) ) then
      select rank(into)
        rank(1)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(2)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(3)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(4)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(5)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(6)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(7)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(8)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(9)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(10)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(11)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(12)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(13)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(14)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(15)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank default
          stat   = ARG_ERR
          errmsg = "Array has invalid rank. Supported ranks: 1-15."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Error reading file "'//file//'" with extension "'//ext//'". '// &
                "If attempting to read a text file, the data_shape argument must not be specified."// &
                "If attempting to read a binary file, the supported file extensions are: "//join(BINARY_EXT)
      return
    end if
  end procedure from_binaryfile_i64
  module procedure from_binaryfile_i32
    character(len=:), allocatable :: ext

    character(len=:), pointer :: errmsg_
    integer,          pointer :: stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( size(data_shape) /= rank(into) ) then
      stat_   = ARG_ERR
      errmsg_ = "RANK/SHAPE mismatch: size of data_shape must match rank of output array."
      return
    end if

    if ( any(BINARY_EXT == ext) ) then
      select rank(into)
        rank(1)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(2)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(3)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(4)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(5)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(6)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(7)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(8)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(9)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(10)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(11)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(12)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(13)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(14)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(15)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank default
          stat   = ARG_ERR
          errmsg = "Array has invalid rank. Supported ranks: 1-15."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Error reading file "'//file//'" with extension "'//ext//'". '// &
                "If attempting to read a text file, the data_shape argument must not be specified."// &
                "If attempting to read a binary file, the supported file extensions are: "//join(BINARY_EXT)
      return
    end if
  end procedure from_binaryfile_i32
  module procedure from_binaryfile_i16
    character(len=:), allocatable :: ext

    character(len=:), pointer :: errmsg_
    integer,          pointer :: stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( size(data_shape) /= rank(into) ) then
      stat_   = ARG_ERR
      errmsg_ = "RANK/SHAPE mismatch: size of data_shape must match rank of output array."
      return
    end if

    if ( any(BINARY_EXT == ext) ) then
      select rank(into)
        rank(1)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(2)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(3)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(4)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(5)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(6)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(7)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(8)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(9)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(10)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(11)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(12)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(13)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(14)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(15)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank default
          stat   = ARG_ERR
          errmsg = "Array has invalid rank. Supported ranks: 1-15."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Error reading file "'//file//'" with extension "'//ext//'". '// &
                "If attempting to read a text file, the data_shape argument must not be specified."// &
                "If attempting to read a binary file, the supported file extensions are: "//join(BINARY_EXT)
      return
    end if
  end procedure from_binaryfile_i16
  module procedure from_binaryfile_i8
    character(len=:), allocatable :: ext

    character(len=:), pointer :: errmsg_
    integer,          pointer :: stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    ext = ext_of(file)

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

    if ( size(data_shape) /= rank(into) ) then
      stat_   = ARG_ERR
      errmsg_ = "RANK/SHAPE mismatch: size of data_shape must match rank of output array."
      return
    end if

    if ( any(BINARY_EXT == ext) ) then
      select rank(into)
        rank(1)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(2)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(3)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(4)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(5)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(6)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(7)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(8)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(9)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(10)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(11)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(12)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(13)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(14)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank(15)
          call from_binary(file, into, data_shape, stat_, errmsg_)
        rank default
          stat   = ARG_ERR
          errmsg = "Array has invalid rank. Supported ranks: 1-15."
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Error reading file "'//file//'" with extension "'//ext//'". '// &
                "If attempting to read a text file, the data_shape argument must not be specified."// &
                "If attempting to read a binary file, the supported file extensions are: "//join(BINARY_EXT)
      return
    end if
  end procedure from_binaryfile_i8
end submodule file_io
