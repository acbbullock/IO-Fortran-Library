! Generated from fypp/io_fortran_lib/file_io_impl.fypp by fypp/generate.sh. Edit that file, not this one.
#include "iofl_config.inc"
submodule (io_fortran_lib) file_io
  !---------------------------------------------------------------------------------------------------------------------
  !! This submodule provides module procedure implementations for the **public interfaces** `to_file` and
  !! `from_file`.
  !---------------------------------------------------------------------------------------------------------------------
  implicit none (type, external)

  ! Definitions and interfaces ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=0), parameter :: EMPTY_STRING    = EMPTY_STR
  character(len=0), parameter :: EMPTY_HEADER(1) = [ EMPTY_STR ]
  character(len=1), parameter :: EXP_FMT         = REAL_FMTS(1)
  character(len=1), parameter :: INT_FMT         = INT_FMTS(1)
  character(len=1), parameter :: COMMA_DELIMITER = COMMA
  character(len=1), parameter :: SEMICOLON_DELIM = SEMICOLON
  character(len=2), parameter :: US_LOCALE       = LOCALES(1)
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

    do i = 1, len(ext)
      if ( (ext(i:i) >= "A") .and. (ext(i:i) <= "Z") ) ext(i:i) = achar(iachar(ext(i:i)) + 32)
    end do
  end procedure ext_of

  module procedure format_of
    character(len=:), allocatable :: ext

    file_format = EMPTY_STR

    if ( present(format) ) then
      if ( (format == "text") .or. (format == "binary") .or. (format == "npy") ) file_format = format
      return
    end if

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      file_format = "text"
    else if ( any(BINARY_EXT == ext) ) then
      file_format = "binary"
    else if ( ext == "npy" ) then
      file_format = "npy"
    end if
  end procedure format_of

  ! Writing Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ifdef IOFL_HAS_R128
  module procedure to_file_c128
    character(len=:), allocatable :: ext

    character(len=:), allocatable :: locale_, delim_, fmt_, im_
    character(len=:), pointer :: errmsg_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    if ( format_of(file, format) == "text" ) then
      select rank(x)
        rank(1); continue
        rank(2); continue
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Cannot write an array of rank "//str(rank(x))//" to text file."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select

      if ( present(header) ) then
        if ( size(header) /= 1 ) then
          select rank(x)
            rank(1)
              stat_ = ARG_ERR
              errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1.'
              call stop_if_unhandled(stat_, errmsg_, present(stat))
              return
            rank(2)
              if ( size(header) /= size(x, dim=2) ) then
                stat_ = ARG_ERR
                errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1 or size '// &
                          str(size(x, dim=2))//"."
                call stop_if_unhandled(stat_, errmsg_, present(stat))
                return
              end if
          end select
        end if
      end if

      if ( .not. present(locale) ) then
        locale_ = US_LOCALE
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid locale "'//locale//'" for file "'//file//'". Locale must be one of: '//join(LOCALES)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == US_LOCALE ) then
          delim_ = COMMA_DELIMITER
        else
          delim_ = SEMICOLON_DELIM
        end if
      else
        delim_ = delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ = EXP_FMT
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for file "'//file//'". Format must be one of: '//join(REAL_FMTS)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if
      end if

      im_ = optval(im, EMPTY_STRING)

      select rank(x)
        rank(1); call to_text(x, file, header, locale_, delim_, fmt_, decimals, im_, stat_, errmsg_)
        rank(2); call to_text(x, file, header, locale_, delim_, fmt_, decimals, im_, stat_, errmsg_)
      end select
    else if ( (format_of(file, format) == "binary") .or. (format_of(file, format) == "npy") ) then
      if ( format_of(file, format) == "npy" ) then
        stat_   = ARG_ERR
        errmsg_ = 'NumPy has no type for real128 data, so "'//file//'" cannot be a .npy file.'
        call stop_if_unhandled(stat_, errmsg_, present(stat))
        return
      end if

      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(locale  ) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'
      if ( present(decimals) ) write(*,"(a)") LF//'WARNING: decimals not supported for file type "'//ext//'".'
      if ( present(im      ) ) write(*,"(a)") LF//'WARNING: im not supported for file type "'//ext//'".'

      select rank(x)
        rank(1)
          call to_binary(x, file, stat_, errmsg_)
        rank(2)
          call to_binary(x, file, stat_, errmsg_)
        rank(3)
          call to_binary(x, file, stat_, errmsg_)
        rank(4)
          call to_binary(x, file, stat_, errmsg_)
        rank(5)
          call to_binary(x, file, stat_, errmsg_)
        rank(6)
          call to_binary(x, file, stat_, errmsg_)
        rank(7)
          call to_binary(x, file, stat_, errmsg_)
        rank(8)
          call to_binary(x, file, stat_, errmsg_)
        rank(9)
          call to_binary(x, file, stat_, errmsg_)
        rank(10)
          call to_binary(x, file, stat_, errmsg_)
        rank(11)
          call to_binary(x, file, stat_, errmsg_)
        rank(12)
          call to_binary(x, file, stat_, errmsg_)
        rank(13)
          call to_binary(x, file, stat_, errmsg_)
        rank(14)
          call to_binary(x, file, stat_, errmsg_)
        rank(15)
          call to_binary(x, file, stat_, errmsg_)
        rank default
          stat_  = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//SPACE//join(BINARY_EXT)//' npy. '// &
                'Or pass format="text", format="binary" or format="npy".'
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure to_file_c128
#endif

  module procedure to_file_c64
    character(len=:), allocatable :: ext

    character(len=:), allocatable :: locale_, delim_, fmt_, im_
    character(len=:), pointer :: errmsg_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    if ( format_of(file, format) == "text" ) then
      select rank(x)
        rank(1); continue
        rank(2); continue
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Cannot write an array of rank "//str(rank(x))//" to text file."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select

      if ( present(header) ) then
        if ( size(header) /= 1 ) then
          select rank(x)
            rank(1)
              stat_ = ARG_ERR
              errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1.'
              call stop_if_unhandled(stat_, errmsg_, present(stat))
              return
            rank(2)
              if ( size(header) /= size(x, dim=2) ) then
                stat_ = ARG_ERR
                errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1 or size '// &
                          str(size(x, dim=2))//"."
                call stop_if_unhandled(stat_, errmsg_, present(stat))
                return
              end if
          end select
        end if
      end if

      if ( .not. present(locale) ) then
        locale_ = US_LOCALE
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid locale "'//locale//'" for file "'//file//'". Locale must be one of: '//join(LOCALES)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == US_LOCALE ) then
          delim_ = COMMA_DELIMITER
        else
          delim_ = SEMICOLON_DELIM
        end if
      else
        delim_ = delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ = EXP_FMT
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for file "'//file//'". Format must be one of: '//join(REAL_FMTS)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if
      end if

      im_ = optval(im, EMPTY_STRING)

      select rank(x)
        rank(1); call to_text(x, file, header, locale_, delim_, fmt_, decimals, im_, stat_, errmsg_)
        rank(2); call to_text(x, file, header, locale_, delim_, fmt_, decimals, im_, stat_, errmsg_)
      end select
    else if ( (format_of(file, format) == "binary") .or. (format_of(file, format) == "npy") ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(locale  ) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'
      if ( present(decimals) ) write(*,"(a)") LF//'WARNING: decimals not supported for file type "'//ext//'".'
      if ( present(im      ) ) write(*,"(a)") LF//'WARNING: im not supported for file type "'//ext//'".'

      select rank(x)
        rank(1)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(2)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(3)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(4)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(5)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(6)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(7)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(8)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(9)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(10)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(11)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(12)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(13)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(14)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(15)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank default
          stat_  = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//SPACE//join(BINARY_EXT)//' npy. '// &
                'Or pass format="text", format="binary" or format="npy".'
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure to_file_c64

  module procedure to_file_c32
    character(len=:), allocatable :: ext

    character(len=:), allocatable :: locale_, delim_, fmt_, im_
    character(len=:), pointer :: errmsg_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    if ( format_of(file, format) == "text" ) then
      select rank(x)
        rank(1); continue
        rank(2); continue
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Cannot write an array of rank "//str(rank(x))//" to text file."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select

      if ( present(header) ) then
        if ( size(header) /= 1 ) then
          select rank(x)
            rank(1)
              stat_ = ARG_ERR
              errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1.'
              call stop_if_unhandled(stat_, errmsg_, present(stat))
              return
            rank(2)
              if ( size(header) /= size(x, dim=2) ) then
                stat_ = ARG_ERR
                errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1 or size '// &
                          str(size(x, dim=2))//"."
                call stop_if_unhandled(stat_, errmsg_, present(stat))
                return
              end if
          end select
        end if
      end if

      if ( .not. present(locale) ) then
        locale_ = US_LOCALE
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid locale "'//locale//'" for file "'//file//'". Locale must be one of: '//join(LOCALES)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == US_LOCALE ) then
          delim_ = COMMA_DELIMITER
        else
          delim_ = SEMICOLON_DELIM
        end if
      else
        delim_ = delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ = EXP_FMT
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for file "'//file//'". Format must be one of: '//join(REAL_FMTS)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if
      end if

      im_ = optval(im, EMPTY_STRING)

      select rank(x)
        rank(1); call to_text(x, file, header, locale_, delim_, fmt_, decimals, im_, stat_, errmsg_)
        rank(2); call to_text(x, file, header, locale_, delim_, fmt_, decimals, im_, stat_, errmsg_)
      end select
    else if ( (format_of(file, format) == "binary") .or. (format_of(file, format) == "npy") ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(locale  ) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'
      if ( present(decimals) ) write(*,"(a)") LF//'WARNING: decimals not supported for file type "'//ext//'".'
      if ( present(im      ) ) write(*,"(a)") LF//'WARNING: im not supported for file type "'//ext//'".'

      select rank(x)
        rank(1)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(2)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(3)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(4)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(5)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(6)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(7)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(8)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(9)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(10)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(11)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(12)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(13)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(14)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(15)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank default
          stat_  = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//SPACE//join(BINARY_EXT)//' npy. '// &
                'Or pass format="text", format="binary" or format="npy".'
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure to_file_c32


#ifdef IOFL_HAS_R128
  module procedure to_file_r128
    character(len=:), allocatable :: ext

    character(len=:), allocatable :: locale_, delim_, fmt_
    character(len=:), pointer :: errmsg_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    if ( format_of(file, format) == "text" ) then
      select rank(x)
        rank(1); continue
        rank(2); continue
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Cannot write an array of rank "//str(rank(x))//" to text file."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select

      if ( present(header) ) then
        if ( size(header) /= 1 ) then
          select rank(x)
            rank(1)
              stat_ = ARG_ERR
              errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1.'
              call stop_if_unhandled(stat_, errmsg_, present(stat))
              return
            rank(2)
              if ( size(header) /= size(x, dim=2) ) then
                stat_ = ARG_ERR
                errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1 or size '// &
                          str(size(x, dim=2))//"."
                call stop_if_unhandled(stat_, errmsg_, present(stat))
                return
              end if
          end select
        end if
      end if

      if ( .not. present(locale) ) then
        locale_ = US_LOCALE
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid locale "'//locale//'" for file "'//file//'". Locale must be one of: '//join(LOCALES)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == US_LOCALE ) then
          delim_ = COMMA_DELIMITER
        else
          delim_ = SEMICOLON_DELIM
        end if
      else
        delim_ = delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ = EXP_FMT
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for file "'//file//'". Format must be one of: '//join(REAL_FMTS)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if
      end if

      select rank(x)
        rank(1); call to_text(x, file, header, locale_, delim_, fmt_, decimals, stat_, errmsg_)
        rank(2); call to_text(x, file, header, locale_, delim_, fmt_, decimals, stat_, errmsg_)
      end select
    else if ( (format_of(file, format) == "binary") .or. (format_of(file, format) == "npy") ) then
      if ( format_of(file, format) == "npy" ) then
        stat_   = ARG_ERR
        errmsg_ = 'NumPy has no type for real128 data, so "'//file//'" cannot be a .npy file.'
        call stop_if_unhandled(stat_, errmsg_, present(stat))
        return
      end if

      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(locale  ) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'
      if ( present(decimals) ) write(*,"(a)") LF//'WARNING: decimals not supported for file type "'//ext//'".'

      select rank(x)
        rank(1)
          call to_binary(x, file, stat_, errmsg_)
        rank(2)
          call to_binary(x, file, stat_, errmsg_)
        rank(3)
          call to_binary(x, file, stat_, errmsg_)
        rank(4)
          call to_binary(x, file, stat_, errmsg_)
        rank(5)
          call to_binary(x, file, stat_, errmsg_)
        rank(6)
          call to_binary(x, file, stat_, errmsg_)
        rank(7)
          call to_binary(x, file, stat_, errmsg_)
        rank(8)
          call to_binary(x, file, stat_, errmsg_)
        rank(9)
          call to_binary(x, file, stat_, errmsg_)
        rank(10)
          call to_binary(x, file, stat_, errmsg_)
        rank(11)
          call to_binary(x, file, stat_, errmsg_)
        rank(12)
          call to_binary(x, file, stat_, errmsg_)
        rank(13)
          call to_binary(x, file, stat_, errmsg_)
        rank(14)
          call to_binary(x, file, stat_, errmsg_)
        rank(15)
          call to_binary(x, file, stat_, errmsg_)
        rank default
          stat_  = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//SPACE//join(BINARY_EXT)//' npy. '// &
                'Or pass format="text", format="binary" or format="npy".'
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure to_file_r128
#endif

  module procedure to_file_r64
    character(len=:), allocatable :: ext

    character(len=:), allocatable :: locale_, delim_, fmt_
    character(len=:), pointer :: errmsg_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    if ( format_of(file, format) == "text" ) then
      select rank(x)
        rank(1); continue
        rank(2); continue
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Cannot write an array of rank "//str(rank(x))//" to text file."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select

      if ( present(header) ) then
        if ( size(header) /= 1 ) then
          select rank(x)
            rank(1)
              stat_ = ARG_ERR
              errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1.'
              call stop_if_unhandled(stat_, errmsg_, present(stat))
              return
            rank(2)
              if ( size(header) /= size(x, dim=2) ) then
                stat_ = ARG_ERR
                errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1 or size '// &
                          str(size(x, dim=2))//"."
                call stop_if_unhandled(stat_, errmsg_, present(stat))
                return
              end if
          end select
        end if
      end if

      if ( .not. present(locale) ) then
        locale_ = US_LOCALE
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid locale "'//locale//'" for file "'//file//'". Locale must be one of: '//join(LOCALES)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == US_LOCALE ) then
          delim_ = COMMA_DELIMITER
        else
          delim_ = SEMICOLON_DELIM
        end if
      else
        delim_ = delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ = EXP_FMT
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for file "'//file//'". Format must be one of: '//join(REAL_FMTS)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if
      end if

      select rank(x)
        rank(1); call to_text(x, file, header, locale_, delim_, fmt_, decimals, stat_, errmsg_)
        rank(2); call to_text(x, file, header, locale_, delim_, fmt_, decimals, stat_, errmsg_)
      end select
    else if ( (format_of(file, format) == "binary") .or. (format_of(file, format) == "npy") ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(locale  ) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'
      if ( present(decimals) ) write(*,"(a)") LF//'WARNING: decimals not supported for file type "'//ext//'".'

      select rank(x)
        rank(1)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(2)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(3)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(4)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(5)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(6)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(7)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(8)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(9)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(10)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(11)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(12)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(13)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(14)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(15)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank default
          stat_  = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//SPACE//join(BINARY_EXT)//' npy. '// &
                'Or pass format="text", format="binary" or format="npy".'
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure to_file_r64

  module procedure to_file_r32
    character(len=:), allocatable :: ext

    character(len=:), allocatable :: locale_, delim_, fmt_
    character(len=:), pointer :: errmsg_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    if ( format_of(file, format) == "text" ) then
      select rank(x)
        rank(1); continue
        rank(2); continue
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Cannot write an array of rank "//str(rank(x))//" to text file."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select

      if ( present(header) ) then
        if ( size(header) /= 1 ) then
          select rank(x)
            rank(1)
              stat_ = ARG_ERR
              errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1.'
              call stop_if_unhandled(stat_, errmsg_, present(stat))
              return
            rank(2)
              if ( size(header) /= size(x, dim=2) ) then
                stat_ = ARG_ERR
                errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1 or size '// &
                          str(size(x, dim=2))//"."
                call stop_if_unhandled(stat_, errmsg_, present(stat))
                return
              end if
          end select
        end if
      end if

      if ( .not. present(locale) ) then
        locale_ = US_LOCALE
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid locale "'//locale//'" for file "'//file//'". Locale must be one of: '//join(LOCALES)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == US_LOCALE ) then
          delim_ = COMMA_DELIMITER
        else
          delim_ = SEMICOLON_DELIM
        end if
      else
        delim_ = delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ = EXP_FMT
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for file "'//file//'". Format must be one of: '//join(REAL_FMTS)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if
      end if

      select rank(x)
        rank(1); call to_text(x, file, header, locale_, delim_, fmt_, decimals, stat_, errmsg_)
        rank(2); call to_text(x, file, header, locale_, delim_, fmt_, decimals, stat_, errmsg_)
      end select
    else if ( (format_of(file, format) == "binary") .or. (format_of(file, format) == "npy") ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(locale  ) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'
      if ( present(decimals) ) write(*,"(a)") LF//'WARNING: decimals not supported for file type "'//ext//'".'

      select rank(x)
        rank(1)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(2)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(3)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(4)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(5)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(6)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(7)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(8)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(9)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(10)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(11)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(12)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(13)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(14)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(15)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank default
          stat_  = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//SPACE//join(BINARY_EXT)//' npy. '// &
                'Or pass format="text", format="binary" or format="npy".'
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure to_file_r32


  module procedure to_file_i64
    character(len=:), allocatable :: ext

    character(len=:), allocatable :: delim_, fmt_
    character(len=:), pointer :: errmsg_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    if ( format_of(file, format) == "text" ) then
      select rank(x)
        rank(1); continue
        rank(2); continue
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Cannot write an array of rank "//str(rank(x))//" to text file."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select

      if ( present(header) ) then
        if ( size(header) /= 1 ) then
          select rank(x)
            rank(1)
              stat_ = ARG_ERR
              errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1.'
              call stop_if_unhandled(stat_, errmsg_, present(stat))
              return
            rank(2)
              if ( size(header) /= size(x, dim=2) ) then
                stat_ = ARG_ERR
                errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1 or size '// &
                          str(size(x, dim=2))//"."
                call stop_if_unhandled(stat_, errmsg_, present(stat))
                return
              end if
          end select
        end if
      end if

      delim_ = optval(delim, COMMA_DELIMITER)

      if ( .not. present(fmt) ) then
        fmt_ = INT_FMT
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ = fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for file "'//file//'". Format must be one of: '//join(INT_FMTS)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if
      end if

      select rank(x)
        rank(1); call to_text(x, file, header, delim_, fmt_, stat_, errmsg_)
        rank(2); call to_text(x, file, header, delim_, fmt_, stat_, errmsg_)
      end select
    else if ( (format_of(file, format) == "binary") .or. (format_of(file, format) == "npy") ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'

      select rank(x)
        rank(1)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(2)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(3)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(4)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(5)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(6)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(7)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(8)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(9)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(10)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(11)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(12)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(13)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(14)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(15)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank default
          stat_  = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//SPACE//join(BINARY_EXT)//' npy. '// &
                'Or pass format="text", format="binary" or format="npy".'
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure to_file_i64

  module procedure to_file_i32
    character(len=:), allocatable :: ext

    character(len=:), allocatable :: delim_, fmt_
    character(len=:), pointer :: errmsg_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    if ( format_of(file, format) == "text" ) then
      select rank(x)
        rank(1); continue
        rank(2); continue
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Cannot write an array of rank "//str(rank(x))//" to text file."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select

      if ( present(header) ) then
        if ( size(header) /= 1 ) then
          select rank(x)
            rank(1)
              stat_ = ARG_ERR
              errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1.'
              call stop_if_unhandled(stat_, errmsg_, present(stat))
              return
            rank(2)
              if ( size(header) /= size(x, dim=2) ) then
                stat_ = ARG_ERR
                errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1 or size '// &
                          str(size(x, dim=2))//"."
                call stop_if_unhandled(stat_, errmsg_, present(stat))
                return
              end if
          end select
        end if
      end if

      delim_ = optval(delim, COMMA_DELIMITER)

      if ( .not. present(fmt) ) then
        fmt_ = INT_FMT
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ = fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for file "'//file//'". Format must be one of: '//join(INT_FMTS)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if
      end if

      select rank(x)
        rank(1); call to_text(x, file, header, delim_, fmt_, stat_, errmsg_)
        rank(2); call to_text(x, file, header, delim_, fmt_, stat_, errmsg_)
      end select
    else if ( (format_of(file, format) == "binary") .or. (format_of(file, format) == "npy") ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'

      select rank(x)
        rank(1)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(2)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(3)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(4)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(5)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(6)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(7)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(8)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(9)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(10)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(11)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(12)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(13)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(14)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(15)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank default
          stat_  = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//SPACE//join(BINARY_EXT)//' npy. '// &
                'Or pass format="text", format="binary" or format="npy".'
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure to_file_i32

  module procedure to_file_i16
    character(len=:), allocatable :: ext

    character(len=:), allocatable :: delim_, fmt_
    character(len=:), pointer :: errmsg_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    if ( format_of(file, format) == "text" ) then
      select rank(x)
        rank(1); continue
        rank(2); continue
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Cannot write an array of rank "//str(rank(x))//" to text file."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select

      if ( present(header) ) then
        if ( size(header) /= 1 ) then
          select rank(x)
            rank(1)
              stat_ = ARG_ERR
              errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1.'
              call stop_if_unhandled(stat_, errmsg_, present(stat))
              return
            rank(2)
              if ( size(header) /= size(x, dim=2) ) then
                stat_ = ARG_ERR
                errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1 or size '// &
                          str(size(x, dim=2))//"."
                call stop_if_unhandled(stat_, errmsg_, present(stat))
                return
              end if
          end select
        end if
      end if

      delim_ = optval(delim, COMMA_DELIMITER)

      if ( .not. present(fmt) ) then
        fmt_ = INT_FMT
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ = fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for file "'//file//'". Format must be one of: '//join(INT_FMTS)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if
      end if

      select rank(x)
        rank(1); call to_text(x, file, header, delim_, fmt_, stat_, errmsg_)
        rank(2); call to_text(x, file, header, delim_, fmt_, stat_, errmsg_)
      end select
    else if ( (format_of(file, format) == "binary") .or. (format_of(file, format) == "npy") ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'

      select rank(x)
        rank(1)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(2)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(3)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(4)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(5)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(6)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(7)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(8)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(9)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(10)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(11)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(12)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(13)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(14)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(15)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank default
          stat_  = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//SPACE//join(BINARY_EXT)//' npy. '// &
                'Or pass format="text", format="binary" or format="npy".'
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure to_file_i16

  module procedure to_file_i8
    character(len=:), allocatable :: ext

    character(len=:), allocatable :: delim_, fmt_
    character(len=:), pointer :: errmsg_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    if ( format_of(file, format) == "text" ) then
      select rank(x)
        rank(1); continue
        rank(2); continue
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Cannot write an array of rank "//str(rank(x))//" to text file."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select

      if ( present(header) ) then
        if ( size(header) /= 1 ) then
          select rank(x)
            rank(1)
              stat_ = ARG_ERR
              errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1.'
              call stop_if_unhandled(stat_, errmsg_, present(stat))
              return
            rank(2)
              if ( size(header) /= size(x, dim=2) ) then
                stat_ = ARG_ERR
                errmsg_ = 'Invalid header for file "'//file//'". Header for this data must have size 1 or size '// &
                          str(size(x, dim=2))//"."
                call stop_if_unhandled(stat_, errmsg_, present(stat))
                return
              end if
          end select
        end if
      end if

      delim_ = optval(delim, COMMA_DELIMITER)

      if ( .not. present(fmt) ) then
        fmt_ = INT_FMT
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ = fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for file "'//file//'". Format must be one of: '//join(INT_FMTS)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if
      end if

      select rank(x)
        rank(1); call to_text(x, file, header, delim_, fmt_, stat_, errmsg_)
        rank(2); call to_text(x, file, header, delim_, fmt_, stat_, errmsg_)
      end select
    else if ( (format_of(file, format) == "binary") .or. (format_of(file, format) == "npy") ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'

      select rank(x)
        rank(1)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(2)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(3)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(4)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(5)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(6)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(7)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(8)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(9)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(10)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(11)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(12)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(13)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(14)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank(15)
          if ( format_of(file, format) == "npy" ) then
            call to_npy(x, file, stat_, errmsg_)
          else
            call to_binary(x, file, stat_, errmsg_)
          end if
        rank default
          stat_  = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//SPACE//join(BINARY_EXT)//' npy. '// &
                'Or pass format="text", format="binary" or format="npy".'
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure to_file_i8


  ! Reading Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ifdef IOFL_HAS_R128
  module procedure from_textfile_c128
    character(len=:), allocatable :: ext

    character(len=:), allocatable :: locale_, delim_, fmt_, im_
    character(len=:), pointer :: errmsg_
    logical :: header_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    if ( format_of(file, format) == "text" ) then
      header_ = optval(header, ABSENT_HEADER)

      if ( .not. present(locale) ) then
        locale_ = US_LOCALE
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid locale "'//locale//'" for file "'//file//'". Locale must be one of: '//join(LOCALES)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == US_LOCALE ) then
          delim_ = COMMA_DELIMITER
        else
          delim_ = SEMICOLON_DELIM
        end if
      else
        delim_ = delim
        if ( locale_ == US_LOCALE ) then
          if ( delim_ == POINT ) then
            stat_   = ARG_ERR
            errmsg_ = "Invalid delimiter for numbers having US style decimal."
            call stop_if_unhandled(stat_, errmsg_, present(stat))
            return
          end if
        else
          if ( delim_ == COMMA ) then
            stat_   = ARG_ERR
            errmsg_ = "Invalid delimiter for numbers having EU style decimal."
            call stop_if_unhandled(stat_, errmsg_, present(stat))
            return
          end if
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = EXP_FMT
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                    "into complex array. Format must be one of: "//join(REAL_FMTS)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if
      end if

      im_ = optval(im, EMPTY_STRING)

      select rank(into)
        rank(1)
          call from_text(file, into, header_, locale_, delim_, fmt_, im_, stat_, errmsg_)
        rank(2)
          call from_text(file, into, header_, locale_, delim_, fmt_, im_, stat_, errmsg_)
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-2."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else if ( format_of(file, format) == "npy" ) then
      stat_   = ARG_ERR
      errmsg_ = 'NumPy has no type for real128 data, so "'//file//'" cannot be a .npy file.'
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//' npy. Or pass format="text" or format="npy".'
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure from_textfile_c128
#endif

  module procedure from_textfile_c64
    character(len=:), allocatable :: ext

    character(len=:), allocatable :: locale_, delim_, fmt_, im_
    character(len=:), pointer :: errmsg_
    logical :: header_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    if ( format_of(file, format) == "text" ) then
      header_ = optval(header, ABSENT_HEADER)

      if ( .not. present(locale) ) then
        locale_ = US_LOCALE
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid locale "'//locale//'" for file "'//file//'". Locale must be one of: '//join(LOCALES)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == US_LOCALE ) then
          delim_ = COMMA_DELIMITER
        else
          delim_ = SEMICOLON_DELIM
        end if
      else
        delim_ = delim
        if ( locale_ == US_LOCALE ) then
          if ( delim_ == POINT ) then
            stat_   = ARG_ERR
            errmsg_ = "Invalid delimiter for numbers having US style decimal."
            call stop_if_unhandled(stat_, errmsg_, present(stat))
            return
          end if
        else
          if ( delim_ == COMMA ) then
            stat_   = ARG_ERR
            errmsg_ = "Invalid delimiter for numbers having EU style decimal."
            call stop_if_unhandled(stat_, errmsg_, present(stat))
            return
          end if
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = EXP_FMT
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                    "into complex array. Format must be one of: "//join(REAL_FMTS)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if
      end if

      im_ = optval(im, EMPTY_STRING)

      select rank(into)
        rank(1)
          call from_text(file, into, header_, locale_, delim_, fmt_, im_, stat_, errmsg_)
        rank(2)
          call from_text(file, into, header_, locale_, delim_, fmt_, im_, stat_, errmsg_)
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-2."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else if ( format_of(file, format) == "npy" ) then
      if ( present(header) ) write(*,"(a)") LF//'WARNING: header not supported for file type "npy".'
      if ( present(locale) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "npy".'
      if ( present(delim ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "npy".'
      if ( present(fmt   ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "npy".'
      if ( present(im    ) ) write(*,"(a)") LF//'WARNING: im not supported for file type "npy".'

      select rank(into)
        rank(1); call from_npy(file, into, stat_, errmsg_)
        rank(2); call from_npy(file, into, stat_, errmsg_)
        rank(3); call from_npy(file, into, stat_, errmsg_)
        rank(4); call from_npy(file, into, stat_, errmsg_)
        rank(5); call from_npy(file, into, stat_, errmsg_)
        rank(6); call from_npy(file, into, stat_, errmsg_)
        rank(7); call from_npy(file, into, stat_, errmsg_)
        rank(8); call from_npy(file, into, stat_, errmsg_)
        rank(9); call from_npy(file, into, stat_, errmsg_)
        rank(10); call from_npy(file, into, stat_, errmsg_)
        rank(11); call from_npy(file, into, stat_, errmsg_)
        rank(12); call from_npy(file, into, stat_, errmsg_)
        rank(13); call from_npy(file, into, stat_, errmsg_)
        rank(14); call from_npy(file, into, stat_, errmsg_)
        rank(15); call from_npy(file, into, stat_, errmsg_)
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//' npy. Or pass format="text" or format="npy".'
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure from_textfile_c64

  module procedure from_textfile_c32
    character(len=:), allocatable :: ext

    character(len=:), allocatable :: locale_, delim_, fmt_, im_
    character(len=:), pointer :: errmsg_
    logical :: header_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    if ( format_of(file, format) == "text" ) then
      header_ = optval(header, ABSENT_HEADER)

      if ( .not. present(locale) ) then
        locale_ = US_LOCALE
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid locale "'//locale//'" for file "'//file//'". Locale must be one of: '//join(LOCALES)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == US_LOCALE ) then
          delim_ = COMMA_DELIMITER
        else
          delim_ = SEMICOLON_DELIM
        end if
      else
        delim_ = delim
        if ( locale_ == US_LOCALE ) then
          if ( delim_ == POINT ) then
            stat_   = ARG_ERR
            errmsg_ = "Invalid delimiter for numbers having US style decimal."
            call stop_if_unhandled(stat_, errmsg_, present(stat))
            return
          end if
        else
          if ( delim_ == COMMA ) then
            stat_   = ARG_ERR
            errmsg_ = "Invalid delimiter for numbers having EU style decimal."
            call stop_if_unhandled(stat_, errmsg_, present(stat))
            return
          end if
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = EXP_FMT
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                    "into complex array. Format must be one of: "//join(REAL_FMTS)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if
      end if

      im_ = optval(im, EMPTY_STRING)

      select rank(into)
        rank(1)
          call from_text(file, into, header_, locale_, delim_, fmt_, im_, stat_, errmsg_)
        rank(2)
          call from_text(file, into, header_, locale_, delim_, fmt_, im_, stat_, errmsg_)
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-2."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else if ( format_of(file, format) == "npy" ) then
      if ( present(header) ) write(*,"(a)") LF//'WARNING: header not supported for file type "npy".'
      if ( present(locale) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "npy".'
      if ( present(delim ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "npy".'
      if ( present(fmt   ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "npy".'
      if ( present(im    ) ) write(*,"(a)") LF//'WARNING: im not supported for file type "npy".'

      select rank(into)
        rank(1); call from_npy(file, into, stat_, errmsg_)
        rank(2); call from_npy(file, into, stat_, errmsg_)
        rank(3); call from_npy(file, into, stat_, errmsg_)
        rank(4); call from_npy(file, into, stat_, errmsg_)
        rank(5); call from_npy(file, into, stat_, errmsg_)
        rank(6); call from_npy(file, into, stat_, errmsg_)
        rank(7); call from_npy(file, into, stat_, errmsg_)
        rank(8); call from_npy(file, into, stat_, errmsg_)
        rank(9); call from_npy(file, into, stat_, errmsg_)
        rank(10); call from_npy(file, into, stat_, errmsg_)
        rank(11); call from_npy(file, into, stat_, errmsg_)
        rank(12); call from_npy(file, into, stat_, errmsg_)
        rank(13); call from_npy(file, into, stat_, errmsg_)
        rank(14); call from_npy(file, into, stat_, errmsg_)
        rank(15); call from_npy(file, into, stat_, errmsg_)
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//' npy. Or pass format="text" or format="npy".'
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure from_textfile_c32


#ifdef IOFL_HAS_R128
  module procedure from_binaryfile_c128
    character(len=:), allocatable :: ext
    integer                       :: into_rank

    character(len=:), pointer :: errmsg_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    select rank(into)
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

    if ( size(data_shape) /= into_rank ) then
      stat_   = ARG_ERR
      errmsg_ = "RANK/SHAPE mismatch: size of data_shape must match rank of output array."
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    if ( format_of(file, format) == "npy" ) then
      stat_   = ARG_ERR
      errmsg_ = 'NumPy has no type for real128 data, so "'//file//'" cannot be a .npy file.'
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    else if ( format_of(file, format) == "binary" ) then
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
          stat_  = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Error reading file "'//file//'" with extension "'//ext//'". '// &
                "If attempting to read a text file, the data_shape argument must not be specified."// &
                "If attempting to read a binary file, the supported file extensions are: "//join(BINARY_EXT)
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure from_binaryfile_c128
#endif

  module procedure from_binaryfile_c64
    character(len=:), allocatable :: ext
    integer                       :: into_rank

    character(len=:), pointer :: errmsg_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    select rank(into)
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

    if ( size(data_shape) /= into_rank ) then
      stat_   = ARG_ERR
      errmsg_ = "RANK/SHAPE mismatch: size of data_shape must match rank of output array."
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    if ( format_of(file, format) == "npy" ) then
      select rank(into)
        rank(1)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(2)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(3)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(4)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(5)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(6)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(7)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(8)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(9)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(10)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(11)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(12)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(13)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(14)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(15)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
      end select
    else if ( format_of(file, format) == "binary" ) then
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
          stat_  = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Error reading file "'//file//'" with extension "'//ext//'". '// &
                "If attempting to read a text file, the data_shape argument must not be specified."// &
                "If attempting to read a binary file, the supported file extensions are: "//join(BINARY_EXT)
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure from_binaryfile_c64

  module procedure from_binaryfile_c32
    character(len=:), allocatable :: ext
    integer                       :: into_rank

    character(len=:), pointer :: errmsg_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    select rank(into)
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

    if ( size(data_shape) /= into_rank ) then
      stat_   = ARG_ERR
      errmsg_ = "RANK/SHAPE mismatch: size of data_shape must match rank of output array."
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    if ( format_of(file, format) == "npy" ) then
      select rank(into)
        rank(1)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(2)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(3)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(4)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(5)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(6)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(7)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(8)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(9)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(10)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(11)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(12)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(13)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(14)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(15)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
      end select
    else if ( format_of(file, format) == "binary" ) then
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
          stat_  = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Error reading file "'//file//'" with extension "'//ext//'". '// &
                "If attempting to read a text file, the data_shape argument must not be specified."// &
                "If attempting to read a binary file, the supported file extensions are: "//join(BINARY_EXT)
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure from_binaryfile_c32


#ifdef IOFL_HAS_R128
  module procedure from_textfile_r128
    character(len=:), allocatable :: ext

    character(len=:), allocatable :: locale_, delim_, fmt_
    character(len=:), pointer :: errmsg_
    logical :: header_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    if ( format_of(file, format) == "text" ) then
      header_ = optval(header, ABSENT_HEADER)

      if ( .not. present(locale) ) then
        locale_ = US_LOCALE
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid locale "'//locale//'" for file "'//file//'". Locale must be one of: '//join(LOCALES)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == US_LOCALE ) then
          delim_ = COMMA_DELIMITER
        else
          delim_ = SEMICOLON_DELIM
        end if
      else
        delim_ = delim
        if ( locale_ == US_LOCALE ) then
          if ( delim_ == POINT ) then
            stat_   = ARG_ERR
            errmsg_ = "Invalid delimiter for numbers having US style decimal."
            call stop_if_unhandled(stat_, errmsg_, present(stat))
            return
          end if
        else
          if ( delim_ == COMMA ) then
            stat_   = ARG_ERR
            errmsg_ = "Invalid delimiter for numbers having EU style decimal."
            call stop_if_unhandled(stat_, errmsg_, present(stat))
            return
          end if
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = EXP_FMT
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                    "into real array. Format must be one of: "//join(REAL_FMTS)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
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
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else if ( format_of(file, format) == "npy" ) then
      stat_   = ARG_ERR
      errmsg_ = 'NumPy has no type for real128 data, so "'//file//'" cannot be a .npy file.'
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//' npy. Or pass format="text" or format="npy".'
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure from_textfile_r128
#endif

  module procedure from_textfile_r64
    character(len=:), allocatable :: ext

    character(len=:), allocatable :: locale_, delim_, fmt_
    character(len=:), pointer :: errmsg_
    logical :: header_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    if ( format_of(file, format) == "text" ) then
      header_ = optval(header, ABSENT_HEADER)

      if ( .not. present(locale) ) then
        locale_ = US_LOCALE
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid locale "'//locale//'" for file "'//file//'". Locale must be one of: '//join(LOCALES)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == US_LOCALE ) then
          delim_ = COMMA_DELIMITER
        else
          delim_ = SEMICOLON_DELIM
        end if
      else
        delim_ = delim
        if ( locale_ == US_LOCALE ) then
          if ( delim_ == POINT ) then
            stat_   = ARG_ERR
            errmsg_ = "Invalid delimiter for numbers having US style decimal."
            call stop_if_unhandled(stat_, errmsg_, present(stat))
            return
          end if
        else
          if ( delim_ == COMMA ) then
            stat_   = ARG_ERR
            errmsg_ = "Invalid delimiter for numbers having EU style decimal."
            call stop_if_unhandled(stat_, errmsg_, present(stat))
            return
          end if
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = EXP_FMT
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                    "into real array. Format must be one of: "//join(REAL_FMTS)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
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
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else if ( format_of(file, format) == "npy" ) then
      if ( present(header) ) write(*,"(a)") LF//'WARNING: header not supported for file type "npy".'
      if ( present(locale) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "npy".'
      if ( present(delim ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "npy".'
      if ( present(fmt   ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "npy".'

      select rank(into)
        rank(1); call from_npy(file, into, stat_, errmsg_)
        rank(2); call from_npy(file, into, stat_, errmsg_)
        rank(3); call from_npy(file, into, stat_, errmsg_)
        rank(4); call from_npy(file, into, stat_, errmsg_)
        rank(5); call from_npy(file, into, stat_, errmsg_)
        rank(6); call from_npy(file, into, stat_, errmsg_)
        rank(7); call from_npy(file, into, stat_, errmsg_)
        rank(8); call from_npy(file, into, stat_, errmsg_)
        rank(9); call from_npy(file, into, stat_, errmsg_)
        rank(10); call from_npy(file, into, stat_, errmsg_)
        rank(11); call from_npy(file, into, stat_, errmsg_)
        rank(12); call from_npy(file, into, stat_, errmsg_)
        rank(13); call from_npy(file, into, stat_, errmsg_)
        rank(14); call from_npy(file, into, stat_, errmsg_)
        rank(15); call from_npy(file, into, stat_, errmsg_)
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//' npy. Or pass format="text" or format="npy".'
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure from_textfile_r64

  module procedure from_textfile_r32
    character(len=:), allocatable :: ext

    character(len=:), allocatable :: locale_, delim_, fmt_
    character(len=:), pointer :: errmsg_
    logical :: header_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    if ( format_of(file, format) == "text" ) then
      header_ = optval(header, ABSENT_HEADER)

      if ( .not. present(locale) ) then
        locale_ = US_LOCALE
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid locale "'//locale//'" for file "'//file//'". Locale must be one of: '//join(LOCALES)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == US_LOCALE ) then
          delim_ = COMMA_DELIMITER
        else
          delim_ = SEMICOLON_DELIM
        end if
      else
        delim_ = delim
        if ( locale_ == US_LOCALE ) then
          if ( delim_ == POINT ) then
            stat_   = ARG_ERR
            errmsg_ = "Invalid delimiter for numbers having US style decimal."
            call stop_if_unhandled(stat_, errmsg_, present(stat))
            return
          end if
        else
          if ( delim_ == COMMA ) then
            stat_   = ARG_ERR
            errmsg_ = "Invalid delimiter for numbers having EU style decimal."
            call stop_if_unhandled(stat_, errmsg_, present(stat))
            return
          end if
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = EXP_FMT
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                    "into real array. Format must be one of: "//join(REAL_FMTS)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
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
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else if ( format_of(file, format) == "npy" ) then
      if ( present(header) ) write(*,"(a)") LF//'WARNING: header not supported for file type "npy".'
      if ( present(locale) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "npy".'
      if ( present(delim ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "npy".'
      if ( present(fmt   ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "npy".'

      select rank(into)
        rank(1); call from_npy(file, into, stat_, errmsg_)
        rank(2); call from_npy(file, into, stat_, errmsg_)
        rank(3); call from_npy(file, into, stat_, errmsg_)
        rank(4); call from_npy(file, into, stat_, errmsg_)
        rank(5); call from_npy(file, into, stat_, errmsg_)
        rank(6); call from_npy(file, into, stat_, errmsg_)
        rank(7); call from_npy(file, into, stat_, errmsg_)
        rank(8); call from_npy(file, into, stat_, errmsg_)
        rank(9); call from_npy(file, into, stat_, errmsg_)
        rank(10); call from_npy(file, into, stat_, errmsg_)
        rank(11); call from_npy(file, into, stat_, errmsg_)
        rank(12); call from_npy(file, into, stat_, errmsg_)
        rank(13); call from_npy(file, into, stat_, errmsg_)
        rank(14); call from_npy(file, into, stat_, errmsg_)
        rank(15); call from_npy(file, into, stat_, errmsg_)
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//' npy. Or pass format="text" or format="npy".'
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure from_textfile_r32


#ifdef IOFL_HAS_R128
  module procedure from_binaryfile_r128
    character(len=:), allocatable :: ext
    integer                       :: into_rank

    character(len=:), pointer :: errmsg_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    select rank(into)
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

    if ( size(data_shape) /= into_rank ) then
      stat_   = ARG_ERR
      errmsg_ = "RANK/SHAPE mismatch: size of data_shape must match rank of output array."
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    if ( format_of(file, format) == "npy" ) then
      stat_   = ARG_ERR
      errmsg_ = 'NumPy has no type for real128 data, so "'//file//'" cannot be a .npy file.'
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    else if ( format_of(file, format) == "binary" ) then
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
          stat_  = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Error reading file "'//file//'" with extension "'//ext//'". '// &
                "If attempting to read a text file, the data_shape argument must not be specified."// &
                "If attempting to read a binary file, the supported file extensions are: "//join(BINARY_EXT)
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure from_binaryfile_r128
#endif

  module procedure from_binaryfile_r64
    character(len=:), allocatable :: ext
    integer                       :: into_rank

    character(len=:), pointer :: errmsg_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    select rank(into)
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

    if ( size(data_shape) /= into_rank ) then
      stat_   = ARG_ERR
      errmsg_ = "RANK/SHAPE mismatch: size of data_shape must match rank of output array."
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    if ( format_of(file, format) == "npy" ) then
      select rank(into)
        rank(1)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(2)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(3)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(4)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(5)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(6)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(7)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(8)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(9)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(10)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(11)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(12)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(13)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(14)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(15)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
      end select
    else if ( format_of(file, format) == "binary" ) then
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
          stat_  = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Error reading file "'//file//'" with extension "'//ext//'". '// &
                "If attempting to read a text file, the data_shape argument must not be specified."// &
                "If attempting to read a binary file, the supported file extensions are: "//join(BINARY_EXT)
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure from_binaryfile_r64

  module procedure from_binaryfile_r32
    character(len=:), allocatable :: ext
    integer                       :: into_rank

    character(len=:), pointer :: errmsg_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    select rank(into)
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

    if ( size(data_shape) /= into_rank ) then
      stat_   = ARG_ERR
      errmsg_ = "RANK/SHAPE mismatch: size of data_shape must match rank of output array."
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    if ( format_of(file, format) == "npy" ) then
      select rank(into)
        rank(1)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(2)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(3)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(4)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(5)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(6)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(7)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(8)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(9)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(10)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(11)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(12)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(13)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(14)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(15)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
      end select
    else if ( format_of(file, format) == "binary" ) then
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
          stat_  = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Error reading file "'//file//'" with extension "'//ext//'". '// &
                "If attempting to read a text file, the data_shape argument must not be specified."// &
                "If attempting to read a binary file, the supported file extensions are: "//join(BINARY_EXT)
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure from_binaryfile_r32


  module procedure from_textfile_i64
    character(len=:), allocatable :: ext

    character(len=:), allocatable :: delim_, fmt_
    character(len=:), pointer :: errmsg_
    logical :: header_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    if ( format_of(file, format) == "text" ) then
      header_ = optval(header, ABSENT_HEADER)

      delim_ = optval(delim, COMMA_DELIMITER)

      if ( .not. present(fmt) ) then
        fmt_ = INT_FMT
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ = fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                    "into integer array. Format must be one of: "//join(INT_FMTS)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
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
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else if ( format_of(file, format) == "npy" ) then
      if ( present(header) ) write(*,"(a)") LF//'WARNING: header not supported for file type "npy".'
      if ( present(delim ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "npy".'
      if ( present(fmt   ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "npy".'

      select rank(into)
        rank(1); call from_npy(file, into, stat_, errmsg_)
        rank(2); call from_npy(file, into, stat_, errmsg_)
        rank(3); call from_npy(file, into, stat_, errmsg_)
        rank(4); call from_npy(file, into, stat_, errmsg_)
        rank(5); call from_npy(file, into, stat_, errmsg_)
        rank(6); call from_npy(file, into, stat_, errmsg_)
        rank(7); call from_npy(file, into, stat_, errmsg_)
        rank(8); call from_npy(file, into, stat_, errmsg_)
        rank(9); call from_npy(file, into, stat_, errmsg_)
        rank(10); call from_npy(file, into, stat_, errmsg_)
        rank(11); call from_npy(file, into, stat_, errmsg_)
        rank(12); call from_npy(file, into, stat_, errmsg_)
        rank(13); call from_npy(file, into, stat_, errmsg_)
        rank(14); call from_npy(file, into, stat_, errmsg_)
        rank(15); call from_npy(file, into, stat_, errmsg_)
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//' npy. Or pass format="text" or format="npy".'
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure from_textfile_i64

  module procedure from_textfile_i32
    character(len=:), allocatable :: ext

    character(len=:), allocatable :: delim_, fmt_
    character(len=:), pointer :: errmsg_
    logical :: header_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    if ( format_of(file, format) == "text" ) then
      header_ = optval(header, ABSENT_HEADER)

      delim_ = optval(delim, COMMA_DELIMITER)

      if ( .not. present(fmt) ) then
        fmt_ = INT_FMT
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ = fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                    "into integer array. Format must be one of: "//join(INT_FMTS)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
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
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else if ( format_of(file, format) == "npy" ) then
      if ( present(header) ) write(*,"(a)") LF//'WARNING: header not supported for file type "npy".'
      if ( present(delim ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "npy".'
      if ( present(fmt   ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "npy".'

      select rank(into)
        rank(1); call from_npy(file, into, stat_, errmsg_)
        rank(2); call from_npy(file, into, stat_, errmsg_)
        rank(3); call from_npy(file, into, stat_, errmsg_)
        rank(4); call from_npy(file, into, stat_, errmsg_)
        rank(5); call from_npy(file, into, stat_, errmsg_)
        rank(6); call from_npy(file, into, stat_, errmsg_)
        rank(7); call from_npy(file, into, stat_, errmsg_)
        rank(8); call from_npy(file, into, stat_, errmsg_)
        rank(9); call from_npy(file, into, stat_, errmsg_)
        rank(10); call from_npy(file, into, stat_, errmsg_)
        rank(11); call from_npy(file, into, stat_, errmsg_)
        rank(12); call from_npy(file, into, stat_, errmsg_)
        rank(13); call from_npy(file, into, stat_, errmsg_)
        rank(14); call from_npy(file, into, stat_, errmsg_)
        rank(15); call from_npy(file, into, stat_, errmsg_)
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//' npy. Or pass format="text" or format="npy".'
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure from_textfile_i32

  module procedure from_textfile_i16
    character(len=:), allocatable :: ext

    character(len=:), allocatable :: delim_, fmt_
    character(len=:), pointer :: errmsg_
    logical :: header_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    if ( format_of(file, format) == "text" ) then
      header_ = optval(header, ABSENT_HEADER)

      delim_ = optval(delim, COMMA_DELIMITER)

      if ( .not. present(fmt) ) then
        fmt_ = INT_FMT
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ = fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                    "into integer array. Format must be one of: "//join(INT_FMTS)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
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
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else if ( format_of(file, format) == "npy" ) then
      if ( present(header) ) write(*,"(a)") LF//'WARNING: header not supported for file type "npy".'
      if ( present(delim ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "npy".'
      if ( present(fmt   ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "npy".'

      select rank(into)
        rank(1); call from_npy(file, into, stat_, errmsg_)
        rank(2); call from_npy(file, into, stat_, errmsg_)
        rank(3); call from_npy(file, into, stat_, errmsg_)
        rank(4); call from_npy(file, into, stat_, errmsg_)
        rank(5); call from_npy(file, into, stat_, errmsg_)
        rank(6); call from_npy(file, into, stat_, errmsg_)
        rank(7); call from_npy(file, into, stat_, errmsg_)
        rank(8); call from_npy(file, into, stat_, errmsg_)
        rank(9); call from_npy(file, into, stat_, errmsg_)
        rank(10); call from_npy(file, into, stat_, errmsg_)
        rank(11); call from_npy(file, into, stat_, errmsg_)
        rank(12); call from_npy(file, into, stat_, errmsg_)
        rank(13); call from_npy(file, into, stat_, errmsg_)
        rank(14); call from_npy(file, into, stat_, errmsg_)
        rank(15); call from_npy(file, into, stat_, errmsg_)
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//' npy. Or pass format="text" or format="npy".'
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure from_textfile_i16

  module procedure from_textfile_i8
    character(len=:), allocatable :: ext

    character(len=:), allocatable :: delim_, fmt_
    character(len=:), pointer :: errmsg_
    logical :: header_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    if ( format_of(file, format) == "text" ) then
      header_ = optval(header, ABSENT_HEADER)

      delim_ = optval(delim, COMMA_DELIMITER)

      if ( .not. present(fmt) ) then
        fmt_ = INT_FMT
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ = fmt
        else
          stat_   = ARG_ERR
          errmsg_ = 'Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                    "into integer array. Format must be one of: "//join(INT_FMTS)
          call stop_if_unhandled(stat_, errmsg_, present(stat))
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
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else if ( format_of(file, format) == "npy" ) then
      if ( present(header) ) write(*,"(a)") LF//'WARNING: header not supported for file type "npy".'
      if ( present(delim ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "npy".'
      if ( present(fmt   ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "npy".'

      select rank(into)
        rank(1); call from_npy(file, into, stat_, errmsg_)
        rank(2); call from_npy(file, into, stat_, errmsg_)
        rank(3); call from_npy(file, into, stat_, errmsg_)
        rank(4); call from_npy(file, into, stat_, errmsg_)
        rank(5); call from_npy(file, into, stat_, errmsg_)
        rank(6); call from_npy(file, into, stat_, errmsg_)
        rank(7); call from_npy(file, into, stat_, errmsg_)
        rank(8); call from_npy(file, into, stat_, errmsg_)
        rank(9); call from_npy(file, into, stat_, errmsg_)
        rank(10); call from_npy(file, into, stat_, errmsg_)
        rank(11); call from_npy(file, into, stat_, errmsg_)
        rank(12); call from_npy(file, into, stat_, errmsg_)
        rank(13); call from_npy(file, into, stat_, errmsg_)
        rank(14); call from_npy(file, into, stat_, errmsg_)
        rank(15); call from_npy(file, into, stat_, errmsg_)
        rank default
          stat_   = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Unsupported file extension "'//ext//'" for file "'//file//'". Extension must be one of: '// &
                join(TEXT_EXT)//' npy. Or pass format="text" or format="npy".'
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure from_textfile_i8


  module procedure from_binaryfile_i64
    character(len=:), allocatable :: ext
    integer                       :: into_rank

    character(len=:), pointer :: errmsg_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    select rank(into)
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

    if ( size(data_shape) /= into_rank ) then
      stat_   = ARG_ERR
      errmsg_ = "RANK/SHAPE mismatch: size of data_shape must match rank of output array."
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    if ( format_of(file, format) == "npy" ) then
      select rank(into)
        rank(1)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(2)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(3)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(4)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(5)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(6)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(7)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(8)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(9)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(10)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(11)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(12)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(13)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(14)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(15)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
      end select
    else if ( format_of(file, format) == "binary" ) then
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
          stat_  = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Error reading file "'//file//'" with extension "'//ext//'". '// &
                "If attempting to read a text file, the data_shape argument must not be specified."// &
                "If attempting to read a binary file, the supported file extensions are: "//join(BINARY_EXT)
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure from_binaryfile_i64

  module procedure from_binaryfile_i32
    character(len=:), allocatable :: ext
    integer                       :: into_rank

    character(len=:), pointer :: errmsg_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    select rank(into)
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

    if ( size(data_shape) /= into_rank ) then
      stat_   = ARG_ERR
      errmsg_ = "RANK/SHAPE mismatch: size of data_shape must match rank of output array."
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    if ( format_of(file, format) == "npy" ) then
      select rank(into)
        rank(1)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(2)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(3)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(4)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(5)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(6)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(7)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(8)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(9)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(10)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(11)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(12)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(13)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(14)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(15)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
      end select
    else if ( format_of(file, format) == "binary" ) then
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
          stat_  = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Error reading file "'//file//'" with extension "'//ext//'". '// &
                "If attempting to read a text file, the data_shape argument must not be specified."// &
                "If attempting to read a binary file, the supported file extensions are: "//join(BINARY_EXT)
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure from_binaryfile_i32

  module procedure from_binaryfile_i16
    character(len=:), allocatable :: ext
    integer                       :: into_rank

    character(len=:), pointer :: errmsg_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    select rank(into)
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

    if ( size(data_shape) /= into_rank ) then
      stat_   = ARG_ERR
      errmsg_ = "RANK/SHAPE mismatch: size of data_shape must match rank of output array."
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    if ( format_of(file, format) == "npy" ) then
      select rank(into)
        rank(1)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(2)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(3)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(4)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(5)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(6)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(7)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(8)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(9)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(10)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(11)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(12)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(13)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(14)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(15)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
      end select
    else if ( format_of(file, format) == "binary" ) then
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
          stat_  = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Error reading file "'//file//'" with extension "'//ext//'". '// &
                "If attempting to read a text file, the data_shape argument must not be specified."// &
                "If attempting to read a binary file, the supported file extensions are: "//join(BINARY_EXT)
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure from_binaryfile_i16

  module procedure from_binaryfile_i8
    character(len=:), allocatable :: ext
    integer                       :: into_rank

    character(len=:), pointer :: errmsg_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
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

    select rank(into)
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

    if ( size(data_shape) /= into_rank ) then
      stat_   = ARG_ERR
      errmsg_ = "RANK/SHAPE mismatch: size of data_shape must match rank of output array."
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    if ( format_of(file, format) == "npy" ) then
      select rank(into)
        rank(1)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(2)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(3)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(4)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(5)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(6)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(7)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(8)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(9)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(10)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(11)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(12)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(13)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(14)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
        rank(15)
          call from_npy(file, into, stat_, errmsg_)
          if ( stat_ == 0 ) then
            if ( any(shape(into) /= data_shape) ) then
              stat_   = READ_ERR
              errmsg_ = 'Error reading file "'//file//'": its shape differs from data_shape.'
            end if
          end if
      end select
    else if ( format_of(file, format) == "binary" ) then
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
          stat_  = ARG_ERR
          errmsg_ = "Array has invalid rank. Supported ranks: 1-15."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
      end select
    else
      stat_   = ARG_ERR
      errmsg_ = 'Error reading file "'//file//'" with extension "'//ext//'". '// &
                "If attempting to read a text file, the data_shape argument must not be specified."// &
                "If attempting to read a binary file, the supported file extensions are: "//join(BINARY_EXT)
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure from_binaryfile_i8

end submodule file_io
