submodule (io_fortran_lib) file_io
  !---------------------------------------------------------------------------------------------------------------------
  !! This submodule provides module procedure implementations for the **public interfaces** `to_file` and
  !! `from_file`.
  !---------------------------------------------------------------------------------------------------------------------
  implicit none (type, external)

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
  module procedure to_file_1dc128
    character(len=:), allocatable :: header_(:)
    character(len=:), allocatable :: ext, locale_, delim_, fmt_, im_
    integer                       :: decimals_, hstat, dim_

    decimals_=0; hstat=0; dim_=0

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = [ EMPTY_STR ]
        hstat = 0
      else
        if ( (size(header, kind=i64) /= 1_i64) .and. (size(header, kind=i64) /= size(x, kind=i64)) ) then
          header_ = [ EMPTY_STR ]
          hstat = -1
          write(*,"(a)") LF//'WARNING: Invalid header for file "'//file//'".'// &
                   LF//'Header for this data must have size (1) or '// &
                     '('//str(size(x, kind=i64))//').'
        else
          header_ = header
          if ( size(header, kind=i64) == 1_i64 ) then
            hstat = 1
          else
            hstat = 2
          end if
        end if
      end if

      if ( .not. present(dim) ) then
        if ( hstat == 2 ) then
          dim_ = 2
        else
          dim_ = 1
        end if
      else
        if ( hstat == 2 ) then
          dim_ = 2
          if ( dim /= 2 ) then
            write(*,"(a)") LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                     file//'" for given header... defaulting to (2).'
          end if
        else
          if ( dim == 1 ) then
            dim_ = 1
          else if ( dim == 2 ) then
            dim_ = 2
          else
            dim_ = 1
            write(*,"(a)") LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                     file//'" for given header... defaulting to (1).'
          end if
        end if
      end if

      if ( .not. present(locale) ) then
        locale_ = "US"
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          locale_ = "US"
          write(*,"(a)") LF//'WARNING: Invalid locale "'//locale//'" for file "'//file//'". '// &
                     'Defaulting to US format.'// &
                   LF//'Locale must be one of: '//join(LOCALES)
        end if
      end if

      if ( .not. present(delim) ) then
        if ( dim_ == 1 ) then
          delim_ = EMPTY_STR
        else
          if ( locale_ == "US" ) then
            delim_ = COMMA
          else
            delim_ = SEMICOLON
          end if
        end if
      else
        if ( dim_ == 1 ) then
          delim_ = EMPTY_STR
        else
          delim_ = delim
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "e"
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          fmt_ = "e"
          write(*,"(a)") LF//'WARNING: Invalid format "'//fmt//'" for file "'//file//'". '// &
                     'Defaulting to exponential format.'// &
                   LF//'Format must be one of: '//join(REAL_FMTS)
        end if
      end if

      if ( .not. present(decimals) ) then
        decimals_ = 150
      else
        decimals_ = decimals
      end if

      if ( .not. present(im) ) then
        im_ = EMPTY_STR
      else
        im_ = trim(adjustl(im))
      end if

      call to_text( x=x, file=file, header=header_, dim=dim_, locale=locale_, delim=delim_, &
              fmt=fmt_, decimals=decimals_, im=im_ )
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(dim     ) ) write(*,"(a)") LF//'WARNING: dim not supported for file type "'//ext//'".'
      if ( present(locale  ) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'
      if ( present(decimals) ) write(*,"(a)") LF//'WARNING: decimals not supported for file type "'//ext//'".'
      if ( present(im      ) ) write(*,"(a)") LF//'WARNING: im not supported for file type "'//ext//'".'

      call to_binary(x=x, file=file)
    else
      write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                'due to unsupported file extension "'//ext//'".'// &
              LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
                join(BINARY_EXT)
    end if
  end procedure to_file_1dc128
  module procedure to_file_1dc64
    character(len=:), allocatable :: header_(:)
    character(len=:), allocatable :: ext, locale_, delim_, fmt_, im_
    integer                       :: decimals_, hstat, dim_

    decimals_=0; hstat=0; dim_=0

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = [ EMPTY_STR ]
        hstat = 0
      else
        if ( (size(header, kind=i64) /= 1_i64) .and. (size(header, kind=i64) /= size(x, kind=i64)) ) then
          header_ = [ EMPTY_STR ]
          hstat = -1
          write(*,"(a)") LF//'WARNING: Invalid header for file "'//file//'".'// &
                   LF//'Header for this data must have size (1) or '// &
                     '('//str(size(x, kind=i64))//').'
        else
          header_ = header
          if ( size(header, kind=i64) == 1_i64 ) then
            hstat = 1
          else
            hstat = 2
          end if
        end if
      end if

      if ( .not. present(dim) ) then
        if ( hstat == 2 ) then
          dim_ = 2
        else
          dim_ = 1
        end if
      else
        if ( hstat == 2 ) then
          dim_ = 2
          if ( dim /= 2 ) then
            write(*,"(a)") LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                     file//'" for given header... defaulting to (2).'
          end if
        else
          if ( dim == 1 ) then
            dim_ = 1
          else if ( dim == 2 ) then
            dim_ = 2
          else
            dim_ = 1
            write(*,"(a)") LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                     file//'" for given header... defaulting to (1).'
          end if
        end if
      end if

      if ( .not. present(locale) ) then
        locale_ = "US"
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          locale_ = "US"
          write(*,"(a)") LF//'WARNING: Invalid locale "'//locale//'" for file "'//file//'". '// &
                     'Defaulting to US format.'// &
                   LF//'Locale must be one of: '//join(LOCALES)
        end if
      end if

      if ( .not. present(delim) ) then
        if ( dim_ == 1 ) then
          delim_ = EMPTY_STR
        else
          if ( locale_ == "US" ) then
            delim_ = COMMA
          else
            delim_ = SEMICOLON
          end if
        end if
      else
        if ( dim_ == 1 ) then
          delim_ = EMPTY_STR
        else
          delim_ = delim
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "e"
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          fmt_ = "e"
          write(*,"(a)") LF//'WARNING: Invalid format "'//fmt//'" for file "'//file//'". '// &
                     'Defaulting to exponential format.'// &
                   LF//'Format must be one of: '//join(REAL_FMTS)
        end if
      end if

      if ( .not. present(decimals) ) then
        decimals_ = 150
      else
        decimals_ = decimals
      end if

      if ( .not. present(im) ) then
        im_ = EMPTY_STR
      else
        im_ = trim(adjustl(im))
      end if

      call to_text( x=x, file=file, header=header_, dim=dim_, locale=locale_, delim=delim_, &
              fmt=fmt_, decimals=decimals_, im=im_ )
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(dim     ) ) write(*,"(a)") LF//'WARNING: dim not supported for file type "'//ext//'".'
      if ( present(locale  ) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'
      if ( present(decimals) ) write(*,"(a)") LF//'WARNING: decimals not supported for file type "'//ext//'".'
      if ( present(im      ) ) write(*,"(a)") LF//'WARNING: im not supported for file type "'//ext//'".'

      call to_binary(x=x, file=file)
    else
      write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                'due to unsupported file extension "'//ext//'".'// &
              LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
                join(BINARY_EXT)
    end if
  end procedure to_file_1dc64
  module procedure to_file_1dc32
    character(len=:), allocatable :: header_(:)
    character(len=:), allocatable :: ext, locale_, delim_, fmt_, im_
    integer                       :: decimals_, hstat, dim_

    decimals_=0; hstat=0; dim_=0

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = [ EMPTY_STR ]
        hstat = 0
      else
        if ( (size(header, kind=i64) /= 1_i64) .and. (size(header, kind=i64) /= size(x, kind=i64)) ) then
          header_ = [ EMPTY_STR ]
          hstat = -1
          write(*,"(a)") LF//'WARNING: Invalid header for file "'//file//'".'// &
                   LF//'Header for this data must have size (1) or '// &
                     '('//str(size(x, kind=i64))//').'
        else
          header_ = header
          if ( size(header, kind=i64) == 1_i64 ) then
            hstat = 1
          else
            hstat = 2
          end if
        end if
      end if

      if ( .not. present(dim) ) then
        if ( hstat == 2 ) then
          dim_ = 2
        else
          dim_ = 1
        end if
      else
        if ( hstat == 2 ) then
          dim_ = 2
          if ( dim /= 2 ) then
            write(*,"(a)") LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                     file//'" for given header... defaulting to (2).'
          end if
        else
          if ( dim == 1 ) then
            dim_ = 1
          else if ( dim == 2 ) then
            dim_ = 2
          else
            dim_ = 1
            write(*,"(a)") LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                     file//'" for given header... defaulting to (1).'
          end if
        end if
      end if

      if ( .not. present(locale) ) then
        locale_ = "US"
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          locale_ = "US"
          write(*,"(a)") LF//'WARNING: Invalid locale "'//locale//'" for file "'//file//'". '// &
                     'Defaulting to US format.'// &
                   LF//'Locale must be one of: '//join(LOCALES)
        end if
      end if

      if ( .not. present(delim) ) then
        if ( dim_ == 1 ) then
          delim_ = EMPTY_STR
        else
          if ( locale_ == "US" ) then
            delim_ = COMMA
          else
            delim_ = SEMICOLON
          end if
        end if
      else
        if ( dim_ == 1 ) then
          delim_ = EMPTY_STR
        else
          delim_ = delim
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "e"
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          fmt_ = "e"
          write(*,"(a)") LF//'WARNING: Invalid format "'//fmt//'" for file "'//file//'". '// &
                     'Defaulting to exponential format.'// &
                   LF//'Format must be one of: '//join(REAL_FMTS)
        end if
      end if

      if ( .not. present(decimals) ) then
        decimals_ = 150
      else
        decimals_ = decimals
      end if

      if ( .not. present(im) ) then
        im_ = EMPTY_STR
      else
        im_ = trim(adjustl(im))
      end if

      call to_text( x=x, file=file, header=header_, dim=dim_, locale=locale_, delim=delim_, &
              fmt=fmt_, decimals=decimals_, im=im_ )
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(dim     ) ) write(*,"(a)") LF//'WARNING: dim not supported for file type "'//ext//'".'
      if ( present(locale  ) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'
      if ( present(decimals) ) write(*,"(a)") LF//'WARNING: decimals not supported for file type "'//ext//'".'
      if ( present(im      ) ) write(*,"(a)") LF//'WARNING: im not supported for file type "'//ext//'".'

      call to_binary(x=x, file=file)
    else
      write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                'due to unsupported file extension "'//ext//'".'// &
              LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
                join(BINARY_EXT)
    end if
  end procedure to_file_1dc32

  module procedure to_file_2dc128
    character(len=:), allocatable :: header_(:)
    character(len=:), allocatable :: ext, locale_, delim_, fmt_, im_
    integer                       :: decimals_

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = [ EMPTY_STR ]
      else
        if ( (size(header, kind=i64) /= 1_i64) .and. (size(header, kind=i64) /= size(x,dim=2, kind=i64)) ) then
          header_ = [ EMPTY_STR ]
          write(*,"(a)") LF//'WARNING: Invalid header for file "'//file//'".'// &
                   LF//'Header for this data must have size (1) or '// &
                     '('//str(size(x, dim=2, kind=i64))//').'
        else
          header_ = header
        end if
      end if

      if ( .not. present(locale) ) then
        locale_ = "US"
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          locale_ = "US"
          write(*,"(a)") LF//'WARNING: Invalid locale "'//locale//'" for file "'//file//'". '// &
                     'Defaulting to US format.'// &
                   LF//'Locale must be one of: '//join(LOCALES)
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == "US" ) then
          delim_ = COMMA
        else
          delim_ = SEMICOLON
        end if
      else
        delim_ = delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "e"
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          fmt_ = "e"
          write(*,"(a)") LF//'WARNING: Invalid format "'//fmt//'" for file "'//file//'". '// &
                     'Defaulting to exponential format.'// &
                   LF//'Format must be one of: '//join(REAL_FMTS)
        end if
      end if

      if ( .not. present(decimals) ) then
        decimals_ = 150
      else
        decimals_ = decimals
      end if

      if ( .not. present(im) ) then
        im_ = EMPTY_STR
      else
        im_ = trim(adjustl(im))
      end if

      call to_text( x=x, file=file, header=header_, locale=locale_, delim=delim_, &
              fmt=fmt_, decimals=decimals_, im=im_ )
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(locale  ) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'
      if ( present(decimals) ) write(*,"(a)") LF//'WARNING: decimals not supported for file type "'//ext//'".'
      if ( present(im      ) ) write(*,"(a)") LF//'WARNING: im not supported for file type "'//ext//'".'

      call to_binary(x=x, file=file)
    else
      write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                'due to unsupported file extension "'//ext//'".'// &
              LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
                join(BINARY_EXT)
    end if
  end procedure to_file_2dc128
  module procedure to_file_2dc64
    character(len=:), allocatable :: header_(:)
    character(len=:), allocatable :: ext, locale_, delim_, fmt_, im_
    integer                       :: decimals_

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = [ EMPTY_STR ]
      else
        if ( (size(header, kind=i64) /= 1_i64) .and. (size(header, kind=i64) /= size(x,dim=2, kind=i64)) ) then
          header_ = [ EMPTY_STR ]
          write(*,"(a)") LF//'WARNING: Invalid header for file "'//file//'".'// &
                   LF//'Header for this data must have size (1) or '// &
                     '('//str(size(x, dim=2, kind=i64))//').'
        else
          header_ = header
        end if
      end if

      if ( .not. present(locale) ) then
        locale_ = "US"
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          locale_ = "US"
          write(*,"(a)") LF//'WARNING: Invalid locale "'//locale//'" for file "'//file//'". '// &
                     'Defaulting to US format.'// &
                   LF//'Locale must be one of: '//join(LOCALES)
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == "US" ) then
          delim_ = COMMA
        else
          delim_ = SEMICOLON
        end if
      else
        delim_ = delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "e"
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          fmt_ = "e"
          write(*,"(a)") LF//'WARNING: Invalid format "'//fmt//'" for file "'//file//'". '// &
                     'Defaulting to exponential format.'// &
                   LF//'Format must be one of: '//join(REAL_FMTS)
        end if
      end if

      if ( .not. present(decimals) ) then
        decimals_ = 150
      else
        decimals_ = decimals
      end if

      if ( .not. present(im) ) then
        im_ = EMPTY_STR
      else
        im_ = trim(adjustl(im))
      end if

      call to_text( x=x, file=file, header=header_, locale=locale_, delim=delim_, &
              fmt=fmt_, decimals=decimals_, im=im_ )
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(locale  ) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'
      if ( present(decimals) ) write(*,"(a)") LF//'WARNING: decimals not supported for file type "'//ext//'".'
      if ( present(im      ) ) write(*,"(a)") LF//'WARNING: im not supported for file type "'//ext//'".'

      call to_binary(x=x, file=file)
    else
      write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                'due to unsupported file extension "'//ext//'".'// &
              LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
                join(BINARY_EXT)
    end if
  end procedure to_file_2dc64
  module procedure to_file_2dc32
    character(len=:), allocatable :: header_(:)
    character(len=:), allocatable :: ext, locale_, delim_, fmt_, im_
    integer                       :: decimals_

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = [ EMPTY_STR ]
      else
        if ( (size(header, kind=i64) /= 1_i64) .and. (size(header, kind=i64) /= size(x,dim=2, kind=i64)) ) then
          header_ = [ EMPTY_STR ]
          write(*,"(a)") LF//'WARNING: Invalid header for file "'//file//'".'// &
                   LF//'Header for this data must have size (1) or '// &
                     '('//str(size(x, dim=2, kind=i64))//').'
        else
          header_ = header
        end if
      end if

      if ( .not. present(locale) ) then
        locale_ = "US"
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          locale_ = "US"
          write(*,"(a)") LF//'WARNING: Invalid locale "'//locale//'" for file "'//file//'". '// &
                     'Defaulting to US format.'// &
                   LF//'Locale must be one of: '//join(LOCALES)
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == "US" ) then
          delim_ = COMMA
        else
          delim_ = SEMICOLON
        end if
      else
        delim_ = delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "e"
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          fmt_ = "e"
          write(*,"(a)") LF//'WARNING: Invalid format "'//fmt//'" for file "'//file//'". '// &
                     'Defaulting to exponential format.'// &
                   LF//'Format must be one of: '//join(REAL_FMTS)
        end if
      end if

      if ( .not. present(decimals) ) then
        decimals_ = 150
      else
        decimals_ = decimals
      end if

      if ( .not. present(im) ) then
        im_ = EMPTY_STR
      else
        im_ = trim(adjustl(im))
      end if

      call to_text( x=x, file=file, header=header_, locale=locale_, delim=delim_, &
              fmt=fmt_, decimals=decimals_, im=im_ )
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(locale  ) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'
      if ( present(decimals) ) write(*,"(a)") LF//'WARNING: decimals not supported for file type "'//ext//'".'
      if ( present(im      ) ) write(*,"(a)") LF//'WARNING: im not supported for file type "'//ext//'".'

      call to_binary(x=x, file=file)
    else
      write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                'due to unsupported file extension "'//ext//'".'// &
              LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
                join(BINARY_EXT)
    end if
  end procedure to_file_2dc32

  module procedure to_file_3dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_3dc128
  module procedure to_file_3dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_3dc64
  module procedure to_file_3dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_3dc32

  module procedure to_file_4dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_4dc128
  module procedure to_file_4dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_4dc64
  module procedure to_file_4dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_4dc32

  module procedure to_file_5dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_5dc128
  module procedure to_file_5dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_5dc64
  module procedure to_file_5dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_5dc32

  module procedure to_file_6dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_6dc128
  module procedure to_file_6dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_6dc64
  module procedure to_file_6dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_6dc32

  module procedure to_file_7dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_7dc128
  module procedure to_file_7dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_7dc64
  module procedure to_file_7dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_7dc32

  module procedure to_file_8dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_8dc128
  module procedure to_file_8dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_8dc64
  module procedure to_file_8dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_8dc32

  module procedure to_file_9dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_9dc128
  module procedure to_file_9dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_9dc64
  module procedure to_file_9dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_9dc32

  module procedure to_file_10dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_10dc128
  module procedure to_file_10dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_10dc64
  module procedure to_file_10dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_10dc32

  module procedure to_file_11dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_11dc128
  module procedure to_file_11dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_11dc64
  module procedure to_file_11dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_11dc32

  module procedure to_file_12dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_12dc128
  module procedure to_file_12dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_12dc64
  module procedure to_file_12dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_12dc32

  module procedure to_file_13dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_13dc128
  module procedure to_file_13dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_13dc64
  module procedure to_file_13dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_13dc32

  module procedure to_file_14dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_14dc128
  module procedure to_file_14dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_14dc64
  module procedure to_file_14dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_14dc32

  module procedure to_file_15dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_15dc128
  module procedure to_file_15dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_15dc64
  module procedure to_file_15dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_15dc32

  module procedure to_file_1dr128
    character(len=:), allocatable :: header_(:)
    character(len=:), allocatable :: ext, locale_, delim_, fmt_
    integer                       :: decimals_, hstat, dim_

    decimals_=0; hstat=0; dim_=0

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = [ EMPTY_STR ]
        hstat = 0
      else
        if ( (size(header, kind=i64) /= 1_i64) .and. (size(header, kind=i64) /= size(x, kind=i64)) ) then
          header_ = [ EMPTY_STR ]
          hstat = -1
          write(*,"(a)") LF//'WARNING: Invalid header for file "'//file//'".'// &
                   LF//'Header for this data must have size (1) or '// &
                     '('//str(size(x, kind=i64))//').'
        else
          header_ = header
          if ( size(header, kind=i64) == 1_i64 ) then
            hstat = 1
          else
            hstat = 2
          end if
        end if
      end if

      if ( .not. present(dim) ) then
        if ( hstat == 2 ) then
          dim_ = 2
        else
          dim_ = 1
        end if
      else
        if ( hstat == 2 ) then
          dim_ = 2
          if ( dim /= 2 ) then
            write(*,"(a)") LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                     file//'" for given header... defaulting to (2).'
          end if
        else
          if ( dim == 1 ) then
            dim_ = 1
          else if ( dim == 2 ) then
            dim_ = 2
          else
            dim_ = 1
            write(*,"(a)") LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                     file//'" for given header... defaulting to (1).'
          end if
        end if
      end if

      if ( .not. present(locale) ) then
        locale_ = "US"
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          locale_ = "US"
          write(*,"(a)") LF//'WARNING: Invalid locale "'//locale//'" for file "'//file//'". '// &
                     'Defaulting to US format.'// &
                   LF//'Locale must be one of: '//join(LOCALES)
        end if
      end if

      if ( .not. present(delim) ) then
        if ( dim_ == 1 ) then
          delim_ = EMPTY_STR
        else
          if ( locale_ == "US" ) then
            delim_ = COMMA
          else
            delim_ = SEMICOLON
          end if
        end if
      else
        if ( dim_ == 1 ) then
          delim_ = EMPTY_STR
        else
          delim_ = delim
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "e"
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          fmt_ = "e"
          write(*,"(a)") LF//'WARNING: Invalid format "'//fmt//'" for file "'//file//'". '// &
                     'Defaulting to exponential format.'// &
                   LF//'Format must be one of: '//join(REAL_FMTS)
        end if
      end if

      if ( .not. present(decimals) ) then
        decimals_ = 150
      else
        decimals_ = decimals
      end if

      call to_text( x=x, file=file, header=header_, dim=dim_, locale=locale_, delim=delim_, &
              fmt=fmt_, decimals=decimals_ )
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(dim     ) ) write(*,"(a)") LF//'WARNING: dim not supported for file type "'//ext//'".'
      if ( present(locale  ) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'
      if ( present(decimals) ) write(*,"(a)") LF//'WARNING: decimals not supported for file type "'//ext//'".'

      call to_binary(x=x, file=file)
    else
      write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                'due to unsupported file extension "'//ext//'".'// &
              LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
                join(BINARY_EXT)
    end if
  end procedure to_file_1dr128
  module procedure to_file_1dr64
    character(len=:), allocatable :: header_(:)
    character(len=:), allocatable :: ext, locale_, delim_, fmt_
    integer                       :: decimals_, hstat, dim_

    decimals_=0; hstat=0; dim_=0

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = [ EMPTY_STR ]
        hstat = 0
      else
        if ( (size(header, kind=i64) /= 1_i64) .and. (size(header, kind=i64) /= size(x, kind=i64)) ) then
          header_ = [ EMPTY_STR ]
          hstat = -1
          write(*,"(a)") LF//'WARNING: Invalid header for file "'//file//'".'// &
                   LF//'Header for this data must have size (1) or '// &
                     '('//str(size(x, kind=i64))//').'
        else
          header_ = header
          if ( size(header, kind=i64) == 1_i64 ) then
            hstat = 1
          else
            hstat = 2
          end if
        end if
      end if

      if ( .not. present(dim) ) then
        if ( hstat == 2 ) then
          dim_ = 2
        else
          dim_ = 1
        end if
      else
        if ( hstat == 2 ) then
          dim_ = 2
          if ( dim /= 2 ) then
            write(*,"(a)") LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                     file//'" for given header... defaulting to (2).'
          end if
        else
          if ( dim == 1 ) then
            dim_ = 1
          else if ( dim == 2 ) then
            dim_ = 2
          else
            dim_ = 1
            write(*,"(a)") LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                     file//'" for given header... defaulting to (1).'
          end if
        end if
      end if

      if ( .not. present(locale) ) then
        locale_ = "US"
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          locale_ = "US"
          write(*,"(a)") LF//'WARNING: Invalid locale "'//locale//'" for file "'//file//'". '// &
                     'Defaulting to US format.'// &
                   LF//'Locale must be one of: '//join(LOCALES)
        end if
      end if

      if ( .not. present(delim) ) then
        if ( dim_ == 1 ) then
          delim_ = EMPTY_STR
        else
          if ( locale_ == "US" ) then
            delim_ = COMMA
          else
            delim_ = SEMICOLON
          end if
        end if
      else
        if ( dim_ == 1 ) then
          delim_ = EMPTY_STR
        else
          delim_ = delim
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "e"
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          fmt_ = "e"
          write(*,"(a)") LF//'WARNING: Invalid format "'//fmt//'" for file "'//file//'". '// &
                     'Defaulting to exponential format.'// &
                   LF//'Format must be one of: '//join(REAL_FMTS)
        end if
      end if

      if ( .not. present(decimals) ) then
        decimals_ = 150
      else
        decimals_ = decimals
      end if

      call to_text( x=x, file=file, header=header_, dim=dim_, locale=locale_, delim=delim_, &
              fmt=fmt_, decimals=decimals_ )
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(dim     ) ) write(*,"(a)") LF//'WARNING: dim not supported for file type "'//ext//'".'
      if ( present(locale  ) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'
      if ( present(decimals) ) write(*,"(a)") LF//'WARNING: decimals not supported for file type "'//ext//'".'

      call to_binary(x=x, file=file)
    else
      write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                'due to unsupported file extension "'//ext//'".'// &
              LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
                join(BINARY_EXT)
    end if
  end procedure to_file_1dr64
  module procedure to_file_1dr32
    character(len=:), allocatable :: header_(:)
    character(len=:), allocatable :: ext, locale_, delim_, fmt_
    integer                       :: decimals_, hstat, dim_

    decimals_=0; hstat=0; dim_=0

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = [ EMPTY_STR ]
        hstat = 0
      else
        if ( (size(header, kind=i64) /= 1_i64) .and. (size(header, kind=i64) /= size(x, kind=i64)) ) then
          header_ = [ EMPTY_STR ]
          hstat = -1
          write(*,"(a)") LF//'WARNING: Invalid header for file "'//file//'".'// &
                   LF//'Header for this data must have size (1) or '// &
                     '('//str(size(x, kind=i64))//').'
        else
          header_ = header
          if ( size(header, kind=i64) == 1_i64 ) then
            hstat = 1
          else
            hstat = 2
          end if
        end if
      end if

      if ( .not. present(dim) ) then
        if ( hstat == 2 ) then
          dim_ = 2
        else
          dim_ = 1
        end if
      else
        if ( hstat == 2 ) then
          dim_ = 2
          if ( dim /= 2 ) then
            write(*,"(a)") LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                     file//'" for given header... defaulting to (2).'
          end if
        else
          if ( dim == 1 ) then
            dim_ = 1
          else if ( dim == 2 ) then
            dim_ = 2
          else
            dim_ = 1
            write(*,"(a)") LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                     file//'" for given header... defaulting to (1).'
          end if
        end if
      end if

      if ( .not. present(locale) ) then
        locale_ = "US"
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          locale_ = "US"
          write(*,"(a)") LF//'WARNING: Invalid locale "'//locale//'" for file "'//file//'". '// &
                     'Defaulting to US format.'// &
                   LF//'Locale must be one of: '//join(LOCALES)
        end if
      end if

      if ( .not. present(delim) ) then
        if ( dim_ == 1 ) then
          delim_ = EMPTY_STR
        else
          if ( locale_ == "US" ) then
            delim_ = COMMA
          else
            delim_ = SEMICOLON
          end if
        end if
      else
        if ( dim_ == 1 ) then
          delim_ = EMPTY_STR
        else
          delim_ = delim
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "e"
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          fmt_ = "e"
          write(*,"(a)") LF//'WARNING: Invalid format "'//fmt//'" for file "'//file//'". '// &
                     'Defaulting to exponential format.'// &
                   LF//'Format must be one of: '//join(REAL_FMTS)
        end if
      end if

      if ( .not. present(decimals) ) then
        decimals_ = 150
      else
        decimals_ = decimals
      end if

      call to_text( x=x, file=file, header=header_, dim=dim_, locale=locale_, delim=delim_, &
              fmt=fmt_, decimals=decimals_ )
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(dim     ) ) write(*,"(a)") LF//'WARNING: dim not supported for file type "'//ext//'".'
      if ( present(locale  ) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'
      if ( present(decimals) ) write(*,"(a)") LF//'WARNING: decimals not supported for file type "'//ext//'".'

      call to_binary(x=x, file=file)
    else
      write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                'due to unsupported file extension "'//ext//'".'// &
              LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
                join(BINARY_EXT)
    end if
  end procedure to_file_1dr32

  module procedure to_file_2dr128
    character(len=:), allocatable :: header_(:)
    character(len=:), allocatable :: ext, locale_, delim_, fmt_
    integer                       :: decimals_

    decimals_=0

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = [ EMPTY_STR ]
      else
        if ( (size(header, kind=i64) /= 1_i64) .and. (size(header, kind=i64) /= size(x,dim=2, kind=i64)) ) then
          header_ = [ EMPTY_STR ]
          write(*,"(a)") LF//'WARNING: Invalid header for file "'//file//'".'// &
                   LF//'Header for this data must have size (1) or '// &
                     '('//str(size(x, dim=2, kind=i64))//').'
        else
          header_ = header
        end if
      end if

      if ( .not. present(locale) ) then
        locale_ = "US"
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          locale_ = "US"
          write(*,"(a)") LF//'WARNING: Invalid locale "'//locale//'" for file "'//file//'". '// &
                     'Defaulting to US format.'// &
                   LF//'Locale must be one of: '//join(LOCALES)
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == "US" ) then
          delim_ = COMMA
        else
          delim_ = SEMICOLON
        end if
      else
        delim_ = delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "e"
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          fmt_ = "e"
          write(*,"(a)") LF//'WARNING: Invalid format "'//fmt//'" for file "'//file//'". '// &
                     'Defaulting to exponential format.'// &
                   LF//'Format must be one of: '//join(REAL_FMTS)
        end if
      end if

      if ( .not. present(decimals) ) then
        decimals_ = 150
      else
        decimals_ = decimals
      end if

      call to_text( x=x, file=file, header=header_, locale=locale_, delim=delim_, &
              fmt=fmt_, decimals=decimals_ )
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(locale  ) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'
      if ( present(decimals) ) write(*,"(a)") LF//'WARNING: decimals not supported for file type "'//ext//'".'

      call to_binary(x=x, file=file)
    else
      write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                'due to unsupported file extension "'//ext//'".'// &
              LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
                join(BINARY_EXT)
    end if
  end procedure to_file_2dr128
  module procedure to_file_2dr64
    character(len=:), allocatable :: header_(:)
    character(len=:), allocatable :: ext, locale_, delim_, fmt_
    integer                       :: decimals_

    decimals_=0

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = [ EMPTY_STR ]
      else
        if ( (size(header, kind=i64) /= 1_i64) .and. (size(header, kind=i64) /= size(x,dim=2, kind=i64)) ) then
          header_ = [ EMPTY_STR ]
          write(*,"(a)") LF//'WARNING: Invalid header for file "'//file//'".'// &
                   LF//'Header for this data must have size (1) or '// &
                     '('//str(size(x, dim=2, kind=i64))//').'
        else
          header_ = header
        end if
      end if

      if ( .not. present(locale) ) then
        locale_ = "US"
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          locale_ = "US"
          write(*,"(a)") LF//'WARNING: Invalid locale "'//locale//'" for file "'//file//'". '// &
                     'Defaulting to US format.'// &
                   LF//'Locale must be one of: '//join(LOCALES)
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == "US" ) then
          delim_ = COMMA
        else
          delim_ = SEMICOLON
        end if
      else
        delim_ = delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "e"
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          fmt_ = "e"
          write(*,"(a)") LF//'WARNING: Invalid format "'//fmt//'" for file "'//file//'". '// &
                     'Defaulting to exponential format.'// &
                   LF//'Format must be one of: '//join(REAL_FMTS)
        end if
      end if

      if ( .not. present(decimals) ) then
        decimals_ = 150
      else
        decimals_ = decimals
      end if

      call to_text( x=x, file=file, header=header_, locale=locale_, delim=delim_, &
              fmt=fmt_, decimals=decimals_ )
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(locale  ) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'
      if ( present(decimals) ) write(*,"(a)") LF//'WARNING: decimals not supported for file type "'//ext//'".'

      call to_binary(x=x, file=file)
    else
      write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                'due to unsupported file extension "'//ext//'".'// &
              LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
                join(BINARY_EXT)
    end if
  end procedure to_file_2dr64
  module procedure to_file_2dr32
    character(len=:), allocatable :: header_(:)
    character(len=:), allocatable :: ext, locale_, delim_, fmt_
    integer                       :: decimals_

    decimals_=0

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = [ EMPTY_STR ]
      else
        if ( (size(header, kind=i64) /= 1_i64) .and. (size(header, kind=i64) /= size(x,dim=2, kind=i64)) ) then
          header_ = [ EMPTY_STR ]
          write(*,"(a)") LF//'WARNING: Invalid header for file "'//file//'".'// &
                   LF//'Header for this data must have size (1) or '// &
                     '('//str(size(x, dim=2, kind=i64))//').'
        else
          header_ = header
        end if
      end if

      if ( .not. present(locale) ) then
        locale_ = "US"
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          locale_ = "US"
          write(*,"(a)") LF//'WARNING: Invalid locale "'//locale//'" for file "'//file//'". '// &
                     'Defaulting to US format.'// &
                   LF//'Locale must be one of: '//join(LOCALES)
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == "US" ) then
          delim_ = COMMA
        else
          delim_ = SEMICOLON
        end if
      else
        delim_ = delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "e"
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          fmt_ = "e"
          write(*,"(a)") LF//'WARNING: Invalid format "'//fmt//'" for file "'//file//'". '// &
                     'Defaulting to exponential format.'// &
                   LF//'Format must be one of: '//join(REAL_FMTS)
        end if
      end if

      if ( .not. present(decimals) ) then
        decimals_ = 150
      else
        decimals_ = decimals
      end if

      call to_text( x=x, file=file, header=header_, locale=locale_, delim=delim_, &
              fmt=fmt_, decimals=decimals_ )
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header  ) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(locale  ) ) write(*,"(a)") LF//'WARNING: locale not supported for file type "'//ext//'".'
      if ( present(delim   ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt     ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'
      if ( present(decimals) ) write(*,"(a)") LF//'WARNING: decimals not supported for file type "'//ext//'".'

      call to_binary(x=x, file=file)
    else
      write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                'due to unsupported file extension "'//ext//'".'// &
              LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
                join(BINARY_EXT)
    end if
  end procedure to_file_2dr32

  module procedure to_file_3dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_3dr128
  module procedure to_file_3dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_3dr64
  module procedure to_file_3dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_3dr32

  module procedure to_file_4dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_4dr128
  module procedure to_file_4dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_4dr64
  module procedure to_file_4dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_4dr32

  module procedure to_file_5dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_5dr128
  module procedure to_file_5dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_5dr64
  module procedure to_file_5dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_5dr32

  module procedure to_file_6dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_6dr128
  module procedure to_file_6dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_6dr64
  module procedure to_file_6dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_6dr32

  module procedure to_file_7dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_7dr128
  module procedure to_file_7dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_7dr64
  module procedure to_file_7dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_7dr32

  module procedure to_file_8dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_8dr128
  module procedure to_file_8dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_8dr64
  module procedure to_file_8dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_8dr32

  module procedure to_file_9dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_9dr128
  module procedure to_file_9dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_9dr64
  module procedure to_file_9dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_9dr32

  module procedure to_file_10dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_10dr128
  module procedure to_file_10dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_10dr64
  module procedure to_file_10dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_10dr32

  module procedure to_file_11dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_11dr128
  module procedure to_file_11dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_11dr64
  module procedure to_file_11dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_11dr32

  module procedure to_file_12dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_12dr128
  module procedure to_file_12dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_12dr64
  module procedure to_file_12dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_12dr32

  module procedure to_file_13dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_13dr128
  module procedure to_file_13dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_13dr64
  module procedure to_file_13dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_13dr32

  module procedure to_file_14dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_14dr128
  module procedure to_file_14dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_14dr64
  module procedure to_file_14dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_14dr32

  module procedure to_file_15dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_15dr128
  module procedure to_file_15dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_15dr64
  module procedure to_file_15dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_15dr32

  module procedure to_file_1di64
    character(len=:), allocatable :: ext, delim_, fmt_
    character(len=:), allocatable :: header_(:)
    integer                       :: hstat, dim_

    hstat=0; dim_=0

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = [ EMPTY_STR ]
        hstat = 0
      else
        if ( (size(header, kind=i64) /= 1_i64) .and. (size(header, kind=i64) /= size(x, kind=i64)) ) then
          header_ = [ EMPTY_STR ]
          hstat = -1
          write(*,"(a)") LF//'WARNING: Invalid header for file "'//file//'".'// &
                   LF//'Header for this data must have size (1) or '// &
                     '('//str(size(x, kind=i64))//').'
        else
          header_ = header
          if ( size(header, kind=i64) == 1_i64 ) then
            hstat = 1
          else
            hstat = 2
          end if
        end if
      end if

      if ( .not. present(dim) ) then
        if ( hstat == 2 ) then
          dim_ = 2
        else
          dim_ = 1
        end if
      else
        if ( hstat == 2 ) then
          dim_ = 2
          if ( dim /= 2 ) then
            write(*,"(a)") LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                     file//'" for given header... defaulting to (2).'
          end if
        else
          if ( dim == 1 ) then
            dim_ = 1
          else if ( dim == 2 ) then
            dim_ = 2
          else
            dim_ = 1
            write(*,"(a)") LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                     file//'" for given header... defaulting to (1).'
          end if
        end if
      end if

      if ( .not. present(delim) ) then
        if ( dim_ == 1 ) then
          delim_ = EMPTY_STR
        else
          delim_ = COMMA
        end if
      else
        if ( dim_ == 1 ) then
          delim_ = EMPTY_STR
        else
          delim_ = delim
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "i"
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ = fmt
        else
          fmt_ = "i"
          write(*,"(a)") LF//'WARNING: Invalid format "'//fmt//'" for file "'//file//'". '// &
                     'Defaulting to integer format.'// &
                   LF//'Format must be one of: '//join(INT_FMTS)
        end if
      end if

      call to_text(x=x, file=file, header=header_, dim=dim_, delim=delim_, fmt=fmt_)
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(dim   ) ) write(*,"(a)") LF//'WARNING: dim not supported for file type "'//ext//'".'
      if ( present(delim ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt   ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'

      call to_binary(x=x, file=file)
    else
      write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                'due to unsupported file extension "'//ext//'".'// &
              LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
                join(BINARY_EXT)
    end if
  end procedure to_file_1di64
  module procedure to_file_1di32
    character(len=:), allocatable :: ext, delim_, fmt_
    character(len=:), allocatable :: header_(:)
    integer                       :: hstat, dim_

    hstat=0; dim_=0

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = [ EMPTY_STR ]
        hstat = 0
      else
        if ( (size(header, kind=i64) /= 1_i64) .and. (size(header, kind=i64) /= size(x, kind=i64)) ) then
          header_ = [ EMPTY_STR ]
          hstat = -1
          write(*,"(a)") LF//'WARNING: Invalid header for file "'//file//'".'// &
                   LF//'Header for this data must have size (1) or '// &
                     '('//str(size(x, kind=i64))//').'
        else
          header_ = header
          if ( size(header, kind=i64) == 1_i64 ) then
            hstat = 1
          else
            hstat = 2
          end if
        end if
      end if

      if ( .not. present(dim) ) then
        if ( hstat == 2 ) then
          dim_ = 2
        else
          dim_ = 1
        end if
      else
        if ( hstat == 2 ) then
          dim_ = 2
          if ( dim /= 2 ) then
            write(*,"(a)") LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                     file//'" for given header... defaulting to (2).'
          end if
        else
          if ( dim == 1 ) then
            dim_ = 1
          else if ( dim == 2 ) then
            dim_ = 2
          else
            dim_ = 1
            write(*,"(a)") LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                     file//'" for given header... defaulting to (1).'
          end if
        end if
      end if

      if ( .not. present(delim) ) then
        if ( dim_ == 1 ) then
          delim_ = EMPTY_STR
        else
          delim_ = COMMA
        end if
      else
        if ( dim_ == 1 ) then
          delim_ = EMPTY_STR
        else
          delim_ = delim
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "i"
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ = fmt
        else
          fmt_ = "i"
          write(*,"(a)") LF//'WARNING: Invalid format "'//fmt//'" for file "'//file//'". '// &
                     'Defaulting to integer format.'// &
                   LF//'Format must be one of: '//join(INT_FMTS)
        end if
      end if

      call to_text(x=x, file=file, header=header_, dim=dim_, delim=delim_, fmt=fmt_)
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(dim   ) ) write(*,"(a)") LF//'WARNING: dim not supported for file type "'//ext//'".'
      if ( present(delim ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt   ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'

      call to_binary(x=x, file=file)
    else
      write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                'due to unsupported file extension "'//ext//'".'// &
              LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
                join(BINARY_EXT)
    end if
  end procedure to_file_1di32
  module procedure to_file_1di16
    character(len=:), allocatable :: ext, delim_, fmt_
    character(len=:), allocatable :: header_(:)
    integer                       :: hstat, dim_

    hstat=0; dim_=0

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = [ EMPTY_STR ]
        hstat = 0
      else
        if ( (size(header, kind=i64) /= 1_i64) .and. (size(header, kind=i64) /= size(x, kind=i64)) ) then
          header_ = [ EMPTY_STR ]
          hstat = -1
          write(*,"(a)") LF//'WARNING: Invalid header for file "'//file//'".'// &
                   LF//'Header for this data must have size (1) or '// &
                     '('//str(size(x, kind=i64))//').'
        else
          header_ = header
          if ( size(header, kind=i64) == 1_i64 ) then
            hstat = 1
          else
            hstat = 2
          end if
        end if
      end if

      if ( .not. present(dim) ) then
        if ( hstat == 2 ) then
          dim_ = 2
        else
          dim_ = 1
        end if
      else
        if ( hstat == 2 ) then
          dim_ = 2
          if ( dim /= 2 ) then
            write(*,"(a)") LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                     file//'" for given header... defaulting to (2).'
          end if
        else
          if ( dim == 1 ) then
            dim_ = 1
          else if ( dim == 2 ) then
            dim_ = 2
          else
            dim_ = 1
            write(*,"(a)") LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                     file//'" for given header... defaulting to (1).'
          end if
        end if
      end if

      if ( .not. present(delim) ) then
        if ( dim_ == 1 ) then
          delim_ = EMPTY_STR
        else
          delim_ = COMMA
        end if
      else
        if ( dim_ == 1 ) then
          delim_ = EMPTY_STR
        else
          delim_ = delim
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "i"
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ = fmt
        else
          fmt_ = "i"
          write(*,"(a)") LF//'WARNING: Invalid format "'//fmt//'" for file "'//file//'". '// &
                     'Defaulting to integer format.'// &
                   LF//'Format must be one of: '//join(INT_FMTS)
        end if
      end if

      call to_text(x=x, file=file, header=header_, dim=dim_, delim=delim_, fmt=fmt_)
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(dim   ) ) write(*,"(a)") LF//'WARNING: dim not supported for file type "'//ext//'".'
      if ( present(delim ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt   ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'

      call to_binary(x=x, file=file)
    else
      write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                'due to unsupported file extension "'//ext//'".'// &
              LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
                join(BINARY_EXT)
    end if
  end procedure to_file_1di16
  module procedure to_file_1di8
    character(len=:), allocatable :: ext, delim_, fmt_
    character(len=:), allocatable :: header_(:)
    integer                       :: hstat, dim_

    hstat=0; dim_=0

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = [ EMPTY_STR ]
        hstat = 0
      else
        if ( (size(header, kind=i64) /= 1_i64) .and. (size(header, kind=i64) /= size(x, kind=i64)) ) then
          header_ = [ EMPTY_STR ]
          hstat = -1
          write(*,"(a)") LF//'WARNING: Invalid header for file "'//file//'".'// &
                   LF//'Header for this data must have size (1) or '// &
                     '('//str(size(x, kind=i64))//').'
        else
          header_ = header
          if ( size(header, kind=i64) == 1_i64 ) then
            hstat = 1
          else
            hstat = 2
          end if
        end if
      end if

      if ( .not. present(dim) ) then
        if ( hstat == 2 ) then
          dim_ = 2
        else
          dim_ = 1
        end if
      else
        if ( hstat == 2 ) then
          dim_ = 2
          if ( dim /= 2 ) then
            write(*,"(a)") LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                     file//'" for given header... defaulting to (2).'
          end if
        else
          if ( dim == 1 ) then
            dim_ = 1
          else if ( dim == 2 ) then
            dim_ = 2
          else
            dim_ = 1
            write(*,"(a)") LF//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                     file//'" for given header... defaulting to (1).'
          end if
        end if
      end if

      if ( .not. present(delim) ) then
        if ( dim_ == 1 ) then
          delim_ = EMPTY_STR
        else
          delim_ = COMMA
        end if
      else
        if ( dim_ == 1 ) then
          delim_ = EMPTY_STR
        else
          delim_ = delim
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "i"
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ = fmt
        else
          fmt_ = "i"
          write(*,"(a)") LF//'WARNING: Invalid format "'//fmt//'" for file "'//file//'". '// &
                     'Defaulting to integer format.'// &
                   LF//'Format must be one of: '//join(INT_FMTS)
        end if
      end if

      call to_text(x=x, file=file, header=header_, dim=dim_, delim=delim_, fmt=fmt_)
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(dim   ) ) write(*,"(a)") LF//'WARNING: dim not supported for file type "'//ext//'".'
      if ( present(delim ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt   ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'

      call to_binary(x=x, file=file)
    else
      write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                'due to unsupported file extension "'//ext//'".'// &
              LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
                join(BINARY_EXT)
    end if
  end procedure to_file_1di8

  module procedure to_file_2di64
    character(len=:), allocatable :: ext, delim_, fmt_
    character(len=:), allocatable :: header_(:)

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = [ EMPTY_STR ]
      else
        if ( (size(header, kind=i64) /= 1_i64) .and. (size(header, kind=i64) /= size(x,dim=2, kind=i64)) ) then
          header_ = [ EMPTY_STR ]
          write(*,"(a)") LF//'WARNING: Invalid header for file "'//file//'".'// &
                   LF//'Header for this data must have size (1) or '// &
                     '('//str(size(x, dim=2, kind=i64))//').'
        else
          header_ = header
        end if
      end if

      if ( .not. present(delim) ) then
        delim_ = COMMA
      else
        delim_ = delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "i"
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ = fmt
        else
          fmt_ = "i"
          write(*,"(a)") LF//'WARNING: Invalid format "'//fmt//'" for file "'//file//'". '// &
                     'Defaulting to integer format.'// &
                   LF//'Format must be one of: '//join(INT_FMTS)
        end if
      end if

      call to_text(x=x, file=file, header=header_, delim=delim_, fmt=fmt_)
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(delim ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt   ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'

      call to_binary(x=x, file=file)
    else
      write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                'due to unsupported file extension "'//ext//'".'// &
              LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
                join(BINARY_EXT)
    end if
  end procedure to_file_2di64
  module procedure to_file_2di32
    character(len=:), allocatable :: ext, delim_, fmt_
    character(len=:), allocatable :: header_(:)

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = [ EMPTY_STR ]
      else
        if ( (size(header, kind=i64) /= 1_i64) .and. (size(header, kind=i64) /= size(x,dim=2, kind=i64)) ) then
          header_ = [ EMPTY_STR ]
          write(*,"(a)") LF//'WARNING: Invalid header for file "'//file//'".'// &
                   LF//'Header for this data must have size (1) or '// &
                     '('//str(size(x, dim=2, kind=i64))//').'
        else
          header_ = header
        end if
      end if

      if ( .not. present(delim) ) then
        delim_ = COMMA
      else
        delim_ = delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "i"
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ = fmt
        else
          fmt_ = "i"
          write(*,"(a)") LF//'WARNING: Invalid format "'//fmt//'" for file "'//file//'". '// &
                     'Defaulting to integer format.'// &
                   LF//'Format must be one of: '//join(INT_FMTS)
        end if
      end if

      call to_text(x=x, file=file, header=header_, delim=delim_, fmt=fmt_)
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(delim ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt   ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'

      call to_binary(x=x, file=file)
    else
      write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                'due to unsupported file extension "'//ext//'".'// &
              LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
                join(BINARY_EXT)
    end if
  end procedure to_file_2di32
  module procedure to_file_2di16
    character(len=:), allocatable :: ext, delim_, fmt_
    character(len=:), allocatable :: header_(:)

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = [ EMPTY_STR ]
      else
        if ( (size(header, kind=i64) /= 1_i64) .and. (size(header, kind=i64) /= size(x,dim=2, kind=i64)) ) then
          header_ = [ EMPTY_STR ]
          write(*,"(a)") LF//'WARNING: Invalid header for file "'//file//'".'// &
                   LF//'Header for this data must have size (1) or '// &
                     '('//str(size(x, dim=2, kind=i64))//').'
        else
          header_ = header
        end if
      end if

      if ( .not. present(delim) ) then
        delim_ = COMMA
      else
        delim_ = delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "i"
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ = fmt
        else
          fmt_ = "i"
          write(*,"(a)") LF//'WARNING: Invalid format "'//fmt//'" for file "'//file//'". '// &
                     'Defaulting to integer format.'// &
                   LF//'Format must be one of: '//join(INT_FMTS)
        end if
      end if

      call to_text(x=x, file=file, header=header_, delim=delim_, fmt=fmt_)
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(delim ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt   ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'

      call to_binary(x=x, file=file)
    else
      write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                'due to unsupported file extension "'//ext//'".'// &
              LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
                join(BINARY_EXT)
    end if
  end procedure to_file_2di16
  module procedure to_file_2di8
    character(len=:), allocatable :: ext, delim_, fmt_
    character(len=:), allocatable :: header_(:)

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = [ EMPTY_STR ]
      else
        if ( (size(header, kind=i64) /= 1_i64) .and. (size(header, kind=i64) /= size(x,dim=2, kind=i64)) ) then
          header_ = [ EMPTY_STR ]
          write(*,"(a)") LF//'WARNING: Invalid header for file "'//file//'".'// &
                   LF//'Header for this data must have size (1) or '// &
                     '('//str(size(x, dim=2, kind=i64))//').'
        else
          header_ = header
        end if
      end if

      if ( .not. present(delim) ) then
        delim_ = COMMA
      else
        delim_ = delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "i"
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ = fmt
        else
          fmt_ = "i"
          write(*,"(a)") LF//'WARNING: Invalid format "'//fmt//'" for file "'//file//'". '// &
                     'Defaulting to integer format.'// &
                   LF//'Format must be one of: '//join(INT_FMTS)
        end if
      end if

      call to_text(x=x, file=file, header=header_, delim=delim_, fmt=fmt_)
    else if ( any(BINARY_EXT == ext) ) then
      if ( present(header) ) write(*,"(a)") LF//'WARNING: header not supported for file type "'//ext//'".'
      if ( present(delim ) ) write(*,"(a)") LF//'WARNING: delim not supported for file type "'//ext//'".'
      if ( present(fmt   ) ) write(*,"(a)") LF//'WARNING: fmt not supported for file type "'//ext//'".'

      call to_binary(x=x, file=file)
    else
      write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                'due to unsupported file extension "'//ext//'".'// &
              LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
                join(BINARY_EXT)
    end if
  end procedure to_file_2di8

  module procedure to_file_3di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_3di64
  module procedure to_file_3di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_3di32
  module procedure to_file_3di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_3di16
  module procedure to_file_3di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_3di8

  module procedure to_file_4di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_4di64
  module procedure to_file_4di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_4di32
  module procedure to_file_4di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_4di16
  module procedure to_file_4di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_4di8

  module procedure to_file_5di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_5di64
  module procedure to_file_5di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_5di32
  module procedure to_file_5di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_5di16
  module procedure to_file_5di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_5di8

  module procedure to_file_6di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_6di64
  module procedure to_file_6di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_6di32
  module procedure to_file_6di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_6di16
  module procedure to_file_6di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_6di8

  module procedure to_file_7di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_7di64
  module procedure to_file_7di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_7di32
  module procedure to_file_7di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_7di16
  module procedure to_file_7di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_7di8

  module procedure to_file_8di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_8di64
  module procedure to_file_8di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_8di32
  module procedure to_file_8di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_8di16
  module procedure to_file_8di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_8di8

  module procedure to_file_9di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_9di64
  module procedure to_file_9di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_9di32
  module procedure to_file_9di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_9di16
  module procedure to_file_9di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_9di8

  module procedure to_file_10di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_10di64
  module procedure to_file_10di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_10di32
  module procedure to_file_10di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_10di16
  module procedure to_file_10di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_10di8

  module procedure to_file_11di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_11di64
  module procedure to_file_11di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_11di32
  module procedure to_file_11di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_11di16
  module procedure to_file_11di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_11di8

  module procedure to_file_12di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_12di64
  module procedure to_file_12di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_12di32
  module procedure to_file_12di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_12di16
  module procedure to_file_12di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_12di8

  module procedure to_file_13di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_13di64
  module procedure to_file_13di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_13di32
  module procedure to_file_13di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_13di16
  module procedure to_file_13di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_13di8

  module procedure to_file_14di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_14di64
  module procedure to_file_14di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_14di32
  module procedure to_file_14di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_14di16
  module procedure to_file_14di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_14di8

  module procedure to_file_15di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_15di64
  module procedure to_file_15di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_15di32
  module procedure to_file_15di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_15di16
  module procedure to_file_15di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      call to_binary(x=x, file=file)
    else
      if ( any(TEXT_EXT == ext) ) then
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'". Cannot write array of '// &
                  'dimension ('//str(rank(x))//') to text.'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      else
        write(*,"(a)")  LF//'WARNING: Skipping write to "'//file//'" '// &
                  'due to unsupported file extension "'//ext//'".'// &
                LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure to_file_15di8

  ! Reading Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  module procedure from_textfile_1dc128
    character(len=:), allocatable :: ext, locale_, delim_, fmt_, im_
    logical                       :: header_

    header_ = .false.

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = .false.
      else
        header_ = header
      end if

      if ( .not. present(locale) ) then
        locale_ = "US"
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          error stop LF//'FATAL: Invalid locale "'//locale//'" for read of file "'//file//'".'// &
                 LF//'Locale must be one of: '//join(LOCALES)
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == "US" ) then
          delim_ = COMMA
        else
          delim_ = SEMICOLON
        end if
      else
        delim_ = delim
        if ( locale_ == "US" ) then
          if ( delim_ == POINT ) then
            error stop LF//'FATAL: Invalid delimiter for read of file "'//file//'" with US decimal.'
          end if
        else
          if ( delim_ == COMMA ) then
            error stop LF//'FATAL: Invalid delimiter for read of file "'//file//'" with EU decimal.'
          end if
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "e"
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                   'into complex array.'// &
                 LF//'Format must be one of: '//join(REAL_FMTS)
        end if
      end if

      if ( .not. present(im) ) then
        im_ = EMPTY_STR
      else
        im_ = im
      end if

      call from_text( file=file, into=into, header=header_, locale=locale_, delim=delim_, &
              fmt=fmt_, im=im_ )
    else
      if ( any(BINARY_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must be specified '// &
                 'for binary data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_textfile_1dc128
  module procedure from_binaryfile_1dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must not be specified '// &
                 'for textual data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_binaryfile_1dc128
  module procedure from_textfile_1dc64
    character(len=:), allocatable :: ext, locale_, delim_, fmt_, im_
    logical                       :: header_

    header_ = .false.

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = .false.
      else
        header_ = header
      end if

      if ( .not. present(locale) ) then
        locale_ = "US"
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          error stop LF//'FATAL: Invalid locale "'//locale//'" for read of file "'//file//'".'// &
                 LF//'Locale must be one of: '//join(LOCALES)
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == "US" ) then
          delim_ = COMMA
        else
          delim_ = SEMICOLON
        end if
      else
        delim_ = delim
        if ( locale_ == "US" ) then
          if ( delim_ == POINT ) then
            error stop LF//'FATAL: Invalid delimiter for read of file "'//file//'" with US decimal.'
          end if
        else
          if ( delim_ == COMMA ) then
            error stop LF//'FATAL: Invalid delimiter for read of file "'//file//'" with EU decimal.'
          end if
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "e"
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                   'into complex array.'// &
                 LF//'Format must be one of: '//join(REAL_FMTS)
        end if
      end if

      if ( .not. present(im) ) then
        im_ = EMPTY_STR
      else
        im_ = im
      end if

      call from_text( file=file, into=into, header=header_, locale=locale_, delim=delim_, &
              fmt=fmt_, im=im_ )
    else
      if ( any(BINARY_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must be specified '// &
                 'for binary data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_textfile_1dc64
  module procedure from_binaryfile_1dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must not be specified '// &
                 'for textual data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_binaryfile_1dc64
  module procedure from_textfile_1dc32
    character(len=:), allocatable :: ext, locale_, delim_, fmt_, im_
    logical                       :: header_

    header_ = .false.

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = .false.
      else
        header_ = header
      end if

      if ( .not. present(locale) ) then
        locale_ = "US"
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          error stop LF//'FATAL: Invalid locale "'//locale//'" for read of file "'//file//'".'// &
                 LF//'Locale must be one of: '//join(LOCALES)
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == "US" ) then
          delim_ = COMMA
        else
          delim_ = SEMICOLON
        end if
      else
        delim_ = delim
        if ( locale_ == "US" ) then
          if ( delim_ == POINT ) then
            error stop LF//'FATAL: Invalid delimiter for read of file "'//file//'" with US decimal.'
          end if
        else
          if ( delim_ == COMMA ) then
            error stop LF//'FATAL: Invalid delimiter for read of file "'//file//'" with EU decimal.'
          end if
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "e"
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                   'into complex array.'// &
                 LF//'Format must be one of: '//join(REAL_FMTS)
        end if
      end if

      if ( .not. present(im) ) then
        im_ = EMPTY_STR
      else
        im_ = im
      end if

      call from_text( file=file, into=into, header=header_, locale=locale_, delim=delim_, &
              fmt=fmt_, im=im_ )
    else
      if ( any(BINARY_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must be specified '// &
                 'for binary data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_textfile_1dc32
  module procedure from_binaryfile_1dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must not be specified '// &
                 'for textual data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_binaryfile_1dc32

  module procedure from_textfile_2dc128
    character(len=:), allocatable :: ext, locale_, delim_, fmt_, im_
    logical                       :: header_

    header_ = .false.

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = .false.
      else
        header_ = header
      end if

      if ( .not. present(locale) ) then
        locale_ = "US"
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          error stop LF//'FATAL: Invalid locale "'//locale//'" for read of file "'//file//'".'// &
                 LF//'Locale must be one of: '//join(LOCALES)
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == "US" ) then
          delim_ = COMMA
        else
          delim_ = SEMICOLON
        end if
      else
        delim_ = delim
        if ( locale_ == "US" ) then
          if ( delim_ == POINT ) then
            error stop LF//'FATAL: Invalid delimiter for read of file "'//file//'" with US decimal.'
          end if
        else
          if ( delim_ == COMMA ) then
            error stop LF//'FATAL: Invalid delimiter for read of file "'//file//'" with EU decimal.'
          end if
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "e"
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                   'into complex array.'// &
                 LF//'Format must be one of: '//join(REAL_FMTS)
        end if
      end if

      if ( .not. present(im) ) then
        im_ = EMPTY_STR
      else
        im_ = im
      end if

      call from_text( file=file, into=into, header=header_, locale=locale_, delim=delim_, &
              fmt=fmt_, im=im_ )
    else
      if ( any(BINARY_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must be specified '// &
                 'for binary data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_textfile_2dc128
  module procedure from_binaryfile_2dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must not be specified '// &
                 'for textual data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_binaryfile_2dc128
  module procedure from_textfile_2dc64
    character(len=:), allocatable :: ext, locale_, delim_, fmt_, im_
    logical                       :: header_

    header_ = .false.

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = .false.
      else
        header_ = header
      end if

      if ( .not. present(locale) ) then
        locale_ = "US"
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          error stop LF//'FATAL: Invalid locale "'//locale//'" for read of file "'//file//'".'// &
                 LF//'Locale must be one of: '//join(LOCALES)
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == "US" ) then
          delim_ = COMMA
        else
          delim_ = SEMICOLON
        end if
      else
        delim_ = delim
        if ( locale_ == "US" ) then
          if ( delim_ == POINT ) then
            error stop LF//'FATAL: Invalid delimiter for read of file "'//file//'" with US decimal.'
          end if
        else
          if ( delim_ == COMMA ) then
            error stop LF//'FATAL: Invalid delimiter for read of file "'//file//'" with EU decimal.'
          end if
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "e"
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                   'into complex array.'// &
                 LF//'Format must be one of: '//join(REAL_FMTS)
        end if
      end if

      if ( .not. present(im) ) then
        im_ = EMPTY_STR
      else
        im_ = im
      end if

      call from_text( file=file, into=into, header=header_, locale=locale_, delim=delim_, &
              fmt=fmt_, im=im_ )
    else
      if ( any(BINARY_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must be specified '// &
                 'for binary data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_textfile_2dc64
  module procedure from_binaryfile_2dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must not be specified '// &
                 'for textual data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_binaryfile_2dc64
  module procedure from_textfile_2dc32
    character(len=:), allocatable :: ext, locale_, delim_, fmt_, im_
    logical                       :: header_

    header_ = .false.

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = .false.
      else
        header_ = header
      end if

      if ( .not. present(locale) ) then
        locale_ = "US"
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          error stop LF//'FATAL: Invalid locale "'//locale//'" for read of file "'//file//'".'// &
                 LF//'Locale must be one of: '//join(LOCALES)
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == "US" ) then
          delim_ = COMMA
        else
          delim_ = SEMICOLON
        end if
      else
        delim_ = delim
        if ( locale_ == "US" ) then
          if ( delim_ == POINT ) then
            error stop LF//'FATAL: Invalid delimiter for read of file "'//file//'" with US decimal.'
          end if
        else
          if ( delim_ == COMMA ) then
            error stop LF//'FATAL: Invalid delimiter for read of file "'//file//'" with EU decimal.'
          end if
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "e"
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                   'into complex array.'// &
                 LF//'Format must be one of: '//join(REAL_FMTS)
        end if
      end if

      if ( .not. present(im) ) then
        im_ = EMPTY_STR
      else
        im_ = im
      end if

      call from_text( file=file, into=into, header=header_, locale=locale_, delim=delim_, &
              fmt=fmt_, im=im_ )
    else
      if ( any(BINARY_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must be specified '// &
                 'for binary data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_textfile_2dc32
  module procedure from_binaryfile_2dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must not be specified '// &
                 'for textual data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_binaryfile_2dc32

  module procedure from_file_3dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_3dc128
  module procedure from_file_3dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_3dc64
  module procedure from_file_3dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_3dc32

  module procedure from_file_4dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_4dc128
  module procedure from_file_4dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_4dc64
  module procedure from_file_4dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_4dc32

  module procedure from_file_5dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_5dc128
  module procedure from_file_5dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_5dc64
  module procedure from_file_5dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_5dc32

  module procedure from_file_6dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_6dc128
  module procedure from_file_6dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_6dc64
  module procedure from_file_6dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_6dc32

  module procedure from_file_7dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_7dc128
  module procedure from_file_7dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_7dc64
  module procedure from_file_7dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_7dc32

  module procedure from_file_8dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_8dc128
  module procedure from_file_8dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_8dc64
  module procedure from_file_8dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_8dc32

  module procedure from_file_9dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_9dc128
  module procedure from_file_9dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_9dc64
  module procedure from_file_9dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_9dc32

  module procedure from_file_10dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_10dc128
  module procedure from_file_10dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_10dc64
  module procedure from_file_10dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_10dc32

  module procedure from_file_11dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_11dc128
  module procedure from_file_11dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_11dc64
  module procedure from_file_11dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_11dc32

  module procedure from_file_12dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_12dc128
  module procedure from_file_12dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_12dc64
  module procedure from_file_12dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_12dc32

  module procedure from_file_13dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_13dc128
  module procedure from_file_13dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_13dc64
  module procedure from_file_13dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_13dc32

  module procedure from_file_14dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_14dc128
  module procedure from_file_14dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_14dc64
  module procedure from_file_14dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_14dc32

  module procedure from_file_15dc128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_15dc128
  module procedure from_file_15dc64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_15dc64
  module procedure from_file_15dc32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_15dc32

  module procedure from_textfile_1dr128
    character(len=:), allocatable :: ext, locale_, delim_, fmt_
    logical                       :: header_

    header_ = .false.

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = .false.
      else
        header_ = header
      end if

      if ( .not. present(locale) ) then
        locale_ = "US"
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          error stop LF//'FATAL: Invalid locale "'//locale//'" for read of file "'//file//'".'// &
                 LF//'Locale must be one of: '//join(LOCALES)
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == "US" ) then
          delim_ = COMMA
        else
          delim_ = SEMICOLON
        end if
      else
        delim_ = delim
        if ( locale_ == "US" ) then
          if ( delim_ == POINT ) then
            error stop LF//'FATAL: Invalid delimiter for read of file "'//file//'" with US decimal.'
          end if
        else
          if ( delim_ == COMMA ) then
            error stop LF//'FATAL: Invalid delimiter for read of file "'//file//'" with EU decimal.'
          end if
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "e"
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                   'into real array.'// &
                 LF//'Format must be one of: '//join(REAL_FMTS)
        end if
      end if

      call from_text(file=file, into=into, header=header_, locale=locale_, delim=delim_, fmt=fmt_)
    else
      if ( any(BINARY_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must be specified '// &
                 'for binary data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_textfile_1dr128
  module procedure from_binaryfile_1dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must not be specified '// &
                 'for textual data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_binaryfile_1dr128
  module procedure from_textfile_1dr64
    character(len=:), allocatable :: ext, locale_, delim_, fmt_
    logical                       :: header_

    header_ = .false.

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = .false.
      else
        header_ = header
      end if

      if ( .not. present(locale) ) then
        locale_ = "US"
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          error stop LF//'FATAL: Invalid locale "'//locale//'" for read of file "'//file//'".'// &
                 LF//'Locale must be one of: '//join(LOCALES)
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == "US" ) then
          delim_ = COMMA
        else
          delim_ = SEMICOLON
        end if
      else
        delim_ = delim
        if ( locale_ == "US" ) then
          if ( delim_ == POINT ) then
            error stop LF//'FATAL: Invalid delimiter for read of file "'//file//'" with US decimal.'
          end if
        else
          if ( delim_ == COMMA ) then
            error stop LF//'FATAL: Invalid delimiter for read of file "'//file//'" with EU decimal.'
          end if
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "e"
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                   'into real array.'// &
                 LF//'Format must be one of: '//join(REAL_FMTS)
        end if
      end if

      call from_text(file=file, into=into, header=header_, locale=locale_, delim=delim_, fmt=fmt_)
    else
      if ( any(BINARY_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must be specified '// &
                 'for binary data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_textfile_1dr64
  module procedure from_binaryfile_1dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must not be specified '// &
                 'for textual data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_binaryfile_1dr64
  module procedure from_textfile_1dr32
    character(len=:), allocatable :: ext, locale_, delim_, fmt_
    logical                       :: header_

    header_ = .false.

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = .false.
      else
        header_ = header
      end if

      if ( .not. present(locale) ) then
        locale_ = "US"
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          error stop LF//'FATAL: Invalid locale "'//locale//'" for read of file "'//file//'".'// &
                 LF//'Locale must be one of: '//join(LOCALES)
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == "US" ) then
          delim_ = COMMA
        else
          delim_ = SEMICOLON
        end if
      else
        delim_ = delim
        if ( locale_ == "US" ) then
          if ( delim_ == POINT ) then
            error stop LF//'FATAL: Invalid delimiter for read of file "'//file//'" with US decimal.'
          end if
        else
          if ( delim_ == COMMA ) then
            error stop LF//'FATAL: Invalid delimiter for read of file "'//file//'" with EU decimal.'
          end if
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "e"
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                   'into real array.'// &
                 LF//'Format must be one of: '//join(REAL_FMTS)
        end if
      end if

      call from_text(file=file, into=into, header=header_, locale=locale_, delim=delim_, fmt=fmt_)
    else
      if ( any(BINARY_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must be specified '// &
                 'for binary data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_textfile_1dr32
  module procedure from_binaryfile_1dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must not be specified '// &
                 'for textual data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_binaryfile_1dr32

  module procedure from_textfile_2dr128
    character(len=:), allocatable :: ext, locale_, delim_, fmt_
    logical                       :: header_

    header_ = .false.

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = .false.
      else
        header_ = header
      end if

      if ( .not. present(locale) ) then
        locale_ = "US"
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          error stop LF//'FATAL: Invalid locale "'//locale//'" for read of file "'//file//'".'// &
                 LF//'Locale must be one of: '//join(LOCALES)
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == "US" ) then
          delim_ = COMMA
        else
          delim_ = SEMICOLON
        end if
      else
        delim_ = delim
        if ( locale_ == "US" ) then
          if ( delim_ == POINT ) then
            error stop LF//'FATAL: Invalid delimiter for read of file "'//file//'" with US decimal.'
          end if
        else
          if ( delim_ == COMMA ) then
            error stop LF//'FATAL: Invalid delimiter for read of file "'//file//'" with EU decimal.'
          end if
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "e"
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                   'into real array.'// &
                 LF//'Format must be one of: '//join(REAL_FMTS)
        end if
      end if

      call from_text(file=file, into=into, header=header_, locale=locale_, delim=delim_, fmt=fmt_)
    else
      if ( any(BINARY_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must be specified '// &
                 'for binary data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_textfile_2dr128
  module procedure from_binaryfile_2dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must not be specified '// &
                 'for textual data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_binaryfile_2dr128
  module procedure from_textfile_2dr64
    character(len=:), allocatable :: ext, locale_, delim_, fmt_
    logical                       :: header_

    header_ = .false.

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = .false.
      else
        header_ = header
      end if

      if ( .not. present(locale) ) then
        locale_ = "US"
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          error stop LF//'FATAL: Invalid locale "'//locale//'" for read of file "'//file//'".'// &
                 LF//'Locale must be one of: '//join(LOCALES)
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == "US" ) then
          delim_ = COMMA
        else
          delim_ = SEMICOLON
        end if
      else
        delim_ = delim
        if ( locale_ == "US" ) then
          if ( delim_ == POINT ) then
            error stop LF//'FATAL: Invalid delimiter for read of file "'//file//'" with US decimal.'
          end if
        else
          if ( delim_ == COMMA ) then
            error stop LF//'FATAL: Invalid delimiter for read of file "'//file//'" with EU decimal.'
          end if
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "e"
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                   'into real array.'// &
                 LF//'Format must be one of: '//join(REAL_FMTS)
        end if
      end if

      call from_text(file=file, into=into, header=header_, locale=locale_, delim=delim_, fmt=fmt_)
    else
      if ( any(BINARY_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must be specified '// &
                 'for binary data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_textfile_2dr64
  module procedure from_binaryfile_2dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must not be specified '// &
                 'for textual data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_binaryfile_2dr64
  module procedure from_textfile_2dr32
    character(len=:), allocatable :: ext, locale_, delim_, fmt_
    logical                       :: header_

    header_ = .false.

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = .false.
      else
        header_ = header
      end if

      if ( .not. present(locale) ) then
        locale_ = "US"
      else
        if ( any(LOCALES == locale) ) then
          locale_ = locale
        else
          error stop LF//'FATAL: Invalid locale "'//locale//'" for read of file "'//file//'".'// &
                 LF//'Locale must be one of: '//join(LOCALES)
        end if
      end if

      if ( .not. present(delim) ) then
        if ( locale_ == "US" ) then
          delim_ = COMMA
        else
          delim_ = SEMICOLON
        end if
      else
        delim_ = delim
        if ( locale_ == "US" ) then
          if ( delim_ == POINT ) then
            error stop LF//'FATAL: Invalid delimiter for read of file "'//file//'" with US decimal.'
          end if
        else
          if ( delim_ == COMMA ) then
            error stop LF//'FATAL: Invalid delimiter for read of file "'//file//'" with EU decimal.'
          end if
        end if
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "e"
      else
        if ( any(REAL_FMTS == fmt) ) then
          fmt_ = fmt
        else
          error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                   'into real array.'// &
                 LF//'Format must be one of: '//join(REAL_FMTS)
        end if
      end if

      call from_text(file=file, into=into, header=header_, locale=locale_, delim=delim_, fmt=fmt_)
    else
      if ( any(BINARY_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must be specified '// &
                 'for binary data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_textfile_2dr32
  module procedure from_binaryfile_2dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must not be specified '// &
                 'for textual data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_binaryfile_2dr32

  module procedure from_file_3dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_3dr128
  module procedure from_file_3dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_3dr64
  module procedure from_file_3dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_3dr32

  module procedure from_file_4dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_4dr128
  module procedure from_file_4dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_4dr64
  module procedure from_file_4dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_4dr32

  module procedure from_file_5dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_5dr128
  module procedure from_file_5dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_5dr64
  module procedure from_file_5dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_5dr32

  module procedure from_file_6dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_6dr128
  module procedure from_file_6dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_6dr64
  module procedure from_file_6dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_6dr32

  module procedure from_file_7dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_7dr128
  module procedure from_file_7dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_7dr64
  module procedure from_file_7dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_7dr32

  module procedure from_file_8dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_8dr128
  module procedure from_file_8dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_8dr64
  module procedure from_file_8dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_8dr32

  module procedure from_file_9dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_9dr128
  module procedure from_file_9dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_9dr64
  module procedure from_file_9dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_9dr32

  module procedure from_file_10dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_10dr128
  module procedure from_file_10dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_10dr64
  module procedure from_file_10dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_10dr32

  module procedure from_file_11dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_11dr128
  module procedure from_file_11dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_11dr64
  module procedure from_file_11dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_11dr32

  module procedure from_file_12dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_12dr128
  module procedure from_file_12dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_12dr64
  module procedure from_file_12dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_12dr32

  module procedure from_file_13dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_13dr128
  module procedure from_file_13dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_13dr64
  module procedure from_file_13dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_13dr32

  module procedure from_file_14dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_14dr128
  module procedure from_file_14dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_14dr64
  module procedure from_file_14dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_14dr32

  module procedure from_file_15dr128
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_15dr128
  module procedure from_file_15dr64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_15dr64
  module procedure from_file_15dr32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_15dr32

  module procedure from_textfile_1di64
    character(len=:), allocatable :: ext, delim_, fmt_
    logical                       :: header_

    header_ = .false.

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = .false.
      else
        header_ = header
      end if

      if ( .not. present(delim) ) then
        delim_ = COMMA
      else
        delim_ = delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "i"
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ = fmt
        else
          error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                   'into integer array.'// &
                 LF//'Format must be one of: '//join(INT_FMTS)
        end if
      end if

      call from_text(file=file, into=into, header=header_, delim=delim_, fmt=fmt_)
    else
      if ( any(BINARY_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must be specified '// &
                 'for binary data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_textfile_1di64
  module procedure from_binaryfile_1di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must not be specified '// &
                 'for textual data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_binaryfile_1di64
  module procedure from_textfile_1di32
    character(len=:), allocatable :: ext, delim_, fmt_
    logical                       :: header_

    header_ = .false.

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = .false.
      else
        header_ = header
      end if

      if ( .not. present(delim) ) then
        delim_ = COMMA
      else
        delim_ = delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "i"
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ = fmt
        else
          error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                   'into integer array.'// &
                 LF//'Format must be one of: '//join(INT_FMTS)
        end if
      end if

      call from_text(file=file, into=into, header=header_, delim=delim_, fmt=fmt_)
    else
      if ( any(BINARY_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must be specified '// &
                 'for binary data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_textfile_1di32
  module procedure from_binaryfile_1di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must not be specified '// &
                 'for textual data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_binaryfile_1di32
  module procedure from_textfile_1di16
    character(len=:), allocatable :: ext, delim_, fmt_
    logical                       :: header_

    header_ = .false.

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = .false.
      else
        header_ = header
      end if

      if ( .not. present(delim) ) then
        delim_ = COMMA
      else
        delim_ = delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "i"
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ = fmt
        else
          error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                   'into integer array.'// &
                 LF//'Format must be one of: '//join(INT_FMTS)
        end if
      end if

      call from_text(file=file, into=into, header=header_, delim=delim_, fmt=fmt_)
    else
      if ( any(BINARY_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must be specified '// &
                 'for binary data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_textfile_1di16
  module procedure from_binaryfile_1di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must not be specified '// &
                 'for textual data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_binaryfile_1di16
  module procedure from_textfile_1di8
    character(len=:), allocatable :: ext, delim_, fmt_
    logical                       :: header_

    header_ = .false.

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = .false.
      else
        header_ = header
      end if

      if ( .not. present(delim) ) then
        delim_ = COMMA
      else
        delim_ = delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "i"
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ = fmt
        else
          error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                   'into integer array.'// &
                 LF//'Format must be one of: '//join(INT_FMTS)
        end if
      end if

      call from_text(file=file, into=into, header=header_, delim=delim_, fmt=fmt_)
    else
      if ( any(BINARY_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must be specified '// &
                 'for binary data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_textfile_1di8
  module procedure from_binaryfile_1di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must not be specified '// &
                 'for textual data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_binaryfile_1di8

  module procedure from_textfile_2di64
    character(len=:), allocatable :: ext, delim_, fmt_
    logical                       :: header_

    header_ = .false.

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = .false.
      else
        header_ = header
      end if

      if ( .not. present(delim) ) then
        delim_ = COMMA
      else
        delim_ = delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "i"
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ = fmt
        else
          error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                   'into integer array.'// &
                 LF//'Format must be one of: '//join(INT_FMTS)
        end if
      end if

      call from_text(file=file, into=into, header=header_, delim=delim_, fmt=fmt_)
    else
      if ( any(BINARY_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must be specified '// &
                 'for binary data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_textfile_2di64
  module procedure from_binaryfile_2di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must not be specified '// &
                 'for textual data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_binaryfile_2di64
  module procedure from_textfile_2di32
    character(len=:), allocatable :: ext, delim_, fmt_
    logical                       :: header_

    header_ = .false.

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = .false.
      else
        header_ = header
      end if

      if ( .not. present(delim) ) then
        delim_ = COMMA
      else
        delim_ = delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "i"
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ = fmt
        else
          error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                   'into integer array.'// &
                 LF//'Format must be one of: '//join(INT_FMTS)
        end if
      end if

      call from_text(file=file, into=into, header=header_, delim=delim_, fmt=fmt_)
    else
      if ( any(BINARY_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must be specified '// &
                 'for binary data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_textfile_2di32
  module procedure from_binaryfile_2di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must not be specified '// &
                 'for textual data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_binaryfile_2di32
  module procedure from_textfile_2di16
    character(len=:), allocatable :: ext, delim_, fmt_
    logical                       :: header_

    header_ = .false.

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = .false.
      else
        header_ = header
      end if

      if ( .not. present(delim) ) then
        delim_ = COMMA
      else
        delim_ = delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "i"
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ = fmt
        else
          error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                   'into integer array.'// &
                 LF//'Format must be one of: '//join(INT_FMTS)
        end if
      end if

      call from_text(file=file, into=into, header=header_, delim=delim_, fmt=fmt_)
    else
      if ( any(BINARY_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must be specified '// &
                 'for binary data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_textfile_2di16
  module procedure from_binaryfile_2di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must not be specified '// &
                 'for textual data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_binaryfile_2di16
  module procedure from_textfile_2di8
    character(len=:), allocatable :: ext, delim_, fmt_
    logical                       :: header_

    header_ = .false.

    ext = ext_of(file)

    if ( any(TEXT_EXT == ext) ) then
      if ( .not. present(header) ) then
        header_ = .false.
      else
        header_ = header
      end if

      if ( .not. present(delim) ) then
        delim_ = COMMA
      else
        delim_ = delim
      end if

      if ( .not. present(fmt) ) then
        fmt_ = "i"
      else
        if ( any(INT_FMTS == fmt) ) then
          fmt_ = fmt
        else
          error stop LF//'FATAL: Invalid format "'//fmt//'" for read of file "'//file//'" '// &
                   'into integer array.'// &
                 LF//'Format must be one of: '//join(INT_FMTS)
        end if
      end if

      call from_text(file=file, into=into, header=header_, delim=delim_, fmt=fmt_)
    else
      if ( any(BINARY_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must be specified '// &
                 'for binary data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_textfile_2di8
  module procedure from_binaryfile_2di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'", data_shape must not be specified '// &
                 'for textual data.'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(TEXT_EXT)//SPACE// &
               join(BINARY_EXT)
      end if
    end if
  end procedure from_binaryfile_2di8

  module procedure from_file_3di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_3di64
  module procedure from_file_3di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_3di32
  module procedure from_file_3di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_3di16
  module procedure from_file_3di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_3di8

  module procedure from_file_4di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_4di64
  module procedure from_file_4di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_4di32
  module procedure from_file_4di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_4di16
  module procedure from_file_4di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_4di8

  module procedure from_file_5di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_5di64
  module procedure from_file_5di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_5di32
  module procedure from_file_5di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_5di16
  module procedure from_file_5di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_5di8

  module procedure from_file_6di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_6di64
  module procedure from_file_6di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_6di32
  module procedure from_file_6di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_6di16
  module procedure from_file_6di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_6di8

  module procedure from_file_7di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_7di64
  module procedure from_file_7di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_7di32
  module procedure from_file_7di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_7di16
  module procedure from_file_7di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_7di8

  module procedure from_file_8di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_8di64
  module procedure from_file_8di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_8di32
  module procedure from_file_8di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_8di16
  module procedure from_file_8di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_8di8

  module procedure from_file_9di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_9di64
  module procedure from_file_9di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_9di32
  module procedure from_file_9di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_9di16
  module procedure from_file_9di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_9di8

  module procedure from_file_10di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_10di64
  module procedure from_file_10di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_10di32
  module procedure from_file_10di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_10di16
  module procedure from_file_10di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_10di8

  module procedure from_file_11di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_11di64
  module procedure from_file_11di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_11di32
  module procedure from_file_11di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_11di16
  module procedure from_file_11di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_11di8

  module procedure from_file_12di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_12di64
  module procedure from_file_12di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_12di32
  module procedure from_file_12di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_12di16
  module procedure from_file_12di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_12di8

  module procedure from_file_13di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_13di64
  module procedure from_file_13di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_13di32
  module procedure from_file_13di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_13di16
  module procedure from_file_13di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_13di8

  module procedure from_file_14di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_14di64
  module procedure from_file_14di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_14di32
  module procedure from_file_14di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_14di16
  module procedure from_file_14di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_14di8

  module procedure from_file_15di64
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_15di64
  module procedure from_file_15di32
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_15di32
  module procedure from_file_15di16
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_15di16
  module procedure from_file_15di8
    character(len=:), allocatable :: ext

    ext = ext_of(file)

    if ( any(BINARY_EXT == ext) ) then
      if ( size(data_shape) /= rank(into) ) then
        error stop LF//'FATAL: Shape mismatch in read of file "'//file//'".'// &
               LF//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
               //str(size(data_shape))//'). These must match.'
      end if

      call from_binary(file=file, into=into, data_shape=data_shape)
    else
      if ( any(TEXT_EXT == ext) ) then
        error stop LF//'FATAL: Error reading file "'//file//'". Textual data cannot be read into '// &
                 'arrays of dimension greater than (2).'
      else
        error stop LF//'FATAL: Unsupported file extension "'//ext//'" for file "'//file//'".'// &
               LF//'Supported file extensions: '//join(BINARY_EXT)
      end if
    end if
  end procedure from_file_15di8
end submodule file_io
