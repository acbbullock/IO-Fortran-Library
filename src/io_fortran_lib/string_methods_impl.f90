submodule (io_fortran_lib) string_methods
  !---------------------------------------------------------------------------------------------------------------------
  !! This submodule provides module procedure implementations for the **type-bound procedures** of type `String`.
  !---------------------------------------------------------------------------------------------------------------------
  implicit none (type, external)

  integer, parameter :: ENDS_COLUMN = 1, ENDS_ROW = 2, ENDS_FILE = 3 ! What ended a cell

  type :: cell_bounds
    integer(i64) :: l = 1_i64, r = 0_i64, tail_l = 1_i64, tail_r = 0_i64
    integer      :: ends = ENDS_FILE
    logical      :: quoted = .false., escaped = .false.
  end type cell_bounds

  ! Definitions and interfaces ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=1), parameter :: LINE_FEED = LF, COMMA_DELIMITER = COMMA
  logical,          target :: NO_APPEND = .false.

  contains ! Procedure bodies for module subprograms <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

  module procedure as_str
    if ( self%len64() < 1_i64 ) then
      string_slice = EMPTY_STR
    else
      string_slice = self%buf(:self%n)
    end if
  end procedure as_str

  module procedure count_substring_chars
    integer(i64) :: self_len, match_len, max_pos, upper_ind, i, j
    integer      :: first_char, last_char

    self_len=0_i64; match_len=0_i64; max_pos=0_i64; upper_ind=0_i64; i=0_i64; j=0_i64; first_char=0; last_char=0

    self_len  = max(self%len64(), 0_i64)
    match_len = len(match, kind=i64)

    if ( self_len < 1_i64 ) then
      if ( self_len == match_len ) then
        occurrences = 1; return
      else
        occurrences = 0; return
      end if
    end if

    if ( (match_len == 0_i64) .or. (match_len > self_len) ) then
      occurrences = 0; return
    end if

    occurrences = 0; max_pos = self_len-match_len+1_i64
    first_char = iachar(match(1:1)); last_char = iachar(match(match_len:match_len))

    if ( match_len == 1_i64 ) then
      i = 1_i64; do
        if ( i > max_pos ) return

        if ( iachar(self%buf(i:i)) /= first_char ) then
          i = i + 1_i64; cycle
        else
          occurrences = occurrences + 1; i = i + 1_i64; cycle
        end if
      end do
    end if

    if ( match_len == 2_i64 ) then
      i = 1_i64; do
        if ( i > max_pos ) return

        if ( iachar(self%buf(i:i)) /= first_char ) then
          i = i + 1_i64; cycle
        end if

        if ( iachar(self%buf(i+1_i64:i+1_i64)) /= last_char ) then
          i = i + 1_i64; cycle
        else
          occurrences = occurrences + 1; i = i + 2_i64; cycle
        end if
      end do
    end if

    i = 1_i64; do
      if ( i > max_pos ) return

      if ( iachar(self%buf(i:i)) /= first_char ) then
        i = i + 1_i64; cycle
      end if

      upper_ind = i+match_len-1_i64

      if ( iachar(self%buf(upper_ind:upper_ind)) /= last_char ) then
        i = i + 1_i64; cycle
      end if

      if ( self%buf(i:upper_ind) == match ) then
        occurrences = occurrences + 1; i = i + match_len; cycle
      else
        i = i + 1_i64; cycle
      end if
    end do
  end procedure count_substring_chars

  module procedure count_substring_string
    integer(i64) :: self_len, match_len, max_pos, upper_ind, i, j
    integer      :: first_char, last_char

    self_len=0_i64; match_len=0_i64; max_pos=0_i64; upper_ind=0_i64; i=0_i64; j=0_i64; first_char=0; last_char=0

    self_len  = max(self%len64(), 0_i64)
    match_len = max(match%len64(), 0_i64)

    if ( self_len < 1_i64 ) then
      if ( self_len == match_len ) then
        occurrences = 1; return
      else
        occurrences = 0; return
      end if
    end if

    if ( (match_len == 0_i64) .or. (match_len > self_len) ) then
      occurrences = 0; return
    end if

    occurrences = 0; max_pos = self_len-match_len+1_i64
    first_char = iachar(match%buf(1:1)); last_char = iachar(match%buf(match_len:match_len))

    if ( match_len == 1_i64 ) then
      i = 1_i64; do
        if ( i > max_pos ) return

        if ( iachar(self%buf(i:i)) /= first_char ) then
          i = i + 1_i64; cycle
        else
          occurrences = occurrences + 1; i = i + 1_i64; cycle
        end if
      end do
    end if

    if ( match_len == 2_i64 ) then
      i = 1_i64; do
        if ( i > max_pos ) return

        if ( iachar(self%buf(i:i)) /= first_char ) then
          i = i + 1_i64; cycle
        end if

        if ( iachar(self%buf(i+1_i64:i+1_i64)) /= last_char ) then
          i = i + 1_i64; cycle
        else
          occurrences = occurrences + 1; i = i + 2_i64; cycle
        end if
      end do
    end if

    i = 1_i64; do
      if ( i > max_pos ) return

      if ( iachar(self%buf(i:i)) /= first_char ) then
        i = i + 1_i64; cycle
      end if

      upper_ind = i+match_len-1_i64

      if ( iachar(self%buf(upper_ind:upper_ind)) /= last_char ) then
        i = i + 1_i64; cycle
      end if

      if ( self%buf(i:upper_ind) == match%buf(:match%n) ) then
        occurrences = occurrences + 1; i = i + match_len; cycle
      else
        i = i + 1_i64; cycle
      end if
    end do
  end procedure count_substring_string

  module procedure empty
    self%buf = EMPTY_STR; self%n = len(self%buf, kind=i64)
  end procedure empty

  module procedure join_into_self
    character(len=:), allocatable :: separator_
    integer(i64)                  :: num_tokens

    num_tokens = size(tokens, kind=i64)

    if ( num_tokens == 1_i64 ) then
      if ( tokens(1_i64)%len64() < 1_i64 ) then
        self%buf = EMPTY_STR; self%n = len(self%buf, kind=i64); return
      else
        self%buf = tokens(1_i64)%buf(:tokens(1_i64)%n); self%n = len(self%buf, kind=i64); return
      end if
    end if

    if ( .not. present(separator) ) then
      separator_ = SPACE
    else
      separator_ = separator
    end if

    call self%join_base(tokens=tokens, separator=separator_)
  end procedure join_into_self

  module procedure join_base
    character(len=:), allocatable :: text
    integer(i64)                  :: num_tokens, sep_len, total_length, pos, n, i

    num_tokens = size(tokens, kind=i64)
    sep_len    = len(separator, kind=i64)

    if ( num_tokens == 0_i64 ) then
      self%buf = EMPTY_STR; self%n = len(self%buf, kind=i64); return
    end if

    total_length = (num_tokens - 1_i64)*sep_len

    do i = 1_i64, num_tokens
      total_length = total_length + max(tokens(i)%len64(), 0_i64)
    end do

    allocate( character(len=total_length) :: text )

    pos = 1_i64

    do i = 1_i64, num_tokens
      n = max(tokens(i)%len64(), 0_i64)

      if ( n > 0_i64 ) then
        text(pos:pos+n-1_i64) = tokens(i)%buf(:tokens(i)%n)
        pos = pos + n
      end if

      if ( (i < num_tokens) .and. (sep_len > 0_i64) ) then
        text(pos:pos+sep_len-1_i64) = separator
        pos = pos + sep_len
      end if
    end do

    self%n = total_length
    call move_alloc(text, self%buf)
  end procedure join_base

  module procedure length
    if ( .not. allocated(self%buf) ) then
      self_len = -1
    else
      self_len = int(self%n)
    end if
  end procedure length

  module procedure length64
    if ( .not. allocated(self%buf) ) then
      self_len = -1_i64
    else
      self_len = self%n
    end if
  end procedure length64

  module procedure push_chars
    call append(self, substring)
  end procedure push_chars

  module procedure push_string
    if ( allocated(substring%buf) ) then
      call append(self, substring%buf(:substring%n))
    else
      call append(self, EMPTY_STR)
    end if
  end procedure push_string

  module procedure read_file
    call read_cells(self, file, .false., cell_array, row_separator, column_separator, stat, errmsg)
  end procedure read_file

  module procedure read_cells
    character(len=:), allocatable :: ext

    character(len=:), pointer :: errmsg_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
    integer,          target :: dummy_stat

    integer(i64) :: file_length
    integer      :: file_unit
    logical      :: exists

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

    if ( .not. any(TEXT_EXT == ext) ) then
      stat_   = ARG_ERR
      errmsg_ = 'Error reading file "'//file//'" with extension "'//ext//'". To use read_file, the data to '// &
                "be read into a String must be formatted as text with one of the following file extensions: "// &
                join(TEXT_EXT)
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    inquire(file=file, exist=exists, iostat=stat_, iomsg=errmsg_)

    if ( stat_ /= 0 ) then
      stat_ = READ_ERR
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    file_unit = input_unit

    if ( exists ) then
      open( newunit=file_unit, file=file, status="old", form="unformatted", &
            action="read", access="stream", position="rewind", iostat=stat_, iomsg=errmsg_ )
    else
      stat_   = READ_ERR
      errmsg_ = 'Error reading file "'//file//'". No such file exists.'
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    if ( stat_ /= 0 ) then
      stat_ = READ_ERR
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    inquire(file=file, size=file_length, iostat=stat_, iomsg=errmsg_)

    if ( stat_ /= 0 ) then
      stat_ = READ_ERR
      call close_quietly(file_unit)
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    if ( file_length == 0_i64 ) then
      stat_   = READ_ERR
      errmsg_ = 'Error reading file "'//file//'". File is empty.'
      call close_quietly(file_unit)
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    if ( allocated(self%buf) ) deallocate(self%buf, stat=stat_, errmsg=errmsg_)

    if ( stat_ /= 0 ) then
      stat_ = ALLOC_ERR
      call close_quietly(file_unit)
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    allocate( character(len=file_length) :: self%buf, stat=stat_, errmsg=errmsg_ )
    self%n = file_length

    if ( stat_ /= 0 ) then
      stat_ = ALLOC_ERR
      call close_quietly(file_unit)
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    read(unit=file_unit, iostat=stat_, iomsg=errmsg_) self%buf(:self%n)

    if ( stat_ /= 0 ) then
      stat_ = READ_ERR
      call close_quietly(file_unit)
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    close(unit=file_unit, iostat=stat_, iomsg=errmsg_)

    if ( stat_ /= 0 ) then
      stat_ = READ_ERR
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    if ( .not. present(cell_array) ) then
      if ( present(row_separator)    ) write(*,"(a)") LF//'WARNING: Row separator was provided for read of file "'// &
                                                      file//'" without a cell array output. Ignoring argument...'
      if ( present(column_separator) ) write(*,"(a)") LF//'WARNING: Column separator was provided for read of file "'//&
                                                      file//'" without a cell array output. Ignoring argument...'
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    if ( present(row_separator) ) then
      if ( len(row_separator) == 0 ) then
        stat_   = ARG_ERR
        errmsg_ = 'Error: Cannot populate a cell array with the contents of file "'//file//'" '// &
                  "using an empty row separator."
        call stop_if_unhandled(stat_, errmsg_, present(stat))
        return
      end if
    end if

    if ( present(column_separator) ) then
      if ( len(column_separator) == 0 ) then
        stat_   = ARG_ERR
        errmsg_ = 'Error: Cannot populate a cell array with the contents of file "'//file//'" '// &
                  "using an empty column separator."
        call stop_if_unhandled(stat_, errmsg_, present(stat))
        return
      end if
    end if

    cell_block: block
      character(len=:), allocatable :: row_separator_, column_separator_

      type(cell_bounds) :: cell
      integer(i64)      :: last, pos
      integer           :: nrows, ncols, row, col
      logical           :: strip_cr

      row_separator_ = optval(row_separator, LINE_FEED)

      column_separator_ = optval(column_separator, COMMA_DELIMITER)

      strip_cr = (row_separator_ == LF)
      last     = end_of_data(self%buf(:self%n), row_separator_, strip_cr)

      if ( last < 1_i64 ) then
        stat_   = READ_ERR
        errmsg_ = 'Error reading file "'//file//'". File has no data.'
        call stop_if_unhandled(stat_, errmsg_, present(stat))
        return
      end if

      nrows = 1; ncols = 0; row = 1; col = 1; pos = 1_i64

      count_cells: do
        call scan_cell(self%buf(:self%n), last, pos, row_separator_, column_separator_, strip_cr, pairs, cell, stat_)

        if ( stat_ /= 0 ) then
          errmsg_ = 'Error reading file "'//file//'". Quote in row '//str(row)//", column "//str(col)// &
                    " is not closed."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if

        if ( cell%ends == ENDS_COLUMN ) then
          col = col + 1; cycle count_cells
        end if

        if ( row == 1 ) ncols = col

        if ( col > ncols ) then
          stat_   = READ_ERR
          errmsg_ = 'Error reading file "'//file//'". Row '//str(row)//" has "//str(col)// &
                    " columns, but the first row has "//str(ncols)//"."
          call stop_if_unhandled(stat_, errmsg_, present(stat))
          return
        end if

        if ( cell%ends == ENDS_FILE ) exit count_cells

        row = row + 1; col = 1
      end do count_cells

      nrows = row

      allocate( cell_array(nrows,ncols), stat=stat_, errmsg=errmsg_ )

      if ( stat_ /= 0 ) then
        stat_ = ALLOC_ERR
        call stop_if_unhandled(stat_, errmsg_, present(stat))
        return
      end if

      row = 1; col = 1; pos = 1_i64

      transfer_cells: do
        call scan_cell(self%buf(:self%n), last, pos, row_separator_, column_separator_, strip_cr, pairs, cell, stat_)

        if ( cell%escaped ) then
          cell_array(row,col)%buf = without_escapes(self%buf(cell%l:cell%r))//self%buf(cell%tail_l:cell%tail_r)
          cell_array(row,col)%n = len(cell_array(row,col)%buf, kind=i64)
        else if ( cell%quoted ) then
          cell_array(row,col)%buf = self%buf(cell%l:cell%r)//self%buf(cell%tail_l:cell%tail_r)
          cell_array(row,col)%n = len(cell_array(row,col)%buf, kind=i64)
        else
          cell_array(row,col)%buf = self%buf(cell%l:cell%r)
          cell_array(row,col)%n = len(cell_array(row,col)%buf, kind=i64)
        end if

        if ( cell%ends == ENDS_COLUMN ) then
          col = col + 1; cycle transfer_cells
        end if

        cell_array(row,col+1:ncols) = String(EMPTY_STR)

        if ( cell%ends == ENDS_FILE ) exit transfer_cells

        row = row + 1; col = 1
      end do transfer_cells
    end block cell_block

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure read_cells

  module procedure replace_ch_copy
    if ( self%len64() < 1_i64 ) then
      new%buf = EMPTY_STR; new%n = len(new%buf, kind=i64)
    else
      new%buf = replaced(self%buf(:self%n), match, substring, optval(back, .false.))
      new%n = len(new%buf, kind=i64)
    end if
  end procedure replace_ch_copy

  module procedure replace_st_copy
    if ( self%len64() < 1_i64 ) then
      new%buf = EMPTY_STR; new%n = len(new%buf, kind=i64)
    else
      new%buf = replaced(self%buf(:self%n), match%as_str(), substring%as_str(), optval(back, .false.))
      new%n = len(new%buf, kind=i64)
    end if
  end procedure replace_st_copy

  module procedure replace_chst_copy
    if ( self%len64() < 1_i64 ) then
      new%buf = EMPTY_STR; new%n = len(new%buf, kind=i64)
    else
      new%buf = replaced(self%buf(:self%n), match, substring%as_str(), optval(back, .false.))
      new%n = len(new%buf, kind=i64)
    end if
  end procedure replace_chst_copy

  module procedure replace_stch_copy
    if ( self%len64() < 1_i64 ) then
      new%buf = EMPTY_STR; new%n = len(new%buf, kind=i64)
    else
      new%buf = replaced(self%buf(:self%n), match%as_str(), substring, optval(back, .false.))
      new%n = len(new%buf, kind=i64)
    end if
  end procedure replace_stch_copy

  module procedure replace_ch_inplace
    if ( self%len64() < 1_i64 ) then
      self%buf = EMPTY_STR; self%n = len(self%buf, kind=i64)
    else
      self%buf = replaced(self%buf(:self%n), match, substring, optval(back, .false.))
      self%n = len(self%buf, kind=i64)
    end if
  end procedure replace_ch_inplace

  module procedure replace_st_inplace
    if ( self%len64() < 1_i64 ) then
      self%buf = EMPTY_STR; self%n = len(self%buf, kind=i64)
    else
      self%buf = replaced(self%buf(:self%n), match%as_str(), substring%as_str(), optval(back, .false.))
      self%n = len(self%buf, kind=i64)
    end if
  end procedure replace_st_inplace

  module procedure replace_chst_inplace
    if ( self%len64() < 1_i64 ) then
      self%buf = EMPTY_STR; self%n = len(self%buf, kind=i64)
    else
      self%buf = replaced(self%buf(:self%n), match, substring%as_str(), optval(back, .false.))
      self%n = len(self%buf, kind=i64)
    end if
  end procedure replace_chst_inplace

  module procedure replace_stch_inplace
    if ( self%len64() < 1_i64 ) then
      self%buf = EMPTY_STR; self%n = len(self%buf, kind=i64)
    else
      self%buf = replaced(self%buf(:self%n), match%as_str(), substring, optval(back, .false.))
      self%n = len(self%buf, kind=i64)
    end if
  end procedure replace_stch_inplace

  module procedure trim_copy
    if ( self%len64() < 1_i64 ) then
      new%buf = EMPTY_STR; new%n = len(new%buf, kind=i64)
    else
      new%buf = without_whitespace(self%buf(:self%n)); new%n = len(new%buf, kind=i64)
    end if
  end procedure trim_copy

  module procedure trim_inplace
    if ( self%len64() < 1_i64 ) then
      self%buf = EMPTY_STR; self%n = len(self%buf, kind=i64)
    else
      self%buf = without_whitespace(self%buf(:self%n)); self%n = len(self%buf, kind=i64)
    end if
  end procedure trim_inplace

  module procedure optval_chars
    if ( present(x) ) then
      value = x
    else
      value = default
    end if
  end procedure optval_chars

  module procedure optval_int
    value = default
    if ( present(x) ) value = x
  end procedure optval_int

  module procedure optval_logical
    value = default
    if ( present(x) ) value = x
  end procedure optval_logical

  module procedure stop_if_unhandled
    if ( (stat /= 0) .and. (.not. handled) ) error stop errmsg(:len_trim(errmsg))
  end procedure stop_if_unhandled

  module procedure close_quietly
    integer :: ignored

    close(unit=unit, iostat=ignored)
  end procedure close_quietly

  module procedure write_file
    call write_cells(self, .false., cell_array, file, row_separator, column_separator, append, stat, errmsg)
  end procedure write_file

  module procedure write_cells
    character(len=:), allocatable :: ext

    character(len=:), allocatable :: row_separator_, column_separator_
    character(len=:), pointer :: errmsg_
    logical :: append_
    integer, pointer :: stat_

    character(len=1024), target :: dummy_msg
    integer,          target :: dummy_stat

    integer(i64), allocatable :: lengths(:,:)
    logical,      allocatable :: quoted(:,:)

    logical :: special(0:255)

    integer(i64) :: total_len, pos
    integer      :: file_unit, nrows, ncols, row_sep_len, col_sep_len, row, col
    logical      :: exists

    dummy_msg=EMPTY_STR; dummy_stat=0
    total_len=0_i64; pos=0_i64
    file_unit=0; nrows=0; ncols=0; row_sep_len=0; col_sep_len=0; row=0; col=0
    exists=.false.

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

    if ( .not. any(TEXT_EXT == ext) ) then
      stat_ = ARG_ERR
      errmsg_ = 'Cannot write to file "'//file//'" due to unsupported file extension "'//ext//'". '// &
                'Supported file extensions: '//join(TEXT_EXT)
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    row_separator_ = optval(row_separator, LINE_FEED)

    column_separator_ = optval(column_separator, COMMA_DELIMITER)

    append_ = optval(append, NO_APPEND)

    nrows = size(cell_array, dim=1)
    ncols = size(cell_array, dim=2)
    row_sep_len = len(row_separator_)
    col_sep_len = len(column_separator_)

    if ( allocated(self%buf) ) deallocate(self%buf, stat=stat_, errmsg=errmsg_)

    if ( stat_ /= 0 ) then
      stat_ = ALLOC_ERR
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    allocate( lengths(nrows,ncols), quoted(nrows,ncols), stat=stat_, errmsg=errmsg_ )

    if ( stat_ /= 0 ) then
      stat_ = ALLOC_ERR
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    special = .false.                                  ! Part of a separator next to a whole one is ambiguous
    special(iachar(QQUOTE)) = .true.; special(iachar(CR)) = .true.; special(iachar(LF)) = .true.

    do row = 1, col_sep_len
      special(iand(ichar(column_separator_(row:row)), 255)) = .true.
    end do

    do row = 1, row_sep_len
      special(iand(ichar(row_separator_(row:row)), 255)) = .true.
    end do

    do col = 1, ncols
      do row = 1, nrows
        lengths(row,col) = 0_i64
        quoted(row,col)  = (ncols == 1) .and. (row == nrows)            ! An empty last row would read as a blank line

        if ( quoted(row,col) ) lengths(row,col) = 2_i64

        if ( .not. allocated(cell_array(row,col)%buf) ) cycle
        if ( cell_array(row,col)%n == 0_i64 ) cycle

        associate ( cell => cell_array(row,col) )
          lengths(row,col) = cell%n
          quoted(row,col)  = needs_quotes(cell%buf(:cell%n), special, pairs)

          if ( quoted(row,col) ) lengths(row,col) = cell%n + 2_i64 + quote_count(cell%buf(:cell%n))
        end associate
      end do
    end do

    total_len = sum(lengths) + int(nrows, kind=i64)*row_sep_len + &
                int(nrows, kind=i64)*(ncols - 1)*col_sep_len

    allocate( character(len=total_len) :: self%buf, stat=stat_, errmsg=errmsg_ )
    self%n = total_len

    if ( stat_ /= 0 ) then
      stat_ = ALLOC_ERR
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    row = 1; col = 1; pos = 1_i64; positional_transfers: do while ( (nrows > 0) .and. (ncols > 0) )
      if ( quoted(row,col) .and. (lengths(row,col) == 2_i64) ) then
        self%buf(pos:pos+1_i64) = QQUOTE//QQUOTE
        pos = pos + 2_i64
      else if ( lengths(row,col) > 0_i64 ) then
        associate ( cell => cell_array(row,col) )
          if ( quoted(row,col) ) then
            call write_quoted(cell%buf(:cell%n), self%buf(pos:pos+lengths(row,col)-1_i64))
          else
            self%buf(pos:pos+lengths(row,col)-1_i64) = cell%buf(:cell%n)
          end if
        end associate

        pos = pos + lengths(row,col)
      end if

      if ( col < ncols ) then
        if ( col_sep_len > 0 ) self%buf(pos:pos+col_sep_len-1_i64) = column_separator_
        pos = pos + col_sep_len; col = col + 1; cycle
      else
        if ( row_sep_len > 0 ) self%buf(pos:pos+row_sep_len-1_i64) = row_separator_

        if ( row < nrows ) then
          pos = pos + row_sep_len; row = row + 1; col = 1; cycle
        else
          exit
        end if
      end if
    end do positional_transfers

    inquire(file=file, exist=exists, iostat=stat_, iomsg=errmsg_)

    if ( stat_ /= 0 ) then
      stat_ = WRITE_ERR
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    file_unit = output_unit

    if ( .not. exists ) then
      open( newunit=file_unit, file=file, status="new", form="unformatted", &
            action="write", access="stream", iostat=stat_, iomsg=errmsg_ )
    else
      if ( .not. append_ ) then
        open( newunit=file_unit, file=file, status="replace", form="unformatted", &
              action="write", access="stream", iostat=stat_, iomsg=errmsg_ )
      else
        open( newunit=file_unit, file=file, status="old", form="unformatted", &
              action="write", access="stream", position="append", iostat=stat_, iomsg=errmsg_ )
      end if
    end if

    if ( stat_ /= 0 ) then
      stat_ = WRITE_ERR
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    write(unit=file_unit, iostat=stat_, iomsg=errmsg_) self%buf(:self%n)

    if ( stat_ /= 0 ) then
      stat_ = WRITE_ERR
      call close_quietly(file_unit)
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    close(unit=file_unit, iostat=stat_, iomsg=errmsg_)

    if ( stat_ /= 0 ) then
      stat_ = WRITE_ERR
      call stop_if_unhandled(stat_, errmsg_, present(stat))
      return
    end if

    call stop_if_unhandled(stat_, errmsg_, present(stat))
  end procedure write_cells

  module procedure write_string
    if ( substring%len64() < 1_i64 ) then
      write(unit=unit, fmt="(a)", iostat=iostat, iomsg=iomsg) EMPTY_STR
    else
      write(unit=unit, fmt="(a)", iostat=iostat, iomsg=iomsg) substring%buf(:substring%n)
    end if
  end procedure write_string

  ! Delimited text ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pure function end_of_data(text, row_sep, strip_cr) result(last)
    !-------------------------------------------------------------------------------------------------------------------
    !! Returns the position of the last character of `text` that is not part of a trailing row separator.
    !-------------------------------------------------------------------------------------------------------------------
    character(len=*), intent(in) :: text, row_sep
    logical,          intent(in) :: strip_cr
    integer(i64)                 :: last

    integer(i64) :: n

    last = len(text, kind=i64); n = len(row_sep, kind=i64)

    do while ( last >= n )
      if ( text(last-n+1_i64:last) /= row_sep ) exit
      last = last - n

      if ( strip_cr .and. (last >= 1_i64) ) then
        if ( text(last:last) == CR ) last = last - 1_i64
      end if
    end do
  end function end_of_data

  pure subroutine scan_cell(text, last, pos, row_sep, col_sep, strip_cr, pairs, cell, status)
    !-------------------------------------------------------------------------------------------------------------------
    !! Finds the cell starting at `pos` and moves `pos` to the start of the next one. A cell that starts with a double
    !! quote runs to the closing quote, with two double quotes inside it standing for one, and separators inside it are
    !! data. Any other cell runs to the next separator. Text between a closing quote and the separator is kept as is.
    !! With `pairs`, separators are also data up to the closing parenthesis of a cell that starts with one.
    !-------------------------------------------------------------------------------------------------------------------
    character(len=*),  intent(in)    :: text, row_sep, col_sep
    integer(i64),      intent(in)    :: last
    integer(i64),      intent(inout) :: pos
    logical,           intent(in)    :: strip_cr, pairs
    type(cell_bounds), intent(out)   :: cell
    integer,           intent(out)   :: status

    character(len=1) :: col_first, row_first
    integer(i64)     :: i, start, finish, col_len, row_len

    status = 0
    col_len = len(col_sep, kind=i64); row_len = len(row_sep, kind=i64)
    col_first = col_sep(1:1); row_first = row_sep(1:1)

    if ( pos <= last ) then
      if ( text(pos:pos) == QQUOTE ) then
        cell%quoted = .true.; cell%l = pos + 1_i64
        i = pos + 1_i64

        do
          if ( i > last ) then
            status = READ_ERR; return
          end if

          if ( text(i:i) /= QQUOTE ) then
            i = i + 1_i64; cycle
          end if

          if ( i < last ) then
            if ( text(i+1_i64:i+1_i64) == QQUOTE ) then
              cell%escaped = .true.; i = i + 2_i64; cycle
            end if
          end if

          exit
        end do

        cell%r = i - 1_i64; pos = i + 1_i64
      end if
    end if

    start = pos; i = pos; cell%ends = ENDS_FILE; finish = last

    if ( pairs .and. (.not. cell%quoted) .and. (pos <= last) ) then
      if ( text(pos:pos) == "(" ) i = pos + index(text(pos:last), ")", kind=i64)
    end if

    do while ( i <= last )
      if ( text(i:i) == col_first ) then
        if ( i + col_len - 1_i64 <= last ) then
          if ( text(i:i+col_len-1_i64) == col_sep ) then
            cell%ends = ENDS_COLUMN; finish = i - 1_i64; pos = i + col_len; exit
          end if
        end if
      end if

      if ( text(i:i) == row_first ) then
        if ( i + row_len - 1_i64 <= last ) then
          if ( text(i:i+row_len-1_i64) == row_sep ) then
            cell%ends = ENDS_ROW; finish = i - 1_i64; pos = i + row_len; exit
          end if
        end if
      end if

      i = i + 1_i64
    end do

    if ( cell%ends == ENDS_FILE ) pos = last + 1_i64

    if ( strip_cr .and. (cell%ends /= ENDS_COLUMN) .and. (finish >= start) ) then
      if ( text(finish:finish) == CR ) finish = finish - 1_i64
    end if

    if ( cell%quoted ) then
      cell%tail_l = start; cell%tail_r = finish
    else
      cell%l = start; cell%r = finish
    end if
  end subroutine scan_cell

  pure function without_escapes(text) result(plain)
    character(len=*), intent(in)  :: text
    character(len=:), allocatable :: plain

    integer(i64) :: i, o, n

    n = len(text, kind=i64)
    allocate( character(len=n) :: plain )

    i = 1_i64; o = 0_i64

    do while ( i <= n )
      o = o + 1_i64; plain(o:o) = text(i:i)
      if ( text(i:i) == QQUOTE ) i = i + 1_i64
      i = i + 1_i64
    end do

    plain = plain(:o)
  end function without_escapes

  pure function needs_quotes(text, special, pairs) result(needed)
    character(len=*), intent(in) :: text
    logical,          intent(in) :: special(0:255), pairs
    logical                      :: needed

    integer(i64) :: i

    needed = .false.

    if ( pairs .and. (len(text) >= 2) ) then
      if ( (text(1:1) == "(") .and. (text(len(text):len(text)) == ")") ) return
    end if

    do i = 1_i64, len(text, kind=i64)
      if ( special(iand(ichar(text(i:i)), 255)) ) then
        needed = .true.; return
      end if
    end do
  end function needs_quotes

  pure function quote_count(text) result(n)
    character(len=*), intent(in) :: text
    integer(i64)                 :: n

    integer(i64) :: i

    n = 0_i64

    do i = 1_i64, len(text, kind=i64)
      if ( text(i:i) == QQUOTE ) n = n + 1_i64
    end do
  end function quote_count

  pure subroutine write_quoted(text, quoted_text)
    character(len=*), intent(in)  :: text
    character(len=*), intent(out) :: quoted_text

    integer(i64) :: i, o

    quoted_text(1:1) = QQUOTE
    o = 1_i64

    do i = 1_i64, len(text, kind=i64)
      o = o + 1_i64; quoted_text(o:o) = text(i:i)

      if ( text(i:i) == QQUOTE ) then
        o = o + 1_i64; quoted_text(o:o) = QQUOTE
      end if
    end do

    quoted_text(o+1_i64:o+1_i64) = QQUOTE
  end subroutine write_quoted

  pure function without_whitespace(text) result(trimmed)
    character(len=*), intent(in)  :: text
    character(len=:), allocatable :: trimmed

    character(len=*), parameter :: WHITESPACE = SPACE//TAB//CR//LF//achar(11)//achar(12)
    integer(i64)                :: first

    first = verify(text, WHITESPACE, kind=i64)

    if ( first == 0_i64 ) then
      trimmed = EMPTY_STR
    else
      trimmed = text(first:verify(text, WHITESPACE, back=.true., kind=i64))
    end if
  end function without_whitespace

  pure function replaced(text, match, substring, back) result(new)
    !-------------------------------------------------------------------------------------------------------------------
    !! Returns `text` with non-overlapping occurrences of `match` replaced, in one pass to count them and one to copy.
    !! Matches are taken from the start of `text`, or from the end with `back`, which matters when they could overlap.
    !-------------------------------------------------------------------------------------------------------------------
    character(len=*), intent(in)  :: text, match, substring
    logical,          intent(in)  :: back
    character(len=:), allocatable :: new

    integer(i64) :: text_len, match_len, sub_len, matches, i, o

    text_len = len(text, kind=i64); match_len = len(match, kind=i64); sub_len = len(substring, kind=i64)

    if ( (match_len < 1_i64) .or. (match_len > text_len) ) then
      new = text; return
    end if

    matches = 0_i64

    if ( back ) then
      i = text_len

      do while ( i >= match_len )
        if ( text(i-match_len+1_i64:i) == match ) then
          matches = matches + 1_i64; i = i - match_len
        else
          i = i - 1_i64
        end if
      end do

      allocate( character(len=text_len+matches*(sub_len-match_len)) :: new )

      i = text_len; o = len(new, kind=i64)

      do while ( i >= 1_i64 )
        if ( i >= match_len ) then
          if ( text(i-match_len+1_i64:i) == match ) then
            if ( sub_len > 0_i64 ) new(o-sub_len+1_i64:o) = substring
            o = o - sub_len; i = i - match_len
            cycle
          end if
        end if

        new(o:o) = text(i:i)
        o = o - 1_i64; i = i - 1_i64
      end do
    else
      i = 1_i64

      do while ( i <= text_len-match_len+1_i64 )
        if ( text(i:i+match_len-1_i64) == match ) then
          matches = matches + 1_i64; i = i + match_len
        else
          i = i + 1_i64
        end if
      end do

      allocate( character(len=text_len+matches*(sub_len-match_len)) :: new )

      i = 1_i64; o = 1_i64

      do while ( i <= text_len )
        if ( i <= text_len-match_len+1_i64 ) then
          if ( text(i:i+match_len-1_i64) == match ) then
            if ( sub_len > 0_i64 ) new(o:o+sub_len-1_i64) = substring
            o = o + sub_len; i = i + match_len
            cycle
          end if
        end if

        new(o:o) = text(i:i)
        o = o + 1_i64; i = i + 1_i64
      end do
    end if
  end function replaced

  pure subroutine append(self, text)
    !-------------------------------------------------------------------------------------------------------------------
    !! Appends in place when there is room, or else at least doubles the capacity, so that repeated appends are linear.
    !-------------------------------------------------------------------------------------------------------------------
    class(String),    intent(inout) :: self
    character(len=*), intent(in)    :: text

    character(len=:), allocatable :: grown
    integer(i64)                  :: old_len, new_len

    old_len = 0_i64
    if ( allocated(self%buf) ) old_len = self%n

    new_len = old_len + len(text, kind=i64)

    if ( allocated(self%buf) ) then
      if ( new_len <= len(self%buf, kind=i64) ) then
        self%buf(old_len+1_i64:new_len) = text
        self%n = new_len; return
      end if
    end if

    allocate( character(len=max(new_len, 2_i64*old_len, 32_i64)) :: grown )

    if ( old_len > 0_i64 ) grown(:old_len) = self%buf(:old_len)
    grown(old_len+1_i64:new_len) = text

    call move_alloc(grown, self%buf)
    self%n = new_len
  end subroutine append
end submodule string_methods
