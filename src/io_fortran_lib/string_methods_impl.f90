submodule (io_fortran_lib) string_methods
  !---------------------------------------------------------------------------------------------------------------------
  !! This submodule provides module procedure implementations for the **type-bound procedures** of type `String`.
  !---------------------------------------------------------------------------------------------------------------------
  implicit none (type, external)

  ! Definitions and interfaces ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=1), target :: LINE_FEED = LF, COMMA_DELIMITER = COMMA
  logical,          target :: NO_APPEND = .false.

  contains ! Procedure bodies for module subprograms <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

  module procedure as_str
    if ( self%len() < 1 ) then
      string_slice = EMPTY_STR
    else
      string_slice = self%s
    end if
  end procedure as_str

  module procedure count_substring_chars
    integer(i64) :: self_len, match_len, max_pos, upper_ind, i, j
    integer      :: first_char, last_char

    self_len=0_i64; match_len=0_i64; max_pos=0_i64; upper_ind=0_i64; i=0_i64; j=0_i64; first_char=0; last_char=0

    self_len  = self%len64()
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

        if ( iachar(self%s(i:i)) /= first_char ) then
          i = i + 1_i64; cycle
        else
          occurrences = occurrences + 1; i = i + 1_i64; cycle
        end if
      end do
    end if

    if ( match_len == 2_i64 ) then
      i = 1_i64; do
        if ( i > max_pos ) return

        if ( iachar(self%s(i:i)) /= first_char ) then
          i = i + 1_i64; cycle
        end if

        if ( iachar(self%s(i+1_i64:i+1_i64)) /= last_char ) then
          i = i + 1_i64; cycle
        else
          occurrences = occurrences + 1; i = i + 2_i64; cycle
        end if
      end do
    end if

    i = 1_i64; do
      if ( i > max_pos ) return

      if ( iachar(self%s(i:i)) /= first_char ) then
        i = i + 1_i64; cycle
      end if

      upper_ind = i+match_len-1_i64

      if ( iachar(self%s(upper_ind:upper_ind)) /= last_char ) then
        i = i + 1_i64; cycle
      end if

      if ( self%s(i:upper_ind) == match ) then
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

    self_len  = self%len64()
    match_len = match%len64()

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
    first_char = iachar(match%s(1:1)); last_char = iachar(match%s(match_len:match_len))

    if ( match_len == 1_i64 ) then
      i = 1_i64; do
        if ( i > max_pos ) return

        if ( iachar(self%s(i:i)) /= first_char ) then
          i = i + 1_i64; cycle
        else
          occurrences = occurrences + 1; i = i + 1_i64; cycle
        end if
      end do
    end if

    if ( match_len == 2_i64 ) then
      i = 1_i64; do
        if ( i > max_pos ) return

        if ( iachar(self%s(i:i)) /= first_char ) then
          i = i + 1_i64; cycle
        end if

        if ( iachar(self%s(i+1_i64:i+1_i64)) /= last_char ) then
          i = i + 1_i64; cycle
        else
          occurrences = occurrences + 1; i = i + 2_i64; cycle
        end if
      end do
    end if

    i = 1_i64; do
      if ( i > max_pos ) return

      if ( iachar(self%s(i:i)) /= first_char ) then
        i = i + 1_i64; cycle
      end if

      upper_ind = i+match_len-1_i64

      if ( iachar(self%s(upper_ind:upper_ind)) /= last_char ) then
        i = i + 1_i64; cycle
      end if

      if ( self%s(i:upper_ind) == match%s ) then
        occurrences = occurrences + 1; i = i + match_len; cycle
      else
        i = i + 1_i64; cycle
      end if
    end do
  end procedure count_substring_string

  module procedure empty
    self%s = EMPTY_STR
  end procedure empty

  module procedure join_into_self
    character(len=:), allocatable :: separator_
    integer(i64)                  :: num_tokens

    num_tokens = size(tokens, kind=i64)

    if ( num_tokens == 1_i64 ) then
      if ( tokens(1_i64)%len64() < 1_i64 ) then
        self%s = EMPTY_STR; return
      else
        self%s = tokens(1_i64)%s; return
      end if
    end if

    if ( .not. present(separator) ) then
      separator_ = SPACE
    else
      separator_ = separator
    end if

    if ( num_tokens > 500_i64 ) then
      call self%join( tokens=[ join(tokens(:num_tokens/2_i64), separator_), &
        join(tokens(1_i64+num_tokens/2_i64:), separator_) ], separator=separator_ )
    else
      call self%join_base(tokens=tokens, separator=separator_)
    end if
  end procedure join_into_self

  module procedure join_base
    integer(i64), dimension(size(tokens, kind=i64)) :: lengths, cumm_lengths
    integer(i64)                                    :: num_tokens, sep_len, total_length, pos, i

    lengths=0_i64; cumm_lengths=0_i64; num_tokens=0_i64; sep_len=0_i64; total_length=0_i64; pos=0_i64; i=0_i64

    num_tokens = size(tokens, kind=i64)

    lengths = tokens%len64()
    sep_len = len(separator, kind=i64)

    where ( lengths == -1_i64 ) lengths = 0_i64
    total_length = sum(lengths)

    if ( total_length == 0_i64 ) then
      self%s = EMPTY_STR; return
    end if

    cumm_lengths(1_i64) = 1_i64

    do i = 2_i64, num_tokens
      cumm_lengths(i) = sum( lengths(:i-1_i64) ) + 1_i64
    end do

    if ( allocated(self%s) ) deallocate(self%s)

    total_length = total_length + (num_tokens - 1_i64)*sep_len
    allocate( character(len=total_length) :: self%s )

    positional_transfer: do i = 1_i64, num_tokens
      pos = cumm_lengths(i) + (i - 1_i64)*sep_len
      if ( lengths(i) > 0_i64 ) then
        self%s(pos:pos+lengths(i)-1_i64) = tokens(i)%s
        if ( sep_len > 0_i64 ) then
          if ( i < num_tokens ) self%s(pos+lengths(i):pos+lengths(i)+sep_len-1_i64) = separator
        end if
      else
        if ( sep_len > 0_i64 ) then
          if ( i < num_tokens ) self%s(pos:pos+sep_len-1_i64) = separator
        end if
      end if
    end do positional_transfer
  end procedure join_base

  module procedure length
    if ( .not. allocated(self%s) ) then
      self_len = -1
    else
      self_len = len(self%s)
    end if
  end procedure length

  module procedure length64
    if ( .not. allocated(self%s) ) then
      self_len = -1_i64
    else
      self_len = len(self%s, kind=i64)
    end if
  end procedure length64

  module procedure push_chars
    if ( self%len() < 1 ) then
      self%s = substring
    else
      self%s = self%s//substring
    end if
  end procedure push_chars

  module procedure push_string
    if ( self%len() < 1 ) then
      if ( substring%len() < 1 ) then
        self%s = EMPTY_STR
      else
        self%s = substring%s
      end if
    else
      if ( substring%len() < 1 ) then
        return
      else
        self%s = self%s//substring%s
      end if
    end if
  end procedure push_string

  module procedure read_file
    character(len=:), allocatable :: ext

    character(len=:), pointer :: errmsg_
    integer,          pointer :: stat_

    character(len=0), target :: dummy_msg
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
      return
    end if

    inquire(file=file, exist=exists, iostat=stat_, iomsg=errmsg_)

    if ( stat_ /= 0 ) then
      stat_ = READ_ERR; return
    end if

    file_unit = input_unit

    if ( exists ) then
      open( newunit=file_unit, file=file, status="old", form="unformatted", &
            action="read", access="stream", position="rewind", iostat=stat_, iomsg=errmsg_ )
    else
      stat_   = READ_ERR
      errmsg_ = 'Error reading file "'//file//'". No such file exists.'
      return
    end if

    if ( stat_ /= 0 ) then
      stat_ = READ_ERR; return
    end if

    inquire(file=file, size=file_length, iostat=stat_, iomsg=errmsg_)

    if ( stat_ /= 0 ) then
      stat_ = READ_ERR; return
    end if

    if ( file_length == 0_i64 ) then
      stat_   = READ_ERR
      errmsg_ = 'Error reading file "'//file//'". File is empty.'
      return
    end if

    if ( allocated(self%s) ) deallocate(self%s, stat=stat_, errmsg=errmsg_)

    if ( stat_ /= 0 ) then
      stat_ = ALLOC_ERR; return
    end if

    allocate( character(len=file_length) :: self%s, stat=stat_, errmsg=errmsg_ )

    if ( stat_ /= 0 ) then
      stat_ = ALLOC_ERR; return
    end if

    read(unit=file_unit, iostat=stat_, iomsg=errmsg_) self%s

    if ( stat_ /= 0 ) then
      stat_ = READ_ERR; return
    end if

    close(unit=file_unit, iostat=stat_, iomsg=errmsg_)

    if ( stat_ /= 0 ) then
      stat_ = READ_ERR; return
    end if

    if ( .not. present(cell_array) ) then
      if ( present(row_separator)    ) write(*,"(a)") LF//'WARNING: Row separator was provided for read of file "'// &
                                                      file//'" without a cell array output. Ignoring argument...'
      if ( present(column_separator) ) write(*,"(a)") LF//'WARNING: Column separator was provided for read of file "'//&
                                                      file//'" without a cell array output. Ignoring argument...'
      return
    end if

    if ( present(row_separator) ) then
      if ( len(row_separator) == 0 ) then
        stat_   = ARG_ERR
        errmsg_ = 'Error: Cannot populate a cell array with the contents of file "'//file//'" '// &
                  "using an empty row separator."
        return
      end if
    end if

    if ( present(column_separator) ) then
      if ( len(column_separator) == 0 ) then
        stat_   = ARG_ERR
        errmsg_ = 'Error: Cannot populate a cell array with the contents of file "'//file//'" '// &
                  "using an empty column separator."
        return
      end if
    end if

    cell_block: block
      character(len=:), pointer :: row_separator_, column_separator_

      integer(i64) :: l, i
      integer      :: nrows, ncols, row, col, row_sep, row_sep_len, col_sep, col_sep_len, quote, current
      logical      :: in_quote

      l=0_i64; i=0_i64
      nrows=0; ncols=0; row=0; col=0; row_sep=0; row_sep_len=0; col_sep=0; col_sep_len=0; quote=0; current=0
      in_quote=.false.

      if ( .not. present(row_separator) ) then
        row_separator_ => LINE_FEED
      else
        row_separator_ => row_separator
      end if

      if ( .not. present(column_separator) ) then
        column_separator_ => COMMA_DELIMITER
      else
        column_separator_ => column_separator
      end if

      row_sep_len = len(row_separator_); col_sep_len = len(column_separator_)
      row_sep = iachar(row_separator_(1:1)); col_sep = iachar(column_separator_(1:1))
      quote = iachar(QQUOTE)

      nrows = self%count(match=row_separator_)

      ncols = 1; i = 1_i64; get_ncols: do
        current = iachar(self%s(i:i))

        if ( (current /= quote) .and. (current /= col_sep) .and. (current /= row_sep) ) then
          i = i + 1_i64; cycle
        end if

        if ( current == quote ) then
          in_quote = (.not. in_quote); i = i + 1_i64; cycle
        end if

        if ( current == col_sep ) then
          if ( in_quote ) then
            i = i + 1_i64; cycle
          end if

          if ( col_sep_len == 1 ) then
            ncols = ncols + 1_i64; i = i + 1_i64; cycle
          else
            if ( self%s(i:i+col_sep_len-1_i64) == column_separator_ ) then
              ncols = ncols + 1_i64; i = i + col_sep_len; cycle
            else
              i = i + 1_i64; cycle
            end if
          end if
        end if

        if ( current == row_sep ) then
          if ( row_sep_len == 1 ) then
            exit get_ncols
          else
            if ( self%s(i:i+row_sep_len-1_i64) == row_separator_ ) then
              exit get_ncols
            else
              i = i + 1_i64; cycle
            end if
          end if
        end if
      end do get_ncols

      allocate( cell_array(nrows,ncols), stat=stat_, errmsg=errmsg_ )

      if ( stat_ /= 0 ) then
        stat_ = ALLOC_ERR; return
      end if

      row = 1; col = 1; l = 1_i64; i = 1_i64; positional_transfers: do
        current = iachar(self%s(i:i))

        if ( (current /= quote) .and. (current /= col_sep) .and. (current /= row_sep) ) then
          i = i + 1_i64; cycle
        end if

        if ( current == quote ) then
          in_quote = (.not. in_quote); i = i + 1_i64; cycle
        end if

        if ( current == col_sep ) then
          if ( in_quote ) then
            i = i + 1_i64; cycle
          end if

          if ( col_sep_len == 1 ) then
            cell_array(row,col)%s = self%s(l:i-1); i = i + 1_i64; l = i; col = col + 1; cycle
          else
            if ( self%s(i:i+col_sep_len-1_i64) == column_separator_ ) then
              cell_array(row,col)%s = self%s(l:i-1); i = i + col_sep_len; l = i; col = col + 1; cycle
            else
              i = i + 1_i64; cycle
            end if
          end if
        end if

        if ( current == row_sep ) then
          if ( row_sep_len == 1 ) then
            cell_array(row,col)%s = self%s(l:i-1)
            if ( row == nrows ) return
            i = i + 1_i64; l = i; col = 1; row = row + 1; cycle
          else
            if ( self%s(i:i+row_sep_len-1_i64) == row_separator_ ) then
              cell_array(row,col)%s = self%s(l:i-1)
              if ( row == nrows ) return
              i = i + row_sep_len; l = i; col = 1; row = row + 1; cycle
            else
              i = i + 1_i64; cycle
            end if
          end if
        end if
      end do positional_transfers
    end block cell_block
  end procedure read_file

  module procedure replace_ch_copy
    integer :: i, self_len, match_len, substring_len, diff_len
    logical :: back_

    i=0; self_len=0; match_len=0; substring_len=0; diff_len=0; back_=.false.

    self_len      = self%len()
    match_len     = len(match)
    substring_len = len(substring)

    if ( self_len < 1 ) then
      new%s = EMPTY_STR; return
    end if

    if ( (match_len < 1) .or. (match_len > self_len) ) then
      new%s = self%s; return
    end if

    if ( .not. present(back) ) then
      back_ = .false.
    else
      back_ = back
    end if

    new%s = self%s

    if ( .not. back_ ) then
      i = 1; diff_len = 0
      match_and_replace_forward: do while ( i <= self_len )
        if ( self%s(i:i) == match(1:1) ) then
          if ( i+match_len-1 > self_len ) exit match_and_replace_forward

          if ( self%s(i:i+match_len-1) == match ) then
            new%s = new%s(:i-1+diff_len)//substring//new%s(i+match_len+diff_len:)
            diff_len = diff_len + ( substring_len - match_len )
            i = i + match_len; cycle match_and_replace_forward
          else
            i = i + 1; cycle match_and_replace_forward
          end if
        else
          i = i + 1; cycle match_and_replace_forward
        end if
      end do match_and_replace_forward
    else
      i = self_len
      match_and_replace_backward: do while ( i > 0 )
        if ( self%s(i:i) == match(match_len:match_len) ) then
          if ( i-match_len+1 < 1 ) exit match_and_replace_backward

          if ( self%s(i-match_len+1:i) == match ) then
            new%s = new%s(:i-match_len)//substring//new%s(i+1:)
            i = i - match_len; cycle match_and_replace_backward
          else
            i = i - 1; cycle match_and_replace_backward
          end if
        else
          i = i - 1; cycle match_and_replace_backward
        end if
      end do match_and_replace_backward
    end if
  end procedure replace_ch_copy

  module procedure replace_st_copy
    character(len=:), allocatable :: substring_
    integer                       :: i, self_len, match_len, substring_len, diff_len
    logical                       :: back_

    i=0; self_len=0; match_len=0; substring_len=0; diff_len=0; back_=.false.

    self_len      = self%len()
    match_len     = match%len()
    substring_len = substring%len()

    if ( self_len < 1 ) then
      new%s = EMPTY_STR; return
    end if

    if ( (match_len < 1) .or. (match_len > self_len) ) then
      new%s = self%s; return
    end if

    if ( substring_len < 1 ) then
      substring_ = EMPTY_STR
    else
      substring_ = substring%s
    end if

    if ( .not. present(back) ) then
      back_ = .false.
    else
      back_ = back
    end if

    new%s = self%s

    if ( .not. back_ ) then
      i = 1; diff_len = 0
      match_and_replace_forward: do while ( i <= self_len )
        if ( self%s(i:i) == match%s(1:1) ) then
          if ( i+match_len-1 > self_len ) exit match_and_replace_forward

          if ( self%s(i:i+match_len-1) == match%s ) then
            new%s = new%s(:i-1+diff_len)//substring_//new%s(i+match_len+diff_len:)
            diff_len = diff_len + ( substring_len - match_len )
            i = i + match_len; cycle match_and_replace_forward
          else
            i = i + 1; cycle match_and_replace_forward
          end if
        else
          i = i + 1; cycle match_and_replace_forward
        end if
      end do match_and_replace_forward
    else
      i = self_len
      match_and_replace_backward: do while ( i > 0 )
        if ( self%s(i:i) == match%s(match_len:match_len) ) then
          if ( i-match_len+1 < 1 ) exit match_and_replace_backward

          if ( self%s(i-match_len+1:i) == match%s ) then
            new%s = new%s(:i-match_len)//substring_//new%s(i+1:)
            i = i - match_len; cycle match_and_replace_backward
          else
            i = i - 1; cycle match_and_replace_backward
          end if
        else
          i = i - 1; cycle match_and_replace_backward
        end if
      end do match_and_replace_backward
    end if
  end procedure replace_st_copy

  module procedure replace_chst_copy
    character(len=:), allocatable :: substring_
    integer                       :: i, self_len, match_len, substring_len, diff_len
    logical                       :: back_

    i=0; self_len=0; match_len=0; substring_len=0; diff_len=0; back_=.false.

    self_len      = self%len()
    match_len     = len(match)
    substring_len = substring%len()

    if ( self_len < 1 ) then
      new%s = EMPTY_STR; return
    end if

    if ( (match_len < 1) .or. (match_len > self_len) ) then
      new%s = self%s; return
    end if

    if ( substring_len < 1 ) then
      substring_ = EMPTY_STR
    else
      substring_ = substring%s
    end if

    if ( .not. present(back) ) then
      back_ = .false.
    else
      back_ = back
    end if

    new%s = self%s

    if ( .not. back_ ) then
      i = 1; diff_len = 0
      match_and_replace_forward: do while ( i <= self_len )
        if ( self%s(i:i) == match(1:1) ) then
          if ( i+match_len-1 > self_len ) exit match_and_replace_forward

          if ( self%s(i:i+match_len-1) == match ) then
            new%s = new%s(:i-1+diff_len)//substring_//new%s(i+match_len+diff_len:)
            diff_len = diff_len + ( substring_len - match_len )
            i = i + match_len; cycle match_and_replace_forward
          else
            i = i + 1; cycle match_and_replace_forward
          end if
        else
          i = i + 1; cycle match_and_replace_forward
        end if
      end do match_and_replace_forward
    else
      i = self_len
      match_and_replace_backward: do while ( i > 0 )
        if ( self%s(i:i) == match(match_len:match_len) ) then
          if ( i-match_len+1 < 1 ) exit match_and_replace_backward

          if ( self%s(i-match_len+1:i) == match ) then
            new%s = new%s(:i-match_len)//substring_//new%s(i+1:)
            i = i - match_len; cycle match_and_replace_backward
          else
            i = i - 1; cycle match_and_replace_backward
          end if
        else
          i = i - 1; cycle match_and_replace_backward
        end if
      end do match_and_replace_backward
    end if
  end procedure replace_chst_copy

  module procedure replace_stch_copy
    integer :: i, self_len, match_len, substring_len, diff_len
    logical :: back_

    i=0; self_len=0; match_len=0; substring_len=0; diff_len=0; back_=.false.

    self_len      = self%len()
    match_len     = match%len()
    substring_len = len(substring)

    if ( self_len < 1 ) then
      new%s = EMPTY_STR; return
    end if

    if ( (match_len < 1) .or. (match_len > self_len) ) then
      new%s = self%s; return
    end if

    if ( .not. present(back) ) then
      back_ = .false.
    else
      back_ = back
    end if

    new%s = self%s

    if ( .not. back_ ) then
      i = 1; diff_len = 0
      match_and_replace_forward: do while ( i <= self_len )
        if ( self%s(i:i) == match%s(1:1) ) then
          if ( i+match_len-1 > self_len ) exit match_and_replace_forward

          if ( self%s(i:i+match_len-1) == match%s ) then
            new%s = new%s(:i-1+diff_len)//substring//new%s(i+match_len+diff_len:)
            diff_len = diff_len + ( substring_len - match_len )
            i = i + match_len; cycle match_and_replace_forward
          else
            i = i + 1; cycle match_and_replace_forward
          end if
        else
          i = i + 1; cycle match_and_replace_forward
        end if
      end do match_and_replace_forward
    else
      i = self_len
      match_and_replace_backward: do while ( i > 0 )
        if ( self%s(i:i) == match%s(match_len:match_len) ) then
          if ( i-match_len+1 < 1 ) exit match_and_replace_backward

          if ( self%s(i-match_len+1:i) == match%s ) then
            new%s = new%s(:i-match_len)//substring//new%s(i+1:)
            i = i - match_len; cycle match_and_replace_backward
          else
            i = i - 1; cycle match_and_replace_backward
          end if
        else
          i = i - 1; cycle match_and_replace_backward
        end if
      end do match_and_replace_backward
    end if
  end procedure replace_stch_copy

  module procedure replace_ch_inplace
    type(String) :: new
    integer      :: i, self_len, match_len, substring_len, diff_len
    logical      :: back_

    i=0; self_len=0; match_len=0; substring_len=0; diff_len=0; back_=.false.

    self_len      = self%len()
    match_len     = len(match)
    substring_len = len(substring)

    if ( self_len < 1 ) then
      self%s = EMPTY_STR; return
    end if

    if ( (match_len < 1) .or. (match_len > self_len) ) return

    if ( .not. present(back) ) then
      back_ = .false.
    else
      back_ = back
    end if

    new%s = self%s

    if ( .not. back_ ) then
      i = 1; diff_len = 0
      match_and_replace_forward: do while ( i <= self_len )
        if ( self%s(i:i) == match(1:1) ) then
          if ( i+match_len-1 > self_len ) exit match_and_replace_forward

          if ( self%s(i:i+match_len-1) == match ) then
            new%s = new%s(:i-1+diff_len)//substring//new%s(i+match_len+diff_len:)
            diff_len = diff_len + ( substring_len - match_len )
            i = i + match_len; cycle match_and_replace_forward
          else
            i = i + 1; cycle match_and_replace_forward
          end if
        else
          i = i + 1; cycle match_and_replace_forward
        end if
      end do match_and_replace_forward
    else
      i = self_len
      match_and_replace_backward: do while ( i > 0 )
        if ( self%s(i:i) == match(match_len:match_len) ) then
          if ( i-match_len+1 < 1 ) exit match_and_replace_backward

          if ( self%s(i-match_len+1:i) == match ) then
            new%s = new%s(:i-match_len)//substring//new%s(i+1:)
            i = i - match_len; cycle match_and_replace_backward
          else
            i = i - 1; cycle match_and_replace_backward
          end if
        else
          i = i - 1; cycle match_and_replace_backward
        end if
      end do match_and_replace_backward
    end if

    self%s = new%s
  end procedure replace_ch_inplace

  module procedure replace_st_inplace
    type(String)                  :: new
    character(len=:), allocatable :: substring_
    integer                       :: i, self_len, match_len, substring_len, diff_len
    logical                       :: back_

    i=0; self_len=0; match_len=0; substring_len=0; diff_len=0; back_=.false.

    self_len      = self%len()
    match_len     = match%len()
    substring_len = substring%len()

    if ( self_len < 1 ) then
      self%s = EMPTY_STR; return
    end if

    if ( (match_len < 1) .or. (match_len > self_len) ) return

    if ( substring_len < 1 ) then
      substring_ = EMPTY_STR
    else
      substring_ = substring%s
    end if

    if ( .not. present(back) ) then
      back_ = .false.
    else
      back_ = back
    end if

    new%s = self%s

    if ( .not. back_ ) then
      i = 1; diff_len = 0
      match_and_replace_forward: do while ( i <= self_len )
        if ( self%s(i:i) == match%s(1:1) ) then
          if ( i+match_len-1 > self_len ) exit match_and_replace_forward

          if ( self%s(i:i+match_len-1) == match%s ) then
            new%s = new%s(:i-1+diff_len)//substring_//new%s(i+match_len+diff_len:)
            diff_len = diff_len + ( substring_len - match_len )
            i = i + match_len; cycle match_and_replace_forward
          else
            i = i + 1; cycle match_and_replace_forward
          end if
        else
          i = i + 1; cycle match_and_replace_forward
        end if
      end do match_and_replace_forward
    else
      i = self_len
      match_and_replace_backward: do while ( i > 0 )
        if ( self%s(i:i) == match%s(match_len:match_len) ) then
          if ( i-match_len+1 < 1 ) exit match_and_replace_backward

          if ( self%s(i-match_len+1:i) == match%s ) then
            new%s = new%s(:i-match_len)//substring_//new%s(i+1:)
            i = i - match_len; cycle match_and_replace_backward
          else
            i = i - 1; cycle match_and_replace_backward
          end if
        else
          i = i - 1; cycle match_and_replace_backward
        end if
      end do match_and_replace_backward
    end if

    self%s = new%s
  end procedure replace_st_inplace

  module procedure replace_chst_inplace
    type(String)                  :: new
    character(len=:), allocatable :: substring_
    integer                       :: i, self_len, match_len, substring_len, diff_len
    logical                       :: back_

    i=0; self_len=0; match_len=0; substring_len=0; diff_len=0; back_=.false.

    self_len      = self%len()
    match_len     = len(match)
    substring_len = substring%len()

    if ( self_len < 1 ) then
      self%s = EMPTY_STR; return
    end if

    if ( (match_len < 1) .or. (match_len > self_len) ) return

    if ( substring_len < 1 ) then
      substring_ = EMPTY_STR
    else
      substring_ = substring%s
    end if

    if ( .not. present(back) ) then
      back_ = .false.
    else
      back_ = back
    end if

    new%s = self%s

    if ( .not. back_ ) then
      i = 1; diff_len = 0
      match_and_replace_forward: do while ( i <= self_len )
        if ( self%s(i:i) == match(1:1) ) then
          if ( i+match_len-1 > self_len ) exit match_and_replace_forward

          if ( self%s(i:i+match_len-1) == match ) then
            new%s = new%s(:i-1+diff_len)//substring_//new%s(i+match_len+diff_len:)
            diff_len = diff_len + ( substring_len - match_len )
            i = i + match_len; cycle match_and_replace_forward
          else
            i = i + 1; cycle match_and_replace_forward
          end if
        else
          i = i + 1; cycle match_and_replace_forward
        end if
      end do match_and_replace_forward
    else
      i = self_len
      match_and_replace_backward: do while ( i > 0 )
        if ( self%s(i:i) == match(match_len:match_len) ) then
          if ( i-match_len+1 < 1 ) exit match_and_replace_backward

          if ( self%s(i-match_len+1:i) == match ) then
            new%s = new%s(:i-match_len)//substring_//new%s(i+1:)
            i = i - match_len; cycle match_and_replace_backward
          else
            i = i - 1; cycle match_and_replace_backward
          end if
        else
          i = i - 1; cycle match_and_replace_backward
        end if
      end do match_and_replace_backward
    end if

    self%s = new%s
  end procedure replace_chst_inplace

  module procedure replace_stch_inplace
    type(String) :: new
    integer      :: i, self_len, match_len, substring_len, diff_len
    logical      :: back_

    i=0; self_len=0; match_len=0; substring_len=0; diff_len=0; back_=.false.

    self_len      = self%len()
    match_len     = match%len()
    substring_len = len(substring)

    if ( self_len < 1 ) then
      self%s = EMPTY_STR; return
    end if

    if ( (match_len < 1) .or. (match_len > self_len) ) return

    if ( .not. present(back) ) then
      back_ = .false.
    else
      back_ = back
    end if

    new%s = self%s

    if ( .not. back_ ) then
      i = 1; diff_len = 0
      match_and_replace_forward: do while ( i <= self_len )
        if ( self%s(i:i) == match%s(1:1) ) then
          if ( i+match_len-1 > self_len ) exit match_and_replace_forward

          if ( self%s(i:i+match_len-1) == match%s ) then
            new%s = new%s(:i-1+diff_len)//substring//new%s(i+match_len+diff_len:)
            diff_len = diff_len + ( substring_len - match_len )
            i = i + match_len; cycle match_and_replace_forward
          else
            i = i + 1; cycle match_and_replace_forward
          end if
        else
          i = i + 1; cycle match_and_replace_forward
        end if
      end do match_and_replace_forward
    else
      i = self_len
      match_and_replace_backward: do while ( i > 0 )
        if ( self%s(i:i) == match%s(match_len:match_len) ) then
          if ( i-match_len+1 < 1 ) exit match_and_replace_backward

          if ( self%s(i-match_len+1:i) == match%s ) then
            new%s = new%s(:i-match_len)//substring//new%s(i+1:)
            i = i - match_len; cycle match_and_replace_backward
          else
            i = i - 1; cycle match_and_replace_backward
          end if
        else
          i = i - 1; cycle match_and_replace_backward
        end if
      end do match_and_replace_backward
    end if

    self%s = new%s
  end procedure replace_stch_inplace

  module procedure trim_copy
    if ( self%len() < 1 ) then
      new%s = EMPTY_STR
    else
      new%s = trim(adjustl(self%s))
    end if
  end procedure trim_copy

  module procedure trim_inplace
    if ( self%len() < 1 ) then
      self%s = EMPTY_STR
    else
      self%s = trim(adjustl(self%s))
    end if
  end procedure trim_inplace

  module procedure write_file
    character(len=:), allocatable :: ext

    character(len=:), pointer :: row_separator_, column_separator_, errmsg_
    logical,          pointer :: append_
    integer,          pointer :: stat_

    character(len=0), target :: dummy_msg
    integer,          target :: dummy_stat

    integer(i64), allocatable :: lengths(:,:)

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
      return
    end if

    if ( .not. present(row_separator) ) then
      row_separator_ => LINE_FEED
    else
      row_separator_ => row_separator
    end if

    if ( .not. present(column_separator) ) then
      column_separator_ => COMMA_DELIMITER
    else
      column_separator_ => column_separator
    end if

    if ( .not. present(append) ) then
      append_ => NO_APPEND
    else
      append_ => append
    end if

    nrows = size(cell_array, dim=1)
    ncols = size(cell_array, dim=2)
    row_sep_len = len(row_separator_)
    col_sep_len = len(column_separator_)

    if ( allocated(self%s) ) deallocate(self%s, stat=stat_, errmsg=errmsg_)

    if ( stat_ /= 0 ) then
      stat_ = ALLOC_ERR; return
    end if

    lengths = cell_array%len64()
    total_len = sum(lengths) + int(nrows*row_sep_len, kind=i64) + int(nrows*(ncols - 1)*col_sep_len, kind=i64)

    allocate( character(len=total_len) :: self%s, stat=stat_, errmsg=errmsg_ )

    if ( stat_ /= 0 ) then
      stat_ = ALLOC_ERR; return
    end if

    row = 1; col = 1; pos = 1_i64; positional_transfers: do
      if ( lengths(row,col) > 0_i64 ) then
        self%s(pos:pos+lengths(row,col)-1_i64) = cell_array(row,col)%s
        pos = pos + lengths(row,col)
      end if

      if ( col < ncols ) then
        if ( col_sep_len > 0 ) self%s(pos:pos+col_sep_len-1_i64) = column_separator_
        pos = pos + col_sep_len; col = col + 1; cycle
      else
        if ( row_sep_len > 0 ) self%s(pos:pos+row_sep_len-1_i64) = row_separator_

        if ( row < nrows ) then
          pos = pos + row_sep_len; row = row + 1; col = 1; cycle
        else
          exit
        end if
      end if
    end do positional_transfers

    inquire(file=file, exist=exists, iostat=stat_, iomsg=errmsg_)

    if ( stat_ /= 0 ) then
      stat_ = WRITE_ERR; return
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
      stat_ = WRITE_ERR; return
    end if

    write(unit=file_unit, iostat=stat_, iomsg=errmsg_) self%s

    if ( stat_ /= 0 ) then
      stat_ = WRITE_ERR; return
    end if

    close(unit=file_unit, iostat=stat_, iomsg=errmsg_)

    if ( stat_ /= 0 ) then
      stat_ = WRITE_ERR; return
    end if
  end procedure write_file

  module procedure write_string
    if ( substring%len() < 1 ) then
      write(unit=unit, fmt="(a)", iostat=iostat, iomsg=iomsg) EMPTY_STR
    else
      write(unit=unit, fmt="(a)", iostat=iostat, iomsg=iomsg) substring%s
    end if
  end procedure write_string

  module procedure scrub
    if ( allocated(self%s) ) deallocate(self%s)
  end procedure scrub
end submodule string_methods
