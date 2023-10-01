submodule (io_fortran_lib) join_split
  !---------------------------------------------------------------------------------------------------------------------
  !! This submodule provides module procedure implementations for the **public interfaces** `join` and `split`.
  !---------------------------------------------------------------------------------------------------------------------
  implicit none (type, external)

  contains ! Procedure bodies for module subprograms <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

  module procedure join_char
    type(String)                  :: temp_String
    character(len=:), allocatable :: separator_

    if ( .not. present(separator) ) then
      separator_ = SPACE
    else
      separator_ = separator
    end if

    temp_String = join(String(tokens), separator=separator_)

    if ( temp_String%len() < 1 ) then
      new = EMPTY_STR
    else
      new = temp_String%s
    end if
  end procedure join_char

  module procedure join_string
    type(String)                  :: token_pair(2)
    character(len=:), allocatable :: separator_
    integer(i64)                  :: num_tokens

    num_tokens = size(tokens, kind=i64)

    if ( num_tokens == 1_i64 ) then
      if ( tokens(1_i64)%len64() < 1_i64 ) then
        new%s = EMPTY_STR; return
      else
        new%s = tokens(1_i64)%s; return
      end if
    end if

    if ( .not. present(separator) ) then
      separator_ = SPACE
    else
      separator_ = separator
    end if

    if ( num_tokens > 500_i64 ) then
      new = join(tokens=[ join(tokens(:num_tokens/2_i64), separator_), &
                join(tokens(1_i64+num_tokens/2_i64:), separator_) ], separator=separator_)
    else
      call new%join_base(tokens=tokens, separator=separator_)
    end if
  end procedure join_string

  module procedure split_char
    character(len=:), allocatable :: separator_

    if ( .not. present(separator) ) then
      separator_ = SPACE
    else
      separator_ = separator
    end if

    tokens = split(String(substring), separator=separator_)
  end procedure split_char

  module procedure split_string
    character(len=:), allocatable :: separator_
    integer(i64)                  :: substring_len, l, i
    integer                       :: sep_len, num_seps, sep, token, current

    substring_len=0_i64; l=0_i64; i=0_i64; sep_len=0; num_seps=0; sep=0; token=0; current=0

    substring_len = substring%len64()

    if ( substring_len < 1_i64 ) then
      allocate( tokens(1) ); tokens(1)%s = EMPTY_STR; return
    end if

    if ( .not. present(separator) ) then
      separator_ = SPACE
    else
      separator_ = separator
    end if

    sep_len = len(separator_)

    if ( sep_len == 0 ) then
      allocate( tokens(substring_len) )
      do i = 1_i64, substring_len
        tokens(i)%s = substring%s(i:i)
      end do
      return
    end if

    num_seps = substring%count(match=separator_)

    if ( num_seps == 0 ) then
      allocate( tokens(1) ); tokens(1)%s = substring%s; return
    end if

    allocate( tokens(num_seps + 1) )

    sep = iachar(separator_(1:1))

    i = 1_i64; l = 1_i64; token = 1; positional_transfers: do
      current = iachar(substring%s(i:i))

      if ( current /= sep ) then
        i = i + 1_i64; cycle
      end if

      if ( sep_len == 1 ) then
        tokens(token)%s = substring%s(l:i-1)
        if ( token == num_seps ) then
          tokens(num_seps+1)%s = substring%s(i+1:); return
        end if
        token = token + 1; i = i + 1_i64; l = i; cycle
      else
        if ( substring%s(i:i+sep_len-1) == separator_ ) then
          tokens(token)%s = substring%s(l:i-1)
          if ( token == num_seps ) then
            tokens(num_seps+1)%s = substring%s(i+sep_len:); return
          end if
          token = token + 1; i = i + sep_len; l = i; cycle
        else
          i = i + 1_i64; cycle
        end if
      end if
    end do positional_transfers
  end procedure split_string
end submodule join_split
