! Generated from fypp/io_fortran_lib/operators_impl.fypp by fypp/generate.sh. Edit that file, not this one.
#include "iofl_config.inc"
submodule (io_fortran_lib) operators
  !---------------------------------------------------------------------------------------------------------------------
  !! This submodule provides module procedure implementations for the **public interfaces** `operator(//)`,
  !! `operator(+)`, `operator(-)`, `operator(**)`, `operator(==)`, and `operator(/=)`.
  !---------------------------------------------------------------------------------------------------------------------
  implicit none (type, external)

  contains ! Procedure bodies for module subprograms <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

  module procedure string_concatenation
    if ( Stringl%len64() < 1_i64 ) then
      if ( Stringr%len64() < 1_i64 ) then
        new%buf = EMPTY_STR; new%n = len(new%buf, kind=i64); return
      else
        new%buf = Stringr%buf(:Stringr%n); new%n = len(new%buf, kind=i64); return
      end if
    end if

    if ( Stringr%len64() < 1_i64 ) then
      new%buf = Stringl%buf(:Stringl%n); new%n = len(new%buf, kind=i64); return
    end if

    new%buf = Stringl%buf(:Stringl%n)//Stringr%buf(:Stringr%n); new%n = len(new%buf, kind=i64)
  end procedure string_concatenation

  module procedure string_char_concatenation
    if ( Stringl%len64() < 1_i64 ) then
      if ( len(charsr) < 1 ) then
        new%buf = EMPTY_STR; new%n = len(new%buf, kind=i64); return
      else
        new%buf = charsr; new%n = len(new%buf, kind=i64); return
      end if
    end if

    if ( len(charsr) < 1 ) then
      new%buf = Stringl%buf(:Stringl%n); new%n = len(new%buf, kind=i64); return
    end if

    new%buf = Stringl%buf(:Stringl%n)//charsr; new%n = len(new%buf, kind=i64)
  end procedure string_char_concatenation

  module procedure char_string_concatenation
    if ( len(charsl) < 1 ) then
      if ( Stringr%len64() < 1_i64 ) then
        new%buf = EMPTY_STR; new%n = len(new%buf, kind=i64); return
      else
        new%buf = Stringr%buf(:Stringr%n); new%n = len(new%buf, kind=i64); return
      end if
    end if

    if ( Stringr%len64() < 1_i64 ) then
      new%buf = charsl; new%n = len(new%buf, kind=i64); return
    end if

    new%buf = charsl//Stringr%buf(:Stringr%n); new%n = len(new%buf, kind=i64)
  end procedure char_string_concatenation

  module procedure string_concat_plus
    if ( Stringl%len64() < 1_i64 ) then
      if ( Stringr%len64() < 1_i64 ) then
        new%buf = EMPTY_STR; new%n = len(new%buf, kind=i64); return
      else
        new%buf = Stringr%buf(:Stringr%n); new%n = len(new%buf, kind=i64); return
      end if
    end if

    if ( Stringr%len64() < 1_i64 ) then
      new%buf = Stringl%buf(:Stringl%n); new%n = len(new%buf, kind=i64); return
    end if

    new%buf = Stringl%buf(:Stringl%n)//Stringr%buf(:Stringr%n); new%n = len(new%buf, kind=i64)
  end procedure string_concat_plus

  module procedure string_char_concat_plus
    if ( Stringl%len64() < 1_i64 ) then
      if ( len(charsr) < 1 ) then
        new%buf = EMPTY_STR; new%n = len(new%buf, kind=i64); return
      else
        new%buf = charsr; new%n = len(new%buf, kind=i64); return
      end if
    end if

    if ( len(charsr) < 1 ) then
      new%buf = Stringl%buf(:Stringl%n); new%n = len(new%buf, kind=i64); return
    end if

    new%buf = Stringl%buf(:Stringl%n)//charsr; new%n = len(new%buf, kind=i64)
  end procedure string_char_concat_plus

  module procedure char_string_concat_plus
    if ( len(charsl) < 1 ) then
      if ( Stringr%len64() < 1_i64 ) then
        new%buf = EMPTY_STR; new%n = len(new%buf, kind=i64); return
      else
        new%buf = Stringr%buf(:Stringr%n); new%n = len(new%buf, kind=i64); return
      end if
    end if

    if ( Stringr%len64() < 1_i64 ) then
      new%buf = charsl; new%n = len(new%buf, kind=i64); return
    end if

    new%buf = charsl//Stringr%buf(:Stringr%n); new%n = len(new%buf, kind=i64)
  end procedure char_string_concat_plus

  module procedure char_excision
    type(String) :: Stringl

    Stringl%buf = charsl; Stringl%n = len(Stringl%buf, kind=i64)

    if ( Stringl%len64() < 1_i64 ) then
      new%buf = EMPTY_STR; new%n = len(new%buf, kind=i64); return
    end if

    if ( len(charsr) < 1 ) then
      new%buf = Stringl%buf(:Stringl%n); new%n = len(new%buf, kind=i64); return
    end if

    new = Stringl%replace(match=charsr, substring=EMPTY_STR)
  end procedure char_excision

  module procedure string_excision
    if ( Stringl%len64() < 1_i64 ) then
      new%buf = EMPTY_STR; new%n = len(new%buf, kind=i64); return
    end if

    if ( Stringr%len64() < 1_i64 ) then
      new%buf = Stringl%buf(:Stringl%n); new%n = len(new%buf, kind=i64); return
    end if

    new = Stringl%replace(match=Stringr%buf(:Stringr%n), substring=EMPTY_STR)
  end procedure string_excision

  module procedure string_char_excision
    if ( Stringl%len64() < 1_i64 ) then
      new%buf = EMPTY_STR; new%n = len(new%buf, kind=i64); return
    end if

    if ( len(charsr) < 1 ) then
      new%buf = Stringl%buf(:Stringl%n); new%n = len(new%buf, kind=i64); return
    end if

    new = Stringl%replace(match=charsr, substring=EMPTY_STR)
  end procedure string_char_excision

  module procedure char_string_excision
    type(String) :: Stringl

    Stringl%buf = charsl; Stringl%n = len(Stringl%buf, kind=i64)

    if ( Stringl%len64() < 1_i64 ) then
      new%buf = EMPTY_STR; new%n = len(new%buf, kind=i64); return
    end if

    if ( Stringr%len64() < 1_i64 ) then
      new%buf = Stringl%buf(:Stringl%n); new%n = len(new%buf, kind=i64); return
    end if

    new = Stringl%replace(match=Stringr%buf(:Stringr%n), substring=EMPTY_STR)
  end procedure char_string_excision

  module procedure repeat_chars
    new = repeat(char_base, ncopies=max(ncopies, 0))
  end procedure repeat_chars

  module procedure repeat_chars_1d
    call repeat_into(char_base, ncopies, new)
  end procedure repeat_chars_1d

  module procedure repeat_chars_2d
    call repeat_into(char_base, ncopies, new)
  end procedure repeat_chars_2d

  module procedure repeat_chars_3d
    call repeat_into(char_base, ncopies, new)
  end procedure repeat_chars_3d

  module procedure repeat_chars_4d
    call repeat_into(char_base, ncopies, new)
  end procedure repeat_chars_4d

  module procedure repeat_chars_5d
    call repeat_into(char_base, ncopies, new)
  end procedure repeat_chars_5d

  module procedure repeat_chars_6d
    call repeat_into(char_base, ncopies, new)
  end procedure repeat_chars_6d

  module procedure repeat_chars_7d
    call repeat_into(char_base, ncopies, new)
  end procedure repeat_chars_7d

  module procedure repeat_chars_8d
    call repeat_into(char_base, ncopies, new)
  end procedure repeat_chars_8d

  module procedure repeat_chars_9d
    call repeat_into(char_base, ncopies, new)
  end procedure repeat_chars_9d

  module procedure repeat_chars_10d
    call repeat_into(char_base, ncopies, new)
  end procedure repeat_chars_10d

  module procedure repeat_chars_11d
    call repeat_into(char_base, ncopies, new)
  end procedure repeat_chars_11d

  module procedure repeat_chars_12d
    call repeat_into(char_base, ncopies, new)
  end procedure repeat_chars_12d

  module procedure repeat_chars_13d
    call repeat_into(char_base, ncopies, new)
  end procedure repeat_chars_13d

  module procedure repeat_chars_14d
    call repeat_into(char_base, ncopies, new)
  end procedure repeat_chars_14d

  module procedure repeat_chars_15d
    call repeat_into(char_base, ncopies, new)
  end procedure repeat_chars_15d


  pure elemental subroutine repeat_into(char_base, ncopies, new)
    character(len=*), intent(in)  :: char_base
    integer,          intent(in)  :: ncopies
    character(len=*), intent(out) :: new

    new = repeat(char_base, ncopies=max(ncopies, 0))
  end subroutine repeat_into

  module procedure repeat_String
    if ( String_base%len64() < 1_i64 ) then
      new%buf = EMPTY_STR; new%n = len(new%buf, kind=i64); return
    end if

    new%buf = repeat(String_base%buf(:String_base%n), ncopies=max(ncopies, 0))
    new%n = len(new%buf, kind=i64)
  end procedure repeat_String

  module procedure string_lt
    if ( allocated(Stringl%buf) .and. allocated(Stringr%buf) ) then
      res = text_order(Stringl%buf(:Stringl%n), Stringr%buf(:Stringr%n)) < 0
    else if ( allocated(Stringl%buf) ) then
      res = text_order(Stringl%buf(:Stringl%n), EMPTY_STR) < 0
    else if ( allocated(Stringr%buf) ) then
      res = text_order(EMPTY_STR, Stringr%buf(:Stringr%n)) < 0
    else
      res = text_order(EMPTY_STR, EMPTY_STR) < 0
    end if
  end procedure string_lt

  module procedure string_char_lt
    if ( allocated(Stringl%buf) ) then
      res = text_order(Stringl%buf(:Stringl%n), charsr) < 0
    else
      res = text_order(EMPTY_STR, charsr) < 0
    end if
  end procedure string_char_lt

  module procedure char_string_lt
    if ( allocated(Stringr%buf) ) then
      res = text_order(charsl, Stringr%buf(:Stringr%n)) < 0
    else
      res = text_order(charsl, EMPTY_STR) < 0
    end if
  end procedure char_string_lt

  module procedure string_le
    if ( allocated(Stringl%buf) .and. allocated(Stringr%buf) ) then
      res = text_order(Stringl%buf(:Stringl%n), Stringr%buf(:Stringr%n)) <= 0
    else if ( allocated(Stringl%buf) ) then
      res = text_order(Stringl%buf(:Stringl%n), EMPTY_STR) <= 0
    else if ( allocated(Stringr%buf) ) then
      res = text_order(EMPTY_STR, Stringr%buf(:Stringr%n)) <= 0
    else
      res = text_order(EMPTY_STR, EMPTY_STR) <= 0
    end if
  end procedure string_le

  module procedure string_char_le
    if ( allocated(Stringl%buf) ) then
      res = text_order(Stringl%buf(:Stringl%n), charsr) <= 0
    else
      res = text_order(EMPTY_STR, charsr) <= 0
    end if
  end procedure string_char_le

  module procedure char_string_le
    if ( allocated(Stringr%buf) ) then
      res = text_order(charsl, Stringr%buf(:Stringr%n)) <= 0
    else
      res = text_order(charsl, EMPTY_STR) <= 0
    end if
  end procedure char_string_le

  module procedure string_gt
    if ( allocated(Stringl%buf) .and. allocated(Stringr%buf) ) then
      res = text_order(Stringl%buf(:Stringl%n), Stringr%buf(:Stringr%n)) > 0
    else if ( allocated(Stringl%buf) ) then
      res = text_order(Stringl%buf(:Stringl%n), EMPTY_STR) > 0
    else if ( allocated(Stringr%buf) ) then
      res = text_order(EMPTY_STR, Stringr%buf(:Stringr%n)) > 0
    else
      res = text_order(EMPTY_STR, EMPTY_STR) > 0
    end if
  end procedure string_gt

  module procedure string_char_gt
    if ( allocated(Stringl%buf) ) then
      res = text_order(Stringl%buf(:Stringl%n), charsr) > 0
    else
      res = text_order(EMPTY_STR, charsr) > 0
    end if
  end procedure string_char_gt

  module procedure char_string_gt
    if ( allocated(Stringr%buf) ) then
      res = text_order(charsl, Stringr%buf(:Stringr%n)) > 0
    else
      res = text_order(charsl, EMPTY_STR) > 0
    end if
  end procedure char_string_gt

  module procedure string_ge
    if ( allocated(Stringl%buf) .and. allocated(Stringr%buf) ) then
      res = text_order(Stringl%buf(:Stringl%n), Stringr%buf(:Stringr%n)) >= 0
    else if ( allocated(Stringl%buf) ) then
      res = text_order(Stringl%buf(:Stringl%n), EMPTY_STR) >= 0
    else if ( allocated(Stringr%buf) ) then
      res = text_order(EMPTY_STR, Stringr%buf(:Stringr%n)) >= 0
    else
      res = text_order(EMPTY_STR, EMPTY_STR) >= 0
    end if
  end procedure string_ge

  module procedure string_char_ge
    if ( allocated(Stringl%buf) ) then
      res = text_order(Stringl%buf(:Stringl%n), charsr) >= 0
    else
      res = text_order(EMPTY_STR, charsr) >= 0
    end if
  end procedure string_char_ge

  module procedure char_string_ge
    if ( allocated(Stringr%buf) ) then
      res = text_order(charsl, Stringr%buf(:Stringr%n)) >= 0
    else
      res = text_order(charsl, EMPTY_STR) >= 0
    end if
  end procedure char_string_ge

  module procedure string_equivalence
    integer(i64) :: Stringl_len, Stringr_len

    Stringl_len = max(Stringl%len64(), 0_i64)
    Stringr_len = max(Stringr%len64(), 0_i64)

    if ( Stringl_len /= Stringr_len ) then
      equal = .false.; return
    end if

    if ( Stringl_len < 1 ) then
      equal = .true.; return
    end if

    equal = ( Stringl%buf(:Stringl%n) == Stringr%buf(:Stringr%n) )
  end procedure string_equivalence

  module procedure string_char_equivalence
    integer(i64) :: Stringl_len, charsr_len

    Stringl_len = max(Stringl%len64(), 0_i64)
    charsr_len  = len(charsr, kind=i64)

    if ( Stringl_len /= charsr_len ) then
      equal = .false.; return
    end if

    if ( Stringl_len < 1 ) then
      equal = .true.; return
    end if

    equal = ( Stringl%buf(:Stringl%n) == charsr )
  end procedure string_char_equivalence

  module procedure char_string_equivalence
    integer(i64) :: charsl_len, Stringr_len

    charsl_len  = len(charsl, kind=i64)
    Stringr_len = max(Stringr%len64(), 0_i64)

    if ( charsl_len /= Stringr_len ) then
      equal = .false.; return
    end if

    if ( charsl_len < 1 ) then
      equal = .true.; return
    end if

    equal = ( charsl == Stringr%buf(:Stringr%n) )
  end procedure char_string_equivalence

  module procedure string_nonequivalence
    integer(i64) :: Stringl_len, Stringr_len

    Stringl_len = max(Stringl%len64(), 0_i64)
    Stringr_len = max(Stringr%len64(), 0_i64)

    if ( Stringl_len /= Stringr_len ) then
      unequal = .true.; return
    end if

    if ( Stringl_len < 1 ) then
      unequal = .false.; return
    end if

    unequal = ( Stringl%buf(:Stringl%n) /= Stringr%buf(:Stringr%n) )
  end procedure string_nonequivalence

  module procedure string_char_nonequivalence
    integer(i64) :: Stringl_len, charsr_len

    Stringl_len = max(Stringl%len64(), 0_i64)
    charsr_len  = len(charsr, kind=i64)

    if ( Stringl_len /= charsr_len ) then
      unequal = .true.; return
    end if

    if ( Stringl_len < 1 ) then
      unequal = .false.; return
    end if

    unequal = ( Stringl%buf(:Stringl%n) /= charsr )
  end procedure string_char_nonequivalence

  module procedure char_string_nonequivalence
    integer(i64) :: charsl_len, Stringr_len

    charsl_len  = len(charsl, kind=i64)
    Stringr_len = max(Stringr%len64(), 0_i64)

    if ( charsl_len /= Stringr_len ) then
      unequal = .true.; return
    end if

    if ( charsl_len < 1 ) then
      unequal = .false.; return
    end if

    unequal = ( charsl /= Stringr%buf(:Stringr%n) )
  end procedure char_string_nonequivalence

  pure function text_order(a, b) result(order)
    !-------------------------------------------------------------------------------------------------------------------
    !! Returns a negative, zero or positive number as `a` comes before, matches or comes after `b`, comparing bytes.
    !! The intrinsic comparisons are not used since they pad with blanks, which would disagree with `==` on length.
    !-------------------------------------------------------------------------------------------------------------------
    character(len=*), intent(in) :: a, b
    integer                      :: order

    integer(i64) :: i

    do i = 1_i64, min(len(a, kind=i64), len(b, kind=i64))
      order = iand(ichar(a(i:i)), 255) - iand(ichar(b(i:i)), 255)
      if ( order /= 0 ) return
    end do

    order = 0
    if ( len(a, kind=i64) < len(b, kind=i64) ) order = -1
    if ( len(a, kind=i64) > len(b, kind=i64) ) order = 1
  end function text_order
end submodule operators
