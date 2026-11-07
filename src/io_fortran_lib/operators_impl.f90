#include "iofl_config.inc"
submodule (io_fortran_lib) operators
  !---------------------------------------------------------------------------------------------------------------------
  !! This submodule provides module procedure implementations for the **public interfaces** `operator(//)`,
  !! `operator(+)`, `operator(-)`, `operator(**)`, `operator(==)`, and `operator(/=)`.
  !---------------------------------------------------------------------------------------------------------------------
  implicit none (type, external)

  contains ! Procedure bodies for module subprograms <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

  module procedure string_concatenation
    if ( Stringl%len() < 1 ) then
      if ( Stringr%len() < 1 ) then
        new%s = EMPTY_STR; return
      else
        new%s = Stringr%s; return
      end if
    end if

    if ( Stringr%len() < 1 ) then
      new%s = Stringl%s; return
    end if

    new%s = Stringl%s//Stringr%s
  end procedure string_concatenation

  module procedure string_char_concatenation
    if ( Stringl%len() < 1 ) then
      if ( len(charsr) < 1 ) then
        new%s = EMPTY_STR; return
      else
        new%s = charsr; return
      end if
    end if

    if ( len(charsr) < 1 ) then
      new%s = Stringl%s; return
    end if

    new%s = Stringl%s//charsr
  end procedure string_char_concatenation

  module procedure char_string_concatenation
    if ( len(charsl) < 1 ) then
      if ( Stringr%len() < 1 ) then
        new%s = EMPTY_STR; return
      else
        new%s = Stringr%s; return
      end if
    end if

    if ( Stringr%len() < 1 ) then
      new%s = charsl; return
    end if

    new%s = charsl//Stringr%s
  end procedure char_string_concatenation

  module procedure string_concat_plus
    if ( Stringl%len() < 1 ) then
      if ( Stringr%len() < 1 ) then
        new%s = EMPTY_STR; return
      else
        new%s = Stringr%s; return
      end if
    end if

    if ( Stringr%len() < 1 ) then
      new%s = Stringl%s; return
    end if

    new%s = Stringl%s//Stringr%s
  end procedure string_concat_plus

  module procedure string_char_concat_plus
    if ( Stringl%len() < 1 ) then
      if ( len(charsr) < 1 ) then
        new%s = EMPTY_STR; return
      else
        new%s = charsr; return
      end if
    end if

    if ( len(charsr) < 1 ) then
      new%s = Stringl%s; return
    end if

    new%s = Stringl%s//charsr
  end procedure string_char_concat_plus

  module procedure char_string_concat_plus
    if ( len(charsl) < 1 ) then
      if ( Stringr%len() < 1 ) then
        new%s = EMPTY_STR; return
      else
        new%s = Stringr%s; return
      end if
    end if

    if ( Stringr%len() < 1 ) then
      new%s = charsl; return
    end if

    new%s = charsl//Stringr%s
  end procedure char_string_concat_plus

  module procedure char_excision
    type(String) :: Stringl

    Stringl%s = charsl

    if ( Stringl%len() < 1 ) then
      new%s = EMPTY_STR; return
    end if

    if ( len(charsr) < 1 ) then
      new%s = Stringl%s; return
    end if

    new = Stringl%replace(match=charsr, substring=EMPTY_STR)
  end procedure char_excision

  module procedure string_excision
    if ( Stringl%len() < 1 ) then
      new%s = EMPTY_STR; return
    end if

    if ( Stringr%len() < 1 ) then
      new%s = Stringl%s; return
    end if

    new = Stringl%replace(match=Stringr%s, substring=EMPTY_STR)
  end procedure string_excision

  module procedure string_char_excision
    if ( Stringl%len() < 1 ) then
      new%s = EMPTY_STR; return
    end if

    if ( len(charsr) < 1 ) then
      new%s = Stringl%s; return
    end if

    new = Stringl%replace(match=charsr, substring=EMPTY_STR)
  end procedure string_char_excision

  module procedure char_string_excision
    type(String) :: Stringl

    Stringl%s = charsl

    if ( Stringl%len() < 1 ) then
      new%s = EMPTY_STR; return
    end if

    if ( Stringr%len() < 1 ) then
      new%s = Stringl%s; return
    end if

    new = Stringl%replace(match=Stringr%s, substring=EMPTY_STR)
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
    if ( String_base%len() < 1 ) then
      new%s = EMPTY_STR; return
    end if

    new%s = repeat(String_base%s, ncopies=max(ncopies, 0))
  end procedure repeat_String

  module procedure string_equivalence
    integer :: Stringl_len, Stringr_len

    Stringl_len = max(Stringl%len(), 0)
    Stringr_len = max(Stringr%len(), 0)

    if ( Stringl_len /= Stringr_len ) then
      equal = .false.; return
    end if

    if ( Stringl_len < 1 ) then
      equal = .true.; return
    end if

    equal = ( Stringl%s == Stringr%s )
  end procedure string_equivalence

  module procedure string_char_equivalence
    integer :: Stringl_len, charsr_len

    Stringl_len = max(Stringl%len(), 0)
    charsr_len  = len(charsr)

    if ( Stringl_len /= charsr_len ) then
      equal = .false.; return
    end if

    if ( Stringl_len < 1 ) then
      equal = .true.; return
    end if

    equal = ( Stringl%s == charsr )
  end procedure string_char_equivalence

  module procedure char_string_equivalence
    integer :: charsl_len, Stringr_len

    charsl_len  = len(charsl)
    Stringr_len = max(Stringr%len(), 0)

    if ( charsl_len /= Stringr_len ) then
      equal = .false.; return
    end if

    if ( charsl_len < 1 ) then
      equal = .true.; return
    end if

    equal = ( charsl == Stringr%s )
  end procedure char_string_equivalence

  module procedure string_nonequivalence
    integer :: Stringl_len, Stringr_len

    Stringl_len = max(Stringl%len(), 0)
    Stringr_len = max(Stringr%len(), 0)

    if ( Stringl_len /= Stringr_len ) then
      unequal = .true.; return
    end if

    if ( Stringl_len < 1 ) then
      unequal = .false.; return
    end if

    unequal = ( Stringl%s /= Stringr%s )
  end procedure string_nonequivalence

  module procedure string_char_nonequivalence
    integer :: Stringl_len, charsr_len

    Stringl_len = max(Stringl%len(), 0)
    charsr_len  = len(charsr)

    if ( Stringl_len /= charsr_len ) then
      unequal = .true.; return
    end if

    if ( Stringl_len < 1 ) then
      unequal = .false.; return
    end if

    unequal = ( Stringl%s /= charsr )
  end procedure string_char_nonequivalence

  module procedure char_string_nonequivalence
    integer :: charsl_len, Stringr_len

    charsl_len  = len(charsl)
    Stringr_len = max(Stringr%len(), 0)

    if ( charsl_len /= Stringr_len ) then
      unequal = .true.; return
    end if

    if ( charsl_len < 1 ) then
      unequal = .false.; return
    end if

    unequal = ( charsl /= Stringr%s )
  end procedure char_string_nonequivalence
end submodule operators
