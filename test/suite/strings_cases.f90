module strings_cases
  !---------------------------------------------------------------------------------------------------------------------
  !! Case conversion, prefix and suffix tests, `find` and `trim` with `chars`: the `io_strings` functions on
  !! `character` and `String` values, and the matching `String` methods (#72).
  !---------------------------------------------------------------------------------------------------------------------
  use io_fortran_lib, only: String, operator(==)
  use io_strings, only: to_upper, to_lower, starts_with, ends_with, find, trim
  use test_support, only: test_suite, check, expect_equal
  implicit none (type, external)
  private

  public :: register_strings

contains

  subroutine register_strings(suite)
    type(test_suite), intent(inout) :: suite

    call suite%group("strings")
    call suite%add("case_chars", case_chars)
    call suite%add("case_string", case_string)
    call suite%add("case_elemental", case_elemental)
    call suite%add("starts_ends_chars", starts_ends_chars)
    call suite%add("starts_ends_string", starts_ends_string)
    call suite%add("find_basic", find_basic)
    call suite%add("find_overlapping", find_overlapping)
    call suite%add("find_string", find_string)
    call suite%add("trim_chars", trim_chars)
    call suite%add("trim_string_chars", trim_string_chars)
    call suite%add("stdlib_keywords", stdlib_keywords)
  end subroutine register_strings

  subroutine case_chars()
    character(len=*), parameter :: mixed = "Hello, World! 123 _x"

    call check(to_upper(mixed) == "HELLO, WORLD! 123 _X", "to_upper converts letters only")
    call check(to_lower(mixed) == "hello, world! 123 _x", "to_lower converts letters only")
    call expect_equal(len(to_upper(mixed)), len(mixed), "to_upper keeps the length")
    call check(to_upper("a"//achar(200)//"z") == "A"//achar(200)//"Z", "Bytes outside ASCII letters are unchanged.")
    call check(to_upper("") == "", "to_upper of the empty string")
  end subroutine case_chars

  subroutine case_string()
    type(String) :: s, unset

    s = String("Mixed Case")
    call check(s%to_upper() == "MIXED CASE", "the to_upper method")
    call check(s%to_lower() == "mixed case", "the to_lower method")
    call check(to_upper(s) == "MIXED CASE", "to_upper of a String")
    call check(s == "Mixed Case", "to_upper returns a copy and leaves the String unchanged")
    call check(unset%to_upper() == "", "An unallocated String converts to the empty string.")
  end subroutine case_string

  subroutine case_elemental()
    character(len=2) :: words(3) = ["ab", "Cd", "e1"]
    type(String)     :: strings(2), converted(2)
    character(len=2) :: upper(3)

    upper = to_upper(words)
    call check(all(upper == ["AB", "CD", "E1"]), "to_upper is elemental for character")
    strings = [String("x"), String("Yz")]
    converted = to_lower(strings)
    call check(converted(1) == "x" .and. converted(2) == "yz", "to_lower is elemental for String")
    converted = strings%to_upper()
    call check(converted(1) == "X" .and. converted(2) == "YZ", "the to_upper method is elemental")
  end subroutine case_elemental

  subroutine starts_ends_chars()
    call check(starts_with("abc", "ab"), "abc starts with ab")
    call check(.not. starts_with("abc", "bc"), "abc does not start with bc")
    call check(starts_with("abc", ""), "Every string starts with the empty string.")
    call check(.not. starts_with("ab", "abc"), "A prefix longer than the string does not match.")
    call check(.not. starts_with("ab", "ab "), "A trailing blank in the prefix counts.")
    call check(ends_with("abc", "bc"), "abc ends with bc")
    call check(.not. ends_with("abc", "ab"), "abc does not end with ab")
    call check(ends_with("abc", ""), "Every string ends with the empty string.")
    call check(.not. ends_with("ab", "b "), "A trailing blank in the suffix counts.")
    call check(all(starts_with(["ab", "ba"], "a") .eqv. [.true., .false.]), "starts_with is elemental")
  end subroutine starts_ends_chars

  subroutine starts_ends_string()
    type(String) :: s, unset

    s = String("prefix.suffix")
    call check(s%starts_with("prefix"), "the starts_with method with character")
    call check(s%starts_with(String("pre")), "the starts_with method with String")
    call check(s%ends_with(".suffix"), "the ends_with method with character")
    call check(.not. s%ends_with(String("prefix")), "the ends_with method with String")
    call check(starts_with(s, "pre") .and. starts_with("prefix", String("pre")), "starts_with with mixed arguments")
    call check(ends_with(s, String("fix")) .and. ends_with("suffix", String("fix")), "ends_with with mixed arguments")
    call check(unset%starts_with("") .and. .not. unset%ends_with("x"), "An unallocated String is the empty string.")
  end subroutine starts_ends_string

  subroutine find_basic()
    call expect_equal(find("hello world", "o"), 5, "the first o")
    call expect_equal(find("hello world", "o", occurrence=2), 8, "the second o")
    call expect_equal(find("hello world", "o", occurrence=3), 0, "There is no third o.")
    call expect_equal(find("hello world", "z"), 0, "an absent pattern")
    call expect_equal(find("hello world", ""), 0, "An empty pattern gives 0.")
    call expect_equal(find("hello world", "o", occurrence=0), 0, "occurrence 0 gives 0")
    call expect_equal(find("ab", "abc"), 0, "a pattern longer than the string")
    call expect_equal(find("abc", "abc"), 1, "the whole string")
  end subroutine find_basic

  subroutine find_overlapping()
    call expect_equal(find("aaaa", "aa", occurrence=2), 2, "Occurrences overlap by default.")
    call expect_equal(find("aaaa", "aa", occurrence=3), 3, "the third overlapping occurrence")
    call expect_equal(find("aaaa", "aa", occurrence=2, consider_overlapping=.false.), 3, "without overlap")
    call expect_equal(find("aaaa", "aa", occurrence=3, consider_overlapping=.false.), 0, "no third without overlap")
  end subroutine find_overlapping

  subroutine find_string()
    type(String) :: s, unset

    s = String("one two one")
    call expect_equal(s%find("one"), 1, "the find method with character")
    call expect_equal(s%find(String("one"), occurrence=2), 9, "the find method with String and occurrence")
    call expect_equal(find(s, "two"), 5, "find of a String")
    call expect_equal(find("one two", String("two")), 5, "find with a String pattern")
    call expect_equal(unset%find("x"), 0, "find in an unallocated String")
    call check(all(find(["ab", "ba"], "a") == [1, 2]), "find is elemental")
  end subroutine find_string

  subroutine trim_chars()
    character(len=8) :: padded = "xxabcxx "

    call check(trim("xxabcxx", "x") == "abc", "trim removes the characters in chars from both ends")
    call check(trim("-*a*b-*", "*-") == "a*b", "chars is a set, and the middle is kept")
    call check(trim("xxx", "x") == "", "Everything removed leaves the empty string.")
    call check(trim("abc", "") == "abc", "Empty chars removes nothing.")
    call check(trim(padded) == "xxabcxx", "The intrinsic trim still works with io_strings in use.")
    call check(trim(padded, "x ") == "abc", "trim of a padded character variable")
  end subroutine trim_chars

  subroutine trim_string_chars()
    type(String) :: s

    s = String("--name--")
    call check(s%trim("-") == "name", "the trim method with chars")
    call check(trim(String("  name ")) == "name", "trim of a String without chars removes whitespace")
    call check(trim(s, "-") == "name", "trim of a String with chars")
    call s%trim_inplace("-")
    call check(s == "name", "trim_inplace with chars")
  end subroutine trim_string_chars

  subroutine stdlib_keywords()
    !! The argument names follow stdlib, so keyword calls written for stdlib work.
    call expect_equal(find(string="abcabc", pattern="c", occurrence=2, consider_overlapping=.true.), 6, "find")
    call check(starts_with(string="abc", substring="a"), "starts_with")
    call check(ends_with(string="abc", substring="c"), "ends_with")
    call check(to_upper(string="a") == "A" .and. to_lower(string="A") == "a", "to_upper and to_lower")
  end subroutine stdlib_keywords
end module strings_cases
