module string_cases
  !---------------------------------------------------------------------------------------------------------------------
  !! The `String` type: comparison, counting, `join`, `trim`, `echo`, repetition, and `push`.
  !---------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: int64
  use io_fortran_lib, only: String, str, join, split, echo, IOFL_LF, IOFL_CR, IOFL_TAB, operator(==), operator(/=), &
                            operator(**), operator(+)
  use test_support, only: test_suite, check, expect_equal, scratch, write_raw
  implicit none (type, external)
  private

  public :: register_string

contains

  subroutine register_string(suite)
    type(test_suite), intent(inout) :: suite

    call suite%group("string")
    call suite%add("string_array_constructors", string_array_constructors)
    call suite%add("unallocated_equals_empty", unallocated_equals_empty)
    call suite%add("count_empty_consistent", count_empty_consistent)
    call suite%add("join_all_empty", join_all_empty)
    call suite%add("join_many_empty", join_many_empty)
    call suite%add("join_many_tokens", join_many_tokens)
    call suite%add("join_into_token", join_into_token)
    call suite%add("join_no_tokens", join_no_tokens)
    call suite%add("join_unallocated_tokens", join_unallocated_tokens)
    call suite%add("trim_whitespace", trim_whitespace)
    call suite%add("echo_blank_line", echo_blank_line)
    call suite%add("echo_unallocated", echo_unallocated)
    call suite%add("repeat_negative", repeat_negative)
    call suite%add("push_then_everything", push_then_everything)
    call suite%add("push_cast_number", push_cast_number)
    call suite%add("push_edge_cases", push_edge_cases)
    call suite%add("count64_agrees_with_count", count64_agrees_with_count)
    call suite%add("repeat_scalar", repeat_scalar)
    call suite%add("repeat_array", repeat_array)
    call suite%add("concat_plus_array", concat_plus_array)
  end subroutine register_string

  subroutine string_array_constructors()
    type(String) :: three(3), special(2,4)

    special(1,:) = [String("a"), String("b"), String("c"), String("d")]
    special(2,:) = [String("e"), String("f"), String("g"), String("h")]
    call expect_equal(special(1,3)%as_str(), "c", "A constructor assigned to a section.")
    call expect_equal(special(2,4)%as_str(), "h", "A constructor assigned to another section.")
    special(1,:) = [String("i"), String("j"), String("k"), String("l")]
    call expect_equal(special(1,3)%as_str(), "k", "A section assigned again.")
    call expect_equal(special(2,1)%as_str(), "e", "Other sections should keep their values.")
    three = [String("x"), String("y"), String("z")]
    call expect_equal(three(3)%as_str(), "z", "A constructor assigned to a whole array.")
  end subroutine string_array_constructors

  subroutine unallocated_equals_empty()
    type(String) :: s, r

    r = String("")
    call check(s == r, "An unallocated String should equal an empty String.")
    call check(r == s, "In either order.")
    call expect_equal(s, "", "An unallocated String should equal an empty character.")
    call check("" == s, "In either order.")
    call check(.not. (s /= r), "Not equal should agree.")
    call check(.not. (s /= ""), "Not equal should agree with a character.")
    call check(.not. ("" /= s), "In either order.")
    call check(s /= "a", "An unallocated String should not equal text.")
  end subroutine unallocated_equals_empty

  subroutine count_empty_consistent()
    type(String) :: s, r

    r = String("")
    call check(s%count("") == r%count(""), "Unallocated and empty Strings should count alike.")
    call check(s%count(r) == r%count(s), "With a String to match as well.")
    call expect_equal(s%count("a"), 0, "An unallocated String holds no text.")
  end subroutine count_empty_consistent

  subroutine join_all_empty()
    type(String) :: r, three(3)

    three = String("")
    r = join(three, ",")
    call expect_equal(r%as_str(), ",,", "Joining empty tokens should keep the separators.")
    call r%join(three, ";")
    call expect_equal(r%as_str(), ";;", "The bound join should agree.")
    three(2) = String("x")
    r = join(three, ",")
    call expect_equal(r%as_str(), ",x,", "Some empty tokens.")
  end subroutine join_all_empty

  subroutine join_many_empty()
    type(String)              :: r
    type(String), allocatable :: many(:)

    allocate( many(1000) )
    many = String("")
    r = join(many, ",")
    call expect_equal(r%len(), 999, "The separators should survive the split for large arrays.")
  end subroutine join_many_empty

  subroutine join_many_tokens()
    type(String)              :: r
    type(String), allocatable :: many(:)

    allocate( many(200000) )
    many = String("ab")
    r = join(many, ",")
    call check(r%len() == 3*200000 - 1, "All tokens and separators should be present.")
    call check((r%count64("ab") == 200000_int64) .and. (r%count64(",") == 199999_int64), "In order.")
    call expect_equal(r%count64("ab,ab"), 100000_int64, "With one separator between tokens.")
    call r%join(many(:3), ";")
    call expect_equal(r%as_str(), "ab;ab;ab", "The bound join should agree.")
  end subroutine join_many_tokens

  subroutine join_into_token()
    type(String) :: three(3)

    three(1) = String("a"); three(2) = String("b"); three(3) = String("c")
    call three(1)%join(three, ",")
    call expect_equal(three(1)%as_str(), "a,b,c", "Joining into one of the tokens.")
  end subroutine join_into_token

  subroutine join_no_tokens()
    type(String)              :: r
    type(String), allocatable :: many(:)

    allocate( many(0) )
    r = join(many, ",")
    call expect_equal(r%len(), 0, "No tokens should give an empty result.")
  end subroutine join_no_tokens

  subroutine join_unallocated_tokens()
    type(String) :: r, three(3)

    r = join(three, ",")
    call expect_equal(r%as_str(), ",,", "Unallocated tokens are empty tokens.")
  end subroutine join_unallocated_tokens

  subroutine trim_whitespace()
    type(String) :: s, r

    s = String(IOFL_TAB//"hi"//IOFL_LF)
    r = s%trim()
    call expect_equal(r%as_str(), "hi", "Trim should remove all leading and trailing whitespace.")
    s = String(" "//IOFL_CR//IOFL_LF//IOFL_TAB//"a b"//IOFL_TAB//"c"//IOFL_CR//IOFL_LF//" ")
    r = s%trim()
    call check(r%as_str() == "a b"//IOFL_TAB//"c", "Whitespace inside should stay.")
    call s%trim_inplace()
    call check(s == r, "The inplace form should agree.")
    s = String(IOFL_TAB//IOFL_LF//"  ")
    r = s%trim()
    call expect_equal(r%len(), 0, "All whitespace trims to empty.")
  end subroutine trim_whitespace

  subroutine echo_blank_line()
    character(len=:), allocatable :: file
    type(String)                  :: s
    character(len=512)            :: msg
    integer                       :: st

    file = scratch(); st = 0; msg = ""

    call write_raw(file//"_out.log", "")
    call echo("a", file=file//"_out.log", stat=st, errmsg=msg)
    call echo("", file=file//"_out.log", stat=st, errmsg=msg)
    call expect_equal(st, 0, "A blank line should be accepted.")
    call echo("b", file=file//"_out.log", stat=st, errmsg=msg)
    call s%read_file(file//"_out.log")
    call check(s%as_str() == "a"//IOFL_LF//IOFL_LF//"b"//IOFL_LF, "The blank line should be in the file.")
  end subroutine echo_blank_line

  subroutine echo_unallocated()
    character(len=:), allocatable :: file
    type(String)                  :: s, r
    character(len=512)            :: msg
    integer                       :: st

    file = scratch(); st = 0; msg = ""

    call write_raw(file//"_out.log", "")
    call s%echo(file//"_out.log", stat=st, errmsg=msg)
    call expect_equal(st, 0, "An unallocated String is an empty line.")
    call r%read_file(file//"_out.log")
    call check(r%as_str() == IOFL_LF, "It should write only the terminator.")
  end subroutine echo_unallocated

  subroutine repeat_negative()
    type(String) :: r

    call check(len("ab"**(-1)) == 0, "A negative count should give an empty result.")
    r = String("ab")**(-1)
    call expect_equal(r%len(), 0, "The same for a String.")
    call check(len(["ab","cd"]**(-2)) == 0, "The same for an array.")
  end subroutine repeat_negative

  subroutine push_then_everything()
    character(len=:), allocatable :: file
    type(String)                  :: s, r
    character(len=512)            :: msg
    integer                       :: st, i
    type(String), allocatable     :: tokens(:)

    file = scratch(); st = 0; msg = ""

    do i = 1, 1000
      call s%push("ab,")
    end do
    call expect_equal(s%len(), 3000, "The length should be the text, not the capacity.")
    call check(s%as_str() == repeat("ab,", 1000), "The text should be intact.")
    call check(s == repeat("ab,", 1000), "Comparison should ignore spare room.")
    call expect_equal(s%count(","), 1000, "Count should ignore spare room.")
    r = s
    call check((r == s) .and. (r%len() == 3000), "A copy should have the same text.")
    call r%push("x")
    call check((r%len() == 3001) .and. (s%len() == 3000), "A copy should be independent.")
    r = s%replace(",", ";")
    call expect_equal(r%len(), 3000, "Replace should ignore spare room.")
    r = s%trim()
    call expect_equal(r%len(), 3000, "Trim should ignore spare room.")
    tokens = split(s, ",")
    call expect_equal(size(tokens), 1001, "Split should ignore spare room.")
    call check(str(s) == repeat("ab,", 1000), "str should ignore spare room.")
    call write_raw(file//"_out.log", "")
    call s%echo(file//"_out.log", stat=st, errmsg=msg)
    call r%read_file(file//"_out.log")
    call expect_equal(r%len(), 3001, "A file should hold only the text and the terminator.")
  end subroutine push_then_everything

  subroutine push_cast_number()
    type(String) :: s
    integer      :: st, i

    st = 0

    call s%push("12"); call s%push("34")
    call s%cast(into=i, stat=st)
    call check((st == 0) .and. (i == 1234), "Spare room should not affect a cast.")
  end subroutine push_cast_number

  subroutine push_edge_cases()
    type(String) :: s, r

    call s%push("")
    call expect_equal(s%len(), 0, "Pushing nothing onto an unallocated String gives an empty String.")
    call s%push(r)
    call expect_equal(s%len(), 0, "Pushing an unallocated String adds nothing.")
    call s%push("ab")
    call s%push(s)
    call expect_equal(s%as_str(), "abab", "Pushing a String onto itself.")
    call s%empty()
    call expect_equal(s%len(), 0, "Empty should clear the text.")
    call s%push("c")
    call expect_equal(s%as_str(), "c", "Pushing after empty.")
  end subroutine push_edge_cases

  subroutine count64_agrees_with_count()
    type(String) :: s, r, three(3)

    s = String("ab,ab,,ab")
    call check(s%count64(",") == int(s%count(","), int64), "With a character match.")
    r = String("ab")
    call expect_equal(s%count64(r), 3_int64, "With a String match.")
    call expect_equal(s%count64("zz"), 0_int64, "No match.")
    call expect_equal(s%count64(""), 0_int64, "An empty match in text.")
    r = String("")
    call expect_equal(r%count64(""), 1_int64, "The empty string occurs once in itself.")
    three(1) = String("a,a"); three(2) = String("a"); three(3) = String("")
    call check(all(three%count64("a") == [2_int64,1_int64,0_int64]), "Elementally.")
    s = String("aaaa")
    call expect_equal(s%count("aa"), 2, "Occurrences do not overlap.")
  end subroutine count64_agrees_with_count

  subroutine repeat_scalar()
    call check("ab"**2 == "abab", "Scalar repetition.")
    call expect_equal(len("ab"**0), 0, "Zero copies.")
  end subroutine repeat_scalar

  subroutine repeat_array()
    call check_repeat_array()
  end subroutine repeat_array

  subroutine concat_plus_array()
    call check(all(["ab","cd"] + "z" == ["abz","cdz"]), "Elemental concatenation.")
  end subroutine concat_plus_array

  subroutine check_repeat_array()
    character(len=2), parameter :: a(2)   = ["ab","cd"]
    character(len=1), parameter :: b(2,2) = reshape(["a","b","c","d"], [2,2])
    character(len=1)            :: c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

    c = "x"
    call expect_equal(len(a**3), 6, "The result length should scale with the count.")
    call check(all(a**2 == ["abab","cdcd"]), "Rank 1 repetition.")
    call check(all(b**2 == reshape(["aa","bb","cc","dd"], [2,2])), "Rank 2 repetition, keeping the shape.")
    call check(all(c**2 == "xx") .and. (size(shape(c**2)) == 15), "Rank 15 repetition.")
  end subroutine check_repeat_array
end module string_cases
