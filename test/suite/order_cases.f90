module order_cases
  !---------------------------------------------------------------------------------------------------------------------
  !! Ordering operators for `String`.
  !---------------------------------------------------------------------------------------------------------------------
  use io_fortran_lib, only: String, operator(==), operator(/=), operator(<), operator(<=), operator(>), operator(>=)
  use test_support, only: test_suite, check
  implicit none (type, external)
  private

  public :: register_order

contains

  subroutine register_order(suite)
    type(test_suite), intent(inout) :: suite

    call suite%group("order")
    call suite%add("order_strings", order_strings)
    call suite%add("order_mixed", order_mixed)
    call suite%add("order_agrees_with_equality", order_agrees_with_equality)
    call suite%add("order_unallocated", order_unallocated)
    call suite%add("order_array", order_array)
    call suite%add("order_after_push", order_after_push)
    call suite%add("order_high_bytes", order_high_bytes)
  end subroutine register_order

  subroutine order_strings()
    type(String) :: s, r

    s = String("apple"); r = String("banana")
    call check(s < r, "Less than.")
    call check(s <= r, "Less than or equal.")
    call check(r > s, "Greater than.")
    call check(r >= s, "Greater than or equal.")
    call check(.not. (r < s), "Not less than.")
    call check(.not. (s > r), "Not greater than.")
    call check((s <= s) .and. (s >= s) .and. (.not. (s < s)) .and. (.not. (s > s)), "Equal strings.")
  end subroutine order_strings

  subroutine order_mixed()
    type(String) :: s

    s = String("b")
    call check(("a" < s) .and. (s < "c"), "Less than, both ways.")
    call check(("c" > s) .and. (s > "a"), "Greater than, both ways.")
    call check((s <= "b") .and. ("b" <= s) .and. (s >= "b") .and. ("b" >= s), "Equal to a character.")
  end subroutine order_mixed

  subroutine order_agrees_with_equality()
    type(String) :: s, r

    s = String("a"); r = String("a ")
    call check(s /= r, "Trailing blanks are significant.")
    call check((s < r) .and. (r > s), "So exactly one of the orderings holds.")
    call check("a" < r, "The same with a character.")
    s = String("ab"); r = String("abc")
    call check(s < r, "A prefix comes first.")
    call check(.not. (r <= s), "And not the other way.")
  end subroutine order_agrees_with_equality

  subroutine order_unallocated()
    type(String) :: s, r

    r = String("")
    call check((s <= r) .and. (s >= r) .and. (.not. (s < r)), "An unallocated String acts as empty.")
    call check(s < "a", "It comes before any text.")
    call check("a" > s, "In either order.")
    call check((s <= "") .and. ("" >= s), "It equals an empty character.")
  end subroutine order_unallocated

  subroutine order_array()
    type(String) :: three(3)

    three(1) = String("a"); three(2) = String("b"); three(3) = String("c")
    call check(all((three < "b") .eqv. [.true.,.false.,.false.]), "Elementally with a scalar.")
    call check(all(("b" < three) .eqv. [.false.,.false.,.true.]), "In either order.")
  end subroutine order_array

  subroutine order_after_push()
    type(String) :: s

    call s%push("ab"); call s%push("c")
    call check((s == "abc") .and. (s >= "abc") .and. (s <= "abc"), "Spare room should not affect ordering.")
  end subroutine order_after_push

  subroutine order_high_bytes()
    type(String) :: s, r

    s = String(achar(200)); r = String("a")
    call check(r < s, "Bytes compare as unsigned.")
  end subroutine order_high_bytes
end module order_cases
