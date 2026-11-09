module harness_cases
  !---------------------------------------------------------------------------------------------------------------------
  !! Self checks of the harness: these confirm that the exit status logic works on the current compiler.
  !---------------------------------------------------------------------------------------------------------------------
  use test_support, only: test_suite, STOPS, KNOWN_BUG, check
  implicit none
  private

  public :: register_harness

contains

  subroutine register_harness(suite)
    type(test_suite), intent(inout) :: suite

    call suite%group("harness")
    call suite%add("self_returns", self_returns)
    call suite%add("self_stops", self_stops, expect=STOPS)
    call suite%add("self_failed_check", self_failed_check, status=KNOWN_BUG)
  end subroutine register_harness

  subroutine self_returns()
    continue
  end subroutine self_returns

  subroutine self_stops()
    error stop "This case stops on purpose."
  end subroutine self_stops

  subroutine self_failed_check()
    call check(.false., "This check fails on purpose.")
  end subroutine self_failed_check
end module harness_cases
