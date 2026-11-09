module read_line_cases
  !---------------------------------------------------------------------------------------------------------------------
  !! `read_line`.
  !---------------------------------------------------------------------------------------------------------------------
  use io_fortran_lib, only: String, IOFL_LF, operator(==), operator(/=)
  use test_support, only: test_suite, STOPS, check, expect_equal, scratch, write_raw
  implicit none
  private

  public :: register_read_line

contains

  subroutine register_read_line(suite)
    type(test_suite), intent(inout) :: suite

    call suite%group("read_line")
    call suite%add("read_line_lines", read_line_lines)
    call suite%add("read_line_exact_chunk", read_line_exact_chunk)
    call suite%add("read_line_then_use", read_line_then_use)
    call suite%add("read_line_bad_unit_stat", read_line_bad_unit_stat)
    call suite%add("write_then_read_line", write_then_read_line)
    call suite%add("read_line_end_stops", read_line_end_stops, expect=STOPS)
  end subroutine register_read_line

  subroutine read_line_lines()
    character(len=:), allocatable :: file
    type(String)                  :: s
    integer                       :: st, i

    file = scratch(); st = 0

    call write_raw(file//".txt", "  first line "//IOFL_LF//IOFL_LF//"third"//IOFL_LF//repeat("x", 700)//IOFL_LF// &
                                 "no newline")
    open(newunit=i, file=file//".txt", status="old", action="read")
    call s%read_line(i, stat=st)
    call check((st == 0) .and. (s == "  first line "), "Blanks should be kept.")
    call s%read_line(i, stat=st)
    call check((st == 0) .and. (s%len() == 0), "A blank line gives an empty String.")
    call s%read_line(i, stat=st)
    call check((st == 0) .and. (s == "third"), "The next line.")
    call s%read_line(i, stat=st)
    call check((st == 0) .and. (s == repeat("x", 700)), "A long line should be read in full.")
    call s%read_line(i, stat=st)
    call check((st == 0) .and. (s == "no newline"), "The last line needs no newline.")
    call s%read_line(i, stat=st)
    call check(is_iostat_end(st), "The end of the file should be reported through stat.")
    close(i)
  end subroutine read_line_lines

  subroutine read_line_exact_chunk()
    character(len=:), allocatable :: file
    type(String)                  :: s
    integer                       :: i

    file = scratch()

    call write_raw(file//".txt", repeat("y", 256)//IOFL_LF//repeat("z", 512)//IOFL_LF//"end"//IOFL_LF)
    open(newunit=i, file=file//".txt", status="old", action="read")
    call s%read_line(i)
    call check(s == repeat("y", 256), "A line exactly as long as the internal buffer.")
    call s%read_line(i)
    call check(s == repeat("z", 512), "A line exactly twice as long.")
    call s%read_line(i)
    call expect_equal(s, "end", "The next line should be intact.")
    close(i)
  end subroutine read_line_exact_chunk

  subroutine read_line_then_use()
    character(len=:), allocatable :: file
    type(String)                  :: s
    integer                       :: i

    file = scratch()

    call write_raw(file//".txt", "abc"//IOFL_LF)
    open(newunit=i, file=file//".txt", status="old", action="read")
    call s%read_line(i)
    close(i)
    call s%push("d")
    call expect_equal(s, "abcd", "A String that was read should work as usual.")
  end subroutine read_line_then_use

  subroutine read_line_bad_unit_stat()
    type(String)       :: s
    character(len=512) :: msg
    integer            :: st

    st = 0; msg = ""

    call s%read_line(98765, stat=st, errmsg=msg)
    call check(st /= 0, "An unconnected unit should be reported through stat.")

    ! gfortran connects an unconnected unit to a new file `fort.98765` instead of failing. Remove it.
    close(98765, status="delete", iostat=st)
  end subroutine read_line_bad_unit_stat

  subroutine write_then_read_line()
    character(len=:), allocatable :: file
    type(String)                  :: s, r
    character(len=512)            :: msg
    integer                       :: st, i

    file = scratch(); st = 0; msg = ""

    s = String("hello, world")
    open(newunit=i, file=file//".txt", status="replace", action="write")
    write(i, "(DT)") s
    close(i)
    open(newunit=i, file=file//".txt", status="old", action="read")
    call r%read_line(i, stat=st, errmsg=msg)
    close(i)
    call check((st == 0) .and. (r == s), "Text should survive a round trip.")
  end subroutine write_then_read_line

  subroutine read_line_end_stops()
    character(len=:), allocatable :: file
    type(String)                  :: s
    integer                       :: i

    file = scratch()

    call write_raw(file//".txt", "")
    open(newunit=i, file=file//".txt", status="old", action="read")
    call s%read_line(i)
  end subroutine read_line_end_stops
end module read_line_cases
