module delimited_cases
  !---------------------------------------------------------------------------------------------------------------------
  !! `read_file` and `write_file`: newlines, CRLF, quoting, uneven rows, and round trips.
  !---------------------------------------------------------------------------------------------------------------------
  use io_fortran_lib, only: String, LF, CR, TAB, operator(==), operator(/=), operator(+), operator(>), operator(>=)
  use test_support, only: test_suite, STOPS, check, expect_equal, scratch, write_raw
  implicit none (type, external)
  private

  public :: register_delimited

contains

  subroutine register_delimited(suite)
    type(test_suite), intent(inout) :: suite

    call suite%group("delimited")
    call suite%add("read_file_missing", read_file_missing, expect=STOPS)
    call suite%add("read_no_final_newline", read_no_final_newline)
    call suite%add("read_one_line_no_newline", read_one_line_no_newline)
    call suite%add("read_crlf", read_crlf)
    call suite%add("read_row_wider_than_first", read_row_wider_than_first)
    call suite%add("read_row_narrower_than_first", read_row_narrower_than_first)
    call suite%add("read_quoted_newline", read_quoted_newline)
    call suite%add("read_empty_file_closes_unit", read_empty_file_closes_unit)
    call suite%add("write_unallocated_cell", write_unallocated_cell)
    call suite%add("csv_quotes_separator", csv_quotes_separator)
    call suite%add("read_row_wider_stops", read_row_wider_stops, expect=STOPS)
    call suite%add("read_quote_not_closed_stops", read_quote_not_closed_stops, expect=STOPS)
    call suite%add("read_quote_not_closed_stat", read_quote_not_closed_stat)
    call suite%add("read_doubled_quotes", read_doubled_quotes)
    call suite%add("read_text_after_closing_quote", read_text_after_closing_quote)
    call suite%add("read_quote_inside_cell", read_quote_inside_cell)
    call suite%add("read_cr_inside_quotes", read_cr_inside_quotes)
    call suite%add("read_trailing_blank_lines", read_trailing_blank_lines)
    call suite%add("read_blank_line_inside", read_blank_line_inside)
    call suite%add("read_only_newlines", read_only_newlines)
    call suite%add("read_multichar_separators", read_multichar_separators)
    call suite%add("read_tab_separator", read_tab_separator)
    call suite%add("read_empty_last_cell", read_empty_last_cell)
    call suite%add("read_narrow_rows_all_allocated", read_narrow_rows_all_allocated)
    call suite%add("write_quotes_only_when_needed", write_quotes_only_when_needed)
    call suite%add("write_doubles_quotes", write_doubles_quotes)
    call suite%add("write_single_column_empty_last", write_single_column_empty_last)
    call suite%add("write_empty_array", write_empty_array)
    call suite%add("round_trip_all_special_cells", round_trip_all_special_cells)
    call suite%add("round_trip_custom_separators", round_trip_custom_separators)
    call suite%add("tutorial_ancestry_round_trip", tutorial_ancestry_round_trip)
    call suite%add("tutorial_hg38_round_trip", tutorial_hg38_round_trip)
    call suite%add("write_file_quotes_text_pairs", write_file_quotes_text_pairs)
  end subroutine register_delimited

  subroutine read_file_missing()
    character(len=:), allocatable :: file
    type(String)                  :: s

    file = scratch()

    call s%read_file(file//"_missing.csv")
  end subroutine read_file_missing

  subroutine read_no_final_newline()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call write_raw(file//".csv", "a,b"//LF//"1,2"//LF//"3,4")
    call s%read_file(file//".csv", cell_array=cells)
    call check(all(shape(cells) == [3,2]), "The last row should be read.")
    call expect_equal(cells(3,2)%as_str(), "4", "The last cell should be complete.")
  end subroutine read_no_final_newline

  subroutine read_one_line_no_newline()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call write_raw(file//".csv", "a,b")
    call s%read_file(file//".csv", cell_array=cells)
    call check(all(shape(cells) == [1,2]), "A single line should be read as one row.")
  end subroutine read_one_line_no_newline

  subroutine read_crlf()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call write_raw(file//".csv", "1,2"//CR//LF//"3,4"//CR//LF)
    call s%read_file(file//".csv", cell_array=cells)
    call check(all(shape(cells) == [2,2]), "CRLF should give the same shape.")
    call expect_equal(cells(1,2)%len(), 1, "The carriage return should not be part of the last cell.")
    call expect_equal(cells(2,2)%as_str(), "4", "The carriage return should not be part of the last row.")
  end subroutine read_crlf

  subroutine read_row_wider_than_first()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)
    character(len=512)            :: msg
    integer                       :: st

    file = scratch(); st = 0; msg = ""

    call write_raw(file//".csv", "a,b"//LF//"1,2,3,4"//LF)
    call s%read_file(file//".csv", cell_array=cells, stat=st, errmsg=msg)
    call check(st /= 0, "A row wider than the first should be reported through stat.")
    call check(index(msg, "Row 2 has 4 columns, but the first row has 2") > 0, "The message should name the row.")
  end subroutine read_row_wider_than_first

  subroutine read_row_narrower_than_first()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call write_raw(file//".csv", "a,b,c"//LF//"1"//LF//"2,3,4"//LF)
    call s%read_file(file//".csv", cell_array=cells)
    call check(all(shape(cells) == [3,3]), "The first row sets the shape.")
    call expect_equal(cells(2,1)%as_str(), "1", "The cell should be read.")
    call expect_equal(cells(2,2)%len(), 0, "Missing cells should be empty, not unallocated.")
    call expect_equal(cells(3,3)%as_str(), "4", "Later rows should line up.")
  end subroutine read_row_narrower_than_first

  subroutine read_quoted_newline()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call write_raw(file//".csv", "a,b"//LF//'"x'//LF//'y",2'//LF)
    call s%read_file(file//".csv", cell_array=cells)
    call check(all(shape(cells) == [2,2]), "A newline inside quotes should not end the row.")
    call check(cells(2,1)%as_str() == "x"//LF//"y", "The newline should be kept and the quotes removed.")
    call expect_equal(cells(2,2)%as_str(), "2", "The next cell should follow.")
  end subroutine read_quoted_newline

  subroutine read_empty_file_closes_unit()
    character(len=:), allocatable :: file
    type(String)                  :: s
    character(len=512)            :: msg
    integer                       :: st
    logical                       :: opened

    file = scratch(); st = 0; msg = ""

    call write_raw(file//".txt", "")
    call s%read_file(file//".txt", stat=st, errmsg=msg)
    inquire(file=file//".txt", opened=opened)
    call check(.not. opened, "The file should be closed on return.")
  end subroutine read_empty_file_closes_unit

  subroutine write_unallocated_cell()
    character(len=:), allocatable :: file
    type(String)                  :: s, cell(2,2)

    file = scratch()

    cell(1,1) = String("a"); cell(1,2) = String("b"); cell(2,1) = String("c")
    call s%write_file(cell, file=file//".csv")
    call check(s%as_str() == "a,b"//LF//"c,"//LF, "An unallocated cell should be written as an empty cell.")
  end subroutine write_unallocated_cell

  subroutine csv_quotes_separator()
    character(len=:), allocatable :: file
    type(String)                  :: s, one_row(1,2)
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    one_row(1,1) = String("a,b"); one_row(1,2) = String("x")
    call s%write_file(one_row, file=file//".csv")
    call s%read_file(file//".csv", cell_array=cells)
    call expect_equal(size(cells, dim=2), 2, "A separator inside a cell should survive a round trip.")
    call expect_equal(cells(1,1)%as_str(), "a,b", "The cell should come back unchanged.")
  end subroutine csv_quotes_separator

  subroutine read_row_wider_stops()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call write_raw(file//".csv", "a,b"//LF//"1,2,3"//LF)
    call s%read_file(file//".csv", cell_array=cells)
  end subroutine read_row_wider_stops

  subroutine read_quote_not_closed_stops()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call write_raw(file//".csv", 'a,"b'//LF)
    call s%read_file(file//".csv", cell_array=cells)
  end subroutine read_quote_not_closed_stops

  subroutine read_quote_not_closed_stat()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)
    character(len=512)            :: msg
    integer                       :: st

    file = scratch(); st = 0; msg = ""

    call write_raw(file//".csv", 'a,b'//LF//'1,"2'//LF)
    call s%read_file(file//".csv", cell_array=cells, stat=st, errmsg=msg)
    call check(st /= 0, "An open quote should be reported through stat.")
    call check(index(msg, "row 2, column 2") > 0, "The message should name the location.")
  end subroutine read_quote_not_closed_stat

  subroutine read_doubled_quotes()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call write_raw(file//".csv", '"say ""hi""",""""'//LF)
    call s%read_file(file//".csv", cell_array=cells)
    call expect_equal(cells(1,1)%as_str(), 'say "hi"', "Doubled quotes stand for one.")
    call expect_equal(cells(1,2)%as_str(), '"', "A cell can be just a quote.")
  end subroutine read_doubled_quotes

  subroutine read_text_after_closing_quote()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call write_raw(file//".csv", '"ab"c,d'//CR//LF)
    call s%read_file(file//".csv", cell_array=cells)
    call expect_equal(cells(1,1)%as_str(), "abc", "Text after a closing quote is kept.")
    call expect_equal(cells(1,2)%as_str(), "d", "The separator still ends the cell.")
  end subroutine read_text_after_closing_quote

  subroutine read_quote_inside_cell()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call write_raw(file//".csv", 'a"b,5"'//LF//' "c",d'//LF)
    call s%read_file(file//".csv", cell_array=cells)
    call expect_equal(cells(1,1)%as_str(), 'a"b', "A quote inside a cell is an ordinary character.")
    call expect_equal(cells(1,2)%as_str(), '5"', "A quote at the end of a cell is an ordinary character.")
    call expect_equal(cells(2,1)%as_str(), ' "c"', "Only a quote at the very start opens a quoted cell.")
  end subroutine read_quote_inside_cell

  subroutine read_cr_inside_quotes()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call write_raw(file//".csv", '"a'//CR//'",b'//CR//LF)
    call s%read_file(file//".csv", cell_array=cells)
    call check(cells(1,1)%as_str() == "a"//CR, "A carriage return inside quotes is data.")
    call expect_equal(cells(1,2)%as_str(), "b", "A carriage return before the newline is not.")
  end subroutine read_cr_inside_quotes

  subroutine read_trailing_blank_lines()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call write_raw(file//".csv", "a,b"//LF//"1,2"//LF//LF//CR//LF)
    call s%read_file(file//".csv", cell_array=cells)
    call check(all(shape(cells) == [2,2]), "Trailing blank lines should not add rows.")
  end subroutine read_trailing_blank_lines

  subroutine read_blank_line_inside()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call write_raw(file//".csv", "a,b"//LF//LF//"1,2"//LF)
    call s%read_file(file//".csv", cell_array=cells)
    call check(all(shape(cells) == [3,2]), "A blank line inside the data keeps its row.")
    call check((cells(2,1)%len() == 0) .and. (cells(2,2)%len() == 0), "Its cells should be empty.")
  end subroutine read_blank_line_inside

  subroutine read_only_newlines()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)
    character(len=512)            :: msg
    integer                       :: st

    file = scratch(); st = 0; msg = ""

    call write_raw(file//".csv", LF//LF)
    call s%read_file(file//".csv", cell_array=cells, stat=st, errmsg=msg)
    call check(st /= 0, "A file with only newlines has no data.")
  end subroutine read_only_newlines

  subroutine read_multichar_separators()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call write_raw(file//".csv", "a||b##c|d||e##")
    call s%read_file(file//".csv", cell_array=cells, row_separator="##", column_separator="||")
    call check(all(shape(cells) == [2,2]), "Separators can be longer than one character.")
    call expect_equal(cells(2,1)%as_str(), "c|d", "A partial separator is data.")
    call expect_equal(cells(2,2)%as_str(), "e", "The last cell should be complete.")
  end subroutine read_multichar_separators

  subroutine read_tab_separator()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call write_raw(file//".csv", "a"//TAB//"b,c"//LF)
    call s%read_file(file//".csv", cell_array=cells, column_separator=TAB)
    call expect_equal(cells(1,2)%as_str(), "b,c", "A comma is data with another separator.")
  end subroutine read_tab_separator

  subroutine read_empty_last_cell()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call write_raw(file//".csv", "a,b,"//LF//"1,2,")
    call s%read_file(file//".csv", cell_array=cells)
    call check(all(shape(cells) == [2,3]), "A trailing separator ends an empty cell.")
    call expect_equal(cells(2,3)%len(), 0, "The cell should be empty.")
  end subroutine read_empty_last_cell

  subroutine read_narrow_rows_all_allocated()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call write_raw(file//".csv", "a,b,c"//LF//"1"//LF//LF//"2,3"//LF)
    call s%read_file(file//".csv", cell_array=cells)
    call check(all(cells%len() >= 0), "Every cell should be allocated.")
  end subroutine read_narrow_rows_all_allocated

  subroutine write_quotes_only_when_needed()
    character(len=:), allocatable :: file
    type(String)                  :: s, cell(2,2)

    file = scratch()

    cell(1,1) = String("1.5"); cell(1,2) = String("a b"); cell(2,1) = String("a;b"); cell(2,2) = String("")
    call s%write_file(cell, file=file//".csv")
    call check(s%as_str() == "1.5,a b"//LF//"a;b,"//LF, "Plain cells should be written as they are.")
  end subroutine write_quotes_only_when_needed

  subroutine write_doubles_quotes()
    character(len=:), allocatable :: file
    type(String)                  :: s, one_row(1,2)

    file = scratch()

    one_row(1,1) = String('say "hi"'); one_row(1,2) = String("a"//LF//"b")
    call s%write_file(one_row, file=file//".csv")
    call check(s%as_str() == '"say ""hi""","a'//LF//'b"'//LF, "Quotes should be doubled.")
  end subroutine write_doubles_quotes

  subroutine write_single_column_empty_last()
    character(len=:), allocatable :: file
    type(String)                  :: s, column(2,1)
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    column(1,1) = String("a"); column(2,1) = String("")
    call s%write_file(column, file=file//".csv")
    call s%read_file(file//".csv", cell_array=cells)
    call check(all(shape(cells) == [2,1]), "The empty last row should survive a round trip.")
    call expect_equal(cells(2,1)%len(), 0, "It should still be empty.")
  end subroutine write_single_column_empty_last

  subroutine write_empty_array()
    character(len=:), allocatable :: file
    type(String)                  :: s, cell(2,2)
    character(len=512)            :: msg
    integer                       :: st

    file = scratch(); st = 0; msg = ""

    call s%write_file(cell(1:0,:), file=file//".csv", stat=st, errmsg=msg)
    call expect_equal(st, 0, "An empty array should write an empty file.")
    call expect_equal(s%len(), 0, "The text should be empty.")
  end subroutine write_empty_array

  subroutine round_trip_all_special_cells()
    character(len=:), allocatable :: file
    type(String)                  :: s, special(2,4)
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    special(1,:) = [String("a,b"), String('q"q'), String('"'), String("")]
    special(2,:) = [String("l"//LF//"f"), String("c"//CR), String(CR//LF), String(" x ")]
    call s%write_file(special, file=file//".csv")
    call s%read_file(file//".csv", cell_array=cells)
    call check(all(shape(cells) == shape(special)), "The shape should survive a round trip.")
    call check(all(cells == special), "Every cell should survive a round trip.")
  end subroutine round_trip_all_special_cells

  subroutine round_trip_custom_separators()
    character(len=:), allocatable :: file
    type(String)                  :: s, special(2,4)
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    special(1,:) = [String("a|b"), String("c;d"), String("e"//LF//"f"), String("g")]
    special(2,:) = [String("##"), String("|"), String(""), String('"')]
    call s%write_file(special, file=file//".csv", row_separator="##", column_separator="|")
    call s%read_file(file//".csv", cell_array=cells, row_separator="##", column_separator="|")
    call check(all(shape(cells) == shape(special)), "The shape should survive a round trip.")
    call check(all(cells == special), "Every cell should survive a round trip.")
  end subroutine round_trip_custom_separators

  subroutine tutorial_ancestry_round_trip()
    character(len=:), allocatable :: file
    type(String)                  :: s, r
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call s%read_file("./data/ancestry_comp.csv", cell_array=cells, row_separator=CR+LF)
    call expect_equal(cells(2,1)%as_str(), "Arab, Egyptian & Levantine", "The quoted cell should be read.")
    call expect_equal(cells(2,5)%as_str(), "168717618", "The next column should line up.")
    call r%write_file(cells, file=file//".csv", row_separator=CR+LF)
    call check(r == s, "The file should be reproduced exactly.")
  end subroutine tutorial_ancestry_round_trip

  subroutine tutorial_hg38_round_trip()
    character(len=:), allocatable :: file
    type(String)                  :: s, r
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call s%read_file("./data/hg38.bed", cell_array=cells, column_separator=TAB)
    call expect_equal(size(cells, dim=1), 192262, "The tutorial says the file has 192262 lines.")
    call r%write_file(cells, file=file//".bed", column_separator=TAB)
    call check(r == s, "The file should be reproduced exactly.")
  end subroutine tutorial_hg38_round_trip

  subroutine write_file_quotes_text_pairs()
    character(len=:), allocatable :: file
    type(String)                  :: s, one_row(1,2)

    file = scratch()

    one_row(1,1) = String("(a,b)"); one_row(1,2) = String("c")
    call s%write_file(one_row, file=file//".csv")
    call check(s%as_str() == '"(a,b)",c'//LF, "write_file should quote text that looks like a pair.")
  end subroutine write_file_quotes_text_pairs
end module delimited_cases
