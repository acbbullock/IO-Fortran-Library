module delimited_cases
  !---------------------------------------------------------------------------------------------------------------------
  !! `read_file` and `write_file`: newlines, CRLF, quoting, uneven rows, and round trips.
  !---------------------------------------------------------------------------------------------------------------------
  use io_fortran_lib, only: String, str, IOFL_LF, IOFL_CR, IOFL_TAB, operator(==), operator(/=), operator(+), &
                            operator(>), operator(>=)
  use test_support, only: test_suite, STOPS, check, expect_equal, scratch, write_raw, read_raw
  use test_values, only: reset_random, random_values
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
    call suite%add("csv_quoted_crlf_round_trip", csv_quoted_crlf_round_trip)
    call suite%add("tsv_large_round_trip", tsv_large_round_trip)
    call suite%add("read_many_rows_mixed_cells", read_many_rows_mixed_cells)
    call suite%add("read_tab_in_quotes", read_tab_in_quotes)
    call suite%add("write_file_quotes_text_pairs", write_file_quotes_text_pairs)
    call suite%add("read_file_tsv_default", read_file_tsv_default)
    call suite%add("write_file_tsv_default", write_file_tsv_default)
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

    call write_raw(file//".csv", "a,b"//IOFL_LF//"1,2"//IOFL_LF//"3,4")
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

    call write_raw(file//".csv", "1,2"//IOFL_CR//IOFL_LF//"3,4"//IOFL_CR//IOFL_LF)
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

    call write_raw(file//".csv", "a,b"//IOFL_LF//"1,2,3,4"//IOFL_LF)
    call s%read_file(file//".csv", cell_array=cells, stat=st, errmsg=msg)
    call check(st /= 0, "A row wider than the first should be reported through stat.")
    call check(index(msg, "Row 2 has 4 columns, but the first row has 2") > 0, "The message should name the row.")
  end subroutine read_row_wider_than_first

  subroutine read_row_narrower_than_first()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call write_raw(file//".csv", "a,b,c"//IOFL_LF//"1"//IOFL_LF//"2,3,4"//IOFL_LF)
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

    call write_raw(file//".csv", "a,b"//IOFL_LF//'"x'//IOFL_LF//'y",2'//IOFL_LF)
    call s%read_file(file//".csv", cell_array=cells)
    call check(all(shape(cells) == [2,2]), "A newline inside quotes should not end the row.")
    call check(cells(2,1)%as_str() == "x"//IOFL_LF//"y", "The newline should be kept and the quotes removed.")
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
    call check(s%as_str() == "a,b"//IOFL_LF//"c,"//IOFL_LF, "An unallocated cell should be written as an empty cell.")
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

    call write_raw(file//".csv", "a,b"//IOFL_LF//"1,2,3"//IOFL_LF)
    call s%read_file(file//".csv", cell_array=cells)
  end subroutine read_row_wider_stops

  subroutine read_quote_not_closed_stops()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call write_raw(file//".csv", 'a,"b'//IOFL_LF)
    call s%read_file(file//".csv", cell_array=cells)
  end subroutine read_quote_not_closed_stops

  subroutine read_quote_not_closed_stat()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)
    character(len=512)            :: msg
    integer                       :: st

    file = scratch(); st = 0; msg = ""

    call write_raw(file//".csv", 'a,b'//IOFL_LF//'1,"2'//IOFL_LF)
    call s%read_file(file//".csv", cell_array=cells, stat=st, errmsg=msg)
    call check(st /= 0, "An open quote should be reported through stat.")
    call check(index(msg, "row 2, column 2") > 0, "The message should name the location.")
  end subroutine read_quote_not_closed_stat

  subroutine read_doubled_quotes()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call write_raw(file//".csv", '"say ""hi""",""""'//IOFL_LF)
    call s%read_file(file//".csv", cell_array=cells)
    call expect_equal(cells(1,1)%as_str(), 'say "hi"', "Doubled quotes stand for one.")
    call expect_equal(cells(1,2)%as_str(), '"', "A cell can be just a quote.")
  end subroutine read_doubled_quotes

  subroutine read_text_after_closing_quote()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call write_raw(file//".csv", '"ab"c,d'//IOFL_CR//IOFL_LF)
    call s%read_file(file//".csv", cell_array=cells)
    call expect_equal(cells(1,1)%as_str(), "abc", "Text after a closing quote is kept.")
    call expect_equal(cells(1,2)%as_str(), "d", "The separator still ends the cell.")
  end subroutine read_text_after_closing_quote

  subroutine read_quote_inside_cell()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call write_raw(file//".csv", 'a"b,5"'//IOFL_LF//' "c",d'//IOFL_LF)
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

    call write_raw(file//".csv", '"a'//IOFL_CR//'",b'//IOFL_CR//IOFL_LF)
    call s%read_file(file//".csv", cell_array=cells)
    call check(cells(1,1)%as_str() == "a"//IOFL_CR, "A carriage return inside quotes is data.")
    call expect_equal(cells(1,2)%as_str(), "b", "A carriage return before the newline is not.")
  end subroutine read_cr_inside_quotes

  subroutine read_trailing_blank_lines()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call write_raw(file//".csv", "a,b"//IOFL_LF//"1,2"//IOFL_LF//IOFL_LF//IOFL_CR//IOFL_LF)
    call s%read_file(file//".csv", cell_array=cells)
    call check(all(shape(cells) == [2,2]), "Trailing blank lines should not add rows.")
  end subroutine read_trailing_blank_lines

  subroutine read_blank_line_inside()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call write_raw(file//".csv", "a,b"//IOFL_LF//IOFL_LF//"1,2"//IOFL_LF)
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

    call write_raw(file//".csv", IOFL_LF//IOFL_LF)
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

    call write_raw(file//".csv", "a"//IOFL_TAB//"b,c"//IOFL_LF)
    call s%read_file(file//".csv", cell_array=cells, column_separator=IOFL_TAB)
    call expect_equal(cells(1,2)%as_str(), "b,c", "A comma is data with another separator.")
  end subroutine read_tab_separator

  subroutine read_empty_last_cell()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call write_raw(file//".csv", "a,b,"//IOFL_LF//"1,2,")
    call s%read_file(file//".csv", cell_array=cells)
    call check(all(shape(cells) == [2,3]), "A trailing separator ends an empty cell.")
    call expect_equal(cells(2,3)%len(), 0, "The cell should be empty.")
  end subroutine read_empty_last_cell

  subroutine read_narrow_rows_all_allocated()
    character(len=:), allocatable :: file
    type(String)                  :: s
    type(String), allocatable     :: cells(:,:)

    file = scratch()

    call write_raw(file//".csv", "a,b,c"//IOFL_LF//"1"//IOFL_LF//IOFL_LF//"2,3"//IOFL_LF)
    call s%read_file(file//".csv", cell_array=cells)
    call check(all(cells%len() >= 0), "Every cell should be allocated.")
  end subroutine read_narrow_rows_all_allocated

  subroutine write_quotes_only_when_needed()
    character(len=:), allocatable :: file
    type(String)                  :: s, cell(2,2)

    file = scratch()

    cell(1,1) = String("1.5"); cell(1,2) = String("a b"); cell(2,1) = String("a;b"); cell(2,2) = String("")
    call s%write_file(cell, file=file//".csv")
    call check(s%as_str() == "1.5,a b"//IOFL_LF//"a;b,"//IOFL_LF, "Plain cells should be written as they are.")
  end subroutine write_quotes_only_when_needed

  subroutine write_doubles_quotes()
    character(len=:), allocatable :: file
    type(String)                  :: s, one_row(1,2)

    file = scratch()

    one_row(1,1) = String('say "hi"'); one_row(1,2) = String("a"//IOFL_LF//"b")
    call s%write_file(one_row, file=file//".csv")
    call check(s%as_str() == '"say ""hi""","a'//IOFL_LF//'b"'//IOFL_LF, "Quotes should be doubled.")
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
    special(2,:) = [String("l"//IOFL_LF//"f"), String("c"//IOFL_CR), String(IOFL_CR//IOFL_LF), String(" x ")]
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

    special(1,:) = [String("a|b"), String("c;d"), String("e"//IOFL_LF//"f"), String("g")]
    special(2,:) = [String("##"), String("|"), String(""), String('"')]
    call s%write_file(special, file=file//".csv", row_separator="##", column_separator="|")
    call s%read_file(file//".csv", cell_array=cells, row_separator="##", column_separator="|")
    call check(all(shape(cells) == shape(special)), "The shape should survive a round trip.")
    call check(all(cells == special), "Every cell should survive a round trip.")
  end subroutine round_trip_custom_separators

  subroutine csv_quoted_crlf_round_trip()
    !! A CSV file of mixed data with CRLF line endings, quoted cells holding commas and quotes, and a header, like the
    !! example in the CSV tutorial.
    character(len=*), parameter :: labels(4) = [ character(len=26) :: '"North, Coastal & Island"', '"South, Inland"', &
                                                 'East', '"West ""Upper"" Valley"' ]
    integer,          parameter :: rows = 190

    type(String)              :: text, s, r
    type(String), allocatable :: cells(:,:)
    integer                   :: numbers(3*rows), i

    call reset_random()
    call random_values(numbers)
    numbers = abs(mod(numbers, 200000000)) + 1

    call text%push("Group,Copy,Chromosome,Start Point,End Point"//IOFL_CR//IOFL_LF)
    do i = 1, rows
      call text%push(trim(labels(mod(i-1, 4) + 1))//","//str(mod(i, 2) + 1)//","//chromosome(numbers(3*i))//","// &
                     str(numbers(3*i-1))//","//str(numbers(3*i-1) + numbers(3*i-2)/4)//IOFL_CR//IOFL_LF)
    end do
    call write_raw(scratch()//".csv", text%as_str())

    call s%read_file(scratch()//".csv", cell_array=cells, row_separator=IOFL_CR+IOFL_LF)
    call expect_equal(size(cells, dim=1), rows + 1, "Every row and the header should be read.")
    call expect_equal(size(cells, dim=2), 5, "The commas inside quotes are not separators.")
    call expect_equal(cells(2,1), "North, Coastal & Island", "The quotes around a cell are removed.")
    call expect_equal(cells(5,1), 'West "Upper" Valley', "A doubled quote is one quote.")
    call expect_equal(cells(2,5), str(numbers(2) + numbers(1)/4), "The last column should line up.")

    call r%write_file(cells, file=scratch()//"_out.csv", row_separator=IOFL_CR+IOFL_LF)
    call check(r == s, "The file should be reproduced exactly.")
  end subroutine csv_quoted_crlf_round_trip

  subroutine tsv_large_round_trip()
    !! A large tab-separated file shaped like a .bed file: a chromosome, a start point and an end point on each of
    !! 192262 lines, like the file in the exome tutorial.
    integer, parameter :: rows = 192262

    type(String)              :: text, s, r
    type(String), allocatable :: cells(:,:)
    integer, allocatable      :: numbers(:)
    integer                   :: i

    call reset_random()
    allocate( numbers(2*rows) )
    call random_values(numbers)
    numbers = abs(mod(numbers, 248000000)) + 1

    do i = 1, rows
      call text%push(chromosome(numbers(2*i))//IOFL_TAB//str(numbers(2*i-1))//IOFL_TAB// &
                     str(numbers(2*i-1) + mod(numbers(2*i), 1000) + 50)//IOFL_LF)
    end do
    call write_raw(scratch()//".bed", text%as_str())

    call s%read_file(scratch()//".bed", cell_array=cells, column_separator=IOFL_TAB)
    call expect_equal(size(cells, dim=1), rows, "Every line should be read.")
    call expect_equal(size(cells, dim=2), 3, "Every line has three columns.")
    call r%write_file(cells, file=scratch()//"_out.bed", column_separator=IOFL_TAB)
    call check(r == s, "The file should be reproduced exactly.")
  end subroutine tsv_large_round_trip

  subroutine read_many_rows_mixed_cells()
    !! Enough rows and cells that read_file grows its record of cells and fills several blocks of rows, with quoted,
    !! escaped, short and CRLF rows throughout, read twice into the same array.
    integer, parameter :: rows = 1000

    character(len=:), allocatable :: file
    type(String)                  :: text, s
    type(String), allocatable     :: cells(:,:)
    integer                       :: i, pass
    logical                       :: ok

    file = scratch()

    call write_raw(file//"_small.csv", "a,b"//IOFL_LF//"c,d"//IOFL_LF)
    call s%read_file(file//"_small.csv", cell_array=cells)

    do i = 1, rows
      if ( mod(i, 7) == 0 ) then
        call text%push(str(i)//',"q,'//str(i)//'"'//IOFL_LF)
      else if ( mod(i, 50) == 0 ) then
        call text%push(str(i)//',"q,'//str(i)//'","say ""'//str(i)//'""",x'//IOFL_CR//IOFL_LF)
      else
        call text%push(str(i)//',"q,'//str(i)//'","say ""'//str(i)//'""",x'//IOFL_LF)
      end if
    end do
    call write_raw(file//".csv", text%as_str())

    do pass = 1, 2
      call s%read_file(file//".csv", cell_array=cells)
      call check(all(shape(cells) == [rows,4]), "Every row should be read, four cells wide.")

      ok = .true.
      do i = 1, rows
        ok = ok .and. (cells(i,1)%as_str() == str(i)) .and. (cells(i,1)%len() == len(str(i)))
        ok = ok .and. (cells(i,2)%as_str() == "q,"//str(i)) .and. (cells(i,2)%len() == len(str(i)) + 2)

        if ( mod(i, 7) == 0 ) then
          ok = ok .and. (cells(i,3)%len() == 0) .and. (cells(i,4)%len() == 0)
        else
          ok = ok .and. (cells(i,3)%as_str() == 'say "'//str(i)//'"') .and. (cells(i,3)%len() == len(str(i)) + 6)
          ok = ok .and. (cells(i,4)%len() == 1)
          ok = ok .and. (cells(i,4)%as_str() == "x")
        end if
      end do

      call check(ok, "Every cell should hold its text, wherever it is in the file.")
    end do
  end subroutine read_many_rows_mixed_cells

  function chromosome(n) result(name)
    !! Returns a chromosome name, chr1 to chr22, chrX or chrY, chosen by `n`.
    integer, intent(in)           :: n
    character(len=:), allocatable :: name

    select case (mod(n, 24))
      case (22)
        name = "chrX"
      case (23)
        name = "chrY"
      case default
        name = "chr"//str(mod(n, 24) + 1)
    end select
  end function chromosome

  subroutine read_tab_in_quotes()
    !! The example in CONTRIBUTING.md.
    type(String)              :: s
    type(String), allocatable :: cells(:,:)

    call write_raw(scratch()//".txt", '"a'//IOFL_TAB//'b"'//IOFL_TAB//"c"//IOFL_LF)
    call s%read_file(scratch()//".txt", cell_array=cells, column_separator=IOFL_TAB)
    call expect_equal(size(cells, dim=2), 2, "A tab inside quotes is not a separator.")
    call expect_equal(cells(1,1), "a"//IOFL_TAB//"b", "The quoted cell keeps its tab.")
  end subroutine read_tab_in_quotes

  subroutine write_file_quotes_text_pairs()
    character(len=:), allocatable :: file
    type(String)                  :: s, one_row(1,2)

    file = scratch()

    one_row(1,1) = String("(a,b)"); one_row(1,2) = String("c")
    call s%write_file(one_row, file=file//".csv")
    call check(s%as_str() == '"(a,b)",c'//IOFL_LF, "write_file should quote text that looks like a pair.")
  end subroutine write_file_quotes_text_pairs
  subroutine read_file_tsv_default()
    !! read_file separates a .tsv file at tabs when no column_separator is given (#70).
    type(String)              :: s
    type(String), allocatable :: cells(:,:)

    call write_raw(scratch()//".tsv", "a"//IOFL_TAB//"b,c"//IOFL_LF//"d"//IOFL_TAB//"e"//IOFL_LF)
    call s%read_file(scratch()//".tsv", cell_array=cells)
    call expect_equal(size(cells, dim=2), 2, "A .tsv file should split at tabs, not commas.")
    call expect_equal(cells(1,2), "b,c", "A comma is ordinary text in a .tsv file.")
    call expect_equal(cells(2,1), "d", "second row")
  end subroutine read_file_tsv_default

  subroutine write_file_tsv_default()
    !! write_file separates cells with tabs in a .tsv file, and quotes a cell that contains a tab.
    type(String) :: s, cells(1,3)

    cells(1,1) = String("a,b"); cells(1,2) = String("c"//IOFL_TAB//"d"); cells(1,3) = String("e")
    call s%write_file(cells, file=scratch()//".tsv")
    call check(read_raw(scratch()//".tsv") == "a,b"//IOFL_TAB//'"c'//IOFL_TAB//'d"'//IOFL_TAB//"e"//IOFL_LF, &
               "write_file should use tabs in a .tsv file and quote a cell containing one.")
  end subroutine write_file_tsv_default
end module delimited_cases
