program split_join
    use io_fortran_lib, only: String, join, split, IOFL_LF, operator(+), operator(-)
    implicit none

    type(String) :: time_stamp, new_time_stamp
    type(String), allocatable :: tokens(:)

    character(len=10) :: date, time

    call date_and_time(date=date, time=time)

    time_stamp = String("Date : " + date + IOFL_LF + "Time : " + time)

    write(*,"(a)") "ORIGINAL TIME STAMP:" + IOFL_LF + time_stamp%as_str() + IOFL_LF

    tokens = split(time_stamp, separator=IOFL_LF) - "Date : " - "Time : " + [" : Date", " : Time"]
    new_time_stamp = join(tokens, separator=" | ")

    write(*,"(a)") "RECONSTRUCTED TIME STAMP:" + IOFL_LF + new_time_stamp%as_str()
end program split_join
