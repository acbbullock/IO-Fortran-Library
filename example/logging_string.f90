program logging_string
    use io_fortran_lib, only: String, str, IOFL_LF, operator(+), operator(**)
    implicit none (type, external)

    type(String) :: logmsg
    character(len=:), allocatable :: logfile
    character(len=10) :: date, time
    integer :: errstat

    call date_and_time(date=date, time=time)
    logfile = "logfile_main_" + trim(adjustl(date)) + "_" + time + ".log"

    logmsg = String("PROGRAM MAIN - BEGINNING EXECUTION")
    call logmsg%push(IOFL_LF + "-"**logmsg%len() + IOFL_LF)

    ! ...

    call logmsg%push("All is good so far..." + IOFL_LF)

    read(*,*) errstat

    if ( errstat /= 0 ) then
        call logmsg%push("Process has non-zero exit status: " + str(errstat) + IOFL_LF + "Stopping...")
        call logmsg%echo(logfile)
        error stop logmsg%as_str()
    end if

    call logmsg%push("All processes have executed successfully.")
    call logmsg%echo(logfile)
end program logging_string
