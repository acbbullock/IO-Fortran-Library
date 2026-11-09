program logging_echo
    use io_fortran_lib, only: echo, str, IOFL_LF
    implicit none (type, external)

    character(len=:), allocatable :: logfile, logmsg
    character(len=10) :: date, time
    integer :: errstat

    call date_and_time(date=date, time=time)
    logfile = "logfile_main_"//trim(adjustl(date))//"_"//time//".log"

    logmsg = "PROGRAM MAIN - BEGINNING EXECUTION"
    call echo(logmsg//IOFL_LF//repeat("-", ncopies=len(logmsg)), file=logfile)

    ! ...

    logmsg = "All is good so far..."
    call echo(logmsg, logfile)

    read(*,*) errstat

    if ( errstat /= 0 ) then
        logmsg = "Process has non-zero exit status: "//str(errstat)//IOFL_LF//"Stopping..."
        call echo(logmsg, logfile)
        error stop logmsg
    end if

    logmsg = "All processes have executed successfully."
    call echo(logmsg, logfile)
end program logging_echo
