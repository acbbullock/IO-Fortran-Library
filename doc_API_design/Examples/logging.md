---
title: log file output
author: Austin C Bullock
---

The following program demonstrates the use of [echo](../Ref/echo.html) for writing messages to a `.log` file, supplemented with the use of [LF](../../module/io_fortran_lib.html#variable-lf) and [str](../Ref/str.html):

```fortran
program main
    use io_fortran_lib, only: echo, LF, str
    implicit none (type,external)

    character(len=:), allocatable :: logfile, logmsg
    character(len=10) :: date, time
    integer :: errstat

    call date_and_time(date=date, time=time)
    logfile = 'logfile_main_'//trim(adjustl(date))//'_'//time//'.log'

    logmsg = 'PROGRAM MAIN - BEGINNING EXECUTION'
    call echo(string=logmsg//LF//repeat('-', ncopies=len(logmsg))//LF, file_name=logfile)

    errstat = 1

    if ( errstat /= 0 ) then
        logmsg = 'Process has non-zero exit status: '//str(errstat)//LF//'Stopping...'
        call echo(logmsg, logfile)
        error stop logmsg
    end if
end program main
```
