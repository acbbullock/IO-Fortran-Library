---
title: log file output
author: Austin C Bullock
---

The following program demonstrates the use of [echo](../Ref/echo.html) for writing messages to a `.log` file, supplemented with the use of [nl](../../module/io_fortran_lib.html#variable-nl) and [str](../Ref/str.html):

```fortran
program main
    use io_fortran_lib, only: echo, nl, str
    implicit none (type,external)

    character(len=:), allocatable :: logfile, logmsg
    character(len=10) :: date, time
    integer :: errstat

    call date_and_time(date=date, time=time)
    logfile = 'logfile_main_'//trim(adjustl(date))//'_'//time//'.log'

    logmsg = 'PROGRAM MAIN - BEGINNING EXECUTION'
    call echo(string=logmsg//nl//repeat('-', ncopies=len(logmsg))//nl, file_name=logfile)

    errstat = 1

    if ( errstat /= 0 ) then
        logmsg = 'Process has non-zero exit status: '//str(errstat)//nl//'Stopping...'
        call echo(logmsg, logfile)
        error stop logmsg
    end if
end program main
```
