---
title: log file output
author: Austin C Bullock
---

The following program demonstrates a simple use of [echo](../Ref/echo.html) for writing messages to a `.log` file, supplemented with the use of [LF](../../module/io_fortran_lib.html#variable-lf) and [str](../Ref/str.html):

```fortran
program main
    use io_fortran_lib, only: echo, str, LF
    implicit none (type,external)

    character(len=:), allocatable :: logfile, logmsg
    character(len=10) :: date, time
    integer :: errstat

    call date_and_time(date=date, time=time)
    logfile = 'logfile_main_'//trim(adjustl(date))//'_'//time//'.log'

    logmsg = 'PROGRAM MAIN - BEGINNING EXECUTION'//LF
    call echo(string=logmsg//repeat('-', ncopies=len(logmsg)), file_name=logfile)

    ! ...

    logmsg = 'All is good so far...'
    call echo(logmsg, logfile)

    read(*,*) errstat

    if ( errstat /= 0 ) then
        logmsg = 'Process has non-zero exit status: '//str(errstat)//LF//'Stopping...'
        call echo(logmsg, logfile)
        error stop logmsg
    end if

    logmsg = 'All processes have executed successfully.'
    call echo(logmsg, logfile)
end program main
```

Depending on style, one may wish to accumulate log messages into a `String` and then `echo` conditionally:

```fortran
program main
    use io_fortran_lib, only: String, str, LF, operator(+), operator(**)
    implicit none (type,external)

    type(String) :: logmsg
    character(len=:), allocatable :: logfile
    character(len=10) :: date, time
    integer :: errstat

    call date_and_time(date=date, time=time)
    logfile = 'logfile_main_' + trim(adjustl(date)) + '_' + time + '.log'

    logmsg = String('PROGRAM MAIN - BEGINNING EXECUTION' + LF)
    call logmsg%push('-'**logmsg%len() + LF)

    ! ...

    call logmsg%push('All is good so far...' + LF)

    read(*,*) errstat

    if ( errstat /= 0 ) then
        call logmsg%push('Process has non-zero exit status: ' + str(errstat) + LF + 'Stopping...')
        call logmsg%echo(file_name=logfile)
        error stop logmsg%as_str()
    end if

    call logmsg%push('All processes have executed successfully.')
    call logmsg%echo(file_name=logfile)
end program main
```

Here, the `error stop` will dump the entire contents of `logmsg` to stdout. We also take advantage of the [operators](../Ref/operators.html) `+` and `**` for concatenation and repetition.
