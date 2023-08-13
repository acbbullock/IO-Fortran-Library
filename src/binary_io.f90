submodule (io_fortran_lib) binary_io
    !-------------------------------------------------------------------------------------------------------------------
    !! This submodule provides module procedure implementations for the **private interfaces** `to_binary` and
    !! `from_binary`.
    !-------------------------------------------------------------------------------------------------------------------
    implicit none (type, external)

    contains ! Procedure bodies for module subprograms <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>!

    ! Writing Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    module procedure to_binary_1dc128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_1dc128
    module procedure to_binary_1dc64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_1dc64
    module procedure to_binary_1dc32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_1dc32

    module procedure to_binary_2dc128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_2dc128
    module procedure to_binary_2dc64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_2dc64
    module procedure to_binary_2dc32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_2dc32

    module procedure to_binary_3dc128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_3dc128
    module procedure to_binary_3dc64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_3dc64
    module procedure to_binary_3dc32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_3dc32

    module procedure to_binary_4dc128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_4dc128
    module procedure to_binary_4dc64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_4dc64
    module procedure to_binary_4dc32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_4dc32

    module procedure to_binary_5dc128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_5dc128
    module procedure to_binary_5dc64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_5dc64
    module procedure to_binary_5dc32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_5dc32

    module procedure to_binary_6dc128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_6dc128
    module procedure to_binary_6dc64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_6dc64
    module procedure to_binary_6dc32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_6dc32

    module procedure to_binary_7dc128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_7dc128
    module procedure to_binary_7dc64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_7dc64
    module procedure to_binary_7dc32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_7dc32

    module procedure to_binary_8dc128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_8dc128
    module procedure to_binary_8dc64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_8dc64
    module procedure to_binary_8dc32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_8dc32

    module procedure to_binary_9dc128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_9dc128
    module procedure to_binary_9dc64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_9dc64
    module procedure to_binary_9dc32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_9dc32

    module procedure to_binary_10dc128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_10dc128
    module procedure to_binary_10dc64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_10dc64
    module procedure to_binary_10dc32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_10dc32

    module procedure to_binary_11dc128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_11dc128
    module procedure to_binary_11dc64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_11dc64
    module procedure to_binary_11dc32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_11dc32

    module procedure to_binary_12dc128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_12dc128
    module procedure to_binary_12dc64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_12dc64
    module procedure to_binary_12dc32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_12dc32

    module procedure to_binary_13dc128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_13dc128
    module procedure to_binary_13dc64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_13dc64
    module procedure to_binary_13dc32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_13dc32

    module procedure to_binary_14dc128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_14dc128
    module procedure to_binary_14dc64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_14dc64
    module procedure to_binary_14dc32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_14dc32

    module procedure to_binary_15dc128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_15dc128
    module procedure to_binary_15dc64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_15dc64
    module procedure to_binary_15dc32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_15dc32

    module procedure to_binary_1dr128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_1dr128
    module procedure to_binary_1dr64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_1dr64
    module procedure to_binary_1dr32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_1dr32

    module procedure to_binary_2dr128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_2dr128
    module procedure to_binary_2dr64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_2dr64
    module procedure to_binary_2dr32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_2dr32

    module procedure to_binary_3dr128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_3dr128
    module procedure to_binary_3dr64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_3dr64
    module procedure to_binary_3dr32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_3dr32

    module procedure to_binary_4dr128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_4dr128
    module procedure to_binary_4dr64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_4dr64
    module procedure to_binary_4dr32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_4dr32

    module procedure to_binary_5dr128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_5dr128
    module procedure to_binary_5dr64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_5dr64
    module procedure to_binary_5dr32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_5dr32

    module procedure to_binary_6dr128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_6dr128
    module procedure to_binary_6dr64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_6dr64
    module procedure to_binary_6dr32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_6dr32

    module procedure to_binary_7dr128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_7dr128
    module procedure to_binary_7dr64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_7dr64
    module procedure to_binary_7dr32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_7dr32

    module procedure to_binary_8dr128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_8dr128
    module procedure to_binary_8dr64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_8dr64
    module procedure to_binary_8dr32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_8dr32

    module procedure to_binary_9dr128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_9dr128
    module procedure to_binary_9dr64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_9dr64
    module procedure to_binary_9dr32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_9dr32

    module procedure to_binary_10dr128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_10dr128
    module procedure to_binary_10dr64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_10dr64
    module procedure to_binary_10dr32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_10dr32

    module procedure to_binary_11dr128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_11dr128
    module procedure to_binary_11dr64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_11dr64
    module procedure to_binary_11dr32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_11dr32

    module procedure to_binary_12dr128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_12dr128
    module procedure to_binary_12dr64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_12dr64
    module procedure to_binary_12dr32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_12dr32

    module procedure to_binary_13dr128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_13dr128
    module procedure to_binary_13dr64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_13dr64
    module procedure to_binary_13dr32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_13dr32

    module procedure to_binary_14dr128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_14dr128
    module procedure to_binary_14dr64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_14dr64
    module procedure to_binary_14dr32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_14dr32

    module procedure to_binary_15dr128
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_15dr128
    module procedure to_binary_15dr64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_15dr64
    module procedure to_binary_15dr32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_15dr32

    module procedure to_binary_1di64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_1di64
    module procedure to_binary_1di32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_1di32
    module procedure to_binary_1di16
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_1di16
    module procedure to_binary_1di8
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_1di8

    module procedure to_binary_2di64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_2di64
    module procedure to_binary_2di32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_2di32
    module procedure to_binary_2di16
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_2di16
    module procedure to_binary_2di8
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_2di8

    module procedure to_binary_3di64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_3di64
    module procedure to_binary_3di32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_3di32
    module procedure to_binary_3di16
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_3di16
    module procedure to_binary_3di8
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_3di8

    module procedure to_binary_4di64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_4di64
    module procedure to_binary_4di32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_4di32
    module procedure to_binary_4di16
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_4di16
    module procedure to_binary_4di8
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_4di8

    module procedure to_binary_5di64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_5di64
    module procedure to_binary_5di32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_5di32
    module procedure to_binary_5di16
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_5di16
    module procedure to_binary_5di8
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_5di8

    module procedure to_binary_6di64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_6di64
    module procedure to_binary_6di32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_6di32
    module procedure to_binary_6di16
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_6di16
    module procedure to_binary_6di8
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_6di8

    module procedure to_binary_7di64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_7di64
    module procedure to_binary_7di32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_7di32
    module procedure to_binary_7di16
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_7di16
    module procedure to_binary_7di8
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_7di8

    module procedure to_binary_8di64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_8di64
    module procedure to_binary_8di32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_8di32
    module procedure to_binary_8di16
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_8di16
    module procedure to_binary_8di8
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_8di8

    module procedure to_binary_9di64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_9di64
    module procedure to_binary_9di32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_9di32
    module procedure to_binary_9di16
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_9di16
    module procedure to_binary_9di8
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_9di8

    module procedure to_binary_10di64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_10di64
    module procedure to_binary_10di32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_10di32
    module procedure to_binary_10di16
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_10di16
    module procedure to_binary_10di8
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_10di8

    module procedure to_binary_11di64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_11di64
    module procedure to_binary_11di32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_11di32
    module procedure to_binary_11di16
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_11di16
    module procedure to_binary_11di8
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_11di8

    module procedure to_binary_12di64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_12di64
    module procedure to_binary_12di32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_12di32
    module procedure to_binary_12di16
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_12di16
    module procedure to_binary_12di8
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_12di8

    module procedure to_binary_13di64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_13di64
    module procedure to_binary_13di32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_13di32
    module procedure to_binary_13di16
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_13di16
    module procedure to_binary_13di8
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_13di8

    module procedure to_binary_14di64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_14di64
    module procedure to_binary_14di32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_14di32
    module procedure to_binary_14di16
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_14di16
    module procedure to_binary_14di8
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_14di8

    module procedure to_binary_15di64
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_15di64
    module procedure to_binary_15di32
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_15di32
    module procedure to_binary_15di16
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_15di16
    module procedure to_binary_15di8
        logical :: exists
        integer :: file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status="new", form="unformatted", &
                  action="write", access="stream" )
        else
            open( newunit=file_unit, file=file_name, status="replace", form="unformatted", &
                  action="write", access="stream" )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_15di8

    ! Reading Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    module procedure from_binary_1dc128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_1dc128
    module procedure from_binary_1dc64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_1dc64
    module procedure from_binary_1dc32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_1dc32

    module procedure from_binary_2dc128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_2dc128
    module procedure from_binary_2dc64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_2dc64
    module procedure from_binary_2dc32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_2dc32

    module procedure from_binary_3dc128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_3dc128
    module procedure from_binary_3dc64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_3dc64
    module procedure from_binary_3dc32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_3dc32

    module procedure from_binary_4dc128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_4dc128
    module procedure from_binary_4dc64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_4dc64
    module procedure from_binary_4dc32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_4dc32

    module procedure from_binary_5dc128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_5dc128
    module procedure from_binary_5dc64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_5dc64
    module procedure from_binary_5dc32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_5dc32

    module procedure from_binary_6dc128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_6dc128
    module procedure from_binary_6dc64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_6dc64
    module procedure from_binary_6dc32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_6dc32

    module procedure from_binary_7dc128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_7dc128
    module procedure from_binary_7dc64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_7dc64
    module procedure from_binary_7dc32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_7dc32

    module procedure from_binary_8dc128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_8dc128
    module procedure from_binary_8dc64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_8dc64
    module procedure from_binary_8dc32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_8dc32

    module procedure from_binary_9dc128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_9dc128
    module procedure from_binary_9dc64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_9dc64
    module procedure from_binary_9dc32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_9dc32

    module procedure from_binary_10dc128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_10dc128
    module procedure from_binary_10dc64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_10dc64
    module procedure from_binary_10dc32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_10dc32

    module procedure from_binary_11dc128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_11dc128
    module procedure from_binary_11dc64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_11dc64
    module procedure from_binary_11dc32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_11dc32

    module procedure from_binary_12dc128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_12dc128
    module procedure from_binary_12dc64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_12dc64
    module procedure from_binary_12dc32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_12dc32

    module procedure from_binary_13dc128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_13dc128
    module procedure from_binary_13dc64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_13dc64
    module procedure from_binary_13dc32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_13dc32

    module procedure from_binary_14dc128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13), data_shape(14)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_14dc128
    module procedure from_binary_14dc64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13), data_shape(14)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_14dc64
    module procedure from_binary_14dc32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13), data_shape(14)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_14dc32

    module procedure from_binary_15dc128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13), data_shape(14), data_shape(15)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_15dc128
    module procedure from_binary_15dc64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13), data_shape(14), data_shape(15)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_15dc64
    module procedure from_binary_15dc32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13), data_shape(14), data_shape(15)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_15dc32

    module procedure from_binary_1dr128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_1dr128
    module procedure from_binary_1dr64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_1dr64
    module procedure from_binary_1dr32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_1dr32

    module procedure from_binary_2dr128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_2dr128
    module procedure from_binary_2dr64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_2dr64
    module procedure from_binary_2dr32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_2dr32

    module procedure from_binary_3dr128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_3dr128
    module procedure from_binary_3dr64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_3dr64
    module procedure from_binary_3dr32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_3dr32

    module procedure from_binary_4dr128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_4dr128
    module procedure from_binary_4dr64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_4dr64
    module procedure from_binary_4dr32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_4dr32

    module procedure from_binary_5dr128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_5dr128
    module procedure from_binary_5dr64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_5dr64
    module procedure from_binary_5dr32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_5dr32

    module procedure from_binary_6dr128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_6dr128
    module procedure from_binary_6dr64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_6dr64
    module procedure from_binary_6dr32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_6dr32

    module procedure from_binary_7dr128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_7dr128
    module procedure from_binary_7dr64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_7dr64
    module procedure from_binary_7dr32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_7dr32

    module procedure from_binary_8dr128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_8dr128
    module procedure from_binary_8dr64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_8dr64
    module procedure from_binary_8dr32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_8dr32

    module procedure from_binary_9dr128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_9dr128
    module procedure from_binary_9dr64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_9dr64
    module procedure from_binary_9dr32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_9dr32

    module procedure from_binary_10dr128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_10dr128
    module procedure from_binary_10dr64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_10dr64
    module procedure from_binary_10dr32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_10dr32

    module procedure from_binary_11dr128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_11dr128
    module procedure from_binary_11dr64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_11dr64
    module procedure from_binary_11dr32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_11dr32

    module procedure from_binary_12dr128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_12dr128
    module procedure from_binary_12dr64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_12dr64
    module procedure from_binary_12dr32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_12dr32

    module procedure from_binary_13dr128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_13dr128
    module procedure from_binary_13dr64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_13dr64
    module procedure from_binary_13dr32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_13dr32

    module procedure from_binary_14dr128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13), data_shape(14)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_14dr128
    module procedure from_binary_14dr64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13), data_shape(14)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_14dr64
    module procedure from_binary_14dr32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13), data_shape(14)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_14dr32

    module procedure from_binary_15dr128
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13), data_shape(14), data_shape(15)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_15dr128
    module procedure from_binary_15dr64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13), data_shape(14), data_shape(15)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_15dr64
    module procedure from_binary_15dr32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13), data_shape(14), data_shape(15)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_15dr32

    module procedure from_binary_1di64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_1di64
    module procedure from_binary_1di32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_1di32
    module procedure from_binary_1di16
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_1di16
    module procedure from_binary_1di8
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_1di8

    module procedure from_binary_2di64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_2di64
    module procedure from_binary_2di32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_2di32
    module procedure from_binary_2di16
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_2di16
    module procedure from_binary_2di8
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_2di8

    module procedure from_binary_3di64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_3di64
    module procedure from_binary_3di32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_3di32
    module procedure from_binary_3di16
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_3di16
    module procedure from_binary_3di8
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_3di8

    module procedure from_binary_4di64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_4di64
    module procedure from_binary_4di32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_4di32
    module procedure from_binary_4di16
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_4di16
    module procedure from_binary_4di8
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_4di8

    module procedure from_binary_5di64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_5di64
    module procedure from_binary_5di32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_5di32
    module procedure from_binary_5di16
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_5di16
    module procedure from_binary_5di8
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_5di8

    module procedure from_binary_6di64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_6di64
    module procedure from_binary_6di32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_6di32
    module procedure from_binary_6di16
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_6di16
    module procedure from_binary_6di8
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_6di8

    module procedure from_binary_7di64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_7di64
    module procedure from_binary_7di32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_7di32
    module procedure from_binary_7di16
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_7di16
    module procedure from_binary_7di8
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_7di8

    module procedure from_binary_8di64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_8di64
    module procedure from_binary_8di32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_8di32
    module procedure from_binary_8di16
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_8di16
    module procedure from_binary_8di8
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_8di8

    module procedure from_binary_9di64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_9di64
    module procedure from_binary_9di32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_9di32
    module procedure from_binary_9di16
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_9di16
    module procedure from_binary_9di8
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_9di8

    module procedure from_binary_10di64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_10di64
    module procedure from_binary_10di32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_10di32
    module procedure from_binary_10di16
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_10di16
    module procedure from_binary_10di8
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_10di8

    module procedure from_binary_11di64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_11di64
    module procedure from_binary_11di32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_11di32
    module procedure from_binary_11di16
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_11di16
    module procedure from_binary_11di8
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_11di8

    module procedure from_binary_12di64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_12di64
    module procedure from_binary_12di32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_12di32
    module procedure from_binary_12di16
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_12di16
    module procedure from_binary_12di8
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_12di8

    module procedure from_binary_13di64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_13di64
    module procedure from_binary_13di32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_13di32
    module procedure from_binary_13di16
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_13di16
    module procedure from_binary_13di8
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_13di8

    module procedure from_binary_14di64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13), data_shape(14)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_14di64
    module procedure from_binary_14di32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13), data_shape(14)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_14di32
    module procedure from_binary_14di16
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13), data_shape(14)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_14di16
    module procedure from_binary_14di8
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13), data_shape(14)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_14di8

    module procedure from_binary_15di64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13), data_shape(14), data_shape(15)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_15di64
    module procedure from_binary_15di32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13), data_shape(14), data_shape(15)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_15di32
    module procedure from_binary_15di16
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13), data_shape(14), data_shape(15)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_15di16
    module procedure from_binary_15di8
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status="old", form="unformatted", &
                  action="read", access="stream", position="rewind" )
        else
            error stop LF//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5), data_shape(6), &
                       data_shape(7), data_shape(8), data_shape(9), data_shape(10), data_shape(11), data_shape(12), &
                       data_shape(13), data_shape(14), data_shape(15)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop LF//'FATAL: Error reading file "'//file_name//'". iostat is '//str(iostat)
            return
        end if

        close(file_unit)
    end procedure from_binary_15di8
end submodule binary_io
