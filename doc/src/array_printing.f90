submodule (io_fortran_lib) array_printing
    !-------------------------------------------------------------------------------------------------------------------
    !! This submodule provides module procedure implementations for the **public interface** `aprint`.
    !-------------------------------------------------------------------------------------------------------------------
    implicit none (type, external)

    contains ! Procedure bodies for module subprograms <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>!

    module procedure aprint_1dc128
        character(len=:), allocatable :: x_str(:)
        character(len=:), allocatable :: fmt_, im_, xre_max_str, xre_min_str, xim_max_str, xim_min_str
        integer                       :: i, decimals_, l

        if ( .not. present(fmt) ) then
            fmt_ = "f"
        else
            if ( any(REAL_FMTS == fmt) ) then
                fmt_ = fmt
            else
                fmt_ = "f"
            end if
        end if

        if ( .not. present(decimals) ) then
            decimals_ = 2
        else
            decimals_ = decimals
        end if

        if ( .not. present(im) ) then
            im_ = "j"
        else
            im_ = trim(adjustl(im))
        end if

        if ( len(im_) > 0 ) then
            l = len(im_)
        else
            l = 3
        end if

        xre_max_str = str(maxval(x%re), fmt=fmt_, decimals=decimals_)
        xre_min_str = str(minval(x%re), fmt=fmt_, decimals=decimals_)
        xim_max_str = str(maxval(x%im), fmt=fmt_, decimals=decimals_)
        xim_min_str = str(minval(x%im), fmt=fmt_, decimals=decimals_)

        if ( len(xre_max_str) > len(xre_min_str) ) then
            l = l + len(xre_max_str)
        else
            l = l + len(xre_min_str)
        end if

        if ( len(xim_max_str) > len(xim_min_str) ) then
            l = l + len(xim_max_str)
        else
            l = l + len(xim_min_str)
        end if

        allocate( character(len=l) :: x_str(lbound(x, dim=1):ubound(x, dim=1)) )

        do i = lbound(x, dim=1), ubound(x, dim=1)
            x_str(i) = str(x(i), fmt=fmt_, decimals=decimals_, im=im_)
        end do

        call aprint(x_str)
    end procedure aprint_1dc128
    module procedure aprint_1dc64
        character(len=:), allocatable :: x_str(:)
        character(len=:), allocatable :: fmt_, im_, xre_max_str, xre_min_str, xim_max_str, xim_min_str
        integer                       :: i, decimals_, l

        if ( .not. present(fmt) ) then
            fmt_ = "f"
        else
            if ( any(REAL_FMTS == fmt) ) then
                fmt_ = fmt
            else
                fmt_ = "f"
            end if
        end if

        if ( .not. present(decimals) ) then
            decimals_ = 2
        else
            decimals_ = decimals
        end if

        if ( .not. present(im) ) then
            im_ = "j"
        else
            im_ = trim(adjustl(im))
        end if

        if ( len(im_) > 0 ) then
            l = len(im_)
        else
            l = 3
        end if

        xre_max_str = str(maxval(x%re), fmt=fmt_, decimals=decimals_)
        xre_min_str = str(minval(x%re), fmt=fmt_, decimals=decimals_)
        xim_max_str = str(maxval(x%im), fmt=fmt_, decimals=decimals_)
        xim_min_str = str(minval(x%im), fmt=fmt_, decimals=decimals_)

        if ( len(xre_max_str) > len(xre_min_str) ) then
            l = l + len(xre_max_str)
        else
            l = l + len(xre_min_str)
        end if

        if ( len(xim_max_str) > len(xim_min_str) ) then
            l = l + len(xim_max_str)
        else
            l = l + len(xim_min_str)
        end if

        allocate( character(len=l) :: x_str(lbound(x, dim=1):ubound(x, dim=1)) )

        do i = lbound(x, dim=1), ubound(x, dim=1)
            x_str(i) = str(x(i), fmt=fmt_, decimals=decimals_, im=im_)
        end do

        call aprint(x_str)
    end procedure aprint_1dc64
    module procedure aprint_1dc32
        character(len=:), allocatable :: x_str(:)
        character(len=:), allocatable :: fmt_, im_, xre_max_str, xre_min_str, xim_max_str, xim_min_str
        integer                       :: i, decimals_, l

        if ( .not. present(fmt) ) then
            fmt_ = "f"
        else
            if ( any(REAL_FMTS == fmt) ) then
                fmt_ = fmt
            else
                fmt_ = "f"
            end if
        end if

        if ( .not. present(decimals) ) then
            decimals_ = 2
        else
            decimals_ = decimals
        end if

        if ( .not. present(im) ) then
            im_ = "j"
        else
            im_ = trim(adjustl(im))
        end if

        if ( len(im_) > 0 ) then
            l = len(im_)
        else
            l = 3
        end if

        xre_max_str = str(maxval(x%re), fmt=fmt_, decimals=decimals_)
        xre_min_str = str(minval(x%re), fmt=fmt_, decimals=decimals_)
        xim_max_str = str(maxval(x%im), fmt=fmt_, decimals=decimals_)
        xim_min_str = str(minval(x%im), fmt=fmt_, decimals=decimals_)

        if ( len(xre_max_str) > len(xre_min_str) ) then
            l = l + len(xre_max_str)
        else
            l = l + len(xre_min_str)
        end if

        if ( len(xim_max_str) > len(xim_min_str) ) then
            l = l + len(xim_max_str)
        else
            l = l + len(xim_min_str)
        end if

        allocate( character(len=l) :: x_str(lbound(x, dim=1):ubound(x, dim=1)) )

        do i = lbound(x, dim=1), ubound(x, dim=1)
            x_str(i) = str(x(i), fmt=fmt_, decimals=decimals_, im=im_)
        end do

        call aprint(x_str)
    end procedure aprint_1dc32

    module procedure aprint_2dc128
        character(len=:), allocatable :: x_str(:,:)
        character(len=:), allocatable :: fmt_, im_, xre_max_str, xre_min_str, xim_max_str, xim_min_str
        integer                       :: i, j, decimals_, l

        if ( .not. present(fmt) ) then
            fmt_ = "f"
        else
            if ( any(REAL_FMTS == fmt) ) then
                fmt_ = fmt
            else
                fmt_ = "f"
            end if
        end if

        if ( .not. present(decimals) ) then
            decimals_ = 2
        else
            decimals_ = decimals
        end if

        if ( .not. present(im) ) then
            im_ = "j"
        else
            im_ = trim(adjustl(im))
        end if

        if ( len(im_) > 0 ) then
            l = len(im_)
        else
            l = 3
        end if

        xre_max_str = str(maxval(x%re), fmt=fmt_, decimals=decimals_)
        xre_min_str = str(minval(x%re), fmt=fmt_, decimals=decimals_)
        xim_max_str = str(maxval(x%im), fmt=fmt_, decimals=decimals_)
        xim_min_str = str(minval(x%im), fmt=fmt_, decimals=decimals_)

        if ( len(xre_max_str) > len(xre_min_str) ) then
            l = l + len(xre_max_str)
        else
            l = l + len(xre_min_str)
        end if

        if ( len(xim_max_str) > len(xim_min_str) ) then
            l = l + len(xim_max_str)
        else
            l = l + len(xim_min_str)
        end if

        allocate( character(len=l) :: x_str(lbound(x, dim=1):ubound(x, dim=1), lbound(x, dim=2):ubound(x, dim=2)) )

        do j = lbound(x, dim=2), ubound(x, dim=2)
            do i = lbound(x, dim=1), ubound(x, dim=1)
                x_str(i,j) = str(x(i,j), fmt=fmt_, decimals=decimals_, im=im_)
            end do
        end do

        call aprint(x_str)
    end procedure aprint_2dc128
    module procedure aprint_2dc64
        character(len=:), allocatable :: x_str(:,:)
        character(len=:), allocatable :: fmt_, im_, xre_max_str, xre_min_str, xim_max_str, xim_min_str
        integer                       :: i, j, decimals_, l

        if ( .not. present(fmt) ) then
            fmt_ = "f"
        else
            if ( any(REAL_FMTS == fmt) ) then
                fmt_ = fmt
            else
                fmt_ = "f"
            end if
        end if

        if ( .not. present(decimals) ) then
            decimals_ = 2
        else
            decimals_ = decimals
        end if

        if ( .not. present(im) ) then
            im_ = "j"
        else
            im_ = trim(adjustl(im))
        end if

        if ( len(im_) > 0 ) then
            l = len(im_)
        else
            l = 3
        end if

        xre_max_str = str(maxval(x%re), fmt=fmt_, decimals=decimals_)
        xre_min_str = str(minval(x%re), fmt=fmt_, decimals=decimals_)
        xim_max_str = str(maxval(x%im), fmt=fmt_, decimals=decimals_)
        xim_min_str = str(minval(x%im), fmt=fmt_, decimals=decimals_)

        if ( len(xre_max_str) > len(xre_min_str) ) then
            l = l + len(xre_max_str)
        else
            l = l + len(xre_min_str)
        end if

        if ( len(xim_max_str) > len(xim_min_str) ) then
            l = l + len(xim_max_str)
        else
            l = l + len(xim_min_str)
        end if

        allocate( character(len=l) :: x_str(lbound(x, dim=1):ubound(x, dim=1), lbound(x, dim=2):ubound(x, dim=2)) )

        do j = lbound(x, dim=2), ubound(x, dim=2)
            do i = lbound(x, dim=1), ubound(x, dim=1)
                x_str(i,j) = str(x(i,j), fmt=fmt_, decimals=decimals_, im=im_)
            end do
        end do

        call aprint(x_str)
    end procedure aprint_2dc64
    module procedure aprint_2dc32
        character(len=:), allocatable :: x_str(:,:)
        character(len=:), allocatable :: fmt_, im_, xre_max_str, xre_min_str, xim_max_str, xim_min_str
        integer                       :: i, j, decimals_, l

        if ( .not. present(fmt) ) then
            fmt_ = "f"
        else
            if ( any(REAL_FMTS == fmt) ) then
                fmt_ = fmt
            else
                fmt_ = "f"
            end if
        end if

        if ( .not. present(decimals) ) then
            decimals_ = 2
        else
            decimals_ = decimals
        end if

        if ( .not. present(im) ) then
            im_ = "j"
        else
            im_ = trim(adjustl(im))
        end if

        if ( len(im_) > 0 ) then
            l = len(im_)
        else
            l = 3
        end if

        xre_max_str = str(maxval(x%re), fmt=fmt_, decimals=decimals_)
        xre_min_str = str(minval(x%re), fmt=fmt_, decimals=decimals_)
        xim_max_str = str(maxval(x%im), fmt=fmt_, decimals=decimals_)
        xim_min_str = str(minval(x%im), fmt=fmt_, decimals=decimals_)

        if ( len(xre_max_str) > len(xre_min_str) ) then
            l = l + len(xre_max_str)
        else
            l = l + len(xre_min_str)
        end if

        if ( len(xim_max_str) > len(xim_min_str) ) then
            l = l + len(xim_max_str)
        else
            l = l + len(xim_min_str)
        end if

        allocate( character(len=l) :: x_str(lbound(x, dim=1):ubound(x, dim=1), lbound(x, dim=2):ubound(x, dim=2)) )

        do j = lbound(x, dim=2), ubound(x, dim=2)
            do i = lbound(x, dim=1), ubound(x, dim=1)
                x_str(i,j) = str(x(i,j), fmt=fmt_, decimals=decimals_, im=im_)
            end do
        end do

        call aprint(x_str)
    end procedure aprint_2dc32

    module procedure aprint_1dr128
        character(len=:), allocatable :: x_str(:)
        character(len=:), allocatable :: fmt_, x_max_str, x_min_str, x_abs_min_str, source
        integer                       :: i, decimals_

        if ( .not. present(fmt) ) then
            fmt_ = "f"
        else
            if ( any(REAL_FMTS == fmt) ) then
                fmt_ = fmt
            else
                fmt_ = "f"
            end if
        end if

        if ( .not. present(decimals) ) then
            decimals_ = 2
        else
            decimals_ = decimals
        end if

        x_max_str = str(maxval(x), fmt=fmt_, decimals=decimals_)
        x_min_str = str(minval(x), fmt=fmt_, decimals=decimals_)
        x_abs_min_str = str(minval(abs(x)), fmt=fmt_, decimals=decimals_)

        if ( len(x_max_str) > len(x_min_str) ) then
            source = x_max_str
        else
            source = x_min_str
        end if

        if ( len(x_abs_min_str) > len(source) ) source = x_abs_min_str

        allocate( x_str(lbound(x, dim=1):ubound(x, dim=1)), source=source )

        do i = lbound(x, dim=1), ubound(x, dim=1)
            x_str(i) = str(x(i), fmt=fmt_, decimals=decimals_)
        end do

        call aprint(x_str)
    end procedure aprint_1dr128
    module procedure aprint_1dr64
        character(len=:), allocatable :: x_str(:)
        character(len=:), allocatable :: fmt_, x_max_str, x_min_str, x_abs_min_str, source
        integer                       :: i, decimals_

        if ( .not. present(fmt) ) then
            fmt_ = "f"
        else
            if ( any(REAL_FMTS == fmt) ) then
                fmt_ = fmt
            else
                fmt_ = "f"
            end if
        end if

        if ( .not. present(decimals) ) then
            decimals_ = 2
        else
            decimals_ = decimals
        end if

        x_max_str = str(maxval(x), fmt=fmt_, decimals=decimals_)
        x_min_str = str(minval(x), fmt=fmt_, decimals=decimals_)
        x_abs_min_str = str(minval(abs(x)), fmt=fmt_, decimals=decimals_)

        if ( len(x_max_str) > len(x_min_str) ) then
            source = x_max_str
        else
            source = x_min_str
        end if

        if ( len(x_abs_min_str) > len(source) ) source = x_abs_min_str

        allocate( x_str(lbound(x, dim=1):ubound(x, dim=1)), source=source )

        do i = lbound(x, dim=1), ubound(x, dim=1)
            x_str(i) = str(x(i), fmt=fmt_, decimals=decimals_)
        end do

        call aprint(x_str)
    end procedure aprint_1dr64
    module procedure aprint_1dr32
        character(len=:), allocatable :: x_str(:)
        character(len=:), allocatable :: fmt_, x_max_str, x_min_str, x_abs_min_str, source
        integer                       :: i, decimals_

        if ( .not. present(fmt) ) then
            fmt_ = "f"
        else
            if ( any(REAL_FMTS == fmt) ) then
                fmt_ = fmt
            else
                fmt_ = "f"
            end if
        end if

        if ( .not. present(decimals) ) then
            decimals_ = 2
        else
            decimals_ = decimals
        end if

        x_max_str = str(maxval(x), fmt=fmt_, decimals=decimals_)
        x_min_str = str(minval(x), fmt=fmt_, decimals=decimals_)
        x_abs_min_str = str(minval(abs(x)), fmt=fmt_, decimals=decimals_)

        if ( len(x_max_str) > len(x_min_str) ) then
            source = x_max_str
        else
            source = x_min_str
        end if

        if ( len(x_abs_min_str) > len(source) ) source = x_abs_min_str

        allocate( x_str(lbound(x, dim=1):ubound(x, dim=1)), source=source )

        do i = lbound(x, dim=1), ubound(x, dim=1)
            x_str(i) = str(x(i), fmt=fmt_, decimals=decimals_)
        end do

        call aprint(x_str)
    end procedure aprint_1dr32

    module procedure aprint_2dr128
        character(len=:), allocatable :: x_str(:,:)
        character(len=:), allocatable :: fmt_, x_max_str, x_min_str, x_abs_min_str, source
        integer                       :: i, j, decimals_

        if ( .not. present(fmt) ) then
            fmt_ = "f"
        else
            if ( any(REAL_FMTS == fmt) ) then
                fmt_ = fmt
            else
                fmt_ = "f"
            end if
        end if

        if ( .not. present(decimals) ) then
            decimals_ = 2
        else
            decimals_ = decimals
        end if

        x_max_str = str(maxval(x), fmt=fmt_, decimals=decimals_)
        x_min_str = str(minval(x), fmt=fmt_, decimals=decimals_)
        x_abs_min_str = str(minval(abs(x)), fmt=fmt_, decimals=decimals_)

        if ( len(x_max_str) > len(x_min_str) ) then
            source = x_max_str
        else
            source = x_min_str
        end if

        if ( len(x_abs_min_str) > len(source) ) source = x_abs_min_str

        allocate( x_str(lbound(x, dim=1):ubound(x, dim=1), lbound(x, dim=2):ubound(x, dim=2)), source=source )

        do j = lbound(x, dim=2), ubound(x, dim=2)
            do i = lbound(x, dim=1), ubound(x, dim=1)
                x_str(i,j) = str(x(i,j), fmt=fmt_, decimals=decimals_)
            end do
        end do

        call aprint(x_str)
    end procedure aprint_2dr128
    module procedure aprint_2dr64
        character(len=:), allocatable :: x_str(:,:)
        character(len=:), allocatable :: fmt_, x_max_str, x_min_str, x_abs_min_str, source
        integer                       :: i, j, decimals_

        if ( .not. present(fmt) ) then
            fmt_ = "f"
        else
            if ( any(REAL_FMTS == fmt) ) then
                fmt_ = fmt
            else
                fmt_ = "f"
            end if
        end if

        if ( .not. present(decimals) ) then
            decimals_ = 2
        else
            decimals_ = decimals
        end if

        x_max_str = str(maxval(x), fmt=fmt_, decimals=decimals_)
        x_min_str = str(minval(x), fmt=fmt_, decimals=decimals_)
        x_abs_min_str = str(minval(abs(x)), fmt=fmt_, decimals=decimals_)

        if ( len(x_max_str) > len(x_min_str) ) then
            source = x_max_str
        else
            source = x_min_str
        end if

        if ( len(x_abs_min_str) > len(source) ) source = x_abs_min_str

        allocate( x_str(lbound(x, dim=1):ubound(x, dim=1), lbound(x, dim=2):ubound(x, dim=2)), source=source )

        do j = lbound(x, dim=2), ubound(x, dim=2)
            do i = lbound(x, dim=1), ubound(x, dim=1)
                x_str(i,j) = str(x(i,j), fmt=fmt_, decimals=decimals_)
            end do
        end do

        call aprint(x_str)
    end procedure aprint_2dr64
    module procedure aprint_2dr32
        character(len=:), allocatable :: x_str(:,:)
        character(len=:), allocatable :: fmt_, x_max_str, x_min_str, x_abs_min_str, source
        integer                       :: i, j, decimals_

        if ( .not. present(fmt) ) then
            fmt_ = "f"
        else
            if ( any(REAL_FMTS == fmt) ) then
                fmt_ = fmt
            else
                fmt_ = "f"
            end if
        end if

        if ( .not. present(decimals) ) then
            decimals_ = 2
        else
            decimals_ = decimals
        end if

        x_max_str = str(maxval(x), fmt=fmt_, decimals=decimals_)
        x_min_str = str(minval(x), fmt=fmt_, decimals=decimals_)
        x_abs_min_str = str(minval(abs(x)), fmt=fmt_, decimals=decimals_)

        if ( len(x_max_str) > len(x_min_str) ) then
            source = x_max_str
        else
            source = x_min_str
        end if

        if ( len(x_abs_min_str) > len(source) ) source = x_abs_min_str

        allocate( x_str(lbound(x, dim=1):ubound(x, dim=1), lbound(x, dim=2):ubound(x, dim=2)), source=source )

        do j = lbound(x, dim=2), ubound(x, dim=2)
            do i = lbound(x, dim=1), ubound(x, dim=1)
                x_str(i,j) = str(x(i,j), fmt=fmt_, decimals=decimals_)
            end do
        end do

        call aprint(x_str)
    end procedure aprint_2dr32

    module procedure aprint_1di64
        character(len=:), allocatable :: x_str(:)
        character(len=:), allocatable :: fmt_, x_max_str, x_min_str, source
        integer                       :: i

        if ( .not. present(fmt) ) then
            fmt_ = "i"
        else
            if ( any(INT_FMTS == fmt) ) then
                fmt_ = fmt
            else
                fmt_ = "i"
            end if
        end if

        x_max_str = str(maxval(x), fmt=fmt_)
        x_min_str = str(minval(x), fmt=fmt_)

        if ( len(x_max_str) > len(x_min_str) ) then
            source = x_max_str
        else
            source = x_min_str
        end if

        allocate( x_str(lbound(x, dim=1):ubound(x, dim=1)), source=source )

        do i = lbound(x, dim=1), ubound(x, dim=1)
            x_str(i) = str(x(i), fmt=fmt_)
        end do

        call aprint(x_str)
    end procedure aprint_1di64
    module procedure aprint_1di32
        character(len=:), allocatable :: x_str(:)
        character(len=:), allocatable :: fmt_, x_max_str, x_min_str, source
        integer                       :: i

        if ( .not. present(fmt) ) then
            fmt_ = "i"
        else
            if ( any(INT_FMTS == fmt) ) then
                fmt_ = fmt
            else
                fmt_ = "i"
            end if
        end if

        x_max_str = str(maxval(x), fmt=fmt_)
        x_min_str = str(minval(x), fmt=fmt_)

        if ( len(x_max_str) > len(x_min_str) ) then
            source = x_max_str
        else
            source = x_min_str
        end if

        allocate( x_str(lbound(x, dim=1):ubound(x, dim=1)), source=source )

        do i = lbound(x, dim=1), ubound(x, dim=1)
            x_str(i) = str(x(i), fmt=fmt_)
        end do

        call aprint(x_str)
    end procedure aprint_1di32
    module procedure aprint_1di16
        character(len=:), allocatable :: x_str(:)
        character(len=:), allocatable :: fmt_, x_max_str, x_min_str, source
        integer                       :: i

        if ( .not. present(fmt) ) then
            fmt_ = "i"
        else
            if ( any(INT_FMTS == fmt) ) then
                fmt_ = fmt
            else
                fmt_ = "i"
            end if
        end if

        x_max_str = str(maxval(x), fmt=fmt_)
        x_min_str = str(minval(x), fmt=fmt_)

        if ( len(x_max_str) > len(x_min_str) ) then
            source = x_max_str
        else
            source = x_min_str
        end if

        allocate( x_str(lbound(x, dim=1):ubound(x, dim=1)), source=source )

        do i = lbound(x, dim=1), ubound(x, dim=1)
            x_str(i) = str(x(i), fmt=fmt_)
        end do

        call aprint(x_str)
    end procedure aprint_1di16
    module procedure aprint_1di8
        character(len=:), allocatable :: x_str(:)
        character(len=:), allocatable :: fmt_, x_max_str, x_min_str, source
        integer                       :: i

        if ( .not. present(fmt) ) then
            fmt_ = "i"
        else
            if ( any(INT_FMTS == fmt) ) then
                fmt_ = fmt
            else
                fmt_ = "i"
            end if
        end if

        x_max_str = str(maxval(x), fmt=fmt_)
        x_min_str = str(minval(x), fmt=fmt_)

        if ( len(x_max_str) > len(x_min_str) ) then
            source = x_max_str
        else
            source = x_min_str
        end if

        allocate( x_str(lbound(x, dim=1):ubound(x, dim=1)), source=source )

        do i = lbound(x, dim=1), ubound(x, dim=1)
            x_str(i) = str(x(i), fmt=fmt_)
        end do

        call aprint(x_str)
    end procedure aprint_1di8

    module procedure aprint_2di64
        character(len=:), allocatable :: x_str(:,:)
        character(len=:), allocatable :: fmt_, x_max_str, x_min_str, source, str_tmp
        integer                       :: i, j

        if ( .not. present(fmt) ) then
            fmt_ = "i"
        else
            if ( any(INT_FMTS == fmt) ) then
                fmt_ = fmt
            else
                fmt_ = "i"
            end if
        end if

        x_max_str = str(maxval(x), fmt=fmt_)
        x_min_str = str(minval(x), fmt=fmt_)

        if ( len(x_max_str) > len(x_min_str) ) then
            source = x_max_str
        else
            source = x_min_str
        end if

        allocate( x_str(lbound(x, dim=1):ubound(x, dim=1), lbound(x, dim=2):ubound(x, dim=2)), source=source )

        do j = lbound(x, dim=2), ubound(x, dim=2)
            do i = lbound(x, dim=1), ubound(x, dim=1)
                x_str(i,j) = str(x(i,j), fmt=fmt_)
            end do
        end do

        call aprint(x_str)
    end procedure aprint_2di64
    module procedure aprint_2di32
        character(len=:), allocatable :: x_str(:,:)
        character(len=:), allocatable :: fmt_, x_max_str, x_min_str, source, str_tmp
        integer                       :: i, j

        if ( .not. present(fmt) ) then
            fmt_ = "i"
        else
            if ( any(INT_FMTS == fmt) ) then
                fmt_ = fmt
            else
                fmt_ = "i"
            end if
        end if

        x_max_str = str(maxval(x), fmt=fmt_)
        x_min_str = str(minval(x), fmt=fmt_)

        if ( len(x_max_str) > len(x_min_str) ) then
            source = x_max_str
        else
            source = x_min_str
        end if

        allocate( x_str(lbound(x, dim=1):ubound(x, dim=1), lbound(x, dim=2):ubound(x, dim=2)), source=source )

        do j = lbound(x, dim=2), ubound(x, dim=2)
            do i = lbound(x, dim=1), ubound(x, dim=1)
                x_str(i,j) = str(x(i,j), fmt=fmt_)
            end do
        end do

        call aprint(x_str)
    end procedure aprint_2di32
    module procedure aprint_2di16
        character(len=:), allocatable :: x_str(:,:)
        character(len=:), allocatable :: fmt_, x_max_str, x_min_str, source, str_tmp
        integer                       :: i, j

        if ( .not. present(fmt) ) then
            fmt_ = "i"
        else
            if ( any(INT_FMTS == fmt) ) then
                fmt_ = fmt
            else
                fmt_ = "i"
            end if
        end if

        x_max_str = str(maxval(x), fmt=fmt_)
        x_min_str = str(minval(x), fmt=fmt_)

        if ( len(x_max_str) > len(x_min_str) ) then
            source = x_max_str
        else
            source = x_min_str
        end if

        allocate( x_str(lbound(x, dim=1):ubound(x, dim=1), lbound(x, dim=2):ubound(x, dim=2)), source=source )

        do j = lbound(x, dim=2), ubound(x, dim=2)
            do i = lbound(x, dim=1), ubound(x, dim=1)
                x_str(i,j) = str(x(i,j), fmt=fmt_)
            end do
        end do

        call aprint(x_str)
    end procedure aprint_2di16
    module procedure aprint_2di8
        character(len=:), allocatable :: x_str(:,:)
        character(len=:), allocatable :: fmt_, x_max_str, x_min_str, source, str_tmp
        integer                       :: i, j

        if ( .not. present(fmt) ) then
            fmt_ = "i"
        else
            if ( any(INT_FMTS == fmt) ) then
                fmt_ = fmt
            else
                fmt_ = "i"
            end if
        end if

        x_max_str = str(maxval(x), fmt=fmt_)
        x_min_str = str(minval(x), fmt=fmt_)

        if ( len(x_max_str) > len(x_min_str) ) then
            source = x_max_str
        else
            source = x_min_str
        end if

        allocate( x_str(lbound(x, dim=1):ubound(x, dim=1), lbound(x, dim=2):ubound(x, dim=2)), source=source )

        do j = lbound(x, dim=2), ubound(x, dim=2)
            do i = lbound(x, dim=1), ubound(x, dim=1)
                x_str(i,j) = str(x(i,j), fmt=fmt_)
            end do
        end do

        call aprint(x_str)
    end procedure aprint_2di8

    module procedure aprint_1dchar
        type(String), allocatable :: rows(:)
        integer                   :: i

        allocate( rows(lbound(x, dim=1):ubound(x, dim=1)) )

        do i = lbound(x, dim=1), ubound(x, dim=1)
            if ( i == lbound(x, dim=1) ) then
                if ( i == ubound(x, dim=1) ) then
                    rows(i)%s = LF//'    '//adjustl( x(i) )//LF
                else
                    rows(i)%s = LF//'    '//adjustl( x(i) )
                end if
            else if ( i == ubound(x, dim=1) ) then
                rows(i)%s = '    '//adjustl( x(i) )//LF
            else
                rows(i)%s = '    '//adjustl( x(i) )
            end if
        end do

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(*,"(a)") rows(i)%s
        end do
    end procedure aprint_1dchar

    module procedure aprint_2dchar
        type(String), allocatable :: rows(:)
        integer                   :: i

        allocate( rows(lbound(x, dim=1):ubound(x, dim=1)) )

        do i = lbound(x, dim=1), ubound(x, dim=1)
            if ( i == lbound(x, dim=1) ) then
                if ( i == ubound(x, dim=1) ) then
                    rows(i)%s = LF//'    '//accum( x(i,:) )//LF
                else
                    rows(i)%s = LF//'    '//accum( x(i,:) )
                end if
            else if ( i == ubound(x, dim=1) ) then
                rows(i)%s = '    '//accum( x(i,:) )//LF
            else
                rows(i)%s = '    '//accum( x(i,:) )
            end if
        end do

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(*,"(a)") rows(i)%s
        end do

        contains
        pure recursive function accum(x) result(x_str)
            character(len=*), intent(in)  :: x(:)
            character(len=:), allocatable :: x_str

            integer :: x_len, x_size, i, pos

            x_len  = len(x)
            x_size = size(x)

            if ( x_size == 1 ) then
                x_str = x(1); return
            end if

            if ( x_len == 0 ) then
                x_str = EMPTY_STR; return
            end if

            allocate( character(len=x_len*x_size + x_size - 1) :: x_str )

            positional_transfer: do i = 1, x_size
                pos = (i-1)*(x_len + 1) + 1
                x_str(pos:pos+x_len-1) = adjustl(x(i))
                if ( i < x_size ) x_str(pos+x_len:pos+x_len) = SPACE
            end do positional_transfer
        end function accum
    end procedure aprint_2dchar

    module procedure aprint_1dString
        character(len=:), allocatable :: char_arr(:)
        integer, allocatable          :: lengths(:)
        integer                       :: i, max_length

        lengths    = x%len()
        max_length = maxval(lengths)

        allocate( character(len=max_length) :: char_arr(lbound(x, dim=1):ubound(x, dim=1)) )

        do i = lbound(x, dim=1), ubound(x, dim=1)
            if ( lengths(i) < 1 ) then
                char_arr(i) = EMPTY_STR
            else
                char_arr(i) = x(i)%s
            end if
        end do

        call aprint(char_arr)
    end procedure aprint_1dString

    module procedure aprint_2dString
        character(len=:), allocatable :: char_arr(:,:)
        integer, allocatable          :: lengths(:,:)
        integer                       :: i, j, max_length

        lengths    = x%len()
        max_length = maxval(lengths)

        allocate( character(len=max_length) :: &
                  char_arr(lbound(x, dim=1):ubound(x, dim=1), lbound(x, dim=2):ubound(x, dim=2)) )

        do j = lbound(x, dim=2), ubound(x, dim=2)
            do i = lbound(x, dim=1), ubound(x, dim=1)
                if ( lengths(i,j) < 1 ) then
                    char_arr(i,j) = EMPTY_STR
                else
                    char_arr(i,j) = x(i,j)%s
                end if
            end do
        end do

        call aprint(char_arr)
    end procedure aprint_2dString
end submodule array_printing
