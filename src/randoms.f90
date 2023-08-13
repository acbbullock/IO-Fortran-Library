module randoms
    !-------------------------------------------------------------------------------------------------------------------
    !! This module provides Gaussian sampling routines for use in unit tests (random data generation).
    !-------------------------------------------------------------------------------------------------------------------
    use, intrinsic :: iso_fortran_env, only: r128=>real128, r64=>real64, r32=>real32
    implicit none (type, external)
    private

    ! Public API list ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    public :: random_gauss

    ! Definitions and Interfaces ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    interface random_gauss
        !---------------------------------------------------------------------------------------------------------------
        !! Applies `gauss` to whole arrays and scalars.
        !---------------------------------------------------------------------------------------------------------------
        module procedure :: random_gauss_r128, random_gauss_r64, random_gauss_r32
    end interface

    interface gauss
        !---------------------------------------------------------------------------------------------------------------
        !! Samples random numbers from the standard Normal (Gaussian) Distribution with the given mean and sigma.
        !! Uses the Acceptance-complement ratio from W. Hoermann and G. Derflinger.
        !! This is one of the fastest existing methods for generating normal random variables.
        !!
        !! REFERENCE: - W. Hoermann and G. Derflinger (1990):
        !!              The ACR Method for generating normal random variables,
        !!              OR Spektrum 12 (1990), 181-185.
        !!
        !! Implementation taken from <https://root.cern.ch/doc/master/TRandom_8cxx_source.html#l00274>
        !! UNURAN (c) 2000  W. Hoermann & J. Leydold, Institut f. Statistik, WU Wien
        !---------------------------------------------------------------------------------------------------------------
        module procedure :: gauss_r128, gauss_r64, gauss_r32
    end interface

    contains

    impure elemental subroutine random_gauss_r128(x, mu, sig)
        real(r128), intent(inout) :: x
        real(r128), intent(in)    :: mu, sig
        x = gauss(mu, sig)
    end subroutine random_gauss_r128
    impure elemental subroutine random_gauss_r64(x, mu, sig)
        real(r64), intent(inout) :: x
        real(r64), intent(in)    :: mu, sig
        x = gauss(mu, sig)
    end subroutine random_gauss_r64
    impure elemental subroutine random_gauss_r32(x, mu, sig)
        real(r32), intent(inout) :: x
        real(r32), intent(in)    :: mu, sig
        x = gauss(mu, sig)
    end subroutine random_gauss_r32

    impure real(r128) function gauss_r128(mu, sig) result(gauss_res)
        real(r128), intent(in) :: mu, sig

        real(r128) :: kC1, kC2, kC3, kD1, kD2, kD3, kHm, kZm, kHp, kZp, kPhln, kHm1
        real(r128) :: kHp1, kHzm, kHzmp, kAs, kBs, kCs, kB, kX0, kYm, kS, kT
        real(r128) :: rn, x, y, z, res

        kC1   = 1.448242853_r128
        kC2   = 3.307147487_r128
        kC3   = 1.46754004_r128
        kD1   = 1.036467755_r128
        kD2   = 5.295844968_r128
        kD3   = 3.631288474_r128
        kHm   = 0.483941449_r128
        kZm   = 0.107981933_r128
        kHp   = 4.132731354_r128
        kZp   = 18.52161694_r128
        kPhln = 0.4515827053_r128
        kHm1  = 0.516058551_r128
        kHp1  = 3.132731354_r128
        kHzm  = 0.375959516_r128
        kHzmp = 0.591923442_r128

        kAs = 0.8853395638_r128
        kBs = 0.2452635696_r128
        kCs = 0.2770276848_r128
        kB  = 0.5029324303_r128
        kX0 = 0.4571828819_r128
        kYm = 0.187308492_r128
        kS  = 0.7270572718_r128
        kT  = 0.03895759111_r128

        outer: do
            call random_number(y)

            if ( y > kHm1 ) then
                res = kHp*y - kHp1; exit outer
            else if ( y < kZm ) then
                rn = kZp*y - 1.0_r128

                if ( rn > 0.0_r128 ) then
                    res = 1.0_r128 + rn; exit outer
                else
                    res = -1.0_r128 + rn; exit outer
                end if
            else if ( y < kHm ) then
                call random_number(rn)
                rn = rn - 1.0_r128 + rn

                if ( rn > 0.0_r128 ) then
                    z = 2.0_r128 - rn
                else
                    z = -2.0_r128 - rn
                end if

                if ( (kC1-y)*(kC3+abs(z)) < kC2 ) then
                    res = z; exit outer
                else
                    x = rn*rn
                    if ( (y+kD1)*(kD3+x) < kD2 ) then
                        res = rn; exit outer
                    else if ( kHzmp-y < exp(-(z*z+kPhln)/2.0_r128) ) then
                        res = z; exit outer
                    else if ( y+kHzm < exp(-(x+kPhln)/2.0_r128) ) then
                        res = rn; exit outer
                    end if
                end if
            end if

            inner: do
                call random_number(x); call random_number(y)
                y = kYm*y
                z = kX0 - kS*x - y

                if ( z > 0.0_r128 ) then
                    rn = 2.0_r128 + y/x
                else
                    x = 1.0_r128 - x
                    y = kYm - y
                    rn = -( 2.0_r128 + y/x )
                end if

                if ( (y-kAs+x)*(kCs+x)+kBs < 0.0_r128 ) then
                    res = rn; exit inner
                else if ( y < x+kT ) then
                    if ( rn*rn < 4.0_r128*(kB-log(x)) ) then
                        res = rn; exit inner
                    end if
                end if
            end do inner

            exit outer
        end do outer

        gauss_res = res*sig + mu
    end function gauss_r128
    impure real(r64) function gauss_r64(mu, sig) result(gauss_res)
        real(r64), intent(in) :: mu, sig

        real(r64) :: kC1, kC2, kC3, kD1, kD2, kD3, kHm, kZm, kHp, kZp, kPhln, kHm1
        real(r64) :: kHp1, kHzm, kHzmp, kAs, kBs, kCs, kB, kX0, kYm, kS, kT
        real(r64) :: rn, x, y, z, res

        kC1   = 1.448242853_r64
        kC2   = 3.307147487_r64
        kC3   = 1.46754004_r64
        kD1   = 1.036467755_r64
        kD2   = 5.295844968_r64
        kD3   = 3.631288474_r64
        kHm   = 0.483941449_r64
        kZm   = 0.107981933_r64
        kHp   = 4.132731354_r64
        kZp   = 18.52161694_r64
        kPhln = 0.4515827053_r64
        kHm1  = 0.516058551_r64
        kHp1  = 3.132731354_r64
        kHzm  = 0.375959516_r64
        kHzmp = 0.591923442_r64

        kAs = 0.8853395638_r64
        kBs = 0.2452635696_r64
        kCs = 0.2770276848_r64
        kB  = 0.5029324303_r64
        kX0 = 0.4571828819_r64
        kYm = 0.187308492_r64
        kS  = 0.7270572718_r64
        kT  = 0.03895759111_r64

        outer: do
            call random_number(y)

            if ( y > kHm1 ) then
                res = kHp*y - kHp1; exit outer
            else if ( y < kZm ) then
                rn = kZp*y - 1.0_r64

                if ( rn > 0.0_r64 ) then
                    res = 1.0_r64 + rn; exit outer
                else
                    res = -1.0_r64 + rn; exit outer
                end if
            else if ( y < kHm ) then
                call random_number(rn)
                rn = rn - 1.0_r64 + rn

                if ( rn > 0.0_r64 ) then
                    z = 2.0_r64 - rn
                else
                    z = -2.0_r64 - rn
                end if

                if ( (kC1-y)*(kC3+abs(z)) < kC2 ) then
                    res = z; exit outer
                else
                    x = rn*rn
                    if ( (y+kD1)*(kD3+x) < kD2 ) then
                        res = rn; exit outer
                    else if ( kHzmp-y < exp(-(z*z+kPhln)/2.0_r64) ) then
                        res = z; exit outer
                    else if ( y+kHzm < exp(-(x+kPhln)/2.0_r64) ) then
                        res = rn; exit outer
                    end if
                end if
            end if

            inner: do
                call random_number(x); call random_number(y)
                y = kYm*y
                z = kX0 - kS*x - y

                if ( z > 0.0_r64 ) then
                    rn = 2.0_r64 + y/x
                else
                    x = 1.0_r64 - x
                    y = kYm - y
                    rn = -( 2.0_r64 + y/x )
                end if

                if ( (y-kAs+x)*(kCs+x)+kBs < 0.0_r64 ) then
                    res = rn; exit inner
                else if ( y < x+kT ) then
                    if ( rn*rn < 4.0_r64*(kB-log(x)) ) then
                        res = rn; exit inner
                    end if
                end if
            end do inner

            exit outer
        end do outer

        gauss_res = res*sig + mu
    end function gauss_r64
    impure real(r32) function gauss_r32(mu, sig) result(gauss_res)
        real(r32), intent(in) :: mu, sig

        real(r32) :: kC1, kC2, kC3, kD1, kD2, kD3, kHm, kZm, kHp, kZp, kPhln, kHm1
        real(r32) :: kHp1, kHzm, kHzmp, kAs, kBs, kCs, kB, kX0, kYm, kS, kT
        real(r32) :: rn, x, y, z, res

        kC1   = 1.448242853_r32
        kC2   = 3.307147487_r32
        kC3   = 1.46754004_r32
        kD1   = 1.036467755_r32
        kD2   = 5.295844968_r32
        kD3   = 3.631288474_r32
        kHm   = 0.483941449_r32
        kZm   = 0.107981933_r32
        kHp   = 4.132731354_r32
        kZp   = 18.52161694_r32
        kPhln = 0.4515827053_r32
        kHm1  = 0.516058551_r32
        kHp1  = 3.132731354_r32
        kHzm  = 0.375959516_r32
        kHzmp = 0.591923442_r32

        kAs = 0.8853395638_r32
        kBs = 0.2452635696_r32
        kCs = 0.2770276848_r32
        kB  = 0.5029324303_r32
        kX0 = 0.4571828819_r32
        kYm = 0.187308492_r32
        kS  = 0.7270572718_r32
        kT  = 0.03895759111_r32

        outer: do
            call random_number(y)

            if ( y > kHm1 ) then
                res = kHp*y - kHp1; exit outer
            else if ( y < kZm ) then
                rn = kZp*y - 1.0_r32

                if ( rn > 0.0_r32 ) then
                    res = 1.0_r32 + rn; exit outer
                else
                    res = -1.0_r32 + rn; exit outer
                end if
            else if ( y < kHm ) then
                call random_number(rn)
                rn = rn - 1.0_r32 + rn

                if ( rn > 0.0_r32 ) then
                    z = 2.0_r32 - rn
                else
                    z = -2.0_r32 - rn
                end if

                if ( (kC1-y)*(kC3+abs(z)) < kC2 ) then
                    res = z; exit outer
                else
                    x = rn*rn
                    if ( (y+kD1)*(kD3+x) < kD2 ) then
                        res = rn; exit outer
                    else if ( kHzmp-y < exp(-(z*z+kPhln)/2.0_r32) ) then
                        res = z; exit outer
                    else if ( y+kHzm < exp(-(x+kPhln)/2.0_r32) ) then
                        res = rn; exit outer
                    end if
                end if
            end if

            inner: do
                call random_number(x); call random_number(y)
                y = kYm*y
                z = kX0 - kS*x - y

                if ( z > 0.0_r32 ) then
                    rn = 2.0_r32 + y/x
                else
                    x = 1.0_r32 - x
                    y = kYm - y
                    rn = -( 2.0_r32 + y/x )
                end if

                if ( (y-kAs+x)*(kCs+x)+kBs < 0.0_r32 ) then
                    res = rn; exit inner
                else if ( y < x+kT ) then
                    if ( rn*rn < 4.0_r32*(kB-log(x)) ) then
                        res = rn; exit inner
                    end if
                end if
            end do inner

            exit outer
        end do outer

        gauss_res = res*sig + mu
    end function gauss_r32

end module randoms
