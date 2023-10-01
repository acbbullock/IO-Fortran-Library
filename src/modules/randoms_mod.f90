module randoms
  !---------------------------------------------------------------------------------------------------------------------
  !! This module provides Gaussian sampling utility routines for use in unit testing.
  !---------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: r128=>real128, r64=>real64, r32=>real32
  implicit none (type, external)
  private

  ! Public API list ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  public :: random_gauss

  ! Definitions and Interfaces ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  interface gauss                                                                          ! Submodule gaussian_sampling
    !-------------------------------------------------------------------------------------------------------------------
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
    impure real(r128) module function gauss_r128(mu, sig) result(gauss_res)
      real(r128), intent(in) :: mu, sig
    end function gauss_r128
    impure real(r64) module function gauss_r64(mu, sig) result(gauss_res)
      real(r64), intent(in) :: mu, sig
    end function gauss_r64
    impure real(r32) module function gauss_r32(mu, sig) result(gauss_res)
      real(r32), intent(in) :: mu, sig
    end function gauss_r32
  end interface

  interface random_gauss                                                                   ! Submodule gaussian_sampling
    !-------------------------------------------------------------------------------------------------------------------
    !! Applies `gauss` to whole arrays and scalars.
    !-------------------------------------------------------------------------------------------------------------------
    impure elemental module subroutine random_gauss_r128(x, mu, sig)
      real(r128), intent(inout) :: x
      real(r128), intent(in)    :: mu, sig
    end subroutine random_gauss_r128
    impure elemental module subroutine random_gauss_r64(x, mu, sig)
      real(r64), intent(inout) :: x
      real(r64), intent(in)    :: mu, sig
    end subroutine random_gauss_r64
    impure elemental module subroutine random_gauss_r32(x, mu, sig)
      real(r32), intent(inout) :: x
      real(r32), intent(in)    :: mu, sig
    end subroutine random_gauss_r32
  end interface
end module randoms
