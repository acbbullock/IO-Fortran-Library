module random_data
  !---------------------------------------------------------------------------------------------------------------------
  !! Normally distributed test data of the kind under test, from the Marsaglia polar method.
  !---------------------------------------------------------------------------------------------------------------------
  use kinds, only: rk
  implicit none (type, external)
  private

  public :: random_gauss

contains

  impure elemental subroutine random_gauss(x, mu, sig)
    real(rk), intent(inout) :: x
    real(rk), intent(in)    :: mu, sig

    real(rk) :: u, v, s

    do
      call random_number(u)
      call random_number(v)

      u = 2.0_rk*u - 1.0_rk
      v = 2.0_rk*v - 1.0_rk
      s = u*u + v*v

      if ( (s > 0.0_rk) .and. (s < 1.0_rk) ) exit
    end do

    x = mu + sig*u*sqrt(-2.0_rk*log(s)/s)
  end subroutine random_gauss
end module random_data
