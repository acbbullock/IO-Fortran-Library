submodule (randoms) gaussian_sampling
  !---------------------------------------------------------------------------------------------------------------------
  !! This submodule provides module procedure implementations for the **public interface** `random_gauss` and the
  !! **private interface** `gauss`.
  !---------------------------------------------------------------------------------------------------------------------
  implicit none (type, external)

  contains ! Procedure bodies for module subprograms <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

  ! Gaussian sampling procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  module procedure gauss_r128
    real(r128) :: rn, x, y, z, res

    real(r128), parameter :: kC1   = 1.448242853_r128
    real(r128), parameter :: kC2   = 3.307147487_r128
    real(r128), parameter :: kC3   = 1.46754004_r128
    real(r128), parameter :: kD1   = 1.036467755_r128
    real(r128), parameter :: kD2   = 5.295844968_r128
    real(r128), parameter :: kD3   = 3.631288474_r128
    real(r128), parameter :: kHm   = 0.483941449_r128
    real(r128), parameter :: kZm   = 0.107981933_r128
    real(r128), parameter :: kHp   = 4.132731354_r128
    real(r128), parameter :: kZp   = 18.52161694_r128
    real(r128), parameter :: kPhln = 0.4515827053_r128
    real(r128), parameter :: kHm1  = 0.516058551_r128
    real(r128), parameter :: kHp1  = 3.132731354_r128
    real(r128), parameter :: kHzm  = 0.375959516_r128
    real(r128), parameter :: kHzmp = 0.591923442_r128

    real(r128), parameter :: kAs = 0.8853395638_r128
    real(r128), parameter :: kBs = 0.2452635696_r128
    real(r128), parameter :: kCs = 0.2770276848_r128
    real(r128), parameter :: kB  = 0.5029324303_r128
    real(r128), parameter :: kX0 = 0.4571828819_r128
    real(r128), parameter :: kYm = 0.187308492_r128
    real(r128), parameter :: kS  = 0.7270572718_r128
    real(r128), parameter :: kT  = 0.03895759111_r128

    rn=0e0_r128; x=0e0_r128; y=0e0_r128; z=0e0_r128; res=0e0_r128; gauss_res=0e0_r128

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
  end procedure gauss_r128
  module procedure gauss_r64
    real(r64) :: rn, x, y, z, res

    real(r64), parameter :: kC1   = 1.448242853_r64
    real(r64), parameter :: kC2   = 3.307147487_r64
    real(r64), parameter :: kC3   = 1.46754004_r64
    real(r64), parameter :: kD1   = 1.036467755_r64
    real(r64), parameter :: kD2   = 5.295844968_r64
    real(r64), parameter :: kD3   = 3.631288474_r64
    real(r64), parameter :: kHm   = 0.483941449_r64
    real(r64), parameter :: kZm   = 0.107981933_r64
    real(r64), parameter :: kHp   = 4.132731354_r64
    real(r64), parameter :: kZp   = 18.52161694_r64
    real(r64), parameter :: kPhln = 0.4515827053_r64
    real(r64), parameter :: kHm1  = 0.516058551_r64
    real(r64), parameter :: kHp1  = 3.132731354_r64
    real(r64), parameter :: kHzm  = 0.375959516_r64
    real(r64), parameter :: kHzmp = 0.591923442_r64

    real(r64), parameter :: kAs = 0.8853395638_r64
    real(r64), parameter :: kBs = 0.2452635696_r64
    real(r64), parameter :: kCs = 0.2770276848_r64
    real(r64), parameter :: kB  = 0.5029324303_r64
    real(r64), parameter :: kX0 = 0.4571828819_r64
    real(r64), parameter :: kYm = 0.187308492_r64
    real(r64), parameter :: kS  = 0.7270572718_r64
    real(r64), parameter :: kT  = 0.03895759111_r64

    rn=0e0_r64; x=0e0_r64; y=0e0_r64; z=0e0_r64; res=0e0_r64; gauss_res=0e0_r64

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
  end procedure gauss_r64
  module procedure gauss_r32
    real(r32) :: rn, x, y, z, res

    real(r32), parameter :: kC1   = 1.448242853_r32
    real(r32), parameter :: kC2   = 3.307147487_r32
    real(r32), parameter :: kC3   = 1.46754004_r32
    real(r32), parameter :: kD1   = 1.036467755_r32
    real(r32), parameter :: kD2   = 5.295844968_r32
    real(r32), parameter :: kD3   = 3.631288474_r32
    real(r32), parameter :: kHm   = 0.483941449_r32
    real(r32), parameter :: kZm   = 0.107981933_r32
    real(r32), parameter :: kHp   = 4.132731354_r32
    real(r32), parameter :: kZp   = 18.52161694_r32
    real(r32), parameter :: kPhln = 0.4515827053_r32
    real(r32), parameter :: kHm1  = 0.516058551_r32
    real(r32), parameter :: kHp1  = 3.132731354_r32
    real(r32), parameter :: kHzm  = 0.375959516_r32
    real(r32), parameter :: kHzmp = 0.591923442_r32

    real(r32), parameter :: kAs = 0.8853395638_r32
    real(r32), parameter :: kBs = 0.2452635696_r32
    real(r32), parameter :: kCs = 0.2770276848_r32
    real(r32), parameter :: kB  = 0.5029324303_r32
    real(r32), parameter :: kX0 = 0.4571828819_r32
    real(r32), parameter :: kYm = 0.187308492_r32
    real(r32), parameter :: kS  = 0.7270572718_r32
    real(r32), parameter :: kT  = 0.03895759111_r32

    rn=0e0_r32; x=0e0_r32; y=0e0_r32; z=0e0_r32; res=0e0_r32; gauss_res=0e0_r32

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
  end procedure gauss_r32

  ! Elemental procedures for gauss ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  module procedure random_gauss_r128
    x = gauss(mu, sig)
  end procedure random_gauss_r128
  module procedure random_gauss_r64
    x = gauss(mu, sig)
  end procedure random_gauss_r64
  module procedure random_gauss_r32
    x = gauss(mu, sig)
  end procedure random_gauss_r32
end submodule gaussian_sampling
