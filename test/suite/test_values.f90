! Generated from fypp/test/suite/test_values.fypp by fypp/generate.sh. Edit that file, not this one.
#include "iofl_config.inc"
module test_values
  !---------------------------------------------------------------------------------------------------------------------
  !! Test data for every kind: edge values, and random values from a small portable generator, so that a case sees the
  !! same values on every compiler. Call `reset_random` at the start of a case to make it independent of the others.
  !!
  !! Random reals cover the whole exponent range of their kind, subnormals included, with both signs. Every fourth one
  !! is of ordinary size, between about 1e-3 and 2e3. Random integers cover the whole range of their kind, and every
  !! fourth one is between -1000 and 1000 (-128 and 127 for `int8`).
  !---------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: r128=>real128, r64=>real64, r32=>real32, &
                                           i64=>int64, i32=>int32, i16=>int16, i8=>int8
  use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_positive_inf, ieee_negative_inf, &
                                           ieee_copy_sign
  implicit none
  private

  public :: reset_random, random_values, edge_values, test_values_of

  integer(i64), parameter :: MODULUS = 2147483647_i64, MULTIPLIER = 48271_i64     ! Park-Miller "minimal standard"
  integer(i64), parameter :: DEFAULT_SEED = 20221_i64

  integer(i64) :: state = DEFAULT_SEED

  interface random_values
    !! Fills an array with random values of its kind.
    module procedure :: random_i64
    module procedure :: random_i32
    module procedure :: random_i16
    module procedure :: random_i8
#ifdef IOFL_HAS_R128
    module procedure :: random_r128
#endif
    module procedure :: random_r64
    module procedure :: random_r32
#ifdef IOFL_HAS_R128
    module procedure :: random_c128
#endif
    module procedure :: random_c64
    module procedure :: random_c32
  end interface random_values

  interface edge_values
    !! Returns the edge values of the kind of `mold`.
    module procedure :: edges_i64
    module procedure :: edges_i32
    module procedure :: edges_i16
    module procedure :: edges_i8
#ifdef IOFL_HAS_R128
    module procedure :: edges_r128
#endif
    module procedure :: edges_r64
    module procedure :: edges_r32
#ifdef IOFL_HAS_R128
    module procedure :: edges_c128
#endif
    module procedure :: edges_c64
    module procedure :: edges_c32
  end interface edge_values

  interface test_values_of
    !! Returns the edge values of the kind of `mold` followed by `n` random values.
    module procedure :: values_i64
    module procedure :: values_i32
    module procedure :: values_i16
    module procedure :: values_i8
#ifdef IOFL_HAS_R128
    module procedure :: values_r128
#endif
    module procedure :: values_r64
    module procedure :: values_r32
#ifdef IOFL_HAS_R128
    module procedure :: values_c128
#endif
    module procedure :: values_c64
    module procedure :: values_c32
  end interface test_values_of

contains

  subroutine reset_random(seed)
    !! Restarts the generator, from `seed` if present.
    integer, intent(in), optional :: seed

    state = DEFAULT_SEED
    if ( present(seed) ) state = mod(abs(int(seed, i64)), MODULUS - 1_i64) + 1_i64
  end subroutine reset_random

  integer(i64) function next_bits(n) result(bits)
    !! Returns `n` random bits, at most 30, as a nonnegative integer.
    integer, intent(in) :: n

    state = mod(MULTIPLIER*state, MODULUS)                                     ! Never overflows: both are below 2**31
    bits = ibits(state, 0, n)
  end function next_bits

  integer(i64) function random_bit_pattern(n) result(bits)
    !! Returns `n` random bits, at most 64, filling `bits` from the lowest bit up.
    integer, intent(in) :: n

    integer :: done, chunk

    bits = 0_i64; done = 0
    do while ( done < n )
      chunk = min(16, n - done)
      bits = ior(bits, shiftl(next_bits(chunk), done))
      done = done + chunk
    end do
  end function random_bit_pattern

  subroutine random_i64(x)
    integer(i64), intent(out) :: x(:)

    integer, parameter :: BITS = bit_size(0_i64)
    integer            :: i

    do i = 1, size(x)
      if ( mod(i, 4) == 0 ) then
        x(i) = int(mod(next_bits(30), 2001_i64) - 1000_i64, i64)
      else
        x(i) = random_bit_pattern(BITS)
      end if
    end do
  end subroutine random_i64

  function edges_i64(mold) result(x)
    integer(i64), intent(in)     :: mold
    integer(i64), allocatable    :: x(:)

    x = [ 0_i64, 1_i64, -1_i64, 2_i64, -2_i64, 10_i64, -10_i64, huge(mold), huge(mold) - 1_i64, -huge(mold), &
          -huge(mold) - 1_i64 ]
  end function edges_i64

  function values_i64(mold, n) result(x)
    integer(i64), intent(in)     :: mold
    integer,      intent(in)     :: n
    integer(i64), allocatable    :: x(:), random(:)

    allocate( random(n) )
    call random_values(random)
    x = [ edges_i64(mold), random ]
  end function values_i64

  subroutine random_i32(x)
    integer(i32), intent(out) :: x(:)

    integer, parameter :: BITS = bit_size(0_i32)
    integer            :: i

    do i = 1, size(x)
      if ( mod(i, 4) == 0 ) then
        x(i) = int(mod(next_bits(30), 2001_i64) - 1000_i64, i32)
      else
        x(i) = int(random_bit_pattern(BITS) - 2_i64**(BITS-1), i32)
      end if
    end do
  end subroutine random_i32

  function edges_i32(mold) result(x)
    integer(i32), intent(in)     :: mold
    integer(i32), allocatable    :: x(:)

    x = [ 0_i32, 1_i32, -1_i32, 2_i32, -2_i32, 10_i32, -10_i32, huge(mold), huge(mold) - 1_i32, -huge(mold), &
          -huge(mold) - 1_i32 ]
  end function edges_i32

  function values_i32(mold, n) result(x)
    integer(i32), intent(in)     :: mold
    integer,      intent(in)     :: n
    integer(i32), allocatable    :: x(:), random(:)

    allocate( random(n) )
    call random_values(random)
    x = [ edges_i32(mold), random ]
  end function values_i32

  subroutine random_i16(x)
    integer(i16), intent(out) :: x(:)

    integer, parameter :: BITS = bit_size(0_i16)
    integer            :: i

    do i = 1, size(x)
      if ( mod(i, 4) == 0 ) then
        x(i) = int(mod(next_bits(30), 2001_i64) - 1000_i64, i16)
      else
        x(i) = int(random_bit_pattern(BITS) - 2_i64**(BITS-1), i16)
      end if
    end do
  end subroutine random_i16

  function edges_i16(mold) result(x)
    integer(i16), intent(in)     :: mold
    integer(i16), allocatable    :: x(:)

    x = [ 0_i16, 1_i16, -1_i16, 2_i16, -2_i16, 10_i16, -10_i16, huge(mold), huge(mold) - 1_i16, -huge(mold), &
          -huge(mold) - 1_i16 ]
  end function edges_i16

  function values_i16(mold, n) result(x)
    integer(i16), intent(in)     :: mold
    integer,      intent(in)     :: n
    integer(i16), allocatable    :: x(:), random(:)

    allocate( random(n) )
    call random_values(random)
    x = [ edges_i16(mold), random ]
  end function values_i16

  subroutine random_i8(x)
    integer(i8), intent(out) :: x(:)

    integer, parameter :: BITS = bit_size(0_i8)
    integer            :: i

    do i = 1, size(x)
      if ( mod(i, 4) == 0 ) then
        x(i) = int(next_bits(8) - 128_i64, i8)
      else
        x(i) = int(random_bit_pattern(BITS) - 2_i64**(BITS-1), i8)
      end if
    end do
  end subroutine random_i8

  function edges_i8(mold) result(x)
    integer(i8), intent(in)     :: mold
    integer(i8), allocatable    :: x(:)

    x = [ 0_i8, 1_i8, -1_i8, 2_i8, -2_i8, 10_i8, -10_i8, huge(mold), huge(mold) - 1_i8, -huge(mold), &
          -huge(mold) - 1_i8 ]
  end function edges_i8

  function values_i8(mold, n) result(x)
    integer(i8), intent(in)     :: mold
    integer,      intent(in)     :: n
    integer(i8), allocatable    :: x(:), random(:)

    allocate( random(n) )
    call random_values(random)
    x = [ edges_i8(mold), random ]
  end function values_i8

#ifdef IOFL_HAS_R128
  subroutine random_r128(x)
    real(r128), intent(out) :: x(:)

    integer, parameter :: LOW  = minexponent(1.0_r128) - digits(1.0_r128)       ! Down to the smallest subnormal
    integer, parameter :: HIGH = maxexponent(1.0_r128) - 1
    integer, parameter :: DRAWS = (digits(1.0_r128) + 29)/30

    real(r128) :: m
    integer   :: i, j, e

    do i = 1, size(x)
      m = 0.0_r128
      do j = 1, DRAWS
        m = (m + real(next_bits(30), r128))/2.0_r128**30
      end do
      m = 1.0_r128 + m
      if ( m >= 2.0_r128 ) m = 1.0_r128                                          ! Rounded up to 2: keep in [1,2)

      if ( mod(i, 4) == 0 ) then
        e = int(mod(next_bits(30), 21_i64)) - 10
      else
        e = LOW + int(mod(random_bit_pattern(30), int(HIGH - LOW + 1, i64)))
      end if

      x(i) = scale(m, e)
      if ( next_bits(1) == 1_i64 ) x(i) = -x(i)
    end do
  end subroutine random_r128

  function edges_r128(mold) result(x)
    real(r128), intent(in)     :: mold
    real(r128), allocatable    :: x(:)

    x = [ 0.0_r128, ieee_copy_sign(0.0_r128, -1.0_r128), 1.0_r128, -1.0_r128, 0.1_r128, -2.5_r128, 1.0_r128/3.0_r128, &
          huge(mold), -huge(mold), tiny(mold), -tiny(mold), nearest(0.0_r128, 1.0_r128), nearest(0.0_r128, -1.0_r128), &
          nearest(tiny(mold), -1.0_r128), epsilon(mold), nearest(1.0_r128, 2.0_r128), &
          ieee_value(mold, ieee_quiet_nan), ieee_value(mold, ieee_positive_inf), ieee_value(mold, ieee_negative_inf) ]
  end function edges_r128

  function values_r128(mold, n) result(x)
    real(r128), intent(in)     :: mold
    integer,   intent(in)     :: n
    real(r128), allocatable    :: x(:), random(:)

    allocate( random(n) )
    call random_values(random)
    x = [ edges_r128(mold), random ]
  end function values_r128
#endif

  subroutine random_r64(x)
    real(r64), intent(out) :: x(:)

    integer, parameter :: LOW  = minexponent(1.0_r64) - digits(1.0_r64)       ! Down to the smallest subnormal
    integer, parameter :: HIGH = maxexponent(1.0_r64) - 1
    integer, parameter :: DRAWS = (digits(1.0_r64) + 29)/30

    real(r64) :: m
    integer   :: i, j, e

    do i = 1, size(x)
      m = 0.0_r64
      do j = 1, DRAWS
        m = (m + real(next_bits(30), r64))/2.0_r64**30
      end do
      m = 1.0_r64 + m
      if ( m >= 2.0_r64 ) m = 1.0_r64                                          ! Rounded up to 2: keep in [1,2)

      if ( mod(i, 4) == 0 ) then
        e = int(mod(next_bits(30), 21_i64)) - 10
      else
        e = LOW + int(mod(random_bit_pattern(30), int(HIGH - LOW + 1, i64)))
      end if

      x(i) = scale(m, e)
      if ( next_bits(1) == 1_i64 ) x(i) = -x(i)
    end do
  end subroutine random_r64

  function edges_r64(mold) result(x)
    real(r64), intent(in)     :: mold
    real(r64), allocatable    :: x(:)

    x = [ 0.0_r64, ieee_copy_sign(0.0_r64, -1.0_r64), 1.0_r64, -1.0_r64, 0.1_r64, -2.5_r64, 1.0_r64/3.0_r64, &
          huge(mold), -huge(mold), tiny(mold), -tiny(mold), nearest(0.0_r64, 1.0_r64), nearest(0.0_r64, -1.0_r64), &
          nearest(tiny(mold), -1.0_r64), epsilon(mold), nearest(1.0_r64, 2.0_r64), &
          ieee_value(mold, ieee_quiet_nan), ieee_value(mold, ieee_positive_inf), ieee_value(mold, ieee_negative_inf) ]
  end function edges_r64

  function values_r64(mold, n) result(x)
    real(r64), intent(in)     :: mold
    integer,   intent(in)     :: n
    real(r64), allocatable    :: x(:), random(:)

    allocate( random(n) )
    call random_values(random)
    x = [ edges_r64(mold), random ]
  end function values_r64

  subroutine random_r32(x)
    real(r32), intent(out) :: x(:)

    integer, parameter :: LOW  = minexponent(1.0_r32) - digits(1.0_r32)       ! Down to the smallest subnormal
    integer, parameter :: HIGH = maxexponent(1.0_r32) - 1
    integer, parameter :: DRAWS = (digits(1.0_r32) + 29)/30

    real(r32) :: m
    integer   :: i, j, e

    do i = 1, size(x)
      m = 0.0_r32
      do j = 1, DRAWS
        m = (m + real(next_bits(30), r32))/2.0_r32**30
      end do
      m = 1.0_r32 + m
      if ( m >= 2.0_r32 ) m = 1.0_r32                                          ! Rounded up to 2: keep in [1,2)

      if ( mod(i, 4) == 0 ) then
        e = int(mod(next_bits(30), 21_i64)) - 10
      else
        e = LOW + int(mod(random_bit_pattern(30), int(HIGH - LOW + 1, i64)))
      end if

      x(i) = scale(m, e)
      if ( next_bits(1) == 1_i64 ) x(i) = -x(i)
    end do
  end subroutine random_r32

  function edges_r32(mold) result(x)
    real(r32), intent(in)     :: mold
    real(r32), allocatable    :: x(:)

    x = [ 0.0_r32, ieee_copy_sign(0.0_r32, -1.0_r32), 1.0_r32, -1.0_r32, 0.1_r32, -2.5_r32, 1.0_r32/3.0_r32, &
          huge(mold), -huge(mold), tiny(mold), -tiny(mold), nearest(0.0_r32, 1.0_r32), nearest(0.0_r32, -1.0_r32), &
          nearest(tiny(mold), -1.0_r32), epsilon(mold), nearest(1.0_r32, 2.0_r32), &
          ieee_value(mold, ieee_quiet_nan), ieee_value(mold, ieee_positive_inf), ieee_value(mold, ieee_negative_inf) ]
  end function edges_r32

  function values_r32(mold, n) result(x)
    real(r32), intent(in)     :: mold
    integer,   intent(in)     :: n
    real(r32), allocatable    :: x(:), random(:)

    allocate( random(n) )
    call random_values(random)
    x = [ edges_r32(mold), random ]
  end function values_r32

#ifdef IOFL_HAS_R128
  subroutine random_c128(z)
    complex(r128), intent(out) :: z(:)

    real(r128), allocatable :: re(:), im(:)

    allocate( re(size(z)), im(size(z)) )
    call random_values(re)
    call random_values(im)
    z = cmplx(re, im, kind=r128)
  end subroutine random_c128

  function edges_c128(mold) result(z)
    !! Every pair of real edge values.
    complex(r128), intent(in)     :: mold
    complex(r128), allocatable    :: z(:)

    real(r128), allocatable :: parts(:)
    integer                :: i, j, n

    parts = edges_r128(real(mold))
    n = size(parts)

    allocate( z(n*n) )
    do j = 1, n
      do i = 1, n
        z(i + (j-1)*n) = cmplx(parts(i), parts(j), kind=r128)
      end do
    end do
  end function edges_c128

  function values_c128(mold, n) result(z)
    complex(r128), intent(in)     :: mold
    integer,      intent(in)     :: n
    complex(r128), allocatable    :: z(:), random(:)

    allocate( random(n) )
    call random_values(random)
    z = [ edges_c128(mold), random ]
  end function values_c128
#endif

  subroutine random_c64(z)
    complex(r64), intent(out) :: z(:)

    real(r64), allocatable :: re(:), im(:)

    allocate( re(size(z)), im(size(z)) )
    call random_values(re)
    call random_values(im)
    z = cmplx(re, im, kind=r64)
  end subroutine random_c64

  function edges_c64(mold) result(z)
    !! Every pair of real edge values.
    complex(r64), intent(in)     :: mold
    complex(r64), allocatable    :: z(:)

    real(r64), allocatable :: parts(:)
    integer                :: i, j, n

    parts = edges_r64(real(mold))
    n = size(parts)

    allocate( z(n*n) )
    do j = 1, n
      do i = 1, n
        z(i + (j-1)*n) = cmplx(parts(i), parts(j), kind=r64)
      end do
    end do
  end function edges_c64

  function values_c64(mold, n) result(z)
    complex(r64), intent(in)     :: mold
    integer,      intent(in)     :: n
    complex(r64), allocatable    :: z(:), random(:)

    allocate( random(n) )
    call random_values(random)
    z = [ edges_c64(mold), random ]
  end function values_c64

  subroutine random_c32(z)
    complex(r32), intent(out) :: z(:)

    real(r32), allocatable :: re(:), im(:)

    allocate( re(size(z)), im(size(z)) )
    call random_values(re)
    call random_values(im)
    z = cmplx(re, im, kind=r32)
  end subroutine random_c32

  function edges_c32(mold) result(z)
    !! Every pair of real edge values.
    complex(r32), intent(in)     :: mold
    complex(r32), allocatable    :: z(:)

    real(r32), allocatable :: parts(:)
    integer                :: i, j, n

    parts = edges_r32(real(mold))
    n = size(parts)

    allocate( z(n*n) )
    do j = 1, n
      do i = 1, n
        z(i + (j-1)*n) = cmplx(parts(i), parts(j), kind=r32)
      end do
    end do
  end function edges_c32

  function values_c32(mold, n) result(z)
    complex(r32), intent(in)     :: mold
    integer,      intent(in)     :: n
    complex(r32), allocatable    :: z(:), random(:)

    allocate( random(n) )
    call random_values(random)
    z = [ edges_c32(mold), random ]
  end function values_c32

end module test_values
