module kinds
  use, intrinsic :: iso_fortran_env, only: r128=>real128, r64=>real64, r32=>real32, &   ! ISO standard real kinds
                                           i64=>int64, i32=>int32, i16=>int16, i8=>int8 ! ISO standard int kinds
  implicit none (type, external)
  private

#ifdef R16
  integer, public, parameter :: rk = r128
#elif R8
  integer, public, parameter :: rk = r64
#else
  integer, public, parameter :: rk = r32
#endif

#ifdef I8
  integer, public, parameter :: ik = i64
#elif I2
  integer, public, parameter :: ik = i16
#elif I1
  integer, public, parameter :: ik = i8
#else
  integer, public, parameter :: ik = i32
#endif

end module kinds
