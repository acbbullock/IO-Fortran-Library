program compiler_check
  use, intrinsic :: iso_fortran_env, only: compiler_version
  use io_fortran_lib, only: String
  implicit none

  type(String) :: compiler
  logical :: GCC, GCC_SUPPORTED, INTEL, INTEL_SUPPORTED

  compiler = String(compiler_version())

  GCC = compiler%count(match="GCC") > 0
  GCC_SUPPORTED = GCC .and. ( compiler%count(match="16.") > 0 )

  INTEL = compiler%count(match="Intel") > 0
  INTEL_SUPPORTED = INTEL .and. ( compiler%count(match="2026.") > 0 )

  write(*,*) compiler

  if ( GCC ) then
    if ( GCC_SUPPORTED ) then
      write(*,*) "This GNU Fortran Compiler version is known to be supported."
    else
      write(*,*) "This GNU Fortran Compiler version may not be supported."
    end if
  else if ( INTEL ) then
    if ( INTEL_SUPPORTED ) then
      write(*,*) "This Intel Fortran Compiler version is known to be supported."
    else
      write(*,*) "This Intel Fortran Compiler version may not be supported."
    end if
  end if
end program compiler_check
