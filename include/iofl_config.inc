! Build configuration for IO-Fortran-Library, processed by the C preprocessor.
!
! IOFL_HAS_R128 selects the real128 and complex(real128) procedures. It is detected from compiler macros because the
! preprocessor cannot read real128 from iso_fortran_env. It stays off unless a macro confirms a usable quad kind, since
! the predefined macros are absent on flang when fpm builds without -cpp, and flang still reads these directives.
! To override the detection, compile with -DIOFL_HAS_R128 or -DIOFL_NO_R128.
#if !defined(IOFL_HAS_R128) && !defined(IOFL_NO_R128)
#  if defined(__GFC_REAL_16__) || defined(__INTEL_COMPILER) || defined(__INTEL_LLVM_COMPILER)
#    define IOFL_HAS_R128
#  endif
#endif
