/* Works around GCC bug 128183 in the test programs (issue #64). The library itself is not changed.

   MinGW has no uselocale(), so libgfortran wraps every formatted I/O statement in

       old = setlocale(LC_NUMERIC, NULL);  setlocale(LC_NUMERIC, "C");  ...  setlocale(LC_NUMERIC, old);

   The Universal C Runtime frees `old` in the second call, so the restore reads freed memory, and now and then crashes
   inside setlocale. libgfortran skips both calls while its count of active formatted statements is nonzero, so this
   sets the "C" locale once, before the program starts, and holds the count at 1. fpm does the same
   (fortran-lang/fpm#1377). Other compilers and platforms compile this file to nothing. */

#if defined(__MINGW32__) && defined(__GNUC__) && !defined(__clang__)
#include <locale.h>

extern int _gfortrani_old_locale_ctr;

__attribute__((constructor)) static void iofl_pin_numeric_locale(void)
{
  setlocale(LC_NUMERIC, "C");
  _gfortrani_old_locale_ctr++;
}
#else
typedef int iofl_no_locale_workaround; /* ISO C does not allow an empty file */
#endif
