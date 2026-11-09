# Security policy

## Supported versions

Fixes are made to the latest release. Version 2.1 is supported; 2.0 and 1.x are not, since 2.1 is compatible
with 2.0 and replaces it.

## Reporting a vulnerability

The library reads files and text, so a crash, out-of-bounds access or unbounded allocation caused by malformed
input is worth reporting. Please report it privately, through
[GitHub's private vulnerability reporting](https://github.com/acbbullock/IO-Fortran-Library/security/advisories/new),
rather than in a public issue. Include a small program and the input that shows the problem. You should hear back
within a week, and a fix will be released and credited to you unless you'd rather not.

Bugs in a compiler or its runtime library, such as the `gfortran` locale bug on Windows described in the
[compiler notes](https://acbbullock.github.io/IO-Fortran-Library/doc/page/UserInfo/compilers.html), belong with
the compiler's maintainers, but a note here is welcome so the documentation can describe a workaround.
