---
title: 'IO-Fortran-Library: Text, CSV, binary and NumPy file input and output for modern Fortran'
tags:
  - Fortran
  - file I/O
  - CSV
  - NumPy
  - scientific computing
authors:
  - name: Austin Chase Bullock
    orcid: 0009-0009-5129-5260
    affiliation: 1
affiliations:
  - name: Los Alamos National Laboratory, Los Alamos, New Mexico, United States
    index: 1
    ror: 01e41cf67
date: 9 November 2026
bibliography: paper.bib
---

<!-- DRAFT. Items marked TODO(author) need the author's facts or confirmation before submission. -->

# Summary

Fortran remains a primary language for numerical simulation [@kedward2022], but moving data between a Fortran
program and the rest of a research workflow is still laborious. The language's built-in input and output work with
records, units and edit descriptors. They have no notion of a CSV file [@rfc4180] with a header row, of quoted cells, or of the
NumPy `.npy` format that Python's analysis tools read and write [@harris2020].

IO-Fortran-Library reads and writes these files from Fortran with one call each [@iofl]:
- `to_file` writes an array to a delimited text file (CSV, TSV or any other delimiter), a raw binary file, a NumPy
  `.npy` file, or a named member of a `.npz` archive. It chooses the format from the file's extension.
- `from_file` reads the array back and allocates it to the shape the file holds.

Around these two procedures, the library converts between numbers and text, provides a `String` type with the usual
string operations, and prints arrays as tables. It is written in standard Fortran 2018 with no dependencies. It
accepts every intrinsic numeric kind and arrays of rank 1 to 15. It is distributed under the MIT license through the
Fortran Package Manager [@curcic2021], or as plain source files.

# Statement of need

Researchers who write simulations in Fortran usually analyse and plot the results in Python, R or a spreadsheet.
They also read parameters, meshes and measured data that come from those tools. Each exchange needs code that is easy
to get subtly wrong:
- format strings with enough digits for a `real64` value to read back exactly;
- loops that read a CSV file whose number of rows isn't known in advance;
- headers, empty cells, Windows line endings and a decimal comma;
- checks that a malformed value is reported rather than read as zero.

Writing a NumPy file by hand needs the header layout, byte order and alignment of the `.npy` format [@nep1]. A `.npz`
archive adds the ZIP container and its CRC-32 checksums [@appnote].

IO-Fortran-Library gives researchers this functionality through a small, uniform interface:
- **Exact round trips.** Every value written in the default scientific form, or in hexadecimal, reads back bit for
  bit.
- **NumPy compatibility.** NumPy's `load`, `save` and `savez` read and write the library's `.npy` and `.npz` files
  unchanged.
- **Errors that are reported.** An error such as a non-numeric cell gives its row and column, instead of passing
  silently.

The audience is researchers and students who write Fortran and need their data to reach other tools. It also
includes maintainers of existing Fortran codes who want to replace hand-written input and output with tested
routines.

# State of the field

Fortran's own input and output [@iso2018] can produce any of these files, but only with the per-format code described
above. Several libraries cover parts of the problem:
- **The Fortran Standard Library** [@stdlib; @kedward2022] provides `loadtxt` and `savetxt` for rank-2 arrays in text
  files, `load_npy` and `save_npy` for `.npy` files, and a `string_type`. Its file modules are marked experimental.
- **csv-fortran** [@csvfortran] reads and writes CSV files through an object-oriented `csv_file` class.
- **NPY-for-Fortran** [@npyforfortran] writes `.npy` and `.npz` files.
- **M_strings** [@mstrings] offers an extensive set of procedures for `character` strings.
- **Self-describing formats** such as NetCDF [@rew1990] and HDF5 [@hdf5] suit large, structured datasets. They bring
  compiled dependencies, though, and aren't plain-text interchange formats.

IO-Fortran-Library differs by putting text, binary, `.npy` and `.npz` files behind two generic procedures with one
error-handling convention, for every intrinsic kind and every rank the language allows.

The library was written because no standard library offered this when development began in December 2022. stdlib's
latest release then, version 0.2.1, could read and write only rank-2 arrays separated by spaces, with no header and no
choice of delimiter, so it couldn't handle CSV files. A delimiter argument arrived in version 0.8.0, in 2026, and stdlib
doesn't read or write `.npz` archives. Today the library remains a single, dependency-free package that is added with
one line of an fpm manifest. Its string functions for `character` values take stdlib's names, so that code can move
between the two.

# Software design

The interface follows from three decisions:
1. **Stateless, generic procedures.** There is no file object to open and close, and the format follows the file
   extension. Writing an array to CSV or to NumPy differs only in the file name. When `.npz` support was added, a
   stateful archive type was considered and rejected. Instead, an archive member is chosen with a `name=` argument,
   and `npz_names` lists the members, which keeps the same calling style.
2. **Errors are never silent.** Each routine takes optional `stat` and `errmsg` arguments. Without them, an error
   stops the program with a message.
3. **Standard Fortran 2018 only** [@iso2018]. Arrays of any rank are passed as assumed-rank dummy arguments and
   dispatched with `select rank`. The specific procedures for each type, kind and rank are generated from templates
   with the Fypp preprocessor [@fypp]: about 10,000 lines of templates are the single source of 24,000 lines of
   Fortran.

**Portability and correctness.** About 300 test cases run in continuous integration:
- **Compilers:** gfortran 13 to 16, Intel ifx and LLVM flang.
- **Platforms:** x86-64 and ARM64 Linux, macOS, Windows, and big-endian s390x Linux.
- **Checks with NumPy:** NumPy reads the files written on the big-endian machine.

Compiler bugs found along the way are reduced to small programs, worked around in the library, and recorded publicly.

**Performance.** A benchmark program compares each operation with plain Fortran that does the same work without the
library's checks, and it verifies that every result is exact (\autoref{tab:bench}). The benchmarks, added in version
2.2, showed where the library cost more than it should. Version 2.3 removed most of those gaps without changing any
output. Three changes illustrate the trade-offs:
- **Block transfers.** With gfortran, writing an assumed-rank array inside `select rank` was three to five times
  slower than plain stream output. Contiguous arrays are now passed to a helper with an explicit-shape dummy argument
  and transferred as one block.
- **Chunked formatting.** Numbers are formatted a column at a time, in chunks of 1024 with one internal `write`,
  instead of one `write` per number.
- **Checksums in Fortran.** The CRC-32 of a `.npz` member is computed eight bytes per step with lookup tables
  [@kounavis2008], in standard Fortran. This avoids depending on a compression library, at a cost of about 100 ms per
  256 MiB written.

| Operation | gfortran 16.2 | ifx 2026.2 | flang 23.1 |
|---|--:|--:|--:|
| Write a CSV file | 5.1 → 1.2 | 5.7 → 2.0 | 4.4 → 1.7 |
| Read a CSV file | 2.8 → 1.9 | 3.6 → 1.5 | 2.1 → 1.3 |
| Write a `.npy` file | 5.6 → 1.0 | 1.0 → 1.2 | 0.9 → 0.8 |
| Read a `.npy` file | 2.7 → 0.6 | 0.7 → 0.7 | 0.8 → 0.8 |
| Write a `.npz` file | 12.2 → 2.0 | 7.8 → 3.0 | 3.8 → 1.6 |
| Split a CSV file into cells | 1.2 → 1.0 | 4.3 → 4.4 | 7.3 → 4.7 |

Table: The library's time divided by plain Fortran's (1.0 is the same speed), in version 2.2 → version 2.3, at
`-O2`. The text files hold 10^6^ `real64` values as 20,000 rows of 50. The binary files hold 2^25^ `real64` values.
\label{tab:bench}

The overhead that remains in the text routines comes from work that plain Fortran skips. The library checks each
value, so that bad input is reported with its position, and it stores each cell as its own `String`.

# Research impact statement

The library has been public since December 2022, and each release is archived on Zenodo [@iofl]. It is used by:
- **fLDPC** [@origlia2025], a Fortran library for decoding low-density parity-check codes. Its examples and tests
  read code definitions from CSV files with `from_file`. Its author also contributed a pull request to this library.
- **The author's neural-network quantum-state solver** [@nnqs], which writes its energy and correlation results to
  CSV files with `to_file` and logs its progress with `echo`.

<!-- TODO(author): JOSS counts realized impact only, not planned use. Once the planned publication of the project that
depends on this library appears (as a paper or a preprint), cite it here. Add any other publications, theses,
courses or users you know of. -->

The 2.x releases make the library ready for wider adoption. They add a documented reference for every procedure,
twelve runnable examples, a changelog, contribution and security guidelines, test-coverage reports, and the
reproducible benchmark above.

# AI usage disclosure

<!-- TODO(author): JOSS requires this section to be complete and accurate. Check every statement, and confirm the
1.x sentence. -->
- **Versions 1.0 to 1.3 (2022–2024)** were written by the author without generative AI.
- **Versions 2.0 to 2.3 (November 2026)** were developed with Claude Code, using Anthropic's Claude Opus 5.5 model.
  The tool wrote code, tests, continuous-integration configuration, documentation and changelog entries under the
  author's direction. Commits it contributed carry a `Co-Authored-By` trailer.
- **Design decisions** were made by the author. These include the scope of each release, the error-handling policy,
  the `.npz` interface and the set of supported compilers.
- **Review and verification.** The author reviewed the changes, and every change went through a pull request. A pull
  request was merged only after the full test suite passed on three compilers and six platforms. Files were checked
  with NumPy, and performance claims were checked with the benchmark program, which verifies its results exactly.
- **This paper** was drafted with the same tool, then reviewed and revised by the author.

# Acknowledgements

The author thanks Ivan Pribec for reporting portability problems, and Marco Origlia for contributing the trimming of
header names in `to_file`. The author developed this software independently. It was not developed on
behalf of Los Alamos National Laboratory, and the views expressed here are not necessarily those of the
Laboratory.
This work received no funding.
<!-- TODO(author): LANL may require its release process (an LA-UR number) for publications by its staff, even for
independent work. Check with your organization before submitting. -->

# References
