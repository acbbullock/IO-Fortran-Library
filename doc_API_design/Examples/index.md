---
title: Tutorials
author: Austin C Bullock
ordered_subpage: csv.md
ordered_subpage: binary.md
ordered_subpage: logging.md
ordered_subpage: manipulations.md
ordered_subpage: fizzbuzz.md
ordered_subpage: exome.md
ordered_subpage: benchmark.md
---

The following subsections provide example programs for common use cases
of the public [interfaces](../../lists/procedures.html):

* [Handling csv files](csv.html)
* [Handling binary and NumPy files](binary.html)
* [Handling log files](logging.html)
* [String manipulation](manipulations.html)
* [FizzBuzz](fizzbuzz.html)
* [NGS Human Core Exome Panel](exome.html)
* [Benchmarking](benchmark.html)

Most of these programs are also in the
[example directory](https://github.com/acbbullock/IO-Fortran-Library/tree/main/example)
of the repository, and CI builds and runs them, so the code shown here
always works with the current release. To run one from a clone of the
repository:

```sh
fpm run --example csv_formats
fpm run --example quickstart
```
