---
title: String manipulations
author: Austin C Bullock
---

## String Queries

Sometimes it is useful or necessary to write execution conditions based
on compiler vendors or version, such as when a certain compiler version
has a known bug or when a piece of code is required for a specific
compiler. The following program shows how one may use the
[count](../Ref/String-methods.html#count) procedure to determine a
compiler and version at runtime:

```fortran
{!example/compiler_check.f90!}
```

## Dynamic string manipulation

Using [operator](../Ref/operators.html) techniques and
[string methods](../Ref/String-methods.html), we may easily perform
complex string manipulations during run time.

The following program demonstrates the use of the
[String](../../type/string.html) type and some
[type-bound procedures](../Ref/String-methods.html) for manipulating a
timestamp:

```fortran
{!example/split_join.f90!}
```

This program produces the following sample output:

```text
ORIGINAL TIME STAMP:
Date : 20240316
Time : 163641.569

RECONSTRUCTED TIME STAMP:
20240316   : Date | 163641.569 : Time
```
