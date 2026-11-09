---
title: npz_names
author: Austin C Bullock
---

## [interface npz_names](../../interface/npz_names.html)

*Description*: Subroutine for listing the names of the arrays in a
NumPy [.npz archive](../UserInfo/file-ext.html#numpy-npz-archives).

```fortran
call npz_names(file, names [, stat, errmsg])
```

* `file` is of type `character(len=*)`
* `names` is of type `type(String), allocatable, dimension(:)`
* `stat` is `optional` and of type `integer`
* `errmsg` is `optional` and of type `character(len=*)`

`names` gets one element for each array, in the order the arrays are
stored, which is the order they were added. Each name is the one
[to_file](to_file.html) and [from_file](from_file.html) take as `name`,
which is the name of the file in the archive without `.npy`, as NumPy
gives it.

```fortran
type(String), allocatable :: names(:)
integer :: i

call npz_names("data.npz", names)

do i = 1, size(names)
    write(*,*) names(i)%as_str()
end do
```

@note A missing file, or one that isn't a ZIP archive, is an
`IOFL_READ_ERR`, and `names` then has size `0`.
