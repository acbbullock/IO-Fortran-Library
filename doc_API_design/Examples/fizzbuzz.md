---
title: fizzbuzz
author: Austin C Bullock
---

The following program demonstrates the use of [aprint](../Ref/aprint.html) for printing an array of [Strings](../../type/string.html) containing values of the first 100 [FizzBuzz](https://en.wikipedia.org/wiki/Fizz_buzz) numbers:

```fortran
program main
    use io_fortran_lib, only: String, aprint
    implicit none (type,external)

    integer, allocatable, dimension(:) :: nums
    integer :: i

    nums = [(i, i = 1, 100)]

    call aprint( FizzBuzz(nums) )

    contains
    pure elemental type(String) function FizzBuzz(number) result(res)
        integer, intent(in) :: number

        if ( mod(number,5) /= 0 ) then
            if ( mod(number,3) /= 0 ) then
                res = String(number)
            else
                res = String('fizz')
            end if
        else
            if ( mod(number,3) /= 0 ) then
                res = String('buzz')
            else
                res = String('fizzbuzz')
            end if
        end if
    end function FizzBuzz
end program main
```
