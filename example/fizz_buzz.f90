program fizz_buzz
    use io_fortran_lib, only: String, aprint
    implicit none

    integer, allocatable :: nums(:)
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
                res = String("fizz")
            end if
        else
            if ( mod(number,3) /= 0 ) then
                res = String("buzz")
            else
                res = String("fizzbuzz")
            end if
        end if
    end function FizzBuzz
end program fizz_buzz
