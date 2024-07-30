function vect_norm(n, vec) result(norm)
    integer, intent(in) :: n
    real, intent(in) :: vec(n)

    real :: norm

    norm = sqrt(sum(vec**2))

    
end function vect_norm

program main
    implicit none

    real :: v(9)
    real :: vect_norm

    v(:) = 9

    print *, vect_norm(9, v)
    
end program main