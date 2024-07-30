program main
    implicit none

    character(len=4) :: first_name
    character(len=11) :: last_name
    character(len=16) :: full_name
    character(:), allocatable :: full_name2

    first_name = 'John'
    last_name = 'Constantine'
    full_name2 = 'John Constantine Complete'

    full_name = first_name // ' ' // last_name

    print *, full_name
    print *, full_name2
    
end program main