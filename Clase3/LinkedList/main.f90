program main
    use linked_list_m  ! Se usa el módulo linked_list_m
    implicit none

    type(linked_list) :: list  ! Se declara una variable de tipo linked_list

    ! Se agregan elementos al inicio de la lista
    call list%push(1)
    call list%push(3)
    call list%push(5)
    call list%push(7)
    call list%push(9)

    ! Se agregan elementos al final de la lista
    call list%append(11)
    call list%append(13)
    call list%append(15)

    ! Se imprime la lista
    call list%print()

end program main
