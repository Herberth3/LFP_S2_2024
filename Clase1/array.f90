program main
    implicit none

    integer, dimension(10) :: array_1 ! declaracion
    integer :: array_2(10) ! Otra forma de declarar
    integer, dimension(10, 10) :: array_3 ! Matriz de 10x10

    array_1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] ! Asignacion

    print *, array_1 ! Imprime todo el arreglo array_1
    print *, array_1(1:10:3) ! Imprime de 1 hasta 10 en saltos de 3
    
end program main