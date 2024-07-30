program main
    implicit none

    integer, dimension(10) :: array_1 ! declaracion
    integer :: array_2(10) ! Otra forma de declarar
    integer, dimension(10, 10) :: array_3 ! Matriz de 10x10
    integer :: i

    array_1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] ! Asignacion
    i = 0

    print *, array_1 ! Imprime todo el arreglo array_1
    print *, array_1(1:10:3) ! Imprime de 1 hasta 10 en saltos de 3
    print *, array_1(10:1:-1) ! Imprimir el array en orden invertido

    array_2(:) = 0 ! Inicializa todo el array a 0

    array_2 = [(i, i=1, 10)] ! Inicializa el array a 1 hasta 10, con un DO implicito
    print *, array_2
    
end program main