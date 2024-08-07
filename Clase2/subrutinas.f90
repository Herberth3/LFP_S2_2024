subroutine routine(n, m, A)
    implicit none

    integer, intent(in) :: n  ! Número de filas
    integer, intent(in) :: m  ! Número de columnas
    real, intent(in) :: A(n, m)  ! Matriz de entrada

    integer :: i  ! Variable auxiliar

    do i = 1, n
        print *, A(i, 1:m)  ! Imprimir cada fila de la matriz
    end do

end subroutine routine

program name
    implicit none
    
    real :: mat(10, 20)  ! Declaración de una matriz de reales 10x20
    mat(:,:) = 0.0  ! Inicialización de la matriz con ceros

    call routine(10, 20, mat)  ! Llamada a la subrutina 'routine' con la matriz 'mat'
end program name