module mi_modulo
    implicit none
    
    private  ! Todo el contenido del módulo es privado por defecto
    public public_var, print_matrix  ! Estas variables y subrutinas son públicas
    
    real, parameter :: public_var = 2  ! Variable pública con un valor constante
    integer :: privat_var  ! Variable privada


    contains

    subroutine print_matrix(A)
        real, intent(in) :: A(:,:)  ! Matriz de entrada
        integer :: i  ! Variable auxiliar

        do i = 1, size(A, 1) ! Size(A, 1) devuelve el número de filas de la matriz
            print *, A(i, :)  ! Imprimir cada fila de la matriz
        end do
    end subroutine print_matrix

end module mi_modulo

program main
    use mi_modulo  ! Usar el módulo 'mi_modulo'
    ! use mi_modulo, only: pm=>print_matrix  ! Importar la subrutina 'print_matrix' del módulo 'mi_modulo' solo para esta función
    implicit none

    real :: mat(10, 10)  ! Declaración de una matriz de reales 10x10

    mat(:, :) = 9  ! Inicialización de la matriz con el valor 9
    call print_matrix(mat)  ! Llamada a la subrutina 'print_matrix' para imprimir la matriz
    
end program main