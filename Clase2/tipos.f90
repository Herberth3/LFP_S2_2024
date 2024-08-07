program main
    implicit none
    
    type :: t
        integer :: i  ! Atributo entero
        logical :: bool  ! Atributo lógico
    end type t

    ! Instanciar un objeto de tipo 't'
    type(t) :: var  ! Declaración de una variable del tipo 't'

    var%i = 9  ! Asignación de valor al atributo 'i' de 'var'
    var%bool = .true.  ! Asignación de valor al atributo 'bool' de 'var'

    print *, "Atributos: ", var%i, var%bool  ! Imprimir los valores de los atributos de 'var'

    var = t(9, .true.)  ! Asignación de valores a 'var' utilizando el constructor del tipo 't'
    var = t(i = 9, bool = .true.)  ! Otra forma de asignar valores a 'var' utilizando el constructor del tipo 't'
end program main