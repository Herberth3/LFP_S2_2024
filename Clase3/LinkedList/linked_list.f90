module linked_list_m
    implicit none
    private

    ! Definición del tipo node, que representa un nodo de la lista enlazada
    type :: node
        private
        integer :: value  ! Valor del nodo
        type(node), pointer :: next => null()  ! Puntero al siguiente nodo
    end type node

    ! Definición del tipo linked_list, que representa la lista enlazada
    type, public :: linked_list
        private
        type(node), pointer :: head => null()  ! Puntero al primer nodo de la lista

    contains
        procedure :: push   ! Procedimiento para agregar un nodo al inicio
        procedure :: append ! Procedimiento para agregar un nodo al final
        ! procedure :: insert ! Procedimiento para insertar un nodo en una posición específica (comentado)
        procedure :: print  ! Procedimiento para imprimir la lista
        final :: destructor  ! Destructor para limpiar la lista
    end type linked_list

contains
    ! Procedimiento para agregar un nodo al inicio de la lista
    subroutine push(self, value)
        class(linked_list), intent(inout) :: self
        integer, intent(in) :: value

        type(node), pointer :: new
        allocate(new)  ! Se aloja memoria para el nuevo nodo

        new%value = value  ! Se asigna el valor al nuevo nodo

        if (.not. associated(self%head)) then
            self%head => new  ! Si la lista está vacía, el nuevo nodo es el primer nodo
        else
            new%next => self%head  ! El nuevo nodo apunta al primer nodo actual
            self%head => new  ! El nuevo nodo se convierte en el primer nodo
        end if
    end subroutine push

    ! Procedimiento para agregar un nodo al final de la lista
    subroutine append(self, value)
        class(linked_list), intent(inout) :: self
        integer, intent(in) :: value

        type(node), pointer :: current
        type(node), pointer :: new
        allocate(new)  ! Se aloja memoria para el nuevo nodo

        new%value = value  ! Se asigna el valor al nuevo nodo

        if (.not. associated(self%head)) then
            self%head => new  ! Si la lista está vacía, el nuevo nodo es el primer nodo
        else
            current => self%head
            do while (associated(current%next))
                current => current%next  ! Se recorre la lista hasta el último nodo
            end do
            current%next => new  ! El último nodo actual apunta al nuevo nodo
        end if
    end subroutine append

    ! Procedimiento para imprimir los valores de la lista
    subroutine print(self)
        class(linked_list), intent(in) :: self
        type(node), pointer :: current
        current => self%head

        do while (associated(current))
            print *, current%value, ","  ! Se imprime el valor del nodo actual
            current => current%next  ! Se avanza al siguiente nodo
        end do
    end subroutine print

    ! Destructor para limpiar la lista y liberar memoria
    subroutine destructor(self)
        type(linked_list), intent(inout) :: self
        type(node), pointer :: aux

        do while (associated(self%head))
            aux => self%head%next  ! Se guarda el puntero al siguiente nodo
            deallocate(self%head)  ! Se libera la memoria del nodo actual
            self%head = aux  ! Se avanza al siguiente nodo
        end do
    end subroutine destructor

end module linked_list_m
