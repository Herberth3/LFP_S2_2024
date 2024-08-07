module shapes_mod
    implicit none
    private
    public :: shape, circle, rectangle

    ! Definición de la clase base "shape"
    type :: shape
    contains
        ! Procedimiento para calcular el área de una forma genérica (base)
        procedure :: area => area_shape
    end type shape

    ! Definición de la clase derivada "circle", que extiende a "shape"
    type, extends(shape) :: circle
        real :: radius  ! Radio del círculo
    contains
        ! Procedimiento para calcular el área del círculo
        procedure :: area => area_circle
    end type circle

    ! Definición de la clase derivada "rectangle", que extiende a "shape"
    type, extends(shape) :: rectangle
        real :: length, width  ! Largo y ancho del rectángulo
    contains
        ! Procedimiento para calcular el área del rectángulo
        procedure :: area => area_rectangle
    end type rectangle

contains

    ! Implementación del procedimiento "area" para la clase base "shape"
    function area_shape(this) result(res)
        class(shape), intent(in) :: this  ! Clase base como argumento
        real :: res
        res = 0.0  ! Área genérica (base) es 0
    end function area_shape

    ! Implementación del procedimiento "area" para la clase "circle"
    function area_circle(this) result(res)
        class(circle), intent(in) :: this  ! Clase círculo como argumento
        real :: res
        res = 3.14159 * this%radius**2  ! Cálculo del área del círculo
    end function area_circle

    ! Implementación del procedimiento "area" para la clase "rectangle"
    function area_rectangle(this) result(res)
        class(rectangle), intent(in) :: this  ! Clase rectángulo como argumento
        real :: res
        res = this%length * this%width  ! Cálculo del área del rectángulo
    end function area_rectangle

end module shapes_mod
