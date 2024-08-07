program test_shapes
    use shapes_mod  ! Uso del módulo shapes_mod
    implicit none

    type(circle) :: c  ! Definición de un objeto de tipo circle
    type(rectangle) :: r  ! Definición de un objeto de tipo rectangle

    ! Inicialización del objeto circle con un radio de 5.0
    c = circle(5.0)
    ! Inicialización del objeto rectangle con largo 2.0 y ancho 3.0
    r = rectangle(2.0, 3.0)

    ! Impresión del área del círculo
    print *, 'Circle area: ', c%area()
    ! Impresión del área del rectángulo
    print *, 'Rectangle area: ', r%area()
end program test_shapes
