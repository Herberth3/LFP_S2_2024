program flow_control
    implicit none

    integer :: i, j

    ! EJEMPLO IF
    if (.true.) then
        ! Codigo que se ejecuta si la condicion es verdadera
    else if (.false.) then
        ! Codigo que se ejecuta si la condicion es falsa
    else
        ! Codigo que se ejecuta si la condicion es falsa
    end if

    ! EJEMPLO DO
    do i = 1, 15, 2
        ! Codigo que se ejecuta 10 veces
        print *, i
    end do

    ! EJEMPLO DO WHILE
    i = 0
    do while (i < 10)
        i = i + 1
        print *, i
    end do

    ! CON NOMBRE EN LOS CICLOS
    afuera: do i = 1, 10
        dentro: do j = 1, 10
            if ((j + i) > 10) then
                cycle afuera  ! Saltar al siguiente ciclo exterior si la condición se cumple
            end if
            print *, 'I=', i, ' J=', j, ' Sum=', j + i  ! Imprimir 'i', 'j' y su suma
        end do dentro
    end do afuera

end program flow_control