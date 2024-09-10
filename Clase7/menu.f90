! Archivo: menu.f90
program automata_menu
    use analizador
    implicit none
    integer :: option
    character(len=1000) :: file_content
    integer :: ios

    ! Ciclo del menú
    do
        ! Mostrar las opciones del menú
        call show_menu()

        ! Leer la opción ingresada por el usuario
        read(*,*) option

        ! Ejecutar acción según la opción seleccionada
        select case(option)
            case(1)
                ! Abrir el archivo y leer su contenido completo
                call read_file("entrada.txt", file_content, ios)
                if (ios /= 0) then
                    print *, "Error al leer el archivo"
                else
                    call analyze(file_content)  ! Llamar a la subrutina analyze con file_content
                end if
            case(2)
                call graph_automata()  ! Llamar a la subrutina para graficar el autómata
            case(3)
                print *, "Saliendo del programa..."
                exit
            case default
                print *, "Opción no válida. Por favor, intente de nuevo."
        end select
    end do
end program automata_menu

! Subrutina para mostrar el menú
subroutine show_menu()
    print *, "==========================="
    print *, "       Menu Principal       "
    print *, "==========================="
    print *, "1. Analizar"
    print *, "2. Graficar Automata"
    print *, "3. Salir"
    print *, "Seleccione una opcion:"
end subroutine show_menu
