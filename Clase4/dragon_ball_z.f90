! Comandos que se pueden ingresar en la consola:
! create <nombre>,<poder>  Ejemplo: create Gohan,1000
! power <nombre>,<poder>   Ejemplo: power Gohan,100
! injury <nombre>,<poder>  Ejemplo: injury Gohan,200
! exit

program dragon_ball_z
    use dragon_module  ! Uso del módulo dragon_module
    implicit none

    ! Arreglo para almacenar los personajes
    type(Personaje), dimension(:), allocatable :: personajes
    ! Variable para capturar el comando
    character(len=256) :: command


    integer :: num_personajes ! Variable para llevar la cuenta del número de personajes
    logical :: done ! Variable para detener el programa

    ! Inicializar variables
    num_personajes = 0
    done = .false.

    ! Bucle principal del programa
    do while (.not. done)
        ! Leer comando desde la consola
        print *, 'Ingresa un comando: '
        read(*, '(A)') command

        ! Procesar el comando 'create'
        if ( index(trim(command), 'create') == 1 ) then
            call create_personaje(command, personajes, num_personajes)

        ! Procesar el comando 'power'
        else if ( index(trim(command), 'power') == 1 ) then
            call modify_power(command, personajes, num_personajes, 'power ')

        ! Procesar el comando 'injury'
        else if ( index(trim(command), 'injury') == 1 ) then
            call modify_power(command, personajes, num_personajes, 'injury')

        ! Procesar el comando 'exit'
        else if (trim(command) == 'exit') then
            done = .true.
            print *, 'Saliendo del programa...'

        ! Comando incorrecto
        else
            print *, 'Comando incorrecto.'
        end if

    end do
    
end program dragon_ball_z