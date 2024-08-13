module dragon_module
    implicit none

    ! Definir la estructura para los Personajes
    type :: Personaje
        character(len=50) :: name
        integer :: power
    end type Personaje

    ! Publicar solo las subrutinas que se quieren usar fuera del módulo
    public :: create_personaje, modify_power, parse_command
    
contains

    ! Subrutina para crear un personaje
    subroutine create_personaje(command, personajes, num_personajes)
        type(Personaje), dimension(:), allocatable, intent(inout) :: personajes
        integer, intent(inout) :: num_personajes
        character(len=256), intent(in) :: command

        character(len=50) :: name
        integer :: power, i
        ! Arreglo temporal para almacenar los personajes actuales
        type(Personaje), dimension(:), allocatable :: temp_personajes

        ! Extraer el nombre y el poder del comando
        call parse_command(command, "create", name, power)

        ! Comprobar si el personaje ya existe
        do i = 1, num_personajes
            if ( personajes(i)%name == name ) then
                print *, 'El personaje ', trim(name), ' ya existe.'
                return
            end if
        end do

        ! Aumentar el tamaño del array de personajes
        num_personajes = num_personajes + 1

        ! Apartar memoria para almacenar temporalmente los personajes actuales
        allocate(temp_personajes(num_personajes))
        ! Copiar los personajes actuales al arreglo temporal
        temp_personajes(1:num_personajes-1) = personajes

        ! Liberar la memoria actual del array de personajes
        if (allocated(personajes)) then
            deallocate(personajes)
        end if

        ! Asignar nuevamente la memoria con el tamaño aumentado
        allocate(personajes(num_personajes))
        ! Copiar los personajes actuales al nuevo array
        personajes = temp_personajes

        ! Añadir el nuevo personaje
        personajes(num_personajes)%name = name
        personajes(num_personajes)%power = power

        print *, 'Personaje creado: ', trim(name), ' con poder ', power
        
    end subroutine create_personaje

    ! Subrutina para modificar el poder de un personaje
    subroutine modify_power(command, personajes, num_personajes, action)
        type(Personaje), dimension(:), allocatable, intent(inout) :: personajes
        integer, intent(in) :: num_personajes
        character(len=6), intent(in) :: action
        character(len=256), intent(in) :: command

        character(len=50) :: name
        integer :: power, i

        ! Extraer el nombre y el poder del comando
        call parse_command(command, action, name, power)

        ! Comprobar si el personaje existe
        do i = 1, num_personajes
            if ( personajes(i)%name == name ) then
                if ( trim(action) == 'power' ) then
                    personajes(i)%power = personajes(i)%power + power
                    print *, 'Poder de ', trim(name), ' incrementado en ', power
                else if ( action == 'injury' ) then
                    personajes(i)%power = personajes(i)%power - power
                    print *, 'Poder de ', trim(name), ' decrementado en ', power
                end if
                return
            end if
        end do

        print *, 'El personaje ', trim(name), ' no existe.'

    end subroutine modify_power

    ! Subrutina para parsear el comando
    subroutine parse_command(command, action, name, power)
        character(len=256), intent(in) :: command
        character(len=6), intent(in) :: action
        character(len=50), intent(out) :: name
        integer, intent(out) :: power
        
        character(len=256) :: temp_command
        integer :: pos_comma ! Variable auxiliar para almacenar la posición de la coma
        character(len=50) :: power_str ! Variable auxiliar para almacenar el poder en formato string

        ! Extraer el nombre y el poder del comando
        temp_command = command(len_trim(action)+2:) ! Remover la parte del comando ("create", "injury", o "power")

        pos_comma = index(temp_command, ",")
        if ( pos_comma > 0 ) then
            name = trim(adjustl(temp_command(1:pos_comma-1)))
            power_str = trim(adjustl(temp_command(pos_comma+1:)))

            ! Leer el valor del poder desde la cadena power_str
            read(power_str, *) power
        else
            print *, 'Comando incorrecto.'
        end if

    end subroutine parse_command
    
end module dragon_module