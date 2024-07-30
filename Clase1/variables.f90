program name
    implicit none
    
    ! EJEMPLO SIN CAPTURAR EXCEPCIONES
    integer :: age ! declaracion de la variable entera age
    ! EJEMPLO CON CAPTURAR EXCEPCIONES
    integer :: edad, iostatus ! declaracion de la variable entera edad
    age = 10 ! Asignacion
    edad = 10 ! Asignacion

    ! EJEMPLO SIN CAPTURAR EXCEPCIONES
    print *, 'Hello World' ! Imprime "Hola mundo"
    print *, 'El valor de la variable age es', age ! Imprime "El valor de la variable age es 10"

    ! Pedir datos desde consola
    print *, "Ingrese su edad: " ! Solicictar al usuario que ingrese su edad
    read(*,*) age ! Capturar la entrada del usuario
    print *, "Su edad es: ", age
    
    ! EJEMPLO CON CAPTURAR EXCEPCIONES
    write(*,*) 'Escribe tu edad: '
    read(*,*, iostat = iostatus) edad

    if ( iostatus /= 0 ) then
        write(*,*) 'Error: Entrada invalida, se espera un valor numerico'
    else
        if ( edad < 18 ) then
            print *, 'Eres menor de edad'
        else
            print *, 'Eres mayor de edad'
        end if
    end if
end program name