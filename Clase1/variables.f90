program name
    implicit none
    
    ! EJEMPLO SIN CAPTURAR EXCEPCIONES
    integer :: age ! declaracion de la variable entera age
    ! EJEMPLO CON CAPTURAR EXCEPCIONES
    integer :: edad ! declaracion de la variable entera edad
    age = 10 ! Asignacion

    ! EJEMPLO SIN CAPTURAR EXCEPCIONES
    print *, 'Hello World' ! Imprime "Hola mundo"
    print *, 'El valor de la variable age es', age ! Imprime "El valor de la variable age es 10"

    ! Pedir datos desde consola
    print *, "Ingrese su edad: " ! Solicictar al usuario que ingrese su edad
    read(*,*) age ! Capturar la entrada del usuario
    print *, "Su edad es: ", age
    
    ! EJEMPLO CON CAPTURAR EXCEPCIONES
end program name