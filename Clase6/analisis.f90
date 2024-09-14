program procesar_datos
    implicit none
    character(len=100) :: entrada
    character(len=100) :: linea
    character(len=100) :: nombre
    integer :: poblacion, ios
    character(len=100) :: bandera

    ! Inicializar la variable de entrada
    entrada = ''

    ! Leer el valor de entrada obtenida desde tkinter y concatenar cada linea al valor de entrada
    do
        read(*, '(A)', iostat = ios) linea
        if (ios /= 0) exit   ! Se alcanzo el fin del archivo
        entrada = trim(entrada) // trim(linea) // char(10) ! Concatenar la línea leida al valor de entrada y agregar un salto de línea
    end do

    ! Procesar los atributos adicionales
    poblacion = 2352342
    nombre = "Korea"
    bandera = "C:/imagen.png"

    ! Imprimir los resultados en un formato delimitado por comas (Esta seria la respuesta para Python)
    print *, trim(entrada) // "," // trim(adjustl(itoa(poblacion))) // "," // trim(nombre) // "," // trim(bandera)

contains

! Subrutina para convertir entero a cadena de texto
function itoa(i) result(str)
    implicit none
    integer, intent(in) :: i
    character(len=12) :: str
    write(str, '(I0)') i
end function itoa

end program procesar_datos

