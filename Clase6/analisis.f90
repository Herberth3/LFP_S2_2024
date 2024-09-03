program procesar_datos
    implicit none
    character(len=100) :: entrada
    character(len=100) :: nombre
    integer :: poblacion
    character(len=100) :: bandera

    ! Leer el valor de entrada
    read(*, '(A)') entrada

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

