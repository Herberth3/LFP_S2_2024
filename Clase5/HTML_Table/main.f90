program generar_tabla_html
    use token_module
    implicit none

    type(Token), dimension(:), allocatable :: tokens_list
    integer :: n

    ! Definir el número de tokens
    n = 3
    allocate(tokens_list(n))

    ! Inicializar los tokens
    tokens_list(1) = Token(1, "if", "Condicional", 1, 1)
    tokens_list(2) = Token(2, "x", "Variable", 2, 5)
    tokens_list(3) = Token(3, "=", "Operador de asignación", 2, 7)

    ! Llamar a la subrutina que genera el archivo HTML
    call generar_html(tokens_list)

    print *, "Archivo HTML generado: TABLA.html"

contains

    subroutine generar_html(tokens)
        type(Token), dimension(:), intent(in) :: tokens
        integer :: unit_number, i
        character(len=256) :: filename

        ! Definir el nombre del archivo HTML
        filename = "TABLA.html"

        ! Abrir el archivo para escribir
        unit_number = 10
        open(unit_number, file=filename, status="replace", action="write")

        ! Escribir la estructura básica del HTML y la tabla
        write(unit_number, '(A)') "<!DOCTYPE html>"
        write(unit_number, '(A)') "<html>"
        write(unit_number, '(A)') "<head>"
        write(unit_number, '(A)') "<title>Tabla de Tokens</title>"
        write(unit_number, '(A)') "<style>"
        write(unit_number, '(A)') "table {width: 50%; border-collapse: collapse;}"
        write(unit_number, '(A)') "th, td {border: 1px solid black; padding: 8px; text-align: left;}"
        write(unit_number, '(A)') "th {background-color: #f2f2f2;}"
        write(unit_number, '(A)') "</style>"
        write(unit_number, '(A)') "</head>"
        write(unit_number, '(A)') "<body>"
        write(unit_number, '(A)') "<h2>Tabla de Tokens</h2>"
        write(unit_number, '(A)') "<table>"
        write(unit_number, '(A)') "<tr><th>Número de Token</th><th>Lexema</th><th>Descripción</th><th>Línea</th><th>Columna</th></tr>"

        ! Escribir cada token en la tabla
        do i = 1, size(tokens)
            write(unit_number, '(A)') "<tr>"
            write(unit_number, '(A)') "<td>" // trim(adjustl(itoa(tokens(i)%numero))) // "</td>"
            write(unit_number, '(A)') "<td>" // trim(tokens(i)%lexema) // "</td>"
            write(unit_number, '(A)') "<td>" // trim(tokens(i)%descripcion) // "</td>"
            write(unit_number, '(A)') "<td>" // trim(adjustl(itoa(tokens(i)%linea))) // "</td>"
            write(unit_number, '(A)') "<td>" // trim(adjustl(itoa(tokens(i)%columna))) // "</td>"
            write(unit_number, '(A)') "</tr>"
        end do

        ! Cerrar la tabla y el archivo HTML
        write(unit_number, '(A)') "</table>"
        write(unit_number, '(A)') "</body>"
        write(unit_number, '(A)') "</html>"

        close(unit_number)
    end subroutine generar_html

    ! Subrutina para convertir entero a cadena de texto
    function itoa(i) result(str)
        implicit none
        integer, intent(in) :: i
        character(len=12) :: str
        write(str, '(I0)') i
    end function itoa

end program generar_tabla_html
