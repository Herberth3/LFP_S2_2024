! Archivo: analizador.f90
module analizador
    implicit none
    integer, parameter :: max_length = 1000
    integer, parameter :: max_tokens = 100
    type :: Token
        character(len=100) :: lexema
        character(len=20) :: tipo
    end type Token
contains
    ! Subrutina para leer el archivo y devolver su contenido
    subroutine read_file(filename, file_content, ios)
        character(len=11), intent(in) :: filename
        character(len=max_length), intent(out) :: file_content
        integer, intent(out) :: ios
        character(len=200) :: temp_line
        integer :: pos

        ! Inicializar el contenido del archivo como vacío
        file_content = ''
        pos = 1  ! Posición para concatenar líneas

        ! Abrir el archivo para lectura
        open(unit=10, file=filename, status='old', action='read', iostat=ios)
        if (ios == 0) then
            ! Leer el archivo línea por línea y agregarlo a file_content
            do while (.not. is_iostat_end(ios))
                read(10, '(A)', iostat=ios) temp_line
                if (ios == 0) then
                    ! Concatenar la línea leída al contenido total
                    file_content(pos:pos+len_trim(temp_line)) = trim(temp_line) // char(10)
                    pos = pos + len_trim(temp_line) + 1
                end if
            end do
            ios = 0
            close(10)
        else
            print *, "Error: No se pudo abrir el archivo ", filename
        end if
    end subroutine read_file

    ! Subrutina para analizar el contenido del archivo
    subroutine analyze(file_content)
        character(len=max_length), intent(inout) :: file_content
        character(len=1) :: buffer(max_length)
        integer :: i, j, length, state, token_index
        type(Token) :: tokens(max_tokens)
        character(len=100) :: current_lexema
        character(len=1) :: caracter

        ! Inicialización
        length = len_trim(file_content)
        buffer = ' '
        current_lexema = ''
        token_index = 1
        state = 0
        i = 0

        ! Agregar carácter de fin de cadena
        if (length < max_length) then
            file_content(length + 1: length + 1) = '#'
            length = length + 1
        end if

        ! Procesar cada carácter con un ciclo for
        do while (i <= length)
            i = i + 1
            caracter = file_content(i:i)
            
            select case(state)
                case(0)  ! Estado inicial
                    if (caracter >= 'a' .and. caracter <= 'z') then  ! Letra [a-z]
                        state = 1
                        current_lexema = caracter
                    else if (caracter == '"') then
                        state = 2
                        current_lexema = caracter
                    else if (caracter == ' ' .or. caracter == char(9) .or. caracter == char(10)) then
                        cycle ! Ignorar espacios, tabulaciones y saltos de línea
                    else if ( caracter == char(35) .and. i == length ) then
            
                    else
                        print *, "Error lexico: Caracter inesperado: ", caracter
                    end if

                case(1)  ! Estado para identificadores
                    if (caracter >= 'a' .and. caracter <= 'z') then
                        current_lexema = trim(current_lexema) // caracter
                    else
                        state = 0
                        if (current_lexema /= '') then
                            ! Verificar si el identificador es una palabra reservada
                            if ( current_lexema == 'int' ) then
                                tokens(token_index)%lexema = trim(current_lexema)
                                tokens(token_index)%tipo = "Reservada"
                            else ! No es una palabra reservada
                                tokens(token_index)%lexema = trim(current_lexema)
                                tokens(token_index)%tipo = "Identificador"
                            end if

                            token_index = token_index + 1
                            current_lexema = ''
                            
                        end if
                        ! Regresar al carácter anterior para seguir evaluando
                        i = i - 1
                    end if

                case(2)  ! Estado para el inicio de una cadena
                    if (caracter /= '"') then
                        state = 3
                        current_lexema = trim(current_lexema) // caracter
                    else
                        print *, "Error lexico: Se esperaba un caracter de cadena"
                    end if

                case(3)  ! Estado para el contenido de la cadena
                    if (caracter /= '"') then
                        current_lexema = trim(current_lexema) // caracter
                    else
                        state = 4
                        current_lexema = trim(current_lexema) // caracter
                    end if

                case(4)  ! Estado final de una cadena
                    state = 0
                    if (current_lexema /= '') then
                        tokens(token_index)%lexema = trim(current_lexema)
                        tokens(token_index)%tipo = "Cadena"
                        token_index = token_index + 1
                        current_lexema = ''
                    end if
                    i = i - 1
            end select

            if (caracter == '#' .and. length == i) then
                print *, "Analisis completo. Tokens encontrados:"
                do j = 1, token_index - 1
                    print *, "Lexema: ", trim(tokens(j)%lexema), " Tipo: ", trim(tokens(j)%tipo)
                end do
            end if
        end do

    end subroutine analyze

    ! Subrutina para la opción "Graficar Autómata"
    subroutine graph_automata()
        integer :: state
        integer :: rama
        character(len=5000) :: dot_code
    
        ! Inicialización
        state = 0
        rama = 0
        dot_code = "digraph Automata {" // new_line('A') // "rankdir=LR;" // new_line('A') &
                  // "node [shape = circle, style=filled];" // new_line('A')
    
        ! Recorrer el contenido para generar los estados y transiciones
        do
            select case (state)
                case (0)
                    if (rama == 0) then
                        state = 1
                        dot_code = trim(dot_code) // "S0 -> S1 [label=""L""];" // new_line('A')
                    else if (rama == 1) then
                        state = 2
                        dot_code = trim(dot_code) // "S0 -> S2 [label=""\" // char(34) // """];" // new_line('A')
                    else
                        ! Otros casos no necesarios
                    end if
    
                case (1)
                    ! Estado de aceptación
                    dot_code = trim(dot_code) // "S1 -> S1 [label=""L""];" // new_line('A')
                    state = 0
                    rama = 1
                case (2)
                    state = 3
                    dot_code = trim(dot_code) // "S2 -> S3 [label=""C""];" // new_line('A')
    
                case (3)
                    dot_code = trim(dot_code) // "S3 -> S3 [label=""C""];" // new_line('A')

                    state = 4
                    dot_code = trim(dot_code) // "S3 -> S4 [label=""\" // char(34) // """];" // char(10)
    
                case (4)
                    state = 0
                    exit
            end select
        end do
    
        ! Finalizar el archivo DOT
        dot_code = trim(dot_code) // "S1 [shape = doublecircle, fillcolor = ""green""];" // new_line('A')
        dot_code = trim(dot_code) // "S4 [shape = doublecircle, fillcolor = ""green""];" // new_line('A')
        dot_code = trim(dot_code) // "}"
    
        ! Escribir el archivo .dot
        open(unit=11, file="automata.dot", status='replace')
        write(11, '(A)') trim(dot_code)
        close(11)
    
        print *, "Automata graficado en automata.dot"

        ! Llamar a Graphviz para crear la imagen PNG
        call system("dot -Tpng automata.dot -o automata.png")

        print *, "Imagen automata.png generada exitosamente."
    end subroutine graph_automata
    
end module analizador
