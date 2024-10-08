module mod_analizador_lexico
    use mod_tokens
    implicit none
    private
    public :: escanear

contains
    subroutine escanear(entrada, listaTokens)
        character(len=*), intent(inout) :: entrada
        ! Variable que representa la lista de tokens
        type(token), dimension(:), allocatable, intent(out) :: listaTokens
        ! Variable que representa el estado actual
        integer :: i, estado, length
        character(len=1) :: c
        ! Variable que representa el lexema que actualmente se esta acumulando
        character(:), allocatable :: auxLex

        ! Inicialización
        length = len_trim(entrada)
        estado = 0
        auxLex = ""
        i = 0
        allocate(listaTokens(0)) ! Inicializar el array de tokens vacío

        ! Le agrego caracter de fin de cadena porque hay lexemas que aceptan con 
        ! el primer caracter del siguiente lexema y si este caracter no existe entonces
        ! perdemos el lexema
        entrada(length + 1: length + 1) = '#'
        length = length + 1

        ! Ciclo que recorre de izquierda a derecha caracter por caracter la cadena de entrada
        do while (i <= length)
            i = i + 1
            c = entrada(i:i)

            ! Select en el que cada caso representa cada uno de los estados del conjunto de estados
            select case (estado)
            case (0)
                if (c >= 'a' .and. c <= 'z' .or. c >= 'A' .and. c <= 'Z' ) then ! Si es una letra minuscula o mayuscula
                    estado = 1
                    auxLex = trim(auxLex) // c
                else if (c == '!' .or. c == '<' .or. c == '>' .or. c == ';' .or. c == '-') then
                    estado = 2
                    auxLex = c
                else if (c == '/') then
                    estado = 3 ! Comentario
                    auxLex = c
                else if (c == char(32) .or. c == char(9) .or. c == char(10)) then
                    ! Ignorar espacios, tabulaciones y saltos de linea
                    cycle
                else if (c == '#' .and. i == length) then
                    print *, "Hemos concluido el analisis lexico satisfactoriamente"
                    exit
                else
                    print *, "Error lexico con: ", trim(c)
                    estado = 0
                end if
            case (1)
                if (c >= 'A' .and. c <= 'Z' .or. c >= 'a' .and. c <= 'z' .or. c >= '0' .and. c <= '9') then
                    estado = 1
                    auxLex = trim(auxLex) // c
                else
                    if ( auxLex == 'Controles' ) then
                        call addToken(RESERVADA_CONTROLES, auxLex, listaTokens)
                    else if ( auxLex == 'Etiqueta' ) then
                        call addToken(RESERVADA_ETIQUETA, auxLex, listaTokens)
                    else if ( auxLex == 'Boton' ) then
                        call addToken(RESERVADA_BOTON, auxLex, listaTokens)
                    else if ( auxLex == 'Check' ) then
                        call addToken(RESERVADA_CHECK, auxLex, listaTokens)
                    else if ( auxLex == 'RadioBoton' ) then
                        call addToken(RESERVADA_RADIOBOTON, auxLex, listaTokens)
                    else if ( auxLex == 'Texto' ) then
                        call addToken(RESERVADA_TEXTO, auxLex, listaTokens)
                    else if ( auxLex == 'AreaTexto' ) then
                        call addToken(RESERVADA_AREATEXTO, auxLex, listaTokens)
                    else if ( auxLex == 'Clave' ) then
                        call addToken(RESERVADA_CLAVE, auxLex, listaTokens)
                    else if ( auxLex == 'Contenedor' ) then
                        call addToken(RESERVADA_CONTENEDOR, auxLex, listaTokens)
                    else
                        call addToken(IDENTIFICADOR, auxLex, listaTokens)
                    end if
                    i = i - 1
                    estado = 0
                end if
            case (2)
                if ( auxLex == '!' ) then
                    call addToken(SIGNO_ADMIRACION_C, auxLex, listaTokens)
                else if ( auxLex == '<' ) then
                    call addToken(SIGNO_MENOR_QUE, auxLex, listaTokens)
                else if ( auxLex == '>' ) then
                    call addToken(SIGNO_MAYOR_QUE, auxLex, listaTokens)
                else if ( auxLex == ';' ) then
                    call addToken(SIGNO_PUNTO_Y_COMA, auxLex, listaTokens)
                else if ( auxLex == '-' ) then
                    call addToken(SIGNO_GUION, auxLex, listaTokens)
                end if
                estado = 0
                i = i - 1
            case (3)
                if ( c == '/' ) then
                    estado = 4
                    auxLex = trim(auxLex) // c
                else
                    print *, "Error: comentario de línea mal formado. Se esperaba '//'"
                end if
            case (4)
                if ( c == char(10) ) then ! Si es un salto de linea
                    estado = 5
                else
                    auxLex = auxLex // c
                    estado = 4
                end if
            case (5)
                call addToken(COMENTARIO_LINEA, auxLex, listaTokens)
                estado = 0
                i = i - 1
            end select
        end do
    end subroutine escanear

    subroutine addToken(tipo, valor, listaTokens)
        integer, intent(in) :: tipo
        character(len=*), intent(inout) :: valor
        type(token), dimension(:), allocatable, intent(inout) :: listaTokens
        type(token) :: nuevoToken
        type(token), dimension(:), allocatable :: tempTokens
        integer :: tamanoActual

        nuevoToken%tipo = tipo
        nuevoToken%valor = valor

        tamanoActual = size(listaTokens)

        ! Redimensionar la lista de tokens para agregar uno nuevo
        allocate(tempTokens(tamanoActual + 1))

        ! Copiar los tokens actuales a la nueva lista
        if ( tamanoActual > 0 ) then
            tempTokens(1:tamanoActual) = listaTokens
        end if

        ! Añadir el token a la lista de tokens
        tempTokens(tamanoActual + 1) = nuevoToken

        ! Transferir la nueva lista a la lista de tokens
        call move_alloc(tempTokens, listaTokens)

        print *, "Token anadido: ", trim(getTipoTokenEnString(tipo)), " ", trim(valor)

        ! Limpieza
        valor = ""
    end subroutine addToken

    function getTipoTokenEnString(p) result(res)
        integer, intent(in) :: p
        character(len=20) :: res

        res = ""
        
        select case (p)
        case (RESERVADA_CONTROLES)
            res = "RESERVADA_CONTROLES"
        case (RESERVADA_ETIQUETA)
            res = "RESERVADA_ETIQUETA"
        case (RESERVADA_BOTON)
            res = "RESERVADA_BOTON"
        case (RESERVADA_CHECK)
            res = "RESERVADA_CHECK"
        case (RESERVADA_RADIOBOTON)
            res = "RESERVADA_RADIOBOTON"
        case (RESERVADA_TEXTO)
            res = "RESERVADA_TEXTO"
        case (RESERVADA_AREATEXTO)
            res = "RESERVADA_AREATEXTO"
        case (RESERVADA_CLAVE)
            res = "RESERVADA_CLAVE"
        case (RESERVADA_CONTENEDOR)
            res = "RESERVADA_CONTENEDOR"
        case (IDENTIFICADOR)
            res = "IDENTIFICADOR"
        case (SIGNO_MENOR_QUE)
            res = "SIGNO_MENOR_QUE"
        case (SIGNO_MAYOR_QUE)
            res = "SIGNO_MAYOR_QUE"
        case (SIGNO_ADMIRACION_C)
            res = "SIGNO_ADMIRACION_C"
        case (SIGNO_PUNTO_Y_COMA)
            res = "SIGNO_PUNTO_Y_COMA"
        case (SIGNO_GUION)
            res = "SIGNO_GUION"
        case (COMENTARIO_LINEA)
            res = "COMENTARIO_LINEA"
        case default
            res = "Desconocido"
        end select
    end function getTipoTokenEnString

end module mod_analizador_lexico
