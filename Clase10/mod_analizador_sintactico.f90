module mod_analizador_sintactico
    use mod_tokens
    implicit none
    private
    public :: parsear

    ! Variables para el analizador sintáctico
    type(token), dimension(:), allocatable :: tokens
    integer :: pos

contains

    subroutine parsear(listaTokens)
        type(token), dimension(:), intent(in) :: listaTokens
        tokens = listaTokens
        pos = 1

        ! Iniciar la producción S
        call parse_S()

        if (pos <= size(tokens)) then
            print *, "Error sintactico: Tokens inesperados al final."
        else
            print *, "Analisis sintactico completado con errores manejados (si los hubo)."
        end if
    end subroutine parsear

    ! Producción S ::= ControlesBlock
    subroutine parse_S()
        call parse_ControlesBlock()
    end subroutine parse_S

    ! Producción ControlesBlock ::= '<' '!' '-' '-' RESERVADA_CONTROLES ControlList RESERVADA_CONTROLES '-' '-' '>'
    subroutine parse_ControlesBlock()
        if (.not. consumirToken(SIGNO_MENOR_QUE)) call modoPanico(';')
        if (.not. consumirToken(SIGNO_ADMIRACION_C)) call modoPanico(';')
        if (.not. consumirToken(SIGNO_GUION)) call modoPanico(';')
        if (.not. consumirToken(SIGNO_GUION)) call modoPanico(';')
        if (.not. consumirToken(RESERVADA_CONTROLES)) call modoPanico(';')

        call parse_ControlList()

        if (.not. consumirToken(RESERVADA_CONTROLES)) call modoPanico(';')
        if (.not. consumirToken(SIGNO_GUION)) call modoPanico(';')
        if (.not. consumirToken(SIGNO_GUION)) call modoPanico(';')
        if (.not. consumirToken(SIGNO_MAYOR_QUE)) call modoPanico(';')
    end subroutine parse_ControlesBlock

    ! Producción ControlList ::= Control ControlList | ε
    recursive subroutine parse_ControlList()
        if (esControlValido()) then
            call parse_Control()
            call parse_ControlList()
        else
            ! Producción vacía (epsilon), no hacemos nada
        end if
    end subroutine parse_ControlList

    ! Producción Control ::= ControlType IDENTIFICADOR ';' | COMENTARIO_LINEA
    subroutine parse_Control()
        if ( tokens(pos)%tipo == COMENTARIO_LINEA ) then
            if ( consumirToken(COMENTARIO_LINEA) ) return 
        else
            call parse_ControlType()
            if (.not. consumirToken(IDENTIFICADOR)) call modoPanico(';')
            if (.not. consumirToken(SIGNO_PUNTO_Y_COMA)) call modoPanico(';')
        end if
    end subroutine parse_Control

    ! Producción ControlType ::= RESERVADA_ETIQUETA | RESERVADA_BOTON | ...
    subroutine parse_ControlType()
        select case (tokens(pos)%tipo)
            case (RESERVADA_ETIQUETA)
                if (.not. consumirToken(RESERVADA_ETIQUETA)) call modoPanico(';')
            case (RESERVADA_BOTON)
                if (.not. consumirToken(RESERVADA_BOTON)) call modoPanico(';')
            case (RESERVADA_CHECK)
                if (.not. consumirToken(RESERVADA_CHECK)) call modoPanico(';')
            case (RESERVADA_RADIOBOTON)
                if (.not. consumirToken(RESERVADA_RADIOBOTON)) call modoPanico(';')
            case (RESERVADA_TEXTO)
                if (.not. consumirToken(RESERVADA_TEXTO)) call modoPanico(';')
            case (RESERVADA_AREATEXTO)
                if (.not. consumirToken(RESERVADA_AREATEXTO)) call modoPanico(';')
            case (RESERVADA_CLAVE)
                if (.not. consumirToken(RESERVADA_CLAVE)) call modoPanico(';')
            case (RESERVADA_CONTENEDOR)
                if (.not. consumirToken(RESERVADA_CONTENEDOR)) call modoPanico(';')
            case default
                print *, "Error sintactico: Tipo de control no valido."
                call modoPanico(';')
        end select
    end subroutine parse_ControlType

    ! Función para consumir tokens
    logical function consumirToken(tipoEsperado)
        integer, intent(in) :: tipoEsperado
        if (pos > size(tokens)) then
            consumirToken = .false.
            print *, "Error sintactico: Fin de tokens inesperado."
            return
        end if
        if (tokens(pos)%tipo == tipoEsperado) then
            pos = pos + 1
            consumirToken = .true.
        else
            consumirToken = .false.
            print *, "Error sintactico: Se esperaba el token ", trim(getTipoTokenEnString(tipoEsperado)), &
                     ", pero se encontro ", trim(getTipoTokenEnString(tokens(pos)%tipo))
        end if
    end function consumirToken

    ! Función para verificar si el siguiente token es un tipo válido de control
    logical function esControlValido()
        esControlValido = (pos <= size(tokens)) .and. &
                          ((tokens(pos)%tipo == RESERVADA_ETIQUETA) .or. &
                           (tokens(pos)%tipo == RESERVADA_BOTON) .or. &
                           (tokens(pos)%tipo == RESERVADA_CHECK) .or. &
                           (tokens(pos)%tipo == RESERVADA_RADIOBOTON) .or. &
                           (tokens(pos)%tipo == RESERVADA_TEXTO) .or. &
                           (tokens(pos)%tipo == RESERVADA_AREATEXTO) .or. &
                           (tokens(pos)%tipo == RESERVADA_CLAVE) .or. &
                           (tokens(pos)%tipo == RESERVADA_CONTENEDOR) .or. &
                           (tokens(pos)%tipo == COMENTARIO_LINEA))
    end function esControlValido

    ! Procedimiento Modo Pánico: busca el token de sincronización (';')
    subroutine modoPanico(tokenSincronizacion)
        character(len=*), intent(in) :: tokenSincronizacion
        print *, "Recuperacion de error en Modo Panico. Buscando ';'..."

        ! Saltar tokens hasta encontrar el token de sincronización (en este caso ';')
        do while (pos <= size(tokens) .and. trim(getTipoTokenEnString(tokens(pos)%tipo)) /= tokenSincronizacion)
            pos = pos + 1
        end do

        ! Una vez que se encuentra ';', avanzar para continuar el análisis
        if (pos <= size(tokens)) then
            print *, "Recuperacion completada, ';' encontrado."
            pos = pos + 1
        else
            print *, "Fin de tokens alcanzado durante la recuperacion en Modo Panico."
        end if
    end subroutine modoPanico

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
            res = ";"
        case (SIGNO_GUION)
            res = "SIGNO_GUION"
        case (COMENTARIO_LINEA)
            res = "COMENTARIO_LINEA"
        case default
            res = "Desconocido"
        end select
    end function getTipoTokenEnString

end module mod_analizador_sintactico
