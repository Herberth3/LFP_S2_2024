module mod_tokens
    implicit none
    private
    public :: token, RESERVADA_CONTROLES, IDENTIFICADOR, SIGNO_MENOR_QUE, SIGNO_MAYOR_QUE, SIGNO_ADMIRACION_C, SIGNO_PUNTO_Y_COMA, SIGNO_GUION, COMENTARIO_LINEA &
        , RESERVADA_ETIQUETA, RESERVADA_BOTON, RESERVADA_CHECK, RESERVADA_RADIOBOTON, RESERVADA_TEXTO &
        , RESERVADA_AREATEXTO, RESERVADA_CLAVE, RESERVADA_CONTENEDOR

    type :: token
        character(len=5000) :: valor
        integer :: tipo
    end type token

    integer, parameter :: RESERVADA_CONTROLES = 1
    integer, parameter :: IDENTIFICADOR = 2
    integer, parameter :: SIGNO_MENOR_QUE = 3
    integer, parameter :: SIGNO_MAYOR_QUE = 4
    integer, parameter :: SIGNO_ADMIRACION_C = 5
    integer, parameter :: SIGNO_PUNTO_Y_COMA = 6
    integer, parameter :: SIGNO_GUION = 7
    integer, parameter :: COMENTARIO_LINEA = 8
    integer, parameter :: RESERVADA_ETIQUETA = 9
    integer, parameter :: RESERVADA_BOTON = 10
    integer, parameter :: RESERVADA_CHECK = 11
    integer, parameter :: RESERVADA_RADIOBOTON = 12
    integer, parameter :: RESERVADA_TEXTO = 13
    integer, parameter :: RESERVADA_AREATEXTO = 14
    integer, parameter :: RESERVADA_CLAVE = 15
    integer, parameter :: RESERVADA_CONTENEDOR = 16

end module mod_tokens
