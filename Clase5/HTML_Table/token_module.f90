module token_module
    implicit none
    type :: Token
        integer :: numero
        character(len=50) :: lexema
        character(len=100) :: descripcion
        integer :: linea
        integer :: columna
    end type Token
end module token_module
