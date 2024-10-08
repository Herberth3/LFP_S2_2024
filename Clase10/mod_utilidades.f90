module mod_utilidades
    implicit none
    private
    public :: leerArchivo

contains
    ! Subrutina para leer un archivo y devolver su contenido
    subroutine leerArchivo(nombreArchivo, cadena, ios)
        character(len=*), intent(in) :: nombreArchivo
        character(len=350), intent(out) :: cadena
        integer, intent(out) :: ios
        integer :: iunit
        character(len=100) :: temp_line

        iunit = 10
        cadena = ''

        ! Abrir el archivo
        open(unit=iunit, file=trim(nombreArchivo), status='old', iostat=ios)
        ! Si no se pudo abrir, mostrar un mensaje de error
        if (ios /= 0) then
            print *, "Error al abrir el archivo."
            cadena = ''
            return
        end if

        ! Leer el contenido del archivo
        do
            read(iunit, '(A)', iostat=ios) temp_line
            if (ios /= 0) exit
            cadena = trim(cadena) // trim(temp_line) // char(10)
        end do
        ios = 0

        ! Cerrar el archivo
        close(iunit)
    end subroutine leerArchivo
end module mod_utilidades
