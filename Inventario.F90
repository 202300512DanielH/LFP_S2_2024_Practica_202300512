MODULE InventarioMod
    IMPLICIT NONE
    CONTAINS

    SUBROUTINE RedimensionarArrays(nombres, ubicaciones, cantidades, precios, newSize)
        IMPLICIT NONE
        CHARACTER(LEN=15), ALLOCATABLE, INTENT(INOUT) :: nombres(:), ubicaciones(:)
        INTEGER, ALLOCATABLE, INTENT(INOUT) :: cantidades(:)
        REAL, ALLOCATABLE, INTENT(INOUT) :: precios(:)
        INTEGER, INTENT(IN) :: newSize
        CHARACTER(LEN=15), ALLOCATABLE :: tmpNombres(:), tmpUbicaciones(:)
        INTEGER, ALLOCATABLE :: tmpCantidades(:)
        REAL, ALLOCATABLE :: tmpPrecios(:)

        ALLOCATE(tmpNombres(newSize), tmpUbicaciones(newSize), tmpCantidades(newSize), tmpPrecios(newSize))
        tmpNombres(1:SIZE(nombres)) = nombres
        tmpUbicaciones(1:SIZE(ubicaciones)) = ubicaciones
        tmpCantidades(1:SIZE(cantidades)) = cantidades
        tmpPrecios(1:SIZE(precios)) = precios

        DEALLOCATE(nombres, ubicaciones, cantidades, precios)
        nombres = tmpNombres
        ubicaciones = tmpUbicaciones
        cantidades = tmpCantidades
        precios = tmpPrecios
    END SUBROUTINE RedimensionarArrays

    SUBROUTINE ExtraerTokens(linea, nombre, cantidad, precio_unitario, ubicacion)
        IMPLICIT NONE
        CHARACTER(LEN=100), INTENT(IN) :: linea
        CHARACTER(LEN=20), INTENT(OUT) :: nombre, ubicacion
        INTEGER, INTENT(OUT) :: cantidad
        REAL, INTENT(OUT) :: precio_unitario
        CHARACTER(LEN=100) :: tmp_linea
        INTEGER :: pos1, pos2

        ! Eliminar la parte "crear_equipo "
        tmp_linea = linea
        tmp_linea = ADJUSTL(tmp_linea(13:))

        ! Extraer nombre
        pos1 = INDEX(tmp_linea, ';')
        nombre = tmp_linea(1:pos1-1)
        tmp_linea = tmp_linea(pos1+1:)

        ! Extraer cantidad
        pos1 = INDEX(tmp_linea, ';')
        READ(tmp_linea(1:pos1-1), *) cantidad
        tmp_linea = tmp_linea(pos1+1:)

        ! Extraer precio unitario
        pos1 = INDEX(tmp_linea, ';')
        READ(tmp_linea(1:pos1-1), *) precio_unitario
        tmp_linea = tmp_linea(pos1+1:)

        ! Extraer ubicación
        ubicacion = tmp_linea
    END SUBROUTINE ExtraerTokens

END MODULE InventarioMod

PROGRAM Inventario
    USE InventarioMod
    IMPLICIT NONE
    INTEGER :: opcion

    DO
        CALL MostrarMenu()
        READ(*,*) opcion

        SELECT CASE (opcion)
            CASE (1)
                CALL CargarInventario()
            CASE (2)
                CALL CargarMovimientos()
            CASE (3)
                CALL CrearInforme()
            CASE (4)
                PRINT *, 'Saliendo del programa...'
                EXIT
            CASE DEFAULT
                PRINT *, 'Opcion no válida. Intente de nuevo.'
        END SELECT
    END DO
END PROGRAM Inventario

SUBROUTINE MostrarMenu()
    PRINT *, '---------------------------------'
    PRINT *, 'Practica 1 - Lenguajes Formales y de Programacion - Inventario'
    PRINT *, '---------------------------------'
    PRINT *, 'Menu Principal'
    PRINT *, '1. Cargar Inventario Inicial'
    PRINT *, '2. Cargar Instrucciones de Movimientos'
    PRINT *, '3. Crear Informe de Inventario'
    PRINT *, '4. Salir'
    PRINT *, 'Seleccione una opcion: '
END SUBROUTINE MostrarMenu

SUBROUTINE CargarInventario()
    USE InventarioMod
    IMPLICIT NONE
    CHARACTER(LEN=100) :: linea
    CHARACTER(LEN=20) :: nombre, ubicacion
    INTEGER :: cantidad, ios
    REAL :: precio_unitario
    INTEGER :: i, numEquipos, newSize
    CHARACTER(LEN=15), ALLOCATABLE :: nombres(:), ubicaciones(:)
    INTEGER, ALLOCATABLE :: cantidades(:)
    REAL, ALLOCATABLE :: precios(:)
    INTEGER :: unidad_archivo

    numEquipos = 0
    newSize = 10
    ALLOCATE(nombres(newSize), ubicaciones(newSize), cantidades(newSize), precios(newSize))

    unidad_archivo = 10

    ! Abre el archivo .inv
    OPEN(unit=unidad_archivo, file='inventario.inv', status='old', action='read', iostat=ios)
    IF (ios /= 0) THEN
        PRINT *, 'Error al abrir el archivo inventario.inv'
        RETURN
    END IF

    ! Leer cada línea del archivo
    DO
        READ(unidad_archivo, '(A)', IOSTAT=ios) linea
        IF (ios /= 0) EXIT

        ! Separar los tokens en la línea
        CALL ExtraerTokens(linea, nombre, cantidad, precio_unitario, ubicacion)

        ! Redimensionar arrays si es necesario
        IF (numEquipos >= SIZE(nombres)) THEN
            newSize = newSize * 2
            CALL RedimensionarArrays(nombres, ubicaciones, cantidades, precios, newSize)
        END IF

        ! Almacenar los datos en arrays
        numEquipos = numEquipos + 1
        nombres(numEquipos) = nombre
        cantidades(numEquipos) = cantidad
        precios(numEquipos) = precio_unitario
        ubicaciones(numEquipos) = ubicacion
    END DO

    ! Cierra el archivo
    CLOSE(unidad_archivo)

    PRINT *, 'Inventario cargado con exito.'
    PRINT *, 'Numero de equipos cargados: ', numEquipos
    PRINT *, '---------------------------------'

    ! Mostrar los datos cargados (solo para verificar)
    DO i = 1, numEquipos
        PRINT *, 'Equipo: ', nombres(i), 'Cantidad: ', cantidades(i), 'Precio: ', precios(i), 'Ubicacion: ', ubicaciones(i)
    END DO
END SUBROUTINE CargarInventario

SUBROUTINE CargarMovimientos()
    ! Código para cargar movimientos de inventario
    PRINT *, 'Cargando movimientos de inventario...'
END SUBROUTINE CargarMovimientos

SUBROUTINE CrearInforme()
    ! Código para crear informe de inventario
    PRINT *, 'Creando informe de inventario...'
END SUBROUTINE CrearInforme
