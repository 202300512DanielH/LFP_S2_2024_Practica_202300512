MODULE InventarioMod
    IMPLICIT NONE
    INTEGER :: numEquipos = 0
    CHARACTER(LEN=15), ALLOCATABLE :: nombres(:), ubicaciones(:)
    INTEGER, ALLOCATABLE :: cantidades(:)
    REAL, ALLOCATABLE :: precios(:)
    LOGICAL :: inventarioCargado = .FALSE.
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

    SUBROUTINE ExtraerTokensMov(linea, accion, nombre, cantidad, ubicacion)
        IMPLICIT NONE
        CHARACTER(LEN=100), INTENT(IN) :: linea
        CHARACTER(LEN=20), INTENT(OUT) :: accion, nombre, ubicacion
        INTEGER, INTENT(OUT) :: cantidad
        CHARACTER(LEN=100) :: tmp_linea
        INTEGER :: pos1, pos2

        ! Extraer acción
        pos1 = INDEX(linea, ' ')
        accion = linea(1:pos1-1)
        tmp_linea = ADJUSTL(linea(pos1+1:))

        ! Extraer nombre
        pos1 = INDEX(tmp_linea, ';')
        nombre = tmp_linea(1:pos1-1)
        tmp_linea = tmp_linea(pos1+1:)

        ! Extraer cantidad
        pos1 = INDEX(tmp_linea, ';')
        READ(tmp_linea(1:pos1-1), *) cantidad
        tmp_linea = tmp_linea(pos1+1:)

        ! Extraer ubicación
        ubicacion = tmp_linea
    END SUBROUTINE ExtraerTokensMov

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
    INTEGER :: i, newSize
    INTEGER :: unidad_archivo

    ! Verificar si el inventario ya ha sido cargado
    IF (inventarioCargado) THEN
        PRINT *, 'Ya has cargado un inventario, si quieres cargar otro, vuelve a iniciar el programa.'
        RETURN
    END IF

    numEquipos = 0
    newSize = 10

    ! Desasignar arrays si ya están asignados
    IF (ALLOCATED(nombres)) THEN
        DEALLOCATE(nombres)
    END IF
    IF (ALLOCATED(ubicaciones)) THEN
        DEALLOCATE(ubicaciones)
    END IF
    IF (ALLOCATED(cantidades)) THEN
        DEALLOCATE(cantidades)
    END IF
    IF (ALLOCATED(precios)) THEN
        DEALLOCATE(precios)
    END IF

    ! Asignar memoria a los arrays
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

    ! Marcar el inventario como cargado
    inventarioCargado = .TRUE.

    PRINT *, 'Inventario cargado con exito.'
    PRINT *, 'Numero de equipos cargados: ', numEquipos
    PRINT *, '---------------------------------'

    ! Mostrar los datos cargados (solo para verificar)
    DO i = 1, numEquipos
        PRINT *, 'Equipo: ', nombres(i), 'Cantidad: ', cantidades(i), 'Precio: ', precios(i), 'Ubicacion: ', ubicaciones(i)
    END DO
END SUBROUTINE CargarInventario

SUBROUTINE CargarMovimientos()
    USE InventarioMod
    IMPLICIT NONE
    CHARACTER(LEN=100) :: linea
    CHARACTER(LEN=20) :: accion, nombre, ubicacion
    INTEGER :: cantidad, ios
    INTEGER :: i, encontrado
    INTEGER :: unidad_archivo

    unidad_archivo = 20

    ! Abre el archivo .mov
    OPEN(unit=unidad_archivo, file='instrucciones.mov', status='old', action='read', iostat=ios)
    IF (ios /= 0) THEN
        PRINT *, 'Error al abrir el archivo movimientos.mov'
        RETURN
    END IF

    ! Leer cada línea del archivo
    DO
        READ(unidad_archivo, '(A)', IOSTAT=ios) linea
        IF (ios /= 0) EXIT

        ! Extraer los tokens de la línea
        CALL ExtraerTokensMov(linea, accion, nombre, cantidad, ubicacion)

        ! Procesar la acción
        IF (accion == 'agregar_stock') THEN
            encontrado = 0
            DO i = 1, numEquipos
                IF (TRIM(nombres(i)) == TRIM(nombre) .AND. TRIM(ubicaciones(i)) == TRIM(ubicacion)) THEN
                    cantidades(i) = cantidades(i) + cantidad
                    encontrado = 1
                    EXIT
                END IF
            END DO
            IF (encontrado == 0) THEN
                PRINT *, 'Error: El equipo ', TRIM(nombre), ' no existe en la ubicacion ', TRIM(ubicacion)
            END IF
        ELSE IF (accion == 'eliminar_equipo') THEN
            encontrado = 0
            DO i = 1, numEquipos
                IF (TRIM(nombres(i)) == TRIM(nombre) .AND. TRIM(ubicaciones(i)) == TRIM(ubicacion)) THEN
                    IF (cantidades(i) >= cantidad) THEN
                        cantidades(i) = cantidades(i) - cantidad
                    ELSE
                        PRINT *, 'Error: No hay suficiente cantidad de ', TRIM(nombre), ' en la ubicacion ', TRIM(ubicacion)
                    END IF
                    encontrado = 1
                    EXIT
                END IF
            END DO
            IF (encontrado == 0) THEN
                PRINT *, 'Error: El equipo ', TRIM(nombre), ' no existe en la ubicacion ', TRIM(ubicacion)
            END IF
        ELSE
            PRINT *, 'Error: Acción desconocida ', TRIM(accion)
        END IF
    END DO

    ! Cierra el archivo
    CLOSE(unidad_archivo)

    PRINT *, 'Movimientos de inventario cargados con exito.'
END SUBROUTINE CargarMovimientos

SUBROUTINE CrearInforme()
    USE InventarioMod
    IMPLICIT NONE
    INTEGER :: i, unit
    REAL :: valor_total

    ! Abrir archivo para escribir
    OPEN(UNIT=unit, FILE='Reporte.txt', STATUS='REPLACE', ACTION='WRITE')

    ! Escribir encabezados
    WRITE(unit, '(A20, A10, A15, A15, A20)') 'Equipo', 'Cantidad', 'Precio U.', 'Valor Total', 'Ubicación'

    ! Escribir datos del inventario
    DO i = 1, numEquipos
        valor_total = cantidades(i) * precios(i)
        WRITE(unit, '(A20, I10, F15.2, F15.2, A20)') TRIM(nombres(i)), cantidades(i), precios(i), valor_total, TRIM(ubicaciones(i))
    END DO

    ! Cerrar archivo
    CLOSE(unit)

    PRINT *, 'Informe de inventario creado en archivo.txt'
END SUBROUTINE CrearInforme

