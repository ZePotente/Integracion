PROGRAM INTEGRAL
    !Modulo
    USE VYM_IO
    USE INTEGRAL_MET
    
    IMPLICIT NONE
    INTEGER :: BANDERA !Global.
    INTEGER :: N, OPCION
    REAL(8) :: H, VAL_INT
    REAL(8), DIMENSION(:), ALLOCATABLE :: X, Y
    PRINT *, 'Leyendo los puntos dato.'
    CALL LEER_VALORES(X, Y)
    IF (BANDERA == 1) THEN; PRINT *, 'Error de lectura.'; GOTO 20; END IF;
    
    N = SIZE(Y)
    H = INTEG_CALC_H(X, N)
    IF (BANDERA == 1) GOTO 20
    PRINT *, LOG(4.)/log(2.)
    
    CALL MOSTRAR_DATOS(X, Y, H, N)
    PRINT *, 'Presione enter para avanzar al menu de opciones (Se borrará la pantalla).'
    READ(*,*)
    CALL SYSTEM("clear")
    30  PRINT *, 'N (cantidad de puntos) = ', N
        CALL MOSTRAR_MENU()
        PRINT *, 'Recordatorio de verificar H en la funcion INTEG_CALC_H'
        READ(*,*) OPCION
        SELECT CASE (OPCION)
            CASE(1)
                IF (COND_TRAPECIOS(N)) THEN
                    VAL_INT = INTEGRAL_TRAPECIOS(Y, N, H)
                    CALL MOSTRAR_RESULTADO(VAL_INT)
                ELSE
                    PRINT *, 'La cantidad de puntos no es la necesaria.'
                END IF
            CASE(2)
                IF (COND_SIMPSON13(N)) THEN
                    VAL_INT = INTEGRAL_SIMPSON13(Y, N, H)
                    CALL MOSTRAR_RESULTADO(VAL_INT)
                ELSE
                    PRINT *, 'La cantidad de puntos no es la necesaria.'
                END IF
            CASE(3)
                IF (COND_SIMPSON38(N)) THEN
                    VAL_INT = INTEGRAL_SIMPSON38(Y, N, H)
                    CALL MOSTRAR_RESULTADO(VAL_INT)
                ELSE
                    PRINT *, 'La cantidad de puntos no es la necesaria.'
                END IF
            CASE(4)
                IF (COND_ROMBERG(N)) THEN
                    VAL_INT = INTEGRAL_ROMBERG(Y, N, H)
                    CALL MOSTRAR_RESULTADO(VAL_INT)
                ELSE
                    PRINT *, 'La cantidad de puntos no es la necesaria.'
                END IF
            CASE(0)
                PRINT *, 'Se seleccionó finalizar el programa.'
        END SELECT
        PRINT *, 'Procedimiento finalizado.'
        PRINT *, 'Presione enter para avanzar (Se borrará la pantalla).'
        READ(*,*)
        CALL SYSTEM("clear")
    IF (OPCION /= 0) GOTO 30
20  PRINT *, 'Fin del programa.'
CONTAINS
    SUBROUTINE LEER_VALORES(X,Y)
        REAL(8), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: X, Y
        !
        REAL(8), DIMENSION(:,:), ALLOCATABLE :: XY
        INTEGER :: N
        CALL MAT_LEER(XY, BANDERA, ARCHIVO = 'Puntos.txt')
        IF (BANDERA == 1) GOTO 10
        N = SIZE(XY,1)
        !PRINT *, 'N = ', N
        ALLOCATE(X(N), Y(N))
        X(:) = XY(:,1)
        Y(:) = XY(:,2)
        
    10  IF (ALLOCATED(X)) DEALLOCATE(XY)
    END SUBROUTINE

    SUBROUTINE MOSTRAR_DATOS(X, Y, H, N)
        REAL(8), INTENT(IN) :: X(:), Y(:), H
        INTEGER, INTENT(IN) :: N
        !
        PRINT *, 'Valores de X: '
        CALL VEC_MOSTRAR(X)
        PRINT *, 'Valores de Y: '
        CALL VEC_MOSTRAR(Y)
        WRITE(*,'(A F15.7)') ' H = ', H
        PRINT *, 'N (cantidad de puntos) = ', N

    END SUBROUTINE
    
    SUBROUTINE MOSTRAR_MENU()
        
        PRINT *, 'Métodos de Newton-Cotes'
        PRINT *, '1- Método de los Trapecios'
        PRINT *, '2- Método de 1/3 de Simpson'
        PRINT *, '3- Método de 3/8 de Simpson'
        
        PRINT *, 'Otro'
        PRINT *, '4- Método de Romberg'
        
        PRINT *, '0- Salir'
    END SUBROUTINE
    
    SUBROUTINE MOSTRAR_RESULTADO(RES)
        REAL(8), INTENT(IN) :: RES
        
        WRITE(*,'(A, F15.7)') 'El resultado de la integral es: ', RES
    END SUBROUTINE
    
    FUNCTION INTEG_CALC_H(X, N)
        REAL(8) :: INTEG_CALC_H
        REAL(8), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: N
        !
        
        IF (N >= 2) THEN
!            INTEG_CALC_H = 0.2
            INTEG_CALC_H = (X(N) - X(1)) / (N-1)
        ELSE
            BANDERA = 1
            PRINT *, 'Estás queriendo calcular la integral de un punto (o menos).'
        END IF
    END FUNCTION
END PROGRAM
