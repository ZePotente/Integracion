PROGRAM INTEGRAL
    !Modulo
    USE VYM_IO
    
    IMPLICIT NONE
    INTEGER :: BANDERA !Global.
    INTEGER :: N
    REAL(8) :: H
    REAL(8), DIMENSION(:), ALLOCATABLE :: X, Y
    PRINT *, 'Leyendo los puntos dato.'
    CALL LEER_VALORES(X, Y)
    IF (BANDERA == 1) THEN; PRINT *, 'Error de lectura.'; GOTO 20; END IF;
    PRINT *, 'Valores de X: '
    CALL VEC_MOSTRAR(X)
    PRINT *, 'Valores de Y: '
    CALL VEC_MOSTRAR(Y)
    
    N = SIZE(Y)
    H = INTEG_CALC_H(X, N)
    
    
    
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

    FUNCTION INTEG_CALC_H(X, N)
        REAL(8) :: INTEG_CALC_H
        REAL(8), INTENT(IN) :: X(:)
        INTEGER, INTENT(IN) :: N
        !
        
        IF (N >= 2) THEN
            IF (MANUAL .EQV. .TRUE.) THEN
                H = (X(N) - X(1)) / (N-1) 
            ELSE
                H = 0.1
            END IF
        ELSE
            PRINT *, 'Estás queriendo calcular la integral de un punto (o menos).'
            GOTO 20 !Ya sé que no es conveniente que salte desde la función al final del programa.
        END IF
    END FUNCTION
END PROGRAM
