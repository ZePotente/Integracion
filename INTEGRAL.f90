PROGRAM INTEGRAL
    !Modulo
    USE VYM_IO
    
    IMPLICIT NONE
    REAL(8), DIMENSION(:), ALLOCATABLE :: X, Y
    INTEGER :: BANDERA
    
    PRINT *, 'Leyendo los puntos dato.'
    CALL LEER_VALORES(X, Y)
    PRINT *, 'Valores de X: '
    CALL VEC_MOSTRAR(X)
    PRINT *, 'Valores de Y: '
    CALL VEC_MOSTRAR(Y)
    
    
CONTAINS
    SUBROUTINE LEER_VALORES(X,Y)
        REAL(8), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: X, Y
        !
        REAL(8), DIMENSION(:,:), ALLOCATABLE :: XY
        INTEGER :: N
        CALL MAT_LECTURA(XY, BANDERA, 'Puntos.txt')
        N = SIZE(XY,1)
        PRINT *, 'N = ', N
        ALLOCATE(X(N), Y(N))
        X(:) = XY(:,1)
        Y(:) = XY(:,2)
        
        IF(ALLOCATED(XY)) DEALLOCATE(XY)
    END SUBROUTINE

END PROGRAM
