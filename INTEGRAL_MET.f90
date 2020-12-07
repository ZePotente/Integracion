MODULE INTEGRAL_MET
    IMPLICIT NONE
CONTAINS
    FUNCTION INTEGRAL_TRAPECIOS(Y, N, H)
        REAL(8) :: INTEGRAL_TRAPECIOS
        REAL(8), INTENT(IN) :: H, Y(:)
        INTEGER, INTENT(IN) :: N
        !
        INTEGRAL_TRAPECIOS = H*(Y(1) + 2.*SUM(Y(2:N-1)) + Y(N))/2.0
    END FUNCTION
    
    FUNCTION INTEGRAL_SIMPSON13(Y, N, H)
        REAL(8) :: INTEGRAL_SIMPSON13
        REAL(8), INTENT(IN) :: H, Y(:)
        INTEGER, INTENT(IN) :: N
        !
        INTEGRAL_SIMPSON13 = (H/3.)*(Y(1) + 4*SUM(Y(2:N-1:2)) + 2*SUM(Y(3:N-2:2)) + Y(N))
    END FUNCTION
    
    FUNCTION INTEGRAL_SIMPSON38(Y, N, H)
        REAL(8) :: INTEGRAL_SIMPSON38
        REAL(8), INTENT(IN) :: H, Y(:)
        INTEGER, INTENT(IN) :: N
        !
        INTEGRAL_SIMPSON38 = (3./8.)*H* (Y(1) + 3*SUM(Y(2:N-3:3)) + 3*SUM(Y(3:N-2:3)) + 2*SUM(Y(4:N-1:3)) + Y(N))
    END FUNCTION
    
    FUNCTION INTEGRAL_ROMBERG(Y, N, H)
        REAL(8) :: INTEGRAL_ROMBERG
        REAL(8), INTENT(IN) :: H, Y(:)
        INTEGER, INTENT(IN) :: N
        !
        REAL(8), DIMENSION(:), ALLOCATABLE :: T
        INTEGER :: I, J, NT, NY, PASO
        
        PASO = N-1
        NT = LOG(REAL(PASO))/LOG(2.)
        ALLOCATE(T(0:NT))
        
        DO I = 0, NT
            T(I) = INTEGRAL_TRAPECIOS(Y(::PASO), INT(2.**I) + 1, PASO*H)
            PASO = PASO/2
        END DO
        
        NY = NT-1
        DO J = 2, NT+1
            DO I = 0, NY
                T(I) = (4**(J-1)*T(I+1) - T(I)) / (4**(J-1) - 1)
            END DO
            NY = NY - 1
        END DO
        
        INTEGRAL_ROMBERG = T(0)
    END FUNCTION
END MODULE
