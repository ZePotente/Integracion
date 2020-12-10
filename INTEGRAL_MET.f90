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
        REAL(8) :: SUMA
        INTEGRAL_SIMPSON38 = (3./8.)*H* (Y(1) + 3*SUM(Y(2:N-3:3)) + 3*SUM(Y(3:N-2:3)) + 2*SUM(Y(4:N-4:3)) + Y(N))
        
        SUMA = CALCULAR(Y, N)
        INTEGRAL_SIMPSON38 = (3./8.)*H* SUMA
    END FUNCTION
    
    FUNCTION CALCULAR(Y, N)
        REAL(8) :: CALCULAR
        REAL(8), DIMENSION(:), INTENT(IN) :: Y
        INTEGER, INTENT(IN) :: N
        !
        REAL(8) :: SUMA
        INTEGER :: I
        
        SUMA = Y(1)
        DO I = 1, (N-1)/3
            SUMA = SUMA + 3.*Y(3*I-1) + 3.*Y(3*I)
!            PRINT *, 'Y(2+3I)', Y(3*I-1)
!            PRINT *, 'Y(3+3I)', Y(3*I)
        END DO
        
        DO I = 1, ((N-1)/3) - 1
            SUMA = SUMA + 2.*Y(3*I+1)
!            PRINT *, 'Y(3I)', Y(3*I+1)
        END DO
        SUMA = SUMA + Y(N)
        CALCULAR = SUMA
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
    
    !---Condiciones sobre N para cada método---!
    
    FUNCTION COND_TRAPECIOS(N)
        LOGICAL :: COND_TRAPECIOS
        INTEGER, INTENT(IN) :: N
        COND_TRAPECIOS = N >= 2
    END FUNCTION
    
    FUNCTION COND_SIMPSON13(N)
        LOGICAL :: COND_SIMPSON13
        INTEGER, INTENT(IN) :: N
        COND_SIMPSON13 = (MOD(N-2,2) == 1)    !n = 2k+1  con k > 0
    END FUNCTION
    
    FUNCTION COND_SIMPSON38(N)
        LOGICAL :: COND_SIMPSON38
        INTEGER, INTENT(IN) :: N
        COND_SIMPSON38 = (MOD(N-3,3) == 1)    !n = 3k+1  con k > 0
    END FUNCTION
    
    FUNCTION COND_ROMBERG(N)
        LOGICAL :: COND_ROMBERG
        INTEGER, INTENT(IN) :: N
        COND_ROMBERG = (N == 2 .OR. N == 3 .OR. N == 5 .OR. N == 9 .OR. N == 17 .OR. N == 33) !n = 2^k + 1, hardcodeado porque no tengo ganas de complicarme.
    END FUNCTION
    
    
END MODULE
