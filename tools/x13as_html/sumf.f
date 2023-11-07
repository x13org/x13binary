**==sumf.f    processed by SPAG 4.03F  at 09:54 on  1 Mar 1994
      DOUBLE PRECISION FUNCTION sumf(X,N1,N2)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      INTEGER i,N1,N2
      DOUBLE PRECISION X
C*** End of declarations inserted by SPAG
C     COMMON SUBROUTINE
      DIMENSION X(*)
      sumf=0D0
      DO i=N1,N2
       sumf=sumf+X(i)
      END DO
      RETURN
      END
