**==varian.f    processed by SPAG 4.03F  at 09:54 on  1 Mar 1994
      DOUBLE PRECISION FUNCTION varian(X,I,J,Iopt)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      DOUBLE PRECISION ave,X
      INTEGER I,Iopt,J,k
C*** End of declarations inserted by SPAG
C --- THIS FUNCTION COMPUTES THE VARIANCE OF X. IF IOPT = 0 COMPUTE THE
C --- MEAN , IF IOPT = 1 THE MEAN IS ASSUMED TO BE ZERO, AND IF IOPT = 2
C --- THE MEAN IS ASSUMED TO BE ONE.
      DIMENSION X(*)
      ave=1D0
      IF(Iopt.ne.2)THEN
       ave=0D0
       IF(Iopt.ne.1)THEN
        DO k=I,J
         ave=ave+X(k)
        END DO
        ave=ave/(J-I+1)
       END IF
      END IF
      varian=0D0
      DO k=I,J
       varian=varian+(X(k)-ave)*(X(k)-ave)
      END DO
      RETURN
      END
