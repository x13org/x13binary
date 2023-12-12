      DOUBLE PRECISION FUNCTION vars(X,I,J,Iopt,Muladd)
      IMPLICIT NONE
C-----------------------------------------------------------------------
      INTEGER I,Iopt,J,Muladd
      DOUBLE PRECISION varian,varlog,X
      DIMENSION X(*)
C-----------------------------------------------------------------------
      IF(Muladd.ne.1)THEN
       vars=varlog(X,I,J,Iopt)
       RETURN
      END IF
      vars=varian(X,I,J,Iopt)
      RETURN
      END
