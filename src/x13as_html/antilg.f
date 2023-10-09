C     Last change:  BCM  27 Apr 1998    7:27 am
      SUBROUTINE antilg(X,I,J)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INTEGER I,J,k
      DOUBLE PRECISION tmp,X
      DIMENSION X(J)
c-----------------------------------------------------------------------
      DO k=I,J
       tmp=X(k)
       X(k)=exp(tmp)
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
