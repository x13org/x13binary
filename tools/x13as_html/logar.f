C     Last change:  BCM  26 Apr 1998    2:46 pm
**==logar.f    processed by SPAG 4.03F  at 09:51 on  1 Mar 1994
      SUBROUTINE logar(X,I,J)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      DOUBLE PRECISION tmp,X
      INTEGER I,J,k
C*** End of declarations inserted by SPAG
      DIMENSION X(J)
      DO k=I,J
       tmp=X(k)
       X(k)=dlog(tmp)
      END DO
      RETURN
      END

