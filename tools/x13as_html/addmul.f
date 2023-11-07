      SUBROUTINE addmul(Z,X,Y,Ib,Ie)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      INCLUDE 'srslen.prm'
      INCLUDE 'x11opt.cmn'
      INTEGER i,Ib,Ie
      DOUBLE PRECISION X,Y,Z
C*** End of declarations inserted by SPAG
C --- THIS SUBROUTINE MULTIPLIES (ADDS) SERIES X TO Y AND STORES THE
C --- RESULT IN Z.
      DIMENSION X(Ie),Y(Ie),Z(Ie)
      IF(Muladd.ne.0)THEN
       DO i=Ib,Ie
        Z(i)=X(i)+Y(i)
       END DO
       RETURN
      END IF
      DO i=Ib,Ie
       Z(i)=X(i)*Y(i)
      END DO
      RETURN
      END
