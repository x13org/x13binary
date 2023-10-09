      SUBROUTINE change(X,Y,Ib,Ie)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c --- this subroutine calculates the percent changes (differences) in
c --- x and stores them in y.
c     ------------------------------------------------------------------
c     Set Gudval in separate routine - BCM March 2006
c     ------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'goodob.cmn'
c-----------------------------------------------------------------------
      INTEGER i,Ib,Ie
      DOUBLE PRECISION X,Y
      DIMENSION X(PLEN),Y(PLEN)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c     ------------------------------------------------------------------
      IF(Muladd.ne.1)THEN
       DO i=Ib,Ie
        IF(Gudval(i-1))THEN
         Y(i)=(X(i)-X(i-1))/X(i-1)
        ELSE
         Y(i)=DNOTST
        END IF
       END DO
       RETURN
      END IF
      DO i=Ib,Ie
       Y(i)=X(i)-X(i-1)
      END DO
      RETURN
      END
