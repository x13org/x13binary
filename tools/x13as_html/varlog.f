      DOUBLE PRECISION FUNCTION varlog(X,I,J,Iopt)
      IMPLICIT NONE
c     ------------------------------------------------------------------
C --- THIS FUNCTION COMPUTES THE LOG VARIANCE OF X. IF IOPT IS EQUAL
C --- TO 1 THE LOG MEAN IS ASSUMED TO BE 1.
c     ------------------------------------------------------------------
      DOUBLE PRECISION ZERO,ONE
      PARAMETER(ZERO=0D0,ONE=1D0)
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'goodob.cmn'
c     ------------------------------------------------------------------
      DOUBLE PRECISION tmp,tmp2,X,numtmp
      INTEGER I,Iopt,J,k
      DIMENSION X(*)
c     ------------------------------------------------------------------
      tmp=ZERO
      numtmp=ZERO
      IF(Iopt.ne.1)THEN
       DO k=I,J
        IF(Gudval(k).and.X(k).gt.ZERO)THEN
         tmp=tmp+dlog(X(k))
         numtmp=numtmp+ONE
        END IF
       END DO
       IF(numtmp.gt.ZERO)THEN
        tmp=tmp/numtmp
       ELSE
        varlog=DNOTST
        RETURN
       END IF
      END IF
c     ------------------------------------------------------------------
      varlog=ZERO
      DO k=I,J
       IF(Gudval(k).and.X(k).gt.ZERO)THEN
        tmp2=dlog(X(k))-tmp
        varlog=varlog+tmp2*tmp2
       END IF
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
