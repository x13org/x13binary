C     Last change:  BCM   3 Aug 1998   12:53 pm
      INTEGER FUNCTION round(x)
      IMPLICIT NONE
c     -------------------------------------------------------------------
c     Round x, returning an integer
c     -------------------------------------------------------------------
      LOGICAL dpeq
      DOUBLE PRECISION X,ceilng,c1,c2
      EXTERNAL ceilng,dpeq
c     -------------------------------------------------------------------
      c1=ceilng(X-0.5D0)
      c2=ceilng(X)
      IF(dpeq(c1,c2))THEN
       round=idint(c2)
      ELSE IF(dpeq(c1,X-0.5D0))THEN
       round=idint(c2)
      ELSE
       round=idint(c2)-1
      END IF
c     -------------------------------------------------------------------
      RETURN
      END
