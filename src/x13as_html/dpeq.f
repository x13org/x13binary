C     Last change:  BCM  30 Oct 97   10:24 am
      LOGICAL FUNCTION dpeq(X,Dtargt)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Avoids a floating point comparison error to test whether X is 
c     equal to Dtargt
c-----------------------------------------------------------------------
      DOUBLE PRECISION DELTA
      PARAMETER(DELTA=3.834D-20)
c-----------------------------------------------------------------------
      DOUBLE PRECISION X,Dtargt,dx
c-----------------------------------------------------------------------
      dpeq=.false.
      dx=dabs(X-Dtargt)
      IF(dx.lt.DELTA)dpeq=.true.
c      dpeq=.true.
c      IF(X.lt.Dtargt.or.X.gt.Dtargt)dpeq=.false.
c-----------------------------------------------------------------------
      RETURN
      END
