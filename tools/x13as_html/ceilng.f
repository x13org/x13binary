C     Last change:  BCM  24 Aug 2001 11:38 am
C                   JK    4 Mar 2009  4:43 pm  STATCAN
      DOUBLE PRECISION FUNCTION ceilng(X)
      IMPLICIT NONE
c     -------------------------------------------------------------------
      DOUBLE PRECISION X
      INTEGER*8 y
c     -------------------------------------------------------------------
      y=X
      ceilng=dble(y)
      IF(X.gt.ceilng)ceilng=ceilng+1
c     -------------------------------------------------------------------
      RETURN
      END
      