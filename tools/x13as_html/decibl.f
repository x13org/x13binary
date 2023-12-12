C     Last change:  BCM  5 Mar 2008    2:38 pm
      DOUBLE PRECISION FUNCTION decibl(X)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Convert X into decibels, use with spectral routines
c-----------------------------------------------------------------------
      DOUBLE PRECISION X
c-----------------------------------------------------------------------
      decibl=10D0*log10(X)
      RETURN
      END