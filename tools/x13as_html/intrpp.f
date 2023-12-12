C     Last change:  BCM   1 Sep 1998    1:07 pm
      DOUBLE PRECISION FUNCTION intrpp(Ppvec,Ppnum,Nobs,Ppi,Plen,Dif2nd)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This routine gets the 1 percent points for the distributions
c     of the normality statistics.
c-----------------------------------------------------------------------
      DOUBLE PRECISION ONE,TWO
      PARAMETER(ONE=1D0,TWO=2D0)
c-----------------------------------------------------------------------
      DOUBLE PRECISION Ppvec,theta
      INTEGER Ppnum,Nobs,Ppi,Plen
      LOGICAL Dif2nd
      DIMENSION Ppvec(Plen),Ppnum(Plen)
c-----------------------------------------------------------------------
c     If length match is exact, return percentage point
c-----------------------------------------------------------------------
      intrpp=Ppvec(Ppi)
      IF(Ppnum(Ppi).eq.Nobs)RETURN
c-----------------------------------------------------------------------
c     Perform interpolation based on first differences
c-----------------------------------------------------------------------
      theta=dble(Nobs-Ppnum(Ppi))/dble(Ppnum(2)-Ppnum(1))
      intrpp=intrpp+theta*(Ppvec(Ppi+1)-Ppvec(Ppi))
c-----------------------------------------------------------------------
c     Refine with interpolation based on second differences, if 
c     requested
c-----------------------------------------------------------------------
      IF(Dif2nd)intrpp=intrpp+((theta*(theta-ONE))/TWO)*
     &   (Ppvec(Ppi+2)-2*Ppvec(Ppi+1)+Ppvec(Ppi))
c-----------------------------------------------------------------------
      RETURN
      END
