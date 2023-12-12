C     Last change:  BCM  4 Mar 2008    3:46 pm
      DOUBLE PRECISION FUNCTION mkmdsx(Sxx,Nfreq,Ldecbl)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INTEGER Nfreq,n1
      DOUBLE PRECISION Sxx
      LOGICAL Ldecbl
      DIMENSION Sxx(*)
c-----------------------------------------------------------------------
      IF(mod(Nfreq,2).eq.0)THEN
       n1=Nfreq/2
       IF(Ldecbl)THEN
        mkmdsx=(Sxx(n1)+Sxx(n1+1))/2D0
       ELSE
        mkmdsx=10D0**((log10(Sxx(n1))+log10(Sxx(n1+1)))/2D0)
       END IF
      ELSE
       n1=(Nfreq+1)/2
       mkmdsx=Sxx(n1)
      END IF
c-----------------------------------------------------------------------
      RETURN
      END