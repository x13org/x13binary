C     Last change:  BCM  21 Nov 97    9:53 pm
**==agr.f    processed by SPAG 4.03F  at 09:46 on  1 Mar 1994
      SUBROUTINE agr(A,B,Iag,J1,J2,J0,Wt)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      DOUBLE PRECISION A,B,Wt
      INTEGER i,Iag,J1,J2,J0,j
C*** End of declarations inserted by SPAG
      INCLUDE 'srslen.prm'
C  THIS SUBROUTINE COMPOSITES THE COMPONENT SERIES
      DIMENSION A(PLEN),B(PLEN)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
      IF(dpeq(Wt,0.D0))Wt=1.D0
      DO i=J1,J2
       j=i+J0-J1
       IF(Iag.eq.0)B(j)=B(j)+(A(i)*Wt)
       IF(Iag.eq.1)B(j)=B(j)-(A(i)*Wt)
       IF(Iag.eq.2)B(j)=B(j)*(A(i)*Wt)
       IF(Iag.eq.3)B(j)=B(j)/(A(i)*Wt)
      END DO
      RETURN
      END
