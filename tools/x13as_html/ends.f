**==ends.f    processed by SPAG 4.03F  at 12:14 on 10 Mar 1994
      SUBROUTINE ends(Stc,Stci,Ib,Ie,K,Rbeta)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C --- X11 TREND CYCLE END WEIGHTS.
c-----------------------------------------------------------------------
      INCLUDE 'hender.prm'
c-----------------------------------------------------------------------
C     Changed March, 1994 By Brian Monsell (SRD) to incorporate
c     Henderson end filter generation routine based on Doherty(1993)
c     algorithm.
c-----------------------------------------------------------------------
      INTEGER i,Ib,Ie,j,K,l,m,n,lm
      DOUBLE PRECISION Stc,Stci,wtcntr,wtend,Rbeta
      DIMENSION Stc(*),Stci(*),wtcntr(PMXHN2),wtend(PMXHN1)
c-----------------------------------------------------------------------
C --- Code starts here
c-----------------------------------------------------------------------
c     Generate central Henderson filter weight for a filter of length K
c-----------------------------------------------------------------------
      CALL hender(wtcntr,K)
c-----------------------------------------------------------------------
      m=K-1
      l=m/2
      lm=(K+1)/2
c-----------------------------------------------------------------------
c     Initialize end and beginning of trend
c-----------------------------------------------------------------------
      DO i=1,l
       Stc(Ib+i-1)=0D0
       Stc(Ie-i+1)=0D0
c-----------------------------------------------------------------------
c     Determine the length of the end filter needed for this observation
c-----------------------------------------------------------------------
       n=lm+i-1
c-----------------------------------------------------------------------
c     Generate Henderson end filters for given value of Rbeta.
c-----------------------------------------------------------------------
       CALL hndend(n,K,wtcntr,wtend,Rbeta)
c-----------------------------------------------------------------------
c     Apply n-term end filter to the beginning and end of the series.
c-----------------------------------------------------------------------
       DO j=1,n
        Stc(Ib+i-1)=wtend(n-j+1)*Stci(Ib+j-1)+Stc(Ib+i-1)
        Stc(Ie-i+1)=wtend(n-j+1)*Stci(Ie-j+1)+Stc(Ie-i+1)
       END DO
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
