      SUBROUTINE spgrh2(X,Sxx,Frq,N1,N2,Nspfrq,Ldecbl)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Routine to compute the periodogram (stored in Sxx) of a series X 
c     at frequencies Frq.  The starting and ending pointers of the 
c     series are given in N1 and N2
c-----------------------------------------------------------------------
c     Programmed by Brian C. Monsell, April 1996
c-----------------------------------------------------------------------
C      INCLUDE 'srslen.prm'
c-----------------------------------------------------------------------
      DOUBLE PRECISION PI,TEN,ZERO,TWO
      PARAMETER(PI=3.14159265358979d0,TWO=2D0,TEN=10D0,ZERO=0D0)
c-----------------------------------------------------------------------
c     replace dimension length for x (BCM May 2007)
      DOUBLE PRECISION X,Sxx,Frq,sumc,sums
      INTEGER i,j,k,N1,N2,n,Nspfrq
      LOGICAL Ldecbl
      DIMENSION X(*),Sxx(*),Frq(*)
c-----------------------------------------------------------------------
c     compute number of observations
c-----------------------------------------------------------------------
      n=N2-N1+1
c-----------------------------------------------------------------------
c     begin loop for frequencies by initialzing sum to zero
c-----------------------------------------------------------------------
      DO i=1,Nspfrq
       sumc=ZERO
       sums=ZERO
c-----------------------------------------------------------------------
c     loop over all the observations to calculate sumx
c     New algorithm out of Shumway's book
c-----------------------------------------------------------------------
       DO j=N1,N2
        k=j-N1
        sumc=sumc+X(j)*DCOS(TWO*PI*Frq(i)*k)
        sums=sums+X(j)*DSIN(TWO*PI*Frq(i)*k)
       END DO
       sumc=sumc/sqrt(dble(n))
       sums=sums/sqrt(dble(n))
       Sxx(i)=(sumc*sumc)+(sums*sums)
       IF(Ldecbl)Sxx(i)=TEN*dlog10(Sxx(i))
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
