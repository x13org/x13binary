C     Last change:  BCM   7 May 2003    2:05 pm
      SUBROUTINE shrink(Stsi,Sts,Mtype,Ishrnk,Muladd,Ny)
      IMPLICIT NONE
C-----------------------------------------------------------------------
c     Generate seasonal factors based on the global shrinkage method
c     given in the paper "Shrinkage Est. of Time Series Seasonal Factors
c     and their Effect on Forecasting Accuracy",
c     Miller & Williams (2003)
C-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'x11ptr.cmn'
C-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO,ONE,THREE,NINE,TEN
      PARAMETER (ZERO=0D0,ONE=1D0,THREE=3D0,NINE=9D0,TEN=10D0)
C-----------------------------------------------------------------------
      DOUBLE PRECISION Stsi,Sts,varsts,wtx11,dn,varmu,dlx11,v,
     &                 sssf,den
      INTEGER Ishrnk,Mtype,i,j,lx11,Ny,i2,Muladd
      DIMENSION Stsi(PLEN),Sts(PLEN),varsts(PSP),lx11(5)
C-----------------------------------------------------------------------
      DATA lx11 /1,3,5,9,15/
C-----------------------------------------------------------------------
c  initialize variables
C-----------------------------------------------------------------------
      CALL setdp(ZERO,PSP,varsts)
      wtx11 = ZERO
      varmu = ZERO
C-----------------------------------------------------------------------
c     compute variance of SI ratios
C-----------------------------------------------------------------------
      IF (Ishrnk.eq.1) THEN
       DO i=Pos1ob,Pos1ob+Ny-1
        dn=ZERO
        i2=MOD(i,Ny)
        IF(i2.eq.0)i2=Ny
        DO j=i,Posfob,Ny
         dn=dn+ONE
         sssf=(Stsi(j)-Sts(j))*(Stsi(j)-Sts(j))
         varsts(i2)=varsts(i2)+sssf
        END DO
        varsts(i2)=varsts(i2)/(dn-ONE)
        varmu=varmu+varsts(i2)
       END DO
       varmu=varmu/DBLE(Ny)
      ELSE
       DO i=Pos1ob,Posfob
        varmu=(Stsi(i)-Sts(i))*(Stsi(i)-Sts(i))+varmu
       END DO
       varmu=varmu/DBLE(Posfob-Pos1ob+1-Ny)
      END IF
C-----------------------------------------------------------------------
c     compute weights based on the seasonal filter used
C-----------------------------------------------------------------------
      IF (Mtype.eq.7) THEN
       wtx11=ONE/THREE
      ELSE
       dlx11=DBLE(lx11(Mtype))
       den=(dlx11*THREE)*(dlx11*THREE)
       wtx11=TEN/den
       DO j=1,lx11(Mtype)-2
        wtx11=wtx11+NINE/den
       END DO
      END IF
      v=wtx11*varmu
C-----------------------------------------------------------------------
C     For global shrinkage estimator, compute a(k)
C-----------------------------------------------------------------------
      IF (Ishrnk.eq.1) THEN
       CALL glbshk(Sts,V,Ny,Muladd)
      ELSE
C-----------------------------------------------------------------------
C     code for local shrinkage estimator
C-----------------------------------------------------------------------
       CALL locshk(Sts,V,Ny)
      END IF
C-----------------------------------------------------------------------
      RETURN
C-----------------------------------------------------------------------
      END
