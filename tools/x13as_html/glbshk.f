C     Last change:  BCM   7 May 2003    2:24 pm
      SUBROUTINE glbshk(Sts,V,Ny,Muladd)
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
      DOUBLE PRECISION ZERO,ONE
      PARAMETER (ZERO=0D0,ONE=1D0)
C-----------------------------------------------------------------------
      INTEGER i,i1,i2,iend,imi,ipi,it1,it2,j,Ny,nt,Muladd,k,kyr
      DOUBLE PRECISION avec,corr,Sts,wvec,sfmu,temps,V,dny
      DIMENSION avec(PYRS),Sts(PLEN),wvec(PYRS),temps(PLEN)
C-----------------------------------------------------------------------
      CALL setdp(ZERO,PYRS,avec)
      CALL setdp(ZERO,PYRS,wvec)
      IF(Muladd.eq.1)THEN
       sfmu=ZERO
      ELSE
       sfmu=ONE
      END IF
C-----------------------------------------------------------------------
C     Copy seasonal factors into double precision variable.
C-----------------------------------------------------------------------
      it1 = mod(Pos1ob,Ny)
      IF (it1.eq.0) it1 = Ny
      nt = Posfob-Pos1ob+1
      CALL copy(Sts(Pos1ob),nt,1,temps(it1))
C-----------------------------------------------------------------------
C     Fill incomplete years at the beginning and end of the array with
C     backcasts/forecasts or the nearest values for the same
C     month/quarter (if necessary).
C-----------------------------------------------------------------------
      IF(it1.gt.1)THEN
       DO i=1,it1-1
        ipi=Pos1ob-i
        imi=it1-i
        IF(ipi.ge.Pos1bk)THEN
         temps(imi)=Sts(ipi)
        ELSE
         temps(imi)=Sts(ipi+Ny)
        END IF
       END DO
      END IF
      iend=it1+(Posfob-Pos1ob)
      it2=mod(Posfob,Ny)
      IF(it2.gt.0)THEN
       DO i=1,(Ny-it2)
        ipi=Posfob+i
        imi=iend+i
        IF(ipi.le.Posffc)THEN
         temps(imi)=Sts(ipi)
        ELSE
         temps(imi)=Sts(ipi-Ny)
        END IF
       END DO
       iend=iend+Ny-it2
      END IF
C-----------------------------------------------------------------------
C     Set kyr to be the number of years given in temps
C-----------------------------------------------------------------------
      kyr=iend/Ny
      dny=DBLE(Ny)
C-----------------------------------------------------------------------
      DO k=1,kyr
       i1=(k-1)*Ny+1
       i2=k*Ny
       DO i=i1,i2
        avec(k)=avec(k)+(Sts(i)-sfmu)*(Sts(i)-sfmu)
       END DO
       avec(k)=(avec(k)/(dny-ONE))-V
       if (avec(k).lt.ZERO) avec(k)=ZERO
      END DO
C-----------------------------------------------------------------------
c     compute global shrinkage estimator for each year
C-----------------------------------------------------------------------
      corr=DBLE(Ny-3)/DBLE(Ny-1)
      DO j=1,kyr
       wvec(j)=corr*V/(V + avec(j))
      END DO
C-----------------------------------------------------------------------
C     compute seasonal factors with global shrinkage estimator applied
C-----------------------------------------------------------------------
      IF(it2.gt.0)iend=iend-Ny+it2
      DO k=1,kyr
       i1=(k-1)*Ny+1
       IF(i1.lt.it1)i1=it1
       i2=k*Ny
       if (i2.gt.iend)i2=iend
       DO i=i1,i2
        j=i+(Pos1ob-it1)
        Sts(j)=temps(i)*(ONE-wvec(k))+wvec(k)
       END DO
      END DO
C-----------------------------------------------------------------------
      RETURN
      END
