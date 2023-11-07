C     Last change:  BCM  12 Mar 98   12:39 pm
**==compb.f    processed by SPAG 4.03F  at 12:23 on 21 Jun 1994
      SUBROUTINE compb(Xavg,Xmnx,Xran,I,Xcm,Ncol)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c  *****  computes monthly means of seasonal factors for each span and
c  *****  all spans.  determines which months have the maximum and
c  *****  minimum averages for each span and all spans (xmnx).
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'ssap.prm'
      INCLUDE 'ssap.cmn'
c-----------------------------------------------------------------------
      CHARACTER Xcm*(3)
      DOUBLE PRECISION Xavg,xmax,xmin,Xmnx,Xran
      INTEGER I,i2,ij,j,ji,k,Kountr,Ncol
      DIMENSION Xavg((MXCOL+1),PSP),Xmnx((MXCOL+1),2),Xran(MXCOL+1),
     &          Xcm((MXCOL+1),PSP),Kountr(MXCOL,PSP)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
      COMMON /kcom  / Kountr
c-----------------------------------------------------------------------
c     compute range of the ith span
c-----------------------------------------------------------------------
      Xran(I)=Xmnx(I,2)-Xmnx(I,1)
c-----------------------------------------------------------------------
c     compute average sf for all spans
c-----------------------------------------------------------------------
      IF(I.eq.Ns1)THEN
       DO j=1,Nsea
        k=0
        DO i2=1,Ncol
         k=Kountr(i2,j)+k
        END DO
        Xavg(I,j)=Xavg(I,j)/k
       END DO
      ELSE
c-----------------------------------------------------------------------
c     compute average sf for span i
c-----------------------------------------------------------------------
       DO j=1,Nsea
        Xavg(I,j)=Xavg(I,j)/Kountr(I,j)
       END DO
c-----------------------------------------------------------------------
c     compute min and max for all spans
c-----------------------------------------------------------------------
       IF(Xmnx(Ns1,1).gt.Xmnx(I,1))Xmnx(Ns1,1)=Xmnx(I,1)
       IF(Xmnx(Ns1,2).lt.Xmnx(I,2))Xmnx(Ns1,2)=Xmnx(I,2)
      END IF
c-----------------------------------------------------------------------
C     Label min and max seasonal factor for span i
c-----------------------------------------------------------------------
      xmax=100.D0
      xmin=100.D0
      ij=0
      ji=0
      DO j=1,Nsea
       Xcm(I,j)='   '
       IF(xmin.gt.Xavg(I,j))THEN
        xmin=Xavg(I,j)
        Xcm(I,j)='min'
        IF(ij.gt.0)Xcm(I,ij)='   '
        ij=j
       ELSE IF(xmax.lt.Xavg(I,j))THEN
        xmax=Xavg(I,j)
        Xcm(I,j)='max'
        IF(ji.gt.0)Xcm(I,ji)='   '
        ji=j
       END IF
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
