C     Last change:  BCM   4 Sep 1998   10:48 am
      SUBROUTINE xrgtrn(X,L1,L2,Psuadd,Muladd,Tdgrp,Haveum,Umean,
     &                  Ndifum,Kswv)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Adjust the irregular for the X-11 Regression
c-----------------------------------------------------------------------
c     X : Irregular component
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'tdtyp.cmn'
      INCLUDE 'xtdtyp.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION X,Umean
      LOGICAL Psuadd,Haveum
      INTEGER L1,L2,i,i2,Muladd,Ndifum,i3,Tdgrp,Kswv
      DIMENSION X(*),Umean(PLEN)
c-----------------------------------------------------------------------
      DO i=L1,L2
       i2=i-L1+1
c-----------------------------------------------------------------------
       IF(Haveum)THEN
        i3=i2+Ndifum
        X(i2)=X(i2)-Umean(i3)
c-----------------------------------------------------------------------
       ELSE IF(Psuadd)THEN
        IF(Tdgrp.gt.0)THEN
         X(i2)=Daybar*(X(i2)-1D0)-Xlpyr(i)
        ELSE
         X(i2)=X(i2)-1D0
        END IF
c-----------------------------------------------------------------------
       ELSE IF(Muladd.eq.0)THEN
c-----------------------------------------------------------------------
c     Multiplicative adjustments
c-----------------------------------------------------------------------
        IF(Tdgrp.gt.0)THEN
         IF(Kswv.eq.3)THEN
          X(i2)=Xnstar(i)*X(i2)-Xnstar(i)
         ELSE
          X(i2)=Xnstar(i)*X(i2)-Xn(i)
         END IF
        ELSE 
         X(i2)=X(i2)-1D0
        END IF
       ELSE IF(Muladd.eq.2)THEN
c-----------------------------------------------------------------------
c     Log-additive adjustments ( N*(log(i)+1) - Nt )
c-----------------------------------------------------------------------
        IF(Tdgrp.gt.0)X(i2)=Xnstar(i)*(X(i2)+1)-Xn(i)
       END IF
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
