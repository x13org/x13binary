C     Last change:  BCM  28 Sep 1998   10:27 am
**==getxtd.f    processed by SPAG 4.03F  at 09:49 on  1 Mar 1994
      SUBROUTINE getxtd(Tdx,Begtd,Ll1,Lld,Muladd)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This subroutine sets up a variable which tells which X-11 trading
c     day factor is associated with which type of month.
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'tdtyp.cmn'
      INCLUDE 'xtdtyp.cmn'
c-----------------------------------------------------------------------
      LOGICAL T
      PARAMETER(T=.true.)
c-----------------------------------------------------------------------
      INTEGER i,i1,i2,Begtd,LL1,Lld,Muladd,ndif,n1,nn,tdgrp
      DOUBLE PRECISION lom,fac,Tdx
      DIMENSION Begtd(2),Tdx(*)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      INTEGER strinx
      EXTERNAL dpeq,strinx
c-----------------------------------------------------------------------
c     First, check to see if there are trading day or stock trading day
c     regressors, not user defined regressors.  If not, return.
c-----------------------------------------------------------------------
      tdgrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Trading Day')
      IF(tdgrp.eq.0)tdgrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,
     &				 'Stock Trading Day')
      IF(tdgrp.eq.0)THEN
       Tdtbl=0
       RETURN
      END IF
c-----------------------------------------------------------------------
c     determine starting and ending points of trading day
c     change of regimes, if necessary.
c-----------------------------------------------------------------------
      IF(Lrgmtd)CALL dfdate(Tddate,Begtd,Sp,ndif)
      n1=Ll1
      nn=Lld
      IF(Tdzero.lt.0)THEN
       n1=ndif+1
      ELSE IF(Lrgmtd)THEN
       nn=ndif
      END IF
c-----------------------------------------------------------------------
c     For each observation, test to see if a factor has been associated
c     with this type of month.
c-----------------------------------------------------------------------
      fac=1D0
      IF(Muladd.ne.1)fac=100D0
      DO i=n1,nn
       IF(dpeq(Tdx11(Tday(i)),DNOTST))THEN
c-----------------------------------------------------------------------
c     Generate a factor for the leap year effect inherent in the X-11
c     trading day factor.
c-----------------------------------------------------------------------
        IF(Muladd.eq.1)THEN
         lom=1D0
        ELSE
c         lom=Xn(i)/Xnstar(i)
         lom=Xnstar(i)/Xn(i)
        END IF
c-----------------------------------------------------------------------
c     Copy X-11 trading day factors for the given type-of-month.
c-----------------------------------------------------------------------
        Tdx11(Tday(i))=Tdx(i)*lom*fac
       END IF
      END DO
c-----------------------------------------------------------------------
      IF((Fulltd.or.Tdzero.eq.2).and.Lrgmtd)THEN
       IF(Tdzero.gt.0)THEN
        i1=nn+1
        i2=Lld
       ELSE
        i1=Ll1
        i2=n1
       END IF
       DO i=i1,i2
        IF(dpeq(Tdx11b(Tday(i)),DNOTST))THEN
         IF(Muladd.eq.1)THEN
          lom=1D0
         ELSE
c         lom=Xn(i)/Xnstar(i)
         lom=Xnstar(i)/Xn(i)
         END IF
         Tdx11b(Tday(i))=Tdx(i)*lom*fac
        END IF
       END DO
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
