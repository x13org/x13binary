      DOUBLE PRECISION FUNCTION tstdrv(Igrp)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
c     ------------------------------------------------------------------
      DOUBLE PRECISION ZERO,TWO,TWOPT5
      PARAMETER(ZERO=0D0,TWO=2D0,TWOPT5=2.5D0)
c     ------------------------------------------------------------------
      DOUBLE PRECISION rmse,seb,xpxinv,tmp,sumb,sumvar
      INTEGER nb2,j,nelt,nfix,begcol,endcol,icol,regidx,Igrp,baselt,jcol
      DIMENSION regidx(PB),xpxinv(PB*(PB+1)/2),tmp(2)
c     ------------------------------------------------------------------
      DOUBLE PRECISION dpmpar
      LOGICAL dpeq
      EXTERNAL dpeq,dpmpar
c     ------------------------------------------------------------------
c     Generate number of unfixed regressors
c     ------------------------------------------------------------------
      tstdrv=ZERO
      nfix=0
      DO j=1,Nb
       IF(Regfx(j))THEN
        nfix=nfix+1
        regidx(j)=NOTSET
       ELSE
        regidx(j)=j-nfix
       END IF
      END DO
      nb2=Nb-nfix
c-----------------------------------------------------------------------
c     Get the root mean square error and X'X inverse.
c-----------------------------------------------------------------------
      IF(nb2.gt.0)THEN
       nelt=(nb2+1)*(nb2+2)/2
       IF(Var.gt.2D0*dpmpar(1))THEN
        rmse=sqrt(Var)
        CALL copy(Chlxpx,nelt,1,xpxinv)
        CALL dppdi(xpxinv,nb2,tmp,1)
c----------------------------------------------------------------------
       ELSE
        rmse=ZERO
       END IF
      ELSE
       rmse=ZERO
      END IF
      IF(dpeq(rmse,ZERO))RETURN
c     ------------------------------------------------------------------
c     generate t-statistics for regressors
c     ------------------------------------------------------------------
      begcol=Grp(Igrp-1)
      endcol=Grp(Igrp)-1
      sumb=-B(begcol)
      baselt=0
      IF(Regfx(begcol))THEN
       sumvar=0D0
      ELSE
       baselt=regidx(begcol)*(regidx(begcol)+1)/2
       sumvar=xpxinv(baselt)
      END IF
c-----------------------------------------------------------------------
      IF(begcol.eq.endcol)THEN
       sumb=sumb*TWOPT5
       IF(baselt.gt.0)seb=(sqrt(sumvar)*rmse)*TWOPT5
c-----------------------------------------------------------------------
      ELSE
       DO icol=begcol+1,endcol
        sumb=sumb-B(icol)
        IF(.not.Regfx(icol))THEN
         nelt=icol-nfix
         baselt=(regidx(icol)-1)*regidx(icol)/2
         sumvar=sumvar+xpxinv(baselt+regidx(icol))
         DO jcol=begcol,icol-1
          IF(regidx(jcol).ne.NOTSET)
     &       sumvar=sumvar+TWO*xpxinv(baselt+regidx(jcol))
         END DO
        END IF
       END DO
       IF(baselt.gt.0)seb=sqrt(sumvar)*rmse
      END IF
c-----------------------------------------------------------------------
      IF(baselt.gt.0)tstdrv=sumb/seb
c-----------------------------------------------------------------------
      RETURN
      END
