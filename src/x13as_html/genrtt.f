      SUBROUTINE genrtt(Tval)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
c     ------------------------------------------------------------------
      DOUBLE PRECISION ZERO
      PARAMETER(ZERO=0D0)
c     ------------------------------------------------------------------
      DOUBLE PRECISION Tval,rmse,seb,xpxinv,tmp
      INTEGER nb2,j,nelt,nfix,igrp,begcol,endcol,icol,regidx
      DIMENSION Tval(PB),xpxinv(PB*(PB+1)/2),tmp(2)
c     ------------------------------------------------------------------
      DOUBLE PRECISION dpmpar
      LOGICAL dpeq
      EXTERNAL dpeq,dpmpar
c     ------------------------------------------------------------------
c     Generate number of unfixed regressors
c     ------------------------------------------------------------------
      nb2=Nb
      IF(Iregfx.ge.2)THEN
       DO j=1,Nb
        IF(Regfx(j))nb2=nb2-1
       END DO
      END IF
c-----------------------------------------------------------------------
c     Get the root mean square error and X'X inverse.
c-----------------------------------------------------------------------
      IF(nb2.gt.0)THEN
       nelt=(nb2+1)*(nb2+2)/2
c-----------------------------------------------------------------------
       IF(Var.gt.2D0*dpmpar(1))THEN
        rmse=sqrt(Var)
        CALL copy(Chlxpx,nelt,1,xpxinv)
        CALL dppdi(xpxinv,nb2,tmp,1)
       ELSE
        rmse=ZERO
       END IF
c-----------------------------------------------------------------------
      ELSE
       rmse=ZERO
      END IF
      IF(dpeq(rmse,ZERO))RETURN
c     ------------------------------------------------------------------
c     generate t-statistics for regressors
c     ------------------------------------------------------------------
      nfix=0
c-----------------------------------------------------------------------
      DO igrp=1,Ngrp
       begcol=Grp(igrp-1)
       endcol=Grp(igrp)-1
c-----------------------------------------------------------------------
c     For each regression variable in the group calculate the standard
c error and t-value if the variance in nonzero
c-----------------------------------------------------------------------
       DO icol=begcol,endcol
        IF(Regfx(icol))THEN
         seb=ZERO
         nfix=nfix+1
c         regidx(icol)=NOTSET
        ELSE
c         regidx(icol)=icol-nfix
         regidx=icol-nfix
         seb=sqrt(xpxinv(regidx*(regidx+1)/2))*rmse
        END IF
c-----------------------------------------------------------------------
        IF(seb.gt.ZERO)THEN
         Tval(icol)=B(icol)/seb
        ELSE
         Tval(icol)=ZERO
        END IF
c-----------------------------------------------------------------------
       END DO
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
