      SUBROUTINE xrghol(Xdisp,Psuadd,Xlpyr,Daybar)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Divide trading day and holiday regressors in X-11 Regression by 
c     length of month/quarter.
c-----------------------------------------------------------------------
c     Author : Brian Monsell, February 1996
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'usrreg.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION Xlpyr,Daybar
      LOGICAL Psuadd
      INTEGER icol,irow,ir1,ir2,iusr,rtype,Xdisp
      DIMENSION Xlpyr(PLEN)
c-----------------------------------------------------------------------
      iusr=1
      DO icol=1,Nb
       rtype=Rgvrtp(icol)
       IF(rtype.eq.PRGTUD.and.Ncusrx.gt.0)THEN
        rtype=Usrtyp(iusr)
        iusr=iusr+1
       END IF
       IF(rtype.eq.PRGTEA.or.rtype.eq.PRGTLD.or.rtype.eq.PRGTTH.or.
     &    rtype.eq.PRGTEC.or.rtype.eq.PRGTUH)THEN
        DO irow=1,Nrxy
c         ir1=irow+(icol-1)*Nrxy
         ir1=icol+(irow-1)*Ncxy
         IF(Psuadd.and.Easidx.eq.0)THEN
          ir2=irow+Xdisp-1
          Xy(ir1)=Xy(ir1)*Daybar-Xlpyr(ir2)/Sp
         ELSE
          Xy(ir1)=Xy(ir1)*Daybar
         END IF
        END DO
       END IF
      END DO
c-----------------------------------------------------------------------
      RETURN
      END

