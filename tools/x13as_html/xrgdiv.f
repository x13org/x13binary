C     Last change:  BCM  27 May 1998   12:27 pm
      SUBROUTINE xrgdiv(Kvec)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Divide trading day and holiday regressors in X-11 Regression by 
c     value of the holiday mean function
c-----------------------------------------------------------------------
c     Author : Brian Monsell, March 1996
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'usrreg.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION Kvec
      INTEGER icol,irow,rtype,iusr,ir1
      DIMENSION Kvec(PLEN)
c-----------------------------------------------------------------------
      iusr=1
      DO icol=1,Nb
       rtype=Rgvrtp(icol)
       IF(rtype.eq.PRGTTD.or.rtype.eq.PRGTEA.or.rtype.eq.PRGTLD.or.
     &    rtype.eq.PRGTTH.or.rtype.eq.PRGTUH.or.rtype.eq.PRGTLY.or.
     &    rtype.eq.PRRTTD.or.rtype.eq.PRRTLY.or.rtype.eq.PRG1TD.or.
     &    rtype.eq.PRATTD.or.rtype.eq.PRATLY.or.rtype.eq.PRGTEC.or.
     &    rtype.eq.PRR1TD.or.rtype.eq.PRA1TD.or.rtype.eq.PRGUTD.or.
     &    rtype.eq.PRGULM.or.rtype.eq.PRGULQ.or.rtype.eq.PRGULY)THEN
        DO irow=1,Nrxy
         ir1=icol+(irow-1)*Ncxy
         Xy(ir1)=Xy(ir1)/Kvec(irow)
        END DO
       END IF
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
