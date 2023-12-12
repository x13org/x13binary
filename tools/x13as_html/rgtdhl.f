C     Last change:  BCM   3 Mar 1999    8:00 am
      SUBROUTINE rgtdhl(A,Nbeg)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     If multiplicative seasonal adjustment and trading day and holiday
c     appear as regressors, calculate the effect of the mean holiday 
c     effect.
c-----------------------------------------------------------------------
      LOGICAL F
      PARAMETER(F=.false.)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'x11reg.cmn'
      INCLUDE 'x11srs.cmn'
      INCLUDE 'x11log.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'x11msc.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'xtdtyp.cmn'
c-----------------------------------------------------------------------
      INTEGER PA
      PARAMETER(PA=PLEN+2*PORDER)
c-----------------------------------------------------------------------
      DOUBLE PRECISION A,xnk
      INTEGER i,i2,frstry,Nbeg
c-----------------------------------------------------------------------
      DIMENSION A(PA),xnk(PLEN)
c-----------------------------------------------------------------------
c     Return if this is not a true multiplicative seasonal adjustment
c     (not Pseudo-Additive) with Bell-Hilmer Easter and trading day 
c     regressors. (This is done within the routine so that the 
c     call from the outlier identification routines is kept as clean as
c     possible.)
c-----------------------------------------------------------------------
      IF(.not.(Xhlnln.and.((.not.Psuadd).and.Muladd.eq.0).and.
     &   Easidx.eq.0.and.((.not.Axruhl).and.Holgrp.gt.0).and.
     &   Tdgrp.gt.0))RETURN
c-----------------------------------------------------------------------
      CALL kfcn(Begxrg,Nrxy,Pos1ob+Nbeg,Xelong)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Perform regression to remove the mean holiday effect
c-----------------------------------------------------------------------
      DO i=1,Nspobs
       i2=i+Pos1ob+Nbeg-1
       xnk(i)=Kvec(i)*Xnstar(i2)*Sti(i2)-Xn(i2)
c       xnk(i)=(Xnstar(i2)*Sti(i2)-Xn(i2))/Kvec(i)
      END DO
      CALL regvar(xnk,Nobspf,Fctdrp,Nfcst,Nbcst,Userx,Bgusrx,Nrusrx,
     &            Priadj,Reglom,Nrxy,Begxy,frstry,F,Xelong)
      IF(Lfatal)RETURN
c      CALL xrghol(Pos1ob+Nbeg,F,Xlpyr,Daybar)
      CALL xrgdiv(Kvec)
      CALL regx11(A)
      IF(.not.Lfatal.and.Armaer.eq.PSNGER)CALL prterx()
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Compute revised Kvec
c-----------------------------------------------------------------------
      CALL kfcn(Begxrg,Nrxy,Pos1ob+Nbeg,Xelong)
c-----------------------------------------------------------------------
c     Compute residuals from this non-linear estimation
c-----------------------------------------------------------------------
      IF(.not.Lfatal)
     &   CALL resid2(Xy,Nrxy,Ncxy,Nb,Pos1ob+Nbeg,B,A,Sti)
c-----------------------------------------------------------------------
      RETURN
      END
