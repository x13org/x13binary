C     Last change:  BCM   4 Sep 1998    3:01 pm
      SUBROUTINE rmfix(Trnsrs,Nbcst,Nrxy,Fxindx)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     If Fxindx = 1, this routine removes fixed regressors from
c     regression matrix, as well as the effects of the fixed regressors
c     from the transformed series.
c     If Fxindx = 2, this routine is used to remove all regressors
c     from the regression matrix, as well as the effects of the 
c     regressors from the transformed series (except for the constant
c     term), prior to identifying differencing and ARMA model orders in
c     the automatic model identification procedure.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'fxreg.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      LOGICAL T
      DOUBLE PRECISION ZERO
      PARAMETER(T=.TRUE.,ZERO=0D0)
c-----------------------------------------------------------------------
      CHARACTER str*(PCOLCR),strgrp*(PGRPCR)
      INTEGER Fxindx,i,icol,nreg,Nbcst,nchr,nchgrp,igrp,oldfix,numgrp,
     &        endcol,begcol,Nrxy
      DOUBLE PRECISION Trnsrs(PLEN)
c-----------------------------------------------------------------------
c     Initialize data dictionary for fixed regression effects and fixed
c     regression effect groups.
c-----------------------------------------------------------------------
      IF(Fxindx.lt.2.or.Nfxttl.eq.0)THEN
       CALL setdp(ZERO,PLEN,Fixfac)
       CALL intlst(PB,Cfxptr,Nfxttl)
       CALL intlst(PGRP,Gfxptr,Ngfxtl)
       CALL intlst(PGRP,Grpfix,Ngrpfx)
      END IF
*      ELSE
      IF(Fxindx.eq.2)CALL setdp(ZERO,PLEN,Fixfc2)
*      END IF
      oldfix=Nfxttl
      nreg=Nfxttl+1
      numgrp=Ngfxtl+1
      IF(Ngrp.eq.0)RETURN
c-----------------------------------------------------------------------
c     Step through each group of regressors, finding those that are
c     fixed.
c-----------------------------------------------------------------------
      DO igrp=Ngrp,1,-1
       CALL getstr(Grpttl,Grpptr,Ngrptl,igrp,strgrp,nchgrp)
       IF(Lfatal)RETURN
       endcol=Grp(igrp)-1
       begcol=Grp(igrp-1)
       icol=endcol
       DO WHILE (icol.ge.begcol)
        IF(Regfx(icol).or.Fxindx.eq.2)THEN
c-----------------------------------------------------------------------
c     If regressor is fixed, make copy of it in the data dictionary
c     of fixed regressors.
c-----------------------------------------------------------------------
         CALL getstr(Colttl,Colptr,Ncoltl,icol,str,nchr)
         IF(.not.Lfatal)
     &      CALL insstr(str(1:nchr),nreg,PB,Cfxttl,Cfxptr,Nfxttl)
         IF(Lfatal)RETURN
         Bfx(Nfxttl)=B(icol)
         Fxtype(Nfxttl)=Rgvrtp(icol)
         Fixind(Nfxttl)=Fxindx
         nreg=nreg+1
c-----------------------------------------------------------------------
c     Generate regression effect for fixed regressors
c-----------------------------------------------------------------------
         IF(Fxindx.eq.2)THEN
          IF(Rgvrtp(icol).ne.PRGTCN)
     &       CALL daxpy(Nrxy,B(icol),Xy(icol),Ncxy,Fixfc2,1)
         ELSE
          CALL daxpy(Nrxy,B(icol),Xy(icol),Ncxy,Fixfac,1)
         END IF
c-----------------------------------------------------------------------
c     Remove from regression matrix.
c-----------------------------------------------------------------------
         IF((Rgvrtp(icol).ge.PRGTUH.and.Rgvrtp(icol).le.PRGUH5).or.
     &       Rgvrtp(icol).eq.PRGTUS.or.Rgvrtp(icol).eq.PRGTUD.or.
     &       Rgvrtp(icol).eq.PRGUAO.or.Rgvrtp(icol).eq.PRGULS.or.
     &       Rgvrtp(icol).eq.PRGUSO.or.Rgvrtp(icol).eq.PRGUTD.or.
     &       Rgvrtp(icol).eq.PRGULM.or.Rgvrtp(icol).eq.PRGULQ.or.
     &       Rgvrtp(icol).eq.PRGULY.or.Rgvrtp(icol).eq.PRGUCN.or.
     &       Rgvrtp(icol).eq.PRGUCY)THEN
          CALL dlusrg(icol-begcol+1)
          IF(Lfatal)RETURN
         END IF
         CALL dlrgef(icol,Nrxy,1)
         IF(Lfatal)RETURN
        END IF
        icol=icol-1
       END DO
       IF(oldfix.lt.Nfxttl)THEN
        CALL insstr(strgrp(1:nchgrp),numgrp,PGRP,Gfxttl,Gfxptr,Ngfxtl)
        IF(.not.Lfatal)
     &     CALL insptr(T,Nfxttl-oldfix,numgrp,PGRP,PB,Grpfix,Ngrpfx)
        IF(Lfatal)RETURN
        oldfix=Nfxttl
        numgrp=numgrp+1
       END IF
      END DO
c-----------------------------------------------------------------------
c     Remove regression effect for fixed regressors from transformed
c     series
c-----------------------------------------------------------------------
      IF(Nfxttl.gt.0)THEN
       IF(Fxindx.eq.2)THEN
        DO i=1,Nspobs
         Trnsrs(i)=Trnsrs(i)-Fixfc2(i+Nbcst)
        END DO
       ELSE
        DO i=1,Nspobs
         Trnsrs(i)=Trnsrs(i)-Fixfac(i+Nbcst)
        END DO
       END IF
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
