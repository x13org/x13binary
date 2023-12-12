C     Last change:  BCM   4 Sep 1998    1:46 pm
      SUBROUTINE addfix(Trnsrs,Nbcst,Rind,Fxindx)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Adds fixed regressors back into the regression matrix, as well as
c     the effects of the fixed regressors to the adjusted transformed
c     series.  This routine also used to add back regressors after the
c     ARIMA model orders are identified.
c-----------------------------------------------------------------------
      LOGICAL F
      PARAMETER(F=.false.)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'fxreg.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'units.cmn'
c----------------------------------------------------------------------- 
      CHARACTER str*(PCOLCR),strgrp*(PGRPCR)
      INTEGER i,icol,Nbcst,nchr,nchgrp,igrp,begcol,endcol,Rind,Fxindx,
     &        nu
      DOUBLE PRECISION Trnsrs(PLEN)
c-----------------------------------------------------------------------
      IF(Ngrpfx.eq.0)RETURN
c-----------------------------------------------------------------------
c     Step through each group of the fixed regressors, and add them back
c     into the regression matrix.
c-----------------------------------------------------------------------
      nu=0
      DO igrp=Ngrpfx,1,-1
       begcol=Grpfix(igrp-1)
       endcol=Grpfix(igrp)-1
c-----------------------------------------------------------------------
       IF((Fxtype(begcol).ge.PRGTUH.and.Fxtype(begcol).le.PRGUH5).or.
     &     Fxtype(begcol).eq.PRGTUS.or.Fxtype(begcol).eq.PRGUTD.or.
     &     Fxtype(begcol).eq.PRGTUD.or.Fxtype(begcol).eq.PRGULM.or.
     &     Fxtype(begcol).eq.PRGULQ.or.Fxtype(begcol).eq.PRGULY.or.
     &     Fxtype(begcol).eq.PRGUAO.or.Fxtype(begcol).eq.PRGULS.or.
     &     Fxtype(begcol).eq.PRGUSO.or.Fxtype(begcol).eq.PRGUCN.or.
     &     Fxtype(begcol).eq.PRGUCY)THEN
        nu=nu+1
c-----------------------------------------------------------------------
c   delete regressor(s) from set   (BCM Jul 2007)
c-----------------------------------------------------------------------
        DO icol=endcol,begcol,-1
         IF(Fixind(icol).eq.Fxindx)THEN
          CALL delstr(icol,Cfxttl,Cfxptr,Nfxttl,PB)
          IF(Lfatal)RETURN
         END IF
        END DO
       ELSE
        CALL getstr(Gfxttl,Gfxptr,Ngfxtl,igrp,strgrp,nchgrp)
        IF(Lfatal)RETURN
        DO icol=endcol,begcol,-1
         IF(Fixind(icol).eq.Fxindx)THEN
          CALL getstr(Cfxttl,Cfxptr,Nfxttl,icol,str,nchr)
          IF(.not.Lfatal)
     &       CALL adrgef(Bfx(icol),str(1:nchr),strgrp(1:nchgrp),
     &                   Fxtype(icol),Fxindx.eq.1,F)
          IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c   delete regressor from set
c-----------------------------------------------------------------------
          CALL delstr(icol,Cfxttl,Cfxptr,Nfxttl,PB)
          IF(Lfatal)RETURN
         END IF
        END DO
       END IF
      END DO
c-----------------------------------------------------------------------
      IF(Userfx.or.(Fxindx.eq.2.and.nu.gt.0))THEN
       CALL addusr(Rind,Fxindx)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
      IF(Fxindx.eq.2)THEN
       DO i=1,Nspobs
        Trnsrs(i)=Trnsrs(i)+Fixfc2(i+Nbcst)
       END DO
      ELSE
       DO i=1,Nspobs
        Trnsrs(i)=Trnsrs(i)+Fixfac(i+Nbcst)
       END DO
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
