      SUBROUTINE pass0(Trnsrs,Frstry,Isig,Istep,Lprt)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Check trading day, easter, constant regressors of final model
c     to see if they are significant.
c-----------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      DOUBLE PRECISION Trnsrs,tval,cval,tderiv
      INTEGER ktd,keastr,kmu,begcol,endcol,igrp,Isig,nsig,icol,Frstry,
     &        Istep
      DIMENSION Trnsrs(PLEN),tval(PB)
c     ------------------------------------------------------------------
      INTEGER strinx
      DOUBLE PRECISION tstdrv
      LOGICAL dpeq,Lprt
      EXTERNAL strinx,tstdrv,dpeq
c-----------------------------------------------------------------------
      ktd=strinx(F,Grpttl,Grpptr,1,Ngrptl,'Trading Day')
      IF(ktd.eq.0.and.(Itdtst.eq.1.or.Itdtst.eq.4.or.Itdtst.eq.5))
     &   ktd=strinx(F,Grpttl,Grpptr,1,Ngrptl,
     &              '1-Coefficient Trading Day')
      IF(ktd.eq.0.and.Itdtst.eq.3)
     &   ktd=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Stock Trading Day')
      IF(ktd.eq.0.and.Itdtst.eq.6)
     &   ktd=strinx(T,Grpttl,Grpptr,1,Ngrptl,
     &              '1-Coefficient Stock Trading Day')
c-----------------------------------------------------------------------
      keastr=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Easter')
      IF(keastr.eq.0)
     &   keastr=strinx(T,Grpttl,Grpptr,1,Ngrptl,'StatCanEaster')
      IF(keastr.eq.0)
     &   keastr=strinx(T,Grpttl,Grpptr,1,Ngrptl,'StockEaster')
c-----------------------------------------------------------------------
      kmu=strinx(F,Grpttl,Grpptr,1,Ngrptl,'Constant')
      IF(ktd.eq.0.and.keastr.eq.0.and.kmu.eq.0)RETURN
c-----------------------------------------------------------------------
c     compute t-statistics for regressors
c-----------------------------------------------------------------------
      CALL genrtt(tval)
      cval=1.96D0
c-----------------------------------------------------------------------
c     If trading day, check the t-statistics to see if there are
c     any significant trading day regressors
c-----------------------------------------------------------------------
      IF(ktd.gt.0.and.Itdtst.gt.0)THEN
       nsig=0
       DO igrp=Ngrp,1,-1
        begcol=Grp(igrp-1)
        endcol=Grp(igrp)-1
        IF(Rgvrtp(begcol).eq.PRGTST.or.Rgvrtp(begcol).eq.PRGTTD.or.
     &     Rgvrtp(begcol).eq.PRRTST.or.Rgvrtp(begcol).eq.PRRTTD.or.
     &     Rgvrtp(begcol).eq.PRATST.or.Rgvrtp(begcol).eq.PRATTD.or.
     &     Rgvrtp(begcol).eq.PRGTLM.or.Rgvrtp(begcol).eq.PRGTLQ.or.
     &     Rgvrtp(begcol).eq.PRGTLY.or.Rgvrtp(begcol).eq.PRGTSL.or.
     &     Rgvrtp(begcol).eq.PRRTLM.or.Rgvrtp(begcol).eq.PRRTLQ.or.
     &     Rgvrtp(begcol).eq.PRRTLY.or.Rgvrtp(begcol).eq.PRRTSL.or.
     &     Rgvrtp(begcol).eq.PRATLM.or.Rgvrtp(begcol).eq.PRATLQ.or.
     &     Rgvrtp(begcol).eq.PRATLY.or.Rgvrtp(begcol).eq.PRATSL.or.
     &     Rgvrtp(begcol).eq.PRG1TD.or.Rgvrtp(begcol).eq.PRR1TD.or.
     &     Rgvrtp(begcol).eq.PRA1TD.or.Rgvrtp(begcol).eq.PRG1ST.or.
     &     Rgvrtp(begcol).eq.PRR1ST.or.Rgvrtp(begcol).eq.PRA1ST.or.
     &     Rgvrtp(begcol).eq.PRGUTD.or.Rgvrtp(begcol).eq.PRGULM.or.
     &     Rgvrtp(begcol).eq.PRGULQ.or.Rgvrtp(begcol).eq.PRGULY)THEN
         DO icol=begcol,endcol
          IF(DABS(tval(icol)).ge.cval)nsig=nsig+1
         END DO
        END IF
       END DO
       IF(nsig.lt.1)THEN
        tderiv=tstdrv(ktd)
        if (DABS(tderiv).lt.cval) ktd=-ktd
       END IF
      END IF
      IF(keastr.gt.0.and.Leastr)THEN
       nsig=0
       begcol=Grp(keastr-1)
       endcol=Grp(keastr)-1
       DO icol=begcol,endcol
        IF(DABS(tval(icol)).ge.cval)nsig=nsig+1
       END DO
       IF(nsig.lt.1)keastr=-keastr
      END IF
      IF(Istep.eq.1)cval=Tsig
      IF(kmu.gt.0.and.Lchkmu)THEN
       begcol=Grp(kmu-1)
       IF(DABS(tval(begcol)).lt.cval)kmu=-kmu
      END IF
      IF(ktd.lt.0)THEN
       DO igrp=Ngrp,1,-1
        begcol=Grp(igrp-1)
        endcol=Grp(igrp)-1
        IF(Rgvrtp(begcol).eq.PRGTST.or.Rgvrtp(begcol).eq.PRGTTD.or.
     &     Rgvrtp(begcol).eq.PRRTST.or.Rgvrtp(begcol).eq.PRRTTD.or.
     &     Rgvrtp(begcol).eq.PRATST.or.Rgvrtp(begcol).eq.PRATTD.or.
     &     Rgvrtp(begcol).eq.PRGTLM.or.Rgvrtp(begcol).eq.PRGTLQ.or.
     &     Rgvrtp(begcol).eq.PRGTLY.or.Rgvrtp(begcol).eq.PRGTSL.or.
     &     Rgvrtp(begcol).eq.PRRTLM.or.Rgvrtp(begcol).eq.PRRTLQ.or.
     &     Rgvrtp(begcol).eq.PRRTLY.or.Rgvrtp(begcol).eq.PRRTSL.or.
     &     Rgvrtp(begcol).eq.PRATLM.or.Rgvrtp(begcol).eq.PRATLQ.or.
     &     Rgvrtp(begcol).eq.PRATLY.or.Rgvrtp(begcol).eq.PRATSL.or.
     &     Rgvrtp(begcol).eq.PRG1TD.or.Rgvrtp(begcol).eq.PRR1TD.or.
     &     Rgvrtp(begcol).eq.PRA1TD.or.Rgvrtp(begcol).eq.PRG1ST.or.
     &     Rgvrtp(begcol).eq.PRR1ST.or.Rgvrtp(begcol).eq.PRA1ST.or.
     &     Rgvrtp(begcol).eq.PRGUTD.or.Rgvrtp(begcol).eq.PRGULM.or.
     &     Rgvrtp(begcol).eq.PRGULQ.or.Rgvrtp(begcol).eq.PRGULY)THEN
         CALL dlrgef(begcol,Nrxy,endcol-begcol+1)
         IF(Lfatal)RETURN
        END IF
       END DO
       Isig=Isig+1
       IF(Lprt)CALL mkPOneLine(Mt1,'@',
     & 'Deleted trading day regressor(s) due to insignificant t-value.')
       Aicint=0
c-----------------------------------------------------------------------
c   If leap year or other prior adjustment done with trading day,
c   remove effect of prior adjustment from series
c-----------------------------------------------------------------------
       IF(Picktd)THEN
        Picktd=.false.
        IF(.not.(Fcntyp.eq.4.OR.dpeq(Lam,1D0)))THEN
         CALL rmlpyr(Trnsrs,Nobspf)
         IF(Lfatal)RETURN
        END IF
       END IF
c-----------------------------------------------------------------------
      END IF
      IF(keastr.lt.0)THEN
       igrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Easter')
       IF(igrp.eq.0)
     &    igrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'StatCanEaster')
       IF(igrp.eq.0)
     &    igrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'StockEaster')
       begcol=Grp(igrp-1)
       endcol=Grp(igrp)-1
       CALL dlrgef(begcol,Nrxy,endcol-begcol+1)
       IF(Lfatal)RETURN
       Isig=Isig+1
       IF(Lprt)CALL mkPOneLine(Mt1,'@',
     &      'Deleted Easter regressor(s) due to insignificant t-value.')
       Aicind=0
      END IF
      IF(kmu.lt.0)THEN
       igrp=strinx(F,Grpttl,Grpptr,1,Ngrptl,'Constant')
       begcol=Grp(igrp-1)
       CALL dlrgef(begcol,Nrxy,1)
       IF(Lfatal)RETURN
       Isig=Isig+1
       IF(Lprt)CALL mkPOneLine(Mt1,'@',
     &    'Deleted constant regressor(s) due to insignificant t-value.')
      END IF
c     ------------------------------------------------------------------
c     If regressors have been deleted, regenerate regression matrix
c     ------------------------------------------------------------------
      IF(Isig.gt.0)
     &   CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &               Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
c-----------------------------------------------------------------------
      RETURN
      END
