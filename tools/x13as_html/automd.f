c     Last change:Mar. 2021- if there are fatal errors when calling
c                 armats,return
C     previous change:  BCM  18 Mar 2003    6:47 am
      SUBROUTINE automd(Trnsrs,Frstry,Nefobs,A,Na,Lsumm,Lidotl,Svldif,
     &                  Lsadj,Ltdlom,Fctok,Lhiddn,Lnoprt)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     This subroutine performs an automatic ARIMA model selection.  The
c     procedure is similar to that of Gomez and Maravall (1998)
c     ------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'adj.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'htmlout.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'mdltbl.i'
      INCLUDE 'svllog.prm'
      INCLUDE 'svllog.cmn'
      INCLUDE 'mdlsvl.i'
c     ------------------------------------------------------------------
      DOUBLE PRECISION BIGCV,TWOPT5,MALIM,ONE,ZERO,TWOPT8
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.,BIGCV=1000001D0,TWOPT5=2.5D0,
     &          MALIM=0.001D0,ONE=1D0,ZERO=0D0,TWOPT8=2.8D0)
c     ------------------------------------------------------------------
      CHARACTER firstw*(11),mdl1st*(132)
      DOUBLE PRECISION A,Trnsrs,blpct0,blq0,rvr0,rtval0,fct2,blpct,blq,
     &                 tmpr,tmps,rvr,rtval,cvl0,cvlold,tair,a0,sumMA,
     &                 adj0,trns0
      INTEGER lpr,lqr,lps,lqs,ldr,lds,lpr0,lqr0,lps0,lqs0,ldr0,lds0,
     &        Frstry,Na,Nefobs,i,nbb,nauto0,nloop,icol,isig,nnsig,igo,
     &        nround,imu,irt,ist,kstep,itmp,na0,tdauto,disp,nfirst,
     &        Lsumm,bldf0,bldf,kmu,aici0,aicit0,fhnote,igrp,cnote,n1mdl,
     &        fh0
*      INTEGER iticks
      LOGICAL argok,Lidotl,Fctok,inptok,Svldif,lmu,Lsadj,pcktd0,
     &        linv,ismd0,lmu0,lidold,redomd,lester,Ltdlom,Lhiddn,Lnoprt
      DIMENSION A(PLEN+2*PORDER),Trnsrs(PLEN),tair(2),cvl0(POTLR),
     &          cvlold(POTLR),a0(PLEN+2*PORDER),adj0(PLEN),trns0(PLEN)
c     ------------------------------------------------------------------
      DOUBLE PRECISION dpmpar
      INTEGER strinx
      LOGICAL dpeq
      EXTERNAL dpeq,dpmpar,strinx
c     ------------------------------------------------------------------
c     print header for output
c     ------------------------------------------------------------------
      IF(Prttab(LAUHDR))THEN
*       IF(Lpage)THEN
*        WRITE(Mt1,Ttlfmt)Newpg,Title(1:Ntitle),Kpage,Serno(1:Nser)
*        Kpage=Kpage+1
*       END IF
       CALL genSkip(LAUHDR)
       CALL writTagOneLine(Mt1,'h3','@',
     &                     'Automatic ARIMA Model Selection')
       WRITE(Mt1,1001)Cbr,Cbr,Cbr
 1001  FORMAT(/,'<p class="center">Procedure based closely on TRAMO ',
     &           ',method of Gomez and Maravall (2000)',a,/,
     &     '"Automatic Modeling Methods for Univariate Series",',a,/,
     &     'A Course in Time Series (Edited by D. Pena, G. C. ',
     &             'Tiao, R. S. Tsay),',a,/,
     &         'New York : J. Wiley and Sons</p>',/)
       WRITE(Mt1,1003)'regular <abbr title="autoregressive moving '//
     &             'average">ARMA</abbr> parameters',Maxord(1)
       IF(Sp.gt.1)WRITE(Mt1,1003)'seasonal <abbr '//
     &             'title="autoregressive moving average">ARMA'//
     &             '</abbr> parameters',Maxord(2)
       IF(Lautod)THEN
        WRITE(Mt1,1003)'regular differencing',Diffam(1)
        IF(Sp.gt.1)WRITE(Mt1,1003)'seasonal differencing',Diffam(2)
       ELSE
        WRITE(Mt1,1004)'Regular differencing',Diffam(1)
        IF(Sp.gt.1)WRITE(Mt1,1004)'Seasonal differencing',Diffam(2)
       END IF
       IF(Laccdf)WRITE(Mt1,1005)
 1003  FORMAT('<p><strong>Maximum order for ',a,' :</strong> ',i3,
     &        '</p>')
 1004  FORMAT('<p>',a,' set to ',i3,'</p>')
 1005  FORMAT('<p>Default model will be accepted if residuals pass ',
     &        'Ljung-Box test</p>')
      END IF
c     ------------------------------------------------------------------
c     If diagnostic output saved, save automatic modeling settings to
c     .udg file (BCM July 2008)
c     ------------------------------------------------------------------
      IF(Lsumm.gt.0)THEN
       WRITE(Nform,1100)'maxorder: ',(Maxord(i),i=1,2)
       IF(Lautod)THEN
        firstw='maxdiff:   '
        nfirst=9
       ELSE
        firstw='diff:      '
        nfirst=6
       END IF
       IF(Sp.gt.1)THEN
        WRITE(Nform,1100)firstw(1:nfirst),(Diffam(i),i=1,2)
       ELSE
        WRITE(Nform,1100)firstw(1:nfirst),Diffam(1),0
       END IF
       IF(Laccdf)THEN
        WRITE(Nform,1050)'acceptdefault: ','yes'
       ELSE
        WRITE(Nform,1050)'acceptdefault: ','no'
       END IF
       IF(Lchkmu)THEN
        WRITE(Nform,1050)'checkmu: ','yes'
       ELSE
        WRITE(Nform,1050)'checkmu: ','no'
       END IF
       IF(Lbalmd)THEN
        WRITE(Nform,1050)'balanced: ','yes'
       ELSE
        WRITE(Nform,1050)'balanced: ','no'
       END IF
       IF(Lmixmd)THEN
        WRITE(Nform,1050)'mixed: ','yes'
       ELSE
        WRITE(Nform,1050)'mixed: ','no'
       END IF
       IF(Exdiff.eq.0)THEN
        WRITE(Nform,1050)'exactdiff: ','no'
       ELSE IF(Exdiff.eq.1)THEN
        WRITE(Nform,1050)'exactdiff: ','yes'
       ELSE IF(Exdiff.eq.2)THEN
        WRITE(Nform,1050)'exactdiff: ','first'
       END IF
      END IF
c     ------------------------------------------------------------------
c     check to see if user requests that the outlier tables be saved;
c     if so, print out warning message.
c     ------------------------------------------------------------------
      fhnote=STDERR
      IF(Lquiet)fhnote=0
      IF(Savtab(LOTLFT).or.Savtab(LOTLIT))THEN
       CALL nWritln('Tables associated with the outlier spec cannot '//
     &              'be saved during',fhnote,Mt2,T,F)
       CALL writln('      automatic model selection.',fhnote,Mt2,F,T)
      END IF
c     ------------------------------------------------------------------
c     set initial "default" model to airline model
c     ------------------------------------------------------------------
      nbb=0
      itmp=0
      nloop=0
      nround=1
      firstw=' Checking  '
      nfirst=9
      igo=0
      CALL setdp(DNOTST,POTLR,cvlold)
      lidold=Lidotl
      IF(Lidotl)THEN
       IF(Ltstao)cvlold(AO)=Critvl(AO)
       IF(Ltstls)cvlold(LS)=Critvl(LS)
       IF(Ltsttc)cvlold(TC)=Critvl(TC)
*       IF(Ltstso)cvlold(SO)=Critvl(SO)
       Lotmod=F
      ELSE IF(Lotmod)THEN
       Ltstao=T
       Lidotl=T
       Critvl(AO)=BIGCV
       DO i=LAUOTH,LAUOTT
        Prttab(i)=F
        Savtab(i)=F
       END DO
      END IF
      fct2=1D0
      IF(Lsadj)fct2=Fct
      lmu=F
      imu=0
      kstep=0
      IF(Nb.gt.0)THEN
       imu=strinx(F,Grpttl,Grpptr,1,Ngrptl,'Constant')
       IF(imu.gt.0)lmu=T
      END IF
      inptok=T
      CALL mdlint()
      lpr=0
      lqr=0
      lps=0
      lqs=0
      lpr0=0
      ldr0=1
      lqr0=1
      lps0=0
      lds0=1
      lqs0=1
      IF(Lseff.or.Sp.eq.1)THEN
       lds0=0
       lqs0=0
      END IF
*    1 CALL mdlset(lpr0,ldr0,lqr0,lps0,lds0,lqs0,inptok)
      CALL mdlset(lpr0,ldr0,lqr0,lps0,lds0,lqs0,inptok)
      IF(.not.Lfatal)
     &   CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &               Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
      IF(Lfatal)RETURN
      CALL ssprep(T,F,F)
c     ------------------------------------------------------------------
c     Perform AIC tests on default model
c     ------------------------------------------------------------------
      argok=T
      lester=F
      IF(Itdtst.gt.0)THEN
       CALL tdaic(Trnsrs,A,Nefobs,Na,Frstry,lester,tdauto,Ltdlom,F,F,F,
     &            F,0,Lhiddn)
       IF(Lfatal)RETURN
      END IF
      IF((.not.lester).and.Lomtst.gt.0)THEN
       CALL lomaic(Trnsrs,A,Nefobs,Na,Frstry,lester,F,F,F,F,0,Lhiddn)
       IF(Lfatal)RETURN
      END IF
      IF((.not.lester).and.Leastr)THEN
       CALL easaic(Trnsrs,A,Nefobs,Na,Frstry,lester,F,F,F,F,0,Lhiddn)
       IF(Lfatal)RETURN
      END IF
      IF((.not.lester).and.(Luser.and.Ncusrx.gt.0))THEN
       CALL usraic(Trnsrs,A,Nefobs,Na,Frstry,lester,F,F,F,F,0,Lhiddn)
       IF(Lfatal)RETURN
       IF(Ncusrx.eq.0.and.Ch2tst)Ch2tst=F
      END IF
      IF(.not.lester.and.(Ch2tst.and.Nguhl.gt.0))THEN
       CALL chkchi(Trnsrs,A,Nefobs,Na,Frstry,lester,F,F,F,F)
       IF(Lfatal)RETURN
      END IF
      IF(lester)THEN
       CALL eWritln('A model estimation error has occurred during '//
     &              'AIC testing within',STDERR,Mt2,T,F)
       CALL writln('       the automatic model identification '//
     &             'procedure.  The error message',STDERR,Mt2,F,F)
       CALL writln('       appears below.',STDERR,Mt2,F,T)
       CALL prterr(nefobs,F)
       IF((.not.Lfatal).and.(.not.Convrg))CALL abend()
       IF(Lfatal)RETURN
      END IF
c     ------------------------------------------------------------------
c     Check for constant term in regressors with the default model if
c     user has not specified a mean term in the model.
c-----------------------------------------------------------------------
      IF(Lchkmu)THEN
       CALL chkmu(Trnsrs,A,Nefobs,Na,Frstry,kstep,Prttab(LAUDFT))
       IF(Lfatal)RETURN
       kmu=strinx(F,Grpttl,Grpptr,1,Ngrptl,'Constant')
       IF(kmu.gt.0)THEN
        lmu=T
       ELSE
        lmu=F
       END IF
      END IF
      CALL rgarma(T,Mxiter,Mxnlit,F,A,Na,nefobs,argok)
      IF(.not.Lfatal)THEN
       CALL prterr(nefobs,T)
       IF(.not.Convrg)THEN
        WRITE(STDERR,1090)
        CALL eWritln('Estimation failed to converge during the '//
     &               'automatic model identification procedure.',
     &               Mt1,Mt2,T,T)
        CALL abend()
       ELSE IF(.not.argok)THEN
        CALL abend()
       END IF
      END IF
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     If outlier identification specified, do it here for default model
c-----------------------------------------------------------------------
c      IF(.not.lester.and.Lidotl)THEN
      IF(Lidotl)THEN
c-----------------------------------------------------------------------
       IF(Prttab(LAUOTH))THEN
*        CALL prothd(Begtst,Endtst,Ltstao,Ltstls,Ltsttc,Ltstso,Ladd1,
*     &              Critvl)
        CALL prothd(Begtst,Endtst,Ltstao,Ltstls,Ltsttc,Ladd1,Critvl)
        IF(Lfatal)RETURN
       END IF
       IF(Prttab(LAUOTI))THEN
        CALL prtmdl(Lestim,Prttab(LESTES),Lcalcm,F,F,F,
     &              Prttab(LESTCM),F,Prttab(LESTES),itmp,
     &              Prttab(LESTES),F,Prttab(LESTIT))
       END IF
c-----------------------------------------------------------------------
c      Call routine that invokes automatic outlier identification,
c      prints out error messages and regenerates the regression
c      matrix (BCM April 2007)
c-----------------------------------------------------------------------
       CALL amidot(A,Trnsrs,Frstry,Nefobs,Priadj,Convrg,Fctok,argok)
       IF(Lfatal)RETURN
       nauto0=Natotl
       IF(Ltstao)cvl0(AO)=Critvl(AO)
       IF(Ltstls)cvl0(LS)=Critvl(LS)
       IF(Ltsttc)cvl0(TC)=Critvl(TC)
*       IF(Ltstso)cvl0(SO)=Critvl(SO)
c-----------------------------------------------------------------------
c     Recheck trading day and easter regressors after outlier
c     identification
c-----------------------------------------------------------------------
       isig=0
       CALL pass0(trnsrs,Frstry,isig,0,Prttab(LAUDFT))
       IF(Lfatal)RETURN
       kmu=strinx(F,Grpttl,Grpptr,1,Ngrptl,'Constant')
       IF(kmu.gt.0)THEN
        lmu=T
       ELSE
        lmu=F
       END IF
      END IF
c     ------------------------------------------------------------------
c     produce residual diagnostics for default model
c     ------------------------------------------------------------------
      CALL rgarma(Lestim,Mxiter,Mxnlit,F,a,na,nefobs,argok)
      IF(.not.Lfatal)THEN
       CALL prterr(nefobs,T)
       IF(.not.Convrg)THEN
        WRITE(STDERR,1090)
        CALL eWritln('Estimation failed to converge during the '//
     &               'automatic model identification procedure.',
     &               Mt1,Mt2,T,T)
        CALL abend()
       ELSE IF(.not.argok)THEN
        CALL abend()
       END IF
      END IF
      IF(.not.Lfatal)
     &   CALL mdlchk(a,na,Nefobs,Blpct0,Blq0,Bldf0,Rvr0,Rtval0)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     generate t-statistics for airline model, if no
c-----------------------------------------------------------------------
      IF(.not.Lidotl)CALL armats(tair)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     If default model accepted, leave routine
c-----------------------------------------------------------------------
      IF(Laccdf.and.Blpct0.lt.Pcr)THEN
       CALL mkmdsn(lpr0,ldr0,lqr0,lps0,lds0,lqs0,Bstdsn,Nbstds)
       IF(Lfatal)RETURN
       lpr=lpr0
       ldr=ldr0
       lqr=lqr0
       lps=lps0
       lds=lds0
       lqs=lqs0
       GO TO 70
      END IF
c-----------------------------------------------------------------------
c     save values for default model, update values for upcoming runs
c-----------------------------------------------------------------------
      lmu0=lmu
c      nr0=nround
      nround=1
      nloop=nloop+1
      kstep=1
      na0=Na
      CALL copy(A,Na,1,a0)
      aici0=Aicind
      aicit0=Aicint
      CALL copy(Adj,PLEN,1,adj0)
      CALL copy(Trnsrs,PLEN,1,trns0)
      pcktd0=Picktd
c-----------------------------------------------------------------------
c     Remove regressors from series before automatic modeling is
c     performed
c     ------------------------------------------------------------------
  10  CALL ssprep(T,F,F)
      CALL bkdfmd(T)
      IF(nloop.gt.1.and.Natotl.eq.0)GO TO 40
      IF(Nb.gt.0)THEN
       nbb=Nb
       CALL rmfix(trnsrs,Nbcst,Nrxy,2)
       IF(.not.Lfatal)
     &    CALL regvar(trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &                Priadj,Reglom,Nrxy,Begxy,frstry,T,Elong)
       IF(Lfatal)RETURN
      END IF
c     ------------------------------------------------------------------
      ldr=Diffam(1)
      lds=Diffam(2)
      IF(Lautod)THEN
       CALL iddiff(ldr,lds,Trnsrs,Nefobs,Frstry,A,Na,imu,lmu,Svldif,
     &             Lsumm)
       IF(Lfatal)RETURN
      ELSE
       CALL mdlint()
       CALL mdlset(0,ldr,0,0,lds,0,inptok)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
c     Identify orders of ARIMA model
c-----------------------------------------------------------------------
      ismd0=F
  50  IF(nloop.eq.1.or.lidold)THEN
       IF(nloop.gt.1.and.Nb.gt.0)THEN
        nbb=Nb
        CALL rmfix(trnsrs,Nbcst,Nrxy,2)
        IF(.not.Lfatal)
     &    CALL regvar(trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &                Priadj,Reglom,Nrxy,Begxy,frstry,T,Elong)
        IF(Lfatal)RETURN
       END IF
c-----------------------------------------------------------------------
       CALL amdid(lpr,ldr,lqr,lps,lds,lqs,trnsrs,Frstry,Nefobs,A,Na,Lmu,
     &            Lsumm,argok)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c    check to see if identified model is equivalent to default model
c    add check for mean term (BCM May 2004)
c-----------------------------------------------------------------------
       ismd0=((Sp.gt.1.and.
     &       (lpr.eq.lpr0.and.ldr.eq.ldr0.and.lqr.eq.lqr0.and.
     &        lps.eq.lps0.and.lds.eq.lds0.and.lqs.eq.lqs0)).or.
     &       (Sp.eq.1.and.
     &       (lpr.eq.lpr0.and.ldr.eq.ldr0.and.lqr.eq.lqr0))).and.
     &        (lmu.eqv.lmu0)
c     &        .and.Lidotl.and.nloop.eq.1)
      END IF
c-----------------------------------------------------------------------
c     put regressors back in regression matrix
c-----------------------------------------------------------------------
      lester=F
      IF(nbb.gt.0)THEN
       CALL addfix(trnsrs,Nbcst,0,2)
       IF(Lfatal)RETURN
       IF(.not.Lmu)THEN
        igrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Constant')
        IF (igrp.gt.0) THEN
         icol=Grp(igrp-1)
         CALL dlrgef(icol,Nrxy,1)
         IF(Lfatal)RETURN
        END IF
       END IF
       IF(Nb.gt.0)lester=T
       IF(nloop.eq.1)THEN
        nbb=0
        IF(ismd0)THEN
         CALL restor(T,F,F)
         CALL copy(a0,Na,1,A)
         CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &              Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
         IF(Lfatal)RETURN
         GO TO 30
        END IF
       END IF
c       IF((.not.(imu.eq.0.and.lmu)).and.(ismd0.and.nloop.eq.1))THEN
c-----------------------------------------------------------------------
c    Remove automatic outliers from model 
c-----------------------------------------------------------------------
       IF(nauto0.gt.0.and.igo.eq.0)CALL clrotl(Nrxy)
       IF(.not.Lfatal)
     &    CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &                Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
       IF(Lfatal)RETURN
      END IF
      IF(imu.eq.0.and.lmu)THEN
       icol=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Constant')
       IF(icol.eq.0)THEN
        CALL adrgef(DNOTST,'Constant','Constant',PRGTCN,F,F)
        IF(.not.Lfatal)
     &     CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,
     &                 Nrusrx,Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
        IF(Lfatal)RETURN
        lester=T
       END IF
      END IF
      IF(lester)THEN
       CALL rgarma(Lestim,Mxiter,Mxnlit,F,a,na,nefobs,argok)
       IF(Lfatal)RETURN
       CALL prterr(nefobs,T)
       IF(.not.Convrg)THEN
        WRITE(STDERR,1090)
        CALL eWritln('Estimation failed to converge during the '//
     &               'automatic model identification procedure.',
     &               Mt1,Mt2,T,T)
        CALL abend()
       ELSE IF(.not.argok)THEN
        CALL abend()
       END IF
      END IF
      IF(ismd0.and.nloop.eq.1)THEN
       CALL copy(a0,Na,1,A)
       GO TO 30
      END IF
c     ------------------------------------------------------------------
c     Perform AIC tests on recently identified model
c     ------------------------------------------------------------------
      argok=T
      lester=F
      CALL ssprep(T,F,F)
      IF(Itdtst.gt.0)THEN
       CALL tdaic(Trnsrs,A,Nefobs,Na,Frstry,lester,tdauto,Ltdlom,F,F,F,
     &            F,0,Lhiddn)
       IF(Lfatal)RETURN
      END IF
      IF((.not.lester).and.Lomtst.gt.0)THEN
       CALL lomaic(Trnsrs,A,Nefobs,Na,Frstry,lester,F,F,F,F,0,Lhiddn)
       IF(Lfatal)RETURN
      END IF
      IF((.not.lester).and.Leastr)THEN
       CALL easaic(Trnsrs,A,Nefobs,Na,Frstry,lester,F,F,F,F,0,Lhiddn)
       IF(Lfatal)RETURN
      END IF
      IF((.not.lester).and.(Luser.and.Ncusrx.gt.0))THEN
       CALL usraic(Trnsrs,A,Nefobs,Na,Frstry,lester,F,F,F,F,0,Lhiddn)
       IF(Lfatal)RETURN
       IF(Ncusrx.eq.0.and.Ch2tst)Ch2tst=F
      END IF
      IF(.not.lester.and.(Ch2tst.and.Nguhl.gt.0))THEN
       CALL chkchi(Trnsrs,A,Nefobs,Na,Frstry,lester,F,F,F,F)
       IF(Lfatal)RETURN
      END IF
      IF(lester)THEN
       CALL eWritln('A model estimation error has occurred during '//
     &              'AIC testing within',STDERR,Mt2,T,F)
       CALL writln('       the automatic model identification '//
     &             'procedure.  The error message',STDERR,Mt2,F,F)
       CALL writln('       appears below.',STDERR,Mt2,F,T)
       CALL prterr(nefobs,F)
       IF((.not.Lfatal).and.(.not.Convrg))CALL abend()
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
c     Redo automatic outlier identification on set of regressors without
c     automatic outliers identified earlier.
c-----------------------------------------------------------------------
  40  IF(Lidotl)THEN
c       IF(Nb.gt.0)THEN
c        CALL rgarma(Lestim,Mxiter,Mxnlit,F,A,Na,nefobs,argok)
c        IF(.not.Lfatal)CALL prterr(nefobs,T)
c       END IF
c-----------------------------------------------------------------------
       IF(Prttab(LAUOTH))THEN
*        CALL prothd(Begtst,Endtst,Ltstao,Ltstls,Ltsttc,Ltstso,Ladd1,
*     &              Critvl)
        CALL prothd(Begtst,Endtst,Ltstao,Ltstls,Ltsttc,Ladd1,Critvl)
        IF(Lfatal)RETURN
       END IF
       IF(Prttab(LAUOTI))THEN
        CALL prtmdl(Lestim,Prttab(LESTES),Lcalcm,F,F,F,
     &              Prttab(LESTCM),F,Prttab(LESTES),itmp,
     &              Prttab(LESTES),F,Prttab(LESTIT))
       END IF
c-----------------------------------------------------------------------
c      Call routine that invokes automatic outlier identification,
c      prints out error messages and regenerates the regression
c      matrix (BCM April 2007)
c-----------------------------------------------------------------------
       CALL amidot(A,Trnsrs,Frstry,Nefobs,Priadj,Convrg,Fctok,argok)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Check to see if automatic model passes tests
c-----------------------------------------------------------------------
      ELSE IF(.not.ismd0)THEN
       CALL tstmd1(Trnsrs,Frstry,A,Na,Nefobs,Blpct0,Rvr0,Rtval0,lpr,lps,
     &             lqr,lqs,ldr,lds,Lmu,Prttab(LAUMCH),aici0,pcktd0,adj0,
     &             trns0,tair)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Redo tests for trading day, Easter, other regressors
c     (if not airline model)
c-----------------------------------------------------------------------
       IF(Itdtst.gt.0.or.Leastr.or.(Luser.and.Ncusrx.gt.0).or.imu.eq.0)
     &    THEN
        argok=T
        lester=F
        IF(Itdtst.gt.0)THEN
         CALL tdaic(Trnsrs,A,Nefobs,Na,Frstry,lester,tdauto,Ltdlom,F,F,
     &              F,F,0,Lhiddn)
         IF(Lfatal)RETURN
        END IF
        IF((.not.lester).and.Lomtst.gt.0)THEN
         CALL lomaic(Trnsrs,A,Nefobs,Na,Frstry,lester,F,F,F,F,0,Lhiddn)
         IF(Lfatal)RETURN
        END IF
        IF((.not.lester).and.Leastr)THEN
         CALL easaic(Trnsrs,A,Nefobs,Na,Frstry,lester,F,F,F,F,0,Lhiddn)
         IF(Lfatal)RETURN
        END IF
        IF((.not.lester).and.(Luser.and.Ncusrx.gt.0))THEN
         CALL usraic(Trnsrs,A,Nefobs,Na,Frstry,lester,F,F,F,F,0,Lhiddn)
         IF(Lfatal)RETURN 
         IF(Ncusrx.eq.0.and.Ch2tst)Ch2tst=F
        END IF
        IF((.not.lester).and.(Ch2tst.and.Nguhl.gt.0))THEN
         CALL chkchi(Trnsrs,A,Nefobs,Na,Frstry,lester,F,F,F,F)
         IF(Lfatal)RETURN
        END IF
        IF(lester)THEN
         CALL eWritln('A model estimation error has occurred during '//
     &                'AIC testing within',STDERR,Mt2,T,F)
         CALL writln('       the automatic model identification '//
     &               'procedure.  The error message',STDERR,Mt2,F,F)
         CALL writln('       appears below.',STDERR,Mt2,F,T)
         CALL prterr(nefobs,F)
         IF((.not.Lfatal).and.(.not.Convrg))CALL abend()
         IF(Lfatal)RETURN
        END IF
c     ------------------------------------------------------------------
c     Check for constant term in regressors with the default model if
c     user has not specified a mean term in the model.
c-----------------------------------------------------------------------
        IF(Lchkmu)THEN
         CALL chkmu(Trnsrs,A,Nefobs,Na,Frstry,kstep,Prttab(LAUFNT))
         IF(Lfatal)RETURN
         kmu=strinx(F,Grpttl,Grpptr,1,Ngrptl,'Constant')
         IF(kmu.gt.0)THEN
          lmu=T
         ELSE
          lmu=F
         END IF
        END IF
        CALL rgarma(Lestim,Mxiter,Mxnlit,F,A,Na,nefobs,argok)
        IF(.not.Lfatal)THEN
         CALL prterr(nefobs,T)
         IF(.not.Convrg)THEN
          WRITE(STDERR,1090)
          CALL eWritln('Estimation failed to converge during the '//
     &                 'automatic model identification procedure.',
     &                 Mt1,Mt2,T,T)
          CALL abend()
         ELSE IF(.not.argok)THEN
          CALL abend()
         END IF
        END IF
        IF(Lfatal)RETURN
       END IF
      END IF
c-----------------------------------------------------------------------
c     Compare model to default (airline) model and change model
c     if necessary.
c-----------------------------------------------------------------------
   30 CALL mdlchk(a,na,Nefobs,blpct,blq,bldf,rvr,rtval)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     save first model in .udg file, if appropriate (June 2008)
c-----------------------------------------------------------------------
      IF(Lsumm.gt.0.and.nloop.eq.1)THEN
       CALL mkmdsn(lpr,ldr,lqr,lps,lds,lqs,mdl1st,n1mdl)
       WRITE(Nform,1050)'automdl.first: ',mdl1st(1:n1mdl)
      END IF
c-----------------------------------------------------------------------
      IF(Lidotl.and.nloop.le.2)THEN
       CALL pass2(Trnsrs,Frstry,lpr,ldr,lqr,lps,lds,lqs,lpr0,ldr0,lqr0,
     &            lps0,lds0,lqs0,Natotl,nauto0,blpct,blpct0,bldf,bldf0,
     &            rvr,rvr0,lmu,lmu0,A,a0,Na,na0,aici0,pcktd0,aicit0,
     &            adj0,trns0,fct2,Prttab(LAUMCH),Prttab(LAUFLB),ismd0,
     &            cvl0,nefobs,nloop,nround,igo)
       IF(Lfatal)RETURN
       IF(igo.eq.1) GO TO 10
       IF(igo.eq.2) GO TO 40
       IF(igo.eq.3) GO TO 50
      END IF
      IF(nloop.gt.0)THEN
       isig=0
       CALL pass0(trnsrs,Frstry,isig,1,Prttab(LAUFNT))
       IF(Lfatal)RETURN
       IF(isig.gt.0)THEN
        CALL rgarma(Lestim,Mxiter,Mxnlit,F,a,na,nefobs,argok)
        IF(.not.Lfatal)THEN
         CALL prterr(nefobs,T)
         IF(.not.Convrg)THEN
          WRITE(STDERR,1090)
          fh0=0
          IF(Prttab(LAUFNT))fh0=Mt1
          CALL eWritln('Estimation failed to converge during the '//
     &                 'automatic model identification procedure.',
     &                 fh0,Mt2,T,T)
          CALL abend()
         ELSE IF(.not.argok)THEN
          CALL abend()
         END IF
        END IF
        IF(Lfatal)RETURN
        IF(nloop.lt.3)nloop=3
c        GO TO 60
        kmu=strinx(F,Grpttl,Grpptr,1,Ngrptl,'Constant')
        IF(kmu.gt.0)THEN
         lmu=T
        ELSE
         lmu=F
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
c    Put final checks of model here, as found in TRAMO/SEATS -
c    Initialize variable to re-estimate model and set up "TRAMO"
c    integer variables for model.  { BCM June 2001  }
c-----------------------------------------------------------------------
c    Print out header for final checks ( BCM July 2007 )
c-----------------------------------------------------------------------
      IF(Prttab(LAUFNT))THEN
       CALL mkPOneLine(Mt1,'@','&nbsp;')
       CALL mkPOneLine(Mt1,'@','Final Checks for Identified Model')
      END IF
      redomd=F
c-----------------------------------------------------------------------
c    Put routine here to check for unit roots.
c-----------------------------------------------------------------------
      IF(Prttab(LAUFNT))THEN
       CALL mkPOneLine(Mt1,'@','&nbsp;')
       CALL mkPOneLine(Mt1,'@','Checking for Unit Roots.')
      END IF
      linv=T
      CALL chkrt1(irt,ist,tmpr,tmps,linv,Ubfin)
c-----------------------------------------------------------------------
c    If unit roots found, decrease appropriate AR orders by 1,
c    increase appropriate differencing order by 1, and re-estimate model
c-----------------------------------------------------------------------
      IF (irt.gt.0.or.ist.gt.0) then
       IF(irt.gt.0.and.ldr.le.2)THEn
        lpr=lpr-1
        ldr=ldr+1
        redomd=T
        IF(Prttab(LAUFNT))THEN
         CALL mkPOneLine(Mt1,'@','Regular unit root found.')
         CALL mkPClass(Mt1,'indent')
         WRITE(Mt1,1020) lpr,ldr,lqr,lps,lds,lqs
         CALL writTag(Mt1,'</p>')
        ENDIF
       end if
       IF(ist.gt.0.and.lds.le.1.and.(.not.Lseff))THEN
        lps=lps-1
        lds=lds+1
        redomd=T
        IF(Prttab(LAUFNT))THEN
         CALL mkPOneLine(Mt1,'@','Seasonal unit root found.')
         CALL mkPClass(Mt1,'indent')
         WRITE(Mt1,1020) lpr,ldr,lqr,lps,lds,lqs
         CALL writTag(Mt1,'</p>')
        end if
       end if
      ELSE
       IF(Prttab(LAUFNT))CALL mkPOneLine(Mt1,'@','No unit root found.')
      END IF
      IF(Svltab(LSLFUR))THEN
       IF(irt.eq.0.and.ist.eq.0)THEN
        CALL mkPOneLine(Ng,'center',
     &     '<strong>Unit roots in final model :</strong> none')
       ELSE IF(irt.gt.0.and.ist.eq.0)THEN
        CALL mkPOneLine(Ng,'center',
     &     '<strong>Unit roots in final model :</strong> nonseasonal')
       ELSE IF(irt.eq.0.and.ist.gt.0)THEN
        CALL mkPOneLine(Ng,'center',
     &     '<strong>Unit roots in final model :</strong> seasonal')
       ELSE
        CALL mkPOneLine(Ng,'center',
     &     '<strong>Unit roots in final model :</strong> nonseasonal'//
     &     ' seasonal')
       END IF
      END IF
      IF(Lsumm.gt.0)THEN
       IF(irt.eq.0.and.ist.eq.0)THEN
        WRITE(Nform,1050)'finalur: ','none'
       ELSE IF(irt.gt.0.and.ist.eq.0)THEN
        WRITE(Nform,1050)'finalur: ','nonseasonal'
       ELSE IF(irt.eq.0.and.ist.gt.0)THEN
        WRITE(Nform,1050)'finalur: ','seasonal'
       ELSE
        WRITE(Nform,1050)'finalur: ','nonseasonal seasonal'
       END IF
      END IF
c-----------------------------------------------------------------------
c   if model changes, re-estimate model.
c-----------------------------------------------------------------------
      IF(redomd)then
       CALL mdlint()
       CALL mdlset(lpr,ldr,lqr,lps,lds,lqs,inptok)
       IF(.not.Lfatal)
     &    CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &             Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c   if model changes, recheck significance of constant term
c   (BCM, April 2007)
c-----------------------------------------------------------------------
       kmu=strinx(F,Grpttl,Grpptr,1,Ngrptl,'Constant')
       IF(kmu.gt.0)
     &    CALL chkmu(Trnsrs,A,Nefobs,Na,Frstry,kstep,Prttab(LAUFNT))
c-----------------------------------------------------------------------
c   If automatic outliers are identified for the model, eliminate the
c   outliers from the model (BCM April 2007)
c-----------------------------------------------------------------------
       IF(Natotl.gt.0)THEN
        CALL clrotl(Nrxy)
        IF(.not.Lfatal)
     &     CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &                 Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
        IF(Lfatal)RETURN
       END IF
c-----------------------------------------------------------------------
       CALL rgarma(Lestim,Mxiter,Mxnlit,F,a,na,nefobs,argok)
       IF(.not.Lfatal)THEN
        CALL prterr(nefobs,T)
        IF(.not.Convrg)THEN
         WRITE(STDERR,1090)
         fh0=0
         IF(Prttab(LAUFNT))fh0=Mt1
         CALL eWritln('Estimation failed to converge during the '//
     &                'automatic model identification procedure.',
     &                fh0,Mt2,T,T)
         CALL abend()
        ELSE IF(.not.argok)THEN
         CALL abend()
        END IF
       END IF
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c   Redo automatic outlier identification (BCM April 2007)
c-----------------------------------------------------------------------
       IF(Lidotl.and.(.not.Lotmod))THEN
        CALL amidot(A,Trnsrs,Frstry,Nefobs,Priadj,Convrg,Fctok,Argok)
        IF(Lfatal)RETURN
       END IF
       CALL mdlchk(a,na,Nefobs,blpct,blq,bldf,rvr,rtval)
       IF(.not.Lfatal)CALL mkmdsn(lpr,ldr,lqr,lps,lds,lqs,Bstdsn,Nbstds)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
c   If model has nonseasonal differencing and MA coefficients or 
c   seasonal differencing and MA coefficients, check for
c   overdifferencing by seeing if sum of MA parameters is close to 1.
c-----------------------------------------------------------------------
      redoMd=F
      CALL tstodf(Trnsrs,Frstry,Nefobs,A,Na,Lsumm,lpr,ldr,lqr,
     &            lps,lds,lqs,Kstep,Lidotl,Lnoprt,FctOK,redoMd,argok)
      IF(Lfatal)RETURN
      IF(redoMd)THEN
       CALL mdlchk(a,na,Nefobs,blpct,blq,bldf,rvr,rtval)
       IF(.not.Lfatal)
     &    CALL mkmdsn(lpr,ldr,lqr,lps,lds,lqs,Bstdsn,Nbstds)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
c   if mean term not selected, see if t-statistic for residual mean
c   is significant.  If so, add constant to model
c-----------------------------------------------------------------------
      IF((.not.lmu).and.Lautod.and.rtval.gt.TWOPT5)THEN
       IF(Prttab(LAUFNT))
     &   CALL mkPOneLine(Mt1,'@','T-statistic for residual mean > 2.5;')
       IF(Lchkmu)THEN
        IF(Prttab(LAUFNT))THEN
         CALL mkPOneLine(Mt1,'@','&nbsp;')
         CALL mkPOneLine(Mt1,'@','Checking for Constant term.')
        END IF
        CALL adrgef(DNOTST,'Constant','Constant',PRGTCN,F,F)
        IF(Lfatal)RETURN
        CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &              Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
        IF(.not.Lfatal)THEN
          CALL rgarma(Lestim,Mxiter,Mxnlit,F,a,na,nefobs,argok)
        END IF
        IF(.not.Lfatal)THEN
         CALL prterr(nefobs,T)
         IF(.not.Convrg)THEN
          WRITE(STDERR,1090)
          fh0=0
          IF(Prttab(LAUFNT))fh0=Mt1
          CALL eWritln('Estimation failed to converge during the '//
     &                 'automatic model identification procedure.',
     &                 fh0,Mt2,T,T)
          CALL abend()
         ELSE IF(.not.argok)THEN
          CALL abend()
         END IF
        END IF
        IF(Lfatal)RETURN
        IF(Prttab(LAUFNT))CALL mkPOneLine(Mt1,'@',
     &                       'constant term added to identified model.')
       ELSE IF(Prttab(LAUFNT))THEN
        CALL mkPOneLine(Mt1,'@',
     &                  'constant term not added to identified model '//
     &                  'since checkmu=no in input specification file.')
       END IF
      END IF
c-----------------------------------------------------------------------
c     CODE MOVED - CHANGE IN LATEST VERSION OF TRAMO - BCM JUNE 2001
c     Check to see if insignificant ARMA coefficients can be eliminated
c-----------------------------------------------------------------------
   60 nnsig=0
      IF(Prttab(LAUFNT))THEN
       CALL mkPOneLine(Mt1,'@','&nbsp;')
       CALL mkPOneLine(Mt1,'@',firstw(1:nfirst)//' for insignificant '//
     &                 '<abbr title="autoregressive moving average">'//
     &                 'ARMA</abbr> coefficients.')
      END IF
      CALL tstmd2(nnsig,Nspobs,lpr,lqr,lps,lqs)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     If more than one coefficient is eliminated and outlier
c     identification is performed, reduce outlier
c     critical value and redo automatic model identification
c-----------------------------------------------------------------------
      IF(nnsig.gt.1.and.lidold.and.nloop.le.2)THEN
       IF((Ltstao.and.Critvl(AO).gt.TWOPT8).or.
     &    (Ltstls.and.Critvl(LS).gt.TWOPT8).or.
*     &    (Ltsttc.and.Critvl(TC).gt.TWOPT8).or.
*     &    (Ltstso.and.Critvl(SO).gt.TWOPT8))THEN
     &    (Ltsttc.and.Critvl(TC).gt.TWOPT8))THEN
        IF(Ltstao)THEN
         cvlold(AO)=Critvl(AO)
         Critvl(AO)=DMAX1(TWOPT8,Critvl(AO)-Critvl(AO)*Predcv)
         IF ((.not.dpeq(cvlold(AO),Critvl(AO))).and.Prttab(LAUFNT)) 
     &       WRITE(Mt1,1030)'Critical Value for AO outlier CHANGED TO:',
     &                      Critvl(AO)
        END IF
        IF(Ltstls)THEN
         cvlold(LS)=Critvl(LS)
         Critvl(LS)=DMAX1(TWOPT8,Critvl(LS)-Critvl(LS)*Predcv)
         IF ((.not.dpeq(cvlold(LS),Critvl(LS))).and.Prttab(LAUFNT)) 
     &       WRITE(Mt1,1030)'Critical Value for LS outlier CHANGED TO:',
     &                      Critvl(LS)
        END IF
        IF(Ltsttc)THEN
         cvlold(TC)=Critvl(TC)
         Critvl(TC)=DMAX1(TWOPT8,Critvl(TC)-Critvl(TC)*Predcv)
         IF ((.not.dpeq(cvlold(TC),Critvl(TC))).and.Prttab(LAUFNT)) 
     &       WRITE(Mt1,1030)'Critical Value for TC outlier CHANGED TO:',
     &                      Critvl(TC)
        END IF
*        IF(Ltstso)THEN
*         cvlold(SO)=Critvl(SO)
*         Critvl(SO)=DMAX1(TWOPT8,Critvl(SO)-Critvl(SO)*Predcv)
*         IF ((.not.dpeq(cvlold(SO),Critvl(SO))).and.Prttab(LAUFNT)) 
*     &       WRITE(Mt1,1030)'Critical Value for SO outlier CHANGED TO:',
*     &                      Critvl(SO)
*        END IF
        CALL rgarma(Lestim,Mxiter,Mxnlit,F,A,Na,nefobs,argok)
        IF(.not.Lfatal)THEN
         CALL prterr(nefobs,T)
         IF(.not.Convrg)THEN
          WRITE(STDERR,1090)
          fh0=0
          IF(Prttab(LAUFNT))fh0=Mt1
          CALL eWritln('Estimation failed to converge during the '//
     &                 'automatic model identification procedure.',
     &                 fh0,Mt2,T,T)
          CALL abend()
         ELSE IF(.not.argok)THEN
          CALL abend()
         END IF
        END IF
        IF(Lfatal.or.(.not.argok))RETURN
        nround=nround+1
        nloop=nloop+1
        IF(Prttab(LAUFNT))CALL mkPOneLine(Mt1,'@','More than one '//
     &             '<abbr title="autoregressive moving average">ARMA'//
     &             '</abbr> coefficient was found to be insignificant.')
        IF(Natotl.eq.0)THEN
         IF(Prttab(LAUMCH))THEN
          CALL writln('Since no outlier were found, model will be '//
     &                'changed to',Mt1,0,T,F)
          WRITE(Mt1,1020) lpr,ldr,lqr,lps,lds,lqs
          CALL writln('and automatic outlier identification will '//
     &                'be redone.',Mt1,0,F,T)
         END IF
         CALL mkmdsn(lpr,ldr,lqr,lps,lds,lqs,Bstdsn,Nbstds)
         IF(Lfatal)RETURN
        ELSE
         IF(Prttab(LAUMCH))THEN
          CALL mkPOneLine(Mt1,'@',
     &       'Identification of model and/or differencing order will '//
     &       'be redone.')
         END IF
        END IF
        GO TO 10
       END IF
      END IF
c-----------------------------------------------------------------------
c     If there are insignificant ARMA parameters, estimate reduced
c     model and check for more insignificant parameters.
c-----------------------------------------------------------------------
      IF(nnsig.gt.0)THEN
       CALL rgarma(Lestim,Mxiter,Mxnlit,F,A,Na,nefobs,argok)
       IF(.not.Lfatal)THEN
        CALL prterr(nefobs,T)
        IF(.not.Convrg)THEN
         WRITE(STDERR,1090)
         fh0=0
         IF(Prttab(LAUFNT))fh0=Mt1
         CALL eWritln('Estimation failed to converge during the '//
     &                'automatic model identification procedure.',
     &                fh0,Mt2,T,T)
         CALL abend()
        ELSE IF(.not.argok)THEN
         CALL abend()
        END IF
       END IF
       IF(Lfatal.or.(.not.argok))RETURN
       IF(Prttab(LAUMCH))THEN
        CALL writln('Due to insignificant <abbr title="autoregressive'//
     &              ' moving average">ARMA</abbr> coefficients, '//
     &              'model changed to',Mt1,0,T,F)
        WRITE(Mt1,1020) lpr,ldr,lqr,lps,lds,lqs
        CALL writTag(Mt1,'</p>')
       END IF
       CALL mkmdsn(lpr,ldr,lqr,lps,lds,lqs,Bstdsn,Nbstds)
       IF(Lfatal)RETURN
       IF(nfirst.eq.6)THEN
        firstw=' Rechecking'
        nfirst=11
       END IF
       GO TO 60
      END IF
c-----------------------------------------------------------------------
   70 IF(Armaer.gt.0)THEN
       CALL autoer(Armaer)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
      IF(Prttab(LAUMCH))THEN
       CALL mkPOneLine(Mt1,'center',
     &    '<strong>Final automatic model choice :</strong> '//
     &    Bstdsn(1:Nbstds))
      END IF
      IF(Svltab(LSLMU).or.Lsumm.gt.0)THEN
       IF(.not.Lchkmu)THEN 
        IF(Svltab(LSLMU))THEN
         CALL mkPOneLine(Ng,'center',
     &      '<strong>Constant Term :</strong> no testing performed')
        END IF
        IF(Lsumm.gt.0)WRITE(Nform,1050)'automean: ','nocheck'
       ELSE IF(imu.gt.0)THEN
        IF(Svltab(LSLMU))THEN
         CALL mkPOneLine(Ng,'center',
     &      '<strong>Constant Term :</strong> Specified in model')
        END IF
        IF(Lsumm.gt.0)WRITE(Nform,1050)'automean: ','userspecified'
       ELSE IF(lmu)THEN
        IF(Svltab(LSLMU))THEN
         CALL mkPOneLine(Ng,'center',
     &      '<strong>Constant Term :</strong> Significant')
        END IF
        IF(Lsumm.gt.0)WRITE(Nform,1050)'automean: ','yes'
       ELSE
        IF(Svltab(LSLMU))THEN
         CALL mkPOneLine(Ng,'center',
     &      '<strong>Constant Term :</strong> Not Significant')
        END IF
        IF(Lsumm.gt.0)WRITE(Nform,1050)'automean: ','no'
       END IF
      END IF
      IF(Lautod.and.Lsumm.gt.0)THEN
       WRITE(Nform,1110)'idnonseasonaldiff: ',ldr
       WRITE(Nform,1110)'idseasonaldiff: ',lds
      END IF
      IF(Lotmod.and.(.not.lidold))THEN
       Ltstao=F
       Lidotl=F
       Critvl(AO)=DNOTST
      END IF
      IF(Prttab(LAUMCH))CALL mkPOneLine(Mt1,'@',
     &                    'End of automatic model selection procedure.')
c-----------------------------------------------------------------------
 1020 FORMAT('  ',2(' (',i2,',',i2,',',i2,')'))
 1030 FORMAT(/,'<p>',a,1X,F12.3,'</p>',/)
 1050 FORMAT(a,a)
 1090 FORMAT(/,' ERROR: Estimation failed to converge during the ',
     &         'automatic model',
     &       /,'        identification procedure.')
 1100 FORMAT(a,2i5)
 1110 FORMAT(a,i5)
 1150 FORMAT(a,f12.6)
c-----------------------------------------------------------------------
      RETURN
      END
