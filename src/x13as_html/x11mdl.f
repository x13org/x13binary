C     Last change:  BCM  20 May 1999    8:36 am
      SUBROUTINE x11mdl(Sti,Muladd,Tmpma,Psuadd,Kpart,Kswv,Lgraf)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This subroutine performs an OLS regression on the irregular
c     component of an X-11 seasonal adjustment.  The regressors have
c     been previously chosen by the user.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
c-----------------------------------------------------------------------
      DOUBLE PRECISION MINONE,ONE,ZERO,SEVEN
      LOGICAL T,F,LCLOSE
      PARAMETER(T=.true.,F=.false.,ONE=1D0,ZERO=0D0,SEVEN=7D0,LCLOSE=T,
     &          MINONE=-1D0)
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'notset.prm'
      INCLUDE 'tbltitle.prm'
      INCLUDE 'desxrg.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'adj.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'xrgtbl.i'
      INCLUDE 'xrgmdl.cmn'
      INCLUDE 'x11reg.cmn'
      INCLUDE 'x11log.cmn'
      INCLUDE 'xrgfct.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11fac.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'inpt.cmn'
      INCLUDE 'ssap.prm'
      INCLUDE 'ssap.cmn'
      INCLUDE 'sspinp.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'svllog.prm'
      INCLUDE 'svllog.cmn'
      INCLUDE 'x11svl.i'
      INCLUDE 'rev.prm'
      INCLUDE 'rev.cmn'
      INCLUDE 'xrgum.cmn'
      INCLUDE 'tdtyp.cmn'
      INCLUDE 'xtdtyp.cmn'
      INCLUDE 'filext.prm'
c-----------------------------------------------------------------------
      INTEGER PA
      PARAMETER(PA=PLEN+2*PORDER)
c-----------------------------------------------------------------------
      CHARACTER tblttl*(PTTLEN),otltmp*(PCOLCR)
      DOUBLE PRECISION trnsrs,Sti,a,bb2,cvec,fcal,ftd,fhol,oldlam,ftmp,
     &                 dvec,tdwsum,tdwfix
      LOGICAL Psuadd,xm,fctok,trumlt,tdhol,Lgraf,lxao,lxls,lxtc,ldiag,
*     &        tdneg,isfix,ldum,lxso
     &        tdneg,isfix,ldum
      INTEGER Muladd,nbeg,nend,frstry,Kpart,fext,fext2,nfac,Tmpma,
     &        irridx,ndifum,igrp,oldfcn,nf2,nb2,rtype,iusr,icol,begcol,
     &        endcol,Kswv,oldnpm,ntbttl,ntmp,ivec,tdindx,iaic,
     &        irrend,lastpr,icol2,ncol0,nbck
      DIMENSION trnsrs(PLEN),a(PA),cvec(POTLR),fcal(PLEN),ftd(PLEN),
     &          fhol(PLEN),rtype(PB),ftmp(PLEN),bb2(PB),dvec(1),ivec(5),
     &          Sti(PLEN)
c-----------------------------------------------------------------------
      INTEGER strinx
      LOGICAL dpeq
      EXTERNAL dpeq,strinx
c-----------------------------------------------------------------------
      INTEGER fhb,fhc
      SAVE fhb,fhc
c-----------------------------------------------------------------------
      INCLUDE 'desxrg.var'
      INCLUDE 'filext.var'
c-----------------------------------------------------------------------
      DATA ivec/LXRTDF,LXRHLF,LXRCLF,LXRTDC,LXRCLC/
c-----------------------------------------------------------------------
      dvec(1)=ZERO
      CALL setdp(ZERO,PA,a)
      trumlt=(.not.Psuadd).and.Muladd.eq.0
      ldiag=Lsumm.gt.0.and.(Issap.LT.2.AND.Irev.lt.4)
      CALL setdp(ZERO,PXPX,Chlxpx)
c-----------------------------------------------------------------------
c    initialize temporary outlier variable (BCM May 2007)
c-----------------------------------------------------------------------
      CALL setchr(' ',PCOLCR,otltmp)
c-----------------------------------------------------------------------
c     Set limits for saving tables when forecasts and backcasts can be
c     saved (BCM October 2006)
c-----------------------------------------------------------------------
      ntmp=Posfob
      IF(Savfct)ntmp=Posffc
      nbck=Pos1ob
      IF(Savbct)nbck=Pos1bk
c-----------------------------------------------------------------------
c     Reset data transformation variables (to get correct AIC for
c     irregular regression)
c-----------------------------------------------------------------------
      oldlam=Lam
      oldfcn=Fcntyp
      IF(Fcntyp.ne.4)THEN
       Lam=ONE
       Fcntyp=4
      END IF
      oldnpm=Nestpm
      IF(oldnpm.gt.0)Nestpm=0
c-----------------------------------------------------------------------
c     Change Begspn and Endspn to match the model span, if necessary.
c-----------------------------------------------------------------------
      CALL dfdate(Begxrg,Begspn,Sp,nbeg)
      IF(nbeg.gt.0)CALL cpyint(Begxrg,2,1,Begspn)
      CALL dfdate(Endspn,Endxrg,Sp,nend)
      IF(nend.gt.0)CALL cpyint(Endxrg,2,1,Endspn)
c-----------------------------------------------------------------------
c     Restore values of Nfcst, Nbcst if this is last iteration
c-----------------------------------------------------------------------
      nf2=Nfcst
      nb2=Nbcst
      IF(Kpart.eq.3.AND.(Nfcstx.gt.0.or.Nbcstx.gt.0))THEN
       Nfcst=Nfcstx
       Nbcst=Nbcstx
c-----------------------------------------------------------------------
c     Reset values of X-11 pointer variables
c-----------------------------------------------------------------------
       Pos1bk=Pos1ob-Nbcst
       Posffc=Posfob+Nfcst
       Nofpob=Nspobs+Nfcst
       Nbfpob=Nspobs+Nfcst+Nbcst
      END IF
c-----------------------------------------------------------------------
c     If automatic AIC tests performed are to be done in the C
c     iteration, add td and easter[8] regressors in the first iteration
c     if they do not exist already.
c-----------------------------------------------------------------------
      IF(Kpart.eq.2)THEN
       IF(Easgrp.eq.0.and.Xeastr)THEN
        CALL addeas(Xeasvc(3)+Easidx,Easidx,1)
        Easgrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Easter')
        IF(Easgrp.eq.0)Easgrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,
     &                               'StatCanEaster')
        IF(Holgrp.eq.0)Holgrp=Easgrp
        IF(.not.Axrghl.and.Ixrghl.gt.0)Axrghl=T
       END IF
       IF((Tdgrp.eq.0.and.Stdgrp.eq.0).and.Xtdtst.gt.0)THEN
c-----------------------------------------------------------------------
c     Add trading day
c-----------------------------------------------------------------------
        tdindx=Xtdtst
        IF(tdindx.eq.3)tdindx=4
        IF(tdindx.eq.2)tdindx=3
        CALL addtd(Xaicst,Xaicrg,Xtdzro,Sp,tdindx)
        IF(Xtdtst.eq.2)THEN
         Stdgrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Stock Trading Day')
        ELSE
         Tdgrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Trading Day')
        END IF
        IF(.not.Axrgtd.and.Ixrgtd.gt.0)Axrgtd=T
       END IF
c-----------------------------------------------------------------------
c     If sliding spans or revisions history run, set starting values of
c     regression estimates to notset.
c-----------------------------------------------------------------------
c       IF((Issap.eq.2.and.(Ssinit.eq.1.or.Nssfxr.gt.0)).or.
       IF(Issap.eq.2.or.Irev.eq.4)THEN
        IF(Nssfxx.gt.0.or.Nrvfxr.gt.0.or.Ssxint.or.Revfxx)THEN
         CALL copy(Bx,PB,1,B)
        ELSE
         CALL setdp(DNOTST,PB,B)
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
c     add leap year regressor if additive seasonal adjustment is done.
c-----------------------------------------------------------------------
      IF((.NOT.(Issap.eq.2.or.Irev.eq.4)).and.Muladd.eq.1.and.
     &    Kpart.eq.2)THEN
       IF(strinx(T,Grpttl,Grpptr,1,Ngrptl,'Trading Day').gt.0)
     &    CALL adrgef(DNOTST,'Leap Year','Leap Year',PRGTLY,F,F)
      END IF
c-----------------------------------------------------------------------
c     store irregular into trnsrs
c-----------------------------------------------------------------------
      CALL dfdate(Endspn,Begspn,Sp,Nspobs)
      Nspobs=Nspobs+1
      IF(nbeg.gt.0.or.nend.gt.0.or.Issap.eq.2)THEN
       CALL dfdate(Begspn,Begsrs,Sp,Frstsy)
       Frstsy=Frstsy+1
       Nobspf=min(Nspobs+max(Nfcst-Fctdrp,0),Nobs-Frstsy+1)
      END IF
      irridx=Pos1ob+nbeg
      CALL copy(Sti(irridx),Nobspf,-1,trnsrs)
      irrend=irridx+Nspobs-1
c-----------------------------------------------------------------------
c     Prior adjust irregular, if necessary
c-----------------------------------------------------------------------
      ndifum=0
      IF(Haveum)CALL dfdate(Begspn,Begum,Sp,ndifum)
      IF((Muladd.eq.0.or.Muladd.eq.2).or.Haveum)
     &   CALL xrgtrn(trnsrs,irridx,irrend,Psuadd,Muladd,Tdgrp,
     &               Haveum,Umean,ndifum,Kswv)
c-----------------------------------------------------------------------
c     Check irregular for extreme values.  These extreme values will be
c     excluded from the regression matrix.
c-----------------------------------------------------------------------
      IF(Sigxrg.gt.ZERO)THEN
       IF(Muladd.eq.2)THEN
        Muladd=0
        CALL antilg(Sti,Pos1ob,Posfob)
       END IF
       fext=LXRIRX+Kpart-2
       CALL tdxtrm(Sti,Faccal,Tday,Sigxrg,Kpart,Muladd,fext,irridx,
     &             irrend)
       IF(Tmpma.eq.2)THEN
        Muladd=2
        CALL logar(Sti,Pos1ob,Posfob)
       END IF
      END IF
c-----------------------------------------------------------------------
c     Short description of the X-11 regression
c-----------------------------------------------------------------------
      fext=LXRXRG+Kpart-2
      IF(Prttab(fext))THEN
       CALL makttl(DSIDIC,dsiptr,PDSI,fext,PDSUM5,tblttl,ntbttl,T,F)
       IF(Lfatal)RETURN
*       IF(Lpage)THEN
*        WRITE(Mt1,Ttlfmt)Newpg,Title(1:Ntitle),Kpage,Serno(1:Nser)
*        Kpage=Kpage+1
*       END IF
       CALL genSkip(fext)
       CALL writTagOneLine(Mt1,'h3','@',tblttl(1:ntbttl))
       IF(Ngrp.gt.0)
     &    CALL desreg('Irregular Regression Model',Ngrp,Grpttl,Grpptr,
     &                Ngrptl)
       IF(.not.Lfatal)CALL prtmsp(Begxrg,Endxrg,Sp,T)
       IF(Lfatal)RETURN
       CALL mkPOneLine(Mt1,'@','Extreme Value Adjustment Method')
       IF(Sigxrg.gt.ZERO)WRITE(Mt1,1021)Sigxrg
 1021  FORMAT('<p>Exclude irregular values outside ',f5.2,
     &        ' sigma limit</p>')
       IF(Otlxrg)
     &    CALL mkPOneLine(Mt1,'@','Automatic AO outlier identification')
      END IF
c-----------------------------------------------------------------------
c     if C iteration, do automatic aic tests, if requested.
c     change to B iteration - march 6 2006 BCM
c-----------------------------------------------------------------------
      IF((Xtdtst.gt.0.or.Xeastr.or.(Xuser.and.Ncusrx.gt.0)).and.
     &    Kpart.eq.2)THEN
       CALL x11aic(irridx,irrend,Muladd,Psuadd,Trnsrs,a,nbeg,Sti,Kswv,
     &             Priadj,trumlt,Prttab(LXAICT),Svltab(LSLXTS),ldiag)
       IF(Lfatal)RETURN
       IF(Xtdtst.gt.0)THEN
        IF(Svltab(LSLXTS).or.ldiag)THEN
         iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Trading Day')
         IF(iaic.eq.0)iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,
     &                                'Stock Trading Day')
         IF(iaic.gt.0)THEN
          IF(Svltab(LSLXTS))CALL mkPOneLine(Ng,'@','AICtdX : accepted')
          IF(ldiag)WRITE(Nform,1025)'aictest.xtd: yes'
         ELSE
          IF(Svltab(LSLXTS))CALL mkPOneLine(Ng,'@','AICtdX : rejected')
          IF(ldiag)WRITE(Nform,1025)'aictest.xtd: no'
         END IF
        END IF
        Xtdtst=0
       END IF
       IF(Xeastr)THEN
        IF(Svltab(LSLXTS).or.ldiag)THEN
         iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Easter')
         IF(iaic.eq.0)iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,
     &                            'StatCanEaster')
         IF(iaic.gt.0)THEN
          IF(Svltab(LSLXTS))
     &       CALL mkPOneLine(Ng,'@','AICeasterX : accepted')
          IF(ldiag)THEN
           WRITE(Nform,1025)'aictest.xe: yes'
           WRITE(Nform,2025)'aictest.xe.window: ',Aicind
 2025      FORMAT(a,i3)
          END IF
         ELSE
          IF(Svltab(LSLXTS))
     &       CALL mkPOneLine(Ng,'@','AICeasterX : rejected')
          IF(ldiag)THEN
           WRITE(Nform,1025)'aictest.xe: no'
           WRITE(Nform,1025)'aictest.xe.window: 0'
          END IF
         END IF
        END IF
        Xeastr=F
       END IF
       IF(Xuser)THEN
        IF(Svltab(LSLXTS).or.ldiag)THEN
         iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,'User-defined')
         IF(iaic.gt.0)THEN
          IF(Svltab(LSLXTS))
     &       CALL mkPOneLine(Ng,'@','AICuserX : accepted')
          IF(ldiag)WRITE(Nform,1025)'aictest.xu: yes'
         ELSE
          IF(Svltab(LSLXTS))
     &       CALL mkPOneLine(Ng,'@','AICuserX : rejected')
          IF(ldiag)WRITE(Nform,1025)'aictest.xu: no'
         END IF
        END IF
        Xuser=F
       END IF
 1025  FORMAT(a)
       CALL loadxr(T)
       IF(Holgrp.gt.0.or.Tdgrp.gt.0.or.Stdgrp.gt.0)THEN
        tdhol=Holgrp.gt.0.and.Tdgrp.gt.0
        xm=Easidx.eq.0.and.(.not.(trumlt.and.tdhol.and.Xhlnln))
c-----------------------------------------------------------------------
c     If td or holiday not estimated, print warning message and leave
c     subroutine.
c-----------------------------------------------------------------------
       ELSE
        IF(.not.Lquiet)WRITE(STDERR,1030)PRGNAM
 1030   FORMAT(' NOTE: Because of the AIC test result, ',a,' ',
     &         'has removed any trading day,',/,7x,
     &         'stock trading day, or holiday regressors from the ',
     &         'irregular component',/,7x,
     &         'regression model.  No further model estimation will ',
     &         'be attempted.',//)
        IF(Prttab(LXAICT))WRITE(Mt1,1031)PRGNAM
        CALL errhdr
        WRITE(Mt2,1031)PRGNAM
 1031   FORMAT('<p><strong>NOTE:</strong> Because of the <abbr ',
     &         'title="Akaike information criterion">AIC</abbr> test ',
     &         'result, ',a,' has removed any trading day,',/,1x,
     &         'stock trading day, or holiday regressors from the ',
     &         'irregular component',/,1x,
     &         'regression model.  No further model estimation will ',
     &         'be attempted.</p>',/)
        IF(Tdtbl.eq.1.or.Tdtbl.eq.3)Tdtbl=Tdtbl-1
c-----------------------------------------------------------------------
c     If factors are to be saved, save vector of 0s (additive adj) or
c     1s (mult, log additive, psuedo additive).
c-----------------------------------------------------------------------
        IF(Savtab(LXRTDF+1).or.Savtab(LXRHLF+1).or.Savtab(LXRCLF+1).or.
     &     Savtab(LXRTDC+1).or.Savtab(LXRCLC+1))THEN
         ntmp=Posfob
         IF(Savfct)ntmp=Posffc
         IF(Muladd.eq.1)THEN
          CALL setdp(ZERO,ntmp,ftmp)
         ELSE
          CALL setdp(ONE,ntmp,ftmp)
         END IF
         DO icol=1,5
          fext2=ivec(icol)+1
          IF(Savtab(fext2))THEN
           IF(icol.ge.4.and.Kswv.gt.0)THEN
            CALL punch(Stptd,nbck,ntmp,fext2,F,F)
           ELSE
            CALL punch(ftmp,nbck,ntmp,fext2,F,F)
           END IF
          END IF
         END DO
        END IF
c-----------------------------------------------------------------------
c     Reset values of model parameters, X-11 pointer variables
c-----------------------------------------------------------------------
        IF(oldnpm.gt.0)Nestpm=oldnpm
        IF(oldfcn.ne.4)THEN
         Lam=oldlam
         Fcntyp=oldfcn
        END IF
        IF(Nfcst.ne.nf2.or.Nbcst.ne.nb2)THEN
         Nfcst=nf2
         Nbcst=nb2
         Pos1bk=Pos1ob-Nbcst
         Posffc=Posfob+Nfcst
         Nofpob=Nspobs+Nfcst
         Nbfpob=Nspobs+Nfcst+Nbcst
        END IF
        IF(nbeg.gt.0.or.nend.gt.0)
     &     CALL setspn(Sp,nend,nbeg,Begspn,Endspn,Begxrg,Endxrg,Nspobs,
     &                 Frstsy,Nobspf,Begsrs,Nobs,Nfcst,Fctdrp,Nomnfy,
     &                 Begadj,Adj1st)
c-----------------------------------------------------------------------
        IF(Axrgtd)Axrgtd=F
        IF(Axrghl)Axrghl=F
        IF(Axruhl)Axruhl=F
c-----------------------------------------------------------------------
        IF(ldiag)THEN
         WRITE(Nform,1025)'nfinalxreg:   1'  
         WRITE(Nform,1060)'finalxreg01: none'
        END IF
        RETURN
       END IF
      ELSE
c-----------------------------------------------------------------------
c     Set up the regression matrix
c-----------------------------------------------------------------------
       tdhol=Holgrp.gt.0.and.Tdgrp.gt.0
       xm=Easidx.eq.0.and.(.not.(trumlt.and.tdhol.and.Xhlnln))
       CALL regvar(trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &             Priadj,Reglom,Nrxy,Begxy,frstry,xm,Xelong)
       IF((.not.Lfatal).and.Iregfx.ge.2)THEN
        CALL rmfix(trnsrs,Nbcst,Nrxy,1)
        IF(.not.Lfatal)
     &    CALL regvar(trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &                Priadj,Reglom,Nrxy,Begxy,frstry,xm,Xelong)
       END IF
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     If both trading day and holiday factors are included, multiply the
c     holiday regressors by Daybar
c     BCM May 1999 - Operation no longer necessary
c-----------------------------------------------------------------------
c       IF((.not.Axruhl).and.tdhol)
c     &    CALL xrghol(irridx,Psuadd,Xlpyr,Daybar)
c-----------------------------------------------------------------------
c     Perform X-11 regression
c-----------------------------------------------------------------------
c       IF(Kpart.eq.3)THEN
c        IF(Prttab(LXRXMX))THEN
c         CALL prtshd('Irregular Component Regression Matrix',Begxy,Sp,
c     &               Nrxy)
c         IF(.not.Lfatal)CALL prtmtx(Begxy,Sp,Xy,Nrxy,Ncxy,Colttl,Colptr,
c     &                              Ncoltl)
c        END IF
c       END IF
c-----------------------------------------------------------------------
       IF(.not.Lfatal)CALL regx11(a)
       IF(.not.Lfatal.and.Armaer.eq.PSNGER)CALL prterx()
       IF(.not.Lfatal)CALL rgtdhl(a,nbeg)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
c     Do automatic AO outlier detection
c-----------------------------------------------------------------------
      IF(Otlxrg.and.(Irev.lt.4.or.(Irev.eq.4.and.Rvxotl)).and.
     &   (Issap.lt.2.or.(Issap.eq.2.and.Ssxotl)))THEN
       cvec(AO)=Critxr
       cvec(LS)=Critxr
       cvec(TC)=Critxr
*       cvec(SO)=Critxr
       lxao=T
       lxls=F
       lxtc=F
*       lxso=F
       ldum=F
       IF(Prttab(LXROHD).and.Prttab(fext))THEN
*        CALL prothd(Begxot,Endxot,lxao,lxls,lxtc,lxso,Ladd1x,cvec)
        CALL prothd(Begxot,Endxot,lxao,lxls,lxtc,Ladd1x,cvec)
        IF(Lfatal)RETURN
       END IF
*       CALL idotlr(lxao,lxls,lxtc,lxso,Ladd1x,cvec,Cvxrdc,Begxot,Endxot,
       CALL idotlr(lxao,lxls,lxtc,Ladd1x,cvec,Cvxrdc,Begxot,Endxot,
     &             Nspobs,Lestim,Mxiter,Mxnlit,ldum,a,trnsrs,Nobspf,
     &             Nfcst,Outfct,fctok,T,nbeg,Prttab(fext),
     &             Prttab(LXROTT),Prttab(LXROIT),Savtab(LXROIT),
     &             Prttab(LXROFT),Savtab(LXROFT),F,F)
       IF(Lfatal)RETURN
c     add new argument for svolit (BCM May 2007)
       IF(Savtab(LXROIT).and.Kpart.eq.3)
     &    CALL svolit(LCLOSE,0,'*',otltmp,1,ZERO,ZERO,ZERO,
     &                Savtab(LXROIT),T)
      END IF
      IF(Nfcst.gt.0.or.Nbcst.gt.0)THEN
       CALL regvar(trnsrs,Nobspf,Fctdrp,Nfcst,Nbcst,Userx,Bgusrx,
     &             Nrusrx,Priadj,Reglom,Nrxy,Begxy,frstry,xm,Xelong)
c       IF(((.not.Axruhl).and.tdhol).and.(.not.Lfatal))
c     &    CALL xrghol(Pos1ob,Psuadd,Xlpyr,Daybar)
       IF(Lfatal)RETURN
      END IF
      IF(Iregfx.ge.2)THEN
       CALL addfix(trnsrs,Nbcst,1,1)
       IF(.not.Lfatal)
     &    CALL regvar(trnsrs,Nobspf,Fctdrp,Nfcst,Nbcst,Userx,Bgusrx,
     &                Nrusrx,Priadj,Reglom,Nrxy,Begxy,frstry,xm,Xelong)
c       IF(((.not.Axruhl).and.tdhol).and.(.not.Lfatal))
c     &    CALL xrghol(Pos1ob,Psuadd,Xlpyr,Daybar)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
c     Print out results of OLS regression
c-----------------------------------------------------------------------
      IF(Kpart.eq.3)THEN
       CALL prtxrg(Lestim,Prttab(fext),Savtab(fext),Prttab(LXRXCM),
     &             Savtab(LXRXCM),fext,fhc,ldiag)
      ELSE
       CALL prtxrg(Lestim,Prttab(fext),Savtab(fext),F,F,fext,fhb,F)
      END IF
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Copy the series into trnsrs, and transform the irregular, if
c     necessary
c-----------------------------------------------------------------------
      IF(Xhlnln.and.trumlt.and.Easidx.eq.0.and.(.not.Axruhl.and.tdhol))
     &   THEN
       CALL copy(Sti(Pos1ob),Nspobs,-1,trnsrs)
       IF((Muladd.eq.0.or.Muladd.eq.2).or.Haveum)
     &   CALL xrgtrn(trnsrs,Pos1ob,Pos1ob+Nspobs-1,Psuadd,Muladd,Tdgrp,
     &               Haveum,Umean,ndifum,Kswv)
c-----------------------------------------------------------------------
c     Generate regression matrix
c-----------------------------------------------------------------------
       CALL regvar(trnsrs,Nobspf,Fctdrp,Nfcst,Nbcst,Userx,Bgusrx,Nrusrx,
     &             Priadj,Reglom,Nrxy,Begxy,frstry,xm,Xelong)
c       IF(((.not.Axruhl).and.tdhol).and.(.not.Lfatal))
c     &    CALL xrghol(Pos1ob,Psuadd,Xlpyr,Daybar)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
c     Print out and/or save the X-11 regression matrix
c-----------------------------------------------------------------------
      IF(Kpart.eq.3)THEN
       IF(Prttab(LXRXMX))THEN
        CALL genSkip(LXRXMX)
        CALL prtshd('Irregular Component Regression Matrix',Begxy,Sp,
     &              Nrxy)
        IF(.not.Lfatal)
     &     CALL prtmtx(Begxy,Sp,Xy,Nrxy,Ncxy,Colttl,Colptr,Ncoltl,
     &                 'Irregular Component Regression Matrix',
     &                 tbxdic(LXRXMX))
       END IF
       IF(.not.Lfatal.and.Savtab(LXRXMX))
     &    CALL savmtx(LXRXMX,Begxy,Sp,Xy,Nrxy,Ncxy,Colttl,Colptr,Ncoltl)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
c     Reset beginning and ending dates for span, if necessary.
c-----------------------------------------------------------------------
      lastpr=Posfob
      IF(Xdsp.gt.0.and.Kpart.eq.3)THEN
       nend=Xdsp
       lastpr=Posfob+Xdsp
      END IF
      IF(nbeg.gt.0.or.nend.gt.0)THEN
       CALL setspn(Sp,nend,nbeg,Begspn,Endspn,Begxrg,Endxrg,Nspobs,
     &             Frstsy,Nobspf,Begsrs,Nobs,Nfcst,Fctdrp,Nomnfy,Begadj,
     &             Adj1st)
       CALL regvar(trnsrs,Nobspf,Fctdrp,Nfcst,Nbcst,Userx,Bgusrx,Nrusrx,
     &             Priadj,Reglom,Nrxy,Begxy,frstry,xm,Xelong)
       IF(.not.Lfatal.and.Xhlnln)
     &    CALL kfcn(Begspn,Nrxy,Pos1ob,Xelong)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
c     Generate adjustment factors for regression variables
c-----------------------------------------------------------------------
      iusr=1
      DO icol=1,Nb
       IF(Rgvrtp(icol).eq.PRGTUD.and.Ncusrx.gt.0)THEN
        rtype(icol)=Usrtyp(iusr)
        iusr=iusr+1
       ELSE
        rtype(icol)=Rgvrtp(icol)
       END IF
      END DO
      IF(Havxtd.and.(.not.Haveum))THEN
c-----------------------------------------------------------------------
c     Set up "X-11 style" daily weights, if possible
c-----------------------------------------------------------------------
       igrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Trading Day')
       IF(igrp.gt.0)THEN
        CALL setdp(DNOTST,7,Dx11)
        begcol=Grp(igrp-1)
        endcol=Grp(igrp)-1
        Dx11(7)=ZERO
        IF(begcol.eq.endcol)THEN
         DO icol=1,5
          Dx11(icol)=B(begcol)
          IF(Muladd.ne.1)Dx11(icol)=ONE+Dx11(icol)
         END DO
         DO icol=6,7
          Dx11(icol)=(-5D0*B(begcol))/2D0
          IF(Muladd.ne.1)Dx11(icol)=ONE+Dx11(icol)
         END DO
        ELSE
         DO icol=begcol,endcol
          IF(Muladd.eq.1)THEN
           Dx11(icol-begcol+1)=B(icol)
          ELSE
           Dx11(icol-begcol+1)=ONE+B(icol)
          END IF
          Dx11(7)=Dx11(7)-B(icol)
         END DO
         IF(Muladd.ne.1)Dx11(7)=Dx11(7)+ONE
        END IF
        IF(Kpart.eq.3.and.ldiag)
     &     WRITE(Nform,1010)'x11tdwt:',(Dx11(icol),icol=1,7)
c-----------------------------------------------------------------------
c     If necessary, reweight trading day daily weights when there are
c     values less than zero
c-----------------------------------------------------------------------
        tdneg=F
        IF(Lxrneg)THEN
         tdwsum=ZERO
         tdwfix=ZERO
         ncol0=endcol-begcol+1
         DO icol=1,ncol0
          isfix=Regfxx(begcol+icol-1)
          IF(Dx11(icol).ge.ZERO)THEN
           IF(isfix)THEN
            tdwfix=tdwfix+Dx11(icol)
           ELSE
            tdwsum=tdwsum+Dx11(icol)
           END IF
          ELSE
           IF(.not.isfix)THEN
            Dx11(icol)=ZERO
            IF(.not.tdneg)tdneg=T
           END IF
          END IF
         END DO
         IF(Dx11(7).ge.ZERO)THEN
          tdwsum=tdwsum+Dx11(icol)
         ELSE
          Dx11(icol)=ZERO
          IF(.not.tdneg)tdneg=T
         END IF
         IF(tdneg)THEN
          IF(tdwsum.gt.ZERO)THEN
           DO icol=1,7
            icol2=begcol+icol-1
            IF(.not.(Regfxx(icol2).and.endcol.gt.begcol))THEN
             IF(Dx11(icol).gt.ZERO)
     &          Dx11(icol)=Dx11(icol)*((SEVEN-tdwfix)/tdwsum)
             IF(icol.le.ncol0)B(icol2)=Dx11(icol)-ONE
            END IF
           END DO
          ELSE
           WRITE(STDERR,1000)
 1000      FORMAT(' ERROR: Cannot generate factor necessary to ',
     &            'reweight trading day',/,
     &            '        daily weights - none of the unfixed daily ',
     &            'weights are greater',/,'       than zero.')
           CALL errhdr
           CALL eWritln('Cannot generate factor necessary to '//
     &                  'reweight trading day daily weights - '//
     &                  'none of the unfixed daily weights are '//
     &                  'greater than zero.',Mt1,Mt2,T,T)
           CALL abend()
           RETURN
          END IF
          IF(Kpart.eq.3.and.ldiag)
     &       WRITE(Nform,1010)'x11tdwt2:',(Dx11(icol),icol=1,7)
 1010     FORMAT(a,7(1x,f15.7))
          IF(Prttab(fext))THEN
           CALL mkPOneLine(Mt1,'@','&nbsp;')
           CALL errhdr
           CALL mkPOneLine(Mt2,'@','&nbsp;')
           IF(Kpart.eq.2)THEN
            WRITE(Mt1,1040)'B','B','preliminary','B'
            WRITE(Mt2,1040)'B','B','preliminary','B'
           ELSE
            WRITE(Mt1,1040)'C','C','final','C'
            WRITE(Mt2,1040)'C','C','final','C'
           END IF
 1040      FORMAT('<p><strong>NOTE:</strong> At least one of the ',
     &            'parameter estimates above yields a negative',/,
     &            ' daily weight for the ',a,' 16 table.  The ',
     &            'reweighting done to avoid',/,
     &            ' negative daily weights in Table ',a,' 16 ',
     &            'produced the following',/,
     &            ' parameter estimates, which were used to ',
     &             'obtain the ',a,/,
     &            ' trading day factors of ',a,' 16:</p>',/)
           CALL mkTableTag(Mt1,'@','@')
           CALL mkTableTag(Mt2,'@','@')
           IF(begcol.eq.endcol)THEN
            WRITE(Mt1,1050)B(begcol),Dx11(7)-ONE
            WRITE(Mt2,1050)B(begcol),Dx11(7)-ONE
 1050       FORMAT('<tr><th>Weekday</th><th>Weekend(**)</th></tr>',/,
     &             '<tr>',2('<td>',F8.4,'</td>'),'</tr>')
           ELSE
            CALL writTag(Mt1,'<tr>')
            CALL mkHeaderCellScope(Mt1,0,0,'col','Monday','Mon')
            CALL mkHeaderCellScope(Mt1,0,0,'col','Tuesday','Tue')
            CALL mkHeaderCellScope(Mt1,0,0,'col','Wednesday','Wed')
            CALL mkHeaderCellScope(Mt1,0,0,'col','Thursday','Thu')
            CALL mkHeaderCellScope(Mt1,0,0,'col','Friday','Fri')
            CALL mkHeaderCellScope(Mt1,0,0,'col','Saturday','Sat')
            CALL mkHeaderCellScope(Mt1,0,0,'col','Sunday derived',
     &                             'Sun*')
            CALL writTag(Mt1,'</tr>')
            WRITE(Mt1,1060)(B(icol),icol=begcol,endcol),Dx11(7)-ONE
            CALL writTag(Mt1,'<tr>')
            CALL mkHeaderCellScope(Mt2,0,0,'col','Monday','Mon')
            CALL mkHeaderCellScope(Mt2,0,0,'col','Tuesday','Tue')
            CALL mkHeaderCellScope(Mt2,0,0,'col','Wednesday','Wed')
            CALL mkHeaderCellScope(Mt2,0,0,'col','Thursday','Thu')
            CALL mkHeaderCellScope(Mt2,0,0,'col','Friday','Fri')
            CALL mkHeaderCellScope(Mt2,0,0,'col','Saturday','Sat')
            CALL mkHeaderCellScope(Mt2,0,0,'col','Sunday derived',
     &                             'Sun*')
            CALL writTag(Mt2,'</tr>')
            WRITE(Mt2,1060)(B(icol),icol=begcol,endcol),Dx11(7)-ONE
 1060       FORMAT('<tr>',7('<td>',F8.4,'</td>'),'</tr>')
            CALL writTag(Mt1,'<tr>')
            CALL writTag(Mt2,'<tr>')
            DO icol=begcol,endcol
             WRITE(Mt1,1061)B(icol)
             WRITE(Mt2,1061)B(icol)
            END DO
            WRITE(Mt1,1061)Dx11(7)-ONE
            WRITE(Mt2,1061)Dx11(7)-ONE
 1061       FORMAT('<td>',F9.4,'</td>')
            CALL writTag(Mt1,'</tr>')
            CALL writTag(Mt2,'</tr>')
           END IF
           CALL writTag(Mt1,'</table>')
           CALL mkPOneLine(Mt1,'@','&nbsp;')
           CALL writTag(Mt2,'</table>')
           CALL mkPOneLine(Mt2,'@','&nbsp;')
          END IF
         END IF
        END IF
       ELSE
        Dx11(1)=DNOTST
        IF(Muladd.eq.0)THEN
         igrp=strinx(T,Grpttx,Gpxptr,1,Ngrptx,'Stock Trading Day')
         IF(igrp.gt.0)THEN
          begcol=Grp(igrp-1)
          endcol=Grp(igrp)-1
          tdneg=F
          DO icol=begcol,endcol
           IF((B(icol).lt.MINONE.or.dpeq(B(icol),MINONE)))tdneg=T
          END DO
          IF(tdneg)THEN
           WRITE(STDERR,1070)
 1070      FORMAT(' ERROR: At least one of the stock trading day ',
     &            'regression coefficient',/,
     &            '        estimates from the irregular regression ',
     &            'model produce',/,
     &            '        nonpositive trading day factors for ',
     &            'multiplicative seasonal',/,'        adjustments.',//,
     &            '        Use the regression spec to estimate the ',
     &            'stock trading day effect.',//)
           CALL errhdr
           CALL eWritln('At least one of the stock trading day '//
     &                  'regression coefficient estimates from the ',
     &                  Mt1,Mt2,T,F)
           CALL writln('irregular regression model produce'//
     &                 ' nonpositive trading day factors for '//
     &                 'multiplicative seasonal adjustments.',
     &                  Mt1,Mt2,F,T)
           CALL writln('Use the regression spec to estimate the '//
     &                 'stock trading day effect.',Mt1,Mt2,T,T)
           CALL abend()
           RETURN
          END IF
         END IF
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
      CALL x11ref(fcal,ftd,fhol,Pos1bk,Muladd,Psuadd,Tdgrp,Stdgrp,
     &            Holgrp,Axruhl,ndifum,rtype,Nrxy,Ncxy,B,Xy,Nb,
     &            Easidx,Kswv,Calfrc,Xhlnln)
c-----------------------------------------------------------------------
c     Copy trading day, holiday, combined calendar factors
c-----------------------------------------------------------------------
      nfac=Posffc-Pos1bk+1
      IF(Xdsp.gt.0.and.Kpart.eq.3)nfac=nfac+Xdsp
      CALL copy(fcal(1),nfac,-1,Faccal(Pos1bk))
      IF(Havxtd)CALL copy(ftd(1),nfac,-1,Factd(Pos1bk))
      IF(Havxhl)CALL copy(fhol(1),nfac,-1,Facxhl(Pos1bk))
c-----------------------------------------------------------------------
c     Print out trading day factors
c-----------------------------------------------------------------------
      IF(Havxtd.and.(.not.Haveum))THEN
       fext=LXRTDF+Kpart-2
c-----------------------------------------------------------------------
       IF(Prttab(fext))CALL table(Factd,Pos1ob,lastpr,16,1,1,Dx11,fext)
       IF(Savtab(fext).OR.(Lgraf.and.Kpart.eq.3))THEN
        IF(Savfct.or.Savbct)THEN
         IF(Savtab(fext))CALL punch(Factd,nbck,ntmp,fext,F,F)
         IF(Lgraf.and.Kpart.eq.3)
     &      CALL punch(Factd,nbck,ntmp,fext,Lgraf,F)
        ELSE
         IF(Savtab(fext))CALL punch(Factd,Pos1ob,lastpr,fext,F,F)
         IF(Lgraf.and.Kpart.eq.3)
     &      CALL punch(Factd,Pos1ob,lastpr,fext,Lgraf,F)
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
c     Print out holiday factors
c-----------------------------------------------------------------------
      IF(Havxhl.and.(.not.Haveum))THEN
       fext=LXRHLF+Kpart-2
       IF(Prttab(fext))CALL table(Facxhl,Pos1ob,lastpr,21,1,1,dvec,fext)
       IF(Savtab(fext).OR.(Lgraf.and.Kpart.eq.3))THEN
        IF(Savfct.or.Savbct)THEN
         IF(Savtab(fext))CALL punch(Facxhl,nbck,ntmp,fext,F,F)
         IF(Lgraf.and.Kpart.eq.3)
     &      CALL punch(Facxhl,nbck,ntmp,fext,Lgraf,F)
        ELSE
         IF(Savtab(fext))CALL punch(Facxhl,Pos1ob,lastpr,fext,F,F)
         IF(Lgraf.and.Kpart.eq.3)
     &      CALL punch(Facxhl,Pos1ob,lastpr,fext,Lgraf,F)
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
c     Print out the combined calendar effects
c-----------------------------------------------------------------------
      IF((Havxtd.and.Havxhl).and.Haveum)THEN
       fext=LXRCLF+Kpart-2
       IF(Prttab(fext))CALL table(Faccal,Pos1ob,lastpr,22,1,1,dvec,fext)
       IF(Savtab(fext).OR.(Lgraf.and.Kpart.eq.3))THEN
        IF(Savfct.or.Savbct)THEN
         IF(Savtab(fext))CALL punch(Faccal,nbck,ntmp,fext,F,F)
         IF(Lgraf.and.Kpart.eq.3)
     &      CALL punch(Faccal,nbck,ntmp,fext,Lgraf,F)
        ELSE
         IF(Savtab(fext))CALL punch(Faccal,Pos1ob,lastpr,fext,F,F)
         IF(Lgraf.and.Kpart.eq.3)
     &      CALL punch(Faccal,Pos1ob,lastpr,fext,Lgraf,F)
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
c     Divide out the trading day effect from the irregular
c-----------------------------------------------------------------------
      IF(Axrgtd.or.Axrghl)THEN
       IF(Muladd.eq.2)THEN
        Muladd=0
        CALL antilg(Sti,Pos1ob,Posfob)
       END IF
       CALL divsub(Sti,Sti,Faccal,Pos1ob,Posfob)
       IF(Tmpma.eq.2)THEN
        Muladd=2
        CALL logar(Sti,Pos1ob,Posfob)
       END IF
      END IF
c-----------------------------------------------------------------------
c     IF prior trading day adjustment done, compute combined trading day
c     weights.
c-----------------------------------------------------------------------
      IF(Kswv.eq.3)THEN
       DO icol=1,7
        Dx11(icol)=Dx11(icol)+Dwt(icol)-ONE
       END DO
       IF(Kpart.eq.3.and.ldiag)
     &    WRITE(Nform,1010)'x11combtdwt:',(Dx11(icol),icol=1,7)
c-----------------------------------------------------------------------
       IF(Calfrc)THEN
        CALL addmul(Faccal,Faccal,Stptd,Pos1bk,Posffc)
        CALL addmul(Factd,Factd,Stptd,Pos1bk,Posffc)
       ELSE
c-----------------------------------------------------------------------
        igrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Trading Day')
        IF(igrp.gt.0)THEN
         begcol=Grp(igrp-1)
         endcol=Grp(igrp)-1
        ELSE
         begcol=0
         endcol=0
        END IF
        DO icol=1,Nb
         IF(icol.ge.begcol.and.icol.le.endcol)THEN
          bb2(icol)=Dx11(icol-begcol+1)-ONE
         ELSE
          bb2(icol)=B(icol)
         END IF
        END DO
        Kswv=4
        CALL x11ref(fcal,ftd,ftmp,Pos1bk,Muladd,Psuadd,Tdgrp,Stdgrp,
     &              Holgrp,Axruhl,ndifum,rtype,Nrxy,Ncxy,bb2,Xy,Nb,
     &              Easidx,Kswv,Calfrc,Xhlnln)
c-----------------------------------------------------------------------
c     Copy trading day, combined calendar factors
c-----------------------------------------------------------------------
        nfac=Posffc-Pos1bk+1
        IF(Xdsp.gt.0.and.Kpart.eq.3)nfac=nfac+Xdsp
        CALL copy(fcal(1),nfac,-1,Faccal(Pos1bk))
        CALL copy(ftd(1),nfac,-1,Factd(Pos1bk))
       END IF
       Kswv=3
       IF(Kpart.eq.3)Kswv=4
      ELSE IF(Kswv.eq.0.and.Kpart.eq.3.and.Ixreg.eq.2)THEN
       IF(Axrgtd)Kswv=2
      END IF
c-----------------------------------------------------------------------
      IF(Tmpma.eq.2)Muladd=Tmpma
      IF(oldfcn.ne.4)THEN
       Lam=oldlam
       Fcntyp=oldfcn
      END IF
      IF(oldnpm.gt.0)Nestpm=oldnpm
c-----------------------------------------------------------------------
c     Reset values of X-11 pointer variables
c-----------------------------------------------------------------------
      IF(Kpart.eq.3)THEN
       IF(Nfcst.ne.nf2.or.Nbcst.ne.nb2)THEN
        Nfcst=nf2
        Nbcst=nb2
        Pos1bk=Pos1ob-Nbcst
        Posffc=Posfob+Nfcst
        Nofpob=Nspobs+Nfcst
        Nbfpob=Nspobs+Nfcst+Nbcst
       END IF
c-----------------------------------------------------------------------
c     Get regression trading day factors for type of month table
c-----------------------------------------------------------------------
       IF(Tdtbl.gt.0)THEN
        IF(.not.Axrgtd)THEN
         IF(Tdtbl.eq.1.or.Tdtbl.eq.3)Tdtbl=Tdtbl-1
        ELSE IF(Irev.lt.4)THEN
         CALL getxtd(Factd,Begspn,Pos1bk,nfac,Muladd)
        END IF
       END IF
c-----------------------------------------------------------------------
c     If sliding spans is done with initial values from the original
c     estimation, reset the value of the x-11 regression starting
c     values OR
c-----------------------------------------------------------------------
       IF((Issap.eq.1.AND.Nssfxr.gt.0).and.
     &    (Irev.lt.4.AND.Nrvfxr.gt.0))CALL copy(B,PB,1,Bx)
c-----------------------------------------------------------------------
c     IF revisions history is done with the refresh option, set saved
c     X-11 regression model to current values.
c-----------------------------------------------------------------------
       IF(Lrfrsh.and.Irev.eq.4)CALL loadxr(T)
c-----------------------------------------------------------------------
c     Copy trading factors into sliding spans variables, if this is a
c     transparent seasonal adjusment run
c-----------------------------------------------------------------------
       IF(Issap.eq.2.and.Itd.eq.1)
     &    CALL ssrit(Factd,Pos1ob,lastpr,1,Series)
c-----------------------------------------------------------------------
       IF(ldiag)THEN
        IF(Nbx.gt.0)THEN
         CALL svfnrg('finalxreg',Ngrp,Grpttl,Grpptr,Ngrptl)
        ELSE
         WRITE(Nform,1025)'nfinalxreg:   1'  
         WRITE(Nform,1060)'finalxreg01: none'
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
      RETURN
      END

