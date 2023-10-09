c     Last change: 8/01/2022 - add tcrate in .udg file
C     Last change: 9/29/2021 - fix the index link for Maximized
c     log-likelihood and model selection criteria if there is no a table
C     previous change:  SRD  25 Jan 100    1:30 pm
      SUBROUTINE arima(Hvmdl,Extok,Lx11,Lseats,Lgraf)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Estimates regression models with ARIMA errors
c-----------------------------------------------------------------------
c Name  Type Description
c-----------------------------------------------------------------------
c a       d  Pa/na long of innovation errors
c-----------------------------------------------------------------------
c     Variable typing and initialization
c-----------------------------------------------------------------------
      LOGICAL T,F
      DOUBLE PRECISION ZERO,ONE
      INTEGER BADITR
      PARAMETER(T=.true.,F=.false.,ZERO=0D0,ONE=1D0,BADITR=500)
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'tbllog.prm'
c-----------------------------------------------------------------------
      INCLUDE 'filext.prm'
      INCLUDE 'model.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'prior.prm'
      INCLUDE 'rev.prm'
      INCLUDE 'svllog.prm'
c-----------------------------------------------------------------------
      INCLUDE 'adj.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'fxreg.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'inpt.cmn'
      INCLUDE 'htmlout.cmn'
      INCLUDE 'lkhd.cmn'
      INCLUDE 'lzero.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'missng.cmn'
      INCLUDE 'model.cmn'
      INCLUDE 'mq3.cmn'
      INCLUDE 'orisrs.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'priadj.cmn'
      INCLUDE 'prior.cmn'
      INCLUDE 'prittl.cmn'
      INCLUDE 'priusr.cmn'
      INCLUDE 'rho.cmn'
      INCLUDE 'rev.cmn'
      INCLUDE 'svllog.cmn'
      INCLUDE 'seatad.cmn'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'tdtyp.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'tukey.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11fac.cmn'
      INCLUDE 'x11log.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'x11ptr.cmn'
c-----------------------------------------------------------------------
      INCLUDE 'mdlsvl.i'
      INCLUDE 'mdltbl.i'
      INCLUDE 'spcsvl.i'
      INCLUDE 'spctbl.i'
      INCLUDE 'tbllog.i'
c     ------------------------------------------------------------------
      INTEGER PA
      PARAMETER(PA=PLEN+2*PORDER)
c     ------------------------------------------------------------------
      REAL ticks
      CHARACTER begstr*(10),cfcst*(3),icoltl*(PCOLCR),tmpttl*(PCOLCR),
     &          tmpstr*(20)
      DOUBLE PRECISION a,trnsrs,orix,orixmv,ftd,fao,fls,ftc,fusr,fmv,
     &                 fhol,fsea,fcyc,mape,dvec,cv,Temp,fcstx,bcstx,
     &                 untfct,untbct,cvbak,fso,tval,rvar,dw,ken
      LOGICAL lester,Extok,Lx11,Lseats,litr,Hvmdl,lidotl,fctok,lfm,
     &        Lgraf,gudrun,ldiag,ltmp,lauto,lsadj,ltdlom,lautid
      INTEGER i,frstry,idate,na,nefobs,n,nbeg,nf2,tdreg,iaic,icol,
     &        nend,nlagbl,i1,i2,hvstar,begopr,endopr,itmp,nobtst,iao,
     &        ils,itc,iramp,itls,iauto,iuser,iso,outdec,ilom,lastpr,
     &        iseq,nchr,rtype,ntmpcr,ipos
      DIMENSION a(PA),idate(2),trnsrs(PLEN),orix(PLEN),orixmv(PLEN),
     &          ftd(PLEN),fao(PLEN),fls(PLEN),ftc(PLEN),fusr(PLEN),
     &          fmv(PLEN),fhol(PLEN),mape(4),dvec(1),
     &          fsea(PLEN),Temp(PLEN),fcstx(PFCST),bcstx(PFCST),
     &          cvbak(POTLR),untfct(PFCST),untbct(PLEN),tval(PB),
     &          fcyc(PLEN),fso(PLEN)
c     ------------------------------------------------------------------
      INTEGER PR
      PARAMETER (PR=PLEN/4)
      INCLUDE 'autoq.cmn'
c-----------------------------------------------------------------------
c      INTEGER iticks
      LOGICAL istrue,dpeq
      INTEGER strinx,nblank
      DOUBLE PRECISION setcv,setcvl,calcqs,chisq,kendalls
      EXTERNAL strinx,istrue,dpeq,setcv,nblank,setcvl,calcqs,chisq,
     &         kendalls
c     ------------------------------------------------------------------
      COMMON /work  / Temp
c-----------------------------------------------------------------------
      CHARACTER num*(2),cdef*(1)
      DIMENSION num(4)
c-----------------------------------------------------------------------
      DATA num/'st','nd','rd','th'/
c-----------------------------------------------------------------------
      INCLUDE 'filext.var'
c-----------------------------------------------------------------------
c     Input series, regression variables, model, options
c-----------------------------------------------------------------------
      itmp=0
      na=0
      lester=F
      fctok=T
      lidotl=Ltstao.or.Ltstls.or.Ltsttc
      Natotl=0
      lauto=Lautom.or.Lautox
      lsadj=Lx11.or.Lseats
      gudrun=Issap.LT.2.AND.Irev.lt.4
      ldiag=Lsumm.gt.0.and.gudrun
      lautid=lauto.and.gudrun
      ltdlom=Kfulsm.eq.2
      CALL setdp(ZERO,PXPX,Chlxpx)
      ilom=Priadj
      ltmp=F
      QsRsd=DNOTST
      QsRsd2=DNOTST
c-----------------------------------------------------------------------
c     Change Begspn and Endspn to match the model span, if necessary.
c-----------------------------------------------------------------------
      nbeg=0
      nend=0
      IF(Ldestm)THEN
       CALL dfdate(Begmdl,Begspn,Sp,nbeg)
       CALL dfdate(Endspn,Endmdl,Sp,nend)
       IF(nbeg.gt.0)CALL cpyint(Begmdl,2,1,Begspn)
       IF(nend.gt.0)CALL cpyint(Endmdl,2,1,Endspn)
      END IF
c-----------------------------------------------------------------------
c     Process the series
c-----------------------------------------------------------------------
      CALL dfdate(Endspn,Begspn,Sp,Nspobs)
      Nspobs=Nspobs+1
      IF(nbeg.gt.0.or.nend.gt.0.or.Issap.eq.2)THEN
       CALL dfdate(Begspn,Begsrs,Sp,Frstsy)
       Frstsy=Frstsy+1
       Nomnfy=Nobs-Frstsy+1
       Nobspf=min(Nspobs+Nfdrp,Nomnfy)
       CALL dfdate(Begspn,Begadj,Sp,Adj1st)
       Adj1st=Adj1st+1
      END IF
      CALL setdp(ZERO,PLEN,trnsrs)
c      write(Mtprof,*) ' Sto(Pos1ob+nbeg) = ',Sto(Pos1ob+nbeg)
      CALL copy(Sto(Pos1ob+nbeg),Nobspf,-1,trnsrs)
c      write(Mtprof,*) ' trnsrs(1) = ',trnsrs(1)
c----------------------------------------------------------------------
c     IF critical value not set, replace with value based on length of
c     series
c----------------------------------------------------------------------
      cv=DNOTST
      IF((Ltstao.and.dpeq(Critvl(AO),DNOTST)).or.
     &   (Ltstls.and.dpeq(Critvl(LS),DNOTST)).or.
*     &   (Ltsttc.and.dpeq(Critvl(TC),DNOTST)).or.
*     &   (Ltstso.and.dpeq(Critvl(SO),DNOTST)))THEN
     &   (Ltsttc.and.dpeq(Critvl(TC),DNOTST)))THEN
       CALL dfdate(Endtst,Begtst,Sp,nobtst)
       nobtst=nobtst+1
       IF(Cvtype)THEN
        cv=setcvl(nobtst,Cvalfa)
       ELSE
        cv=setcv(nobtst,Cvalfa)
       END IF
       IF(dpeq(cv,DNOTST))THEN
        CALL abend()
        RETURN
       END IF
       IF(Ltstao.and.dpeq(Critvl(AO),DNOTST))Critvl(AO)=cv
       IF(Ltstls.and.dpeq(Critvl(LS),DNOTST))Critvl(LS)=cv
       IF(Ltsttc.and.dpeq(Critvl(TC),DNOTST))Critvl(TC)=cv
*       IF(Ltstso.and.dpeq(Critvl(SO),DNOTST))Critvl(SO)=cv
      END IF
c-----------------------------------------------------------------------
c     Write out model span and outlier testing span into diagnostics 
c     summary file, if requested
c-----------------------------------------------------------------------
      IF(Lsumm.gt.0.and.gudrun)THEN
       i1=Begmdl(MO)
       IF(i1.gt.4)i1=4
       i2=Endmdl(MO)
       IF(i2.gt.4)i2=4
       IF(Sp.eq.12.or.Sp.eq.4)THEN
        WRITE(Nform,1000)'modelspan: ',Begmdl(MO),num(i1),
     &                   Moqu(1:nblank(Moqu)),Begmdl(YR),Endmdl(MO),
     &                   num(i2),Moqu(1:nblank(Moqu)),Endmdl(YR)
       ELSE IF(Sp.eq.1)THEN
        WRITE(Nform,1001)'modelspan: ',Begmdl(YR),Endmdl(YR)
       ELSE
        WRITE(Nform,1000)'modelspan: ',Begmdl(MO),num(i1),
     &                   'period',Begmdl(YR),Endmdl(MO),num(i2),
     &                   'period',Endmdl(YR)
       END IF
       WRITE(Nform,1061)'nobsmodelspan: ',Nspobs
       IF(lidotl)THEN
        i1=Begtst(MO)
        IF(i1.gt.4)i1=4
        i2=Endtst(MO)
        IF(i2.gt.4)i2=4
        IF(Sp.eq.12.or.Sp.eq.4)THEN
         WRITE(Nform,1000)'outlierspan: ',Begtst(MO),num(i1),
     &                    Moqu(1:nblank(Moqu)),Begtst(YR),Endtst(MO),
     &                    num(i2),Moqu(1:nblank(Moqu)),Endtst(YR)
        ELSE IF(Sp.eq.1)THEN
         WRITE(Nform,1001)'outlierspan: ',Begtst(YR),Endtst(YR)
        ELSE
         WRITE(Nform,1000)'outlierspan: ',Begtst(MO),num(i1),
     &                    'period',Begtst(YR),Endtst(MO),num(i2),
     &                    'period',Endtst(YR)
        END IF
        WRITE(Nform,1061)'nobsoutlierspan: ',nobtst
       END IF
       IF(Itdtst.gt.0.or.Lomtst.gt.0.or.Leastr.or.Luser)THEN
        tmpstr = ' '
        ipos=2
        IF(Itdtst.gt.0)THEN
         tmpstr(ipos:(ipos+1))='td'
         ipos=5
        END IF
        IF(Lomtst.eq.1)THEN
         tmpstr(ipos:(ipos+2))='lom'
         ipos=ipos+4
        ELSE IF(Lomtst.eq.2)THEN
         tmpstr(ipos:(ipos+2))='loq'
         ipos=ipos+4
        ELSE IF(Lomtst.eq.3)THEN
         tmpstr(ipos:(ipos+5))='lpyear'
         ipos=ipos+7
        END IF
        IF(Leastr)THEN
         tmpstr(ipos:(ipos+5))='easter'
         ipos=ipos+7
        END IF
        IF(Luser)THEN
         tmpstr(ipos:(ipos+3))='user'
         ipos=ipos+5
        END IF
        WRITE(Nform,1060)'aictest:',tmpstr(1:ipos)
       ELSE
        WRITE(Nform,1060)'aictest:',' none'
       END IF
      END IF
c-----------------------------------------------------------------------
c     BoxCox transform the (prior adjusted, if requested) data.
c Lam=1 means no transformation, Lam=0 is the log transform,
c this is all part of the Box-Cox transformation.
c-----------------------------------------------------------------------
      IF(Lmvaft.or.Ln0aft)THEN
       CALL trnfcn(trnsrs,Nspobs,Fcntyp,Lam,trnsrs)
      ELSE
       CALL trnfcn(trnsrs,Nobspf,Fcntyp,Lam,trnsrs)
      END IF
      IF(Lfatal)RETURN
c     ------------------------------------------------------------------
      IF(Prttab(LTRNDT))THEN
       CALL genSkip(LTRNDT)
       CALL prtshd(
     &        'Transformed (prior-adjusted) data for regARIMA modeling',
     &             Begspn,Sp,Nspobs)
       IF(Lfatal)RETURN
       outdec=Kdec
       IF((.not.dpeq(Lam,ONE)).and.outdec.lt.3)outdec=3
       CALL prttbl(Begspn,Sp,trnsrs,Nspobs,'Data',outdec,'xxx')
      END IF
c-----------------------------------------------------------------------
      IF(Savtab(LTRNDT))THEN
       CALL savtbl(LTRNDT,Begspn,1,Nspobs,Sp,trnsrs,Serno,Nser,F)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
c     Set up the regression matrix
c-----------------------------------------------------------------------
      CALL regvar(trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &            Priadj,Reglom,Nrxy,Begxy,frstry,T,Elong)
      IF((.not.Lfatal).and.Iregfx.ge.2)THEN
       CALL rmfix(trnsrs,Nbcst,Nrxy,1)
       IF(.not.Lfatal)
     &    CALL regvar(trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &                Priadj,Reglom,Nrxy,Begxy,frstry,T,Elong)
      END IF
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c      IF(Prttab(LREGDT).and.Nb.gt.0)THEN
c       CALL genSkip(LREGDT)
c       CALL prtshd('Regression Matrix',Begxy,Sp,Nrxy)
c       IF(.not.Lfatal)
c     &    CALL prtmtx(Begxy,Sp,Xy,Nrxy,Ncxy,Colttl,Colptr,Ncoltl,
c     &                'Regression Matrix',tbxdic(LREGDT))
c       IF(Lfatal)RETURN
c      END IF
c-----------------------------------------------------------------------
c     Identify the differencing in the model
c-----------------------------------------------------------------------
      IF(Niddf.gt.0.or.Nidsdf.gt.0)THEN
       CALL prprad(Adjttl,Nadjcr,Nustad,Nuspad,Priadj,Reglom)
*       IF(Priadj.gt.0.or.Nustad.gt.0.or.Nuspad.gt.0)
*     &    CALL mkPOneLine(Mt1,'@','&nbsp;')
       IF(.not.Lhiddn)CALL prtnfn(Fcntyp,Lam,0)
c-----------------------------------------------------------------------
       IF(.not.Lhiddn)THEN
        IF(.not.Lcmpaq)CALL mkPOneLine(Mt1,'@','&nbsp;')
        CALL genSkip(1013)
        IF(Nb.gt.0)THEN
         IF(.not.Lhiddn)
     &      CALL writTagOneLine(Mt1,'h2','@',
     &         'MODEL IDENTIFICATION (Using regression residuals)')
        ELSE
         IF(.not.Lhiddn)
     &      CALL writTagOneLine(Mt1,'h2','@','MODEL IDENTIFICATION')
        END IF
       END IF
c-----------------------------------------------------------------------
       IF(Ngrp.gt.0)
     &    CALL desreg('Regression Model',Ngrp,Grpttl,Grpptr,Ngrptl)
       IF(Ngrpfx.gt.0)
     &    CALL desreg('Regression Model (fixed)',Ngrpfx,Gfxttl,Gfxptr,
     &                Ngfxtl)
       IF(.not.Lfatal)CALL idmdl(Dflist,Niddf,Nidsdf,mxidlg,Lgraf)
       IF(Lfatal)RETURN
       IF(Niddf.gt.0)Niddf=0
       IF(Nidsdf.gt.0)Nidsdf=0
      END IF
c-----------------------------------------------------------------------
c     Everything beyond, outlier identification, model checking, and
c forecasting all require estimation so the program does not continue
c unless estimation/evaluation is done.
c-----------------------------------------------------------------------
      IF(Ldestm)THEN
c-----------------------------------------------------------------------
c     If automatic modelling option selected, select a model.
c-----------------------------------------------------------------------
       IF(lauto)THEN
        IF(Lautom)THEN
         IF(Lsumm.gt.0.and.gudrun)
     &      WRITE(Nform,1060)'automodeltype: ','automdl'
         IF(lidotl)THEN
          DO i=1,POTLR
           cvbak(i)=Critvl(i)
          END DO
         END IF
C-----------------------------------------------------------------------
         IF(Ltimer)THEN
          CALL cpu_time(ticks)
          IF(Issap.lt.2.and.Irev.lt.4)THEN
           WRITE(Nform,9000) 'bautomd:',ticks
          ELSE
           WRITE(Nform,9000) 'bautomd.diag:',ticks
          END IF
         END IF
C-----------------------------------------------------------------------
         CALL automd(trnsrs,frstry,nefobs,a,na,Lsumm,lidotl,
     &               Svltab(LSLADF),lsadj,Ltdlom,fctok,Lhiddn,Lnoprt)
         IF((.not.Lautom).or.Lfatal)RETURN
         Hvmdl=T
C-----------------------------------------------------------------------
         IF(Ltimer)THEN
          CALL cpu_time(ticks)
          IF(Issap.lt.2.and.Irev.lt.4)THEN
           WRITE(Nform,9000) 'eautomd:',ticks
          ELSE
           WRITE(Nform,9000) 'eautomd.diag:',ticks
          END IF
         END IF
C-----------------------------------------------------------------------
c     If rejectfcst = yes, test forecast error to see if forecast
c     extension should be rejected  (BCM July 2007)
C-----------------------------------------------------------------------
         IF(Lrejfc)THEN
          cfcst='no '
          CALL amdfct(trnsrs,mape,Nobspf,Nfcst,F,fctok,F)
          IF(Lfatal)RETURN
          IF(mape(4).gt.Fctlm2)THEN
           CALL nofcst(trnsrs,frstry,Lx11)
           IF(Lfatal)RETURN
*           IF(Laccss)CALL insacd(Mt1,'p',T)
           WRITE(Mt1,1160)mape(4),Fctlm2
*           IF(Laccss)CALL insacd(Mt1,'p',F)
           cfcst='yes'
          END IF
          IF(Lsumm.gt.0.and.gudrun)THEN
           WRITE(Nform,1060)'rejectfcst: ','yes'
           WRITE(Nform,1200)'fcstlim: ',fctlm2
           WRITE(Nform,1200)'mape3yr: ',mape(4)
           WRITE(Nform,1060)'fcstrejected: ',cfcst
          END IF
         END IF
C-----------------------------------------------------------------------
         IF(Lsumm.gt.0.and.gudrun)THEN
c         add tcrate in .udg file
          WRITE(Nform,1020)'tcrate: ',Tcalfa
          IF(lidotl)THEN
           IF(Ltstao)THEN
            cdef=' '
            IF(dpeq(Critvl(AO),cv))cdef='*'
            IF(.not.dpeq(Critvl(AO),cvbak(AO)))cdef='-'
            WRITE(Nform,1010)'aocrit: ',Critvl(AO),cdef
           END IF
           IF(Ltstls)THEN
            cdef=' '
            IF(dpeq(Critvl(LS),cv))cdef='*'
            IF(.not.dpeq(Critvl(LS),cvbak(LS)))cdef='-'
            WRITE(Nform,1010)'lscrit: ',Critvl(LS),cdef
           END IF
           IF(Ltsttc)THEN
            cdef=' '
            IF(dpeq(Critvl(TC),cv))cdef='*'
            IF(.not.dpeq(Critvl(TC),cvbak(TC)))cdef='-'
            WRITE(Nform,1010)'tccrit: ',Critvl(TC),cdef
           END IF
*           IF(Ltstso)THEN
*            cdef=' '
*            IF(dpeq(Critvl(SO),cv))cdef='*'
*            IF(.not.dpeq(Critvl(SO),cvbak(SO)))cdef='-'
*            WRITE(Nform,1010)'socrit: ',Critvl(SO),cdef
*           END IF
           WRITE(Nform,1020)'reducecv: ',Predcv
          END IF
          WRITE(Nform,1060)'automdl: ',Bstdsn(1:Nbstds)
          WRITE(Nform,1050)Bstdsn(1:Nbstds)
          WRITE(Nform,1061)'maxiter: ',Mxiter
          WRITE(Nform,1021)'tol: ',Tol
         END IF 
         IF(Svltab(LSLAMD))
     &      WRITE(Ng,1030)'Automatic model chosen',Bstdsn(1:Nbstds)
        ELSE
         hvstar=0
         IF(Lsumm.gt.0.and.gudrun)
     &      WRITE(Nform,1060)'automodeltype: ','pickmdl'
         CALL automx(trnsrs,frstry,nefobs,a,na,Hvmdl,hvstar,lsadj,
     &               lidotl,ltdlom,fctok,Lhiddn,Lsumm)
         IF(Lfatal)RETURN
         IF(Lsumm.gt.0.and.gudrun)THEN
c         add tcrate in .udg file
          WRITE(Nform,1020)'tcrate: ',Tcalfa
          IF(lidotl)THEN
           IF(Ltstao)THEN
            cdef=' '
            IF(dpeq(Critvl(AO),cv))cdef='*'
            WRITE(Nform,1010)'aocrit: ',Critvl(AO),cdef
           END IF
           IF(Ltstls)THEN
            cdef=' '
            IF(dpeq(Critvl(LS),cv))cdef='*'
            WRITE(Nform,1010)'lscrit: ',Critvl(LS),cdef
           END IF
           IF(Ltsttc)THEN
            cdef=' '
            IF(dpeq(Critvl(TC),cv))cdef='*'
            WRITE(Nform,1010)'tccrit: ',Critvl(TC),cdef
           END IF
*           IF(Ltstso)THEN
*            cdef=' '
*            IF(dpeq(Critvl(SO),cv))cdef='*'
*            WRITE(Nform,1010)'socrit: ',Critvl(SO),cdef
*           END IF
          END IF
          IF(hvstar.eq.2)THEN
           WRITE(Nform,1060)'automdl(default): ',Bstdsn(1:Nbstds)
          ELSE
           WRITE(Nform,1060)'automdl: ',Bstdsn(1:Nbstds)
          END IF
          WRITE(Nform,1050)Bstdsn(1:Nbstds)
          WRITE(Nform,1061)'maxiter: ',Mxiter
          WRITE(Nform,1021)'tol: ',Tol
         END IF
         IF(Svltab(LSLAMX))THEN
          IF(hvstar.eq.2)THEN
           WRITE(Ng,1030)'Default model used',
     &                   Bstdsn(1:Nbstds)//' (no model selected)'
          ELSE
           WRITE(Ng,1030)'Automatic model chosen',Bstdsn(1:Nbstds)
          END IF
         END IF
        END IF
c-----------------------------------------------------------------------
c     Save aictest information in log file, if AIC tests performed.
c-----------------------------------------------------------------------
        IF(Svltab(LSLTST).or.(Lsumm.gt.0.and.gudrun))THEN
         IF((Itdtst.gt.0.or.Lomtst.gt.0.or.Leastr.or.Luser).and.
     &      (Lsumm.gt.0.and.gudrun))THEN
          IF(.not.dpeq(Pvaic,DNOTST))
     &       WRITE(Nform,1021)'aictest.pv: ',ONE-Pvaic
         END IF
         CALL svaict(Itdtst.gt.0,Lomtst.gt.0,Leastr,Luser,
     &               Svltab(LSLTST),Hvmdl,Lsumm,'selected ',8)
         IF(Lfatal)RETURN
        END IF
        IF(Itdtst.gt.0)Itdtst=0
        IF(Leastr)Leastr=F
        IF(Luser)Luser=F
c-----------------------------------------------------------------------
c     If a model is not selected, turn off prior adjustment by regARIMA
c     factors.
c-----------------------------------------------------------------------
        IF(Hvmdl)THEN
         CALL ssprep(T,F,F)
        ELSE
         IF(Adjtd.eq.1)Adjtd=0
         IF(Adjhol.eq.1)Adjhol=0
         IF(Adjao.eq.1)Adjao=0
         IF(Adjls.eq.1)Adjls=0
         IF(Adjtc.eq.1)Adjtc=0
         IF(Adjso.eq.1)Adjso=0
         IF(Adjcyc.eq.1)Adjcyc=0
         IF(Adjusr.eq.1)Adjusr=0
         IF(Adjsea.eq.1)Adjsea=0
         IF((.NOT.(Axrghl.or.Axruhl.or.Khol.ge.1)).and.Finhol)Finhol=F
         IF(Finao)Finao=F
         IF(Finls)Finls=F
         IF(Fintc)Fintc=F
         IF(Finusr)Finusr=F
         IF(Nfcst.gt.0)THEN
          Posffc=Posfob
          IF(Nfdrp.gt.0)Nfdrp=0
         END IF
         IF(nbeg.gt.0.or.nend.gt.0)
     &      CALL setspn(Sp,nend,nbeg,Begspn,Endspn,Begmdl,Endmdl,Nspobs,
     &                  Frstsy,Nobspf,Begsrs,Nobs,Nfcst,Fctdrp,Nomnfy,
     &                  Begadj,Adj1st)
         IF(Itdtst.gt.0)Itdtst=0
         IF(Leastr)Leastr=F
         IF(Luser)Luser=F
         IF(Irev.gt.0)THEN
          IF(Lrvfct)Lrvfct=F
          IF(Lrvaic)Lrvaic=F
          IF(Lrvarma)Lrvarma=F
          IF(Lrvtdrg)Lrvtdrg=F
          IF(lsadj)THEN
           IF(.not.(Lrvsa.or.Lrvsf.or.Lrvch.or.Lrvtrn.or.Lrvtch.or.
     &              Lrvarma.or.Lrvtdrg))Irev=0
          ELSE
           Irev=0
          END IF
         END IF
         IF(Missng.and.lsadj)THEN
          IF(Lx11)Lx11=F
          IF(Lseats)Lseats=F
          CALL eWritln('Cannot perform seasonal adjustment if the '//
     &                 'automatic model',STDERR,Mt2,T,F)
          CALL writln('       selection procedure cannot select an '//
     &                'ARIMA model and missing',STDERR,Mt2,F,F)
          CALL writln('       value regressors are part of the model.',
     &                STDERR,Mt2,F,T)
         END IF
         IF(.not.Convrg)CALL abend()
         RETURN
        END IF
       ELSE
        IF(Lsumm.gt.0.and.gudrun)
     &     WRITE(Nform,1060)'automodeltype: ','none'
c-----------------------------------------------------------------------
c     Print the BoxCox transformation parameter
c-----------------------------------------------------------------------
        IF(Prttab(LESTMD))THEN
         IF(.not.Lcmpaq)CALL mkPOneLine(Mt1,'@','&nbsp;')
         CALL genSkip(1014)
         CALL writTagOneLine(Mt1,'h2','@','MODEL DEFINITION')
         CALL prprad(Adjttl,Nadjcr,Nustad,Nuspad,Priadj,Reglom)
         IF(Priadj.gt.0)CALL mkPOneLine(Mt1,'@','&nbsp;')
         CALL prtnfn(Fcntyp,Lam,0)
c-----------------------------------------------------------------------
c     Short description of the regression and ARIMA parts of the model
c-----------------------------------------------------------------------
         IF(Ngrp.gt.0)
     &      CALL desreg('Regression Model',Ngrp,Grpttl,Grpptr,Ngrptl)
         IF(Ngrpfx.gt.0)
     &      CALL desreg('Regression Model (fixed)',Ngrpfx,Gfxttl,Gfxptr,
     &                  Ngfxtl)
         IF(Lfatal)RETURN
         CALL dsarma(Lcmpaq)
         CALL prtmsp(Begmdl,Endmdl,Sp,F)
         IF(Lfatal)RETURN
        END IF
c     ------------------------------------------------------------------
        IF(Lsumm.gt.0.and.gudrun)THEN
         IF(Nmdl.gt.0)THEN
          WRITE(Nform,1050)Mdldsn(1:Nmddcr)
         ELSE
          WRITE(Nform,1050)'(0 0 0)'
         END IF
         WRITE(Nform,1061)'maxiter: ',Mxiter
         WRITE(Nform,1021)'tol: ',Tol
        END IF
c     ------------------------------------------------------------------
        IF(.not.Lhiddn.and.istrue(Prttab,LESTOP,LESTRS))THEN
         IF(.not.Lcmpaq)CALL mkPOneLine(Mt1,'@','&nbsp;')
         CALL genSkip(1015)
         CALL writTagOneLine(Mt1,'h2','@',
     &                       'MODEL ESTIMATION/EVALUATION')
        END IF
        IF(Prttab(LESTOP))CALL prtopt(Lestim,Mxiter,Mxnlit)
c-----------------------------------------------------------------------
c     Estimate the regression and ARMA parameters and print the results.
c-----------------------------------------------------------------------
        IF(Leastr.or.(Luser.and.Ncusrx.gt.0).or.Itdtst.gt.0.or.
     &     Lomtst.gt.0.or.(Ch2tst.and.Nguhl.gt.0))THEN
         litr=Prttab(LESTIT).or.Savtab(LESTIT)
         lfm=Prttab(LRGATS).and.Prttab(LESTFM)
         IF((Itdtst.gt.0.or.Lomtst.gt.0.or.Leastr.or.Luser).and.
     &      (Lsumm.gt.0.and.gudrun))THEN
          IF(.not.dpeq(Pvaic,DNOTST))
     &       WRITE(Nform,1021)'aictest.pv: ',ONE-Pvaic
         END IF
         IF(Itdtst.gt.0)THEN
          CALL tdaic(trnsrs,a,nefobs,na,frstry,lester,iaic,ltdlom,litr,
     &               Prttab(LRGATS),lfm,Svltab(LSLTST),Lsumm,Lhiddn)
          IF(Lfatal)RETURN
          IF(lester)THEN
           CALL eWritln('An model estimation error has occurred '//
     &                  'during the AIC testing of',STDERR,Mt2,T,F)
           CALL writln('       trading day regressor(s).  The error '//
     &                 'message appears below.',STDERR,Mt2,F,T)
           CALL prterr(nefobs,lauto)
           IF(Lfatal)RETURN
          END IF
c-----------------------------------------------------------------------
c     If different trading day chosen, resave regARIMA model variables
c     for sliding spans analysis
c-----------------------------------------------------------------------
          IF(iaic.ge.1)CALL ssprep(T,F,F)
         END IF
c-----------------------------------------------------------------------
         IF(.not.lester.and.Lomtst.gt.0)THEN
          CALL lomaic(trnsrs,a,nefobs,na,frstry,lester,litr,
     &                Prttab(LRGATS),lfm,Svltab(LSLTST),Lsumm,Lhiddn)
          IF(Lfatal)RETURN
          IF(lester)THEN
           CALL eWritln('An model estimation error has occurred '//
     &                  'during the AIC testing of',STDERR,Mt2,T,F)
           CALL writln('       lom/loq/lpyear regressor(s).  The '//
     &                 'error message appears below.',STDERR,Mt2,F,T)
           CALL prterr(nefobs,lauto)
           IF(Lfatal)RETURN
          END IF
          CALL ssprep(T,F,F)
         END IF
c-----------------------------------------------------------------------
         IF(.not.lester.and.Leastr)THEN
          CALL easaic(trnsrs,a,nefobs,na,frstry,lester,litr,
     &                Prttab(LRGATS),lfm,Svltab(LSLTST),Lsumm,Lhiddn)
          IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Resave regARIMA model variables for sliding spans analysis
c-----------------------------------------------------------------------
          IF(lester)THEN
           CALL eWritln('An model estimation error has occurred '//
     &                  'during the AIC testing of',STDERR,Mt2,T,F)
           CALL writln('       an Easter regressor.  The error '//
     &                  'message appears below.',STDERR,Mt2,F,T)
           CALL prterr(nefobs,lauto)
           IF(Lfatal)RETURN
          END IF
          CALL ssprep(T,F,F)
         END IF
         IF(.not.lester.and.(Luser.and.Ncusrx.gt.0))THEN
          CALL usraic(trnsrs,a,nefobs,na,frstry,lester,litr,
     &                Prttab(LRGATS),lfm,Svltab(LSLTST),Lsumm,Lhiddn)
          IF(Lfatal)RETURN
          IF(lester)THEN
           CALL eWritln('An model estimation error has occurred '//
     &                  'during the AIC testing of',STDERR,Mt2,T,F)
           CALL writln('       user defined regressor(s).  The error '//
     &                 'message appears below.',STDERR,Mt2,F,T)
           CALL prterr(nefobs,lauto)
           IF(Lfatal)RETURN
          END IF
          CALL ssprep(T,F,F)
         END IF
c-----------------------------------------------------------------------
         IF(Svltab(LSLTST).or.(Lsumm.gt.0.and.gudrun))THEN
          CALL svaict(Itdtst.gt.0,Lomtst.gt.0,Leastr,Luser,
     &                Svltab(LSLTST),.not.lester,Lsumm,'estimated',9)
          IF(Lfatal)RETURN
         END IF
c-----------------------------------------------------------------------
c     perform chi-square testing for user-defined holiday regressors
c-----------------------------------------------------------------------
         IF(.not.lester.and.Ch2tst.and.Ncusrx.eq.0)THEN
          Ch2tst=F
          Nguhl=0
          IF(Prttab(LRGCTS))THEN
           CALL mkPOneLine(Mt1,'center','All user-defined '//
     &        'regressors have been deleted; no chi-square testing '//
     &        'will be performed.')
           IF(.not.Lcmpaq)CALL mkPOneLine(Mt1,'@','&nbsp;')
          END IF
          IF(Svltab(LSLCTS))THEN
           CALL mkPOneLine(Ng,'center','All user-defined '//
     &        'regressors have been deleted; no chi-square testing '//
     &        'will be performed.')
           IF(.not.Lcmpaq)CALL mkPOneLine(Ng,'@','&nbsp;')
          END IF
         END IF
         IF(.not.lester.and.(Ch2tst.and.Nguhl.gt.0))THEN
          IF(Prttab(LRGCTS))CALL genSkip(1500)
          CALL chkchi(trnsrs,a,nefobs,na,frstry,lester,litr,
     &                Prttab(LRGCTS),Lsumm.gt.0.and.gudrun,
     &                Svltab(LSLCTS))
          IF(lester)THEN
           CALL eWritln('An model estimation error has occurred '//
     &                  'during the AIC testing of',STDERR,Mt2,T,F)
           CALL writln('       an Easter regressor.  The error '//
     &                 'message appears below.',STDERR,Mt2,F,T)
           CALL prterr(nefobs,lauto)
           IF(Lfatal)RETURN
          END IF
          CALL ssprep(T,F,F)
         END IF
c-----------------------------------------------------------------------
c     Turn off AIC test options
c-----------------------------------------------------------------------
         IF(Itdtst.gt.0)Itdtst=0
         IF(Leastr)Leastr=F
         IF(Luser)Luser=F
         IF(Ch2tst)Ch2tst=F
        ELSE
c-----------------------------------------------------------------------
c     Estimate the regression and ARMA parameters and print the results.
c-----------------------------------------------------------------------
*         IF(Savtab(LREGDT))THEN
*          CALL savmtx(LREGDT,Begxy,Sp,Xy,Nrxy,Ncxy,Colttl,Colptr,Ncoltl)
*          IF(Lfatal)RETURN
*         END IF
         CALL rgarma(Lestim,Mxiter,Mxnlit,(Prttab(LESTIT).or.
     &               Savtab(LESTIT)),a,na,nefobs,ltmp)
c-----------------------------------------------------------------------
c     check to see if there are estimation errors in previous model
c     estimation
c-----------------------------------------------------------------------
         IF(.not.Lfatal)CALL prterr(nefobs,lauto)
         IF(Lfatal)RETURN
c------------------------------------------------------------------------
c     If model estimation saves the estimation iterations, turn this off
c     for the rest of the run
c------------------------------------------------------------------------
         IF(Savtab(LESTIT))Savtab(LESTIT)=F
        END IF
        Hvmdl=T
c-----------------------------------------------------------------------
c     Outlier identification
c-----------------------------------------------------------------------
        IF(.not.lester.and.lidotl)THEN
c-----------------------------------------------------------------------
         IF(Prttab(LOTLHD))THEN
          CALL prothd(Begtst,Endtst,Ltstao,Ltstls,Ltsttc,Ladd1,Critvl)
          IF(Lfatal)RETURN
         END IF
c-----------------------------------------------------------------------
         IF(Prttab(LOTLIT))THEN
          IF(Prttab(LESAFC).and.Var.gt.ZERO)THEN
           CALL amdfct(trnsrs,mape,Nobspf,Nfcst,F,fctok,F)
           IF(Lfatal)RETURN
           IF(fctok)CALL prafce(Mt1,mape,Outfct,T)
          END IF
          IF(.not.Lfatal)CALL prtmdl(Lestim,Prttab(LESTES),Lcalcm,F,F,F,
     &                               Prttab(LESTCM),F,Prttab(LESTES),
     &                               itmp,Prttab(LESTES),
     &                               F,Prttab(LESTIT))
c-----------------------------------------------------------------------
          IF(.not.Lfatal)CALL prlkhd(Y(Frstsy),Adj(Adj1st),Adjmod,
     &                               Fcntyp,Lam,F,F,F)
         END IF
C-----------------------------------------------------------------------
         IF(Ltimer)THEN
          CALL cpu_time(ticks)
          IF(Issap.lt.2.and.Irev.lt.4)THEN
           WRITE(Nform,9000) 'bidotlr:',ticks
          ELSE
           WRITE(Nform,9000) 'bidotlr.diag:',ticks
          END IF
         END IF
C-----------------------------------------------------------------------
         IF(.not.Lfatal)
     &    CALL idotlr(Ltstao,Ltstls,Ltsttc,Ladd1,Critvl,Cvrduc,
     &                Begtst,Endtst,nefobs,Lestim,Mxiter,Mxnlit,lautid,
     &                a,trnsrs,Nobspf,Nfcst,Outfct,fctok,F,0,F,
     &                Prttab(LOTLTS),Prttab(LOTLIT),Savtab(LOTLIT),
     &                Prttab(LOTLFT),Savtab(LOTLFT),Lgraf,
     &                Lsumm.gt.0.and.gudrun)
C-----------------------------------------------------------------------
         IF(Ltimer)THEN
          CALL cpu_time(ticks)
          IF(Issap.lt.2.and.Irev.lt.4)THEN
           WRITE(Nform,9000) 'eidotlr:',ticks
          ELSE
           WRITE(Nform,9000) 'eidotlr.diag:',ticks
          END IF
         END IF
C-----------------------------------------------------------------------
         IF((.not.Lfatal).and.(.not.Convrg))CALL prterr(Nefobs,lauto)
         IF(.not.Lfatal)CALL regvar(trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,
     &                              Bgusrx,Nrusrx,Priadj,Reglom,Nrxy,
     &                              Begxy,frstry,T,Elong)
         IF(Lfatal)RETURN
         CALL ssprep(T,F,F)
         IF(Lsumm.gt.0.and.gudrun)THEN
c         add tcrate in .udg file
          WRITE(Nform,1020)'tcrate: ',Tcalfa
          IF(Ltstao)THEN
           cdef=' '
           IF(dpeq(Critvl(AO),cv))cdef='*'
           WRITE(Nform,1010)'aocrit: ',Critvl(AO),cdef
          END IF
          IF(Ltstls)THEN
           cdef=' '
           IF(dpeq(Critvl(LS),cv))cdef='*'
           WRITE(Nform,1010)'lscrit: ',Critvl(LS),cdef
          END IF
          IF(Ltsttc)THEN
           cdef=' '
           IF(dpeq(Critvl(TC),cv))cdef='*'
           WRITE(Nform,1010)'tccrit: ',Critvl(TC),cdef
          END IF
         END IF
        END IF
       END IF
c-----------------------------------------------------------------------
c     Check AO or LS sequence regressors  (BCM: April 2012)
c-----------------------------------------------------------------------
       IF(Nseq.gt.0)THEN
        IF(dpeq(Tlimit,DNOTST))THEN
         DO i=1,Nb
          rtype=Rgvrtp(i)
          IF(rtype.eq.PRSQLS.or.rtype.eq.PRSQAO)THEN
           Rgvrtp(i)=Rgvrtp(i)-100
          END IF
         END DO
        ELSE
c-----------------------------------------------------------------------
c     Print model with all members of sequence
c-----------------------------------------------------------------------
         IF(Var.gt.ZERO)THEN
          IF(Prttab(LESAFC))
     &     CALL amdfct(trnsrs,mape,Nobspf,Nfcst,F,fctok,F)
          IF(Lfatal)RETURN
          IF(fctok.and.Prttab(LESAFC))CALL prafce(Mt1,mape,Outfct,T)
         END IF
         CALL prtmdl(Lestim,Prttab(LESTES),Lcalcm,F,F,F,F,F,
     &               Prttab(LESTES),itmp,Prttab(LESTES),F,F)
c-----------------------------------------------------------------------
c     Generate t-statistics
c-----------------------------------------------------------------------
         CALL genrtt(tval)
c-----------------------------------------------------------------------
c     Check to see if abs(t-stat) > Tlimit
c-----------------------------------------------------------------------
         iseq=0
         i=Nb
         DO WHILE (i.ge.1)
          rtype=Rgvrtp(i)
          IF(rtype.eq.PRSQLS.or.rtype.eq.PRSQAO)THEN
           IF(abs(tval(i)).lt.Tlimit)THEN
c-----------------------------------------------------------------------
            IF(Prttab(LESTES))THEN
             CALL getstr(Colttl,Colptr,Ncoltl,i,tmpttl,ntmpcr)
             IF(iseq.eq.0)WRITE(Mt1,1120)Tlimit,Cbr
             WRITE(Mt1,1130)tmpttl(1:ntmpcr),tval(i),Cbr
            END IF
c-----------------------------------------------------------------------
            CALL dlrgef(i,Nrxy,1)
            IF(Lfatal)RETURN
            iseq=iseq+1
           ELSE
            Rgvrtp(i)=Rgvrtp(i)-100
           END IF
          END IF
          i=i-1
         END DO
c-----------------------------------------------------------------------
         IF(iseq.gt.0)THEN
          IF(Prttab(LESTES))WRITE(Mt1,1140)
c     ------------------------------------------------------------------
c     If model has been changed, regenerate regression matrix
c     ------------------------------------------------------------------
          CALL regvar(trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &                Priadj,Reglom,Nrxy,Begxy,frstry,T,Elong)
          CALL rgarma(Lestim,Mxiter,Mxnlit,(Prttab(LESTIT).or.
     &                Savtab(LESTIT)),a,na,nefobs,ltmp)
c     ------------------------------------------------------------------
          IF(.not.Lfatal)CALL prterr(nefobs,lauto)
          IF(Lfatal)RETURN
         END IF
        END IF
        CALL ssprep(T,F,F)
       END IF
c-----------------------------------------------------------------------
c     Compute and print out average forecast error from original 
c     X-11-ARIMA
c-----------------------------------------------------------------------
       IF(Var.gt.ZERO)THEN
        IF(Prttab(LESAFC).or.Svltab(LSLAFC).or.ldiag)
     &     CALL amdfct(trnsrs,mape,Nobspf,Nfcst,F,fctok,F)
        IF(Lfatal)RETURN
        IF(fctok.and.Prttab(LESAFC))CALL prafce(Mt1,mape,Outfct,T)
c-----------------------------------------------------------------------
c     Print out entry for log
c-----------------------------------------------------------------------
        IF(fctok.and.Svltab(LSLAFC))THEN
         Inlgfl=Inlgfl+1
         WRITE(Ng,1040)'aape',Inlgfl
         IF(Outfct)THEN
          CALL mkTableTag(Ng,'w60',
     &                    'Average Absolute Percentage Error : '//
     &                    'out-of-sample forecasts')
          CALL mkCaption(Ng,
     &                   'Average Absolute Percentage Error : '//Cbr//
     &                   'out-of-sample forecasts')
         ELSE
          CALL mkTableTag(Ng,'w60',
     &                    'Average Absolute Percentage Error : '//
     &                    'within-of-sample forecasts')
          CALL mkCaption(Ng,
     &                   'Average Absolute Percentage Error : '//Cbr//
     &                   'within-of-sample forecasts')
         END IF
         CALL writTag(Ng,'<tr>')
         CALL writTagOneLine(Ng,'td','head','&nbsp;')
         CALL mkHeaderCellScope(Ng,0,0,'col',
     &                          'Average Absolute Percentage Error',
     &                          'AAPE')
         CALL writTag(Ng,'</tr>')
         WRITE(Ng,1220)(mape(i),i=1,4)
         CALL writTag(Ng,'</table></div>')
         CALL mkPOneLine(Ng,'@','&nbsp;')
        END IF
        IF(fctok.and.ldiag)THEN
         IF(Outfct)THEN
          WRITE(Nform,'(a)')'aape.mode: outofsample'
         ELSE
          WRITE(Nform,'(a)')'aape.mode: withinsample'
         END IF
         WRITE(Nform,1200)'aape.0: ',mape(4)
         WRITE(Nform,1200)'aape.1: ',mape(1)
         WRITE(Nform,1200)'aape.2: ',mape(2)
         WRITE(Nform,1200)'aape.3: ',mape(3)
        END IF
       ELSE
        fctok=F
       END IF
       IF(.not.fctok)THEN
        IF(Svltab(LSLAFC))
     &     WRITE(Ng,1030)'Average Absolute Percentage Error','none'
        IF(ldiag)WRITE(Nform,'(a)')'aape.mode: none'
       END IF
c-----------------------------------------------------------------------
c     Print out the final model
c-----------------------------------------------------------------------
       IF(Iregfx.ge.2)THEN
        CALL addfix(trnsrs,Nbcst,0,1)
        IF(.not.Lfatal)
     &     CALL regvar(trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &                 Priadj,Reglom,Nrxy,Begxy,frstry,T,Elong)
        IF(Lfatal)RETURN
       END IF
       CALL prtmdl(Lestim,Prttab(LESTES),Lcalcm,Savtab(LESTES),Lgraf,
     &             ldiag,Prttab(LESTCM),Savtab(LESTCM),Prttab(LESTES),
     &             Lsrun,Prttab(LESTES),Prttab(LOTLTL),Prttab(LESTIT))
c-----------------------------------------------------------------------
c     Print out entries for diagnostic and log files related to outlier
c     regressors
c-----------------------------------------------------------------------
       CALL savotl(Lsumm,Svltab(LSLAOT),gudrun,lidotl)
c-----------------------------------------------------------------------
       IF(Convrg.and.Lestim.and.gudrun.and.Nliter.gt.BADITR)THEN
        IF(.not.Lquiet)WRITE(STDERR,1101)BADITR
        IF(Prttab(LESTIT).or.Prttab(LESTES))WRITE(Mt1,1102)BADITR
        WRITE(Mt2,1102)
 1101   FORMAT(/,' WARNING: Convergence of the coefficient estimation ',
     &          'procedure required',/,
     &         '          more than ',i3,' iterations. This often ',
     &         'indicates some inadequacy',/
     &         '          in the model being estimated.',/)
 1102   FORMAT(/,' <p><strong>WARNING:</strong> Convergence of the ',
     &           'coefficient estimation procedure required',
     &         /,' more than ',i3,' iterations. This often ',
     &         'indicates some inadequacy',
     &         /,' in the model being estimated.</p>',/)
       END IF
c-----------------------------------------------------------------------
       IF(Lestim.and.Prttab(LESTES).and.gudrun)THEN
        begopr=Opr(Mdl(AR-1)-1)
        endopr=Opr(Mdl(MA)-1)-1
        IF(istrue(Arimaf,begopr,endopr))THEN
         CALL nWritln('Fixed values have been assigned to some '//
     &                'regression and ARIMA model',Mt1,Mt2,T,F)
         CALL writln('      coefficients.  If these values are '//
     &               'estimates calculated by '//PRGNAM//',',
     &               Mt1,Mt2,F,F)
         CALL writln('      then the model comparison statistics '//
     &               '(<abbr title="Akaike information criterion">'//
     &                'AIC</abbr>,',Mt1,Mt2,F,F)
         CALL writln('       <abbr title="corrected Akaike '//
     &                'information criterion">AICC</abbr> ',Mt1,Mt2,F,F)
         CALL writln('      Hannan Quinn, and <abbr title="Bayesian '//
     &                'information criterion">BIC</abbr>)',Mt1,Mt2,F,F)
         CALL writln('      and the P-values of the Q''s of the '//
     &               'sample autocorrelations of the',Mt1,Mt2,F,F)
         CALL writln('      residuals below are invalid and should '//
     &               'not be used.',Mt1,Mt2,F,T)
        ELSE IF(istrue(Regfx,1,Nb))THEN
         CALL nWritln('Fixed values have been assigned to some '//
     &                'regression coefficients.',Mt1,Mt2,T,F)
         CALL writln('      If these values are estimates calculated '//
     &      'by '//PRGNAM//', then the',Mt1,Mt2,F,F)
         CALL writln('      model comparison statistics (<abbr '//
     &               'title="Akaike information criterion">AIC'//
     &               '</abbr>,',Mt1,Mt2,F,F)
         CALL writln('      <abbr title="corrected Akaike '//
     &               'information criterion">AICC</abbr>, Hannan '//
     &               'Quinn and ',Mt1,Mt2,F,F)
         CALL writln('      <abbr title="Bayesian information '//
     &               'criterion">BIC</abbr>)',Mt1,Mt2,F,F)
         CALL writln('      below are invalid and should not be used.',
     &               Mt1,Mt2,F,T)
        END IF
       END IF
c-----------------------------------------------------------------------
       IF(.not.Lfatal.and.Prttab(LESTAM).and.Lcalcm)THEN
        CALL genSkip(LESTAM)
        CALL armacr
       END IF
       IF(.not.Lfatal.and.Savtab(LESTAM).and.Lcalcm)CALL svamcm
       IF(Lfatal)RETURN
       IF(Prttab(LESTST).or.Savtab(LESTST).or.Prttab(LESTFM).or.
     &    Irev.eq.4.or.ldiag)THEN
        IF(.not.Lnoprt.and.(Prttab(LESTST).or.Savtab(LESTST).or.
     &     Prttab(LESTFM)))CALL genSkip(LESTST)
        CALL prlkhd(Y(Frstsy),Adj(Adj1st),Adjmod,Fcntyp,Lam,
     &              Savtab(LESTST),Prttab(LESTST),Prttab(LESTFM))
       END IF
       IF(Lfatal)RETURN
       IF(ldiag.and.(.not.dpeq(Lnlkhd,DNOTST)))THEN
        WRITE(Nform,1061)'nefobs: ',nefobs
        WRITE(Nform,1200)'loglikelihood: ',Lnlkhd
        WRITE(Nform,1200)'aic: ',Aic
        WRITE(Nform,1200)'aicc: ',Aicc
        WRITE(Nform,1200)'bic: ',Bic
        WRITE(Nform,1200)'hq: ',Hnquin
        IF(Eick.gt.0)THEN
         WRITE(Nform,1200)'eic: ',Eic
         WRITE(Nform,1200)'k: ',Eick
        END IF
       END IF
       IF(istrue(Svltab,LSLAIC,LSLEIC).and.
     &          (.not.dpeq(Lnlkhd,DNOTST)))THEN
        Inlgfl=Inlgfl+1
        WRITE(Ng,1040)'lk',Inlgfl
        CALL mkTableTag(Ng,'w60','Summary of Likelihood Statistics')
        CALL mkCaption(Ng,'Summary of Likelihood Statistics')
        CALL writTag(Ng,'<tr>')
        CALL mkTableCell(Ng,'head','&nbsp;')
        CALL mkHeaderCellScope(Ng,0,0,'col','@',
     &                         'Value of '//Cbr//'Likelihood Statistic')
        CALL writTag(Ng,'</tr>')
       END IF
       IF(Svltab(LSLAIC).and.(.not.dpeq(Aic,DNOTST)))
     &    WRITE(Ng,1202)'<abbr title="Akaike information criterion">'//
     &                  'AIC</abbr>',Aic
       IF(Svltab(LSLACC).and.(.not.dpeq(Aicc,DNOTST)))
     &    WRITE(Ng,1202)'<abbr title="corrected Akaike information '//
     &                  'criterion">AICC</abbr>',Aicc
       IF(Svltab(LSLBIC).and.(.not.dpeq(Bic,DNOTST)))
     &    WRITE(Ng,1202)'<abbr title="Bayesian information '//
     &                  'criterion">BIC</abbr>',Bic
       IF(Svltab(LSLHQ).and.(.not.dpeq(Hnquin,DNOTST)))
     &    WRITE(Ng,1202)'Hannan-Quinn',Hnquin
c       IF(Svltab(LSLEIC).and.(.not.dpeq(Eic,DNOTST)))
c     &    WRITE(Ng,1203)'<abbr title="empiricle information '//
c     &                  'criterion">EIC</abbr> (k=',Eick,')',Eic
       IF(istrue(Svltab,LSLAIC,LSLEIC).and.
     &          (.not.dpeq(Lnlkhd,DNOTST)))THEN
        CALL writTag(Ng,'</table></div>')
        CALL mkPOneLine(Ng,'@','&nbsp;')
       END IF
c-----------------------------------------------------------------------
c     Print out the roots of the AR and MA polynomials
c-----------------------------------------------------------------------
       IF(Prttab(LESTRT).or.Savtab(LESTRT).or.Svltab(LSLRTS).or.
     &    ldiag)THEN
        IF(Prttab(LESTRT))CALL genSkip(LESTRT)
        CALL prtrts(Prttab(LESTRT),Savtab(LESTRT),Svltab(LSLRTS),ldiag)
        IF(Lfatal)RETURN
       END IF
      END IF
c-----------------------------------------------------------------------
c     Print out the regression matrix with the added outliers
c-----------------------------------------------------------------------
      IF(Prttab(LREGDT).and.Nb.gt.0)THEN
       CALL genSkip(LREGDT)
       CALL prtshd('Regression Matrix',Begxy,Sp,Nrxy)
       IF(.not.Lfatal)
     &    CALL prtmtx(Begxy,Sp,Xy,Nrxy,Ncxy,Colttl,Colptr,Ncoltl,
     &                'Regression Matrix',tbxdic(LREGDT))
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
      IF(Savtab(LREGDT))THEN
       CALL savmtx(LREGDT,Begxy,Sp,Xy,Nrxy,Ncxy,Colttl,Colptr,Ncoltl)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
      IF(Prttab(LRGTDW).and.Lcalcm)CALL prtdwr(LRGTDW)
c-----------------------------------------------------------------------
      IF(Ldestm)THEN
       IF(Savtab(LESTMD))THEN
        CALL savmdl(Begxy,Nrxy,Elong)
        IF(Lfatal)RETURN
       END IF
c-----------------------------------------------------------------------
c     Print out the residuals
c-----------------------------------------------------------------------
       IF(Prttab(LESTRS).or.Savtab(LESTRS))THEN
        CALL addate(Begspn,Sp,Nspobs-na,idate)
c-----------------------------------------------------------------------
        IF(Prttab(LESTRS))THEN
         outdec=Kdec
         IF(outdec.lt.3)outdec=3
         CALL genSkip(LESTRS)
         CALL prtshd('Model Residuals',idate,Sp,na)
         IF(Lfatal)RETURN
         CALL prttbl(idate,Sp,a,na,'Model ',outdec,'xxx')
        END IF
c-----------------------------------------------------------------------
        IF(Savtab(LESTRS))THEN
         CALL savtbl(LESTRS,idate,1,na,Sp,a,Serno,Nser,F)
         IF(Lfatal)RETURN
        END IF
       END IF
c-----------------------------------------------------------------------
c     Calculate the ACF, ACF of squared residual, and residual histogram
c-----------------------------------------------------------------------
       IF(Convrg)THEN
        IF(Mxcklg.gt.0)THEN
         IF(.not.Lhiddn.and.istrue(Prttab,LCKACF,LCKNRM))THEN
          IF(.not.Lcmpaq)CALL mkPOneLine(Mt1,'@','&nbsp;')
          CALL genSkip(1016)
          CALL writTagOneLine(Mt1,'h2','@','DIAGNOSTIC CHECKING')
         END IF
         CALL prtacf(LSPCHK,nefobs,a,na,Mxcklg,Lgraf,ldiag,NOTSET,
     &               NOTSET)
         IF(Lfatal)RETURN
c-----------------------------------------------------------------------
         IF(gudrun)THEN
          nlagbl=MIN(Mxcklg,Sp*2)
          IF(ldiag.or.Svltab(LSLLBQ).or.Svltab(LSLLBQ+1))
     &       CALL acfdgn(nefobs,a,na,Mxcklg,nlagbl,ldiag)
c-----------------------------------------------------------------------
          CALL pracf2(nefobs,a,na,Mxcklg,Lgraf,ldiag)
          IF(Lfatal)RETURN
         END IF
c-----------------------------------------------------------------------
         IF(Prttab(LCKHST).and.Var.gt.ZERO)THEN
          CALL genSkip(LCKHST)
          CALL makeSkipLink(Mt1,Idxtab,
     &                      'Histogram of the Standardized and Mean-'//
     &                      'Centered Residuals',T)
          CALL writTagOneLine(Mt1,'h3','@',
     &      'Histogram of the Standardized and Mean-Centered Residuals')
          CALL hist(a(na-nefobs+1),Begspn,Sp,nefobs,Nobs-nefobs,Muladd)
          IF(Lfatal)RETURN
         END IF
c-----------------------------------------------------------------------
         IF((Prttab(LCKNRM).or.Savtab(LCKNRM).or.Svltab(LSLNRM)).and.
     &       Var.gt.ZERO.and.gudrun)THEN
          IF(Prttab(LCKNRM))THEN
           CALL genSkip(LCKNRM)
           CALL writTagOneLine(Mt1,'h3','@',
     &              'Normality Statistics for regARIMA Model Residuals')
          END IF
          CALL nrmtst(a(na-nefobs+1),nefobs,Prttab(LCKNRM),
     &                Savtab(LCKNRM),Svltab(LSLNRM))
         END IF
c-----------------------------------------------------------------------
         IF((Prttab(LCKDW).or.Savtab(LCKDW).or.Svltab(LSLCDW)).and.
     &       Var.gt.ZERO.and.gudrun)THEN
          IF(Prttab(LCKDW))THEN
           CALL genSkip(LCKDW)
           CALL writTagOneLine(Mt1,'h3','@','Durbin-Watson '//
     &                        'Statistic for regARIMA Model Residuals')
          END IF
          rvar = ZERO
          DO i = na-nefobs+1,na
           rvar = rvar + a(i)*a(i)
          END DO
          dw = ZERO
          do i = na-nefobs+2,Na
           dw = dw + (a(i)-a(i-1))**2
          end do
          dw = dw / rvar
          IF(Prttab(LCKDW))write(Mt1,1300)'dw',dw
          IF(Svltab(LSLCDW))WRITE(Ng,1300)'Durbin-Watson statistic',dw
          IF(Savtab(LCKDW))write(Nform,9000)'durbinwatson: ',dw
         END IF
c-----------------------------------------------------------------------
         IF((Prttab(LCKFRT).or.Savtab(LCKFRT).or.Svltab(LSLCFR)).and.
     &       Var.gt.ZERO.and.gudrun)THEN
          IF(Prttab(LCKFRT))THEN
           CALL genSkip(LCKFRT)
           CALL writTagOneLine(Mt1,'h3','@','Friedman Non-Parametric '//
     &                        'Test for regARIMA Model Residuals')
          END IF
          Ken = kendalls(a(na-nefobs+1),nefobs,Sp)
          IF(Prttab(LCKFRT))
     &       write(Mt1,1310)'ken',Ken,Sp-1,chisq(Ken,Sp-1)
          IF(Savtab(LCKFRT))
     &       write(Nform,9001)'friedman: ',Ken,Sp-1,chisq(Ken,Sp-1)
          IF(Svltab(LSLCFR))
     &       write(Ng,1310)'Friedman test',Ken,Sp-1,chisq(Ken,Sp-1)
         END IF
        END IF
c-----------------------------------------------------------------------
        IF((Prttab(LSPCQS).or.Savtab(LSPCQS).or.Svltab(LSLQS)).and.
     &       Var.gt.ZERO.and.gudrun.and.Sp.gt.1)THEN
          QsRsd = calcqs(a,na-nefobs,na,Sp)
          IF(Prttab(LSPCQS))THEN
           CALL genSkip(LSPCQS+4000)
           CALL writTagOneLine(Mt1,'h3','@',
     &                      'QS Statistic for regARIMA Model Residuals')
           WRITE(Mt1,1185)'full series',QsRsd,chisq(QsRsd,2)
          END IF
          CALL addate(Begspn,Sp,Nspobs-na,idate)
          CALL dfdate(Bgspec,idate,Sp,ipos)
          IF(ipos.gt.0)THEN
           QsRsd2 = calcqs(a,ipos,na,Sp)
           CALL wrtdat(Bgspec,Sp,begstr,nchr)
           IF(Prttab(LSPCQS))
     &        WRITE(Mt1,1185)'starting '//begstr(1:nchr),QsRsd2,
     &                       chisq(QsRsd2,2)
          END IF
        END IF
c-----------------------------------------------------------------------
        IF((Prttab(LSPCRS).or.Savtab(LSPCRS).or.Prttab(LSPCTP).or.
     &       Lsumm.gt.0.or.Lgraf).and.Var.gt.ZERO.AND.(Sp.eq.12).and.
     &       gudrun)THEN
          CALL addate(Begspn,Sp,Nspobs-na,idate)
c          CALL addate(idate,Sp,na-nefobs+1,idate)
          CALL spcrsd(a,na,idate,Sp,Endspn,LSPCRS,F,Lsumm,Lgraf)
c          CALL spcrsd(a(na-nefobs+1),nefobs,idate,Sp,Endspn,LSPCRS,
c     &                Lsumm,Lgraf)
          IF(Lfatal)RETURN
        END IF
c-----------------------------------------------------------------------
c     If Length of Month Adjustment indicator has changed, adjust
c     value of Sto and related variables (BCM 3-30-2011)
c-----------------------------------------------------------------------
        IF(ilom.ne.Priadj)THEN
         CALL copy(Orig(Pos1ob),Nomnfy,-1,Sto(Pos1ob))
         lastpr=Nofpob
         IF(Pos1ob.gt.1)lastpr=lastpr+Pos1ob-1
         CALL divsub(Sto,Sto,Sprior,Pos1ob,lastpr)
        END IF
c-----------------------------------------------------------------------
c     Reset ending date of span for forecasting, if necessary.
c-----------------------------------------------------------------------
        IF(nend.gt.0)THEN
         CALL setspn(Sp,nend,0,Begspn,Endspn,Begmdl,Endmdl,Nspobs,
     &               Frstsy,Nobspf,Begsrs,Nobs,Nfcst,Fctdrp,Nomnfy,
     &               Begadj,Adj1st)
c-----------------------------------------------------------------------
c     Recopy the series into trnsrs, and redo the transformation
c-----------------------------------------------------------------------
         CALL copy(Sto(Pos1ob+nbeg),Nspobs,-1,trnsrs)
         CALL trnfcn(trnsrs,Nspobs,Fcntyp,Lam,trnsrs)
         IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Generate regression matrix
c-----------------------------------------------------------------------
         CALL regvar(trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &               Priadj,Reglom,Nrxy,Begxy,frstry,T,Elong)
         IF(Lfatal)RETURN
        END IF
c-----------------------------------------------------------------------
c     Forecasts
c-----------------------------------------------------------------------
        IF(Nfcst.gt.0)THEN
         CALL prtfct(Nobspf,Nrxy,Fcntyp,Lam,Lognrm,Fctdrp,Nfcst,Ciprob,
     &               fcstx,untfct,Kdec,Posfob,Lgraf,
     &               Lsumm.gt.0.and.gudrun,Lseats,Khol,Kswv)
         IF(Lfatal)RETURN
        END IF
c-----------------------------------------------------------------------
c     Backcasts
c-----------------------------------------------------------------------
        IF(Nbcst.gt.0)THEN
         CALL mkback(trnsrs,Priadj,bcstx,untbct,Pos1bk,Kdec,Lgraf)
         IF(Lfatal)RETURN
        END IF
c-----------------------------------------------------------------------
c     Reset beginning dates of span, if necessary.
c-----------------------------------------------------------------------
        IF(nbeg.gt.0)THEN
         CALL setspn(Sp,nend,nbeg,Begspn,Endspn,Begmdl,Endmdl,Nspobs,
     &               Frstsy,Nobspf,Begsrs,Nobs,Nfcst,Fctdrp,Nomnfy,
     &               Begadj,Adj1st)
c-----------------------------------------------------------------------
         IF(ilom.ne.Priadj)THEN
          CALL copy(Orig(Pos1ob),Nomnfy,-1,Sto(Pos1ob))
          lastpr=Nofpob
          IF(Pos1ob.gt.1)lastpr=lastpr+Pos1ob-1
          CALL divsub(Sto,Sto,Sprior,Pos1ob,lastpr)
          IF(Nustad.gt.0)THEN
           CALL copy(Usrtad(Frstat+Lsp-1),Nspobs,1,dvec(Pos1ob))
           CALL divsub(Stoap,Sto,dvec,Pos1ob,Posfob)
          END IF
          IF(Nuspad.gt.0)THEN
           CALL copy(Usrpad(Frstap+Lsp-1),Nspobs,1,dvec(Pos1ob))
           CALL divsub(Stopp,Sto,dvec,Pos1ob,Posfob)
           CALL divsub(Stoap,Stoap,dvec,Pos1ob,Posfob)
          END IF
         END IF
c-----------------------------------------------------------------------
c     Recopy the series into trnsrs, and redo the transformation
c-----------------------------------------------------------------------
         CALL copy(Sto(Pos1ob),Nspobs,-1,trnsrs)
         CALL trnfcn(trnsrs,Nspobs,Fcntyp,Lam,trnsrs)
         IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Generate regression matrix
c-----------------------------------------------------------------------
         CALL regvar(trnsrs,Nobspf,Fctdrp,Nfcst,Nbcst,Userx,Bgusrx,
     &               Nrusrx,Priadj,Reglom,Nrxy,Begxy,frstry,T,Elong)
         IF(Lfatal)RETURN
        END IF
c-----------------------------------------------------------------------
c     If estimation did not converge, exit with an error
c-----------------------------------------------------------------------
       ELSE
        CALL abend
        RETURN
       END IF
      END IF
c-----------------------------------------------------------------------
c     Extend Series with backcasts and forecasts
c-----------------------------------------------------------------------
      CALL setdp(ZERO,PLEN,orix)
      IF(Ldestm.and.((Nfcst.gt.0.and.Nfdrp.gt.0).or.Nbcst.gt.0))THEN
       CALL extend(trnsrs,Begxy,orix,Extok,Lam,fcstx,bcstx)
       IF(Lfatal)RETURN
      ELSE
       CALL copy(trnsrs,Nobspf,1,orix(Pos1ob))
      END IF
c-----------------------------------------------------------------------
c     initialize regression factors to zero
c-----------------------------------------------------------------------
      CALL setdp(ZERO,PLEN,ftd)
      CALL setdp(ZERO,PLEN,fhol)
      CALL setdp(ZERO,PLEN,fao)
      CALL setdp(ZERO,PLEN,fls)
      CALL setdp(ZERO,PLEN,ftc)
      CALL setdp(ZERO,PLEN,fso)
      CALL setdp(ZERO,PLEN,fusr)
      CALL setdp(ZERO,PLEN,fsea)
      CALL setdp(ZERO,PLEN,fmv)
      CALL setdp(ZERO,PLEN,fcyc)
c-----------------------------------------------------------------------
c     Print the header for the regression effects matrix, if necessary
c-----------------------------------------------------------------------
      nf2=NOTSET
      IF(Ldestm)THEN
       IF(Prttab(LESTRE))THEN
        CALL genSkip(LESTRE)
        CALL prtshd('Regression Effects',Begxy,Sp,Nrxy)
        IF(Lfatal)RETURN
       END IF
c-----------------------------------------------------------------------
       CALL chkadj(tdreg,Khol,Lseats,Lam)
       IF(Prttab(LESTRE).or.Savtab(LESTRE).or.Adjtd.eq.1.or.Adjhol.eq.1
     &    .or.Adjao.eq.1.or.Adjls.eq.1.or.Adjtc.eq.1.or.Adjso.eq.1.or.
     &    Adjsea.eq.1.or.Adjcyc.eq.1.or.Adjusr.eq.1.or.Finhol.or.Finao
     &    .or.Finls.or.Fintc.or.Finusr.or.Missng)THEN
c-----------------------------------------------------------------------
c     If number of forecasts is less that the length of the seasonal
c     period, redo the regression variables so that there are enough
c     regressors for one year of forecasts.
c-----------------------------------------------------------------------
*        IF(Nfcst.lt.Sp.and.(Adjtd.eq.1.or.Adjhol.eq.1.or.Adjao.eq.1.or.
*     &     Adjls.eq.1.or.Adjtc.eq.1.or.Adjtc.eq.1.or.Adjsea.eq.1.or.
*     &     Adjcyc.eq.1.or.Adjusr.eq.1.or.Finhol.or.Finao.or.Finls.or.
*     &     Fintc.or.Finusr.or.Missng).and.Lx11)THEN
*         nf2=Nfcst
*         Nfcst=Sp
*         Nobspf=min(Nspobs+max(Nfcst-Fctdrp,0),Nomnfy)
*         CALL regvar(trnsrs,Nobspf,Fctdrp,Nfcst,Nbcst,Userx,Bgusrx,
*     &               Nrusrx,Priadj,Reglom,Nrxy,Begxy,frstry,T,Elong)
*         IF(Lfatal)RETURN
*        END IF
c-----------------------------------------------------------------------
c     Print out the regression effects with the added outliers for the
c     series extended by forecasts and backcasts
c-----------------------------------------------------------------------
        outdec=Kdec
        IF((.not.dpeq(Lam,ONE)).and.outdec.lt.3)outdec=3
        CALL regvar(orix,Nrxy,Fctdrp,Nfcst,Nbcst,Userx,Bgusrx,
     &              Nrusrx,Priadj,Reglom,Nrxy,Begxy,frstry,T,Elong)
        IF(Lfatal)RETURN
        CALL prtref(Begxy,Nrxy,Fctdrp,Nfcst,Nbcst,outdec,ftd,fhol,fao,
     &              fls,ftc,fso,fusr,fsea,fmv,fcyc,Nusrrg,Lseats,Rmcnst,
     &              Lgraf)
c     &              Lgraf,gudrun)
        IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Check for improper adjustment mode in seasonal adjustment
c-----------------------------------------------------------------------
        IF(Lx11.and.(Adjtd.eq.1.or.Adjhol.eq.1.or.Adjao.eq.1.or.
     &     Adjls.eq.1.or.Adjtc.eq.1.or.Adjso.eq.1.or.Adjsea.eq.1.or.
     &     Adjcyc.eq.1.or.Adjusr.eq.1.or.Finao.or.(Finhol.and.Nhol.gt.0)
     &     .or.Finls.or.Fintc.or.Finusr))THEN
         IF(Muladd.eq.1.AND.(.not.dpeq(Lam,ONE)))THEN
          CALL eWritln('Additive seasonal adjustment will not be '//
     &                 'performed when',STDERR,Mt2,T,F)
          CALL writln('       preadjustment factors are derived from '//
     &                'a REGARIMA model ',STDERR,Mt2,F,F)
          CALL writln('       for transformed data.',STDERR,Mt2,F,T)
          CALL writln('       Check the values for the power or '//
     &                'function arguments of the ',STDERR,Mt2,T,F)
          CALL writln('       transform spec and mode of the x11 spec.',
     &                STDERR,Mt2,F,F)
          CALL abend()
c-----------------------------------------------------------------------
         ELSE IF((Muladd.eq.0.or.Muladd.eq.2).AND.
     &           (.not.dpeq(Lam,ZERO)))THEN
          CALL eWritln('Multiplicative or log additive seasonal '//
     &                 'adjustment cannot be',STDERR,Mt2,T,F)
          CALL writln('       performed when preadjustment factors '//
     &                'are derived from a regARIMA',STDERR,Mt2,F,F)
          CALL writln('       model for data which have not been log '//
     &                'transformed.',STDERR,Mt2,F,T)
          CALL writln('       Check the values for the power or '//
     &                'function arguments of the ',STDERR,Mt2,T,F)
          CALL writln('       transform spec and mode of the x11 spec.',
     &                STDERR,Mt2,F,T)
          CALL abend()
         END IF
         IF(Lfatal)RETURN
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
c     Generate adjustment factors for regression variables
c-----------------------------------------------------------------------
      CALL adjreg(orix,orixmv,Temp,ftd,fao,fls,ftc,fso,fsea,fcyc,fusr,
     &            fmv,fhol,Fcntyp,Lam,Nrxy,n)
c-----------------------------------------------------------------------
c     copy missing value adjusted series into variable for SEATS
c     (August 2008 BCM)
c-----------------------------------------------------------------------
      IF(Lseats)THEN
       Nobspf=Nspobs+Nfcst
       DO i = 1, Nobspf
        Orixs(i)=orixmv(Pos1ob+i-1)
       END DO
      END IF
c-----------------------------------------------------------------------
c     IF regression matrix was extended, restore variables to their 
c     original state
c-----------------------------------------------------------------------
*      IF(nf2.ne.NOTSET)THEN
*       Nfcst=nf2
*       Nobspf=min(Nspobs+max(Nfcst-Fctdrp,0),Nomnfy)
*       CALL regvar(trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
*     &             Priadj,Reglom,Nrxy,Begxy,frstry,T,Elong)
*       IF(Lfatal)RETURN
*      END IF
c-----------------------------------------------------------------------
c     Get regression trading day factors for type of month table
c-----------------------------------------------------------------------
      IF((Tdtbl.ge.2.and.Lx11.and.gudrun).and.Ldestm)THEN
       IF(tdreg.eq.0)THEN
        Tdtbl=Tdtbl-2
       ELSE IF(Irev.lt.4)THEN
        tdreg=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Trading Day')
        IF(tdreg.eq.0)tdreg=strinx(T,Grpttl,Grpptr,1,Ngrptl,
     &                             'Stock Trading Day')
        IF(tdreg.eq.0)tdreg=strinx(T,Grpttl,Grpptr,1,Ngrptl,
     &                             '1-Coefficient Trading Day')
        IF(tdreg.eq.0)tdreg=strinx(T,Grpttl,Grpptr,1,Ngrptl,
     &                             '1-Coefficient Stock Trading Day')
c        IF(tdreg.eq.0)tdreg=-strinx(F,Grpttl,Grpptr,1,Ngrptl,
c     &                              'User-defined')
        CALL getmtd(tdreg,Begxy,Nrxy,Fcntyp,Lam)
        IF(Lfatal)RETURN
       END IF
      END IF
c-----------------------------------------------------------------------
c     If missing value codes are found in the series, replace values of
c     the original series with the missing value adjusted series.
c-----------------------------------------------------------------------
      IF(Missng.and.Ldestm)THEN
       DO i=Pos1ob,Posfob
        IF(.not.dpeq(Series(i),Orixmv(i)))THEN
         Series(i)=Orixmv(i)
         Orig(i)=Orixmv(i)
         Stoap(i)=Orixmv(i)
         Stopp(i)=Orixmv(i)
         IF(Nuspad.gt.0)THEN
          i2=Frstap+Lsp-1+(i-Pos1ob)
          IF(Muladd.eq.1)THEN
           Stoap(i)=Stoap(i)-Usrpad(i2)
           Stopp(i)=Stopp(i)-Usrpad(i2)
          ELSE
           Stoap(i)=Stoap(i)/Usrpad(i2)
           Stopp(i)=Stopp(i)/Usrpad(i2)
          END IF
         END IF
         IF(Nustad.gt.0)THEN
          i2=Frstat+Lsp-1+(i-Pos1ob)
          IF(Muladd.eq.1)THEN
           Stoap(i)=Stoap(i)-Usrtad(i2)
          ELSE
           Stoap(i)=Stoap(i)/Usrtad(i2)
          END IF
         END IF
        END IF
       END DO
c-----------------------------------------------------------------------
c     Print and/or save missing value adjusted series
c-----------------------------------------------------------------------
       IF(Prttab(LSRSMV))
     &    CALL table(Series,Pos1ob,Posfob,1,1,2,dvec,LSRSMV)
       IF(.not.Lfatal.and.Savtab(LSRSMV))
     &    CALL punch(Series,Pos1ob,Posfob,LSRSMV,F,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(Series,Pos1ob,Posfob,LSRSMV,Lgraf,F)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Delete missing value regressors
c-----------------------------------------------------------------------
       i=Nb
       DO WHILE (i.ge.1)
        IF(Rgvrtp(i).eq.PRGTMV)THEN
         CALL dlrgef(i,Nrxy,1)
         IF(Lfatal)RETURN
        END IF
        i=i-1
       END DO
      END IF
c-----------------------------------------------------------------------
c     Reset model parameters for sliding spans, revisions history
c-----------------------------------------------------------------------
      CALL ssprep(T,F,F)
c-----------------------------------------------------------------------
      IF(.not.gudrun)CALL copy(Orig,PLEN,1,Orig2)
      IF(Nfcst.gt.0)THEN
       DO i=Posfob+1,Posffc
        Orig2(i)=untfct(i-Posfob)
       END DO
      END IF
      IF(Nbcst.gt.0)THEN
       DO i=Pos1bk,Pos1ob-1
        Orig2(i)=untbct(i-Pos1bk+1)
       END DO
      END IF
c-----------------------------------------------------------------------
      IF(ldiag)THEN
       IF(Nb.gt.0)THEN
        CALL svfnrg('finalreg',Ngrp,Grpttl,Grpptr,Ngrptl)
       ELSE
        WRITE(Nform,1190)'nfinalreg:  ',1
        WRITE(Nform,1060)'finalreg01:',' none'
       END IF
      END IF
c-----------------------------------------------------------------------
 1000 FORMAT(a,i2,a2,1x,a,',',i4,' to ',i2,a2,1x,a,',',i4)
 1001 FORMAT(a,i4,' to ',i4)
 1010 FORMAT(a,e22.15,1x,a)
 1020 FORMAT(a,f10.6)
 1021 FORMAT(a,e13.6)
 1030 FORMAT('<p class="center"><strong>',a,' :</strong> ',a,'</p>')
 1040 FORMAT('<div id="lg',a,i6.6,'">')
 1050 FORMAT('arimamdl: ',a)
 1060 FORMAT(a:,a)
 1061 FORMAT(a,i6)
 1120 FORMAT(/,2x,'<p>The following sequence outliers have been ',
     &         'deleted since the',/,
     &         2x,'absolute value of their t-values are less than ',
     &         f10.3,':',a)
 1130 FORMAT(2x,'&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;',a,'   (t-value = ',
     &          f10.3,')',a)
 1140 FORMAT(2x,'regARIMA model will be restimated without these ',
     &          'regressors.</p>',/)
 1160 FORMAT(2x,'<p>Number of forecasts/backcasts set to zero because ',
     &          'forecast error for the',/,
     &       2x,' model identified, ',F10.3,', is greater than ',f10.3,
     &          '.</p>')
 1185 FORMAT(/,' <p>QS Statistic for regARIMA Model Residuals (',a,
     &         '):  ',f16.2,'  (P-Value = ',f10.4,')</p>')
 1190 FORMAT(a,i2)
 1200 FORMAT(a,e22.15)
 1201 FORMAT(a,f6.2,a,f12.4)
 1202 FORMAT('<tr><th scope="row">',a,'</th><td class="center">',f12.4,
     &       '</td></tr>')
c 1203 FORMAT('<tr><th scope="row">',a,f6.2,a,'</th><td class="center">',
c     &       f12.4,'</td></tr>')
 1220 FORMAT('<tr><th scope="row">Last year</th><td class="center">',
     &       f16.2,'</td></tr>',/,
     &       '<tr><th scope="row"><abbr title="Last minus 1">',
     &       'Last-1</abbr> year</th><td class="center">',f16.2,
     &       '</td></tr>',/,
     &       '<tr><th scope="row"><abbr title="Last minus 2">',
     &       'Last-2</abbr> year</th><td class="center">',f16.2,
     &       '</td></tr>',/,
     &       '<tr><th scope="row">Last 3 years</th><td class="center">',
     &       f16.2,'</td></tr>')
 1300 FORMAT('<p> ',a,' = ',f12.6,'</p>')
 1310 FORMAT('<p> ',a,' = ',f12.6,' ( Asymptotically distributed as ',
     &           'Chi-Square(',i2,'), P-Value = ',f12.6,' ) </p>')
 9000 FORMAT(a,e15.8)
 9001 FORMAT(a,e15.8,1x,i3,1x,e15.8)
c-----------------------------------------------------------------------
      RETURN
      END
