      SUBROUTINE amdid(Irar,Irdf,Irma,Isar,Isdf,Isma,Trnsrs,Frstry,
     &                 Nefobs,A,Na,Lmu,Lsumm,Locok)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     Select ARIMA model via procedure in Gomez and Maravall (1998)
c     ------------------------------------------------------------------
      DOUBLE PRECISION THREE2,THREE3,PTOL
      INTEGER NMOD
      LOGICAL T,F
      PARAMETER(T=.true.,F=.FALSE.,THREE2=0.03D0,THREE3=0.003D0,NMOD=5,
     &          PTOL=1.0D-3)
c     ------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'mdltbl.i'
      INCLUDE 'svllog.prm'
      INCLUDE 'svllog.cmn'
      INCLUDE 'mdlsvl.i'
      INCLUDE 'htmlout.cmn'
c     ------------------------------------------------------------------
      CHARACTER tmpmdl*(132)
      DOUBLE PRECISION A,bstbic,tolbak,txy,Trnsrs,vc11,vc2,vc22,bic1,
     &                 bmax,dbic,xmu,nltbak
      LOGICAL inptok,lxar,lxma,Lmu,Locok
      INTEGER Lsumm,bstrar,bstrdf,bstrma,bstsar,bstsdf,bstsma,frstry,i,
     &        Isar,Isma,Irar,Irma,Isdf,Irdf,id,nelta,psar,Na,nefobs,
     &        ichk,ntmp,ir1,is1,irr1,iss1,ir2,is2,irr2,iss2,icon,ngood
      DIMENSION A(PLEN+2*PORDER),bstbic(5),bstrar(5),bstrdf(5),
     &          bstrma(5),bstsar(5),bstsdf(5),bstsma(5),txy(PLEN),
     &          Trnsrs(PLEN)
c     ------------------------------------------------------------------
      LOGICAL mdlmch,dpeq
      EXTERNAL mdlmch,dpeq
c     ------------------------------------------------------------------
c     Set up initial values
c     ------------------------------------------------------------------
      Irar=3
      Irma=0
      id=Diffam(1)+Diffam(2)
      CALL setdp(DNOTST,5,bstbic)
      CALL setint(0,5,bstrar)
      CALL setint(0,5,bstrdf)
      CALL setint(0,5,bstrma)
      CALL setint(0,5,bstsar)
      CALL setint(0,5,bstsdf)
      CALL setint(0,5,bstsma)
c     ------------------------------------------------------------------
c     Loosen tolerance for Exact likelihood model estimation
c     ------------------------------------------------------------------
      nltbak=DNOTST
      tolbak=Tol
      IF(Tol.lt.PTOL)THEN
       nltbak=Nltol
       Tol=PTOL
       Nltol=PTOL
       Nltol0=100D0*Tol
      END IF
c     ------------------------------------------------------------------
      lxar=Lextar
      lxma=Lextma
      Lextar=T
      Lextma=T
      Lar=Lextar.and.Mxarlg.gt.0
      Lma=Lextma.and.Mxmalg.gt.0
      IF(Lextar)THEN
       Nintvl=Mxdflg
       Nextvl=Mxarlg+Mxmalg
      ELSE
       Nintvl=Mxdflg+Mxarlg
       Nextvl=0
       IF(Lextma)Nextvl=Mxmalg
      END IF
c     ------------------------------------------------------------------
c       Difference data, if necessary
c     ------------------------------------------------------------------
      nelta=Nspobs
      CALL copy(Trnsrs,nelta,1,txy)
      IF(id.gt.0)
     &   CALL arflt(nelta,Arimap,Arimal,Opr,Mdl(DIFF-1),Mdl(DIFF)-1,txy,
     &              nelta)
      IF(Lmu)CALL smeadl(txy,1,nelta,nelta,xmu)
c-----------------------------------------------------------------------
      CALL regvar(Trnsrs,nspobs,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &            Priadj,Reglom,Nrxy,Begxy,frstry,T,F)
c-----------------------------------------------------------------------
c     Print header for model estimation done to determine ARMA model
c     order (BCM July 2007
c-----------------------------------------------------------------------
      IF(Prttab(LAUMDL))THEN
       CALL mkPOneLine(Mt1,'@','&nbsp;')
       CALL mkPOneLine(Mt1,'@','ARIMA Estimates and Likelihood '//
     &                 'Values for <abbr title="autoregressive '//
     &                 'moving average">ARMA</abbr> Order '//
     &                 'Identification')
      END IF
c-----------------------------------------------------------------------
c     Determine the order of the seasonal parameters by fitting ARMA
c     models with a regular AR(3)
c-----------------------------------------------------------------------
      IF(Sp.eq.1)THEN
       Isar=0
       Isma=0
       psar=0
      ELSE
       ngood=0
       DO Isar=0,Maxord(2)
        DO Isma=0,Maxord(2)
         IF(Lmixmd.or.(Isar.eq.0.or.Isma.eq.0))THEN
          CALL amdid2(3,Irdf,0,Isar,Isdf,Isma,txy,nelta,Lmu,inptok)
          IF(Lfatal)RETURN
          IF(inptok)THEN
           CALL bestmd(3,Irdf,0,Isar,Isdf,Isma,bstrar,bstrdf,bstrma,
     &                 bstsar,bstsdf,bstsma,bstbic)
           ngood=ngood+1
          END IF
         END IF
        END DO
       END DO
       IF(ngood.gt.0)THEN
        Isar=bstsar(1)
        Isma=bstsma(1)
       ELSE
        WRITE(STDERR,1000)'seasonal'
        CALL eWritln('Cannot make a choice of seasonal <abbr title="'//
     &               'autoregressive moving average">ARMA</abbr> ',
     &               Mt1,Mt2,T,F)
        CALL writln(' order due to model estimation errors within '//
     &              'the automatic model selection procedure.',
     &              Mt1,Mt2,F,T)
        CALL abend
        RETURN
       END IF
       IF(Maxord(1).lt.3)THEN
        CALL setdp(DNOTST,5,bstbic)
        CALL setint(0,5,bstrar)
        CALL setint(0,5,bstrdf)
        CALL setint(0,5,bstrma)
        CALL setint(0,5,bstsar)
        CALL setint(0,5,bstsdf)
        CALL setint(0,5,bstsma)
       END IF
      END IF
c-----------------------------------------------------------------------
c     Using seasonal orders from before, identify regular ARMA orders.
c-----------------------------------------------------------------------
      ngood=0
      DO Irar=0,Maxord(1)
       DO Irma=0,Maxord(1)
        IF((Lmixmd.or.(Irar.eq.0.or.Irma.eq.0)).and.
     &    (.not.mdlmch(Irar,Irdf,Irma,Isar,Isdf,Isma,bstrar,bstrdf,
     &                 bstrma,bstsar,bstsdf,bstsma,bstbic)))THEN
         CALL amdid2(Irar,Irdf,Irma,Isar,Isdf,Isma,txy,nelta,Lmu,inptok)
         IF(Lfatal)RETURN
         IF(inptok)THEN
          CALL bestmd(Irar,Irdf,Irma,Isar,Isdf,Isma,bstrar,bstrdf,
     &                bstrma,bstsar,bstsdf,bstsma,bstbic)
          ngood=ngood+1
         END IF
        END IF
       END DO
      END DO
      IF(ngood.gt.0)THEN
       Irar=bstrar(1)
       Irma=bstrma(1)
      ELSE
       WRITE(STDERR,1000)'nonseasonal'
       CALL eWritln('Cannot make a choice of nonseasonal <abbr '//
     &              'title="autoregressive moving average">ARMA</abbr>',
     &               Mt1,Mt2,T,F)
       CALL writln(' order due to model estimation errors within '//
     &             'the automatic model selection procedure.',
     &             Mt1,Mt2,F,T)
       CALL abend
       RETURN
      END IF
c-----------------------------------------------------------------------
c     If no MA or AR orders are identified, use the next model to get
c     regular AR & MA orders for the next stage.
c-----------------------------------------------------------------------
      IF((Isar+Isma+Irar+Irma).eq.0)THEN
       Irar=bstrar(2)
       Irma=bstrma(2)
      END IF
c-----------------------------------------------------------------------
c     Using regular orders from before, re-identify seasonal ARMA
c     orders.
c-----------------------------------------------------------------------
      psar=Maxord(2)
      IF(psar.lt.2.and.Isdf.eq.1)psar=0
      IF(Sp.gt.1)THEN
       ngood=0
       DO Isar=0,psar
        DO Isma=0,Maxord(2)
         IF((Lmixmd.or.(Isar.eq.0.or.Isma.eq.0)).and.
     &    (.not.mdlmch(Irar,Irdf,Irma,Isar,Isdf,Isma,bstrar,bstrdf,
     &             bstrma,bstsar,bstsdf,bstsma,bstbic)))THEN
          CALL amdid2(Irar,Irdf,Irma,Isar,Isdf,Isma,txy,nelta,Lmu,
     &                inptok)
          IF(Lfatal)RETURN
          IF(inptok)THEN
           CALL bestmd(Irar,Irdf,Irma,Isar,Isdf,Isma,bstrar,bstrdf,
     &                 bstrma,bstsar,bstsdf,bstsma,bstbic)
           ngood=ngood+1
          END IF
         END IF
        END DO
       END DO
       IF(ngood.gt.0)THEN
        Isar=bstsar(1)
        Isma=bstsma(1)
*       ELSE
*        WRITE(STDERR,1000)'seasonal'
*        WRITE(Mt1,1000)'seasonal'
*        WRITE(Mt2,1000)'seasonal'
*        CALL abend
*        RETURN
       END IF
      END IF
c-----------------------------------------------------------------------
c     Check for closeness of BIC, model balance.
c-----------------------------------------------------------------------
      bic1=bstbic(1)
      Irar=bstrar(1)
      Isar=bstsar(1)
      Irma=bstrma(1)
      Isma=bstsma(1)
      icon=1
      ir1=bstrar(1)+bstrma(1)
      is1=bstsar(1)+bstsma(1)
      irr1=IABS(bstrar(1)+Irdf-bstrma(1))
      iss1=IABS(bstsar(1)+Isdf-bstsma(1))
      bmax=DABS(bic1-bstbic(NMOD))
      IF (bmax.LT.THREE3) THEN
       bmax=.0625D0
      ELSE IF (bmax.LT.THREE2) THEN
       bmax=.25D0
      ELSE
       bmax=1.D0
      END IF
      vc11=.01D0*bmax
      vc2=.0025D0*bmax
      vc22=.0075D0*bmax
c-----------------------------------------------------------------------
c     Begin looping through models
c-----------------------------------------------------------------------
      DO i=2,NMOD
       ir2=bstrar(i)+bstrma(i)
       is2=bstsar(i)+bstsma(i)
       irr2=IABS(bstrar(i)+Irdf-bstrma(i))
       iss2=IABS(bstsar(i)+Isdf-bstsma(i))
       dbic=DABS(bstbic(i)-bstbic(icon))
       ichk=0
       IF ((irr2.LT.irr1.OR.iss2.LT.iss1).AND.
     &       ir1.EQ.ir2.AND.is1.EQ.is2.AND.dbic.LE.vc11.and.Lbalmd)THEN
        ichk=1
       ELSE IF (irr2.LT.irr1.AND.ir2.LE.ir1.AND.is2.EQ.is1.AND.
     &      bstrar(i).GT.0.AND.bstrma(i).GT.0.AND.dbic.LE.vc2
     &      .and.Lbalmd)THEN
        ichk=2
       ELSE IF (((irr2.EQ.0.AND.irr2.LT.irr1.AND.Irdf.GT.0).OR.
     &       (iss2.EQ.0.AND.iss2.LT.iss1.AND.Isdf.GT.0)).AND.
     &        ir1.EQ.ir2.AND.is1.EQ.is2.AND.dbic.LE.vc11.and.Lbalmd)
     & THEN  
        ichk=3
       ELSE IF (irr2.EQ.0.AND.iss2.EQ.0.AND.dbic.LT.vc2.and.Lbalmd)THEN
        ichk=4
       ELSE IF (ir2.GT.ir1.AND.irr2.EQ.0.AND.is2.EQ.is1.AND.dbic.LT.vc2
     &      .and.Lbalmd)THEN
        ichk=5
       ELSE IF (is2.GT.is1.AND.iss2.EQ.0.AND.ir2.EQ.ir1.AND.dbic.LT.vc2
     &      .and.Lbalmd)THEN
        ichk=6
       ELSE IF(is2.LT.is1.AND.is2.GT.0.AND.ir2.EQ.ir1.AND.iss2.EQ.0.AND.
     &      dbic.LE.vc2.and.Lbalmd)THEN
        ichk=7
       ELSE IF (i.EQ.2.AND.ir1.EQ.0.AND.ir2.EQ.1.AND.is2.EQ.is1.AND.
     &      dbic.LT.vc2)THEN
        ichk=8
       ELSE IF(ir2.LT.ir1.AND.ir2.GT.0.AND.is2.EQ.is1.AND.dbic.LT.vc2)
     &  THEN
        ichk=9
       ELSE IF(is2.LT.is1.AND.is2.GT.0.AND.ir2.EQ.ir1.AND.dbic.LT.vc2)
     &  THEN
        ichk=10
       ELSE IF(bstrar(i).LT.Irar.AND.bstrma(i).EQ.Irma.AND.ir2.GT.0
     &      .AND.is2.EQ.is1.AND.dbic.LT.vc22)THEN
        ichk=11
       END IF
       IF(ichk.gt.0)THEN
        WRITE(Mt1,2000)i,ichk
c-----------------------------------------------------------------------
c     redo limits for bic with this model
c-----------------------------------------------------------------------
        vc11=vc11-DABS(bstbic(1)-bstbic(i))
        vc2=vc2-DABS(bstbic(1)-bstbic(i))
        vc22=vc22-DABS(bstbic(1)-bstbic(i))
c-----------------------------------------------------------------------
        ir1=ir2
        is1=is2
        irr1=irr2
        iss1=iss2
        icon=i
        bic1=bstbic(i)
        Irar=bstrar(i)
        Isar=bstsar(i)
        Irma=bstrma(i)
        Isma=bstsma(i)
c-----------------------------------------------------------------------
       END IF
      END DO
c-----------------------------------------------------------------------
c     Check to see if a model with no ARMA parameters is specified
c-----------------------------------------------------------------------
      IF(((Isar+Isma+Irar+Irma).eq.0).and.icon.lt.NMOD)THEN
       Irar=bstrar(icon+1)
       Irma=bstrma(icon+1)
       Isar=bstsar(icon+1)
       Isma=bstsma(icon+1)
      END IF
c-----------------------------------------------------------------------
      Lextar=lxar
      Lextma=lxma
      IF(nltbak.ne.NOTSET)THEN
       Tol=tolbak
       Nltol=nltbak
       Nltol0=Tol*100D0
      END IF
c-----------------------------------------------------------------------
      CALL mdlint()
      CALL mdlset(Irar,Irdf,Irma,Isar,Isdf,Isma,Locok)
      IF(.not.Lfatal)
     &   CALL rgarma(Lestim,Mxiter,Mxnlit,F,A,Na,nefobs,Lautom)
      IF(.not.Lautom)CALL abend()
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
      IF(.not.Convrg)THEN
       CALL eWritln('Estimation failed to converge for '//
     &              'automatically identified model',Mt1,Mt2,T,F)
       WRITE(Mt1,1030)Tol
       WRITE(Mt2,1030)Tol
       CALL writln('Rerun program trying one of the following:',
     &             Mt1,Mt2,F,T)
c-----------------------------------------------------------------------
       CALL writTagClass(Mt1,'ol','indent')
       CALL writTagOneLine(Mt1,'li','@',
     &         'Allow more iterations (set a larger value of maxiter).')
       CALL writTagOneLine(Mt1,'li','@',
     &           'Try a different model or different modeling options.')
       CALL writTag(Mt1,'</ol>')
c-----------------------------------------------------------------------
       CALL writTagClass(Mt2,'ol','indent')
       CALL writTagOneLine(Mt2,'li','@',
     &         'Allow more iterations (set a larger value of maxiter).')
       CALL writTagOneLine(Mt2,'li','@',
     &           'Try a different model or different modeling options.')
       CALL writTag(Mt2,'</ol>')
c-----------------------------------------------------------------------
       CALL writln('See '//MDLSEC//' of the '//PRGNAM//' '//DOCNAM//
     &             ' for more discussion.',Mt1,Mt2,T,T)
       CALL abend()
       RETURN
      END IF
c-----------------------------------------------------------------------
      Bstdsn=Mdldsn
      Nbstds=Nmddcr
      IF(Prttab(LAUB5M).or.Svltab(LSLB5M).or.Lsumm.gt.0)THEN
       IF(Prttab(LAUB5M))THEN
        CALL mkPOneLine(Mt1,'@','Best Five ARIMA Models')
        CALL mkPClass(Mt1,'indent')
       END IF
       IF(Svltab(LSLB5M))THEN
        Inlgfl=Inlgfl+1
        WRITE(Ng,1040)Inlgfl
        CALL mkTableTag(Ng,'w60','Best Five ARIMA Models')
        CALL mkCaption(Ng,'Best Five ARIMA Models')
        CALL writTag(Ng,'<tr>')
        CALL mkTableCell(Ng,'head','&nbsp;')
        CALL mkHeaderCellScope(Ng,0,0,'col','@','ARIMA Model')
        CALL mkHeaderCellScope(Ng,0,0,'col','Bayesian information '//
     &                         'criterion 2','BIC2')
        CALL writTag(Ng,'</tr>')
       END IF
       DO i=1,NMOD
        CALL mkmdsn(bstrar(i),bstrdf(i),bstrma(i),bstsar(i),bstsdf(i),
     &              bstsma(i),tmpmdl,ntmp)
        IF(Lfatal)RETURN
        IF(Prttab(LAUB5M))WRITE(Mt1,1050)i,tmpmdl(1:ntmp),bstbic(i),Cbr
        IF(Svltab(LSLB5M))WRITE(Ng,1020)i,tmpmdl(1:ntmp),bstbic(i)
        IF(Lsumm.gt.0)THEN
         WRITE(Nform,1060)i,tmpmdl(1:ntmp)
         WRITE(Nform,1070)i,bstbic(i)
        END IF
       END DO
      END IF
      IF(Prttab(LAUB5M))CALL writTag(Mt2,'</p>')
      IF(Prttab(LAUMCH))
     &   CALL mkPOneLine(Mt1,'center',
     &                  'Preliminary model choice : '//Bstdsn(1:Nbstds))
      IF(Svltab(LSLB5M))THEN
       CALL writTag(Ng,'</table></div>')
       CALL mkPOneLine(Ng,'@','&nbsp;')
      END IF
c-----------------------------------------------------------------------
      RETURN
c-----------------------------------------------------------------------
 1000 FORMAT(/,' ERROR: Cannot make a choice of ',a,
     &         ' ARMA order due to',
     &       /,'        model estimation errors within the automatic ',
     &         'model',/,'        selection procedure.')
 1020 FORMAT('<tr><th scope="row">Model ',i2,'</th><td class="center">',
     &       a,'</td><td class="center">',f10.3,'</td></tr>')
 1030 FORMAT(' when convergence tolerence reset to ',e13.6,'.')
 1040 FORMAT('<div id="lgb5m',i6.6,'">')
 1050 FORMAT('     Model # ',i2,' : ',a,' (<abbr title="Bayesian ',
     &       'information criterion 2">BIC2</abbr> = ',f10.3,')',a)
 1060 FORMAT('automdl.best5.mdl',i2.2,': ',a)
 1070 FORMAT('automdl.best5.bic',i2.2,': ',f10.3)
 2000 FORMAT('<p> ichk (best model vrs. model ',i1,') = ',i3,'</p>')
c-----------------------------------------------------------------------
      END
