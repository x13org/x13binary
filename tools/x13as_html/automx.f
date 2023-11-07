C     Last change:  BCM   3 Mar 1999    8:33 am
      SUBROUTINE automx(Trnsrs,Frstry,Nefobs,A,Na,Hvmdl,Hvstar,Lsadj,
     &                  Lidotl,Ltdlom,Fctok,Lhiddn,Lsumm)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     This subroutine performs an automatic model selection from
c     model stored in the file x12a.mdl.  The procedure is the same as
c     in X-11-ARIMA/88
c     ------------------------------------------------------------------
      LOGICAL T,F
      INTEGER MULT,DIV
      PARAMETER(MULT=3,DIV=4,T=.true.,F=.false.)
c     ------------------------------------------------------------------
      INCLUDE 'lex.i'
      INCLUDE 'stdio.i'
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'prittl.cmn'
      INCLUDE 'priadj.cmn'
      INCLUDE 'priusr.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'mdltbl.i'
      INCLUDE 'adj.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'usrreg.cmn'
c     ------------------------------------------------------------------
      INTEGER PR
      PARAMETER(PR=PLEN/4)
      INCLUDE 'autoq.cmn'
c      INTEGER PXY
c      PARAMETER(PXY=PLEN*(PB+1))
c     ------------------------------------------------------------------
      CHARACTER tmpttl*(PCOLCR),cttl1*(PCOLCR*PB),gttl1*(PGRPCR*PGRP)
      DOUBLE PRECISION A,a2,b1,Trnsrs,mape,seacf,smpac,blchi,qchi,sma,
     &                 rma,loclim,tsrs
      INTEGER Frstry,i,Na,Nefobs,begopr,endopr,beglag,endlag,iopr,ilag,
     &        ntmpcr,nummdl,numbst,nerr,padj2,ngr1,ngrt1,ncxy1,nb1,nct1,
     &        clptr1,g1,gptr1,rgv1,tdauto,Hvstar,nf2,ncttl,ngttl,np,
     &        dgfchi,fhnote,Lsumm
      LOGICAL argok,bstptd,Hvmdl,inptok,estbst,ovrdff,ovrsdf,mdskip,
     &        tstmdl,Lidotl,id,pktd,Fctok,lester,gsovdf,begrgm,sviter,
     &        Lsadj,anymdl,havfil,Lhiddn,Ltdlom
      DIMENSION A(PLEN+2*PORDER),a2(PLEN),b1(PB),begrgm(PLEN),
     &          clptr1(0:PB),g1(0:PGRP),gptr1(0:PGRP),mape(4),rgv1(PB),
     &          seacf(PR),smpac(PR),Trnsrs(PLEN),tsrs(PLEN)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      INTEGER nblank
      EXTERNAL dpeq,nblank
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
c     Initialize input of automatic model selection procedure.
c     ------------------------------------------------------------------
      sviter=F
      gsovdf=F
      loclim=Fctlim
      nerr=0
      havfil=.not.(Autofl(1:1).eq.CNOTST)
      inptok=T
      IF(havfil)THEN
       CALL mdlinp(Autofl(1:nblank(Autofl)),inptok)
       IF(Lfatal)RETURN
       IF(.not.inptok)THEN
        CALL eWritln('Must have user supplied models stored in '//
     &               Autofl(1:nblank(Autofl))//'.',STDERR,Mt2,T,T)
        CALL abend
        RETURN
       END IF
      END IF
c     ------------------------------------------------------------------
c     Set up temporary variables for transformed series, adjustment
c     factors for automatic trading day selection.
c     ------------------------------------------------------------------
      pktd=Picktd
      padj2=Priadj
      CALL copy(Trnsrs,PLEN,1,tsrs)
      CALL copy(Adj,PLEN,1,a2)
c-----------------------------------------------------------------------
c     Print automatic modelling heading
c-----------------------------------------------------------------------
      IF(Prttab(LAXHDR))THEN
       CALL genSkip(LAXHDR)
       CALL writTagOneLine(Mt1,'h2','center',
     &            ' Autoregressive Integrated Moving Average (ARIMA) '//
     &            'extrapolation program')
       CALL writTagOneLine(Mt1,'h3','center',
     &            ' ARIMA extrapolation model (forecast)')
       IF(Pck1st)THEN
        WRITE(Mt1,1020)
       ELSE
        WRITE(Mt1,1030)
       END IF
      END IF
c     ------------------------------------------------------------------
c     If diagnostic output saved, save automatic modeling settings to
c     .udg file (BCM July 2008)
c     ------------------------------------------------------------------
      IF(Lsumm.gt.0)THEN
       IF(Pck1st)THEN
        WRITE(Nform,1140)'pickfirst','yes'
       ELSE
        WRITE(Nform,1140)'pickfirst','no'
       END IF
       IF(Id1st)THEN
        WRITE(Nform,1140)'idfirst','yes'
       ELSE
        WRITE(Nform,1140)'idfirst','no'
       END IF
       IF(havfil)THEN
        WRITE(Nform,1140)'mdlfile','yes'
        WRITE(Nform,1140)'mdlfilename',Autofl(1:nblank(Autofl))
       ELSE
        WRITE(Nform,1140)'mdlfile','no'
       END IF
       WRITE(Nform,1150)'fcstlimit',Fctlim
       IF(Nbcst.gt.0)WRITE(Nform,1150)'bcstlimit',Bcklim
       WRITE(Nform,1150)'qlim',Qlim
       WRITE(Nform,1150)'overdiff',Ovrdif
      END IF
c-----------------------------------------------------------------------
c     Print short description of the prior adjustment factors and
c     regression part of the model
c-----------------------------------------------------------------------
      IF(Prttab(LAXHDR))THEN
       CALL prprad(Adjttl,Nadjcr,Nustad,Nuspad,Priadj,Reglom)
       IF(Priadj.gt.1)WRITE(Mt1,'()')
       CALL prtnfn(Fcntyp,Lam,0)
       IF(.not.Lfatal.and.Lidotl.and.Prttab(LOTLHD))
c     &   CALL prothd(Begtst,Endtst,Ltstao,Ltstls,Ltsttc,Ltstso,Ladd1,
     &   CALL prothd(Begtst,Endtst,Ltstao,Ltstls,Ltsttc,Ladd1,
     &               Critvl)
       IF(.not.Lfatal)CALL prtmsp(Begmdl,Endmdl,Sp,F)
       IF(Lfatal)RETURN
      END IF
c     ------------------------------------------------------------------
c     Set up temporary storage for regressors.
c     ------------------------------------------------------------------
      ngr1=Ngrp
      ngrt1=Ngrptl
      ncxy1=Ncxy 
      nb1=Nb
      nct1=Ncoltl
      ncttl=PCOLCR*PB
      cttl1(1:ncttl)=Colttl(1:ncttl)
      ngttl=PGRPCR*PB
      gttl1(1:ngttl)=Grpttl(1:ngttl)
      CALL cpyint(Colptr(0),PB+1,1,clptr1(0))
      CALL cpyint(Grp(0),PGRP+1,1,g1(0))
      CALL cpyint(Grpptr(0),PGRP+1,1,gptr1(0))
      CALL cpyint(Rgvrtp,PB,1,rgv1)
      CALL copy(B,PB,1,b1)
c     ------------------------------------------------------------------
c     Loop through models
c     ------------------------------------------------------------------
      Hvmdl=F
      estbst=F
      nummdl=0
      tstmdl=T
      numbst=0
      bstptd=F
      anymdl=F
      DO WHILE (tstmdl)
c-----------------------------------------------------------------------
c     Test to see if all of the models have been tested (ie, test for
c     an end of file, or if five models are done).
c-----------------------------------------------------------------------
       IF((havfil.and.Nxtktp.eq.EOF).or.((.not.havfil).and.
     &    nummdl.eq.5))THEN
c-----------------------------------------------------------------------
c     If none of the models have been selected, close file and exit
c     routine
c-----------------------------------------------------------------------
        IF(havfil)THEN
         CALL fclose(Inputx)
c-----------------------------------------------------------------------
c     See if any models were read by the program - if not, print error
c     message and exit
c-----------------------------------------------------------------------
         IF(nummdl.eq.0)THEN
          CALL eWritln('No ARIMA models stored in '//
     &                 Autofl(1:nblank(Autofl))//'.',STDERR,Mt2,T,T)
          CALL writln('        Check contents of file and try again.',
     &                STDERR,Mt2,T,T)
          CALL abend
          RETURN
         END IF
        END IF
c-----------------------------------------------------------------------
c     If one of the models was not read in correctly, print message
c     warning user to correct problem
c-----------------------------------------------------------------------
        IF(.not.inptok)THEN
         IF(havfil)THEN
          CALL wWritln('Unable to generate at least one of the ARIMA '//
     &                 'models stored in ',fhnote,Mt2,T,F)
          CALL writln('          '//Autofl(1:nblank(Autofl))//'.',
     &                fhnote,Mt2,F,T)
          CALL writln('          Check contents of this file and try '//
     &                'again.',fhnote,Mt2,T,T)
         END IF
        END IF
c-----------------------------------------------------------------------
c     If no models selected, write out message
c-----------------------------------------------------------------------
        IF(.not.Hvmdl.and.Hvstar.eq.0)THEN
         IF(nerr.gt.0)THEN
          CALL wWritln('Estimation errors occured during the '//
     &                 'automatic model selection procedure.',
     &                 fhnote,Mt2,T,T)
          CALL writln('          For more details, check error file ',
     &                fhnote,Mt2,T,F)
          CALL writln('          '//Cursrs(1:nblank(Cursrs))//
     &                '_err.html.',fhnote,Mt2,F,T)
         END IF
         IF(Prttab(LAXMCH))
     &      CALL mkPOneLine(Mt1,'center',
     &                      'None of the models were chosen.')
         Bstdsn(1:4)='none'
         Nbstds=4
         RETURN
        END IF
c-----------------------------------------------------------------------
c     If all of the models had estimation errors, stop execution now.
c-----------------------------------------------------------------------
        IF(.not.anymdl)THEN
         CALL abend()
         RETURN
        END IF
c-----------------------------------------------------------------------
c     Check to see if the best model was the last model estimated.
c     If this is not the case, reset and reestimate model parameters.
c-----------------------------------------------------------------------
        IF(nummdl.eq.numbst)GO TO 20
        CALL bstget(Nbstds,Bstdsn)
        estbst=T
c-----------------------------------------------------------------------
c     If the current setting of Picktd is not equal to the setting for
c     the best model, change the values of the transformed series and
c     prior adjusted series to correspond to the best model's setting of
c     Picktd
c-----------------------------------------------------------------------
        IF(.not.(bstptd.eqv.Picktd))THEN
         Picktd=bstptd
         IF(bstptd.eqv.pktd)THEN
          CALL copy(tsrs,PLEN,1,Trnsrs)
          CALL copy(a2,PLEN,1,Adj)
          padj2=Priadj
         ELSE
          IF(Picktd)THEN
           IF(Lrgmtd.and.MOD(Tdzero,2).ne.0)THEN
            CALL gtrgpt(Begadj,Tddate,Tdzero,begrgm,Nadj)
           ELSE
            CALL setlg(T,PLEN,begrgm)
           END IF
           IF(Lfatal)RETURN
           CALL td7var(Begadj,Sp,Nadj,1,1,F,F,T,Adj,begrgm)
           IF(Nustad.gt.0)CALL eltfcn(MULT,Adj,Usrtad(Frstat),Nspobs,
     &                                PLEN,Adj)
           IF(Nuspad.gt.0)CALL eltfcn(MULT,Adj,Usrpad(Frstap),Nspobs,
     &                                PLEN,Adj)
           CALL eltfcn(DIV,Y(Frstsy),Adj(Adj1st),Nspobs,PLEN,Trnsrs)
           Priadj=4
          ELSE
           IF(Nustad.gt.0.or.Nuspad.gt.0)THEN
            IF(Nustad.gt.0)CALL eltfcn(DIV,Y(Frstsy),Usrtad(Frstat),
     &                                 Nspobs,PLEN,Trnsrs)
            IF(Nuspad.gt.0)CALL eltfcn(DIV,Y(Frstsy),Usrpad(Frstap),
     &                                 Nspobs,PLEN,Trnsrs)
           ELSE
            CALL copy(Y(Frstsy),Nspobs,-1,Trnsrs)
           END IF
           Priadj=1
          END IF
          IF(Lmvaft.or.Ln0aft)THEN
           CALL trnfcn(trnsrs,Nspobs,Fcntyp,Lam,trnsrs)
          ELSE
           CALL trnfcn(trnsrs,Nobspf,Fcntyp,Lam,trnsrs)
          END IF
         END IF
        END IF
c-----------------------------------------------------------------------
c     If an end of file has not been reached, initialize the parameters
c     and lag vectors.
c-----------------------------------------------------------------------
       ELSE
        mdskip=F
        CALL mdlint()
c-----------------------------------------------------------------------
c     IF identifying outliers and trading day for each model, restore
c     original regressors.
c-----------------------------------------------------------------------
        IF(.not.Id1st.and.(Lidotl.or.Itdtst.gt.0).and.nummdl.gt.0)THEN
         Ngrp=ngr1
         Ngrptl=ngrt1
         Ncxy=ncxy1
         Nb=nb1
         Ncoltl=nct1
         Colttl(1:ncttl)=cttl1(1:ncttl)
         Grpttl(1:ngttl)=gttl1(1:ngttl)
         CALL cpyint(clptr1(0),PB+1,1,Colptr(0))
         CALL cpyint(g1(0),PGRP+1,1,Grp(0))
         CALL cpyint(gptr1(0),PGRP+1,1,Grpptr(0))
         CALL cpyint(rgv1,PB,1,Rgvrtp)
         CALL copy(b1,PB,1,B)
c-----------------------------------------------------------------------
c     IF td has changed, restore transformed series, original prior
c     adjustment.
c-----------------------------------------------------------------------
         IF(.not.(Picktd.eqv.pktd))THEN
          CALL copy(tsrs,PLEN,1,Trnsrs)
          CALL copy(a2,PLEN,1,Adj)
          Picktd=pktd
          Priadj=padj2
         END IF
        END IF
c-----------------------------------------------------------------------
c     Read in ARIMA lags for automatic model selection
c-----------------------------------------------------------------------
        IF(havfil)THEN
         DO WHILE (Nxtktp.ne.LPAREN)
          CALL lex()
          IF(Nxtktp.eq.EOF)GO TO 10
         END DO
        END IF
c-----------------------------------------------------------------------
        nummdl=nummdl+1
        argok=T
        id=(Id1st.and.nummdl.eq.1).or.(.not.Id1st)
        IF(havfil)THEN
         CALL getmdl(argok,inptok,T)
         IF(Lfatal)THEN
          CALL eWritln('Unable to read automatic model for the '//
     &                 'reason(s) given above.',STDERR,Mt2,T,T)
          CALL writln('        Check the models stored in '//
     &                Autofl(1:nblank(Autofl))//'.',STDERR,Mt2,T,T)
          RETURN
         END IF
        ELSE
         CALL setamx(nummdl,Lseff,argok,inptok)
         IF(Lfatal)RETURN
        END IF
c-----------------------------------------------------------------------
c     Compute the number of effective observations and initialize |G'G|
c-----------------------------------------------------------------------
        Lar=Lextar.and.Mxarlg.gt.0
        Lma=Lextma.and.Mxmalg.gt.0
c     ------------------------------------------------------------------
        IF(Lextar)THEN
         Nintvl=Mxdflg
         Nextvl=Mxarlg+Mxmalg
c     ------------------------------------------------------------------
        ELSE
         Nintvl=Mxdflg+Mxarlg
c     ------------------------------------------------------------------
         Nextvl=0
         IF(Lextma)Nextvl=Mxmalg
        END IF
c-----------------------------------------------------------------------
c     If able to read model, store model parameters for later retrieval
c-----------------------------------------------------------------------
        CALL ssprep(T,F,F)
c-----------------------------------------------------------------------
c     IF one of the models was not read in correctly, attempt to read in
c     the next model in the file.
c-----------------------------------------------------------------------
        IF(.not.argok)GO TO 10
c-----------------------------------------------------------------------
        IF(havfil.and.Nxtktp.eq.STAR)THEN
         IF(Hvstar.gt.0)THEN
          CALL nWritln('Default model already specified.',fhnote,Mt2,T,
     &                  T)
          CALL writln('       Check the model file '//
     &                Autofl(1:nblank(Autofl))//'.',fhnote,Mt2,T,T)
         ELSE
          Hvstar=1
         END IF
        ELSE IF(.not.havfil.and.nummdl.eq.1)THEN
         Hvstar=1
        END IF
       END IF
c-----------------------------------------------------------------------
c     Set up the regression matrix
c-----------------------------------------------------------------------
       CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &             Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Estimate the regression and ARMA parameters
c-----------------------------------------------------------------------
       argok=T
       lester=F
c-----------------------------------------------------------------------
c     If automatic trading day testing is done, perform test here
c-----------------------------------------------------------------------
       IF((Itdtst.gt.0.or.Leastr.or.(Luser.and.Ncusrx.gt.0).or.
     &     Lomtst.gt.0).and.(id.and.(.not.estbst)))THEN
        IF(Itdtst.gt.0)THEN
         CALL tdaic(Trnsrs,A,Nefobs,Na,Frstry,lester,tdauto,Ltdlom,F,F,
     &              F,F,0,Lhiddn)
         IF(Lfatal)RETURN
         IF(lester)THEN
          CALL eWritln('An model estimation error has occurred '//
     &                 'during the AIC testing of',STDERR,Mt2,T,F)
          CALL writln('       trading day regressor(s).  The error '//
     &                'message appears below.',STDERR,Mt2,F,T)
          CALL prterr(nefobs,T)
          IF(Id1st)THEN
           IF(Prttab(LAXMCH))
     &        CALL mkPOneLine(Mt1,'center',
     &                        'None of the models were chosen.')
           Bstdsn(1:4)='none'
           Nbstds=4
           Hvmdl=F
           RETURN
          END IF
         END IF
        END IF
        IF((.not.lester).and.Lomtst.gt.0)THEN
         CALL lomaic(Trnsrs,A,Nefobs,Na,Frstry,lester,F,F,F,F,0,Lhiddn)
         IF(Lfatal)RETURN
         IF(lester)THEN
          CALL eWritln('An model estimation error has occurred '//
     &                 'during the AIC testing of',STDERR,Mt2,T,F)
          CALL writln('       lom/loq/lpyear regressor(s).  The '//
     &                'error message appears below.',STDERR,Mt2,F,T)
          CALL prterr(nefobs,T)
          IF(Id1st)THEN
           IF(Prttab(LAXMCH))
     &        CALL mkPOneLine(Mt1,'center',
     &                        'None of the models were chosen.')
           Bstdsn(1:4)='none'
           Nbstds=4
           Hvmdl=F
           RETURN
          END IF
         END IF
        END IF
        IF((.not.lester).and.Leastr)THEN
         CALL easaic(Trnsrs,A,Nefobs,Na,Frstry,lester,F,F,F,F,0,Lhiddn)
         IF(Lfatal)RETURN
         IF(lester)THEN
          CALL eWritln('An model estimation error has occurred '//
     &                 'during the AIC testing of',STDERR,Mt2,T,F)
          CALL writln('       an Easter regressor.  The error '//
     &                'message appears below.',STDERR,Mt2,F,T)
          CALL prterr(nefobs,T)
          IF(Id1st)THEN
           IF(Prttab(LAXMCH))
     &        CALL mkPOneLine(Mt1,'center',
     &                        'None of the models were chosen.')
           Bstdsn(1:4)='none'
           Nbstds=4
           Hvmdl=F
           RETURN
          END IF
         END IF
        END IF
        IF((.not.lester).and.(Luser.and.Ncusrx.gt.0))THEN
         CALL usraic(Trnsrs,A,Nefobs,Na,Frstry,lester,F,F,F,F,0,Lhiddn)
         IF(Lfatal)RETURN
         IF(lester)THEN
          CALL eWritln('An model estimation error has occurred '//
     &                 'during the AIC testing of',STDERR,Mt2,T,F)
          CALL writln('       user defined regressor(s).  The error '//
     &                'message appears below.',STDERR,Mt2,F,T)
          CALL prterr(nefobs,T)
          IF(Id1st)THEN
           IF(Prttab(LAXMCH))
     &        CALL mkPOneLine(Mt1,'center',
     &                        'None of the models were chosen.')
           Bstdsn(1:4)='none'
           Nbstds=4
           Hvmdl=F
           RETURN
          END IF
         END IF
         IF(Ncusrx.eq.0.and.Ch2tst)Ch2tst=F
        END IF
        IF(.not.lester.and.(Ch2tst.and.Nguhl.gt.0))THEN
         CALL chkchi(Trnsrs,A,Nefobs,Na,Frstry,lester,F,F,F,F)
         IF(Lfatal)RETURN
         IF(lester)THEN
          CALL eWritln('An model estimation error has occurred '//
     &                 'during the chi square testing',STDERR,Mt2,T,F)
          CALL writln('       of user defined holiday regressor(s).'//
     &                '  The error message appears below.',
     &                STDERR,Mt2,F,T)
          CALL prterr(nefobs,T)
          IF(Id1st)THEN
           IF(Prttab(LAXMCH))
     &        CALL mkPOneLine(Mt1,'center',
     &                        'None of the models were chosen.')
           Bstdsn(1:4)='none'
           Nbstds=4
           Hvmdl=F
           RETURN
          END IF
         END IF
        END IF
        argok=.not.lester
       ELSE
        CALL rgarma(T,Mxiter,Mxnlit,F,A,Na,Nefobs,argok)
       END IF
       IF(Lfatal)RETURN
       IF(argok)THEN
c-----------------------------------------------------------------------
c     If re-estimating the best model, break out of loop now
c-----------------------------------------------------------------------
        IF(estbst)GO TO 20
c-----------------------------------------------------------------------
c     If outlier identification specified, do it here
c-----------------------------------------------------------------------
        IF(Lidotl.and.id.and.argok.and.(.not.lester))THEN
*         CALL idotlr(Ltstao,Ltstls,Ltsttc,Ltstso,Ladd1,Critvl,Cvrduc,
         CALL idotlr(Ltstao,Ltstls,Ltsttc,Ladd1,Critvl,Cvrduc,
     &               Begtst,Endtst,Nefobs,Lestim,Mxiter,Mxnlit,argok,A,
     &               Trnsrs,Nobspf,Nfcst,Outfer,Fctok,F,0,F,F,F,sviter,
     &               F,F,F,F)
         IF((.not.Lfatal).and.(.not.Convrg))THEN
          IF(Id1st)THEN
           CALL mkPOneLine(Mt1,'@',
     &                     'Rerun program trying one of the following:')
           CALL writTagClass(Mt1,'ol','indent')
           CALL writTagOneLine(Mt1,'li','@',
     &         'Allow more iterations (set a larger value of maxiter).')
           IF(havfil)THEN
            CALL writTagOneLine(Mt1,'li','@',
     &                       'Remove model from automatic model file '//
     &                       Autofl(1:nblank(Autofl))//'.')
           ELSE
            CALL writTagOneLine(Mt1,'li','@',
     &           'Use an automatic model file to specify other models.')
           END IF
           CALL writTag(Mt1,'</ol>')
           CALL mkPOneLine(Mt1,'@','See '//SPCSEC//' of the '//PRGNAM//
     &                     ' '//DOCNAM//' for more discussion.')
           IF(Prttab(LAXMCH))
     &        CALL mkPOneLine(Mt1,'center',
     &                        'None of the models were chosen.')
           Bstdsn(1:4)='none'
           Nbstds=4
           Hvmdl=F
           RETURN
          END IF 
          argok=F
         END IF
         IF((.not.Lfatal).and.argok)
     &      CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,
     &                  Nrusrx,Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
         IF(Lfatal)RETURN
        END IF
c-----------------------------------------------------------------------
c     Print model estimation errors or warning messages in the error
c     file.
c-----------------------------------------------------------------------
        IF(Armaer.lt.0.or.Armaer.gt.1)THEN
         nerr=nerr+1
         CALL mkPOneLine(Mt2,'@',
     &                ' Estimation errors associated with the model: '//
     &                Mdldsn(1:Nmddcr))
         Nefobs=Nspobs-Nintvl
         CALL prterr(nefobs,T)
         IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     If the nature of the estimation error warrants, remove the model
c     from future consideration in the automatic model search.
c-----------------------------------------------------------------------
         IF(Armaer.eq.PMXIER.or.Armaer.eq.PSNGER.or.Armaer.eq.PISNER.or.
     &      Armaer.eq.PNIFER.or.Armaer.eq.PNIMER.or.Armaer.eq.PCNTER.or.
     &      Armaer.eq.POBFN0.or.Armaer.eq.PACSER.or.Armaer.lt.0)THEN
          mdskip=T
          IF(Hvstar.eq.1)THEN
           Hvstar=0
           CALL wWritln(
     &          'Estimation error encountered for default model.',
     &          fhnote,Mt2,T,T)
           CALL writln('         Default model will not be used.',
     &                 fhnote,Mt2,T,T)
          END IF
         END IF
         Armaer=0
        END IF
        anymdl=anymdl.or.argok
        IF((.not.argok).and.(.not.mdskip))mdskip=T
c-----------------------------------------------------------------------
c     Calculate forecasts, average MAPE for three years
c-----------------------------------------------------------------------
        IF(.not.mdskip)THEN
         CALL amdfct(Trnsrs,mape,Nobspf,Nfcst,F,Fctok,argok)
         IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c      Get Ljung-Box Chi-Square results
c     check to see if argok false - if so, do not perform remaining
c     tests (BCM May 2007)
c-----------------------------------------------------------------------
         IF(argok)THEN
          i=24
          IF(Sp.eq.4)i=12
          np=0
          endlag=Opr(Nopr)-1
          DO ilag=1,endlag
           IF(.not.Arimaf(ilag))np=np+1
          END DO
          blchi=DNOTST
          IF(i.lt.Nefobs)THEN
           CALL acf(A(Na-Nefobs+1),Nefobs,Nefobs,smpac,seacf,i,np,Sp,0,
     &              T,F,F)
           IF(.not.dpeq(Qpv(i),DNOTST))THEN
            blchi=Qpv(i)*100D0
            qchi=Qs(i)
            dgfchi=Dgf(i)
           END IF
          END IF
c-----------------------------------------------------------------------
c      Sum regular and seasonal MA terms to test for overdifferencing
c-----------------------------------------------------------------------
          begopr=Mdl(MA-1)
          endopr=Mdl(MA)-1
          sma=0D0
          rma=0D0
          ovrdff=F
          ovrsdf=F
          DO iopr=begopr,endopr
           beglag=Opr(iopr-1)
           endlag=Opr(iopr)-1
           CALL getstr(Oprttl,Oprptr,Noprtl,iopr,tmpttl,ntmpcr)
           IF(Lfatal)RETURN
           IF(tmpttl(1:ntmpcr).eq.'Seasonal MA')THEN
            DO ilag=beglag,endlag
             sma=sma+Arimap(ilag)
            END DO
           ELSE IF(tmpttl(1:ntmpcr).eq.'Nonseasonal MA')THEN
            DO ilag=beglag,endlag
             rma=rma+Arimap(ilag)
            END DO
           END IF
          END DO
          IF(Nnsedf.gt.0.and.rma.ge.Ovrdif)ovrdff=T
          IF(Nseadf.gt.0.and.sma.ge.Ovrdif)THEN
           ovrsdf=T
           IF(.not.gsovdf)gsovdf=T
          END IF
         END IF
c-----------------------------------------------------------------------
c     Print out model information.
c     add argok to arguments of prtamd (BCM May 2007)
c-----------------------------------------------------------------------
         IF(Prttab(LAXMDL))THEN
          CALL prtamd(Mdldsn(1:Nmddcr),mape,blchi,qchi,dgfchi,nummdl,T,
     &                ovrdff,ovrsdf,Fctok,argok)
          IF(Lfatal)RETURN
         END IF
c-----------------------------------------------------------------------
c     Test to see if model is accepted.  If so, print out message and
c     exit while loop.
c     add check to see if argok false (BCM May 2007)
c-----------------------------------------------------------------------
         IF(((Fctok.and.argok).and.mape(4).le.loclim).and.blchi.gt.Qlim
     &      .and.(.not.ovrdff))THEN
          IF(.not.Hvmdl)THEN
           Hvmdl=T
           IF(Pck1st)tstmdl=F
          END IF
          numbst=nummdl
          loclim=mape(4)
          CALL bstmdl(Nbstds,Bstdsn,bstptd)
          IF(Hvstar.eq.2)Hvstar=3
         ELSE IF(.not.Hvmdl.and.Hvstar.eq.1.and.argok)THEN
          Hvstar=2
          numbst=nummdl
          CALL bstmdl(Nbstds,Bstdsn,bstptd)
         END IF
        END IF
c-----------------------------------------------------------------------
       ELSE
c-----------------------------------------------------------------------
c     Print model estimation errors or warning messages in the error
c     file.
c-----------------------------------------------------------------------
        IF(Armaer.lt.0.or.Armaer.gt.1)THEN
         nerr=nerr+1
         CALL mkPOneLine(Mt2,'@',
     &                ' Estimation errors associated with the model: '//
     &                Mdldsn(1:Nmddcr))
         Nefobs=Nspobs-Nintvl
         CALL prterr(Nefobs,T)
         IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     If the nature of the estimation error warrants, remove the model
c     from future consideration in the automatic model search.
c-----------------------------------------------------------------------
         IF(Armaer.eq.PMXIER.or.Armaer.eq.PSNGER.or.Armaer.eq.PISNER.or.
     &      Armaer.eq.PNIFER.or.Armaer.eq.PNIMER.or.Armaer.eq.PCNTER.or.
     &      Armaer.eq.POBFN0.or.Armaer.eq.PACSER.or.Armaer.lt.0)THEN
          mdskip=T
          IF(Hvstar.eq.1)THEN
           Hvstar=0
           CALL wWritln(
     &          'Estimation error encountered for default model.',
     &          fhnote,Mt2,T,T)
           CALL writln('         Default model will not be used.',
     &                 fhnote,Mt2,T,T)
          END IF
         END IF
         Armaer=0
        END IF
       END IF
   10 CONTINUE
      END DO
c-----------------------------------------------------------------------
c     Print out more complete message concerning seasonal 
c     overdifferencing.
c-----------------------------------------------------------------------
      IF(gsovdf)WRITE(Mt1,1130)Ovrdif
c-----------------------------------------------------------------------
c     If first model used for identification of outliers and td, check
c     to see if another model was selected.
c-----------------------------------------------------------------------
   20 IF(Id1st.and.(Lidotl.or.Leastr.or.Itdtst.gt.0).and.numbst.gt.1)
     &   THEN
c-----------------------------------------------------------------------
c     If so, restore original regressors and redo model estimation.
c-----------------------------------------------------------------------
       Ngrp=ngr1
       Ngrptl=ngrt1
       Ncxy=ncxy1
       Nb=nb1
       Ncoltl=nct1
       Colttl(1:ncttl)=cttl1(1:ncttl)
       Grpttl(1:ngttl)=gttl1(1:ngttl)
       CALL cpyint(clptr1(0),PB+1,1,Colptr(0))
       CALL cpyint(g1(0),PGRP+1,1,Grp(0))
       CALL cpyint(gptr1(0),PGRP+1,1,Grpptr(0))
       CALL cpyint(rgv1,PB,1,Rgvrtp)
       CALL copy(b1,PB,1,B)
c-----------------------------------------------------------------------
c     IF td has changed, restore transformed series, original prior
c     adjustment.
c-----------------------------------------------------------------------
       IF(.not.(Picktd.eqv.pktd))THEN
        CALL copy(tsrs,PLEN,1,Trnsrs)
        CALL copy(a2,PLEN,1,Adj)
        Picktd=pktd
        Priadj=padj2
       END IF
c-----------------------------------------------------------------------
c     Compute the number of effective observations and initialize |G'G|
c-----------------------------------------------------------------------
       Lar=Lextar.and.Mxarlg.gt.0
       Lma=Lextma.and.Mxmalg.gt.0
c     ------------------------------------------------------------------
       IF(Lextar)THEN
        Nintvl=Mxdflg
        Nextvl=Mxarlg+Mxmalg
c     ------------------------------------------------------------------
       ELSE
        Nintvl=Mxdflg+Mxarlg
c     ------------------------------------------------------------------
        Nextvl=0
        IF(Lextma)Nextvl=Mxmalg
       END IF
c-----------------------------------------------------------------------
c     Set up the regression matrix
c-----------------------------------------------------------------------
       CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &             Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Estimate the regression and ARMA parameters
c-----------------------------------------------------------------------
       argok=T
       lester=F
       IF(Leastr.or.Itdtst.gt.0.or.(Luser.and.Ncusrx.gt.0).or.
     &    Lomtst.gt.0)THEN
c-----------------------------------------------------------------------
c     If automatic trading day testing is done, perform test here
c-----------------------------------------------------------------------
        CALL ssprep(T,F,F)
        IF((.not.lester).and.Itdtst.gt.0)THEN
         CALL tdaic(Trnsrs,A,Nefobs,Na,Frstry,lester,tdauto,Ltdlom,F,F,
     &              F,F,0,Lhiddn)
         IF(Lfatal)RETURN
         IF(lester)THEN
          CALL eWritln('A model estimation error has occurred during '//
     &                 'the AIC testing of',STDERR,Mt2,T,F)
          CALL writln('       trading day regressor(s).  The error '//
     &                'message appears below.',STDERR,Mt2,F,T)
          CALL prterr(nefobs,T)
          IF(Prttab(LAXMCH))
     &       CALL mkPOneLine(Mt1,'center',
     &                       'None of the models were chosen.')
          Bstdsn(1:4)='none'
          Nbstds=4
          Hvmdl=F
          RETURN
         END IF
        END IF
        IF((.not.lester).and.Lomtst.gt.0)THEN
         CALL lomaic(Trnsrs,A,Nefobs,Na,Frstry,lester,F,F,F,F,0,Lhiddn)
         IF(Lfatal)RETURN
         IF(lester)THEN
          CALL eWritln('A model estimation error has occurred during '//
     &                 'the AIC testing of',STDERR,Mt2,T,F)
          CALL writln('       lom/loq/lpyear regressor(s).  The '//
     &                'error message appears below.',STDERR,Mt2,F,T)
          CALL prterr(nefobs,T)
          IF(Prttab(LAXMCH))
     &       CALL mkPOneLine(Mt1,'center',
     &                       'None of the models were chosen.')
          Bstdsn(1:4)='none'
          Nbstds=4
          Hvmdl=F
          RETURN
         END IF
        END IF
        IF((.not.lester).and.Leastr)THEN
         CALL easaic(Trnsrs,A,Nefobs,Na,Frstry,lester,F,F,F,F,0,Lhiddn)
         IF(Lfatal)RETURN
         IF(lester)THEN
          CALL eWritln('A model estimation error has occurred during '//
     &                 'the AIC testing of',STDERR,Mt2,T,F)
          CALL writln('       an Easter regressor.  The error '//
     &                'message appears below.',STDERR,Mt2,F,T)
          CALL prterr(nefobs,T)
          IF(Prttab(LAXMCH))
     &       CALL mkPOneLine(Mt1,'center',
     &                       'None of the models were chosen.')
          Bstdsn(1:4)='none'
          Nbstds=4
          Hvmdl=F
          RETURN
         END IF
        END IF
        IF((.not.lester).and.(Luser.and.Ncusrx.gt.0))THEN
         CALL usraic(Trnsrs,A,Nefobs,Na,Frstry,lester,F,F,F,F,0,Lhiddn)
         IF(Lfatal)RETURN
         IF(lester)THEN
          CALL eWritln('A model estimation error has occurred during '//
     &                 'the AIC testing of',STDERR,Mt2,T,F)
          CALL writln('       user defined regressor(s).  The error '//
     &                'message appears below.',STDERR,Mt2,F,T)
          CALL prterr(nefobs,T)
          IF(Prttab(LAXMCH))
     &       CALL mkPOneLine(Mt1,'center',
     &                       'None of the models were chosen.')
          Bstdsn(1:4)='none'
          Nbstds=4
          Hvmdl=F
          RETURN
         END IF
         IF(Ncusrx.eq.0.and.Ch2tst)Ch2tst=F
        END IF
        IF((.not.lester).and.(Ch2tst.and.Nguhl.gt.0))THEN
         CALL chkchi(Trnsrs,A,Nefobs,Na,Frstry,lester,F,F,F,F)
         IF(Lfatal)RETURN
         IF(lester)THEN
          CALL eWritln('An model estimation error has occurred '//
     &                 'during the chi square testing',STDERR,Mt2,T,F)
          CALL writln('       of user defined holiday regressor(s).'//
     &                '  The error message appears below.',
     &                STDERR,Mt2,F,T)
          CALL prterr(nefobs,T)
          IF(Id1st)THEN
           IF(Prttab(LAXMCH))
     &        CALL mkPOneLine(Mt1,'center',
     &                        'None of the models were chosen.')
           Bstdsn(1:4)='none'
           Nbstds=4
           Hvmdl=F
           RETURN
          END IF
         END IF
        END IF
       ELSE
        CALL rgarma(T,Mxiter,Mxnlit,F,A,Na,Nefobs,argok)
        IF(.not.argok)CALL abend()
       END IF
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     If outlier identification specified, do it here
c-----------------------------------------------------------------------
       IF(Lidotl)THEN
*        CALL idotlr(Ltstao,Ltstls,Ltsttc,Ltstso,Ladd1,Critvl,Cvrduc,
        CALL idotlr(Ltstao,Ltstls,Ltsttc,Ladd1,Critvl,Cvrduc,
     &              Begtst,Endtst,Nefobs,Lestim,Mxiter,Mxnlit,argok,A,
     &              Trnsrs,Nobspf,Nfcst,Outfer,Fctok,F,0,F,F,F,sviter,
     &              F,F,F,F)
        IF((.not.Lfatal).and.(.not.Convrg))THEN
         CALL eWritln('No ARIMA models stored in '//
     &                Autofl(1:nblank(Autofl))//'.',STDERR,Mt2,T,T)
         CALL writln('        Check contents of file and try again.',
     &               STDERR,Mt2,T,T)
         IF(Prttab(LAXMCH))
     &      CALL mkPOneLine(Mt1,'center',
     &                      'None of the models were chosen.')
         Bstdsn(1:4)='none'
         Nbstds=4
         Hvmdl=F
         RETURN
        END IF
        IF(.not.argok)CALL abend()
        IF(.not.Lfatal)
     &     CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &                 Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
        IF(Lfatal)RETURN
       END IF
      END IF
c-----------------------------------------------------------------------
c     Check to see if default model is to be used.
c-----------------------------------------------------------------------
      IF(.not.Hvmdl.and.Hvstar.eq.2)THEN
       IF(Prttab(LAXMCH))
     &   CALL mkPOneLine(Mt1,'center','None of the models were chosen.')
       IF(Adjtd.eq.1.OR.(AdjAO.eq.1.and.Nao.gt.0).OR.(AdjLS.eq.1.and.
     &    (Nls.gt.0.or.Nramp.gt.0)).OR.(AdjTC.eq.1.and.Ntc.gt.0).or.
     &    (AdjSO.eq.1.and.Nso.gt.0).or.Adjsea.eq.1.or.Adjusr.eq.1.or.
     &    Adjhol.eq.1.or.Finusr.or.FinAO.or.FinLS.or.Fintc.or.Finhol)
     &    THEN
        Hvmdl=T
        CALL nofcst(Trnsrs,Frstry,Lsadj)
        IF(Lfatal)RETURN
        IF(Prttab(LAXMCH))WRITE(Mt1,1091)Bstdsn(1:Nbstds)
       ELSE
        Bstdsn(1:4)='none'
        Nbstds=4
        IF(nerr.gt.0)THEN
         CALL wWritln('Estimation errors occured during the '//
     &                'automatic model selection procedure.',
     &                fhnote,Mt2,T,T)
         CALL writln('          For more details, check error file ',
     &                fhnote,Mt2,T,F)
         CALL writln('          '//Cursrs(1:nblank(Cursrs))//
     &               '_err.html.',fhnote,Mt2,F,T)
        END IF
        RETURN
       END IF
c-----------------------------------------------------------------------
c     Print out message describing model chosen
c-----------------------------------------------------------------------
      ELSE IF(Prttab(LAXMCH))THEN
       CALL mkPOneLine(Mt1,'center',
     &                 'The model chosen is '//Bstdsn(1:Nbstds))
      END IF
c-----------------------------------------------------------------------
c     If Backcasting is done, test backcast extrapolation model.
c-----------------------------------------------------------------------
      IF(Nbcst.gt.0.and.Hvstar.ne.2)THEN
       CALL amdfct(Trnsrs,mape,Nobspf,Nfcst,T,Fctok,argok)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Print out backcasting infromation
c-----------------------------------------------------------------------
       IF(Prttab(LAXHDB))THEN
        CALL genSkip(LAXHDB)
        CALL writTagOneLine(Mt1,'h2','center',
     &            ' Autoregressive Integrated Moving Average (ARIMA) '//
     &            'extrapolation program')
        CALL writTagOneLine(Mt1,'h3','center',
     &            ' ARIMA extrapolation model (backcast)')
       END IF
c-----------------------------------------------------------------------
       IF(Prttab(LAXMDL))THEN
        CALL prtamd(Mdldsn(1:Nmddcr),mape,blchi,qchi,dgfchi,numbst,F,
     &              ovrdff,ovrsdf,Fctok,argok)
        IF(Lfatal)RETURN
       END IF
c-----------------------------------------------------------------------
c     Test to see if model is accepted.  If so, print out message.
c     check to see if argok false and print out error message for
c     backcasts (BCM May 2007)
c-----------------------------------------------------------------------
       IF(mape(4).gt.Bcklim.and.(.not.argok))THEN
        Nbcst=0
        Pos1bk=Pos1ob
        IF(Prttab(LAXMCH))CALL mkPOneLine(Mt1,'indent',
     &                     'This model was not chosen for backcasting.')
       ELSE IF(Prttab(LAXMCH))THEN
        CALL mkPOneLine(Mt1,'center',
     &                  'The model chosen is '//Bstdsn(1:Nbstds))
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(nerr.gt.0.or.(.not.argok))THEN
       CALL wWritln('Estimation errors occured during the automatic '//
     &              'model selection procedure.',fhnote,Mt2,T,T)
       CALL writln('          For more details, check error file ',
     &             fhnote,Mt2,T,F)
       CALL writln('          '//Cursrs(1:nblank(Cursrs))//
     &             '_err.html.',fhnote,Mt2,F,T)
      END IF
c-----------------------------------------------------------------------
 1020 FORMAT(' <p><strong>Model selected:</strong> First model that ',
     &       'meets acceptance criteria.</p>')
 1030 FORMAT(' <p><strong>Model selected:</strong> Model with lowest ',
     &       'average forecast error that',/,
     &       ' meets acceptance criteria.</p>')
 1091 FORMAT('<p class="indent">A default model specified by the user,',
     &       ' ',a,',',/,
     &       ' will be used to generate regARIMA preadjustment factors',
     &       '</p>')
 1130 FORMAT(' <p><strong>WARNING:</strong> The seasonal <abbr ',
     &       'title="moving average">MA</abbr> coefficient(s) ',
     &       'for at least one of the models',/,
     &       ' tested above have a sum exceeding ',f6.3,'.</p>',/,
     &       '<p>Examine whether a differencing can be eliminated from',
     &       ' the',/,
     &       ' regARIMA model in favor of a trend constant in the ',
     &       'regression',/,
     &       ' spec, or whether a seasonal differencing should be ',
     &       'replaced',/,
     &       ' by the use of fixed seasonal effects in the regression ',
     &       'spec.</p>')
 1140 FORMAT(a,': ',a)
 1150 FORMAT(a,': ',f12.6)
c-----------------------------------------------------------------------
      RETURN
      END

