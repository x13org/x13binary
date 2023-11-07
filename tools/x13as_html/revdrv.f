C     Last change:  Jan. 2021,
c change variable name cmdl to cmdls to avoid conflit with common block
c name cmdl in model.cmn and change an error message--Jan. 2021
C     previous change:  BCM  23 Mar 2005    3:38 pm
      SUBROUTINE revdrv(Ltmax,Lmodel,Lx11,X11agr,Lseats,Lcomp,Lgraf,
     &                  Iagr,Ncomp)
      IMPLICIT NONE
C-----------------------------------------------------------------------
c     Driver routine for the revision analysis procedure.
C-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'xrgmdl.cmn'
      INCLUDE 'stdio.i'
      INCLUDE 'arima.cmn'
      INCLUDE 'inpt.cmn'
      INCLUDE 'lkhd.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'rev.prm'
      INCLUDE 'rev.cmn'
      INCLUDE 'revtrg.cmn'
      INCLUDE 'revsrs.cmn'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'seatdg.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'xrgtbl.i'
      INCLUDE 'mdltbl.i'
      INCLUDE 'revtbl.i'
      INCLUDE 'svllog.prm'
      INCLUDE 'svllog.cmn'
      INCLUDE 'dgnsvl.i'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'otlrev.cmn'
      INCLUDE 'otxrev.cmn'
      INCLUDE 'x11reg.cmn'
      INCLUDE 'x11log.cmn'
      INCLUDE 'usrxrg.cmn'
      INCLUDE 'missng.cmn'
      INCLUDE 'cchars.i'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'orisrs.cmn'
      INCLUDE 'xeastr.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      LOGICAL T,F
      INTEGER LRVR1S,LRVR1A
      PARAMETER(T=.true.,F=.false.,LRVR1S=LREVR1+1,LRVR1A=LREVR1+2)
c-----------------------------------------------------------------------
      CHARACTER outstr*(85),igrptl*(PGRPCR),datstr*(10),cmdls*(132),
     &          usfxtl*(PCOLCR*PUREG*2),chARMA*(PGRPCR+5),cblnk*(25),
     &          chash*(25),tfmt1*(5),
     &          outARMA*(6+(23*PARIMA)),chRgGp*(PGRPCR),chTDrg*(PCOLCR),
     &          outTDrg*(6+(16*(PGRPCR+PCOLCR)))
      LOGICAL Lmodel,Lx11,X11agr,locok,lchmsr,lhdr,lx11fn,addreg,erregm,
     &        Lcomp,revmdl,lr1y2y,Lgraf,tdfix,holfix,usrfix,Lseats,
     &        lsetfn,otlfix,revsa,ltmp,lastfx,upuser,bfx2,upusrx,lstxfx,
     &        svltbl
      DOUBLE PRECISION revaic,rvlkhd,CncARMA,Cnctdrg
      INTEGER i,i2,j,k,fh,fh2,ipos,Ltmax,Iagr,smdmat,ncmdl,idtpos,
     &        nefobs,vmsr,endall,idate,rdbdat,ndate,othndl,begcol,igrp,
     &        nchr,lf1,begrgm,regmdt,nbeg,nend,mdl2,mdl2x,nendx,endcol,
     &        icol,nusfx,nusftl,usfptr,Ncomp,fhnote,nchARMA,nRgGp,
     &        rGrpNm,nTDrg,grptot,Nrvtdrg,jgrp,ipos2
*      INTEGER itick1,itick2
      DIMENSION smdmat(PREV,6),revaic(PREV),vmsr(PREV),rvlkhd(PREV),
     &          endall(2),idate(2),regmdt(2),mdl2(2),mdl2x(2),bfx2(PB),
     &          usfptr(0:PUREG),chARMA(PARIMA),nchARMA(PARIMA),
     &          CncARMA(PARIMA,PREV),ChTDrg(PARIMA),NTDrg(16),NRgGp(5),
     &          Cnctdrg(16,PREV),ChRgGp(5),rGrpNm(5)
C-----------------------------------------------------------------------
      LOGICAL istrue,dpeq
      INTEGER nblank,strinx
      EXTERNAL istrue,nblank,strinx,dpeq
C-----------------------------------------------------------------------
c     Check if revisions history is set up correctly
c-----------------------------------------------------------------------
      fhnote=STDERR
      IF(Lquiet)fhnote=0
      CALL revchk(Irev,Irevsa,Ixreg,Ny,Pos1ob,Posfob,Ltmax,Nspobs,
     &            Begspn,Endspn,Begmdl,Lx11,Lseats,Lmodel,Lnoprt,Iagr,
     &            Ncomp,Fctdrp,lr1y2y,revsa,revmdl,fhnote,Khol,Kfulsm)
c-----------------------------------------------------------------------
      IF(Irev.eq.0.or.Lfatal)RETURN
C-----------------------------------------------------------------------
c     Print out revisions history header
c-----------------------------------------------------------------------
      IF((Lrvsa.or.Lrvsf.or.Lrvch.or.Lrvtrn.or.Lrvtch.or.Lrvaic
     &    .or.Lrvfct).and.Prttab(LRVHDR))THEN
c-----------------------------------------------------------------------
       CALL genSkip(LRVHDR)
       CALL revhdr(lr1y2y,revsa,revmdl,Lmodel,Ny,Endspn,Iagr)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
      addreg=F
c-----------------------------------------------------------------------
c     If automatic modelling done, setup so the model selected for
c     the entire series will be used in each model estimation done by
c     the revisions history procedure.
c-----------------------------------------------------------------------
      Lautox=F
      Lautom=F
      Lautod=F
c-----------------------------------------------------------------------
      tdfix=F
      holfix=F
      usrfix=F
      otlfix=F
      IF(Nrvfxr.gt.0.and.((Nb.gt.0.and.Iregfx.lt.3).or.Nbx.gt.0))THEN
       DO i=1,Nrvfxr
        IF(Rvfxrg(i).eq.1)THEN
         tdfix=T
        ELSE IF(Rvfxrg(i).eq.2)THEN
         holfix=T
        ELSE IF(Rvfxrg(i).eq.3)THEN
         usrfix=T
        ELSE IF(Rvfxrg(i).eq.4)THEN
         otlfix=T
        END IF
       END DO
      END IF
      IF(Nb.gt.0)CALL rvfixd(tdfix,holfix,otlfix,usrfix,Iregfx,Regfx,
     &                       Nb,Rgvrtp,Nusrrg,Usrtyp,Ncusrx,Userfx)
      IF(Nbx.gt.0)CALL rvfixd(tdfix,holfix,otlfix,usrfix,Irgxfx,Regfxx,
     &                        Nbx,Rgxvtp,Nusxrg,Usxtyp,Nusxrg,Usrxfx)
      IF((Lrvsa.or.Lrvch).and.Kfulsm.eq.2)THEN
       IF((tdfix.and.(Adjtd.eq.1.or.Axrgtd)).or.
     &    (holfix.and.(Adjhol.eq.1.or.Axrghl.or.Khol.gt.0)).or.
     &    (Revfix.and.(Adjtd.eq.1.or.Adjhol.eq.1)))THEN
        IF(Lrvsa)THEN
         IF(Iagr.eq.0.or.Iagr.ge.5)THEN
          CALL eWritln('Cannot calculate revision statistics for '//
     &                 'seasonally adjusted data',STDERR,Mt2,T,F)
          CALL writln('       if a trend estimation run is specified'//
     &                ' in the x11 spec and',STDERR,Mt2,F,F)
          CALL writln('       trading day and/or holiday factors are '//
     &                'held fixed.',STDERR,Mt2,F,T)
          Lrvsa=F
         ELSE
          IF(Prttab(LREVR1))Prttab(LREVR1)=F
          IF(Prttab(LRVR1S))Prttab(LRVR1S)=F
          IF(Prttab(LRVR1A))Prttab(LRVR1A)=F
          IF(Savtab(LREVR1))Savtab(LREVR1)=F
         END IF
        END IF
        IF(Lrvch)THEN
         CALL eWritln('Cannot calculate revision statistics for '//
     &                'changes in the adjusted data',STDERR,Mt2,T,F)
          CALL writln('       if a trend estimation run is specified'//
     &                ' in the x11 spec and',STDERR,Mt2,F,F)
          CALL writln('       trading day and/or holiday factors are'//
     &                ' held fixed.',STDERR,Mt2,F,T)
         Lrvch=F
        END IF
       END IF
       IF(.not.(Lrvsa.or.Lrvsf.or.Lrvch.or.Lrvaic.or.Lrvfct.or.Lrvtrn
     &    .or.Lrvtch))THEN
        CALL wWritln('History analysis will not be performed for this'//
     &               ' run because',fhnote,Mt2,T,F)
        CALL writln('         of error(s) indicated above.',
     &              fhnote,Mt2,F,T)
        Irev=0
        IF(Irev.gt.0)Irevsa=-1
        RETURN
       END IF
      END IF
c-----------------------------------------------------------------------
c     Remove level changes for series, regression matrix, if necessary
c-----------------------------------------------------------------------
      lhdr=T
c-----------------------------------------------------------------------
      IF(Lmodel)THEN
       addreg=T
c-----------------------------------------------------------------------
c     Check to see if there are any change of regime regression
c     variables in the model.  If there is, check to see if the
c     change of regime will be defined over the revision period.
c-----------------------------------------------------------------------
       IF(Lrgmse.or.Lrgmtd.or.Lrgmln)THEN
        erregm=F
        locok=T
        DO igrp=1,Ngrp
         begcol=Grp(igrp-1)
         endcol=Grp(igrp)-1
         IF(Rgvrtp(begcol).eq.PRATST.or.Rgvrtp(begcol).eq.PRATTD.or.
     &      Rgvrtp(begcol).eq.PRATSE.or.Rgvrtp(begcol).eq.PRATTS.or.
     &      Rgvrtp(begcol).eq.PRATLM.or.Rgvrtp(begcol).eq.PRATLQ.or.
     &      Rgvrtp(begcol).eq.PRATLY.or.Rgvrtp(begcol).eq.PRATSL.or.
     &      Rgvrtp(begcol).eq.PRA1TD.or.Rgvrtp(begcol).eq.PRA1ST)THEN
c-----------------------------------------------------------------------
          CALL getstr(Grpttl,Grpptr,Ngrp,igrp,igrptl,nchr)
          IF(Lfatal)RETURN
          idtpos=index(igrptl(1:nchr),'(starting ')+10
          CALL ctodat(igrptl(1:nchr-1),Ny,idtpos,regmdt,locok)
          CALL dfdate(Lupbeg,regmdt,Ny,begrgm)
          IF(begrgm.le.Ny)THEN
           IF(.not.erregm)THEN
            CALL nWritln('The following change of regime regression '//
     &                   'variables are not',Mt1,Mt2,T,F)
            CALL writln(' defined for at least one year before the'//
     &                  ' startup period of the',Mt1,Mt2,F,F)
            CALL writln(' history analysis:',Mt1,Mt2,F,F)
           END IF
           erregm=T
           CALL writln(' '//igrptl(1:nchr),Mt1,Mt2,F,T)
c-----------------------------------------------------------------------
           DO icol=begcol,endcol
            IF(.not.Regfx(icol))Regfx(icol)=T
           END DO
c-----------------------------------------------------------------------
          END IF
         END IF
        END DO
        IF(erregm)THEN
         CALL writln(' The regressors listed above will be fixed to'//
     &               ' their estimated values from the original '//
     &               'series',Mt1,Mt2,T,T)
         Iregfx=2
        END IF
c-----------------------------------------------------------------------
c     If change-of-regime regression variables check out, be sure
c     model parameters are fixed.
c-----------------------------------------------------------------------
c        IF(.not.Revfix)THEN
c         Revfix=T
c         CALL writln('NOTE: Since change of regime regression variables
c     &are used, model',STDERR,Mt2,T)
c         CALL writln('      parameters will be held fixed during the his
c     &tory analysis.',STDERR,Mt2,F)
c        END IF
       END IF
c-----------------------------------------------------------------------
c     Check the user defined regression variables
c-----------------------------------------------------------------------
       CALL dfdate(Begmdl,Begspn,Ny,nbeg)
       CALL dfdate(Endspn,Endmdl,Ny,nend)
       CALL cpyint(Endmdl,2,1,mdl2)
c-----------------------------------------------------------------------
c     Fix model parameters, if requested.
c-----------------------------------------------------------------------
       IF(Revfix)THEN
        CALL setlg(T,PARIMA,Arimaf)
        CALL setlg(T,PB,Regfx)
        Iregfx=3
        IF(.not.Userfx)THEN
         Userfx=Ncusrx.gt.0
         IF(Userfx)
     &      CALL bakusr(Userx,Usrtyp,Usrptr,Ncusrx,Usrttl,Regfx,B,
     &                  Rgvrtp,Ngrp,Grpttl,Grp,Grpptr,Ngrptl,0,T)
        END IF
       END IF
c-----------------------------------------------------------------------
c     Turn off automatic outlier identification.
c-----------------------------------------------------------------------
       IF(Otlrev.lt.2)THEN
        IF(Ltstls)Ltstls=F
        IF(Ltstao)Ltstao=F
        IF(Ltsttc)Ltsttc=F
*        IF(Ltstso)Ltstso=F
       ELSE
*        IF((.not.Ltstls).and.(.not.Ltstao).and.(.not.Ltsttc).and.
*     &     (.not.Ltstso))THEN
        IF((.not.Ltstls).and.(.not.Ltstao).and.(.not.Ltsttc))THEN
         Ltstls=T
         Ltstao=T
        END IF
       END IF
c-----------------------------------------------------------------------
c     Outliers automatically identified in a previous run will be
c     removed from the regression model
c-----------------------------------------------------------------------
       IF(Otlrev.gt.0)THEN
        CALL rmatot(Nrxy,Otlrev,Otlwin,Beglup,Begxy,othndl,otlfix,
     &              Prttab(LREVOT),Savtab(LREVOT),lhdr)
        IF(Lfatal)RETURN
       END IF
c-----------------------------------------------------------------------
c     Remove outliers from the regression variables if they occur
c     after the starting date of the revision history analysis.
c-----------------------------------------------------------------------
       CALL intlst(PB,Otrptr,Notrtl)
       CALL rmotrv(Begxy,Beglup,Nrxy,Botr,Otrptr,Notrtl,Fixotr,Otrttl,
     &             Otlrev.eq.0.or.Otlrev.eq.2,othndl,Prttab(LREVOT),
     &             Savtab(LREVOT),lhdr)
       IF(Lfatal)RETURN
       IF(Notrtl.gt.0)CALL ssprep(Lmodel,F,F)
      END IF
c-----------------------------------------------------------------------
      IF(Ixreg.gt.0)THEN
       CALL cpyint(Endxrg,2,1,mdl2x)
       IF(Revfxx)THEN
        CALL setlg(T,PB,Regfxx)
        IF(Irgxfx.lt.3)Irgxfx=3
        IF(.not.Usrxfx)THEN
         Usrxfx=Nusxrg.gt.0
         IF(Usrxfx)
     &      CALL bakusr(Xuserx,Usxtyp,Usrxpt,Nusxrg,Usrxtt,Regfxx,Bx,
     &                  Rgxvtp,Nxgrp,Grpttx,Grpx,Gpxptr,Ngrptx,1,T)
        END IF
       END IF
c-----------------------------------------------------------------------
c     If reweighting is to be done, check to see if trading day
c     regressors are fixed.  If so, reset Lxrneg
c-----------------------------------------------------------------------
       igrp=strinx(T,Grpttx,Gpxptr,1,Ngrptx,'Trading Day')
       IF(Lxrneg.and.igrp.gt.0)THEN
        begcol=Grpx(igrp-1)
        endcol=Grpx(igrp)-1
        Lxrneg=.not.istrue(Regfxx,begcol,endcol)
       END IF
c-----------------------------------------------------------------------
c     Outliers automatically identified in a previous run will be
c     removed from the irregular regression model
c-----------------------------------------------------------------------
       CALL loadxr(F)
       IF(Rvxotl)THEN
        CALL rmatot(Nrxy,1,Otlwin,Beglup,Begxy,0,otlfix,F,F,lhdr)
        IF(Lfatal)RETURN
       END IF
c-----------------------------------------------------------------------
c     Remove outliers from the irregular regression variables if they
c     occur after the starting date of the revision history analysis.
c-----------------------------------------------------------------------
       CALL intlst(PB,Otxptr,Notxtl)
       CALL rmotrv(Begxy,Beglup,Nrxy,Botx,Otxptr,Notxtl,Fixotx,Otxttl,
     &             .not.Rvxotl,0,F,F,ltmp)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
       CALL loadxr(T)
       IF(Lmodel)CALL restor(Lmodel,F,F)
      END IF
c-----------------------------------------------------------------------
c     Set up printing control variables.
c-----------------------------------------------------------------------
      IF(Rvtran)THEN
       Lhiddn=T
*       write(*,*)'  suppressing printing'
*      ELSE
*       write(*,*)'  no print suppression'
      END IF
      DO i=1,NTBL
c     ------------------------------------------------------------------
c     BCM - July 29, 2009
c     Special code to save seasonal factor forecasts for concurrent
c     SEATS adjustments.
c       IF(i.lt.LRVHDR.or.i.GT.(LREVR8+2))THEN
c     ------------------------------------------------------------------
       IF(i.lt.LRVHDR.or.i.GT.(LRVR9B))THEN
        IF(Rvtran)THEN
         Prttab(i)=F
         Savtab(i)=F
        ELSE
         IF(i.ne.LESTES.and.i.ne.LXRXRG.and.i.ne.(LXRXRG+1))Savtab(i)=F
        END IF
       END IF
      END DO
      DO i=1,NSVLOG
       IF(i.lt.LSLASA.or.i.gt.LSLALR)Svltab(i)=F
      END DO
c-----------------------------------------------------------------------
c     Prepare for revisions looping by storing current values of the
c     modeling parameters.
c-----------------------------------------------------------------------
C      CALL ssprep(Lmodel,Lx11,Ixreg.gt.0)
      CALL ssprep(Lmodel,F,F)
c-----------------------------------------------------------------------
c     Start to loop through the last Ny*3 seasonal adjustment,
c     storing the concurrent seasonal factor and seasonally adjusted
c     series.
c-----------------------------------------------------------------------
      Irev=4
      locok=T
      lchmsr=F
c-----------------------------------------------------------------------
      CALL intlst(PUREG,usfptr,nusftl)
      nusfx=nusftl+1
c-----------------------------------------------------------------------
      lx11fn=Lx11
      lsetfn=Lseats
      Ierhdr=NOTSET
c     ------------------------------------------------------------------
c     BCM - July 29, 2009
c     Special code to save seasonal factor forecasts for concurrent
c     adjustments.
c     ------------------------------------------------------------------
      IF(Savtab(LRVSSH))THEN
       CALL opnfil(T,F,LRVSSH,Fhsfh,locok)
       IF(.not.locok)THEN
        CALL abend
        RETURN
       END IF
      END IF
c     ------------------------------------------------------------------
      DO i=Beglup,Endrev
c-----------------------------------------------------------------------
c     Check to see if we have gotten to the end of the seasonal
c     adjustment revisions history.  If so, ensure seasonal adjustment
c     is not performed until the final seasonal adjustment.
c-----------------------------------------------------------------------
       IF(i.gt.Endsa)THEN
        IF(i.eq.Endrev.and.lx11fn)THEN
         Lx11=T
        ELSE IF(Lx11)THEN
         Lx11=F
        END IF
        IF(i.eq.Endrev.and.lsetfn)THEN
         Lseats=T
        ELSE IF(Lseats)THEN
         Lseats=F
        END IF
       END IF
c-----------------------------------------------------------------------
c     IF i < Begrev, then either do a special seasonal adjustment for
c     projected factors, or fit model parameters for the first occurance
c     of the period Fixper.
c-----------------------------------------------------------------------
       IF(i.ge.Begrev)THEN
c-----------------------------------------------------------------------
c     if no seasonal adjustment and none of the model diagnostics are
c     being saved, then break out of revision loop.
c-----------------------------------------------------------------------
        IF(i.eq.Begrev)THEN
         Lx11=lx11fn
         Lseats=lsetfn
        END IF
        IF(.not.(Lx11.or.Lseats).and..not.revmdl)GO TO 10
       ELSE IF(i.eq.Beglup)THEN
        IF(Beglup.lt.Frstsa)THEN
         Lx11=F
         Lseats=F
        END IF
       ELSE IF(i.eq.Frstsa)THEN
        IF(Beglup.lt.Frstsa)THEN
         Lx11=lx11fn
         Lseats=lsetfn
        END IF
       ELSE
        GO TO 10
       END IF
c-----------------------------------------------------------------------
c     Initialize variables for seasonal adjustment loop.
c-----------------------------------------------------------------------
       Posfob=i
       CALL x11int
       Posffc=i+Nfcst
       Length=Posfob-Pos1ob+1
       Nspobs=Length
       Nofpob=Nspobs+Nfcst
       Nbfpob=Nspobs+Nfcst+Nbcst
       Nobspf=min(Nspobs+max(Nfcst-Fctdrp,0),Nomnfy)
       Lstyr=Lyr+(Posfob/Ny)
       IF(mod(Posfob,Ny).eq.0)Lstyr=Lstyr-1
c-----------------------------------------------------------------------
c     Copy original series into STcsi and Series
c-----------------------------------------------------------------------
       CALL copy(Orig(Pos1ob),Length,1,Stcsi(Pos1ob))
       CALL copy(Orig(Pos1ob),Length,1,Series(Pos1ob))
c-----------------------------------------------------------------------
c     Set span Beginning and Ending Date for modeling routines
c-----------------------------------------------------------------------
       CALL addate(Begspn,Ny,Posfob-Pos1ob,Endspn)
       CALL wrtdat(Endspn,Sp,Crvend,Nrvend)
       IF(Lfatal)RETURN
       IF(Lmodel)THEN
        IF(Fixper.gt.0)THEN
         CALL cpyint(Endspn,2,1,Endmdl)
         IF(Endmdl(MO).eq.Fixper)THEN
          addreg=T
         ELSE
          IF(Endmdl(MO).lt.Fixper)Endmdl(YR)=Endmdl(YR)-1
          Endmdl(MO)=Fixper
          IF(addreg)addreg=F
         END IF
        ELSE
         CALL dfdate(Endspn,mdl2,Ny,nend)
         IF(nend.le.0)THEN
          CALL cpyint(Endspn,2,1,Endmdl)
         ELSE
          CALL cpyint(mdl2,2,1,Endmdl)
         END IF
        END IF
       END IF
       IF(Fxprxr.gt.0)THEN
        CALL cpyint(Endspn,2,1,Endxrg)
        IF(Endxrg(MO).ne.Fxprxr)THEN
         IF(Endxrg(MO).lt.Fxprxr)Endxrg(YR)=Endxrg(YR)-1
         Endxrg(MO)=Fxprxr
        END IF
       ELSE
        nendx=0
        IF(Ixreg.gt.0)CALL dfdate(Endspn,mdl2x,Ny,nendx)
        IF(nendx.le.0)THEN
         CALL cpyint(Endspn,2,1,Endxrg)
        ELSE
         CALL cpyint(mdl2x,2,1,Endxrg)
        END IF
       END IF
c-----------------------------------------------------------------------
c     Reset start and end of outlier testing
c-----------------------------------------------------------------------
       IF(Otlrev.ge.2)THEN
        CALL addate(Endspn,Ny,-Otlwin,Begtst)
        CALL cpyint(Endspn,2,1,Endtst)
       END IF
c-----------------------------------------------------------------------
c     Reset pointer for concurrent revision.
c-----------------------------------------------------------------------
       Revptr=i-Begrev+1
c-----------------------------------------------------------------------
c     Restore previous seasonal adjustment and modelling settings
c-----------------------------------------------------------------------
       CALL restor(Lmodel,Lx11,Ixreg.gt.0)
       IF(Lmsr.eq.6)Lterm=Lmsr
       IF(Ixreg.eq.3)THEN
        Ixreg=1
        IF(Lmodel.or.Fxprxr.gt.0.or.Khol.gt.0)Ixreg=2
       END IF
c       IF(.not.Lrfrsh)Iregfx=1
c-----------------------------------------------------------------------
c     Set logical variable that generates X-11 holiday date indicator
c     variable
c-----------------------------------------------------------------------
       IF(Lgenx)THEN
        Lgenx=F
c-----------------------------------------------------------------------
c     Check to see if easter adjustment can be done in all spans,
c     if specified.
c-----------------------------------------------------------------------
        IF(Keastr.eq.1)THEN
         lf1=(Pos1bk/12)*12+3
         IF(lf1.lt.Pos1bk)lf1=lf1+12
         CALL chkeas(lf1,Posfob)
         IF((Ieast(1)*Ieast(2)*Ieast(3)*Ieast(4)).eq.0)THEN
          CALL eWritln('X-11 Easter adjustment cannot be estimated '//
     &                'for the history',STDERR,Mt2,T,F)
          CALL writln('       analysis specified in this run',
     &                STDERR,Mt2,F,T)
          CALL mkPOneLine(Mt2,'@','This is because there are:')
          CALL writTag(Mt2,'<ul>')
          Keastr=0
         END IF
         IF(Ieast(1).eq.0)
     &      CALL writTagOneLine(Mt2,'li','@',
     &         'No years of data with Easter before April 1st.')
         IF(Ieast(2).eq.0)
     &      CALL writTagOneLine(Mt2,'li','@',
     &         'No years of data with Easter after April 16th.')
         IF(Ieast(3).eq.0)
     &      CALL writTagOneLine(Mt2,'li','@',
     &         'No years of data with Easter between April 2nd and '//
     &         'April 8th.')
         IF(Ieast(4).eq.0)
     &      CALL writTagOneLine(Mt2,'li','@',
     &         'No years of data with Easter between April 8th and '//
     &         'April 15th.')
         IF((Ieast(1)*Ieast(2)*Ieast(3)*Ieast(4)).eq.0)THEN
          CALL writTag(Mt2,'</ul>')
          CALL writln('       Either choose a later starting date '//
     &                'for the history analysis',
     &                STDERR,Mt2,T,F)
          CALL writln('or preadjust the series using Easter effects '//
     &                'estimated from a',
     &                STDERR,Mt2,F,F)
          CALL writln('regARIMA model.',STDERR,Mt2,F,T)
         END IF
        END IF
       END IF
c-----------------------------------------------------------------------
c     Check to see if X-11 holiday adjustment can still be done.
c-----------------------------------------------------------------------
       IF(Khol.eq.2)Khol=Keastr
c-----------------------------------------------------------------------
c     If outliers have been removed, check if they can be
c     reintroduced into the regression matrix.
c-----------------------------------------------------------------------
       IF(addreg.and.i.gt.Begrev.and.Notrtl.gt.0)THEN
        CALL dfdate(Endspn,Endmdl,Ny,nend)
        CALL chkorv(Begxy,i-nend,Botr,Otrptr,Notrtl,Fixotr,Otrttl,
     &              othndl,otlfix,Nrxy,Prttab(LREVOT),Savtab(LREVOT),
     &              lhdr,T)
        IF(Lfatal)RETURN
       END IF
       IF(Ixreg.gt.0)THEN
        CALL loadxr(F)
        IF(i.gt.Begrev.and.Notxtl.gt.0.and.(.not.Rvxotl))THEN
         CALL dfdate(Endspn,Endxrg,Ny,nend)
         CALL chkorv(Begxy,i-nend,Botx,Otxptr,Notxtl,Fixotx,Otxttl,0,
     &               otlfix,Nrxy,F,F,ltmp,F)
         IF(Lfatal)RETURN
        ELSE IF(Rvxotl)THEN
         CALL rmatot(Nrxy,1,Otlwin,Beglup,Begxy,0,otlfix,F,F,lhdr)
         IF(Lfatal)RETURN
        END IF
        CALL loadxr(T)
        IF(Lmodel)CALL restor(Lmodel,F,F)
       END IF
c-----------------------------------------------------------------------
c     Reset missing value code.
c-----------------------------------------------------------------------
       IF(Missng)Missng=F
c-----------------------------------------------------------------------
c     Check user-defined regressors to see that they are well-defined
c     for the span of data
c-----------------------------------------------------------------------
       upuser=F
       upusrx=F
       lastfx=Userfx
       lstxfx=Usrxfx
       IF(Nusxrg.gt.0)THEN
        CALL copylg(Regfxx,Nbx,1,bfx2)
        CALL chusrg(upusrx,usfxtl,nusfx,nusftl,usfptr)
        IF(Lfatal)RETURN
        IF(upusrx)THEN
         IF(.not.Usrxfx)Usrxfx=T
         CALL bakusr(Xuserx,Usxtyp,Usrxpt,Nusxrg,Usrxtt,Regfxx,Bx,
     &               Rgxvtp,Nxgrp,Grpttx,Grpx,Gpxptr,Ngrptx,1,
     &               .not.lstxfx)
        END IF
       END IF
       IF(Ncusrx.gt.0)THEN
        CALL copylg(Regfx,Nb,1,bfx2)
        CALL chusrg(upuser,usfxtl,nusfx,nusftl,usfptr)
        IF(Lfatal)RETURN
        IF(upuser)THEN
         IF(.not.Userfx)Userfx=T
         CALL bakusr(Userx,Usrtyp,Usrptr,Ncusrx,Usrttl,Regfx,B,Rgvrtp,
     &               Ngrp,Grpttl,Grp,Grpptr,Ngrptl,0,.not.lastfx)
         CALL ssprep(T,F,F)
        END IF 
       END IF 
c-----------------------------------------------------------------------
c     Perform seasonal adjustment.
c-----------------------------------------------------------------------
      CALL x11ari(Lmodel,Lx11,X11agr,Lseats,Lcomp,Issap,Irev,Irevsa,
     &             Ixreg,0,F,F)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
       IF(Irev.lt.0)THEN
        Irev=0
        IF(Irevsa.gt.0)Irevsa=-1
        RETURN
       END IF
C-----------------------------------------------------------------------
c     If there was an error in the ARIMA model estimation, print out
c     the error message here.
C-----------------------------------------------------------------------
       IF(Armaer.lt.0.or.Armaer.gt.1)THEN
        nefobs=Nspobs-Nintvl
        CALL prterr(nefobs,F)
        IF(Armaer.eq.PMXIER.or.Lfatal)THEN
         IF(.not.Lfatal)call abend
         RETURN
        END IF
        Armaer=0
       END IF
C-----------------------------------------------------------------------
c     Store AICC and likelihood values
C-----------------------------------------------------------------------
       IF(Revptr.gt.0)THEN
        IF(Lrvaic)THEN
         revaic(Revptr)=Aicc
         rvlkhd(Revptr)=Olkhd
        END IF
C-----------------------------------------------------------------------
c     Store ARMA Model Parameters
C-----------------------------------------------------------------------
        IF(Lrvarma)THEN
         CALL rvarma(Revptr,chARMA,nchARMA,Nrvarma,CncARMA)
         IF(Lfatal)RETURN
        END IF
C-----------------------------------------------------------------------
c     Store regARMA trading day Parameters
C-----------------------------------------------------------------------
        IF(Lrvtdrg)THEN
         CALL rvtdrg(Revptr,chRgGp,nRgGp,rGrpNm,grptot,chTDrg,nTDrg,
     &               Nrvtdrg,Cnctdrg)
         IF(Lfatal)RETURN
         IF(grptot.eq.0)Lrvtdrg=.false.
        END IF
C-----------------------------------------------------------------------
c     Store Seats Model
C-----------------------------------------------------------------------
        IF(Lseats)THEN
         smdmat(Revptr,1)=Iprsm
         smdmat(Revptr,2)=Idrsm
         smdmat(Revptr,3)=Iqrsm
         IF(Ipssm.eq.NOTSET)THEN
          smdmat(Revptr,4)=Ipssm
         ELSE
          smdmat(Revptr,4)=Ipssm/Ny
         END IF
         smdmat(Revptr,5)=Idssm
         IF(Iqssm.eq.NOTSET)THEN
          smdmat(Revptr,6)=Iqssm
         ELSE
          smdmat(Revptr,6)=Iqssm/Ny
         END IF
        END IF
C-----------------------------------------------------------------------
c     Store choice of seasonal filter if MSR seasonal chosen.  Test if
c     choice of seasonal filter has changed.
C-----------------------------------------------------------------------
        IF(Lmsr.eq.5.and.Revptr.gt.0)THEN
         vmsr(Revptr)=Lterm
         IF(.not.lchmsr.and.(vmsr(1).ne.vmsr(Revptr)))lchmsr=T
        END IF
       END IF
C-----------------------------------------------------------------------
c     Delete automatically identified outliers if any are found.
C-----------------------------------------------------------------------
       IF(Otlrev.ge.2)THEN
        CALL rmatot(Nrxy,Otlrev,Otlwin,i,Begxy,othndl,otlfix,
     &              Prttab(LREVOT),Savtab(LREVOT),lhdr)
        IF(Lfatal)RETURN
C-----------------------------------------------------------------------
c     Create revision files for Andrew Bruce...
C-----------------------------------------------------------------------
c       CALL copy(Stci(Pos1ob),Nobspf,1,temp)
c       WRITE(rsahnd,*) (temp(j),j=1,nrvopf)
c       CALL copy(Facao(Pos1ob),Nobspf,1,temp)
c       WRITE(raohnd,*) (temp(j),j=1,nrvopf)
c       CALL copy(Facls(Pos1ob),Nobspf,1,temp)
c       WRITE(rlshnd,*) (temp(j),j=1,nrvopf)
C-----------------------------------------------------------------------
       END IF
C-----------------------------------------------------------------------
c     Refresh stored model parameters
C-----------------------------------------------------------------------
       IF(Lmodel)THEN
        IF(upuser)THEN
         CALL copylg(bfx2,Nb,1,Regfx)
         Userfx=lastfx
         CALL ssprep(T,F,F)
        END IF
        IF(Lrfrsh)CALL restor(Lmodel,F,F)
       END IF
       IF(upusrx)THEN
        CALL copylg(bfx2,Nb,1,Regfxx)
        Usrxfx=lstxfx
       END IF
   10  CONTINUE
      END DO
c     ------------------------------------------------------------------
c     BCM - September 9, 2009
c     Special code to save seasonal factor forecasts for concurrent
c     adjustments.
c     ------------------------------------------------------------------
      IF(Savtab(LRVSSH))CALL fclose(Fhsfh)
C-----------------------------------------------------------------------
      Irev=5
      IF(Ierhdr.ne.NOTSET)CALL errhdr
c-----------------------------------------------------------------------
      IF(Prttab(LREVOT))THEN
       IF(lhdr)THEN
        CALL mkPOneLine(Mt1,'@',
     &   'No outliers kept or deleted during this history analysis.')
       ELSE
        CALL writTag(Mt1,'</table></div>')
        CALL mkPOneLine(Mt1,'@','&nbsp;')
       END IF
      END IF
C-----------------------------------------------------------------------
      IF(nusftl.gt.0)THEN
       CALL nWritln('The user defined regressors listed below were '//
     &              'held fixed',Mt1,Mt2,T,F)
       CALL writln(' for at least one span during the history '//
     &             'analysis:',Mt1,Mt2,F,F)
       DO igrp=1,nusftl
        CALL getstr(usfxtl,usfptr,nusfx,igrp,outstr,ipos)
        IF(Lfatal)RETURN
        CALL writln(' '//outstr(1:ipos),Mt1,Mt2,F,T)
       END DO
      END IF
c-----------------------------------------------------------------------
c     close outlier files.
C-----------------------------------------------------------------------
c      IF(Otlrev.ge.2)THEN
c       CALL fclose(othndl)
c       CALL fclose(rsahnd)
c       CALL fclose(raohnd)
c       CALL fclose(rlshnd)
c       WRITE(STDOUT,*)'  P x N Revisions matrices have been stored in:'
c       WRITE(STDOUT,*)'  ',Cursrs(1:nsrs)//'.rsa, ',
c     &                Cursrs(1:nsrs)//'.rao, ',Cursrs(1:nsrs)//'.rls'
c       WRITE(STDOUT,*)'  P=# of revisions=',nrvsrs
c       WRITE(STDOUT,*)'  N=# of observations in fcst extended series=',
c     &                nrvopf
c      END IF
c-----------------------------------------------------------------------
c     Compute the starting and ending date of revisions
c-----------------------------------------------------------------------
      CALL addate(Rvstrt,Ny,Revnum-1,endall)
      IF(Lsumm.gt.0)CALL svrvhd(endall,Ny,Irevsa)
c-----------------------------------------------------------------------
c     If seasonal filter selection from the moving seasonality ratio
c     has changed over the revision span, print a warning message.
c-----------------------------------------------------------------------
      IF(lchmsr)THEN
       fh2=0
       IF(.not.Lnoprt)fh2=Mt1
       CALL nWritln('The seasonal filter used to generate the '//
     &              'seasonal component has ',Mt2,fh2,T,F)
       CALL writln('       changed during the revision period.  ',
     &             Mt2,fh2,F,F)
       CALL writln('       This could increase the size of revisions.',
     &             Mt2,fh2,F,T)
       IF(.not.Prttab(LREVR0))Prttab(LREVR0)=T
      END IF
c-----------------------------------------------------------------------
      IF(Lmsr.eq.5)THEN
       CALL prtmsr(vmsr,Rvstrt,Ny,LREVR0)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
      Lhiddn=F
      Kpart=7
c-----------------------------------------------------------------------
c     Print and/or save the revisions of the seasonally adjusted series.
c-----------------------------------------------------------------------
      IF(istrue(Svltab,LSLASA,LSLASP))THEN
       Inlgfl=Inlgfl+1
       WRITE(Ng,1001)Inlgfl
       CALL mkTableTag(Ng,'w60',
     &                'History: Summary of Average Absolute Revision')
       CALL mkCaption(Ng,
     &                'History: Summary of Average Absolute Revision')
       CALL writTag(Ng,'<tr>')
       CALL mkTableCell(Ng,'head','&nbsp;')
       CALL mkHeaderCellScope(Ng,0,0,'col','@',
     &                        'Average Absolute'//Cbr//'Revision')
       CALL writTag(Ng,'</tr>')
      END IF
      IF(Lrvsa)
     &   CALL prtrev(Finsa,Cncsa,Rvstrt,1,LREVR1,Ntarsa,Targsa,Ny,Lsumm,
     &               Lgraf,lr1y2y)
c-----------------------------------------------------------------------
c     Print and/or save the changes in the seasonally adjusted series
c     revisions.
c-----------------------------------------------------------------------
      IF(.not.Lfatal.and.Lrvch)
     &   CALL prtrev(Finch,Cncch,Rvstrt,2,LREVR2,Ntarsa,Targsa,Ny,Lsumm,
     &               Lgraf,lr1y2y)
c-----------------------------------------------------------------------
c     Print and/or save the revisions of the indirect seasonally
c     adjusted series.
c-----------------------------------------------------------------------
      IF(.not.Lfatal.and.(Iagr.ge.5.and.Lrvsa))THEN
       IF(Nrcomp.eq.Ncomp.and.Indrev.gt.0)THEN
        IF(Lsumm.gt.0)WRITE(Nform,1130)'yes'
        CALL prtrev(Finisa,Cncisa,Rvstrt,3,LREVR3,Ntarsa,Targsa,Ny,
     &              Lsumm,Lgraf,lr1y2y)
       ELSE
        IF(Lsumm.gt.0)WRITE(Nform,1130)'no'
       END IF
      END IF
c-----------------------------------------------------------------------
c     Print and/or save the trend revisions.
c-----------------------------------------------------------------------
      IF(.not.Lfatal.and.Lrvtrn)
     &   CALL prtrev(Fintrn,Cnctrn,Rvstrt,4,LREVR4,Ntartr,Targtr,Ny,
     &               Lsumm,Lgraf,F)
c-----------------------------------------------------------------------
c     Print and/or save the revisions in the month-to-month (or quarter-
c     to-quarter) changes in the trend.
c-----------------------------------------------------------------------
      IF(.not.Lfatal.and.Lrvtch)
     &   CALL prtrev(Fintch,Cnctch,Rvstrt,5,LREVR5,Ntartr,Targtr,Ny,
     &               Lsumm,Lgraf,F)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Print and/or save the seasonal factor revisions.
c-----------------------------------------------------------------------
      IF(.not.Lfatal.and.Lrvsf)
     &  CALL prtrv2(Finsf,Cncsf,Cncsfp,Rvstrt,LREVR6,Ny,Lsumm,Lgraf)
      IF(istrue(Svltab,LSLASA,LSLASP))THEN
       CALL writTag(Ng,'</table></div>')
       CALL mkPOneLine(Ng,'@','&nbsp;')
      END IF
c-----------------------------------------------------------------------
c     Reset ending date if AICC or forecast revisions are printed out.
c-----------------------------------------------------------------------
      IF(Lrvaic.or.Lrvfct)CALL addate(endall,Ny,1,endall)
c-----------------------------------------------------------------------
c     Print Likelihood statistics
c-----------------------------------------------------------------------
      IF(Lrvaic)THEN
       j=0
       IF(Savtab(LREVR7).or.Lgraf)THEN
        IF(Savtab(LREVR7))CALL opnfil(T,F,LREVR7,fh,locok)
        IF(locok.and.Lgraf)CALL opnfil(T,Lgraf,LREVR7,fh2,locok)
        IF(.not.locok)THEN
         CALL abend
         RETURN
        END IF
c-----------------------------------------------------------------------
c     Print header for AICC revisions
c-----------------------------------------------------------------------
        IF(Savtab(LREVR7))THEN
         WRITE(fh,1120)'date',TABCHR,'Log_Likelihood',TABCHR,'AICC'
         WRITE(fh,1120)'------',TABCHR,'-----------------------',
     &                          TABCHR,'-----------------------'
        END IF
        IF(Lgraf)THEN
         WRITE(fh2,1120)'date',TABCHR,'Log_Likelihood',TABCHR,'AICC'
         WRITE(fh2,1120)'------',TABCHR,'-----------------------',
     &                           TABCHR,'-----------------------'
        END IF
       END IF
c-----------------------------------------------------------------------
c     Print header for likelihood statistics
c-----------------------------------------------------------------------
       IF(Prttab(LREVR7))THEN
        CALL genSkip(LREVR7)
        WRITE(Mt1,1090)Rvstrt(MO),Rvstrt(YR),endall(MO),endall(YR)
 1090   FORMAT('<h3>R 7.  Likelihood statistics from estimating ',
     &         'regARIMA model over spans with',/,
     &         ' ending dates ',i2,':',i4,' to ',i2,':',i4,'</h3>')
        Inrv=Inrv+1
        WRITE(Ng,1000)Inrv
        CALL mkTableTag(Mt1,'w40','@')
        CALL mkCaption(Mt1,'Likelihood statistic history table')
        CALL writTag(Mt1,'<tr>')
        CALL mkTableCell(Mt1,'head','&nbsp;')
        CALL mkHeaderCellScope(Mt1,0,0,'col','@','Log Likelihood')
        CALL mkHeaderCellScope(Mt1,0,0,'col','@','<abbr title="'//
     &            'corrected Akaike information criterion">AICC</abbr>')
        CALL writTag(Mt1,'</tr>')
       END IF
       DO i=Begrev,Endrev
        Revptr=i-Begrev+1
        j=j+1
c-----------------------------------------------------------------------
c     Print out AICC and Likelihood value
c-----------------------------------------------------------------------
        CALL addate(Rvstrt,Ny,j-1,idate)
        CALL wrtdat(idate,Ny,datstr,ndate)
        IF(Lfatal)RETURN
        IF(Prttab(LREVR7))THEN
         CALL writTag(Mt1,'<tr>')
         CALL mkHeaderCellScope(Mt1,0,0,'row','@',datstr(1:ndate))
         WRITE(Mt1,1110)rvlkhd(Revptr),revaic(Revptr)
         CALL writTag(Mt1,'</tr>')
        END IF
 1110   FORMAT(2('<td>',f15.3,'</td>'))
        IF(Savtab(LREVR7).or.Lgraf)THEN
c-----------------------------------------------------------------------
c     Set date of revision for observation Revptr
c-----------------------------------------------------------------------
         rdbdat=100*idate(YR)+idate(MO)
c-----------------------------------------------------------------------
c     Save AICC revisions with date
c-----------------------------------------------------------------------
         ipos=1
         CALL itoc(rdbdat,outstr,ipos)
         IF(Lfatal)RETURN
         outstr(ipos:ipos)=TABCHR
         ipos=ipos+1
         CALL dtoc(rvlkhd(Revptr),outstr,ipos)
         IF(Lfatal)RETURN
         outstr(ipos:ipos)=TABCHR
         ipos=ipos+1
         CALL dtoc(revaic(Revptr),outstr,ipos)
         IF(Lfatal)RETURN
         IF(Savtab(LREVR7))WRITE(fh,1120)outstr(1:ipos-1)
         IF(Lgraf)WRITE(fh2,1120)outstr(1:ipos-1)
        END IF
       END DO
       IF(Prttab(LREVR7))THEN
        CALL writTag(Mt1,'</table></div>')
        CALL mkPOneLine(Mt1,'@','&nbsp;')
       END IF
       IF(Savtab(LREVR7))CALL fclose(fh)
       IF(Lgraf)CALL fclose(fh2)
      END IF
c-----------------------------------------------------------------------
c     Print forecast errors
c-----------------------------------------------------------------------
      IF(Lrvfct)THEN
*       CALL gnfcrv(fcter,fctss,Orig)
*       CALL prfcrv(fcter,fctss,endall,Ny,LREVR8,LSLAFE,Lgraf,Lsumm)
       CALL prfcrv(Orig,endall,Ny,Lam,Fcntyp,LREVR8,LSLAFE,Lgraf,Lsumm)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
c     Print SEATS models
c-----------------------------------------------------------------------
      IF(Lseats.and.(Prttab(LREVR9).or.Savtab(LREVR9)))THEN
       IF(Savtab(LREVR9))THEN
        CALL opnfil(T,F,LREVR9,fh,locok)
        IF(.not.locok)THEN
         CALL abend
         RETURN
        END IF
        WRITE(fh,1120)'date',TABCHR,'SEATS_Model'
        WRITE(fh,1120)'------',TABCHR,'-----------------------'
       END IF
c-----------------------------------------------------------------------
c     Print header for SEATS model
c-----------------------------------------------------------------------
       IF(Prttab(LREVR9))THEN
        CALL genSkip(LREVR9)
        WRITE(Mt1,2090)Rvstrt(MO),Rvstrt(YR),endall(MO),endall(YR)
 2090   FORMAT('<h3>R 9.  SEATS ARIMA model used for spans with',/,
     &         ' ending dates ',i2,':',i4,' to ',i2,':',i4,'</h3>')
        CALL makDivId(Mt1,'r9','@')
        CALL mkTableTag(Mt1,'w40','@')
        CALL mkCaption(Mt1,'SEATS Model history table')
        CALL writTag(Mt1,'<tr>')
        CALL mkTableCell(Mt1,'head','&nbsp;')
        CALL mkHeaderCellScope(Mt1,0,0,'col','@','ARIMA Model')
        CALL writTag(Mt1,'</tr>')
       END IF
c-----------------------------------------------------------------------
       j=0
       DO i=Begrev,Endrev
        Revptr=i-Begrev+1
        j=j+1
c-----------------------------------------------------------------------
c     Print out SEATS model
c-----------------------------------------------------------------------
        CALL addate(Rvstrt,Ny,j-1,idate)
        CALL wrtdat(idate,Ny,datstr,ndate)
        IF(Lfatal)RETURN
        IF(smdmat(Revptr,1).eq.NOTSET)THEN
         IF(Prttab(LREVR9))THEN
          CALL writTag(Mt1,'<tr>')
          CALL mkHeaderCellScope(Mt1,0,0,'row','@',datstr(1:ndate))
          CALL mkTableCell(Mt1,'@','no model')
          CALL writTag(Mt1,'</tr>')
         END IF
         IF(Savtab(LREVR9))WRITE(fh,1120)datstr(1:ndate),TABCHR,'none'
        ELSE
         CALL mkmdsn(smdmat(Revptr,1),smdmat(Revptr,2),smdmat(Revptr,3),
     &               smdmat(Revptr,4),smdmat(Revptr,5),smdmat(Revptr,6),
     &               cmdls,ncmdl)
         IF(Lfatal)RETURN
         IF(Prttab(LREVR9))THEN
          CALL writTag(Mt1,'<tr>')
          CALL mkHeaderCellScope(Mt1,0,0,'row','@',datstr(1:ndate))
          CALL mkTableCell(Mt1,'center',cmdls(1:ncmdl))
          CALL writTag(Mt1,'</tr>')
         END IF
         IF(Savtab(LREVR9))WRITE(fh,1120)datstr(1:ndate),TABCHR,
     &                                   cmdls(1:ncmdl)
        END IF
       END DO
       IF(Prttab(LREVR9))THEN
        CALL writTag(Mt1,'</table></div>')
        CALL mkPOneLine(Mt1,'@','&nbsp;')
       END IF
       IF(Savtab(LREVR9))CALL fclose(fh)
      END IF
c-----------------------------------------------------------------------
c     Print history of ARIMA coefficients
c-----------------------------------------------------------------------
      IF(Lrvarma)THEN
       IF(.not.(Prttab(LRVR9A).or.Savtab(LRVR9A)))RETURN
       j=0
       IF(Savtab(LRVR9A))THEN
        CALL opnfil(T,F,LRVR9A,fh,locok)
        IF(.not.locok)THEN
         CALL abend
         RETURN
        END IF
        write(tfmt1,1140)2*Nrvarma+1
 1140   FORMAT('(',i2,'a)')
        WRITE(fh,tfmt1)'date',
     &                (TABCHR,chARMA(i)(1:nchARMA(i)),i=1,Nrvarma)
        WRITE(fh,tfmt1)'------',
     &       (TABCHR,'-----------------------',i=1,Nrvarma)
       END IF
       CALL setchr('-',25,chash)
       CALL setchr(' ',25,cblnk)
c-----------------------------------------------------------------------
c     Print header for ARIMA coefficients
c-----------------------------------------------------------------------
       IF(Prttab(LRVR9A))THEN
        CALL genSkip(LRVR9A)
        WRITE(Mt1,3090)Rvstrt(MO),Rvstrt(YR),endall(MO),endall(YR)
 3090   FORMAT('<h3>R 9.A ARIMA model coefficients used for spans ',
     &     'with ending dates ',i2,':',i4,' to ',i2,':',i4,'</h3>',/)
        CALL makDivId(Mt1,'r9a','@')
        CALL mkTableTag(Mt1,'w60','@')
        CALL mkCaption(Mt1,'ARIMA model coefficients history table')
        CALL writTag(Mt1,'<tr>')
        CALL mkTableCell(Mt1,'head','&nbsp;')
        DO k=1,Nrvarma
         CALL mkHeaderCellScope(Mt1,0,0,'col','@',
     &                          chARMA(k)(1:nchARMA(k)))
        END DO
        CALL writTag(Mt1,'</tr>')
       END IF
       DO i=Begrev,Endrev
        Revptr=i-Begrev+1
        j=j+1
c-----------------------------------------------------------------------
c     Print out ARIMA coefficients
c-----------------------------------------------------------------------
        CALL addate(Rvstrt,Ny,j-1,idate)
        CALL wrtdat(idate,Ny,datstr,ndate)
        IF(Lfatal)RETURN
        IF(Prttab(LRVR9A))THEN
         CALL writTag(Mt1,'<tr>')
         CALL mkHeaderCellScope(Mt1,0,0,'row','@',datstr(1:ndate))
         WRITE(Mt1,1260)(CncARMA(k,Revptr),k=1,Nrvarma)
         CALL writTag(Mt1,'</tr>')
        END IF
c-----------------------------------------------------------------------
c     Save ARIMA model coefficients
c-----------------------------------------------------------------------
        IF(Savtab(LRVR9A))THEN
         rdbdat=100*idate(YR)+idate(MO)
         ipos=1
         CALL itoc(rdbdat,outARMA,ipos) 
         IF(Lfatal)RETURN
         DO k=1,Nrvarma
          outARMA(ipos:ipos)=TABCHR
          ipos=ipos+1
          CALL dtoc(CncARMA(k,Revptr),outARMA,ipos)
          IF(Lfatal)RETURN
         END DO
         WRITE(fh,1120)outARMA(1:ipos-1)
        END IF
       END DO
c-----------------------------------------------------------------------
       IF(Prttab(LREVR9))THEN
        CALL writTag(Mt1,'</table></div>')
        CALL mkPOneLine(Mt1,'@','&nbsp;')
       END IF
       IF(Savtab(LRVR9A))CALL fclose(fh)
      END IF
c-----------------------------------------------------------------------
c     Print history of regARIMA TD coefficients
c-----------------------------------------------------------------------
      IF(Lrvtdrg)THEN
       IF(.not.(Prttab(LRVR9B).or.Savtab(LRVR9B)))RETURN
       IF(Savtab(LRVR9B))THEN
        CALL opnfil(T,F,LRVR9B,fh,locok)
        IF(.not.locok)THEN
         CALL abend
         RETURN
        END IF
        outTDrg(1:8)='Span End'
        icol=0
        ipos=9
        DO igrp=1,grptot
         DO jgrp=1,RGrpNm(igrp)
          icol=icol+1
          outTDrg(ipos:ipos)=TABCHR
          ipos=ipos+1
          ipos2=ipos+(NRgGp(igrp)+NTDrg(icol))
          outTDrg(ipos:ipos2)=ChRgGp(igrp)(1:NRgGp(igrp))//'.'//
     &                        ChTDrg(icol)(1:NTDrg(icol))
          ipos=ipos2+1
         END DO
        END DO
        WRITE(fh,1120)outTDrg(1:ipos-1)
        write(tfmt1,1141)2*Nrvtdrg+1
 1141   FORMAT('(',i2,'a)')
        WRITE(fh,tfmt1)'------',
     &       (TABCHR,'-----------------------',i=1,Nrvtdrg)
       END IF
       CALL setchr('-',25,chash)
       CALL setchr(' ',25,cblnk)
c-----------------------------------------------------------------------
       IF(Prttab(LRVR9B))THEN
        CALL genSkip(LRVR9B)
        WRITE(Mt1,3091)Rvstrt(MO),Rvstrt(YR),endall(MO),endall(YR)
 3091   FORMAT('<h3>R 9.B regARIMA trading day coefficients used for ',
     &         'spans with ending dates ',i2,':',i4,' to ',i2,':',i4,
     &         '</h3>')
        icol=0
        DO igrp=1,grptot
         WRITE(Mt1,3092)igrp,ChRgGp(igrp)(1:NRgGp(igrp))
 3092    FORMAT(/,'<h4>Group ',i2,' : ',a,'</h4>',/)
         write(Mt1,3093)igrp
 3093    FORMAT('<div id="r9b.g',i3.3,'">')
         IF(RGrpNm(igrp).eq.1)THEN
          CALL mkTableTag(Mt1,'w20','@')
         ELSE IF(RGrpNm(igrp).ge.6)THEN 
          CALL mkTableTag(Mt1,'w60','@')
         ELSE
          CALL mkTableTag(Mt1,'w40','@')
         END IF
         CALL mkCaption(Mt1,
     &      'regARIMA trading day coefficients history table : '//
     &      ChRgGp(igrp)(1:NRgGp(igrp)))
         CALL writTag(Mt1,'<tr>')
         CALL mkTableCell(Mt1,'head','&nbsp;')
         DO i2=icol+1,icol+RGrpNm(igrp)
          CALL mkHeaderCellScope(Mt1,0,0,'col','@',
     &                           ChTDrg(i2)(1:NTDrg(i2)))
         END DO
         CALL writTag(Mt1,'</tr>')
         j=0
         DO i=Begrev,Endrev
          Revptr=i-Begrev+1
          j=j+1
c-----------------------------------------------------------------------
c     Print out regARIMA trading day coefficients
c-----------------------------------------------------------------------
          CALL addate(Rvstrt,Ny,j-1,idate)
          CALL wrtdat(idate,Ny,datstr,ndate)
          IF(Lfatal)RETURN
          CALL writTag(Mt1,'<tr>')
          CALL mkHeaderCellScope(Mt1,0,0,'row','@',datstr(1:ndate))
          WRITE(Mt1,1260)(CncTDrg(k,Revptr),k=icol+1,icol+RGrpNm(igrp))
          CALL writTag(Mt1,'</tr>')
         END DO
         icol=icol+RGrpNm(igrp)
         CALL writTag(Mt1,'</table></div>')
         CALL mkPOneLine(Mt1,'@','&nbsp;')
        END DO
       END IF
c-----------------------------------------------------------------------
c     Save regARIMA trading day coefficients
c-----------------------------------------------------------------------
       IF(Savtab(LRVR9B))THEN
        j=0
        DO i=Begrev,Endrev
         Revptr=i-Begrev+1
         j=j+1
         CALL addate(Rvstrt,Ny,j-1,idate)
         rdbdat=100*idate(YR)+idate(MO)
         ipos=1
         CALL itoc(rdbdat,outTDrg,ipos) 
         IF(Lfatal)RETURN
         DO k=1,NrvTDrg
          outTDrg(ipos:ipos)=TABCHR
          ipos=ipos+1
          CALL dtoc(CncTDrg(k,Revptr),outTDrg,ipos)
          IF(Lfatal)RETURN
         END DO
         WRITE(fh,1120)outTDrg(1:ipos-1)
        END DO
        CALL fclose(fh)
       END IF
      END IF
c-----------------------------------------------------------------------
      RETURN
c-----------------------------------------------------------------------
 1000 FORMAT('<div id="rv',i3.3,'">')
 1001 FORMAT('<div id="lgrv',i6.6,'">')
 1120 FORMAT(a:,a,a:,a,a,a,a)
 1130 FORMAT('historyindsa: ',a)
 1260 FORMAT('<td class="nowrap">',F16.6,'</td>') 
      END
c-----------------------------------------------------------------------
