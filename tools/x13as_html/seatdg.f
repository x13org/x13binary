C     Last change:  BCM  19 May 2003    9:29 am
      SUBROUTINE seatdg(Issap,Irev,Irevsa,Ny,Iag,Iagr,Muladd,Lsumm,
     &                  Lseats,Lgraf,Lam,Nfcst,Length)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'dimensions.i'
      INCLUDE 'notset.prm'
      INCLUDE 'model.prm'
      INCLUDE 'stdio.i'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'seatcm.cmn'
      INCLUDE 'seatdg.cmn'
      INCLUDE 'seatlg.cmn'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'orisrs.cmn'
      INCLUDE 'rev.prm'
      INCLUDE 'rev.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'svllog.prm'
      INCLUDE 'svllog.cmn'
      INCLUDE 'setsvl.i'
      INCLUDE 'inpt.cmn'
      INCLUDE 'cmpflts.i'
      INCLUDE 'force.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'htmlout.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'seattb.i'
      INCLUDE 'sig.i'
      INCLUDE 'revtbl.i'
c     ------------------------------------------------------------------
      INTEGER N1,N12
      DOUBLE PRECISION ZERO
      PARAMETER (N12 = 12, N1 = 1, ZERO = 0D0)
      INCLUDE 'calc.i'
c     ------------------------------------------------------------------
      DOUBLE PRECISION TSELIM
      LOGICAL F,T
      PARAMETER (TSELIM=0.001D0,T=.true.,F=.false.)
c     ------------------------------------------------------------------
      INTEGER Issap,Irev,Irevsa,Ny,Iag,Iagr,Muladd,i,ncmdl,Lsumm,nstr1,
     &        nstr2,Nfcst,idate,bjmdvc,Length,fh2
      DOUBLE PRECISION Lam,tval
      LOGICAL Lseats,Lgraf
c change variable name cmdl to cmdls, sig to sigs to avoid conflit with
c common block name cmdl in model.cmn and sig in sig.i --Jan. 2021
      CHARACTER sigs*(1),cmdls*(132),ssig*(1),str1*(8),str2*(19)
      DIMENSION ssig(-1:1),idate(2),bjmdvc(6)
c     ------------------------------------------------------------------
      INTEGER getSsf,getSsp2,getSsh
      LOGICAL dpeq,istrue
      EXTERNAL dpeq,istrue,getSsf,getSsp2,getSsh
c     ------------------------------------------------------------------
      DATA ssig / '+','0','-' /
c     ------------------------------------------------------------------
      IF(Issap.lt.2.and.Irev.lt.4)THEN
       IF(Havesa)THEN
        IF(Havesf)THEN
         IF(Lsumm.gt.0)WRITE(Nform,1030)'seatsadj','yes'
        ELSE
         IF(Lsumm.gt.0)WRITE(Nform,1030)'seatsadj','nosf'
        END IF
       ELSE
        IF(Lsumm.gt.0)WRITE(Nform,1030)'seatsadj','no'
       END IF
      END IF
c     ------------------------------------------------------------------
c     If runs for the revisions history or sliding spans analysis are 
c     done, store the required components.
c     ------------------------------------------------------------------
      IF(Issap.gt.0)THEN
       IF((.not.Havesa).or.(.not.Havesf))THEN
        IF(.not.Havesa)THEN
         str1 = 'cannot'
         nstr1 = 6
         str2 = 'signal extraction'
         nstr2 = 17
        ELSE
         str1 = 'does not'
         nstr1 = 8
         str2 = 'seasonal adjustment'
         nstr2 = 19
        END IF
        fh2=0
        IF(.not.Lquiet)fh2=STDERR
        CALL wWritln('Sliding spans analysis cannot be done when'//
     &               ' SEATS '//str1(1:nstr1)//' perform',
     &               fh2,Mt2,T,F)
        IF(Issap.eq.2)THEN
         CALL writln('          a '//str2(1:nstr2)//
     &               ' for a span of data.',
     &               fh2,Mt2,F,T)
         Issap=0-Issap
         RETURN
        ELSE
         CALL writln('          a '//str2(1:nstr2)//'.',
     &               fh2,Mt2,T,F)
         Issap=0
        END IF
       END IF
      END IF
      IF(Issap.eq.2)THEN
       CALL ssrit(Seatsf,Pos1ob,Posfob,2,Series)
       IF(Lrndsa)THEN
        CALL ssrit(Stsarn,Pos1ob,Posfob,3,Series)
       ELSE IF(Iyrt.gt.0)THEN
        CALL ssrit(Setsa2,Pos1ob,Posfob,3,Series)
       ELSE
        CALL ssrit(Seatsa,Pos1ob,Posfob,3,Series)
       END IF
       RETURN
      END IF
c     ------------------------------------------------------------------
      IF(Irev.gt.0)THEN
       IF(((Lrvsa.or.Lrvch).and.((.not.Havesa).or.(.not.Havesf))).or.
     &   ((Lrvtrn.or.Lrvtch).and.(.not.Havetr)).or.
     &   (Lrvsf.and.(.not.Havesf)))THEN
        CALL wWritln('History analysis for estimates derived from '//
     &               'SEATS adjustments',STDERR,Mt2,T,F)
        CALL writln('         cannot be done when SEATS cannot '//
     &              'perform a signal extraction.',STDERR,Mt2,F,T)
        IF((Lrvsa.or.Lrvch).and.((.not.Havesa).or.(.not.Havesf)))THEN
         IF(Lrvsa)Lrvsa=F
         IF(Lrvch)Lrvch=F
        END IF
        IF((Lrvtrn.or.Lrvtch).and.(.not.Havetr))THEN
         IF(Lrvtrn)Lrvtrn=F
         IF(Lrvtch)Lrvtch=F
        END IF
        IF(Lrvsf.and.(.not.Havesf))Lrvsf=F
c     ------------------------------------------------------------------
        IF(.not.(Lrvsf.or.Lrvsa.or.Lrvch.or.Lrvtrn.or.Lrvtch))THEN
         IF(Irevsa.gt.0)Irevsa=-1
         IF(Lrvaic.or.Lrvfct)THEN
          Lseats=F
          IF(Irev.eq.4)RETURN
         ELSE
          IF(Irev.eq.4)THEN
           Irev=0-Irev
           RETURN
          ELSE
           Irev=0
          END IF
         END IF
        END IF
       END IF
      END IF
c     ------------------------------------------------------------------
      IF(Irev.eq.4)THEN
       IF(Lrvsf)THEN
        CALL getrev(Seatsf,Posfob,Muladd,0,Ny,Iag,Iagr)
c     ------------------------------------------------------------------
c     BCM - July 29, 2009
c     Special code to save seasonal factor forecasts for concurrent
c     adjustments.
c     ------------------------------------------------------------------
        IF(Revptr.gt.0.and.Savtab(LRVSSH))THEN
         CALL addate(Rvstrt,Ny,Revptr,idate)
*         WRITE(Fhsfh,1220)'begfct.rev',Revptr,idate
         WRITE(Fhsfh,1220)idate
         IF(Muladd.ne.1)THEN
          DO i=1,Nfcst
           WRITE(Fhsfh,1230)Setfsf(i)/100D0
          END DO
         ELSE
          DO i=1,Nfcst
           WRITE(Fhsfh,1230)Setfsf(i)
          END DO
         END IF
        END IF
       END IF
       IF(Lrvsa.or.Lrvch)THEN
        IF(Lrndsa)THEN
         CALL getrev(Stsarn,Posfob,Muladd,1,Ny,Iag,Iagr)
        ELSE IF(Iyrt.gt.0)THEN
         CALL getrev(Setsa2,Posfob,Muladd,1,Ny,Iag,Iagr)
        ELSE
         CALL getrev(Seatsa,Posfob,Muladd,1,Ny,Iag,Iagr)
        END IF
       END IF
       IF(Lrvtrn.or.Lrvtch)
     &    CALL getrev(Seattr,Posfob,Muladd,2,Ny,Iag,Iagr)
      END IF
c     ------------------------------------------------------------------
      IF(Issap.eq.2.or.Irev.eq.4)RETURN
c-----------------------------------------------------------------------
c     Save squared gain and phase delay, if requested
c-----------------------------------------------------------------------
      IF(Savtab(LSESGS).and.lSAGain(1))THEN
       CALL svfltd(fltW,SAGain,LSESGS,F,1,'SA_Squ_Gain_Symetric')
       IF(Lfatal)RETURN
      END IF
      IF(Savtab(LSESGC).and.lSAGain(2))THEN
       CALL svfltd(fltW,SAGain,LSESGC,F,2,'SA_Squ_Gain_Conc')
       IF(Lfatal)RETURN
      END IF
      IF(Savtab(LSETGS).and.lTreGain(1))THEN
       CALL svfltd(fltW,treGain,LSETGS,F,1,'Trn_Squ_Gain_Symetric')
       IF(Lfatal)RETURN
      END IF
      IF(Savtab(LSETGC).and.lTreGain(2))THEN
       CALL svfltd(fltW,treGain,LSETGC,F,2,'Trn_Squ_Gain_Conc')
       IF(Lfatal)RETURN
      END IF
      IF(Savtab(LSESDC).and.lSATmShf(2))THEN
       CALL svfltd(fltW,SATmShf,LSESDC,F,2,'SA_Time_Shift_Conc')
       IF(Lfatal)RETURN
      END IF
      IF(Savtab(LSETDC).and.ltreTmShf(2))THEN
       CALL svfltd(fltW,treTmShf,LSETDC,F,2,'Trn_Time_Shift_Conc')
       IF(Lfatal)RETURN
      END IF
      IF(Lgraf)THEN
       IF(lSAGain(1))THEN
        CALL svfltd(fltW,SAGain,LSESGS,Lgraf,1,'SA_Squ_Gain_Symetric')
        IF(Lfatal)RETURN
       END IF
       IF(lSAGain(2))THEN
        CALL svfltd(fltW,SAGain,LSESGC,Lgraf,2,'SA_Squ_Gain_Conc')
        IF(Lfatal)RETURN
       END IF
       IF(lTreGain(1))THEN
        CALL svfltd(fltW,treGain,LSETGS,Lgraf,1,'Trn_Squ_Gain_Symetric')
        IF(Lfatal)RETURN
       END IF
       IF(lTreGain(2))THEN
        CALL svfltd(fltW,treGain,LSETGC,Lgraf,2,'Trn_Squ_Gain_Conc')
        IF(Lfatal)RETURN
       END IF
       IF(lSATmShf(2))THEN
        CALL svfltd(fltW,SATmShf,LSESDC,Lgraf,2,'SA_Time_Shift_Conc')
        IF(Lfatal)RETURN
       END IF
       IF(ltreTmShf(2))THEN
        CALL svfltd(fltW,treTmShf,LSETDC,Lgraf,2,'Trn_Time_Shift_Conc')
        IF(Lfatal)RETURN
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Savtab(LSESFS).and.lSaFlt(1))THEN
       CALL svflt(Pos1ob,Posfob,SAFlt,LSESFS,F,1,'SA_Filter_Symetric')
       IF(Lfatal)RETURN
      END IF
      IF(Savtab(LSESFC).and.lSaFlt(2))THEN
       CALL svflt(Pos1ob,Posfob,SAFlt,LSESFC,F,2,'SA_Filter_Conc')
       IF(Lfatal)RETURN
      END IF
      IF(Savtab(LSETFS).and.lTreFlt(1))THEN
       CALL svflt(Pos1ob,Posfob,treFlt,LSETFS,F,1,
     &             'Trn_Filter_Symetric')
       IF(Lfatal)RETURN
      END IF
      IF(Savtab(LSETFC).and.lTreFlt(2))THEN
       CALL svflt(Pos1ob,Posfob,treFlt,LSETFC,F,2,'Trn_Filter_Conc')
       IF(Lfatal)RETURN
      END IF
      IF(Lgraf)THEN
       IF(lSaFlt(1))THEN
        CALL svflt(Pos1ob,Posfob,SAFlt,LSESFS,Lgraf,1,
     &             'SA_Filter_Symetric')
        IF(Lfatal)RETURN
       END IF
       IF(lSaFlt(2))THEN
        CALL svflt(Pos1ob,Posfob,SAFlt,LSESFC,Lgraf,2,
     &             'SA_Filter_Conc')
        IF(Lfatal)RETURN
       END IF
       IF(lTreFlt(1))THEN
        CALL svflt(Pos1ob,Posfob,treFlt,LSETFS,Lgraf,1,
     &             'Trn_Filter_Symetric')
        IF(Lfatal)RETURN
       END IF
       IF(lTreFlt(2))THEN
        CALL svflt(Pos1ob,Posfob,treFlt,LSETFC,Lgraf,2,
     &             'Trn_Filter_Conc')
        IF(Lfatal)RETURN
       END IF
      END IF
c     ------------------------------------------------------------------
      ncmdl=0
      IF(Iprsm.ne.NOTSET.and.Iqrsm.ne.NOTSET.and.Ipssm.ne.NOTSET.and.
     &   Iqssm.ne.NOTSET.and.Idrsm.ne.NOTSET.and.Idssm.ne.NOTSET)THEN
       Ipssm=Ipssm/Ny
       Iqssm=Iqssm/Ny
       CALL mkmdsn(Iprsm,Idrsm,Iqrsm,Ipssm,Idssm,Iqssm,cmdls,ncmdl)
       bjmdvc(1)=Iprsm
       bjmdvc(2)=Idrsm
       bjmdvc(3)=Iqrsm
       bjmdvc(4)=Ipssm
       bjmdvc(5)=Idssm
       bjmdvc(6)=Iqssm
      ELSE
       CALL mkmdsn(P,D,Q,Bp,Bd,Bq,cmdls,ncmdl)
       bjmdvc(1)=P
       bjmdvc(2)=D
       bjmdvc(3)=Q
       bjmdvc(4)=Bp
       bjmdvc(5)=Bd
       bjmdvc(6)=Bq
      END IF
      IF(Lfatal)RETURN
      IF(Svltab(LSLXMD).or.Svltab(LSLSMD))THEN
       Inlgfl=Inlgfl+1
       WRITE(Ng,1000)Inlgfl
       CALL mkTableTag(Ng,'w60','ARIMA Model estimated')
       CALL mkCaption(Ng,'ARIMA Model estimated')
      END IF
      IF(Nxmdl.gt.0)THEN
       IF(Svltab(LSLXMD))WRITE(Ng,1170)'X-13A-S model',X13mdl(1:Nxmdl)
      END IF
      IF(ncmdl.gt.0)THEN
       IF(Svltab(LSLSMD))WRITE(Ng,1170)'SEATS model',cmdls(1:ncmdl)
      END IF
      IF(Svltab(LSLXMD).or.Svltab(LSLSMD))THEN
       CALL writTag(Ng,'</table></div>')
       CALL mkPOneLine(Ng,'@','&nbsp;')
       IF(Nxmdl.gt.0.and.ncmdl.gt.0)THEN
        IF(cmdls(1:ncmdl).ne.X13mdl(1:Nxmdl))THEN
         CALL mkPOneLine(Ng,'center',
     &                   '(SEATS routines changed ARIMA model)')
        END IF
       END IF
      END IF
      IF(Lsumm.gt.0)WRITE(Nform,1030)'seatsmdl',cmdls(1:ncmdl)
      IF(cmdls(1:ncmdl).ne.X13mdl(1:Nxmdl))THEN
       CALL nWritln('Model used for SEATS decomposition is different'//
     &              ' from the model',STDERR,Mt2,T,F)
       CALL writln('      estimated in the regARIMA modeling module'//
     &             ' of '//PRGNAM//'.',STDERR,Mt2,F,T)
      END IF
c     ------------------------------------------------------------------
      IF ((.not.istrue(Svltab,LSLSMD,LSLSSG)).and.Lsumm.eq.0) RETURN
c-----------------------------------------------------------------------
c   BCM 4-11-2006 - add seats model coefficients to savelog output
c-----------------------------------------------------------------------
      IF(Lsumm.gt.0.or.Svltab(LSLSMD))THEN
       IF(Svltab(LSLSMD))THEN
        Inlgfl=Inlgfl+1
        WRITE(Ng,1000)Inlgfl
        CALL mkTableTag(Ng,'w60','@')
        CALL mkCaption(Ng,'SEATS model coefficients')
        CALL writTag(Ng,'<tr>')
        CALL writTagOneLine(Ng,'td','head','&nbsp;')
        CALL mkHeaderCellScope(Ng,0,0,'col','@',
     &                         'Parameter'//Cbr//'Value')
        CALL mkHeaderCellScope(Ng,0,0,'col','@',
     &                         'Standard'//Cbr//'Error')
        CALL mkHeaderCellScope(Ng,0,0,'col','@','t-Statistic')
        CALL writTag(Ng,'</tr>')
       END IF
       IF(Lsumm.gt.0)THEN 
        WRITE(Nform,1180)'seats$nonseasonaldiff',D
        WRITE(Nform,1180)'seats$seasonaldiff',Bd
        WRITE(Nform,1180)'seats$nmodel',P+Q+Bp+Bq
       END IF
       IF(P.gt.0)THEN
        DO i=1,P
         tval=ZERO
         IF(Sep(i).gt.ZERO)tval=Phi(i)/Sep(i)
         IF(Svltab(LSLSMD))THEN
          CALL writTag(Ng,'<tr>')
          WRITE(Ng,1200)'Nonseasonal <abbr title="autoregression">AR'//
     &                  '</abbr>',i
          WRITE(Ng,1210)Phi(i),Sep(i),tval
          CALL writTag(Ng,'</tr>')
         END IF
         IF(Lsumm.gt.0)
     &      WRITE(Nform,1190)'seats$AR$Nonseasonal$01$',i,': ',Phi(i),
     &                       ' ',Sep(i),' ',tval
        END DO
       END IF
       IF(Bp.gt.0)THEN
        DO i=1,Bp
         tval=ZERO
         IF(Sebp(i).gt.ZERO)tval=Bphi(i)/Sebp(i)
         IF(Svltab(LSLSMD))THEN
          CALL writTag(Ng,'<tr>')
          WRITE(Ng,1200)'Seasonal <abbr title="autoregression">AR'//
     &                  '</abbr>',i
          WRITE(Ng,1210)Bphi(i),Sebp(i),tval
          CALL writTag(Ng,'</tr>')
         END IF
         IF(Lsumm.gt.0)
     &      WRITE(Nform,1190)'seats$AR$Seasonal$12$',i*Ny,': ',Bphi(i),
     &                       ' ',Sebp(i),' ',tval
        END DO
       END IF
       IF(Q.gt.0)THEN
        DO i=1,Q
         tval=ZERO
         IF(Seq(i).gt.ZERO)tval=Th(i)/Seq(i)
         IF(Svltab(LSLSMD))THEN
          CALL writTag(Ng,'<tr>')
          WRITE(Ng,1200)'Nonseasonal <abbr title="moving average">'//
     &                  'MA</abbr>',i
          WRITE(Ng,1210)Th(i),Seq(i),tval
          CALL writTag(Ng,'</tr>')
         END IF
         IF(Lsumm.gt.0)
     &      WRITE(Nform,1190)'seats$MA$Nonseasonal$01$',i,': ',Th(i),
     &                       ' ',Seq(i),' ',tval
        END DO
       END IF
       IF(Bq.gt.0)THEN
        DO i=1,Bq
         tval=ZERO
         IF(Sebq(i).gt.ZERO)tval=Bth(i)/Sebq(i)
         IF(Svltab(LSLSMD))THEN
          CALL writTag(Ng,'<tr>')
          WRITE(Ng,1200)'Seasonal <abbr title="moving average">'//
     &                  'MA</abbr>',i
          WRITE(Ng,1210)Bth(i),Sebq(i),tval
          CALL writTag(Ng,'</tr>')
         END IF
         IF(Lsumm.gt.0)
     &      WRITE(Nform,1190)'seats$MA$Seasonal$12$',i*Ny,': ',Bth(i),
     &                       ' ',Sebq(i),' ',tval
        END DO
       END IF
       IF(Svltab(LSLSMD))THEN
        CALL writTag(Ng,'</table></div>')
        CALL mkPOneLine(Ng,'@','&nbsp;')
       END IF
      END IF
c     ------------------------------------------------------------------
      IF(Svltab(LSLSNR).or.Lsumm.gt.0)THEN
       IF(Svltab(LSLSNR))THEN
        Inlgfl=Inlgfl+1
        WRITE(Ng,1000)Inlgfl
        CALL mkTableTag(Ng,'w60','SEATS Normality Statistics')
        CALL mkCaption(Ng,'SEATS Normality Statistics')
       END IF
       IF(.not.dpeq(Testnm,DNOTST))THEN
        IF(Svltab(LSLSNR))
     &     WRITE(Ng,1050)'Normality Test',Testnm,'( Chi-Squared(2) )'
        IF(Lsumm.gt.0)WRITE(Nform,1040)'normalitytest',Testnm,' '
       END IF
       IF(.not.dpeq(Kurt,DNOTST))THEN
        IF(Svltab(LSLSNR))WRITE(Ng,1070)'SEATS Kurtosis',Kurt,Kurtse
        IF(Lsumm.gt.0)WRITE(Nform,1060)'SEATSkurtosis',Kurt,Kurtse
       END IF
       IF(.not.dpeq(Skew,DNOTST))THEN
        IF(Svltab(LSLSNR))WRITE(Ng,1070)'SEATS Skewness',Skew,Skewse
        IF(Lsumm.gt.0)WRITE(Nform,1060)'SEATSskewness',Skew,Skewse
       END IF
       IF(.not.dpeq(Sdres,DNOTST))THEN
        IF(Svltab(LSLSNR))THEN
         WRITE(Ng,1090)'Residual Standard Deviation',Sdres,' '
         WRITE(Ng,1090)'Residual Variance',Sdres*Sdres,' '
        END IF
        IF(Lsumm.gt.0)THEN
         WRITE(Nform,1080)'varsd',Sdres,' '
         WRITE(Nform,1080)'varres',Sdres*Sdres,' '
        END IF
       END IF
       IF(Svltab(LSLSNR))THEN
        CALL writTag(Ng,'</table></div>')
        CALL mkPOneLine(Ng,'@','&nbsp;')
       END IF
      END IF
c     ------------------------------------------------------------------
      IF(Ny.gt.1)THEN
       IF(istrue(Svltab,LSLDW,LSLFRS))THEN
        Inlgfl=Inlgfl+1
        WRITE(Ng,1000)Inlgfl
        CALL mkTableTag(Ng,'w60','SEATS Residual Diagnostics')
        CALL mkCaption(Ng,'SEATS Residual Diagnostics')
       END IF
       IF(.not.dpeq(Dwstat,DNOTST))THEN
        IF(Svltab(LSLDW))WRITE(Ng,1050)'Durbin-Watson',Dwstat,' '
        IF(Lsumm.gt.0)WRITE(Nform,1040)'SEATSdurbanwatson',Dwstat,' '
       END IF
       IF(.not.dpeq(SeasNP,DNOTST))THEN
        IF(Svltab(LSLFRS))THEN
         nstr1=1
         CALL itoc(Ny-1,str1,nstr1)
         WRITE(Ng,1050)'Non-parametric Test for Residual '//
     &                 'Seasonality (Friedman)',SeasNP,
     &                 '( Chi-Squared('//str1(1:(nstr1-1))//' )'
        END IF
        IF(Lsumm.gt.0)WRITE(Nform,1040)'SEATSfriedman',SeasNP,' '
       END IF
       IF(istrue(Svltab,LSLDW,LSLFRS))THEN
        CALL writTag(Ng,'</table></div>')
        CALL mkPOneLine(Ng,'@','&nbsp;')
       END IF
      END IF
c     ------------------------------------------------------------------
c     write message to savelog if seasonal adjustment not performed
c     and exit if signal extraction not done
c     Added by BCM 10-04-05
c     ------------------------------------------------------------------
      IF(.not.Havesa)THEN
       IF(istrue(Svltab,LSLSMD,LSLSSG))
     &    CALL mkPOneLine(Ng,'@',
     &       'SEATS adjustment diagnostics cannot be saved when '//
     &       'SEATS cannot perform a signal extraction.')
       RETURN
      END IF
c     ------------------------------------------------------------------
      IF(Svltab(LSLTSE).or.Lsumm.gt.0)THEN
       IF(Svltab(LSLTSE))THEN
        Inlgfl=Inlgfl+1
        WRITE(Ng,1000)Inlgfl
        CALL mkTableTag(Ng,'w60','SEATS Total Squared Error Diagnostic')
        CALL mkCaption(Ng,'SEATS Total Squared Error Diagnostic')
       END IF
       IF(.not.dpeq(Tsetrn,DNOTST))THEN
        sigs=' '
        IF(Tsetrn.gt.TSELIM)sigs='*'
        IF(Svltab(LSLTSE))
     &     WRITE(Ng,1090)'Trend',Tsetrn,sigs
        IF(Lsumm.gt.0)WRITE(Nform,1080)'tsetrend',Tsetrn,sigs
       END IF
       IF(.not.dpeq(Tsesea,DNOTST))THEN
        sigs=' '
        IF(Tsesea.gt.TSELIM)sigs='*'
        IF(Svltab(LSLTSE))
     &     WRITE(Ng,1090)'Seasonal',Tsesea,sigs
        IF(Lsumm.gt.0)WRITE(Nform,1080)'tseseasonal',Tsesea,sigs
       END IF
       IF(.not.dpeq(Tsetcm,DNOTST))THEN
        sigs=' '
        IF(Tsetcm.gt.TSELIM)sigs='*'
        IF(Svltab(LSLTSE))
     &     WRITE(Ng,1090)'Transistory',Tsetcm,sigs
        IF(Lsumm.gt.0)WRITE(Nform,1080)'tsetransitory',Tsetcm,sigs
       END IF
       IF(.not.dpeq(Tsesad,DNOTST))THEN
        sigs=' '
        IF(Tsesad.gt.TSELIM)sigs='*'
        IF(Svltab(LSLTSE))
     &     WRITE(Ng,1090)'Seasonal adjustment',Tsesad,sigs
        IF(Lsumm.gt.0)WRITE(Nform,1080)'tseseasadj',Tsesad,sigs
       END IF
       IF(Svltab(LSLTSE))THEN
        CALL writTag(Ng,'</table></div>')
        CALL mkPOneLine(Ng,'@','&nbsp;')
       END IF
      END IF
c     ------------------------------------------------------------------
      IF(Svltab(LSLCVR).or.Lsumm.gt.0)THEN
       IF(Svltab(LSLCVR))THEN
        Inlgfl=Inlgfl+1
        WRITE(Ng,1000)Inlgfl
        CALL mkTableTag(Ng,'w60','SEATS Variance Diagnostic')
        CALL mkCaption(Ng,'SEATS Variance Diagnostic')
        CALL writTag(Ng,'<tr>')
        CALL mkTableCell(Ng,'head','&nbsp;')
        CALL mkHeaderCellScope(Ng,0,0,'col','@','Component')
        CALL mkHeaderCellScope(Ng,0,0,'col','@','Estimator')
        CALL mkHeaderCellScope(Ng,0,0,'col','@','Estimate')
        CALL mkHeaderCellScope(Ng,0,0,'col','@','Warning?')
        CALL writTag(Ng,'</tr>')
       END IF
       IF(.not.dpeq(Vartrn(1),DNOTST))THEN
        sigs=' '
        IF(Vartrn(1).le.Vartrn(2))sigs='*'
        IF(Svltab(LSLCVR))
     &     WRITE(Ng,1110)'Trend Variance',(Vartrn(i),i=1,3),sigs
        IF(Lsumm.gt.0)WRITE(Nform,1100)'vartrend',(Vartrn(i),i=1,3),sigs
       END IF
       IF(.not.dpeq(Varsad(1),DNOTST))THEN
        sigs=' '
        IF(Varsad(1).le.Varsad(2))sigs='*'
        IF(Svltab(LSLCVR))
     &     WRITE(Ng,1110)'Sadj Variance',(Varsad(i),i=1,3),sigs
        IF(Lsumm.gt.0)
     &     WRITE(Nform,1100)'varseasadj',(Varsad(i),i=1,3),sigs
       END IF
       IF(.not.dpeq(Varirr(1),DNOTST))THEN
        sigs=' '
        IF(Varirr(1).le.Varirr(2))sigs='*'
        IF(Svltab(LSLCVR))
     &     WRITE(Ng,1110)'Irregular Variance',(Varirr(i),i=1,3),sigs
        IF(Lsumm.gt.0)WRITE(Nform,1100)'varirreg',(Varirr(i),i=1,3),sigs
       END IF
       IF(.not.dpeq(Varsea(1),DNOTST))THEN
        sigs=' '
        IF(Varsea(1).le.Varsea(2))sigs='*'
        IF(Svltab(LSLCVR))
     &     WRITE(Ng,1110)'Seasonal Variance',(Varsea(i),i=1,3),sigs
        IF(Lsumm.gt.0)
     &     WRITE(Nform,1100)'varseasonal',(Varsea(i),i=1,3),sigs
       END IF
       IF(Svltab(LSLCVR))THEN
        CALL writTag(Ng,'</table></div>')
        CALL mkPOneLine(Ng,'@','&nbsp;')
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Svltab(LSLCEE).or.Lsumm.gt.0)THEN
       IF(.not.(dpeq(Ceetrn,DNOTST).or.dpeq(Ceesad,DNOTST)))THEN
        IF(Svltab(LSLCEE))THEN
         Inlgfl=Inlgfl+1
         WRITE(Ng,1000)Inlgfl
         CALL mkTableTag(Ng,'w60','SEATS Concurrent estimation error')
         CALL mkCaption(Ng,'SEATS Concurrent estimation error')
         WRITE(Ng,1050)'Trend',Ceetrn,' '
         WRITE(Ng,1050)'Seasonally Adjusted',Ceesad,' '
         CALL writTag(Ng,'</table></div>')
         CALL mkPOneLine(Ng,'@','&nbsp;')
        END IF
        IF(Lsumm.gt.0)WRITE(Nform,1060)'concesterr',Ceetrn,Ceesad
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Svltab(LSLPRS).or.Lsumm.gt.0)THEN
       IF(Svltab(LSLPRS))THEN
        Inlgfl=Inlgfl+1
        WRITE(Ng,1000)Inlgfl
        CALL mkTableTag(Ng,'w60','SEATS Percent Reduction')
        CALL mkCaption(Ng,'SEATS Percent Reduction')
        CALL writTag(Ng,'<tr>')
        CALL mkTableCell(Ng,'head','&nbsp;')
        CALL mkHeaderCellScope(Ng,0,0,'col','@','Trend')
        CALL mkHeaderCellScope(Ng,0,0,'col','@',
     &       'Seasonally'//Cbr//'Adjusted')
        CALL writTag(Ng,'</tr>')
       END IF
       DO i=1,5
        IF(.not.(dpeq(Prsetr(i),DNOTST).or.dpeq(Prsesa(1),DNOTST)))THEN
         IF(Svltab(LSLPRS))
     &      WRITE(Ng,1130)'After ',i,' Year',Prsetr(i),Prsesa(i)
         IF(Lsumm.gt.0)
     &      WRITE(Nform,1120)'pctreductionyr',i,Prsetr(i),Prsesa(i)
        END IF
       END DO
       IF(Svltab(LSLPRS))THEN
        CALL writTag(Ng,'</table></div>')
        CALL mkPOneLine(Ng,'@','&nbsp;')
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Svltab(LSLAAD).or.Lsumm.gt.0)THEN
       IF(.not.(dpeq(Aadatr,DNOTST).or.dpeq(Aadasa,DNOTST)))THEN
        IF(Svltab(LSLAAD))THEN
         Inlgfl=Inlgfl+1
         WRITE(Ng,1000)Inlgfl
         CALL mkTableTag(Ng,'w60',
     &                   'SEATS Average Value of Absolute Difference')
         CALL mkCaption(Ng,
     &                  'SEATS Average Value of Absolute Difference')
         CALL writTag(Ng,'<tr>')
         CALL mkTableCell(Ng,'head','&nbsp;')
         CALL mkHeaderCellScope(Ng,0,0,'col','@','Trend')
         CALL mkHeaderCellScope(Ng,0,0,'col','@',
     &                          'Seasonally'//Cbr//'Adjusted')
         CALL writTag(Ng,'<tr>')
         WRITE(Ng,1160)'Annual Averages',Aadatr,Aadasa
         CALL writTag(Ng,'</table></div>')
         CALL mkPOneLine(Ng,'@','&nbsp;')
        END IF
        IF(Lsumm.gt.0)WRITE(Nform,1060)'avadaa',Aadatr,Aadasa
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Svltab(LSLSSG).or.Lsumm.gt.0)THEN
       IF(Svltab(LSLSSG))THEN
        IF(Ssghst.ne.NOTSET.and.Ssgcnc.ne.NOTSET.and.Ssgfct.ne.NOTSET)
     &     THEN
         Inlgfl=Inlgfl+1
         WRITE(Ng,1000)Inlgfl
         CALL mkTableTag(Ng,'w60','Significant Seasonal Periods')
         CALL mkCaption(Ng,'Significant Seasonal Periods')
         CALL writTag(Ng,'<tr>')
         CALL mkTableCell(Ng,'head','&nbsp;')
         CALL mkHeaderCellScope(Ng,0,0,'col','@',
     &                    'Number of'//Cbr//'Significant Periods (95%)')
         CALL mkHeaderCellScope(Ng,0,0,'col','@','Significant?')
         CALL writTag(Ng,'<tr>')
        END IF
        IF(Ssghst.ne.NOTSET)
     &     WRITE(Ng,1150)'Historical Estimator',getSsh(),ssig(Ssghst)
        IF(Ssgcnc.ne.NOTSET)
     &     WRITE(Ng,1150)'Concurrent Estimator',getSsp2(),ssig(Ssgcnc)
        IF(Ssgfct.ne.NOTSET)
     &     WRITE(Ng,1150)'Forecast for Next Year',getSsf(),ssig(Ssgfct)
        IF(Ssghst.ne.NOTSET.and.Ssgcnc.ne.NOTSET.and.Ssgfct.ne.NOTSET)
     &     THEN
         CALL writTag(Ng,'</table></div>')
         CALL mkPOneLine(Ng,'@','&nbsp;')
        END IF
       END IF
       IF(Lsumm.gt.0)THEN
        IF(Ssghst.ne.NOTSET)
     &     WRITE(Nform,1140)'sigseashist',getSsh(),ssig(Ssghst)
        IF(Ssgcnc.ne.NOTSET)
     &     WRITE(Nform,1140)'sigseasconc',getSsp2(),ssig(Ssgcnc)
        IF(Ssgfct.ne.NOTSET)
     &     WRITE(Nform,1140)'sigseasfcst',getSsf(),ssig(Ssgfct)
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Lsumm.gt.0)THEN
       IF(dpeq(Lam,ZERO))THEN
        WRITE(Nform,1030)'finmode','multiplicative'
       ELSE
        WRITE(Nform,1030)'finmode','additive'
       END IF
      END IF
c-----------------------------------------------------------------------
c     Save over/under adjustment diagnostics to diagnostic output and/or
c     Log file
c-----------------------------------------------------------------------
      IF(Svltab(LSLOUE).or.Lsumm.gt.0.and.((out.eq.0).or.(out.eq.2)))
     &   CALL svoudg(Svltab(LSLOUE),Lsumm,Ny)
c-----------------------------------------------------------------------
 1000 FORMAT('<div id="lgseat',i6.6,'">')
 1030 FORMAT(a,': ',a)
 1040 FORMAT(a,':',f10.4,3x,a)
 1050 FORMAT('<tr><th scope="row">',a,'</th><td class="center">',f10.4,
     &       ' &nbsp; ',a,' &nbsp;</td></tr>')
 1060 FORMAT(a,':',f10.4,3x,f10.4)
 1070 FORMAT('<tr><th scope="row">',a,'</th><td class="center">',f10.4,
     &       ' ( <abbr title="standard error">SE</abbr> = ',f10.4,
     &       ')</td></tr>')
 1080 FORMAT(a,':',e20.10,3x,a)
 1090 FORMAT('<tr><th scope="row">',a,'</th><td class="center">',e20.10,
     &       ' &nbsp;',a,' &nbsp;</td></tr>')
 1100 FORMAT(a,':',3(f10.4,1x),2x,a)
 1110 FORMAT('<tr><th scope="row">',a,'</th>',
     &       3('<td class="center">',f10.4,'</td>'),
     &       '<td class="center">',a,'</td></tr>')
 1120 FORMAT(a,i1,':',f10.4,3x,f10.4)
 1130 FORMAT('<tr><th scope="row">',a,i1,a,'</th>',
     &       2('<td class="center">',f10.4,'</td>'),'</tr>')
 1140 FORMAT(a,':',i4,3x,a)
 1150 FORMAT('<tr><th scope="row">',a,'</th><td class="center">',i4,
     &       '</td><td class="center">',a,'</td></tr>')
 1160 FORMAT('<tr><th scope="row">',a,'</th>',
     &       2('<td class="center">',f10.4,'</td>'),'</tr>')
 1170 FORMAT('<tr><th scope="row">',a,'</th>','<td class="center">',
     &       a,'</td></tr>')
 1180 FORMAT(a,': ',i3)
 1190 FORMAT(a,i2.2,3(a,e21.14))
 1200 FORMAT('<th scope="row">',a,'(',i2.2,')</th>')
 1210 FORMAT(3('<td>',e21.14,'</td>'))
 1220 FORMAT(2i5)
 1230 FORMAT(1x,e21.14)
c-----------------------------------------------------------------------
      RETURN
      END
