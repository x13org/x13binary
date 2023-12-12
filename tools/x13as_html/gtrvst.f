C     Last change:  Mar.2021 allow oltwin equals to 0
C     previous change:  BCM  23 Mar 2005    3:07 pm
**==gtrvst.f    processed by SPAG 4.03F  at 10:40 on 20 Oct 1994
      SUBROUTINE gtrvst(Havesp,Sp,Irev,Irevsa,Rfctlg,Nfctlg,Rvstrt,
     &                  Rvend,Otlrev,Otlwin,Lrvsa,Lrvch,Lrvtrn,Lrvaic,
     &                  Lrvfct,Lrvtch,Lrvsf,Lrvarma,Lrvtdrg,Revfix,
     &                  Cnctar,Targsa,Ntarsa,Targtr,Ntartr,Lrfrsh,
     &                  Rvtran,Rvfxrg,Nrvfxr,Rvxotl,Rvdiff,Revfxx,
     &                  Rvtrfc,Indrev,Indrvs,Iagr,Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Read options which control revisions history analysis
c-----------------------------------------------------------------------
c     Variable typing and parameters initialization
c-----------------------------------------------------------------------
      LOGICAL T,F
      INTEGER MO,YR
      PARAMETER(T=.true.,F=.false.,MO=2,YR=1)
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'lex.i'
      INCLUDE 'tbllog.i'
      INCLUDE 'svllog.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'rev.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      LOGICAL Lrvsa,Lrvch,Lrvtrn,Lrvaic,Lrvfct,Lrvtch,Lrvsf,Lrvarma,
     &        Lrvtdrg,Inptok,argok,Rvtran,Havesp,Revfix,Lrfrsh,Cnctar,
     &        Rvxotl,Revfxx,Rvtrfc,lprt2
      INTEGER nelt,i,Irev,Rfctlg,Sp,Rvstrt,Rvend,Nfctlg,Otlwin,Rvdiff,
     &        ivec2,ivec,Targsa,Ntarsa,Targtr,Ntartr,Rvfxrg,Nrvfxr,
     &        Iagr,Irevsa,Indrev,Indrvs
      DIMENSION ivec(1),ivec2(2),Rfctlg(PFCLAG),Rvstrt(2),Rvend(2),
     &          Targsa(PTARGT),Targtr(PTARGT),Indrvs(2)
c-----------------------------------------------------------------------
      LOGICAL gtarg
      EXTERNAL gtarg
c-----------------------------------------------------------------------
      CHARACTER ARGDIC*155
      INTEGER argidx,arglog,argptr,PARG
      PARAMETER(PARG=20)
      DIMENSION argptr(0:PARG),arglog(2,PARG)
      PARAMETER(ARGDIC='estimatessadjlagstrendlagsfstepstartendtablefixm
     &dltransparentrefreshoutlieroutlierwintargetprintsavesavelogfixregx
     &11outlierfixx11regadditivesatransformfcst')
c-----------------------------------------------------------------------
      CHARACTER ESTDIC*47
      INTEGER estidx,estptr,PRVEST
      PARAMETER(PRVEST=9)
      DIMENSION estptr(0:PRVEST),estidx(PRVEST)
      PARAMETER(ESTDIC=
     &   'sadjseasonalsadjchngaicfcsttrendtrendchngarmatd')
c-----------------------------------------------------------------------
      CHARACTER YSNDIC*5
      INTEGER PYSN,ysnptr
      PARAMETER(PYSN=2)
      DIMENSION ysnptr(0:PYSN)
      PARAMETER(YSNDIC='yesno')
c-----------------------------------------------------------------------
      CHARACTER OTLDIC*14
      INTEGER Otlrev,otlptr,POTLRV
      PARAMETER(POTLRV=3)
      DIMENSION otlptr(0:POTLRV)
      PARAMETER(OTLDIC='keepremoveauto')
c-----------------------------------------------------------------------
      CHARACTER TRGDIC*15
      INTEGER trgptr,PTRG
      PARAMETER(PTRG=2)
      DIMENSION trgptr(0:PTRG)
      PARAMETER(TRGDIC='concurrentfinal')
c-----------------------------------------------------------------------
      CHARACTER FXRDIC*20
      INTEGER fxrptr,PFXR
      PARAMETER(PFXR=4)
      DIMENSION fxrptr(0:PFXR),Rvfxrg(PFXR)
      PARAMETER(FXRDIC='tdholidayuseroutlier')
c-----------------------------------------------------------------------
      CHARACTER ADDDIC*17
      INTEGER addptr,PADD
      PARAMETER(PADD=2)
      DIMENSION addptr(0:PADD)
      PARAMETER(ADDDIC='differencepercent')
c-----------------------------------------------------------------------
      DATA argptr/1,10,18,27,32,37,45,51,62,69,76,86,92,97,101,108,114,
     &            124,133,143,156 /
      DATA estptr/1,5,13,21,24,28,33,42,46,48/
      DATA ysnptr/1,4,6/
      DATA otlptr/1,5,11,15/
      DATA trgptr/1,11,16/
      DATA fxrptr/1,3,10,14,21/
      DATA addptr/1,11,18/
c-----------------------------------------------------------------------
      argok=T
      CALL setint(NOTSET,2*PARG,arglog)
      DO WHILE (T)
c-----------------------------------------------------------------------
c     Argument get loop
c-----------------------------------------------------------------------
       IF(gtarg(ARGDIC,argptr,PARG,argidx,arglog,Inptok))THEN
        IF(Lfatal)RETURN
        GO TO(10,20,30,40,50,60,70,80,90,100,110,150,170,180,190,200,
     &        160,120,130,140),argidx
c     ------------------------------------------------------------------
c     estimates variable
c     ------------------------------------------------------------------
   10   CALL gtdcvc(LPAREN,F,PRVEST,ESTDIC,estptr,PRVEST,
     &              'Choices of estimates are sadj, seasonal, '//
     &              'sadjchng, trend, trendchng,',
     &              estidx,nelt,F,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
        IF(argok)THEN
         DO i=1,nelt
          IF(estidx(i).eq.1)THEN
           Lrvsa=T
          ELSE IF(estidx(i).eq.2)THEN
           Lrvsf=T
          ELSE IF(estidx(i).eq.3)THEN
           Lrvch=T
          ELSE IF(estidx(i).eq.4)THEN
           Lrvaic=T
          ELSE IF(estidx(i).eq.5)THEN
           Lrvfct=T
          ELSE IF(estidx(i).eq.6)THEN
           Lrvtrn=T
          ELSE IF(estidx(i).eq.7)THEN
           Lrvtch=T
          ELSE IF(estidx(i).eq.8)THEN
           Lrvarma=T
          ELSE IF(estidx(i).eq.9)THEN
           Lrvtdrg=T
          END IF
         END DO
        ELSE
         CALL writln('        aic, fcst, arma, and td.',STDERR,Mt2,F,T)
        END IF
        GO TO 210
c-----------------------------------------------------------------------
c     sadjlags variable
c-----------------------------------------------------------------------
   20   CALL getivc(LPAREN,T,PTARGT,Targsa,Ntarsa,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.Ntarsa.gt.0)THEN
c     ------------------------------------------------------------------
c     Check individual forecast lags for errors
c     ------------------------------------------------------------------
         DO i=1,Ntarsa
          IF(Targsa(i).le.0)THEN
           CALL inpter(PERROR,Lstpos,
     &              'Entries for sadjlags must be greater than zero.',T)
           Inptok=F
          END IF
         END DO
c     ------------------------------------------------------------------
        END IF
        GO TO 210
c-----------------------------------------------------------------------
c     trendlags lag variable
c-----------------------------------------------------------------------
   30   CALL getivc(LPAREN,T,PTARGT,Targtr,Ntartr,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.Ntartr.gt.0)THEN
c     ------------------------------------------------------------------
c     Check individual forecast lags for errors
c     ------------------------------------------------------------------
         DO i=1,Ntartr
          IF(Targtr(i).le.0)THEN
           CALL inpter(PERROR,Lstpos,
     &             'Entries for trendlags must be greater than zero.',T)
           Inptok=F
          END IF
         END DO
c     ------------------------------------------------------------------
        END IF
        GO TO 210
c-----------------------------------------------------------------------
c     forecast lag variable
c-----------------------------------------------------------------------
   40   CALL getivc(LPAREN,T,PFCLAG,Rfctlg,Nfctlg,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.Nfctlg.gt.0)THEN
c     ------------------------------------------------------------------
c     Check individual forecast lags for errors
c     ------------------------------------------------------------------
         DO i=1,Nfctlg
          IF(Rfctlg(i).le.0)THEN
           CALL inpter(PERROR,Lstpos,
     &                 'Entries for fstep must be greater than zero.',T)
           Inptok=F
          END IF
c     ------------------------------------------------------------------
          IF(Rfctlg(i).gt.PFCST)THEN
           CALL inpter(PERROR,Lstpos,
     &                 'Entries for fstep cannot exceed the maximum '//
     &                 'value specified for maxlead.',T)
           Inptok=F
          END IF
         END DO
c     ------------------------------------------------------------------
        END IF
        GO TO 210
c     ------------------------------------------------------------------
c     Start argument
c     ------------------------------------------------------------------
   50   CALL gtdtvc(Havesp,Sp,LPAREN,F,1,Rvstrt,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        GO TO 210
c     ------------------------------------------------------------------
c     endtable argument
c     ------------------------------------------------------------------
   60   CALL gtdtvc(Havesp,Sp,LPAREN,F,1,Rvend,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        GO TO 210
c     ------------------------------------------------------------------
c     fixmdl argument
c-----------------------------------------------------------------------
   70   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for fixmdl are no or yes.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Revfix=ivec(1).eq.1
        GO TO 210
c     ------------------------------------------------------------------
c     transparent argument
c-----------------------------------------------------------------------
   80   CALL gtdcvc(LPAREN,.true.,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for transparent are no or yes.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Rvtran=ivec(1).eq.1
        GO TO 210
c     ------------------------------------------------------------------
c     refresh argument
c-----------------------------------------------------------------------
   90   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for refresh are no or yes',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lrfrsh=ivec(1).eq.1
        GO TO 210
c     ------------------------------------------------------------------
c     outlier argument
c     ------------------------------------------------------------------
  100   CALL gtdcvc(LPAREN,T,2,OTLDIC,otlptr,POTLRV,
     &        'Available options for outlier are remove, keep or auto.',
     &              ivec2,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok)THEN
         Otlrev=0
         DO i=1,nelt
          Otlrev=Otlrev+(ivec2(i)-1)
         END DO
         IF(nelt.eq.2.and.Otlrev.eq.1)THEN
          CALL inpter(PERROR,Errpos,
     &                'Cannot specify both remove and keep for the '//
     &                'outlier argument.',T)
          Inptok=F
         END IF
        END IF
        GO TO 210
c-----------------------------------------------------------------------
c     outlierwin variable
c-----------------------------------------------------------------------
  110   CALL getivc(LPAREN,T,1,ivec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        Otlwin=ivec(1)
        IF(argok.and.Otlwin.lt.0)THEN
         CALL inpter(PERROR,Errpos,
     &               'Value of outlierwin must be an integer greater'//
     &               ' than or equal to zero.',T)
         Inptok=F
        END IF
        GO TO 210
c     ------------------------------------------------------------------
c     fixx11reg argument
c-----------------------------------------------------------------------
  120   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for fixx11reg are no or yes.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Revfxx=ivec(1).eq.1
        GO TO 210
c     ------------------------------------------------------------------
c     additive seasonal adjustment argument
c-----------------------------------------------------------------------
  130   CALL gtdcvc(LPAREN,T,1,ADDDIC,addptr,PADD,
     &    'Available options for additivesa are difference or percent.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Rvdiff=ivec(1)
        GO TO 210
c     ------------------------------------------------------------------
c     transformfcst argument
c-----------------------------------------------------------------------
  140   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &             'Available options for transformfcst are no or yes.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Rvtrfc=ivec(1).eq.1
        GO TO 210
c     ------------------------------------------------------------------
c     target argument
c-----------------------------------------------------------------------
  150   CALL gtdcvc(LPAREN,T,1,TRGDIC,trgptr,PTRG,
     &           'Available options for target are concurrent or final',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Cnctar=ivec(1).eq.1
        GO TO 210
c     ------------------------------------------------------------------
c     x11outlier argument
c-----------------------------------------------------------------------
  160   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for x11outlier are no or yes.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Rvxotl=ivec(1).eq.1
        GO TO 210
c     ------------------------------------------------------------------
c     Print argument
c     ------------------------------------------------------------------
  170   CALL getprt(LSPREV,NSPREV,Inptok)
        GO TO 210
c     ------------------------------------------------------------------
c     Save argument
c     ------------------------------------------------------------------
  180   CALL getsav(LSPREV,NSPREV,Inptok)
        GO TO 210
c     ------------------------------------------------------------------
c     Savelog argument
c     ------------------------------------------------------------------
  190   CALL getsvl(LSLREV,NSLREV,Inptok)
        GO TO 210
c     ------------------------------------------------------------------
c     regression parameter fixing argument
c-----------------------------------------------------------------------
  200   CALL gtdcvc(LPAREN,T,PFXR,FXRDIC,fxrptr,PFXR,
     &         'Available options for fixreg are td, holiday, user or'//
     &         ' outlier.',Rvfxrg,Nrvfxr,T,argok,Inptok)
        IF(Lfatal)RETURN
        GO TO 210
       END IF
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Check arguments set by user
c-----------------------------------------------------------------------
       IF(Irev.eq.0)Irev=1
c-----------------------------------------------------------------------
c     If no estimates set, only do seasonally adjusted series.
c     comment out for build59 -9/24/2021 if print=none, no table printed
c-----------------------------------------------------------------------
       IF(.not.Lrvsa.and.Ntarsa.gt.0)Lrvsa=T
       IF(.not.Lrvtrn.and.Ntartr.gt.0)Lrvtrn=T
c       IF((.not.Lrvsa).and.(.not.Lrvsf).and.(.not.Lrvch).and.
c     &    (.not.Lrvtrn).and.(.not.Lrvaic).and.(.not.Lrvfct).and.
c     &    (.not.Lrvtch).and.(.not.Lrvarma).and.(.not.Lrvtdrg))Lrvsa=T
       IF(Lrvsa.or.Lrvsf.or.Lrvch.or.Lrvtrn.or.Lrvtch)Irevsa=1
c-----------------------------------------------------------------------
       IF(Otlwin.eq.NOTSET)Otlwin=Sp
c-----------------------------------------------------------------------
       IF(Iagr.gt.0)THEN
        IF(Indrev.eq.NOTSET)THEN
         IF(Lrvsa)THEN
          Indrev=1
         ELSE
          Indrev=0
         END IF
        END IF
        lprt2=F
        IF(Indrev.eq.1)THEN
c-----------------------------------------------------------------------
c    check to see if seasonal adjustment is specified - if not, then
c    print out message and turn off revision history of indirect
c    seasonal adjustment
c-----------------------------------------------------------------------
         IF(.not.Lrvsa)THEN
          Indrev=0
          CALL wWritln('Need to specify revisons history for  '//
     &                 'seasonal adjustments in all ',STDERR,Mt2,T,F)
          CALL writln('          components of a composite '//
     &                'adjustment to get a revisions history of the',
     &                STDERR,Mt2,F,F)
          CALL writln('          indirect seasonally adjusted series.',
     &                STDERR,Mt2,F,T)
          lprt2=T
c-----------------------------------------------------------------------
c    if start date specified, check to see if start date of indirect
c    revisions analysis is set.  If not, set this date to be the same as
c    the revisions starting date.  
c-----------------------------------------------------------------------
         ELSE IF(Rvstrt(YR).gt.0)THEN
          IF(Indrvs(YR).eq.0)THEN
           Indrvs(YR)=Rvstrt(YR)
           Indrvs(MO)=Rvstrt(MO)
c-----------------------------------------------------------------------
c    If the date has been set previously, check to see if the starting
c    date for this component matches the date for the indirect revisions
c    history.  If it does not, print out message and turn off revision
c    history of indirect seasonal adjustment.
c-----------------------------------------------------------------------
          ELSE IF(.not.(Indrvs(YR).eq.Rvstrt(YR).and.
     &                  Indrvs(MO).eq.Rvstrt(MO)))THEN
           Indrev=0
           CALL wWritln('Starting date of revisons history analysis '//
     &                  'must be the same for all',STDERR,Mt2,T,F)
           CALL writln('         components of a composite '//
     &                 'adjustment to get a revisions history of the',
     &                 STDERR,Mt2,F,F)
           CALL writln('         indirect seasonally adjusted series.',
     &                 STDERR,Mt2,F,T)
           lprt2=T
          END IF
c-----------------------------------------------------------------------
c    if start date is not specified, check to see if start date of
c    indirect revisions analysis is set.  If so, print out message and
c    turn off revision history of indirect seasonal adjustment.
c-----------------------------------------------------------------------
         ELSE IF(Rvstrt(YR).eq.0.and.Indrev.gt.0)THEN
          Indrev=0
          CALL wWritln('Starting date of revisons history analysis '//
     &                 'must be specified for all',STDERR,Mt2,T,F)
          CALL writln('         components of a composite '//
     &                'adjustment to get a revisions history of the',
     &                STDERR,Mt2,F,F)
          CALL writln('         indirect seasonally adjusted series.',
     &                STDERR,Mt2,F,T)
          lprt2=T
         END IF
        END IF
        IF(lprt2)THEN
         CALL writln('         Edit all input specification files '//
     &               'to correct this and rerun the',
     &               STDERR,Mt2,T,F)
         CALL writln('         metafile.',
     &               STDERR,Mt2,F,T)
        END IF
       END IF
c-----------------------------------------------------------------------
       Inptok=Inptok.and.argok
       RETURN
  210  CONTINUE
      END DO
c     ------------------------------------------------------------------
      END
