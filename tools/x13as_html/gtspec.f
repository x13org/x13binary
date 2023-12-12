C     Last change:  BCM  5 Feb 2008    10:05 am
      SUBROUTINE gtspec(Sp,Begspn,Endspn,Havesp,Bgspec,Spcdff,Spctyp,
     &                  Spcsrs,Mxarsp,Spclim,Peakwd,Lfqalt,Axsame,
     &                  Svallf,Ldecbl,Plocal,Spdfor,Lstdff,Lprsfq,
     &                  Ltk120,Llogqs,Lqchk,Lrbstsa,Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Get options related to the spectrum.
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'stdio.i'
      INCLUDE 'lex.i'
      INCLUDE 'tbllog.i'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'svllog.i'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      LOGICAL F,T
      INTEGER YR,MO,MINNIN,NINE
      DOUBLE PRECISION ZERO
      PARAMETER(T=.true.,F=.false.,YR=1,MO=2,MINNIN=-9,NINE=9,ZERO=0D0)
C-----------------------------------------------------------------------
      DOUBLE PRECISION Spclim,dvec,Plocal
      LOGICAL argok,Havesp,Spcdff,Lfqalt,Axsame,Svallf,Ldecbl,Lprsfq,
     &        Inptok,Lstdff,locok,Llogqs,Ltk120,Lqchk,Lrbstsa
      INTEGER Sp,Begspn,Endspn,Bgspec,nspec,Kdec,Spctyp,Spcsrs,Mxarsp,
     &        Peakwd,nelt,ivec,Spdfor
      DIMENSION Begspn(2),Endspn(2),Bgspec(2),dvec(1),ivec(1)
c-----------------------------------------------------------------------
      LOGICAL gtarg,isdate
      EXTERNAL gtarg,isdate
c-----------------------------------------------------------------------
c     This dictionary was made with this command
c  ../../dictionary/strary < ../../dictionary/series.dic
c-----------------------------------------------------------------------
      CHARACTER ARGDIC*153
      INTEGER argidx,argptr,PARG,arglog
      PARAMETER(PARG=21)
      DIMENSION argptr(0:PARG),arglog(2,PARG)
      PARAMETER(ARGDIC='startdifferencetypeseriessiglevelpeakwidthmaxara
     &ltfreqaxisprintsavesavelogsaveallfreqdecibellocalpeakstartdiffshow
     &seasonalfreqtukey120logqsqcheckrobustsa')
c-----------------------------------------------------------------------
c     type of compositing data dictionary
c-----------------------------------------------------------------------
      CHARACTER YSNDIC*5
      INTEGER ysnptr,PYSN
      PARAMETER(PYSN=2)
      DIMENSION ysnptr(0:PYSN)
      PARAMETER(YSNDIC='yesno')
c     ------------------------------------------------------------------
      CHARACTER DFFDIC*10
      INTEGER dffptr,PDFF
      PARAMETER(PDFF=3)
      DIMENSION dffptr(0:PDFF)
      PARAMETER(DFFDIC='yesfirstno')
c     ------------------------------------------------------------------
      CHARACTER STPDIC*17
      INTEGER stpptr,PSTP
      PARAMETER(PSTP=2)
      DIMENSION stpptr(0:PSTP)
      PARAMETER(STPDIC='arspecperiodogram')
c-----------------------------------------------------------------------
c     series used in spectrum data dictionary
c-----------------------------------------------------------------------
      CHARACTER SPSDIC*57
      INTEGER spsptr,PSPS
      PARAMETER(PSPS=8)
      DIMENSION spsptr(0:PSPS)
      PARAMETER(SPSDIC='originaloutlieradjoriginaladjoriginalmodoriginal
     &a1a19b1e1')
c     ------------------------------------------------------------------
c     data dictionary for spectral axis
c     ------------------------------------------------------------------
      CHARACTER AXSDIC*17
      INTEGER axsptr,PAXS
      PARAMETER(PAXS=3)
      DIMENSION axsptr(0:PAXS)
      PARAMETER(AXSDIC='samedifferentdiff')
c     ------------------------------------------------------------------
      DATA argptr/1,6,16,20,26,34,43,48,55,59,64,68,75,86,93,102,111,
     &            127,135,140,146,154/
      DATA ysnptr/1,4,6/
      DATA dffptr/1,4,9,11/
      DATA stpptr/1,7,18/
      DATA spsptr/1,9,27,38,49,51,54,56,58/
      DATA axsptr/1,5,14,18/
c-----------------------------------------------------------------------
c     Assume the input is OK and we don't have any of the arguments
c-----------------------------------------------------------------------
      locok=T
      CALL setint(NOTSET,2*PARG,arglog)
c-----------------------------------------------------------------------
c     Argument get loop
c-----------------------------------------------------------------------
      DO WHILE (T)
       IF(gtarg(ARGDIC,argptr,PARG,argidx,arglog,Inptok))THEN
        IF(Lfatal)RETURN
        GO TO(10,20,30,40,60,70,50,90,130,100,110,120,140,150,160,170,
     &        80,180,175,190,200),argidx
c-----------------------------------------------------------------------
c     start argument
c-----------------------------------------------------------------------
   10   CALL gtdtvc(Havesp,Sp,LPAREN,F,1,Bgspec,nelt,argok,locok)
        IF(Lfatal)RETURN
        GO TO 210
c-----------------------------------------------------------------------
c     difference argument
c-----------------------------------------------------------------------
   20   CALL gtdcvc(LPAREN,T,1,DFFDIC,dffptr,PDFF,
     &         'Available options for difference are yes, no or first.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         Spcdff=ivec(1).lt.3
         IF(ivec(1).eq.2)THEN
          Spdfor=1
         ELSE IF(ivec(1).eq.3)THEN
          Spdfor=0
         END IF
        END IF
        GO TO 210
c-----------------------------------------------------------------------
c     type argument
c-----------------------------------------------------------------------
   30   CALL gtdcvc(LPAREN,T,1,STPDIC,stpptr,PSTP,
     &  'Available options for spectrumtype are periodogram or arspec.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Spctyp=ivec(1)-1
        GO TO 210
c-----------------------------------------------------------------------
c     series argument
c-----------------------------------------------------------------------
   40   CALL gtdcvc(LPAREN,T,1,SPSDIC,spsptr,PSPS,
     &          'Improper entry found for the spectrumseries argument.',
     &              ivec,nelt,F,argok,Inptok)
        IF(Lfatal)RETURN
        IF(nelt.eq.0)THEN
         CALL writln('        Valid entries for spectrumseries are '//
     &               'a1, a19, b1, e1, original,',STDERR,Mt2,F,F)
         CALL writln('        outlieradjoriginal, adjoriginal, or '//
     &               'modoriginal.',STDERR,Mt2,F,T)
        ELSE IF(argok.and.nelt.gt.0)THEN
         Spcsrs=ivec(1)-1
         IF(Spcsrs.gt.3)Spcsrs=Spcsrs-4
        END IF
        GO TO 210
c-----------------------------------------------------------------------
c     maxar argument
c-----------------------------------------------------------------------
   50   CALL getivc(LPAREN,T,1,ivec,nelt,argok,locok)
        IF(Lfatal)RETURN
        IF(argok)THEN
         IF(ivec(1).lt.1.or.ivec(1).gt.30)THEN
          CALL inpter(PERROR,Errpos,
     &                'Max order for AR spectrum must be between '//
     &                '1 and 30, inclusive',T)
          locok=F
         ELSE
          Mxarsp=ivec(1)
         END IF
        END IF
        GO TO 210
c-----------------------------------------------------------------------
c     siglevel argument
c-----------------------------------------------------------------------
   60   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok)THEN
         IF(dvec(1).le.ZERO)THEN
          CALL inpter(PERROR,Errpos,
     &       'Visual signifcance criteria must be greater than zero.',T)
          locok=F
         ELSE
          Spclim=dvec(1)
         END IF
        END IF
        GO TO 210
c-----------------------------------------------------------------------
c     peakwidth argument
c-----------------------------------------------------------------------
   70   CALL getivc(LPAREN,T,1,ivec,nelt,argok,locok)
        IF(Lfatal)RETURN
        IF(argok)THEN
         IF(ivec(1).lt.1.or.ivec(1).gt.4)THEN
          CALL inpter(PERROR,Errpos,
     &       'Spectral peak width must be between 1 and 4, inclusive',T)
          locok=F
         ELSE
          Peakwd=ivec(1)
         END IF
        END IF
        GO TO 210
c-----------------------------------------------------------------------
c     showseasonalfreq argument
c-----------------------------------------------------------------------
   80   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &          'Available options for showseasonalfreq are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lprsfq=ivec(1).eq.1
        GO TO 210
c-----------------------------------------------------------------------
c     altfreq argument
c-----------------------------------------------------------------------
   90   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for altfreq are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lfqalt=ivec(1).eq.1
        GO TO 210
c-----------------------------------------------------------------------
c     Print argument
c-----------------------------------------------------------------------
  100   CALL getprt(LSPSPC,NSPSPC,locok)
        GO TO 210
c-----------------------------------------------------------------------
c     Save argument
c-----------------------------------------------------------------------
  110   CALL getsav(LSPSPC,NSPSPC,locok)
        GO TO 210
c-----------------------------------------------------------------------
c     savelog  argument
c-----------------------------------------------------------------------
  120   CALL getsvl(LSLSPC,NSLSPC,Inptok)
        GO TO 210
c-----------------------------------------------------------------------
c     axis argument
c-----------------------------------------------------------------------
  130   CALL gtdcvc(LPAREN,T,1,AXSDIC,axsptr,PAXS,
     &              'Available options for axis are same, diff or '//
     &              'difference.',ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Axsame=ivec(1).eq.1
        GO TO 210
c-----------------------------------------------------------------------
c     saveallfreq argument
c-----------------------------------------------------------------------
  140   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for saveallfreq are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Svallf=ivec(1).eq.1
        GO TO 210
c-----------------------------------------------------------------------
c     decibel argument
c-----------------------------------------------------------------------
  150   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for decibel are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Ldecbl=ivec(1).eq.1
        GO TO 210
c-----------------------------------------------------------------------
c     localpeak argument
c-----------------------------------------------------------------------
  160   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok)THEN
         IF(dvec(1).lt.ZERO)THEN
          CALL inpter(PERROR,Errpos,
     &             'Localpeak must be greater than or equal to zero.',T)
          locok=F
         ELSE
          Plocal=dvec(1)
         END IF
        END IF
        GO TO 210
c-----------------------------------------------------------------------
c     startdiff argument
c-----------------------------------------------------------------------
  170   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for startdiff are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lstdff=ivec(1).eq.1
        GO TO 210
c-----------------------------------------------------------------------
c     logqs argument
c-----------------------------------------------------------------------
  175   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for logqs are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Llogqs=ivec(1).eq.1
        GO TO 210
c-----------------------------------------------------------------------
c     tukey120 argument
c-----------------------------------------------------------------------
  180   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for tukey120 are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Ltk120=ivec(1).eq.1
        GO TO 210
c-----------------------------------------------------------------------
c     qcheck argument
c-----------------------------------------------------------------------
  190   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for qcheck are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lqchk=ivec(1).eq.1
        GO TO 210
c-----------------------------------------------------------------------
c     robustsa argument
c-----------------------------------------------------------------------
  200   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for robustsa are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lrbstsa=ivec(1).eq.1
        GO TO 210
       END IF
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     If beginning date of spectral plot is undefined, set equal to 
c     either the beginning date of series, or the date eight years from
c     the end of the span.
c-----------------------------------------------------------------------
       IF(Bgspec(YR).eq.NOTSET)THEN
        CALL addate(Endspn,Sp,-95,Bgspec)
        CALL dfdate(Bgspec,Begspn,Sp,nspec)
        IF(nspec.lt.0)CALL cpyint(Begspn,2,1,Bgspec)
       ELSE
c-----------------------------------------------------------------------
c     Else, check that the span is within the series
c-----------------------------------------------------------------------
        IF(.not.isdate(Bgspec,Sp))THEN
         CALL inpter(PERRNP,Pos,'Spectrum starting date not valid',T)
         Havesp=F
         locok=F
        ELSE
         CALL dfdate(Bgspec,Begspn,Sp,nspec)
         IF(nspec.lt.0)THEN
          CALL inpter(PERRNP,Errpos,
     &                'Starting date of spectral plots is before '//
     &                'start of series.',T)
          locok=F
         END IF
         CALL dfdate(Bgspec,Endspn,Sp,nspec)
         IF(nspec.ge.0)THEN
          CALL inpter(PERRNP,Errpos,
     &      'Starting date of spectral plots is after end of series.',T)
          locok=F
         END IF
        END IF
       END IF
c     ------------------------------------------------------------------
c     Set Peakwd according to the seasonal period, if it is not already
c     set (BCM May 2007)
c     ------------------------------------------------------------------
       IF(Peakwd.eq.NOTSET)Peakwd=1
       IF(Mxarsp.eq.NOTSET)Mxarsp=30*Sp/12
c        IF(Sp.eq.4)Peakwd=3
c       END IF
c     ------------------------------------------------------------------
       Inptok=Inptok.and.locok
       RETURN
  210  CONTINUE
      END DO
c     ------------------------------------------------------------------
      END
