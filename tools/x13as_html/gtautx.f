C     Last change:  BCM  28 Jul 1998    8:28 am
      SUBROUTINE gtautx(Iautom,Autofl,Fctlim,Bcklim,Qlim,Ovrdif,Pck1st,
     &                  Id1st,Outamd,Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Specify settings for X-11-ARIMA's automatic modelling procedure
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'lex.i'
      INCLUDE 'tbllog.i'
      INCLUDE 'svllog.i'
      INCLUDE 'notset.prm'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION ONE,ONEHUN,ZERO
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.,ONE=1D0,ONEHUN=100D0,ZERO=0D0)
c-----------------------------------------------------------------------
      LOGICAL havfil,argok,Inptok,Pck1st,Id1st
      DOUBLE PRECISION Fctlim,Bcklim,Qlim,Ovrdif,dvec
      CHARACTER Autofl*(PFILMD)
      INTEGER Iautom,nelt,itmpvc,ivec,Outamd
      DIMENSION itmpvc(0:1),dvec(1),ivec(1)
c-----------------------------------------------------------------------
      LOGICAL gtarg
      EXTERNAL gtarg
c-----------------------------------------------------------------------
      CHARACTER ARGDIC*71
      INTEGER arglog,argidx,argptr,PARG
      PARAMETER(PARG=11)
      DIMENSION argptr(0:PARG),arglog(2,PARG)
      PARAMETER(ARGDIC='modefileqlimfcstlimbcstlimoverdiffprintmethodout
     &ofsampleidentifysavelog')
c-----------------------------------------------------------------------
      CHARACTER AUTDIC*8
      INTEGER autptr,PAUT
      PARAMETER(PAUT=2)
      DIMENSION autptr(0:PAUT)
      PARAMETER(AUTDIC='bothfcst')
c-----------------------------------------------------------------------
      CHARACTER MTHDIC*9
      INTEGER mthptr,PMTH
      PARAMETER(PMTH=2)
      DIMENSION mthptr(0:PMTH)
      PARAMETER(MTHDIC='bestfirst')
c-----------------------------------------------------------------------
      CHARACTER IDDIC*8
      INTEGER idptr,PID
      PARAMETER(PID=2)
      DIMENSION idptr(0:PID)
      PARAMETER(IDDIC='firstall')
c-----------------------------------------------------------------------
      CHARACTER YSNDIC*5
      INTEGER ysnptr,PYSN
      PARAMETER(PYSN=2)
      DIMENSION ysnptr(0:PYSN)
      PARAMETER(YSNDIC='yesno')
c-----------------------------------------------------------------------
      DATA argptr/1,5,9,13,20,27,35,40,46,57,65,72/
      DATA mthptr/1,5,10/
      DATA autptr/1,5,9/
      DATA idptr/1,6,9/
      DATA ysnptr/1,4,6/
c-----------------------------------------------------------------------
c     Set automatic ARIMA modelling option
c-----------------------------------------------------------------------
      havfil=F
      CALL setint(NOTSET,2*PARG,arglog)
c-----------------------------------------------------------------------
c     Argument get loop
c-----------------------------------------------------------------------
      DO WHILE (T)
       IF(gtarg(ARGDIC,argptr,PARG,argidx,arglog,Inptok))THEN
        IF(Lfatal)RETURN
        GO TO(10,20,30,40,50,60,70,80,90,100,110),argidx
c-----------------------------------------------------------------------
c     set mode for automatic model identification procedure
c     ------------------------------------------------------------------
   10   CALL gtdcvc(LPAREN,T,1,AUTDIC,autptr,PAUT,
     &              'The automatic modelling options are fcst or both.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         Iautom=2
         IF(ivec(1).gt.1)Iautom=1
        END IF
        GO TO 120
c     ------------------------------------------------------------------
   20   CALL gtnmvc(LPAREN,T,1,Autofl,itmpvc,nelt,PFILMD,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)havfil=T
        GO TO 120
c     ------------------------------------------------------------------
c     Qlim argument
c     ------------------------------------------------------------------
   30   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     Error Checking for Qlim
c     ------------------------------------------------------------------
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).lt.ZERO)THEN
          CALL inpter(PERROR,Errpos,
     &                'Ljung-Box Q limit cannot be less than zero.',T)
          Inptok=F
         ELSE IF(dvec(1).gt.ONEHUN)THEN
          CALL inpter(PERROR,Errpos,
     &                'Ljung-Box Q limit cannot be greater than 100.',T)
          Inptok=F
         ELSE
          Qlim=dvec(1)
         END IF
        END IF
        GO TO 120
c     ------------------------------------------------------------------
c     Fcstlim argument
c     ------------------------------------------------------------------
   40   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     Error Checking for Fcstlim
c     ------------------------------------------------------------------
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).lt.ZERO)THEN
          CALL inpter(PERROR,Errpos,
     &               'Forecast error limit cannot be less than zero.',T)
          Inptok=F
         ELSE IF(dvec(1).gt.ONEHUN)THEN
          CALL inpter(PERROR,Errpos,
     &             'Forecast error limit cannot be greater than 100.',T)
          Inptok=F
         ELSE
          Fctlim=dvec(1)
         END IF
        END IF
        GO TO 120
c     ------------------------------------------------------------------
c     Bcstlim argument
c     ------------------------------------------------------------------
   50   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     Error Checking for Bcstlim
c     ------------------------------------------------------------------
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).lt.ZERO)THEN
          CALL inpter(PERROR,Errpos,
     &               'Backcast error limit cannot be less than zero.',T)
          Inptok=F
         ELSE IF(dvec(1).gt.ONEHUN)THEN
          CALL inpter(PERROR,Errpos,
     &             'Backcast error limit cannot be greater than 100.',T)
          Inptok=F
         ELSE
          Bcklim=dvec(1)
         END IF
        END IF
        GO TO 120
c     ------------------------------------------------------------------
c     Overdiff argument
c     ------------------------------------------------------------------
   60   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     Error Checking for Overdiff
c     ------------------------------------------------------------------
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).lt.ZERO)THEN
          CALL inpter(PERROR,Errpos,
     &             'Overdifferencing limit cannot be less than zero.',T)
          Inptok=F
         ELSE IF(dvec(1).gt.ONE)THEN
          CALL inpter(PERROR,Errpos,
     &           'Overdifferencing limit cannot be greater than one.',T)
          Inptok=F
         ELSE
          Ovrdif=dvec(1)
         END IF
        END IF
        GO TO 120
c     ------------------------------------------------------------------
c     Print argument
c-----------------------------------------------------------------------
   70   CALL getprt(LSPAXM,NSPAXM,Inptok)
        GO TO 120
c-----------------------------------------------------------------------
c     method argument
c-----------------------------------------------------------------------
   80   CALL gtdcvc(LPAREN,T,1,MTHDIC,mthptr,PMTH,
     &              'Choices are BEST or FIRST.',ivec,nelt,T,argok,
     &              Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Pck1st=ivec(1).eq.2
        GO TO 120
c-----------------------------------------------------------------------
c     outofsample argument
c-----------------------------------------------------------------------
   90   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for outofsample are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Outamd=ivec(1)
        GO TO 120
c-----------------------------------------------------------------------
c     identify argument
c-----------------------------------------------------------------------
  100   CALL gtdcvc(LPAREN,T,1,IDDIC,idptr,PID,
     &              'Choices are ALL or FIRST.',ivec,nelt,T,argok,
     &              Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Id1st=ivec(1).eq.1
        GO TO 120
c-----------------------------------------------------------------------
c     savelog  argument
c-----------------------------------------------------------------------
  110   CALL getsvl(LSLAXM,NSLAXM,Inptok)
        GO TO 120
c-----------------------------------------------------------------------
       END IF
       IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     If no filename selected for automatic model selection set to
c     x12a.mdl
c     ------------------------------------------------------------------
       IF(.not.havfil)Autofl(1:1)=CNOTST
c     ------------------------------------------------------------------
c     IF automatic model not selected, set to forecast only.
c     ------------------------------------------------------------------
       IF(Iautom.eq.0)Iautom=1
c     ------------------------------------------------------------------
       RETURN
  120  CONTINUE
      END DO
c     ------------------------------------------------------------------
      END
