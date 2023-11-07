C     Last change:  SRD  2 Feb 2006 1:33 pm
      SUBROUTINE gtauto(Lautom,Lautod,Ub1lim,Ub2lim,Cancel,Maxord,
     &                  Diffam,Exdiff,Lbalmd,Hrinit,Tsig,Pcr,
     &                  Fct,Predcv,Laccdf,Lotmod,Ubfin,Frstar,Lchkmu,
     &                  Lmixmd,Lrejfc,Fctlm2,Lsovdf,Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Specify settings for the automatic modelling procedure
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'lex.i'
      INCLUDE 'tbllog.i'
      INCLUDE 'svllog.i'
      INCLUDE 'notset.prm'
      INCLUDE 'error.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'mdltbl.i'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION ONE,ONEHUN,ZERO
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.,ONE=1D0,ONEHUN=100D0,ZERO=0D0)
c-----------------------------------------------------------------------
      LOGICAL argok,Inptok,Lautom,Lautod,Lbalmd,Hrinit,Lrejfc,
     &        Laccdf,Lotmod,hvdiff,Lchkmu,Lmixmd,Lsovdf
      DOUBLE PRECISION Predcv,Ub1lim,Ub2lim,Ubfin,Cancel,dvec,Tsig,Fct,
     &                 Pcr,Fctlm2
      INTEGER nelt,ivec,omax,Maxord,adif,Diffam,Frstar,i,Exdiff,fh2
      DIMENSION dvec(1),ivec(1),omax(2),Maxord(2),adif(2),Diffam(2)
c-----------------------------------------------------------------------
      LOGICAL gtarg,istrue
      EXTERNAL gtarg,istrue
c-----------------------------------------------------------------------
      CHARACTER ARGDIC*194
      INTEGER argidx,argptr,PARG,arglog
      PARAMETER(PARG=24)
      DIMENSION argptr(0:PARG),arglog(2,PARG)
      PARAMETER(ARGDIC='maxdiffub1ub2cancelmaxorderdiffprintsavelogbalan
     &cedexactdiffhrinitialarmalimitpercentrsereducecvljungboxlimitaccep
     &tdefaultnoautooutlierurfinalfirstarcheckmumixedrejectfcstfcstlimse
     &asonaloverdiff')
c-----------------------------------------------------------------------
      CHARACTER NOTDIC*9
      INTEGER notptr,PNOT
      PARAMETER(PNOT=2)
      DIMENSION notptr(0:PNOT)
      PARAMETER(NOTDIC='sametramo')
c-----------------------------------------------------------------------
c     data dictionary of yes/no choice
c-----------------------------------------------------------------------
      CHARACTER YSNDIC*5
      INTEGER ysnptr,PYSN
      PARAMETER(PYSN=2)
      DIMENSION ysnptr(0:PYSN)
      PARAMETER(YSNDIC='yesno')
c-----------------------------------------------------------------------
c     data dictionary of exactdiff
c-----------------------------------------------------------------------
      CHARACTER EXDDIC*10
      INTEGER exdptr,PEXD
      PARAMETER(PEXD=3)
      DIMENSION exdptr(0:PEXD)
      PARAMETER(EXDDIC='noyesfirst')
c-----------------------------------------------------------------------
      DATA argptr/1,8,11,14,20,28,32,37,44,52,61,70,79,89,97,110,123,
     &            136,143,150,157,162,172,179,195/
      DATA notptr/1,5,10/
      DATA ysnptr/1,4,6/
      DATA exdptr/1,3,6,11/
c-----------------------------------------------------------------------
c     Set automatic ARIMA modelling option
c-----------------------------------------------------------------------
      CALL setint(NOTSET,2*PARG,arglog)
      CALL setint(NOTSET,2,omax)
      CALL setint(NOTSET,2,adif)
      hvdiff=F
      fh2=0
      IF(.not.Lquiet)fh2=STDERR
c-----------------------------------------------------------------------
c     Argument get loop
c-----------------------------------------------------------------------
      DO WHILE (T)
       IF(gtarg(ARGDIC,argptr,PARG,argidx,arglog,Inptok))THEN
        IF(Lfatal)RETURN
        GO TO(10,20,30,40,60,70,80,90,100,110,120,130,140,150,160,170,
     &        180,190,50,200,210,220,230,235),argidx
c-----------------------------------------------------------------------
c     Specify if only automatic modelling will be done.
c     ------------------------------------------------------------------
   10   CALL getivc(LPAREN,T,2,adif,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(hvdiff)THEN
         CALL nWritln('Arguments diff and maxdiff are both specified;',
     &                fh2,Mt2,T,F)
         CALL writln('       only maxdiff will be used.',fh2,Mt2,F,T)
        END IF
        IF(nelt.eq.1)THEN
         CALL inpter(PERROR,Errpos,'Two values are needed.',T)
         Inptok=F
        ELSE IF(nelt.gt.0)THEN
         IF(adif(1).gt.2)THEN
          CALL inpter(PERROR,Errpos,
     &                'Maximum order of regular differencing must '//
     &                'be less than or equal to 2.',T)
          Inptok=F
         END IF
c-----------------------------------------------------------------------
         IF(adif(2).GT.1)THEN
          CALL inpter(PERROR,Errpos,
     &                'Maximum order of seasonal differencing must '//
     &                'be less than or equal to 1.',T)
          Inptok=F
         END IF
c-----------------------------------------------------------------------
         IF(adif(1).lt.0.or.adif(2).lt.0)THEN
          CALL inpter(PERROR,Errpos,
     &                'Maximum order of differencing specified must '//
     &                'be greater than zero.',T)
          Inptok=F
         END IF
c-----------------------------------------------------------------------
         IF(Inptok)THEN
          CALL cpyint(adif,2,1,Diffam)
          hvdiff=T
          Lautod=T
         END IF
        END IF
        GO TO 240
c     ------------------------------------------------------------------
c     Ub1lim argument
c     ------------------------------------------------------------------
   20   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     Error Checking for Ub1lim
c     ------------------------------------------------------------------
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).le.ONE)THEN
          CALL inpter(PERROR,Errpos,
     &                'Initial unit root limit must be greater than '//
     &                'one.',T)
          Inptok=F
         ELSE
          Ub1lim=dvec(1)
         END IF
        END IF
        GO TO 240
c     ------------------------------------------------------------------
c     Ub2lim argument
c     ------------------------------------------------------------------
   30   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     Error Checking for Ub2lim
c     ------------------------------------------------------------------
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).le.ZERO)THEN
          CALL inpter(PERROR,Errpos,
     &             'Final unit root limit must be greater than zero.',T)
          Inptok=F
         ELSE IF(dvec(1).ge.ONE)THEN
          CALL inpter(PERROR,Errpos,
     &                'Final unit root limit must be less than one.',T)
          Inptok=F
         ELSE
          Ub2lim=dvec(1)
         END IF
        END IF
        GO TO 240
c     ------------------------------------------------------------------
c     Cancel argument
c     ------------------------------------------------------------------
   40   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     Error Checking for Cancel
c     ------------------------------------------------------------------
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).le.ZERO)THEN
          CALL inpter(PERROR,Errpos,
     &                'Cancelation limit must be greater than zero.',T)
          Inptok=F
         ELSE
          Cancel=dvec(1)
         END IF
        END IF
        GO TO 240
c-----------------------------------------------------------------------
c     firstar argument
c-----------------------------------------------------------------------
   50   CALL getivc(LPAREN,T,1,ivec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         IF(.not.(ivec(1).eq.2.or.ivec(1).eq.3.or.ivec(1).eq.4))THEN
          CALL inpter(PERROR,Errpos,
     &                'Value of firstar must be 2, 3 or 4.',T)
          Inptok=F
         ELSE
          Frstar=ivec(1)
         END IF
        END IF
        GO TO 240
c-----------------------------------------------------------------------
c     maxorder argument
c-----------------------------------------------------------------------
   60   CALL getivc(LPAREN,F,2,omax,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(nelt.eq.1)THEN
         CALL inpter(PERROR,Errpos,
     &      'Two values are needed (or use a comma as place holder).',T)
         Inptok=F
        ELSE IF(nelt.gt.0)THEN
         IF(omax(1).eq.NOTSET)omax(1)=2
         IF(omax(2).eq.NOTSET)omax(2)=1
*         IF(omax(1).le.0.or.omax(2).le.0)THEN
         IF(omax(1).lt.0.or.omax(2).lt.0)THEN
          CALL inpter(PERROR,Errpos,
     &                'AR and MA orders must be greater than or '//
     &                'equal to zero.',T)
*     &an zero.')
          Inptok=F
c-----------------------------------------------------------------------
         ELSE
          IF(omax(1).gt.4)THEN
           CALL inpter(PERROR,Errpos,
     &              'Regular orders must be less than or equal to 4.',T)
           Inptok=F
          END IF
c-----------------------------------------------------------------------
          IF(omax(2).gt.2)THEN
           CALL inpter(PERROR,Errpos,
     &             'Seasonal orders must be less than or equal to 2.',T)
           Inptok=F
          END IF
c-----------------------------------------------------------------------
          IF(Inptok)CALL cpyint(omax,2,1,Maxord)
         END IF
        END IF
        GO TO 240
c-----------------------------------------------------------------------
c     diff argument
c-----------------------------------------------------------------------
   70   CALL getivc(LPAREN,T,2,adif,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(hvdiff)THEN
         CALL nWritln('Arguments diff and maxdiff are both specified;',
     &                fh2,Mt2,T,F)
         CALL writln('       only maxdiff will be used.',fh2,Mt2,F,T)
         GO TO 240
        END IF
        IF(nelt.eq.1)THEN
         CALL inpter(PERROR,Errpos,'Two values are needed.',T)
         Inptok=F
        ELSE IF(nelt.gt.0)THEN
         IF(adif(1).gt.2)THEN
          CALL inpter(PERROR,Errpos,
     &                'Order of regular differencing must be less '//
     &                'than or equal to 2.',T)
          Inptok=F
         END IF
c-----------------------------------------------------------------------
         IF(adif(2).GT.1)THEN
          CALL inpter(PERROR,Errpos,
     &                'Order of seasonal differencing must be less '//
     &                'than or equal to 1.',T)
          Inptok=F
         END IF
c-----------------------------------------------------------------------
         IF(adif(1).lt.0.or.adif(2).lt.0)THEN
          CALL inpter(PERROR,Errpos,
     &                'Order of differencing specified must be '//
     &                'greater than zero.',T)
          Inptok=F
         END IF
c-----------------------------------------------------------------------
         IF(Inptok)THEN
          CALL cpyint(adif,2,1,Diffam)
          hvdiff=T
         END IF
        END IF
        GO TO 240
c     ------------------------------------------------------------------
c     Print argument
c-----------------------------------------------------------------------
   80   CALL getprt(LSPAUM,NSPAUM,Inptok)
        GO TO 240
c-----------------------------------------------------------------------
c     savelog  argument
c-----------------------------------------------------------------------
   90   CALL getsvl(LSLAUM,NSLAUM,Inptok)
        GO TO 240
c-----------------------------------------------------------------------
c     balanced argument
c-----------------------------------------------------------------------
  100   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for balanced are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lbalmd=ivec(1).eq.1
        GO TO 240
c-----------------------------------------------------------------------
c     exactdiff argument
c-----------------------------------------------------------------------
  110   CALL gtdcvc(LPAREN,T,1,EXDDIC,exdptr,PEXD,
     &          'Available options for exactdiff are yes, first or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Exdiff=ivec(1)-1
        GO TO 240
c-----------------------------------------------------------------------
c     hrinitial argument
c-----------------------------------------------------------------------
  120   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for hrinitial are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Hrinit=ivec(1).eq.1
        GO TO 240
c     ------------------------------------------------------------------
c     armalimit argument
c     ------------------------------------------------------------------
  130   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     Error Checking for Tsig
c     ------------------------------------------------------------------
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).le.ZERO)THEN
          CALL inpter(PERROR,Errpos,
     &                'Final limit for ARMA t-value must be greater '//
     &                'than zero.',T)
          Inptok=F
         ELSE
          Tsig=dvec(1)
         END IF
        END IF
        GO TO 240
c     ------------------------------------------------------------------
c     percentrse argument
c     ------------------------------------------------------------------
  140   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     Error Checking for percentrse
c     ------------------------------------------------------------------
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).le.ZERO)THEN
          CALL inpter(PERROR,Errpos,
     &                'Percentrse must be greater than zero.',T)
          Inptok=F
         ELSE
          Fct=dvec(1)
         END IF
        END IF
        GO TO 240
c     ------------------------------------------------------------------
c     reducecv argument
c     ------------------------------------------------------------------
  150   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     Error Checking for reducecv
c     ------------------------------------------------------------------
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).le.ZERO)THEN
          CALL inpter(PERROR,Errpos,
     &                'Percent reduction in critical value must be '//
     &                'greater than zero.',T)
          Inptok=F
         ELSE IF(dvec(1).ge.ONE)THEN
          CALL inpter(PERROR,Errpos,
     &                'Percent reduction in critical value cannot be'//
     &                ' greater than 1.',T)
          Inptok=F
         ELSE
          Predcv=dvec(1)
         END IF
        END IF
        GO TO 240
c     ------------------------------------------------------------------
c     ljungboxlimit argument
c     ------------------------------------------------------------------
  160   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     Error Checking for ljungboxlimit
c     ------------------------------------------------------------------
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).le.ZERO)THEN
          CALL inpter(PERROR,Errpos,
     &      'Ljung-Box Q probability limit cannot be less than zero.',T)
          Inptok=F
         ELSE IF(dvec(1).ge.ONE)THEN
          CALL inpter(PERROR,Errpos,
     &                'Ljung-Box Q probability limit must be less '//
     &                'than one.',T)
          Inptok=F
         ELSE
          Pcr=dvec(1)
         END IF
        END IF
        GO TO 240
c-----------------------------------------------------------------------
c     acceptdefault argument
c-----------------------------------------------------------------------
  170   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &             'Available options for acceptdefault are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Laccdf=ivec(1).eq.1
        GO TO 240
c-----------------------------------------------------------------------
c     noautooutlier argument
c-----------------------------------------------------------------------
  180   CALL gtdcvc(LPAREN,T,1,NOTDIC,notptr,PNOT,
     &         'Available options for noautooutlier are same or tramo.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lotmod=ivec(1).eq.1
        GO TO 240
c-----------------------------------------------------------------------
c     Ub1lim argument
c     ------------------------------------------------------------------
  190   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     Error Checking for Ub1lim
c     ------------------------------------------------------------------
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).le.ONE)THEN
          CALL inpter(PERROR,Errpos,
     &    'Unit root limit for final model must be greater than one.',T)
          Inptok=F
         ELSE 
          Ubfin=dvec(1)
         END IF
        END IF
        GO TO 240
c-----------------------------------------------------------------------
c     checkmu argument (may be added later)
c-----------------------------------------------------------------------
  200   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for checkmu are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lchkmu=ivec(1).eq.1
        GO TO 240
c-----------------------------------------------------------------------
c     mixed argument
c-----------------------------------------------------------------------
  210   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for mixed are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lmixmd=ivec(1).eq.1
        GO TO 240
c-----------------------------------------------------------------------
c     rejectfcst argument
c-----------------------------------------------------------------------
  220   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for rejectfcst are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lrejfc=ivec(1).eq.1
        GO TO 240
c     ------------------------------------------------------------------
c     Fcstlim argument
c     ------------------------------------------------------------------
  230   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
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
          Fctlm2=dvec(1)
         END IF
        END IF
        GO TO 240
c-----------------------------------------------------------------------
c     seasonaloverdiff argument
c-----------------------------------------------------------------------
  235   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &          'Available options for seasonaloverdiff are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lsovdf=ivec(1).eq.1
        GO TO 240
c-----------------------------------------------------------------------
       END IF
       IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     IF automatic model selection not selected, set to both, unless
c     orders of differencing are specified.
c     ------------------------------------------------------------------
       IF((.not.Lautom))Lautom=T
       IF((.not.Lautod).and.(.not.hvdiff))Lautod=T
       IF(omax(1).eq.NOTSET)THEN
        Maxord(1)=2
        Maxord(2)=1
       END IF
       IF(adif(1).eq.NOTSET)THEN
        Diffam(1)=2
        Diffam(2)=1
c     ------------------------------------------------------------------
c     Remove check to see if maxdiff = 0 (BCM 10-14-2008)
c     ------------------------------------------------------------------
*       ELSE IF(Lautod)THEN
*        IF(Diffam(1).eq.0.or.Diffam(2).eq.0)THEN
*         CALL writln('ERROR: Maximum order for automatic difference sele
*     &ction procedure cannot',STDERR,Mt2,T)
*         CALL writln('       be set to zero.',STDERR,Mt2,F)
*         Inptok=F
*        END IF
       END IF
c     ------------------------------------------------------------------
c     Ensure that automatic outlier identification output is turned off
c     during automatic model selection (can remove later when debugging)
c     BCM 02-02-2006
c     ------------------------------------------------------------------
c       IF(istrue(Prttab,LAUOTH,LAUOFT))THEN
c        DO i=LAUOTH,LAUOFT
c         IF(Prttab(i))Prttab(i)=F
c        END DO
c       END IF
       IF(istrue(Savtab,LAUOTH,LAUOFT))THEN
        DO i=LAUOTH,LAUOFT
         IF(Savtab(i))Savtab(i)=F
        END DO
       END IF
c     ------------------------------------------------------------------
       RETURN
  240  CONTINUE
      END DO
c     ------------------------------------------------------------------
      END
