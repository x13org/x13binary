C     Last change:  BCM  13 May 2003    9:10 am
      SUBROUTINE getx11(Havesp,Sp,Muladd,Kfulsm,Sigml,Sigmu,Lterm,
     &                  Ktcopt,Lter,Notc,Imad,Ttlvec,Tic,Ksdev,Csigvc,
     &                  Keastr,Thtapr,Finhol,Finao,Finls,Fintc,Finusr,
     &                  Shrtsf,Psuadd,Prt1ps,Noxfct,Tru7hn,
     &                  Lcentr,Ishrnk,Inptok)
c     &                  Lcentr,Ishrnk,Kexopt,Iwt,Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Get the X-11 seasonal adjustment options for X-13ARIMA-SEATS.
c----------------------------------------------------------------------
c     Variable typing and parameters initialization
c-----------------------------------------------------------------------
c    Add appendbcst argument, october 2006, bcm
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'lex.i'
      INCLUDE 'stdio.i'
      INCLUDE 'tbllog.i'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'svllog.i'
      INCLUDE 'error.cmn'
      INCLUDE 'units.cmn'
c     ------------------------------------------------------------------
      DOUBLE PRECISION ONE,ZERO
      LOGICAL T,F
      INTEGER PCHR,PTIT
      PARAMETER(PCHR=1000,PTIT=10,T=.true.,F=.false.,ONE=1D0,ZERO=0D0)
c     ------------------------------------------------------------------
      CHARACTER cny*2,chrstr*(PCHR),Ttlvec*(*)
      LOGICAL Inptok,argok,gtarg,Csigvc,Havesp,Finhol,Finao,Finls,Fintc,
     &        Finusr,Shrtsf,Psuadd,dpeq,Prt1ps,Noxfct,
     &        Lcentr,Tru7hn
      INTEGER i,isf,nelt,Imad,Muladd,Kfulsm,Lterm,Ktcopt,isvc,Ishrnk,
     &        Lter,Notc,Ksdev,Sp,ivec,Keastr,ptrstr,calidx
      DOUBLE PRECISION Sigml,Sigmu,sigl,Tic,Thtapr,dvec
      DIMENSION Csigvc(*),Lter(*),Ttlvec(*),isf(PSP),calidx(PSP),
     &          sigl(2),ptrstr(0:PTIT),ivec(1),dvec(1)
c     ------------------------------------------------------------------
c      INTEGER Kexopt,Iwt
      EXTERNAL gtarg,dpeq
c     ------------------------------------------------------------------
c     Define the data dictionaries for the X11 arguments and other
c     function declarations.
c     ------------------------------------------------------------------
c     x11 arguments data dictionary
c     ------------------------------------------------------------------
      CHARACTER X11DIC*201
c      CHARACTER X11DIC*215
      INTEGER x11log,x11idx,x11ptr,PX11
      PARAMETER(PX11=25)
c      PARAMETER(PX11=27)
      DIMENSION x11ptr(0:PX11),x11log(2,PX11)
      PARAMETER(X11DIC='modesigmalimseasonalmatrendmatitleextremeadjtype
     &appendfcsttrendiccalendarsigmasigmavecx11eastertaperkeepholidayfin
     &alsfshortprintsavesavelogprint1stpassexcludefcsttrue7termshrinkcen
     &terseasonalappendbcst')
c     &terseasonalappendbcststrikeitrendma')
c     ------------------------------------------------------------------
c     seasonal adjustment mode data dictionary
c     ------------------------------------------------------------------
      CHARACTER MODDIC*22
      INTEGER modptr,PMODE
      PARAMETER(PMODE=4)
      DIMENSION modptr(0:PMODE)
      PARAMETER(MODDIC='multaddlogaddpseudoadd')
c     ------------------------------------------------------------------
c     seasonal filter data dictionary
c     ------------------------------------------------------------------
      CHARACTER SFDIC*40
      INTEGER sfptr,PSF
      PARAMETER(PSF=8)
      DIMENSION sfptr(0:PSF)
      PARAMETER(SFDIC='x11defaults3x3s3x5s3x9s3x15stablemsrs3x1')
c     ------------------------------------------------------------------
c     seasonal adjustment type data dictionary
c     ------------------------------------------------------------------
      CHARACTER TYPDIC*14
      INTEGER typptr,PSATYP
      PARAMETER(PSATYP=3)
      DIMENSION typptr(0:PSATYP)
      PARAMETER(TYPDIC='sasummarytrend')
c     ------------------------------------------------------------------
c     X-11 extreme variance data dictionary
c     ------------------------------------------------------------------
      CHARACTER OTLDIC*23
      INTEGER otlptr,POTLXV
      PARAMETER(POTLXV=5)
      DIMENSION otlptr(0:POTLXV)
      PARAMETER(OTLDIC='stdwmadwmadlogtautaulog')
c-----------------------------------------------------------------------
c     calendar sigma data dictionary
c-----------------------------------------------------------------------
      CHARACTER BNDDIC*19
      INTEGER bndptr,PBND
      PARAMETER(PBND=4)
      DIMENSION bndptr(0:PBND)
      PARAMETER(BNDDIC='nonesignifallselect')
c-----------------------------------------------------------------------
c     data dictionary of yes/no choice
c-----------------------------------------------------------------------
      CHARACTER YSNDIC*5
      INTEGER ysnptr,PYSN
      PARAMETER(PYSN=2)
      DIMENSION ysnptr(0:PYSN)
      PARAMETER(YSNDIC='yesno')
c     ------------------------------------------------------------------
c     data dictionary for final argument      
c     ------------------------------------------------------------------
      CHARACTER FINDIC*10
      INTEGER finind,finptr,PFIN
      PARAMETER(PFIN=4)
      DIMENSION finind(PFIN),finptr(0:PFIN)
      PARAMETER(FINDIC='aolsusertc')
c     ------------------------------------------------------------------
c     data dictionary for initial trend moving average
c     ------------------------------------------------------------------
*      CHARACTER ITRDIC*22
*      INTEGER itrptr,PITR
*      PARAMETER(PITR=2)
*      DIMENSION itrptr(0:PITR)
*      PARAMETER(ITRDIC='centered1yrcholette2yr')
c-----------------------------------------------------------------------
c     sigmavec data dictionary
c-----------------------------------------------------------------------
      CHARACTER SUMDIC*118
      INTEGER sumptr,PSUM
      PARAMETER(PSUM=28)
      DIMENSION sumptr(0:PSUM)
      PARAMETER(SUMDIC=
     &'janfebmaraprmayjunjulaugsepoctnovdecjanuaryfebruarymarchaprilmayj
     &unejulyaugustseptemberoctobernovemberdecemberq1q2q3q4')
c     ------------------------------------------------------------------
c     data dictionary for shrinkage estimators
c     ------------------------------------------------------------------
      CHARACTER SHKDIC*15
      INTEGER shkptr,PSHK
      PARAMETER(PSHK=3)
      DIMENSION shkptr(0:PSHK)
      PARAMETER(SHKDIC='nonegloballocal')
c     ------------------------------------------------------------------
c     Define data dictionary pointers
c     ------------------------------------------------------------------
      DATA x11ptr / 1,5,13,23,30,35,45,49,59,66,79,87,96,101,112,117,
     &            124,129,133,140,152,163,172,178,192,202/
c     &            124,129,133,140,152,163,172,178,192,202,208,216/
      DATA modptr/1,5,8,14,23/
      DATA sfptr/1,11,15,19,23,28,34,37,41/
      DATA sumptr/1,4,7,10,13,16,19,22,25,28,31,34,37,44,52,57,62,65,69,
     &            73,79,88,95,103,111,113,115,117,119/
      DATA typptr/1,3,10,15/
      DATA otlptr/1,4,8,15,18,24/
      DATA bndptr/1,5,11,14,20/
      DATA ysnptr/1,4,6/
      DATA finptr/1,3,5,9,11/
*      DATA itrptr/1,12,23/
      DATA shkptr/1,5,11,16/
c-----------------------------------------------------------------------
c     Initialize variables
c-----------------------------------------------------------------------
      argok=T
      CALL setdp(DNOTST,2,sigl)
      CALL setint(NOTSET,PSP,isf)
      CALL setint(NOTSET,2*PX11,x11log)
c-----------------------------------------------------------------------
      DO WHILE (T)
c-----------------------------------------------------------------------
c     Argument get loop
c-----------------------------------------------------------------------
       IF(gtarg(X11DIC,x11ptr,PX11,x11idx,x11log,Inptok))THEN
        IF(Lfatal)RETURN
        GO TO(10,20,30,40,60,70,80,90,120,130,140,210,250,260,290,265,
     &        270,280,300,100,310,320,200,160,170),x11idx
c     &        270,280,300,100,310,320,200,160,170,150,50),x11idx
c-----------------------------------------------------------------------
c     mode argument
c-----------------------------------------------------------------------
   10   CALL gtdcvc(LPAREN,T,1,MODDIC,modptr,PMODE,
     &  'Improper seasonal adjustment mode: valid choices for mode are',
     &              ivec,nelt,F,argok,Inptok)
        IF(Lfatal)RETURN
        IF(nelt.le.0)THEN
         CALL writln('       mult, add, logadd or pseudoadd.',
     &               STDERR,Mt2,F,T)
        ELSE
         Muladd=ivec(1)
         IF(argok)THEN
          IF(Muladd.eq.4)THEN
           Muladd=0
           Psuadd=T
          ELSE
           Muladd=Muladd-1
          END IF
         END IF
        END IF
        GO TO 330
c-----------------------------------------------------------------------
c     sigmalim argument
c-----------------------------------------------------------------------
   20   CALL gtdpvc(LPAREN,F,2,sigl,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(nelt.eq.1)THEN
         CALL inpter(PERROR,Errpos,
     &               'Two sigma limits needed (or use a comma as '//
     &               'place holder).',T)
         Inptok=F
        ELSE IF(nelt.gt.0)THEN
         IF(dpeq(sigl(1),DNOTST))sigl(1)=1.5D0
         IF(dpeq(sigl(2),DNOTST))sigl(2)=2.5D0
         IF(sigl(1).le.ZERO.or.sigl(2).le.ZERO)THEN
          CALL inpter(PERROR,Errpos,
     &                'Sigma limits must be greater than zero.',T)
          Inptok=F
c-----------------------------------------------------------------------
         ELSE IF(sigl(1).gt.sigl(2))THEN
          CALL inpter(PERROR,Errpos,
     &                'Lower sigma limit must be less than upper '//
     &                'sigma limit.',T)
          Inptok=F
c-----------------------------------------------------------------------
         ELSE
          Sigml=sigl(1)
          Sigmu=sigl(2)
         END IF
        END IF
        GO TO 330
c-----------------------------------------------------------------------
c     seasonalma argument
c-----------------------------------------------------------------------
   30   CALL gtdcvc(LPAREN,F,PSP,SFDIC,sfptr,PSF,
     &              'Improper value(s) entered for seasonalma.',
     &              isf,nelt,F,argok,Inptok)
        IF(Lfatal)RETURN
        IF(nelt.eq.0)THEN
         CALL writln('       Valid choices of seasonal filter are '//
     &               's3x1, s3x3, s3x5, s3x9,',STDERR,Mt2,F,F)
         CALL writln('        s3x15, stable, msr, or x11default.',
     &               STDERR,Mt2,F,T)
        END IF
        IF(argok.and.nelt.gt.0)THEN
c-----------------------------------------------------------------------
         IF(.not.Havesp)THEN
          CALL inpter(PERROR,Errpos,
     &                'No seasonal period specified in series spec.',T)
          Inptok=F
c-----------------------------------------------------------------------
c     If only one filter given, use it all year
c-----------------------------------------------------------------------
         ELSE IF(nelt.eq.1)THEN
          Lterm=isf(1)-1
          DO i=1,Sp
           Lter(i)=Lterm
          END DO
c-----------------------------------------------------------------------
c     If filters are given for every month (or quarter) of the year,
c     reset those periods not set to be the same as the first period.
c-----------------------------------------------------------------------
         ELSE IF(nelt.eq.Sp)THEN
          IF(isf(1).eq.NOTSET)isf(1)=6
          Lterm=isf(1)-1
          Lter(1)=Lterm
          DO i=2,Sp
           IF(isf(i).eq.NOTSET)THEN
            Lter(i)=Lterm
           ELSE
            Lter(i)=isf(i)-1
           END IF
          END DO
c-----------------------------------------------------------------------
c     Else, print out an error message.
c-----------------------------------------------------------------------
         ELSE
          i=1
          CALL itoc(Sp,cny,i)
          CALL inpter(PERROR,Errpos,'Specify either 1 or '//
     &         cny(1:(i-1))//' seasonal filters (or use a comma as a',F)
          CALL writln('       place holder).',STDERR,Mt2,F,T)
          Inptok=F
         END IF
        END IF
        GO TO 330
c-----------------------------------------------------------------------
c     trendma argument
c-----------------------------------------------------------------------
   40   CALL getivc(LPAREN,T,1,ivec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
        IF(argok.and.nelt.gt.0)THEN
         IF(mod(ivec(1),2).eq.0.or.ivec(1).le.0)THEN
          CALL inpter(PERROR,Errpos,
     &                'Length of Henderson trend filter must be a '//
     &                'positive odd integer.',T)
          Inptok=F
         ELSE
          Ktcopt=ivec(1)
         END IF
        END IF
        GO TO 330
c-----------------------------------------------------------------------
c     itrendma argument
c-----------------------------------------------------------------------
c   50   CALL gtdcvc(LPAREN,F,1,ITRDIC,itrptr,PSF,'Available options for 
c     &itrendma are centered1yr and chollette2yr.',
c     &              ivec,nelt,argok,Inptok)
c        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c        IF(argok.and.nelt.gt.0)Iwt=ivec(1)-1
c        GO TO 330
c-----------------------------------------------------------------------
c     Title argument
c-----------------------------------------------------------------------
   60   CALL getttl(LPAREN,T,PTIT,chrstr,ptrstr,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         DO i=1,nelt
          CALL getstr(chrstr,ptrstr,nelt,i,Ttlvec(i),Notc)
          IF(Lfatal)RETURN
         END DO
         Notc=nelt
        END IF
        CALL setchr(' ',PCHR,chrstr)
        GO TO 330
c-----------------------------------------------------------------------
c     X-11 Extreme value detection argument
c-----------------------------------------------------------------------
   70   CALL gtdcvc(LPAREN,T,1,OTLDIC,otlptr,POTLXV,'Improper X-11 outli
     &er option: valid choices for extremeadj are',
     &              ivec,nelt,F,argok,Inptok)
        IF(Lfatal)RETURN
        IF(nelt.le.0)THEN
         CALL writln('       std, wmad, wmadlog, tau, taulog.',STDERR,
     &               Mt2,F,T)
        ELSE
         IF(argok)Imad=ivec(1)-1
        END IF
        GO TO 330
c-----------------------------------------------------------------------
c     type argument
c-----------------------------------------------------------------------
   80   CALL gtdcvc(LPAREN,T,1,TYPDIC,typptr,PSATYP,
     &      'The available adjustment types are sa, summary, or trend.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Kfulsm=ivec(1)-1
        GO TO 330
c-----------------------------------------------------------------------
c     appendfcst argument
c-----------------------------------------------------------------------
   90   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &       'Available options for appending forecasts are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Savfct=ivec(1).eq.1
        GO TO 330
c-----------------------------------------------------------------------
c     print1stpass argument
c-----------------------------------------------------------------------
  100   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for print1stpass are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Prt1ps=ivec(1).eq.1
        GO TO 330
c-----------------------------------------------------------------------
c     trendic argument
c-----------------------------------------------------------------------
  120   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).le.ZERO)THEN
          CALL inpter(PERROR,Errpos,
     &               'Specified I/C ratio must be greater than zero.',T)
          Inptok=F
         ELSE
          Tic=dvec(1)
         END IF
        END IF
        GO TO 330
c-----------------------------------------------------------------------
c     Bundesbank outlier adjustment argument
c-----------------------------------------------------------------------
  130   CALL gtdcvc(LPAREN,T,1,BNDDIC,bndptr,PBND,
     &              'Available options for calendarsigma are none, '//
     &              'signif, all or select.',ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        Ksdev=ivec(1)
        GO TO 330
c-----------------------------------------------------------------------
c     Periods to be grouped together for CALENDARSIGMA=SELECT option
c-----------------------------------------------------------------------
  140   CALL gtdcvc(LPAREN,T,PSP,SUMDIC,sumptr,PSUM,
     &              'Improper value(s) entered for sigmavec.',
     &              calidx,nelt,F,argok,Inptok)
        IF(Lfatal)RETURN
        IF(nelt.eq.0)THEN
         CALL writln('       Valid choices for sigmavec are the '//
     &               'name(s) of a month or',STDERR,Mt2,F,F)
         CALL writln('       quarter.',STDERR,Mt2,F,T)
        END IF
c-----------------------------------------------------------------------
        IF(argok.and.nelt.gt.0)THEN
         IF(.not.Havesp)THEN
          CALL inpter(PERROR,Errpos,
     &                'No seasonal period specified in series spec.',T)
          Inptok=F
c-----------------------------------------------------------------------
         ELSE
          DO i=1,nelt
           isvc=calidx(i)
           IF(isvc.ge.13.and.isvc.le.24.and.Sp.eq.12)THEN
            isvc=isvc-12
           ELSE IF(isvc.ge.25.and.Sp.eq.4)THEN
            isvc=isvc-24
           ELSE
            IF(Sp.eq.12.and.isvc.ge.25)THEN
             CALL inpter(PERROR,Errpos,
     &                   'Entry for sigmavec not valid for monthly '//
     &                   'data.',T)
             Inptok=F
             isvc=NOTSET
            ELSE IF(Sp.eq.4.and.isvc.lt.25)THEN
             CALL inpter(PERROR,Errpos,
     &                   'Entry for sigmavec not valid for '//
     &                   'quarterly data.',T)
             Inptok=F
             isvc=NOTSET
            END IF
           END IF
           IF(isvc.gt.0)Csigvc(isvc)=T
          END DO
         END IF
        END IF
        GO TO 330
c-----------------------------------------------------------------------
c     strike argument
c-----------------------------------------------------------------------
c  150   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
c     &              'Available options for strike are yes or no.',
c     &              ivec,nelt,argok,Inptok)
c        IF(Lfatal)RETURN
c        IF(argok.and.nelt.gt.0)Kexopt=2-ivec(1)
c        GO TO 330
c-----------------------------------------------------------------------
c     centerseasonal argument
c-----------------------------------------------------------------------
  160   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &            'Available options for centerseasonal are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lcentr=ivec(1).eq.1
        GO TO 330
c-----------------------------------------------------------------------
c     appendbcst argument
c-----------------------------------------------------------------------
  170   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &       'Available options for appending backcasts are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Savbct=ivec(1).eq.1
        GO TO 330
c-----------------------------------------------------------------------
c      shrink argument
c-----------------------------------------------------------------------
  200   CALL gtdcvc(LPAREN,T,1,SHKDIC,shkptr,PSHK,
     &       'Entry for shrink argument must be none, global or local.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Ishrnk=ivec(1)-1
        GO TO 330
c-----------------------------------------------------------------------
c     x11easter argument
c-----------------------------------------------------------------------
  210   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for x11easter are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Keastr=2-ivec(1)
        GO TO 330
c-----------------------------------------------------------------------
c     Taper argument
c-----------------------------------------------------------------------
  250   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Error Checking for taper
c-----------------------------------------------------------------------
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).lt.ZERO.or.dvec(1).gt.ONE)THEN
          CALL inpter(PERROR,Errpos,
     &                'Value of taper must be between zero and 1.',T)
          Inptok=F
         ELSE
          Thtapr=dvec(1)
         END IF
        END IF
        GO TO 330
c-----------------------------------------------------------------------
c     keepholiday argument
c-----------------------------------------------------------------------
  260   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &             'Available options for keepholiday are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
        IF(argok.and.nelt.gt.0)Finhol=ivec(1).eq.2
        GO TO 330
c-----------------------------------------------------------------------
c     final argument
c-----------------------------------------------------------------------
  290   CALL gtdcvc(LPAREN,T,PFIN,FINDIC,finptr,PFIN,
     &            'Choices for final argument are ao, ls, tc, or user.',
     &              finind,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
        IF(argok.and.nelt.gt.0)THEN
         DO i=1,nelt
          IF(finind(i).eq.1)THEN
           Finao=T
          ELSE IF(finind(i).eq.2)THEN
           Finls=T
          ELSE IF(finind(i).eq.3)THEN
           Finusr=T
          ELSE IF(finind(i).eq.4)THEN
           Fintc=T
          END IF
         END DO
        END IF
        GO TO 330
c-----------------------------------------------------------------------
c     sfshort argument
c-----------------------------------------------------------------------
  265   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for sfshort are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Shrtsf=ivec(1).eq.1
        GO TO 330
c-----------------------------------------------------------------------
c     print argument
c-----------------------------------------------------------------------
  270   CALL getprt(LSPX11,NSPX11,Inptok)
        GO TO 330
c-----------------------------------------------------------------------
c     save  argument
c-----------------------------------------------------------------------
  280   CALL getsav(LSPX11,NSPX11,Inptok)
        GO TO 330
c-----------------------------------------------------------------------
c     savelog  argument
c-----------------------------------------------------------------------
  300   CALL getsvl(LSLX11,NSLX11,Inptok)
        GO TO 330
c-----------------------------------------------------------------------
c     excludefcst argument
c-----------------------------------------------------------------------
  310   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for excludefcst are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Noxfct=ivec(1).eq.1
        GO TO 330
c-----------------------------------------------------------------------
c     true7term argument
c-----------------------------------------------------------------------
  320   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for true7term are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Tru7hn=ivec(1).eq.1
        GO TO 330
c     ------------------------------------------------------------------
       END IF
       IF(Lfatal)RETURN
       Inptok=Inptok.and.argok 
       RETURN
  330  CONTINUE
      END DO
c     ------------------------------------------------------------------
      END
