C     Last change:  BCM  16 Sep 2005    1:27 pm
      SUBROUTINE getcmp(Probs,Havesp,Sp,Y,Nobs,Start,Nspobs,Begspn,
     &                  Srsttl,Nttlcr,Srsnam,Nser,Itest,Kdec,Begmdl,
     &                  Endmdl,Svprec,Locok,Yr2000,Lindot,Isrflw,
     &                  Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Set options for final direct and indirect composite adjustment,
c including the number of observations, nobs, start date, start, and
c seasonal period, Sp.
c-----------------------------------------------------------------------
c     Variable typing and parameters initialization
c-----------------------------------------------------------------------
c    Add appendfcst and appendbcst arguments, october 2006, bcm
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'lex.i'
      INCLUDE 'tbllog.i'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'svllog.i'
      INCLUDE 'stdio.i'
      INCLUDE 'error.cmn'
      INCLUDE 'units.cmn'
c-----------------------------------------------------------------------
      LOGICAL F,T
      INTEGER YR,MO
      DOUBLE PRECISION ZERO
      PARAMETER(F=.false.,T=.true.,YR=1,MO=2,ZERO=0D0)
c-----------------------------------------------------------------------
      CHARACTER Srsttl*(*),Srsnam*(64)
      LOGICAL argok,Locok,Inptok,Havesp,Yr2000,Lindot
      INTEGER Sp,nelt,Nobs,Nttlcr,Probs,Start,tmpptr,Itest,endspn,nspec,
     &        Kdec,Begspn,Begmdl,Endmdl,spnmdl,nobmdl,Nspobs,Nser,ivec,
     &        Isrflw,Svprec
      DOUBLE PRECISION Y,Spclim,dvec
      DIMENSION Start(2),tmpptr(0:1),Itest(5),endspn(2),Y(Probs),
     &          Begspn(2),Begmdl(2),Endmdl(2),spnmdl(2,2),ivec(1),
     &          dvec(1)
c-----------------------------------------------------------------------
      LOGICAL gtarg,chkcvr
      EXTERNAL chkcvr,gtarg
c-----------------------------------------------------------------------
      CHARACTER ARGDIC*95
      INTEGER argidx,argptr,PARG,arglog
      PARAMETER(PARG=13)
      DIMENSION argptr(0:PARG),arglog(2,PARG)
      PARAMETER(ARGDIC='nametitleprintsavedecimalsmodelspansaveprecision
     &savelogyr2000indoutlierappendfcstappendbcsttype')
c-----------------------------------------------------------------------
      CHARACTER YSNDIC*5
      INTEGER ysnptr,PYSN
      PARAMETER(PYSN=2)
      DIMENSION ysnptr(0:PYSN)
      PARAMETER(YSNDIC='yesno')
c     ------------------------------------------------------------------
      CHARACTER TYPDIC*9
      INTEGER typptr,PTYP
      PARAMETER(PTYP=2)
      DIMENSION typptr(0:PTYP)
      PARAMETER(TYPDIC='flowstock')
c-----------------------------------------------------------------------
      DATA argptr / 1,5,10,15,19,27,36,49,56,62,72,82,92,96 /
      DATA ysnptr / 1,4,6 /
      DATA typptr/1,5,10/
c-----------------------------------------------------------------------
c     Assume the input is OK and we don't have any of the arguments
c-----------------------------------------------------------------------
      Locok=T
      CALL setint(NOTSET,4,spnmdl)
      CALL setint(NOTSET,2*PARG,arglog)
c-----------------------------------------------------------------------
c     Argument get loop
c-----------------------------------------------------------------------
      DO WHILE (T)
       IF(gtarg(ARGDIC,argptr,PARG,argidx,arglog,Inptok))THEN
        IF(Lfatal)RETURN
        GO TO(10,20,30,40,50,60,70,80,90,100,110,120,130),argidx
c-----------------------------------------------------------------------
c     Series name argument
c-----------------------------------------------------------------------
   10   CALL gtnmvc(LPAREN,T,1,Srsnam,tmpptr,nelt,64,argok,Locok)
        IF(Lfatal)RETURN
        IF(argok)CALL eltlen(1,tmpptr,nelt,Nser)
        GO TO 140
c-----------------------------------------------------------------------
c     Title argument
c-----------------------------------------------------------------------
   20   CALL getttl(LPAREN,T,1,Srsttl,tmpptr,nelt,argok,Locok)
        IF(Lfatal)RETURN
        IF(argok)CALL eltlen(1,tmpptr,nelt,Nttlcr)
        GO TO 140
c-----------------------------------------------------------------------
c     Print argument
c-----------------------------------------------------------------------
   30   CALL getprt(LSPCMP,NSPCMP,Locok)
        GO TO 140
c-----------------------------------------------------------------------
c     Save argument
c-----------------------------------------------------------------------
   40   CALL getsav(LSPCMP,NSPCMP,Locok)
        GO TO 140
c-----------------------------------------------------------------------
c     decimals argument
c-----------------------------------------------------------------------
   50   CALL getivc(LPAREN,T,1,ivec,nelt,argok,Locok)
        IF(Lfatal)RETURN
        IF(argok)THEN
         IF(ivec(1).lt.0.or.ivec(1).gt.5)THEN
          CALL inpter(PERROR,Errpos,
     &                'Number of output decimals must be between 0 '//
     &                'and 5, inclusive.',T)
          Locok=F
         ELSE
          Kdec=ivec(1)
         END IF
        END IF
        GO TO 140
c-----------------------------------------------------------------------
c     Span for the model estimation.
c-----------------------------------------------------------------------
   60   CALL gtdtvc(Havesp,Sp,LPAREN,F,2,spnmdl,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(nelt.eq.1)THEN
         CALL inpter(PERROR,Errpos,'Need two dates for the model '//
     &               'span or use a comma as place holder.',T)
         Inptok=F
        END IF
        GO TO 140
c-----------------------------------------------------------------------
c     saveprecision argument
c-----------------------------------------------------------------------
   70   CALL getivc(LPAREN,T,1,ivec,nelt,argok,Locok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         IF(ivec(1).le.0.or.ivec(1).gt.15)THEN
          CALL inpter(PERROR,Errpos,
     &                'Value of saveprecision must be greater than '//
     &                'zero and less than 15.',T)
          Inptok=F
         ELSE
          Svprec=ivec(1)
         END IF
        END IF
        GO TO 140
c-----------------------------------------------------------------------
c     Savelog argument
c-----------------------------------------------------------------------
   80   CALL getsvl(LSLCMP,NSLCMP,Locok)
        GO TO 140
c-----------------------------------------------------------------------
c     yr2000 argument
c-----------------------------------------------------------------------
   90   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for yr2000 are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Yr2000=ivec(1).eq.1
        GO TO 140
c-----------------------------------------------------------------------
c     indoutlier argument
c-----------------------------------------------------------------------
  100   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for indoutlier are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lindot=ivec(1).eq.1
        GO TO 140
c-----------------------------------------------------------------------
c     appendfcst argument
c-----------------------------------------------------------------------
  110   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &       'Available options for appending forecasts are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Savfct=ivec(1).eq.1
        GO TO 140
c-----------------------------------------------------------------------
c     appendbcst argument
c-----------------------------------------------------------------------
  120   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &       'Available options for appending backcasts are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Savbct=ivec(1).eq.1
        GO TO 140
c-----------------------------------------------------------------------
c     type argument
c-----------------------------------------------------------------------
  130   CALL gtdcvc(LPAREN,T,1,TYPDIC,typptr,PTYP,
     &              'Available options for type are flow or stock.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Isrflw=ivec(1)
        GO TO 140
       END IF
       IF(Lfatal)RETURN
c----------------------------------------------------------------------
c     Set starting date, seasonal period
c----------------------------------------------------------------------
       Start(YR)=Itest(4)
       Start(MO)=Itest(2)
       Begspn(YR)=Itest(4)
       Begspn(MO)=Itest(2)
       endspn(YR)=Itest(5)
       endspn(MO)=Itest(3)
       Sp=Itest(1)
       Havesp=T
       CALL dfdate(endspn,Begspn,Sp,Nspobs)
       Nspobs=Nspobs+1
       Nobs=Nspobs
c----------------------------------------------------------------------
c     If beginning or ending date in the model span is undefined, set 
c     equal to beginning date of the span.
c----------------------------------------------------------------------
       IF(spnmdl(YR,1).eq.NOTSET)THEN
        CALL cpyint(Begspn,2,1,Begmdl)
       ELSE
        CALL cpyint(spnmdl,2,1,Begmdl)
       END IF
       IF(spnmdl(YR,2).eq.NOTSET.or.spnmdl(YR,2).eq.0)THEN
        CALL addate(Begspn,Sp,Nspobs-1,Endmdl)
        IF(spnmdl(YR,2).eq.0)THEN
         Endmdl(MO)=spnmdl(MO,2)
         IF(Endmdl(MO).gt.Endspn(MO))Endmdl(YR)=Endmdl(YR)-1
        END IF
       ELSE
        CALL cpyint(spnmdl(1,2),2,1,Endmdl)
       END IF
c-----------------------------------------------------------------------
c     Check that the span is within the series
c-----------------------------------------------------------------------
       CALL dfdate(Endmdl,Begmdl,Sp,nobmdl)
       nobmdl=nobmdl+1
       IF(.not.chkcvr(Begspn,Nspobs,Begmdl,nobmdl,Sp))THEN
        CALL inpter(PERRNP,Errpos,
     &         'Model span is not within the span of available data.',T)
        CALL cvrerr('span',Begspn,Nspobs,'model span',Begmdl,nobmdl,Sp)
        IF(Lfatal)RETURN
        Inptok=F
       END IF
c-----------------------------------------------------------------------
       IF(Locok)CALL agr1(Y,Nobs)
       IF(Isrflw.eq.NOTSET)Isrflw=0
       Inptok=Inptok.and.Locok
c     ------------------------------------------------------------------
       RETURN
  140  CONTINUE
      END DO
c     ------------------------------------------------------------------
      END
