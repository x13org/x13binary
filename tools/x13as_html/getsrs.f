C     Last change:  BCM  16 Sep 2005    1:25 pm
      SUBROUTINE getsrs(Sp,Y,Nobs,Start,Nspobs,Begspn,Srsttl,Nttlcr,
     &                  Srsnam,Nser,Havsrs,Havesp,Kdec,Begmdl,Endmdl,
     &                  Ldata,Dtafil,Iag,Iagr,Lagr,W,Mvcode,Mvval,
     &                  Fixper,Svprec,Yr2000,Divpwr,Isrflw,Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Get the time series, y, including the number of observations,
c nobs, start date, start, and seasonal period, Sp.
c-----------------------------------------------------------------------
c    Add appendfcst and appendbcst arguments, october 2006, bcm
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
      DOUBLE PRECISION Mvcode,Mvval,Y,W,dvec
      CHARACTER Dtafil*(PFILCR),file*(PFILCR),fmt*(PFILCR),Srsttl*(*),
     &          Srsnam*(64)
      LOGICAL argok,havfil,havfmt,Havesp,Havsrs,Ldata,hvstrt,locok,
     &        Inptok,hvnam,Lagr,hvmdsp,Yr2000,havttl
      INTEGER Begspn,endspn,Sp,spnvec,ivec,nflchr,nfmtch,nelt,Nobs,
     &        Nspobs,Nttlcr,Start,tmpptr,numdec,nspec,Kdec,Begmdl,
     &        Endmdl,spnmdl,nmdl,Nser,Iag,Iagr,Fixper,Svprec,Divpwr,
     &        Isrflw,ltrim
      DIMENSION Begspn(2),endspn(2),spnvec(2,2),Start(2),tmpptr(0:1),
     &          Begmdl(2),Endmdl(2),spnmdl(2,2),Y(PLEN),dvec(1),ivec(1)
c-----------------------------------------------------------------------
      INTEGER nblank
      LOGICAL chkcvr,gtarg,isdate
      EXTERNAL nblank,chkcvr,gtarg,isdate
c-----------------------------------------------------------------------
c     This dictionary was made with this command
c  ../../dictionary/strary < ../../dictionary/series.dic
c-----------------------------------------------------------------------
      CHARACTER ARGDIC*167
      INTEGER argidx,argptr,PARG,arglog
      PARAMETER(PARG=24)
      DIMENSION argptr(0:PARG),arglog(2,PARG)
      PARAMETER(ARGDIC='datastartperiodspantitlefileformatprintsavenamep
     &recisiondecimalsmodelspancomptypecompwtmissingcodemissingvalsavepr
     &ecisionyr2000trimzerodivpowerappendfcstappendbcsttype')
c-----------------------------------------------------------------------
c     type of compositing data dictionary
c-----------------------------------------------------------------------
      CHARACTER CMPDIC*17
      INTEGER cmpptr,PCMP
      PARAMETER(PCMP=5)
      DIMENSION cmpptr(0:PCMP)
      PARAMETER(CMPDIC='noneaddsubmultdiv')
c-----------------------------------------------------------------------
      CHARACTER YSNDIC*5
      INTEGER ysnptr,PYSN
      PARAMETER(PYSN=2)
      DIMENSION ysnptr(0:PYSN)
      PARAMETER(YSNDIC='yesno')
c-----------------------------------------------------------------------
      CHARACTER ZRODIC*9
      INTEGER zroptr,PZRO
      PARAMETER(PZRO=3)
      DIMENSION zroptr(0:PZRO)
      PARAMETER(ZRODIC='yesspanno')
c     ------------------------------------------------------------------
      CHARACTER TYPDIC*9
      INTEGER typptr,PTYP
      PARAMETER(PTYP=2)
      DIMENSION typptr(0:PTYP)
      PARAMETER(TYPDIC='flowstock')
c-----------------------------------------------------------------------
      DATA argptr/1,5,10,16,20,25,29,35,40,44,48,57,65,74,82,88,99,109,
     &            122,128,136,144,154,164,168/
      DATA ysnptr/1,4,6/
      DATA zroptr/1,4,8,10/
      DATA cmpptr/1,5,8,11,15,18/
      DATA typptr/1,5,10/
c-----------------------------------------------------------------------
c     Assume the input is OK and we don't have any of the arguments
c-----------------------------------------------------------------------
      locok=T
      Havsrs=F
      havfil=F
      havttl=F
      havfmt=F
      hvstrt=F
      Havesp=F
      hvnam=F
      hvmdsp=F
      ltrim=0
      nfmtch=1
      CALL setint(NOTSET,4,spnvec)
      CALL setint(NOTSET,4,spnmdl)
      CALL setint(NOTSET,2*PARG,arglog)
      CALL setint(NOTSET,2,endspn)
      numdec=0
c-----------------------------------------------------------------------
c     Initialize the format and file
c-----------------------------------------------------------------------
      CALL setchr(' ',PFILCR,file)
      CALL setchr(' ',PFILCR,fmt)
      IF(Ldata)THEN
       file=Dtafil
       nflchr=nblank(Dtafil)
       havfil=T
      END IF
c-----------------------------------------------------------------------
c     Argument get loop
c-----------------------------------------------------------------------
      DO WHILE (T)
       IF(gtarg(ARGDIC,argptr,PARG,argidx,arglog,Inptok))THEN
        IF(Lfatal)RETURN
        GO TO(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,
     &        170,180,190,200,210,220,230,240),argidx
c-----------------------------------------------------------------------
c     Data argument
c-----------------------------------------------------------------------
   10   IF(Ldata)THEN
         CALL inpter(PERROR,Errpos,
     &       'Cannot use data argument when a data metafile is used.',T)
         locok=F
        ELSE IF(havfil)THEN
         CALL inpter(PERROR,Errpos,
     &               'Use either data or file, not both',T)
         locok=F
        END IF
c     ------------------------------------------------------------------
        CALL gtdpvc(LPAREN,T,PLEN,Y,Nobs,argok,locok)
        IF(Lfatal)RETURN
        IF(argok.and.Nobs.gt.0)THEN
         Havsrs=T
        ELSE
         Nobs=0
        END IF
        GO TO 250
c-----------------------------------------------------------------------
c     Start argument
c-----------------------------------------------------------------------
   20   CALL gtdtvc(Havesp,Sp,LPAREN,F,1,Start,nelt,argok,locok)
        IF(Lfatal)RETURN
        hvstrt=argok.and.nelt.gt.0
c        IF(hvstrt.and.Sp.eq.1)THEN
c         CALL inpter(PERROR,Errpos,
c     &         'Starting date entered here is for an annual series.',T)
c         locok=F
c        END IF
c     ------------------------------------------------------------------
        GO TO 250
c-----------------------------------------------------------------------
c     Period argument
c-----------------------------------------------------------------------
   30   CALL getivc(LPAREN,T,1,ivec,nelt,argok,locok)
        IF(Lfatal)RETURN
        IF(nelt.gt.0)THEN
         IF(.not.argok)THEN
          CALL inpter(PERROR,Errpos,'Invalid seasonal period',T)
          locok=F
c     ------------------------------------------------------------------
         ELSE IF(ivec(1).gt.PSP)THEN
          CALL inpter(PERROR,Errpos,'Seasonal period too large.',F)
          CALL writln('        See '//LIMSEC//' of the '//DOCNAM//
     &                ' on program limits',STDERR,Mt2,F,T)
          locok=F
c     ------------------------------------------------------------------
         ELSE IF(Havesp.and.ivec(1).ne.Sp)THEN
          CALL inpter(PERROR,Errpos,'Assumed seasonal period of 12',T)
          locok=F
c     ------------------------------------------------------------------
         ELSE
          Havesp=T
          Sp=ivec(1)
         END IF
        END IF
        GO TO 250
c-----------------------------------------------------------------------
c     Span argument
c-----------------------------------------------------------------------
   40   CALL gtdtvc(Havesp,Sp,LPAREN,F,2,spnvec,nelt,argok,locok)
        IF(Lfatal)RETURN
        IF(nelt.eq.1)THEN
         CALL inpter(PERROR,Errpos,
     &               'Need two dates for the span or use a comma as '//
     &               'place holder.',T)
         locok=F
        END IF
        GO TO 250
c-----------------------------------------------------------------------
c     Title argument
c-----------------------------------------------------------------------
   50   CALL getttl(LPAREN,T,1,Srsttl,tmpptr,nelt,argok,locok)
        IF(.not.Lfatal.and.argok.and.nelt.eq.1)THEN
         CALL eltlen(1,tmpptr,nelt,Nttlcr)
         havttl=T
        END IF
        IF(Lfatal)RETURN
        GO TO 250
c-----------------------------------------------------------------------
c     File argument
c-----------------------------------------------------------------------
   60   IF(Havsrs)THEN
         CALL inpter(PERROR,Errpos,
     &               'Use either data or file, not both',T)
         locok=F
        END IF
        IF(Ldata)THEN
         CALL inpter(PERROR,Errpos,
     &       'Cannot use file argument when a data metafile is used.',T)
         locok=F
        END IF
c     ------------------------------------------------------------------
        CALL gtnmvc(LPAREN,T,1,file,tmpptr,nelt,PFILCR,argok,locok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.eq.1)THEN
         CALL eltlen(1,tmpptr,nelt,nflchr)
         IF(Lfatal)RETURN
         havfil=T
        END IF
        GO TO 250
c-----------------------------------------------------------------------
c     Format argument
c-----------------------------------------------------------------------
   70   CALL gtnmvc(LPAREN,T,1,fmt,tmpptr,nelt,PFILCR,argok,locok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.eq.1)THEN
         CALL eltlen(1,tmpptr,nelt,nfmtch)
         IF(Lfatal)RETURN
         havfmt=T
        END IF
        GO TO 250
c-----------------------------------------------------------------------
c     Print argument
c-----------------------------------------------------------------------
   80   CALL getprt(LSPSRS,NSPSRS,locok)
        GO TO 250
c-----------------------------------------------------------------------
c     Save argument
c-----------------------------------------------------------------------
   90   CALL getsav(LSPSRS,NSPSRS,locok)
        GO TO 250
c-----------------------------------------------------------------------
c     Series name argument
c-----------------------------------------------------------------------
  100   CALL gtnmvc(LPAREN,T,1,Srsnam,tmpptr,nelt,64,argok,locok)
        IF(Lfatal)RETURN
        IF(argok)THEN
         hvnam=T
         CALL eltlen(1,tmpptr,nelt,Nser)
        END IF
        GO TO 250
c-----------------------------------------------------------------------
c     precision argument
c-----------------------------------------------------------------------
  110   CALL getivc(LPAREN,T,1,ivec,nelt,argok,locok)
        IF(Lfatal)RETURN
        IF(argok)THEN
         IF(ivec(1).lt.0.or.ivec(1).gt.5)THEN
          CALL inpter(PERROR,Errpos,
     &  'Number of input decimals must be between 0 and 5, inclusive',T)
          locok=F
         ELSE
          numdec=ivec(1)
         END IF
        END IF
        GO TO 250
c-----------------------------------------------------------------------
c     decimals argument
c-----------------------------------------------------------------------
  120   CALL getivc(LPAREN,T,1,ivec,nelt,argok,locok)
        IF(Lfatal)RETURN
        IF(argok)THEN
         IF(ivec(1).lt.0.or.ivec(1).gt.5)THEN
          CALL inpter(PERROR,Errpos,
     & 'Number of output decimals must be between 0 and 5, inclusive',T)
          locok=F
         ELSE
          Kdec=ivec(1)
         END IF
        END IF
        GO TO 250
c-----------------------------------------------------------------------
c     Span for the model estimation.
c-----------------------------------------------------------------------
  130   CALL gtdtvc(Havesp,Sp,LPAREN,F,2,spnmdl,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(nelt.eq.1)THEN
         CALL inpter(PERROR,Errpos,
     &               'Need two dates for the model span or use a '//
     &               'comma as place holder.',T)
         Inptok=F
        ELSE IF(argok)THEN
         hvmdsp=T
        END IF
        GO TO 250
c-----------------------------------------------------------------------
c     Composite type argument
c-----------------------------------------------------------------------
  140   CALL gtdcvc(LPAREN,T,1,CMPDIC,cmpptr,PCMP,
     &       'Available composite types are none, add, sub, mult, div.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         Iag=ivec(1)-2
         IF(Iagr.eq.0.and.Iag.ge.0)Lagr=T
        END IF
        GO TO 250
c-----------------------------------------------------------------------
c     Composite adjustment weight argument
c-----------------------------------------------------------------------
  150   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
c--------*---------------------------------------------------------------
c     Error Checking for composite adjustment weight
c-----------------------------------------------------------------------
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).le.0D0)THEN
          CALL inpter(PERROR,Errpos,
     &                'Value of composite weight must be greater '//
     &                'than zero.',T)
          Inptok=F
         ELSE
          W=dvec(1)
         END IF
        END IF
        GO TO 250
c-----------------------------------------------------------------------
c     missingcode argument
c-----------------------------------------------------------------------
  160   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        Mvcode=dvec(1)
        GO TO 250
c-----------------------------------------------------------------------
c     missingval argument
c-----------------------------------------------------------------------
  170   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        Mvval=dvec(1)
        GO TO 250
c-----------------------------------------------------------------------
c     saveprecision argument
c-----------------------------------------------------------------------
  180   CALL getivc(LPAREN,T,1,ivec,nelt,argok,locok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         IF(ivec(1).le.0.and.ivec(1).gt.15)THEN
          CALL inpter(PERROR,Errpos,'Value of saveprecision must be '//
     &                'greater than zero and less than 15.',T)
          Inptok=F
         ELSE
          Svprec=ivec(1)
         END IF
        END IF
        GO TO 250
c-----------------------------------------------------------------------
c     yr2000 argument
c-----------------------------------------------------------------------
  190   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for yr2000 are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Yr2000=ivec(1).eq.1
        GO TO 250
c-----------------------------------------------------------------------
c     trimzero argument
c-----------------------------------------------------------------------
  200   CALL gtdcvc(LPAREN,T,1,ZRODIC,zroptr,PZRO,
     &            'Available options for trimzero are yes, span or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)ltrim=ivec(1)-1
        GO TO 250
c-----------------------------------------------------------------------
c     divpower argument
c-----------------------------------------------------------------------
  210   CALL getivc(LPAREN,T,1,ivec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         IF(ivec(1).lt.MINNIN.or.ivec(1).gt.NINE)THEN
          CALL inpter(PERROR,Errpos,
     &                'Value entered for divpower must be between -9 '//
     &                'and 9, inclusive.',T)
          Inptok=F
         ELSE
          Divpwr=ivec(1)
         END IF
        END IF
        GO TO 250
c-----------------------------------------------------------------------
c     appendfcst argument
c-----------------------------------------------------------------------
  220   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &       'Available options for appending forecasts are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Savfct=ivec(1).eq.1
        GO TO 250
c-----------------------------------------------------------------------
c     appendbcst argument
c-----------------------------------------------------------------------
  230   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &       'Available options for appending backcasts are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Savbct=ivec(1).eq.1
        GO TO 250
c-----------------------------------------------------------------------
c     type argument
c-----------------------------------------------------------------------
  240   CALL gtdcvc(LPAREN,T,1,TYPDIC,typptr,PTYP,
     &              'Available options for type are flow or stock.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Isrflw=ivec(1)
        GO TO 250
       END IF
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c      Check to see if 
c-----------------------------------------------------------------------
       IF(.not.Havesp.and.hvstrt)THEN
        Sp=12
        Havesp=T
       END IF
       IF(.not.Havesp.and.Sp.gt.12)Sp=12
c     ------------------------------------------------------------------
       IF(.not.isdate(Start,Sp))THEN
        CALL inpter(PERRNP,Pos,'Start date not valid',T)
        Havesp=F
        locok=F
       END IF
c-----------------------------------------------------------------------
c     copy span starting and ending dates into proper variables, and
c     check to see if they are valid dates (BCM, Nov 2004)
c-----------------------------------------------------------------------
       IF(spnvec(YR,1).ne.NOTSET)THEN
        CALL cpyint(spnvec,2,1,Begspn)
        IF(.not.isdate(Begspn,Sp))THEN
         CALL inpter(PERRNP,Pos,'Span starting date not valid',T)
         Havesp=F
         locok=F
        END IF
       END IF
       IF(spnvec(YR,2).ne.NOTSET)THEN
        CALL cpyint(spnvec(1,2),2,1,endspn)
        IF(.not.isdate(endspn,Sp))THEN
         CALL inpter(PERRNP,Pos,'Span ending date not valid',T)
         Havesp=F
         locok=F
        END IF
       END IF
       IF((spnvec(YR,1).eq.NOTSET.or.spnvec(YR,2).eq.NOTSET).and.
     &    ltrim.eq.1)THEN
        CALL inpter(PERRNP,Errpos,
     &    'Must specify starting and ending span when trimzero=span.',T)
        locok=F
       END IF
c-----------------------------------------------------------------------
c     If the data are from the file get the data
c-----------------------------------------------------------------------
       IF(locok.and.havfil.and.(.not.Havsrs))THEN
        CALL gtfldt(PLEN,file,nflchr,havfmt,fmt,nfmtch,ltrim,Y,Nobs,
     &              Havesp,Sp,hvnam,Srsnam,Nser,havttl,Srsttl,Nttlcr,
     &              numdec,hvstrt,Start,1,Begspn,endspn,F,argok,locok)
        IF(argok)Havsrs=T
       END IF
c-----------------------------------------------------------------------
c     Check for the required arguments
c-----------------------------------------------------------------------
       IF(.not.Havsrs)THEN
        IF(locok)THEN
         CALL inpter(PERRNP,Errpos,'No time series specified',T)
         locok=F
        ELSE IF(havfil)THEN
         CALL inpter(PERRNP,Errpos,'Time series could not be read '//
     &                             'due to previously found errors',T)
        END IF
       ELSE
c-----------------------------------------------------------------------
c     If beginning or ending date in span is undefined, set equal to
c     beginning date of series.
c-----------------------------------------------------------------------
        IF(spnvec(YR,1).eq.NOTSET)CALL cpyint(Start,2,1,Begspn)
        IF(spnvec(YR,2).eq.NOTSET)CALL addate(Start,Sp,Nobs-1,endspn)
c-----------------------------------------------------------------------
c     Check that the span is within the series
c-----------------------------------------------------------------------
        CALL dfdate(endspn,Begspn,Sp,Nspobs)
        Nspobs=Nspobs+1
        IF(.not.chkcvr(Start,Nobs,Begspn,Nspobs,Sp))THEN
         CALL inpter(PERRNP,Errpos,'Span not within the series',T)
         CALL cvrerr('series',Start,Nobs,'span',Begspn,Nspobs,Sp)
         IF(Lfatal)RETURN
         locok=F
        END IF
c-----------------------------------------------------------------------
c     If beginning or ending date in the model span is undefined, set
c     equal to beginning date of the span.
c-----------------------------------------------------------------------
        IF(spnmdl(YR,1).eq.NOTSET)THEN
         CALL cpyint(Begspn,2,1,Begmdl)
        ELSE
         CALL cpyint(spnmdl,2,1,Begmdl)
         IF(.not.isdate(Begmdl,Sp))THEN
          CALL inpter(PERRNP,Pos,'Model span starting date not valid',T)
          Havesp=F
          locok=F
         END IF
        END IF
        IF(spnmdl(YR,2).eq.NOTSET.or.spnmdl(YR,2).eq.0)THEN
         CALL addate(Begspn,Sp,Nspobs-1,Endmdl)
         IF(spnmdl(YR,2).eq.0)THEN
          Endmdl(MO)=spnmdl(MO,2)
          IF(Endmdl(MO).gt.Endspn(MO))Endmdl(YR)=Endmdl(YR)-1
          Fixper=Endmdl(MO)
         END IF
        ELSE
         CALL cpyint(spnmdl(1,2),2,1,Endmdl)
         IF(.not.isdate(Endmdl,Sp))THEN
          CALL inpter(PERRNP,Pos,'Model span ending date not valid',T)
          Havesp=F
          locok=F
         END IF
        END IF
c-----------------------------------------------------------------------
c     Check that the span is within the series
c-----------------------------------------------------------------------
        IF(hvmdsp)THEN
         CALL dfdate(Endmdl,Begmdl,Sp,nmdl)
         nmdl=nmdl+1
         IF(.not.chkcvr(Begspn,Nspobs,Begmdl,nmdl,Sp))THEN
          CALL inpter(PERRNP,Errpos,
     &            'Model span not within the span of available data.',T)
          CALL cvrerr('span',Begspn,Nspobs,'model span',Begmdl,nmdl,Sp)
          Inptok=F
          IF(Lfatal)RETURN
         END IF
        END IF
       END IF
c     ------------------------------------------------------------------
       IF(Iag.eq.NOTSET)Iag=-1
       IF(Isrflw.eq.NOTSET)Isrflw=0
c     ------------------------------------------------------------------
       Inptok=Inptok.and.locok
       RETURN
  250  CONTINUE
      END DO
c     ------------------------------------------------------------------
      END

