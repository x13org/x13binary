C     Last change:  Mar.2021, add a pass parameter Endspn to call gtpdrg
C     Last change:  BCM  28 Sep 99    3:17 pm
      SUBROUTINE gtxreg(Begsrs,Nobs,Endspn,Havsrs,Havesp,Muladd,
     &                  Xuserx,Bgusrx,Ixreg,Nusxrg,Sigxrg,Critxr,Otlxrg,
     &                  Umean,Begum,Haveum,Noxfac,Ladd1x,Xtdtst,Xeastr,
     &                  Xuser,Dwt,Ixrgtd,Ixrghl,Xhlnln,Xelong,Calfrc,
     &                  Begxrg,Endxrg,Fxprxr,Begxot,Endxot,Havxhl,
     &                  Havxtd,Axrghl,Axrgtd,Lxrneg,Cvxalf,Cvxtyp,
     &                  Cvxrdc,Xraicd,Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Specify the X-11 regression on the irregular
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'lex.i'
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'tbllog.i'
      INCLUDE 'svllog.i'
      INCLUDE 'usrxrg.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'htmlout.cmn'
c     ------------------------------------------------------------------
      DOUBLE PRECISION ONE,ZERO,PTONE
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.,ONE=1D0,ZERO=0D0,PTONE=0.1D0)
c     ------------------------------------------------------------------
      CHARACTER effttl*(PCOLCR),xrfile*(PFILCR),xrfmt*(PFILCR),
     &          umname*(64),umfile*(PFILCR),umfmt*(PFILCR),umtit*(80),
     &          cblank*(64)
      LOGICAL argok,Havesp,havfmt,Havsrs,haveux,hvfile,hvstrt,hvuttl,
     &        Inptok,Otlxrg,Haveum,hvumfl,hvumst,hvumft,Havxtd,Havxhl,
     &        hvumnm,Ladd1x,Xeastr,Xhlnln,Xelong,Calfrc,hvmdsp,hvotsp,
     &        Noxfac,Xuser,Axrghl,Axrgtd,havlp,havln,lumean,lprior,
     &        luseas,fixvec,Lxrneg,Cvxtyp
      INTEGER Bgusrx,Begsrs,i,j,k,idisp,itmpvc,nchr,nelt,nflchr,nfmtch,
     &        neltux,Nobs,peltux,Muladd,Ixreg,Nusxrg,igrp,numfch,
     &        neltum,numnam,Begum,numftc,Xtdtst,ivec,spnxrg,Begxrg,
     &        Endxrg,nxrg,spnotl,Begxot,Endxot,Ixrgtd,Ixrghl,Fxprxr,
     &        neltdw,Endspn,numdec,numtit,tmppa,i2,n2,k2,ispn,nbvec,
     &        begcol,endcol,ltrim
      DOUBLE PRECISION Xuserx,Sigxrg,Critxr,Umean,Dwt,dvec,Xraicd,
     &                 urmean,urnum,bvec,Cvxalf,Cvxrdc
      DIMENSION Bgusrx(2),Begsrs(2),itmpvc(0:1),Xuserx(*),spnotl(2,2),
     &          Begxot(2),Endxot(2),Umean(PLEN),Begum(2),Dwt(7),ivec(1),
     &          dvec(1),spnxrg(2,2),Begxrg(2),Endxrg(2),Endspn(2),
     &          urmean(PB),urnum(PB),ispn(2),fixvec(PB),bvec(PB)
c-----------------------------------------------------------------------
      INTEGER strinx
      LOGICAL chkcvr,gtarg,dpeq,istrue
      EXTERNAL chkcvr,gtarg,dpeq,strinx,istrue
c-----------------------------------------------------------------------
c     The spec dictionary was made with this command
c  ../../dictionary/strary < ../../dictionary/regression.dic
c-----------------------------------------------------------------------
      CHARACTER ARGDIC*271
      INTEGER argidx,argptr,PARG,arglog
      PARAMETER(PARG=36)
      DIMENSION argptr(0:PARG),arglog(2,PARG)
      PARAMETER(ARGDIC='variablesuserdatastartfileformatbprintsaveuserty
     &pesigmacriticalumdataumstartumfileumformatumnameoutliermethodaicte
     &sttdpriornoapplyholidaynonlineastermeansforcecalspanoutlierspanump
     &recisionaicdiffsavelogumtrimzerocenteruserreweightcriticalalphadef
     &aultcriticalprioralmost')
c-----------------------------------------------------------------------
      CHARACTER USXDIC*15
      INTEGER usxidx,usxptr,PUSX
      PARAMETER(PUSX=4)
      DIMENSION usxptr(0:PUSX),usxidx(PUREG)
      PARAMETER(USXDIC='tdaoholidayuser')
c-----------------------------------------------------------------------
      CHARACTER MTDDIC*12
      INTEGER mtdptr,PMTD
      PARAMETER(PMTD=2)
      DIMENSION mtdptr(0:PMTD)
      PARAMETER(MTDDIC='addoneaddall')
c-----------------------------------------------------------------------
      CHARACTER NAPDIC*9
      INTEGER napidx,napptr,PNAP
      PARAMETER(PNAP=2)
      DIMENSION napptr(0:PNAP),napidx(2)
      PARAMETER(NAPDIC='tdholiday')
c     ------------------------------------------------------------------
      CHARACTER XAICDC*38
      INTEGER xaicid,xaicpt,PXAIC
      PARAMETER(PXAIC=6)
      DIMENSION xaicpt(0:PXAIC),xaicid(3)
      PARAMETER(XAICDC='tdtdstocktd1coeftdstock1coefeasteruser')
c-----------------------------------------------------------------------
c     data dictionary of yes/no choice
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
      CHARACTER URRDIC*12
      INTEGER urrptr,PURR
      PARAMETER(PURR=2)
      DIMENSION urrptr(0:PURR)
      PARAMETER(URRDIC='meanseasonal')
c-----------------------------------------------------------------------
c     default critical value types dictionary 
c-----------------------------------------------------------------------
      CHARACTER DEFDIC*14
      INTEGER defptr,PDEF
      PARAMETER(PDEF=2)
      DIMENSION defptr(0:PDEF)
      PARAMETER(DEFDIC='ljungcorrected')
c-----------------------------------------------------------------------
      DATA argptr/1,10,14,18,23,27,33,34,39,43,51,56,64,70,77,83,91,97,
     &            110,117,124,131,144,155,163,167,178,189,196,203,213,
     &            223,231,244,259,264,272/
      DATA usxptr/1,3,5,12,16/
      DATA mtdptr/1,7,13/
      DATA napptr/1,3,10/
      DATA xaicpt/1,3,10,17,29,35,39/
      DATA ysnptr/1,4,6/
      DATA zroptr/1,4,8,10/
      DATA urrptr/1,5,13/
      DATA defptr/1,6,15/
c-----------------------------------------------------------------------
c     Assume the input is OK and we don't have any of the arguments
c-----------------------------------------------------------------------
      argok=T
      peltux=PLEN*PUREG
      haveux=F
      hvuttl=F
      hvfile=F
      havfmt=F
      hvstrt=F
      hvumnm=F
      hvumfl=F
      hvumft=F
      hvumst=F
      nfmtch=1
      numdec=0
      numftc=1
      hvmdsp=F
      hvotsp=F
      havlp=F
      havln=F
      ltrim=0
      lumean=F
      luseas=F
      neltdw=0
      nbvec=NOTSET
      CALL setlg(F,PB,fixvec)
      lprior=F
c-----------------------------------------------------------------------
      CALL setint(NOTSET,2*PARG,arglog)
      CALL setint(NOTSET,4,spnxrg)
      CALL setint(NOTSET,4,spnotl)
      CALL setint(NOTSET,2,ispn)
      CALL setint(0,PUREG,Usxtyp)
c-----------------------------------------------------------------------
c     Initialize the format and file
c-----------------------------------------------------------------------
      CALL setchr(' ',PFILCR,xrfile)
      CALL setchr(' ',PFILCR,xrfmt)
      CALL setchr(' ',64,cblank)
c-----------------------------------------------------------------------
c     Argument get loop
c-----------------------------------------------------------------------
      DO WHILE (T)
       IF(gtarg(ARGDIC,argptr,PARG,argidx,arglog,Inptok))THEN
        IF(Lfatal)RETURN
        GO TO(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,
     &        170,180,190,200,210,220,230,240,250,260,270,280,290,300,
     &        310,320,330,340,345,346),argidx
c-----------------------------------------------------------------------
c     Regression argument
c-----------------------------------------------------------------------
   10   CALL gtpdrg(Begsrs,Endspn,Nobs,Havsrs,Havesp,T,Havxtd,Havxhl,
     &              havln,havlp,argok,Inptok)
        IF(Lfatal)RETURN
        IF((.not.Lfatal).and.Picktd)THEN
         IF(Muladd.eq.NOTSET)THEN
         ELSE IF(Muladd.ne.1)THEN
          tmppa=NOTSET
          CALL rmlnvr(tmppa,0,Nspobs)
          IF(Lfatal)RETURN
         END IF
        END IF
        GO TO 350
c-----------------------------------------------------------------------
c     Names and number of columns for the user regression variables
c-----------------------------------------------------------------------
   20   CALL gtnmvc(LPAREN,T,PUREG,Usrxtt,Usrxpt,Ncxusx,PCOLCR,argok,
     &              Inptok)
        IF(Lfatal)RETURN
        hvuttl=argok.and.Ncxusx.gt.0
        GO TO 350
c-----------------------------------------------------------------------
c     Data argument
c-----------------------------------------------------------------------
   30   IF(hvfile)
     &     CALL inpter(PERROR,Errpos,'Getting data from a file',T)
c     ------------------------------------------------------------------
        CALL gtdpvc(LPAREN,T,peltux,Xuserx,neltux,argok,Inptok)
        IF(Lfatal)RETURN
        haveux=argok.and.neltux.gt.0
        GO TO 350
c-----------------------------------------------------------------------
c     Start argument
c-----------------------------------------------------------------------
   40   CALL gtdtvc(Havesp,Sp,LPAREN,F,1,Bgusrx,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        hvstrt=argok.and.nelt.gt.0
        GO TO 350
c-----------------------------------------------------------------------
c     File argument
c-----------------------------------------------------------------------
   50   IF(haveux)CALL inpter(PERROR,Errpos,
     &                        'Already have user regression',T)
        CALL gtnmvc(LPAREN,T,1,xrfile,itmpvc,neltux,PFILCR,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
        IF(argok.and.neltux.gt.0)THEN
         nflchr=itmpvc(1)-1
         hvfile=T
        END IF
        GO TO 350
c-----------------------------------------------------------------------
c     Format argument
c-----------------------------------------------------------------------
   60   CALL gtnmvc(LPAREN,T,1,xrfmt,itmpvc,nelt,PFILCR,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok)THEN
         nfmtch=itmpvc(1)-1
         havfmt=T
        END IF
        GO TO 350
c-----------------------------------------------------------------------
c     Initial values for the irregular regression.  May want to change 
c this later so that the betas only need take some initial values 
c instead of all or none.
c-----------------------------------------------------------------------
   70   CALL gtrgvl(nbvec,fixvec,bvec,Inptok)
        IF(Lfatal)RETURN
        GO TO 350
c-----------------------------------------------------------------------
c     Print argument
c-----------------------------------------------------------------------
   80   CALL getprt(LSPXRG,NSPXRG,Inptok)
        GO TO 350
c-----------------------------------------------------------------------
c     Save argument
c-----------------------------------------------------------------------
   90   CALL getsav(LSPXRG,NSPXRG,Inptok)
        GO TO 350
c-----------------------------------------------------------------------
c     usertype argument
c-----------------------------------------------------------------------
  100   CALL gtdcvc(LPAREN,F,PUREG,USXDIC,usxptr,PUSX,
     &              'Improper entry for usertype.  See '//SPCSEC//
     &              ' of '//DOCNAM//'.',usxidx,Nusxrg,T,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
        IF(argok.and.Nusxrg.gt.0)THEN
         DO i=1,Nusxrg
          IF(usxidx(i).eq.1)THEN
           Usxtyp(i)=PRGUTD
           Havxtd=T
          ELSE IF(usxidx(i).eq.2)THEN
           Usxtyp(i)=PRGUAO
          ELSE IF(usxidx(i).eq.3)THEN
           Usxtyp(i)=PRGTUH
           Havxhl=T
          ELSE IF(usxidx(i).eq.4.or.usxidx(i).eq.NOTSET)THEN
           Usxtyp(i)=PRGTUD
          END IF
         END DO
        END IF
        GO TO 350
c     ------------------------------------------------------------------
c     sigma argument
c     ------------------------------------------------------------------
  110   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     Error Checking for tdsigma
c     ------------------------------------------------------------------
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).le.0)THEN
          CALL inpter(PERROR,Errpos,
     &                'Trading day sigma limit must be greater than '//
     &                'zero.',T)
          Inptok=F
         ELSE
          Sigxrg=dvec(1)
         END IF
        END IF
        GO TO 350
c     ------------------------------------------------------------------
c     critical argument
c     ------------------------------------------------------------------
  120   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     Error Checking for tdsigma
c     ------------------------------------------------------------------
        IF(argok.and.nelt.gt.0)THEN
         IF(dpeq(dvec(1),ZERO))THEN
          Critxr=DNOTST
          Otlxrg=T
         ELSE IF(dvec(1).lt.0)THEN
          CALL inpter(PERROR,Errpos,
     &                'Critical value for outlier detection must be '//
     &                'greater than zero.',T)
          Inptok=F
         ELSE
          Critxr=dvec(1)
          Otlxrg=T
         END IF
        END IF
        GO TO 350
c-----------------------------------------------------------------------
c     usermean argument
c-----------------------------------------------------------------------
  130   IF(hvumfl)
     &     CALL inpter(PERROR,Errpos,'Getting mean from a file.',T)
c     ------------------------------------------------------------------
        CALL gtdpvc(LPAREN,T,PLEN,Umean,neltum,argok,Inptok)
        IF(Lfatal)RETURN
        Haveum=argok.and.neltum.gt.0
        GO TO 350
c-----------------------------------------------------------------------
c     umstart argument
c-----------------------------------------------------------------------
  140   CALL gtdtvc(Havesp,Sp,LPAREN,F,1,Begum,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        hvumst=argok.and.nelt.gt.0
        GO TO 350
c-----------------------------------------------------------------------
c     umfile argument
c-----------------------------------------------------------------------
  150   IF(Haveum)CALL inpter(PERROR,Errpos,
     &                        'Already have user effect mean.',T)
        CALL gtnmvc(LPAREN,T,1,umfile,itmpvc,neltum,PFILCR,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
        IF(argok.and.neltum.gt.0)THEN
         numfch=itmpvc(1)-1
         hvumfl=T
        END IF
        GO TO 350
c-----------------------------------------------------------------------
c     umformat argument
c-----------------------------------------------------------------------
  160   CALL gtnmvc(LPAREN,T,1,umfmt,itmpvc,nelt,PFILCR,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok)THEN
         numftc=itmpvc(1)-1
         hvumft=T
        END IF
        GO TO 350
c-----------------------------------------------------------------------
c     mean name argument
c-----------------------------------------------------------------------
  170   CALL gtnmvc(LPAREN,T,1,umname,itmpvc,nelt,64,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok)THEN
         numnam=itmpvc(1)-1
         hvumnm=T
        END IF
        GO TO 350
c-----------------------------------------------------------------------
c     outliermethod argument
c-----------------------------------------------------------------------
  180   CALL gtdcvc(LPAREN,T,1,MTDDIC,mtdptr,PMTD,
     &              'Choices are ADDONE or ADDALL',ivec,nelt,T,argok,
     &              Inptok)
        IF(Lfatal)RETURN
c----------------------------------------------------------------------
        IF(nelt.gt.0)Ladd1x=ivec(1).eq.1
        GO TO 350
c-----------------------------------------------------------------------
c     aictest argument
c-----------------------------------------------------------------------
  190   CALL gtdcvc(LPAREN,F,3,XAICDC,xaicpt,PXAIC,
     &    'Choices for aictest are td, tdstock, td1coef, tdstock1coef,',
     &              xaicid,nelt,F,argok,Inptok)
        IF(Lfatal)RETURN
        IF(.not.argok)CALL writln('        user, and easter.',
     &                            STDERR,Mt2,F,T)
        IF(argok)THEN
         DO i=1,nelt
          IF(xaicid(i).eq.5)THEN
           Xeastr=T
           Havxhl=T
          ELSE IF(xaicid(i).eq.6)THEN
           Xuser=T
          ELSE
           IF(Xtdtst.eq.0)THEN
            Xtdtst=xaicid(i)
            Havxtd=T
           ELSE
            CALL inpter(PERROR,Errpos,
     &         'Can only specify one type of trading day in aictest.',T)
            Inptok=F
           END IF
          END IF
         END DO
        END IF
        GO TO 350
c-----------------------------------------------------------------------
c     tdprior argument
c-----------------------------------------------------------------------
  200   CALL gtdpvc(LPAREN,T,7,Dwt,neltdw,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.neltdw.ne.7)THEN
         CALL inpter(PERROR,Errpos,
     &               'Must have seven prior trading day weights.',T)
         Inptok=F
        END IF
        GO TO 350
c-----------------------------------------------------------------------
c     noapply argument
c-----------------------------------------------------------------------
  210   CALL gtdcvc(LPAREN,T,3,NAPDIC,napptr,PNAP,
     &              'Choices are TD or HOLIDAY.',napidx,nelt,T,argok,
     &              Inptok)
        IF(Lfatal)RETURN
c----------------------------------------------------------------------
        IF(nelt.gt.0)THEN
         DO i=1,nelt
          IF(napidx(i).eq.1)THEN
           Ixrgtd=0
          ELSE
           Ixrghl=0
          END IF
         END DO 
        END IF
        GO TO 350
c-----------------------------------------------------------------------
c     holidaynonlin argument
c-----------------------------------------------------------------------
  220   CALL gtdcvc(LPAREN,F,1,YSNDIC,ysnptr,PYSN,
     &              'Choices for holidaynonlin are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Xhlnln=ivec(1).eq.1
        GO TO 350
c-----------------------------------------------------------------------
c     eastermeans argument
c-----------------------------------------------------------------------
  230   CALL gtdcvc(LPAREN,F,1,YSNDIC,ysnptr,PYSN,
     &              'Choices for eastermeans are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Xelong=ivec(1).eq.1
        GO TO 350
c-----------------------------------------------------------------------
c     forcecal argument
c-----------------------------------------------------------------------
  240   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Choices for forcecal are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Calfrc=ivec(1).eq.1
        GO TO 350
c-----------------------------------------------------------------------
c     Span for the irregular regression estimation.
c-----------------------------------------------------------------------
  250   CALL gtdtvc(Havesp,Sp,LPAREN,F,2,spnxrg,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(nelt.eq.1)THEN
         CALL inpter(PERROR,Errpos,
     &               'Need two dates for the irregular component '//
     &               'regression span or',F)
         CALL writln('       use a comma as a place holder.',STDERR,Mt2,
     &               F,T)
         Inptok=F
        ELSE IF(argok)THEN
         hvmdsp=T
        END IF
        GO TO 350
c-----------------------------------------------------------------------
c     Span argument for outlier identification
c-----------------------------------------------------------------------
  260   CALL gtdtvc(T,Sp,LPAREN,F,2,spnotl,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(nelt.eq.1)THEN
         CALL inpter(PERROR,Errpos,
     &               'Need two dates for the span or use a comma as '//
     &               'a place holder.',T)
         Inptok=F
        ELSE
         hvotsp=T
        END IF
        GO TO 350
c-----------------------------------------------------------------------
c     umprecision argument
c-----------------------------------------------------------------------
  270   CALL getivc(LPAREN,T,1,ivec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok)THEN
         IF(ivec(1).lt.0.or.ivec(1).gt.5)THEN
          CALL inpter(PERROR,Errpos,
     &                'Number of input decimals must be between 0 '//
     &                'and 5, inclusive',T)
          Inptok=F
         ELSE
          numdec=ivec(1)
         END IF
        END IF
        GO TO 350
c-----------------------------------------------------------------------
c     AIC test difference for the regression-based AIC test
c-----------------------------------------------------------------------
  280   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(nelt.gt.0.and.argok)Xraicd=dvec(1)
        GO TO 350
c-----------------------------------------------------------------------
c     savelog  argument
c-----------------------------------------------------------------------
  290   CALL getsvl(LSLXRG,NSLXRG,Inptok)
        GO TO 350
c-----------------------------------------------------------------------
c     umtrimzero argument
c-----------------------------------------------------------------------
  300   CALL gtdcvc(LPAREN,F,1,ZRODIC,zroptr,PZRO,
     &              'Choices for umtrimzero are yes, span or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)ltrim=ivec(1)-1
        GO TO 350
c-----------------------------------------------------------------------
c     centeruser argument
c-----------------------------------------------------------------------
  310   CALL gtdcvc(LPAREN,F,1,URRDIC,urrptr,PURR,
     &              'Choices for centeruser are mean and seasonal.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         lumean=ivec(1).eq.1
         luseas=ivec(1).eq.2
        END IF
        GO TO 350
c-----------------------------------------------------------------------
c     reweight argument
c-----------------------------------------------------------------------
  320   CALL gtdcvc(LPAREN,F,1,YSNDIC,ysnptr,PYSN,
     &              'Choices for reweight are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lxrneg=ivec(1).eq.1
        GO TO 350
c-----------------------------------------------------------------------
c     criticalalpha - alpha value for outlier critical value
c-----------------------------------------------------------------------
  330   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).le.ZERO.or.dvec(1).gt.PTONE)THEN
          CALL inpter(PERROR,Errpos,
     &           'Value of criticalalpha must be between 0 and 0.10.',T)
          Inptok=F
         ELSE
          Cvxalf=dvec(1)
         END IF
        END IF
        GO TO 350
c-----------------------------------------------------------------------
c     Default critical value generation method specification
c-----------------------------------------------------------------------
  340   CALL gtdcvc(LPAREN,T,1,DEFDIC,defptr,PDEF,
     &              'Choices are ljung or corrected.',ivec,nelt,T,argok,
     &              Inptok)
        IF(Lfatal)RETURN
c----------------------------------------------------------------------
        IF(nelt.gt.0.and.argok)Cvxtyp=ivec(1).eq.1
        GO TO 350
c-----------------------------------------------------------------------
c     holidaynonlin argument
c-----------------------------------------------------------------------
  345   CALL gtdcvc(LPAREN,F,1,YSNDIC,ysnptr,PYSN,
     &              'Choices for prior are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)lprior=ivec(1).eq.1
        GO TO 350
c-----------------------------------------------------------------------
c     almost - amount to reduce outlier critical value to identify
c                "almost" outliers
c-----------------------------------------------------------------------
  346   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).le.ZERO)THEN
          CALL inpter(PERROR,Errpos,
     &                'Value of almost must be greater than 0.',T)
          Inptok=F
         ELSE
          Cvxrdc=dvec(1)
         END IF
        END IF
        GO TO 350
       END IF
c-----------------------------------------------------------------------
       IF(nbvec.ne.NOTSET)THEN
        IF(nbvec.gt.0.and.nbvec.NE.(Nb+Ncxusx))THEN
         CALL eWritln('Number of initial values is not the same as '//
     &                'the number of',STDERR,Mt2,T,F)
         CALL writln('       regression variables.',STDERR,Mt2,F,T)
        ELSE
         DO i=1,Nb+Ncxusx
          Regfx(i)=fixvec(i)
          B(i)=bvec(i)
         END DO
        END IF
       END IF
c-----------------------------------------------------------------------
c     If the data are from the file get the data
c-----------------------------------------------------------------------
       IF(Inptok.and.hvfile.and..not.haveux)
     &    CALL gtfldt(peltux,xrfile,nflchr,havfmt,xrfmt,nfmtch,2,Xuserx,
     &                neltux,Havesp,Sp,F,cblank,0,F,' ',0,0,hvstrt,
     &                Bgusrx,Ncxusx,ispn,ispn,T,haveux,Inptok)
c-----------------------------------------------------------------------
c     If beginning or ending date in the model span is undefined, set
c     equal to beginning date of the span.
c-----------------------------------------------------------------------
       IF(spnxrg(YR,1).eq.NOTSET)THEN
        CALL cpyint(Begspn,2,1,Begxrg)
       ELSE
        CALL cpyint(spnxrg,2,1,Begxrg)
       END IF
       IF(spnxrg(YR,2).eq.NOTSET.or.spnxrg(YR,2).eq.0)THEN
        CALL addate(Begspn,Sp,Nspobs-1,Endxrg)
        IF(spnxrg(YR,2).eq.0)THEN
         Endxrg(MO)=spnxrg(MO,2)
         IF(Endxrg(MO).gt.Endspn(MO))Endxrg(YR)=Endxrg(YR)-1
         Fxprxr=Endxrg(MO)
        END IF
       ELSE
        CALL cpyint(spnxrg(1,2),2,1,Endxrg)
       END IF
c-----------------------------------------------------------------------
c     Check that the span is within the series
c-----------------------------------------------------------------------
       IF(hvmdsp)THEN
        CALL dfdate(Endxrg,Begxrg,Sp,nxrg)
        nxrg=nxrg+1
        IF(.not.chkcvr(Begspn,Nspobs,Begxrg,nxrg,Sp))THEN
         CALL inpter(PERRNP,Errpos,
     &               'Irregular component regression span not within '//
     &               'the span of available data.',T)
         CALL cvrerr('span',Begspn,Nspobs,
     &               'irregular component regression span',Begxrg,nxrg,
     &               Sp)
         IF(Lfatal)RETURN
         Inptok=F
        END IF
       END IF
c-----------------------------------------------------------------------
c     set span for outlier test
c-----------------------------------------------------------------------
       IF(spnotl(YR,1).eq.NOTSET)THEN
        CALL cpyint(Begspn,2,1,Begxot)
       ELSE
        CALL cpyint(spnotl,2,1,Begxot)
       END IF
       IF(spnotl(YR,2).eq.NOTSET)THEN
        CALL addate(Begsrs,Sp,Nobs-1,Endxot)
       ELSE
        CALL cpyint(spnotl(1,2),2,1,Endxot)
       END IF
c----------------------------------------------------------------------
c     Check that the span is within the series
c----------------------------------------------------------------------
       IF(hvotsp)THEN
        CALL dfdate(Endxot,Begxot,Sp,nelt)
        nelt=nelt+1
        CALL dfdate(Endxrg,Begxrg,Sp,nmdl)
        nmdl=nmdl+1
        IF(.not.chkcvr(Begsrs,Nobs,Begxot,nelt,Sp))THEN
         CALL inpter(PERROR,Errpos,'Span not within the series',T)
         CALL cvrerr('Series',Begsrs,Nobs,'outlier test span',Begxot,
     &               nelt,Sp)
         Inptok=F
        ELSE IF(.not.chkcvr(Begxrg,nmdl,Begxot,nelt,Sp))THEN
         CALL inpter(PERROR,Errpos,'Span not within the model span',T)
         CALL cvrerr('Model span',Begxrg,nmdl,'outlier test span',
     &               Begxot,nelt,Sp)
         Inptok=F
        END IF
        IF(Lfatal)RETURN
       END IF
c-----------------------------------------------------------------------
c     Check for the required arguments
c-----------------------------------------------------------------------
       IF(Inptok.and.(hvuttl.or.haveux))THEN
        IF(.not.(hvuttl.eqv.haveux))THEN
         CALL eWritln('Need to specify both user-defined irregular '//
     &                'component',STDERR,Mt2,T,F)
         CALL writln('        regression variables and X-matrix.',
     &               STDERR,Mt2,F,T)
         Inptok=F
c     ------------------------------------------------------------------
        ELSE IF(mod(neltux,Ncxusx).ne.0)THEN
         WRITE(STDERR,1020)neltux,Ncxusx
         WRITE(Mt2,1021)neltux,Cbr,Ncxusx
 1020    FORMAT(/,' ERROR: Number of user-defined X elements=',i4,
     &          /,'        not equal to a multiple of the number of ',
     &            'columns=',i3,'.',/)
 1021    FORMAT(/,' <p><strong>ERROR:</strong> Number of user-',
     &            'defined X elements=',i4,a,
     &          /,' not equal to a multiple of the number of columns=',
     &            i3,'.</p>',/)
         Inptok=F
c     ------------------------------------------------------------------
        ELSE
         IF(.not.hvstrt)CALL cpyint(Begsrs,2,1,Bgusrx)
         Nrxusx=neltux/Ncxusx
         IF(.not.chkcvr(Bgusrx,Nrxusx,Begspn,Nspobs,Sp))THEN
          CALL cvrerr('user-defined regression variables',Bgusrx,Nrxusx,
     &                'span of the data',Begspn,Nspobs,Sp)
          IF(Lfatal)RETURN
          Inptok=F
c     ------------------------------------------------------------------
         ELSE
          idisp=Grp(Ngrp)-1
          DO i=1,Ncxusx
           idisp=idisp+1
           CALL getstr(Usrxtt,Usrxpt,Ncxusx,i,effttl,nchr)
           IF(.not.Lfatal)THEN
            IF(Usxtyp(i).eq.PRGTUH)THEN
             CALL adrgef(B(idisp),effttl(1:nchr),'User-defined Holiday',
     &                   Usxtyp(i),Regfx(idisp),T)
            ELSE IF(Usxtyp(i).eq.PRGUTD)THEN
             CALL adrgef(B(idisp),effttl(1:nchr),
     &                   'User-defined Trading Day',
     &                   Usxtyp(i),Regfx(idisp),T)
            ELSE IF(Usxtyp(i).eq.PRGUAO)THEN
             CALL adrgef(B(idisp),effttl(1:nchr),'User-defined AO',
     &                   Usxtyp(i),Regfx(idisp),T)
            ELSE
             CALL adrgef(B(idisp),effttl(1:nchr),'User-defined',PRGTUD,
     &                    Regfx(idisp),T)
            END IF
           END IF
           IF(Lfatal)RETURN
          END DO
c     ------------------------------------------------------------------
c     estimate and Remove either regressor mean or seasonal mean
c     ------------------------------------------------------------------
          IF(lumean)THEN
           CALL setdp(ZERO,PB,urmean)
           DO i=1,neltux
            i2=MOD(i,Ncxusx)
            IF(i2.eq.0)i2=Ncxusx
            urmean(i2)=urmean(i2)+Xuserx(i)
           END DO
           DO i=1,Ncxusx
            urmean(i)=urmean(i)/DBLE(Nrxusx)
           END DO
           DO i=1,neltux
            i2=MOD(i,Ncxusx)
            IF(i2.eq.0)i2=Ncxusx
            Xuserx(i)=Xuserx(i)-urmean(i2)
           END DO
          ELSE IF(luseas)THEN
           n2=Sp*Ncxusx
           DO i=1,Sp
            CALL setdp(ZERO,PB,urmean)
            CALL setdp(ZERO,PB,urnum)
            i2=(i-1)*Ncxusx+1
            DO j=i2,neltux,n2
             DO k=j,Ncxusx+j-1
              k2=MOD(k,Ncxusx)
              IF(k2.eq.0)k2=Ncxusx
              urmean(k2)=urmean(k2)+Xuserx(k)
              urnum(k2)=urnum(k2)+ONE
             END DO
            END DO
            DO j=1,Ncxusx
             urmean(j)=urmean(j) / urnum(j)
            END DO
            DO j=i2,neltux,n2
             DO k=j,Ncxusx+j-1
              k2=MOD(k,Ncxusx)
              IF(k2.eq.0)k2=Ncxusx
              Xuserx(k)=Xuserx(k)-urmean(k2)
             END DO
            END DO
           END DO
          END IF
c     ------------------------------------------------------------------
         END IF
        END IF
       END IF
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Read in and check the means for the user-defined regression 
c     effects
c-----------------------------------------------------------------------
       IF(Inptok.and.(hvumfl.or.Haveum))THEN
        IF(Ncxusx.eq.0)THEN
         CALL eWritln('User-defined mean can only be specified if ',
     &                STDERR,Mt2,T,F)
         CALL writln(
     &              '        user defined regressors are also present.',
     &                STDERR,Mt2,F,T)
         Inptok=F
        ELSE IF(hvumfl.and..not.Haveum)THEN
         CALL gtfldt(PLEN,umfile,numfch,hvumft,umfmt,numftc,ltrim,Umean,
     &               neltum,Havesp,Sp,hvumnm,umname,numnam,hvuttl,umtit,
     &               numtit,numdec,hvumst,Begum,1,Begspn,Endspn,F,
     &               Haveum,Inptok)
        END IF
        IF(Inptok.and.Haveum)THEN
         IF(.not.hvumst)CALL cpyint(Begsrs,2,1,Begum)
         IF(.not.chkcvr(Begum,neltum,Begspn,Nspobs,Sp))THEN
          CALL cvrerr('user-defined mean effects',Begum,neltum,
     &                'span of the data',Begspn,Nspobs,Sp)
          IF(Lfatal)RETURN
          Inptok=F
         END IF
        END IF
       END IF
c-----------------------------------------------------------------------
c     Check if the regression model parameters are fixed.  Sets iregfx.
c-----------------------------------------------------------------------
       IF(Nb.gt.0)THEN
c-----------------------------------------------------------------------
c     check user-defined X-11 regression type selection.  First, check 
c     to see if user-defined X-11 regression variables are defined.
c-----------------------------------------------------------------------
        IF(Nusxrg.gt.0)THEN
         igrp=strinx(F,Grpttl,Grpptr,1,Ngrptl,'User-defined')
         IF(igrp.eq.0)
     &      igrp=strinx(F,Grpttl,Grpptr,1,Ngrptl,'User-defined Holiday')
         IF(igrp.eq.0)THEN
          CALL eWritln('Cannot specify group types for user-defined '//
     &                 'irregular component',STDERR,Mt2,T,F)
          CALL writln('        regression variables if user-defined '//
     &           'irregular component',STDERR,Mt2,F,F)
          CALL writln('        regression variables are not defined '//
     &           'in the x11regression spec.',STDERR,Mt2,F,T)
          Inptok=F
         END IF
c----------------------------------------------------------------------
c     If only one type given, use it for all user-defined regression 
c     variables.
c-----------------------------------------------------------------------
         IF(Nusxrg.eq.1)THEN
          DO i=2,Ncxusx
           Usxtyp(i)=Usxtyp(1)
          END DO
         END IF
        END IF
c-----------------------------------------------------------------------
c     Check if the regression model parameters are fixed.  Sets iregfx.
c-----------------------------------------------------------------------
        CALL regfix()
c     ------------------------------------------------------------------
c     set indicator variable for fixed user defined regressors.
c     ------------------------------------------------------------------
        Userfx=F
        IF(Nusxrg.gt.0.and.Iregfx.ge.2)THEN
         IF(Iregfx.eq.3)THEN
          Userfx=T
         ELSE
          igrp=strinx(F,Grpttl,Grpptr,1,Ngrptl,'User-defined')
          begcol=Grp(igrp-1)
          endcol=Grp(igrp)-1
          Userfx=istrue(Regfx,begcol,endcol)
         END IF
        END IF
c-----------------------------------------------------------------------
c     sort outlier regressors specified by the user, if any.
c-----------------------------------------------------------------------
        CALL otsort()
       END IF
c-----------------------------------------------------------------------
       Inptok=Inptok.and.argok
       IF(Inptok)THEN
        IF(Nb.gt.0.or.Xeastr.or.Xtdtst.gt.0)Ixreg=1
        IF(Ixreg.gt.0.and.lprior)Ixreg=2
        IF(.not.Havxtd)Ixrgtd=0
        IF(Ixrgtd.gt.0)Axrgtd=T
        IF(.not.Havxhl)Ixrghl=0
        IF(Ixrghl.gt.0)Axrghl=T
c        IF(dpeq(Sigxrg,DNOTST))Sigxrg=2.5D0
        IF(.not.(Axrgtd.or.Axrghl.or.neltdw.gt.0))THEN
         CALL eWritln('Must adjust for either trading day or holiday '//
     &                'in the x11regression spec.',STDERR,Mt2,T,T)
         Inptok=F
        END IF
c-----------------------------------------------------------------------
        Noxfac=Haveum.and.Havxtd.and.Havxhl
        IF(Noxfac.and.(Ixrgtd.eq.0.or.Ixrghl.eq.0))THEN
         CALL eWritln('Cannot specify noapply when user-defined '//
     &                'mean is also present.',STDERR,Mt2,T,T)
         Inptok=F
        END IF
       END IF
c----------------------------------------------------------------------
       RETURN
  350  CONTINUE
      END DO
c     -----------------------------------------------------------------
      END
