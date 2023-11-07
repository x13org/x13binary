C     Last change: Otc, 2021 - add trendtc argument in regression
C     previous change:Mar. 2021
C     previous change:  BCM  28 Sep 99    2:46 pm
      SUBROUTINE getreg(Begsrs,Endmdl,Nobs,Havsrs,Havesp,Userx,Nrusrx,
     &                  Bgusrx,Itdtst,Leastr,Eastst,Luser,Lttc,Elong,
     &                  Adjtd,Adjao,Adjls,Adjtc,Adjso,Adjhol,Adjsea,
     &                  Adjcyc,Adjusr,Nusrrg,Havtca,Rgaicd,Lam,Fcntyp,
     &                  Havhol,Lomtst,Ch2tst,Chi2cv,Tlimit,Pvaic,Lceaic,
     &                  Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     getreg.f, Release 1, Subroutine Version 1.6, Modified 03 Feb 1995.
c-----------------------------------------------------------------------
c     Specify the regression and time series parts of the model
c-----------------------------------------------------------------------
c     Code added to incorporate automatic TD selection
c     BCM - January 1994
c-----------------------------------------------------------------------
c     Add Endmdl as argument to getreg, gtpdrg for a new format of the
c     end of the series for sequence outliers such as
c     AOSdate-0.0/LSSdate-0.0
c     Mar. 2021
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
      INCLUDE 'usrreg.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      DOUBLE PRECISION ONE,ZERO
      LOGICAL F,T
      PARAMETER(ONE=1D0,ZERO=0D0,F=.false.,T=.true.)
c     ------------------------------------------------------------------
      CHARACTER effttl*(PCOLCR),rgfile*(PFILCR),rgfmt*(PFILCR),clen*(4),
     &          clim*(4),cblank*(64)
      LOGICAL argok,Havesp,havfmt,Havsrs,haveux,hvfile,hvstrt,hvuttl,
     &        Inptok,Elong,havtd,Havhol,havln,havlp,Luser,Havtca,
     &        lumean,luseas,fixvec,havcyc,herror,Ch2tst,Leastr,Lceaic,
     &        hvaicd,hvpva,locok,Lttc
      INTEGER Bgusrx,Begsrs,Endmdl,i,j,k,idisp,itmpvc,nchr,nelt,nflchr,
     &        nfmtch,neltux,Nobs,Lomtst,Nrusrx,peltux,Itdtst,ivec,igrp,
     &        i2,n2,k2,ispn,Adjtd,Adjao,Adjls,Adjtc,Adjso,Adjhol,Adjsea,
     &        Adjcyc,Adjusr,Nusrrg,nbvec,icol,ic1,Fcntyp,begcol,endcol,
     &        iuhl,Eastst,ielt,ip1,ip2,rtype
      DOUBLE PRECISION Userx,dvec,Rgaicd,urmean,urnum,bvec,Lam,Chi2cv,
     &                 daicdf,Tlimit,Pvaic
      DIMENSION Bgusrx(2),Begsrs(2),Endmdl(2),itmpvc(0:1),Userx(*),
     &          ivec(1),dvec(1),urmean(PB),urnum(PB),ispn(2),fixvec(PB),
     &          bvec(PB),iuhl(PUHLGP),Rgaicd(PAICT),daicdf(PAICT)
c-----------------------------------------------------------------------
      INTEGER strinx
      LOGICAL chkcvr,gtarg,dpeq,istrue
      EXTERNAL strinx,chkcvr,gtarg,dpeq,istrue
c-----------------------------------------------------------------------
c     The spec dictionary was made with this command
c  ../../dictionary/strary < ../../dictionary/regression.dic
c-----------------------------------------------------------------------
      CHARACTER ARGDIC*158
      INTEGER argidx,argptr,PARG,arglog
      PARAMETER(PARG=23)
      DIMENSION argptr(0:PARG),arglog(2,PARG)
      PARAMETER(ARGDIC='variablesuserdatastartfileformatbprintsaveaictes
     &teastermeansnoapplyusertypetcrateaicdiffsavelogcenteruserchi2testc
     &hi2testcvtlimitpvaictesttestalleastertrendtc')
c-----------------------------------------------------------------------
      CHARACTER YSNDIC*5
      INTEGER ysnptr,PYSN
      PARAMETER(PYSN=2)
      DIMENSION ysnptr(0:PYSN)
      PARAMETER(YSNDIC='yesno')
c     ------------------------------------------------------------------
      CHARACTER AICDIC*82
      INTEGER aicidx,aicptr,PAIC
      PARAMETER(PAIC=12)
      DIMENSION aicptr(0:PAIC),aicidx(4)
      PARAMETER(AICDIC='tdtdnolpyeartdstocktd1coeftd1nolpyeartdstock1coe
     &feastereasterstockuserlomloqlpyear')
c-----------------------------------------------------------------------
      CHARACTER URGDIC*89
      INTEGER urgidx,urgptr,PURG
      PARAMETER(PURG=16)
      DIMENSION urgptr(0:PURG),urgidx(PURG)
      PARAMETER(URGDIC='constantseasonaltdlomloqlpyearholidayholiday2hol
     &iday3holiday4holiday5aolssotransitoryuser')
c     ------------------------------------------------------------------
      CHARACTER MDLDIC*33
      INTEGER mdlind,mdlptr,PMODEL
      PARAMETER(PMODEL=8)
      DIMENSION mdlptr(0:PMODEL),mdlind(PMODEL)
      PARAMETER(MDLDIC='tdaolsholidayuserseasonalusertcso')
c     ------------------------------------------------------------------
      CHARACTER URRDIC*12
      INTEGER urrptr,PURR
      PARAMETER(PURR=2)
      DIMENSION urrptr(0:PURR)
      PARAMETER(URRDIC='meanseasonal')
c     ------------------------------------------------------------------
      DATA argptr/1,10,14,18,23,27,33,34,39,43,50,61,68,76,82,89,96,106,
     &            114,124,130,139,152,159/
      DATA ysnptr/1,4,6/
      DATA aicptr/1,3,13,20,27,38,50,56,67,71,74,77,83/
      DATA urgptr/1,9,17,19,22,25,31,38,46,54,62,70,72,74,76,86,90/
      DATA mdlptr/1,3,5,7,14,26,30,32,34/
      DATA urrptr/1,5,13/
c-----------------------------------------------------------------------
c     Assume the input is OK and we don't have any of the arguments
c-----------------------------------------------------------------------
      locok=T
      peltux=PLEN*PUREG
      haveux=F
      hvuttl=F
      hvfile=F
      havfmt=F
      hvstrt=F
      nfmtch=1
      havtd=F
      Havhol=F
      havln=F
      havlp=F
      havcyc=F
      lumean=F
      luseas=F
      hvaicd=F
      hvpva=F
      nbvec=NOTSET
      CALL setlg(F,PB,fixvec)
c-----------------------------------------------------------------------
      CALL setint(NOTSET,2*PARG,arglog)
      CALL setint(NOTSET,2,ispn)
      CALL setint(0,PUHLGP,iuhl)
c-----------------------------------------------------------------------
c     Initialize the format and file
c-----------------------------------------------------------------------
      CALL setchr(' ',PFILCR,rgfile)
      CALL setchr(' ',PFILCR,rgfmt)
      CALL setchr(' ',64,cblank)
c-----------------------------------------------------------------------
c     Argument get loop
c-----------------------------------------------------------------------
      DO WHILE (T)
       IF(gtarg(ARGDIC,argptr,PARG,argidx,arglog,Inptok))THEN
        IF(Lfatal)RETURN
        GO TO(10,20,30,40,50,60,70,80,90,100,110,120,130,150,160,170,
     &        140,180,190,191,192,193,199)argidx
c-----------------------------------------------------------------------
c     variables argument
c-----------------------------------------------------------------------
c     Add Endmdl as an argument to gtpdrg
   10   CALL gtpdrg(Begsrs,Endmdl,Nobs,Havsrs,Havesp,F,havtd,
     &              Havhol,havln,havlp,argok,locok)
        IF(Lfatal)RETURN
        GO TO 200
c-----------------------------------------------------------------------
c     Names and number of columns for the user regression variables
c-----------------------------------------------------------------------
   20   CALL gtnmvc(LPAREN,T,PUREG,Usrttl,Usrptr,Ncusrx,PCOLCR,argok,
     &              locok)
        IF(Lfatal)RETURN
        hvuttl=argok.and.Ncusrx.gt.0
        GO TO 200
c-----------------------------------------------------------------------
c     Data argument
c-----------------------------------------------------------------------
   30   IF(hvfile)
     &     CALL inpter(PERROR,Errpos,'Getting data from a file',T)
c     ------------------------------------------------------------------
        CALL gtdpvc(LPAREN,T,peltux,Userx,neltux,argok,locok)
        IF(Lfatal)RETURN
        haveux=argok.and.neltux.gt.0
        GO TO 200
c-----------------------------------------------------------------------
c     Start argument
c-----------------------------------------------------------------------
   40   CALL gtdtvc(Havesp,Sp,LPAREN,F,1,Bgusrx,nelt,argok,locok)
        IF(Lfatal)RETURN
        hvstrt=argok.and.nelt.gt.0
        GO TO 200
c-----------------------------------------------------------------------
c     File argument
c-----------------------------------------------------------------------
   50   IF(haveux)CALL inpter(PERROR,Errpos,
     &                        'Already have user regression',T)
        CALL gtnmvc(LPAREN,T,1,rgfile,itmpvc,neltux,PFILCR,argok,locok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
        IF(argok.and.neltux.gt.0)THEN
         CALL eltlen(1,itmpvc,neltux,nflchr)
         IF(Lfatal)RETURN
         hvfile=T
        END IF
        GO TO 200
c-----------------------------------------------------------------------
c     Format argument
c-----------------------------------------------------------------------
   60   CALL gtnmvc(LPAREN,T,1,rgfmt,itmpvc,nelt,PFILCR,argok,locok)
        IF(Lfatal)RETURN
        IF(argok)THEN
         nfmtch=itmpvc(1)-1
         havfmt=T
        END IF
        GO TO 200
c-----------------------------------------------------------------------
c     Initial values for the regression.  May want to change this
c later so that the betas only need take some initial values instead
c of all or none.
c-----------------------------------------------------------------------
   70   CALL gtrgvl(nbvec,fixvec,bvec,locok)
        IF(Lfatal)RETURN
        GO TO 200
c-----------------------------------------------------------------------
c     Print argument
c-----------------------------------------------------------------------
   80   CALL getprt(LSPREG,NSPREG,locok)
        GO TO 200
c-----------------------------------------------------------------------
c     Save argument
c-----------------------------------------------------------------------
   90   CALL getsav(LSPREG,NSPREG,locok)
        GO TO 200
c-----------------------------------------------------------------------
c     aictest argument
c-----------------------------------------------------------------------
  100   CALL gtdcvc(LPAREN,F,4,AICDIC,aicptr,PAIC,'Choices for aictest a
     &re td, tdnolpyear, tdstock, td1coef, td1nolpyear,',
     &              aicidx,nelt,F,argok,locok)
        IF(Lfatal)RETURN
        IF(.not.argok)THEN
         CALL writln('        tdstock1coef, lom, loq, '//
     &      'lpyear, easter, easterstock, and user.',STDERR,Mt2,F,T)
        END IF
        IF(argok)THEN
         DO i=1,nelt
          IF(aicidx(i).eq.7.or.aicidx(i).eq.8)THEN
           Leastr=T
           IF(Eastst.eq.0)THEN
            Eastst=aicidx(i)-6
           ELSE
            CALL inpter(PERROR,Errpos,
     &   'Can only specify one of easter and easterstock in aictest.',T)
            locok=F
           END IF
*           Havhol=T
          ELSE IF(aicidx(i).eq.9)THEN
           Luser=T
c-----------------------------------------------------------------------
c      input for Lomtst  (BCM March 2008)
c-----------------------------------------------------------------------
          ELSE IF(aicidx(i).gt.9)THEN
           IF(Lomtst.eq.0)THEN
            Lomtst=aicidx(i)-9
           ELSE
            CALL inpter(PERROR,Errpos,
     &      'Can only specify one of lom, loq, or lpyear in aictest.',T)
            locok=F
           END IF
          ELSE
           IF(Itdtst.eq.0)THEN
            Itdtst=aicidx(i)
*            havtd=T
           ELSE
            CALL inpter(PERROR,Errpos,
     &         'Can only specify one type of trading day in aictest.',T)
            locok=F
           END IF
          END IF
         END DO
         IF(locok)Iregfx=0
        END IF
        GO TO 200
c-----------------------------------------------------------------------
c     eastermeans argument
c-----------------------------------------------------------------------
  110   CALL gtdcvc(LPAREN,F,1,YSNDIC,ysnptr,PYSN,
     &              'Choices for eastermeans are yes and no.',
     &              ivec,nelt,T,argok,locok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Elong=ivec(1).eq.1
        GO TO 200
c-----------------------------------------------------------------------
c     noapply argument
c-----------------------------------------------------------------------
  120   CALL gtdcvc(LPAREN,T,PMODEL,MDLDIC,mdlptr,PMODEL,'Choices for th
     &e noapply argument are td, ao, ls, holiday, or user.',
     &              mdlind,nelt,T,argok,locok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
        IF(argok.and.nelt.gt.0)THEN
         DO i=1,nelt
          IF(mdlind(i).eq.1)THEN
           Adjtd=-1
          ELSE IF(mdlind(i).eq.2)THEN
           Adjao=-1
          ELSE IF(mdlind(i).eq.3)THEN
           Adjls=-1
          ELSE IF(mdlind(i).eq.4)THEN
           Adjhol=-1
          ELSE IF(mdlind(i).eq.5)THEN
           Adjsea=-1
          ELSE IF(mdlind(i).eq.6)THEN
           Adjusr=-1
          ELSE IF(mdlind(i).eq.7)THEN
           Adjtc=-1
          ELSE IF(mdlind(i).eq.8)THEN
           Adjso=-1
          END IF
         END DO
        END IF
        GO TO 200
c-----------------------------------------------------------------------
c     usertype argument
c-----------------------------------------------------------------------
  130   CALL gtdcvc(LPAREN,F,PUREG,URGDIC,urgptr,PURG,
     &              'Improper entry for usertype.  See '//SPCSEC//
     &              ' of '//DOCNAM//'.',urgidx,Nusrrg,T,argok,locok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
        IF(argok.and.Nusrrg.gt.0)THEN
         DO i=1,Nusrrg
          IF(urgidx(i).eq.1)THEN
           Usrtyp(i)=PRGUCN
          ELSE IF(urgidx(i).eq.2)THEN
           Usrtyp(i)=PRGTUS
          ELSE IF(urgidx(i).eq.3)THEN
           Usrtyp(i)=PRGUTD
           havtd=T
          ELSE IF(urgidx(i).eq.4)THEN
           Usrtyp(i)=PRGULM
           IF(.not.havln)havln=T
          ELSE IF(urgidx(i).eq.5)THEN
           Usrtyp(i)=PRGULQ
           IF(.not.havln)havln=T
          ELSE IF(urgidx(i).eq.6)THEN
           Usrtyp(i)=PRGULY
           IF(.not.havlp)havlp=T
          ELSE IF(urgidx(i).ge.7.and.urgidx(i).le.11)THEN
           IF(.not.Havhol)Havhol=T
           IF(iuhl(urgidx(i)-6).eq.0)iuhl(urgidx(i)-6)=1
           IF(urgidx(i).eq.7)THEN
            Usrtyp(i)=PRGTUH
           ELSE IF(urgidx(i).eq.8)THEN
            Usrtyp(i)=PRGUH2
           ELSE IF(urgidx(i).eq.9)THEN
            Usrtyp(i)=PRGUH3
           ELSE IF(urgidx(i).eq.10)THEN
            Usrtyp(i)=PRGUH4
           ELSE IF(urgidx(i).eq.11)THEN
            Usrtyp(i)=PRGUH5
           END IF
          ELSE IF(urgidx(i).eq.12)THEN
           Usrtyp(i)=PRGUAO
          ELSE IF(urgidx(i).eq.13)THEN
           Usrtyp(i)=PRGULS
          ELSE IF(urgidx(i).eq.14)THEN
           Usrtyp(i)=PRGUSO
          ELSE IF(urgidx(i).eq.15)THEN
           Usrtyp(i)=PRGUCY
           IF(.not.havcyc)havcyc=T
          ELSE IF(urgidx(i).eq.16.or.urgidx(i).eq.NOTSET)THEN
           Usrtyp(i)=PRGTUD
          END IF
         END DO
        END IF
        GO TO 200
c-----------------------------------------------------------------------
c     centeruser argument
c-----------------------------------------------------------------------
  140   CALL gtdcvc(LPAREN,F,1,URRDIC,urrptr,PURR,
     &              'Choices for centeruser are mean and seasonal.',
     &              ivec,nelt,T,argok,locok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         lumean=ivec(1).eq.1
         luseas=ivec(1).eq.2
        END IF
        GO TO 200
c-----------------------------------------------------------------------
c     tcrate - alpha value for all TC outliers
c-----------------------------------------------------------------------
  150   IF(Havtca)THEN
         CALL inpter(PERROR,Errpos,
     &               'Cannot specify tcrate in both the regression '//
     &               'and outlier specs',T)
         locok=F
        ELSE
         CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,locok)
         IF(Lfatal)RETURN
         IF(argok.and.nelt.gt.0)THEN
          IF(dvec(1).le.ZERO.or.dvec(1).ge.ONE)THEN
           CALL inpter(PERROR,Errpos,
     &                 'Value of tcrate must be between 0 and 1.',T)
           locok=F
          ELSE
           Tcalfa=dvec(1)
           Havtca=T
          END IF
         END IF
        END IF
        GO TO 200
c-----------------------------------------------------------------------
c     AIC test difference for the regression-based AIC test (aicdiff)
c-----------------------------------------------------------------------
  160   IF(hvpva)THEN
         CALL inpter(PERROR,Errpos,
     &               'Use either aicdiff or pvaictest, not both',T)
         locok=F
        END IF
        CALL gtdpvc(LPAREN,F,PAICT,daicdf,nelt,argok,locok)
        IF(Lfatal)RETURN
        IF(argok)THEN
         hvaicd=T
         IF(nelt.eq.1)THEN
          DO ielt=1,PAICT
           Rgaicd(ielt)=daicdf(1)
          END DO
         ELSE IF(nelt.gt.0)THEN
          DO ielt=1,PAICT
           IF(.not.dpeq(daicdf(ielt),DNOTST))Rgaicd(ielt)=daicdf(ielt)
          END DO
         END IF
        END IF
        GO TO 200
c-----------------------------------------------------------------------
c     savelog  argument
c-----------------------------------------------------------------------
  170   CALL getsvl(LSLREG,NSLREG,locok)
        GO TO 200
c-----------------------------------------------------------------------
c     chi2test argument
c-----------------------------------------------------------------------
  180   CALL gtdcvc(LPAREN,F,1,YSNDIC,ysnptr,PYSN,
     &              'Choices for chi2test are yes and no.',
     &              ivec,nelt,T,argok,locok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Ch2tst=ivec(1).eq.1
        GO TO 200
c-----------------------------------------------------------------------
c     chi2testcv argument
c-----------------------------------------------------------------------
  190   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,locok)
        IF(Lfatal)RETURN
        IF(nelt.gt.0.and.argok)THEN
         IF(dvec(1).le.ZERO.or.dvec(1).ge.ONE)THEN
          CALL inpter(PERROR,Errpos,
     &                'Value of chi2testcv must be between 0 and 1.',T)
          locok=F
         ELSE
          Chi2cv=dvec(1)
         END IF
        END IF
        GO TO 200
c-----------------------------------------------------------------------
c     tlimit argument
c-----------------------------------------------------------------------
  191   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,locok)
        IF(Lfatal)RETURN
        IF(nelt.gt.0.and.argok)THEN
         IF(dvec(1).le.ZERO)THEN
          CALL inpter(PERROR,Errpos,
     &                'Value of tlimit must be greater than 0.',T)
          locok=F
         ELSE
          Tlimit=dvec(1)
         END IF
        END IF
        GO TO 200
c-----------------------------------------------------------------------
c     pvaictest argument
c-----------------------------------------------------------------------
  192   IF(hvaicd)THEN
         CALL inpter(PERROR,Errpos,
     &               'Use either aicdiff or pvaictest, not both',T)
         locok=F
        END IF
        CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,locok)
        IF(Lfatal)RETURN
        IF(nelt.gt.0.and.argok)THEN
         IF(dvec(1).le.ZERO)THEN
          CALL inpter(PERROR,Errpos,
     &                'Value of pvaictest must be greater than 0.',T)
          locok=F
         ELSE IF(dvec(1).ge.ONE)THEN
          CALL inpter(PERROR,Errpos,
     &                'Value of pvaictest must be less than 1.',T)
          locok=F
         ELSE
          Pvaic=ONE-dvec(1)
          hvpva=T
         END IF
        END IF
        GO TO 200
c-----------------------------------------------------------------------
c     testalleaster argument
c-----------------------------------------------------------------------
  193   CALL gtdcvc(LPAREN,F,1,YSNDIC,ysnptr,PYSN,
     &              'Choices for testalleaster are yes and no.',
     &              ivec,nelt,T,argok,locok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lceaic=ivec(1).eq.1
        GO TO 200
c-----------------------------------------------------------------------
c     trendtc argument
c-----------------------------------------------------------------------
  199   CALL gtdcvc(LPAREN,F,1,YSNDIC,ysnptr,PYSN,
     &              'Choices for trendtc are yes and no.',
     &              ivec,nelt,T,argok,locok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lttc=ivec(1).eq.1
        GO TO 200
       END IF
c-----------------------------------------------------------------------
       IF(nbvec.ne.NOTSET)THEN
c-----------------------------------------------------------------------
c     Insert value for Leap Year regressor that will be removed
c-----------------------------------------------------------------------
        IF(Picktd.and.(Fcntyp.ne.4.and.(.not.dpeq(Lam,1D0))))THEN
         ic1=1
         icol=strinx(T,Colttl,Colptr,ic1,Nb,'Leap Year')
         DO WHILE (icol.gt.0)
          IF(icol.le.nbvec)THEN
           DO i=nbvec,icol,-1
            bvec(i+1)=bvec(i)
            fixvec(i+1)=fixvec(i)
           END DO
          END IF
          Bvec(icol)=ONE
          nbvec=nbvec+1
          IF(icol.eq.Nb)THEN
           icol=0
          ELSE
           ic1=icol+1
           icol=strinx(T,Colttl,Colptr,ic1,Nb,'Leap Year')
          END IF
         END DO
        END IF                                                                                                                                                                             
        IF(nbvec.gt.0.and.nbvec.NE.(Nb+Ncusrx))THEN
         CALL eWritln('Number of initial values is not the same as '//
     &                'the number of regression variables.',
     &                STDERR,Mt2,T,T)
        ELSE
         DO i=1,Nb+Ncusrx
          Regfx(i)=fixvec(i)
          B(i)=bvec(i)
         END DO
        END IF
       END IF
c     ------------------------------------------------------------------
c     If the data are from the file get the data
c-----------------------------------------------------------------------
       IF(locok.and.hvfile.and..not.haveux)THEN
        IF(Ncusrx.gt.0)THEN
         CALL gtfldt(peltux,rgfile,nflchr,havfmt,rgfmt,nfmtch,2,Userx,
     &               neltux,Havesp,Sp,F,cblank,0,F,' ',0,0,hvstrt,
     &               Bgusrx,Ncusrx,ispn,ispn,T,haveux,locok)
        ELSE
         CALL eWritln('Need to specify both user-defined '//
     &                'regression variables (with user',
     &                STDERR,Mt2,T,F)
         CALL writln('        argument) and X matrix (with file or '//
     &               'data argument).',STDERR,Mt2,F,T)
        END IF
       END IF
c-----------------------------------------------------------------------
c     Check for the required arguments
c-----------------------------------------------------------------------
       IF(locok.and.(hvuttl.or.haveux))THEN
c-----------------------------------------------------------------------
c     check user-defined regression type selection.  First, check to 
c     see if user-defined regression variables are defined.
c-----------------------------------------------------------------------
        IF(Nusrrg.gt.0)THEN
c-----------------------------------------------------------------------
c     If only one type given, use it for all user-defined regression 
c     variables.
c-----------------------------------------------------------------------
         IF(Nusrrg.eq.1)THEN
          DO i=2,Ncusrx
           Usrtyp(i)=Usrtyp(1)
          END DO
         END IF
c-----------------------------------------------------------------------
c      Check to see if User-defined holiday groups are defined
c-----------------------------------------------------------------------
         CALL chkuhg(iuhl,Nguhl,herror)
         IF(herror)THEN
          CALL eWritln('Cannot specify holiday group types for user-'//
     &                 'defined regression ',
     &                STDERR,Mt2,T,F)
          CALL writln('        variables out of sequence.',STDERR,Mt2,
     &                F,T)
          locok=F
         END IF
        END IF
        IF(.not.(hvuttl.eqv.haveux))THEN
         CALL eWritln('Need to specify both user-defined '//
     &                'regression variables (with user',
     &                STDERR,Mt2,T,F)
         CALL writln('        argument) and X matrix (with file or '//
     &               'data argument).',STDERR,Mt2,F,T)
         locok=F
c     ------------------------------------------------------------------
        ELSE IF(mod(neltux,Ncusrx).ne.0)THEN
         ip1=1
         ip2=1
         CALL itoc(neltux,clen,ip1)
         IF(.not.Lfatal)CALL itoc(Ncusrx,clim,ip2)
         CALL eWritln('Number of user-defined X elements='//
     &                clen(1:(ip1-1)),STDERR,Mt2,T,F)
         CALL writln('        not equal to a multiple of the number '//
     &               'of columns='//clim(1:(ip2-1))//'.',STDERR,Mt2,F,T)
         locok=F
c     ------------------------------------------------------------------
        ELSE
         IF(.not.hvstrt)CALL cpyint(Begsrs,2,1,Bgusrx)
         Nrusrx=neltux/Ncusrx
         IF(.not.chkcvr(Bgusrx,Nrusrx,Begspn,Nspobs,Sp))THEN
          CALL cvrerr('user-defined regression variables',Bgusrx,Nrusrx,
     &                'span of the data',Begspn,Nspobs,Sp)
          IF(Lfatal)RETURN
          locok=F
c     ------------------------------------------------------------------
         ELSE
          idisp=Grp(Ngrp)-1
          DO i=1,Ncusrx
           idisp=idisp+1
           CALL getstr(Usrttl,Usrptr,Ncusrx,i,effttl,nchr)
           IF(.not.Lfatal)THEN
            IF(Usrtyp(i).eq.PRGTUH)THEN
             CALL adrgef(B(idisp),effttl(1:nchr),'User-defined Holiday',
     &                   PRGTUH,Regfx(idisp),T)
            ELSE IF(Usrtyp(i).eq.PRGUH2)THEN
             CALL adrgef(B(idisp),effttl(1:nchr),
     &                   'User-defined Holiday Group 2',PRGUH2,
     &                   Regfx(idisp),T)
            ELSE IF(Usrtyp(i).eq.PRGUH3)THEN
             CALL adrgef(B(idisp),effttl(1:nchr),
     &                   'User-defined Holiday Group 3',PRGUH3,
     &                   Regfx(idisp),T)
            ELSE IF(Usrtyp(i).eq.PRGUH4)THEN
             CALL adrgef(B(idisp),effttl(1:nchr),
     &                   'User-defined Holiday Group 4',PRGUH4,
     &                   Regfx(idisp),T)
            ELSE IF(Usrtyp(i).eq.PRGUH5)THEN
             CALL adrgef(B(idisp),effttl(1:nchr),
     &                   'User-defined Holiday Group 5',PRGUH5,
     &                   Regfx(idisp),T)
            ELSE IF(Usrtyp(i).eq.PRGTUS)THEN
             CALL adrgef(B(idisp),effttl(1:nchr),
     &                   'User-defined Seasonal',PRGTUS,Regfx(idisp),T)
            ELSE IF(Usrtyp(i).eq.PRGUCN)THEN
             CALL adrgef(B(idisp),effttl(1:nchr),
     &                   'User-defined Constant',PRGUCN,Regfx(idisp),T)
            ELSE IF(Usrtyp(i).eq.PRGUTD)THEN
             CALL adrgef(B(idisp),effttl(1:nchr),
     &                   'User-defined Trading Day',PRGUTD,
     &                   Regfx(idisp),T)
            ELSE IF(Usrtyp(i).eq.PRGULM)THEN
             CALL adrgef(B(idisp),effttl(1:nchr),
     &                   'User-defined LOM',PRGULM,Regfx(idisp),T)
            ELSE IF(Usrtyp(i).eq.PRGULQ)THEN
             CALL adrgef(B(idisp),effttl(1:nchr),
     &                   'User-defined LOQ',PRGULQ,Regfx(idisp),T)
            ELSE IF(Usrtyp(i).eq.PRGULY)THEN
             CALL adrgef(B(idisp),effttl(1:nchr),
     &                   'User-defined Leap Year',PRGULY,Regfx(idisp),T)
            ELSE IF(Usrtyp(i).eq.PRGUAO)THEN
             CALL adrgef(B(idisp),effttl(1:nchr),'User-defined AO',
     &                   PRGUAO,Regfx(idisp),T)
            ELSE IF(Usrtyp(i).eq.PRGULS)THEN
             CALL adrgef(B(idisp),effttl(1:nchr),'User-defined LS',
     &                   PRGULS,Regfx(idisp),T)
            ELSE IF(Usrtyp(i).eq.PRGUSO)THEN
             CALL adrgef(B(idisp),effttl(1:nchr),'User-defined SO',
     &                   PRGUSO,Regfx(idisp),T)
            ELSE IF(Usrtyp(i).eq.PRGUCY)THEN
             CALL adrgef(B(idisp),effttl(1:nchr),
     &                   'User-defined Transitory',PRGUCY,
     &                   Regfx(idisp),T)
            ELSE
             CALL adrgef(B(idisp),effttl(1:nchr),'User-defined',PRGTUD,
     &                   Regfx(idisp),T)
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
            i2=MOD(i,Ncusrx)
            IF(i2.eq.0)i2=Ncusrx
            urmean(i2)=urmean(i2)+Userx(i)
           END DO
           DO i=1,Ncusrx
            urmean(i)=urmean(i)/DBLE(Nrusrx)
           END DO
           DO i=1,neltux
            i2=MOD(i,Ncusrx)
            IF(i2.eq.0)i2=Ncusrx
            Userx(i)=Userx(i)-urmean(i2)
           END DO
          ELSE IF(luseas)THEN
           n2=Sp*Ncusrx
           DO i=1,Sp
            CALL setdp(ZERO,PB,urmean)
            CALL setdp(ZERO,PB,urnum)
            i2=(i-1)*Ncusrx+1
            DO j=i2,neltux,n2
             DO k=j,Ncusrx+j-1
              k2=MOD(k,Ncusrx)
              IF(k2.eq.0)k2=Ncusrx
              urmean(k2)=urmean(k2)+Userx(k)
              urnum(k2)=urnum(k2)+ONE
             END DO
            END DO
            DO j=1,Ncusrx
             urmean(j)=urmean(j) / urnum(j)
            END DO
            DO j=i2,neltux,n2
             DO k=j,Ncusrx+j-1
              k2=MOD(k,Ncusrx)
              IF(k2.eq.0)k2=Ncusrx
              Userx(k)=Userx(k)-urmean(k2)
             END DO
            END DO
           END DO
          END IF
c     ------------------------------------------------------------------
         END IF
        END IF
       END IF
       IF(Lfatal)RETURN
       IF(Nb.gt.0)THEN
c-----------------------------------------------------------------------
c     Check if the regression model parameters are fixed.  Sets iregfx.
c-----------------------------------------------------------------------
        CALL regfix()
c     ------------------------------------------------------------------
c     set indicator variable for fixed User-defined regressors.
c     ------------------------------------------------------------------
        Userfx=F
        IF(Ncusrx.gt.0.and.Iregfx.ge.2)THEN
         IF(Iregfx.eq.3)THEN
          Userfx=T
         ELSE
          DO igrp=1,Ngrp
           begcol=Grp(igrp-1)
           endcol=Grp(igrp)-1
           rtype=Rgvrtp(begcol)
           IF(rtype.eq.PRGTUD.or.rtype.eq.PRGTUS.or.rtype.eq.PRGTUH.or.
     &        rtype.eq.PRGUH2.or.rtype.eq.PRGUH3.or.rtype.eq.PRGUH4.or.
     &        rtype.eq.PRGUH5.or.rtype.eq.PRGUAO.or.rtype.eq.PRGULS.or.
     &        rtype.eq.PRGUSO.or.rtype.eq.PRGUCN.or.rtype.eq.PRGUCY.or.
     &        rtype.eq.PRGUTD.or.rtype.eq.PRGULM.or.rtype.eq.PRGULQ.or.
     &        rtype.eq.PRGULY)THEN
            DO i=begcol,endcol
             Userfx=Userfx.or.Regfx(i)
            END DO
           END IF
          END DO
         END IF
        END IF
c-----------------------------------------------------------------------
c     sort outlier regressors specified by the user, if any.
c-----------------------------------------------------------------------
        CALL otsort()
c-----------------------------------------------------------------------
        IF(Nusrrg.gt.0.and.Ncusrx.eq.0)THEN
         CALL eWritln('Cannot specify group types for '//
     &                'user-defined regression',Mt2,STDERR,T,F)
         CALL writln('        variables if user-defined regression '//
     &               'variables are not',Mt2,STDERR,F,F)
         CALL writln('        defined in the regression spec.',
     &               Mt2,STDERR,F,F)
         locok=F
        END IF
       END IF
c-----------------------------------------------------------------------
c      Check to see if lom, loq, or lpyear regressors can be generated
c      for this series.  (BCM March 2008)
c-----------------------------------------------------------------------
       IF(Lomtst.eq.1.and.Sp.ne.12)THEN
        CALL wWritln('The program will only perform an AIC test on '//
     &               'the length of month',STDERR,Mt2,T,F)
        CALL writln('         regressor for monthly time series.',
     &              STDERR,Mt2,F,T)
        Lomtst=0
       ELSE IF(Lomtst.eq.2.and.Sp.ne.4)THEN
        CALL wWritln('The program will only perform an AIC test on '//
     &               'the length of quarter',STDERR,Mt2,T,F)
        CALL writln('         regressor for quarterly time series.',
     &              STDERR,Mt2,F,T)
        Lomtst=0
       ELSE IF(Lomtst.eq.3.and.(.not.(Sp.eq.4.or.Sp.eq.12)))THEN
        CALL wWritln('The program will only perform an AIC test on '//
     &               'the leap year',STDERR,Mt2,T,F)
        CALL writln('         regressor for monthly or quarterly '//
     &              'time series.',STDERR,Mt2,F,T)
        Lomtst=0
       END IF
c-----------------------------------------------------------------------
c      Check to see if trading day model selected is compatable with
c      choice of Lomtst (BCM March 2008)
c-----------------------------------------------------------------------
       IF((Lomtst.eq.1.or.Lomtst.eq.2).and.Picktd)THEN
        IF(Lomtst.eq.1)
     &     CALL eWritln('AIC test for the length of month regressor '//
     &                  'cannot be specified when',Mt2,STDERR,T,F)
        IF(Lomtst.eq.2)
     &     CALL eWritln('AIC test for the length of quarter '//
     &                  'regressor cannot be specified when',
     &                  Mt2,STDERR,T,F)
        CALL writln('       the td or td1coef option is given in the '//
     &              'variables argument.',Mt2,STDERR,F,T)
        Lomtst=0
        locok=F
       ELSE IF(Lomtst.eq.3.and.(Picktd.and.(.not.dpeq(Lam,ONE))))THEN
        CALL eWritln('AIC test for the leap year regressor cannot be '//
     &               'specified when the',Mt2,STDERR,T,F)
        CALL writln('       td or td1coef option is given in the '//
     &              'variables argument and a',Mt2,STDERR,F,F)
        CALL writln('       power transformation is performed.',
     &              Mt2,STDERR,F,T)
        Lomtst=0
        locok=F
       END IF
c-----------------------------------------------------------------------
       IF(Itdtst.eq.3.and.Itdtst.eq.6)THEN
        IF(Lomtst.eq.1)THEN
         CALL eWritln('AIC test for the length of month regressor '//
     &                'cannot be specified when',Mt2,STDERR,T,F)
        ELSE IF(Lomtst.eq.2)THEN
         CALL eWritln('AIC test for the length of quarter regressor '//
     &                'cannot be specified when',Mt2,STDERR,T,F)
        ELSE
         CALL eWritln('AIC test for the leap year regressor cannot '//
     &                'be specified when',Mt2,STDERR,T,F)
        END IF
        CALL writln('       the tdstock or tdstock1coef option is '//
     &              'given in the aictest argument.',Mt2,STDERR,F,T)
        Lomtst=0
        locok=F
       END IF
c-----------------------------------------------------------------------
       IF(Itdtst.gt.0.and.(.not.havtd))havtd=T
       IF(Leastr.and.(.not.Havhol))Havhol=T
       IF((Lomtst.eq.1.or.Lomtst.eq.2).and.(.not.havln))havln=T
       IF(Lomtst.eq.3.and.(.not.havln))havlp=T
       IF(Adjtd.eq.1.and.(.NOT.(havtd.or.havln.or.havlp)))Adjtd=0
       IF(Adjhol.eq.1.and.(.not.Havhol))Adjhol=0
       IF(Adjcyc.eq.1.and.(.not.havcyc))Adjcyc=0
       IF(Nguhl.eq.0.and.Ch2tst)Ch2tst=F
c-----------------------------------------------------------------------
       Inptok=Inptok.and.locok
       RETURN
  200  CONTINUE
      END DO
c     -----------------------------------------------------------------
      END
