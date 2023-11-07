C     Last change:  BCM  16 Jul 2003    5:26 pm
      SUBROUTINE gtotlr(Begsrs,Nobs,Begmdl,Endmdl,Sp,Ltstao,Ltstls,
*     &                  Ltsttc,Ltstso,Ladd1,Critvl,Begtst,Endtst,Lsrun,
     &                  Ltsttc,Ladd1,Critvl,Begtst,Endtst,Lsrun,
     &                  Tcalfa,Havtca,Cvalfa,Cvtype,Cvrduc,Locok,Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'lex.i'
      INCLUDE 'tbllog.i'
      INCLUDE 'svllog.i'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO,ONE,PTONE
      INTEGER YR,PNOTL
      LOGICAL F,T
*      PARAMETER(YR=1,PNOTL=4,F=.false.,T=.true.,ZERO=0D0,ONE=1D0,
      PARAMETER(YR=1,PNOTL=3,F=.false.,T=.true.,ZERO=0D0,ONE=1D0,
     &          PTONE=0.1D0)
c-----------------------------------------------------------------------
*      LOGICAL Ladd1,argok,Inptok,Locok,Ltstao,Ltstls,Ltsttc,Ltstso,
      LOGICAL Ladd1,argok,Inptok,Locok,Ltstao,Ltstls,Ltsttc,
     &        Havtca,Cvtype
      INTEGER Begmdl,Begsrs,Begtst,Endmdl,Endtst,ielt,Lsrun,nelt,Nobs,
     &        Sp,spnvec,nmdl,ivec
      DOUBLE PRECISION Critvl,critmp,dvec,Tcalfa,Cvalfa,Cvrduc
      DIMENSION Begmdl(2),Begsrs(2),Begtst(2),Endmdl(2),Endtst(2),
     &          spnvec(2,2),Critvl(PNOTL),critmp(PNOTL),
     &          ivec(1),dvec(1)
c-----------------------------------------------------------------------
      LOGICAL chkcvr,gtarg,dpeq
      EXTERNAL chkcvr,gtarg,dpeq
c-----------------------------------------------------------------------
c     Argument dictionary was made with the following command
c ../../dictionary/strary < ../../dictionary/outlier.dic
c-----------------------------------------------------------------------
      CHARACTER ARGDIC*84
      INTEGER arglog,argidx,argptr,PARG
      PARAMETER(PARG=12)
      DIMENSION argptr(0:PARG),arglog(2,PARG)
      PARAMETER(ARGDIC='typesmethodcriticallsrunspanprintsavetcratecriti
     &calalphadefaultcriticalalmostsavelog')
c-----------------------------------------------------------------------
c     Identification method dictionary was made with the following
c command,
c ../../dictionary/strary mtd < ../../dictionary/outlier.method.dic
c-----------------------------------------------------------------------
      CHARACTER MTDDIC*12
      INTEGER mtdptr,PMTD
      PARAMETER(PMTD=2)
      DIMENSION mtdptr(0:PMTD)
      PARAMETER(MTDDIC='addoneaddall')
c-----------------------------------------------------------------------
c     Outlier types dictionary was made with the following command,
c ../../dictionary/strary typ < ../../dictionary/outlier.types.dic
c-----------------------------------------------------------------------
*      CHARACTER TYPDIC*15
      CHARACTER TYPDIC*13
      INTEGER typptr,PTYP,typidx
*      PARAMETER(PTYP=6)
      PARAMETER(PTYP=5)
      DIMENSION typptr(0:PTYP),typidx(PTYP)
*      PARAMETER(TYPDIC='noneaolstcsoall')
      PARAMETER(TYPDIC='noneaolstcall')
c-----------------------------------------------------------------------
c     default critical value types dictionary 
c-----------------------------------------------------------------------
      CHARACTER DEFDIC*14
      INTEGER defptr,PDEF
      PARAMETER(PDEF=2)
      DIMENSION defptr(0:PDEF)
      PARAMETER(DEFDIC='ljungcorrected')
c-----------------------------------------------------------------------
      DATA argptr/1,6,12,20,25,29,34,38,44,57,72,78,85/
      DATA mtdptr/1,7,13/
*      DATA typptr/1,5,7,9,11,13,16/
      DATA typptr/1,5,7,9,11,14/
      DATA defptr/1,6,15/
c-----------------------------------------------------------------------
c     Just by asking for outlier identification w/o any arguments
c will give identification.  Initialize the test period too.
c-----------------------------------------------------------------------
      Locok=T
      Ltstao=T
      Ltstls=T
      Ltsttc=F
*      Ltstso=F
      CALL setdp(DNOTST,PNOTL,critmp)
      CALL setint(NOTSET,2*PARG,arglog)
c-----------------------------------------------------------------------
c     Argument get loop
c-----------------------------------------------------------------------
      DO WHILE (T)
       IF(gtarg(ARGDIC,argptr,PARG,argidx,arglog,Locok))THEN
        IF(Lfatal)RETURN
        GO TO(10,20,30,40,50,60,70,80,90,100,110,120),argidx
c-----------------------------------------------------------------------
c     Argument to specify types of outliers to identify
c-----------------------------------------------------------------------
   10   CALL gtdcvc(LPAREN,T,PTYP,TYPDIC,typptr,PTYP,
     &              'Choices of outlier types to identify are NONE, '//
     &              'AO, LS, TC, and ALL',
     &              typidx,nelt,T,argok,Locok)
        IF(Lfatal)RETURN
c-----------------------------------------------------------------------
        IF(nelt.gt.0)THEN
         Ltstao=F
         Ltstls=F
         Ltsttc=F
*         Ltstso=F
         DO ielt=1,nelt
          IF(typidx(ielt).eq.1)THEN
           Ltstao=F
           Ltstls=F
           Ltsttc=F
*           Ltstso=F
          ELSE IF(typidx(ielt).eq.2)THEN
           Ltstao=T
          ELSE IF(typidx(ielt).eq.3)THEN
           Ltstls=T
          ELSE IF(typidx(ielt).eq.4)THEN
           Ltsttc=T
          ELSE IF(typidx(ielt).eq.5)THEN
*           Ltstso=T
*          ELSE IF(typidx(ielt).eq.6)THEN
           Ltstao=T
           Ltstls=T
           IF(Sp.ge.4)Ltsttc=T
*           IF(Sp.eq.4.or.Sp.eq.12)Ltstso=T
          END IF
         END DO
        END IF
        GO TO 130
c-----------------------------------------------------------------------
c     Identification method specification
c-----------------------------------------------------------------------
   20   CALL gtdcvc(LPAREN,T,1,MTDDIC,mtdptr,PMTD,
     &              'Choices are ADDONE or ADDALL',ivec,nelt,T,argok,
     &              Locok)
        IF(Lfatal)RETURN
c----------------------------------------------------------------------
        IF(nelt.gt.0.and.argok)Ladd1=ivec(1).eq.1
        GO TO 130
c-----------------------------------------------------------------------
c     Critical t value specification
c-----------------------------------------------------------------------
   30   CALL gtdpvc(LPAREN,F,PNOTL,critmp,nelt,argok,Locok)
        IF(Lfatal)RETURN
        IF(nelt.eq.1)THEN
         DO ielt=1,PNOTL
          Critvl(ielt)=critmp(1)
         END DO
        ELSE IF(nelt.gt.0)THEN
         DO ielt=1,PNOTL
          IF(.not.dpeq(critmp(ielt),DNOTST))Critvl(ielt)=critmp(ielt)
         END DO
        END IF
        GO TO 130
c-----------------------------------------------------------------------
c     Lsrun, maximum number of ls's to test for
c-----------------------------------------------------------------------
   40   CALL getivc(LPAREN,T,1,ivec,nelt,argok,Locok)
        IF(nelt.gt.0.and.ivec(1).gt.7)THEN
         CALL inpter(PERROR,Errpos,'Lsrun must be less than or equal '//
     &               'to seven.',T)
         Locok=F
        ELSE
         Lsrun=ivec(1)
        END IF
        GO TO 130
c-----------------------------------------------------------------------
c     Span argument
c-----------------------------------------------------------------------
   50   CALL gtdtvc(T,Sp,LPAREN,F,2,spnvec,nelt,argok,Locok)
        IF(Lfatal)RETURN
        IF(nelt.eq.1)THEN
         CALL inpter(PERROR,Errpos,
     &               'Need two dates for the span or use a comma as '//
     &               'a place holder.',T)
         Locok=F
        END IF
c-----------------------------------------------------------------------
c     set span for outlier test
c-----------------------------------------------------------------------
        IF (Locok) THEN
         IF(spnvec(YR,1).eq.NOTSET)THEN
           CALL cpyint(Begmdl,2,1,Begtst)
         ELSE
           CALL cpyint(spnvec,2,1,Begtst)
         END IF
         IF(spnvec(YR,2).eq.NOTSET)THEN
           CALL cpyint(Endmdl,2,1,Endtst)
         ELSE
           CALL cpyint(spnvec(1,2),2,1,Endtst)
         END IF
c----------------------------------------------------------------------
c     Check that the span is within the series
c----------------------------------------------------------------------
         CALL dfdate(Endtst,Begtst,Sp,nelt)
         nelt=nelt+1
         CALL dfdate(Endmdl,Begmdl,Sp,nmdl)
         nmdl=nmdl+1
         IF(.not.chkcvr(Begsrs,Nobs,Begtst,nelt,Sp))THEN
           CALL inpter(PERROR,Errpos,'Span not within the series',T)
           CALL cvrerr('Series',Begsrs,Nobs,'outlier test span',Begtst,
     &               nelt,Sp)
           Locok=F
         ELSE IF(.not.chkcvr(Begmdl,nmdl,Begtst,nelt,Sp))THEN
           CALL inpter(PERROR,Errpos,'Span not within the model span',T)
           CALL cvrerr('Model span',Begmdl,nmdl,'outlier test span',
     &                 Begtst,nelt,Sp)
           Locok=F
         END IF
         IF(Lfatal)RETURN
        END IF
        GO TO 130
c-----------------------------------------------------------------------
c     Print argument
c-----------------------------------------------------------------------
   60   CALL getprt(LSPOTL,NSPOTL,Locok)
        GO TO 130
c-----------------------------------------------------------------------
c     Save argument
c-----------------------------------------------------------------------
   70   CALL getsav(LSPOTL,NSPOTL,Locok)
        GO TO 130
c-----------------------------------------------------------------------
c     tcrate - alpha value for all TC outliers
c-----------------------------------------------------------------------
   80   IF(Havtca)THEN
         CALL inpter(PERROR,Errpos,
     &               'Cannot specify tcrate in both the regression '//
     &               'and outlier specs',T)
         Inptok=F
        ELSE
         CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
         IF(Lfatal)RETURN
         IF(argok.and.nelt.gt.0)THEN
          IF(dvec(1).le.ZERO.or.dvec(1).ge.ONE)THEN
           CALL inpter(PERROR,Errpos,
     &                 'Value of tcrate must be between 0 and 1.',T)
           Inptok=F
          ELSE
           Tcalfa=dvec(1)
           Havtca=T
          END IF
         END IF
        END IF
        GO TO 130
c-----------------------------------------------------------------------
c     criticalalpha - alpha value for outlier critical value
c-----------------------------------------------------------------------
   90   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).le.ZERO.or.dvec(1).gt.PTONE)THEN
          CALL inpter(PERROR,Errpos,
     &           'Value of criticalalpha must be between 0 and 0.10.',T)
          Inptok=F
         ELSE
          Cvalfa=dvec(1)
         END IF
        END IF
        GO TO 130
c-----------------------------------------------------------------------
c     Default critical value generation method specification
c-----------------------------------------------------------------------
  100   CALL gtdcvc(LPAREN,T,1,DEFDIC,defptr,PDEF,
     &              'Choices are ljung or corrected',ivec,nelt,T,argok,
     &              Locok)
        IF(Lfatal)RETURN
c----------------------------------------------------------------------
        IF(nelt.gt.0.and.argok)Cvtype=ivec(1).eq.1
        GO TO 130
c-----------------------------------------------------------------------
c     almost - amount to reduce outlier critical value to identify
c                "almost" outliers
c-----------------------------------------------------------------------
  110   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).le.ZERO)THEN
          CALL inpter(PERROR,Errpos,
     &                'Value of almost must be greater than 0.',T)
          Inptok=F
         ELSE
          Cvrduc=dvec(1)
         END IF
        END IF
        GO TO 130
c-----------------------------------------------------------------------
c     savelog  argument
c-----------------------------------------------------------------------
  120   CALL getsvl(LSLOTL,NSLOTL,Inptok)
        GO TO 130
       END IF
       IF(Lfatal)RETURN
c----------------------------------------------------------------------
       Inptok=Locok.and.Inptok
       RETURN
  130  CONTINUE
      END DO
c----------------------------------------------------------------------
      END


