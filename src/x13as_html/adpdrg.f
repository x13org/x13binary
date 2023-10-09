c     Last Change: allow AOSdate-0.0 or LSSdate-0.0 format.change error
C     message for AOS and LSS, pass Endmdl variable to rdotlr.f, Mar-21
C     Last change:  BCM  23 Jul 1998   12:19 pm
      SUBROUTINE adpdrg(Begsrs,Endmdl,Nobs,Havsrs,Havesp,Rgname,
     &                  Nrgchr,X11reg,Havtd,Havhol,Havln,Havlp,Locok,
     &                  Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     BCM April, 2016 - generate easter(0) regressor
c-----------------------------------------------------------------------
c     adpdrg.f, Release 1, Subroutine Version 1.11, Modified 16 Feb 1995.
c-----------------------------------------------------------------------
c     Get the predefined regression variables
c-----------------------------------------------------------------------
c     Variable typing and parameters initialization
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'lex.i'
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      CHARACTER day*3,effttl*(PCOLCR),cmonth*3,ordend*2,Rgname*(LINLEN),
     &          tgrptl*(PGRPCR)
      LOGICAL argok,Havsrs,Havesp,Inptok,Locok,X11reg,Havhol,Havtd,
     &        Havln,Havlp
      INTEGER Begsrs,Endmdl,begdat,enddat,i,ipos,isncos,nchr,neastr,
     &        nelt,nlabor,Nobs,Nrgchr,nsncos,nthank,spm1,tdspdy,
     &        tmpdat,vartyp,ivec,zeroz,igrp,ewlim
      DIMENSION Begsrs(2),Endmdl(2),day(7),isncos(PSP/2),cmonth(12),
     &          ordend(0:9),tmpdat(2),ivec(1),ewlim(0:1)
c-----------------------------------------------------------------------
      INTEGER strinx
      EXTERNAL strinx
c-----------------------------------------------------------------------
c     The argument dictionary was created with the following command:
c  ../../dictionary/strary < ../../dictionary/variables.dic
c-----------------------------------------------------------------------
      CHARACTER REGDIC*123
      INTEGER regidx,regptr,PREG
      PARAMETER(PREG=18)
      DIMENSION regptr(0:PREG)
      PARAMETER(REGDIC='constseasonalsincostdtdnolpyearlomloqlpyeartdsto
     &cklomstockeastersceasterlaborthanktd1coeftd1nolpyeartdstock1coefea
     &sterstock')
c-----------------------------------------------------------------------
c     The ao,ls, ramp type dictionary was created
c with the following command:
c  ../../dictionary/strary typ < ../../dictionary/outlier.type.dic
c-----------------------------------------------------------------------
      CHARACTER TYPDIC*24
      INTEGER typidx,typptr,POTYPE
      PARAMETER(POTYPE=11)
      DIMENSION typptr(0:POTYPE)
      PARAMETER(TYPDIC='aolsrpmvtcsotlqiqdaoslss')
c     ------------------------------------------------------------------
      DATA regptr/1,6,14,20,22,32,35,38,44,51,59,65,73,78,83,90,101,113,
     &            124/
      DATA typptr/1,3,5,7,9,11,13,15,17,19,22,25/
      DATA (ewlim(i),i=0,1)/25,24/
c     ------------------------------------------------------------------
      DATA cmonth/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep',
     &     'Oct','Nov','Dec'/
      DATA day/'Mon','Tue','Wed','Thu','Fri','Sat','Sun'/
      DATA ordend/'th','st','nd','rd','th','th','th','th','th','th'/
c     ------------------------------------------------------------------
      Locok=T
      CALL setchr(' ',PCOLCR,effttl)
      zeroz=0
c-----------------------------------------------------------------------
c     Change by BCM Feb 1996:
c     read arguments for X11-irregular regression as well as regARIMA
c-----------------------------------------------------------------------
      regidx=strinx(F,REGDIC,regptr,1,PREG,Rgname(1:Nrgchr))
      IF(X11reg.and.(regidx.le.3.or.(regidx.gt.4.and.regidx.lt.9).or.
     &   regidx.eq.10.or.regidx.eq.16))regidx=0
      GO TO(10,20,30,40,40,40,40,40,50,40,60,60,70,80,40,40,50,60)regidx
c-----------------------------------------------------------------------
c     AO, LS, Ramp, or error
c-----------------------------------------------------------------------
      typidx=strinx(F,TYPDIC,typptr,1,POTYPE,Rgname(1:3))
      if(typidx.eq.0)typidx=strinx(F,TYPDIC,typptr,1,POTYPE,Rgname(1:2))
c-----------------------------------------------------------------------
c     Change by BCM Feb 1996:
c     only AOs allowed in X11-irregular regression
c-----------------------------------------------------------------------
      IF(X11reg.and.typidx.gt.1)typidx=0
c-----------------------------------------------------------------------
      GO TO(90,100,110,90,120,130,140,160,170,180,190),typidx
c-----------------------------------------------------------------------
c     Not an  ao, ls, or rp
c-----------------------------------------------------------------------
c     Change by BCM Feb 1996:
c     Add error message for X-11 regression
c----------------------------------------------------------------------- 
      IF(X11reg)THEN
       CALL inpter(PERROR,Lstpos,
     &             'Irregular Component Regression variable name "'//
     &             Rgname(1:Nrgchr)//'" not found',T)
      ELSE
       CALL inpter(PERROR,Lstpos,'Regression variable name "'//
     &             Rgname(1:Nrgchr)//'" not found',T)
      END IF
      GO TO 200
c-----------------------------------------------------------------------
c     AO outlier
c-----------------------------------------------------------------------
   90 IF(.not.Havsrs)THEN
       CALL inpter(PERROR,Lstpos,
     &             'No time series specified for AO outliers',T)
       GO TO 200
      ELSE
       CALL rdotlr(Rgname(1:Nrgchr),Begsrs,Sp,typidx,begdat,enddat,
     &             argok)
       IF(.not.argok)THEN
c     ------------------------------------------------------------------
        CALL inpter(PERROR,Lstpos,
     &       'See the above AO, LS, RP, SO, TL, TC, QI, or QD error.',T)
        GO TO 200
       ELSE IF(begdat.gt.Nobs.or.begdat.lt.1)THEN
        CALL inpter(PERROR,Lstpos,'Not within series',T)
        GO TO 200
       END IF
      END IF
c     ------------------------------------------------------------------
      IF(typidx.eq.1)THEN
       Rgname(1:2)='AO'
       vartyp=PRGTAO
      ELSE
       Rgname(1:2)='MV'
       vartyp=PRGTMV
      END IF
      GO TO 210
c-----------------------------------------------------------------------
c     Level-shift variable
c-----------------------------------------------------------------------
  100 IF(.not.Havsrs)THEN
       CALL inpter(PERROR,Lstpos,
     &             'No time series specified for level-shift',T)
       GO TO 200
      ELSE
       CALL rdotlr(Rgname(1:Nrgchr),Begsrs,Sp,typidx,begdat,enddat,
     &             argok)
       IF(.not.argok)THEN
        CALL inpter(PERROR,Lstpos,
     &       'See the above AO, LS, RP, SO, TL, TC, QI, or QD error.',T)
        GO TO 200
       ELSE IF(begdat.gt.Nobs.or.begdat.lt.2)THEN
        CALL inpter(PERROR,Lstpos,'Not within series',T)
        GO TO 200
       END IF
      END IF
c     ------------------------------------------------------------------
      Rgname(1:2)='LS'
      vartyp=PRGTLS
      GO TO 210
c-----------------------------------------------------------------------
c     Ramp variable
c-----------------------------------------------------------------------
  110 IF(.not.Havsrs)THEN
       CALL inpter(PERROR,Lstpos,
     &             'No time series specified for ramp variable',T)
       GO TO 200
c     ------------------------------------------------------------------
      ELSE
       CALL rdotlr(Rgname(1:Nrgchr),Begsrs,Sp,typidx,begdat,enddat,
     &             argok)
       IF(.not.argok)THEN
        CALL inpter(PERROR,Lstpos,
     &     'See the above AO, LS, RP, SO, TL, TC, QI, or QD error.',T)
        GO TO 200
       ELSE IF(enddat.gt.Nobs)THEN
        CALL inpter(PERROR,Lstpos,'End of ramp not within series',T)
        GO TO 200
c     ------------------------------------------------------------------
       ELSE IF(begdat.lt.1)THEN
        CALL inpter(PERROR,Lstpos,'Beginning of ramp not within series',
     &              T)
        GO TO 200
c     ------------------------------------------------------------------
       ELSE IF(enddat.le.begdat)THEN
        CALL inpter(PERROR,Lstpos,'Beginning and end of ramp reversed',
     &              T)
        GO TO 200
       END IF
      END IF
c     ------------------------------------------------------------------
      Rgname(1:2)='Rp'
      vartyp=PRGTRP
      GO TO 210
c-----------------------------------------------------------------------
c     TC outlier
c-----------------------------------------------------------------------
  120 IF(.not.Havsrs)THEN
       CALL inpter(PERROR,Lstpos,
     &             'No time series specified for TC outliers',T)
       GO TO 200
      ELSE
       CALL rdotlr(Rgname(1:Nrgchr),Begsrs,Sp,typidx,begdat,enddat,
     &             argok)
       IF(.not.argok)THEN
c     ------------------------------------------------------------------
        CALL inpter(PERROR,Lstpos,
     &     'See the above AO, LS, RP, SO, TL, TC, QI, or QD error.',T)
        GO TO 200
       ELSE IF(begdat.gt.Nobs.or.begdat.lt.1)THEN
        CALL inpter(PERROR,Lstpos,'Not within series',T)
        GO TO 200
       END IF
      END IF
c     ------------------------------------------------------------------
      Rgname(1:2)='TC'
      vartyp=PRGTTC
      GO TO 210
c-----------------------------------------------------------------------
c     SO outlier
c-----------------------------------------------------------------------
  130 IF(.not.Havsrs)THEN
       CALL inpter(PERROR,Lstpos,
     &             'No time series specified for seasonal outliers',T)
       GO TO 200
      ELSE
       CALL rdotlr(Rgname(1:Nrgchr),Begsrs,Sp,typidx,begdat,enddat,
     &             argok)
       IF(.not.argok)THEN
c     ------------------------------------------------------------------
        CALL inpter(PERROR,Lstpos,
     &     'See the above AO, LS, RP, SO, TL, TC, QI, or QD error.',T)
        GO TO 200
       ELSE IF(begdat.gt.Nobs.or.begdat.lt.1)THEN
        CALL inpter(PERROR,Lstpos,'Not within series',T)
        GO TO 200
       END IF
      END IF
c     ------------------------------------------------------------------
      Rgname(1:2)='SO'
      vartyp=PRGTSO
      GO TO 210
c-----------------------------------------------------------------------
c     TLS variable
c-----------------------------------------------------------------------
  140 IF(.not.Havsrs)THEN
       CALL inpter(PERROR,Lstpos,
     &           'No time series specified for temporary LS variable',T)
       GO TO 200
c     ------------------------------------------------------------------
      ELSE
       CALL rdotlr(Rgname(1:Nrgchr),Begsrs,Sp,typidx,begdat,enddat,
     &             argok)
       IF(.not.argok)THEN
        CALL inpter(PERROR,Lstpos,
     &     'See the above AO, LS, RP, SO, TL, TC, QI, or QD error.',T)
        GO TO 200
       ELSE IF(enddat.gt.Nobs)THEN
        CALL inpter(PERROR,Lstpos,
     &              'End of temporary LS not within series',T)
        GO TO 200
c     ------------------------------------------------------------------
       ELSE IF(begdat.lt.1)THEN
        CALL inpter(PERROR,Lstpos,
     &              'Beginning of temporary LS not within series',T)
        GO TO 200
c     ------------------------------------------------------------------
       ELSE IF(enddat.le.begdat)THEN
        CALL inpter(PERROR,Lstpos,
     &              'Beginning and end of temporary LS reversed',T)
        GO TO 200
       END IF
      END IF
c     ------------------------------------------------------------------
      Rgname(1:2)='TL'
      vartyp=PRGTTL
      GO TO 210
c-----------------------------------------------------------------------
c     Quadratic Ramp variable, increasing rate
c-----------------------------------------------------------------------
  160 IF(.not.Havsrs)THEN
       CALL inpter(PERROR,Lstpos,
     &    'No time series specified for quadratic ramp (QI) variable',T)
       GO TO 200
c     ------------------------------------------------------------------
      ELSE
       CALL rdotlr(Rgname(1:Nrgchr),Begsrs,Sp,typidx,begdat,enddat,
     &             argok)
       IF(.not.argok)THEN
        CALL inpter(PERROR,Lstpos,
     &     'See the above AO, LS, RP, SO, TL, TC, QI, or QD error.',T)
        GO TO 200
       ELSE IF(enddat.gt.Nobs)THEN
        CALL inpter(PERROR,Lstpos,
     &              'End of quadratic ramp (QI) not within series',T)
        GO TO 200
c     ------------------------------------------------------------------
       ELSE IF(begdat.lt.1)THEN
        CALL inpter(PERROR,Lstpos,
     &           'Beginning of quadratic ramp (QI) not within series',T)
        GO TO 200
c     ------------------------------------------------------------------
       ELSE IF(enddat.le.begdat)THEN
        CALL inpter(PERROR,Lstpos,
     &            'Beginning and end of quadratic ramp (QI) reversed',T)
        GO TO 200
       END IF
      END IF
c     ------------------------------------------------------------------
      Rgname(1:2)='QI'
      vartyp=PRGTQI
      GO TO 210
c-----------------------------------------------------------------------
c     Quadratic Ramp variable, decreasing rate
c-----------------------------------------------------------------------
  170 IF(.not.Havsrs)THEN
       CALL inpter(PERROR,Lstpos,
     &    'No time series specified for quadratic ramp (QD) variable',T)
       GO TO 200
c     ------------------------------------------------------------------
      ELSE
       CALL rdotlr(Rgname(1:Nrgchr),Begsrs,Sp,typidx,begdat,enddat,
     &             argok)
       IF(.not.argok)THEN
        CALL inpter(PERROR,Lstpos,
     &       'See the above AO, LS, RP, SO, TL, TC, QI, or QD error.',T)
        GO TO 200
       ELSE IF(enddat.gt.Nobs)THEN
        CALL inpter(PERROR,Lstpos,
     &              'End of quadratic ramp (QD) not within series',T)
        GO TO 200
c     ------------------------------------------------------------------
       ELSE IF(begdat.lt.1)THEN
        CALL inpter(PERROR,Lstpos,
     &           'Beginning of quadratic ramp (QD) not within series',T)
        GO TO 200
c     ------------------------------------------------------------------
       ELSE IF(enddat.le.begdat)THEN
        CALL inpter(PERROR,Lstpos,
     &            'Beginning and end of quadratic ramp (QD) reversed',T)
        GO TO 200
       END IF
      END IF
c     ------------------------------------------------------------------
      Rgname(1:2)='QD'
      vartyp=PRGTQD
      GO TO 210
c-----------------------------------------------------------------------
c     AO sequence variable
c-----------------------------------------------------------------------
  180 IF(.not.Havsrs)THEN
       CALL inpter(PERROR,Lstpos,
     &      'No time series specified for AO sequence (AOS) variable',T)
       GO TO 200
c     ------------------------------------------------------------------
      ELSE
       CALL rdotls(Rgname(1:Nrgchr),Begsrs,Endmdl,Sp,typidx,begdat,
     &             enddat,argok)
       IF(.not.argok)THEN
        CALL inpter(PERROR,Lstpos,'See the above AOS or LSS error.',T)
        GO TO 200
       ELSE IF(enddat.gt.Nobs)THEN
        CALL inpter(PERROR,Lstpos,
     &              'End of AO sequence (AOS) not within series',T)
        GO TO 200
c     ------------------------------------------------------------------
       ELSE IF(begdat.lt.1)THEN
        CALL inpter(PERROR,Lstpos,
     &             'Beginning of AO sequence (AOS) not within series',T)
        GO TO 200
c     ------------------------------------------------------------------
       ELSE IF(enddat.le.begdat)THEN
        CALL inpter(PERROR,Lstpos,
     &              'beginning of AO sequence (AOS) occurs on or '//
     &               'after end of AO sequence (AOS)',T)
       END IF
      END IF
c     ------------------------------------------------------------------
      Rgname(1:3)='AOS'
      vartyp=PRSQAO
      GO TO 220
c-----------------------------------------------------------------------
c     LS sequence variable
c-----------------------------------------------------------------------
  190 IF(.not.Havsrs)THEN
       CALL inpter(PERROR,Lstpos,
     &      'No time series specified for LS sequence (LSS) variable',T)
       GO TO 200
c     ------------------------------------------------------------------
      ELSE
       CALL rdotls(Rgname(1:Nrgchr),Begsrs,Endmdl,Sp,typidx,begdat,
     &             enddat,argok)
       IF(.not.argok)THEN
        CALL inpter(PERROR,Lstpos,'See the above AOS or LSS error.',T)
        GO TO 200
       ELSE IF(enddat.gt.Nobs)THEN
        CALL inpter(PERROR,Lstpos,
     &              'End of LS sequence (LSS) not within series',T)
        GO TO 200
c     ------------------------------------------------------------------
       ELSE IF(begdat.lt.1)THEN
        CALL inpter(PERROR,Lstpos,
     &             'Beginning of LS sequence (LSS) not within series',T)
        GO TO 200
c     ------------------------------------------------------------------
       ELSE IF(enddat.le.begdat)THEN
        CALL inpter(PERROR,Lstpos,
     &              'Beginning of LS sequence (LSS) occurs on or '//
     &              'after end of LS sequence (LSS)',T)
        GO TO 200
       END IF
      END IF
c     ------------------------------------------------------------------
      Rgname(1:3)='LSS'
      vartyp=PRSQLS
      GO TO 220
c     ------------------------------------------------------------------
  200 Locok=F
      CALL lex()
      GO TO 230
c     ------------------------------------------------------------------
  210 CALL adrgef(DNOTST,Rgname(1:Nrgchr),Rgname(1:Nrgchr),vartyp,F,T)
      IF(Lfatal)RETURN
      CALL lex()
      GO TO 230
c     ------------------------------------------------------------------
  220 DO i=begdat,enddat
       CALL addate(Begsrs,Sp,i-1,tmpdat)
       CALL wrtdat(tmpdat,Sp,Rgname(3:),Nrgchr)
       IF(Lfatal)RETURN
       Nrgchr=Nrgchr+2
       CALL adrgef(DNOTST,Rgname(1:Nrgchr),Rgname(1:Nrgchr),vartyp,F,T)
      IF(Lfatal)RETURN
      END DO
      CALL lex()
      GO TO 230
c-----------------------------------------------------------------------
c     Overall constant on the AR side
c-----------------------------------------------------------------------
   10 CALL adrgef(DNOTST,'Constant','Constant',PRGTCN,F,T)
      IF(Lfatal)RETURN
      CALL lex()
      GO TO 230
c-----------------------------------------------------------------------
c     Seasonal effects
c-----------------------------------------------------------------------
   20 CALL lex()
      IF(.not.Havesp)THEN
       CALL inpter(PERROR,Lstpos,
     &  'No seasonal period specified to determine seasonal effects.',T)
       Locok=F
c     ------------------------------------------------------------------
      ELSE IF(Sp.eq.1)THEN
       CALL inpter(PERROR,Lstpos,
     &             'Seasonal effects with nonseasonal data.',T)
       Locok=F
      ELSE IF(Nxtktp.ne.SLASH.AND.(Lseff.or.Lseadf.or.Lidsdf))THEN
       IF(Lidsdf)THEN
        CALL inpter(PERROR,Lstpos,
     &     'Already have a seasonal difference in the identify spec.',T)
       ELSE
        CALL inpter(PERROR,Lstpos,
     &    'Already have seasonal effects or seasonal difference.',T)
       END IF
       Locok=F
      ELSE IF(Lrgmse.and.Nxtktp.eq.SLASH)THEN
       CALL inpter(PERROR,Lstpos,
     &             'Already have change of regime seasonal effects.',T)
       Locok=F
c     ------------------------------------------------------------------
      ELSE
       IF(.not.Lseff)THEN
        spm1=Sp-1
c     ------------------------------------------------------------------
        IF(Sp.eq.12)THEN
         DO i=1,spm1
          effttl=cmonth(i)
          nchr=3
          CALL adrgef(DNOTST,effttl(1:nchr),'Seasonal',PRGTSE,F,T)
          IF(Lfatal)RETURN
         END DO
c     ------------------------------------------------------------------
        ELSE
         DO i=1,spm1
          ipos=1
          CALL itoc(i,effttl,ipos)
          IF(Lfatal)RETURN
          IF(mod(i,100).ge.11.and.mod(i,100).le.13)THEN
           effttl(ipos:ipos+1)='th'
          ELSE
           effttl(ipos:ipos+1)=ordend(mod(i,10))
          END IF
          nchr=ipos+1
          CALL adrgef(DNOTST,effttl(1:nchr),'Seasonal',PRGTSE,F,T)
          IF(Lfatal)RETURN
         END DO
        END IF
       END IF
       IF(Nxtktp.eq.SLASH)THEN
        CALL adrgim(Begsrs,Nobs,Havesp,'Seasonal',PRRTSE,PRATSE,zeroz,
     &              .not.Lseff,Lrgmse,Lseff,Locok)
        IF(zeroz.eq.0.AND.(Lseff.or.Lseadf.or.Lidsdf))THEN
         IF(Lidsdf)THEN
          CALL inpter(PERROR,Lstpos,
     &     'Already have a seasonal difference in the identify spec.',T)
         ELSE
          CALL inpter(PERROR,Lstpos,
     &     'Already have seasonal effects or seasonal difference.',T)
         END IF
         Locok=F
        END IF
       END IF
      END IF
      IF(Locok.and.zeroz.eq.0)Lseff=T
      IF(Lfatal)RETURN
c      IF(Lseff) write(Mtprof,*) ' Lseff = .true.'
      GO TO 230
c-----------------------------------------------------------------------
c     Seasonal sine-cosine
c-----------------------------------------------------------------------
   30 CALL lex()
      CALL getivc(LBRAKT,T,Sp/2,isncos,nsncos,Locok,Inptok)
      IF(Lfatal)RETURN
      IF(nsncos.le.0)THEN
       CALL inpter(PERROR,Lstpos,
     &             'Must specify the sine-cosine term explicitly.',T)
       Locok=F
c     ------------------------------------------------------------------
      ELSE IF(.not.Havesp)THEN
       CALL inpter(PERROR,Lstpos,
     &  'No seasonal period specified to determine seasonal effects.',T)
       Locok=F
c     ------------------------------------------------------------------
      ELSE IF(Sp.eq.1)THEN
       CALL inpter(PERROR,Lstpos,
     &             'Seasonal effects with nonseasonal data.',T)
c     ------------------------------------------------------------------
      ELSE IF(Nxtktp.ne.SLASH.AND.(Lseff.or.Lseadf.or.Lidsdf))THEN
       IF(Lidsdf)THEN
        CALL inpter(PERROR,Lstpos,
     &      'Already have a seasonal difference in the identify spec',T)
       ELSE
        CALL inpter(PERROR,Lstpos,
     &         'Already have seasonal effects or seasonal difference',T)
       END IF
       Locok=F
c     ------------------------------------------------------------------
      ELSE IF(Nxtktp.eq.SLASH.and.Lrgmse)THEN
       CALL inpter(PERROR,Lstpos,
     &             'Already have change of regime seasonal effects.',T)
       Locok=F
      ELSE
       IF(.not.Lseff)THEN
        IF(isncos(nsncos).eq.Sp/2)THEN
         nsncos=2*nsncos-1
c     ------------------------------------------------------------------
        ELSE
         nsncos=2*nsncos
        END IF
c     ------------------------------------------------------------------
        DO i=2,nsncos+1,2
         effttl='cos(2pi*'
         ipos=9
         CALL itoc(isncos(i/2),effttl,ipos)
         IF(Lfatal)RETURN
         effttl(ipos:ipos+1)='t/'
         ipos=ipos+2
         CALL itoc(Sp,effttl,ipos)
         IF(Lfatal)RETURN
         effttl(ipos:ipos)=')'
         CALL adrgef(DNOTST,effttl(1:ipos),'Trigonometric Seasonal',
     &               PRGTTS,F,T)
         IF(Lfatal)RETURN
c     ------------------------------------------------------------------
         IF(isncos(i/2).lt.Sp/2)THEN
          effttl='sin(2pi*'
          ipos=9
          CALL itoc(isncos(i/2),effttl,ipos)
          IF(Lfatal)RETURN
          effttl(ipos:ipos+1)='t/'
          ipos=ipos+2
          CALL itoc(Sp,effttl,ipos)
          IF(Lfatal)RETURN
          effttl(ipos:ipos)=')'
          CALL adrgef(DNOTST,effttl(1:ipos),'Trigonometric Seasonal',
     &                PRGTTS,F,T)
          IF(Lfatal)RETURN
         END IF
c     ------------------------------------------------------------------
         IF(isncos(i/2).gt.Sp/2.or.isncos(i/2).lt.1)THEN
          WRITE(STDERR,1010)isncos(i/2)
          WRITE(Mt2,1011)isncos(i/2)
 1010     FORMAT(/,' ERROR: Cannot have a sine-cosine variable pair ',
     &           'with, i=',i4,'.',/)
 1011     FORMAT(/,'<p><strong>ERROR:</strong> Cannot have a sine-',
     &           'cosine variable pair with, i=',i4,'.</p>',/)
          Locok=F
          GO TO 230
         END IF
        END DO
       END IF
       IF(Nxtktp.eq.SLASH)THEN
        CALL adrgim(Begsrs,Nobs,Havesp,'Trigonometric Seasonal',
     &              PRRTTS,PRATTS,zeroz,.not.Lseff,Lrgmse,Lseff,Locok)
       END IF
       IF(Lfatal)RETURN
      END IF
      IF(Locok.and.zeroz.eq.0)Lseff=T
      GO TO 230
c-----------------------------------------------------------------------
c     First six trading day effects
c-----------------------------------------------------------------------
   40 CALL lex()
      IF(.not.Havesp)THEN
       CALL inpter(PERROR,Lstpos,
     &             'No seasonal period specified in series spec.',T)
       Locok=F
      ELSE IF(Sp.ne.12.and.Sp.ne.4)THEN
       CALL inpter(PERROR,Lstpos,
     &             ' Need monthly or quarterly data for trading day',T)
       Locok=F
      ELSE IF(Begsrs(1).lt.1776)THEN
       CALL inpter(PERROR,Lstpos,
     &'No trading variables before 1776.  Try including the century in',
     &             F)
       CALL writln('        the start date',Mt2,STDERR,F,T)
       Locok=F
      ELSE
c     ------------------------------------------------------------------
       IF(regidx.eq.4.or.regidx.eq.5.or.regidx.eq.15.or.
     &    regidx.eq.16)THEN
        IF(Isrflw.eq.2)THEN
         CALL inpter(PERROR,Lstpos,
     &     'Cannot use flow trading day regressors for stock series.',T)
         Locok=F
        ELSE IF(Fulltd.AND.(.not.Nxtktp.eq.SLASH))THEN
         CALL inpter(PERROR,Lstpos,
     &               'Already have trading day effects.',T)
         Locok=F
        ELSE IF (Nxtktp.eq.SLASH.and.Lrgmtd) then
         CALL inpter(PERROR,Lstpos,
     &           'Already have change of regime trading day effects.',T)
         Locok=F
        ELSE
c        IF (.not.Havtd.OR.(Havtd.and.Tdzero.ne.0)) THEN
         IF(.not.Fulltd)THEN
          Picktd=regidx.eq.4.or.regidx.eq.15
          IF(regidx.eq.15.or.regidx.eq.16)THEN
           CALL adrgef(DNOTST,'Weekday','1-Coefficient Trading Day',
     &                 PRG1TD,F,T)
           IF(Lfatal)RETURN
          ELSE
           DO i=1,6
            CALL adrgef(DNOTST,day(i),'Trading Day',PRGTTD,F,T)
            IF(Lfatal)RETURN
           END DO
          END IF
         END IF
         IF(Nxtktp.eq.SLASH)THEN
          IF(regidx.eq.15.or.regidx.eq.16)THEN
           CALL adrgim(Begsrs,Nobs,Havesp,'1-Coefficient Trading Day',
     &                 PRR1TD,PRA1TD,zeroz,.not.Fulltd,Lrgmtd,Fulltd,
     &                 Locok)
          ELSE
           CALL adrgim(Begsrs,Nobs,Havesp,'Trading Day',PRRTTD,PRATTD,
     &                 zeroz,.not.Fulltd,Lrgmtd,Fulltd,Locok)
          END IF
          IF(Lfatal)RETURN
          Tdzero=zeroz
          IF(Picktd)Lnzero=zeroz
          IF(zeroz.eq.0)Fulltd=T
c          write(Mtprof,*) ' Tdzero = ', Tdzero
c          write(Mtprof,*) ' Lnzero = ', Lnzero
c          IF(Fulltd) write(Mtprof,*) ' Fulltd = .true.'
c          IF(Lrgmtd) write(Mtprof,*) ' Lrgmtd = .true.'
         ELSE
          Fulltd=T
         END IF
        END IF
       END IF
c-----------------------------------------------------------------------
c     Seventh trading day variables, lom, lpyear, lomstock
c-----------------------------------------------------------------------
       IF((regidx.eq.6.or.regidx.eq.7.or.regidx.eq.8).and.Picktd)THEN
        CALL inpter(PERROR,Lstpos,
     &              'Can''t add a length of month, quarter, or leap '//
     &              'year variable when using',F)
        CALL writln('        the td or td1coef option.',Mt2,STDERR,F,T)
        Locok=F
c-----------------------------------------------------------------------
       ELSE IF(regidx.eq.6.or.regidx.eq.7)THEN
c-----------------------------------------------------------------------
        IF(Havlp)THEN
         IF(Sp.eq.12)THEN
          CALL inpter(PERROR,Lstpos,'Can''t add a length of month '//
     &                'variable when using the leap year',F)
         ELSE
          CALL inpter(PERROR,Lstpos,'Can''t add a length of quarter '//
     &                'variable when using the leap year',F)
         END IF
         CALL writln('        variable.',Mt2,STDERR,F,T)
         Locok=F
c-----------------------------------------------------------------------
        ELSE IF(Isrflw.eq.2)THEN
         IF(regidx.eq.6)THEN
          CALL inpter(PERROR,Lstpos,
     &  'Cannot use flow length of month regressor for stock series.',F)
         ELSE
          CALL inpter(PERROR,Lstpos,
     &'Cannot use flow length of quarter regressor for stock series.',F)
         END IF
         Locok=F
        ELSE IF(Sp.eq.12)THEN
         IF(.not.Fullln)CALL adrgef(DNOTST,'Length-of-Month',
     &                              'Length-of-Month',PRGTLM,F,T)
         IF(.not.Lfatal.and.Nxtktp.eq.SLASH)THEN
          CALL adrgim(Begsrs,Nobs,Havesp,'Length-of-Month',PRRTLM,
     &                PRATLM,zeroz,.not.Fullln,Lrgmln,Fullln,Locok)
          Lnzero=zeroz
c          write(Mtprof,*) ' Lnzero = ', Lnzero
         END IF
        ELSE
c-----------------------------------------------------------------------
         IF(.not.Fullln)CALL adrgef(DNOTST,'Length-of-Quarter',
     &                              'Length-of-Quarter',PRGTLQ,F,T)
         IF(.not.Lfatal.and.Nxtktp.eq.SLASH)THEN
          CALL adrgim(Begsrs,Nobs,Havesp,'Length-of-Quarter',PRRTLQ,
     &                PRATLQ,zeroz,.not.Fullln,Lrgmln,Fullln,Locok)
          Lnzero=zeroz
c          write(Mtprof,*) ' Lnzero = ', Lnzero
         END IF
        END IF
        IF(zeroz.eq.0)Fullln=T
c-----------------------------------------------------------------------
       ELSE IF((.not.Havtd.AND.(regidx.eq.4.or.regidx.eq.15)).or.
     &         regidx.eq.8)THEN
        IF(Havln)THEN
         IF(Sp.eq.12)THEN
          CALL inpter(PERROR,Lstpos,'Can''t add a leap year variable'//
     &                ' when using the length of month',F)
         ELSE
          CALL inpter(PERROR,Lstpos,'Can''t add a leap year variable'//
     &                ' when using the length of quarter',F)
         END IF
         CALL writln('        variable.',Mt2,STDERR,F,T)
         Locok=F
c-----------------------------------------------------------------------
        ELSE IF(Isrflw.eq.2)THEN
         CALL inpter(PERROR,Lstpos,
     &        'Cannot use flow leap year regressor for stock series.',T)
         Locok=F
        ELSE
         IF((.not.Fulllp).and.(.not.(Lrgmtd.and.Picktd)))
     &      CALL adrgef(DNOTST,'Leap Year','Leap Year',PRGTLY,F,T)
         IF(Lfatal)RETURN
         IF(regidx.eq.8)THEN
          IF(Nxtktp.eq.SLASH)THEN
           CALL adrgim(Begsrs,Nobs,Havesp,'Leap Year',PRRTLY,PRATLY,
     &                   zeroz,.not.Fulllp,Lrgmln,Fulllp,Locok)
           Lnzero=zeroz
c           write(Mtprof,*) ' Lnzero = ', Lnzero
          END IF
          IF(zeroz.eq.0)Fulllp=T
         END IF
        END IF
c     ------------------------------------------------------------------
       ELSE IF(regidx.eq.10)THEN
        IF(Isrflw.eq.1)THEN
         CALL inpter(PERROR,Lstpos,
     &  'Cannot use stock length of month regressor for flow series.',T)
         Locok=F
        ELSE IF(Havlp)THEN
         CALL inpter(PERROR,Lstpos,'Can''t add a stock length of '//
     &               'month variable when using the',F)
         CALL writln('        leap year variable.',Mt2,STDERR,F,T)
         Locok=F
c-----------------------------------------------------------------------
        ELSE IF(Sp.ne.12)THEN
         CALL inpter(PERROR,Lstpos,
     &               'Need monthly data for stock trading day',T)
         Locok=F
         CALL lex()
c     ------------------------------------------------------------------
        ELSE
         IF(.not.Fullln)CALL adrgef(DNOTST,'Stock Length-of-Month',
     &                              'Stock Length-of-Month',PRGTSL,F,T)
         IF(Lfatal)RETURN
c         CALL lex()
         IF(Nxtktp.eq.SLASH)
     &      CALL adrgim(Begsrs,Nobs,Havesp,'Stock Length-of-Month',
     &                  PRRTSL,PRATSL,zeroz,.not.Fullln,Lrgmln,Fullln,
     &                  Locok)
        END IF
        IF(zeroz.eq.0)Fullln=T
       END IF
       IF(Lfatal)RETURN
       IF((regidx.eq.4.or.regidx.eq.5.or.regidx.eq.15.or.regidx.eq.16)
     &    .and.Locok)Havtd=T
       IF((regidx.eq.6.or.regidx.eq.7.or.regidx.eq.10).and.Locok)Havln=T
       IF(regidx.eq.8.and.Locok)Havlp=T
      END IF
c     ------------------------------------------------------------------
      GO TO 230
c-----------------------------------------------------------------------
c     First six stock trading day effects
c-----------------------------------------------------------------------
   50 CALL lex()
      IF(.not.Havesp)THEN
       CALL inpter(PERROR,Lstpos,
     &             'No seasonal period specified in series spec.',T)
       Locok=F
c     ------------------------------------------------------------------
      ELSE IF(Sp.ne.12)THEN
       CALL inpter(PERROR,Lstpos,
     &             'Need monthly data for stock trading day',T)
       Locok=F
c     ------------------------------------------------------------------
      ELSE IF(Begsrs(1).lt.1776)THEN
       CALL inpter(PERROR,Lstpos,
     &             'No trading variables before 1776.  Try including '//
     &             'the century in',F)
       CALL writln('        the start date.',Mt2,STDERR,F,T)
       Locok=F
      ELSE IF(Isrflw.eq.1)THEN
       CALL inpter(PERROR,Lstpos,
     &     'Cannot use stock trading day regressors for flow series.',T)
       Locok=F
      END IF
c     ------------------------------------------------------------------
      CALL getivc(LBRAKT,T,1,ivec,nelt,argok,Locok)
      IF(Lfatal)RETURN
      tdspdy=ivec(1)
c     ------------------------------------------------------------------
      IF(nelt.le.0)THEN
       CALL inpter(PERROR,Lstpos,
     &             'Must specify the Stock TD sample day explicitly',T)
       Locok=F
c     ------------------------------------------------------------------
      ELSE IF(tdspdy.le.0.or.tdspdy.gt.31)THEN
       CALL inpter(PERROR,Lstpos,'Stock TD sample day must be (1:31)',T)
*       CALL inpter(PERROR,Lstpos,'Stock TD sample day must be (28:31)')
       Locok=F
c     ------------------------------------------------------------------
      ELSE
       tgrptl='Stock Trading Day['
       ipos=19
*       IF(tdspdy.le.27.and.tdspdy.gt.0)THEN
*        CALL inpter(PWARN,Lstpos,'Stock TD sample day reset to 28')
*        tdspdy=28
*       END IF
       CALL itoc(tdspdy,tgrptl,ipos)
       IF(Lfatal)RETURN
       tgrptl(ipos:ipos)=']'
       nchr=ipos
c     ------------------------------------------------------------------
       IF(Fulltd.AND.(.not.Nxtktp.eq.SLASH))THEN
        CALL inpter(PERROR,Lstpos,
     &              'Already have stock trading day effects.',T)
        Locok=F
       ELSE IF (Nxtktp.eq.SLASH.and.Lrgmtd) then
        CALL inpter(PERROR,Lstpos,
     &     'Already have change of regime stock trading day effects.',T)
        Locok=F
       ELSE
c        IF (.not.Havtd.OR.(Havtd.and.Tdzero.ne.0)) THEN
        IF(.not.Fulltd)THEN
         IF(regidx.eq.17)THEN
          CALL adrgef(DNOTST,'Weekday','1-Coefficient '//tgrptl(1:nchr),
     &                PRG1ST,F,T)
         ELSE
          DO i=1,6
           CALL adrgef(DNOTST,day(i),tgrptl(1:nchr),PRGTST,F,T)
           IF(Lfatal)RETURN
          END DO
         END IF
        END IF
c       CALL lex()
        IF(Nxtktp.eq.SLASH)THEN
         IF(regidx.eq.17)THEN
          CALL adrgim(Begsrs,Nobs,Havesp,'1-Coefficient '//
     &                tgrptl(1:nchr),PRR1ST,PRA1ST,zeroz,.not.Fulltd,
     &                Lrgmtd,Fulltd,Locok)
         ELSE
          CALL adrgim(Begsrs,Nobs,Havesp,tgrptl(1:nchr),PRRTST,PRATST,
     &                zeroz,.not.Fulltd,Lrgmtd,Fulltd,Locok)
         END IF
         Tdzero=zeroz
        ELSE
         Fulltd=T
        END IF
        Havtd=T
        IF(Lfatal)RETURN
       END IF
      END IF
      GO TO 230
c-----------------------------------------------------------------------
c     Easter effect
c-----------------------------------------------------------------------
   60 CALL lex()
      IF(regidx.eq.18)THEN
       Easidx=0
      ELSE
       Easidx=regidx-11
      END IF
      IF(.not.Havesp)THEN
       CALL inpter(PERROR,Lstpos,
     &             'No seasonal period specified in series spec.',T)
       Locok=F
c     ------------------------------------------------------------------
      ELSE IF(Sp.ne.12.and.Sp.ne.4)THEN
       CALL inpter(PERROR,Lstpos,
     &         'Need monthly or quarterly data for an Easter effect',T)
       Locok=F
c     ------------------------------------------------------------------
      ELSE IF(Begsrs(1).lt.1901)THEN
       CALL inpter(PERROR,Lstpos,
     &             'No Easter effect before 1901.  Try including the '//
     &             'century in the',F)
       CALL writln('        start date.',Mt2,STDERR,F,T)
       Locok=F
      ELSE IF(Isrflw.eq.2.and.regidx.lt.18)THEN
       CALL inpter(PERROR,Lstpos,
     &             'Cannot use Easter regressor for stock series.',T)
       Locok=F
      ELSE IF(Isrflw.eq.1.and.regidx.eq.18)THEN
       CALL inpter(PERROR,Lstpos,
     &           'Cannot use stock Easter regressor for flow series.',T)
       Locok=F
      END IF
c     ------------------------------------------------------------------
      CALL addate(Begsrs,Sp,Nobs-1,tmpdat)
      IF(tmpdat(1).gt.2100)THEN
       CALL inpter(PERROR,Lstpos,
     &             'Cannot compute holiday effect after 2100',T)
       Locok=F
      END IF
c     ------------------------------------------------------------------
      CALL getivc(LBRAKT,T,1,ivec,nelt,argok,Locok)
      IF(Lfatal)RETURN
      neastr=ivec(1)
c     ------------------------------------------------------------------
      IF(nelt.le.0)THEN
       CALL inpter(PERROR,Errpos,
     &             'Must specify the Easter window length explicitly',T)
       Locok=F
c     ------------------------------------------------------------------
      ELSE IF(neastr.lt.(0+Easidx).or.neastr.gt.ewlim(Easidx))THEN
       IF(Easidx.eq.0)THEN
        CALL inpter(PERROR,Errpos,
     &              'The Easter window must be from 0 to 25.',T)
       ELSE
        CALL inpter(PERROR,Errpos,
     &    'The Statistics Canada Easter window must be from 1 to 24.',T)
       END IF
       Locok=F
c     ------------------------------------------------------------------
      ELSE
       IF(Easidx.eq.0)THEN
        IF(regidx.eq.18)THEN
         effttl='StockEaster['
         ipos=13
        ELSE
         effttl='Easter['
         ipos=8
        END IF
       ELSE IF(Easidx.eq.1)THEN
        effttl='StatCanEaster['
        ipos=15
       END IF
       CALL itoc(neastr,effttl,ipos)
       IF(Lfatal)RETURN
       effttl(ipos:ipos)=']'
       nchr=ipos
       IF(Easidx.eq.0)THEN
        IF(regidx.eq.18)THEN
         CALL adrgef(DNOTST,effttl(1:nchr),'StockEaster',PRGTES,F,T)
        ELSE
         CALL adrgef(DNOTST,effttl(1:nchr),'Easter',PRGTEA,F,T)
        END IF
       ELSE IF(Easidx.eq.1)THEN
        CALL adrgef(DNOTST,effttl(1:nchr),'StatCanEaster',PRGTEC,F,T)
       END IF
       IF(Lfatal)RETURN
       Havhol=T
      END IF
      GO TO 230
c-----------------------------------------------------------------------
c     Labor day effect
c-----------------------------------------------------------------------
   70 CALL lex()
      igrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Labor')
      IF(.not.Havesp)THEN
       CALL inpter(PERROR,Lstpos,
     &             'No seasonal period specified in series spec.',T)
       Locok=F
c     ------------------------------------------------------------------
      ELSE IF(Sp.ne.12)THEN
       CALL inpter(PERROR,Lstpos,
     &             'Need monthly data for a Labor Day effect',T)
       Locok=F
c     ------------------------------------------------------------------
      ELSE IF(Begsrs(1).lt.1901)THEN
       CALL inpter(PERROR,Lstpos,
     &             'No Labor Day effect before 1901.  Try including '//
     &             'the century in',F)
       CALL writln('        the start date',Mt2,STDERR,F,T)
       Locok=F
c     ------------------------------------------------------------------
      ELSE IF(Isrflw.eq.2)THEN
       CALL inpter(PERROR,Lstpos,
     &             'Cannot use Labor Day regressor for stock series.',T)
       Locok=F
c     ------------------------------------------------------------------
      ELSE IF(igrp.gt.0)THEN
       CALL inpter(PERROR,Lstpos,'A Labor Day regressor is already '//
     &             'included in the regARIMA model.',T)
       Locok=F
      END IF
c     ------------------------------------------------------------------
      CALL addate(Begsrs,Sp,Nobs-1,tmpdat)
      IF(tmpdat(1).gt.2100)THEN
       CALL inpter(PERROR,Lstpos,
     &             'Cannot compute holiday effect after 2100',T)
       Locok=F
      END IF
c     ------------------------------------------------------------------
      CALL getivc(LBRAKT,T,1,ivec,nelt,argok,Locok)
      IF(Lfatal)RETURN
      nlabor=ivec(1)
c     ------------------------------------------------------------------
      IF(nelt.le.0)THEN
       CALL inpter(PERROR,Errpos,
     &          'Must specify the Labor Day window length explicitly',T)
       Locok=F
c     ------------------------------------------------------------------
      ELSE IF(nlabor.le.0.or.nlabor.gt.25)THEN
       CALL inpter(PERROR,Errpos,
     &             'The Labor Day window must be from 1 to 25.',T)
       Locok=F
c     ------------------------------------------------------------------
      ELSE
       effttl='Labor['
       ipos=7
       CALL itoc(nlabor,effttl,ipos)
       IF(Lfatal)RETURN
       effttl(ipos:ipos)=']'
       nchr=ipos
       CALL adrgef(DNOTST,effttl(1:nchr),effttl(1:nchr),PRGTLD,F,T)
       IF(Lfatal)RETURN
       Havhol=T
      END IF
      GO TO 230
c-----------------------------------------------------------------------
c     Thanksgiving-Christmas effect
c-----------------------------------------------------------------------
   80 CALL lex()
      igrp=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Thanksgiving')
c     ------------------------------------------------------------------
      IF(.not.Havesp)THEN
       CALL inpter(PERROR,Lstpos,
     &             'No seasonal period specified in series spec.',T)
       Locok=F
c     ------------------------------------------------------------------
      ELSE IF(Sp.ne.12)THEN
       CALL inpter(PERROR,Lstpos,
     &    'Need monthly data for a Thanksgiving-Christmas day effect',T)
       Locok=F
c     ------------------------------------------------------------------
      ELSE IF(Begsrs(1).lt.1939)THEN
       CALL inpter(PERROR,Lstpos,
     &   'Thanksgiving-Christmas day effect not defined before 1939.',
     &   Begsrs(1).ge.100)
       IF(Begsrs(1).lt.100)
     &    CALL writln(
     &       '        Try including the century in the start date.',
     &                Mt2,STDERR,F,T)
       Locok=F
c     ------------------------------------------------------------------
      ELSE IF(Isrflw.eq.2)THEN
       CALL inpter(PERROR,Lstpos,
     &'Cannot use Thanksgiving-Christmas regressor for stock series.',T)
       Locok=F
c     ------------------------------------------------------------------
      ELSE IF(igrp.gt.0)THEN
       CALL inpter(PERROR,Lstpos,'A Thanksgiving-Christmas regressor '//
     &             'is already included ',F)
       CALL writln('        in the regARIMA model.',Mt2,STDERR,F,T)
       Locok=F
      END IF
c     ------------------------------------------------------------------
      CALL addate(Begsrs,Sp,Nobs-1,tmpdat)
      IF(tmpdat(1).gt.2100)THEN
       CALL inpter(PERROR,Lstpos,
     &             'Cannot compute holiday effect after 2100',T)
       Locok=F
      END IF
c     ------------------------------------------------------------------
      CALL getivc(LBRAKT,T,1,ivec,nelt,argok,Locok)
      IF(Lfatal)RETURN
      nthank=ivec(1)
c     ------------------------------------------------------------------
      IF(nelt.le.0)THEN
       CALL inpter(PERROR,Errpos,
     &   'Must specify the Thanksgiving day window length explicitly',T)
       Locok=F
c     ------------------------------------------------------------------
      ELSE IF(nthank.lt.-8.or.nthank.gt.17.or.nthank.eq.0)THEN
       CALL inpter(PERROR,Errpos,'The Thanksgiving day window must '//
     &             'be from -8 to 17 (excluding 0)',T)
c     ------------------------------------------------------------------
      ELSE
       effttl='Thanksgiving['
       ipos=14
       CALL itoc(nthank,effttl,ipos)
       IF(Lfatal)RETURN
       effttl(ipos:ipos)=']'
       nchr=ipos
       CALL adrgef(DNOTST,effttl(1:nchr),effttl(1:nchr),PRGTTH,F,T)
       IF(Lfatal)RETURN
       Havhol=T
      END IF
  230 Inptok=Inptok.and.Locok
c     -----------------------------------------------------------------
      RETURN
      END
