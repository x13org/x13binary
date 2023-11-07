C     Last change:  BCM  29 Jan 1999   11:36 am
      SUBROUTINE getadj(Begsrs,Havsrs,Havesp,Sp,Begspn,Nspobs,Endspn,
     &                  Usrtad,Nustad,Bgutad,Tmpnam,Ntser,Usrpad,Nuspad,
     &                  Bgupad,Prmnam,Npser,Adjttl,Nadjtl,Priadj,Reglom,
     &                  Fcntyp,Lam,Prtype,Nprtyp,Percnt,Traicd,Lprntr,
     &                  Hvx12f,Cnstnt,Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     getadj.f, Release 1, Subroutine Version 1.6, Modified 16 Feb 1995.
c-----------------------------------------------------------------------
c     Gets the Box-Cox transformation parameter, a series of adjustments
c adjustments, and flags the length-of-month adjustment (prilom) as
c opposed to the regression lom.
c-----------------------------------------------------------------------
c     Variable typing and parameters initialization
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'lex.i'
      INCLUDE 'tbllog.i'
      INCLUDE 'svllog.i'
      INCLUDE 'notset.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      CHARACTER DBLMSG*(34)
      INTEGER PNADJ,YR,MO
      LOGICAL T,F
      DOUBLE PRECISION ZERO
      PARAMETER(DBLMSG='Use either data or file, not both',YR=1,MO=2,
     &          PNADJ=2,T=.true.,F=.false.,ZERO=0D0)
C-----------------------------------------------------------------------
      CHARACTER adjfmt*(PFILCR),Adjttl*(*),adfile*(PFILCR),srsnam*(64),
     &          fmtstr*(PNADJ*PFILCR),filstr*(PNADJ*PFILCR),tmpnam*(64),
     &          Prmnam*(64),namstr*(PNADJ*64)
      LOGICAL argok,Hvx12f,havadj,Havesp,Havsrs,hvstrt,hvfile,Inptok,
     &        havenm,hvafmt,havtad,havpad,havttl,Lprntr
      INTEGER Begspn,Begsrs,Bgutad,Bgupad,bgusra,Endspn,Fcntyp,Nadjtl,
     &        nadfmt,Nspobs,Nuspad,Nustad,Priadj,Reglom,Sp,tmpptr,
     &        Percnt,numdec,ivec,Prtype,Nprtyp,namptr,fmtptr,
     &        filptr,numnam,nfmt,numfil,nadtmp,tmpdat,ndate,numper,nd,
     &        nelt,nflchr,decvec,i,nsrs,Ntser,Npser,numpri,ltrim
      DOUBLE PRECISION Usrtad,Usrpad,adjtmp,Lam,dvec,Traicd,Cnstnt
      DIMENSION Begspn(2),Begsrs(2),Bgutad(2),Bgupad(2),bgusra(2),
     &          Usrtad(PLEN),Usrpad(PLEN),adjtmp(PNADJ*PLEN),Endspn(2),
     &          Prtype(PNADJ),Hvx12f(PNADJ),namptr(0:PNADJ),dvec(1),
     &          ivec(1),fmtptr(0:PNADJ),filptr(0:PNADJ),tmpdat(2,PNADJ),
     &          tmpptr(0:1),Percnt(PNADJ),decvec(PNADJ)
c     ------------------------------------------------------------------
      INTEGER strinx
      LOGICAL chkcvr,gtarg,dpeq
      EXTERNAL chkcvr,gtarg,strinx,dpeq
c     ------------------------------------------------------------------
      CHARACTER ARGDIC*126
      INTEGER arglog,argidx,argptr,PARG
      PARAMETER(PARG=20)
      DIMENSION argptr(0:PARG),arglog(2,PARG)
      PARAMETER(ARGDIC='datastarttitlefileformatadjustadjustregpowerfunc
     &tionprintsavemodenametypeprecisionsavelogaicdifftrimzerotemppriort
     &rendconstant')
c     ------------------------------------------------------------------
      CHARACTER ADJDIC*16
      INTEGER adjptr,PADJ
      PARAMETER(PADJ=4)
      DIMENSION adjptr(0:PADJ)
      PARAMETER(ADJDIC='nonelomloqlpyear')
c     ------------------------------------------------------------------
      CHARACTER RGADIC*9
      INTEGER rgaptr,PRGA
      PARAMETER(PRGA=3)
      DIMENSION rgaptr(0:PRGA)
      PARAMETER(RGADIC='nonetdall')
c-----------------------------------------------------------------------
      CHARACTER MODDIC*18
      INTEGER modptr,PMOD
      PARAMETER(PMOD=3)
      DIMENSION modptr(0:PMOD)
      PARAMETER(MODDIC='percentratiodiff')
c     ------------------------------------------------------------------
      CHARACTER FCNDIC*30
      INTEGER fcnptr,PFCN
      PARAMETER(PFCN=6)
      DIMENSION fcnptr(0:PFCN)
      PARAMETER(FCNDIC='logsqrtlogisticnoneinverseauto')
c     ------------------------------------------------------------------
      CHARACTER TYPDIC*26
      INTEGER typptr,PATYPE
      PARAMETER(PATYPE=4)
      DIMENSION typptr(0:PATYPE)
      PARAMETER(TYPDIC='temporarypermanenttempperm')
c-----------------------------------------------------------------------
      CHARACTER XFSDIC*14
      INTEGER xfsptr,PXFS
      PARAMETER(PXFS=2)
      DIMENSION xfsptr(0:PXFS)
      PARAMETER(XFSDIC='x12savex13save')
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
c-----------------------------------------------------------------------
      DATA fcnptr/1,4,8,16,20,27,31/
      DATA argptr/1,5,10,15,19,25,31,40,45,53,58,62,66,70,74,83,90,97,
     &            105,119,127/
      DATA adjptr/1,5,8,11,17/
      DATA rgaptr/1,5,7,10/
      DATA modptr/1,8,13,17/
      DATA typptr/1,10,19,23,27/
      DATA xfsptr/1,8,15/
      DATA ysnptr/1,4,6/
      DATA zroptr/1,4,8,10/
c-----------------------------------------------------------------------
c     Assume the input is OK and we don't have any of the arguments
c-----------------------------------------------------------------------
      havadj=F
      hvafmt=F
      havpad=F
      havtad=F
      hvfile=F
      CALL setlg(F,PNADJ,Hvx12f)
      hvstrt=F
      havttl=F
      havenm=F
      ltrim=0
      nadfmt=1
      numnam=0
      numper=0
      nfmt=0
      numfil=0
      CALL setint(3,PNADJ,decvec)
      CALL setint(NOTSET,2*PARG,arglog)
c-----------------------------------------------------------------------
c     Initialize the starting date
c-----------------------------------------------------------------------
      CALL cpyint(Begsrs,2,1,bgusra)
      CALL setint(NOTSET,2*PNADJ,tmpdat)
c-----------------------------------------------------------------------
c     Initialize the format and file
c-----------------------------------------------------------------------
      CALL setchr(' ',PFILCR,adfile)
      CALL setchr(' ',PFILCR,adjfmt)
c-----------------------------------------------------------------------
      IF(.not.Havsrs)THEN
       CALL inpter(PERROR,Errpos,
     &             'Specify series before user-defined adjustments',T)
       Inptok=F
      END IF
c-----------------------------------------------------------------------
      DO WHILE (T)
c-----------------------------------------------------------------------
c     Argument get loop
c-----------------------------------------------------------------------
       IF(gtarg(ARGDIC,argptr,PARG,argidx,arglog,Inptok))THEN
        IF(Lfatal)RETURN
        GO TO(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,170,
     &        160,175,176,177)argidx
c-----------------------------------------------------------------------
c     Data argument
c-----------------------------------------------------------------------
   10   IF(hvfile)THEN
         CALL inpter(PERROR,Errpos,DBLMSG,T)
         Inptok=F
        END IF
c     ------------------------------------------------------------------
        CALL gtdpvc(LPAREN,T,PLEN*PNADJ,adjtmp,nadtmp,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
        IF(nadtmp.gt.0)THEN
         IF(argok)THEN
          havadj=T
         ELSE
          nadtmp=0
         END IF
c     ------------------------------------------------------------------
        END IF
        GO TO 180
c-----------------------------------------------------------------------
c     Start argument
c-----------------------------------------------------------------------
   20   CALL gtdtvc(Havesp,Sp,LPAREN,F,PNADJ,tmpdat,ndate,argok,Inptok)
        IF(Lfatal)RETURN
        hvstrt=argok.and.ndate.gt.0
        GO TO 180
c-----------------------------------------------------------------------
c     Title argument
c-----------------------------------------------------------------------
   30   CALL getttl(LPAREN,T,1,Adjttl,tmpptr,nelt,argok,Inptok)
        IF(.not.Lfatal.and.argok.and.nelt.gt.0)THEN
         CALL eltlen(1,tmpptr,nelt,Nadjtl)
         havttl=T
        END IF
        IF(Lfatal)RETURN
        GO TO 180
c-----------------------------------------------------------------------
c     File argument
c-----------------------------------------------------------------------
   40   IF(havadj)THEN
         CALL inpter(PERROR,Errpos,DBLMSG,T)
         Inptok=F
        END IF
c     ------------------------------------------------------------------
        CALL gtnmvc(LPAREN,T,PNADJ,filstr,filptr,numfil,PFILCR,argok,
     &              Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.numfil.gt.0)hvfile=T
        GO TO 180
c-----------------------------------------------------------------------
c     Format argument
c-----------------------------------------------------------------------
   50   CALL gtnmvc(LPAREN,T,PNADJ,fmtstr,fmtptr,nfmt,PFILCR,argok,
     &              Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nfmt.gt.0)hvafmt=T
        GO TO 180
c-----------------------------------------------------------------------
c     Predefined prior adjustment argument (1=none, 2=lom, 3=loq,
c and 4=lpyear)
c-----------------------------------------------------------------------
   60   CALL gtdcvc(LPAREN,T,1,ADJDIC,adjptr,PADJ,
     &             'The predefined adjustments are lom, loq, or lpyear.'
     &             ,ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
        IF(nelt.gt.0)THEN
         Priadj=ivec(1)
         IF(argok.and.Priadj.gt.1.and..not.Havesp)THEN
          CALL inpter(PERROR,Errpos,
     &                'No seasonal period specified in series spec.',T)
          Inptok=F
c     ------------------------------------------------------------------
         ELSE IF(Sp.ne.12.and.Sp.ne.4)THEN
          CALL inpter(PERROR,Errpos,
     &               ' Need monthly or quarterly data for adjustment',T)
          Inptok=F
c     ------------------------------------------------------------------
         ELSE IF(Begsrs(1).lt.1776)THEN
          CALL inpter(PERROR,Errpos,
     &                'No adjustment before 1776.  Try including the '//
     &                'century in the start date',T)
          Inptok=F
         END IF
c-----------------------------------------------------------------------
c     Correct length of month and length of quarter errors
c-----------------------------------------------------------------------
         IF(Priadj.eq.2.and.Sp.eq.4)Priadj=3
         IF(Priadj.eq.3.and.Sp.eq.12)Priadj=2
        END IF
        GO TO 180
c-----------------------------------------------------------------------
c     Regression variable prior adjustment argument (1=none, 2=td, and
c 3=all).  Regadjust determines which variables are going to be adjusted
c and ajust determines the type of adjustment.
c-----------------------------------------------------------------------
   70   CALL gtdcvc(LPAREN,T,1,RGADIC,rgaptr,PRGA,
     &              'The predefined adjustments are none, td, or all',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
        IF(nelt.gt.0)THEN
         IF(argok.and.ivec(1).gt.0.and..not.Havesp)THEN
          CALL inpter(PERROR,Errpos,
     &                'No seasonal period specified in series spec.',T)
          Inptok=F
         ELSE IF(Sp.ne.12.and.Sp.ne.4)THEN
          CALL inpter(PERROR,Errpos,
     &                'Need monthly or quarterly data for adjustment',T)
          Inptok=F
         ELSE
          Reglom=ivec(1)
         END IF
        END IF
        GO TO 180
c-----------------------------------------------------------------------
c     Box-Cox power transformation parameter
c-----------------------------------------------------------------------
   80   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        Lam=dvec(1)
c     ------------------------------------------------------------------
        IF(nelt.gt.0)THEN
         IF(argok)THEN
          Fcntyp=5
         ELSE
          CALL inpter(PERROR,Errpos,
     &          'Enter a real number for the Box-Cox Transformation.',T)
          CALL lex()
          Inptok=F
         END IF
        END IF
        GO TO 180
c-----------------------------------------------------------------------
c     Box-Cox and other function specified by name
c-----------------------------------------------------------------------
   90   CALL gtdcvc(LPAREN,T,1,FCNDIC,fcnptr,PFCN,
     &        'Choices are log, sqrt, inverse, logistic, auto, or none',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
        IF(nelt.gt.0)THEN
         IF(argok.and.ivec(1).eq.1)THEN
          Fcntyp=1
          Lam=0D0
         ELSE IF(ivec(1).eq.2)THEN
          Fcntyp=6
          Lam=.5D0
         ELSE IF(ivec(1).eq.4)THEN
          Fcntyp=4
          Lam=1D0
         ELSE IF(ivec(1).eq.5)THEN
          Fcntyp=6
          Lam=-1D0
         ELSE IF(ivec(1).eq.3)THEN
          Fcntyp=3
          Lam=DNOTST
         ELSE IF(ivec(1).eq.6)THEN
          Fcntyp=0
          Lam=DNOTST
         END IF
        END IF
        GO TO 180
c-----------------------------------------------------------------------
c     Print argument
c-----------------------------------------------------------------------
  100   CALL getprt(LSPTRN,NSPTRN,Inptok)
        GO TO 180
c-----------------------------------------------------------------------
c     Save argument
c-----------------------------------------------------------------------
  110   CALL getsav(LSPTRN,NSPTRN,Inptok)
        GO TO 180
c-----------------------------------------------------------------------
c     Prior factor mode
c-----------------------------------------------------------------------
  120   CALL gtdcvc(LPAREN,T,PNADJ,MODDIC,modptr,PMOD,
     &              'Choices are percent, ratio, and diff',
     &              Percnt,numper,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.numper.gt.0)THEN
         DO i=1,numper
          Percnt(i)=Percnt(i)-1
         END DO
        END IF
        GO TO 180
c-----------------------------------------------------------------------
c     Series name argument
c-----------------------------------------------------------------------
  130   CALL gtnmvc(LPAREN,T,PNADJ,namstr,namptr,numnam,64,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.numnam.gt.0)havenm=T
        GO TO 180
c-----------------------------------------------------------------------
c     Type parameter
c-----------------------------------------------------------------------
  140   CALL gtdcvc(LPAREN,T,PNADJ,TYPDIC,typptr,PATYPE,
     &              'Choices are temporary, temp, permanent and perm.',
     &              Prtype,Nprtyp,T,argok,Inptok)
        IF(Lfatal)RETURN
c     ------------------------------------------------------------------
        IF(argok.and.Nprtyp.gt.0)THEN
         DO i=1,Nprtyp
          IF(Prtype(i).gt.2)Prtype(i)=Prtype(i)-2
         END DO
        END IF
        GO TO 180
c-----------------------------------------------------------------------
c     Precision argument
c-----------------------------------------------------------------------
  150   CALL getivc(LPAREN,T,PNADJ,decvec,numdec,argok,Inptok)
        IF(Lfatal)RETURN
        IF(numdec.gt.0)THEN
         IF(.not.argok)THEN
          CALL inpter(PERROR,Errpos,'Invalid number of input decimals',
     &                T)
          Inptok=F
         ELSE 
          DO i=1,numdec
           IF(decvec(i).lt.0.or.decvec(i).gt.5)THEN
            CALL inpter(PERROR,Errpos,
     &                  'Number of input decimals must be between 0 '//
     &                  'and 5, inclusive',T)
            Inptok=F
           END IF
          END DO
         END IF
        END IF
        GO TO 180
c-----------------------------------------------------------------------
c     AIC test difference for the transformation AIC test
c-----------------------------------------------------------------------
  160   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(nelt.gt.0.and.argok)Traicd=dvec(1)
        GO TO 180
c-----------------------------------------------------------------------
c     savelog  argument
c-----------------------------------------------------------------------
  170   CALL getsvl(LSLADJ,NSLADJ,Inptok)
        GO TO 180
c-----------------------------------------------------------------------
c     trimzero argument
c-----------------------------------------------------------------------
  175   CALL gtdcvc(LPAREN,T,1,ZRODIC,zroptr,PZRO,
     &            'Available options for trimzero are yes, span or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)ltrim=ivec(1)-1
        GO TO 180
c-----------------------------------------------------------------------
c     temppriortrend argument
c-----------------------------------------------------------------------
  176   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &            'Available options for temppriortrend are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lprntr=ivec(1).eq.1
        GO TO 180
c     ------------------------------------------------------------------
c     Constant argument
c     ------------------------------------------------------------------
  177   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(dvec(1).lt.ZERO.or.dpeq(dvec(1),ZERO))THEN
         CALL inpter(PERROR,Errpos,
     &      'Constant argument cannot be less than or equal to zero.',T)
         Inptok=F
        ELSE
         Cnstnt=dvec(1)
        END IF
        GO TO 180
       END IF
c     ------------------------------------------------------------------
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     set how many prior adjustments there are
c-----------------------------------------------------------------------
       IF(Nprtyp.gt.0)THEN
        numpri=Nprtyp
       ELSE IF(hvfile.or.havadj)THEN
        IF(numfil.eq.2.or.numnam.eq.2)THEN
         CALL eWritln('If more than one prior adjustment factor is '//
     &                'read in, a type must',STDERR,Mt2,T,F)
         CALL writln('       be specified for each.',STDERR,Mt2,F,T)
         Inptok=F
        ELSE 
         numpri=1
         Prtype(1)=2
         Nprtyp=numpri
        END IF
       ELSE
        numpri=0
       END IF
c-----------------------------------------------------------------------
       IF(numpri.gt.0)THEN
        IF(tmpdat(YR,1).ne.NOTSET)THEN
         bgusra(YR)=tmpdat(YR,1)
         bgusra(MO)=tmpdat(MO,1)
        END IF
        CALL setchr(' ',64,srsnam)
        IF(numnam.gt.0)THEN
         IF(numpri.gt.numnam)THEN
          CALL eWritln('If a series name is specified, there should '//
     &                 'be a name for each ',STDERR,Mt2,T,F)
          CALL writln('       prior adjustment series specifed.',
     &                STDERR,Mt2,F,T)
          Inptok=F
         ELSE
          CALL getstr(namstr,namptr,numnam,1,srsnam,nsrs)
         END IF
        ELSE
         nsrs=1
        END IF
       END IF
c-----------------------------------------------------------------------
c     If the data are from the file get the data
c-----------------------------------------------------------------------
       IF(Inptok.and.hvfile.and..not.havadj)THEN
c-----------------------------------------------------------------------
c     initialize variables needed for file input.
c-----------------------------------------------------------------------
        CALL getstr(filstr,filptr,numfil,1,adfile,nflchr)
        IF(hvafmt)THEN
         CALL getstr(fmtstr,fmtptr,nfmt,1,adjfmt,nadfmt)
         IF(strinx(F,XFSDIC,xfsptr,1,PXFS,adjfmt(1:nadfmt)).gt.0)
     &      Hvx12f(1)=T
        END IF
        nd=decvec(1)
c-----------------------------------------------------------------------
c     IF only one file specified for two sets of preadjustment factors, 
c     read both sets of data into a temporary data set to be processed
c     later.
c-----------------------------------------------------------------------
        IF(numfil.eq.1.and.numpri.gt.1)THEN
         CALL gtfldt(PLEN*PNADJ,adfile,nflchr,hvafmt,
     &               adjfmt,nadfmt,ltrim,adjtmp,nadtmp,Havesp,Sp,
     &               havenm,srsnam,nsrs,havttl,Adjttl,Nadjtl,nd,hvstrt,
     &               bgusra,numnam,Begspn,Endspn,F,argok,Inptok)
         IF(argok)havadj=T
        ELSE
c-----------------------------------------------------------------------
c     ELSE, read the series from the separate files and assign the
c     data from each to either the temporary or permanent prior 
c     adjustment factors.
c-----------------------------------------------------------------------
         DO i=1,numpri
c-----------------------------------------------------------------------
c     reset variables needed for file input, if necessary.
c-----------------------------------------------------------------------
          IF(i.gt.1)THEN
           CALL getstr(filstr,filptr,numfil,i,adfile,nflchr)
           IF(numnam.gt.0)CALL getstr(namstr,namptr,numnam,i,srsnam,
     &                                nsrs)
           IF(hvafmt.and.nfmt.gt.1)THEN
            CALL getstr(fmtstr,fmtptr,nfmt,i,adjfmt,nadfmt)
            IF(strinx(F,XFSDIC,xfsptr,1,PXFS,adjfmt(1:nadfmt)).gt.0)
     &         Hvx12f(i)=T
           END IF
           IF(numdec.gt.1)nd=decvec(1)
           IF(tmpdat(YR,i).ne.NOTSET)THEN
            bgusra(YR)=tmpdat(YR,i)
            bgusra(MO)=tmpdat(MO,i)
           END IF
          END IF
c-----------------------------------------------------------------------
c     Get data from file
c-----------------------------------------------------------------------
          CALL gtfldt(PLEN,adfile,nflchr,hvafmt,adjfmt,nadfmt,ltrim,
     &                adjtmp,nadtmp,Havesp,Sp,havenm,srsnam,nsrs,havttl,
     &                Adjttl,Nadjtl,nd,hvstrt,bgusra,1,Begspn,Endspn,F,
     &                argok,Inptok)
c-----------------------------------------------------------------------
c     Put results into variable for correct type of prior adjustment
c-----------------------------------------------------------------------
          IF(Prtype(i).eq.1)THEN
           IF(numnam.eq.0)THEN
            srsnam(1:7)='TempAdj'
            nsrs=7
           END IF
           CALL setadj(Usrtad,Nustad,Tmpnam,Ntser,Bgutad,havtad,Nprtyp,
     &                 adjtmp,nadtmp,bgusra,srsnam,nsrs,0,Argok)
          ELSE
           IF(numnam.eq.0)THEN
            srsnam(1:7)='PermAdj'
            nsrs=7
           END IF
           CALL setadj(Usrpad,Nuspad,Prmnam,Npser,Bgupad,havpad,Nprtyp,
     &                 adjtmp,nadtmp,bgusra,srsnam,nsrs,0,Argok)
          END IF
         END DO
        END IF
       END IF
c     ------------------------------------------------------------------
c     IF data stored temporarily as matrix, separate results into 
c     variables for correct type of prior adjustment
c     ------------------------------------------------------------------
       IF(Inptok.and.havadj)THEN
        DO i=1,Nprtyp
         IF(numnam.gt.0)CALL getstr(namstr,namptr,numnam,i,srsnam,nsrs)
         IF(Prtype(i).eq.1)THEN
          IF(numnam.eq.0)THEN
           srsnam(1:7)='TempAdj'
           nsrs=7
          END IF
          CALL setadj(Usrtad,Nustad,Tmpnam,Ntser,Bgutad,havtad,Nprtyp,
     &                adjtmp,nadtmp,bgusra,srsnam,nsrs,i,Argok)
         ELSE IF(Prtype(i).eq.2)THEN
          IF(numnam.eq.0)THEN
           srsnam(1:7)='PermAdj'
           nsrs=7
          END IF
          CALL setadj(Usrpad,Nuspad,Prmnam,Npser,Bgupad,havpad,Nprtyp,
     &                adjtmp,nadtmp,bgusra,srsnam,nsrs,i,Argok)
         END IF
        END DO
       END IF
       havadj=havtad.or.havpad
c     ------------------------------------------------------------------
       IF(hvstrt.and..not.havadj)THEN
        CALL eWritln(
     &     'Have a start date without user-defined adjustments.',
     &     STDERR,Mt2,T,T)
        Inptok=F
       END IF
c     ------------------------------------------------------------------
       IF(havtad.and..not.chkcvr(Bgutad,Nustad,Begspn,Nspobs,Sp))THEN
        CALL cvrerr('temporary adjustments',Bgutad,Nustad,'span',Begspn,
     &              Nspobs,Sp)
        IF(Lfatal)RETURN
        Inptok=F
       END IF
       IF(havpad.and..not.chkcvr(Bgupad,Nuspad,Begspn,Nspobs,Sp))THEN
        CALL cvrerr('permanent adjustments',Bgupad,Nuspad,'span',Begspn,
     &              Nspobs,Sp)
        IF(Lfatal)RETURN
        Inptok=F
       END IF
c     ------------------------------------------------------------------
       IF(numper.lt.numpri)THEN
        DO i=2,Nprtyp
         Percnt(i)=Percnt(1)
        END DO
       END IF
c     ------------------------------------------------------------------
       RETURN
  180  CONTINUE
      END DO
c     ------------------------------------------------------------------
      END
