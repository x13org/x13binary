C     Last change:  BCM  14 Oct 1998    4:01 pm
      SUBROUTINE getchk(Mxcklg,Acflim,Qcheck,Iqtype,Sp,Inptok)
c     ------------------------------------------------------------------
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INCLUDE 'lex.i'
      INCLUDE 'stdio.i'
      INCLUDE 'notset.prm'
      INCLUDE 'tbllog.i'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'svllog.i'
      INCLUDE 'mdltbl.i'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'units.cmn'
c     ------------------------------------------------------------------
      LOGICAL F,T
      DOUBLE PRECISION ONE,ZERO
      PARAMETER(ONE=1.0D0,ZERO=0.0D0,F=.false.,T=.true.)
c     ------------------------------------------------------------------
      DOUBLE PRECISION Acflim,Qcheck,dvec
      LOGICAL argok,Inptok
      INTEGER Mxcklg,Iqtype,nelt,ivec,Sp
      DIMENSION dvec(1),ivec(1)
c-----------------------------------------------------------------------
      LOGICAL gtarg
      EXTERNAL gtarg
c-----------------------------------------------------------------------
c     Argument dictionary was made with the following command
c ../../dictionary/strary < ../../dictionary/check.dic
c-----------------------------------------------------------------------
      CHARACTER ARGDIC*41
      INTEGER arglog,argidx,argptr,PARG
      PARAMETER(PARG=7)
      DIMENSION argptr(0:PARG),arglog(2,PARG)
      PARAMETER(ARGDIC='maxlagprintsavesavelogacflimitqtypeqlimit')
c     ------------------------------------------------------------------
c     Q stat type dictionary
c     ------------------------------------------------------------------
      CHARACTER QDIC*21
      INTEGER qptr,QARG
      PARAMETER(QARG=4)
      DIMENSION qptr(0:QARG)
      PARAMETER(QDIC='ljungboxlbboxpiercebp')
c-----------------------------------------------------------------------
      DATA argptr/1,7,12,16,23,31,36,42/
      DATA qptr/1,9,11,20,22/
c-----------------------------------------------------------------------
c     If Mxcklg is 0 if check{} is not specified and the acf's and
c pacf's are not printed out so Mxcklg is set to the default 36
c (for monthly; 12 for quarterly series)
c when check is specified.  Whether or not the histogram and
c summary statistics are printed out is controled by the prttab
c switch.  It is off in the default table set in gtinpt and is
c turned on by default here.
c-----------------------------------------------------------------------
      argok=T
      IF(.not.Lnoprt)THEN
       Prttab(LCKHST)=T
       Prttab(LCKNRM)=T
      END IF
      IF(Sp.eq.1)THEN
       Mxcklg=10
      ELSE
       Mxcklg=2*Sp
      END IF
      CALL setint(NOTSET,2*PARG,arglog)
c-----------------------------------------------------------------------
c     Argument get loop
c-----------------------------------------------------------------------
      DO WHILE (T)
       IF(gtarg(ARGDIC,argptr,PARG,argidx,arglog,argok))THEN
        IF(Lfatal)RETURN
        GO TO(10,20,30,40,50,60,70),argidx
c-----------------------------------------------------------------------
c     Number of acf and pacf lags to calculate and print out
c-----------------------------------------------------------------------
   10   CALL getivc(LPAREN,T,1,ivec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         IF(ivec(1).le.0)THEN
          CALL inpter(PERROR,Errpos,
     &                'Value of maxlag must be greater than 0.',T)
          Inptok=F
         ELSE
          Mxcklg=ivec(1)
         END IF
        END IF
        GO TO 80
c-----------------------------------------------------------------------
c     Print argument
c-----------------------------------------------------------------------
   20   CALL getprt(LSPCHK,NSPCHK,Inptok)
        GO TO 80
c-----------------------------------------------------------------------
c     Save argument
c-----------------------------------------------------------------------
   30   CALL getsav(LSPCHK,NSPCHK,Inptok)
        GO TO 80
c-----------------------------------------------------------------------
c     savelog  argument
c-----------------------------------------------------------------------
   40   CALL getsvl(LSLCHK,NSLCHK,Inptok)
        GO TO 80
c-----------------------------------------------------------------------
c     acflimit argument
c-----------------------------------------------------------------------
   50   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).le.ZERO)THEN
          CALL inpter(PERROR,Errpos,
     &                'Value of acflimit must be greater than 0.',T)
          Inptok=F
         ELSE
          Acflim=dvec(1)
         END IF
        END IF
        GO TO 80
c-----------------------------------------------------------------------
c     qtype argument
c-----------------------------------------------------------------------
   60   CALL gtdcvc(LPAREN,T,1,QDIC,qptr,QARG,
     &              'Improper entry for qtype: valid choices are ',
     &              ivec,nelt,F,argok,Inptok)
        IF(Lfatal)RETURN
        IF(nelt.le.0)THEN
         CALL writln('       ljungbox, lb, boxpierce or bp.',STDERR,Mt2,
     &               F,T)
        ELSE
         IF(ivec(1).gt.2)THEN
          Iqtype=1
         ELSE
          Iqtype=0
         END IF
        END IF
        GO TO 80
c-----------------------------------------------------------------------
c     qlimit argument
c-----------------------------------------------------------------------
   70   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)THEN
         IF(dvec(1).le.ZERO)THEN
          CALL inpter(PERROR,Errpos,
     &                'Value of qlimit must be greater than 0.',T)
          Inptok=F
         ELSE IF(dvec(1).gt.ONE)THEN
          CALL inpter(PERROR,Errpos,
     &                'Value of qlimit must be less than 1.',T)
         ELSE
          Qcheck=dvec(1)
         END IF
        END IF
        GO TO 80
       END IF
c     -----------------------------------------------------------------
       RETURN
   80  CONTINUE
      END DO
      END
