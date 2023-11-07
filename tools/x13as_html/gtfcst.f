C     Last change:  BCM  28 Jan 98    3:29 pm
c      SUBROUTINE gtfcst(Sp,Fctdrp,Nfcst,Nbcst,Ciprob,Inptok)
      SUBROUTINE gtfcst(Fctdrp,Nfcst,Nbcst,Ciprob,Lognrm,Inptok)
c     ------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'lex.i'
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'tbllog.i'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      LOGICAL argok,Lognrm,Inptok
c      INTEGER Fctdrp,nelt,Nbcst,Nfcst,Sp,ivec
      INTEGER Fctdrp,nelt,Nbcst,Nfcst,ivec
      DOUBLE PRECISION Ciprob,dvec
      DIMENSION ivec(1),dvec(1)
c-----------------------------------------------------------------------
      CHARACTER YSNDIC*5
      INTEGER ysnptr,PYSN
      PARAMETER(PYSN=2)
      DIMENSION ysnptr(0:PYSN)
      PARAMETER(YSNDIC='yesno')
c-----------------------------------------------------------------------
      LOGICAL gtarg
      EXTERNAL gtarg
c-----------------------------------------------------------------------
c     Argument dictionary was made with the following command
c ../../dictionary/strary < ../../dictionary/forecast.dic
c-----------------------------------------------------------------------
      CHARACTER ARGDIC*50
      INTEGER arglog,argidx,argptr,PARG
      PARAMETER(PARG=7)
      DIMENSION argptr(0:PARG),arglog(2,PARG)
      PARAMETER(ARGDIC='excludemaxleadprobabilityprintsavemaxbacklognorm
     &al')
c-----------------------------------------------------------------------
      DATA ysnptr/1,4,6/
      DATA argptr/1,8,15,26,31,35,42,51/
c-----------------------------------------------------------------------
c     Default number of forecasts by specifying the forecast spec.
c-----------------------------------------------------------------------
c      Nfcst=Sp
      CALL setint(NOTSET,2*PARG,arglog)
c-----------------------------------------------------------------------
c     Argument get loop
c-----------------------------------------------------------------------
      DO WHILE (T)
       IF(gtarg(ARGDIC,argptr,PARG,argidx,arglog,Inptok))THEN
        IF(Lfatal)RETURN
        GO TO(10,20,30,40,50,60,70),argidx
c-----------------------------------------------------------------------
c     Drop data in the forecast origin
c-----------------------------------------------------------------------
   10   CALL getivc(LPAREN,T,1,ivec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        Fctdrp=ivec(1)
        GO TO 80
c-----------------------------------------------------------------------
c     Nfcst of forecasts
c-----------------------------------------------------------------------
   20   CALL getivc(LPAREN,T,1,ivec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(nelt.gt.0)THEN
         IF(ivec(1).gt.PFCST)THEN
          CALL inpter(PERROR,Errpos,'Too many forecasts specified',T)
          Inptok=F
         ELSE
          Nfcst=ivec(1)
         END IF
        END IF
        GO TO 80
c-----------------------------------------------------------------------
c     Width of the confidense intervals in standard errors
c-----------------------------------------------------------------------
   30   CALL gtdpvc(LPAREN,T,1,dvec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        IF(nelt.gt.0)THEN
         IF(dvec(1).le.0D0.or.dvec(1).ge.1D0)THEN
          CALL inpter(PERROR,Errpos,
     &       'Coverage probability must be strictly between 0 and 1.',T)
          Inptok=F
         ELSE
          Ciprob=dvec(1)
         END IF
        END IF
        GO TO 80
c-----------------------------------------------------------------------
c     Print argument
c-----------------------------------------------------------------------
   40   CALL getprt(LSPFOR,NSPFOR,Inptok)
        GO TO 80
c-----------------------------------------------------------------------
c     Save argument
c-----------------------------------------------------------------------
   50   CALL getsav(LSPFOR,NSPFOR,Inptok)
        GO TO 80
c-----------------------------------------------------------------------
c     backcasts argument
c-----------------------------------------------------------------------
   60   CALL getivc(LPAREN,T,1,ivec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        Nbcst=ivec(1)
        IF(Nbcst.gt.PFCST)THEN
         CALL inpter(PERROR,Errpos,'Too many backcasts specified',T)
         Inptok=F
        END IF
        GO TO 80
c-----------------------------------------------------------------------
c     lognormal argument
c-----------------------------------------------------------------------
   70   CALL gtdcvc(LPAREN,T,1,YSNDIC,ysnptr,PYSN,
     &              'Available options for lognormal are yes or no.',
     &              ivec,nelt,T,argok,Inptok)
        IF(Lfatal)RETURN
        IF(argok.and.nelt.gt.0)Lognrm=ivec(1).eq.1
        GO TO 80
       END IF
c     -----------------------------------------------------------------
       RETURN
   80  CONTINUE
      END DO
c     -----------------------------------------------------------------
      END
