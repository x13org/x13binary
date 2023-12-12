C     Last change:  BCM   4 Aug 1998    8:28 am
      SUBROUTINE getid(Dflist,Niddf,Nidsdf,Mxidlg,Inptok)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INCLUDE 'lex.i'
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'tbllog.i'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      LOGICAL argok,gtarg,Inptok
      INTEGER Dflist,itmp,Mxidlg,mxnsdf,nelt,Niddf,Nidsdf,ivec
      DIMENSION Dflist(PDFLG,2),ivec(1)
      EXTERNAL gtarg
c-----------------------------------------------------------------------
c     Argument dictionary was made with the following command
c ../../dictionary/strary < ../../dictionary/identify.dic
c-----------------------------------------------------------------------
      CHARACTER ARGDIC*24
      INTEGER arglog,argidx,argptr,PARG
      PARAMETER(PARG=5)
      DIMENSION argptr(0:PARG),arglog(2,PARG)
      PARAMETER(ARGDIC='diffsdiffmaxlagprintsave')
      DATA argptr/1,5,10,16,21,25/
c-----------------------------------------------------------------------
c     Defaults for calling identify are diff=0, sdiff=0 which will
c give the acf's and pacf's of undifferenced series.
c-----------------------------------------------------------------------
      Niddf=1
      Nidsdf=1
      Dflist(1,1)=0
      Dflist(1,2)=0
      CALL setint(NOTSET,2*PARG,arglog)
      argok=T
c-----------------------------------------------------------------------
c     Argument get loop
c-----------------------------------------------------------------------
      DO WHILE (T)
       IF(gtarg(ARGDIC,argptr,PARG,argidx,arglog,argok))THEN
        IF(Lfatal)RETURN
        GO TO(10,20,30,40,50),argidx
c-----------------------------------------------------------------------
c     List of nonseasonal differences
c-----------------------------------------------------------------------
   10   CALL getivc(LPAREN,T,PDFLG,Dflist,Niddf,argok,Inptok)
        IF(Lfatal)RETURN
        GO TO 60
c-----------------------------------------------------------------------
c     List of seasonal differences
c-----------------------------------------------------------------------
   20   CALL getivc(LPAREN,T,PDFLG,Dflist(1,2),Nidsdf,argok,Inptok)
        IF(Lfatal)RETURN
        CALL maxidx(Dflist(1,2),Nidsdf,itmp,mxnsdf)
        IF(mxnsdf.gt.0)THEN
         IF(Sp.le.1)THEN
          CALL inpter(PERROR,Errpos,
     &                'Must specify a seasonal period, PERIOD>1, to '//
     &                'use SDIFF.',T)
          Inptok=F
         ELSE IF(Lseff)THEN
          CALL inpter(PERROR,Errpos,
     &                'Need to remove fixed seasonal effects in order'//
     &                ' to identify seasonal orders of differencing',T)
          Inptok=F
         ELSE
          Lidsdf=T
         END IF
        END IF
        GO TO 60
c-----------------------------------------------------------------------
c     Number of acf and pacf lags to calculate and print out
c-----------------------------------------------------------------------
   30   CALL getivc(LPAREN,T,1,ivec,nelt,argok,Inptok)
        IF(Lfatal)RETURN
        Mxidlg=ivec(1)
        GO TO 60
c-----------------------------------------------------------------------
c     Print argument
c-----------------------------------------------------------------------
   40   CALL getprt(LSPIDN,NSPIDN,Inptok)
        GO TO 60
c-----------------------------------------------------------------------
c     Save argument
c-----------------------------------------------------------------------
   50   CALL getsav(LSPIDN,NSPIDN,Inptok)
        GO TO 60
       END IF
c     -----------------------------------------------------------------
c     change the default of annual series to 10
c     BCM August 2010
c     -----------------------------------------------------------------
       IF(Mxidlg.eq.NOTSET)THEN
        IF(Sp.eq.1)THEN
         Mxidlg=10
        ELSE
         Mxidlg=3*Sp
        END IF
       END IF
c     -----------------------------------------------------------------
       RETURN
   60  CONTINUE
      END DO
c     -----------------------------------------------------------------
      END
