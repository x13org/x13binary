C     Last change:  BCM   4 Sep 1998    1:47 pm
      SUBROUTINE gtarma(Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Specify the regression and time series parts of the model
c-----------------------------------------------------------------------
      INCLUDE 'lex.i'
c      INCLUDE 'stdio.i'
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      LOGICAL argok,Inptok
      INTEGER itmpvc,nelt
      DIMENSION itmpvc(0:1)
c-----------------------------------------------------------------------
      LOGICAL gtarg
      EXTERNAL gtarg
c-----------------------------------------------------------------------
c     This dictionary was created with
c ../../dictionary/strary  < ../../dictionary/arima.dic
c-----------------------------------------------------------------------
      CHARACTER ARGDIC*18
      INTEGER arglog,argidx,argptr,PARG
      PARAMETER(PARG=5)
      DIMENSION argptr(0:PARG),arglog(2,PARG)
      PARAMETER(ARGDIC='titlemodeldiffarma')
c-----------------------------------------------------------------------
      DATA argptr/1,6,11,15,17,19/
c-----------------------------------------------------------------------
      CALL setint(NOTSET,2*PARG,arglog)
c-----------------------------------------------------------------------
c     Argument get loop
c-----------------------------------------------------------------------
      DO WHILE (T)
       IF(gtarg(ARGDIC,argptr,PARG,argidx,arglog,Inptok))THEN
        IF(Lfatal)RETURN
        GO TO(10,20,30,30,30),argidx
c-----------------------------------------------------------------------
c     Title argument
c-----------------------------------------------------------------------
   10   CALL getttl(LPAREN,T,1,Mdlttl,itmpvc,nelt,argok,Inptok)
        IF(.not.Lfatal.and.argok.and.nelt.gt.0)
     &     CALL eltlen(nelt,itmpvc,nelt,Nmdlcr)
        IF(Lfatal)RETURN
        GO TO 40
c-----------------------------------------------------------------------
c     Get the orders and lags of the ARIMA model
c-----------------------------------------------------------------------
   20   CALL getmdl(argok,Inptok,F)
        IF(Lfatal)RETURN
        GO TO 40
c-----------------------------------------------------------------------
c     ARIMA initial and/or fixed values.  Argidx-2, 2 is
c the displacement or number of arguments before diff in the
c argument dictionary.
c-----------------------------------------------------------------------
   30   CALL gtinvl(argidx-2,Inptok)
        IF(Lfatal)RETURN
        IF(argidx.eq.3)Lprtdf=T
        GO TO 40
       END IF
       IF(Lfatal)RETURN
c----------------------------------------------------------------------
c     Check if the Regression and arima models are fixed (sets imdlfx).
c----------------------------------------------------------------------
       CALL mdlfix()
c     ------------------------------------------------------------------
       RETURN
   40  CONTINUE
      END DO
c     ------------------------------------------------------------------
      END
