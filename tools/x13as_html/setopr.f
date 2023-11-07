C     Last change:  BCM  18 Dec 1998   10:13 am
      SUBROUTINE setopr(Optype,Coef,Lag,Fix,Ncoef,Nd,Naimcf,Locok,
     &                  Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Setup the variables for the MA and AR parameters and add the
c coefficients and lags to the model.
c-----------------------------------------------------------------------
c Name  Type Description
c-----------------------------------------------------------------------
c coef    d  Input pcoef ncoef used long vector of nonzero coefficients
c             to be added to arimap
c fixvec  l  Input array to determine what parameters are fixed and
c             not estimated.
c i       i  Local do loop index and temporary scalar
c lag     i  Input pcoef ncoef used long vector of the lags of the nonzero
c             coefficients to be added to arimal.
c ncoef   i  Input number of non zero coefficients in coef and lag
c optitl  c  Output 20 character scalar for the title of the current
c             operator.
c optypn  c  Local 20 character 3 long vector of names of the types of
c             operators.
c-----------------------------------------------------------------------
c      INCLUDE 'lex.i'
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'error.cmn'
      INCLUDE 'stdio.i'
      INCLUDE 'units.cmn'
c     ------------------------------------------------------------------
      INTEGER LINLEN
      LOGICAL T,F
      PARAMETER(LINLEN=133,T=.true.,F=.false.)
c     ------------------------------------------------------------------
      CHARACTER str*(LINLEN)
      LOGICAL Fix,getint,Inptok,Locok
      INTEGER i,ipos,Lag,mxord,Naimcf,nchr,Ncoef,Optype,ivec,Nd
      DOUBLE PRECISION Coef,dpvec
      DIMENSION Coef(*),Fix(*),Lag(*),dpvec(1),ivec(1)
      EXTERNAL getint
c     ------------------------------------------------------------------
      CHARACTER OPRDIC*8
      INTEGER oprptr,POPDIC
      PARAMETER(POPDIC=3)
      DIMENSION oprptr(0:POPDIC)
      PARAMETER(OPRDIC='DIFFARMA')
      DATA oprptr/1,5,7,9/
c-----------------------------------------------------------------------
c     Get the number of differences, number of lags, or the lags
c themselves for one of the AR, differencing, or MA operators within
c an ARIMA, (AR DIFF MA)Period, factor.  Called from GETMDL.
c-----------------------------------------------------------------------
      Locok=T
      IF(Optype.eq.DIFF)THEN
       mxord=PDIFOR
      ELSE
       mxord=PORDER
      END IF
c-----------------------------------------------------------------------
c     If only the number of differences or lags are specified then
c fill then in.
c-----------------------------------------------------------------------
      IF(Ncoef.gt.mxord)THEN
       str='Maximum number of '
       ipos=19
       CALL getstr(OPRDIC,oprptr,POPDIC,Optype,str(ipos:),nchr)
       IF(Lfatal)RETURN
       ipos=ipos+nchr
       str(ipos:)=' lags, '
       ipos=ipos+7
c     ------------------------------------------------------------------
       CALL itoc(mxord,str,ipos)
       IF(Lfatal)RETURN
       str(ipos:)=', exceeded.'
       ipos=ipos+11
       CALL eWritln(str(1:ipos-1),STDERR,Mt2,T,T)
       Locok=F
c     ------------------------------------------------------------------
      ELSE IF(Naimcf+Ncoef-1.gt.PARIMA)THEN
       str='Maximum number of ARIMA coefficients, '
       ipos=39
       CALL itoc(PARIMA,str,ipos)
       IF(Lfatal)RETURN
       str(ipos:)=', exceeded.  Reduce the model order.'
       ipos=ipos+36
       CALL eWritln(str(1:ipos-1),STDERR,Mt2,T,T)
       Locok=F
c-----------------------------------------------------------------------
c     Set up the (1-B)^nd difference operator
c-----------------------------------------------------------------------
      ELSE IF(Optype.eq.DIFF)THEN
c       Nd=Ncoef
       IF(Nd.gt.0)THEN
        Ncoef=0
        ivec(1)=1
        dpvec(1)=1D0
        DO i=1,Nd
         CALL polyml(dpvec,ivec,1,Coef,Lag,Ncoef,PDIFOR,Coef,Lag,Ncoef)
        END DO
       END IF
c-----------------------------------------------------------------------
c     Fill in the lags for the AR or MA operator and put in default
c starting values of 0.1.  Note, AR and MA operators are not fixed
c by default.
c-----------------------------------------------------------------------
      ELSE IF(Ncoef.gt.0)THEN
       DO i=1,Ncoef
        Lag(i)=i
       END DO
      END IF
c-----------------------------------------------------------------------
c     Set the coefficient and fix vector if it hasn't been done.
c-----------------------------------------------------------------------
      IF(Optype.ne.DIFF)CALL setdp(DNOTST,Ncoef,Coef)
      CALL setlg(Optype.eq.DIFF,Ncoef,Fix)
c     ------------------------------------------------------------------
      Naimcf=Naimcf+Ncoef
      Inptok=Inptok.and.Locok
      RETURN
      END

