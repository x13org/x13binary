C     Last change: Mar. 2021, change a content of a message
C     Last change:  BCM  25 Nov 97    9:25 am
      SUBROUTINE getopr(Optype,Coef,Lag,Fix,Ncoef,Nd,Naimcf,Locok,
     &                  Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Check the input for MA and AR parameters and add the coefficients
c and lags to the model.
c-----------------------------------------------------------------------
c Changed:
c  Determine if the lags are not specified from smallest to
c  largest, print a warning message if this is so and sort the lags
c  into the correct order by BCM on 05 Feb 2004.
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
c Nd      i  Ouput number of differences.
c optitl  c  Output 20 character scalar for the title of the current
c             operator.
c optypn  c  Local 20 character 3 long vector of names of the types of
c             operators.
c-----------------------------------------------------------------------
      INCLUDE 'lex.i'
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      CHARACTER str*(LINLEN)
      LOGICAL Fix,getint,hvcmma,Inptok,Locok,mislag,opngrp,resort
      INTEGER i,ipos,Lag,mxord,Naimcf,nchr,Ncoef,Nd,Optype,tmp,ivec,
     &        lastlg
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
      resort=F
      Nd=0
      IF(Optype.eq.DIFF)THEN
       mxord=PDIFOR
      ELSE
       mxord=PORDER
      END IF
c     -----------------------------------------------------------------
      IF(Nxtktp.eq.EOF)THEN
       Locok=F
c-----------------------------------------------------------------------
c     Look for an integer, which will indicate the order of an operator
c with no missing lags.
c-----------------------------------------------------------------------
      ELSE IF(getint(tmp))THEN
       Ncoef=tmp
       mislag=F
c-----------------------------------------------------------------------
c     Otherwise the lags with possibly missing lags with specified
c explicitly within braces, e.g., [1,4].  Anything else is an error.
c-----------------------------------------------------------------------
      ELSE IF(Nxtktp.ne.LBRAKT)THEN
       CALL inpter(PERROR,Lstpos,'Expected an INTEGER or "[" not "'//
     &                Nxttok(1:Nxtkln)//'"',T)
       Locok=F
c     -----------------------------------------------------------------
      ELSE
       Ncoef=0
       mislag=T
       opngrp=T
       hvcmma=F
       CALL lex()
c-----------------------------------------------------------------------
c     Get each of the lags, which maybe separated by commas.  No NULL
c lags are allowed, so `[,', `,,', and `,]' are not allowed.
c-----------------------------------------------------------------------
       DO WHILE (T)
        IF(Nxtktp.ne.RBRAKT)THEN
c-----------------------------------------------------------------------
c     Checking for `[,' or `,,' error.
c-----------------------------------------------------------------------
         IF(Nxtktp.eq.COMMA)THEN
          IF(hvcmma.or.opngrp)THEN
           CALL inpter(PERROR,Lstpos,
     &                 'Found a NULL lag; check your commas.',T)
           Locok=F
          END IF
c     -----------------------------------------------------------------
          CALL lex()
          hvcmma=T
          opngrp=F
          GO TO 10
c-----------------------------------------------------------------------
c     Check for something other than LAGs or COMMAs.
c-----------------------------------------------------------------------
         ELSE IF(.not.(getint(tmp)))THEN
          CALL inpter(PERROR,Lstpos,'Expected an integer not "'
     &                         //Nxttok(1:Nxtkln)//'"',T)
          Locok=F
          GO TO 10
c-----------------------------------------------------------------------
c     Check that the number of lags does not exceed the order of the
c operator.  Missing lag models could still cause problems, but those
c are checked in GETMDL when we have the periodicity of the factor.
c-----------------------------------------------------------------------
         ELSE IF(Ncoef.ge.mxord)THEN
          CALL getstr(OPRDIC,oprptr,POPDIC,Optype,str,nchr)
          IF(Lfatal)RETURN
          ipos=nchr+17
          str((nchr+1):(ipos-1))=' vector exceeds '
          CALL itoc(mxord,str,ipos)
          IF(Lfatal)RETURN
          str(ipos:ipos+7)=' lag[s].'
          ipos=ipos+8
          CALL inpter(PERROR,Lstpos,str(1:ipos-1),T)
          Locok=F
         ELSE IF(tmp.lt.0)THEN
          CALL inpter(PERROR,Lstpos,
     &           'Lags specified in model must be positive integers.',T)
          Locok=F
c-----------------------------------------------------------------------
c     No errors, add the lag.
c-----------------------------------------------------------------------
         ELSE
          Ncoef=Ncoef+1
c     -----------------------------------------------------------------
          IF(Naimcf+Ncoef-1.gt.PARIMA)THEN
           CALL inpter(PERROR,Lstpos,
     &                 'No room to add more ARIMA coefficients. '//
     &                 'Reduce the model order.',T)
           Locok=F
          END IF
c     -----------------------------------------------------------------
          IF(Ncoef.eq.1)THEN
           lastlg=tmp
          ELSE IF(.not.resort)THEN
           IF(tmp.lt.lastlg)THEN
            resort=T
            CALL inpter(PWARN,Lstpos,
     &                  'Lags must be specified from smallest to '//
     &                  'largest; lags will be sorted.',T)
           ELSE
            lastlg=tmp
           END IF
          END IF
          Lag(Ncoef)=tmp
          hvcmma=F
          opngrp=F
          GO TO 10
c     -----------------------------------------------------------------
         END IF
c----------------------------------------------------------------------
c     Check for a comma after the last lag and before the close of
c the list.  This indicates a NULL lag, for example, [1,3,].
c----------------------------------------------------------------------
        ELSE IF(hvcmma)THEN
         CALL inpter(PERROR,Lstpos,
     &               'Found a NULL lag; check your commas.',T)
         Locok=F
        END IF
c     -----------------------------------------------------------------
        IF(Locok)THEN
         CALL lex()
        ELSE
         CALL skplst(RBRAKT)
        END IF
        GO TO 20
   10   CONTINUE
       END DO
      END IF
c-----------------------------------------------------------------------
c     If only the number of differences or lags are specified then
c fill then in.
c-----------------------------------------------------------------------
   20 IF(.not.mislag)THEN
       IF(Ncoef.gt.mxord)THEN
        ipos=19
        str(1:(ipos-1))='Maximum number of '
        CALL getstr(OPRDIC,oprptr,POPDIC,Optype,str(ipos:),nchr)
        IF(Lfatal)RETURN
        ipos=ipos+nchr
        str(ipos:(ipos+6))=' lags, '
        ipos=ipos+7
c     ------------------------------------------------------------------
        CALL itoc(mxord,str,ipos)
        IF(Lfatal)RETURN
        str(ipos:(ipos+10))=', exceeded.'
        ipos=ipos+11
        CALL inpter(PERROR,Errpos,str(1:ipos-1),T)
        Locok=F
c     ------------------------------------------------------------------
       ELSE IF(Naimcf+Ncoef-1.gt.PARIMA)THEN
        ipos=39
        str(1:(ipos-1))='Maximum number of ARIMA coefficients, '
        CALL itoc(PARIMA,str,ipos)
        IF(Lfatal)RETURN
        str(ipos:(ipos+35))=', exceeded.  Reduce the model order.'
        ipos=ipos+36
        CALL inpter(PERROR,Errpos,str(1:ipos-1),T)
        Locok=F
c-----------------------------------------------------------------------
c     Set up the (1-B)^nd difference operator
c-----------------------------------------------------------------------
       ELSE IF(Optype.eq.DIFF)THEN
        Nd=Ncoef
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
      END IF
c-----------------------------------------------------------------------
c     Set the coefficient and fix vector if it hasn't been done.
c-----------------------------------------------------------------------
      IF(mislag.or.Optype.ne.DIFF)CALL setdp(DNOTST,Ncoef,Coef)
      IF(mislag.and.resort)CALL intsrt(Ncoef,Lag)
      CALL setlg(Optype.eq.DIFF,Ncoef,Fix)
c     ------------------------------------------------------------------
      Naimcf=Naimcf+Ncoef
      Inptok=Inptok.and.Locok
      RETURN
      END
