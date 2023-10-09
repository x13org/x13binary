C     Last change:  SRD  19 Nov 99    6:05 am
      SUBROUTINE prterr(Nefobs,Lauto)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     prterr.f, Release 1, Subroutine Version 1.7, Modified 14 Feb 1995.
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'mdltbl.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'filext.prm'
c-----------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      CHARACTER str*(PMDLCR)
      LOGICAL ltmper,Lauto
      INTEGER itmp,nchr,Nefobs,nfil,Begxy,Nrxy,Endspn,fh2
      DIMENSION Begxy(2),Endspn(2)
c     ------------------------------------------------------------------
      DOUBLE PRECISION dpmpar
      INTEGER nblank
      EXTERNAL nblank,dpmpar
c-----------------------------------------------------------------------
      COMMON /armaxy/ Endspn,Begxy,Nrxy
c-----------------------------------------------------------------------
      INCLUDE 'filext.var'
c-----------------------------------------------------------------------
      nfil=nblank(Cursrs)
c-----------------------------------------------------------------------
c     Unknown error
c-----------------------------------------------------------------------
      fh2=0
      IF(.not.Lauto)fh2=Mt1
c     ------------------------------------------------------------------
      IF(Armaer.eq.PUNKER.or.Armaer.lt.0)THEN
       Convrg=F
       Var=0D0
       IF(Issap.eq.2.or.Irev.eq.4)
     &   CALL eWritln('Nonlinear estimation error with unknown cause '//
     &                'during ',Mt1,Mt2,T,F)
       IF(Issap.eq.2)THEN
        WRITE(STDERR,1230)Cursrs(1:nfil)
        CALL writln('sliding spans analysis.',Mt1,Mt2,F,T)
       ELSE IF(Irev.eq.4)THEN
        WRITE(STDERR,1230)Cursrs(1:nfil)
        CALL writln('revisions analysis.',Mt1,Mt2,F,T)
       ELSE
        IF(.NOT.Lauto)WRITE(STDERR,1230)Cursrs(1:nfil)
        CALL eWritln('Nonlinear estimation error with unknown cause.',
     &               fh2,Mt2,T,T)
       END IF
c-----------------------------------------------------------------------
c     Xy is singular
c-----------------------------------------------------------------------
      ELSE IF(Armaer.eq.PSNGER.or.Armaer.eq.PISNER)THEN
       IF(Sngcol.lt.Ncxy)THEN
        CALL getstr(Colttl,Colptr,Ncoltl,Sngcol,str,nchr)
        IF(Lfatal)RETURN
c     -------------------------------------------------------------------
       ELSE
        nchr=4
        str(1:nchr)='data'
       END IF
c     -------------------------------------------------------------------
       IF(Armaer.eq.PISNER)THEN
        IF(.not.Lauto)WRITE(STDERR,1230)Cursrs(1:nfil)
        CALL eWritln('Regression matrix singular because of '//
     &               str(1:nchr)//'.',fh2,Mt2,T,F)
        CALL writln(' Remove variable(s) from regression spec and '//
     &         'try again.',fh2,Mt2,F,T)
c     ------------------------------------------------------------------
       ELSE
        IF(.not.Lauto)WRITE(STDERR,1230)Cursrs(1:nfil)
        CALL eWritln('Regression matrix singular because of '//
     &               str(1:nchr)//'.',fh2,Mt2,T,F)
        CALL writln(' Check regression model or change automatic '//
     &              'outlier options',fh2,Mt2,T,F)
        CALL writln(' i.e. method to addone or types to identify AO '//
     &              'only.',fh2,Mt2,F,T)
c-----------------------------------------------------------------------
c     Added printing of regression matrix (BCM 5-97)
c-----------------------------------------------------------------------
        IF(Lauto)THEN
         IF(Prttab(LREGDT))THEN
          CALL genSkip(LREGDT)
          CALL prtshd('Regression Matrix',Begxy,Sp,Nrxy)
          IF(.not.Lfatal)CALL prtmtx(Begxy,Sp,Xy,Nrxy,Ncxy,Colttl,
     &                               Colptr,Ncoltl,'Regression Matrix',
     &                               'regression.matrix.error')
          IF(Lfatal)RETURN
         END IF
        END IF
       END IF
c     -------------------------------------------------------------------
       IF(.not.Lauto)CALL abend()
       RETURN
c-----------------------------------------------------------------------
c     Improper input to the nonlinear routine
c-----------------------------------------------------------------------
      ELSE IF(Armaer.eq.PINPER)THEN
       WRITE(STDERR,1230)Cursrs(1:nfil)
       CALL wWritln('Improper input parameters to the likelihood '//
     &              'minimization routine.',Mt1,Mt2,T,F)
       CALL writln(' Please send us the data and spec file that '//
     &             'produced this',Mt1,Mt2,F,F)
       CALL writln(' message (x12@census.gov).',Mt1,Mt2,F,T)
c     ------------------------------------------------------------------
      ELSE IF(Armaer.eq.PMXIER)THEN
       CALL errhdr
       CALL itrerr('iterations          ',10,Lauto,Issap,Irev)
c     ------------------------------------------------------------------
      ELSE IF(Armaer.eq.PMXFER)THEN
       CALL errhdr
       CALL itrerr('function evaluations',20,Lauto,Issap,Irev)
c     ------------------------------------------------------------------
      ELSE IF(Armaer.eq.PSCTER.or.Armaer.eq.PSPMER.or.Armaer.eq.PCOSER)
     &        THEN
       IF(.not.Lauto)THEN
        WRITE(STDERR,1230)Cursrs(1:nfil)
       END IF
       CALL wWritln('Estimation was terminated because no '//
     &              'further improvement in',fh2,Mt2,T,F)
       CALL writln(' the likelihood was possible.',fh2,Mt2,F,T)
       CALL writln(' Check iteration output to confirm that model '//
     &             'estimation really converged.',fh2,Mt2,T,T)
       IF(.not.Lprier)THEN
        IF(.not.Lauto)CALL mkPOneLine(Mt1,'@','&nbsp;')
        CALL mkPOneLine(Mt2,'@','&nbsp;')
       ELSE IF(Armaer.eq.PSCTER)THEN
        IF(.not.Lauto)WRITE(STDERR,1230)Cursrs(1:nfil)
        CALL writln(' Convergence tolerance on the likelihood is '//
     &              'too strict.',fh2,Mt2,T,T)
c     ------------------------------------------------------------------
       ELSE IF(Armaer.eq.PSPMER)THEN
        IF(.not.Lauto)WRITE(STDERR,1230)Cursrs(1:nfil)
        CALL wWritln('Convergence tolerance for the relative '//
     &               'difference in the',fh2,Mt2,T,F)
        CALL writln(' parameter estimates is too strict.',fh2,Mt2,F,T)
c     ------------------------------------------------------------------
       ELSE IF(Armaer.eq.PCOSER)THEN
        IF(.not.Lauto)WRITE(STDERR,1230)Cursrs(1:nfil)
        CALL writln(' Cosine of the angle between the vector of '//
     &              'expected values and ',fh2,Mt2,T,F)
        CALL writln(' any column of the jacobian is too small.',
     &              fh2,Mt2,T,T)
       END IF
       IF(Issap.eq.2)THEN
        CALL mkPOneLine(Mt1,'@',' This warning occurred during the '//
     &                          'sliding spans analysis.')
       ELSE IF(Irev.eq.4)THEN
        CALL mkPOneLine(Mt1,'@',' This warning occurred during the '//
     &                          'history analysis.')
       END IF
c-----------------------------------------------------------------------
c     Invertibility errors.  Print the estimates, and stop.
c-----------------------------------------------------------------------
      ELSE IF(Armaer.eq.PNIFER)THEN
       CALL getstr(Oprttl,Oprptr,Noprtl,Prbfac,str,nchr)
       IF(Lfatal)RETURN
       IF(.not.Lauto)WRITE(STDERR,1230)Cursrs(1:nfil)
       CALL eWritln(
     &        str(1:nchr)//' has roots inside the unit circle but some',
     &              fh2,Mt2,T,F)
       CALL writln(
     &           ' parameters are fixed so cannot invert the operator.',
     &             fh2,Mt2,F,T)
       IF(Issap.eq.2)THEN
        CALL mkPOneLine(Mt1,'@',' This error occurred during the '//
     &                          'sliding spans analysis.')
       ELSE IF(Irev.eq.4)THEN
        CALL mkPOneLine(Mt1,'@',' This error occurred during the '//
     &                          'history analysis.')
       END IF
       ltmper=Lprier
       Lprier=T
       CALL chkrt2(F,itmp,Lhiddn)
       IF(Lfatal)RETURN
       Lprier=ltmper
       IF(.not.Lauto)CALL abend()
       RETURN
c     ------------------------------------------------------------------
      ELSE IF(Armaer.eq.PNIMER)THEN
       CALL getstr(Oprttl,Oprptr,Noprtl,Prbfac,str,nchr)
       IF(Lfatal)RETURN
       IF(.not.Lauto)WRITE(STDERR,1230)Cursrs(1:nfil)
       CALL eWritln(str(1:nchr)//' has roots inside the unit circle '//
     &              'but some are missing',fh2,Mt2,T,F)
       CALL writln(' so cannot invert the operator.  Try including '//
     &             'all lags.',fh2,Mt2,F,T)
       IF(Issap.eq.2)THEN
        CALL mkPOneLine(Mt1,'@',' This error occurred during the '//
     &                          'sliding spans analysis.')
       ELSE IF(Irev.eq.4)THEN
        CALL mkPOneLine(Mt1,'@',' This error occurred during the '//
     &                          'history analysis.')
       END IF
       ltmper=Lprier
       Lprier=T
       CALL chkrt2(F,itmp,Lhiddn)
       IF(Lfatal)RETURN
       Lprier=ltmper
       IF(.not.Lauto)CALL abend()
       RETURN
c-----------------------------------------------------------------------
c     Stpitr convergence errors
c-----------------------------------------------------------------------
      ELSE IF(Armaer.eq.PCNTER)THEN
       CALL errhdr
       IF(.not.Lauto)THEN
        WRITE(STDERR,1230)Cursrs(1:nfil)
        WRITE(Mt1,1180)2D0/Nefobs*dpmpar(1)
       END IF
       WRITE(Mt2,1180)2D0/Nefobs*dpmpar(1)
       IF(Issap.eq.2)THEN
        CALL mkPOneLine(Mt1,'@',' This error occurred during the '//
     &                          'sliding spans analysis.')
       ELSE IF(Irev.eq.4)THEN
        CALL mkPOneLine(Mt1,'@',' This error occurred during the '//
     &                          'history analysis.')
       END IF
       IF(.not.Lauto)CALL abend()
       RETURN
c     ------------------------------------------------------------------
      ELSE IF(Armaer.eq.PDVTER)THEN
       IF(.not.Lauto)WRITE(STDERR,1230)Cursrs(1:nfil)
       CALL wWritln('Deviance was less than machine precision so '//
     &              'could not',fh2,Mt2,T,F)
       CALL writln(' calculate the relative deviance.',fh2,Mt2,T,F)
       IF(Issap.eq.2)THEN
        CALL mkPOneLine(Mt1,'@',' This warning occurred during the '//
     &                          'sliding spans analysis.')
       ELSE IF(Irev.eq.4)THEN
        CALL mkPOneLine(Mt1,'@',' This warning occurred during the '//
     &                          'history analysis.')
       END IF
c-----------------------------------------------------------------------
c     Singular ARMA covariance matrix
c-----------------------------------------------------------------------
      ELSE IF(Armaer.eq.PACSER)THEN
       IF(.not.Lauto)WRITE(STDERR,1230)Cursrs(1:nfil)
       CALL wWritln('The covariance matrix of the <abbr '//
     &              'title="autoregressive moving average">ARMA'//
     &              '</abbr> parameters is singular,',fh2,Mt2,T,F)
       CALL writln(' so the standard errors and the correlation '//
     &             'matrix of the <abbr title="autoregressive '//
     &              'moving average">ARMA</abbr>',fh2,Mt2,F,F)
       CALL writln(' parameters will not be printed out.',fh2,Mt2,F,T)
       IF(Issap.eq.2)THEN
        CALL mkPOneLine(Mt1,'@',' This warning occurred during the '//
     &                          'sliding spans analysis.')
       ELSE IF(Irev.eq.4)THEN
        CALL mkPOneLine(Mt1,'@',' This warning occurred during the '//
     &                          'history analysis.')
       END IF
c     ------------------------------------------------------------------
c     Objective function equal to zero
c     ------------------------------------------------------------------
      ELSE IF(Armaer.eq.POBFN0)THEN
       IF(.not.Lauto)WRITE(STDERR,1230)Cursrs(1:nfil)
       CALL eWritln('Differencing has annihilated the series.',
     &              fh2,Mt2,T,F)
       CALL writln(' Check the model specified in the arima spec,'//
     &             ' set or change',fh2,Mt2,F,F)
       CALL writln(' the possible differencing orders (if using the '//
     &             'automdl spec), or',fh2,Mt2,F,F)
       CALL writln(' change the models specified in the automatic '//
     &             'model file',fh2,Mt2,F,F)
       CALL writln(' (if using the pickmdl spec).',fh2,Mt2,F,T)
       IF(Issap.eq.2)THEN
        CALL mkPOneLine(Mt1,'@',' This error occurred during the '//
     &                          'sliding spans analysis.')
       ELSE IF(Irev.eq.4)THEN
        CALL mkPOneLine(Mt1,'@',' This error occurred during the '//
     &                          'history analysis.')
       END IF
       IF(.not.Lauto)CALL abend()
      END IF
c     ------------------------------------------------------------------
      RETURN
 1180 FORMAT(/,' <p><strong>ERROR:</strong> Convergence tolerance ',
     &         'must be set larger than machine',
     &       /,'precision ',e25.14,'.</p>',/)
 1230 FORMAT(/,' Error(s) found while estimating the regARIMA model.',/,
     &       ' For more details, check the error file (',a,
     &       '_err.html).',/)
      END
