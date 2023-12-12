      SUBROUTINE prterx()
      IMPLICIT NONE
c     -------------------------------------------------------------------
c      If irregular regression matrix is singular, print out error 
c      message.
c     -------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c     -------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'xrgtbl.i'
      INCLUDE 'filext.prm'
c     -------------------------------------------------------------------
      CHARACTER str*(PMDLCR)
      INTEGER nchr,nfil
c     -------------------------------------------------------------------
      INTEGER nblank
      EXTERNAL nblank
c-----------------------------------------------------------------------
      INCLUDE 'filext.var'
c-----------------------------------------------------------------------
      IF(Sngcol.lt.Ncxy)THEN
       CALL getstr(Colttl,Colptr,Ncoltl,Sngcol,str,nchr)
       IF(Lfatal)RETURN
      ELSE
       nchr=4
       str(1:nchr)='data'
      END IF
      nfil=nblank(Cursrs)
      WRITE(STDERR,1230)Cursrs(1:nfil)
 1230 FORMAT(' Error(s) found while estimating the irregular ',
     &       'regression model.',/,
     &       ' For more details, check the error file (',a,
     &       '_err.html).')
      CALL eWritln('Irregular regression matrix singular '//
     &             'because of '//str(1:nchr)//'.',Mt1,Mt2,T,T)
      CALL writln(' Check irregular regression model.',Mt1,Mt2,T,T)
c-----------------------------------------------------------------------
c     Added printing of regression matrix 
c-----------------------------------------------------------------------
      IF(Prttab(LXRXMX))THEN
       CALL genSkip(LXRXMX)
       CALL prtshd('Irregular Component Regression Matrix',Begxy,Sp,
     &             Nrxy)
       IF(.not.Lfatal)
     &    CALL prtmtx(Begxy,Sp,Xy,Nrxy,Ncxy,Colttl,Colptr,Ncoltl,
     &                'Irregular Component Regression Matrix',
     &                tbxdic(LXRXMX))
       IF(Lfatal)RETURN
      END IF
      CALL abend()
c-----------------------------------------------------------------------
      RETURN
      END
