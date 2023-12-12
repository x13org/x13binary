C     Last change:  BCM  25 Nov 97    9:05 am
      SUBROUTINE insopr(Optype,Coef,Lag,Fix,Ncoef,Facsp,Ioprtl,Locok,
     &                  Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Check the input for MA and AR parameters and add the coefficients
c and lags to the model.
c-----------------------------------------------------------------------
c Name  Type Description
c-----------------------------------------------------------------------
c coef    d  Input pcoef ncoef used long vector of nonzero coefficients
c             to be added to arimap
c fix  l  Input array to determine what parameters are fixed and
c             not estimated.
c lag     i  Input pcoef ncoef used long vector of the lags of the nonzero
c             coefficients to be added to arimal.
c ncoef   i  Input number of non zero coefficients in coef and lag
c oprttl  i  Local number of characters in the optitl
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      CHARACTER Ioprtl*(*)
      LOGICAL Fix,Locok,Inptok
      INTEGER Facsp,i,iopr,Lag,Ncoef,Optype
      DOUBLE PRECISION Coef
      DIMENSION Coef(Ncoef),Fix(Ncoef),Lag(Ncoef)
c-----------------------------------------------------------------------
c     Insert the title, coefficients, lags, and fix vectors in the
c ARIMA model and update the model and operator pointers.
c Later we may want to rewrite ins* to do error checking at a higher
c level to give more understandable error messages.  Here the error
c is reported in GETOPR.
c-----------------------------------------------------------------------
      Locok=T
      i=3
      CALL insptr(F,1,Optype,3,POPR,Mdl,i)
      IF(Lfatal)RETURN
c     ------------------------------------------------------------------
      IF(Opr(Nopr)+Ncoef-1.gt.PARIMA)Locok=F
c     ------------------------------------------------------------------
      IF(Locok)THEN
       iopr=Mdl(Optype)-1
       CALL insptr(T,Ncoef,iopr,POPR,PARIMA,Opr,Nopr)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Add the factor of the operator.  Assume if it can be added to
c Opr it can be added to Oprfac.
c-----------------------------------------------------------------------
       DO i=Nopr,iopr+1,-1
        Oprfac(i)=Oprfac(i-1)
       END DO
       Oprfac(iopr)=Facsp
c     ------------------------------------------------------------------
       CALL insdbl(Coef,iopr,Opr,Nopr,Arimap)
       IF(.not.Lfatal)CALL insint(Lag,iopr,Opr,Nopr,Arimal)
       IF(.not.Lfatal)CALL inslg(Fix,iopr,Opr,Nopr,Arimaf)
       IF(.not.Lfatal)CALL insstr(Ioprtl,iopr,POPR,Oprttl,Oprptr,Noprtl)
       IF(Lfatal)RETURN
      END IF
c     ------------------------------------------------------------------
      Inptok=Inptok.and.Locok
      RETURN
      END
