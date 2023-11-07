C     Last change:  BCM  29 Jan 1999    9:56 am
      SUBROUTINE xrlkhd(Aicc,Nxcld) 
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Generates Likelihood statistics for use with x11regression
c-----------------------------------------------------------------------
c     Data typing and initialization
c-----------------------------------------------------------------------
      DOUBLE PRECISION ONE,TWO,ZERO
      PARAMETER(ONE=1D0,TWO=2.0D0,ZERO=0D0)
c     ------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
c     ------------------------------------------------------------------
      INTEGER ilag,nefobs,Nxcld
      DOUBLE PRECISION Aicc,dnefob,dnp,dnp1
c-----------------------------------------------------------------------
      INTEGER strinx
      LOGICAL dpeq
      EXTERNAL dpeq,strinx
c-----------------------------------------------------------------------
      nefobs=Nspobs-Nxcld
      dnefob=dble(nefobs)
c-----------------------------------------------------------------------
c     Calculate the AIC.  First find the number of estimated parameters,
c including the regression and ARIMA parameters, and the variance.
c-----------------------------------------------------------------------
      dnp=dble(Ncxy)
      IF(Nb.gt.0)THEN
       DO ilag=1,Nb
        IF(Regfx(ilag))dnp=dnp-ONE
       END DO
      END IF
      dnp1=dnp+ONE
c-----------------------------------------------------------------------
c     Calculate the jacobian of the transformation and print out the
c AIC's if valid to do so.
c-----------------------------------------------------------------------
      Aicc=DNOTST
      IF(Var.gt.ZERO.and.Convrg.and.(dnefob.gt.dnp1))
     &   Aicc=-TWO*(Lnlkhd-dnefob*dnp/(dnefob-dnp1))
c     ------------------------------------------------------------------
c      IF(Aicc.lt.ZERO)THEN
c       WRITE(Mt1,1010)
c       WRITE(Mt2,1010)
c       CALL abend()
c      END IF
c     ------------------------------------------------------------------
c 1010 FORMAT('ERROR : Cannot generate AICC for irregular regression ',
c     &       'model.')
c     ------------------------------------------------------------------
      RETURN
      END
