C     Last change:  SRD  31 Jan 100    8:17 am
      SUBROUTINE chkurt(Urpr,Urps,Urqr,Urqs)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Performs chek of the roots of phi(B)=0 and theta(B)=0;
c each root has four components: Real, Imaginary, Module, and Frequency
c The module of the roots are checked, and indicator variables are
c returned that show where a unit root has occurred.
c-----------------------------------------------------------------------
c Name   Type Description
c-----------------------------------------------------------------------
c degree  i  Maximum lag of phi(B) or theta(B)
c degp1   i  degree + 1
c coeff   d  Coefficients of phi(B) or theta(B) in order of increasing
c             powers
c rcoef   d  Coefficients of phi(B) or theta(B) in order of decreasing
c             powers
c zeror   d  Real part of the roots
c zeroi   d  Imaginary part of the roots
c-----------------------------------------------------------------------
      DOUBLE PRECISION ONE,ZERO,MONE
      PARAMETER(ONE=1D0,ZERO=0D0,MONE=-1D0)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      LOGICAL allinv
      INTEGER i,beglag,begopr,endlag,endopr,factor,iflt,ilag,iopr,
     &        degree,Urpr,Urps,Urqr,Urqs
      DOUBLE PRECISION coeff,zeror,zeroi,zerom,zerof,modlim
      DIMENSION coeff(PORDER+1),zeror(PORDER),zeroi(PORDER),
     &          zerom(PORDER),zerof(PORDER)
c-----------------------------------------------------------------------
c     Set up indicator varibles for the unit root test.
c-----------------------------------------------------------------------
      Urpr=0
      Urps=0
      Urqr=0
      Urqs=0
c-----------------------------------------------------------------------
c     check for unit roots in all
c-----------------------------------------------------------------------
      begopr=Mdl(AR-1)
      beglag=Opr(begopr-1)
      endopr=Mdl(MA)-1
c     ------------------------------------------------------------------
      IF(endopr.gt.0)THEN
       modlim=ONE/0.95D0
       endlag=Opr(endopr)-1
c     ------------------------------------------------------------------
       DO iflt=AR,MA
        begopr=Mdl(iflt-1)
        endopr=Mdl(iflt)-1
c     ------------------------------------------------------------------
        DO iopr=begopr,endopr
         beglag=Opr(iopr-1)
         endlag=Opr(iopr)-1
c     ------------------------------------------------------------------
         factor=Oprfac(iopr)
         degree=Arimal(endlag)/factor
         coeff(1)=MONE
         CALL setdp(ZERO,degree,coeff(2))
c     ------------------------------------------------------------------
         DO ilag=beglag,endlag
          coeff(Arimal(ilag)/factor+1)=Arimap(ilag)
         END DO
         CALL roots(coeff,degree,allinv,zeror,zeroi,zerom,zerof)
         IF(Lfatal)RETURN
c     ------------------------------------------------------------------
c     Test to see if there are unit roots in any of the AR or MA
c     polynomials
c     ------------------------------------------------------------------
         DO i=1,degree
c          IF (zerom(i).le.ublim.AND.zeroi(i).LE.5.0D-2.AND.
c     &        zeror(i).GT.0.D0) THEN
          IF(zerom(i).le.modlim)THEN
           IF(factor.eq.1)THEN
            IF(iflt.eq.AR)THEN
             Urpr=Urpr+1
            ELSE
             Urqr=Urqr+1
            END IF
           ELSE
            IF(iflt.eq.AR)THEN
             Urps=Urps+1
            ELSE
             Urqs=Urqs+1
            END IF
           END IF
          END IF
         END DO
        END DO
       END DO
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
