C     Last change:  BCM  19 Feb 1999   10:37 am
      SUBROUTINE chkrt1(Irunit,Isunit,Rmaxr,Rmaxs,Linv,Ublim)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Performs chek of the roots of phi(B)=0 and theta(B)=0;
c each root has four components: Real, Imaginary, Module, and Frequency
c The module of the AR roots are checked, and the .
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
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      LOGICAL allinv,Linv
      INTEGER i,beglag,begopr,endlag,endopr,factor,iflt,ilag,iopr,
     &        Irunit,Isunit,degree
      DOUBLE PRECISION coeff,zeror,zeroi,zerom,zerof,Rmaxr,Rmaxs,Ublim,
     &                 zmi
      DIMENSION coeff(PORDER+1),zeror(PORDER),zeroi(PORDER),
     &          zerom(PORDER),zerof(PORDER)
c-----------------------------------------------------------------------
c     Set up indicator varibles for the unit root test.
c-----------------------------------------------------------------------
      Irunit=0
      Isunit=0
      Rmaxr=DNOTST
      Rmaxs=DNOTST
c-----------------------------------------------------------------------
c     Print out the roots of phi(B)=0 and theta(B)=0 with AR part first
c-----------------------------------------------------------------------
      begopr=Mdl(AR-1)
      beglag=Opr(begopr-1)
      endopr=Mdl(MA)-1
c     ------------------------------------------------------------------
      IF(endopr.gt.0)THEN
       endlag=Opr(endopr)-1
c     ------------------------------------------------------------------
       iflt=AR
       begopr=Mdl(iflt-1)
       endopr=Mdl(iflt)-1
       IF(begopr.gt.endopr)RETURN
c     ------------------------------------------------------------------
       DO iopr=begopr,endopr
        beglag=Opr(iopr-1)
        endlag=Opr(iopr)-1
c     ------------------------------------------------------------------
        factor=Oprfac(iopr)
        degree=Arimal(endlag)/factor
        coeff(1)=-1.0D0
        CALL setdp(0D0,degree,coeff(2))
c     ------------------------------------------------------------------
        DO ilag=beglag,endlag
         coeff(Arimal(ilag)/factor+1)=Arimap(ilag)
        END DO
        CALL roots(coeff,degree,allinv,zeror,zeroi,zerom,zerof)
        IF(Lfatal)RETURN
        Linv=Linv.and.allinv
c     ------------------------------------------------------------------
        DO i=1,degree
         IF (zerom(i).le.Ublim.AND.zeroi(i).LE.5.0D-2.AND.
     &       zeror(i).GT.0.D0) THEN
          IF (factor.eq.1) THEN
           Irunit=Irunit+1
          ELSE
           Isunit=Isunit+1
          END IF
         ELSE IF (zeroi(i).LE.2.0D-2.AND.zeror(i).GT.0.D0) THEN
          zmi=1/zerom(i)
          IF (factor.eq.1) THEN
           IF (zmi.GT.Rmaxr) Rmaxr=zmi
          ELSE
           IF (zmi.GT.Rmaxs) Rmaxs=zmi
          END IF
         END IF
        END DO
       END DO
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
