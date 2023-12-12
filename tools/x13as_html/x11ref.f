C     Last change:  BCM  27 May 1998   12:45 pm
      SUBROUTINE x11ref(Fcal,Ftd,Fhol,Xdev,Muladd,Psuadd,Tdgrp,Stdgrp,
     &                  Holgrp,Axruhl,Ndifum,Rtype,Nrxy,Ncxy,B,Xy,Nb,
     &                  Easidx,Kswv,Calfrc,Xhlnln)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Computes X-11 Regression factors
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'xrgum.cmn'
      INCLUDE 'xtdtyp.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION ONE,ZERO
      LOGICAL F,T
      PARAMETER(ONE=1D0,ZERO=0D0,F=.false.,T=.true.)
c-----------------------------------------------------------------------
      LOGICAL Psuadd,Axruhl,Trumlt,Calfrc,Xhlnln
      DOUBLE PRECISION Fcal,Ftd,Fhol,B,Xy
      INTEGER icol,irow,Tdgrp,Xdev,Muladd,ir2,Rtype,Holgrp,Ndifum,Nrxy,
     &        Ncxy,Nb,Easidx,Kswv,Stdgrp
      DIMENSION Fcal(*),Ftd(*),Fhol(*),Rtype(*),B(*),Xy(*)
c-----------------------------------------------------------------------
c     initialize regression factors to zero
c-----------------------------------------------------------------------
      CALL setdp(ZERO,PLEN,Fcal)
      CALL setdp(ZERO,PLEN,Ftd)
      CALL setdp(ZERO,PLEN,Fhol)
c-----------------------------------------------------------------------
      trumlt=(.not.Psuadd).and.Muladd.eq.0
c-----------------------------------------------------------------------
c     get raw factors for trading day, holiday effects
c-----------------------------------------------------------------------
      DO icol=1,Nb
       IF(Rtype(icol).eq.PRGTTD.or.Rtype(icol).eq.PRGTST.or.
     &    Rtype(icol).eq.PRRTTD.or.Rtype(icol).eq.PRRTST.or.
     &    Rtype(icol).eq.PRATTD.or.Rtype(icol).eq.PRATST.or.
     &    Rtype(icol).eq.PRGTLY.or.Rtype(icol).eq.PRRTLY.or.
     &    Rtype(icol).eq.PRATLY.or.Rtype(icol).eq.PRG1TD.or.
     &    Rtype(icol).eq.PRR1TD.or.Rtype(icol).eq.PRA1TD.or.
     &    Rtype(icol).eq.PRG1ST.or.Rtype(icol).eq.PRR1ST.or.
     &    Rtype(icol).eq.PRA1ST.or.
     &   (Rtype(icol).eq.PRGUTD.or.Rtype(icol).eq.PRGULM.or.
     &    Rtype(icol).eq.PRGULQ.or.Rtype(icol).eq.PRGULY))THEN
        CALL daxpy(Nrxy,B(icol),Xy(icol),Ncxy,Ftd,1)
       ELSE IF(Rtype(icol).eq.PRGTEA.or.Rtype(icol).eq.PRGTLD.or.
     &         Rtype(icol).eq.PRGTTH.or.Rtype(icol).eq.PRGTEC.or.
     &         Rtype(icol).eq.PRGTUH)THEN
        CALL daxpy(Nrxy,B(icol),Xy(icol),Ncxy,Fhol,1)
       END IF
      END DO
c-----------------------------------------------------------------------
c     Complete creating factors with user defined means, or derived from 
c     an additive seasonal adjustment.
c-----------------------------------------------------------------------
      IF(Haveum.or.Muladd.eq.1)THEN
       DO irow=1,Nrxy
        Fcal(irow)=Ftd(irow)+Fhol(irow)
        IF(Haveum)THEN
         ir2=irow+Ndifum
         Fcal(irow)=Fcal(irow)+Umean(ir2)
         IF(Muladd.eq.2)Fcal(irow)=dexp(Fcal(irow))
         IF(.not.Noxfac)THEN
          IF(Tdgrp.gt.0)THEN
           Ftd(irow)=Fcal(irow)
          ELSE
           Fhol(irow)=Fcal(irow)
          END IF
         END IF
        END IF
       END DO
       RETURN
      END IF
c-----------------------------------------------------------------------
c     Adjust raw factors as necessary
c-----------------------------------------------------------------------
      IF(Tdgrp.gt.0)THEN
       IF(Psuadd)THEN
        CALL mulref(Nrxy,Fcal,Ftd,Xdev,Xnstar,Daybar,F)
        CALL mulref(Nrxy,Ftd,Ftd,Xdev,Xnstar,Daybar,T)
       ELSE
        CALL mulref(Nrxy,Fcal,Ftd,Xdev,Xnstar,DNOTST,F)
        CALL mulref(Nrxy,Ftd,Ftd,Xdev,Xnstar,DNOTST,T)
       END IF
      END IF
      IF(Holgrp.gt.0)THEN
       IF((Muladd.eq.2.or.Trumlt).and.Tdgrp.gt.0)THEN
        CALL mulref(Nrxy,Fcal,Fhol,Xdev,Xnstar,DNOTST,F)
c        CALL mulref(Nrxy,Fhol,Fhol,Xdev,Xn,DNOTST,T)
        CALL mulref(Nrxy,Fhol,Fhol,Xdev,Xnstar,DNOTST,T)
       ELSE 
        CALL mulref(Nrxy,Fcal,Fhol,Xdev,Xnstar,ONE,F)
       END IF
      END IF
c-----------------------------------------------------------------------
c     Finish creating the factors 
c-----------------------------------------------------------------------
      DO irow=1,Nrxy
       ir2=irow+Xdev-1
       Fhol(irow)=Fhol(irow)+ONE
c-----------------------------------------------------------------------
c     Pseudo additive calendar factors
c-----------------------------------------------------------------------
       IF(Psuadd)THEN
        Fcal(irow)=Fcal(irow)+ONE
        IF(Tdgrp.gt.0)THEN
         Ftd(irow)=Ftd(irow)+ONE
        ELSE
         IF(Stdgrp.gt.0)Ftd(irow)=Ftd(irow)+ONE
        END IF
c-----------------------------------------------------------------------
c     Multiplicative calendar factors
c-----------------------------------------------------------------------
       ELSE IF(Muladd.eq.0)THEN
        IF(Tdgrp.gt.0)THEN
         IF(Kswv.eq.3)THEN
          Ftd(irow)=Ftd(irow)+ONE
          Fcal(irow)=Fcal(irow)+ONE
         ELSE
          Ftd(irow)=Ftd(irow)+Xn(ir2)/Xnstar(ir2)
          Fcal(irow)=Fcal(irow)+Xn(ir2)/Xnstar(ir2)
         END IF
         IF(Holgrp.gt.0)THEN
          IF((.not.Axruhl).and.Easidx.eq.0.and.Xhlnln)THEN
           Fhol(irow)=Fhol(irow)/Kvec(irow)
           IF(.not.Calfrc)Fcal(irow)=Fcal(irow)/Kvec(irow)
          ELSE IF(Calfrc)THEN
           Fcal(irow)=Ftd(irow)*Fhol(irow)
          END IF
         END IF
        ELSE
         Fcal(irow)=Fcal(irow)+ONE
         IF(Stdgrp.gt.0)Ftd(irow)=Ftd(irow)+ONE
        END IF
c-----------------------------------------------------------------------
c     log-additive calendar factors
c-----------------------------------------------------------------------
       ELSE IF(Muladd.eq.2)THEN
        IF(Tdgrp.gt.0)THEN
         IF(Calfrc)THEN
          Ftd(irow)=(Ftd(irow)+1)*(Xn(ir2)/Xnstar(ir2))
          IF(Holgrp.gt.0)THEN
           Fcal(irow)=Ftd(irow)*Fhol(irow)
          ELSE
           Fcal(irow)=Ftd(irow)
          END IF
         ELSE
          Fcal(irow)=dexp(Fcal(irow)+(Xlpyr(ir2)/Xnstar(ir2)))
          Ftd(irow)=Fcal(irow)
         END IF
        ELSE
         Fcal(irow)=dexp(Fcal(irow))
         Fhol(irow)=Fcal(irow)
         IF(Stdgrp.gt.0)Ftd(irow)=Fcal(irow)
c         IF(Holgrp.gt.0)Fhol(irow)=exp(Fhol(irow))
        END IF
       END IF
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
