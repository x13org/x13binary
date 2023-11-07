C     Last change:  BCM  16 Sep 2005   11:15 am
      SUBROUTINE getmtd(Tdgrp,Begxy,Nrxy,Fcntyp,Lam)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This subroutine sets up a variable which tells which regARIMA
c     trading day factor is associated with which type of month.
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'tdtyp.cmn'
c-----------------------------------------------------------------------
      INTEGER Begxy,clrngs,i,icol,Tdgrp,Nrxy,bdif,ib,Fcntyp,ndif,nn,n1,
     &        clend,igrp,lpyrgp
      DOUBLE PRECISION ttd,Lam,fac,tlpyr
      DIMENSION Begxy(2),ttd(PLEN),tlpyr(PLEN)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
c     Initialize ttd to zero
c-----------------------------------------------------------------------
      CALL setdp(0.0D0,Nrxy,ttd)
      CALL setdp(0.0D0,Nrxy,tlpyr)
c-----------------------------------------------------------------------
c     Generate trading day factor for first 6 trading day regression
c     variables (the "pure td"), and transform back to original scale.
c-----------------------------------------------------------------------
      IF(Tdgrp.gt.0)THEN
       clrngs=Grp(Tdgrp-1)
       clend=MIN((Grp(Tdgrp)-1),(clrngs+5))
       DO icol=clrngs,clend
        CALL daxpy(Nrxy,B(icol),Xy(icol),Ncxy,ttd,1)
       END DO
      ELSE
       Tdtbl=0
       RETURN
      END IF
      IF((Fulltd.or.Tdzero.eq.2).and.Lrgmtd)THEN
       clrngs=0
       igrp=Tdgrp+1
       DO WHILE (clrngs.eq.0)
        IF(Rgvrtp(Grp(igrp-1)).eq.PRRTTD.or.
     &     Rgvrtp(Grp(igrp-1)).eq.PRRTST.or.
     &     Rgvrtp(Grp(igrp-1)).eq.PRATTD.or.
     &     Rgvrtp(Grp(igrp-1)).eq.PRATST.or.
     &     Rgvrtp(Grp(igrp-1)).eq.PRR1TD.or.
     &     Rgvrtp(Grp(igrp-1)).eq.PRA1TD)THEN
         clrngs=Grp(igrp-1)
         clend=MIN((Grp(igrp)-1),(clrngs+5))
         DO icol=clrngs,clend
          CALL daxpy(Nrxy,B(icol),Xy(icol),Ncxy,ttd,1)
         END DO
        END IF
        igrp=igrp+1
       END DO
      END IF
c-----------------------------------------------------------------------
c     Convert ttd so that the variable is indexed properly for the
c     seasonal adjustment routines.
c-----------------------------------------------------------------------
      CALL invfcn(ttd,Nrxy,Fcntyp,Lam,ttd)
      fac=1D0
      IF(Muladd.ne.1)fac=100D0
c-----------------------------------------------------------------------
c     see if there are leapyear regression factors
c-----------------------------------------------------------------------
      lpyrgp=0
      IF(.not.Picktd)THEN
       DO igrp=1,Ngrp
        IF(Rgvrtp(Grp(igrp-1)).eq.PRGTLY.or.
     &     Rgvrtp(Grp(igrp-1)).eq.PRRTLY.or.
     &     Rgvrtp(Grp(igrp-1)).eq.PRATLY)THEN
         clrngs=Grp(igrp-1)
         CALL daxpy(Nrxy,B(clrngs),Xy(clrngs),Ncxy,tlpyr,1)
         IF(lpyrgp.eq.0)lpyrgp=igrp
        END IF
       END DO
       IF(lpyrgp.gt.0)CALL invfcn(tlpyr,Nrxy,Fcntyp,Lam,tlpyr)
      END IF
c-----------------------------------------------------------------------
c     Copy regARIMA trading day factors for the given type-of-month.
c-----------------------------------------------------------------------
*      CALL dfdate(Begxy,Begtdy,Sp,ndif)
      n1=1
      nn=Nrxy
      bdif=Nbcst2-Nbcst
      IF(Lrgmtd)THEN
       CALL dfdate(Tddate,Begxy,Sp,ndif)
       IF(Tdzero.lt.0)THEN
        n1=ndif+n1
       ELSE
        nn=ndif
       END IF
      END IF
      DO i=n1,nn
       ib=i+bdif
       IF(dpeq(Tdmdl(Tday(ib)),DNOTST))Tdmdl(Tday(ib))=ttd(i)*fac
       IF(lpyrgp.gt.0)THEN
        IF(Tday(ib).gt.21)THEN
         IF(dpeq(Lpmdl(1),DNOTST))Lpmdl(1)=tlpyr(i)*fac
        ELSE IF(Tday(ib).ge.15)THEN
         IF(dpeq(Lpmdl(2),DNOTST))Lpmdl(2)=tlpyr(i)*fac
        END IF
       END IF
      END DO
      IF((Fulltd.or.Tdzero.eq.2).and.Lrgmtd)THEN
       IF(Tdzero.ge.0)THEN
        DO i=nn+1,Nrxy
         ib=i+bdif
         IF(dpeq(Tdmdl1(Tday(ib)),DNOTST))Tdmdl1(Tday(ib))=ttd(i)*fac
         IF(lpyrgp.gt.0.AND.(dpeq(Lpmdl1(1),DNOTST).or.
     &      dpeq(Lpmdl1(2),DNOTST)).and.Tday(ib).ge.15)THEN
          IF(Tday(ib).gt.21)THEN
           IF(dpeq(Lpmdl1(1),DNOTST))Lpmdl1(1)=tlpyr(i)*fac
          ELSE IF(Tday(ib).ge.15)THEN
           IF(dpeq(Lpmdl1(2),DNOTST))Lpmdl1(2)=tlpyr(i)*fac
          END IF
         END IF
        END DO
       ELSE
        DO i=1,n1
         ib=i+bdif
         IF(dpeq(Tdmdl1(Tday(ib)),DNOTST))Tdmdl1(Tday(ib))=ttd(i)*fac
         IF(lpyrgp.gt.0.AND.(dpeq(Lpmdl1(1),DNOTST).or.
     &      dpeq(Lpmdl1(2),DNOTST)).and.Tday(ib).ge.15)THEN
          IF(Tday(ib).gt.21)THEN
           IF(dpeq(Lpmdl1(1),DNOTST))Lpmdl1(1)=tlpyr(i)*fac
          ELSE IF(Tday(ib).ge.15)THEN
           IF(dpeq(Lpmdl1(2),DNOTST))Lpmdl1(2)=tlpyr(i)*fac
          END IF
         END IF
        END DO
       END IF
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
