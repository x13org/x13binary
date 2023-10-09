C     Last change:  BCM  27 May 1998    1:17 pm
      SUBROUTINE pritd(Ptdwt,Ptdfac,Nrxy,Sp,Begdat,Muladd,Psuadd,Kswv,
     &                 Frstob)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This subroutine generates trading day prior adjustment factors
c     from a set of user-specified trading day weights
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c-----------------------------------------------------------------------
      LOGICAL Psuadd,begrgm
      DOUBLE PRECISION tdxy,Ptdwt,btd,Ptdfac,tmp1,tmp2
      INTEGER i,rtype,ncxy,Sp,Begdat,Nrxy,Muladd,Kswv,Frstob,Pridat
      DIMENSION Begdat(2),Tdxy(PLEN*6),Ptdwt(7),btd(6),rtype(6),
     &          Pridat(2),Ptdfac(PLEN),tmp1(PLEN),tmp2(PLEN),
     &          begrgm(PLEN)
c-----------------------------------------------------------------------
c     Generate trading day regressors
c-----------------------------------------------------------------------
      ncxy=6
      CALL setlg(T,PLEN,begrgm)
      IF(Frstob.gt.1)THEN
       CALL addate(Begdat,Sp,Frstob-1,Pridat)
      ELSE
       CALL cpyint(Begdat,2,1,Pridat)
      END IF
      CALL td6var(Pridat,Sp,Nrxy,ncxy,1,ncxy,0,tdxy,begrgm,F)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     convert X-11 style td weights, set up rtype
c-----------------------------------------------------------------------
      DO i=1,6
       btd(i)=Ptdwt(i)-1D0
       rtype(i)=PRGTTD
      END DO
c-----------------------------------------------------------------------
c     generate factors for prior trading day
c-----------------------------------------------------------------------
      CALL x11ref(tmp1,Ptdfac,tmp2,Frstob,Muladd,Psuadd,1,0,0,F,0,rtype,
     &            Nrxy,ncxy,btd,tdxy,6,0,Kswv,T,F)
c-----------------------------------------------------------------------
      IF(Frstob.gt.1)THEN
       DO i=Nrxy+Frstob-1,Frstob,-1
        Ptdfac(i)=Ptdfac(i-Frstob+1)
       END DO
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
