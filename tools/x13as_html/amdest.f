C     Last change:  BCM   5 Mar 1999   11:08 am
      SUBROUTINE amdest(Trnsrs,Nelta,Nefobs,Ardsp,Lmu,Lprt,Info)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c       Driver for routine that performs Hannen/Riesen estimation of
c       ARMA models as in TRAMO/SEATS program by Gomez/Maravall.
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      DOUBLE PRECISION ZERO
      INTEGER KP,PR
      LOGICAL F
      PARAMETER(PR=PLEN/4,KP=50,F=.FALSE.,ZERO=0D0)
c     ------------------------------------------------------------------
      DOUBLE PRECISION Trnsrs,hrp,r,tmp,tmpsrs
      INTEGER Ardsp,i,iar,ipr,ips,idr,ids,iqr,iqs,id,ip,iq,iprs,iqrs,n,
     &        Nelta,m1,Nefobs,n35,n1,Info
      LOGICAL Lmu,Lprt
      DIMENSION Trnsrs(PLEN),hrp(PARIMA),r(PR),tmp(PR),tmpsrs(PLEN)
c     ------------------------------------------------------------------
c       Convert X-13A-S model variables into variables compatable with
c       TRAMO/SEATS model data structure
c     ------------------------------------------------------------------
      CALL cnvmdl(ipr,ips,idr,ids,iqr,iqs,id,ip,iq,iprs,iqrs,n)
      IF(Lfatal)RETURN
      IF ((ip+iq).EQ.0) RETURN
      DO i=1,iprs+iqrs
       hrp(i)=ZERO
      END DO
c     ------------------------------------------------------------------
c       Compute ACF
c     ------------------------------------------------------------------
      n35=INT(DLOG(DBLE(Nelta))**2)
      iar=n35
      m1=MAX0(iar,MAX0(ip,2*iq))
      iar=MAX0(m1,n35)
      n1=Nelta
      IF (iar.GE.n1) iar=MIN0(m1,n1-n1/4)
      IF (iar.GT.KP) iar=KP
      CALL acf(Trnsrs,Nelta,Nefobs,r,tmp,iar,n,Sp,0,Lmu,F,F)
c     ------------------------------------------------------------------
c       Get Hannen-Rissen estimates of ARMA(p,q) model.
c     ------------------------------------------------------------------
      Info=0
      CALL hrest(iar,Trnsrs,r,hrp,ipr,ips,iqr,iqs,iq,iprs,Sp,Nelta,
     &           Nefobs,Lprt,Info)
c     ------------------------------------------------------------------
c	put HR estimates into X-13ARIMA-SEATS variable
c     ------------------------------------------------------------------
      IF(Info.eq.0)THEN
c     ------------------------------------------------------------------
       DO i=1,ipr
        Arimap(Ardsp+i)=-hrp(i)
       END DO
       DO i=1,ips
        Arimap(Ardsp+ipr+i)=-hrp(ipr+i)
       END DO
       DO i=1,iqr
        Arimap(Ardsp+iprs+i)=-hrp(iprs+i)
       END DO
       DO i=1,iqs
        Arimap(Ardsp+iprs+iqr+i)=-hrp(iprs+iqr+i)
       END DO
      ELSE
       IF(Info.lt.0)Armaer=-Info
       RETURN
      END IF
c     ------------------------------------------------------------------
c       Perform AR filtering if MA and AR terms are in model.
c     ------------------------------------------------------------------
      IF(Ip.gt.0.and.Iq.gt.0)THEN
       CALL copy(Trnsrs,Nelta,-1,tmpsrs)
       CALL arflt(Nelta,Arimap,Arimal,Opr,Mdl(AR-1),Mdl(AR)-1,tmpsrs,
     &            n1)
       n35=INT(DLOG(DBLE(n1))**2)
       iar=n35
       m1=MAX0(iar,2*iq)
       iar=MAX0(m1,n35)
       IF (iar.GE.n1) iar=MIN0(m1,n1-n1/4)
       CALL acfar(iar,r,tmpsrs,Nelta-ip,ip,iq)
c       CALL acfar(iar,r,tmpsrs,Nelta,ip,iq)
       CALL hrest(iar,tmpsrs,r,hrp,0,0,iqr,iqs,iq,iprs,Sp,Nelta-ip,
     &            Nefobs,Lprt,Info)
       IF(Info.eq.0)THEN
        DO i=1,iqr
         Arimap(Ardsp+iprs+i)=-hrp(iprs+i)
        END DO
        DO i=1,iqs
         Arimap(Ardsp+iprs+iqr+i)=-hrp(iprs+iqr+i)
        END DO
       ELSE
        IF(Info.lt.0)Armaer=-Info
        RETURN
       END IF
      END IF
      RETURN
      END

