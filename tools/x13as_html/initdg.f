C     Last change:  BCM   6 Aug 2004    2:29 pm
      SUBROUTINE initdg(Lsumm,Irev,Issap,Muladd)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     Initial variables used to store SEATS diagnostics to NULL value
c     ------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'seatdg.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'svllog.prm'
      INCLUDE 'svllog.cmn'
      INCLUDE 'setsvl.i'
c     ------------------------------------------------------------------
      INTEGER ipr,ips,idr,ids,iqr,iqs,id,ip,iq,iprs,iqrs,Lsumm,Irev,n,
     &        Issap,Muladd
      LOGICAL istrue
      EXTERNAL istrue
c     ------------------------------------------------------------------
      CALL initst
c-----------------------------------------------------------------------
c       Convert X-13A-S model variables into variables compatable with
c       TRAMO/SEATS model data structure
c     ------------------------------------------------------------------
      CALL cnvmdl(ipr,ips,idr,ids,iqr,iqs,id,ip,iq,iprs,iqrs,n)
      IF(Lfatal)RETURN
      CALL mkmdsn(ipr,idr,iqr,ips,ids,iqs,X13mdl,Nxmdl)
      IF(Lfatal)RETURN
c     ------------------------------------------------------------------
      IF (Issap.eq.2.or.Irev.eq.4) RETURN
      IF ((.not.istrue(Svltab,LSLSMD,LSLAAD)).and.Lsumm.eq.0) RETURN
c     ------------------------------------------------------------------
      IF(Svltab(LSLSNR).or.Lsumm.gt.0)THEN
       Kurt=DNOTST
       Kurtse=DNOTST
       Testnm=DNOTST
       Skew=DNOTST
       Skewse=DNOTST
       Sdres=DNOTST
      END IF
      IF(Svltab(LSLCEE).or.Lsumm.gt.0)THEN
       Ceetrn=DNOTST
       Ceesad=DNOTST
      END IF
      IF(Svltab(LSLAAD).or.Lsumm.gt.0)THEN
       Aadasa=DNOTST
       Aadatr=DNOTST
      END IF
      IF(Svltab(LSLTSE).or.Lsumm.gt.0)THEN
       Tsetrn=DNOTST
       Tsesea=DNOTST
       Tsetcm=DNOTST
       Tsesad=DNOTST
      END IF
      IF(Svltab(LSLSSG).or.Lsumm.gt.0)THEN
       Ssghst=NOTSET
       Ssgcnc=NOTSET
       Ssgfct=NOTSET
      ENDIF
c-----------------------------------------------------------------------
      IF(Svltab(LSLPRS).or.Lsumm.gt.0)THEN
       CALL setdp(DNOTST,5,Prsetr)
       CALL setdp(DNOTST,5,Prsesa)
      END IF
      IF(Svltab(LSLCVR).or.Lsumm.gt.0)THEN
       CALL setdp(DNOTST,3,Vartrn)
       CALL setdp(DNOTST,3,Varsad)
       CALL setdp(DNOTST,3,Varirr)
       CALL setdp(DNOTST,3,Varsea)
      END IF
c-----------------------------------------------------------------------
      IF(Svltab(LSLSMD).or.Lsumm.gt.0)THEN
       Iprsm=NOTSET
       Iqrsm=NOTSET
       Ipssm=NOTSET
       Iqssm=NOTSET
       Idrsm=NOTSET
       Idssm=NOTSET
      END IF
c-----------------------------------------------------------------------
      IF(Lsumm.gt.0)THEN
       IF(Muladd.eq.0)THEN
        WRITE(Nform,1000)'samodeseats','logarithmic seasonal adjustment'
       ELSE
        WRITE(Nform,1000)'samodeseats','additive seasonal adjustment'
       END IF
       WRITE(Nform,1000)'x13mdl',X13mdl(1:Nxmdl)
      END IF
 1000 FORMAT(a,': ',a)
c-----------------------------------------------------------------------
      RETURN
      END
