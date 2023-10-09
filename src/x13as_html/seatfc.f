      SUBROUTINE seatfc(Ny,Iagr)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     perform procedures from the force spec on SEATS seasonal
c     adjustments - June 2005 - BCM
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'seatcm.cmn'
      INCLUDE 'force.cmn'
      INCLUDE 'inpt.cmn'
      INCLUDE 'orisrs.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11fac.cmn'
      INCLUDE 'x11ptr.cmn'
c     ------------------------------------------------------------------
      LOGICAL rndok
      DOUBLE PRECISION stbase,tempk
      INTEGER i,Iagr,ib,ie,Ny,lstfrc
      DIMENSION stbase(PLEN)
c     ------------------------------------------------------------------
c     use Lfctfr to set last observation to be forced (BCM, May 2006)
c     ------------------------------------------------------------------
      IF(Lfctfr)THEN
       lstfrc=Posffc
      ELSE
       lstfrc=Posfob
      END IF
c     ------------------------------------------------------------------
      IF(Iyrt.gt.0)THEN
c     ------------------------------------------------------------------
c     Based on value of Iyrt, set up target (stbase) for forcing the
c     seasonally adjusted series (BCM, May 2003)
c     ------------------------------------------------------------------
       IF(Iftrgt.eq.0)THEN
        CALL copy(Series,lstfrc,1,stbase)
       ELSE IF(Iftrgt.eq.1)THEN
        CALL copy(Stocal,lstfrc,1,stbase)
       ELSE
        CALL copy(Stopp,lstfrc,1,stbase)
        IF(Iftrgt.eq.3)CALL divsub(stbase,stbase,Faccal,Pos1ob,lstfrc)
       END IF
c     -----------------------------------------------------------------
       IF(Iyrt.eq.1)THEN
        CALL qmap(stbase,Seatsa,Setsa2,Pos1ob,lstfrc,Ny,ib,ie,Begyrt)
c     ------------------------------------------------------------------
c     Change made October 1995 to duplicate X-11-ARIMA/88 partial year
c     adjustment of yearly totals. BCM
c     ------------------------------------------------------------------
        IF(ie.lt.lstfrc)THEN
         tempk=Setsa2(ie)-Seatsa(ie)
         DO i=ie+1,lstfrc
          Setsa2(i)=Seatsa(i)+tempk
         END DO
        END IF
c     ------------------------------------------------------------------
c     Change made May 2005 to do the same partial year adjustment
c     to early data BCM
c     ------------------------------------------------------------------
        IF(ib.gt.Pos1ob)THEN
         tempk=Setsa2(ib)-Seatsa(ib)
         DO i=Posfob,ib-1
          Setsa2(i)=Seatsa(i)+tempk
         END DO
        END IF
       ELSE IF(Iyrt.eq.2)THEN
        CALL qmap2(stbase,Seatsa,Setsa2,Pos1ob,lstfrc,Ny,Iagr)
       END IF
      ELSE
       CALL copy(Seatsa,lstfrc,1,Setsa2) 
      END IF
c     ------------------------------------------------------------------
c     If option selected ensure the rounded seasonally adjusted values
c     equals the rounded seasonally adjusted total.
c     ------------------------------------------------------------------
      IF(Lrndsa)THEN
       CALL rndsa(Setsa2,Stsarn,Pos1ob,Posfob,rndok)
       IF(.not.rndok)Lrndsa=rndok
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
