      SUBROUTINE mkealb(Eastr,Neachr,Eastst,Easidx,Easwin,Lbase,Lcap)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     Generate string for label of easter effect within AIC test
c     for trading day
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      CHARACTER Eastr*(30),cwin*2
      INTEGER Eastst,Easidx,Easwin,Neachr,nwin
      LOGICAL Lbase,Lcap
c-----------------------------------------------------------------------
      CALL setchr(' ',30,eastr)
      Neachr=0
      IF(Easidx.eq.0)THEN
       IF(Eastst.eq.1)THEN
        eastr(1:7)='easter['
        Neachr=7
        IF(Lcap)eastr(1:1)='E'
       ELSE
        eastr(1:12)='easterstock['
        Neachr=12
        IF(Lcap)eastr(1:1)='E'
       END IF
      ELSE
       eastr(1:14)='statcaneaster['
       Neachr=14
       IF(Lcap)eastr(1:1)='S'
      END IF
c-----------------------------------------------------------------------
      nwin=1
      CALL setchr(' ',2,cwin)
      CALL itoc(Easwin,cwin,nwin)
      IF(Lfatal)RETURN
      Eastr((Neachr+1):(Neachr+nwin))=cwin(1:(nwin-1))//']'
c-----------------------------------------------------------------------
      IF(Lbase)THEN
       Neachr=Neachr-1
      ELSE
       Neachr=Neachr+nwin
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
      