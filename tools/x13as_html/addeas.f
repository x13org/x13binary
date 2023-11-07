C     Last change:  BCM  22 Sep 1998   10:59 am
      SUBROUTINE addeas(Keastr,Easidx,Eastst)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This subroutine adds trading day or holiday regressors for the 
c     automatic AIC test.
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      LOGICAL F
      PARAMETER(F=.FALSE.)
c-----------------------------------------------------------------------
      CHARACTER tgrptl*(PGRPCR)
      INTEGER ipos,Keastr,nchr,Easidx,Eastst,etype
c-----------------------------------------------------------------------
c     Add Easter regressor to the regression matrix
c-----------------------------------------------------------------------
      IF(Easidx.eq.0)THEN
       IF(Eastst.eq.1)THEN
        tgrptl='Easter['
        ipos=8
        etype=PRGTEA
       ELSE
        tgrptl='StockEaster['
        ipos=13
        etype=PRGTES
       END IF
       CALL itoc(Keastr,tgrptl,ipos)
      ELSE
       tgrptl='StatCanEaster['
       ipos=15
       CALL itoc(Keastr-Easidx,tgrptl,ipos)
       etype=PRGTEC
      END IF
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
      tgrptl(ipos:ipos)=']'
      nchr=ipos
      CALL adrgef(DNOTST,tgrptl(1:nchr),tgrptl(1:nchr),etype,F,F)
c-----------------------------------------------------------------------
      RETURN
      END
