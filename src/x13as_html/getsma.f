      DOUBLE PRECISION FUNCTION getsma()
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     Function that returns value of first order seasonal moving
c     term in ARIMA model - will return 0.0 if not found.
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      DOUBLE PRECISION ZERO
      PARAMETER(ZERO=0d0)
c     ------------------------------------------------------------------
      CHARACTER tmpttl*(PGRPCR)
      INTEGER begopr,endopr,iopr,beglag,endlag,ilag,ntmpcr
c     ------------------------------------------------------------------
      getsma=ZERO
      begopr=Mdl(MA-1)
      endopr=Mdl(MA)-1
      IF(begopr.le.endopr)THEN
       DO iopr=begopr,endopr
        CALL getstr(Oprttl,Oprptr,Noprtl,iopr,tmpttl,ntmpcr)
        IF(Lfatal)RETURN
        IF(tmpttl(1:ntmpcr).eq.'Seasonal MA')THEN
         beglag=Opr(iopr-1)
         endlag=Opr(iopr)-1
         DO ilag=beglag,endlag
          IF(Arimal(ilag).eq.Sp)THEN
           getsma=Arimap(beglag)
           RETURN
          END IF
         END DO
        END IF
       END DO
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
	  
