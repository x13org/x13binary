C     Last change:  SRD  31 Jan 100    8:37 am
      SUBROUTINE armats(Tval)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     Generate t statistics for ARMA parameter estimates
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'units.cmn'
c-----------------------------------------------------------------------
      INTEGER itv,iflt,begopr,endopr,beglag,endlag,ilag,iopr
      DOUBLE PRECISION Tval
      DIMENSION Tval(*)
c-----------------------------------------------------------------------
c     generate t-statistics for ARMA parameters
c-----------------------------------------------------------------------
      IF(Armaer.eq.PACSER)THEN
       CALL eWritln('The covariance matrix of the ARMA parameters is '//
     &              'singular;',STDERR,Mt2,T,F)
       CALL writln('       cannot compute t-statistics for the ARMA '//
     &             'parameters.',STDERR,Mt2,F,T)
       CALL abend()
       RETURN
      END IF
      itv=0
      DO iflt=AR,MA
       begopr=Mdl(iflt-1)
       endopr=Mdl(iflt)-1
       DO iopr=begopr,endopr
        beglag=Opr(iopr-1)
        endlag=Opr(iopr)-1
        DO ilag=beglag,endlag
         itv=itv+1
         tval(itv)=Arimap(ilag)/sqrt(Var*Armacm(itv,itv))
        END DO
       END DO
      END DO
c-----------------------------------------------------------------------
      RETURN
      END

