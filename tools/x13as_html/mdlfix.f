C     Last change:  BCM  13 Oct 1998    3:31 pm
      SUBROUTINE mdlfix()
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Test whether all the fixed parameters have values
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER (T=.TRUE.,F=.FALSE.)
c     ------------------------------------------------------------------
      INTEGER beglag,begopr,endlag,endopr,iflt,ilag,iopr
      LOGICAL lmdlfx
c     ------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c     ------------------------------------------------------------------
      lmdlfx=T
      Imdlfx=0
      DO iflt=DIFF,MA
       begopr=Mdl(iflt-1)
       endopr=Mdl(iflt)-1
c     ------------------------------------------------------------------
       DO iopr=begopr,endopr
        beglag=Opr(iopr-1)
        endlag=Opr(iopr)-1
c     ------------------------------------------------------------------
        DO ilag=beglag,endlag
         IF(dpeq(Arimap(ilag),DNOTST))THEN
          IF(lmdlfx)lmdlfx=F
         ELSE
          lmdlfx=lmdlfx.and.Arimaf(ilag)
          IF(Imdlfx.eq.0)Imdlfx=1
          IF(Arimaf(ilag).and.Imdlfx.eq.1)Imdlfx=2
         END IF
        END DO
       END DO
      END DO
      IF(lmdlfx.and.Imdlfx.gt.0)Imdlfx=3
c     ------------------------------------------------------------------
      RETURN
      END
