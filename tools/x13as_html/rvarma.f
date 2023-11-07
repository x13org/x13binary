      SUBROUTINE rvarma(Revptr,ChARMA,NchARMA,Nrvarma,CncARMA)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'rev.prm'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      INTEGER Revptr,iarma,iflt,begopr,endopr,iopr,beglag,endlag,ilag,
     &        ntmpcr,ictmp,NchARMA,Nrvarma
      CHARACTER tmpttl*(PGRPCR+5),ChARMA*(PGRPCR+5),ctmp*(3)
      DOUBLE PRECISION CncARMA
      DIMENSION ChARMA(PARIMA),NchARMA(PARIMA),CncARMA(PARIMA,PREV)
c-----------------------------------------------------------------------
      iarma=1
      DO iflt=AR,MA
       begopr=Mdl(iflt-1)
       endopr=Mdl(iflt)-1
c-----------------------------------------------------------------------
       DO iopr=begopr,endopr
        beglag=Opr(iopr-1)
        endlag=Opr(iopr)-1
        CALL getstr(Oprttl,Oprptr,Noprtl,iopr,tmpttl,ntmpcr)
        IF(Lfatal)RETURN
c-----------------------------------------------------------------------
        DO ilag=beglag,endlag
         IF(.not.Arimaf(ilag))THEN
          CncARMA(iarma,Revptr)=Arimap(ilag)
          IF(Revptr.eq.1)THEN
           chARMA(iarma)=' '
           ictmp=1
           CALL itoc(Arimal(ilag),ctmp,ictmp)
           IF(Lfatal)RETURN
           ChARMA(iarma)=tmpttl(1:ntmpcr)//'['//ctmp(1:(ictmp-1))//']'
           NchARMA(iarma)=ntmpcr+ictmp+1
           Nrvarma=iarma
          END IF
          iarma=iarma+1
         END IF
        END DO
c-----------------------------------------------------------------------
       END DO
c-----------------------------------------------------------------------
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
      