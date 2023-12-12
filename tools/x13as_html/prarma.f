      SUBROUTINE prARMA(Fh,Lhtml)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Prints out input file with regression, ARIMA specs
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      CHARACTER armopr*(4),firstC*(25),endC*(18)
      INTEGER Fh,iflt,begopr,endopr,iopr,beglag,endlag,nfirst,nend,ilag
      LOGICAL Lhtml
      DIMENSION armopr(2:3)
c-----------------------------------------------------------------------
      DATA armopr/'ar  ','ma  '/
c-----------------------------------------------------------------------
c     Write out the values
c Probably should only the differencing if it is different
c then the (1-B^sp)^d form.  This would be hard.
c-----------------------------------------------------------------------
      firstC=' '
      endC=' '
      IF(Lhtml)THEN
       CALL writTag(Fh,'<p>')
       firstC=Cbr//'&nbsp;&nbsp;&nbsp;'
       nfirst=24
       endC='&nbsp;'
       nend=6
      ELSE
       nfirst=3
       nend=3
      END IF
c     ------------------------------------------------------------------
      DO iflt=AR,MA
       begopr=Mdl(iflt-1)
       endopr=Mdl(iflt)-1
       IF(endopr.ge.begopr)THEN
        WRITE(Fh,1070)endC(1:nend),armopr(iflt)
 1070   FORMAT(a,a,'=(')
c     ------------------------------------------------------------------
        DO iopr=begopr,endopr
         beglag=Opr(iopr-1)
         endlag=Opr(iopr)-1
c     ------------------------------------------------------------------
         DO ilag=beglag,endlag
          IF(Arimaf(ilag))THEN
           WRITE(Fh,1080)firstC(1:nfirst),Arimap(ilag),'f'
 1080      FORMAT(a,e24.10,a)
          ELSE
           WRITE(Fh,1080)firstC(1:nfirst),Arimap(ilag),' '
          END IF
         END DO
        END DO
        WRITE(Fh,1090)endC
 1090   FORMAT(a,')')
        IF(iflt.eq.AR.and.Lhtml)CALL writTag(Fh,Cbr)
       END IF
      END DO
c     ------------------------------------------------------------------
      IF(Lhtml)CALL writTag(Fh,'</p>')
c     ------------------------------------------------------------------
      RETURN
      END