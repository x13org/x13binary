C     Last change:  BCM  24 Nov 97   12:26 pm
      SUBROUTINE armacr
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Calculates and print the correlation matrix from (X'X)^-1
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'htmlout.cmn'
c     ------------------------------------------------------------------
      DOUBLE PRECISION ZERO
      INTEGER OPRS
      PARAMETER(OPRS=2,ZERO=0D0)
c     ------------------------------------------------------------------
      CHARACTER cfix*7,tmpttl*(POPRCR)
      DOUBLE PRECISION tmpR
      INTEGER beglag,begopr,endlag,endopr,icol,iestpm,iflt,ilag,iopr,j,
     &        ntmpcr,ncol
c     ------------------------------------------------------------------
      IF(Nestpm.le.1)RETURN
      iestpm=0
      ncol=0
c     ------------------------------------------------------------------
      CALL writTagOneLine(Mt1,'h3','@',
     &             '<abbr title="autoregressive moving average">ARMA'//
     &             '</abbr> Parameter Correlation Matrix')
      CALL mkTableTag(Mt1,'w90','@')
      CALL mkCaption(Mt1,'<abbr title="autoregressive moving '//
     &             'average">ARMA</abbr> Parameter Correlation Matrix')
      CALL writTag(Mt1,'<tr>')
      CALL mkTableCell(Mt1,'head','&nbsp;')
      DO iflt=AR,MA
       begopr=Mdl(iflt-1)
       endopr=Mdl(iflt)-1
c     ------------------------------------------------------------------
       DO iopr=begopr,endopr
        beglag=Opr(iopr-1)
        endlag=Opr(iopr)-1
c     ------------------------------------------------------------------
        CALL isfixd(OPRS,Arimaf,beglag,endlag,cfix)
        IF(cfix.eq.' &nbsp;')THEN
         CALL getstr(Oprttl,Oprptr,Noprtl,iopr,tmpttl,ntmpcr)
         IF(Lfatal)RETURN
c     ------------------------------------------------------------------
         DO ilag=beglag,endlag
          IF(.not.Arimaf(ilag))THEN
           WRITE(Mt1,1010)'col',tmpttl(1:ntmpcr)//Cbr,Arimal(ilag)
 1010      FORMAT('<th scope="',a,'">',a,'Lag ',i2,'</th>')
           ncol=ncol+1
          END IF
         END DO
        END IF
       END DO
      END DO
      CALL writTag(Mt1,'</tr>')
c     ------------------------------------------------------------------
      DO iflt=AR,MA
       begopr=Mdl(iflt-1)
       endopr=Mdl(iflt)-1
c     ------------------------------------------------------------------
       DO iopr=begopr,endopr
        beglag=Opr(iopr-1)
        endlag=Opr(iopr)-1
c     ------------------------------------------------------------------
        CALL isfixd(OPRS,Arimaf,beglag,endlag,cfix)
        IF(cfix.eq.' &nbsp;')THEN
         CALL getstr(Oprttl,Oprptr,Noprtl,iopr,tmpttl,ntmpcr)
         IF(Lfatal)RETURN
c     ------------------------------------------------------------------
         DO ilag=beglag,endlag
          IF(.not.Arimaf(ilag))THEN
           iestpm=iestpm+1
           CALL writTag(Mt1,'<tr>')
           WRITE(Mt1,1010)'row',tmpttl(1:ntmpcr)//Cbr,Arimal(ilag)
           DO j=1,iestpm
            tmpR=Armacm(iestpm,j)/
     &           sqrt(Armacm(j,j)*Armacm(iestpm,iestpm))
            IF (tmpR.lt.ZERO)THEN
             WRITE(Mt1,1030)tmpR
            ELSE
             WRITE(Mt1,1040)tmpR
            END IF
           END DO
 1030      FORMAT('<td class="nowrapcenter"> ',F10.4,' </td>')
 1040      FORMAT('<td class="center"> ',F10.4,' </td>')
           IF(iestpm.lt.ncol)THEN
            DO j=iestpm+1,ncol
             CALL mkTableCell(Mt1,'@','&nbsp;')
            END DO
           END IF
           CALL writTag(Mt1,'</tr>')
          END IF
         END DO
        END IF
       END DO
      END DO
      CALL writTag(Mt1,'</table>')
      CALL mkPOneLine(Mt1,'@','&nbsp;')
c     ------------------------------------------------------------------
      RETURN
      END

