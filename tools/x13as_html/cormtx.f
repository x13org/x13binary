C     Last change:  BCM   2 Jun 1998   11:33 am
      SUBROUTINE cormtx(Xpxinv,Regidx)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Calculates and print the correlation matrix from (X'X)^-1
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO
      PARAMETER(ZERO=0D0)
c     ------------------------------------------------------------------
      CHARACTER str*(PCOLCR),thisVar*13
      INTEGER icol,idiag,ielt,irow,nchr,Regidx,tcol,jcol,i1,i2,j
      DOUBLE PRECISION scale,Xpxinv
      DIMENSION Xpxinv(Nb*Ncxy/2),Regidx(PB),tcol(PB)
c-----------------------------------------------------------------------
      jcol=Nb
      j=1
      CALL writTagOneLine(Mt1,'h3','@',
     &                    'Regression Parameter Correlation Matrix')
      CALL mkTableTag(Mt1,'w90','@')
      CALL mkCaption(Mt1,'Regression Parameter Correlation Matrix')
      CALL writTag(Mt1,'<tr>')
      CALL mkTableCell(Mt1,'head','&nbsp;')
      IF(Iregfx.eq.2)THEN
       DO icol=1,Nb
        IF(Regidx(icol).ne.NOTSET)THEN
         tcol(j)=icol
         j=j+1
        ElSE
         jcol=jcol-1
        END IF
       END DO
       DO icol=1,jcol
        CALL getstr(Colttl,Colptr,Ncoltl,tcol(icol),str,nchr)
        CALL mkHeaderCellScope(Mt1,0,0,'col','@',str(1:nchr))
       END DO
      ELSE
       DO icol=j,jcol
        CALL getstr(Colttl,Colptr,Ncoltl,icol,str,nchr)
        CALL mkHeaderCellScope(Mt1,0,0,'col','@',str(1:nchr))
       END DO
      END IF
      CALL writTag(Mt1,'</tr>')
c-----------------------------------------------------------------------
      idiag=0
      DO icol=1,Nb
       IF(Regidx(icol).ne.NOTSET)THEN
        CALL writTag(Mt1,'<tr>')
        idiag=idiag+Regidx(icol)
        scale=sqrt(Xpxinv(idiag))
c-----------------------------------------------------------------------
        DO ielt=idiag-Regidx(icol)+1,idiag
c         IF(Regidx(ielt).ne.NOTSET)THEN
c          jcol=Regidx(ielt)
          Xpxinv(ielt)=Xpxinv(ielt)/scale
c         END IF
        END DO
c-----------------------------------------------------------------------
        ielt=idiag
        DO irow=icol,Nb
         IF(Regidx(irow).ne.NOTSET)THEN
          Xpxinv(ielt)=Xpxinv(ielt)/scale
          ielt=ielt+Regidx(irow)
         END IF
        END DO
c-----------------------------------------------------------------------
        CALL getstr(Colttl,Colptr,Ncoltl,icol,str,nchr)
        IF(Lfatal)RETURN
        CALL mkHeaderCellScope(Mt1,0,0,'row','@',str(1:nchr))
        i1=idiag-Regidx(icol)+1
        i2=idiag
        DO ielt=i1,i2
         IF(Xpxinv(ielt).lt.ZERO)THEN
          WRITE(Mt1,1020)Xpxinv(ielt)
         ELSE
          WRITE(Mt1,1020)Xpxinv(ielt)
         END IF
        END DO
 1020   FORMAT('<td class="nowrap">',F10.4,'</td>')
 1030   FORMAT('<td>',F10.4,'</td>')
        IF((i2-i1+1).lt.jcol)THEN
         DO ielt=i2-i1+2,jcol
          CALL mkTableCell(Mt1,'@','&nbsp;')
         END DO
        END IF
        CALL writTag(Mt1,'</tr>')
       END IF
      END DO
      CALL writTag(Mt1,'</table>')
      CALL mkPOneLine(Mt1,'@','&nbsp;')
c-----------------------------------------------------------------------
      RETURN
c-----------------------------------------------------------------------
      END
