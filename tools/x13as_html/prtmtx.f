C     Last change:  BCM  12 Mar 98   12:30 pm
      SUBROUTINE prtmtx(Begxy,Sp,Xy,Nrxy,Ncxy,Ttlstr,Ttlptr,Nttl,
     &                  Tblttl,thisId)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c      INCLUDE 'srslen.prm'
c      INCLUDE 'model.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'htmlout.cmn'
c     ------------------------------------------------------------------
      CHARACTER dash*25,space*25,Tblttl*(*),thisId*(3)
      CHARACTER str*(10),Ttlstr*(*),fmt1*(7),fmt2*(9),fmt3*(21)
      INTEGER Begxy,ibeg,ielt,iend,idate,nchr,Ncxy,Nrxy,Nttl,Sp,Ttlptr,
     &        nt,colwid,maxwid,ncol
      INTEGER Mxtbwd
      DOUBLE PRECISION Xy
      DIMENSION Begxy(2),idate(2),Ttlptr(0:Nttl),Xy(*),colwid(11)
      DATA dash/'-------------------------'/
      DATA space/'                         '/
c     ------------------------------------------------------------------
c     Initialize variables used in printing matrix
c     Changed by Brian Monsell, October 25, 1994
c     ------------------------------------------------------------------
      IF(.not.(thisId.eq.'xxx'))CALL makDivId(Mt1,thisId,'@')
      CALL mkTableTag(Mt1,'w90',Tblttl)
      CALL mkCaption(Mt1,Tblttl)
c     ------------------------------------------------------------------
      CALL writTag(Mt1,'<tr>')
      CALL mkTableCell(Mt1,'head','&nbsp;')
      DO ielt=1,Nttl
       CALL mkHeaderCellScope(Mt1,0,0,'col','@',
     &                        Ttlstr(Ttlptr(ielt-1):Ttlptr(ielt)-1))
      END DO
      CALL writTag(Mt1,'</tr>')
c     ------------------------------------------------------------------
*      WRITE(fmt3,1020)ncol,maxwid
* 1020 FORMAT('(2x,a8,(:t11,',i1,'E',i2,'.4))')
      DO iend=Nttl,Ncxy*Nrxy,Ncxy
       CALL writTag(Mt1,'<tr>')
       ibeg=iend-Nttl+1
       CALL addate(Begxy,Sp,(iend-Nttl+Ncxy)/Ncxy-1,idate)
       CALL wrtdat(idate,Sp,str,nchr)
       IF(Lfatal)RETURN
       CALL mkHeaderCellScope(Mt1,0,0,'row','@',str(1:nchr))
       DO ielt=ibeg,iend
        IF(Xy(ielt).ge.0D0)THEN
         WRITE(Mt1,1020)'right',Xy(ielt)
        ELSE
         WRITE(Mt1,1020)'nowrap',Xy(ielt)
        END IF
       END DO
       CALL writTag(Mt1,'</tr>')
      END DO
 1020 FORMAT('<td class="',a,'">',g16.6,'</td>')
      IF(thisId.eq.'xxx')THEN
       CALL writTag(Mt1,'</table>')
      ELSE
       CALL writTag(Mt1,'</table></div>')
      END IF
      CALL mkPOneLine(Mt1,'@','&nbsp;')
c     ------------------------------------------------------------------
      RETURN
      END
