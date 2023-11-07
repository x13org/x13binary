C     Last change:  BCM  15 Jan 98   11:54 am
      SUBROUTINE rvrghd(Othndl,Mt1,Lsav,Lprt)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Print out header information for revisions regressor history 
c     table      
c-----------------------------------------------------------------------
      INCLUDE 'title.cmn'
      INCLUDE 'revtbl.i'
      INCLUDE 'cchars.i'
c-----------------------------------------------------------------------
      INTEGER Othndl,Mt1
      LOGICAL Lsav,Lprt,locok
c-----------------------------------------------------------------------
      IF(Lsav)THEN
       CALL opnfil(.true.,.false.,LREVOT,Othndl,locok)
       IF(.not.locok)THEN
        CALL abend
        RETURN
       END IF
       WRITE(Othndl,1010)'date',TABCHR,'action',TABCHR,'regressors'
       WRITE(Othndl,1010)'----',TABCHR,'------',TABCHR,'----------'
      END IF
      IF(Lprt)THEN
       CALL genSkip(LREVOT)
       CALL mkTableTag(Mt1,'w80','@')
       CALL mkCaption(Mt1,'Actions on regARIMA outlier '//
     &                'regressors from full data span')
       CALL writTag(Mt1,'<tr>')
       CALL mkHeaderCellScope(Mt1,0,0,'col','@','Ending Date')
       CALL mkHeaderCellScope(Mt1,0,0,'col','@','Action')
       CALL mkHeaderCellScope(Mt1,0,0,'col','@','Outliers')
       CALL writTag(Mt1,'</tr>')
      END IF
c-----------------------------------------------------------------------
      RETURN
 1010 FORMAT(a,a,a,a,a)
      END
