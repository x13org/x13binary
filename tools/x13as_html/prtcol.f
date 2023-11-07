      SUBROUTINE prtcol(Ny,Mt1,Nop,Noplbl,Nopabb,Colhdr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This subroutine prints column headers for the table subroutine.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
c-----------------------------------------------------------------------
      CHARACTER Colhdr*22,Noplbl*5,Nopabb*(*),amonth*9,cperiod*2
      INTEGER i,Mt1,Nop,Ny,nmonth
      DIMENSION Colhdr(PSP+2),amonth(12),nmonth(12),cperiod(12)
c-----------------------------------------------------------------------
      DATA amonth/'January  ','February ','March    ','April    ',
     &            '@        ','June     ','July     ','August   ',
     &            'September','October  ','November ','December '/
      DATA nmonth/7,8,5,5,1,4,4,6,9,7,8,8/
      DATA cperiod/' 1',' 2',' 3',' 4',' 5',' 6',' 7',' 8',' 9','10',
     &             '11','12'/
c-----------------------------------------------------------------------
      CALL writTag(Mt1,'<tr>')
      CALL mkTableCell(Mt1,'head','&nbsp;')
c-----------------------------------------------------------------------
      IF(Ny.eq.12)THEN
       do i = 1,Ny
        CALL mkHeaderCellScope(Mt1,0,0,'col',amonth(i)(1:nmonth(i)),
     &                         Colhdr(i+1))
       end do
      ELSE IF (Ny.eq.4)THEN
       do i = 1,Ny
        CALL mkHeaderCellScope(Mt1,0,0,'col','@',
     &                         Colhdr(i+1)//' Quarter')
       end do
      ELSE
       do i = 1,Ny
        CALL mkHeaderCellScope(Mt1,0,0,'col','@','Period '//cperiod(i))
       end do
      END IF
c-----------------------------------------------------------------------
      IF(Nop.lt.5)CALL mkHeaderCellScope(Mt1,0,0,'col',Nopabb,Noplbl)
      CALL writTag(Mt1,'</tr>')
c-----------------------------------------------------------------------
      RETURN
      END
