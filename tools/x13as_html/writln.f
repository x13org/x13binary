C     Last change:  BCM  13 Oct 1998   11:09 am
**==writln.f    processed by SPAG 4.03F  at 09:55 on  1 Mar 1994
      SUBROUTINE writln(Oline,Flhdnl,Flhdn2,Lblnk,Lend)
      IMPLICIT NONE
c     -----------------------------------------------------------------
      INCLUDE 'units.cmn'
*      INCLUDE 'error.cmn'
c     -----------------------------------------------------------------
      INTEGER Flhdnl,Flhdn2
      CHARACTER Oline*(*)
      LOGICAL Lblnk,Lend
c     -----------------------------------------------------------------
      IF(Flhdnl.eq.Mt2.or.Flhdn2.eq.Mt2)CALL errhdr
      IF(Flhdnl.gt.0)THEN
       IF(Lblnk)THEN
        IF(Flhdnl.eq.Mt1.or.Flhdnl.eq.Mt2)THEN
         WRITE(Flhdnl,1010)'<p>'
        ELSE
         WRITE(Flhdnl,1010)' '
        END IF
       END IF
       WRITE(Flhdnl,1010)Oline
       IF(Flhdnl.eq.Mt1.or.Flhdnl.eq.Mt2)THEN
        IF(Lend)WRITE(Flhdnl,1010)'</p>'
       END IF
      END IF
      IF(Flhdn2.gt.0)THEN
       IF(Lblnk)THEN
        IF(Flhdn2.eq.Mt1.or.Flhdn2.eq.Mt2)THEN
         WRITE(Flhdn2,1010)'<p>'
        ELSE
         WRITE(Flhdn2,1010)' '
        END IF
       END IF
       WRITE(Flhdn2,1010)Oline
       IF(Flhdn2.eq.Mt1.or.Flhdn2.eq.Mt2)THEN
        IF(Lend)WRITE(Flhdn2,1010)'</p>'
       END IF
      END IF
 1010 FORMAT('  ',a)
c     -----------------------------------------------------------------
      RETURN
      END
c     -----------------------------------------------------------------
      SUBROUTINE eWritln(Oline,Flhdnl,Flhdn2,Lblnk,Lend)
      IMPLICIT NONE
c     -----------------------------------------------------------------
      INCLUDE 'units.cmn'
*      INCLUDE 'error.cmn'
c     -----------------------------------------------------------------
      INTEGER Flhdnl,Flhdn2
      CHARACTER Oline*(*)
      LOGICAL Lblnk,Lend
c     -----------------------------------------------------------------
      IF(Flhdnl.eq.Mt2.or.Flhdn2.eq.Mt2)CALL errhdr
      IF(Flhdnl.gt.0)THEN
       IF(Flhdnl.eq.Mt1.or.Flhdnl.eq.Mt2)THEN
        WRITE(Flhdnl,1010)'<p><strong>ERROR:</strong> &nbsp; ',Oline
        IF(Lend)WRITE(Flhdnl,1010)'</p>'
       ELSE
        IF(Lblnk)WRITE(Flhdnl,1010)' '
        WRITE(Flhdnl,1010)'ERROR: ',Oline
       END IF
      END IF
      IF(Flhdn2.gt.0)THEN
       IF(Flhdn2.eq.Mt1.or.Flhdn2.eq.Mt2)THEN
        WRITE(Flhdn2,1010)'<p><strong>ERROR:</strong> &nbsp; ',Oline
        IF(Lend)WRITE(Flhdn2,1010)'</p>'
       ELSE
        IF(Lblnk)WRITE(Flhdn2,1010)' '
        WRITE(Flhdn2,1010)'ERROR: ',Oline
       END IF
      END IF
 1010 FORMAT('  ',a:,a)
c     -----------------------------------------------------------------
      RETURN
      END
c     -----------------------------------------------------------------
      SUBROUTINE wWritln(Oline,Flhdnl,Flhdn2,Lblnk,Lend)
      IMPLICIT NONE
c     -----------------------------------------------------------------
      INCLUDE 'units.cmn'
*      INCLUDE 'error.cmn'
c     -----------------------------------------------------------------
      INTEGER Flhdnl,Flhdn2
      CHARACTER Oline*(*)
      LOGICAL Lblnk,Lend
c     -----------------------------------------------------------------
      IF(Flhdnl.eq.Mt2.or.Flhdn2.eq.Mt2)CALL errhdr
      IF(Flhdnl.gt.0)THEN
       IF(Flhdnl.eq.Mt1.or.Flhdnl.eq.Mt2)THEN
        WRITE(Flhdnl,1010)'<p><strong>WARNING:</strong> &nbsp; ',Oline
        IF(Lend)WRITE(Flhdnl,1010)'</p>'
       ELSE
        IF(Lblnk)WRITE(Flhdnl,1010)' '
        WRITE(Flhdnl,1010)'WARNING: ',Oline
       END IF
      END IF
      IF(Flhdn2.gt.0)THEN
       IF(Flhdn2.eq.Mt1.or.Flhdn2.eq.Mt2)THEN
        WRITE(Flhdn2,1010)'<p><strong>WARNING:</strong> &nbsp; ',Oline
        IF(Lend)WRITE(Flhdn2,1010)'</p>'
       ELSE
        IF(Lblnk)WRITE(Flhdn2,1010)' '
        WRITE(Flhdn2,1010)'WARNING: ',Oline
       END IF
      END IF
 1010 FORMAT('  ',a:,a)
c     -----------------------------------------------------------------
      RETURN
      END
c     -----------------------------------------------------------------
c     subroutine for the warning message written to the *_err.html files
c     regarding a link that won't work if it is a relative paths and
c     not a full path.
c     -----------------------------------------------------------------
      SUBROUTINE weWritln(Oline,Eline,Flhdnl,Flhdn2,Lblnk,Lend)
      IMPLICIT NONE
c     -----------------------------------------------------------------
      INCLUDE 'units.cmn'
*      INCLUDE 'error.cmn'
c     -----------------------------------------------------------------
      INTEGER Flhdnl,Flhdn2
      CHARACTER Oline*(*),Eline*(*)
      LOGICAL Lblnk,Lend
c     -----------------------------------------------------------------
      IF(Flhdnl.eq.Mt2.or.Flhdn2.eq.Mt2)CALL errhdr
      IF(Flhdnl.gt.0)THEN
       IF(Flhdnl.eq.Mt1.or.Flhdnl.eq.Mt2)THEN
        WRITE(Flhdnl,1010)'<p><strong>WARNING:</strong> &nbsp; ',Oline
        IF(Lend)WRITE(Flhdnl,1010)'</p>'
       ELSE
        IF(Lblnk)WRITE(Flhdnl,1010)' '
        WRITE(Flhdnl,1010)'WARNING: ',Oline
       END IF
      END IF
      IF(Flhdn2.gt.0)THEN
       IF(Flhdn2.eq.Mt1.or.Flhdn2.eq.Mt2)THEN
        WRITE(Flhdn2,1010)'<p><strong>WARNING:</strong> &nbsp; ',Eline
        IF(Lend)WRITE(Flhdn2,1010)'</p>'
       ELSE
        IF(Lblnk)WRITE(Flhdn2,1010)' '
        WRITE(Flhdn2,1010)'WARNING: ',Eline
       END IF
      END IF
 1010 FORMAT('  ',a:,a)
c     -----------------------------------------------------------------
      RETURN
      END
c     -----------------------------------------------------------------
      SUBROUTINE nWritln(Oline,Flhdnl,Flhdn2,Lblnk,Lend)
      IMPLICIT NONE
c     -----------------------------------------------------------------
      INCLUDE 'units.cmn'
*      INCLUDE 'error.cmn'
c     -----------------------------------------------------------------
      INTEGER Flhdnl,Flhdn2
      CHARACTER Oline*(*)
      LOGICAL Lblnk,Lend
c     -----------------------------------------------------------------
      IF(Flhdnl.eq.Mt2.or.Flhdn2.eq.Mt2)CALL errhdr
      IF(Flhdnl.gt.0)THEN
       IF(Flhdnl.eq.Mt1.or.Flhdnl.eq.Mt2)THEN
        WRITE(Flhdnl,1010)'<p><strong>NOTE:</strong> &nbsp; ',Oline
        IF(Lend)WRITE(Flhdnl,1010)'</p>'
       ELSE
        IF(Lblnk)WRITE(Flhdnl,1010)' '
        WRITE(Flhdnl,1010)'NOTE: ',Oline
       END IF
      END IF
      IF(Flhdn2.gt.0)THEN
       IF(Flhdn2.eq.Mt1.or.Flhdn2.eq.Mt2)THEN
        WRITE(Flhdn2,1010)'<p><strong>NOTE:</strong> &nbsp; ',Oline
        IF(Lend)WRITE(Flhdn2,1010)'</p>'
       ELSE
        IF(Lblnk)WRITE(Flhdn2,1010)' '
        WRITE(Flhdn2,1010)'NOTE: ',Oline
       END IF
      END IF
 1010 FORMAT('  ',a:,a)
c     -----------------------------------------------------------------
      RETURN
      END
