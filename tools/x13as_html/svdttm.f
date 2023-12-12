      SUBROUTINE svdttm(Nform,datstr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Function for Lahey fortran (PC) to print date and time information
c     to the .xdg file.
c-----------------------------------------------------------------------
      CHARACTER datstr*24
      INTEGER Nform
c-----------------------------------------------------------------------
*      CHARACTER cvdttm*(24)
*      EXTERNAL cvdttm
c-----------------------------------------------------------------------
      WRITE(Nform,1020)datstr(1:15)
 1020 FORMAT('date:',a)
      WRITE(Nform,1030)datstr(15:24)
 1030 FORMAT('time:',a)
c-----------------------------------------------------------------------
      RETURN
      END

