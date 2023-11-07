C     Last change:  BCM  18 Nov 97    1:49 pm
      INTEGER FUNCTION lstpth(Filnam,Nfil)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c  Returns the position of the last character in the path of the file
c  name given in Filnam (returns 0 if there is no file name).
c-----------------------------------------------------------------------
      INCLUDE 'lex.i'
      CHARACTER Filnam*(*)
      INTEGER jchr,Nfil
c-----------------------------------------------------------------------
      DO lstpth=Nfil,1,-1
       jchr=ichar(Filnam(lstpth:lstpth))
cdos  backslash for directory
cdos       IF(jchr.eq.COLON.or.jchr.eq.BSLASH)GO TO 10
cunix forward slash for directory
       IF(jchr.eq.SLASH.or.jchr.eq.COLON)GO TO 10
      END DO
c     ------------------------------------------------------------------
      lstpth=0
   10 RETURN
      END
