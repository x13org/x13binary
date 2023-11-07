      SUBROUTINE cnvfil(Oldfile,Nold,Newfile,Nnew,Nlstpth)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This subroutine replaces spaces in file names and paths with %20
c     and (for DOS systems) forward slashes for back slashes
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'lex.i'
c-----------------------------------------------------------------------
      CHARACTER Oldfile*(PFILCR),Newfile*(PFILCR)
      INTEGER Nold,Nnew,Nlstpth,i,jchr
c-----------------------------------------------------------------------
      Nnew=0
      DO i=1,Nold
       jchr=ichar(Oldfile(i:i))
       Nnew=Nnew+1
       IF(Oldfile(i:i).eq.' ')THEN
        Newfile(Nnew:(Nnew+2))='%20'
        Nnew=Nnew+2
       ELSE IF(jchr.eq.BSLASH)THEN
        Newfile(Nnew:Nnew)='/'
       ELSE
        Newfile(Nnew:Nnew)=Oldfile(i:i)
       END IF
      END DO
c-----------------------------------------------------------------------
      DO Nlstpth=Nnew,1,-1
       jchr=ichar(Newfile(Nlstpth:Nlstpth))
cdos  backslash for directory
cdos       IF(jchr.eq.COLON.or.jchr.eq.SLASH)GO TO 10
cunix forward slash for directory
       IF(jchr.eq.SLASH.or.jchr.eq.COLON)GO TO 10
      END DO
c     ------------------------------------------------------------------
      Nlstpth=0
c-----------------------------------------------------------------------
  10  RETURN
      END
