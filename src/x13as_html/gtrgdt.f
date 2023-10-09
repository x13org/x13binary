C     Last change:  BCM  28 Sep 1998    8:54 am
      SUBROUTINE gtrgdt(Havesp,Sp,Regdat,Zeroz,Locok,Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     gets the date for a change of regime regressor
c-----------------------------------------------------------------------
      INCLUDE 'lex.i'
c     ------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c     ------------------------------------------------------------------
      LOGICAL argok,Havesp,Inptok,Locok
      INTEGER Regdat,Sp,Zeroz
      DIMENSION Regdat(2)
c     ------------------------------------------------------------------
      Locok=T
      Zeroz=0
c     ------------------------------------------------------------------
      IF(Nxtktp.eq.EOF)THEN
       Locok=F
c     ------------------------------------------------------------------
      ELSE
c     ------------------------------------------------------------------
c     IF double slash found at beginning, get the next character and
c     set Zeroz to indicate zeros before the regime date.
c     ------------------------------------------------------------------
       CALL lex()
       IF(Nxtktp.eq.SLASH)THEN
        Zeroz=-1
        CALL lex()
       END IF
c     ------------------------------------------------------------------
c     Get regime date.  
c     ------------------------------------------------------------------
       CALL getdat(Havesp,Sp,Regdat,argok,Locok)
       IF(.not.argok)THEN
        CALL inpter(PERROR,Errpos,'Expected a date not "'//
     &              Nxttok(1:Nxtkln)//'"',T)
        Locok=F
       ELSE
c     ------------------------------------------------------------------
c     IF double slash found at end, get the next character and set Zeroz
c     to indicate zeros after the regime date.
c     ------------------------------------------------------------------
        CALL lex()
        IF(Nxtktp.eq.SLASH)THEN
         Zeroz=1-Zeroz
         CALL lex()
        END IF
       END IF
c     ------------------------------------------------------------------
      END IF
      Inptok=Inptok.and.Locok
c     ------------------------------------------------------------------
      RETURN
      END
