C     Last change:  BCM   4 Sep 1998    1:52 pm
**==gtdcnm.f    processed by SPAG 4.03F  at 09:49 on  1 Mar 1994
      SUBROUTINE gtdcnm(Args,Argptr,Nargs,Argidx,Argok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Returns the dictionary index if the name is found, 0 if the name
c is not found, and argOK is false if the token is not a name and the
c index, argidx, is 0.  If the name is not in the dictionary the
c next token isn't input.
c-----------------------------------------------------------------------
      INCLUDE 'lex.i'
c     ------------------------------------------------------------------
      CHARACTER Args*(*)
      INTEGER Argptr,Nargs,Argidx,strinx
      LOGICAL Argok
      DIMENSION Argptr(0:Nargs)
      EXTERNAL lex,strinx
c     ------------------------------------------------------------------
      Argidx=0
      Argok=.true.
      IF(Nxtktp.ne.EOF)THEN
       IF(Nxtktp.ne.NAME)THEN
        Argok=.false.
c     ------------------------------------------------------------------
       ELSE
        Argidx=strinx(.false.,Args,Argptr,1,Nargs,Nxttok(1:Nxtkln))
        IF(Argidx.gt.0)CALL lex()
       END IF
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
