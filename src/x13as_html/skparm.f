**==skparm.f    processed by SPAG 4.03F  at 09:53 on  1 Mar 1994
      SUBROUTINE skparm(Lauto)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Trys to point the input stream beyond the current an ARMA model
c-----------------------------------------------------------------------
      INCLUDE 'lex.i'
c     ------------------------------------------------------------------
      LOGICAL Lauto
c     ------------------------------------------------------------------
      DO WHILE (.true.)
c     ------------------------------------------------------------------
       IF(Nxtktp.ne.NAME.and.Nxtktp.ne.QUOTE.and.Nxtktp.ne.RBRACE.and.
     &    Nxtktp.ne.EOF)THEN
c     ------------------------------------------------------------------
c     Added by BCM April 1996 to break out of endless loop when a "bad" 
c     model is entered in a model file.
c     ------------------------------------------------------------------
        IF(Lauto.and.Nxtktp.eq.STAR)RETURN
c     ------------------------------------------------------------------
        CALL lex()
        GO TO 10
       END IF
c     ------------------------------------------------------------------
       RETURN
   10  CONTINUE
      END DO
      END
