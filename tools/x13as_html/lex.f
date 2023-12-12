C     Last change:  BCM  12 Mar 98   12:20 pm
**==lex.f    processed by SPAG 4.03F  at 09:50 on  1 Mar 1994
      SUBROUTINE lex()
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Lex returns the token in Nxtktp, its length in Nxtkln, and its
c type in Nxtktp.
c-----------------------------------------------------------------------
      INCLUDE 'lex.i'
      INCLUDE 'cchars.i'
c     ------------------------------------------------------------------
      LOGICAL alsoin,qcmmnt,qdoble,qname,qquote
      INTEGER whitsp
      EXTERNAL qcmmnt,qdoble,qname,qquote,whitsp
c     ------------------------------------------------------------------
      DO WHILE (whitsp().ne.EOF)
       IF(qcmmnt(Nxttok,Nxtkln))THEN
        Nxtktp=COMMNT
c     ------------------------------------------------------------------
       ELSE IF(qquote(Nxttok,Nxtkln))THEN
        Nxtktp=QUOTE
c     ------------------------------------------------------------------
       ELSE IF(qname(Nxttok,Nxtkln))THEN
        Nxtktp=NAME
c     ------------------------------------------------------------------
       ELSE IF(qdoble(Nxttok,Nxtkln,alsoin))THEN
        IF(alsoin)THEN
         Nxtktp=INTGR
c     ------------------------------------------------------------------
        ELSE
         Nxtktp=DBL
        END IF
       ELSE
        CALL qtoken()
       END IF
c     ------------------------------------------------------------------
       IF(Nxtktp.ne.COMMNT)GO TO 10
      END DO
c     ------------------------------------------------------------------
      Nxtktp=EOF
      Nxttok(1:1)=CHREOF
      Nxtkln=1
c     ------------------------------------------------------------------
   10 CALL cpyint(Lstpos,2,1,Errpos)
      Lstpos(PLINE)=Pos(PLINE)
      Lstpos(PCHAR)=Pos(PCHAR)-Nxtkln
      IF(Nxtktp.eq.QUOTE)Lstpos(PCHAR)=Lstpos(PCHAR)-2
c     ------------------------------------------------------------------
      RETURN
      END
