C     Last change:  BCM  12 Mar 98   12:21 pm
**==qtoken.f    processed by SPAG 4.03F  at 09:52 on  1 Mar 1994
      SUBROUTINE qtoken()
      IMPLICIT NONE
c     -----------------------------------------------------------------
      INCLUDE 'lex.i'
      INCLUDE 'cchars.i'
c     -----------------------------------------------------------------
      CHARACTER chr*1,dmychr*1
      INTEGER toktyp
c     -----------------------------------------------------------------
      CHARACTER getchr*1
      EXTERNAL getchr
c     -----------------------------------------------------------------
      chr=getchr(dmychr)
      IF(chr.eq.CHREOF.or.chr.eq.NEWLIN)THEN
       CALL putbak(chr)
c     ------------------------------------------------------------------
      ELSE
       IF(chr.eq.'{')THEN
        toktyp=LBRACE
c     ------------------------------------------------------------------
       ELSE IF(chr.eq.'}')THEN
        toktyp=RBRACE
c     ------------------------------------------------------------------
       ELSE IF(chr.eq.'(')THEN
        toktyp=LPAREN
c     ------------------------------------------------------------------
       ELSE IF(chr.eq.')')THEN
        toktyp=RPAREN
c     ------------------------------------------------------------------
       ELSE IF(chr.eq.'[')THEN
        toktyp=LBRAKT
c     ------------------------------------------------------------------
       ELSE IF(chr.eq.']')THEN
        toktyp=RBRAKT
c     ------------------------------------------------------------------
       ELSE IF(chr.eq.',')THEN
        toktyp=COMMA
c     ------------------------------------------------------------------
       ELSE IF(chr.eq.'+')THEN
        toktyp=PLUS
c     ------------------------------------------------------------------
       ELSE IF(chr.eq.'-')THEN
        toktyp=MINUS
c     ------------------------------------------------------------------
       ELSE IF(chr.eq.'=')THEN
        toktyp=EQUALS
c     -----------------------------------------------------------------
       ELSE IF(chr.eq.'.')THEN
        toktyp=PERIOD
c     -----------------------------------------------------------------
       ELSE IF(chr.eq.'/')THEN
        toktyp=SLASH
c     -----------------------------------------------------------------
       ELSE IF(chr.eq.'*')THEN
        toktyp=STAR
c     -----------------------------------------------------------------
       ELSE
        toktyp=BADTOK
       END IF
       Nxtkln=1
       Nxtktp=toktyp
       Nxttok(1:1)=chr
      END IF
c     -----------------------------------------------------------------
      RETURN
      END
