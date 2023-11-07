**==skparg.f    processed by SPAG 4.03F  at 09:53 on  1 Mar 1994
      SUBROUTINE skparg
      IMPLICIT NONE
c----------------------------------------------------------------------
c     Tries to skip over to the next argument in the function as
c best it can.  Since the function arguments are of the form:
c     NAME = VALUE   or
c     NAME = (VALUE LIST) .
c If the last token is not the ='s then it finds the value
c or value list.  If the last token is then ='s then it tries to
c find the ='s first.  Note this will skip over comments and quotes
c because it uses lex.
c----------------------------------------------------------------------
      LOGICAL T
      PARAMETER(T=.true.)
c     -----------------------------------------------------------------
      INCLUDE 'lex.i'
c     -----------------------------------------------------------------
      INTEGER clsgrp
      EXTERNAL clsgrp
c     -----------------------------------------------------------------
      IF(Nxtktp.eq.EQUALS)CALL lex()
c     -----------------------------------------------------------------
      IF(Nxtktp.eq.LPAREN.or.Nxtktp.eq.LBRAKT)THEN
       CALL skplst(clsgrp(Nxtktp))
      ELSE IF(Nxtktp.eq.DBL.or.Nxtktp.eq.INTGR.or.Nxtktp.eq.NAME.or.
     &        Nxtktp.eq.QUOTE)THEN
       CALL lex()
      ELSE
       CALL inpter(PERROR,Lstpos,'Expected NAME=VALUE or NAME=(LIST) '//
     &             'not "'//Nxttok(1:Nxtkln)//'"',T)
       CALL abend
      END IF
c     -----------------------------------------------------------------
      RETURN
c     -----------------------------------------------------------------
      END
