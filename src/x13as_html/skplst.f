**==skplst.f    processed by SPAG 4.03F  at 09:53 on  1 Mar 1994
      SUBROUTINE skplst(Clsgtp)
c----------------------------------------------------------------------
c     Looks for a close list character or end-of-file and returns.
c Note this will skip over comments and quotes because it uses lex.
c----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'lex.i'
c     -----------------------------------------------------------------
      INTEGER Clsgtp
c     -----------------------------------------------------------------
      DO WHILE (.true.)
       IF(Nxtktp.ne.Clsgtp.and.Nxtktp.ne.EOF)THEN
        CALL lex()
c     -----------------------------------------------------------------
       ELSE
        CALL lex()
        RETURN
       END IF
      END DO
c     -----------------------------------------------------------------
      END
