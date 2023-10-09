C     Last change:  BCM  15 Jan 98   11:08 am
**==intinp.f    processed by SPAG 4.03F  at 09:50 on  1 Mar 1994
      SUBROUTINE intinp(Instr)
      IMPLICIT NONE
c     -----------------------------------------------------------------
      LOGICAL T
      PARAMETER(T=.true.)
c     -----------------------------------------------------------------
      INCLUDE 'lex.i'
      INCLUDE 'error.cmn'
c     -----------------------------------------------------------------
      INTEGER Instr
      LOGICAL ldmy,rngbuf
      EXTERNAL rngbuf
c     -----------------------------------------------------------------
      Inputx=Instr
      Lineno=0
      Lineln=0
      ldmy=rngbuf(1,Lineno,Linex,Lineln)
      IF(.not.ldmy.or.Lfatal)RETURN
      Pos(PLINE)=0
      Pos(PCHAR)=1
      Lstpos(PLINE)=0
      Lstpos(PCHAR)=1
      Errpos(PLINE)=0
      Errpos(PCHAR)=1
c     -----------------------------------------------------------------
      CALL lex()
      IF(Nxtktp.eq.EOF)THEN
       CALL inpter(PERROR,Pos,
     &             'Cannot process empty input specifications file.',T)
       CALL abend()
      END IF
c     -----------------------------------------------------------------
      RETURN
      END
