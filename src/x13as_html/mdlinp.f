C     Last change:  BCM  10 Feb 1999    4:19 pm
**==mdlinp.f    processed by SPAG 4.03F  at 15:51 on 14 Apr 1994
      SUBROUTINE mdlinp(File,Inptok)
      IMPLICIT NONE
c     -----------------------------------------------------------------
      CHARACTER File*(*)
      LOGICAL Inptok,ldmy,rngbuf
      EXTERNAL rngbuf
c     -----------------------------------------------------------------
      INCLUDE 'lex.i'
      INCLUDE 'error.cmn'
c     -----------------------------------------------------------------
      CALL fopen(File,'model file for automatic model selection','OLD',
     &           Inputx,Inptok)
      IF(.not.Inptok)RETURN
      REWIND(Inputx)
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
      CALL lex()
c     -----------------------------------------------------------------
      RETURN
      END
