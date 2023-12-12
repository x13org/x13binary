C     Last change:  BCM  15 Jan 98   11:08 am
      CHARACTER*1 FUNCTION getchr(Nxtchr)
c-----------------------------------------------------------------------
c     getchr.f, Release 1, Subroutine Version 1.3, Modified 20 Oct 1994.
c-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'lex.i'
      LOGICAL rngbuf
      CHARACTER*1 Nxtchr
      EXTERNAL rngbuf
c     -----------------------------------------------------------------
      IF(Pos(PCHAR).gt.Lineln)THEN
       IF(rngbuf(2,Lineno,Linex,Lineln))THEN
        Pos(PLINE)=Lineno
        Pos(PCHAR)=1
       ELSE
        Pos(PCHAR)=1
       END IF
      END IF
c     -----------------------------------------------------------------
      getchr=Linex(Pos(PCHAR):Pos(PCHAR))
      Pos(PCHAR)=Pos(PCHAR)+1
c     -----------------------------------------------------------------
      Nxtchr=getchr
c     -----------------------------------------------------------------
      RETURN
      END
