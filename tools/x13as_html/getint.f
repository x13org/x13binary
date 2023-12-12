C     Last change:  BCM  15 Jan 98   11:07 am
**==getint.f    processed by SPAG 4.03F  at 09:49 on  1 Mar 1994
      LOGICAL FUNCTION getint(Tmp)
      IMPLICIT NONE
c----------------------------------------------------------------------
c     Returns an integer from the input stream and returns
c true otherwise returns false and tmp is undefined.
c----------------------------------------------------------------------
      INCLUDE 'lex.i'
c----------------------------------------------------------------------
      INTEGER ctoi,ipos,Tmp
      EXTERNAL ctoi
c----------------------------------------------------------------------
      getint=.false.
c----------------------------------------------------------------------
      IF(Nxtktp.ne.EOF)THEN
       ipos=Lstpos(PCHAR)
c----------------------------------------------------------------------
       Tmp=ctoi(Linex(1:Lineln),ipos)
       IF(ipos.gt.Lstpos(PCHAR))THEN
        Pos(PCHAR)=ipos
        getint=.true.
        CALL lex()
       END IF
      END IF
c----------------------------------------------------------------------
      RETURN
      END
