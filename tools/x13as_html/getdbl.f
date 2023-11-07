C     Last change:  BCM  15 Jan 98   11:07 am
**==getdbl.f    processed by SPAG 4.03F  at 09:49 on  1 Mar 1994
      LOGICAL FUNCTION getdbl(Tmp)
      IMPLICIT NONE
c----------------------------------------------------------------------
c     Returns an integer from the input stream and returns
c true otherwise returns false and tmp is undefined.
c----------------------------------------------------------------------
      INCLUDE 'lex.i'
c     -----------------------------------------------------------------
      INTEGER ipos
      DOUBLE PRECISION ctod,Tmp
      EXTERNAL ctod
c     -----------------------------------------------------------------
      getdbl=.false.
c     -----------------------------------------------------------------
      IF(Nxtktp.ne.EOF)THEN
       ipos=Lstpos(PCHAR)
c     -----------------------------------------------------------------
       Tmp=ctod(Linex(1:Lineln),ipos)
       IF(ipos.gt.Lstpos(PCHAR))THEN
        Pos(PCHAR)=ipos
        getdbl=.true.
        CALL lex()
       END IF
      END IF
c     -----------------------------------------------------------------
      RETURN
      END
