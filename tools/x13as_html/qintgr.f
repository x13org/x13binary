C     Last change:  BCM  14 Oct 97    9:11 am
**==qintgr.f    processed by SPAG 4.03F  at 09:52 on  1 Mar 1994
      LOGICAL FUNCTION qintgr(Str,Nchr)
      IMPLICIT NONE
c     -----------------------------------------------------------------
      INCLUDE 'lex.i'
c     -----------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      CHARACTER chr*1,dmychr*1,Str*(*)
      INTEGER Nchr,pchr
c     ------------------------------------------------------------------
      CHARACTER getchr*1
      EXTERNAL getchr
c     -----------------------------------------------------------------
      pchr=len(Str)
      qintgr=F
      Nchr=0
      DO WHILE (T)
c     -----------------------------------------------------------------
       IF(Nchr.le.pchr)THEN
        chr=getchr(dmychr)
        IF(chr.ge.CZERO.and.chr.le.CNINE)THEN
         qintgr=T
         Nchr=Nchr+1
         Str(Nchr:Nchr)=chr
         GO TO 10
        END IF
c     -----------------------------------------------------------------
        CALL putbak(chr)
       END IF
       GO TO 20
   10  CONTINUE
      END DO
c     -----------------------------------------------------------------
   20 RETURN
      END
