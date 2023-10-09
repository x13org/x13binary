C     Last change:  SRD  19 Nov 99    5:57 am
      SUBROUTINE mkoprt(Optype,Period,Sp,Oprnam,Noprcr)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      CHARACTER armatl*(2),Oprnam*(*)
      INTEGER ipos,Noprcr,Optype,Period,Sp
      DIMENSION armatl(2:3)
      SAVE armatl
      DATA armatl/'AR','MA'/
c     ------------------------------------------------------------------
      IF(Period.eq.1)THEN
       Oprnam(1:11)='Nonseasonal'
       ipos=12
c     ------------------------------------------------------------------
      ELSE IF(Period.eq.Sp)THEN
       Oprnam(1:8)='Seasonal'
       ipos=9
c     ------------------------------------------------------------------
      ELSE
       Oprnam(1:7)='Period '
       ipos=8
       CALL itoc(Period,Oprnam,ipos)
       IF(Lfatal)RETURN
      END IF
c     ------------------------------------------------------------------
      IF(Optype.eq.DIFF)THEN
       Noprcr=ipos+10
       Oprnam(ipos:Noprcr)=' Difference'
      ELSE
       Noprcr=ipos+2
       Oprnam(ipos:Noprcr)=' '//armatl(Optype)
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
