**==isdate.f    processed by SPAG 4.03F  at 09:50 on  1 Mar 1994
      LOGICAL FUNCTION isdate(Dat,Sp)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Check the date
c-----------------------------------------------------------------------
      INTEGER Dat,Sp
      DIMENSION Dat(2)
c     ------------------------------------------------------------------
c     BCM changed routine Oct 31, 1995 to allow for year zero.
c     ------------------------------------------------------------------
      IF((Sp.gt.1.and.(Dat(2).lt.1.or.Dat(2).gt.Sp)).or.Dat(1).lt.0)THEN
       isdate=.false.
      ELSE
       isdate=.true.
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
