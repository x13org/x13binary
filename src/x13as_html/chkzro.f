      SUBROUTINE chkzro(Ori,Sa,Sa2,Sarnd,Ocal,Pos1,Pos2,Kfulsm)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c --- this subroutine checks the values of the original series,
c     seasonally adjusted series, trend and calendar adjusted series
c     prior to the computation of month-to-month changes to set the
c     value of Gudval to false if any of the values are less than or
c     equal to zero.
c     ------------------------------------------------------------------
c     written by Brian Monsell, March 2006
c     ------------------------------------------------------------------
      LOGICAL F
      DOUBLE PRECISION ZERO
      PARAMETER(F=.false.,ZERO=0D0)
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'force.cmn'
      INCLUDE 'goodob.cmn'
c     ------------------------------------------------------------------
      DOUBLE PRECISION Ori,Sa,Sa2,Sarnd,Ocal
      INTEGER Pos1,Pos2,i,Kfulsm
      DIMENSION Ori(PLEN),Sa(PLEN),Sa2(PLEN),Sarnd(PLEN),Ocal(PLEN)
c     ------------------------------------------------------------------
      DO i = Pos1, Pos2
       IF(Gudval(i))THEN
        IF(.not.(Ori(i).gt.ZERO.and.(Kfulsm.eq.0.and.Sa(i).gt.ZERO)
     &     .and.Ocal(i).gt.ZERO))THEN
         Gudval(i)=F
        ELSE IF((Iyrt.gt.0.or.Lrndsa).and.Kfulsm.eq.0)THEN
         IF(Iyrt.gt.0)THEN
          IF(.not.(Sa2(i).gt.ZERO))Gudval(i)=F
         END IF
         IF(Lrndsa)THEN
          IF(.not.(Sarnd(i).gt.ZERO))Gudval(i)=F
         END IF
        END IF
       END IF
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
