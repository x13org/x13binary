**==issame.f    processed by SPAG 4.03F  at 09:50 on  1 Mar 1994
      LOGICAL FUNCTION issame(Lsrs,L1,L2)
c-----------------------------------------------------------------------
c     Check to see if all the values of a double precision array
c     between pointers L1 and L2 are the same.
c-----------------------------------------------------------------------
c     Revised March 2006 BCM - only test "good" observations.
c-----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION Lsrs(*),base
      INTEGER i,L1,L2
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'goodob.cmn'
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
      issame=.true.
      base=Lsrs(L1)
      DO i=L1+1,L2
       IF(Gudval(i))THEN
        issame=issame.and.dpeq(Lsrs(i),base)
        IF(.not.issame)RETURN
       END IF
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
