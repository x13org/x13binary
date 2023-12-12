**==issame.f    processed by SPAG 4.03F  at 09:50 on  1 Mar 1994
      LOGICAL FUNCTION ispos(Lsrs,L1,L2)
c-----------------------------------------------------------------------
c     Check to see if all the values of a double precision array
c     between pointers L1 and L2 are greater than zero.
c-----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION Lsrs(*)
      INTEGER i,L1,L2
c-----------------------------------------------------------------------
      ispos=.true.
      DO i=L1,L2
       IF(Lsrs(i).le.0D0)THEN
        ispos=.false.
        RETURN
       END IF
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
