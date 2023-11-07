**==istrue.f    processed by SPAG 4.03F  at 09:50 on  1 Mar 1994
      LOGICAL FUNCTION istrue(Lsrs,L1,L2)
c-----------------------------------------------------------------------
c     Check to see if there is at least one true element in an array
c     of logicals between two positions of the array.
c-----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL Lsrs(*)
      INTEGER i,L1,L2
c-----------------------------------------------------------------------
      istrue=.false.
      DO i=L1,L2
       istrue=istrue.or.Lsrs(i)
       IF(istrue)RETURN
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
