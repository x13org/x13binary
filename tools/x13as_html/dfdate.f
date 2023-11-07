**==dfdate.f    processed by SPAG 4.03F  at 09:47 on  1 Mar 1994
      SUBROUTINE dfdate(Datea,Dateb,Sp,Diff)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Finds the difference between two dates where the dates, datea and
c dateb are length 2 arrays of year then period.
c-----------------------------------------------------------------------
      INTEGER Datea(2),Dateb(2),Sp,Diff
c-----------------------------------------------------------------------
      IF(Sp.gt.1)THEN
       Diff=Sp*(Datea(1)-Dateb(1))+Datea(2)-Dateb(2)
      ELSE
       Diff=Datea(1)-Dateb(1)
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
