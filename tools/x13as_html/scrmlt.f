**==scrmlt.f    processed by SPAG 4.03F  at 09:53 on  1 Mar 1994
      SUBROUTINE scrmlt(C,N,X)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Subroutine to multiply a double precision vector by a scalar
c-----------------------------------------------------------------------
      INTEGER i,N
      DOUBLE PRECISION C,X(*)
c     ------------------------------------------------------------------
      DO i=1,N
       X(i)=C*X(i)
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
