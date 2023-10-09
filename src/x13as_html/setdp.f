**==setdp.f    processed by SPAG 4.03F  at 09:53 on  1 Mar 1994
      SUBROUTINE setdp(Const,N,X)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Subroutine to set a double precision vector to const*J
c-----------------------------------------------------------------------
      INTEGER i,N
      DOUBLE PRECISION Const,X(*)
c     ------------------------------------------------------------------
      DO i=1,N
       X(i)=Const
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
