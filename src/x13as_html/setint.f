**==setint.f    processed by SPAG 4.03F  at 09:53 on  1 Mar 1994
      SUBROUTINE setint(Intcnt,Nh,H)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Subroutine to set an integer vector to intcnt*J
c-----------------------------------------------------------------------
      INTEGER i,Intcnt,Nh,H(*)
c     ------------------------------------------------------------------
      DO i=1,Nh
       H(i)=Intcnt
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
