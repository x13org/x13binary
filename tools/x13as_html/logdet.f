**==logdet.f    processed by SPAG 4.03F  at 09:51 on  1 Mar 1994
      SUBROUTINE logdet(Ap,N,Lgdt)
c-----------------------------------------------------------------------
c     Returns the log of the determinate of a packed triangular matrix
c-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER i,ielt,N
      DOUBLE PRECISION Ap(*),Lgdt
c     ------------------------------------------------------------------
      Lgdt=0D0
      ielt=0
c     ------------------------------------------------------------------
      DO i=1,N
       ielt=ielt+i
       Lgdt=Lgdt+2D0*log(Ap(ielt))
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
