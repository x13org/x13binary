**==setlg.f    processed by SPAG 4.03F  at 09:53 on  1 Mar 1994
      SUBROUTINE setlg(Lconst,N,X)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Subroutine to set a logical vector to lconst*J
c-----------------------------------------------------------------------
      LOGICAL Lconst,X
      INTEGER i,N
      DIMENSION X(*)
c     ------------------------------------------------------------------
      DO i=1,N
       X(i)=Lconst
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
