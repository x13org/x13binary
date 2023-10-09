**==yprmy.f    processed by SPAG 4.03F  at 09:56 on  1 Mar 1994
      SUBROUTINE yprmy(Y,Nr,Ypy)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Calculate the inner product of y
c-----------------------------------------------------------------------
c Name  Type Description
c-----------------------------------------------------------------------
c i       i  Local do loop index
c nr      i  Input number of rows in both x and y
c y       r  Input nr long vector
c ypy     r  Output scalar of y'y
c-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO
      PARAMETER(ZERO=0.0D0)
c      LOGICAL T,F
c      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      INTEGER i,Nr
      DOUBLE PRECISION Y(*),Ypy
c-----------------------------------------------------------------------
c     Take the sum of squares of y.
c-----------------------------------------------------------------------
      Ypy=ZERO
c     ------------------------------------------------------------------
c      CALL under0(T)
c     ------------------------------------------------------------------
      DO i=1,Nr
       Ypy=Ypy+Y(i)**2
      END DO
c     ------------------------------------------------------------------
c      CALL under0(F)
c     ------------------------------------------------------------------
      RETURN
      END
