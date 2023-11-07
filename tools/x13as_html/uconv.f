**==uconv.f    processed by SPAG 4.03F  at 09:54 on  1 Mar 1994
      SUBROUTINE uconv(Fulma,Mxmalg,C)
*      SUBROUTINE uconv(Fulma,Mxmalg,C,Pc)
c-----------------------------------------------------------------------
c     Calculates the autocovariance function of a moving average
c model whose coefficients are in A, using the innovation variance V and
c saving the result in C .
c The definition in terms of generating functions is C(Z)=A(Z).V.A(ZINV).
c     FORTRAN corrections made by Bill Bell -- 9/10/92
c 1. Integer declaration statement put before double precision declaration
c    statement
c 2. REAL A(mxmalg) changed to double precision A(max(mxmalg,1)) to handle case
c    where mxmalg = 0
c Changes made:  9/21/92,  Bill Bell
c 1. IMPLICIT NONE    statement added
c 2. REAL type statements changed to DOUBLE PRECISION
c-----------------------------------------------------------------------
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INTEGER i,Mxmalg,k,qmi
*      INTEGER Pc
*      DOUBLE PRECISION Fulma(0:Mxmalg),C(0:Pc),sum
      DOUBLE PRECISION Fulma(0:Mxmalg),C(0:Mxmalg),sum
c-----------------------------------------------------------------------
      DO i=0,Mxmalg
       C(i)=Fulma(i)
      END DO
c     ------------------------------------------------------------------
      DO i=0,Mxmalg
       sum=Fulma(i)
       qmi=Mxmalg-i
c     ------------------------------------------------------------------
       DO k=1,qmi
        sum=sum+Fulma(k)*C(i+k)
       END DO
c     ------------------------------------------------------------------
       C(i)=sum
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
