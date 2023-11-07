      SUBROUTINE xpand(B,Mxarlg,Na,Nc,C,Pc)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     FORTRAN corrections made by Bill Bell -- 9/10/92
c 1. Integer declaration statement put before double precision declaration
c    statement
c 2. Variable name "O" changed to "m" for clarity
c 3. REAL B(P) changed to double precision B(max(P,1)) to handle case
c    where P = 0
c 4. REAL A(m:Na) changed to double precision A(m:max(Na,1))
c    to handle case where Na = 0
c Changes made:  9/21/92,  Bill Bell
c 1. IMPLICIT NONE   statement added
c 2. REAL type statements changed to DOUBLE PRECISION
c Changes made:  8/30/2005, REG
c 1. Changed dimension on arrays a(0:porder) and c(0:porder) to (0:pobs)
c    and commented out include file 'model.prm' that is no longer needed
c Changes made:  9/15/2005, REG
c 1. Added input argument Pc. Changed dimension on arrays a(0:pobs) and
c    c(0:pobs) to (0:Pc) and commented out include file 'srslen.prm'
c   that is no longer needed
c-----------------------------------------------------------------------
c     INCLUDE 'srslen.prm'
c     INCLUDE 'model.prm'
      DOUBLE PRECISION ZERO
      PARAMETER(ZERO=0D0)
c     ------------------------------------------------------------------
      INTEGER i,Mxarlg,Na,Nc,nlag,Pc,w
      DOUBLE PRECISION a(0:Pc),B(0:Mxarlg),C(0:Pc),sum
c-----------------------------------------------------------------------
c     Calculates the expansion of A(Z)/B(Z) = C(Z) up to order nc.
c-----------------------------------------------------------------------
*      CALL copy(C,Na+1,1,a)
      DO i=0,Na
       a(i)=C(i)
      END DO
c     ------------------------------------------------------------------
      DO i=0,Nc
       nlag=min(Mxarlg,i)
c     ------------------------------------------------------------------
       IF(i.le.Na)THEN
        sum=a(i)
       ELSE
        sum=ZERO
       END IF
c     ------------------------------------------------------------------
       DO w=1,nlag
        sum=sum-B(w)*C(i-w)
       END DO
c     ------------------------------------------------------------------
       C(i)=sum
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
