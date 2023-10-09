**==insort.f    processed by SPAG 4.03F  at 09:50 on  1 Mar 1994
      SUBROUTINE insort(Tcoef,Lagt,Ncoef,Coef,Lag)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Puts the coefficient in order of the power of its lag by checking
c backwards through the orders lag powers.
c-----------------------------------------------------------------------
c Name Type Description
c-----------------------------------------------------------------------
c coef   r  In/out ncoef long vector of polymonial coefficients
c i      i  Local do loop index
c lag    i  In/out ncoef long vector of lag corresponding to the
c            coefficients
c lagt   i  Input scalar for the current lag
c ncoef  i  In/out scalar for the number of coefficients
c tcoef  r  Input scalar for the current coefficient value
c-----------------------------------------------------------------------
      INTEGER i,Lagt,Lag(*),Ncoef
      DOUBLE PRECISION Tcoef,Coef(*)
c-----------------------------------------------------------------------
c     Go back to a lag the is equal to or smaller than the current lag
c-----------------------------------------------------------------------
      DO i=Ncoef,1,-1
c-----------------------------------------------------------------------
c     Found a lag less than the current lag so insert the new coeffient
c in a space between k and k+1 by moving the higher powered coefficients
c over.
c-----------------------------------------------------------------------
       IF(Lagt.gt.Lag(i))THEN
        CALL inbtwn(Tcoef,Lagt,i,Ncoef,Coef,Lag)
        GO TO 10
c-----------------------------------------------------------------------
c     Found a lag equal to the current lag so add the coefficients.
c-----------------------------------------------------------------------
       ELSE IF(Lagt.eq.Lag(i))THEN
        Coef(i)=Coef(i)+Tcoef
        GO TO 10
       END IF
      END DO
c-----------------------------------------------------------------------
c     The current lag is the smallest so place the new coefficient at
c the begining.
c-----------------------------------------------------------------------
      CALL inbtwn(Tcoef,Lagt,0,Ncoef,Coef,Lag)
c     ------------------------------------------------------------------
   10 RETURN
      END
