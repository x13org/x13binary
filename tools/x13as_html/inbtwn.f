**==inbtwn.f    processed by SPAG 4.03F  at 09:50 on  1 Mar 1994
      SUBROUTINE inbtwn(Tcoef,Lagt,In,Ncoef,Coef,Lag)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Puts the current coefficent, t, between the in and in+1 coefficients
c by moving everything above in over (up) one place.
c-----------------------------------------------------------------------
c Name  Type Description
c-----------------------------------------------------------------------
c after   i  Local index for the coeffiecents and lags to be moved in+1
c coef    r  In/out ncoef long vector of polymonial coefficients
c i       i  Local do loop index
c in      i  Input scalar to indicate that the current coefficient
c             t will go just after the inth coefficient
c lag     i  In/out ncoef long vector of lag corresponding to the
c             coefficients
c lagt    i  Input scalar for the current lag
c ncoef   i  In/out scalar for the number of coefficients
c tcoef       r  Input scalar for the current coefficient value
c-----------------------------------------------------------------------
      INTEGER i,In,after,Lagt,Lag(*),Ncoef
      DOUBLE PRECISION Tcoef,Coef(*)
c-----------------------------------------------------------------------
c     Move over the coefficients of the higher lags
c-----------------------------------------------------------------------
      after=In+1
      DO i=Ncoef,after,-1
       Coef(i+1)=Coef(i)
       Lag(i+1)=Lag(i)
      END DO
c-----------------------------------------------------------------------
c     Add the current coefficient value and update the number of coefficient
c counter
c-----------------------------------------------------------------------
      Coef(after)=Tcoef
      Lag(after)=Lagt
      Ncoef=Ncoef+1
c     ------------------------------------------------------------------
      RETURN
      END
