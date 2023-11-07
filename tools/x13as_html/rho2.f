**==rho2.f    processed by SPAG 4.03F  at 16:22 on 30 Mar 1994
c-----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION rho2(U)
      IMPLICIT NONE
      DOUBLE PRECISION U,u2
c-----------------------------------------------------------------------
      IF(abs(U).gt.2.798D0)THEN
       rho2=6.502D0
      ELSE
       u2=U*U
       rho2=(0.9249D0*u2)+(0.0812D0*u2*u2)-(0.0119D0*u2*u2*u2)
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
