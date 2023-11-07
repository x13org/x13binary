**==chisq.f    processed by SPAG 4.03F  at 09:47 on  1 Mar 1994
      DOUBLE PRECISION FUNCTION chisq(X,N)
c-----------------------------------------------------------------------
c     This function calculates chi-squared probability levels for
c pr( chi-sq. r.v. with n degrees of freedom > x ).
c-----------------------------------------------------------------------
c     This function/subroutine was developed by Statistics Canada.
c     We thank them for permission to use it here.
c-----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION C,c1,c2,c3,y,X,gauss
      INTEGER N,i,m
      EXTERNAL gauss
      PARAMETER(C=0.797884560802864D0)
c-----------------------------------------------------------------------
      IF(X.le.0D0)THEN
       chisq=1D0
       RETURN
c-----------------------------------------------------------------------
      ELSE IF(X.lt.90D0)THEN
       c1=1.0D0
       c2=c1
       c3=0.0D0
       y=dble(X)
       m=N/2
       i=m*2-N
c-----------------------------------------------------------------------
       IF(i.eq.0)THEN
        y=y/2.0D0
c-----------------------------------------------------------------------
        IF(m.ne.1)THEN
         m=m-1
         DO i=1,m
          c2=c2*y/i
          c1=c1+c2
         END DO
        END IF
c-----------------------------------------------------------------------
       ELSE
        IF(m.ne.0)THEN
         DO i=1,m
          c1=c1*y/c2
          c3=c3+c1
          c2=c2+2.0D0
         END DO
        END IF
c-----------------------------------------------------------------------
        c2=dsqrt(y)
        chisq=1.0D0-gauss(c2)+C*c3*dexp(-y/2.0D0)/c2
        RETURN
       END IF
c-----------------------------------------------------------------------
      ELSE
       chisq=0.0D0
       RETURN
      END IF
      chisq=c1*dexp(-y)
c-----------------------------------------------------------------------
      RETURN
      END
