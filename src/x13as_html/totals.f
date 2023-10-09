C     Last change:  BCM  26 Aug 1998    4:08 pm
**==totals.f    processed by SPAG 4.03F  at 09:54 on  1 Mar 1994
      DOUBLE PRECISION FUNCTION totals(X,I,J,K,Iopt)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C --- THIS FUNCTION CALCULATES TOTALS AND AVERAGES. A TOTAL IS RETURNED
C --- IF IOPT IS ZERO, AN AVERAGE IF IOPT IS ONE, and the absolute 
c     average if IOPT is TWO.
c-----------------------------------------------------------------------
c     revised by BCM March 2006 to handle cases where "bad" values for
c     multiplicative seasonal adjustment are found and missing values
c     in the series, and added an additonal value of IOPT to return the
c     number of "good" observations (IOPT = 3)
c-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO,ONE
      PARAMETER(ZERO=0D0,ONE=1D0)
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
c-----------------------------------------------------------------------
      DOUBLE PRECISION fn,X
      INTEGER I,Iopt,J,K,l
      DIMENSION X(*)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
      totals=ZERO
      fn=ZERO
      DO l=I,J,K
*       IF((.not.(Missng.and.X(l).eq.Mvval).and.Gudval(l))THEN
       IF(.not.dpeq(X(l),DNOTST))THEN
        IF(Iopt.eq.2)THEN
         totals=totals+abs(X(l))
        ELSE IF(Iopt.lt.3)THEN
         totals=totals+X(l)
        END IF
        IF(Iopt.ne.0)fn=fn+ONE
       END IF
      END DO
c-----------------------------------------------------------------------
      IF(Iopt.eq.3)THEN
       totals=fn
      ELSE IF(Iopt.ne.0)THEN
       IF(fn.gt.ZERO)THEN
        totals=totals/fn
       ELSE
        totals=DNOTST
       END IF
      END IF
      RETURN
      END
