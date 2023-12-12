**==sdev.f    processed by SPAG 4.03F  at 09:53 on  1 Mar 1994
      DOUBLE PRECISION FUNCTION sdev(X,I,J,K,Iopt)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C --- THIS FUNCTION CALCULATES THE STANDARD DEVIATION OF X. IF IOPT = 0
C --- THE MEAN OF X IS COMPUTED, IF IOPT = 1 THE MEAN IS ASSUMED TO BE
C --- ZERO, AND IF IOPT = 2 THE MEAN IS ASSUMED TO BE ONE.
c-----------------------------------------------------------------------
c     revised by BCM March 2006 to handle cases where "bad" values for
c     multiplicative seasonal adjustment are found 
c-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO,ONE
      PARAMETER(ZERO=0D0,ONE=1D0)
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
c-----------------------------------------------------------------------
      DOUBLE PRECISION ave,fn,X,totals
      INTEGER I,Iopt,J,K,l
      DIMENSION X(*)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
      fn=ZERO
      IF(Iopt.lt.1)THEN
       ave=totals(X,I,J,K,1)
      ELSE IF(Iopt.eq.1)THEN
       ave=ZERO
      ELSE 
       ave=ONE
      END IF
      sdev=ZERO
      DO l=I,J,K
*       IF((.not.(Missng.and.X(l).eq.Mvval).and.Gudval(l))THEN
       IF(.not.dpeq(X(l),DNOTST))THEN
        sdev=sdev+(X(l)-ave)*(X(l)-ave)
        fn=fn+ONE
       END IF
      END DO
      IF(fn.gt.ZERO)THEN
       sdev=sqrt(sdev/fn)
      ELSE
       sdev=DNOTST
      END IF
      RETURN
      END
