**==taper.f    processed by SPAG 4.03F  at 09:54 on  1 Mar 1994
      SUBROUTINE taper(X,L1,L2,R)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      DOUBLE PRECISION R,r1,r2,tap,X,xpi,xtap
      INTEGER i,l,L1,L2
C*** End of declarations inserted by SPAG
      DIMENSION X(*)
      DOUBLE PRECISION PI,ONE,TWO
      PARAMETER(PI=3.14159265358979d0,ONE=1D0,TWO=2D0)
C  ***
C  ***  THIS SUBROUTINE APPLIES THE TUKEY-HANNING TAPER TO A SERIES
C  ***  PRIOR TO CALCULATING THE SPECTRUM.
C  ***
      l=L2-L1+1
      r1=R/2
      r2=1-r1
      DO i=L1,L2
       xtap=(dble(i-L1)+0.5D0)/(dble(l))
       IF(xtap.ge.r1.and.xtap.le.r2)THEN
        tap=ONE
       ELSE
        IF(xtap.lt.r1)xpi=(TWO*PI*xtap)/R
        IF(xtap.gt.r2)xpi=(TWO*PI*(1-xtap))/R
        tap=(ONE-cos(xpi))/TWO
       END IF
       X(i)=X(i)*tap
      END DO
      RETURN
      END
