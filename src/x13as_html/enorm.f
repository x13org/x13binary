C     Last change:  BCM  29 Sep 97    9:42 am
**==enorm.f    processed by SPAG 4.03F  at 09:48 on  1 Mar 1994
      DOUBLE PRECISION FUNCTION enorm(N,X)
      IMPLICIT NONE
      INTEGER N
      DOUBLE PRECISION X(N)
C     **********
C
C     FUNCTION ENORM
C
C     GIVEN AN N-VECTOR X, THIS FUNCTION CALCULATES THE
C     EUCLIDEAN NORM OF X.
C
C     THE EUCLIDEAN NORM IS COMPUTED BY ACCUMULATING THE SUM OF
C     SQUARES IN THREE DIFFERENT SUMS. THE SUMS OF SQUARES FOR THE
C     SMALL AND LARGE COMPONENTS ARE SCALED SO THAT NO OVERFLOWS
C     OCCUR. NON-DESTRUCTIVE UNDERFLOWS ARE PERMITTED. UNDERFLOWS
C     AND OVERFLOWS DO NOT OCCUR IN THE COMPUTATION OF THE UNSCALED
C     SUM OF SQUARES FOR THE INTERMEDIATE COMPONENTS.
C     THE DEFINITIONS OF SMALL, INTERMEDIATE AND LARGE COMPONENTS
C     DEPEND ON TWO CONSTANTS, RDWARF AND RGIANT. THE MAIN
C     RESTRICTIONS ON THESE CONSTANTS ARE THAT RDWARF**2 NOT
C     UNDERFLOW AND RGIANT**2 NOT OVERFLOW. THE CONSTANTS
C     GIVEN HERE ARE SUITABLE FOR EVERY KNOWN COMPUTER.
C
C     THE FUNCTION STATEMENT IS
C
C       DOUBLE PRECISION FUNCTION ENORM(N,X)
C
C     WHERE
C
C       N IS A POSITIVE INTEGER INPUT VARIABLE.
C
C       X IS AN INPUT ARRAY OF LENGTH N.
C
C     SUBPROGRAMS CALLED
C
C       FORTRAN-SUPPLIED ... DABS,DSQRT
C
C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. MARCH 1980.
C     BURTON S. GARBOW, KENNETH E. HILLSTROM, JORGE J. MORE
C
C     **********
c-----------------------------------------------------------------------
c     Made the data statements parameters
c-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO,ONE,RDWARF,RGIANT
      PARAMETER(ZERO=0.0D0,ONE=1.0D0,RDWARF=3.834D-20,RGIANT=1.304D19)
      DOUBLE PRECISION agiant,floatn,s1,s2,s3,xabs,x1max,x3max
      INTEGER i
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
c      DOUBLE PRECISION AGIANT,FLOATN,ONE,RDWARF,RGIANT,S1,S2,S3,XABS,
c     *                 X1MAX,X3MAX,ZERO
c      DATA ONE,ZERO,RDWARF,RGIANT /1.0D0,0.0D0,3.834D-20,1.304D19/
      s1=ZERO
      s2=ZERO
      s3=ZERO
      x1max=ZERO
      x3max=ZERO
      floatn=N
      agiant=RGIANT/floatn
      DO i=1,N
       xabs=dabs(X(i))
       IF(xabs.gt.RDWARF.and.xabs.lt.agiant)THEN
C
C           SUM FOR INTERMEDIATE COMPONENTS.
C
        s2=s2+xabs**2
       ELSE IF(xabs.le.RDWARF)THEN
C
C              SUM FOR SMALL COMPONENTS.
C
        IF(xabs.le.x3max)THEN
         IF(.not.dpeq(xabs,ZERO))s3=s3+(xabs/x3max)**2
        ELSE
         s3=ONE+s3*(x3max/xabs)**2
         x3max=xabs
        END IF
C
C              SUM FOR LARGE COMPONENTS.
C
       ELSE IF(xabs.le.x1max)THEN
        s1=s1+(xabs/x1max)**2
       ELSE
        s1=ONE+s1*(x1max/xabs)**2
        x1max=xabs
       END IF
      END DO
C
C     CALCULATION OF NORM.
C
      IF(.not.dpeq(s1,ZERO))THEN
       enorm=x1max*dsqrt(s1+(s2/x1max)/x1max)
      ELSE IF(dpeq(s2,ZERO).and.s3.gt.ZERO)THEN
       enorm=x3max*dsqrt(s3)
      ELSE
       IF(dpeq(x3max,ZERO).and.dpeq(dsqrt(s2),ZERO))THEN
        enorm=ZERO
       ELSE
        IF(s2.ge.x3max)enorm=dsqrt(s2*(ONE+(x3max/s2)*(x3max*s3)))
        IF(s2.lt.x3max)enorm=dsqrt(x3max*((s2/x3max)+(x3max*s3)))
       END IF
      END IF
      RETURN
C
C     LAST CARD OF FUNCTION ENORM.
C
      END
