**==devlpl.f    processed by SPAG 4.03F  at 14:31 on 28 Jul 1994
      DOUBLE PRECISION FUNCTION devlpl(A,N,X)
      IMPLICIT NONE
C**********************************************************************
C
C     DOUBLE PRECISION FUNCTION DEVLPL(A,N,X)
C              Double precision EVALuate a PoLynomial at X
C
C
C                              Function
C
C
C     returns
C          A(1) + A(2)*X + ... + A(N)*X**(N-1)
C
C
C                              Arguments
C
C
C     A --> Array of coefficients of the polynomial.
C                                        A is DOUBLE PRECISION(N)
C
C     N --> Length of A, also degree of polynomial - 1.
C                                        N is INTEGER
C
C     X --> Point at which the polynomial is to be evaluated.
C                                        X is DOUBLE PRECISION
C
C**********************************************************************
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
      INTEGER N
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION A(N)
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION term
      INTEGER i
C     ..
C     .. Executable Statements ..
      term=A(N)
      DO i=N-1,1,-1
       term=A(i)+term*X
      END DO
      devlpl=term
      RETURN
 
      END
