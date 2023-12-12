**==apply.f    processed by SPAG 4.03F  at 09:46 on  1 Mar 1994
      DOUBLE PRECISION FUNCTION apply(X,K,W,N)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      DOUBLE PRECISION W,X
      INTEGER i,j,K,l,m,N
C*** End of declarations inserted by SPAG
C
C     THIS FUNCTION APPLIES SYMMETRIC WEIGHTS TO X(K)
C
      DIMENSION X(*),W(*)
      m=(N+1)/2
      apply=W(1)*X(K)
      DO i=2,m
       j=K-i+1
       l=K+i-1
       apply=apply+W(i)*(X(j)+X(l))
      END DO
      RETURN
      END

