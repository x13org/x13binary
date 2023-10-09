**==crosco.f    processed by SPAG 4.03F  at 09:47 on  1 Mar 1994
      SUBROUTINE crosco(X,Y,N1,N2,N,C,Lagh1)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      DOUBLE PRECISION t,an,bn,bn1,C,ct0,X,Y
      INTEGER i,ii,il,j,j1,Lagh1,N,N1,N2
C*** End of declarations inserted by SPAG
C     COMMON SUBROUTINE
C     THIS SUBROUTINE COMPUTES C(L)=COVARIANCE(X(S+L),Y(S))
C     (L=0,1,...,LAGH1-1).
      DIMENSION X(*),Y(*),C(*)
      an=dble(N)
      bn1=1.0D-00
      bn=bn1/an
      ct0=0.0D-00
      DO ii=1,Lagh1
       i=ii-1
       t=ct0
       il=N2-i
       DO j=N1,il
        j1=j+i
        t=t+X(j1)*Y(j)
       END DO
       C(ii)=t*bn
      END DO
      RETURN
      END
