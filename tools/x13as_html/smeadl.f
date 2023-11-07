**==smeadl.f    processed by SPAG 4.03F  at 09:53 on  1 Mar 1994
      SUBROUTINE smeadl(X,N1,N2,N,Xmean)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      DOUBLE PRECISION an,sumf,X,Xmean
      INTEGER i,N,N1,N2
C*** End of declarations inserted by SPAG
C     MEAN DELETION
C     SUMF: FUNCTION
      DIMENSION X(*)
      an=dble(N)
      Xmean=sumf(X,N1,N2)/an
      DO i=N1,N2
       X(i)=X(i)-Xmean
      END DO
      RETURN
      END
