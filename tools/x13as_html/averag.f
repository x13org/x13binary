**==averag.f    processed by SPAG 4.03F  at 09:47 on  1 Mar 1994
      SUBROUTINE averag(X,Y,Ib,Ie,M,N)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      DOUBLE PRECISION fmn,tmp,X,Y
      INTEGER i,i1,i2,Ib,Ie,j,ji,k,kb,ke,ki,M,N
C*** End of declarations inserted by SPAG
C
C --- THIS SUBROUTINE APPLIES AN M-OF-N MOVING AVERAGE TO THE SERIES
C ---  X AND STORES THE RESULTS IN Y.
C
      DIMENSION X(Ie),Y(Ie)
      ki=(M+N)/2-1
      kb=Ib+ki
      ke=Ie-ki
      IF(ke.ge.kb)THEN
       fmn=dble(M*N)
       DO k=kb,ke
        tmp=0D0
        i1=k-ki
        i2=i1+M-1
        DO i=i1,i2
         ji=i+N-1
         DO j=i,ji
          tmp=tmp+X(j)
         END DO
        END DO
        Y(k)=tmp/fmn
       END DO
      END IF
      RETURN
      END
