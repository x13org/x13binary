**==dppfa.f    processed by SPAG 4.03F  at 09:48 on  1 Mar 1994
      SUBROUTINE dppfa(Ap,N,Info)
      IMPLICIT NONE
      INTEGER N,Info
      DOUBLE PRECISION dpmpar,mprec,Ap(*)
      EXTERNAL dpmpar
C
C     DPPFA FACTORS A DOUBLE PRECISION SYMMETRIC POSITIVE DEFINITE
C     MATRIX STORED IN PACKED FORM.
C
C     DPPFA IS USUALLY CALLED BY DPPCO, BUT IT CAN BE CALLED
C     DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.
C     (TIME FOR DPPCO) = (1 + 18/N)*(TIME FOR DPPFA) .
C
C     ON ENTRY
C
C        AP      DOUBLE PRECISION (N*(N+1)/2)
C                THE PACKED FORM OF A SYMMETRIC MATRIX  A .  THE
C                COLUMNS OF THE UPPER TRIANGLE ARE STORED SEQUENTIALLY
C                IN A ONE-DIMENSIONAL ARRAY OF LENGTH  N*(N+1)/2 .
C                SEE COMMENTS BELOW FOR DETAILS.
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C     ON RETURN
C
C        AP      AN UPPER TRIANGULAR MATRIX  R , STORED IN PACKED
C                FORM, SO THAT  A = TRANS(R)*R .
C
C        INFO    INTEGER
C                = 0  FOR NORMAL RETURN.
C                = K  IF THE LEADING MINOR OF ORDER  K  IS NOT
C                     POSITIVE DEFINITE.
C
C
C     PACKED STORAGE
C
C          THE FOLLOWING PROGRAM SEGMENT WILL PACK THE UPPER
C          TRIANGLE OF A SYMMETRIC MATRIX.
C
C                K = 0
C                DO 20 J = 1, N
C                   DO 10 I = 1, J
C                      K = K + 1
C                      AP(K) = A(I,J)
C             10    CONTINUE
C             20 CONTINUE
C
C     LINPACK.  THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
C
C     SUBROUTINES AND FUNCTIONS
C
C     BLAS DDOT
C     FORTRAN DSQRT
C
C     INTERNAL VARIABLES
C
      DOUBLE PRECISION ddot,t
      DOUBLE PRECISION s
      INTEGER j,jj,jm1,k,kj,kk
C     BEGIN BLOCK WITH ...EXITS TO 40
C
C
      mprec=dpmpar(1)
      jj=0
      DO j=1,N
       Info=j
       s=0.0D0
       jm1=j-1
       kj=jj
       kk=0
       IF(jm1.ge.1)THEN
        DO k=1,jm1
         kj=kj+1
         t=Ap(kj)-ddot(k-1,Ap(kk+1),1,Ap(jj+1),1)
         kk=kk+k
         t=t/Ap(kk)
         Ap(kj)=t
         s=s+t*t
        END DO
       END IF
       jj=jj+j
       s=Ap(jj)-s
C     ......EXIT
       IF(s.gt.0.0D0)THEN
        Ap(jj)=dsqrt(s)
       ELSE
        IF(s.ge.-mprec)Ap(jj)=0D0
c       ELSE
        GO TO 10
       END IF
      END DO
      Info=0
   10 RETURN
      END
