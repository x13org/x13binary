C     Last change:  BCM  29 Sep 97   10:29 am
**==lmpar.f    processed by SPAG 4.03F  at 09:51 on  1 Mar 1994
      SUBROUTINE lmpar(N,R,Ldr,Ipvt,Diag,Qtb,Delta,Par,X,Sdiag,Wa1,Wa2)
      IMPLICIT NONE
      INTEGER N,Ldr
      INTEGER Ipvt(N)
      DOUBLE PRECISION Delta,Par
      DOUBLE PRECISION R(Ldr,N),Diag(N),Qtb(N),X(N),Sdiag(N),Wa1(N),
     &                 Wa2(N)
C     **********
C
C     SUBROUTINE LMPAR
C
C     GIVEN AN M BY N MATRIX A, AN N BY N NONSINGULAR DIAGONAL
C     MATRIX D, AN M-VECTOR B, AND A POSITIVE NUMBER DELTA,
C     THE PROBLEM IS TO DETERMINE A VALUE FOR THE PARAMETER
C     PAR SUCH THAT IF X SOLVES THE SYSTEM
C
C           A*X = B ,     SQRT(PAR)*D*X = 0 ,
C
C     IN THE LEAST SQUARES SENSE, AND DXNORM IS THE EUCLIDEAN
C     NORM OF D*X, THEN EITHER PAR IS ZERO AND
C
C           (DXNORM-DELTA) .LE. 0.1*DELTA ,
C
C     OR PAR IS POSITIVE AND
C
C           ABS(DXNORM-DELTA) .LE. 0.1*DELTA .
C
C     THIS SUBROUTINE COMPLETES THE SOLUTION OF THE PROBLEM
C     IF IT IS PROVIDED WITH THE NECESSARY INFORMATION FROM THE
C     QR FACTORIZATION, WITH COLUMN PIVOTING, OF A. THAT IS, IF
C     A*P = Q*R, WHERE P IS A PERMUTATION MATRIX, Q HAS ORTHOGONAL
C     COLUMNS, AND R IS AN UPPER TRIANGULAR MATRIX WITH DIAGONAL
C     ELEMENTS OF NONINCREASING MAGNITUDE, THEN LMPAR EXPECTS
C     THE FULL UPPER TRIANGLE OF R, THE PERMUTATION MATRIX P,
C     AND THE FIRST N COMPONENTS OF (Q TRANSPOSE)*B. ON OUTPUT
C     LMPAR ALSO PROVIDES AN UPPER TRIANGULAR MATRIX S SUCH THAT
C
C            T   T                   T
C           P *(A *A + PAR*D*D)*P = S *S .
C
C     S IS EMPLOYED WITHIN LMPAR AND MAY BE OF SEPARATE INTEREST.
C
C     ONLY A FEW ITERATIONS ARE GENERALLY NEEDED FOR CONVERGENCE
C     OF THE ALGORITHM. IF, HOWEVER, THE LIMIT OF 10 ITERATIONS
C     IS REACHED, THEN THE OUTPUT PAR WILL CONTAIN THE BEST
C     VALUE OBTAINED SO FAR.
C
C     THE SUBROUTINE STATEMENT IS
C
C       SUBROUTINE LMPAR(N,R,LDR,IPVT,DIAG,QTB,DELTA,PAR,X,SDIAG,
C                        WA1,WA2)
C
C     WHERE
C
C       N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE ORDER OF R.
C
C       R IS AN N BY N ARRAY. ON INPUT THE FULL UPPER TRIANGLE
C         MUST CONTAIN THE FULL UPPER TRIANGLE OF THE MATRIX R.
C         ON OUTPUT THE FULL UPPER TRIANGLE IS UNALTERED, AND THE
C         STRICT LOWER TRIANGLE CONTAINS THE STRICT UPPER TRIANGLE
C         (TRANSPOSED) OF THE UPPER TRIANGULAR MATRIX S.
C
C       LDR IS A POSITIVE INTEGER INPUT VARIABLE NOT LESS THAN N
C         WHICH SPECIFIES THE LEADING DIMENSION OF THE ARRAY R.
C
C       IPVT IS AN INTEGER INPUT ARRAY OF LENGTH N WHICH DEFINES THE
C         PERMUTATION MATRIX P SUCH THAT A*P = Q*R. COLUMN J OF P
C         IS COLUMN IPVT(J) OF THE IDENTITY MATRIX.
C
C       DIAG IS AN INPUT ARRAY OF LENGTH N WHICH MUST CONTAIN THE
C         DIAGONAL ELEMENTS OF THE MATRIX D.
C
C       QTB IS AN INPUT ARRAY OF LENGTH N WHICH MUST CONTAIN THE FIRST
C         N ELEMENTS OF THE VECTOR (Q TRANSPOSE)*B.
C
C       DELTA IS A POSITIVE INPUT VARIABLE WHICH SPECIFIES AN UPPER
C         BOUND ON THE EUCLIDEAN NORM OF D*X.
C
C       PAR IS A NONNEGATIVE VARIABLE. ON INPUT PAR CONTAINS AN
C         INITIAL ESTIMATE OF THE LEVENBERG-MARQUARDT PARAMETER.
C         ON OUTPUT PAR CONTAINS THE FINAL ESTIMATE.
C
C       X IS AN OUTPUT ARRAY OF LENGTH N WHICH CONTAINS THE LEAST
C         SQUARES SOLUTION OF THE SYSTEM A*X = B, SQRT(PAR)*D*X = 0,
C         FOR THE OUTPUT PAR.
C
C       SDIAG IS AN OUTPUT ARRAY OF LENGTH N WHICH CONTAINS THE
C         DIAGONAL ELEMENTS OF THE UPPER TRIANGULAR MATRIX S.
C
C       WA1 AND WA2 ARE WORK ARRAYS OF LENGTH N.
C
C     SUBPROGRAMS CALLED
C
C       MINPACK-SUPPLIED ... DPMPAR,ENORM,QRSOLV
C
C       FORTRAN-SUPPLIED ... DABS,DMAX1,DMIN1,DSQRT
C
C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. MARCH 1980.
C     BURTON S. GARBOW, KENNETH E. HILLSTROM, JORGE J. MORE
C
C     **********
      LOGICAL dpeq
      INTEGER i,iter,j,jm1,jp1,k,l,nsing
      DOUBLE PRECISION dxnorm,dwarf,fp,gnorm,parc,parl,paru,p1,p001,sum,
     &                 temp,zero
      DOUBLE PRECISION dpmpar,enorm
      DATA p1,p001,zero/1.0D-1,1.0D-3,0.0D0/
      EXTERNAL dpeq
C
C     DWARF IS THE SMALLEST POSITIVE MAGNITUDE.
C
      dwarf=dpmpar(2)
C
C     COMPUTE AND STORE IN X THE GAUSS-NEWTON DIRECTION. IF THE
C     JACOBIAN IS RANK-DEFICIENT, OBTAIN A LEAST SQUARES SOLUTION.
C
      nsing=N
      DO j=1,N
       Wa1(j)=Qtb(j)
       IF(dpeq(R(j,j),zero).and.nsing.eq.N)nsing=j-1
       IF(nsing.lt.N)Wa1(j)=zero
      END DO
      IF(nsing.ge.1)THEN
       DO k=1,nsing
        j=nsing-k+1
        Wa1(j)=Wa1(j)/R(j,j)
        temp=Wa1(j)
        jm1=j-1
        IF(jm1.ge.1)THEN
         DO i=1,jm1
          Wa1(i)=Wa1(i)-R(i,j)*temp
         END DO
        END IF
       END DO
      END IF
      DO j=1,N
       l=Ipvt(j)
       X(l)=Wa1(j)
      END DO
C
C     INITIALIZE THE ITERATION COUNTER.
C     EVALUATE THE FUNCTION AT THE ORIGIN, AND TEST
C     FOR ACCEPTANCE OF THE GAUSS-NEWTON DIRECTION.
C
      iter=0
      DO j=1,N
       Wa2(j)=Diag(j)*X(j)
      END DO
      dxnorm=enorm(N,Wa2)
      fp=dxnorm-Delta
      IF(fp.gt.p1*Delta)THEN
C
C     IF THE JACOBIAN IS NOT RANK DEFICIENT, THE NEWTON
C     STEP PROVIDES A LOWER BOUND, PARL, FOR THE ZERO OF
C     THE FUNCTION. OTHERWISE SET THIS BOUND TO ZERO.
C
       parl=zero
       IF(nsing.ge.N)THEN
        DO j=1,N
         l=Ipvt(j)
         Wa1(j)=Diag(l)*(Wa2(l)/dxnorm)
        END DO
        DO j=1,N
         sum=zero
         jm1=j-1
         IF(jm1.ge.1)THEN
          DO i=1,jm1
           sum=sum+R(i,j)*Wa1(i)
          END DO
         END IF
         Wa1(j)=(Wa1(j)-sum)/R(j,j)
        END DO
        temp=enorm(N,Wa1)
        parl=((fp/Delta)/temp)/temp
       END IF
C
C     CALCULATE AN UPPER BOUND, PARU, FOR THE ZERO OF THE FUNCTION.
C
       DO j=1,N
        sum=zero
        DO i=1,j
         sum=sum+R(i,j)*Qtb(i)
        END DO
        l=Ipvt(j)
        Wa1(j)=sum/Diag(l)
       END DO
       gnorm=enorm(N,Wa1)
       paru=gnorm/Delta
       IF(dpeq(paru,zero))paru=dwarf/dmin1(Delta,p1)
C
C     IF THE INPUT PAR LIES OUTSIDE OF THE INTERVAL (PARL,PARU),
C     SET PAR TO THE CLOSER ENDPOINT.
C
       Par=dmax1(Par,parl)
       Par=dmin1(Par,paru)
       IF(dpeq(Par,zero))Par=gnorm/dxnorm
       DO WHILE (.true.)
C
C     BEGINNING OF AN ITERATION.
C
        iter=iter+1
C
C        EVALUATE THE FUNCTION AT THE CURRENT VALUE OF PAR.
C
        IF(dpeq(Par,zero))Par=dmax1(dwarf,p001*paru)
        temp=dsqrt(Par)
        DO j=1,N
         Wa1(j)=temp*Diag(j)
        END DO
        CALL qrsolv(N,R,Ldr,Ipvt,Wa1,Qtb,X,Sdiag,Wa2)
        DO j=1,N
         Wa2(j)=Diag(j)*X(j)
        END DO
        dxnorm=enorm(N,Wa2)
        temp=fp
        fp=dxnorm-Delta
C
C        IF THE FUNCTION IS SMALL ENOUGH, ACCEPT THE CURRENT VALUE
C        OF PAR. ALSO TEST FOR THE EXCEPTIONAL CASES WHERE PARL
C        IS ZERO OR THE NUMBER OF ITERATIONS HAS REACHED 10.
C
        IF(dabs(fp).le.p1*Delta.or.dpeq(parl,zero).and.fp.le.temp.and.
     &     temp.lt.zero.or.iter.eq.10)GO TO 10
C
C        COMPUTE THE NEWTON CORRECTION.
C
        DO j=1,N
         l=Ipvt(j)
         Wa1(j)=Diag(l)*(Wa2(l)/dxnorm)
        END DO
        DO j=1,N
         Wa1(j)=Wa1(j)/Sdiag(j)
         temp=Wa1(j)
         jp1=j+1
         IF(N.ge.jp1)THEN
          DO i=jp1,N
           Wa1(i)=Wa1(i)-R(i,j)*temp
          END DO
         END IF
        END DO
        temp=enorm(N,Wa1)
        parc=((fp/Delta)/temp)/temp
C
C        DEPENDING ON THE SIGN OF THE FUNCTION, UPDATE PARL OR PARU.
C
        IF(fp.gt.zero)parl=dmax1(parl,Par)
        IF(fp.lt.zero)paru=dmin1(paru,Par)
C
C        COMPUTE AN IMPROVED ESTIMATE FOR PAR.
C
C
C        END OF AN ITERATION.
C
        Par=dmax1(parl,Par+parc)
       END DO
      END IF
C
C     TERMINATION.
C
   10 IF(iter.eq.0)Par=zero
      RETURN
C
C     LAST CARD OF SUBROUTINE LMPAR.
C
      END
