C     Last change:  BCM  21 Nov 97   10:24 pm
**==qrsolv.f    processed by SPAG 4.03F  at 09:52 on  1 Mar 1994
      SUBROUTINE qrsolv(N,R,Ldr,Ipvt,Diag,Qtb,X,Sdiag,Wa)
      IMPLICIT NONE
      INTEGER N,Ldr
      INTEGER Ipvt(N)
      DOUBLE PRECISION R(Ldr,N),Diag(N),Qtb(N),X(N),Sdiag(N),Wa(N)
C     **********
C
C     SUBROUTINE QRSOLV
C
C     GIVEN AN M BY N MATRIX A, AN N BY N DIAGONAL MATRIX D,
C     AND AN M-VECTOR B, THE PROBLEM IS TO DETERMINE AN X WHICH
C     SOLVES THE SYSTEM
C
C           A*X = B ,     D*X = 0 ,
C
C     IN THE LEAST SQUARES SENSE.
C
C     THIS SUBROUTINE COMPLETES THE SOLUTION OF THE PROBLEM
C     IF IT IS PROVIDED WITH THE NECESSARY INFORMATION FROM THE
C     QR FACTORIZATION, WITH COLUMN PIVOTING, OF A. THAT IS, IF
C     A*P = Q*R, WHERE P IS A PERMUTATION MATRIX, Q HAS ORTHOGONAL
C     COLUMNS, AND R IS AN UPPER TRIANGULAR MATRIX WITH DIAGONAL
C     ELEMENTS OF NONINCREASING MAGNITUDE, THEN QRSOLV EXPECTS
C     THE FULL UPPER TRIANGLE OF R, THE PERMUTATION MATRIX P,
C     AND THE FIRST N COMPONENTS OF (Q TRANSPOSE)*B. THE SYSTEM
C     A*X = B, D*X = 0, IS THEN EQUIVALENT TO
C
C                  T       T
C           R*Z = Q *B ,  P *D*P*Z = 0 ,
C
C     WHERE X = P*Z. IF THIS SYSTEM DOES NOT HAVE FULL RANK,
C     THEN A LEAST SQUARES SOLUTION IS OBTAINED. ON OUTPUT QRSOLV
C     ALSO PROVIDES AN UPPER TRIANGULAR MATRIX S SUCH THAT
C
C            T   T               T
C           P *(A *A + D*D)*P = S *S .
C
C     S IS COMPUTED WITHIN QRSOLV AND MAY BE OF SEPARATE INTEREST.
C
C     THE SUBROUTINE STATEMENT IS
C
C       SUBROUTINE QRSOLV(N,R,LDR,IPVT,DIAG,QTB,X,SDIAG,WA)
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
C       X IS AN OUTPUT ARRAY OF LENGTH N WHICH CONTAINS THE LEAST
C         SQUARES SOLUTION OF THE SYSTEM A*X = B, D*X = 0.
C
C       SDIAG IS AN OUTPUT ARRAY OF LENGTH N WHICH CONTAINS THE
C         DIAGONAL ELEMENTS OF THE UPPER TRIANGULAR MATRIX S.
C
C       WA IS A WORK ARRAY OF LENGTH N.
C
C     SUBPROGRAMS CALLED
C
C       FORTRAN-SUPPLIED ... DABS,DSQRT
C
C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. MARCH 1980.
C     BURTON S. GARBOW, KENNETH E. HILLSTROM, JORGE J. MORE
C
C     **********
      INTEGER i,j,jp1,k,kp1,l,nsing
      DOUBLE PRECISION cosine,cotan,p5,p25,qtbpj,sine,summ,tangnt,temp,
     &                 zero
      LOGICAL dpeq
      EXTERNAL dpeq
      DATA p5,p25,zero/5.0D-1,2.5D-1,0.0D0/
C
C     COPY R AND (Q TRANSPOSE)*B TO PRESERVE INPUT AND INITIALIZE S.
C     IN PARTICULAR, SAVE THE DIAGONAL ELEMENTS OF R IN X.
C
      DO j=1,N
       DO i=j,N
        R(i,j)=R(j,i)
       END DO
       X(j)=R(j,j)
       Wa(j)=Qtb(j)
      END DO
C
C     ELIMINATE THE DIAGONAL MATRIX D USING A GIVENS ROTATION.
C
      DO j=1,N
C
C        PREPARE THE ROW OF D TO BE ELIMINATED, LOCATING THE
C        DIAGONAL ELEMENT USING P FROM THE QR FACTORIZATION.
C
       l=Ipvt(j)
       IF(.not.dpeq(Diag(l),zero))THEN
        DO k=j,N
         Sdiag(k)=zero
        END DO
        Sdiag(j)=Diag(l)
C
C        THE TRANSFORMATIONS TO ELIMINATE THE ROW OF D
C        MODIFY ONLY A SINGLE ELEMENT OF (Q TRANSPOSE)*B
C        BEYOND THE FIRST N, WHICH IS INITIALLY ZERO.
C
        qtbpj=zero
        DO k=j,N
C
C           DETERMINE A GIVENS ROTATION WHICH ELIMINATES THE
C           APPROPRIATE ELEMENT IN THE CURRENT ROW OF D.
C
         IF(.not.dpeq(Sdiag(k),zero))THEN
          IF(dabs(R(k,k)).ge.dabs(Sdiag(k)))THEN
           tangnt=Sdiag(k)/R(k,k)
           cosine=p5/dsqrt(p25+p25*tangnt**2)
           sine=cosine*tangnt
          ELSE
           cotan=R(k,k)/Sdiag(k)
           sine=p5/dsqrt(p25+p25*cotan**2)
           cosine=sine*cotan
          END IF
C
C           COMPUTE THE MODIFIED DIAGONAL ELEMENT OF R AND
C           THE MODIFIED ELEMENT OF ((Q TRANSPOSE)*B,0).
C
          R(k,k)=cosine*R(k,k)+sine*Sdiag(k)
          temp=cosine*Wa(k)+sine*qtbpj
          qtbpj=-sine*Wa(k)+cosine*qtbpj
          Wa(k)=temp
C
C           ACCUMULATE THE TRANFORMATION IN THE ROW OF S.
C
          kp1=k+1
          IF(N.ge.kp1)THEN
           DO i=kp1,N
            temp=cosine*R(i,k)+sine*Sdiag(i)
            Sdiag(i)=-sine*R(i,k)+cosine*Sdiag(i)
            R(i,k)=temp
           END DO
          END IF
         END IF
        END DO
       END IF
C
C        STORE THE DIAGONAL ELEMENT OF S AND RESTORE
C        THE CORRESPONDING DIAGONAL ELEMENT OF R.
C
       Sdiag(j)=R(j,j)
       R(j,j)=X(j)
      END DO
C
C     SOLVE THE TRIANGULAR SYSTEM FOR Z. IF THE SYSTEM IS
C     SINGULAR, THEN OBTAIN A LEAST SQUARES SOLUTION.
C
      nsing=N
      DO j=1,N
       IF(dpeq(Sdiag(j),zero).and.nsing.eq.N)nsing=j-1
       IF(nsing.lt.N)Wa(j)=zero
      END DO
      IF(nsing.ge.1)THEN
       DO k=1,nsing
        j=nsing-k+1
        summ=zero
        jp1=j+1
        IF(nsing.ge.jp1)THEN
         DO i=jp1,nsing
          summ=summ+R(i,j)*Wa(i)
         END DO
        END IF
        Wa(j)=(Wa(j)-summ)/Sdiag(j)
       END DO
      END IF
C
C     PERMUTE THE COMPONENTS OF Z BACK TO COMPONENTS OF X.
C
      DO j=1,N
       l=Ipvt(j)
       X(l)=Wa(j)
      END DO
      RETURN
C
C     LAST CARD OF SUBROUTINE QRSOLV.
C
      END
