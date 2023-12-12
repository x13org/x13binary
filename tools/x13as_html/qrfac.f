C     Last change:  BCM  29 Sep 97    8:59 am
**==qrfac.f    processed by SPAG 4.03F  at 09:52 on  1 Mar 1994
      SUBROUTINE qrfac(M,N,A,Lda,Pivot,Ipvt,Lipvt,Rdiag,Acnorm,Wa)
      IMPLICIT NONE
      INTEGER M,N,Lda,Lipvt
      INTEGER Ipvt(Lipvt)
      LOGICAL Pivot
      DOUBLE PRECISION A(Lda,N),Rdiag(N),Acnorm(N),Wa(N)
C     **********
C
C     SUBROUTINE QRFAC
C
C     THIS SUBROUTINE USES HOUSEHOLDER TRANSFORMATIONS WITH COLUMN
C     PIVOTING (OPTIONAL) TO COMPUTE A QR FACTORIZATION OF THE
C     M BY N MATRIX A. THAT IS, QRFAC DETERMINES AN ORTHOGONAL
C     MATRIX Q, A PERMUTATION MATRIX P, AND AN UPPER TRAPEZOIDAL
C     MATRIX R WITH DIAGONAL ELEMENTS OF NONINCREASING MAGNITUDE,
C     SUCH THAT A*P = Q*R. THE HOUSEHOLDER TRANSFORMATION FOR
C     COLUMN K, K = 1,2,...,MIN(M,N), IS OF THE FORM
C
C                           T
C           I - (1/U(K))*U*U
C
C     WHERE U HAS ZEROS IN THE FIRST K-1 POSITIONS. THE FORM OF
C     THIS TRANSFORMATION AND THE METHOD OF PIVOTING FIRST
C     APPEARED IN THE CORRESPONDING LINPACK SUBROUTINE.
C
C     THE SUBROUTINE STATEMENT IS
C
C       SUBROUTINE QRFAC(M,N,A,LDA,PIVOT,IPVT,LIPVT,RDIAG,ACNORM,WA)
C
C     WHERE
C
C       M IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER
C         OF ROWS OF A.
C
C       N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER
C         OF COLUMNS OF A.
C
C       A IS AN M BY N ARRAY. ON INPUT A CONTAINS THE MATRIX FOR
C         WHICH THE QR FACTORIZATION IS TO BE COMPUTED. ON OUTPUT
C         THE STRICT UPPER TRAPEZOIDAL PART OF A CONTAINS THE STRICT
C         UPPER TRAPEZOIDAL PART OF R, AND THE LOWER TRAPEZOIDAL
C         PART OF A CONTAINS A FACTORED FORM OF Q (THE NON-TRIVIAL
C         ELEMENTS OF THE U VECTORS DESCRIBED ABOVE).
C
C       LDA IS A POSITIVE INTEGER INPUT VARIABLE NOT LESS THAN M
C         WHICH SPECIFIES THE LEADING DIMENSION OF THE ARRAY A.
C
C       PIVOT IS A LOGICAL INPUT VARIABLE. IF PIVOT IS SET TRUE,
C         THEN COLUMN PIVOTING IS ENFORCED. IF PIVOT IS SET FALSE,
C         THEN NO COLUMN PIVOTING IS DONE.
C
C       IPVT IS AN INTEGER OUTPUT ARRAY OF LENGTH LIPVT. IPVT
C         DEFINES THE PERMUTATION MATRIX P SUCH THAT A*P = Q*R.
C         COLUMN J OF P IS COLUMN IPVT(J) OF THE IDENTITY MATRIX.
C         IF PIVOT IS FALSE, IPVT IS NOT REFERENCED.
C
C       LIPVT IS A POSITIVE INTEGER INPUT VARIABLE. IF PIVOT IS FALSE,
C         THEN LIPVT MAY BE AS SMALL AS 1. IF PIVOT IS TRUE, THEN
C         LIPVT MUST BE AT LEAST N.
C
C       RDIAG IS AN OUTPUT ARRAY OF LENGTH N WHICH CONTAINS THE
C         DIAGONAL ELEMENTS OF R.
C
C       ACNORM IS AN OUTPUT ARRAY OF LENGTH N WHICH CONTAINS THE
C         NORMS OF THE CORRESPONDING COLUMNS OF THE INPUT MATRIX A.
C         IF THIS INFORMATION IS NOT NEEDED, THEN ACNORM CAN COINCIDE
C         WITH RDIAG.
C
C       WA IS A WORK ARRAY OF LENGTH N. IF PIVOT IS FALSE, THEN WA
C         CAN COINCIDE WITH RDIAG.
C
C     SUBPROGRAMS CALLED
C
C       MINPACK-SUPPLIED ... DPMPAR,ENORM
C
C       FORTRAN-SUPPLIED ... DMAX1,DSQRT,MIN0
C
C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. MARCH 1980.
C     BURTON S. GARBOW, KENNETH E. HILLSTROM, JORGE J. MORE
C
C     **********
      INTEGER i,j,jp1,k,kmax,minmn
      DOUBLE PRECISION ajnorm,epsmch,one,p05,sum,temp,zero
      DOUBLE PRECISION dpmpar,enorm
      LOGICAL dpeq
      EXTERNAL dpeq
      DATA one,p05,zero/1.0D0,5.0D-2,0.0D0/
C
C     EPSMCH IS THE MACHINE PRECISION.
C
      epsmch=dpmpar(1)
C
C     COMPUTE THE INITIAL COLUMN NORMS AND INITIALIZE SEVERAL ARRAYS.
C
      DO j=1,N
       Acnorm(j)=enorm(M,A(1,j))
       Rdiag(j)=Acnorm(j)
       Wa(j)=Rdiag(j)
       IF(Pivot)Ipvt(j)=j
      END DO
C
C     REDUCE A TO R WITH HOUSEHOLDER TRANSFORMATIONS.
C
      minmn=min0(M,N)
      DO j=1,minmn
       IF(Pivot)THEN
C
C        BRING THE COLUMN OF LARGEST NORM INTO THE PIVOT POSITION.
C
        kmax=j
        DO k=j,N
         IF(Rdiag(k).gt.Rdiag(kmax))kmax=k
        END DO
        IF(kmax.ne.j)THEN
         DO i=1,M
          temp=A(i,j)
          A(i,j)=A(i,kmax)
          A(i,kmax)=temp
         END DO
         Rdiag(kmax)=Rdiag(j)
         Wa(kmax)=Wa(j)
         k=Ipvt(j)
         Ipvt(j)=Ipvt(kmax)
         Ipvt(kmax)=k
        END IF
       END IF
C
C        COMPUTE THE HOUSEHOLDER TRANSFORMATION TO REDUCE THE
C        J-TH COLUMN OF A TO A MULTIPLE OF THE J-TH UNIT VECTOR.
C
       ajnorm=enorm(M-j+1,A(j,j))
       IF(.not.dpeq(ajnorm,zero))THEN
        IF(A(j,j).lt.zero)ajnorm=-ajnorm
        DO i=j,M
         A(i,j)=A(i,j)/ajnorm
        END DO
        A(j,j)=A(j,j)+one
C
C        APPLY THE TRANSFORMATION TO THE REMAINING COLUMNS
C        AND UPDATE THE NORMS.
C
        jp1=j+1
        IF(N.ge.jp1)THEN
         DO k=jp1,N
          sum=zero
          DO i=j,M
           sum=sum+A(i,j)*A(i,k)
          END DO
          temp=sum/A(j,j)
          DO i=j,M
           A(i,k)=A(i,k)-temp*A(i,j)
          END DO
          IF(.not.(.not.Pivot.or.dpeq(Rdiag(k),zero)))THEN
           temp=A(j,k)/Rdiag(k)
           Rdiag(k)=Rdiag(k)*dsqrt(dmax1(zero,one-temp**2))
           IF(p05*(Rdiag(k)/Wa(k))**2.le.epsmch)THEN
            Rdiag(k)=enorm(M-j,A(jp1,k))
            Wa(k)=Rdiag(k)
           END IF
          END IF
         END DO
        END IF
       END IF
       Rdiag(j)=-ajnorm
      END DO
      RETURN
C
C     LAST CARD OF SUBROUTINE QRFAC.
C
      END
