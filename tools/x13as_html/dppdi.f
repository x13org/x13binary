C     Last change:  BCM  29 Sep 97    9:41 am
**==dppdi.f    processed by SPAG 4.03F  at 09:48 on  1 Mar 1994
      SUBROUTINE dppdi(Ap,N,Det,Job)
      IMPLICIT NONE
      DOUBLE PRECISION ZERO,ONE
      PARAMETER(ZERO=0D0,ONE=1D0)
      INTEGER N,Job
      DOUBLE PRECISION Ap(*)
      DOUBLE PRECISION Det(2)
c
c     dppdi computes the determinant and inverse
c     of a double precision symmetric positive definite matrix
c     using the factors computed by dppco or dppfa .
c
c     on entry
c
c        ap      double precision (n*(n+1)/2)
c                the output from dppco or dppfa.
c
c        n       integer
c                the order of the matrix  a .
c
c        job     integer
c                = 11   both determinant and inverse.
c                = 01   inverse only.
c                = 10   determinant only.
c
c     on return
c
c        ap      the upper triangular half of the inverse .
c                the strict lower triangle is unaltered.
c
c        det     double precision(2)
c                determinant of original matrix if requested.
c                otherwise not referenced.
c                determinant = det(1) * 10.0**det(2)
c                with  1.0 .le. det(1) .lt. 10.0
c                or  det(1) .eq. 0.0 .
c
c     error condition
c
c        a division by zero will occur if the input factor contains
c        a zero on the diagonal and the inverse is requested.
c        it will not occur if the subroutines are called correctly
c        and if dpoco or dpofa has set info .eq. 0 .
c
c     linpack.  this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas daxpy,dscal
c     fortran mod
c
c     internal variables
c
      DOUBLE PRECISION t
      DOUBLE PRECISION s
      INTEGER i,ii,j,jj,jm1,j1,k,kj,kk,kp1,k1
      LOGICAL dpeq
      EXTERNAL dpeq
c
c     compute determinant
c
      IF(Job/10.ne.0)THEN
       Det(1)=ONE
       Det(2)=ZERO
       s=10.0D0
       ii=0
       DO i=1,N
        ii=ii+i
        Det(1)=Ap(ii)**2*Det(1)
c        ...exit
        IF(.not.dpeq(Det(1),ZERO))THEN
         DO WHILE (Det(1).lt.ONE)
          Det(1)=s*Det(1)
          Det(2)=Det(2)-ONE
         END DO
         DO WHILE (Det(1).ge.s)
          Det(1)=Det(1)/s
          Det(2)=Det(2)+ONE
         END DO
        END IF
       END DO
      END IF
c
c     compute inverse(r)
c
      IF(mod(Job,10).ne.0)THEN
       kk=0
       DO k=1,N
        k1=kk+1
        kk=kk+k
        Ap(kk)=ONE/Ap(kk)
        t=-Ap(kk)
        CALL dscal(k-1,t,Ap(k1),1)
        kp1=k+1
        j1=kk+1
        kj=kk+k
        IF(N.ge.kp1)THEN
         DO j=kp1,N
          t=Ap(kj)
          Ap(kj)=ZERO
          CALL daxpy(k,t,Ap(k1),1,Ap(j1),1)
          j1=j1+j
          kj=kj+j
         END DO
        END IF
       END DO
c
c        form  inverse(r) * trans(inverse(r))
c
       jj=0
       DO j=1,N
        j1=jj+1
        jj=jj+j
        jm1=j-1
        k1=1
        kj=j1
        IF(jm1.ge.1)THEN
         DO k=1,jm1
          t=Ap(kj)
          CALL daxpy(k,t,Ap(j1),1,Ap(k1),1)
          k1=k1+k
          kj=kj+1
         END DO
        END IF
        t=Ap(jj)
        CALL dscal(j,t,Ap(j1),1)
       END DO
      END IF
      RETURN
      END
