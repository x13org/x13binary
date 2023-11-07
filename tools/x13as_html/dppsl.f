**==dppsl.f    processed by SPAG 4.03F  at 09:48 on  1 Mar 1994
      SUBROUTINE dppsl(Ap,N,B,Alt)
      IMPLICIT NONE
      INTEGER N
      DOUBLE PRECISION Ap(*),B(*)
      LOGICAL Alt
c
c     dppsl solves the double precision symmetric positive definite
c     system a * x = b
c     using the factors computed by dppco or dppfa.
c     unless alt is true, in which case it solves the system
c     l * x = b
c     where a=l*l'
c
c     on entry
c
c        ap      double precision (n*(n+1)/2)
c                the output from dppco or dppfa.
c
c        n       integer
c                the order of the matrix  a .
c
c        b       double precision(n)
c                the right hand side vector.
c
c     on return
c
c        b       the solution vector  x .
c
c     error condition
c
c        a division by zero will occur if the input factor contains
c        a zero on the diagonal.  technically this indicates
c        singularity but it is usually caused by improper subroutine
c        arguments.  it will not occur if the subroutines are called
c        correctly and  info .eq. 0 .
c
c     to compute  inverse(a) * c  where  c  is a matrix
c     with  p  columns
c           call dppco(ap,n,rcond,z,info)
c           if (rcond is too small .or. info .ne. 0) go to ...
c           do 10 j = 1, p
c              call dppsl(ap,n,c(1,j))
c        10 continue
c
c     linpack.  this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     alt option added 5/1/90 by larry bobbitt, census bureau,
c       statistical research division.
c
c     subroutines and functions
c
c     blas daxpy,ddot
c
c     internal variables
c
      DOUBLE PRECISION ddot,t
      INTEGER k,kb,kk
c
      kk=0
      DO k=1,N
       t=ddot(k-1,Ap(kk+1),1,B(1),1)
       kk=kk+k
       B(k)=(B(k)-t)/Ap(kk)
      END DO
c 
      IF(Alt)RETURN
c
      DO kb=1,N
       k=N+1-kb
       B(k)=B(k)/Ap(kk)
       kk=kk-k
       t=-B(k)
       CALL daxpy(k-1,t,Ap(kk+1),1,B(1),1)
      END DO
      RETURN
      END
