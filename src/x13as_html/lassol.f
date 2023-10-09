**==lassol.f    processed by SPAG 4.03F  at 09:50 on  1 Mar 1994
      SUBROUTINE lassol(N,A,B,M,X,Iflag)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c  lassol solves a system of n linear equations in n unkowns, AX = B,
c  using gaussian elimination with parital pivoting and row
c  equilibration.
c
c  taken from NUMERICAL COMPUTING: AN INTRODUCTION by shampine and
c  allen.  adapted by brian monsell, 12-6-88.
c-----------------------------------------------------------------------
      INTEGER i,ib,idxpiv,Iflag,ip1,j,k,kp1,M,N,nm1,np1
      DOUBLE PRECISION A(M,M),B(N),X(N),ab(3,4),rowmax,big,quot,
     &                 sum,tempb,tempi,scale,one,zero
      EQUIVALENCE(big,quot,sum,scale),(rowmax,tempb,tempi)
      DATA one,zero/1.0D0,0D0/
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
      np1=N+1
      nm1=N-1
c-----------------------------------------------------------------------
c  form the n by (n+1) matrix AB, the first n columns of which are
c  A and the remaining column B.  calculate scale factors and scale
c  AB.
c-----------------------------------------------------------------------
      DO i=1,N
       rowmax=zero
       DO j=1,N
        rowmax=dmax1(rowmax,abs(A(i,j)))
       END DO
       scale=one/rowmax
       DO j=1,N
        ab(i,j)=A(i,j)*scale
       END DO
       ab(i,np1)=B(i)*scale
      END DO
c-----------------------------------------------------------------------
c  begin basic elimination loop.  rows of ab are physically
c  interchanged in order to bring element of largest magnitude
c  into pivotal position
c-----------------------------------------------------------------------
      DO k=1,nm1
       big=zero
       DO i=k,N
        tempb=abs(ab(i,k))
        IF(big.lt.tempb)THEN
         big=tempb
         idxpiv=i
        END IF
       END DO
       IF(dpeq(big,zero))GO TO 10
       IF(idxpiv.ne.k)THEN
c-----------------------------------------------------------------------
c  pivot is in row idxpiv.  interchange rouw idxpiv with row k.
c-----------------------------------------------------------------------
        DO i=k,np1
         tempi=ab(k,i)
         ab(k,i)=ab(idxpiv,i)
         ab(idxpiv,i)=tempi
        END DO
       END IF
       kp1=k+1
c-----------------------------------------------------------------------
c  eliminate x(k) from equations k+1,k+2,...,k+n.
c-----------------------------------------------------------------------
       DO i=kp1,N
        quot=ab(i,k)/ab(k,k)
        DO j=kp1,np1
         ab(i,j)=ab(i,j)-(quot*ab(k,j))
        END DO
       END DO
      END DO
c-----------------------------------------------------------------------
c  begin calculation of solution x using back substitution.
c-----------------------------------------------------------------------
      IF(.not.dpeq(ab(N,N),zero))THEN
       X(N)=ab(N,np1)/ab(N,N)
       DO ib=2,N
        i=np1-ib
        ip1=i+1
        sum=zero
        DO j=ip1,N
         sum=sum+ab(i,j)*X(j)
        END DO
        X(i)=(ab(i,np1)-sum)/ab(i,i)
       END DO
c-----------------------------------------------------------------------
c  set iflag = 1 for normal return
c            = 2 if matrix appears singular to the code.
c-----------------------------------------------------------------------
       Iflag=1
       RETURN
      END IF
   10 Iflag=2
      RETURN
      END
