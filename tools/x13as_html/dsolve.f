**==dsolve.f    processed by SPAG 4.03F  at 09:48 on  1 Mar 1994
      SUBROUTINE dsolve(A,Nr,Nc,Lainvb,B)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     A variation Linpack's DPOSL subroutine to solve a double precision
c symmetric positive definite system A*x=b
c using the factors computed by DPOFA where b can have more than one
c column.
c-----------------------------------------------------------------------
c Name     Type Description
c-----------------------------------------------------------------------
c a        r    Packed nr by nr cholesky decompostion calculated by
c                DPOFA with nr(nr+1)/2 elements
c b        r    Nr by nc matrix.  On input it is b, the right hand
c                side of the equation and on output it is the solution
c ddot     r    Function that calculates the inner product
c diag     r    Temporary scalar to store A(k,k)
c i        i    Row counter
c ib       i    Packed row counter
c ielt     i    Packed element counter
c j        i    Column counter
c nc       i    Number of columns in the b and x matrices
c nr       i    Number of rows in the a, b and x matrices
c sum      r    Temporary scalar to store the inner product sum
c tmp      r    Temporary scalar
c-----------------------------------------------------------------------
      LOGICAL Lainvb
      INTEGER i,ib,j,ielt,Nc,Nr
      DOUBLE PRECISION A(Nr*(Nr+1)/2),B(Nc,Nr),sum,diag
      DOUBLE PRECISION ddot
c-----------------------------------------------------------------------
c     Solve R'w=b
c-----------------------------------------------------------------------
      ielt=0
      DO i=1,Nr
       diag=A(ielt+i)
       DO j=1,Nc
        sum=ddot(i-1,A(ielt+1),1,B(j,1),Nc)
        B(j,i)=(B(j,i)-sum)/diag
       END DO
       ielt=ielt+i
      END DO
c-----------------------------------------------------------------------
c     Solve R*x=w
c-----------------------------------------------------------------------
      IF(Lainvb)THEN
       DO ib=1,Nr
        i=Nr+1-ib
        diag=A(ielt)
        ielt=ielt-i
c     ------------------------------------------------------------------
        DO j=1,Nc
         B(j,i)=B(j,i)/diag
         CALL daxpy(i-1,-B(j,i),A(ielt+1),1,B(j,1),Nc)
        END DO
       END DO
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
