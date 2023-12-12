      SUBROUTINE getTr( mA, nA, mATr, nATr )
c-----------------------------------------------------------------------
c     getTr.f, Release 1, Subroutine Version 1.0, Created 11 Apr 2005.
c-----------------------------------------------------------------------
c     This subroutine calculates the transpose matrix mAT of a matrix mA
c     mATr = mA'.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c mA        d     input matrix to perform transpose on
c mATr      d     output matrix to contain transpose of mA
c nA        i     size (rows,columns) of mA matrix
c nATr      i     size (rows,columns) of mATr matrix
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c i,j       i     index variables for do loops
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER nA(2), nATr(2)
      DOUBLE PRECISION mA( nA(1), nA(2) ), mATr( nA(2), nA(1) )

c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i, j

c-----------------------------------------------------------------------
c     Calculate the dimensions of mAT.
c-----------------------------------------------------------------------
      nATr(1) = nA(2)
      nATr(2) = nA(1)

c-----------------------------------------------------------------------
c     Compute the matrix transpose of mATr = mA'.
c-----------------------------------------------------------------------
      DO i = 1, nATr(1)
c     ------------------------------------------------------------------
c     Get mAT row i from mA column i.
c     ------------------------------------------------------------------
       DO j = 1, nATr(2)
        mATr(i,j) = mA(j,i)
       END DO
      END DO
c     ------------------------------------------------------------------
      RETURN
      END