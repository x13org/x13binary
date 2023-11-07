      SUBROUTINE cpyMat( mA, nA, mB, nB )
c-----------------------------------------------------------------------
c     cpyMat.f, Release 1, Subroutine Version 1.0, Created 14 Apr 2005.
c-----------------------------------------------------------------------
c     This subroutine copies matrix mA to matrix mB where nA and nB 
c     contain the dimensions of mA and mB.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c mA        d     input matrix to be copied from
c mB        d     output matrix to be copied to
c nA        i     size (rows,columns) of mA matrix
c nB        i     size (rows,columns) of mB matrix
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c i,j       i     index variables for do loops
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER nA(2), nB(2)
      DOUBLE PRECISION mA( nA(1), nA(2) ), mB( nA(1), nA(2) )

c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i, j

c-----------------------------------------------------------------------
c     Establish dimensions of mB matrix.
c-----------------------------------------------------------------------
      nB(1) = nA(1)
      nB(2) = nA(2)

c-----------------------------------------------------------------------
c     Perform matrix copy of mB = mA
c-----------------------------------------------------------------------
      DO j = 1, nA(2)
c     ------------------------------------------------------------------
c     Copy mA column j to mB column j.
c     ------------------------------------------------------------------
       DO i = 1, nA(1)
        mB(i,j) = mA(i,j)
       END DO
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
      