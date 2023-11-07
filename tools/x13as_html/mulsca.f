      SUBROUTINE mulSca( sA, mB, nB )
c-----------------------------------------------------------------------
c     mulSca.f, Release 1, Subroutine Version 1.0, Created 11 Apr 2005.
c-----------------------------------------------------------------------
c     This subroutine calculates the product of a scalar times a matrix
c     mB = sA x mB where nB contains the dimensions of mB.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c sA        d     input scalar to multiply mB by
c mB        d     input matrix to be multiplied by scalar sA
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
      INTEGER nB(2)
      DOUBLE PRECISION sA, mB( nB(1), nB(2) )

c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i, j

c-----------------------------------------------------------------------
c     Perform scalar times matrix multiply of mB = sA x mB.
c-----------------------------------------------------------------------
      DO i = 1, nB(1)
c     ------------------------------------------------------------------
c     Compute product of sA x mB( row i, column j).
c     ------------------------------------------------------------------
       DO j = 1, nB(2)
        mB(i,j) = sA*mB(i,j)
       END DO
      END DO
c     ------------------------------------------------------------------
      RETURN
      END