      SUBROUTINE addMat( mA, nA, mB, nB, mC, nC )
c-----------------------------------------------------------------------
c     addMat.f, Release 1, Subroutine Version 1.0, Created 11 Apr 2005.
c-----------------------------------------------------------------------
c     This subroutine calculates the matrix addition of mC = mA + mB 
c     where nA, nB, and nC contain the dimensions of mA, mB, and mC.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c mA        d     first input matrix to be added
c mB        d     second input matrix to be added
c mC        d     matrix output result of mA+mB
c nA        i     size (rows,columns) of mA matrix
c nB        i     size (rows,columns) of mB matrix
c nC        i     size (rows,columns) of mC matrix
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c i,j       i     index variables for do loops
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER nA(2), nB(2), nC(2)
      DOUBLE PRECISION mA( nA(1), nA(2) ), mB( nB(1), nB(2) ), 
     &                 mC( nA(1), nB(2) )

c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i, j

c-----------------------------------------------------------------------
c     Check for valid matrix addition.
c-----------------------------------------------------------------------
      IF (( nA(1) .eq. nB(1) ) .and. ( nA(2) .eq. nB(2) )) THEN

c-----------------------------------------------------------------------
c     Establish dimensions of mC matrix.
c-----------------------------------------------------------------------
       nC(1) = nA(1)
       nC(2) = nB(2)

c-----------------------------------------------------------------------
c     Perform matrix multiply of mC = mA + mB.
c-----------------------------------------------------------------------
       DO i = 1, nC(1)
c     ------------------------------------------------------------------
c     Compute vector addition of mA row i plus mB row i.
c     ------------------------------------------------------------------
        DO j = 1, nC(2)
         mC(i,j) = mA(i,j) + mB(i,j)
        END DO
       END DO

c-----------------------------------------------------------------------
c     Invalid matrix addition.
c-----------------------------------------------------------------------
      ELSE
       nC(1) = 0
       nC(2) = 0
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
      