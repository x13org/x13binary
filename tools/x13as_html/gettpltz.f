      SUBROUTINE getTpltz( vA, rA, rcB, mB, nB )
c-----------------------------------------------------------------------
c     getTpltz.f, Release 1, Subroutine Version 1.0, Created 11 Apr 2005.
c-----------------------------------------------------------------------
c     This subroutine calculates the Toeplitz matrix mB given the vector
c     vA of diagonal entries.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c mB        d     output Toeplitz square matrix
c nB        i     size (rows,columns) of Toeplitz matrix mB
c rA        i     length of vector vA
c rcB       i     row/column size of output Toeplitz matrix mB
c vA        d     input vector of Toeplitz diagonal entries
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c i,j       i     index variables for do loops
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER rA, rcB, nB(2)
      DOUBLE PRECISION vA( rA ), mB( rcB, rcB )

c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i, j

c-----------------------------------------------------------------------
c     Check for valid Toeplitz matrix size.
c-----------------------------------------------------------------------
      IF ( rcB .gt. 0 ) THEN

c-----------------------------------------------------------------------
c     Establish dimensions of mC matrix.
c-----------------------------------------------------------------------
       nB(1) = rcB
       nB(2) = rcB

c-----------------------------------------------------------------------
c     Zero out mB if rcB > rA
c-----------------------------------------------------------------------
       IF ( rcB .gt. rA ) THEN
        DO i = 1, rcB
         DO j = 1, rcB
          mB(i,j) = 0.0D0
         END DO
        END DO
       END IF

c-----------------------------------------------------------------------
c     Create constant diagonals in Toeplitz matrix based on vA.
c-----------------------------------------------------------------------
       DO i = 1, min( rA, rcB )

c     ------------------------------------------------------------------
c     Process main diagonal.
c     ------------------------------------------------------------------
        IF ( i .eq. 1 ) THEN
         DO j = 1, rcB
          mB(j,j) = vA(1)
         END DO

c     ------------------------------------------------------------------
c     Process off diagonals.
c     ------------------------------------------------------------------
        ELSE IF ( i .le. rcB ) THEN
         DO j = 1, rcB+1-i
          mB(j,i-1+j) = vA(i)
          mB(i-1+j,j) = vA(i)
         END DO
        END IF
       END DO

c-----------------------------------------------------------------------
c     Invalid Toeplitz matrix size.
c-----------------------------------------------------------------------
      ELSE
       nB(1) = 0
       nB(2) = 0
      END IF
c     ------------------------------------------------------------------
      RETURN
      END