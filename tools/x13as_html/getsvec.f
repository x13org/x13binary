      SUBROUTINE getSVec( vA, nA, nStart, nEnd, vB, nB )
c-----------------------------------------------------------------------
c     getSVec.f, Release 1, Subroutine Version 1.0, Created 22 Jul 2005.
c-----------------------------------------------------------------------
c     This subroutine extracts the subvector of 
c     vB = vA(nStart:nEnd).
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c nA        i     size (rows,columns) of vA vector
c nB        i     size (rows,columns) of vB vector
c nEnd      i     ending index of subvector in vA
c nStart    i     starting index of subvector in vA
c vA        d     input vector to extract subvector from
c vB        d     output vector to contain subvector of vA
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c i         i     index variable for do loops
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER nA(2), nStart, nEnd, nB(2)
      DOUBLE PRECISION vA(nA(1)), vB(nEnd-nStart+1)

c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i

c-----------------------------------------------------------------------
c     Extract the submatrix.
c-----------------------------------------------------------------------
      IF (( nA(1) .gt. 0 ) .and. ( nA(2) .eq. 1 )) THEN
        nB(1) = nEnd - nStart + 1
        nB(2) = 1
        DO i = nStart, nEnd
         vB(i-nStart+1) = vA(i)
        END DO
      ELSE
        nB(1) = 0
        nB(2) = 0
      END IF
c     ------------------------------------------------------------------
      RETURN
      END