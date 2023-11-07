      SUBROUTINE getSMat( mA, nA, nStart, nEnd, mB, nB )
c-----------------------------------------------------------------------
c     getSMat.f, Release 1, Subroutine Version 1.0, Created 18 Apr 2005.
c-----------------------------------------------------------------------
c     This subroutine extracts the submatrix of 
c     mA(nStart:nEnd,nStart:nEnd).
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c mA        d     input matrix to extract submatrix from
c mB        d     output matrix to contain submatrix of mA
c nA        i     size (rows,columns) of mA matrix
c nB        i     size (rows,columns) of mB matrix
c nEnd      i     ending row/column index of submatrix in mA
c nStart    i     starting row/column index of submatrix in mA
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c i,j       i     index variables for do loops
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER nA(2), nStart, nEnd, nB(2)
      DOUBLE PRECISION mA(nA(1),nA(2)), mB(nEnd-nStart+1,nEnd-nStart+1)

c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i,j

c-----------------------------------------------------------------------
c     Extract the submatrix.
c-----------------------------------------------------------------------
      IF (( nA(1) .gt. 0 ) .and. (nA(2) .gt. 0)) THEN
        nB(1) = nEnd - nStart + 1
        nB(2) = nEnd - nStart + 1
        DO i = nStart, nEnd
         DO j = nStart, nEnd
          mB(i-nStart+1, j-nStart+1) = mA(i,j)
         END DO
        END DO
      ELSE
        nB(1) = 0
        nB(2) = 0
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE getSRMat( mA, nA, nStartR, nEndR, nStartC, nEndC,
     &                     mB, nB )
c-----------------------------------------------------------------------
c     Release 1, Subroutine Version 1.0, Created 20 Mar 2006.
c-----------------------------------------------------------------------
c     This subroutine extracts the submatrix of 
c     mA(nStartR:nEndR,nStartC:nEndC).
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c mA        d     input matrix to extract submatrix from
c mB        d     output matrix to contain submatrix of mA
c nA        i     size (rows,columns) of mA matrix
c nB        i     size (rows,columns) of mB matrix
c nEnd      i     ending row/column index of submatrix in mA
c nStart    i     starting row/column index of submatrix in mA
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c i,j       i     index variables for do loops
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER nA(2), nB(2), nEndC, nEndR, nStartC, nStartR
      DOUBLE PRECISION mA(nA(1),nA(2)),
     &                 mB(nEndR-nStartR+1,nEndC-nStartC+1)

c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i,j

c-----------------------------------------------------------------------
c     Extract the submatrix.
c-----------------------------------------------------------------------
      IF (( nA(1) .gt. 0 ) .and. (nA(2) .gt. 0)) THEN
        nB(1) = nEndR - nStartR + 1
        nB(2) = nEndC - nStartC + 1
        DO j = nStartC, nEndC
         DO i = nStartR, nEndR
          mB(i-nStartR+1, j-nStartC+1) = mA(i,j)
         END DO
        END DO
      ELSE
        nB(1) = 0
        nB(2) = 0
      END IF
c     ------------------------------------------------------------------
      RETURN
      END