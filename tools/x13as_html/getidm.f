      SUBROUTINE getIdM( n, mId, nId )
c-----------------------------------------------------------------------
c     getIdM.f, Release 1, Subroutine Version 1.0, Created 14 Apr 2005.
c-----------------------------------------------------------------------
c     This subroutine calculates the identity matrix mId of size 
c     nId = ( n, n ).
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c mId       d     output Identity matrix of size n x x
c nId       o     size (rows,columns) of identity matrix mId
c n         i     input row and column size for identity matrix mId
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c i,j       i     index variables for do loops
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER n, nId(2)
      DOUBLE PRECISION mId( n, n )

c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i, j

c-----------------------------------------------------------------------
c     Calculate the dimensions of mAT.
c-----------------------------------------------------------------------
      nId(1) = n
      nId(2) = n

c-----------------------------------------------------------------------
c     Create the identity matrix one row at a time.
c-----------------------------------------------------------------------
      DO j = 1, nId(2)
c     ------------------------------------------------------------------
c     For the jth column of the identity matrix, set the jth element to 0.
c     ------------------------------------------------------------------
       DO i = 1, nId(1)
        IF ( i .eq. j ) THEN
         mId(i,j) = 1.0D0
        ELSE
         mId(i,j) = 0.0D0
        END IF
       END DO
      END DO
c     ------------------------------------------------------------------
      RETURN
      END