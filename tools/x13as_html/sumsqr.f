      DOUBLE PRECISION FUNCTION sumsqr( vA, nStart, nEnd )
c-----------------------------------------------------------------------
c     sumsqr.f, Release 1, Subroutine Version 1.0, Created 18 Apr 2005.
c-----------------------------------------------------------------------
c     This subroutine calculates the sum of square of the entries in 
c     the vector vA from start index nStart to end index nEnd.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c nEnd      i     ending index of vA subvector for sum of squares
c nStart    i     starting index of vA subvector for sum of squares
c vA        d     input vector for calculation of sum of squares
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c i         i     index variable for do loops
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER nStart, nEnd
      DOUBLE PRECISION vA(*)

c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i

c-----------------------------------------------------------------------
c     Calculate the requisite sum of squares.
c-----------------------------------------------------------------------
      sumsqr = 0.0D0
      DO i = nStart, nEnd
       sumsqr = sumsqr + ( vA(i)*vA(i) )
      END DO
c     ------------------------------------------------------------------
      RETURN
      END