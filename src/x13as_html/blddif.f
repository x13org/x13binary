      SUBROUTINE bldDif( dS, dT, nT, nPer, nDiff, vSeaD, oSeaD,
     &                   vTreD, oTreD, mDelS, dDelS, nDelS,
     &                   mDelT, dDelT, nDelT,
     &                   mRedDelS, dRedDelS, nRedDelS,
     &                   mRedDelT, dRedDelT, nRedDelT,
     &                   mDel, dDel, nDel )
c-----------------------------------------------------------------------
c     bldDif.f, Release 1, Subroutine Version 1.3, Modified 19 Jan 2006.
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 04 Apr 2005.
c     Modified by REG on 12 Sep 2005, to reverse order of difference 
c       polynomials inserted into difference matrices.
c     Modified by REG on 19 Sep 2005, to correct tab stops.
c     Modified by REG on 19 Jan 2006, to optimize mDel processing,
c       and to utilize diagonal matrix representation.
c-----------------------------------------------------------------------
c     This subroutine calculates the differencing matrices needed for 
c     forming the reduced model filters.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c dS      i   size of Seasonal Differencing
c dT      i   size of Trend Differencing
c dDel    d   diagonal representation for overall differencing matrix
c dDelS   d   diagonal representation for seasonal differencing matrix
c dDelT   d   diagonal representation for trend differencing matrix
c mDel    d   overall differencing matrix
c mDelS   d   seasonal differencing matrix
c mDelT   d   trend differencing matrix
c nDiff   i   vector of (d,D) differencing orders
c nPer    i   size of seasonal period
c mRedDelS d  smaller version of mDelS
c mRedDelT d  smaller version of mDelT
c nDel    i   size (rows,columns) of mDel
c nDelS   i   size (rows,columns) of mDelS
c nDelT   i   size (rows,columns) of mDelT
c nRedDelS i  size (rows,columns) of mRedDelS
c nRedDelT i  size (rows,columns) of mRedDelT
c oSeaD   i   max order of vSeaD polynomial
c oTreD   i   max order of vTreD polynomial
c nT      i   number of observations available
c vSeaD   d   seasonal differencing polynomial of size nSeaD
c vTreD   d   trend differencing polynomial of size vTreD
c-----------------------------------------------------------------------
c Name   Type Description (Local Variables)
c-----------------------------------------------------------------------
c i,j    i   index variables for do loops
c uS     d   seasonal differencing polynomial of length dS
c uT     d   trend    differencing polynomial of length dT+1
c ZERO   d   parameter variable
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER dS, dT, nT, oSeaD, oTreD
      INTEGER nDelS(2), nDelT(2), nDel(2), nDiff(2), nPer,
     &        nRedDelS(2), nRedDelT(2)
      DOUBLE PRECISION vSeaD(oSeaD+1), vTreD(oTreD+1)
      DOUBLE PRECISION mDelS(nT-dS,nT), mDelT(nT-dT,nT), 
     &                 mDel(nT-dS-dT,nT), mRedDelS(nT-dS-dT,nT-dT), 
     &                 mRedDelT(nT-dS-dT,nT-dS)
      DOUBLE PRECISION dDel(dS+dT+1), dDelS(dS+1), dDelT(dT+1),
     &                 dRedDelS(dS+1), dRedDelT(dT+1)

c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i, j, sFulD
      DOUBLE PRECISION uD(oSeaD+oTreD+1), uS(oSeaD+1), uT(oTreD+1),
     &                 vFulD(oSeaD+oTreD+1), ZERO
      PARAMETER (ZERO=0.0D0)

c-----------------------------------------------------------------------
c     Create seasonal differencing polynomial.
c-----------------------------------------------------------------------
      DO j = 1, oSeaD+1
       uS(j) = vSeaD(oSead+2-j)
      END DO

c     ------------------------------------------------------------------
c     Create seasonal differencing matrix (full and diagonal forms)
c     ------------------------------------------------------------------
      nDelS(1) = nT - dS
      nDelS(2) = nT
      DO j= 1, nDelS(2)
        DO i = 1, nDelS(1)
          mDelS( i, j ) = ZERO
        END DO
      END DO
      DO i = 1, nDelS(1)
        DO j = 1, dS+1
          mDelS( i, i+j-1 ) = uS(j)
        END DO
      END DO
c     ------------------------------------------------------------------
      DO j = 1, dS+1
        dDelS(j) = mDelS(1,j)
      END DO

c     ------------------------------------------------------------------
c     Create reduced seasonal differencing matrix
c     (full and diagonal forms)
c     ------------------------------------------------------------------
      nRedDelS(1) = nT - dS - dT
      nRedDelS(2) = nT - dT
      DO j = 1, nRedDelS(2)
        DO i = 1, nRedDelS(1)
         mRedDelS( i, j ) = mDelS( i, j )
        END DO
      END DO
c     ------------------------------------------------------------------
      DO j = 1, dS+1
        dRedDelS(j) = mRedDelS(1,j)
      END DO

c-----------------------------------------------------------------------
c     Create trend differencing polynomial.
c-----------------------------------------------------------------------
      DO j = 1, oTreD+1
       uT(j) = vTreD(oTreD+2-j)
      END DO

c     ------------------------------------------------------------------
c     Create trend differencing matrix (full and diagonal forms).
c     ------------------------------------------------------------------
      nDelT(1) = nT - dT
      nDelT(2) = nT
      DO j= 1, nDelT(2)
        DO i = 1, nDelT(1)
          mDelT( i, j ) = ZERO
        END DO
      END DO
      DO i = 1, nDelT(1)
       DO j = 1, min(3,dT+1)
        mDelT( i, i+j-1 ) = uT(j)
       END DO
      END DO
c     ------------------------------------------------------------------
      DO j = 1, dT+1
        dDelT(j) = mDelT(1,j)
      END DO

c     ------------------------------------------------------------------
c     Create reduced trend differencing matrix (full and diagonal forms).
c     ------------------------------------------------------------------
      nRedDelT(1) = nT - dS - dT
      nRedDelT(2) = nT - dS
      DO j = 1, nRedDelT(2)
        DO i = 1, nRedDelT(1)
          mRedDelT( i, j ) = mDelT( i, j )
        END DO
      END DO
c     ------------------------------------------------------------------
      DO j = 1, dT+1
        dRedDelT(j) = mRedDelT(1,j)
      END DO

c-----------------------------------------------------------------------
c     Create full differencing polynomial.
c-----------------------------------------------------------------------
      CALL CONV( vSeaD, oSeaD+1, vTreD, oTreD+1, vFulD, sFulD )
      DO j = 1, sFulD
       uD(j) = vFulD(sFulD+1-j)
      END DO

c     ------------------------------------------------------------------
c     Create full differencing matrix. Could use product of reduced 
c     seasonal differencing matrix and trend differencing matrix or 
c     vice versa using either of two different matrix multiplcations.
c     Instead for optimization reasons, use polynomial calculated above.
c     ------------------------------------------------------------------
c     CALL mulMat( mRedDelT, nRedDelT, mDelS, nDelS, mDel, nDel )
c     CALL mulMat( mRedDelS, nRedDelS, mDelT, nDelT, mDel, nDel )
c     ------------------------------------------------------------------
      nDel(1) = nT - dS - dT
      nDel(2) = nT
      DO j= 1, nDel(2)
        DO i = 1, nDel(1)
          mDel( i, j ) = ZERO
        END DO
      END DO
      DO i = 1, nDel(1)
        DO j = 1, sFulD
          mDel( i, i+j-1 ) = uD(j)
        END DO
      END DO
c     ------------------------------------------------------------------
      DO j = 1, dS+dT+1
        dDel(j) = mDel(1,j)
      END DO

c     ------------------------------------------------------------------
      RETURN
      END