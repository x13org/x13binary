      SUBROUTINE compLagDiag( lag, nT, dS, dT, nPer,
     &                        finfact, sdSig, vIrrEst, nIrrEst,
     &                        vSeaEst, nSeaEst, vTreEst, nTreEst,
     &                        dDelS, nDelS, dDelT, nDelT,
     &                        mCovIrr, nCovIrr, mCovSea, nCovSea,
     &                        mCovTre, nCovTre, mCovSA, nCovSA,
     &                        fulEst, noeEst, fulEso, noeEso,
     &                        fulVar, noeVar, fulDia, noeDia )
c-----------------------------------------------------------------------
c compLagDiag.f, Release 1, Subroutine Version 1.3, Modified 01 May 2006.
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 21 Jul 2005.
c     Modified by REG, on 15 Sep 2005, to change local variable sTemp8
c       from an undimensioned variable to a vector of size one, 
c       and to correct tab alignments.
c     Modified by REG, on 13 Jan 2006, to optimize matrix operations,
c       by using getTrcAB utility, and by using diagonal matrix
c       utilities.
c     Modified by REG, on 01 May 2006, to calculate estimate and 
c       variance relative to innovation variance to match SEATS output,
c       and to move sdSig finite correction factor processing 
c       to getDiag() eliminating need for input nParam, nFixed, 
c       and nDiff.
c-----------------------------------------------------------------------
c     This subroutine calculates some lag diagnostics.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c dDelS   d   diagonal form of seasonal differencing matrix: mDelS
c dDelT   d   diagonal form of trend differencing matrix: mDelT
c dS      i   size of Seasonal Differencing
c dT      i   size of Trend Differencing
c finfact d   finite sample correction factor
c fulDia  d   vector of normalized diagnostics from full signals
c             for irregular, seasonal, trend, and SA
c fulEso  d   vector of null means of estimates from full signals
c             for irregular, seasonal, trend, and SA
c fulEst  d   vector of diagnostic estimates from full signals
c             for irregular, seasonal, trend, and SA
c fulVar  d   vector of variances of diagnostics from full signals
c             for irregular, seasonal, trend, and SA
c lag     i   lag of diagnostics to calculate
c mCovIrr d   covariance of estimated irregular
c mCovSA  d   covariance of estimated seasonal adjusted
c mCovSea d   covariance of estimated seasonal
c mCovTre d   covariance of estimated trend
c mDelS   d   seasonal differencing matrix
c mDelT   d   trend differencing matrix
c nCovIrr d   size (rows,columns) of mCovIrr matrix
c nCovSA  d   size (rows,columns) of mCovSA  matrix
c nCovSea d   size (rows,columns) of mCovSea matrix
c nCovTre d   size (rows,columns) of mCovTre matrix
c nDelS   i   size (rows,columns) of mDelS
c nDelT   i   size (rows,columns) of mDelT
c nIrrEst d   size (rows,columns) of vIrrEst vector
c nSeaEst d   size (rows,columns) of vSeaEst vector
c nTreEst d   size (rows,columns) of vTreEst vector
c nPer    i   size of seasonal period
c nT      i   size of data available
c noeDia  d   vector of normalized diagnostics from trimmed signals
c             for irregular, seasonal, trend, and SA
c noeEso  d   vector of null means of estimate from trimmed signals
c             for irregular, seasonal, trend, and SA
c noeEst  d   vector of diagnostic estimates from trimmed signals
c             for irregular, seasonal, trend, and SA
c noeVar  d   vector of variances of diagnostics from trimmed signals
c             for irregular, seasonal, trend, and SA
c sdSig   d   estimated data innovation stdev adjusted for number of
c             estimated parameters
c vIrrEst d   estimated irregular
c vSeaEst d   estimated seasonal
c vTreEst d   estimated trend
c-----------------------------------------------------------------------
c Name   Type Description (Local Variables)
c-----------------------------------------------------------------------
c getTrc  d   external function reference
c HALF    d   constant parameter
c innovar d   model innovation variance adjusted for finite sample
c i,j     i   index variables
c k1,k2,k3 d  miscelaneous constants
c mCovIrrNoe d no-end irregular covariance matrix
c mCovSeaNoe d no-end seasonal covariance matrix
c mCovTreNoe d no-end trend covariance matrix
c mCovSANoe  d no-end SA covariance matrix
c mLag    d   selection matrix (using all data)
c mLagNoe d   selection matrix (using noend data)
c mLagS   d   seasonal selection matrix (using all data)
c mLagSNoe d  seasonal selection matrix (using noend data)
c mLagT    d  trend selection matrix (using all data)
c mLagTNoe d  trend selection matrix (using noend data)
c mTemp4  d   temporary matrix 4
c mTemp5  d   temporary matrix 5
c mTemp6  d   temporary matrix 6
c mTemp7  d   temporary matrix 7
c nCovIrrNoe d size (rows,columns) of mCovIrrNoe matrix
c nCovSeaNoe d size (rows,columns) of mCovSeaNoe matrix
c nCovTreNoe d size (rows,columns) of mCovTreNoe matrix
c nCovSANoe  d size (rows,columns) of mCovSANoe matrix
c nLag    i   size (rows,columns) of mLag matrix
c nLagS   i   size (rows,columns) of mLagS matrix
c nLagT   i   size (rows,columns) of mLagT matrix
c nSave   i   identifies default size of large matrices
c             that are saved (not dynamic)
c nTemp1  d   size (rows,columns) of vTemp1 vector
c nTemp2  d   size (rows,columns) of vTemp2 vector
c nTemp3  d   size (rows,columns) of vTemp3 vector
c nTemp4  d   size (rows,columns) of mTemp4 matrix
c nTemp5  d   size (rows,columns) of mTemp5 matrix
c nTemp6  d   size (rows,columns) of mTemp6 matrix
c nTemp7  d   size (rows,columns) of mTemp7 matrix
c nTemp8  d   size (rows,columns) of sTemp8 scalar
c nTemp9  d   size (rows,columns) of vTemp9 vector
c ONE     d   constant parameter
c sTemp8  d   temporary scalar 8
c sumsqr  d   external function reference
c trCovIrr      d trace of mCovIrr matrix
c trCovIrrNoe   d trace of mCovIrrNoe matrix
c trCovIrrNoeSq d trace of mCovIrrNoe squared matrix
c trCovIrrSq    d trace of mCovIrr squared matrix
c trCovSea      d trace of mCovSea matrix
c trCovSeaNoe   d trace of mCovSeaNoe matrix
c trCovSeaNoeSq d trace of mCovSeaNoe squared matrix
c trCovSeaSq    d trace of mCovSea squared matrix
c trCovTre      d trace of mCovTre matrix
c trCovTreNoe   d trace of mCovTreNoe matrix
c trCovTreNoeSq d trace of mCovTreNoe squared matrix
c trCovTreSq    d trace of mCovTre squared matrix
c trCovSA       d trace of mCovSA matrix
c trCovSANoe    d trace of mCovSANoe matrix
c trCovSANoeSq  d trace of mCovSANoe squared matrix
c trCovSASq     d trace of mCovSA squared matrix
c TWO     d   constant parameter
c vTemp1  d   temporary vector 1
c vTemp2  d   temporary vector 2
c vTemp3  d   temporary vector 3
c vTemp9  d   temporary vector 9
c ZERO    d   constant parameter
c-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'srslen.prm'
c     ------------------------------------------------------------------
c     added by BCM to correctly dimension variables 
c     ------------------------------------------------------------------
      INTEGER pdA
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER lag, nT, dS, dT, nPer
      INTEGER nIrrEst(2), nSeaEst(2), nTreEst(2)
      INTEGER nDelS(2), nDelT(2)
      INTEGER nCovIrr(2), nCovSea(2), nCovTre(2), nCovSA(2)
      DOUBLE PRECISION finfact, sdSig
      DOUBLE PRECISION vIrrEst(nT), vSeaEst(nT), vTreEst(nT)
      DOUBLE PRECISION dDelS(dS+1), dDelT(dT+1)
c     DOUBLE PRECISION mDelS(nT-dS,nT), mDelT(nT-dT,nT)
      DOUBLE PRECISION mCovIrr(nT,nT), mCovSea(nT-dS,nT-dS), 
     &                 mCovTre(nT-dT,nT-dT), mCovSA(nT-dT,nT-dT)
      DOUBLE PRECISION fulEst(4), noeEst(4), fulEso(4), noeEso(4),
     &                 fulVar(4), noeVar(4), fulDia(4), noeDia(4)

c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i, j
      INTEGER nLag(2), nLagS(2), nLagT(2)
      INTEGER nLagNoe(2), nLagSNoe(2), nLagTNoe(2)
      INTEGER nTemp1(2), nTemp2(2), nTemp3(2), nTemp4(2), nTemp5(2)
      INTEGER nTemp6(2), nTemp7(2), nTemp8(2), nTemp9(2)
      INTEGER nCovIrrNoe(2), nCovSeaNoe(2), nCovTreNoe(2), nCovSANoe(2)
      DOUBLE PRECISION innovar
      DOUBLE PRECISION vTemp1(nT-dS), vTemp2(nT-dT), vTemp3(nT),
     &                 vTemp9(nT-2*nPer)
      DOUBLE PRECISION trCovIrr, trCovIrrNoe, trCovSea, trCovSeaNoe,
     &                 trCovTre, trCovTreNoe, trCovSA,  trCovSANoe
      DOUBLE PRECISION trCovIrrSq, trCovIrrNoeSq, trCovSeaSq,
     &                 trCovSeaNoeSq, trCovTreSq, trCovTreNoeSq,
     &                 trCovSASq, trCovSANoeSq
      DOUBLE PRECISION k1, k2, k3, ZERO, HALF, ONE, TWO
      DOUBLE PRECISION getTrc, getTrcAB, sumsqr
      PARAMETER (ZERO=0.0D0, HALF=0.5D0, ONE=1.0D0, TWO=2.0D0)
      DOUBLE PRECISION sTemp8(1)
      LOGICAL dpeq

c     ------------------------------------------------------------------
c     Dynamic (commented) versus static (uncommented) matrices
c     ------------------------------------------------------------------
c     DOUBLE PRECISION mLag(nT,nT), mLagS(nT-dS,nT-dS),
c    &                 mLagT(nT-dT,nT-dT), mLagNoe(nT-2*nPer,nT-2*nPer)
c    &                 mLagSNoe(nT-dS-2*nPer,nT-dS-2*nPer),
c    &                 mLagTNoe(nT-dT-2*nPer,nT-dT-2*nPer)
c     DOUBLE PRECISION mTemp4(nT,nT),mTemp5(nT-dS,nT-dS),
c    &                 mTemp6(nT-dT,nT-dT), mTemp7(nT-2*nPer,nT-2*nPer)
c     DOUBLE PRECISION mCovIrrNoe(nT-2*nPer,nT-2*nPer),
c    &                 mCovSeaNoe(nT-2*nPer,nT-2*nPer),
c    &                 mCovTreNoe(nT-2*nPer,nT-2*nPer),
c    &                 mCovSANoe(nT-2*nPer,nT-2*nPer)
c     ------------------------------------------------------------------
      INTEGER nSave
      PARAMETER (nSave=POBS*POBS)
      DOUBLE PRECISION mLag(nSave), mLagS(nSave), mLagT(nSave),
     &                 mLagNoe(nSave), mLagSNoe(nSave), mLagTNoe(nSave)
      DOUBLE PRECISION mTemp4(nSave), mTemp5(nSave),
     &                 mTemp6(nSave), mTemp7(nSave)
      DOUBLE PRECISION mCovIrrNoe(nSave), mCovSeaNoe(nSave),
     &                 mCovTreNoe(nSave), mCovSANoe(nSave)
      SAVE mLag, mLagS, mLagT, mLagNoe, mLagSNoe, mLagTNoe,
     &     mTemp4, mCovIrrNoe, mCovSeaNoe, mCovTreNoe, mCovSANoe
      EQUIVALENCE (mTemp4, mTemp5), (mTemp4, mTemp6), (mTemp4, mTemp7)

c-----------------------------------------------------------------------
c     Initialize the outputs.
c-----------------------------------------------------------------------
      DO i = 1,4
        fulEst(i) = ZERO
        fulEso(i) = ZERO
        fulVar(i) = ZERO
        fulDia(i) = ZERO
        noeEst(i) = ZERO
        noeEso(i) = ZERO
        noeVar(i) = ZERO
        noeDia(i) = ZERO
      END DO

c-----------------------------------------------------------------------
c     Calculate model innovation variance adjusted for finite sample.
c-----------------------------------------------------------------------
      innovar = ( sdSig*sdSig )

c-----------------------------------------------------------------------
c     Compute the lag matrix.
c-----------------------------------------------------------------------
c     DO j = 1, nT
c      DO i = 1, nT
c       mLag((j-1)*nT+i) = ZERO
c      END DO
c     END DO
c     DO j = 1, nT
c      IF (lag.eq.0) THEN
c        i = j
c        mLag((j-1)*nT+i) = ONE
c      ELSE
c       i = j+lag
c       IF ((i.ge.1) .and. (i.le.nT)) THEN
c        mLag((j-1)*nT+i) = HALF
c        mLag((i-1)*nT+j) = HALF
c       END IF
c      END IF
c     END DO
c     nLag(1) = nT
c     nLag(2) = nT
c     ------------------------------------------------------------------
      CALL getLagM( nT, lag, ONE, HALF, .true., mLag, nLag )
c     ------------------------------------------------------------------
      CALL getSMat( mLag, nLag, 1, nT-dS, mLagS, nLagS )
      CALL getSMat( mLag, nLag, 1, nT-dT, mLagT, nLagT )
      CALL getSMat( mLag, nLag, nPer+1, nT-nPer, mLagNoe, nLagNoe )
      CALL getSmat( mLag, nLag, nPer+1, nT-dS-nPer, mLagSNoe, nLagSNoe )
      CALL getSMat( mLag, nLag, nPer+1, nT-dT-nPer, mLagTNoe, nLagTNoe )

c-----------------------------------------------------------------------
c     Compute the estimates relative to the innovation variance.
c-----------------------------------------------------------------------
      IF ( nIrrEst(1) .ne. 0 ) THEN
        CALL mulQMatTr( vIrrEst, nIrrEst, mLag, nLag,
     &                  sTemp8(1), nTemp8 )
        fulEst(1) = sTemp8(1)/(innovar*DBLE( nIrrEst(1) ))
      END IF
c     ------------------------------------------------------------------
      CALL getSVec( vIrrEst, nIrrEst, nPer+1, nIrrEst(1)-nPer,
     &              vTemp9, nTemp9 )
      IF ( nTemp9(1) .ne. 0 ) THEN
        CALL mulQMatTr( vTemp9, nTemp9, mLagNoe, nLagNoe,
     &                  sTemp8(1), nTemp8 )
        noeEst(1) = sTemp8(1)/(innovar*DBLE( nTemp9(1) ))
      END IF
c     ------------------------------------------------------------------
      pdA = max(nDelS(2)-nDelS(1)+1, 1)
      CALL mulDMat( dDelS, nDelS, vSeaEst, nSeaEst, vTemp1, nTemp1,
     &              pdA )
      IF ( nTemp1(1) .ne. 0 ) THEN
        CALL mulQMatTr( vTemp1, nTemp1, mLagS, nLagS,
     &                  sTemp8(1), nTemp8 )
        fulEst(2) = sTemp8(1)/(innovar*DBLE( nTemp1(1) ))
      END IF
c     ------------------------------------------------------------------
      CALL getSVec( vTemp1, nTemp1, nPer+1, nTemp1(1)-nPer,
     &              vTemp9, nTemp9 )
      IF ( nTemp9(1) .ne. 0 ) THEN
        CALL mulQMatTr( vTemp9, nTemp9, mLagSNoe, nLagSNoe,
     &                          sTemp8(1), nTemp8 )
        noeEst(2) = sTemp8(1)/(innovar*DBLE( nTemp9(1) ))
      END IF
c     ------------------------------------------------------------------
      pdA = max(nDelt(2)-nDelT(1)+1, 1)
      CALL mulDMat( dDelT, nDelT, vTreEst, nTreEst, vTemp2, nTemp2,
     &              pdA )
      IF ( nTemp2(1) .ne. 0 ) THEN
        CALL mulQMatTr( vTemp2, nTemp2, mLagT, nLagT,
     &                  sTemp8(1), nTemp8 )
        fulEst(3) = sTemp8(1)/(innovar*DBLE( nTemp2(1) ))
      END IF
c     ------------------------------------------------------------------
      CALL getSVec( vTemp2, nTemp2, nPer+1, nTemp2(1)-nPer,
     &              vTemp9, nTemp9 )
      IF ( nTemp9(1) .ne. 0 ) THEN
        CALL mulQMatTr( vTemp9, nTemp9, mLagTNoe, nLagTNoe,
     &                  sTemp8(1), nTemp8 )
        noeEst(3) = sTemp8(1)/(innovar*DBLE( nTemp9(1) ))
      END IF
c     ------------------------------------------------------------------
      CALL addMat( vTreEst, nTreEst, vIrrEst, nIrrEst, vTemp3, nTemp3 )
      pdA = max(nDelT(2)-nDelT(1)+1, 1)
      CALL mulDMat( dDelT, nDelT, vTemp3, nTemp3, vTemp2, nTemp2, pdA )
      IF ( nTemp2(1) .ne. 0 ) THEN
        CALL mulQMatTr( vTemp2, nTemp2, mLagT, nLagT,
     &                  sTemp8(1), nTemp8 )
        fulEst(4) = sTemp8(1)/(innovar*DBLE( nTemp2(1) ))
      END IF
c     ------------------------------------------------------------------
      CALL getSVec( vTemp2, nTemp2, nPer+1, nTemp2(1)-nPer,
     &              vTemp9, nTemp9 )
      IF (nTemp9(1) .ne. 0 ) THEN
        CALL mulQMatTr( vTemp9, nTemp9, mLagTNoe, nLagTNoe,
     &                  sTemp8(1), nTemp8 )
        noeEst(4) = sTemp8(1)/(innovar*DBLE( nTemp9(1) ))
      END IF

c-----------------------------------------------------------------------
c     Get some covariance submatrices
c-----------------------------------------------------------------------
      CALL getSMat( mCovIrr, nCovIrr, nPer+1, nCovIrr(1)-nPer, 
     &              mCovIrrNoe, nCovIrrNoe )
      CALL getSMat( mCovSea, nCovSea, nPer+1, nCovSea(1)-nPer, 
     &              mCovSeaNoe, nCovSeaNoe )
      CALL getSMat( mCovTre, nCovTre, nPer+1, nCovTre(1)-nPer, 
     &              mCovTreNoe, nCovTreNoe )
      CALL getSMat( mCovSA, nCovSA, nPer+1, nCovSA(1)-nPer, 
     &              mCovSANoe, nCovSANoe )

c-----------------------------------------------------------------------
c     Compute some traces of matrices and their squares.
c-----------------------------------------------------------------------
      CALL mulMat( mCovIrr, nCovIrr, mLag, nLag, mTemp4, nTemp4 )
      trCovIrr = getTrc( mTemp4, nTemp4 )
      trCovIrrSq = getTrcAB( mTemp4, nTemp4, mTemp4, nTemp4 )
c     ------------------------------------------------------------------
      CALL mulMat( mCovSea, nCovSea, mLagS, nLagS, mTemp5, nTemp5 )
      trCovSea = getTrc( mTemp5, nTemp5 )
      trCovSeaSq = getTrcAB( mTemp5, nTemp5, mTemp5, nTemp5 )
c     ------------------------------------------------------------------
      CALL mulMat( mCovTre, nCovTre, mLagT, nLagT, mTemp6, nTemp6 )
      trCovTre = getTrc( mTemp6, nTemp6 )
      trCovTreSq = getTrcAB( mTemp6, nTemp6, mTemp6, nTemp6 )
c     ------------------------------------------------------------------
      CALL mulMat( mCovSA, nCovSA, mLagT, nLagT, mTemp6, nTemp6 )
      trCovSA = getTrc( mTemp6,  nTemp6  )
      trCovSASq = getTrcAB( mTemp6, nTemp6, mTemp6, nTemp6 )
c     ------------------------------------------------------------------
      CALL mulMat( mCovIrrNoe, nCovIrrNoe, mLagNoe, nLagNoe,
     &             mTemp7, nTemp7 )
      trCovIrrNoe = getTrc( mTemp7, nTemp7 )
      trCovIrrNoeSq = getTrcAB( mTemp7, nTemp7, mTemp7, nTemp7 )
c     ------------------------------------------------------------------
      CALL mulMat( mCovSeaNoe, nCovSeaNoe, mLagSNoe, nLagSNoe,
     &             mTemp7, nTemp7 )
      trCovSeaNoe = getTrc( mTemp7, nTemp7 )
      trCovSeaNoeSq = getTrcAB( mTemp7, nTemp7, mTemp7, nTemp7 )
c     ------------------------------------------------------------------
      CALL mulMat( mCovTreNoe, nCovTreNoe, mLagTNoe, nLagTNoe,
     &             mTemp7, nTemp7 )
      trCovTreNoe = getTrc( mTemp7, nTemp7 )
      trCovTreNoeSq = getTrcAB( mTemp7, nTemp7, mTemp7, nTemp7 )
c     ------------------------------------------------------------------
      CALL mulMat( mCovSANoe, nCovSANoe, mLagTNoe, nLagTNoe,
     &             mTemp7, nTemp7 )
      trCovSANoe = getTrc( mTemp7,  nTemp7  )
      trCovSANoeSq = getTrcAB( mTemp7, nTemp7, mTemp7, nTemp7 )

c-----------------------------------------------------------------------
c     Compute the full estimators, null estimators, and diagnostics.
c-----------------------------------------------------------------------
      k1 = TWO
      k3 = (TWO*finfact - finfact**2)/DBLE(nT-dS-dT)

c     ------------------------------------------------------------------
c     For the irregular component (for input lag)
c     ------------------------------------------------------------------
      IF ( nCovIrr(1) .ne. 0 ) THEN
        fulEso(1) = trCovIrr/DBLE(nCovIrr(1))
        fulVar(1) = k1*(trCovIrrSq - k3*(trCovIrr**2))
     &            /DBLE(nCovIrr(1)**2)
        IF ( .not. dpeq(fulVar(1),ZERO) ) THEN
          fulDia(1) = (fulEst(1) - fulEso(1))/DSQRT(fulVar(1))
        END IF
      END IF
      
c     ------------------------------------------------------------------
c     For the seasonal component (for input lag)
c     ------------------------------------------------------------------
      IF ( nCovSea(1) .ne. 0 ) THEN
        fulEso(2) = trCovSea/DBLE(nCovSea(1))
        fulVar(2) = k1*(trCovSeaSq - k3*(trCovSea**2))
     &            /DBLE(nCovSea(1)**2)
        IF ( .not. dpeq(fulVar(2),ZERO) ) THEN
          fulDia(2) = (fulEst(2) - fulEso(2))/DSQRT(fulVar(2))
        END IF
      END IF
      
c     ------------------------------------------------------------------
c     For the trend component (for input lag)
c     ------------------------------------------------------------------
      IF ( nCovTre(1) .ne. 0 ) THEN
        fulEso(3) = trCovTre/DBLE(nCovTre(1))
        fulVar(3) = k1*(trCovTreSq - k3*(trCovTre**2))
     &            /DBLE(nCovTre(1)**2)
        IF ( .not. dpeq(fulVar(3),ZERO) ) THEN
          fulDia(3) = (fulEst(3) - fulEso(3))/DSQRT(fulVar(3))
        END IF
      END IF
      
c     ------------------------------------------------------------------
c     For the seasonal adjustment (for input lag)
c     ------------------------------------------------------------------
      IF ( nCovSA(1) .ne. 0 ) THEN
        fulEso(4) = trCovSA/DBLE(nCovSA(1))
        fulVar(4) = k1*(trCovSASq -  k3*(trCovSA**2))
     &            /DBLE(nCovSA(1)**2)
        IF ( .not. dpeq(fulVar(4),ZERO) ) THEN
          fulDia(4) = (fulEst(4) - fulEso(4))/DSQRT(fulVar(4))
        END IF
      END IF

c-----------------------------------------------------------------------
c     Compute the noend estimators, null estimators, and diagnostics.
c-----------------------------------------------------------------------
      k2 = TWO
c     k3 = (TWO*finfact - finfact**2)/DBLE(nT-dS-dT)

c     ------------------------------------------------------------------
c     For the irregular component (for input lag)
c     ------------------------------------------------------------------
      IF ( nCovIrrNoe(1) .ne. 0 ) THEN
        noeEso(1) = trCovIrrNoe/DBLE(nCovIrrNoe(1))
        noeVar(1) = k2*(trCovIrrNoeSq - k3*(trCovIrrNoe**2))
     &                    /DBLE(nCovIrrNoe(1)**2)
        IF ( .not. dpeq(noeVar(1),ZERO) ) THEN
          noeDia(1) = (noeEst(1) - noeEso(1))/DSQRT(noeVar(1))
        END IF
      END IF

c     ------------------------------------------------------------------
c     For the seasonal component (for input lag)
c     ------------------------------------------------------------------
      IF ( nCovSeaNoe(1) .ne. 0 ) THEN
        noeEso(2) = trCovSeaNoe/DBLE(nCovSeaNoe(1))
        noeVar(2) = k2*(trCovSeaNoeSq - k3*(trCovSeaNoe**2))
     &                    /DBLE(nCovSeaNoe(1)**2)
        IF ( .not. dpeq(noeVar(2),ZERO) ) THEN
          noeDia(2) = (noeEst(2) - noeEso(2))/DSQRT(noeVar(2))
        END IF
      END IF


c     ------------------------------------------------------------------
c     For the trend component (for input lag)
c     ------------------------------------------------------------------
      IF ( nCovTreNoe(1) .ne. 0 ) THEN
        noeEso(3) = trCovTreNoe/DBLE(nCovTreNoe(1))
        noeVar(3) = k2*(trCovTreNoeSq - k3*(trCovTreNoe**2))
     &                    /DBLE(nCovTreNoe(1)**2)
        IF ( .not. dpeq(noeVar(3),ZERO) ) THEN
          noeDia(3) = (noeEst(3) - noeEso(3))/DSQRT(noeVar(3))
        END IF
      END IF

c     ------------------------------------------------------------------
c     For the seasonal adjustment (for input lag)
c     ------------------------------------------------------------------
      IF ( nCovSANoe(1) .ne. 0 ) THEN
        noeEso(4) = trCovSANoe/DBLE(nCovSANoe(1))
        noeVar(4) = k2*(trCovSANoeSq -  k3*(trCovSANoe**2))
     &                    /DBLE(nCovSANoe(1)**2)
        IF ( .not. dpeq(noeVar(4),ZERO) ) THEN
          noeDia(4) = (noeEst(4) - noeEso(4))/DSQRT(noeVar(4))
        END IF
      END IF

c     ------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE getWghLagDia( lag, nT, dS, dT,  nPer,
     &                         finfact, sdSig, vY, dDel, nDel,
     &                         dRedDelS, nRedDelS, dRedDelT, nRedDelT,
     &                         mSigUI, nSigUI, mSigUS, nSigUS,
     &                         mSigUT, nSigUT, mSigWT, nSigWT,
     &                         mInvSigW, nInvSigW,
     &                         wghEst, wghEso, wghVar, wghDia )
c-----------------------------------------------------------------------
c Release 1, Subroutine Version 1.1, Modified 01 May 2006.
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 07 Apr 2006.
c     Modified by REG, on 01 May 2006, to calculate the estimate 
c       and variance relative to the innovation variance, in order 
c       to match SEATS output, and to remove finite factor processing
c       associated with the estimated innovation variance, eliminating
c       the need for input nParam, nFixed, and nDiff.
c-----------------------------------------------------------------------
c     This subroutine calculates some weighted autocovariance
c     diagnostics for lags: 0, 1, nPer. For each diagnostic,
c     an estimate, estimator, and variance are calculated relative
c     to the innovation variance.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c dDel    d   diagonal form of full differencing matrix: mDel
c dRedDelS d  diagonal form of reduced seasonal differencing matrix:
c             mRedDelS
c dRedDelT d  diagonal form of reduced trend differencing matrix:
c             mRedDelT
c dS      i   size of Seasonal Differencing
c dT      i   size of Trend Differencing
c finfact d   finite sample correction factor
c lag     i   lag of diagnostics to calculate
c mInvSigW d  inverse of covariance matrix for differenced data
c mRedDelS d  reduced seasonal differencing matrix
c mRedDelT d  reduced trend differencing matrix
c mSigUI  d   covariance matrix for undifferenced irregular
c mSigUS  d   covariance matrix for differenced seasonal
c mSigUT  d   covariance matrix for differenced trend
c mSigWS  d   covariance matrix for differenced trend adjusted
c mSigWT  d   covariance matrix for differenced seasonally adjusted
c nDel    i   size (rows,columns) of mDel
c nInvSigW i  size (rows,columns) of mInvSigW matrix
c nPer    i   size of seasonal period
c nRedDelS i  size (rows,columns) of mDelS
c nRedDelT i  size (rows,columns) of mDelT
c nSigUI  i   size (rows,columns) of mSigUI matrix
c nSigUS  i   size (rows,columns) of mSigUS matrix
c nSigUT  i   size (rows,columns) of mSigUT matrix
c nSigWS  i   size (rows,columns) of mSigWS matrix
c nSigWT  i   size (rows,columns) of mSigWT matrix
c nT      i   size of data available
c sdSig   d   estimated data innovation stdev adjusted for number of
c             estimated model parameters
c vY      d   vector of observations
c wghDia  d   vector of normalized diagnostics from weighted signals
c             for irregular, seasonal, trend, and SA
c wghEso  d   vector of null means of estimates from weighted signals
c             for irregular, seasonal, trend, and SA
c wghEst  d   vector of diagnostic estimates from weighted signals
c             for irregular, seasonal, trend, and SA
c wghVar  d   vector of variances of diagnostics from weighted signals
c             for irregular, seasonal, trend, and SA
c-----------------------------------------------------------------------
c Name   Type Description (Local Variables)
c-----------------------------------------------------------------------
c dpeq    l   external function reference
c i       i   index variables
c innovar d   model innovation variance adjusted for finite sample
c getTrc  d   external function reference
c getTrcAB d  external function reference
c k1,k3   d   miscelaneous constants
c mB      d   working matrix used to calculate estimators and variances
c mLag    d   selection matrix (using all data)
c mTemp1  d   working matrix used to calculate mB matrix and estimates
c mTemp2  d   working matrix used to calculate mB matrix
c nB      i   size (rows,columns) of mB matrix
c nLag    i   size (rows,columns) of mLag matrix
c nResult
c nSave   i   identifies default size of large matrices
c             that are saved (not dynamic)
c nTemp1  i   size (rows,columns) of mTemp1 matrix
c nTemp2  i   size (rows,columns) of mTemp1 matrix
c nW      i   size (rows,columns) of mW matrix
c nY      i   size (rows,columns) of vY vector
c ONE     d   constant parameter
c sResult d   scalar result of quadratic matrix operation
c trB     d   trace of mB matrix
c trB2    d   trace of mB squared matrix
c TWO     d   constant paramenter
c vW      d   vector of differenced data
c ZERO    d   constant parameter
c-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'srslen.prm'
c     ------------------------------------------------------------------
c     added by BCM to correctly dimension variables 
c     ------------------------------------------------------------------
      INTEGER pdA
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER dS, dT, lag, nPer, nT
      INTEGER nDel(2), nRedDelS(2), nRedDelT(2)
      INTEGER nSigUI(2), nSigUS(2), nSigUT(2), nSigWT(2), nInvSigW(2)
      DOUBLE PRECISION sdSig, vY(nT)
      DOUBLE PRECISION dDel(dS+dT+1), dRedDelS(dS+1), dRedDelT(dT+1)
      DOUBLE PRECISION mSigUI(nT,nT), mSigUS(nT-dS,nT-dS),
     &                 mSigUT(nT-dT,nT-dT), mSigWT(nT-dT,nT-dT),
     &                 mInvSigW(nT-dS-dT,nT-dS-dT)
      DOUBLE PRECISION wghEst(4), wghEso(4), wghVar(4), wghDia(4)
c     DOUBLE PRECISION wghAltVar(4), wghAltDia(4)

c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i, nSave
      INTEGER nB(2), nLag(2), nResult(2), nTemp1(2), nTemp2(2), nW(2),
     &        nY(2)
      DOUBLE PRECISION finfact, innovar, k1, k3, ONE, trB, trB2,
     &                 vW(nT-dS-dT), sResult(1), TWO, ZERO
      DOUBLE PRECISION getTrc, getTrcAB
      PARAMETER (nSave=POBS*POBS, ONE=1.0d0, TWO=2.0d0, ZERO=0.0d0)
      LOGICAL dpeq
c     DOUBLE PRECISION wghAltVar(4), wghAltDia(4)
      
      DOUBLE PRECISION mB(nSave), mLag(nSave), mTemp1(nSave),
     &                 mTemp2(nSave)
      SAVE mB, mLag, mTemp1, mTemp2

c-----------------------------------------------------------------------
c     Initialize the outputs.
c-----------------------------------------------------------------------
      DO i = 1, 4
       wghEst(i) = ZERO
       wghEso(i) = ZERO
       wghVar(i) = ZERO
       wghDia(i) = ZERO
c      wghAltVar(i) = ZERO
c      wghAltDia(i) = ZERO
      END DO
c     ------------------------------------------------------------------
c     Identify size of observation vector.
c     ------------------------------------------------------------------
      nY(1) = nT
      nY(2) = 1

c-----------------------------------------------------------------------
c     Calculate model innovation variance adjusted for finite sample.
c-----------------------------------------------------------------------
      innovar = ( sdSig*sdSig )
c     ------------------------------------------------------------------
      k1 = TWO
      k3 = (TWO*finfact - finfact**2)/DBLE(nT-dS-dT)

c-----------------------------------------------------------------------
c     Compute the weighted estimate, estimator, variance,
c     and diagnostic, for the irregular component.
c-----------------------------------------------------------------------
c     Calculate the mB matrix used to calculate the estimator, variance.
c     ------------------------------------------------------------------
      CALL getLagM( nT, lag, ONE, ONE, .false., mLag, nLag )
      CALL mulMat( mSigUI, nSigUI, mLag, nLag, mTemp1, nTemp1 )
      CALL getSymM( mTemp1, nTemp1, mTemp2, nTemp2 )
      CALL mulQDMat( dDel, nDel, mTemp2, nTemp2,
     &               mTemp1, nTemp1 )
      CALL mulMat( mInvSigW, nInvSigW, mTemp1, nTemp1, mB, nB )
c     ------------------------------------------------------------------
      trB  = getTrc( mB, nB )
      trB2 = getTrcAB( mB, nB, mB, nB )
c     ------------------------------------------------------------------
c     Calculate the estimate relative to the innovation variance.
c     ------------------------------------------------------------------
      pdA = max(nDel(2)-nDel(1)+1, 1)
      CALL mulDMat( dDel, nDel, vY, nY, vW, nW, pdA )
      CALL mulMat( mB, nB, mInvSigW, nInvSigW, mTemp1, nTemp1 )
      CALL mulQMatTr( vW, nW, mTemp1, nTemp1, sResult, nResult )
      IF ( nResult(1) .eq. 1 ) THEN
       wghEst(1) = sResult(1)/(innovar*DBLE(nW(1)))
      END IF
c     ------------------------------------------------------------------
c     Calculate the estimator and variance relative to the innovation
c     variance, and then calculate the diagostic.
c     ------------------------------------------------------------------
      IF ( nB(1) . gt. 0 ) THEN
        wghEso(1) = trB/DBLE(nB(1))
        wghVar(1) = k1*(trB2 - k3*(trB**2))/DBLE(nB(1)**2)
c       wghAltVar(1) = k1*trB2/DBLE(nB(1)**2)
        IF ( .not. dpeq(wghVar(1),ZERO) ) THEN
          wghDia(1) = (wghEst(1) - wghEso(1))/DSQRT(wghVar(1))
c         wghAltDia(1) = (wghEst(1) - innovar*wghEso(1))
c    &                 /DSQRT(wghAltVar(1))
        END IF
      END IF

c-----------------------------------------------------------------------
c     Compute the weighted estimate, estimator, variance,
c     and diagnostic, for the seasonal component.
c-----------------------------------------------------------------------
c     Calculate the mB matrix used to calculate the estimator, variance.
c     ------------------------------------------------------------------
      CALL getLagM( nT-dS, lag, ONE, ONE, .false., mLag, nLag )
      CALL mulMat( mSigUS, nSigUS, mLag, nLag, mTemp1, nTemp1 )
      CALL getSymM( mTemp1, nTemp1, mTemp2, nTemp2 )
      CALL mulQDMat( dRedDelT, nRedDelT, mTemp2, nTemp2,
     &               mTemp1, nTemp1 )
      CALL mulMat( mInvSigW, nInvSigW, mTemp1, nTemp1, mB, nB )
c     ------------------------------------------------------------------
      trB  = getTrc( mB, nB )
      trB2 = getTrcAB( mB, nB, mB, nB )
c     ------------------------------------------------------------------
c     Calculate the estimate relative to the innovation variance.
c     ------------------------------------------------------------------
c     CALL mulDMat( dDel, nDel, vY, nY, vW, nW )
      CALL mulMat( mB, nB, mInvSigW, nInvSigW, mTemp1, nTemp1 )
      CALL mulQMatTr( vW, nW, mTemp1, nTemp1, sResult, nResult )
      IF ( nResult(1) .eq. 1 ) THEN
       wghEst(2) = sResult(1)/(innovar*DBLE(nW(1)))
      END IF
c     ------------------------------------------------------------------
c     Calculate the estimator and variance relative to the innovation
c     variance, and then calculate the diagostic.
c     ------------------------------------------------------------------
      IF ( nB(1) . gt. 0 ) THEN
        wghEso(2) = trB/DBLE(nB(1))
        wghVar(2) = k1*(trB2 - k3*(trB**2))/DBLE(nB(1)**2)
c       wghAltVar(2) = k1*trB2/DBLE(nB(1)**2)
        IF ( .not. dpeq(wghVar(2),ZERO) ) THEN
          wghDia(2) = (wghEst(2) - innovar*wghEso(2))/DSQRT(wghVar(2))
c         wghAltDia(2) = (wghEst(2) - innovar*wghEso(2))
c    &                 /DSQRT(wghAltVar(2))
        END IF
      END IF

c-----------------------------------------------------------------------
c     Compute the weighted estimate, estimator, variance,
c     and diagnostic, for the trend component.
c-----------------------------------------------------------------------
c     Calculate the mB matrix used to calculate the estimator, variance.
c     ------------------------------------------------------------------
      CALL getLagM( nT-dT, lag, ONE, ONE, .false., mLag, nLag )
      CALL mulMat( mSigUT, nSigUT, mLag, nLag, mTemp1, nTemp1 )
      CALL getSymM( mTemp1, nTemp1, mTemp2, nTemp2 )
      CALL mulQDMat( dRedDelS, nRedDelS, mTemp2, nTemp2,
     &               mTemp1, nTemp1 )
      CALL mulMat( mInvSigW, nInvSigW, mTemp1, nTemp1, mB, nB )
c     ------------------------------------------------------------------
      trB  = getTrc( mB, nB )
      trB2 = getTrcAB( mB, nB, mB, nB )
c     ------------------------------------------------------------------
c     Calculate the estimate relative to the innovation variance.
c     ------------------------------------------------------------------
c     CALL mulDMat( dDel, nDel, vY, nY, vW, nW )
      CALL mulMat( mB, nB, mInvSigW, nInvSigW, mTemp1, nTemp1 )
      CALL mulQMatTr( vW, nW, mTemp1, nTemp1, sResult, nResult )
      IF ( nResult(1) .eq. 1 ) THEN
       wghEst(3) = sResult(1)/(innovar*DBLE(nW(1)))
      END IF
c     ------------------------------------------------------------------
c     Calculate the estimator and variance relative to the innovation
c     variance, and then calculate the diagostic.
c     ------------------------------------------------------------------
      IF ( nB(1) . gt. 0 ) THEN
        wghEso(3) = trB/DBLE(nB(1))
        wghVar(3) = k1*(trB2 - k3*(trB**2))/DBLE(nB(1)**2)
c       wghAltVar(3) = k1*trB2/DBLE(nB(1)**2)
        IF ( .not. dpeq(wghVar(3),ZERO) ) THEN
          wghDia(3) = (wghEst(3) - wghEso(3))/DSQRT(wghVar(3))
c         wghAltDia(3) = (wghEst(3) - innovar*wghEso(3))
c    &                 /DSQRT(wghAltVar(3))
        END IF
      END IF

c-----------------------------------------------------------------------
c     Compute the weighted estimate, estimator, variance,
c     and diagnostic, for the seasonally adjusted component.
c-----------------------------------------------------------------------
c     Calculate the mB matrix used to calculate the estimator, variance.
c     ------------------------------------------------------------------
c     CALL getLagM( nT-dT, lag, ONE, ONE, .false., mLag, nLag )
      CALL mulMat( mSigWT, nSigWT, mLag, nLag, mTemp1, nTemp1 )
      CALL getSymM( mTemp1, nTemp1, mTemp2, nTemp2 )
      CALL mulQDMat( dRedDelS, nRedDelS, mTemp2, nTemp2,
     &               mTemp1, nTemp1 )
      CALL mulMat( mInvSigW, nInvSigW, mTemp1, nTemp1, mB, nB )
c     ------------------------------------------------------------------
      trB  = getTrc( mB, nB )
      trB2 = getTrcAB( mB, nB, mB, nB )
c     ------------------------------------------------------------------
c     Calculate the estimate relative to the innovation variance.
c     ------------------------------------------------------------------
c     CALL mulDMat( dDel, nDel, vY, nY, vW, nW )
      CALL mulMat( mB, nB, mInvSigW, nInvSigW, mTemp1, nTemp1 )
      CALL mulQMatTr( vW, nW, mTemp1, nTemp1, sResult, nResult )
      IF ( nResult(1) .eq. 1 ) THEN
       wghEst(4) = sResult(1)/(innovar*DBLE(nW(1)))
      END IF
c     ------------------------------------------------------------------
c     Calculate the estimator and variance relative to the innovation
c     variance, and then calculate the diagostic.
c     ------------------------------------------------------------------
      IF ( nB(1) . gt. 0 ) THEN
        wghEso(4) = trB/DBLE(nB(1))
        wghVar(4) = k1*(trB2 - k3*(trB**2))/DBLE(nB(1)**2)
c       wghAltVar(4) = k1*trB2/DBLE(nB(1)**2)
        IF ( .not. dpeq(wghVar(4),ZERO) ) THEN
          wghDia(4) = (wghEst(4) - wghEso(4))/DSQRT(wghVar(4))
c         wghAltDia(4) = (wghEst(4) - innovar*wghEso(4))
c    &                 /DSQRT(wghAltVar(4))
        END IF
      END IF

c     ------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE getLagM( nRC, hLag, sC0, sCh, lSym, mL, nL )
c-----------------------------------------------------------------------
c Release 1, Subroutine Version 1.0, Created 07 Apr 2006.
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 07 Apr 2006.
c-----------------------------------------------------------------------
c     This subroutine creates a symetric or non-symetric selection 
c     matrix.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c hLag    i   lag value denotes non-zero diagonal
c lSym    l   t = create symetric selection matrix
c mL      d   output selection matrix
c nL      i   size (rows,columns) of mL matrix
c nRC     i   size of square selection matrix to create
c sC0     d   main diagonal value for selection matrix
c sCh     d   off diagonal value for selection matrix
c-----------------------------------------------------------------------
c Name   Type Description (Local Variables)
c-----------------------------------------------------------------------
c i,j     i   index variables
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER nRC, hLag, nL(2)
      DOUBLE PRECISION mL(nRC,nRC), sC0, sCh, ZERO
      PARAMETER (ZERO=0.0d0)
      LOGICAL lSym
      
c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i, j
      
c-----------------------------------------------------------------------
c     Initialize the selection matrix to zeros.
c-----------------------------------------------------------------------
      DO j = 1, nRC
       DO i = 1, nRC
        mL(i,j) = ZERO
       END DO
      END DO

c-----------------------------------------------------------------------
c     Update the non-zero entries in the selection matrix.
c-----------------------------------------------------------------------
      DO j = 1, nRC
c     ------------------------------------------------------------------
c     Check for updating the main diagonal and use main diagonal value.
c     ------------------------------------------------------------------
       IF ( hLag .eq. 0 ) THEN
        mL(j,j) = sC0

c     ------------------------------------------------------------------
c     Else for updating off diagonals use off diagonal value.
c     ------------------------------------------------------------------
       ELSE
        i = j + hLag
        IF ((i .ge. 1) .and. (i .le. nRC )) THEN
         mL(i,j) = sCh
c     ------------------------------------------------------------------
c     Check for creating symetric selection matrix.
c     ------------------------------------------------------------------
         IF ( lSym ) THEN
          mL(j,i) = sCh
         END IF
        END IF
       END IF
      END DO
      
c-----------------------------------------------------------------------
c     Identify size of selection matrix.
c-----------------------------------------------------------------------
      nL(1) = nRC
      nL(2) = nRC
      
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE getSymM( mNonSymM, nNonSymM, mSymM, nSymM )
c-----------------------------------------------------------------------
c Release 1, Subroutine Version 1.0, Created 07 Apr 2006.
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 07 Apr 2006.
c-----------------------------------------------------------------------
c     This subroutine creates a symetric version of an input
c     non-symetric matrix using mSymM = (mNonSymM + mNonSymM')/2 .
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c mNonSymM d  input non-symetric matrix 
c mSymM    d  output symetric matrix
c nNonSymM i  size (rows,columns) of mNonSymM matrix
c nSymM    i  size (rows,columns) of mSymM matrix
c-----------------------------------------------------------------------
c Name   Type Description (Local Variables)
c-----------------------------------------------------------------------
c i,j     i   index variables
c TWO     d   constant parameter
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER nNonSymM(2), nSymM(2)
      DOUBLE PRECISION mNonSymM( nNonSymM(1), nNonSymM(2) ),
     &                 mSymM( nNonSymM(1), nNonSymM(2) )
      
c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i,j
      DOUBLE PRECISION TWO
      PARAMETER (TWO=2.0d0)
      
c-----------------------------------------------------------------------
c     Create symetric matrix if square and size is positive.
c-----------------------------------------------------------------------
      IF (( nNonSymM(1) .eq. nNonSymM(2) ) .and. 
     &    ( nNonSymM(1) .gt. 0 )) THEN
       DO i = 1, nNonSymM(1)
        DO j = 1, nNonSymM(2)
         mSymM(i,j) = ( mNonSymM(i,j) + mNonSymM(j,i) )/TWO
        END DO
       END DO
      
c     ------------------------------------------------------------------
c     Identify size of symetric matrix.
c     ------------------------------------------------------------------
       nSymM(1) = nNonSymM(1)
       nSymM(2) = nNonSymM(2)

c-----------------------------------------------------------------------
c     Else declare invalid result.
c-----------------------------------------------------------------------
      ELSE
       nSymM(1) = 0
       nSymM(2) = 0
      END IF
      
      RETURN
      END
      