      SUBROUTINE compDiag( nT, dS, dT, nPer, nParam, nFixed, nDiff,
     &                     sdSig,  sdSigAlt, vIrrEst, nIrrEst,
     &                     vSeaEst, nSeaEst, vTreEst, nTreEst,
     &                     dDelS, nDelS, dDelT, nDelT,
     &                     mCovIrr, nCovIrr, mCovSea, nCovSea,
     &                     mCovTre, nCovTre, mCovSA, nCovSA,
     &                     fulEst, noeEst, fulEso, noeEso,
     &                     fulVar, noeVar, fulDia, noeDia )
c-----------------------------------------------------------------------
c     compDiag.f, Release 1, Subroutine Version 1.1, Modified 13 Apr 2006.
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 14 Apr 2005.
c     Modified by REG, on 13 Jan 2006, to optimize matrix processing,
c       by using getTrcAB utility, and by using diagonal matrix
c       utilities; to clean up tab stops.
c-----------------------------------------------------------------------
c     This subroutine calculates some diagnostics.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c dDelS   d   diagonal form of seasonal differencing matrix: mDelS
c dDelT   d   diagonal form of trend differencing matrix: mDelT
c dS      i   size of Seasonal Differencing
c dT      i   size of Trend Differencing
c fulDia  d   vector of normalized diagnostics from full signals
c             for irregular, seasonal, trend, and SA
c fulEso  d   vector of null means of estimates from full signals
c             for irregular, seasonal, trend, and SA
c fulEst  d   vector of diagnostic estimates from full signals
c             for irregular, seasonal, trend, and SA
c fulVar  d   vector of variances of diagnostics from full signals
c             for irregular, seasonal, trend, and SA
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
c nDiff   i   vector of (d,D) differencing orders
c nFixed  i   number of fixed parameters
c nPer    i   size of seasonal period
c nParam  i   number of parameters
c nT      i   size of data available
c noeDia  d   vector of normalized diagnostics from trimmed signals
c             for irregular, seasonal, trend, and SA
c noeEso  d   vector of null means of estimate from trimmed signals
c             for irregular, seasonal, trend, and SA
c noeEst  d   vector of diagnostic estimates from trimmed signals
c             for irregular, seasonal, trend, and SA
c noeVar  d   vector of variances of diagnostics from trimmed signals
c             for irregular, seasonal, trend, and SA
c sdSig   d   data innovation stdev
c sdSigAlt d  alternate data innovation stdev when parameters are fixed
c vIrrEst d   estimated irregular
c vSeaEst d   estimated seasonal
c vTreEst d   estimated trend
c-----------------------------------------------------------------------
c Name   Type Description (Local Variables)
c-----------------------------------------------------------------------
c finfact d   finite sample correction factor
c getTrc  d   external function reference
c innovar d   model innovation variance adjusted for finite sample
c k1,k2,k3 d  miscellaneous constants
c mCovIrrNoe d no-end irregular covariance matrix
c mCovSeaNoe d no-end seasonal covariance matrix
c mCovTreNoe d no-end trend covariance matrix
c mCovSANoe d no-end SA covariance matrix
c nCovIrrNoe d size (rows,columns) of mCovIrrNoe matrix
c nCovSeaNoe d size (rows,columns) of mCovSeaNoe matrix
c nCovTreNoe d size (rows,columns) of mCovTreNoe matrix
c nCovSANoe d size (rows,columns) of mCovSANoe matrix
c nSave   i   identifies default size of large matrices
c             that are saved (not dynamic)
c nTemp1  d   size (rows,columns) of vTemp1 vector
c nTemp2  d   size (rows,columns) of vTemp2 vector
c nTemp3  d   size (rows,columns) of vTemp3 vector
c ONE     d   constant paramenter
c sumsqr  d   external function reference
c trCovIrr d  trace of mCovIrr matrix
c trCovIrrNoe d trace of mCovIrrNoe matrix
c trCovSea d  trace of mCovSea matrix
c trCovSeaNoe d trace of mCovSeaNoe matrix
c trCovTre d  trace of mCovTre matrix
c trCovTreNoe d trace of mCovTreNoe matrix
c trCovSA  d  trace of mCovSA matrix
c trCovSANoe d trace of mCovSANoe matrix
c trCovIrrSq d trace of mCovIrr squared matrix
c trCovIrrNoeSq d trace of mCovIrrNoe squared matrix
c trCovSeaSq d trace of mCovSea squared matrix
c trCovSeaNoeSq d trace of mCovSeaNoe squared matrix
c trCovTreSq d trace of mCovTre squared matrix
c trCovTreNoeSq d trace of mCovTreNoe squared matrix
c trCovSASq d  trace of mCovSA squared matrix
c trCovSANoeSq d trace of mCovSANoe squared matrix
c vTemp1  d   temporary vector 1
c vTemp2  d   temporary vector 2
c vTemp3  d   temporary vector 3
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
      INTEGER nT, dS, dT, nPer, nParam, nFixed, nDiff(2)
      INTEGER nIrrEst(2), nSeaEst(2), nTreEst(2)
      INTEGER nDelS(2), nDelT(2)
      INTEGER nCovIrr(2), nCovSea(2), nCovTre(2), nCovSA(2)
      DOUBLE PRECISION sdSig, sdSigAlt
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
      INTEGER i
      INTEGER nTemp1(2), nTemp2(2), nTemp3(2)
      INTEGER nCovIrrNoe(2), nCovSeaNoe(2), nCovTreNoe(2), nCovSANoe(2)
      DOUBLE PRECISION finfact, innovar
      DOUBLE PRECISION vTemp1(nT-dS), vTemp2(nT-dT), vTemp3(nT)
      DOUBLE PRECISION trCovIrr, trCovIrrNoe, trCovSea, trCovSeaNoe,
     &                 trCovTre, trCovTreNoe, trCovSA,  trCovSANoe
      DOUBLE PRECISION trCovIrrSq, trCovIrrNoeSq, trCovSeaSq,
     &                 trCovSeaNoeSq, trCovTreSq, trCovTreNoeSq,
     &                 trCovSASq, trCovSANoeSq
      DOUBLE PRECISION k1, k2, k3, TWO, ZERO
      DOUBLE PRECISION getTrc, getTrcAB,  sumsqr
      PARAMETER (ZERO=0.0D0, TWO=2.0D0)
      LOGICAL dpeq

c     ------------------------------------------------------------------
c     Dynamic (commented) versus static (uncommented) matrices
c     ------------------------------------------------------------------
c     DOUBLE PRECISION mCovIrrNoe(nT-2*nPer,nT-2*nPer),
c    &                 mCovSeaNoe(nT-2*nPer,nT-2*nPer),
c    &                 mCovTreNoe(nT-2*nPer,nT-2*nPer),
c    &                 mCovSANoe(nT-2*nPer,nT-2*nPer)
c     ------------------------------------------------------------------
      INTEGER nSave
      PARAMETER (nSave=POBS*POBS)
      DOUBLE PRECISION mCovIrrNoe(nSave), mCovSeaNoe(nSave),
     &                 mCovTreNoe(nSave), mCovSANoe(nSave)
      SAVE mCovIrrNoe, mCovSeaNoe, mCovTreNoe, mCovSANoe

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
c     Calculate finite sample correction factor and 
c     model innovation variance adjusted for finite sample.
c-----------------------------------------------------------------------
      finfact = DBLE(nT - ( nDiff(1) + nPer*nDiff(2) ))/
     &      DBLE(nT - ( nDiff(1) + nPer*nDiff(2) + (nParam - nFixed) ))
      innovar = finfact*( sdSigAlt*sdSigAlt )
c     innovar = finfact*( sdSig*sdSig )

c-----------------------------------------------------------------------
c     Compute the estimates.
c-----------------------------------------------------------------------
      IF ( nIrrEst(1) .ne. 0 ) THEN
        fulEst(1) = sumsqr( vIrrEst, 1, nIrrEst(1) )/DBLE( nIrrEst(1) )
      END IF
      IF ( nIrrEst(1)  .gt. 2*nPer ) THEN 
        noeEst(1) = sumsqr( vIrrEst, nPer+1, nIrrEst(1)-nPer )
     &                    /DBLE( nIrrEst(1) - 2*nPer )
      END IF
c     ------------------------------------------------------------------
      pdA = max(nDelS(2)-nDelS(1)+1, 1)
      CALL mulDMat( dDelS, nDelS, vSeaEst, nSeaEst, vTemp1, nTemp1,
     &              pdA )
      IF ( nTemp1(1) .ne. 0 ) THEN
        fulEst(2) = sumsqr( vTemp1, 1, nTemp1(1) )/DBLE( nTemp1(1) )
      END IF
      IF ( nTemp1(1) .gt. 2*nPer ) THEN 
        noeEst(2) = sumsqr( vTemp1, nPer+1, nTemp1(1)-nPer )
     &                  /DBLE( nTemp1(1) - 2*nPer )
      END IF
c     ------------------------------------------------------------------
      pdA = max(nDelT(2)-nDelT(1)+1, 1)
      CALL mulDMat( dDelT, nDelT, vTreEst, nTreEst, vTemp2, nTemp2,
     &              pdA )
      IF ( nTemp2(1) .ne. 0 ) THEN
        fulEst(3) = sumsqr( vTemp2, 1, nTemp2(1) )/DBLE( nTemp2(1) )
      END IF
      IF ( nTemp2(1) .gt. 2*nPer ) THEN 
        noeEst(3) = sumsqr( vTemp2, nPer+1, nTemp2(1)-nPer )
     &                    /DBLE( nTemp2(1) - 2*nPer )
      END IF
c     ------------------------------------------------------------------
      CALL addMat( vTreEst, nTreEst, vIrrEst, nIrrEst, vTemp3, nTemp3 )
      pdA = max(nDelT(2)-nDelT(1)+1, 1)
      CALL mulDMat( dDelT, nDelT, vTemp3, nTemp3, vTemp2, nTemp2, pdA )
      IF ( nTemp2(1) .ne. 0 ) THEN
        fulEst(4) = sumsqr( vTemp2, 1, nTemp2(1) )/DBLE( nTemp2(1) )
      END IF
      IF ( nTemp2(1) .gt. 2*nPer ) THEN 
        noeEst(4) = sumsqr( vTemp2, nPer+1, nTemp2(1)-Nper )
     &            /DBLE( nTemp2(1) - 2*nPer )
      END IF

c-----------------------------------------------------------------------
c     Get some covariance submatrices
c-----------------------------------------------------------------------
      CALL getSMat( mCovIrr, nCovIrr, nPer+1, nCovIrr(1)-nPer, 
     &                    mCovIrrNoe, nCovIrrNoe )
      CALL getSMat( mCovSea, nCovSea, nPer+1, nCovSea(1)-nPer, 
     &                    mCovSeaNoe, nCovSeaNoe )
      CALL getSMat( mCovTre, nCovTre, nPer+1, nCovTre(1)-nPer, 
     &                    mCovTreNoe, nCovTreNoe )
      CALL getSMat( mCovSA, nCovSA, nPer+1, nCovSA(1)-nPer, 
     &                    mCovSANoe, nCovSANoe )

c-----------------------------------------------------------------------
c     Compute some traces of matrices.
c-----------------------------------------------------------------------
      trCovIrr    = getTrc( mCovIrr, nCovIrr )
      trCovSea    = getTrc( mCovSea, nCovSea )
      trCovTre    = getTrc( mCovTre, nCovTre )
      trCovSA     = getTrc( mCovSA,  nCovSA  )
c     ------------------------------------------------------------------
      trCovIrrNoe = getTrc( mCovIrrNoe, nCovIrrNoe )
      trCovSeaNoe = getTrc( mCovSeaNoe, nCovSeaNoe )
      trCovTreNoe = getTrc( mCovTreNoe, nCovTreNoe )
      trCovSANoe  = getTrc( mCovSANoe,  nCovSANoe  )
c     ------------------------------------------------------------------
      trCovIrrSq = getTrcAB( mCovIrr, nCovIrr, mCovIrr, nCovIrr )
      trCovSeaSq = getTrcAB( mCovSea, nCovSea, mCovSea, nCovSea )
      trCovTreSq = getTrcAB( mCovTre, nCovTre, mCovTre, nCovTre )
      trCovSASq  = getTrcAB( mCovSA,  nCovSA,  mCovSA,  nCovSA )
c     ------------------------------------------------------------------
      trCovIrrNoeSq = getTrcAB( mCovIrrNoe, nCovIrrNoe,
     &                          mCovIrrNoe, nCovIrrNoe )
      trCovSeaNoeSq = getTrcAB( mCovSeaNoe, nCovSeaNoe,
     &                          mCovSeaNoe, nCovSeaNoe )
      trCovTreNoeSq = getTrcAB( mCovTreNoe, nCovTreNoe,
     &                          mCovTreNoe, nCovTreNoe )
      trCovSANoeSq  = getTrcAB( mCovSANoe,  nCovSANoe,
     &                          mCovSANoe,  nCovSANoe )

c-----------------------------------------------------------------------
c     Compute the full estimators, null variances, and diagnostics
c-----------------------------------------------------------------------
      k1 = TWO*(innovar**2)
      k3 = (TWO*finfact - finfact**2)/DBLE(nT-dS-dT)

c     ------------------------------------------------------------------
c     For the irregular component (lag 0)
c     ------------------------------------------------------------------
      IF ( nCovIrr(1) .ne. 0 ) THEN
        fulEso(1) = trCovIrr/DBLE(nCovIrr(1))
        fulVar(1) = k1*(trCovIrrSq - k3*(trCovIrr**2))
     &            /DBLE(nCovIrr(1)**2)
        IF ( .not. dpeq(fulVar(1),ZERO) ) THEN
          fulDia(1) = (fulEst(1) - innovar*fulEso(1))/DSQRT(fulVar(1))
        END IF
      END IF

c     ------------------------------------------------------------------
c     For the seasonal component (lag 0)
c     ------------------------------------------------------------------
      IF ( nCovSea(1) .ne. 0 ) THEN
        fulEso(2) = trCovSea/DBLE(nCovSea(1))
        fulVar(2) = k1*(trCovSeaSq - k3*(trCovSea**2))
     &            /DBLE(nCovSea(1)**2)
        IF ( .not. dpeq(fulVar(2),ZERO) ) THEN
          fulDia(2) = (fulEst(2) - innovar*fulEso(2))/DSQRT(fulVar(2))
        END IF
      END IF

c     ------------------------------------------------------------------
c     For the trend component (lag 0)
c     ------------------------------------------------------------------
      IF ( nCovTre(1) .ne. 0 ) THEN
        fulEso(3) = trCovTre/DBLE(nCovTre(1))
        fulVar(3) = k1*(trCovTreSq - k3*(trCovTre**2))
     &            /DBLE(nCovTre(1)**2)
        IF ( .not. dpeq(fulVar(3),ZERO) ) THEN
          fulDia(3) = (fulEst(3) - innovar*fulEso(3))/DSQRT(fulVar(3))
        END IF
      END IF

c     ------------------------------------------------------------------
c     For the seasonal adjustment (lag 0)
c     ------------------------------------------------------------------
      IF ( nCovSA(1) .ne. 0 ) THEN
        fulEso(4) = trCovSA/DBLE(nCovSA(1))
        fulVar(4) = k1*(trCovSASq -  k3*(trCovSA**2))
     &            /DBLE(nCovSA(1)**2)
        IF ( .not. dpeq(fulVar(4),ZERO) ) THEN
          fulDia(4) = (fulEst(4) - innovar*fulEso(4))/DSQRT(fulVar(4))
        END IF
      END IF

c-----------------------------------------------------------------------
c     Compute the noend estimators, null variances, and diagnostics
c-----------------------------------------------------------------------
      k2 = TWO*(innovar**2)
c     k3 = (TWO*finfact - finfact**2)/DBLE(nT-dS-dT)

c     ------------------------------------------------------------------
c     For the irregular component (lag 0)
c     ------------------------------------------------------------------
      IF ( nCovIrrNoe(1) .ne. 0 ) THEN
        noeEso(1) = trCovIrrNoe/DBLE(nCovIrrNoe(1))
        noeVar(1) = k2*(trCovIrrNoeSq - k3*(trCovIrrNoe**2))
     &            /DBLE(nCovIrrNoe(1)**2)
        IF ( .not. dpeq(noeVar(1),ZERO) ) THEN
          noeDia(1) = (noeEst(1) - innovar*noeEso(1))/DSQRT(noeVar(1))
        END IF
      END IF

c     ------------------------------------------------------------------
c     For the seasonal component (lag 0)
c     ------------------------------------------------------------------
      IF ( nCovSeaNoe(1) .ne. 0 ) THEN
        noeEso(2) = trCovSeaNoe/DBLE(nCovSeaNoe(1))
        noeVar(2) = k2*(trCovSeaNoeSq - k3*(trCovSeaNoe**2))
     &            /DBLE(nCovSeaNoe(1)**2)
        IF ( .not. dpeq(noeVar(2),ZERO) ) THEN
          noeDia(2) = (noeEst(2) - innovar*noeEso(2))/DSQRT(noeVar(2))
        END IF
      END IF

c     ------------------------------------------------------------------
c     For the trend component (lag 0)
c     ------------------------------------------------------------------
      IF ( nCovTreNoe(1) .ne. 0 ) THEN
        noeEso(3) = trCovTreNoe/DBLE(nCovTreNoe(1))
        noeVar(3) = k2*(trCovTreNoeSq - k3*(trCovTreNoe**2))
     &            /DBLE(nCovTreNoe(1)**2)
        IF ( .not. dpeq(noeVar(3),ZERO) ) THEN
          noeDia(3) = (noeEst(3) - innovar*noeEso(3))/DSQRT(noeVar(3))
        END IF
      END IF

c     ------------------------------------------------------------------
c     For the seasonal adjustment (lag 0)
c     ------------------------------------------------------------------
      IF ( nCovSANoe(1) .ne. 0 ) THEN
        noeEso(4) = trCovSANoe/DBLE(nCovSANoe(1))
        noeVar(4) = k2*(trCovSANoeSq -  k3*(trCovSANoe**2))
     &            /DBLE(nCovSANoe(1)**2)
        IF ( .not. dpeq(noeVar(4),ZERO) ) THEN
          noeDia(4) = (noeEst(4) - innovar*noeEso(4))/DSQRT(noeVar(4))
        END IF
      END IF

c     ------------------------------------------------------------------
      RETURN
      END