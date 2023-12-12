      SUBROUTINE compCroDiag( nT, dS, dT, nPer,
     &                        finfact, sdSig, vIrrEst, nIrrEst,
     &                        vSeaEst, nSeaEst, vTreEst, nTreEst,
     &                        dDelS, nDelS, dDelT, nDelT, dDel, nDel,
     &                        dRedDelS, nRedDelS, dRedDelT, nRedDelT,
     &                        mInvSigW, nInvSigW, mSigUS, nSigUS,
     &                        mSigUT, nSigUT, mSigUI, nSigUI,
     &                        fulEst, fulEso, fulVar, fulDia, pLagSmT )
c-----------------------------------------------------------------------
c compLagDiag.f, Release 1, Subroutine Version 1.3, Modified 04 May 2006.
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 21 Jul 2005.
c     Modified by REG, on 11/10/2005, to generalize covariance structure
c       of irregular component, using mSigUI instead of sIrrVar;
c       and to clean up tab stops in code.
c     Modified by REG, on 13 Jan 2006, to optimize matrix operations,
c       by using diagonal matrix utilities.
c     Modified by REG, on 04 May 2006, to calculate estimate and variance
c       for each diagnostic relative to the innovation variance to match
c       SEATS output, and to move finite factor processing to getDiag()
c       eliminating the need for input nParam, nFixed, and nDiff.
c-----------------------------------------------------------------------
c     This subroutine calculates some cross component diagnostics.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c dDel    d   diagonal form of overall differencing matrix: mDel
c dDelS   d   diagonal form of seasonal differencing matrix: mDelS
c dDelT   d   diagonal form of trend differencing matrix: mDelT
c dRedDelS d  diagonal form of reduced mDelS: mRedDelS
c dRedDelT d  diagonal form of reduced mDelT: mRedDelT
c dS      i   size of Seasonal Differencing
c dT      i   size of Trend Differencing
c finfact d   finite sample correction factor
c fulDia  d   vector of normalized diagnostics from full signals
c             for irregular, seasonal, and trend
c fulEso  d   vector of null means of estimates from full signals
c             for irregular, seasonal, and trend
c fulEst  d   vector of diagnostic estimates from full signals
c             for irregular, seasonal, and trend
c fulVar  d   vector of variances of diagnostics from full signals
c             for irregular, seasonal, and trend
c mDel    d   full differencing matrix
c mDelS   d   seasonal differencing matrix
c mDelT   d   trend differencing matrix
c mDel    d   size (rows,columns) of mDel
c mInvSigW d  inverse of matrix mSigW
c mRedDelS d  smaller version of mDelS
c mRedDelT d  smaller version of mDelT
c mSigUI  d   covariance matrix for undifferenced irregular
c mSigUS  d   covariance matrix for differenced seasonal
c mSigUT  d   covariance matrix for differenced trend
c nDelS   i   size (rows,columns) of mDelS
c nDelT   i   size (rows,columns) of mDelT
c nInvSigW i  size (rows,columns) of mInvSigW
c nRedDelS i  size (rows,columns) of mRedDelS
c nRedDelT i  size (rows,columns) of mRedDelT
c nSigUI  i   size (rows,columns) of mSigUI matrix
c nSigUS  i   size (rows,columns) of mSigUS matrix
c nSigUT  i   size (rows,columns) of mSigUT matrix
c nIrrEst d   size (rows,columns) of vIrrEst vector
c nSeaEst d   size (rows,columns) of vSeaEst vector
c nTreEst d   size (rows,columns) of vTreEst vector
c nPer    i   size of seasonal period
c nT      i   size of data available
c sdSig   d   estimated data innovation stdev corrected for number of
c             estimated model parameters
c vIrrEst d   estimated irregular
c vSeaEst d   estimated seasonal
c vTreEst d   estimated trend
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c ddot    d   external function reference
c dLagS   d   diagonal form of seasonal selection matrix: mLagS
c dLagSmT d   diagonal form of seasonal-trend selection matrix: mLagSmT
c dLagT   d   diagonal form of trend selection matrix: mLagT
c getTrc  d   external function reference
c HALF    d   constant parameter
c innovar d   model innovation variance adjusted for finite sample
c i,j     i   index variables for do loops
c k1,k2,k3 d  miscellaneous constants
c mCov    d   storage of covariance matrix for each diagnostic
c mLagS   d   seasonal selection matrix
c mLagSmT d   seasonal-trend selection matrix
c mLagT   d   trend selection matrix
c mTemp3  d   temporary matrix 3 used as storage for intermediate results
c mTemp4  d   temporary matrix 4 used as storage for intermediate results
c mTemp5  d   temporary matrix 5 used as storage for intermediate results
c mTemp6  d   temporary matrix 6 used as storage for intermediate results
c mTemp6Tr d  temporary matrix 6Tr used as storage for intermediate results
c mTemp7  d   temporary matrix 7 used as storage for intermediate results
c mTempA  d   temporary matrix A used as storage for intermediate results
c mTempB  d   temporary matrix B used as storage for intermediate results
c mTempD  d   temporary matrix D used as storage for intermediate results
c mTempE  d   temporary matrix E used as storage for intermediate results
c mTempF  d   temporary matrix F used as storage for intermediate results
c nCov    i   size (rows,columns) of mCov matrix
c nLagS   i   size (rows,columns) of mLagS matrix
c nLagT   i   size (rows,columns) of mLagT matrix
c nLagSmT i   size (rows,columns) of mLagSmT matrix
c nSave   i   identifies default size of large matrices
c             that are saved (not dynamic)
c nTemp1  i   size (rows,columns) of vTemp1 vector
c nTemp2  i   size (rows,columns) of vTemp2 vector
c nTemp3  i   size (rows,columns) of mTemp3 scalar
c nTemp4  i   size (rows,columns) of mTemp4 matrix
c nTemp5  i   size (rows,columns) of mTemp5 matrix
c nTemp6  i   size (rows,columns) of mTemp6 matrix
c nTemp6Tr i  size (rows,columns) of mTemp6Tr matrix
c nTemp7  i   size (rows,columns) of mTemp7 matrix
c nTemp9  i   size (rows,columns) of vTemp9 vector
c nTempA  i   size (rows,columns) of mTempA matrix
c nTempB  i   size (rows,columns) of mTempB matrix
c nTempC  i   size (rows,columns) of vTempC vector
c nTempD  i   size (rows,columns) of mTempD matrix
c nTempE  i   size (rows,columns) of mTempE matrix
c nTempF  i   size (rows,columns) of mTempF matrix
c ONE     d   constant parameter
c TWO     d   constant parameter
c vTemp1  d   temporary vector 1
c vTemp2  d   temporary vector 2
c vTemp9  d   temporary vector 9
c vTempC  d   temporary vector C
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
      INTEGER nDel(2), nDelS(2), nDelT(2)
      INTEGER nRedDelS(2), nRedDelT(2), nInvSigW(2)
      INTEGER nSigUS(2), nSigUT(2), nSigUI(2), pLagSmT
      DOUBLE PRECISION finfact, sdSig
      DOUBLE PRECISION vIrrEst(nT), vSeaEst(nT), vTreEst(nT)
      DOUBLE PRECISION dDel(dS+dT+1), dDelS(dS+1), dDelT(dT+1),
     &                 dRedDelS(dS+1), dRedDelT(dT+1)
c     DOUBLE PRECISION mDel(nT-dS-dT,nT), mDelS(nT-dS,nT),
c    &                 mDelT(nT-dT,nT)
c     DOUBLE PRECISION mRedDelS(nT-dS-dT,nT-dT),mRedDelT(nT-dS-dT,nT-dS)
      DOUBLE PRECISION mInvSigW(nT-dS-dT,nT-dS-dT)
      DOUBLE PRECISION mSigUS(nT-dS,nT-dS), mSigUT(nT-dT,nT-dT),
     &                 mSigUI(nT,nT)
      DOUBLE PRECISION fulEst(3), fulEso(3), fulVar(3), fulDia(3)

c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i, j
      INTEGER nLagS(2), nLagT(2), nLagSmT(2)
      INTEGER nTemp1(2), nTemp2(2), nTemp3(2), nTemp4(2), nTemp5(2),
     &        nTemp6(2), nTemp6Tr(2), nTemp7(2), nTemp9(2), nTempA(2),
     &        nTempB(2), nTempC(2), nTempD(2), nTempE(2), nTempF(2)
      INTEGER nCov(2)
      DOUBLE PRECISION innovar
      DOUBLE PRECISION dLagS(dS+1), dLagT(dT+1), dLagSmT(pLagSmT)
      DOUBLE PRECISION vTemp1(nT-dS), vTemp2(nT-dS),
     &                 vTemp9(nT-dT), vTempC(nT-dT)
      DOUBLE PRECISION ddot
      DOUBLE PRECISION k1, k2, k3, ZERO, HALF, ONE, TWO
      DOUBLE PRECISION getTrc, getTrcAB
      PARAMETER (ZERO=0.0D0, HALF=0.5D0, ONE=1.0D0, TWO=2.0D0)
      LOGICAL dpeq

c     ------------------------------------------------------------------
c     Dynamic (commented) versus static (uncommented) matrices
c     ------------------------------------------------------------------
c     DOUBLE PRECISION mLagS(nT-dS,nT), mLagT(nT-dT,nT),
c    &                 mLagSmT(nT-dS,nT-dT)
c     DOUBLE PRECISION mTemp3(nT-dS,nT), mTemp4(nT-dS-dT,nT-dS),
c    &                 mTemp5(nT-dS,nT-dS-dT),
c    &                 mTemp6(nT-dS-dT,nT-dS-dT),
c    &                 mTemp6Tr(nT-dS-dT,nT-dS-dT),
c    &                 mTemp7(nT-dS-dT,nT-dS-dT),
c    &                 mTempA(nT-dS,nT-dT), mTempB(nT-dS,nT-dS-dT),
c    &                 mTempD(nT-dS-dT,nT-dT), mTempE(nT-dT,nT-dS-dT)
c    &                 mTempF(nT-dT,nT)
c     DOUBLE PRECISION mCov(nT-dS-dT,nT-dS-dT)
c     ------------------------------------------------------------------
      INTEGER nSave
      PARAMETER (nSave=POBS*POBS)
c     DOUBLE PRECISION mLagS(nSave), mLagT(nSave), mLagSmT(nSave)
      DOUBLE PRECISION mTemp3(nSave), mTemp4(nSave), mTemp5(nSave),
     &                 mTemp6(nSave), mTemp6Tr(nSave),
     &                 mTemp7(nSave), mTempA(nSave), mTempB(nSave),
     &                 mTempD(nSave), mTempE(nSave), mTempF(nSave)
      DOUBLE PRECISION mCov(nSave)
c     SAVE mLagS, mLagT, mLagSmT
      SAVE mTemp4, mTemp5, mTemp6, mTemp6Tr, mCov, mTempA
      EQUIVALENCE (mTemp4,mTemp7),(mTemp5, mTempB),(mTemp4,mTempD),
     &            (mTemp5,mTempE),(mTempA,mTemp3),(mTempA,mTempF)

c-----------------------------------------------------------------------
c     Initialize the outputs.
c-----------------------------------------------------------------------
      DO i = 1,3
        fulEst(i) = ZERO
        fulEso(i) = ZERO
        fulVar(i) = ZERO
        fulDia(i) = ZERO
      END DO

c-----------------------------------------------------------------------
c     Calculate model innovation variance adjusted for finite sample.
c-----------------------------------------------------------------------
      innovar = ( sdSig*sdSig )
      k1 = TWO/DBLE(nT*nT)
      k3 = (TWO*finfact - (finfact*finfact))/DBLE(nT-dS-dT)

c-----------------------------------------------------------------------
c     Compute the lag matrices.
c-----------------------------------------------------------------------
      nLagS(1) = nT-dS
      nLagS(2) = nT
      nLagT(1) = nT-dT
      nLagT(2) = nT
      nLagSmT(1) = nT-dS
      nLagSmT(2) = nT-dT
c     ------------------------------------------------------------------
c     DO j = 1, nLagS(2)
c      DO i = 1, nLagS(1)
c       mLagS((j-1)*nLagS(1)+i) = ZERO
c      END DO
c      IF (j.gt.dS) THEN
c       i = j-dS
c       mLagS((j-1)*nLagS(1)+i) = ONE
c      END IF
c     END DO
c     ------------------------------------------------------------------
      DO j = 1, dS
       dLagS(j) = ZERO
      END DO
      dLagS(dS+1) = ONE
c     ------------------------------------------------------------------
c     DO j = 1, nLagT(2)
c      DO i = 1, nLagT(1)
c       mLagT((j-1)*nLagT(1)+i) = ZERO
c      END DO
c      IF (j.gt.dT) THEN
c       i = j-dT
c       mLagT((j-1)*nLagT(1)+i) = ONE
c      END IF
c     END DO
c     ------------------------------------------------------------------
      DO j = 1, dT
       dLagT(j) = ZERO
      END DO
      dLagT(dT+1) = ONE
c     ------------------------------------------------------------------
c     DO j = 1, nLagSmT(2)
c      DO i = 1, nLagSmT(1)
c       mLagSmT((j-1)*nLagSmT(1)+i) = ZERO
c      END DO
c      IF (j.gt.(dS-dT)) THEN
c       i = j-(dS-dT)
c       mLagSmT((j-1)*nLagSmT(1)+i) = ONE
c      END IF
c     END DO
c     ------------------------------------------------------------------
      DO j = 1, dS-dT
       dLagSmT(j) = ZERO
      END DO
      dLagSmT(max(dS-dT+1,1)) = ONE

c-----------------------------------------------------------------------
c     Compute the Seasonal-Irregular cross component diagnostic
c-----------------------------------------------------------------------
c     Calculate the estimate
c     ------------------------------------------------------------------
      pdA = max(nDelS(2)-nDelS(1)+1, 1)
      CALL mulDMat( dDelS, nDelS, vSeaEst, nSeaEst, vTemp1, nTemp1,
     &              pdA )
      pdA = max(nLagS(2)-nLagS(1)+1, 1)
      CALL mulDMat( dLagS, nLagS, vIrrEst, nIrrEst, vTemp2, nTemp2,
     &              pdA )
      IF (( nTemp1(1) .ne. 0 ) .and. ( nTemp1(1) .eq. nTemp2(1) )) THEN
        fulEst(1) = ddot( nTemp1(1), vTemp1, 1, vTemp2, 1 )
     &            / (innovar*DBLE(nT))
      END IF
c     ------------------------------------------------------------------
c     Calculate the covariance matrix
c     ------------------------------------------------------------------
      pdA = max(nRedDelT(2)-nRedDelT(1)+1, 1)
      CALL mulDMat( dRedDelT, nRedDelT, mSigUS, nSigUS, mTemp4, nTemp4,
     &              pdA )
      pdA = max(nLagS(2)-nLagS(1)+1, 1)
      CALL mulDMat( dLagS, nLagS, mSigUI, nSigUI, mTemp3, nTemp3, pdA )
      pdA = max(nDel(2)-nDel(1)+1, 1)
      CALL mulMatDTr( mTemp3, nTemp3, dDel, nDel, mTemp5, nTemp5, pdA )
      CALL mulMat( mTemp4, nTemp4, mTemp5, nTemp5, mTemp6, nTemp6 )
      CALL getTr( mTemp6, nTemp6, mTemp6Tr, nTemp6Tr )
      CALL addMat( mTemp6, nTemp6, mTemp6Tr, nTemp6Tr, mCov, nCov )
      CALL mulSca( HALF, mCov, nCov )
c     ------------------------------------------------------------------
c     Calculate the estimator
c     ------------------------------------------------------------------
      CALL mulMat( mInvSigW, nInvSigW, mCov, nCov, mTemp7, nTemp7 )
      IF ( nTemp7(1) .ne. 0 ) THEN
        fulEso(1) = getTrc( mTemp7, nTemp7 )/DBLE(nT)
      END IF
c     ------------------------------------------------------------------
c     Calculate the variance
c     ------------------------------------------------------------------
      fulVar(1) = k1*( getTrcAB( mTemp7, nTemp7, mTemp7, nTemp7 )
     &          - k3*( getTrc( mTemp7, nTemp7 )**2 ))
c     ------------------------------------------------------------------
c     Calculate the cross component diagnostic
c     ------------------------------------------------------------------
      IF ( .not. dpeq(fulVar(1),ZERO) ) THEN
        fulDia(1) = (fulEst(1) - fulEso(1))/DSQRT(fulVar(1))
      END IF

c-----------------------------------------------------------------------
c     Compute the Seasonal-Trend cross component diagnostic
c-----------------------------------------------------------------------
c     Calculate the estimate relative to the innovation variance
c     ------------------------------------------------------------------
c     CALL mulDMat( dDelS, nDelS, vSeaEst, nSeaEst, vTemp1, nTemp1 )
      pdA = max(nDelT(2)-nDelT(1)+1, 1)
      CALL mulDMat( dDelT, nDelT, vTreEst, nTreEst, vTemp9, nTemp9,
     &              pdA )
      pdA = max(nLagSmT(2)-nLagSmT(1)+1, 1)
      CALL mulDMat( dLagSmT, nLagSmT, vTemp9, nTemp9, vTemp2, nTemp2,
     &              pdA )
      IF (( nTemp1(1) .ne. 0 ) .and. ( nTemp1(1) .eq. nTemp2(1) )) THEN
        fulEst(2) = ddot( nTemp1(1), vTemp1, 1, vTemp2, 1 )
     &            / (innovar*DBLE(nT))
      END IF
c     ------------------------------------------------------------------
c     Calculate the covariance matrix
c     ------------------------------------------------------------------
      pdA = max(nRedDelT(2)-nRedDelT(1)+1, 1)
      CALL mulDMat( dRedDelT, nRedDelT, mSigUS, nSigUS, mTemp4, nTemp4,
     &              pdA )
      pdA = max(nLagSmT(2)-nLagSmT(1)+1, 1)
      CALL mulDMat( dLagSmT, nLagSmT, mSigUT, nSigUT, mTempA, nTempA,
     &              pdA )
      pdA = max(nRedDelS(2)-nRedDelS(1)+1, 1)
      CALL mulMatDTr( mTempA, nTempA, dRedDelS, nRedDelS,
     &                mTempB, nTempB, pdA )
      CALL mulMat( mTemp4, nTemp4, mTempB, nTempB, mTemp6, nTemp6 )
      CALL getTr( mTemp6, nTemp6, mTemp6Tr, nTemp6Tr )
      CALL addMat( mTemp6, nTemp6, mTemp6Tr, nTemp6Tr, mCov, nCov )
      CALL mulSca( HALF, mCov, nCov )
c     ------------------------------------------------------------------
c     Calculate the estimator
c     ------------------------------------------------------------------
      CALL mulMat( mInvSigW, nInvSigW, mCov, nCov, mTemp7, nTemp7 )
      IF ( nTemp7(1) .ne. 0 ) THEN
        fulEso(2) = getTrc( mTemp7, nTemp7 )/DBLE(nT)
      END IF
c     ------------------------------------------------------------------
c     Calculate the variance relative to the innovation variance
c     ------------------------------------------------------------------
      fulVar(2) = k1*( getTrcAB( mTemp7, nTemp7, mTemp7, nTemp7 )
     &          - k3*( getTrc( mTemp7, nTemp7 )**2 ))
c     ------------------------------------------------------------------
c     Calculate the cross component diagnostic
c     ------------------------------------------------------------------
      IF ( .not. dpeq(fulVar(2),ZERO) ) THEN
        fulDia(2) = (fulEst(2) - fulEso(2))/DSQRT(fulVar(2))
      END IF

c-----------------------------------------------------------------------
c     Compute the Trend-Irregular cross component diagnostic
c-----------------------------------------------------------------------
c     Calculate the estimate relative to the innovation variance
c     ------------------------------------------------------------------
c     CALL mulDMat( dDelT, nDelT, vTreEst, nTreEst, vTemp9, nTemp9 )
      pdA = max(nLagT(2)-nLagT(1)+1, 1)
      CALL mulDMat( dLagT, nLagT, vIrrEst, nIrrEst, vTempC, nTempC,
     &              pdA )
      IF (( nTemp9(1) .ne. 0 ) .and. ( nTemp9(1) .eq. nTempC(1) )) THEN
        fulEst(3) = ddot( nTemp9(1), vTemp9, 1, vTempC, 1 )
     &            / (innovar*DBLE(nT))
      END IF
c     ------------------------------------------------------------------
c     Calculate the covariance matrix
c     ------------------------------------------------------------------
      pdA = max(nRedDelS(2)-nRedDelS(1)+1, 1)
      CALL mulDMat( dRedDelS, nRedDelS, mSigUT, nSigUT, mTempD, nTempD,
     &              pdA )
      pdA = max(nLagT(2)-nLagT(1)+1, 1)
      CALL mulDMat( dLagT, nLagT, mSigUI, nSigUI, mTempF, nTempF,
     &              pdA )
      pdA = max(nDel(2)-nDel(1)+1, 1)
      CALL mulMatDTr( mTempF, nTempF, dDel, nDel, mTempE, nTempE, pdA )
      CALL mulMat( mTempD, nTempD, mTempE, nTempE, mTemp6, nTemp6 )
      CALL getTr( mTemp6, nTemp6, mTemp6Tr, nTemp6Tr )
      CALL addMat( mTemp6, nTemp6, mTemp6Tr, nTemp6Tr, mCov, nCov )
      CALL mulSca( HALF, mCov, nCov )
c     ------------------------------------------------------------------
c     Calculate the estimator
c     ------------------------------------------------------------------
      CALL mulMat( mInvSigW, nInvSigW, mCov, nCov, mTemp7, nTemp7 )
      IF ( nTemp7(1) .ne. 0 ) THEN
        fulEso(3) = getTrc( mTemp7, nTemp7 )/DBLE(nT)
      END IF
c     ------------------------------------------------------------------
c     Calculate the variance relative to the innovation variance
c     ------------------------------------------------------------------
      fulVar(3) = k1*( getTrcAB( mTemp7, nTemp7, mTemp7, nTemp7 )
     &          - k3*( getTrc( mTemp7, nTemp7 )**2 ))
c     ------------------------------------------------------------------
c     Calculate the cross component diagnostic
c     ------------------------------------------------------------------
      IF ( .not. dpeq(fulVar(3),ZERO) ) THEN
        fulDia(3) = (fulEst(3) - fulEso(3))/DSQRT(fulVar(3))
      END IF
c-----------------------------------------------------------------------
      RETURN
      END