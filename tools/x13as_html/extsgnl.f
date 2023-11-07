      SUBROUTINE extSgnl( nT, dS, dT, vY, mDel, dDel, nDel,
     &                    mDelS, dDelS, nDelS, mDelT, dDelT, nDelT,
     &                    sdSigAlt, mRedDelS, dRedDelS, nRedDelS,
     &                    mRedDelT, dRedDelT, nRedDelT, mSigUS, nSigUS,
     &                    mSigUT, nSigUT, mSigUI, nSigUI, 
     &                    mSigWS, nSigWS, mSigWT, nSigWT,
     &                    mSigW, nSigW, mInvSigW, nInvSigW,
     &                    mInvSigUS, nInvSigUS, mInvSigUT, nInvSigUT,
     &                    mInvSigWS, nInvSigWS, mInvSigWT, nInvSigWT,
     &                    vIrrEst, nIrrEst, vSeaEst, nSeaEst,
     &                    vTreEst, nTreEst, mCovIrr, nCovIrr,
     &                    mCovSea, nCovSea, mCovTre, nCovTre,
     &                    mCovSA,  nCovSA,
     &                    mIrrPFlt, nIrrPFlt, mSeaPFlt, nSeaPFlt,
     &                    mTrePFlt, nTrePFlt, mSAPFlt,  nSAPFlt )
c-----------------------------------------------------------------------
c    extSgnl.f, Release 1, Subroutine Version 1.4, Modified 24 Jan 2006.
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 11 Apr 2005.
c     Modified by REG, on 19 Sep 2005, to add output of SA filter,
c       and to clean up tab stops.
c     Modified by REG, on 20 Oct 2005, to move sdSigAlt processing 
c       from bldCov() to this routine, and to remove calculation 
c       of signal extraction MSE variances that are now calculated 
c       by compMSE: mIrrVar, mSeaVar, mTreVar.
c     Modified by REG, on 07 Nov 2005, to generalize irregular component
c       to allow non white noise covariance structure: mSigUI.
c     Modified by REG, on 24 Jan 2006, to use the diagonal form of the 
c       mDel matrices and associated matrix manipulation utilities,
c       and to use inverse of matrices calculated externally.
c-----------------------------------------------------------------------
c     This subroutine calculates the some signal estimators and some 
c     covariance matrices.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c dS      i   size of Seasonal Differencing
c dT          i   size of Trend Differencing
c dDel    d   diagonal form of overall differencing matrix
c dDelS   d   diagonal form of seasonal differencing matrix
c dDelT   d   diagonal form of trend differencing matrix
c dRedDelS d  diagonal form of smaller version of mDelS
c dRedDelT d  diagonal form of smaller version of mDelT
c mCovIrr d   covariance of estimated irregular
c mCovSA  d   covariance of estimated seasonal adjusted
c mCovSea d   covariance of estimated seasonal
c mCovTre d   covariance of estimated trend
c mInvSigUS d inverse of mSigUS
c mInvSigUT d inverse of mSigUT
c mInvSigW d  inverse of mSigW
c mInvSigWS d inverse of mSigWS
c mInvSigWT d inverse of mSigWT
c mSigUI  d   covariance matrix for undifferenced irregular
c mSigUS  d   covariance matrix for differenced seasonal
c mSigUT  d   covariance matrix for differenced trend
c mSigW   d   covariance matrix for differenced data
c mSigWS  d   covariance matrix for differenced trend adjusted
c mSigWT  d   covariance matrix for differenced seasonally adjusted
c mIrrPFlt d  irregular component filter matrix
c             (row 1 = symmetric, row 2 = concurrent)
c mSAPFlt  d  seasonal adjustment filter matrix
c             (row 1 = symmetric, row 2 = concurrent)
c mSeaPFlt d  seasonal component filter matrix
c             (row 1 = symmetric, row 2 = concurrent)
c mTrePFlt d  trend component filter matrix
c             (row 1 = symmetric, row 2 = concurrent)
c nCovIrr d   size (rows,columns) of mCovIrr matrix
c nCovSA  d   size (rows,columns) of mCovSA  matrix
c nCovSea d   size (rows,columns) of mCovSea matrix
c nCovTre d   size (rows,columns) of mCovTre matrix
c nDel    i   size (rows,columns) of dDel
c nDelS   i   size (rows,columns) of dDelS
c nDelT   i   size (rows,columns) of dDelT
c nInvSigUS i size (rows,columns) of mInvSigUS matrix
c nInvSigUT i size (rows,columns) of mInvSigUT matrix
c nInvSigW i  size (rows,columns) of mInvSigW matrix
c nInvSigWS i size (rows,columns) of mInvSigWS matrix
c nInvSigWT i size (rows,columns) of mInvSigWT matrix
c nRedDelS i  size (rows,columns) of dRedDelS
c nRedDelT i  size (rows,columns) of dRedDelT
c nSigUI  i   size (rows,columns) of mSigUI matrix
c nSigUS  i   size (rows,columns) of mSigUS matrix
c nSigUT  i   size (rows,columns) of mSigUT matrix
c nSigW   i   size (rows,columns) of mSigW matrix
c nSigWS  i   size (rows,columns) of mSigWS matrix
c nSigWT  i   size (rows,columns) of mSigWT matrix
c nIrrEst i   size (rows,columns) of vIrrEst vector
c nIrrPFlt i  size (rows,columns) of mIrrFlt matrix
c nSAPFlt  i  size (rows,columns) of mSAFlt  matrix
c nSeaEst i   size (rows,columns) of vSeaEst vector
c nSeaPFlt i  size (rows,columns) of mSeaFlt matrix
c nTreEst i   size (rows,columns) of vTreEst vector
c nTrePFlt i  size (rows,columns) of mTreFlt matrix
c nT      i   size of data available
c sdSigAlt d  alternate data innovation stdev when parameters are fixed
c vIrrEst d   estimated irregular
c vSeaEst d   estimated seasonal
c vTreEst d   estimated trend
c vY      d   data vector
c-----------------------------------------------------------------------
c Name   Type Description (Local Variables)
c-----------------------------------------------------------------------
c i,j     i   index variables
c mFSTIIrr d  irregular extraction matrix
c mQuadUS d   result of quadratic matrix operation
c mQuadUT d   result of quadratic matrix operation
c mQuadWS d   result of quadratic matrix operation
c mQuadWSp1 d intermediate result of quadratic matrix operation
c mQuadWT d   result of quadratic matrix operation
c mQuadWTp1 d intermediate result of quadratic matrix operation
c mQuadW  d   result of quadratic matrix operation
c mQuadWp1 d  intermediate result of quadratic matrix operation
c nSave   i   identifies default size of large matrices
c             that are saved (not dynamic)
c mTemp1  d   temporary matrix 1
c mTemp2  d   temporary matrix 2
c mTemp4  d   temporary matrix 4
c mTemp5  d   temporary matrix 5
c mTemp6  d   temporary matrix 6
c mTemp8  d   temporary matrix 8
c nY      i   size (rows,columns) of vY vector
c nFSTIrr i   size (rows,columns) of mFSTIrr matrix
c nResult i   size (rows,columns) of sResult scalar
c nTemp1  i   size (rows,columns) of mTemp1 matrix
c nTemp2  i   size (rows,columns) of mTemp2 matrix
c nTemp4  i   size (rows,columns) of mTemp4 matrix
c nTemp5  i   size (rows,columns) of mTemp5 matrix
c nTemp6  i   size (rows,columns) of mTemp6 matrix
c nTemp8  i   size (rows,columns) of mTemp8 matrix
c nTempC  i   size (rows,columns) of mTempC matrix
c nQuadUS i   size (rows,columns) of nQuadUS matrix
c nQuadUT i   size (rows,columns) of nQuadUT matrix
c nQuadWS i   size (rows,columns) of nQuadWS matrix
c nQuadWSp1 i size (rows,columns) of nQuadWSp1 matrix
c nQuadWT i   size (rows,columns) of nQuadWT matrix
c nQuadWTp1 i size (rows,columns) of nQuadWTp1 matrix
c nQuadW  i   size (rows,columns) of nQuadW matrix
c nQuadWp1 i  size (rows,columns) of nQuadWp1 matrix
c sResult d   scalar result of matrix operations
c vTempC  d   temporary vector C
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
      INTEGER nT, dS, dT
      INTEGER nDel(2), nDelS(2), nDelT(2), nRedDelS(2), nRedDelT(2)
      INTEGER nSigUI(2), nSigUS(2), nSigUT(2), nInvSigUS(2),
     &        nInvSigUT(2)
      INTEGER nSigWS(2), nSigWT(2), nSigW(2),  nInvSigWS(2),
     &        nInvSigWT(2)
      INTEGER nInvSigW(2)
      INTEGER nIrrEst(2), nSeaEst(2), nTreEst(2)
      INTEGER nCovIrr(2), nCovSea(2), nCovTre(2), nCovSA(2)
      INTEGER nIrrPFlt(2), nSeaPFlt(2), nTrePFlt(2), nSAPFlt(2)
      DOUBLE PRECISION vY(nT), sdSigAlt
      DOUBLE PRECISION dDel(dS+dT+1),dDelS(dS+1),dDelT(dT+1),
     &                 dRedDelS(dS+1), dRedDelT(dT+1)
      DOUBLE PRECISION mDel(nT-dS-dT,nT), mDelS(nT-dS,nT),
     &                 mDelT(nT-dT,nT), mRedDelS(nT-dS-dT,nT-dT),
     &                 mRedDelT(nT-dS-dT,nT-dS)
      DOUBLE PRECISION mSigUI(nT,nT), mSigUS(nT-dS,nT-dS),
     &                 mSigUT(nT-dT,nT-dT)
      DOUBLE PRECISION mSigWS(nT-dS,nT-dS), mSigWT(nT-dT,nT-dT),
     &                 mSigW(nT-dS-dT,nT-dS-dT)
      DOUBLE PRECISION vIrrEst(nT), vSeaEst(nT), vTreEst(nT)
      DOUBLE PRECISION mCovIrr(nT,nT), mCovSea(nT-dS,nT-dS), 
     &                 mCovTre(nT-dT,nT-dT), mCovSA(nT-dT,nT-dT)
      DOUBLE PRECISION mInvSigUS(nT-dS,nT-dS), mInvSigUT(nT-dT,nT-dT)
      DOUBLE PRECISION mInvSigWS(nT-dS,nT-dS), mInvSigWT(nT-dT,nT-dT),
     &                 mInvSigW(nT-dS-dT,nT-dS-dT)
      DOUBLE PRECISION mIrrPFlt(nT-dS-dT,2), mSAPFlt(nT-dS,2),
     &                 mSeaPFlt(nT-dT,2), mTrePFlt(nT-dS,2)

c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i, j, ibase
      INTEGER nResult(2), nY(2)
      INTEGER nTemp1(2), nTemp2(2), nTemp4(2), 
     &        nTemp5(2), nTemp6(2), nTemp8(2)
      INTEGER nTempC(2)
      INTEGER nFSTIIrr(2)
      INTEGER nQuadUS(2), nQuadUT(2), nQuadWS(2), nQuadWSp1(2),
     &        nQuadWT(2), nQuadWTp1(2), nQuadW(2), nQuadWp1(2)
      DOUBLE PRECISION sResult(1)
      DOUBLE PRECISION vTempC(nT-dS-dT)

c     ------------------------------------------------------------------
c     Dynamic (commented) versus static (uncommented) matrices
c     ------------------------------------------------------------------
c     DOUBLE PRECISION mTemp1(nT,nT-dS-dT), mTemp2(nT,nT), 
c    &                 mTemp4(nT-dS,nT-dS), mTemp5(nT,nT-dT),
c    &                 mTemp6(nT,nT-dS), mTemp8(nT-dT,nT-dT)
c     DOUBLE PRECISION mFSISea(nT,nT), mFTITre(nT,nT), mFSTIIrr(nT,nT)
c     DOUBLE PRECISION mQuadUS(nT,nT), mQuadUT(nT,nT),
c    &                 mQuadWS(nT,nT), mQuadWSp1(nT,nT-dS),
c    &                 mQuadWT(nT,nT), mQuadWTp1(nT,nT-dT),
c    &                 mQuadW(nT,nT),  mQuadWp1(nT,nT-dS-dT)
c     ------------------------------------------------------------------
      INTEGER nSave
      PARAMETER (nSave=POBS*POBS)
      DOUBLE PRECISION mTemp1(nSave), mTemp2(nSave), 
     &                 mTemp4(nSave), mTemp5(nSave),
     &                 mTemp6(nSave), mTemp8(nSave)
      DOUBLE PRECISION mFSTIIrr(nSave)
      DOUBLE PRECISION mQuadUS(nSave), mQuadUT(nSave),
     &                 mQuadWS(nSave), mQuadWSp1(nSave),
     &                 mQuadWT(nSave), mQuadWTp1(nSave),
     &                 mQuadW(nSave),  mQuadWp1(nSave)
      SAVE mTemp1, mTemp2, mFSTIIrr,
     &     mQuadUS, mQuadUT, mQuadWS, mQuadWSp1, mQuadWT, mQuadWTp1,
     &     mQuadW, mQuadWp1
      EQUIVALENCE (mTemp1,mTemp4),(mTemp2,mTemp8),(mTemp5,mTemp6)

c-----------------------------------------------------------------------
c     Set some local sizes.
c-----------------------------------------------------------------------
      nY(1) = nT
      nY(2) = 1

c-----------------------------------------------------------------------
c     Calculate m_dSigmaAlt (scalar)
c-----------------------------------------------------------------------
      pdA = max(nDel(2)-nDel(1)+1, 1)
      CALL mulDMat( dDel, nDel, vY, nY, vTempC, nTempC, pdA )
      CALL mulQMatTr( vTempC, nTempC, mInvSigW, nInvSigW, 
     &                sResult(1), nResult )
      IF (( nResult(1) .eq. 1 ) .and. ( nDel(1) .gt. 0 )) THEN
        sdSigAlt = DSQRT( sResult(1) / DBLE(nDel(1)) )
      ELSE
        sdSigAlt = 0.0d0
      END IF

c-----------------------------------------------------------------------
c     Calculate some quadratic matrices.
c-----------------------------------------------------------------------
      CALL mulQdMatTr( dDelS, nDelS, mInvSigUS, nInvSigUS, 
     &                 mQuadUS, nQuadUS )
c     ------------------------------------------------------------------
      CALL mulQdMatTr( dDelT, nDelT, mInvSigUT, nInvSigUT, 
     &                 mQuadUT, nQuadUT )
c     ------------------------------------------------------------------
c     The following commented code is carried out in 2 matrix operations
c     in order to save the intermediate matrix result.
c     ------------------------------------------------------------------
c     CALL mulQMatTr( mDelS, nDelS, mInvSigWS, nInvSigWS, 
c    &                mQuadWS, nQuadWS )
      pdA = max(nDelS(2)-nDelS(1)+1, 1)
      CALL mulDTrMat( dDelS, nDelS, mInvSigWS, nInvSigWS, 
     &                mQuadWSp1, nQuadWSp1, pdA )
      CALL mulMatD( mQuadWSp1, nQuadWSp1, dDelS, nDelS,
     &              mQuadWS, nQuadWS, pdA )
c     ------------------------------------------------------------------
c     The following commented code is carried out in 2 matrix operations
c     in order to save the intermediate matrix result.
c     ------------------------------------------------------------------
c     CALL mulQMatTr( mDelT, nDelT, mInvSigWT, nInvSigWT,
c    &                mQuadWT, nQuadWT )
      pdA = max(nDelT(2)-nDelT(1)+1, 1)
      CALL mulDTrMat( dDelT, nDelT, mInvSigWT, nInvSigWT,
     &                mQuadWTp1, nQuadWTp1, pdA )
      CALL mulMatD( mQuadWTp1, nQuadWTp1, dDelT, nDelT,
     &              mQuadWT, nQuadWT, pdA )
c     ------------------------------------------------------------------
c     The following commented code is carried out in 2 matrix operations
c     in order to save the intermediate matrix result.
c     ------------------------------------------------------------------
c     CALL mulQMatTr( mDel, nDel, mInvSigW, nInvSigW, mQuadW, nQuadW )
      pdA = max(nDel(2)-nDel(1)+1, 1)
      CALL mulDTrMat( dDel, nDel, mInvSigW, nInvSigW,
     &                mQuadWp1, nQuadWp1, pdA )
      CALL mulMatD( mQuadWp1, nQuadWp1, dDel, nDel, mQuadW, nQuadW,
     &              pdA )

c-----------------------------------------------------------------------
c     Calculate some extraction matrices.
c-----------------------------------------------------------------------
      CALL mulMat( mSigUI, nSigUI, mQuadW, nQuadW, mFSTIIrr, nFSTIIrr )

c-----------------------------------------------------------------------
c     Calculate the estimators and partial filters.
c-----------------------------------------------------------------------
c     First the Irregular estimate and partial filter.
c     ------------------------------------------------------------------
      CALL mulMat( mFSTIIrr, nFSTIIrr, vY, nY, vIrrEst, nIrrEst )
c     ------------------------------------------------------------------
      CALL mulMat( mSigUI, nSigUI, mQuadWp1, nQuadWp1, mTemp1, nTemp1 )
      i = nTemp1(1)
      DO j = 1, nTemp1(2)
       ibase = (j-1)*i
       mIrrPFlt(j,1) = mTemp1(ibase+(i+1)/2)
       mIrrPFlt(j,2) = mTemp1(ibase+i)
      END DO
      nIrrPFlt(1) = nTemp1(2)
      nIrrPFlt(2) = 2
c     ------------------------------------------------------------------
c     And then the Seasonal estimate and partial filter.
c     ------------------------------------------------------------------
      CALL addMat( mQuadUS, nQuadUS, mQuadWT, nQuadWT, mTemp1, nTemp1 )
      CALL invMat( mTemp1, nTemp1, mTemp2, nTemp2 )
      CALL mulMat( mTemp2, nTemp2, mQuadWT, nQuadWT, mTemp1, nTemp1 )
      CALL mulMat( mTemp1, nTemp1, vY, nY, vSeaEst, nSeaEst )
c     ------------------------------------------------------------------
      CALL mulMat( mTemp2, nTemp2, mQuadWTp1, nQuadWTp1,
     &             mTemp5, nTemp5 )
      i = nTemp5(1)
      DO j = 1, nTemp5(2)
       ibase = (j-1)*i
       mSeaPFlt(j,1) = mTemp5(ibase+(i+1)/2)
       mSeaPFlt(j,2) = mTemp5(ibase+i)
      END DO
      nSeaPFlt(1) = nTemp5(2)
      nSeaPFlt(2) = 2
c     ------------------------------------------------------------------
c     Next the Trend estimate and partial filter.
c     ------------------------------------------------------------------
      CALL addMat( mQuadUT, nQuadUT, mQuadWS, nQuadWS, mTemp1, nTemp1 )
      CALL invMat( mTemp1, nTemp1, mTemp2, nTemp2 )
      CALL mulMat( mTemp2, nTemp2, mQuadWS, nQuadWS, mTemp1, nTemp1 )
      CALL mulMat( mTemp1, nTemp1, vY, nY, vTreEst, nTreEst )
c     ------------------------------------------------------------------
      CALL mulMat( mTemp2, nTemp2, mQuadWSp1, nQuadWSp1,
     &             mTemp6, nTemp6 )
      i = nTemp6(1)
      DO j = 1, nTemp6(2)
       ibase = (j-1)*i
       mTrePFlt(j,1) = mTemp6(ibase+(i+1)/2)
       mTrePFlt(j,2) = mTemp6(ibase+i)
      END DO
      nTrePFlt(1) = nTemp6(2)
      nTrePFlt(2) = 2
c     ------------------------------------------------------------------
c     Finally the SA partial filter.
c     ------------------------------------------------------------------
      pdA = max(nRedDelT(2)-nRedDelT(1)+1, 1)
      CALL mulDTrMat( dRedDelT, nRedDelT, mIrrPFlt, nIrrPFlt,
     &                mSAPFlt, nSAPFlt, pdA )
      CALL addMat( mTrePFlt, nTrePFlt, mSAPFlt, nSAPFlt,
     &             mSAPFlt, nSAPFlt )

c-----------------------------------------------------------------------
c     Calculate some covariance matrices.
c-----------------------------------------------------------------------
      CALL mulQMat( mSigUI, nSigUI, mQuadW, nQuadW, mCovIrr, nCovIrr )
c     ------------------------------------------------------------------
      CALL mulQdMatTr( dRedDelT, nRedDelT, mInvSigW, nInvSigW, 
     &                 mTemp4, nTemp4 )
      CALL mulQMat( mSigUS, nSigUS, mTemp4, nTemp4, mCovSea, nCovSea )
c     ------------------------------------------------------------------
      CALL mulQdMatTr( dRedDelS, nRedDelS, mInvSigW, nInvSigW, 
     &                 mTemp8, nTemp8 )
      CALL mulQMat( mSigUT, nSigUT, mTemp8, nTemp8, mCovTre, nCovTre )
c     ------------------------------------------------------------------
      CALL mulQdMatTr( dRedDelS, nRedDelS, mInvSigW, nInvSigW, 
     &                 mTemp8, nTemp8 )
      CALL mulQMat( mSigWT, nSigWT, mTemp8, nTemp8, mCovSA, nCovSA )

c-----------------------------------------------------------------------
      RETURN
      END