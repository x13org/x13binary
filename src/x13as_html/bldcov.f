      SUBROUTINE bldCov( nT, dS, dT, nPer, lSeaPre,
     &                   lSigUf, lInvSigUS, lInvSigUT,
     &                   lInvSigW, lInvSigWS, lInvSigWT,
     &                   vSeaAR, oSeaAR, vSeaMA, oSeaMA,
     &                   vTreAR, oTreAR, vTreMA, oTreMA,
     &                   vCycAR, oCycAR, vCycMA, oCycMA,
     &                   dDel, nDel, dDelS, nDelS, dDelT, nDelT,
     &                   dRedDelS, nRedDelS, dRedDelT, nRedDelT,
     &                   sSeaVar, sTreVar, sCycVar, sIrrVar,
     &                   mSigUS, nSigUS, mSigUT, nSigUT, mSigUI, nSigUI,
     &                   mSigWS, nSigWS, mSigWT, nSigWT, mSigW, nSigW,
     &                   mSigUTf, nSigUTf, mSigUTfUT, nSigUTfUT,
     &                   mSigWTf, nSigWTf, mSigWTfWT, nSigWTfWT,
     &                   mSigWf, nSigWf, mSigWfW, nSigWfW,
     &                   mInvSigUS, nInvSigUS, mInvSigUT, nInvSigUT,
     &                   mInvSigWS, nInvSigWS, mInvSigWT, nInvSigWT,
     &                   mInvSigW, nInvSigW )
c-----------------------------------------------------------------------
c     bldCov.f, Release 1, Subroutine Version 1.8, Modified 30 May 2006.
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 04 Apr 2005.
c     Modified by REG, on 15 Sep 2005, to change local variable sResult
c       from a non-dimensioned variable to a vector of size one.
c     Modified by REG, on 20 Sep 2005, to move calculation of sdSigAlt 
c       to extSgnl(), and to correct tab stops.
c     Modified by REG, on 07 Nov 2005, to generalize structure of 
c       irregular component, by adding mSigUI matrix; to modify 
c       calculation of mSigWT and mSigWS; and to add cycle covariance 
c       matrix to irregular covariance matrix, instead of to 
c       trend covariance matrix.
c     Modified by REG, on 05 Jan 2006, to add logical
c       for inverting mSigW.
c     Modified by REG, on 20 Jan 2006, to optimze processing
c        by using diagonal form of mDel matrices.
c     Modified by REG, on 04 Apr 2006, to add calculation of
c        future covariance matrices for UT, W, and WT.
c     Modified by REG, on 27 Apr 2006, to handle special case
c       of no seasonal component.
c     Modified by REG, on 30 May 2006, to add generalized check 
c       for no seasonal component processing.
c-----------------------------------------------------------------------
c     This subroutine calculates the covariance matrices for differenced
c     trend and differenced seasonal.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c dS      i   size of Seasonal Differencing
c dT      i   size of Trend Differencing
c lInvSigUS l logical when true to invert mSigUS
c lInvSigUT l logical when true to invert mSigUT
c lInvSigW  l logical when true to invert mSigW
c lInvSigWS l logical when true to invert mSigWS
c lInvSigWT l logical when true to invert mSigWT
c lSeaPre l   logical indicating rpesence of seasonal component
c lSigUf  l   logical to generate future covariance matrices
c dDel    d   diagonal form of overall differencing matrix: mDel
c dDelS   d   diagonal form of seasonal differencing matrix: mDelS
c dDelT   d   diagonal form of trend differencing matrix: mDelT
c mInvSigUS d contains inverse of mSigUS
c mInvSigUT d contains inverse of mSigUT
c mInvSigW  d contains inverse of mSigW
c mInvSigWS d contains inverse of mSigWS
c mInvSigWT d contains inverse of mSigWT
c dRedDelS d  diagonal form of smaller version of mDelS
c dRedDelT d  diagonal form of smaller version of mDelT
c mSigUI  d   covariance matrix for undifferenced irregular
c mSigUS  d   covariance matrix for differenced seasonal
c mSigUT  d   covariance matrix for differenced trend (UT)
c mSigUTf d   covariance matrix for future differenced trend (UTf)
c mSigUTfUT d cross covariance matrix for (UTf,UT)
c mSigW   d   covariance matrix for differenced data (W)
c mSigWf  d   covariance matrix for future differenced data (Wf)
c mSigWfW d   cross covariance matrix for (Wf,W)
c mSigWS  d   covariance matrix for differenced trend adjusted
c mSigWT  d   covariance matrix for differenced seasonally adjusted (WT)
c mSigWTf d   covariance matrix for future WT (WTf)
c mSigWTfWT d cross covariance matrix for (WTf,WT)
c nDel    i   size (rows,columns) of mDel
c nDelS   i   size (rows,columns) of mDelS
c nDelT   i   size (rows,columns) of mDelT
c nInvSigUS i size (rows,columns) of mInvSigUS matrix
c nInvSigUT i size (rows,columns) of mInvSigUT matrix
c nInvSigW  i size (rows,columns) of mInvSigW  matrix
c nInvSigWS i size (rows,columns) of mInvSigWS matrix
c nInvSigWT i size (rows,columns) of mInvSigWT matrix
c nPer    i   length of period asssociated with the seasonal component
c nRedDelS i  size (rows,columns) of mRedDelS
c nRedDelT i  size (rows,columns) of mRedDelT
c nSigUI  i   size (rows,columns) of mSigUI matrix
c nSigUS  i   size (rows,columns) of mSigUS matrix
c nSigUT  i   size (rows,columns) of mSigUT matrix
c nSigUTf i   size (rows,columns) of mSigUTf matrix
c nSigUTfUT i size (rows,columns) of mSigUTfUT matrix
c nSigW   i   size (rows,columns) of mSigW matrix
c nSigWf  i   size (rows,columns) of mSigWf matrix
c nSigWfW i   size (rows,columns) of mSigWfW matrix
c nSigWS  i   size (rows,columns) of mSigWS matrix
c nSigWT  i   size (rows,columns) of mSigWT matrix
c nSigWTf i   size (rows,columns) of mSigWTf matrix
c nSigWTfWT i size (rows,columns) of mSigWTfWT matrix
c nT      i   size of data available
c oCycAR  i   max order of vCycAR polynomial
c oCycAR  i   max order of vCycMA polynomial
c oSeaMA  i   max order of vSeaAR polynomial
c oSeaAR  i   max order of vSeaMA polynomial
c oTreAR  i   max order of vTreAR polynomial
c oTreMA  i   max order of vTreMA polynomial
c sCycVar d   cycle innovation variance
c sIrrVar d   irregular innovation variance
c sSeaVar d   seasonal innovation variance
c sTreVar d   trend innovation variance
c vCycAR  d   AR polynomial vector for cycle component
c vCycMA  d   MA polynomial vector for cycle component
c vSeaAR  d   AR polynomial vector for seasonal component
c vSeaMA  d   MA polynomial vector for seasonal component
c vTreAR  d   AR polynomial vector for Trend component
c vTreMA  d   MA polynomial vector for Trend component
c-----------------------------------------------------------------------
c Name   Type Description (Local Variables)
c-----------------------------------------------------------------------
c i       i   for index varable
c ind1    i   result indicator variable from GTWACF (1 = ok)
c ind2    i   result indicator variable from GTWACF (1 = ok)
c ind3    i   result indicator variable from GTWACF (1 = ok)
c mPartA  d   temporary matrix used to calculate mSigW
c mSigUC  d   covariance matrix for differenced cycle
c mU      d   working covariance matrix for any component 
c             including future elements
c mW      d   local version of mSigW including future elements
c mWT     d   local version of mSigWT including future elements
c nFurDel i   size (rows,columns) of future version of dDel
c               using full differencing
c nFurDelS i  size (rows,columns) of future version of dDelS
c               using trend differencing
c nFurDelT i  size (rows,columns) of future version of dDelT 
c               using seasonal differencing
c nForTDelT i size (rows,columns) of future version of dDelT
c nLags   i   identifies maximum number of lags to calculate
c nPartA  i   size (rows,columns) of mPartA matrix
c nSave   i   identifies default size of large matrices
c               that are saved (not dynamic)
c nSigUC  i   size (rows,columns) of mSigUC matrix
c nU      i   size (rows,columns) of mU matrix
c nW      i   size (rows,columns) of mW matrix
c nWT     i   size (rows,columns) of mWT matrix
c vCorC   d   vector of Cycle autocorrelations
c vCorS   d   vector of Seasonal autocorrelations
c vCorT   d   vector of Trend autocorrelations
c vCovC   d   vector of Cycle autocovariances
c vCovS   d   vector of Seasonal autocovariances
c vCovT   d   vector of Trend autocovariances
c ZERO    d   constant parameter
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      LOGICAL lInvSigUS, lInvSigUT, lInvSigW, lInvSigWS, lInvSigWT,
     &        lSeaPre, lSigUf
      INTEGER dS, dT, nPer, nT
      INTEGER oSeaAR, oSeaMA, oTreAR, oTreMA, oCycAR, oCycMA
      INTEGER nDel(2), nDelS(2), nDelT(2), nRedDelS(2), nRedDelT(2)
      INTEGER nSigUI(2), nSigUS(2), nSigUT(2)
      INTEGER nSigWS(2), nSigWT(2), nSigW(2)
      INTEGER nInvSigUS(2), nInvSigUT(2)
      INTEGER nInvSigWS(2), nInvSigWT(2), nInvSigW(2)
      INTEGER nSigUTf(2), nSigUTfUT(2), nSigWTf(2), nSigWTfWT(2)
      INTEGER nSigWf(2), nSigWfW(2)
      DOUBLE PRECISION sSeaVar, sTreVar, sCycVar, sIrrVar
      DOUBLE PRECISION vSeaAR(0:oSeaAR), vSeaMA(0:oSeaMA)
      DOUBLE PRECISION vTreAR(0:oTreAR), vTreMA(0:oTreMA)
      DOUBLE PRECISION vCycAR(0:oCycAR), vCycMA(0:oCycMA)
      DOUBLE PRECISION dDel(dS+dT+1), dDelS(dS+1), dDelT(dT+1),
     &                 dRedDelS(dS+1), dRedDelT(dT+1)
c     DOUBLE PRECISION mDel(nT-dS-dT,nT), mDelS(nT-dS,nT),
c    &                 mDelT(nT-dT,nT), mRedDelS(nT-dS-dT,nT-dT),
c    &                 mRedDelT(nT-dS-dT,nT-dS)
      DOUBLE PRECISION mSigUS(nT-dS,nT-dS), mSigUT(nT-dT,nT-dT), 
     &                 mSigUI(nT,nT)
      DOUBLE PRECISION mSigWS(nT-dS,nT-dS), mSigWT(nT-dT,nT-dT),
     &                 mSigW(nT-dS-dT,nT-dS-dT)
      DOUBLE PRECISION mInvSigUS(nT-dS,nT-dS), mInvSigUT(nT-dT,nT-dT)
      DOUBLE PRECISION mInvSigWS(nT-dS,nT-dS), mInvSigWt(nT-dT,nT-dT),
     &                 mInvSigW(nT-dS-dT,nT-dS-dT)
      DOUBLE PRECISION mSigUTf(nPer,nPer), mSigUTfUT(nPer,nT-dT),
     &                 mSigWTf(nPer,nPer), mSigWTfWT(nPer,nT-dT),
     &                 mSigWf(nPer,nPer),  mSigWfW(nPer,nT-dS-dT)

c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER ind1, ind2, ind3, i, nLags, ppqa, pp1
      INTEGER nPartA(2), nSigUC(2), nU(2), nW(2), nWT(2)
      INTEGER nFurDel(2), nFurDelS(2), nFurDelT(2), nForTDelT(2)
      DOUBLE PRECISION vCovS(nT+nPer), vCorS(nT+nPer-1)
      DOUBLE PRECISION vCovT(nT+nPer), vCorT(nT+nPer-1)
      DOUBLE PRECISION vCovC(nT+nPer), vCorC(nT+nPer-1)
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.0d0)

c     ------------------------------------------------------------------
c     Dynamic (commented) versus static (uncommented) matrices
c     ------------------------------------------------------------------
c     DOUBLE PRECISION mPartA(nT-dS-dT,nT-dS-dT)
c     DOUBLE PRECISION mSigUC(nT,nT)
c     ------------------------------------------------------------------
      INTEGER nSave,nSave2
      PARAMETER ( nSave=POBS*POBS, nSave2=(POBS+12)*(POBS+12) )
      DOUBLE PRECISION mPartA(nSave2), mSigUC(nSave2),
     &                 mU(nSave2), mW(nSave2), mWT(nSave2)
      SAVE mPartA, mSigUC, mU, mW, mWT

c-----------------------------------------------------------------------
c     Initialize some size variables: for diagonal matrices, 
c     and for working matrices mW and mWT.
c-----------------------------------------------------------------------
      IF ( lSigUf ) THEN
        nLags = nT + nPer
        nFurDel(1) = nT + nPer - dS - dT
        nFurDel(2) = nT + nPer
        nFurDelS(1) = nT + nPer - dS - dT
        nFurDelS(2) = nT + nPer - dT
        nFurDelT(1) = nT + nPer - dS - dT
        nFurDelT(2) = nT + nPer - dS
        nForTDelT(1) = nT + nPer - dT
        nForTDelT(2) = nT + nPer
      ELSE
        nLags = nT
      END IF
      nW(1) = 0
      nW(2) = 0
      nWT(1) = 0
      nWT(2) = 0

c-----------------------------------------------------------------------
c     Calculate seasonal, trend, and cycle auto covariance functions
c-----------------------------------------------------------------------
      ppqa = max( oSeaAR, oSeaMA, nLags )
      pp1 = max( oSeaAR, 1 )
      CALL GTWACF( oSeaAR, oSeaMA, nLags, vSeaAR, vSeaMA, sSeaVar,
     &             vCovS, vCorS, ind1, ppqa, pp1 )
      ppqa = max( oTreAR, oTreMA, nLags )
      pp1 = max( oTreAR, 1 )
      CALL GTWACF( oTreAR, oTreMA, nLags, vTreAR, vTreMA, sTreVar,
     &             vCovT, vCorT, ind2, ppqa, pp1 )
      ppqa = max( oCycAR, oCycMA, nLags )
      pp1 = max( oCycAR, 1 )
      CALL GTWACF( oCycAR, oCycMA, nLags, vCycAR, vCycMA, sCycVar,
     &             vCovC, vCorC, ind3, ppqa, pp1 )

c     ------------------------------------------------------------------
c     Debug code.
c     ------------------------------------------------------------------
c     WRITE(6,1000)(vSeaAR(i),i=0,oSeaAR)
c     WRITE(6,1000)(vSeaMA(i),i=0,oSeaMA)
c     WRITE(6,1001)oSeaAR,oSeaMA,sSeaVar
c     WRITE(6,1000)(vCovS(i),i=1,nT)
c     WRITE(6,1001)nT,ind1
c1000	FORMAT( 300(1x,G12.5) )
c1001	FORMAT( 2(1x,I5), (1x,G12.5) )
c     ------------------------------------------------------------------
c     WRITE(6,1000)(vTreAR(i),i=0,oTreAR)
c     WRITE(6,1000)(vTreMA(i),i=0,oTreMA)
c     WRITE(6,1001)oTreAR,oTreMA,sTreVar
c     WRITE(6,1000)(vCovT(i),i=1,nT)
c     WRITE(6,1001)nT,ind2
c     ------------------------------------------------------------------
c     WRITE(6,1000)(vCycAR(i),i=0,oCycAR)
c     WRITE(6,1000)(vCycMA(i),i=0,oCycMA)
c     WRITE(6,1001)oCycAR,oCycMA,sCycVar
c     WRITE(6,1000)(vCovC(i),i=1,nT)
c     WRITE(6,1001)nT,ind3

c-----------------------------------------------------------------------
c     Calculate output covariance matrices
c-----------------------------------------------------------------------
c     Get Toeplitz matrices for seasonal component
c     ------------------------------------------------------------------
      IF (ind1 .eq. 0) THEN
        IF ( .not.lSigUf ) THEN
          CALL getTpltz( vCovS, nLags, nT-dS, mSigUS, nSigUS )

c     ------------------------------------------------------------------
c     If future covariance matrices required
c     then extract from overall covariance matrix
c     and initialize mW matrix
c     ------------------------------------------------------------------
        ELSE
          CALL getTpltz( vCovS, nLags, nT-dS+nPer, mU, nU )
          CALL getSMat( mU, nU, 1, nT-dS, mSigUS, nSigUS )
          CALL mulQdMat( dDelT, nFurDelT, mU, nU, mW, nW )
        END IF

c     ------------------------------------------------------------------
c     Else error encountered when calculating autocovariances
c     for desired lags
c     ------------------------------------------------------------------
      ELSE
        nSigUS(1) = 0
        nSigUS(2) = 0
      END IF

c     ------------------------------------------------------------------
c     Get Toeplitz matrices for trend component
c     ------------------------------------------------------------------
      IF (ind2 .eq. 0) THEN
        IF ( .not.lSigUf ) THEN
          CALL getTpltz( vCovT, nT, nT-dT, mSigUT, nSigUT )

c     ------------------------------------------------------------------
c     If future covariance matrices required
c     then extract from overall covariance matrix,
c     adjust mW matrix, and initialize mWT matrix
c     ------------------------------------------------------------------
        ELSE 
          CALL getTpltz( vCovT, nT+nPer, nT-dT+nPer, mU, nU )
          CALL getSMat( mU, nU, 1, nT-dT, mSigUT, nSigUT )
          CALL getSMat( mU, nU, nT-dT+1, nT-dT+nPer, mSigUTf, nSigUTf )
          CALL getSRMat( mU, nU, nT-dT+1, nT-dT+nPer, 1, nT-dT,
     &                   mSigUTfUT, nSigUTfUT )
          CALL mulQdMat( dDelS, nFurDelS, mU, nU, mPartA, nPartA )
          CALL addMat( mPartA, nPartA, mW, nW, mW, nW )
          CALL cpyMat( mU, nU, mWT, nWT )
        END IF

c     ------------------------------------------------------------------
c     Else error encountered when calculating autocovariances
c     for desired lags
c     ------------------------------------------------------------------
      ELSE
        nSigUT(1) = 0
        nSigUT(2) = 0
      END IF

c     ------------------------------------------------------------------
c     Get Toeplitz matrices for cycle component
c     ------------------------------------------------------------------
      IF (ind3 .eq. 0) THEN
        CALL getTpltz( vCovC, nLags, nLags, mSigUC, nSigUC )

c     ------------------------------------------------------------------
c     Else error encountered when calculating autocovariances
c     for desired lags
c     ------------------------------------------------------------------
      ELSE
        nSigUC(1) = 0
        nSigUC(2) = 0
      END IF

c     ------------------------------------------------------------------
c     Get Toeplitz matrices for irregular component
c     ------------------------------------------------------------------
      IF ( .not.lSigUf ) THEN
        CALL getIdM( nLags, mSigUI, nSigUI )
        CALL mulSca( sIrrVar, mSigUI, nSigUI )
      ELSE
        CALL getIdM( nLags, mU, nU )
        CALL mulSca( sIrrVar, mU, nU )
      END IF

c     ------------------------------------------------------------------
c     Add mSigUC to mSigUI
c     ------------------------------------------------------------------
      IF ( .not.lSigUf ) THEN
        CALL addMat( mSigUC, nSigUC, mSigUI, nSigUI, mSigUI, nSigUI )

c     ------------------------------------------------------------------
c     If future covariance matrices required
c     then adjust mW matrix and mWT matrix
c     ------------------------------------------------------------------
      ELSE
        CALL addMat( mSigUC, nSigUC, mU, nU, mU, nU )
        CALL getSMat( mU, nU, 1, nT, mSigUI, nSigUI )
        CALL mulQdMat( dDel, nFurDel, mU, nU, mPartA, nPartA )
        CALL addMat( mPartA, nPartA, mW, nW, mW, nW )
        CALL mulQdMat( dDelT, nForTDelT, mU, nU, mPartA, nPartA )
        CALL addMat( mPartA, nPartA, mWT, nWT, mWT, nWT )
      END IF

c     ------------------------------------------------------------------
c     Calculate mInvSigUS and mInvSigUT
c     ------------------------------------------------------------------
      IF (lInvSigUS) THEN
       IF (lSeaPre) THEN
        CALL invMat( mSigUS, nSigUS, mInvSigUS, nInvSigUS )
       ELSE
        CALL getIdM( nT, mInvSigUS, nInvSigUS )
        CALL mulSca( ZERO, mInvSigUS, nInvSigUS )
       END IF
      END IF
c     ------------------------------------------------------------------
      IF (lInvSigUT) THEN
        CALL invMat( mSigUT, nSigUT, mInvSigUT, nInvSigUT )
      END IF

c     ------------------------------------------------------------------
c     Calculate mSigWS only if inverse needed.
c     ------------------------------------------------------------------
      IF (lInvSigWS) THEN
        CALL mulQdMat( dDelS, nDelS, mSigUI, nSigUI, mSigWS, nSigWS )
        CALL addMat( mSigUS, nSigUS, mSigWS, nSigWS, mSigWS, nSigWS )
        CALL invMat( mSigWS, nSigWS, mInvSigWS, nInvSigWS )
      END IF

c     ------------------------------------------------------------------
c     Calculate mSigWT only if inverse needed.
c     ------------------------------------------------------------------
      IF (lInvSigWT) THEN
        IF ( .not.lSigUf ) THEN
          CALL mulQdMat( dDelT, nDelT, mSigUI, nSigUI, mSigWT, nSigWT )
          CALL addMat( mSigUT, nSigUT, mSigWT, nSigWT, mSigWT, nSigWT )

c     ------------------------------------------------------------------
c     If future covariance matrices required
c     then extract from overall covariance matrix mWT
c     ------------------------------------------------------------------
        ELSE
          CALL getSMat( mWT, nWT, 1, nT-dT, mSigWT, nSigWT )
          CALL getSMat( mWT, nWT, nT-dT+1, nT+nPer-dT,
     &                  mSigWTf, nSigWTf )
          CALL getSRMat( mWT, nWT, nT-dT+1, nT+nPer-dT, 1, nT-dT,
     &                   mSigWTfWT, nSigWTfWT )
        END IF
        CALL invMat( mSigWT, nSigWT, mInvSigWT, nInvSigWT )
      END IF

c     ------------------------------------------------------------------
c     Calculate mSigW only if inverse needed.
c     ------------------------------------------------------------------
      IF (lInvSigW) THEN
        IF ( .not.lSigUf ) THEN
          CALL mulQdMat( dRedDelS, nRedDelS, mSigUT, nSigUT,
     &                   mSigW, nSigW )
          CALL mulQdMat( dRedDelT, nRedDelT, mSigUS, nSigUS,
     &                   mPartA, nPartA )
          CALL addMat( mPartA, nPartA, mSigW, nSigW, mSigW, nSigW )
          CALL mulQdMat( dDel, nDel, mSigUI, nSigUI, mPartA, nPartA )
          CALL addMat( mPartA, nPartA, mSigW, nSigW, mSigW, nSigW )

c     ------------------------------------------------------------------
c     If future covariance matrices required
c     then extract from overall covariance matrix mW
c     ------------------------------------------------------------------
        ELSE
          CALL getSMat( mW, nW, 1, nT-dS-dT, mSigW, nSigW )
          CALL getSMat( mW, nW, nT-dS-dT+1, nT+nPer-dS-dT,
     &                  mSigWf, nSigWf )
          CALL getSRMat( mW, nW, nT-dS-dT+1, nT-dS-dT+nPer,
     &                   1, nT-dS-dT, mSigWfW, nSigWfW )
        END IF

c     ------------------------------------------------------------------
c     Calculate inverse of mSigW
c     ------------------------------------------------------------------
        CALL invMat( mSigW, nSigW, mInvSigW, nInvSigW )
      END IF

c     ------------------------------------------------------------------
      RETURN
      END