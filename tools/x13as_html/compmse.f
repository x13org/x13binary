      SUBROUTINE compMSE( nT, dS, dT, lSeaPre, dDel, nDel, dDelS, nDelS,
     &                    dDelT, nDelT, sdSig, mSigUI, nSigUI,
     &                    mInvSigUS, nInvSigUS, mInvSigUT, nInvSigUT,
     &                    mInvSigWS, nInvSigWS, mInvSigWT, nInvSigWT,
     &                    mInvSigW, nInvSigW, lInvSig,
     &                    mIrrVar, nIrrVar, mSeaVar, nSeaVar,
     &                    mTreVar, nTreVar )
c-----------------------------------------------------------------------
c     compMSE.f, Release 1, Subroutine Version 1.6, Modified 30 May 2006.
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 13 Oct 2005.
c     Modified by REG, on 07 Nov 2005, to generalize irregular component
c       via new mSigUI matrix.
c     Modified by REG, on 05 Jan 2006, to add compile time logicals
c       for enabling each of the tree covariance matrix calculations.
c     Modified by REG, on 12 Jan 2006, to optimize calculations
c       of mSeaVar and mTreVar.
c     Modified by REG, on 20 Jan 2006, to optimize processing
c       by using diagonal form of difference matrices, to use input
c       inverted mSig matrices, and to input logial switch for 
c       for calculating mTreVar matrices.
c     Modified by REG, on 27 Apr 2006, to handle special case
c       of no seasonal component.
c     Modified by REG, on 30 May 2006, to add generalized check 
c       for no seasonal component processing.
c-----------------------------------------------------------------------
c     This subroutine calculates the signal extraction MSE matrices
c     relative to the innovation variance sdSig.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c dDel    d   diagonal form of overall differencing matrix
c dDelS   d   diagonal form of seasonal differencing matrix
c dDelT   d   diagonal form of trend differencing matrix
c dS      i   size of Seasonal Differencing
c dT          i   size of Trend Differencing
c lInvSig d   logical when true then all mInvSig matrices are available
c lSeaPre l   logical indicating presence of seasonal component
c mInvSigUS d inverse of mSigUS: covariance matrix for differenced
c             seasonal
c mInvSigUT d inverse of mSigUT: covariance matrix of differenced trend
c mInvSigW d  inverse of mSigW: covariance matrix of differenced data
c mInvSigWS d inverse of mSigWS: covariance matrix of differenced
c             trend adjusted
c mInvSigWT d inverse of mSigWT: covariance matrix of differenced
c             seasonally adjusted
c mSigUI  d   covariance matrix of undifferenced irregular
c mIrrVar d   variance matrix of estimated irregular
c mSeaVar d   variance matrix of estimated seasonal
c mTreVar d   variance matrix of estimated trend
c nDel    i   size (rows,columns) of dDel
c nDelS   i   size (rows,columns) of dDelS
c nDelT   i   size (rows,columns) of dDelT
c nInvSigUS i size (rows,columns) of mInvSigUS matrix
c nInvSigUT i size (rows,columns) of mInvSigUT matrix
c nInvSigW i  size (rows,columns) of mInvSigW matrix
c nInvSigWS i size (rows,columns) of mInvSigWS matrix
c nInvSigWT i size (rows,columns) of mInvSigWT matrix
c nSigUI  i   size (rows,columns) of mSigUI matrix
c nIrrVar i   size (rows,columns) of mIrrVar matrix
c nSeaVar i   size (rows,columns) of mSeaVar matrix
c nTreVar i   size (rows,columns) of mTreVar matrix
c nT      i   size of data available
c sdSig   d   data innovation stdev
c-----------------------------------------------------------------------
c Name   Type Description (Local Variables)
c-----------------------------------------------------------------------
c lIrrVar l   when true allows calculation of mIrrVar
c lSeaVar l   when true allows calculation of mSeaVar
c lTreVar l   when true allows calculation of mTreVar
c mFTIT   d   Trend filter assuming 2 components: T + I
c mId     d   Identity matrix
c mInvFSIS d  Inverse of seasonal filter assuming 2 components: S + I
c MONE    d   constant parameter
c mQuadUS d   result of quadratic matrix operation
c mQuadUT d   result of quadratic matrix operation
c mQuadW  d   result of quadratic matrix operation
c mQuadWS d   result of quadratic matrix operation
c mQuadWT d   result of quadratic matrix operation
c mTemp1  d   temporary matrix 1
c nFTIT   i   size (rows,columns) of mFTIT matrix
c nId     i   size (rows,columns) of mId matrix
c nInvFSIS i  size (rows,columns) of mInvFSIS matrix
c nQuadW  i   size (rows,columns) of nQuadW matrix
c nQuadUS i   size (rows,columns) of nQuadUS matrix
c nQuadUT i   size (rows,columns) of nQuadUT matrix
c nQuadWS i   size (rows,columns) of nQuadWS matrix
c nQuadWT i   size (rows,columns) of nQuadWT matrix
c nSave   i   identifies default size of large matrices
c             that are saved (not dynamic)
c nTemp1  i   size (rows,columns) of mTemp1 matrix
c ZERO    d   constant parameter
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INTEGER nT, dS, dT
      INTEGER nDel(2), nDelS(2), nDelT(2)
      INTEGER nSigUI(2)
      INTEGER nInvSigUS(2), nInvSigUT(2)
      INTEGER nInvSigW(2), nInvSigWS(2), nInvSigWT(2)
      INTEGER nIrrVar(2), nSeaVar(2), nTreVar(2)
      LOGICAL lInvSig, lSeaPre
      DOUBLE PRECISION sdSig
      DOUBLE PRECISION dDel(dS+dT+1), dDelS(dS+1), dDelT(dT+1)
c     DOUBLE PRECISION mDel(nT-dS-dT,nT), mDelS(nT-dS,nT),
c    &                 mDelT(nT-dT,nT)
      DOUBLE PRECISION mSigUI(nT,nT)
      DOUBLE PRECISION mIrrVar(nT,nT), mSeaVar(nT,nT), mTreVar(nT,nT)
      DOUBLE PRECISION mInvSigUS(nT-dS,nT-dS), mInvSigUT(nT-dT,nT-dT),
     &                 mInvSigWS(nT-dS,nT-dS), mInvSigWT(nT-dT,nT-dT),
     &                 mInvSigW(nT-dS-dT,nT-dS-dT)

c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      LOGICAL lIrrVar, lSeaVar, lTreVar
      INTEGER nFTIT(2), nId(2), nInvFSIS(2), nTemp1(2)
      INTEGER nQuadUS(2), nQuadUT(2), nQuadWS(2), nQuadWT(2), nQuadW(2)
      DOUBLE PRECISION MINUSONE, ZERO
      PARAMETER (lIrrVar=.false., lSeaVar=.true., lTreVar=.true.)
      PARAMETER (MINUSONE=-1.0D0, ZERO=0.0D0)
      
c     ------------------------------------------------------------------
c     Dynamic (commented) versus static (uncommented) matrices
c     ------------------------------------------------------------------
c     DOUBLE PRECISION mTemp2(nT,nT), 
c     DOUBLE PRECISION mQuadUS(nT,nT), mQuadUT(nT,nT),
c    &                 mQuadWS(nT,nT), mQuadWT(nT,nT), mQuadW(nT,nT)
c     ------------------------------------------------------------------
      INTEGER nSave
      PARAMETER (nSave=POBS*POBS)
      DOUBLE PRECISION mId(nSave), mTemp1(nSave)
      DOUBLE PRECISION mFTIT(nSave), mInvFSIS(nSave)
      DOUBLE PRECISION mQuadUS(nSave), mQuadUT(nSave),
     &                 mQuadWS(nSave), mQuadWT(nSave), mQuadW(nSave)
      SAVE mFTIT, mId, mQuadUS, mQuadW, mTemp1
      EQUIVALENCE (mQuadUS, mQuadUT), (mFTIT, mInvFSIS),
     &            (mQuadW, mQuadWS), (mQuadW, mQuadWT)

c-----------------------------------------------------------------------
c     Calculate the Irregular extraction MSE matrices.
c-----------------------------------------------------------------------
c     Calculate a quadratic matrix.
c     ------------------------------------------------------------------
      IF (lIrrVar) THEN
        CALL mulQdMatTr( dDel, nDel, mInvSigW, nInvSigW,
     &                   mQuadW, nQuadW )
        CALL mulQMat( mSigUI, nSigUI, mQuadW, nQuadW, mTemp1, nTemp1 )
c     ------------------------------------------------------------------
c     Calculate the MSE matrix relative to sdSig,
c     i.e. do not multiply by sdSig^2.
c     ------------------------------------------------------------------
        CALL mulSca( MINUSONE, mTemp1, nTemp1 )
        CALL addMat( mSigUI, nSigUI, mTemp1, nTemp1, mIrrVar, nIrrVar )
c       CALL mulSca( DBLE(sdSig*sdSig), mIrrVar, nIrrVar )
      END IF

c-----------------------------------------------------------------------
c     Calculate the Seasonal extraction MSE matrices.
c-----------------------------------------------------------------------
c     Calculate some quadratic matrices.
c     ------------------------------------------------------------------
      IF (lSeaVar) THEN
       IF (lSeaPre) THEN
        CALL mulQdMatTr( dDelS, nDelS, mInvSigUS, nInvSigUS, 
     &                   mQuadUS, nQuadUS )
c     ------------------------------------------------------------------
        CALL mulQdMatTr( dDelT, nDelT, mInvSigWT, nInvSigWT,
     &                   mQuadWT, nQuadWT )
c     ------------------------------------------------------------------
c     Calculate the MSE matrix relative to sdSig,
c     i.e. do not multiply by sdSig^2.
c     ------------------------------------------------------------------
        CALL addMat( mQuadWT, nQuadWT, mQuadUS, nQuadUS,
     &               mTemp1, nTemp1 )
        CALL invMat( mTemp1, nTemp1, mSeaVar, nSeaVar )
c       CALL mulSca( DBLE(sdSig*sdSig), mSeaVar, nSeaVar )
       ELSE
        CALL getIdM( nT, mSeaVar, nSeaVar )
        CALL mulSca( ZERO, mSeaVar, nSeaVar )
       END IF
      END IF

c-----------------------------------------------------------------------
c     Calculate the Trend extraction MSE matrices.
c-----------------------------------------------------------------------
      IF (lTreVar) THEN
c     ------------------------------------------------------------------
c     If the inverses for mSigUT and mSigWS are available then 
c     calculate mTreVar starting from scratch.
c     ------------------------------------------------------------------
        IF (lInvSig) THEN
c     ------------------------------------------------------------------
c     Calculate some quadratic matrices.
c     ------------------------------------------------------------------
          CALL mulQdMatTr( dDelT, nDelT, mInvSigUT, nInvSigUT, 
     &                     mQuadUT, nQuadUT )
          CALL mulQdMatTr( dDelS, nDelS, mInvSigWS, nInvSigWS, 
     &                     mQuadWS, nQuadWS )
c     ------------------------------------------------------------------
c     Calculate the MSE matrix relative to sdSig,
c     i.e. do not multiply by sdSig^2.
c     ------------------------------------------------------------------
          CALL addMat( mQuadUT, nQuadUT, mQuadWS, nQuadWS,
     &                 mTemp1, nTemp1 )
          CALL invMat( mTemp1, nTemp1, mTreVar, nTreVar )
c         CALL mulSca( DBLE(sdSig*sdSig), mTreVar, nTreVar )
c     ------------------------------------------------------------------
c     If mSeaVar previously calculated then take advantage previous
c     calculation of mSeaVar in order to calculate mTreVar using
c     MSE(\hat{T}) = inv(F_{SI}^S) x MSE(\hat(S)) x (F_{TI}^T)'.
c     ------------------------------------------------------------------
        ELSE IF (lSeaVar) THEN
         IF (lSeaPre) THEN
c     ------------------------------------------------------------------
c     Get Id Matrix
c     ------------------------------------------------------------------
          CALL getIdM( nT, mId, nId )
c     ------------------------------------------------------------------
c     Calculate inverse of F_{SI}^S (2 component filter)
c     ------------------------------------------------------------------
          CALL mulMat( mSigUI, nSigUI, mQuadUS, nQuadUS,
     &                 mInvFSIS, nInvFSIS )
          CALL addMat( mId, nId, mInvFSIS, nInvFSIS,
     &                 mInvFSIS, nInvFSIS )
c     ------------------------------------------------------------------
          CALL mulMat( mInvFSIS, nInvFSIS, mSeaVar, nSeaVar,
     &                 mTemp1, nTemp1 )
c     ------------------------------------------------------------------
c     Calculate F_{TI}^T (2 component filter)
c     ------------------------------------------------------------------
          CALL mulMat( mSigUI, nSigUI, mQuadWT, nQuadWT, mFTIT, nFTIT )
          CALL mulSca( MINUSONE, mFTIT, nFTIT )
          CALL addMat( mId, nId, mFTIT, nFTIT, mFTIT, nFTIT )
c     ------------------------------------------------------------------
          CALL mulMatTr( mTemp1, nTemp1, mFTIT, nFTIT,
     &                   mTreVar, nTreVar )
c     ------------------------------------------------------------------
c     Else for no seasonal component, calculate 
c     MSE(\hat{T}) = F_{TI}^T x \Sigma_I
c     ------------------------------------------------------------------
         ELSE
c     ------------------------------------------------------------------
c     Calculate F_{TI}^T (2 component filter)
c     ------------------------------------------------------------------
          CALL mulQdMatTr( dDelT, nDelT, mInvSigWT, nInvSigWT,
     &                     mQuadWT, nQuadWT )
          CALL mulMat( mSigUI, nSigUI, mQuadWT, nQuadWT, mFTIT, nFTIT )
          CALL mulSca( MINUSONE, mFTIT, nFTIT )
          CALL addMat( mId, nId, mFTIT, nFTIT, mFTIT, nFTIT )
c     ------------------------------------------------------------------
c     Calculate MSE(\Hat{T})
c     ------------------------------------------------------------------
          CALL mulMat( mFTIT, nFTIT, mSigUI, nSigUI, mTreVar, nTreVar )
         END IF
        END IF
      END IF

c-----------------------------------------------------------------------
      RETURN
      END