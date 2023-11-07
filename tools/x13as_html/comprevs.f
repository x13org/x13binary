      SUBROUTINE compRevs( dS, dT, nT, nPer, nDiff, lSeaPre,
     &                     nSize, nSize2, nSize3,
     &                     vSeaAR, oSeaAR, vSeaMA, oSeaMA,
     &                     vTreAR, oTreAR, vTreMA, oTreMA,
     &                     vCycAR, oCycAR, vCycMA, oCycMA,
     &                     vSeaD,  oSeaD,  vTreD,  oTreD,
     &                     sSeaVar, sTreVar, sCycVar, sIrrVar,
     &                     mDelS, dDelS, nDelS,
     &                     mDelT, dDelT, nDelT,
     &                     mDel, dDel, nDel,
     &                     mRedDelS, dRedDelS, nRedDelS,
     &                     mRedDelT, dRedDelT, nRedDelT,
     &                     mSigUS, nSigUS, mSigUT, nSigUT,
     &                     mSigUI, nSigUI, mSigWS, nSigWS,
     &                     mSigWT, nSigWT, mSigW, nSigW,
     &                     mSigUTf, nSigUTf, mSigUTfUT, nSigUTfUT,
     &                     mSigWTf, nSigWTf, mSigWTfWT, nSigWTfWT,
     &                     mSigWf, nSigWf, mSigWfW, nSigWfW,
     &                     mInvSigUS, nInvSigUS, mInvSigUT, nInvSigUT,
     &                     mInvSigWS, nInvSigWS, mInvSigWT, nInvSigWT,
     &                     mInvSigW, nInvSigW, mIrrVar, nIrrVar,
     &                     mSeaVar, nSeaVar, mTreVar, nTreVar,
     &                     sdSigAlt, curMSEs, finMSEs, finRevs )
c-----------------------------------------------------------------------
c     bldDif.f, Release 1, Subroutine Version 1.6, Modified 30 May 2006.
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 20 Oct 2005.
c     Modified by REG, on 07 Nov 2005, to generalize irregular component
c       via new mSigUI matrix.
c     Modified by REG, on 05 Jan 2006, to disable inverting of mSigW
c       in bldCov().
c     Modified by REG, on 20 Jan 2006, to optimize processing, 
c       by using diagonal form of mDel matrices.
c     Modified by REG, on 07 Apr 2006, to determine opitmization
c       based on non-zero seasonal and trend innovation variances.
c     Modified by REG, on 14 Apr 2006, to add future covariance 
c       matrices associated with bldCov().
c     Modified by REG, on 30 May 2006, to add generalized check 
c       for no seasonal component processing.
c-----------------------------------------------------------------------
c     This subroutine calculates finite revisions for lead times ranging
c     from one through five years.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c dS      i   size of Seasonal Differencing
c dT      i   size of Trend Differencing
c curMSEs d   MSEs of concurrent component estimates
c             (1: irregular, 2: seasonal, 3: trend)
c finMSEs d   MSEs of concurrent component estimates
c             with up to 5 years of future observations
c finRevs d   finite revisions of concurrent component estimates
c             with up to 5 years of future observations
c nPer    i   size of seasonal period
c nSize   i   allocated storage size for large matrices
c nSize2  i   allocated storage size for large matrices
c             (including future elements)
c nSize3  i   allocated storage size for large cross covariance matrices
c nT      i   number of observations available
c-----------------------------------------------------------------------
c Name   Type Description (bldDif Input/Output Variables)
c-----------------------------------------------------------------------
c dDel    d   diagonal form of overall differencing matrix
c dDelS   d   diagonal form of seasonal differencing matrix
c dDelT   d   diagonal form of trend differencing matrix
c dRedDelS d  diagonal form of smaller version of mDelS
c dRedDelT d  diagonal form of smaller version of mDelT
c mDel    d   overall differencing matrix
c mDelS   d   seasonal differencing matrix
c mDelT   d   trend differencing matrix
c nDiff   i   vector of (d,D) differencing orders
c mRedDelS d  smaller version of mDelS
c mRedDelT d  smaller version of mDelT
c nDel    i   size (rows,columns) of mDel
c nDelS   i   size (rows,columns) of mDelS
c nDelT   i   size (rows,columns) of mDelT
c nRedDelS i  size (rows,columns) of mRedDelS
c nRedDelT i  size (rows,columns) of mRedDelT
c oSeaD   i   max order of vSeaD polynomial
c oTreD   i   max order of vTreD polynomial
c vSeaD   d   seasonal differencing polynomial of size nSeaD
c vTreD   d   trend differencing polynomial of size vTreD
c-----------------------------------------------------------------------
c Name   Type Description (bldCov Input/Output Variables)
c-----------------------------------------------------------------------
c mInvSigUS d contains inverse of mSigUS
c mInvSigUT d contains inverse of mSigUT
c mInvSigW  d contains inverse of mSigW
c mInvSigWS d contains inverse of mSigWS
c mInvSigWT d contains inverse of mSigWT
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
c mSigWTf d   covariance matrix for future differenced seasonally 
c             adjusted (WTf)
c mSigWTfWT d cross covariance matrix for (WTf,WT)
c nInvSigUS i size (rows,columns) of mInvSigUS matrix
c nInvSigUT i size (rows,columns) of mInvSigUT matrix
c nInvSigW  i size (rows,columns) of mInvSigW matrix
c nInvSigWS i size (rows,columns) of mInvSigWS matrix
c nInvSigWT i size (rows,columns) of mInvSigWT matrix
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
c Name   Type Description (compMSE Input/Output Variables)
c-----------------------------------------------------------------------
c lSeaPre l   logical indicating presence of seasonal component
c mIrrVar d   variance matrix of estimated irregular
c mSeaVar d   variance matrix of estimated seasonal
c mTreVar d   variance matrix of estimated trend
c nIrrVar i   size (rows,columns) of mIrrVar matrix
c nSeaVar i   size (rows,columns) of mSeaVar matrix
c nTreVar i   size (rows,columns) of mTreVar matrix
c sdSigAlt d  alternate data innovation stdev when parameters are fixed
c-----------------------------------------------------------------------
c Name   Type Description (Local Variables)
c-----------------------------------------------------------------------
c iLead   i   identifies Lead year 1 through 5
c lInvSig l   logical indicates whether all inverses are available 
c             for compMSE()
c lInvSigUS l logical to enable calculation of mInvSigUS in bldCov()
c lInvSigUT l logical to enable calculation of mInvSigUT in bldCov()
c lInvSigW  l logical to enable calculation of mInvSigW  in bldCov()
c lInvSigWS l logical to enable calculation of mInvSigWS in bldCov()
c lInvSigWT l logical to enable calculation of mInvSigWT in bldCov()
c lSigUf  l   logical to enable calculation of covariance matrices
c             for future elements in bldCov()
c nTLead  i   adds iLead years to nT
c ZERO    d   parameter constant
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables
c-----------------------------------------------------------------------
      INTEGER dS, dT, nSize, nSize2, nSize3, nT
      DOUBLE PRECISION curMSEs(3), finMSEs(3,5), finRevs(3,5)

c-----------------------------------------------------------------------
c     Declare additional Input/Output variables for bldDif()
c-----------------------------------------------------------------------
      LOGICAL lSeaPre
      INTEGER oSeaD, oTreD
      INTEGER nDelS(2), nDelT(2), nDel(2), nDiff(2), nPer,
     &        nRedDelS(2), nRedDelT(2)
      DOUBLE PRECISION vSeaD(0:oSeaD), vTreD(0:oTreD)
      DOUBLE PRECISION mDelS(nSize2), mDelT(nSize2), mDel(nSize2),
     &                 mRedDelS(nSize2), mRedDelT(nSize2)
      DOUBLE PRECISION dDel(dS+dT+1), dDelS(dS+1), dDelT(dT+1),
     &                 dRedDelS(dS+1), dRedDelT(dT+1)

c-----------------------------------------------------------------------
c     Declare additional Input/Output variables for bldCov()
c-----------------------------------------------------------------------
      INTEGER oSeaAR, oSeaMA, oTreAR, oTreMA, oCycAR, oCycMA
      INTEGER nSigUI(2), nSigUS(2), nSigUT(2)
      INTEGER nSigWS(2), nSigWT(2), nSigW(2)
      INTEGER nSigUTf(2), nSigUTfUT(2), nSigWTf(2), nSigWTfWT(2),
     &        nSigWf(2), nSigWfW(2)
      INTEGER nInvSigUS(2), nInvSigUT(2)
      INTEGER nInvSigW(2), nInvSigWS(2), nInvSigWT(2)
      DOUBLE PRECISION sSeaVar, sTreVar, sCycVar, sIrrVar
      DOUBLE PRECISION vSeaAR(0:oSeaAR), vSeaMA(0:oSeaMA)
      DOUBLE PRECISION vTreAR(0:oTreAR), vTreMA(0:oTreMA)
      DOUBLE PRECISION vCycAR(0:oCycAR), vCycMA(0:oCycMA)
      DOUBLE PRECISION mSigUI(nSize2), mSigUS(nSize2), mSigUT(nSize2), 
     &                 mSigWS(nSize2), mSigWT(nSize2), mSigW(nSize2)
      DOUBLE PRECISION mSigUTf(12*12), mSigUTfUT(nSize3),
     &                 mSigWTf(12*12), mSigWTfWT(nSize3),
     &                 mSigWf(12*12),  mSigWfW(nSize3)
      DOUBLE PRECISION mInvSigUS(nSize2), mInvSigUT(nSize2), 
     &                 mInvSigWS(nSize2), mInvSigWT(nSize2),
     &                 mInvSigW(nSize2)

c-----------------------------------------------------------------------
c     Declare additional Input/Output variables for compMSE()
c-----------------------------------------------------------------------
      INTEGER nIrrVar(2), nSeaVar(2), nTreVar(2)
      DOUBLE PRECISION sdSigAlt
      DOUBLE PRECISION mIrrVar(nSize2), mSeaVar(nSize2), mTreVar(nSize2)

c-----------------------------------------------------------------------
c     Declare local variables
c-----------------------------------------------------------------------
      LOGICAL lInvSigUS, lInvSigUT, lInvSigW, lInvSigWS, lInvSigWT,
     &        lInvSig, lSigUf
      LOGICAL dpeq
      INTEGER iLead, nTLead
      DOUBLE PRECISION ZERO
      PARAMETER (lInvSigW=.false., lSigUf=.false., ZERO=0.0D0)

c-----------------------------------------------------------------------
c     Decide on which inverses to perform.
c-----------------------------------------------------------------------
      IF (( dpeq(sSeaVar,ZERO) ) .or. ( dpeq(sTreVar,ZERO) )) THEN
       lInvSigUS = .true.
       lInvSigUT = .true.
       lInvSigWS = .true.
       lInvSigWT = .true.
       lInvSig = .true.
      ELSE
       lInvSigUS = .true.
       lInvSigUT = .false.
       lInvSigWS = .false.
       lInvSigWT = .true.
       lInvSig = .false.
      END IF

c-----------------------------------------------------------------------
c     Process each of the lead times
c-----------------------------------------------------------------------
      DO iLead = 1, 5
c-----------------------------------------------------------------------
c     Calculate adjusted nT for desired lead time
c-----------------------------------------------------------------------
        nTLead = nT + iLead*nPer

c-----------------------------------------------------------------------
c     bldDif processing
c-----------------------------------------------------------------------
        CALL bldDif( dS, dT, nTLead, nPer, nDiff, vSeaD, oSeaD,
     &               vTreD, oTreD, mDelS, dDelS, nDelS,
     &               mDelT, dDelT, nDelT,
     &               mRedDelS, dRedDelS, nRedDelS,
     &               mRedDelT, dRedDelT, nRedDelT,
     &               mDel, dDel, nDel )

c-----------------------------------------------------------------------
c     bldCov processing
c-----------------------------------------------------------------------
        CALL bldCov( nTLead, dS, dT, nPer, lSeaPre,
     &               lSigUf, lInvSigUS, lInvSigUT,
     &               lInvSigW, lInvSigWS, lInvSigWT,
     &               vSeaAR, oSeaAR, vSeaMA, oSeaMA,
     &               vTreAR, oTreAR, vTreMA, oTreMA,
     &               vCycAR, oCycAR, vCycMA, oCycMA,
     &               dDel, nDel, dDelS, nDelS, dDelT, nDelT,
     &               dRedDelS, nRedDelS, dRedDelT, nRedDelT,
     &               sSeaVar, sTreVar, sCycVar, sIrrVar,
     &               mSigUS, nSigUS, mSigUT, nSigUT, mSigUI, nSigUI,
     &               mSigWS, nSigWS, mSigWT, nSigWT, mSigW, nSigW,
     &               mSigUTf, nSigUTf, mSigUTfUT, nSigUTfUT,
     &               mSigWTf, nSigWTf, mSigWTfWT, nSigWTfWT,
     &               mSigWf, nSigWf, mSigWfW, nSigWfW,
     &               mInvSigUS, nInvSigUS, mInvSigUT, nInvSigUT,
     &               mInvSigWS, nInvSigWS, mInvSigWT, nInvSigWT,
     &               mInvSigW, nInvSigW )

c-----------------------------------------------------------------------
c     compMSE processing
c-----------------------------------------------------------------------
        CALL compMSE( nTLead, dS, dT, lSeaPre, dDel, nDel, dDelS, nDelS,
     &                dDelT, nDelT, sdSigAlt, mSigUI, nSigUI,
     &                mInvSigUS, nInvSigUS, mInvSigUT, nInvSigUT,
     &                mInvSigWS, nInvSigWS, mInvSigWT, nInvSigWT,
     &                mInvSigW, nInvSigW, lInvSig,
     &                mIrrVar, nIrrVar, mSeaVar, nSeaVar,
     &                mTreVar, nTreVar )

c-----------------------------------------------------------------------
c     Calculate the finite revisions
c-----------------------------------------------------------------------
c       finMSEs(1,iLead) = mIrrVar( (nT-1)*nTLead+nT )
        finMSEs(1,iLead) = ZERO
        finMSEs(2,iLead) = mSeaVar( (nT-1)*nTLead+nT )
        finMSEs(3,iLead) = mTreVar( (nT-1)*nTLead+nT )
c       finRevs(1,iLead) = curMSEs(1) - finMSEs(1,iLead)
        finRevs(1,iLead) = ZERO
        finRevs(2,iLead) = curMSEs(2) - finMSEs(2,iLead)
        finRevs(3,iLead) = curMSEs(3) - finMSEs(3,iLead)
        
c-----------------------------------------------------------------------
c     Some debug output
c-----------------------------------------------------------------------
c       WRITE(6,1000)nTLead
c1000   FORMAT(' nTLead = ', i4)
        
      END DO

      RETURN
      END