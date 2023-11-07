      SUBROUTINE getDiag( dS, dT, nT, vY, out, init,
     &                    vSeaAR, oSeaAR, vSeaD, oSeaD, vSeaMA, oSeaMA,
     &                    vTreAR, oTreAR, vTreD, oTreD, vTreMA, oTreMA,
     &                    vCycAR, oCycAR, vCycMA, oCycMA, vMA, oMA,
     &                    vSAAR, oSAAR, vSAMA, oSAMA,
     &                    vTAAR, oTAAR, vTAMA, oTAMA,
     &                    sSeaVar, sTreVar, sCycVar, sSAVar, sTAVar,
     &                    sIrrVar, sdSig, nPer, nParam, nFixed, nDiff )
c-----------------------------------------------------------------------
c    getDiag.f, Release 1, Subroutine Version 1.9, Modified 30 May 2006.
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 03 Apr 2005.
c     Modified by REG, on 19 Sep 2005, to add SA Filter from extSgnl(),
c       to add filter gain\phase-delay calculation, 
c       and to correct tab stops.
c     Modified by REG, on 20 Oct 2005, to move sdSigAlt processing 
c       from bldCov() to extSgnl().
c     Modified by REG, on 07 Nov 2005, to generalize irregular component
c       to include mSigUI, created by bldCov(), and used by other
c       routines.
c     Modified by REG, on 17 Nov 2005, to add revision processing.
c     Modified by REG, on 13 Mar 2006, to add growth rate processing.
c     Modified by REG, on 04 Apr 2006, to add weighted version
c       of over-under lag diagnostics.
c     Modified by REG, on 27 Apr 2008, to restrict finite revisions
c       calculations unless out=0, and to adjust sdSigAlt for
c       a finite sample factor previously performed in 
c       compLagDiag() and compCrosDiag() processing.
c     Modified by REG, on 30 May 2006, to add check for no seasonal
c       component which affects bldCov(), compMSE(), and
c       compRevs() processing.
c-----------------------------------------------------------------------
c     This subroutine provides a fortran implementation of Ox code 
c     called SigDiag originally developed by Tucker McElroy that 
c     calculates some diagnostics. The following code implements 
c     four Ox procedures: buildDiffMatrices(), buildCovMatrices(),
c     ExtractSignals(), ComputeDiagnostics().
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c dS      i   size of Seasonal Differencing
c dT      i   size of Trend Differencing
c init    i   SEATS ARIMA model usage:
c               0=initialize and re-estimate,
c               1=re-estimate
c               2=use X-13ARIMA-SEATS model
c nDiff   i   vector of (d,D) differencing orders
c nFixed  i   number of fixed parameters
c nPer    i   size of seasonal period
c nParam  i   number of parameters in model ARIMA:
c               (1) = p, (2) = q, (3) = bp, (4) = bq .
c nT      i   size of data available
c oCycAR  i   max order of vCycAR polynomial
c oCycAR  i   max order of vCycMA polynomial
c oMA     i   max order of vMA polynomial
c oSAMA   i   max order of vSAMA polynomial
c oSeaAR  i   max order of vSeaAR polynomial
c oSeaD   i   max order of vSeaD  polynomial
c oSeaMA  i   max order of vSeaMA polynomial
c oTAMA   i   max order of vTAMA polynomial
c oTreAR  i   max order of vTreAR polynomial
c oTreD   i   max order of vTreD  polynomial
c oTreMA  i   max order of vTreMA polynomial
c out     i   SEATS output parameter
c sdSig   d   data innovation stdev, note that Var provides another 
c               estimate of the innovation variance
c sCycVar d   cycle innovation variance
c sIrrVar d   irregular innovation variance
c sSAVar  d   seasonal adjusted innovation variance
c sSeaVar d   seasonal innovation variance
c sTAVar  d   trend adjusted innovation variance
c sTreVar d   trend innovation variance
c vCycAR  d   AR polynomial vector for cycle component
c vCycMA  d   MA polynomial vector for cycle component
c vMA     d   MA polynomial vector for original model
c vSAMA   d   MA polynomial vector for seasonal adjusted component
c vSeaAR  d   AR polynomial vector for seasonal component
c vSeaD   d   D  polynomial vector for seasonal component
c vSeaMA  d   MA polynomial vector for seasonal component
c vTAMA   d   MA polynomial vector for trend adjusted component
c vTreAR  d   AR polynomial vector for Trend component
c vTreD   d   D  polynomial vector for Trend component
c vTreMA  d   MA polynomial vector for Trend component
c vY      d   data vector
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
      LOGICAL T
      PARAMETER(T=.true.)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'stdio.i'
      INCLUDE 'units.cmn'
      INCLUDE 'acfast.i'
      INCLUDE 'across.i'
      INCLUDE 'model.prm'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'revs.i'
      INCLUDE 'tbl5x.i'
c-----------------------------------------------------------------------
c     Input variables
c-----------------------------------------------------------------------
      INTEGER  dS, dT, init, nT, out
      DOUBLE PRECISION vY(nT)
      INTEGER oSeaAR, oSeaD, oSeaMA, oTreAR, oTreD, oTreMA,
     &        oCycAR, oCycMA, oSAAR, oSAMA, oTAAR, oTAMA, oMA, fh0
      DOUBLE PRECISION vSeaAR(0:oSeaAR), vSeaD(0:oSeaD),
     &                 vSeaMA(0:oSeaMA)
      DOUBLE PRECISION vTreAR(0:oTreAR), vTreD(0:oTred),
     &                 vTreMA(0:oTreMA)
      DOUBLE PRECISION vCycAR(0:oCycAR), vCycMA(0:oCycMA)
      DOUBLE PRECISION vSAAR(0:oSAAR), vSAMA(0:oSAMA)
      DOUBLE PRECISION vTAAR(0:oTAAR), vTAMA(0:oTAMA)
      DOUBLE PRECISION vMA(0:oMA)
      DOUBLE PRECISION sSeaVar, sTreVar, sCycVar, sMAVar, sIrrVar,
     &                 sSAVar,  sTAVar
      DOUBLE PRECISION sdSig
      INTEGER nPer, nParam(4), nFixed, nDiff(2)
c-----------------------------------------------------------------------
c   note - pLagSmT added by BCM April 24, 2006 to allow dimensioning of
c   dLagSmT argument in compCroDiag routine
c   also added 7 integer scalars below to allow getRevDec to dimension
c   variables
c-----------------------------------------------------------------------
      INTEGER pd1, pd2, pd3, pd4, pd5, pd6, pd7, pLagSmT
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c fulPva  d   vector of p-values associated with fulDia
c i,j     i   indexes
c gauss   d   external function
c getAma  c   external function declaration
c getTmcs c   external function declaration
c lag     i   identifies lag to processed by compLagDiag()
c noePva  d   vector of p-values associated with noeDia
c nSave   i   identifies default size of large matrices
c             that are saved (not dynamic)
c ONE,TWO d   constant parameters
c sInnovSd d  innovation variance, one of SEATS (sdSig) or alternative
c             sdSigAlt. Note that a third version (SQRT(Var)) exists.
c vSAAR   d   AR polynomial for seasonal adjusted
c-----------------------------------------------------------------------
      INTEGER i, j, lag, nSave, nSave2, nSave3
      CHARACTER getAna, getTmcs
      DOUBLE PRECISION gauss, fulPva(4), noePva(4),
     &                 ONE, ONEP, sInnovSd, TWO, ZERO
      PARAMETER (nSave=POBS*POBS, nSave2=(POBS+60)*(POBS+60),
     &           nSave3=12*POBS, ONE=1.0D0, ONEP=100.0D0, TWO=2.0D0,
     &           ZERO=0.0D0)
      LOGICAL dpeq

c-----------------------------------------------------------------------
c Name   Type Description (bldDif local Variables)
c-----------------------------------------------------------------------
c dDel    d   diagonal form of overall differencing matrix: mDel
c dDelS   d   diagonal form of seasonal differencing matrix: mDelS
c dDelT   d   diagonal form of trend differencing matrix: mDelT
c dRedDelS d  diagonal form of reduced mDelS: mRedDelS
c dRedDelT d  diagonal form of reduced mDelT: mRedDelT
c mDel    d   overall differencing matrix
c mDelS   d   seasonal differencing matrix
c mDelT   d   trend differencing matrix
c mRedDelS d  smaller version of mDelS
c mRedDelT d  smaller version of mDelT
c nDel    i   size (rows,columns) of mDel
c nDelS   i   size (rows,columns) of mDelS
c nDelT   i   size (rows,columns) of mDelT
c nRedDelS i  size (rows,columns) of mRedDelS
c nRedDelT i  size (rows,columns) of mRedDelT
c-----------------------------------------------------------------------
      INTEGER nDelS(2), nDelT(2), nDel(2), nRedDelS(2), nRedDelT(2)
      DOUBLE PRECISION dDel(dS+dT+1), dDelS(dS+1), dDelT(dT+1),
     &                 dRedDelS(dS+1), dRedDelT(dT+1)
c     ------------------------------------------------------------------
c     Dynamic (commented) versus static (uncommented) matrices
c     ------------------------------------------------------------------
c     DOUBLE PRECISION mDel(nT-dS-dT,nT), mDelS(nT-dS,nT),
c    &                 mDelT(nT-dT,nT), mRedDelS(nT-dS-dT,nT-dT),
c    &                 mRedDelT(nT-dS-dT,nT-dS)
c     ------------------------------------------------------------------
      DOUBLE PRECISION mDel(nSave2), mDelS(nSave2), mDelT(nSave2), 
     &                 mRedDelS(nSave2), mRedDelT(nSave2)
      SAVE mDel, mDelS, mDelT, mRedDelS, mRedDelT

c-----------------------------------------------------------------------
c Name   Type Description (bldCov local Variables)
c-----------------------------------------------------------------------
c lInvSigUS l logical to allow inverting of mSigUS
c lInvSigUT l logical to allow inverting of mSigUT
c lInvSigW l  logical to allow inverting of mSigW
c lInvSigWS l logical to allow inverting of mSigWS
c lInvSigWT l logical to allow inverting of mSigWT
c lSeaPre   l logical indicating presence of seasonal component
c lSigUf    l logical to calculate future covariance matrices
c mInvSigUS d inverse of mSigUS: covariance matrix 
c             for seaonal differenced seasonal cpmponent
c mInvSigUT d inverse of mSigUT: covariance matrix 
c             for trend differenced trend component
c mInvSigW d  inverse of mSigW: covariance matrix for differenced data
c mInvSigWS d inverse of mSigWS: covariance matrix 
c             for seasonal differenced trend adjusted component
c mInvSigWT d inverse of mSigWT: covariance matrix 
c             for trend differenced seasonal adjusted component
c mSigUI  d   covariance matrix for undifferenced irregular
c mSigUS  d   covariance matrix for differenced seasonal
c mSigUT  d   covariance matrix for differenced trend (UT)
c mSigUTf d   covariance matrix for future differenced trend (UTf)
c mSigUTfUT d cross covariance matrix for (UTf,UT)
c mSigW   d   covariance matrix for differenced data (W)
c mSigWf  d   covariance matrix for future differenced data (Wf)
c mSigWfW d   cross covariance matrix for (Wf,W)
c mSigWT  d   covariance matrix for differenced seasonally adjusted (WT)
c mSigWTf d   covariance matrix for differenced future seasonally  
c             adjusted (WTf)
c mSigWTfWT d cross covariance matrix for (WTf,WT)
c nInvSigUS i size (rows,columns) of mInvSigUS matrix
c nInvSigUT i size (rows,columns) of mInvSigUT matrix
c nInvSigW i  size (rows,columns) of mInvSigW matrix
c nInvSigWS i size (rows,columns) of mInvSigWS matrix
c nInvSigWT i size (rows,columns) of mInvSigWT matrix
c mSigWT  d   covariance matrix for differenced seasonally adjusted
c nInvSigW i  size (rows,columns) of mInvSigW matrix
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
c-----------------------------------------------------------------------
      LOGICAL lInvSigUS, lInvSigUT, lInvSigW, lInvSigWS, lInvSigWT,
     &        lSeaPre, lSigUf
      INTEGER nSigUI(2), nSigUS(2), nSigUT(2)
      INTEGER nSigWS(2), nSigWT(2), nSigW(2)
      INTEGER nInvSigUS(2), nInvSigUT(2)
      INTEGER nInvSigW(2), nInvSigWS(2), nInvSigWT(2)
      INTEGER nSigUTf(2), nSigUTfUT(2), nSigWTf(2), nSigWTfWT(2),
     &        nSigWf(2), nSigWfW(2)
      PARAMETER (lInvSigUS=.true., lInvSigUT=.true., lInvSigW=.true.,
     &           lInvSigWS=.true., lInvSigWT=.true., lSigUf=.true.)
c     ------------------------------------------------------------------
c     Dynamic (commented) versus static (uncommented) matrices
c     ------------------------------------------------------------------
c     DOUBLE PRECISION mSigUI(nT,nT), mSigUS(nT-dS,nT-dS),
c    &                 mSigUT(nT-dT,nT-dT)
c     DOUBLE PRECISION mSigWS(nT-dS,nT-dS), mSigWT(nT-dT,nT-dT),
c    &                 mSigW(nT-dS-dT,nT-dS-dT)
c     DOUBLE PRECISION mInvSigUS(nT-dS,nT-dS), mInvSigUT(nT-dT,nT-dT)
c     DOUBLE PRECISION mInvSigWS(nT-dS,nT-dS), mInvSigWT(nT-dT,nT-dT),
c                      mInvSigW(nT-dS-dT,nT-dS-dT)
c     ------------------------------------------------------------------
      DOUBLE PRECISION mSigUI(nSave2), mSigUS(nSave2), mSigUT(nSave2)
      DOUBLE PRECISION mSigWS(nSave2), mSigWT(nSave2), mSigW(nSave2)
      DOUBLE PRECISION mInvSigUS(nSave2), mInvSigUT(nSave2)
      DOUBLE PRECISION mInvSigWS(nSave2), mInvSigWT(nSave2),
     &                 mInvSigW(nSave2)
      DOUBLE PRECISION mSigUTf(12*12), mSigUTfUT(nSave3),
     &                 mSigWTf(12*12), mSigWTfWT(nSave3),
     &                 mSigWf(12*12), mSigWfW(nSave3)
      SAVE mSigUI, mSigUS, mSigUT, mSigW, mSigWS, mSigWT,
     &     mInvSigUS, mInvSigUT, mInvSigW, mInvSigWS, mInvSigWT,
     &     mSigUTf, mSigUTfUT, mSigWTf, mSigWTfWT, mSigWf, mSigWfW

c-----------------------------------------------------------------------
c Name   Type Description (extSgnl local Variables)
c-----------------------------------------------------------------------
c finfact d   finite sample correction factor
c mCovIrr d   covariance of estimated irregular
c mCovSA  d   covariance of estimated seasonal adjusted
c mCovSea d   covariance of estimated seasonal
c mCovTre d   covariance of estimated trend
c mIrrPFlt d  irregular partial filters: 
c               column 1 = symmetric, column 2 = concurrent
c mSAPFlt d   seasonal adjustment partial filters: 
c               column 1 = symmetric, column 2 = concurrent
c mSeaPFlt d  seasonal partial filters: 
c               column 1 = symmetric, column 2 = concurrent
c mTrePFlt d  trend partial filters: 
c               column 1 = symmetric, column 2 = concurrent
c nW      i   size of the differenced data
c nCovIrr i   size (rows,columns) of mCovIrr matrix
c nCovSA  i   size (rows,columns) of mCovSA  matrix
c nCovSea i   size (rows,columns) of mCovSea matrix
c nCovTre i   size (rows,columns) of mCovTre matrix
c nIrrEst i   size (rows,columns) of vIrrEst vector
c nIrrPFlt i  size (rows,columns) of mIrrPFlt matrix
c nParams i   total number of parameters in nParam
c nPStar  i   total number of AR parameters
c nSAPFlt i   size (rows,columns) of mSAPFlt matrix
c nSeaEst i   size (rows,columns) of vSeaEst vector
c nSeaPFlt i  size (rows,columns) of mSeaPFlt matrix
c nTreEst i   size (rows,columns) of vTreEst vector
c nTrePFlt i  size (rows,columns) of mTrePFlt matrix
c sdSigAlt d  alternate estimated data innovation stdev adjusted for
c             number of estimated model parameters
c vIrrEst d   estimated irregular
c vSeaEst d   estimated seasonal
c vTreEst d   estimated trend
c-----------------------------------------------------------------------
      INTEGER nIrrEst(2), nSeaEst(2), nTreEst(2)
      INTEGER nCovIrr(2), nCovSea(2), nCovTre(2), nCovSA(2)
      INTEGER nIrrPFlt(2), nSAPFlt(2),  nSeaPFlt(2), nTrePFlt(2)
      INTEGER nParams, nPStar, nW
      DOUBLE PRECISION vIrrEst(nT), vSeaEst(nT), vTreEst(nT)
      DOUBLE PRECISION mIrrPFlt(nT-dS-dT,2), mSAPFlt(nT-dS,2),
     &                 mSeaPFlt(nT-dT,2), mTrePFlt(nT-dS,2)
      DOUBLE PRECISION finfact, sdSigAlt
c     ------------------------------------------------------------------
c     Dynamic (commented) versus static (uncommented) matrices
c     ------------------------------------------------------------------
c     DOUBLE PRECISION mCovIrr(nT,nT), mCovSea(nT-dS,nT-dS), 
c    &                 mCovTre(nT-dT,nT-dT), mCovSA(nT-dT,nT-dT)
c     ------------------------------------------------------------------
      DOUBLE PRECISION mCovIrr(nSave), mCovSea(nSave), 
     &                 mCovTre(nSave), mCovSA(nSave)
      SAVE mCovIrr, mCovSea, mCovTre, mCovSA

c-----------------------------------------------------------------------
c Name   Type Description (compMSE local Variables)
c-----------------------------------------------------------------------
c lInvSig l   logical indicating whether all mInvSig matrices are 
c               available
c mIrrVar d   variance matrix of estimated irregular
c mSeaVar d   variance matrix of estimated seasonal
c mTreVar d   variance matrix of estimated trend
c nIrrVar i   size (rows,columns) of mIrrVar matrix
c nSeaVar i   size (rows,columns) of mSeaVar matrix
c nTreVar i   size (rows,columns) of mTreVar matrix
c-----------------------------------------------------------------------
      INTEGER nIrrVar(2), nSeaVar(2), nTreVar(2)
      LOGICAL lInvSig
      PARAMETER (lInvSig=.true.)
c     ------------------------------------------------------------------
c     Dynamic (commented) versus static (uncommented) matrices
c     ------------------------------------------------------------------
c     DOUBLE PRECISION mIrrVar(nT,nT), mSeaVar(nT,nT), mTreVar(nT,nT)
c     ------------------------------------------------------------------
      DOUBLE PRECISION mIrrVar(nSave2), mSeaVar(nSave2), mTreVar(nSave2)
      SAVE mIrrVar, mSeaVar, mTreVar

c----------------------------------------------------------------------- 
c Name   Type Description (compDiagand, CompLagDiag, getWghLagDia,
c                          and CompCroDiag local Variables)
c-----------------------------------------------------------------------
c fulDia  d   vector of normalized diagnostics from full signals
c             for irregular, seasonal, trend, and SA
c fulEso  d   vector of null means of estimates from full signals
c             for irregular, seasonal, trend, and SA
c fulEst  d   vector of diagnostic estimates from full signals
c             for irregular, seasonal, trend, and SA
c fulVar  d   vector of variances of diagnostics from full signals
c             for irregular, seasonal, trend, and SA
c noeDia  d   vector of normalized diagnostics from trimmed signals
c             for irregular, seasonal, trend, and SA
c noeEso  d   vector of null means of estimate from trimmed signals
c             for irregular, seasonal, trend, and SA
c noeEst  d   vector of diagnostic estimates from trimmed signals
c             for irregular, seasonal, trend, and SA
c noeVar  d   vector of variances of diagnostics from trimmed signals
c             for irregular, seasonal, trend, and SA
c wghDia  d   vector of normalized diagnostics from weighted signals
c             for irregular, seasonal, trend, and SA
c wghEso  d   vector of null means of estimate from weighted signals
c             for irregular, seasonal, trend, and SA
c wghEst  d   vector of diagnostic estimates from weighted signals
c             for irregular, seasonal, trend, and SA
c wghVar  d   vector of variances of diagnostics from weighted signals
c             for irregular, seasonal, trend, and SA
c-----------------------------------------------------------------------
      DOUBLE PRECISION fulEst(4), noeEst(4), fulEso(4), noeEso(4),
     &                 fulVar(4), noeVar(4), fulDia(4), noeDia(4),
     &                 wghEst(4), wghEso(4), wghVar(4), wghDia(4)

c-----------------------------------------------------------------------
c Name   Type Description (Revision local Variables)
c-----------------------------------------------------------------------
c finMSEs d   MSEs for each component given nT observations in the pass
c             and up to 5 years of future observations
c             (i,j), i=1,3 represents irregular, seasonal, trend comps
c                    j=1,5 represents # years of future observations
c finRevs d   revisions for each component using finMSEs
c infMSEs d   MSEs for each component given nT observations in the pass
c             and infinite observations in the future
c             (i), i=1,3 represents irregular, seasonal, trend comps
c infRevs d   revisions for each component using infMSEs
c lCurMSEs d  MSEs for components estimates for the last 5 years: 
c             first column for seasonal, second column for trend
c lInfMSEs d  lag 0 MSEs for component estimates for the last five years 
c             and the next year: first column for seasonal,
c             second column for trend
c lInfMSE1s d lag 1 MSEs for component estimates for the last five years 
c             and the next year: first column for seasonal,
c             second column for trend
c lInfMSE2s d lag nPer MSEs for component estimates 
c             for the last five years and the next year: 
c             first column for seasonal, second column for trend
c lInfMSE3s d lag iTbl53Lag MSEs for component estimates 
c             for the last five years and the next year: 
c             first column for seasonal, second column for trend
c nRevs   i   row index in lCurMSEs and lInfMSExs associated 
c             with last observation
c oIrrAR  i   order of AR polynomial for irregular component
c oIrrMA  i   order of MA polynomial for irregular component
c relRevs d   finRevs relative to infRevs
c sSeaARD i   size of vSeaARD vector
c sTreARD i   size of vTreARD vector
c vIrrAR  i   AR polynomial for the irregular component
c vIrrMA  i   MA polynomial for the irregular component
c vSeaARD d   combined AR x D polynomial for the seasonal component
c vTreARD d   combined AR x D polynomial for the trend component
c-----------------------------------------------------------------------
      INTEGER sSeaARD, sTreARD, oIrrAR, oIrrMA, nRevs
c     DOUBLE PRECISION finMSEs(3,5), finRevs(3,5), relRevs(3,5)
      DOUBLE PRECISION vSeaARD(0:(oSeaAR+oSeaD))
      DOUBLE PRECISION vTreARD(0:(oTreAR+oTreD))
      DOUBLE PRECISION vIrrAR(0:0), vIrrMA(0:0)
      DOUBLE PRECISION lCurMSEs(60,2), lInfMSEs(72,2), lInfMSE1s(72,2)
      DOUBLE PRECISION lInfMSE2s(72,2), lInfMSE3s(72,2)
*      PARAMETER (oIrrAR=0, oIrrMA=0, vIrrAR(0)=ONE, vIrrMA(0)=ONE)
      PARAMETER (oIrrAR=0, oIrrMA=0)

c-----------------------------------------------------------------------
c Name   Type Description (Growth Rate local Variables)
c-----------------------------------------------------------------------
c iTbl53Lag i lag between last observation of data and last observation
c             of previous year
c nSeaGRSE1 i size (rows,columns) of vSeaGRSE1 vector
c nSeaGRSE2 i size (rows,columns) of vSeaGRSE2 vector
c nTreGRSE1 i size (rows,columns) of vTreGRSE1 vector
c nTreGRSE2 i size (rows,columns) of vTreGRSE2 vector
c vSeaGRSE1 d vector of seasonal component growth rate SEs for table 5.2
c vSeaGRSE2 d vector of seasonal component growth rate SEs for table 5.5
c vTbl51    d vector of table 5.1 MSEs
c vTbl53    d vector of table 5.3 SEs
c vTbl54    d vector of table 5.4 MSEs
c vTbl56    d vector of table 5.6 SEs
c vTbl57    d vector of table 5.7 SEs
c vTreGRSE1 d vector of trend component growth rate SEs for table 5.2
c vTreGRSE2 d vector of trend component growth rate SEs for table 5.5
c-----------------------------------------------------------------------
      INTEGER iTbl53Lag
c     INTEGER nSeaGRSE1(2), nSeaGRSE2(2), nTreGRSE1(2), nTreGRSE2(2)
c     DOUBLE PRECISION vSeaGRSE1(nT-1), vTreGRSE1(nT-1)
c     DOUBLE PRECISION vSeaGRSE2(nT-1), vTreGRSE2(nT-1)
c     DOUBLE PRECISION vTbl51(6), vTbl53(2), vTbl54(6), vTbl56(6,2),
c    &                 vTbl57(3,3)

c-----------------------------------------------------------------------
c     Some debug output
c-----------------------------------------------------------------------
*      WRITE (6, 9990) dS, dT, nT, oSeaAR, oSeaD, oSeaMA,
*     &                oTreAR, oTreD, oTreMA, oCycAR, oCycMA, nPer,
*     &                nParam(1), nParam(2), nParam(3), nParam(4),
*     &                nFixed, nDiff(1), nDiff(2)
*      WRITE (6, 9991) sSeaVar, sTreVar, sCycVar, sSAVar, sTAVar,
*     &                sIrrVar
c     WRITE (6, 9993) sdSig**2, Var
*      WRITE (6, 9992)(vSeaAR(i),i=0,oSeaAR)
*      WRITE (6, 9992)(vSeaD(i), i=0,oSeaD)
*      WRITE (6, 9992)(vSeaMA(i),i=0,oSeaMA)
*      WRITE (6, 9992)(vTreAR(i),i=0,oTreAR)
*      WRITE (6, 9992)(vTreD(i), i=0,oTreD)
*      WRITE (6, 9992)(vTreMA(i),i=0,oTreMA)
*      WRITE (6, 9992)(vCycAR(i),i=0,oCycAR)
*      WRITE (6, 9992)(vCycMA(i),i=0,oCycMA)
*      WRITE (6, 9992)(vSAAR(i),i=0,oSAAR)
*      WRITE (6, 9992)(vSAMA(i),i=0,oSAMA)
*      WRITE (6, 9992)(vTAAR(i),i=0,oTAAR)
*      WRITE (6, 9992)(vTAMA(i),i=0,oTAMA)
*      WRITE (6, 9992)(vMA(i),i=0,oMA)
* 9990 FORMAT( 3( 8(1x, i4), / ) )
* 9991 FORMAT( 2( 5(1x, G12.5), / ) )
* 9992 FORMAT( 100(1x, G12.5) )
* 9993 FORMAT( 2(1x, G20.13) )
      vIrrAR(0)=ONE
      vIrrMA(0)=ONE
c-----------------------------------------------------------------------
c     Check for some differencing else exit with warning
c-----------------------------------------------------------------------
      IF (( nDiff(1) .eq. 0 ).and.( nDiff(2) .eq. 0)) THEN
       fh0=0
       IF(Issap.lt.2.and.Irev.lt.4)fh0=STDERR
       CALL writln(' *** No differencing provided, finite sample'//
     &             ' diagnostic processing aborted. ***',fh0,Mt2,T,T)
       RETURN
      END IF
c-----------------------------------------------------------------------
c     bldDif processing
c-----------------------------------------------------------------------
      CALL bldDif( dS, dT, nT, nPer, nDiff, vSeaD, oSeaD,
     &             vTreD, oTreD, mDelS, dDelS, nDelS,
     &             mDelT, dDelT, nDelT,
     &             mRedDelS, dRedDelS, nRedDelS,
     &             mRedDelT, dRedDelT, nRedDelT,
     &             mDel, dDel, nDel )

c     ------------------------------------------------------------------
c     Debug code.
c     ------------------------------------------------------------------
c     WRITE(6,2000)
c     ------------------------------------------------------------------
c     DO i= 1, nDelS(1), nDelS(1)-1
c      WRITE(6,2001)(mDelS(i+(j-1)*nDelS(1)),j=1,nDelS(2))
c     END DO
c     WRITE(6,2006)nDelS
c     ------------------------------------------------------------------
c     DO i = 1, nDelT(1), nDelT(1)-1
c      WRITE(6,2001)(mDelT(i+(j-1)*nDelT(1)),j=1,nDelT(2))
c     END DO
c     WRITE(6,2006)nDelT
c     ------------------------------------------------------------------
c     DO i = 1, nRedDelS(1), nRedDelS(1)-1
c      WRITE(6,2001)(mRedDelS(i+(j-1)*nRedDelS(1)),j=1,nRedDelS(2))
c     END DO
c     WRITE(6,2006)nRedDelS
c     ------------------------------------------------------------------
c     DO i = 1, nRedDelT(1), nRedDelT(1)-1
c      WRITE(6,2001)(mRedDelT(i+(j-1)*nRedDelT(1)),j=1,nRedDelT(2))
c     END DO
c     WRITE(6,2006)nRedDelT
c     ------------------------------------------------------------------
c     DO i= 1, nDel(1), nDel(1)-1
c      WRITE(6,2001)(mDel(i+(j-1)*nDel(1)),j=1,nDel(2))
c     END DO
c     WRITE(6,2006)nDel
c     ------------------------------------------------------------------
* 2000 FORMAT( ' Output from bldDif. ', /)
c2001 FORMAT( 300(1x,F6.2) )
c2006 FORMAT( 2(1x,I3), / )

c-----------------------------------------------------------------------
c     bldCov processing
c-----------------------------------------------------------------------
      lSeaPre = ( (nParam(3)+nParam(4)+nDiff(2)) .gt. 0 )
      CALL bldCov( nT, dS, dT, nPer, lSeaPre,
     &             lSigUf, lInvSigUS, lInvSigUT,
     &             lInvSigW, lInvSigWS, lInvSigWT,
     &             vSeaAR, oSeaAR, vSeaMA, oSeaMA,
     &             vTreAR, oTreAR, vTreMA, oTreMA,
     &             vCycAR, oCycAR, vCycMA, oCycMA,
     &             dDel, nDel, dDelS, nDelS, dDelT, nDelT,
     &             dRedDelS, nRedDelS, dRedDelT, nRedDelT,
     &             sSeaVar, sTreVar, sCycVar, sIrrVar,
     &             mSigUS, nSigUS, mSigUT, nSigUT, mSigUI, nSigUI,
     &             mSigWS, nSigWS, mSigWT, nSigWT, mSigW, nSigW,
     &             mSigUTf, nSigUTf, mSigUTfUT, nSigUTfUT,
     &             mSigWTf, nSigWTf, mSigWTfWT, nSigWTfWT,
     &             mSigWf, nSigWf, mSigWfW, nSigWfW,
     &             mInvSigUS, nInvSigUS, mInvSigUT, nInvSigUT,
     &             mInvSigWS, nInvSigWS, mInvSigWT, nInvSigWT,
     &             mInvSigW, nInvSigW )

c     ------------------------------------------------------------------
c     Debug code.
c     ------------------------------------------------------------------
c     WRITE(6,3000)
c     ------------------------------------------------------------------
c     DO i = 1, nSigUS(1), nSigUS(1)-1
c      WRITE(6,3001)(mSigUS(i+(j-1)*nSigUS(1)),j=1,nSigUS(2))
c     END DO
c     WRITE(6,2006)nSigUS
c     ------------------------------------------------------------------
c     DO i = 1, nSigUT(1), nSigUT(1)-1
c      WRITE(6,3001)(mSigUT(i+(j-1)*nSigUT(1)),j=1,nSigUT(2))
c     END DO
c     WRITE(6,2006)nSigUT
c     ------------------------------------------------------------------
c     DO i = 1, nSigWS(1), nSigWS(1)-1
c      WRITE(6,3001)(mSigWS(i+(j-1)*nSigWS(1)),j=1,nSigWS(2))
c     END DO
c     WRITE(6,2006)nSigWS
c     ------------------------------------------------------------------
c     DO i = 1, nSigWT(1), nSigWT(1)-1
c      WRITE(6,3001)(mSigWT(i+(j-1)*nSigWT(1)),j=1,nSigWT(2))
c     END DO
c     WRITE(6,2006)nSigWT
c     ------------------------------------------------------------------
c     DO i = 1, nSigW(1), nSigW(1)-1
c      WRITE(6,3001)(mSigW(i+(j-1)*nSigW(1)),j=1,nSigW(2))
c     END DO
c     WRITE(6,2006)nSigW
c     ------------------------------------------------------------------
* 3000 FORMAT( ' Output from bldCov. ', /)
c3001 FORMAT( 300(1x,F6.2) )
 3004 FORMAT( 1x,G17.10, / )

c-----------------------------------------------------------------------
c     extSgnl processing
c-----------------------------------------------------------------------
      CALL extSgnl( nT, dS, dT, vY, mDel, dDel, nDel,
     &              mDelS, dDelS, nDelS, mDelT, dDelT, nDelT,
     &              sdSigAlt, mRedDelS, dRedDelS, nRedDelS,
     &              mRedDelT, dRedDelT, nRedDelT, mSigUS, nSigUS,
     &              mSigUT, nSigUT, mSigUI, nSigUI, 
     &              mSigWS, nSigWS, mSigWT, nSigWT,
     &              mSigW, nSigW, mInvSigW, nInvSigW,
     &              mInvSigUS, nInvSigUS, mInvSigUT, nInvSigUT,
     &              mInvSigWS, nInvSigWS, mInvSigWT, nInvSigWT,
     &              vIrrEst, nIrrEst, vSeaEst, nSeaEst,
     &              vTreEst, nTreEst, mCovIrr, nCovIrr,
     &              mCovSea, nCovSea, mCovTre, nCovTre,
     &              mCovSA,  nCovSA,
     &              mIrrPFlt, nIrrPFlt, mSeaPFlt, nSeaPFlt,
     &              mTrePFlt, nTrePFlt, mSAPFlt,  nSAPFlt )

c     ------------------------------------------------------------------
c     Calculate finite sample sample correction factor and 
c     model innovation variance adjusted for finite sample.
c     If model estimated by X-13A-S then adjust for fixed parameters,
c     else if model estimated by SEATS then adjust for conditional
c     estimation using initial AR (nParam(1) + nPer*nParam(3)) values.
c     ------------------------------------------------------------------
      nParams = nParam(1) + nParam(2) + nParam(3) + nParam(4)
      nPStar = nParam(1) + nPer*nParam(3)
      nW = nT - (nDiff(1) + nPer*nDiff(2))
      if ((getAna() .ne. 'Y') .and. (getTmcs() .ne. 'Y') .and.
     &    (init .eq. 2)) then
        finfact = DBLE( nW )/ DBLE( nW - (nParams - nFixed) )
      ELSE
        finfact = DBLE( nW )/ DBLE( nW - (nParams + nPStar) )
      END IF
      sdSigAlt = DSQRT( finfact )*sdSigAlt
*      WRITE(6,4010)getAna(), getTmcs(), init
* 4010 FORMAT( 1x, a1, 1x, a1, 1x, i2 )

c     ------------------------------------------------------------------
c     Choose the innovation standard deviation to use: 
c     sdSigAlt over sdSig. Note that there is also SQRT(Var) from SEATS.
c     ------------------------------------------------------------------
      sInnovSd = sdSig

c     ------------------------------------------------------------------
c     Debug code.
c     ------------------------------------------------------------------
*      WRITE(6,4000)
c     ------------------------------------------------------------------
*      WRITE(6,3004)sdSigAlt
c     ------------------------------------------------------------------
c     WRITE(6,4001)(vIrrEst(i),i=1,nIrrEst(1))
c     WRITE(6,2006)nIrrEst
c     WRITE(6,4001)(vSeaEst(i),i=1,nSeaEst(1))
c     WRITE(6,2006)nSeaEst
c     WRITE(6,4001)(vTreEst(i),i=1,nTreEst(1))
c     WRITE(6,2006)nTreEst
c     ------------------------------------------------------------------
c     DO i = 1, nCovIrr(1), nCovIrr(1)-1
c      WRITE(6,4001)(mCovIrr(i+(j-1)*nCovIrr(1)),j=1,nCovIrr(2))
c     END DO
c     WRITE(6,2006)nCovIrr
c     ------------------------------------------------------------------
c     DO i = 1, nCovSea(1), nCovSea(1)-1
c      WRITE(6,4001)(mCovSea(i+(j-1)*nCovSea(1)),j=1,nCovSea(2))
c     END DO
c     WRITE(6,2006)nCovSea
c     ------------------------------------------------------------------
c     DO i = 1, nCovTre(1), nCovTre(1)-1
c      WRITE(6,4001)(mCovTre(i+(j-1)*nCovTre(1)),j=1,nCovTre(2))
c     END DO
c     WRITE(6,2006)nCovTre
c     ------------------------------------------------------------------
c     DO i = 1, nCovSA(1), nCovSA(1)-1
c      WRITE(6,4001)(mCovSA(i+(j-1)*nCovSA(1)),j=1,nCovSA(2))
c     END DO
c     WRITE(6,2006)nCovSA
c     ------------------------------------------------------------------
c     WRITE(6,4002)(mIrrPFlt(j,1),j=1,5)
c     WRITE(6,4002)(mIrrPFlt(nIrrPFlt(2)-5+j,1),j=1,5)
c     WRITE(6,4002)(mIrrPFlt(j,2),j=1,5)
c     WRITE(6,4002)(mIrrPFlt(nIrrPFlt(2)-5+j,2),j=1,5)
c     WRITE(6,4002)(mSeaPFlt(j,1),j=1,5)
c     WRITE(6,4002)(mSeaPFlt(nSeaPFlt(2)-5+j,1),j=1,5)
c     WRITE(6,4002)(mSeaPFlt(j,2),j=1,5)
c     WRITE(6,4002)(mSeaPFlt(nSeaPFlt(2)-5+j,2),j=1,5)
c     WRITE(6,4002)(mTrePFlt(j,1),j=1,5)
c     WRITE(6,4002)(mTrePFlt(nTrePFlt(2)-5+j,1),j=1,5)
c     WRITE(6,4002)(mTrePFlt(j,2),j=1,5)
c     WRITE(6,4002)(mTrePFlt(nTrePFlt(2)-5+j,2),j=1,5)
c     ------------------------------------------------------------------
 4000 FORMAT( ' Output from extSgnl. ', /)
c4001 FORMAT( 300(1x,F11.5), /)
c4002 FORMAT( 100(6(1x,G12.5),/), /)
c4006 FORMAT( )

c-----------------------------------------------------------------------
c     compMSE processing
c-----------------------------------------------------------------------
      CALL compMSE( nT, dS, dT, lSeaPre, dDel, nDel, dDelS, nDelS,
     &              dDelT, nDelT, DSQRT(Var), mSigUI, nSigUI,
     &              mInvSigUS, nInvSigUS, mInvSigUT, nInvSigUT,
     &              mInvSigWS, nInvSigWS, mInvSigWT, nInvSigWT,
     &              mInvSigW, nInvSigW, lInvSig,
     &              mIrrVar, nIrrVar, mSeaVar, nSeaVar,
     &              mTreVar, nTreVar )
      curMSEs(1) = ZERO
      curMSEs(2) = mSeaVar(nT*nT)
      curMSEs(3) = mTreVar(nT*nT)
      nRevs = min( nT,60)
      DO i=0,nRevs-1
       lCurMSEs(nRevs-i,1) = mSeaVar( (nT-i-1)*nT + (nT-i) )
       lCurMSEs(nRevs-i,2) = mTreVar( (nT-i-1)*nT + (nT-i) )
      END DO

c     ------------------------------------------------------------------
c     Debug code.
c     ------------------------------------------------------------------
c     WRITE(6,4100)
c     ------------------------------------------------------------------
c     WRITE(6,4002)(mIrrVar((j-1)*nT+j),j=1,nT)
c     WRITE(6,4006)
c     ------------------------------------------------------------------
c     WRITE(6,4002)(mSeaVar((j-1)*nT+j),j=1,nT)
c     WRITE(6,4006)
c     ------------------------------------------------------------------
c     WRITE(6,4002)(mTreVar((j-1)*nT+j),j=1,nT)
c     WRITE(6,4006)
c     ------------------------------------------------------------------
* 4100 FORMAT( ' Output from compMSE. ', /)

c-----------------------------------------------------------------------
c     compMSEAlt processing
c-----------------------------------------------------------------------
c     call compMSEAlt( nT, dS, dT, mDel, nDel, mDelS, nDelS,
c    &                 mDelT, nDelT, sIrrVar, DSQRT(Var),
c    &                 mSigWS, nSigWS, mSigWT, nSigWT,
c    &                 mInvSigW, nInvSigW, mIrrVar, nIrrVar,
c    &                 mSeaVar, nSeaVar, mTreVar, nTreVar )

c     ------------------------------------------------------------------
c     Debug code.
c     ------------------------------------------------------------------
c     WRITE(6,4200)
c     ------------------------------------------------------------------
c     WRITE(6,4002)(mIrrVar((j-1)*nT+j),j=1,nT)
c     WRITE(6,4006)
c     ------------------------------------------------------------------
c     WRITE(6,4002)(mSeaVar((j-1)*nT+j),j=1,nT)
c     WRITE(6,4006)
c     ------------------------------------------------------------------
c     WRITE(6,4002)(mTreVar((j-1)*nT+j),j=1,nT)
c     WRITE(6,4006)
c     ------------------------------------------------------------------
c4200 FORMAT( ' Output from compMSEAlt. ', /)

c-----------------------------------------------------------------------
c     compDiag processing
c-----------------------------------------------------------------------
c     CALL compDiag( nT, dS, dT, nPer, nParam, nFixed, nDiff,
c    &               DSQRT(Var), sInnovSd, vIrrEst, nIrrEst,
c    &               vSeaEst, nSeaEst, vTreEst, nTreEst,
c    &               dDelS, nDelS, dDelT, nDelT,
c    &               mCovIrr, nCovIrr, mCovSea, nCovSea,
c    &               mCovTre, nCovTre, mCovSA, nCovSA,
c    &               fulEst, noeEst, fulEso, noeEso,
c    &               fulVar, noeVar, fulDia, noeDia )

c     ------------------------------------------------------------------
c     Move the relevant results to common block output variables.
c     ------------------------------------------------------------------
c     lag = 0
c     Facfiem(lag)=fulEst(1)
c     Facfsem(lag)=fulEst(2)
c     Facfpem(lag)=fulEst(3)
c     Facfaem(lag)=fulEst(4)
c     Nacfiem(lag)=noeEst(1)
c     Nacfsem(lag)=noeEst(2)
c     Nacfpem(lag)=noeEst(3)
c     Nacfaem(lag)=noeEst(4)
c     ------------------------------------------------------------------
c     Facfier(lag)=fulEso(1)
c     Facfser(lag)=fulEso(2)
c     Facfper(lag)=fulEso(3)
c     Facfaer(lag)=fulEso(4)
c     Nacfier(lag)=noeEso(1)
c     Nacfser(lag)=noeEso(2)
c     Nacfper(lag)=noeEso(3)
c     Nacfaer(lag)=noeEso(4)
c     ------------------------------------------------------------------
c     Facfidg(lag)=fulDia(1)
c     Facfsdg(lag)=fulDia(2)
c     Facfpdg(lag)=fulDia(3)
c     Facfadg(lag)=fulDia(4)
c     Nacfidg(lag)=noeDia(1)
c     Nacfsdg(lag)=noeDia(2)
c     Nacfpdg(lag)=noeDia(3)
c     Nacfadg(lag)=noeDia(4)

c     ------------------------------------------------------------------
c     Debug code.
c     ------------------------------------------------------------------
c     fulPva(1) = (ONE - gauss( fulDia(1) ))/TWO
c     fulPva(2) = (ONE - gauss( fulDia(2) ))/TWO
c     fulPva(3) = (ONE - gauss( fulDia(3) ))/TWO
c     fulPva(4) = (ONE - gauss( fulDia(4) ))/TWO
c     noePva(1) = (ONE - gauss( noeDia(1) ))/TWO
c     noePva(2) = (ONE - gauss( noeDia(2) ))/TWO
c     noePva(3) = (ONE - gauss( noeDia(3) ))/TWO
c     noePva(4) = (ONE - gauss( noeDia(4) ))/TWO
c     ------------------------------------------------------------------
c     WRITE(6,5000)
c     WRITE(6,5001)fulEst(1), fulEst(2), fulEst(3), fulEst(4)
c     WRITE(6,5001)noeEst(1), noeEst(2), noeEst(3), noeEst(4)
c     WRITE(6,5001)fulEso(1), fulEso(2), fulEso(3), fulEso(4)
c     WRITE(6,5001)noeEso(1), noeEso(2), noeEso(3), noeEso(4)
c     WRITE(6,5001)fulVar(1), fulVar(2), fulVar(3), fulVar(4)
c     WRITE(6,5001)noeVar(1), noeVar(2), noeVar(3), noeVar(4)
c     WRITE(6,5001)fulDia(1), fulDia(2), fulDia(3), fulDia(4)
c     WRITE(6,5001)noeDia(1), noeDia(2), noeDia(3), noeDia(4)
c     WRITE(6,5001)fulPva(1), fulPva(2), fulPva(3), fulPva(4)
c     WRITE(6,5001)noePva(1), noePva(2), noePva(3), noePva(4)
c     ------------------------------------------------------------------
c5000 FORMAT( ' Output from compDiag. ', /)
* 5001 FORMAT( 4(1x,G17.8), /)

c-----------------------------------------------------------------------
c     compLagDiag(0) processing - same as compDiag()
c-----------------------------------------------------------------------
      lag = 0
      CALL compLagDiag( lag, nT, dS, dT, nPer,
     &                  finfact, sInnovSd, vIrrEst, nIrrEst,
     &                  vSeaEst, nSeaEst, vTreEst, nTreEst,
     &                  dDelS, nDelS, dDelT, nDelT,
     &                  mCovIrr, nCovIrr, mCovSea, nCovSea,
     &                  mCovTre, nCovTre, mCovSA, nCovSA,
     &                  fulEst, noeEst, fulEso, noeEso,
     &                  fulVar, noeVar, fulDia, noeDia )

c     ------------------------------------------------------------------
c     Move the relevant results to common block output variables.
c     ------------------------------------------------------------------
      Facfiem(lag)=fulEst(1)
      Facfsem(lag)=fulEst(2)
      Facfpem(lag)=fulEst(3)
      Facfaem(lag)=fulEst(4)
      Nacfiem(lag)=noeEst(1)
      Nacfsem(lag)=noeEst(2)
      Nacfpem(lag)=noeEst(3)
      Nacfaem(lag)=noeEst(4)
c     ------------------------------------------------------------------
      Facfier(lag)=fulEso(1)
      Facfser(lag)=fulEso(2)
      Facfper(lag)=fulEso(3)
      Facfaer(lag)=fulEso(4)
      Nacfier(lag)=noeEso(1)
      Nacfser(lag)=noeEso(2)
      Nacfper(lag)=noeEso(3)
      Nacfaer(lag)=noeEso(4)
c     ------------------------------------------------------------------
      Facfidg(lag)=fulDia(1)
      Facfsdg(lag)=fulDia(2)
      Facfpdg(lag)=fulDia(3)
      Facfadg(lag)=fulDia(4)
      Nacfidg(lag)=noeDia(1)
      Nacfsdg(lag)=noeDia(2)
      Nacfpdg(lag)=noeDia(3)
      Nacfadg(lag)=noeDia(4)

c     ------------------------------------------------------------------
c     Debug code.
c     ------------------------------------------------------------------
c     fulPva(1) = (ONE - gauss( fulDia(1) ))/TWO
c     fulPva(2) = (ONE - gauss( fulDia(2) ))/TWO
c     fulPva(3) = (ONE - gauss( fulDia(3) ))/TWO
c     fulPva(4) = (ONE - gauss( fulDia(4) ))/TWO
c     noePva(1) = (ONE - gauss( noeDia(1) ))/TWO
c     noePva(2) = (ONE - gauss( noeDia(2) ))/TWO
c     noePva(3) = (ONE - gauss( noeDia(3) ))/TWO
c     noePva(4) = (ONE - gauss( noeDia(4) ))/TWO
c     ------------------------------------------------------------------
c     WRITE(6,6000)
c     WRITE(6,5001)fulEst(1), fulEst(2), fulEst(3), fulEst(4)
c     WRITE(6,5001)noeEst(1), noeEst(2), noeEst(3), noeEst(4)
c     WRITE(6,5001)fulEso(1), fulEso(2), fulEso(3), fulEso(4)
c     WRITE(6,5001)noeEso(1), noeEso(2), noeEso(3), noeEso(4)
c     WRITE(6,5001)fulVar(1), fulVar(2), fulVar(3), fulVar(4)
c     WRITE(6,5001)noeVar(1), noeVar(2), noeVar(3), noeVar(4)
c     WRITE(6,5001)fulDia(1), fulDia(2), fulDia(3), fulDia(4)
c     WRITE(6,5001)noeDia(1), noeDia(2), noeDia(3), noeDia(4)
c     WRITE(6,5001)fulPva(1), fulPva(2), fulPva(3), fulPva(4)
c     WRITE(6,5001)noePva(1), noePva(2), noePva(3), noePva(4)
c     ------------------------------------------------------------------
* 6000 FORMAT( ' Output from compLagDiag(0). ', /)

c-----------------------------------------------------------------------
c     getWghLagDiag(0) processing
c-----------------------------------------------------------------------
      CALL getWghLagDia( lag, nT, dS, dT, nPer,
     &                   finfact, sInnovSd, vY, dDel, nDel,
     &                   dRedDelS, nRedDelS, dRedDelT, nRedDelT,
     &                   mSigUI, nSigUI, mSigUS, nSigUS,
     &                   mSigUT, nSigUT, mSigWT, nSigWT,
     &                   mInvSigW, nInvSigW,
     &                   wghEst, wghEso, wghVar, wghDia )

c     ------------------------------------------------------------------
c     Move the relevant results to common block output variables.
c     ------------------------------------------------------------------
      Wacfiem(lag)=wghEst(1)
      Wacfsem(lag)=wghEst(2)
      Wacfpem(lag)=wghEst(3)
      Wacfaem(lag)=wghEst(4)
c     ------------------------------------------------------------------
      Wacfier(lag)=wghEso(1)
      Wacfser(lag)=wghEso(2)
      Wacfper(lag)=wghEso(3)
      Wacfaer(lag)=wghEso(4)
c     ------------------------------------------------------------------
      Wacfidg(lag)=wghDia(1)
      Wacfsdg(lag)=wghDia(2)
      Wacfpdg(lag)=wghDia(3)
      Wacfadg(lag)=wghDia(4)

c     ------------------------------------------------------------------
c     Debug code.
c     ------------------------------------------------------------------
c     WRITE(6,6100)
c     WRITE(6,5001)wghEst(1), wghEst(2), wghEst(3), wghEst(4)
c     WRITE(6,5001)wghEso(1), wghEso(2), wghEso(3), wghEso(4)
c     WRITE(6,5001)wghVar(1), wghVar(2), wghVar(3), wghVar(4)
c     WRITE(6,5001)wghDia(1), wghDia(2), wghDia(3), wghDia(4)
c     ------------------------------------------------------------------
* 6100 FORMAT( ' Output from getWghLagDiag(0). ', /)

c-----------------------------------------------------------------------
c     compLagDiag(1) processing
c-----------------------------------------------------------------------
      lag = 1
      CALL compLagDiag( lag, nT, dS, dT, nPer,
     &                  finfact, sInnovSd, vIrrEst, nIrrEst,
     &                  vSeaEst, nSeaEst, vTreEst, nTreEst,
     &                  dDelS, nDelS, dDelT, nDelT,
     &                  mCovIrr, nCovIrr, mCovSea, nCovSea,
     &                  mCovTre, nCovTre, mCovSA, nCovSA,
     &                  fulEst, noeEst, fulEso, noeEso,
     &                  fulVar, noeVar, fulDia, noeDia )

c     ------------------------------------------------------------------
c     Move the relevant results to common block output variables.
c     ------------------------------------------------------------------
      Facfiem(lag)=fulEst(1)
      Facfsem(lag)=fulEst(2)
      Facfpem(lag)=fulEst(3)
      Facfaem(lag)=fulEst(4)
      Nacfiem(lag)=noeEst(1)
      Nacfsem(lag)=noeEst(2)
      Nacfpem(lag)=noeEst(3)
      Nacfaem(lag)=noeEst(4)
c     ------------------------------------------------------------------
      Facfier(lag)=fulEso(1)
      Facfser(lag)=fulEso(2)
      Facfper(lag)=fulEso(3)
      Facfaer(lag)=fulEso(4)
      Nacfier(lag)=noeEso(1)
      Nacfser(lag)=noeEso(2)
      Nacfper(lag)=noeEso(3)
      Nacfaer(lag)=noeEso(4)
c     ------------------------------------------------------------------
      Facfidg(lag)=fulDia(1)
      Facfsdg(lag)=fulDia(2)
      Facfpdg(lag)=fulDia(3)
      Facfadg(lag)=fulDia(4)
      Nacfidg(lag)=noeDia(1)
      Nacfsdg(lag)=noeDia(2)
      Nacfpdg(lag)=noeDia(3)
      Nacfadg(lag)=noeDia(4)

c     ------------------------------------------------------------------
c     Debug code.
c     ------------------------------------------------------------------
c     fulPva(1) = (ONE - gauss( fulDia(1) ))/TWO
c     fulPva(2) = (ONE - gauss( fulDia(2) ))/TWO
c     fulPva(3) = (ONE - gauss( fulDia(3) ))/TWO
c     fulPva(4) = (ONE - gauss( fulDia(4) ))/TWO
c     noePva(1) = (ONE - gauss( noeDia(1) ))/TWO
c     noePva(2) = (ONE - gauss( noeDia(2) ))/TWO
c     noePva(3) = (ONE - gauss( noeDia(3) ))/TWO
c     noePva(4) = (ONE - gauss( noeDia(4) ))/TWO
c     ------------------------------------------------------------------
c     WRITE(6,7000)
c     WRITE(6,5001)fulEst(1), fulEst(2), fulEst(3), fulEst(4)
c     WRITE(6,5001)noeEst(1), noeEst(2), noeEst(3), noeEst(4)
c     WRITE(6,5001)fulEso(1), fulEso(2), fulEso(3), fulEso(4)
c     WRITE(6,5001)noeEso(1), noeEso(2), noeEso(3), noeEso(4)
c     WRITE(6,5001)fulVar(1), fulVar(2), fulVar(3), fulVar(4)
c     WRITE(6,5001)noeVar(1), noeVar(2), noeVar(3), noeVar(4)
c     WRITE(6,5001)fulDia(1), fulDia(2), fulDia(3), fulDia(4)
c     WRITE(6,5001)noeDia(1), noeDia(2), noeDia(3), noeDia(4)
c     WRITE(6,5001)fulPva(1), fulPva(2), fulPva(3), fulPva(4)
c     WRITE(6,5001)noePva(1), noePva(2), noePva(3), noePva(4)
c     ------------------------------------------------------------------
* 7000 FORMAT( ' Output from compLagDiag(1). ', /)

c-----------------------------------------------------------------------
c     getWghLagDiag(1) processing
c-----------------------------------------------------------------------
      CALL getWghLagDia( lag, nT, dS, dT, nPer,
     &                   finfact, sInnovSd, vY, dDel, nDel,
     &                   dRedDelS, nRedDelS, dRedDelT, nRedDelT,
     &                   mSigUI, nSigUI, mSigUS, nSigUS,
     &                   mSigUT, nSigUT, mSigWT, nSigWT,
     &                   mInvSigW, nInvSigW,
     &                   wghEst, wghEso, wghVar, wghDia )

c     ------------------------------------------------------------------
c     Move the relevant results to common block output variables.
c     ------------------------------------------------------------------
      Wacfiem(lag)=wghEst(1)
      Wacfsem(lag)=wghEst(2)
      Wacfpem(lag)=wghEst(3)
      Wacfaem(lag)=wghEst(4)
c     ------------------------------------------------------------------
      Wacfier(lag)=wghEso(1)
      Wacfser(lag)=wghEso(2)
      Wacfper(lag)=wghEso(3)
      Wacfaer(lag)=wghEso(4)
c     ------------------------------------------------------------------
      Wacfidg(lag)=wghDia(1)
      Wacfsdg(lag)=wghDia(2)
      Wacfpdg(lag)=wghDia(3)
      Wacfadg(lag)=wghDia(4)

c     ------------------------------------------------------------------
c     Debug code.
c     ------------------------------------------------------------------
c     WRITE(6,7100)
c     WRITE(6,5001)wghEst(1), wghEst(2), wghEst(3), wghEst(4)
c     WRITE(6,5001)wghEso(1), wghEso(2), wghEso(3), wghEso(4)
c     WRITE(6,5001)wghVar(1), wghVar(2), wghVar(3), wghVar(4)
c     WRITE(6,5001)wghDia(1), wghDia(2), wghDia(3), wghDia(4)
c     ------------------------------------------------------------------
* 7100 FORMAT( ' Output from getWghLagDiag(1). ', /)

c-----------------------------------------------------------------------
c     compLagDiag(nPer) processing
c-----------------------------------------------------------------------
      lag = nPer
      CALL compLagDiag( lag, nT, dS, dT, nPer,
     &                  finfact, sInnovSd, vIrrEst, nIrrEst,
     &                  vSeaEst, nSeaEst, vTreEst, nTreEst,
     &                  dDelS, nDelS, dDelT, nDelT,
     &                  mCovIrr, nCovIrr, mCovSea, nCovSea,
     &                  mCovTre, nCovTre, mCovSA, nCovSA,
     &                  fulEst, noeEst, fulEso, noeEso,
     &                  fulVar, noeVar, fulDia, noeDia )

c     ------------------------------------------------------------------
c     Move the relevant results to common block output variables.
c     ------------------------------------------------------------------
      Facfiem(lag)=fulEst(1)
      Facfsem(lag)=fulEst(2)
      Facfpem(lag)=fulEst(3)
      Facfaem(lag)=fulEst(4)
      Nacfiem(lag)=noeEst(1)
      Nacfsem(lag)=noeEst(2)
      Nacfpem(lag)=noeEst(3)
      Nacfaem(lag)=noeEst(4)
c     ------------------------------------------------------------------
      Facfier(lag)=fulEso(1)
      Facfser(lag)=fulEso(2)
      Facfper(lag)=fulEso(3)
      Facfaer(lag)=fulEso(4)
      Nacfier(lag)=noeEso(1)
      Nacfser(lag)=noeEso(2)
      Nacfper(lag)=noeEso(3)
      Nacfaer(lag)=noeEso(4)
c     ------------------------------------------------------------------
      Facfidg(lag)=fulDia(1)
      Facfsdg(lag)=fulDia(2)
      Facfpdg(lag)=fulDia(3)
      Facfadg(lag)=fulDia(4)
      Nacfidg(lag)=noeDia(1)
      Nacfsdg(lag)=noeDia(2)
      Nacfpdg(lag)=noeDia(3)
      Nacfadg(lag)=noeDia(4)

c     ------------------------------------------------------------------
c     Debug code.
c     ------------------------------------------------------------------
c     fulPva(1) = (ONE - gauss( fulDia(1) ))/TWO
c     fulPva(2) = (ONE - gauss( fulDia(2) ))/TWO
c     fulPva(3) = (ONE - gauss( fulDia(3) ))/TWO
c     fulPva(4) = (ONE - gauss( fulDia(4) ))/TWO
c     noePva(1) = (ONE - gauss( noeDia(1) ))/TWO
c     noePva(2) = (ONE - gauss( noeDia(2) ))/TWO
c     noePva(3) = (ONE - gauss( noeDia(3) ))/TWO
c     noePva(4) = (ONE - gauss( noeDia(4) ))/TWO
c     ------------------------------------------------------------------
c     WRITE(6,8000)
c     WRITE(6,5001)fulEst(1), fulEst(2), fulEst(3), fulEst(4)
c     WRITE(6,5001)noeEst(1), noeEst(2), noeEst(3), noeEst(4)
c     WRITE(6,5001)fulEso(1), fulEso(2), fulEso(3), fulEso(4)
c     WRITE(6,5001)noeEso(1), noeEso(2), noeEso(3), noeEso(4)
c     WRITE(6,5001)fulVar(1), fulVar(2), fulVar(3), fulVar(4)
c     WRITE(6,5001)noeVar(1), noeVar(2), noeVar(3), noeVar(4)
c     WRITE(6,5001)fulDia(1), fulDia(2), fulDia(3), fulDia(4)
c     WRITE(6,5001)noeDia(1), noeDia(2), noeDia(3), noeDia(4)
c     WRITE(6,5001)fulPva(1), fulPva(2), fulPva(3), fulPva(4)
c     WRITE(6,5001)noePva(1), noePva(2), noePva(3), noePva(4)
c     ------------------------------------------------------------------
* 8000 FORMAT( ' Output from compLagDiag(nPer). ', /)

c-----------------------------------------------------------------------
c     getWghLagDiag(nPer) processing
c-----------------------------------------------------------------------
      CALL getWghLagDia( lag, nT, dS, dT, nPer,
     &                   finfact, sInnovSd, vY, dDel, nDel,
     &                   dRedDelS, nRedDelS, dRedDelT, nRedDelT,
     &                   mSigUI, nSigUI, mSigUS, nSigUS,
     &                   mSigUT, nSigUT, mSigWT, nSigWT,
     &                   mInvSigW, nInvSigW,
     &                   wghEst, wghEso, wghVar, wghDia )

c     ------------------------------------------------------------------
c     Move the relevant results to common block output variables.
c     ------------------------------------------------------------------
      Wacfiem(lag)=wghEst(1)
      Wacfsem(lag)=wghEst(2)
      Wacfpem(lag)=wghEst(3)
      Wacfaem(lag)=wghEst(4)
c     ------------------------------------------------------------------
      Wacfier(lag)=wghEso(1)
      Wacfser(lag)=wghEso(2)
      Wacfper(lag)=wghEso(3)
      Wacfaer(lag)=wghEso(4)
c     ------------------------------------------------------------------
      Wacfidg(lag)=wghDia(1)
      Wacfsdg(lag)=wghDia(2)
      Wacfpdg(lag)=wghDia(3)
      Wacfadg(lag)=wghDia(4)

c     ------------------------------------------------------------------
c     Debug code.
c     ------------------------------------------------------------------
c     WRITE(6,8100)
c     WRITE(6,5001)wghEst(1), wghEst(2), wghEst(3), wghEst(4)
c     WRITE(6,5001)wghEso(1), wghEso(2), wghEso(3), wghEso(4)
c     WRITE(6,5001)wghVar(1), wghVar(2), wghVar(3), wghVar(4)
c     WRITE(6,5001)wghDia(1), wghDia(2), wghDia(3), wghDia(4)
c     ------------------------------------------------------------------
* 8100 FORMAT( ' Output from getWghLagDiag(nPer). ', /)

c-----------------------------------------------------------------------
c     compCroDiag processing
c-----------------------------------------------------------------------
      pLagSmT = max(dS-dT+1,1)
      CALL compCroDiag( nT, dS, dT, nPer,
     &                  finfact, sInnovSd, vIrrEst, nIrrEst,
     &                  vSeaEst, nSeaEst, vTreEst, nTreEst,
     &                  dDelS, nDelS, dDelT, nDelT, dDel, nDel,
     &                  dRedDelS, nRedDelS, dRedDelT, nRedDelT,
     &                  mInvSigW, nInvSigW,  mSigUS, nSigUS,
     &                  mSigUT, nSigUT, mSigUI, nSigUI,
     &                  fulEst, fulEso, fulVar, fulDia, pLagSmT )

c     ------------------------------------------------------------------
c     Move the relevant results to common block output variables.
c     ------------------------------------------------------------------
      seaIrrEst = fulEst(1)
      seaIrrEso = fulEso(1)
      seaIrrVar = fulVar(1)
      seaIrrDia = fulDia(1)
c     ------------------------------------------------------------------
      seaTreEst = fulEst(2)
      seaTreEso = fulEso(2)
      seaTreVar = fulVar(2)
      seaTreDia = fulDia(2)
c     ------------------------------------------------------------------
      treIrrEst = fulEst(3)
      treIrrEso = fulEso(3)
      treIrrVar = fulVar(3)
      treIrrDia = fulDia(3)

c     ------------------------------------------------------------------
c     Debug code.
c     ------------------------------------------------------------------
c     fulPva(1) = (ONE - gauss( fulDia(1) ))/TWO
c     fulPva(2) = (ONE - gauss( fulDia(2) ))/TWO
c     fulPva(3) = (ONE - gauss( fulDia(3) ))/TWO
c     ------------------------------------------------------------------
c     WRITE(6,9000)
c     WRITE(6,5001)fulEst(1), fulEst(2), fulEst(3)
c     WRITE(6,5001)fulEso(1), fulEso(2), fulEso(3)
c     WRITE(6,5001)fulVar(1), fulVar(2), fulVar(3)
c     WRITE(6,5001)fulDia(1), fulDia(2), fulDia(3)
c     WRITE(6,5001)fulPva(1), fulPva(2), fulPva(3)
c     ------------------------------------------------------------------
* 9000 FORMAT( ' Output from compCroDiag. ', /)

c-----------------------------------------------------------------------
c     Filter square-gain/phase-delay processing (for Trend and SA)
c-----------------------------------------------------------------------
      CALL procFlts( dS, dT, nT, nPer, mDelS, nDelS,
     &               mSAPFlt, nSAPFlt, mTrePFlt, nTrePFlt )

c-----------------------------------------------------------------------
c     Semi-infinite Revision processing
c-----------------------------------------------------------------------
c     Get Table 5.3 lag.
c     ------------------------------------------------------------------
      CALL getTbl53Lag( nPer, iTbl53Lag )
c     ------------------------------------------------------------------
c     No revision processing for Irregular component.
c     ------------------------------------------------------------------
      infMSEs(1)=ZERO
      infRevs(1)=ZERO
c     ------------------------------------------------------------------
c     Revision processing for seasonal component.
c     ------------------------------------------------------------------
      CALL CONV( vSeaAR, oSeaAR+1, vSeaD, oSeaD+1, vSeaARD, sSeaARD )
      IF (( .not. dpeq(sSeaVar,ZERO) ) .and. 
     &    ( .not. dpeq(sSAVar,ZERO) )) THEN
       pd1 = max(sSeaARD-1,oSAAR,oMA)
       pd2 = max(oMA,oSAMA+sSeaARD-1)
       pd3 = max(nT+nPer-1+oSAMA,oSAAR,oMa-1)
       pd4 = max(oMa,oSAAR+oSeaMA)
       pd5 = max(nT+nPer-1+oSeaMA,sSeaARD-1,oMA-1)
       pd6 = max(sSeaARD-1+oSAMA,oSAAR+oSeaMA)
       pd7 = max(pd6,oMA)
       CALL getRevDecomp( vSeaARD, sSeaARD-1, vSAAR, oSAAR,
     &                    vSeaMA, oSeaMA, vSAMA, oSAMA, vMA, oMA,
     &                    nPer, iTbl53Lag, sInnovSd, sSeaVar, sSAVar,
     &                    nT-nRevs, nT+nPer-1,lInfMSEs(1,1),
     &                    lInfMSE1s(1,1), lInfMSE2s(1,1),
     &                    lInfMSE3s(1,1), pd1, pd2, pd3, pd4, pd5,
     &                    pd6, pd7 )
       infMSEs(2) = lInfMSEs(nRevs,1)
       infRevs(2) = curMSEs(2) - lInfMSEs(nRevs,1)
      ELSE IF (( .not. dpeq(sSeaVar,ZERO) ) .and.
     &         ( .not. dpeq(sIrrVar,ZERO) ) .and.
     &         ( dpeq(sCycVar,ZERO) )) THEN
       pd1 = max(sSeaARD-1,oIrrAR,oMA)
       pd2 = max(oMA,oIrrMA+sSeaARD-1)
       pd3 = max(nT+nPer-1+oIrrMA,oIrrAR,oMa-1)
       pd4 = max(oMa,oIrrAR+oSeaMA)
       pd5 = max(nT+nPer-1+oSeaMA,sSeaARD-1,oMA-1)
       pd6 = max(sSeaARD-1+oIrrMA,oIrrAR+oSeaMA)
       pd7 = max(pd6,oMA)
       CALL getRevDecomp( vSeaARD, sSeaARD-1, vIrrAR, oIrrAR,
     &                    vSeaMA, oSeaMA, vIrrMA, oIrrMA, vMA, oMA,
     &                    nPer, iTbl53Lag, sInnovSd, sSeaVar, sIrrVar,
     &                    nT-nRevs, nT+nPer-1, lInfMSEs(1,1),
     &                    lInfMSE1s(1,1), lInfMSE2s(1,1),
     &                    lInfMSE3s(1,1), pd1, pd2, pd3, pd4, pd5,
     &                    pd6, pd7 )
       infMSEs(2) = lInfMSEs(nRevs,1)
       infRevs(2) = curMSEs(2) - lInfMSEs(nRevs,1)
      ELSE
       infMSEs(2)=ZERO
       infRevs(2)=ZERO
      END IF
c     ------------------------------------------------------------------
c     Revision processing for trend component.
c     ------------------------------------------------------------------
      CALL CONV( vTreAR, oTreAR+1, vTreD, oTreD+1, vTreARD, sTreARD )
      IF (( .not. dpeq(sTreVar,ZERO) ) .and. 
     &    ( .not. dpeq(sTAVar,ZERO) )) THEN
       pd1 = max(sTreARD-1,oTAAR,oMA)
       pd2 = max(oMA,sTreARD-1+oTAMA)
       pd3 = max(nT+nPer-1+oTAMA,oTAAR,oMa-1)
       pd4 = max(oMa,oTAAR+oTreMA)
       pd5 = max(nT+nPer-1+oTreMA,sTreARD-1,oMA-1)
       pd6 = max(sTreARD-1+oTAMA,oTAAR+oTreMA)
       pd7 = max(pd6,oMA)
       CALL getRevDecomp( vTreARD, sTreARD-1, vTAAR, oTAAR,
     &                    vTreMA, oTreMA, vTAMA, oTAMA, vMA, oMA,
     &                    nPer, iTbl53Lag, sInnovSd, sTreVar, sTAVar,
     &                    nT-nRevs, nT+nPer-1, lInfMSEs(1,2),
     &                    lInfMSE1s(1,2), lInfMSE2s(1,2),
     &                    lInfMSE3s(1,2), pd1, pd2, pd3, pd4, pd5,
     &                    pd6, pd7)
       infMSEs(3) = lInfMSEs(nRevs,2)
       infRevs(3) = curMSEs(3) - lInfMSEs(nRevs,2)
      ELSE IF (( .not. dpeq(sTreVar,ZERO) ) .and.
     &         ( .not. dpeq(sIrrVar,ZERO) ) .and.
     &         ( dpeq(sCycVar,ZERO) )) THEN
       pd1 = max(sTreARD-1,oIrrAR,oMA)
       pd2 = max(oMA,sTreARD-1+oIrrMA)
       pd3 = max(nT+nPer-1+oIrrMA,oIrrAR,oMa-1)
       pd4 = max(oMa,oIrrAR+oTreMA)
       pd5 = max(nT+nPer-1+oTreMA,sTreARD-1,oMA-1)
       pd6 = max(sTreARD-1+oIrrMA,oIrrAR+oTreMA)
       pd7 = max(pd6,oMA)
       CALL getRevDecomp( vTreARD, sTreARD-1, vIrrAR, oIrrAR,
     &                    vTreMA, oTreMA, vIrrMA, oIrrMA, vMA, oMA,
     &                    nPer, iTbl53Lag, sInnovSd, sTreVar, sIrrVar,
     &                    nT-nRevs, nT+nPer-1, lInfMSEs(1,2),
     &                    lInfMSE1s(1,2), lInfMSE2s(1,2),
     &                    lInfMSE3s(1,2), pd1, pd2, pd3, pd4, pd5,
     &                    pd6, pd7 )
       infMSEs(3) = lInfMSEs(nRevs,2)
       infRevs(3) = curMSEs(3) - lInfMSEs(nRevs,2)
      ELSE
       infMSEs(3)=ZERO
       infRevs(3)=ZERO
      END IF
c     ------------------------------------------------------------------
c     Calculate standard error of revisions for last 5 years.
c     ------------------------------------------------------------------
c     Revision by BCM - only compute seRevs if the corresponding
c     infRevs are > ZERO - 4-11-2006
c     ------------------------------------------------------------------
      DO i=1,nRevs
       IF(infRevs(2).gt.ZERO)THEN
        seRevs(i,1) = lCurMSEs(i,1)-lInfMSEs(i,1)
        IF ( seRevs(i,1) .gt. ZERO ) THEN
         seRevs(i,1) = DSQRT( seRevs(i,1) )*sInnovSd
        ELSE
         seRevs(i,1) = ZERO
        END IF
       ELSE
        seRevs(i,1) = ZERO
       END IF
       IF(infRevs(3).gt.ZERO)THEN
        seRevs(i,2) = lCurMSEs(i,2)-lInfMSEs(i,2)
        IF ( seRevs(i,2) .gt. ZERO ) THEN
          seRevs(i,2) = DSQRT( seRevs(i,2) )*sInnovSd
        ELSE
          seRevs(i,2) = ZERO
        END IF
       ELSE
        seRevs(i,2) = ZERO
       END IF
      END DO

c     ------------------------------------------------------------------
c     Debug code.
c     ------------------------------------------------------------------
c     WRITE(6,200)
c     WRITE(6,201)lInfMSEs(nRevs,1),lInfMSE1s(nRevs,1),
c    &            lInfMSE2s(nRevs,1),lInfMSE3s(nRevs,1)
c     WRITE(6,201)lInfMSEs(nRevs,2),lInfMSE1s(nRevs,2),
c    &            lInfMSE2s(nRevs,2),lInfMSE3s(nRevs,2)
c     WRITE(6,201)mSeaVar(nT*nT),mSeaVar(nT*nT-1),
c    &            mSeaVar(nT*nT-nPer),mSeaVar(nT*nT-iTbl53Lag)
c     WRITE(6,201)mTreVar(nT*nT),mTreVar(nT*nT-1),
c    &            mTreVar(nT*nT-nPer),mTreVar(nT*nT-iTbl53Lag)
c 200 FORMAT(' getRevDecomp processing' )
c 201 FORMAT( 4( 1x, g12.5 ) )

c-----------------------------------------------------------------------
c     Growth Rate processing (for Trend and SA)
c-----------------------------------------------------------------------
      CALL getGR( nT, dS, dT, nPer, dDel, nDel, dDelS, nDelS,
     &            dDelT, nDelT, mInvSigUT, nInvSigUT,
     &            mInvSigWT, nInvSigWT, mInvSigW, nInvSigW,
     &            mSigUTf, nSigUTf, mSigUTfUT, nSigUTfUT,
     &            mSigWTf, nSigWTf, mSigWTfWT, nSigWTfWT,
     &            mSigWf, nSigWf, mSigWfW, nSigWfW,
     &            mSeaVar, nSeaVar, mTreVar, nTreVar,
     &            nRevs, lInfMSEs, lInfMSE1s, lInfMSE2s, lInfMSE3s,
     &            sInnovSd, iTbl53Lag,
     &            vSeaGRSE1, nSeaGRSE1, vTreGRSE1, nTreGRSE1,
     &            vSeaGRSE2, nSeaGRSE2, vTreGRSE2, nTreGRSE2,
     &            vTbl51, vTbl53, vTbl54, vTbl56, vTbl57 )

c     ------------------------------------------------------------------
c     Debug code.
c     WRITE(6,9001)
c     WRITE(6,9010)vTbl51(1),vTbl51(2)
c     WRITE(6,9010)vTbl51(3),vTbl51(4)
c     WRITE(6,9010)vTbl51(5),vTbl51(6)
c     WRITE(6,9010)DSQRT(vTbl51(5)),DSQRT(vTbl51(6))
c     ------------------------------------------------------------------
c     WRITE(6,9002)
c     DO i=1,nSeaGRSE1(1)
c       WRITE(6,9010)vSeaGRSE1(i),vTreGRSE1(i)
c     END DO
c     ------------------------------------------------------------------
c     WRITE(6,9003)
c     WRITE(6,9010)vTbl53(1),vTbl53(2)
c     ------------------------------------------------------------------
c     WRITE(6,9004)
c     WRITE(6,9010)vTbl54(1),vTbl54(2)
c     WRITE(6,9010)vTbl54(3),vTbl54(4)
c     WRITE(6,9010)vTbl54(5),vTbl54(6)
c     WRITE(6,9010)DSQRT(vTbl54(5)),DSQRT(vTbl54(6))
c     ------------------------------------------------------------------
c     WRITE(6,9005)
c     DO i=1,nSeaGRSE2(1)
c       WRITE(6,9010)vSeaGRSE2(i),vTreGRSE2(i)
c     END DO
c     ------------------------------------------------------------------
c     WRITE(6,9006)
c     WRITE(6,9010)vTbl56(1,1),vTbl56(1,2),vTbl56(1,2)-vTbl56(1,1)
c     WRITE(6,9010)vTbl56(2,1),vTbl56(2,2)
c     WRITE(6,9010)vTbl56(3,1),vTbl56(3,2),vTbl56(3,2)-vTbl56(3,1)
c     WRITE(6,9010)vTbl56(4,1),vTbl56(4,2)
c     ------------------------------------------------------------------
 9010 FORMAT( 3( 1x, G12.5 ) )
 9001 FORMAT( /, ' Output from getGR: Table 5.1 ', /)
 9002 FORMAT( /, ' Output from getGR: Table 5.2 ', /)
 9003 FORMAT( /, ' Output from getGR: Table 5.3 ', /)
 9004 FORMAT( /, ' Output from getGR: Table 5.4 ', /)
 9005 FORMAT( /, ' Output from getGR: Table 5.5 ', /)
 9006 FORMAT( /, ' Output from getGR: Table 5.6 ', /)

c-----------------------------------------------------------------------
c     Finite Revision processing
c-----------------------------------------------------------------------
      IF ( out .eq. 0 ) THEN
        CALL compRevs( dS, dT, nT, nPer, nDiff, lSeaPre,
     &                 nSave, nSave2, nSave3,
     &                 vSeaAR, oSeaAR, vSeaMA, oSeaMA,
     &                 vTreAR, oTreAR, vTreMA, oTreMA,
     &                 vCycAR, oCycAR, vCycMA, oCycMA,
     &                 vSeaD,  oSeaD,  vTreD,  oTreD,
     &                 sSeaVar, sTreVar, sCycVar, sIrrVar,
     &                 mDelS, dDelS, nDelS,
     &                 mDelT, dDelT, nDelT,
     &                 mDel, dDel, nDel,
     &                 mRedDelS, dRedDelS, nRedDelS,
     &                 mRedDelT, dRedDelT, nRedDelT,
     &                 mSigUS, nSigUS, mSigUT, nSigUT,
     &                 mSigUI, nSigUI, mSigWS, nSigWS,
     &                 mSigWT, nSigWT, mSigW, nSigW,
     &                 mSigUTf, nSigUTf, mSigUTfUT, nSigUTfUT,
     &                 mSigWTf, nSigWTf, mSigWTfWT, nSigWTfWT,
     &                 mSigWf, nSigWf, mSigWfW, nSigWfW,
     &                 mInvSigUS, nInvSigUS, mInvSigUT, nInvSigUT,
     &                 mInvSigWS, nInvSigWS, mInvSigWT, nInvSigWT,
     &                 mInvSigW, nInvSigW, mIrrVar, nIrrVar,
     &                 mSeaVar, nSeaVar, mTreVar, nTreVar,
     &                 DSQRT(Var), curMSEs, finMSEs, finRevs )

c     ------------------------------------------------------------------
c     Debug code.
c     ------------------------------------------------------------------
c       WRITE(6,9100)
c       WRITE(6,9101)(infMSEs(i),i=1,3)
c       WRITE(6,9101)(infRevs(i),i=1,3)
c       WRITE(6,9101)(curMSEs(i),i=1,3)
c       WRITE(6,9101)
c       WRITE(6,9101)(finRevs(i,1)/infRevs(i),i=1,3)
c       WRITE(6,9101)(finRevs(i,2)/infRevs(i),i=1,3)
c       WRITE(6,9101)(finRevs(i,3)/infRevs(i),i=1,3)
c       WRITE(6,9101)(finRevs(i,4)/infRevs(i),i=1,3)
c       WRITE(6,9101)(finRevs(i,5)/infRevs(i),i=1,3)
c       DO j=1,5
c        WRITE(6,9101)(finMSEs(i,j),i=1,3)
c       END DO
c       WRITE(6,9101)

c     ------------------------------------------------------------------
c     Convert the revision variances to SEATS definition of 
c     standard error of revisions.
c     ------------------------------------------------------------------
        DO j=1,5
         DO i=1,3
          IF ( (infRevs(i) .gt. ZERO) .and.
     &         (infRevs(i) .ge. finRevs(i,j)) ) THEN
           relRevs(i,j)=ONE-DSQRT((infRevs(i)-finRevs(i,j))/infRevs(i))
           relRevs(i,j)=relRevs(i,j)*ONEP
          ELSE IF ( DABS(infRevs(i)-finRevs(i,j)) .lt. 1.0D-5 ) THEN
           relRevs(i,j)=ONEP
          ELSE
           relRevs(i,j)=ZERO
          END IF
c       WRITE(6,9101)infRevs(i),finRevs(i,j),finMSEs(i,j),
c    &               finMSEs(i,j)-infMSEs(i)
         END DO
c      WRITE(6,9101)(relRevs(i,j),i=1,3)
      END DO

c     ------------------------------------------------------------------
c     Debug code.
c     ------------------------------------------------------------------
c     WRITE(6,9101)
c     DO i=1,nRevs,6
c       WRITE(6,9101)(lCurMSEs(j,1)-lInfMSEs(j,1),j=i,min(i+5,nRevs))
c     END DO
c     WRITE(6,9101)
c     DO i=1,nRevs,6
c       WRITE(6,9101)(lCurMSEs(j,2)-lInfMSEs(j,2),j=i,min(i+5,nRevs))
c     END DO
c     ------------------------------------------------------------------
* 9100 FORMAT( ' Output from Revision processing. ', /)
* 9101 FORMAT( 6( 1x, G12.5 ) )
* 9102 FORMAT( 1x, a, 6( 1x, G12.5 ) )
      END IF

      RETURN
      END