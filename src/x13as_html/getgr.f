      SUBROUTINE getGR( nT, dS, dT, nPer, dDel, nDel, dDelS, nDelS,
     &                  dDelT, nDelT, mInvSigUT, nInvSigUT,
     &                  mInvSigWT, nInvSigWT, mInvSigW, nInvSigW,
     &                  mSigUTf, nSigUTf, mSigUTfUT, nSigUTfUT,
     &                  mSigWTf, nSigWTf, mSigWTfWT, nSigWTfWT,
     &                  mSigWf, nSigWf, mSigWfW, nSigWfW,
     &                  mSeaVar, nSeaVar, mTreVar, nTreVar,
     &                  nRevs, infMSEs, infMSE1s, infMSE2s, infMSE3s,
     &                  sdSig, tbl53Lag,
     &                  vSeaGRSE1, nSeaGRSE1, vTreGRSE1, nTreGRSE1,
     &                  vSeaGRSE2, nSeaGRSE2, vTreGRSE2, nTreGRSE2,
     &                  vTbl51, vTbl53, vTbl54, vTbl56, vTbl57 )
c-----------------------------------------------------------------------
c getGR.f, Release 1, Subroutine Version 1.0, Created 14 Apr 2006.
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 14 Apr 2006.
c     Modified by REG, on 26 May 2006, to add error processing
c       of square SEs numerically less than zero.
c-----------------------------------------------------------------------
c     This subroutine calculates some growth rate MSEs and SEs per
c     McElroy's paper "Model-Based Formulas for Growth Rates and their
c     Standard Errors" in order to generate tables 5.1 through 5.7.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c dDel    d   diagonal form of overall differencing matrix: mDel
c dDelS   d   diagonal form of seasonal differencing matrix: mDelS
c dDelT   d   diagonal form of trend differencing matrix: mDelT
c dS      i   size of Seasonal Differencing
c dT      i   size of Trend Differencing
c infMSEs d   lag 0 MSEs for component estimates for the last five years 
c             and the next year: first column for seasonal,
c             second column for trend
c infMSE1s d  lag 1 MSEs for component estimates for the last five years 
c             and the next year: first column for seasonal,
c             second column for trend
c infMSE2s d  lag nPer MSEs for component estimates 
c             for the last five years and the next year: 
c             first column for seasonal, second column for trend
c infMSE3s d  lag iTbl53Lag MSEs for component estimates 
c             for the last five years and the next year: 
c             first column for seasonal, second column for trend
c mInvSigUT d inverse of mSigUT: covariance matrix 
c             for trend differenced trend component
c mInvSigW  d inverse of mSigW: covariance matrix for differenced data
c mInvSigWT d inverse of mSigWT: covariance matrix 
c             for trend differenced seasonal adjusted component
c mSeaVar d   variance matrix of estimated seasonal
c mSigUT  d   covariance matrix for differenced trend (UT)
c mSigUTf d   covariance matrix for future differenced trend (UTf)
c mSigUTfUT d cross covariance matrix for (UTf,UT)
c mSigW   d   covariance matrix for differenced data (W)
c mSigWf  d   covariance matrix for future differenced data (Wf)
c mSigWfW d   cross covariance matrix for (Wf,W)
c mSigWT  d   covariance matrix for differenced seasonally adjusted (WT)
c mSigWTf d   covariance matrix for future differenced seasonally  
c             adjusted (WTf)
c mSigWTfWT d cross covariance matrix for (WTf,WT)
c mTreVar d   variance matrix of estimated trend
c nDel    i   size (rows,columns) of mDel
c nDelS   i   size (rows,columns) of mDelS
c nDelT   i   size (rows,columns) of mDelT
c nInvSigUT i size (rows,columns) of mInvSigUT matrix
c nInvSigW  i size (rows,columns) of mInvSigW  matrix
c nInvSigWT i size (rows,columns) of mInvSigWT matrix
c nPer    i   size of seasonal period
c nRevs   i   row index in infMSExs associated 
c             with last observation
c nSeaGRSE1 i size (rows,columns) of vSeaGRSE1 vector
c nSeaGRSE2 i size (rows,columns) of vSeaGRSE2 vector
c nSeaVar i   size (rows,columns) of mSeaVar matrix
c nSigUT  i   size (rows,columns) of mSigUT matrix
c nSigUTf i   size (rows,columns) of mSigUTf matrix
c nSigUTfUT i size (rows,columns) of mSigUTfUT matrix
c nSigW   i   size (rows,columns) of mSigW matrix
c nSigWf  i   size (rows,columns) of mSigWf matrix
c nSigWfW i   size (rows,columns) of mSigWfW matrix
c nSigWT  i   size (rows,columns) of mSigWT matrix
c nSigWTf i   size (rows,columns) of mSigWTf matrix
c nSigWTfWT i size (rows,columns) of mSigWTfWT matrix
c nT      i   size of data available
c nTreGRSE1 i size (rows,columns) of vTreGRSE1 vector
c nTreGRSE2 i size (rows,columns) of vTreGRSE2 vector
c nTreVar i   size (rows,columns) of mTreVar matrix
c sdSig   d   data innovation stdev
c tbl53Lag i  lag between last observation of data and last observation
c             of previous year
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
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c DSqrtOr0 d external function reference
c finMSE  i   variable used to calculate finite sample MSE part 
c             of growth rate
c i1      i   finite sample index of base entry in growth rate
c i1m     i   semi-infinite sample index of base entry in growth rate
c i2      i   finite sample index of second entry in growth rate
c i2m     i   semi-infinite sample index of second entry in growth rate
c infMSE  i   variable used to calculate semi-finite sample MSE part 
c             of growth rate
c j       i   do loop index variable
c nSigSAf i   size (rows,columns) of mSigSAf matrix
c nSigSAfSA i size (rows,columns) of mSigSAfSA matrix
c nSigTf  i   size (rows,columns) of mSigTf matrix
c nSigTfT i   size (rows,columns) of mSigTfT matrix
c nSigYf  i   size (rows,columns) of mSigYf matrix
c mSigSAf d   covariance matrix for residuals of forecasts of future 
c             seasonal adjustment (SAf)
c mSigSAfSA d cross covariance matrix of (SAf,SA)
c mSigTf  d   covariance matrix for residuals of forecasts of future 
c             trend (Tf)
c mSigTfT d   cross covariance matrix of (Tf,T)
c mSigYf  d   covariance matrix for residuals of forecasts of future 
c             observations (Yf)
c num     i   identifies maximum number of entries in tables 5.2 and 5.5
c num1    i   size of table 5.2 (vSeaGRSE1 and vTreGRSE1 vectors)
c num2    i   size of table 5.5 (vSeaGRSE2 and vTreGRSE2 vectors)
c nSave2  i   size of local large matrices
c varSig  d   data innovation variance
c-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'srslen.prm'
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER dS, dT, nPer, nRevs, nT, tbl53Lag
      INTEGER nDel(2), nDelS(2), nDelT(2)
      INTEGER nInvSigUT(2), nInvSigWT(2), nInvSigW(2)
      INTEGER nSigUTf(2), nSigUTfUT(2), nSigWTf(2), nSigWTfWT(2),
     &        nSigWf(2), nSigWfW(2)
      INTEGER nSeaVar(2), nTreVar(2)
      INTEGER nSeaGRSE1(2), nSeaGRSE2(2), nTreGRSE1(2), nTreGRSE2(2)
      DOUBLE PRECISION sdSig
      DOUBLE PRECISION dDel(dS+dT+1), dDelS(dS+1), dDelT(dT+1)
      DOUBLE PRECISION mInvSigUT(nT-dT,nT-dT), mInvSigWT(nT-dT,nT-dT),
     &                 mInvSigW(nT-dS-dT,nT-dS-dT)
      DOUBLE PRECISION mSigUTf(nPer,nPer), mSigUTfUT(nPer,nT-dT)
*      DOUBLE PRECISION mSigWTf(nPer,nPer), mSigWTfWT(nPer,nT-dS)
      DOUBLE PRECISION mSigWTf(nPer,nPer), mSigWTfWT(nPer,nT-dT)
      DOUBLE PRECISION mSigWf(nPer,nPer), mSigWfW(nPer,nT-dS-dT)
      DOUBLE PRECISION mSeaVar(nT,nT), mTreVar(nT,nT)
      DOUBLE PRECISION vSeaGRSE1(nT-1), vTreGRSE1(nT-1)
      DOUBLE PRECISION vSeaGRSE2(nT-1), vTreGRSE2(nT-1)
      DOUBLE PRECISION infMSEs(72,2), infMSE1s(72,2)
      DOUBLE PRECISION infMSE2s(72,2), infMSE3s(72,2)
      DOUBLE PRECISION vTbl51(6), vTbl53(2), vTbl54(6), vTbl56(6,2),
     &                 vTbl57(3,3)

c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i1, i1m, i2, i2m, j
      INTEGER num, num1, num2, nSave2
      INTEGER nSigTf(2), nSigTfT(2), nSigSAf(2), nSigSAfSA(2), nSigYf(2)
      PARAMETER (nSave2=12*POBS)
      DOUBLE PRECISION varSig
      DOUBLE PRECISION mSigTf(nPer,nPer), mSigTfT(nSave2)
      DOUBLE PRECISION mSigSAf(nPer,nPer), mSigSAfSA(nSave2)
      DOUBLE PRECISION mSigYf(nPer,nPer)
      DOUBLE PRECISION finMSE, infMSE, ZERO
      DOUBLE PRECISION DSqrtOr0
      PARAMETER (ZERO=0.0d0)
      SAVE mSigTfT, mSigSAfSA

c-----------------------------------------------------------------------
c     Determine max size of tables 5.2 and 5.5 based on nPer.
c-----------------------------------------------------------------------
      IF (nPer .eq. 12) THEN
       num = 36
      ELSEIF (nPer .eq. 6) THEN
       num = 18
      ELSEIF (nPer .eq. 4) THEN
       num = 12
      ELSE
       num = 8
      END IF
      varSig = sdSig**2

c-----------------------------------------------------------------------
c     Calculate Table 5.1 MSEs for last 2 months in sample using one 
c     period (current month minus previous month) growth rates.
c-----------------------------------------------------------------------
c     Identify one period behind growth rate indexes 
c     for last month in data minus previous period.
c     ------------------------------------------------------------------
      i1  = nT
      i1m = i1-1
      i2  = nRevs
      i2m = i2-1
c     ------------------------------------------------------------------
c     Calculate one period behind growth rate SEs for the trend and
c     seasonal adjustment.
c     ------------------------------------------------------------------
      vTbl51(1) = varSig * 
     &  (infMSEs(i2,2)  + infMSEs(i2m,2)   - 2.0d0*infMSE1s(i2m,2))
      vTbl51(5) = varSig * 
     &  (mTreVar(i1,i1) + mTreVar(i1m,i1m) - 2.0d0*mTreVar(i1m,i1))
      vTbl51(3) = vTbl51(5) - vTbl51(1)
c     ------------------------------------------------------------------
      vTbl51(2) = varSig * 
     &  (infMSEs(i2,1)  + infMSEs(i2m,1)   - 2.0d0*infMSE1s(i2m,1))
      vTbl51(6) = varSig * 
     &  (mSeaVar(i1,i1) + mSeaVar(i1m,i1m) - 2.0d0*mSeaVar(i1m,i1))
      vTbl51(4) = vTbl51(6) - vTbl51(2)

c-----------------------------------------------------------------------
c     Table 5.2 size restriction due to number of observations.
c-----------------------------------------------------------------------
      IF (num .gt. nT-1) THEN
       num1 = nT-1
      ELSE
       num1 = num
      END IF

c-----------------------------------------------------------------------
c     Calculate Table 5.2 Standard Errors of Revision for each of the 
c     desired periods using one period (current month minus previous 
c     month) growth rates.
c-----------------------------------------------------------------------
c     Identify size of table 5.2 vectors.
c     ------------------------------------------------------------------
      nSeaGRSE1(1) = num1
      nSeaGRSE1(2) = 1
      nTreGRSE1(1) = num1
      nTreGRSE1(2) = 1
c     ------------------------------------------------------------------
c     For each month in the table 5.2
c     ------------------------------------------------------------------
      DO j = 1, num1
c     ------------------------------------------------------------------
c     Identify one period behind growth rate indexes 
c     for desired month in data minus previous period.
c     ------------------------------------------------------------------
       i1  = nT-j+1
       i1m = i1-1
       i2  = nRevs-j+1
       i2m = i2-1
c     ------------------------------------------------------------------
c     Calculate one period behind growth rate SEs for the trend and
c     seasonal adjustment.
c     ------------------------------------------------------------------
       vSeaGRSE1(j) = 
     &     mSeaVar(i1,i1) + mSeaVar(i1m,i1m) - 2.0d0*mSeaVar(i1m,i1)
     &   - infMSEs(i2,1)  - infMSEs(i2m,1)   + 2.0d0*infMSE1s(i2m,1)
*       vSeaGRSE1(j) = DSQRT( vSeaGRSE1(j) ) * sdSig
       vSeaGRSE1(j) = DSqrtOr0( vSeaGRSE1(j) ) * sdSig
c     ------------------------------------------------------------------
       vTreGRSE1(j) = 
     &     mTreVar(i1,i1) + mTreVar(i1m,i1m) - 2.0d0*mTreVar(i1m,i1)
     &   - infMSEs(i2,2)  - infMSEs(i2m,2)   + 2.0d0*infMSE1s(i2m,2)
*       vTreGRSE1(j) = DSQRT( vTreGRSE1(j) ) * sdSig
       vTreGRSE1(j) = DSqrtOr0( vTreGRSE1(j) ) * sdSig
      END DO

c-----------------------------------------------------------------------
c     Calculate Table 5.3 Standard Errors of Revision using growth rate
c     from last month in data minus last month from previous year.
c-----------------------------------------------------------------------
c     Identify growth rate indexes over current year
c     for last month in data minus last month in previous year.
c     ------------------------------------------------------------------
      i1  = nT
      i1m = i1-tbl53Lag
      i2  = nRevs
      i2m = i2-tbl53Lag
c     ------------------------------------------------------------------
c     Calculate current year growth rate SEs for the trend 
c     and seasonal adjustment.
c     ------------------------------------------------------------------
      vTbl53(1) = 
     &    mTreVar(i1,i1) + mTreVar(i1m,i1m) - 2.0d0*mTreVar(i1m,i1)
     &  - infMSEs(i2,2)  - infMSEs(i2m,2)   + 2.0d0*infMSE3s(i2m,2)
      vTbl53(1) = DSqrtOr0( vTbl53(1) ) * sdSig
c     ------------------------------------------------------------------
      vTbl53(2) = 
     &    mSeaVar(i1,i1) + mSeaVar(i1m,i1m) - 2.0d0*mSeaVar(i1m,i1)
     &  - infMSEs(i2,1)  - infMSEs(i2m,1)   + 2.0d0*infMSE3s(i2m,1)
      vTbl53(2) = DSqrtOr0( vTbl53(2) ) * sdSig

c-----------------------------------------------------------------------
c     Calculate Table 5.4 MSEs using growth rate from last month in data
c     minus one seasonal period previous.
c-----------------------------------------------------------------------
c     Identify growth rate indexes over last seasonal period
c     for last month in data minus same month in previous year.
c     ------------------------------------------------------------------
      i1  = nT
      i1m = i1-nPer
      i2  = nRevs
      i2m = i2-nPer
c     ------------------------------------------------------------------
c     Calculate one seasonal period behind growth rate SEs for the trend 
c     and seasonal adjustment.
c     ------------------------------------------------------------------
      vTbl54(1) = varSig * 
     &  (infMSEs(i2,2)  + infMSEs(i2m,2)   - 2.0d0*infMSE2s(i2m,2))
      vTbl54(5) = varSig * 
     &  (mTreVar(i1,i1) + mTreVar(i1m,i1m) - 2.0d0*mTreVar(i1m,i1))
      vTbl54(3) = vTbl54(5) - vTbl54(1)
c     ------------------------------------------------------------------
      vTbl54(2) = varSig * 
     &  (infMSEs(i2,1)  + infMSEs(i2m,1)   - 2.0d0*infMSE2s(i2m,1))
      vTbl54(6) = varSig * 
     &  (mSeaVar(i1,i1) + mSeaVar(i1m,i1m) - 2.0d0*mSeaVar(i1m,i1))
      vTbl54(4) = vTbl54(6) - vTbl54(2)

c-----------------------------------------------------------------------
c     Table 5.5 size restriction due to number of observations.
c-----------------------------------------------------------------------
      IF (num-nPer+1 .gt. nT-nPer) THEN
       num2 = nT-nPer
      ELSE
       num2 = num-nPer+1
      END IF
      
c-----------------------------------------------------------------------
c     Calculate Table 5.5 Standard Errors of Revision using growth rate
c     from current month minus one seasonal period previous.
c-----------------------------------------------------------------------
c     Identify size of table 5.5 vectors.
c     ------------------------------------------------------------------
      nSeaGRSE2(1) = num2
      nSeaGRSE2(2) = 1
      nTreGRSE2(1) = num2
      nTreGRSE2(2) = 1
c     ------------------------------------------------------------------
c     For each month in the table 5.5
c     ------------------------------------------------------------------
      DO j = 1, num2
c     ------------------------------------------------------------------
c     Identify one seasonal period behind growth rate indexes 
c     for desired month in data minus same month in previous year.
c     ------------------------------------------------------------------
       i1  = nT-j+1
       i1m = i1-nPer
       i2  = nRevs-j+1
       i2m = i2-nPer
c     ------------------------------------------------------------------
c     Calculate one seasonal period behind growth rate SEs for the trend 
c     and seasonal adjustment.
c     ------------------------------------------------------------------
       vSeaGRSE2(j) = 
     &     mSeaVar(i1,i1) + mSeaVar(i1m,i1m) - 2.0d0*mSeaVar(i1m,i1)
     &   - infMSEs(i2,1)  - infMSEs(i2m,1)   + 2.0d0*infMSE2s(i2m,1)
*       vSeaGRSE2(j) = DSQRT( vSeaGRSE2(j) ) * sdSig
       vSeaGRSE2(j) = DSqrtOr0( vSeaGRSE2(j) ) * sdSig
c     ------------------------------------------------------------------
       vTreGRSE2(j) = 
     &     mTreVar(i1,i1) + mTreVar(i1m,i1m) - 2.0d0*mTreVar(i1m,i1)
     &   - infMSEs(i2,2)  - infMSEs(i2m,2)   + 2.0d0*infMSE2s(i2m,2)
*       vTreGRSE2(j) = DSQRT( vTreGRSE2(j) ) * sdSig
       vTreGRSE2(j) = DSqrtOr0( vTreGRSE2(j) ) * sdSig
      END DO

c-----------------------------------------------------------------------
c     Calculate Table 5.6 Standard Errors using centered growth rate
c     from current month plus half of seasonal period (forecasts) 
c     minus current month minus half of seasonal period,
c-----------------------------------------------------------------------
c     Calculate covariance matrix for residuals of forecasts 
c     of future data.
c     ------------------------------------------------------------------
      CALL getForYMSE( nT, dS+dT, nPer, dDel, nDel,
     &                 mInvSigW, nInvSigW, mSigWf, nSigWf,
     &                 mSigWfW, nSigWfW, mSigYf, nSigYf )
c     ------------------------------------------------------------------
c     Calculate covariance matrices for residuals of forecasts 
c     of future seasonal adjustment.
c     ------------------------------------------------------------------
      CALL getForMSE( nT, dT, nPer, dDelT, nDelT,
     &                mTreVar, nTreVar, mInvSigUT, nInvSigUT,
     &                mSigUTf, nSigUTf, mSigUTfUT, nSigUTfUT,
     &                mSigTf, nSigTf, mSigTfT, nSigTfT )
c     ------------------------------------------------------------------
c     Calculate covariance matrices for residuals of forecasts 
c     of future trend adjustment.
c     ------------------------------------------------------------------
      CALL getForMSE( nT, dT, nPer, dDelT, nDelT,
     &                mSeaVar, nSeaVar, mInvSigWT, nInvSigWT,
     &                mSigWTf, nSigWTf, mSigWTfWT, nSigWTfWT,
     &                mSigSAf, nSigSAf, mSigSAfSA, nSigSAfSA )
c     ------------------------------------------------------------------
c     Identify centered seasonal growth rate indexes 
c     for last month in data.
c     ------------------------------------------------------------------
      i1  = nPer/2
      i1m = nT-(nPer/2)
      i2  = nRevs+(nPer/2)
      i2m = nRevs-(nPer/2)
c     ------------------------------------------------------------------
c     Calculate centered seasonal growth rate SEs for the data.
c     ------------------------------------------------------------------
*      vTbl56(1,1) = DSQRT( mSigYf(i1,i1) * varSig )
*      vTbl56(1,2) = DSQRT( mSigYf(i1,i1) * varSig )
      vTbl56(1,1) = DSqrtOr0( mSigYf(i1,i1) ) * sdSig
      vTbl56(1,2) = vTbl56(1,1)
c     ------------------------------------------------------------------
c     Calculate centered seasonal growth rate SEs for Trend.
c     ------------------------------------------------------------------
      finMSE = mSigTf(i1,i1) + mTreVar(i1m,i1m)
     &       - 2.0d0*mSigTfT((i1m-1)*nPer + i1)
      infMSE = infMSEs(i2,2) + infMSEs(i2m,2)
     &       - 2.0d0*infMSE2s(i2m,2)
*      vTbl56(3,1) = DSQRT( (finMSE - infMSE) * varSig )
*      vTbl56(3,2) = DSQRT( finMSE * varSig )
      vTbl56(3,1) = DSqrtOr0( (finMSE - infMSE) ) * sdSig
      vTbl56(3,2) = DSqrtOr0( finMSE ) * sdSig
c     ------------------------------------------------------------------
c     Calculate centered seasonal growth rate SEs for Seasonal Adjustment.
c     ------------------------------------------------------------------
      finMSE = mSigSAf(i1,i1) + mSeaVar(i1m,i1m)
     &       - 2.0d0*mSigSAfSA((i1m-1)*nPer + i1)
      infMSE = infMSEs(i2,1) + infMSEs(i2m,1)
     &       - 2.0d0*infMSE2s(i2m,1)
*      vTbl56(5,1) = DSQRT( (finMSE - infMSE) * varSig )
*      vTbl56(5,2) = DSQRT( finMSE * varSig )
      vTbl56(5,1) = DSqrtOr0( (finMSE - infMSE) ) * sdSig
      vTbl56(5,2) = DSqrtOr0( finMSE ) * sdSig
c     ------------------------------------------------------------------
c     Identify centered seasonal growth rate indexes 
c     for next to last month in data.
c     ------------------------------------------------------------------
      i1  = i1-1
      i1m = i1m-1
      i2  = i2-1
      i2m = i2m-1
c     ------------------------------------------------------------------
c     Calculate centered seasonal growth rate SEs for the data.
c     ------------------------------------------------------------------
      vTbl56(2,1) = DSqrtOr0( mSigYf(i1,i1) ) * sdSig
      vTbl56(2,2) = vTbl56(2,1)
c     ------------------------------------------------------------------
c     Calculate centered seasonal growth rate SEs for Trend.
c     ------------------------------------------------------------------
      finMSE = mSigTf(i1,i1) + mTreVar(i1m,i1m)
     &       - 2.0d0*mSigTfT((i1m-1)*nPer + i1)
      infMSE = infMSEs(i2,2) + infMSEs(i2m,2)
     &       - 2.0d0*infMSE2s(i2m,2)
      vTbl56(4,1) = DSqrtOr0( (finMSE - infMSE) ) * sdSig
      vTbl56(4,2) = DSqrtOr0( finMSE ) * sdSig
c     ------------------------------------------------------------------
c     Calculate centered seasonal growth rate SEs for Seasonal Adjustment.
c     ------------------------------------------------------------------
      finMSE = mSigSAf(i1,i1) + mSeaVar(i1m,i1m)
     &       - 2.0d0*mSigSAfSA((i1m-1)*nPer + i1)
      infMSE = infMSEs(i2,1) + infMSEs(i2m,1)
     &       - 2.0d0*infMSE2s(i2m,1)
      vTbl56(6,1) = DSqrtOr0( (finMSE - infMSE) ) * sdSig
      vTbl56(6,2) = DSqrtOr0( finMSE ) * sdSig


c-----------------------------------------------------------------------
c     Calculate Table 5.7 Standard Errors using forcasted growth rates
c     MSEs.
c-----------------------------------------------------------------------
c     Identify one period ahead growth rate indexes 
c     from last month in data versus one period ahead.
c     ------------------------------------------------------------------
      i1  = 1
      i1m = nT
      i2  = nRevs+1
      i2m = nRevs
c     ------------------------------------------------------------------
c     Calculate growth rate SEs for the data.
c     ------------------------------------------------------------------
      vTbl57(1,1) = DSqrtOr0( mSigYf(i1,i1) ) * sdSig
c     ------------------------------------------------------------------
c     Calculate growth rate SEs for trend.
c     ------------------------------------------------------------------
      finMSE = mSigTf(i1,i1) + mTreVar(i1m,i1m)
     &       - 2.0d0*mSigTfT((i1m-1)*nPer + i1)
      infMSE = infMSEs(i2,2) + infMSEs(i2m,2)
     &       - 2.0d0*infMSE1s(i2m,2)
      vTbl57(1,2) = DSqrtOr0( (finMSE - infMSE) ) * sdSig
c     ------------------------------------------------------------------
c     Calculate growth rate SEs for seasonal adjustment.
c     ------------------------------------------------------------------
      finMSE = mSigSAf(i1,i1) + mSeaVar(i1m,i1m)
     &       - 2.0d0*mSigSAfSA((i1m-1)*nPer + i1)
      infMSE = infMSEs(i2,1) + infMSEs(i2m,1)
     &       - 2.0d0*infMSE1s(i2m,1)
      vTbl57(1,3) = DSqrtOr0( (finMSE - infMSE) ) * sdSig
c-----------------------------------------------------------------------
c     Identify one seasonal period ahead growth rate indexes 
c     from last month in data versus one seasonal period ahead.
c     ------------------------------------------------------------------
      i1  = nPer
      i1m = nT
      i2  = nRevs+nPer
      i2m = nRevs
c     ------------------------------------------------------------------
c     Calculate growth rate SEs for the data.
c     ------------------------------------------------------------------
      vTbl57(2,1) = DSqrtOr0( mSigYf(i1,i1) ) * sdSig
c     ------------------------------------------------------------------
c     Calculate growth rate SEs for trend.
c     ------------------------------------------------------------------
      finMSE = mSigTf(i1,i1) + mTreVar(i1m,i1m)
     &       - 2.0d0*mSigTfT((i1m-1)*nPer + i1)
      infMSE = infMSEs(i2,2) + infMSEs(i2m,2)
     &       - 2.0d0*infMSE2s(i2m,2)
      vTbl57(2,2) = DSqrtOr0( (finMSE - infMSE) ) * sdSig
c     ------------------------------------------------------------------
c     Calculate growth rate SEs for seasonal adjustment.
c     ------------------------------------------------------------------
      finMSE = mSigSAf(i1,i1) + mSeaVar(i1m,i1m)
     &       - 2.0d0*mSigSAfSA((i1m-1)*nPer + i1)
      infMSE = infMSEs(i2,1) + infMSEs(i2m,1)
     &       - 2.0d0*infMSE2s(i2m,1)
      vTbl57(2,3) = DSqrtOr0( (finMSE - infMSE) ) * sdSig
c-----------------------------------------------------------------------
c     Identify  growth rate indexes over current year
c     from last month in data versus last month in previous year.
c     ------------------------------------------------------------------
      i1  = nPer-tbl53Lag
      i1m = nT-tbl53Lag
      i2  = nRevs+nPer-tbl53Lag
      i2m = nRevs-tbl53Lag
c     ------------------------------------------------------------------
c     Calculate growth rate SEs for the data.
c     ------------------------------------------------------------------
      IF ( i1 .gt. 0 ) THEN
        vTbl57(3,1) = mSigYf(i1,i1)
      ELSE
        vTbl57(3,1) = 0.0d0
      END IF
      vTbl57(3,1) = DSqrtOr0( vTbl57(3,1) ) * sdSig
c     ------------------------------------------------------------------
c     Calculate growth rate SEs for trend.
c     ------------------------------------------------------------------
      IF ( i1 .gt. 0 ) THEN
        finMSE = mSigTf(i1,i1) + mTreVar(i1m,i1m)
     &         - 2.0d0*mSigTfT((i1m-1)*nPer + i1)
        infMSE = infMSEs(i2,2) + infMSEs(i2m,2)
     &         - 2.0d0*infMSE2s(i2m,2)
      ELSE
        finMSE = mTreVar(nT,nT) + mTreVar(i1m,i1m)
     &         - 2.0d0*mTreVar(nT,i1m)
        infMSE = infMSEs(i2,2) + infMSEs(i2m,2)
     &         - 2.0d0*infMSE2s(i2m,2)
      END IF
      vTbl57(3,2) = DSqrtOr0( (finMSE - infMSE) ) * sdSig
c     ------------------------------------------------------------------
c     Calculate growth rate SEs for seasonal adjustment.
c     ------------------------------------------------------------------
      IF ( i1 .gt. 0 ) THEN
        finMSE = mSigSAf(i1,i1) + mSeaVar(i1m,i1m)
     &         - 2.0d0*mSigSAfSA((i1m-1)*nPer + i1)
        infMSE = infMSEs(i2,1) + infMSEs(i2m,1)
     &         - 2.0d0*infMSE2s(i2m,1)
      ELSE
        finMSE = mSeaVar(nT,nT) + mSeaVar(i1m,i1m)
     &         - 2.0d0*mSeaVar(nT,i1m)
        infMSE = infMSEs(i2,1) + infMSEs(i2m,1)
     &         - 2.0d0*infMSE2s(i2m,1)
      END IF
      vTbl57(3,3) = DSqrtOr0( (finMSE - infMSE) ) * sdSig

c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE getTbl53Lag( Mq, iLag )
c-----------------------------------------------------------------------
c Release 1, Subroutine Version 1.0, Created 14 Apr 2006.
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 14 Apr 2006.
c-----------------------------------------------------------------------
c     This subroutine calculates the lag between the last month 
c     in the data versus the last month in the prior year of data.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c Mq      i   size of seasonal period
c iLag    i   output lag between last month in data and last month
c             in prior year
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c getLastPeriod i external function reference
c lastPer i   iLag value returned from getLastPeriod()
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INCLUDE 'sform.i'
      INTEGER iLag, Mq
      
c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER lastPer
      INTEGER getLastPeriod
      
c-----------------------------------------------------------------------
c     Retrieve iLag value via call to getLastPeriod().
c-----------------------------------------------------------------------
      lastPer = getLastPeriod(Nz,Nper,Nyer,Mq)
      iLag = lastPer
      
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE getForMSE( nT, d, nPer, dDel, nDel,
     &                      mCmpVar, nCmpVar, mInvSigU, nInvSigU,
     &                      mSigUf, nSigUf, mSigUfU, nSigUfU,
     &                      mSigCf, nSigCf, mSigCfC, nSigCfC)
c-----------------------------------------------------------------------
c Release 1, Subroutine Version 1.0, Created 14 Apr 2006.
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 14 Apr 2006.
c-----------------------------------------------------------------------
c     This subroutine calculates a components forecast covariance 
c     structure as calculated in theorem 2 of McElroy's paper
c     "Matrix Formulas for Nonstationary ARIMA Signal Extraction".
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c d       i   degree of differencing polynomial dDel
c dDel    d   diagonal form of differencing polynomial
c mCmpVar d   covariance matrix of estimated component
c mInvSigU d  inverse covariance matrix of differenced component (U)
c mSigCf  d   component covariance matrix of forecast residuals
c mSigCfC d   component covariance matrix between forecast residuals 
c             and earlier residuals
c mSigUf  d   covariance matrix of future differenced component (Uf)
c mSigUfU d   covariance matrix of (Uf,U)
c nCmpVar i   size (rows,columns) of mCmpVar matrix
c nDel    i   size (rows,columns) of differencing polynomial vector
c nInvSigU i  size (rows,columns) of mInvSigU matrix
c nPer    i   size of seasonal period
c nSigCf  i   size (rows,columns) of mSigCf matrix
c nSigCfC i   size (rows,columns) of mSigCfC matrix
c nSigUf  i   size (rows,columns) of mSigUf matrix
c nSigUfU i   size (rows,columns) of mSigUfU matrix
c nT      i   size of data available
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c i, j    i   do loop index variables
c iBaseD  i   base index associated with mD matrix processing
c mA      d   contains lower left partition of mInvDif matrix
c mB      d   contains lower right partition of mInvDif matrix
c mD      d   working matrix needed to calculate results
c mDif    d   difference matrix
c mG      d   working matrix needed to calculate results
c mInvDif d   inverse of difference matrix that contains mA and mB
c MONE    d   constant parameter
c mTemp1  d   temporary working matrix 1
c mTemp2  d   temporary working matrix 2
c nA      i   size (rows,columns) of mA matrix
c nB      i   size (rows,columns) of mB matrix
c nD      i   size (rows,columns) of mD matrix
c nDif    i   size (rows,columns) of mDif matrix
c nG      i   size (rows,columns) of mG matrix
c nInvDif i   size (rows,columns) of mInvDif matrix
c nTemp1  i   size (rows,columns) of mTemp1 matrix
c nTemp2  i   size (rows,columns) of mTemp2 matrix
c nSave   i   size of local large matrices that are saved
c ONE     d   constant parameter
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
      INTEGER d, nPer, nT
      INTEGER nCmpVar(2), nDel(2), nInvSigU(2)
      INTEGER nSigCf(2), nSigCfC(2), nSigUf(2), nSigUfU(2)
      DOUBLE PRECISION dDel(d+1)
      DOUBLE PRECISION mCmpVar(nT,nT), mInvSigU(nT-d,nT-d)
      DOUBLE PRECISION mSigCf(nPer,nPer), mSigCfC(nPer,nT)
*      DOUBLE PRECISION mSigUf(nPer,nPer), mSigUfU(nPer,nT)
      DOUBLE PRECISION mSigUf(nPer,nPer), mSigUfU(nPer,nT-d)
      
c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i, iBaseD, j
      INTEGER nSave
      INTEGER nA(2), nB(2), nD(2), nDif(2), nG(2), nInvDif(2)
      INTEGER nTemp1(2), nTemp2(2)
      PARAMETER (nSave=12*POBS)
      DOUBLE PRECISION MONE, ONE, ZERO
      DOUBLE PRECISION mA(nPer,d), mB(nPer,nPer), mD(nSave)
      DOUBLE PRECISION mG(nPer,nPer)
      DOUBLE PRECISION mDif(d+nPer,d+nPer), mInvDif(d+nPer,d+nPer)
      DOUBLE PRECISION mTemp1(nSave), mTemp2(nSave)
      PARAMETER (MONE=-1.0d0, ONE=1.0d0, ZERO=0.0d0)
      SAVE mD, mTemp1, mTemp2
      
c-----------------------------------------------------------------------
c     Initialize mDif matrix including size.
c-----------------------------------------------------------------------
      nDif(1) = d+nPer
      nDif(2) = d+nPer
c     ------------------------------------------------------------------
      DO j = 1, nDif(2)
       DO i = 1, nDif(1)
        mDif(i,j) = ZERO
       END DO
      END DO
c     ------------------------------------------------------------------
      DO i = 1, d
       mDif(i,i) = ONE
      END DO
      DO i = d+1, nDif(1)
       DO j = 1, d+1
        mDif(i,j+i-d-1) = dDel(j)
       END DO
      END DO

c-----------------------------------------------------------------------
c     Invert mDif matrix and extract mA and mB lower partitions.
c-----------------------------------------------------------------------
      CALL invLTMat( mDif, nDif, mInvDif, nInvDif )
      CALL getSMat( mInvDif, nInvDif, d+1, d+nPer, mB, nB )
      CALL getSRMat( mInvDif, nInvDif, d+1, d+nPer, 1, d, mA, nA )

c-----------------------------------------------------------------------
c     Calculate D filter that provides forecast of component
c     given past values of component.
c-----------------------------------------------------------------------
      CALL mulMat( mSigUfU, nSigUfU, mInvSigU, nInvSigU,
     &             mTemp1, nTemp1 )
      CALL mulMat( mB, nB, mTemp1, nTemp1, mTemp2, nTemp2 )
      pdA = max(nDel(2)-nDel(1)+1, 1)
      CALL mulMatD( mTemp2, nTemp2, dDel, nDel, mD, nD, pdA )
      DO j = 1, d
       iBaseD = (nT-d+j-1)*nPer
       DO i = 1, nPer
        mD(iBaseD+i) = mD(iBaseD+i) + mA(i,j)
       END DO
      END DO

c-----------------------------------------------------------------------
c     Calculate G matrix as specified in proof of theorem 2.
c-----------------------------------------------------------------------
      CALL mulMatTr( mTemp1, nTemp1, mSigUfU, nSigUfU, mTemp2, nTemp2 )
      CALL mulSca( MONE, mTemp2, nTemp2 )
      CALL addMat( mSigUf, nSigUf, mTemp2, nTemp2, mTemp1, nTemp1 )
      CALL mulQMat( mB, nB, mTemp1, nTemp1, mG, nG )

c-----------------------------------------------------------------------
c     Calculate residual covariance matrix for estimated (Cf,C) and then
c     Calculate residual covariance matrix for forecasted Cf.
c-----------------------------------------------------------------------
      CALL mulMat( mD, nD, mCmpVar, nCmpVar, mSigCfC, nSigCfC )
      CALL mulMatTr( mSigCfC, nSigCfC, mD, nD, mSigCf, nSigCf )
      CALL addMat( mSigCf, nSigCf, mG, nG, mSigCf, nSigCf )
      
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE getForYMSE( nT, d, nPer, dDel, nDel,
     &                       mInvSigW, nInvSigW, mSigWf, nSigWf,
     &                       mSigWfW, nSigWfW, mSigYf, nSigYf )
c-----------------------------------------------------------------------
c Release 1, Subroutine Version 1.0, Created 14 Apr 2006.
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 14 Apr 2006.
c-----------------------------------------------------------------------
c     This subroutine calculates a components forecast covariance 
c     structure as calculated in theorem 2 of McElroy's paper
c     "Matrix Formulas for Nonstationary ARIMA Signal Extraction".
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c d       i   degree of differencing polynomial dDel
c dDel    d   diagonal form of differencing polynomial
c mInvSigW d  inverse covariance matrix of differenced data (W)
c mSigWf  d   covariance matrix of future differenced data (Wf)
c mSigWfW d   covariance matrix of (Wf,W)
c mSigYf  d   data covariance matrix of forecast residuals
c nDel    i   size (rows,columns) of differencing polynomial vector
c nInvSigW i  size (rows,columns) of mInvSigW matrix
c nPer    i   size of seasonal period
c nSigWf  i   size (rows,columns) of mSigWf matrix
c nSigWfW i   size (rows,columns) of mSigWfW matrix
c nSigYf  i   size (rows,columns) of mSigYf matrix
c nT      i   size of data available
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c i, j    i   do loop index variables
c iBaseD  i   base index associated with mD matrix processing
c mB      d   contains lower right partition of mInvDif matrix
c mDif    d   difference matrix
c mInvDif d   inverse of difference matrix that contains mA and mB
c MONE    d   constant parameter
c mTemp1  d   temporary working matrix 1
c nB      i   size (rows,columns) of mB matrix
c nDif    i   size (rows,columns) of mDif matrix
c nInvDif i   size (rows,columns) of mInvDif matrix
c nTemp1  i   size (rows,columns) of mTemp1 matrix
c nSave   i   size of local large matrices that are saved
c ONE     d   constant parameter
c ZERO    d   constant parameter
c-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'srslen.prm'
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER d, nPer, nT
      INTEGER nDel(2), nInvSigW(2), nSigWf(2), nSigWfW(2), nSigYf(2)
      DOUBLE PRECISION dDel(d+1), mInvSigW(nT-d,nT-d)
*      DOUBLE PRECISION mSigWf(nPer,nPer), mSigWfW(nPer,nT)
      DOUBLE PRECISION mSigWf(nPer,nPer), mSigWfW(nPer,nT-d)
      DOUBLE PRECISION mSigYf(nPer,nPer)
      
c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i, iBaseD, j
      INTEGER nSave
      INTEGER nB(2), nDif(2), nInvDif(2)
      INTEGER nTemp1(2)
      PARAMETER (nSave=12*POBS)
      DOUBLE PRECISION MONE, ONE, ZERO
      DOUBLE PRECISION mB(nPer,nPer)
      DOUBLE PRECISION mDif(d+nPer,d+nPer), mInvDif(d+nPer,d+nPer)
      DOUBLE PRECISION mTemp1(nSave)
      PARAMETER (MONE=-1.0d0, ONE=1.0d0, ZERO=0.0d0)
      SAVE mTemp1
      
c-----------------------------------------------------------------------
c     Initialize mDif matrix including size.
c-----------------------------------------------------------------------
      nDif(1) = d+nPer
      nDif(2) = d+nPer
c     ------------------------------------------------------------------
      DO j = 1, nDif(2)
       DO i = 1, nDif(1)
        mDif(i,j) = ZERO
       END DO
      END DO
c     ------------------------------------------------------------------
      DO i = 1, d
       mDif(i,i) = ONE
      END DO
      DO i = d+1, nDif(1)
       DO j = 1, d+1
        mDif(i,j+i-d-1) = dDel(j)
       END DO
      END DO

c-----------------------------------------------------------------------
c     Invert mDif matrix and extract mB lower partition.
c-----------------------------------------------------------------------
      CALL invLTMat( mDif, nDif, mInvDif, nInvDif )
      CALL getSMat( mInvDif, nInvDif, d+1, d+nPer, mB, nB )

c-----------------------------------------------------------------------
c     Calculate residual covariance matrix for forecasted Yf.
c-----------------------------------------------------------------------
      CALL mulQMat( mSigWfW, nSigWfW, mInvSigW, nInvSigW,
     &              mTemp1, nTemp1 )
      CALL mulSca( MONE, mTemp1, nTemp1 )
      CALL addMat( mSigWf, nSigWf, mTemp1, nTemp1, mTemp1, nTemp1 )
      CALL mulQMat( mB, nB, mTemp1, nTemp1, mSigYf, nSigYf )
      
      RETURN
      END

c-----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION DSqrtOr0( Arg )
c-----------------------------------------------------------------------
c Release 1, Subroutine Version 1.0, Created 30 May 2006.
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 30 May 2006.
c-----------------------------------------------------------------------
c     This subroutine calculates the square root of a positive argument
c     or returns zero for negative arguments.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c Arg     d   input argument, should be positive in order to return
c             square root.
c-----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE precision Arg
      
c-----------------------------------------------------------------------
c     Return square root for positive argument,
c     else return zero for non-positive argument.
c-----------------------------------------------------------------------
      IF ( Arg .gt. 0.0d0 ) THEN
        DSqrtOr0 = DSQRT( arg )
      ELSE
        DSqrtOr0 = 0.0d0
      END IF
      
      RETURN
      END
      