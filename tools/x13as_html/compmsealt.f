	SUBROUTINE compMSEAlt( nT, dS, dT, mDel, nDel, mDelS, nDelS,
     &                       mDelT, nDelT, sIrrVar, sdSig,
     &                       mSigWS, nSigWS, mSigWT, nSigWT,
     &                       mInvSigW, nInvSigW, mIrrVar, nIrrVar,
     &                       mSeaVar, nSeaVar, mTreVar, nTreVar )
c-----------------------------------------------------------------------
c     compMSEAlt.f, Release 1, Subroutine Version 1.0, Created 20 Oct 2005.
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 20 Oct 2005.
c-----------------------------------------------------------------------
c	This subroutine calculates the signal extraction MSE matrices
c     relative to the innovation variance sdSig 
c     using an alternate set of equations than used by compMSE().
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c dS      i   size of Seasonal Differencing
c dT		  i   size of Trend Differencing
c mDel    d   overall differencing matrix
c mDelS   d   seasonal differencing matrix
c mDelT   d   trend differencing matrix
c mInvSigW d  inverse of mSigW: covariance matrix for differenced data
c mSigWS  d   covariance matrix for differenced trend adjusted
c mSigWT  d   covariance matrix for differenced seasonally adjusted
c mIrrVar d   variance matrix of estimated irregular
c mSeaVar d   variance matrix of estimated seasonal
c mTreVar d   variance matrix of estimated trend
c nDel    i   size (rows,columns) of mDel
c nDelS   i   size (rows,columns) of mDelS
c nDelT   i   size (rows,columns) of mDelT
c nInvSigW i  size (rows,columns) of mInvSigW matrix
c nIrrVar i   size (rows,columns) of mIrrVar matrix
c nSigWS  i   size (rows,columns) of mSigWS matrix
c nSigWT  i   size (rows,columns) of mSigWT matrix
c nSeaVar i   size (rows,columns) of mSeaVar matrix
c nTreVar i   size (rows,columns) of mTreVar matrix
c sdSig   d   data innovation stdev
c-----------------------------------------------------------------------
c Name   Type Description (Local Variables)
c-----------------------------------------------------------------------
c mFSISea d   seasonal extraction matrix
c mFTITre d   trend extraction matrix
c mFSTIIrr d  irregular extraction matrix
c mID     d   identity matrix
c mInvSigWS d inverse of mSigWS
c mInvSigWT d inverse of mSigWT
c mQuadWS d   result of quadratic matrix operation
c mQuadWT d   result of quadratic matrix operation
c mQuadW  d   result of quadratic matrix operation
c nSave   i   identifies default size of large matrices
c             that are saved (not dynamic)
c mTemp2  d   temporary matrix 2
c mTemp2A d   temporary matrix 2A
c nId     i   size (rows,columns) of mID matrix
c nFSISea i   size (rows,columns) of mFSISea matrix
c nFTITre i   size (rows,columns) of mFTITre matrix
c nFSTIrr i   size (rows,columns) of mFSTIrr matrix
c nTemp2  i   size (rows,columns) of mTemp2 matrix
c nTemp2A i   size (rows,columns) of mTemp2A matrix
c nInvSigWS i size (rows,columns) of mInvSigWS matrix
c nInvSigWT i size (rows,columns) of mInvSigWT matrix
c nQuadWS i   size (rows,columns) of nQuadWS matrix
c nQuadWT i   size (rows,columns) of nQuadWT matrix
c nQuadW  i   size (rows,columns) of nQuadW matrix
c-----------------------------------------------------------------------
	IMPLICIT NONE
c-----------------------------------------------------------------------
c	Declare Input/Output variables.
c-----------------------------------------------------------------------
	INCLUDE 'srslen.prm'
	INTEGER nT, dS, dT
	INTEGER nDel(2), nDelS(2), nDelT(2)
	INTEGER nSigWS(2), nSigWT(2)
	INTEGER nInvSigW(2)
	INTEGER nIrrVar(2), nSeaVar(2), nTreVar(2)
	DOUBLE PRECISION sIrrVar, sdSig
	DOUBLE PRECISION mDel(nT-dS-dT,nT), mDelS(nT-dS,nT), 
     &                 mDelT(nT-dT,nT)
	DOUBLE PRECISION mSigWS(nT-dS,nT-dS), mSigWT(nT-dT,nT-dT)
	DOUBLE PRECISION mInvSigW(nT-dS-dT,nT-dS-dT)
	DOUBLE PRECISION mIrrVar(nT,nT), mSeaVar(nT,nT), mTreVar(nT,nT)

c	------------------------------------------------------------------
c	Declare local variables.
c	------------------------------------------------------------------
	INTEGER nInvSigWS(2), nInvSigWT(2)
	INTEGER nQuadWS(2), nQuadWT(2), nQuadW(2), nId(2)
	INTEGER nFSISea(2), nFTITre(2), nFSTIIrr(2)
	INTEGER nTemp2(2), nTemp2A(2)

c	------------------------------------------------------------------
c	Dynamic (commented) versus static (uncommented) matrices
c	------------------------------------------------------------------
c	DOUBLE PRECISION mInvSigWS(nT-dS,nT-dS), mInvSigWT(nT-dT,nT-dT)
c	DOUBLE PRECISION mQuadWS(nT,nT), mQuadWT(nT,nT), mQuadW(nT,nT)
c	DOUBLE PRECISION mFSISea(nT,nT), mFTITre(nT,nT), mFSTIIrr(nT,nT)
c	DOUBLE PRECISION mTemp2(nT,nT), mTemp2A(nT,nT)
c     DOUBLE PRECISION mId(nT,nT)
c	------------------------------------------------------------------
	INTEGER nSave
	PARAMETER (nSave=POBS*POBS)
	DOUBLE PRECISION mInvSigWS(nSave), mInvSigWT(nSave)
	DOUBLE PRECISION mQuadWS(nSave), mQuadWT(nSave), mQuadW(nSave)
	DOUBLE PRECISION mFSISea(nSave), mFTITre(nSave), 
     &                 mFSTIIrr(nSave)
	DOUBLE PRECISION mTemp2(nSave), mTemp2A(nSave)
	DOUBLE PRECISION mId(nSave)
	SAVE mInvSigWS, mInvSigWT, mQuadWS, mQuadWT, mQuadW, mFSISea,
     &     mFTITre, mFSTIIrr, mTemp2, mTemp2A, mId

c-----------------------------------------------------------------------
c	Get the identity matrix (nTxnT).
c-----------------------------------------------------------------------
	CALL getIdM( nT, mId, nId )

c-----------------------------------------------------------------------
c	Calculate some matrix inverses.
c-----------------------------------------------------------------------
	CALL invMat( mSigWS, nSigWS, mInvSigWS, nInvSigWS )
	CALL invMat( mSigWT, nSigWT, mInvSigWT, nInvSigWT )

c-----------------------------------------------------------------------
c	Calculate some quadratic matrices.
c-----------------------------------------------------------------------
	CALL mulQMatTr( mDelS, nDelS, mInvSigWS, nInvSigWS, 
     &                mQuadWS, nQuadWS )
	CALL mulQMatTr( mDelT, nDelT, mInvSigWT, nInvSigWT,
     &                mQuadWT, nQuadWT )
	CALL mulQMatTr( mDel, nDel, mInvSigW, nInvSigW, mQuadW, nQuadW )

c-----------------------------------------------------------------------
c	Calculate some extraction matrices.
c-----------------------------------------------------------------------
	CALL cpyMat( mQuadW, nQuadW, mFSTIIrr, nFSTIIrr )
	CALL mulSca( sIrrVar, mFSTIIrr, nFSTIIrr )
c	------------------------------------------------------------------
	CALL cpyMat( mQuadWS, nQuadWS, mTemp2, nTemp2 )
	CALL mulSca( DBLE(-sIrrVar), mTemp2, nTemp2 )
	CALL addMat( mTemp2, nTemp2, mId, nId, mFSISea, nFSISea )
c	------------------------------------------------------------------
	CALL cpyMat( mQuadWT, nQuadWT, mTemp2, nTemp2 )
	CALL mulSca( DBLE(-sIrrVar), mTemp2, nTemp2 )
	CALL addMat( mTemp2, nTemp2, mId, nId, mFTITre, nFTITre )

c-----------------------------------------------------------------------
c	Calculate signal extraction MSE matrices relative to sdSig,
c     i.e. do not multiply by sdSig^2.
c-----------------------------------------------------------------------
	CALL cpyMat( mFSTIIrr, nFSTIIrr, mTemp2, nTemp2 )
	CALL mulSca( -1.0D0, mTemp2, nTemp2 )
	CALL addMat( mId, nId, mTemp2, nTemp2, mIrrVar, nIrrVar )
	CALL mulSca( DBLE(sIrrVar), mIrrVar, nIrrVar )
c	CALL mulSca( DBLE(sdSig*sdSig), mIrrVar, nIrrVar )
c	------------------------------------------------------------------
	CALL mulQMat( mFTITre, nFTITre, mFSISea, nFSISea, mTemp2,
     &              nTemp2 )
	CALL mulSca( -1.0D0, mTemp2, nTemp2 )
	CALL addMat( mFTITre, nFTITre, mTemp2, nTemp2, mTemp2A, nTemp2A )
	CALL invMat( mTemp2A, nTemp2A, mTemp2, nTemp2 )
	CALL mulMat( mTemp2, nTemp2, mFTITre, nFTITre, mTemp2A, nTemp2A )
	CALL mulMat( mTemp2A, nTemp2A, mFSISea, nFSISea,
     &             mSeaVar, nSeaVar )
	CALL mulSca( DBLE(sIrrVar), mSeaVar, nSeaVar )
c	CALL mulSca( DBLE(sdSig*sdSig), mSeaVar, nSeaVar )
c	------------------------------------------------------------------
	CALL mulQMat( mFSISea, nFSISea, mFTITre, nFTITre, mTemp2,
     &              nTemp2 )
	CALL mulSca( -1.0D0, mTemp2, nTemp2 )
	CALL addMat( mFSISea, nFSISea, mTemp2, nTemp2, mTemp2A, nTemp2A )
	CALL invMat( mTemp2A, nTemp2A, mTemp2, nTemp2 )
	CALL mulMat( mTemp2, nTemp2, mFSISea, nFSISea, mTemp2A, nTemp2A )
	CALL mulMat( mTemp2A, nTemp2A, mFTITre, nFTITre,
     &             mTreVar, nTreVar )
	CALL mulSca( DBLE(sIrrVar), mTreVar, nTreVar )
c	CALL mulSca( DBLE(sdSig*sdSig), mTreVar, nTreVar )

c-----------------------------------------------------------------------
	RETURN
	END
      