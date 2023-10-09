      SUBROUTINE procFlts( dS, dT, nT, nPer, mDelS, nDelS,
     &                     mSAPFlt, nSAPFlt, mTrePFlt, nTrePFlt )
c-----------------------------------------------------------------------
c   procFlts.f, Release 1, Subroutine Version 1.2, Modified 20 Apr 2006.
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 30 Sep 2005.
c     Modified by REG, on 19 Jan 2006, to provide new description 
c       for fltLmbda, changed from frequencies to cycles/period.
c     Modified by REG, on 20 Apr 2006, to provide different phase-delay
c       adjustment for concurrent filters when w = 0; and to handle
c       SqGain, Phase, and Phase-Delay matrices of sizes (1201 x 2).
c     Modified by REG, on 05 Jun 2006, to output 
c       time-shift = -phase-delay, instead of phase-delay.
c-----------------------------------------------------------------------
c     This subroutine processes the input symmetric and concurrent
c     filters for the trend and seasonal adjustment. The processing
c     consisting of calculating the square-gain and phase-delay 
c     for each filter. For the symmetric filters, the square-gain and 
c     phase-delay is calculated using the full filters. For the 
c     concurrent filters, the square-gain and phase-delay of the full 
c     filters is found by finding the square-gain and phase-delay for 
c     the partial filters and then adjusting using the square-gain and 
c     constant phase-delay for the seasonal differencing polynomial.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c dS      i   size of Seasonal Differencing
c dT      i   size of Trend Differencing
c mDelS   d   seasonal differencing matrix
c mSAPFlt d   seasonal adjustment partial filters: 
c               column 1 = symmetric, column 2 = concurrent
c mTrePFlt d  trend partial filters: 
c               column 1 = symmetric, column 2 = concurrent
c nPer    i   size of seasonal period
c nDelS   i   size (rows,columns) of mDelS
c nSAPFlt i   size (rows,columns) of mSAPFlt
c nT      i   size of data available
c nTrePFlt i  size (rows,columns) of mTrePFlt
c-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'cmpflts.i'
      INTEGER nT, dS, dT, nPer, nDelS(2)
      INTEGER nSAPFlt(2), nTrePFlt(2)
      DOUBLE PRECISION mDelS(nT-dS,nT)
      DOUBLE PRECISION mSAPFlt(nT-dS,2), mTrePFlt(nT-dS,2)
      
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c fltLmbda d  vector (grid) of cycles/period set
c grid2   i   gride size (200) divided by 2
c i       i   index variable for for loops
c iT      i   temporary integer base variable
c mSAFFlt d   seasonal adjustment full filters: 
c               column 1 = symmetric, column 2 = concurrent
c mSAPhase d  SA filter phase values for each w in fltLmbda:
c               column 1 = symmetric, column 2 = concurrent
c mSAPhDly d  SA filter phase delay values for each w in fltLmbda:
c               column 1 = symmetric, column 2 = concurrent
c mSASqGain d SA filter square gain values for each w in fltLmbda:
c               column 1 = symmetric, column 2 = concurrent
c mTreFFlt d  trend full filters: 
c               column 1 = symmetric, column 2 = concurrent
c mTrePhase d trend filter phase values for each w in fltLmbda:
c               column 1 = symmetric, column 2 = concurrent
c mTrePhDly d trend filter phase delay values for each w in fltLmbda:
c               column 1 = symmetric, column 2 = concurrent
c mTreSqGain d trend filter square gain values for each w in fltLmbda:
c               column 1 = symmetric, column 2 = concurrent
c nSAFFlt i   size (rows,columns) of mSAFFlt matrix
c nTreFFlt i  size (rows,columns) of mTreFFlt matrix
c phAdj   d   phase delay adjustment for concurrent partial filters
c PI      d   constant parameter
c sqGainAdj   square gain adjustment for concurrent partial filters
c startPoint i  indicates negative of number of future values
c               for each filter (1: symmetric, 2: concurrent)
c TWO     d   constant parameter
c w       d   one frequency from the frequency set fltLmbda
c ZERO    d   constant parameter
c-----------------------------------------------------------------------
      INTEGER grid2, i, iT, startPoint(2)
      INTEGER nSAFFlt(2), nTreFFlt(2)
      PARAMETER (grid2=200/2)
      DOUBLE PRECISION mSAFFlt(nT,2), mTreFFlt(nT,2)
      DOUBLE PRECISION fltLmbda(0:grid2*nPer)
      DOUBLE PRECISION mTreSqGain(0:grid2*nPer,2),
     &                 mSASqGain(0:grid2*nPer,2),
     &                 mTrePhase(0:grid2*nPer,2),
     &                 mSAPhase(0:grid2*nPer,2),
     &                 mTrePhDly(0:grid2*nPer,2),
     &                 mSAPhDly(0:grid2*nPer,2)
      DOUBLE PRECISION phAdj, ONE, PI, sqGainAdj, TWO,  w, ZERO
      PARAMETER (ONE=1.0d0, PI=3.14159265358979d0, TWO=2.0d0, 
     &           ZERO=0.0d0)
c-----------------------------------------------------------------------
c     define function istrue - added by BCM 10-04-2006
c-----------------------------------------------------------------------
      LOGICAL istrue
      EXTERNAL istrue
c-----------------------------------------------------------------------
c     Debug variables.
c-----------------------------------------------------------------------
      CHARACTER*20 filename
      INTEGER ifail
c-----------------------------------------------------------------------
c     Initialize logical vectors for whether filters, phase delay are
c     computed to true - BCM 10-04-2006
c-----------------------------------------------------------------------
      do i = 1,2
       ltreFlt(i)=.true.
       ltreGain(i)=.true.
       ltreTmShf(i)=.true.
       lSAFlt(i)=.true.
       lSAGain(i)=.true.
       lSATmShf(i)=.true.
      end do
c-----------------------------------------------------------------------
c     Multiply by seasonal differencing to obtain full filters.
c-----------------------------------------------------------------------
      CALL mulTrMat( mDelS, nDelS, mSAPFlt, nSAPFlt,
     &               mSAFFlt, nSAFFlt )
      CALL mulTrMat( mDelS, nDelS, mTrePFlt, nTrePFlt,
     &               mTreFFlt, nTreFFlt )
c-----------------------------------------------------------------------
c     Calculate the square-gain for symmetric filters using full filters.
c       Phase delay is zero for symmetric filters.
c     Calculate the square-gain/phase delay for concurrent filters 
c       using partial filters and then adjusting below.
c-----------------------------------------------------------------------
      startPoint(1) = (nT+1)/2 - nT
      startPoint(2) = 0
c     ------------------------------------------------------------------
      IF(nSAFFlt(1).gt.0)THEN
       CALL phaseGain( mSAFFlt(1,1), nSAFFlt(1), startPoint(1), nPer,
     &                 fltLmbda, mSASqGain(0,1), mSAPhase(0,1),
     &                 mSAPhDly(0,1) )
      ELSE
c-----------------------------------------------------------------------
c     If dimension of seasonal adjustment filter is 0, set logical
c     values corresponding to filter diagnostics to false -
c     BCM 10-04-2006
c-----------------------------------------------------------------------
       lSAFlt(1)=.false.
       lSAGain(1)=.false.
       lSATmShf(1)=.false.
      END IF
      IF(nSAPFlt(1).gt.0)THEN
       CALL phaseGain( mSAPFlt(1,2), nSAPFlt(1), startPoint(2), nPer,
     &                 fltLmbda, mSASqGain(0,2), mSAPhase(0,2),
     &                 mSAPhDly(0,2) )
      ELSE
c-----------------------------------------------------------------------
c     If dimension of seasonal adjustment filter is 0, set logical
c     values corresponding to filter diagnostics to false -
c     BCM 10-04-2006
c-----------------------------------------------------------------------
       lSAFlt(2)=.false.
       lSAGain(2)=.false.
       lSATmShf(2)=.false.
      END IF
c     ------------------------------------------------------------------
      IF(nTreFFlt(1).gt.0)THEN
       CALL phaseGain( mTreFFlt(1,1), nTreFFlt(1), startPoint(1), nPer,
     &                 fltLmbda, mTreSqGain(0,1), mTrePhase(0,1),
     &                 mTrePhDly(0,1) )
      ELSE
c-----------------------------------------------------------------------
c     If dimension of trend filter is 0, set logical
c     values corresponding to filter diagnostics to false -
c     BCM 10-04-2006
c-----------------------------------------------------------------------
       lTreFlt(1)=.false.
       lTreGain(1)=.false.
       lTreTmShf(1)=.false.
      END IF
      IF(nTrePFlt(1).gt.0)THEN
       CALL phaseGain( mTrePFlt(1,2), nTrePFlt(1), startPoint(2), nPer,
     &                fltLmbda, mTreSqGain(0,2), mTrePhase(0,2),
     &                mTrePhDly(0,2) )
      ELSE
c-----------------------------------------------------------------------
c     If dimension of seasonal adjustment filter is 0, set logical
c     values corresponding to filter diagnostics to false -
c     BCM 10-04-2006
c-----------------------------------------------------------------------
       lTreFlt(2)=.false.
       lTreGain(2)=.false.
       lTreTmShf(2)=.false.
      END IF
c-----------------------------------------------------------------------
c   check to see if any of the filters, square-gain and phase-delay
c   data were generated.  If not, return
c     BCM 10-04-2006
c-----------------------------------------------------------------------
      concFltZ(1) = .false.
      concFltZ(2) = .false.
      IF(.not.(istrue(ltreFlt,1,2).or.istrue(ltreGain,1,2).or.
     &   istrue(lTreTmShf,1,2).or.istrue(lSAFlt,1,2).or.
     &   istrue(lSAGain,1,2).or.istrue(lSATmShf,1,2)))RETURN
c-----------------------------------------------------------------------
c     Process the partial filter data, square-gain, and phase-delay
c-----------------------------------------------------------------------
      DO i = 0, 200*((nPer+1)/2)
c-----------------------------------------------------------------------
c     Move filter frequency from local storage to common storage.
c-----------------------------------------------------------------------
        fltW(i) = fltLmbda(i)
c-----------------------------------------------------------------------
c     Adjust square-gain results by square-gain associated with mDelS
c     for concurrent filters.
c-----------------------------------------------------------------------
        w = fltW(i)*PI
        IF ( dS .gt. 0 ) THEN
          IF ( i .ne. 0 ) THEN
            sqGainAdj = DSIN( w )/DSIN( w/DBLE(nPer) )
            phAdj = DBLE(nPer-1)/TWO
          ELSE
            sqGainAdj = DBLE(nPer)
            phAdj = DBLE(nPer-1)/TWO
          END IF
        ELSE
          sqGainAdj = ONE
          phAdj = ZERO
        END IF
        sqGainAdj = sqGainAdj*sqGainAdj
        SAGain(i,1)  = mSASqGain(i,1)
        SAGain(i,2)  = sqGainAdj*mSASqGain(i,2)
        treGain(i,1) = mTreSqGain(i,1)
        treGain(i,2) = sqGainAdj*mTreSqGain(i,2)
        IF ( mSASqGain(i,1) .lt. 1.0d-10 ) THEN
          mSAPhDly(i,1) = ZERO
        END IF
        IF ( mSASqGain(i,2) .lt. 1.0d-10 ) THEN
          concFltZ(1) = .true.
          mSAPhDly(i,2) = ZERO
c         WRITE(6,200)'SA(i,2)',i,mSASqGain(i,2)
c 200     FORMAT( 1x, a, 1x, i4, 1x, g12.5 )
        END IF
        IF ( mTreSqGain(i,1) .lt. 1.0d-10 ) THEN
          mTrePhDly(i,1) = ZERO
        END IF
        IF ( mTreSqGain(i,2) .lt. 1.0d-10 ) THEN
          concFltZ(2) = .true.
          mTrePhDly(i,2) = ZERO
c         WRITE(6,200)'Tre(i,2)',i,mTreSqGain(i,2)
        END IF
c-----------------------------------------------------------------------
c     Adjust phase-delay results by phase-delay associated with mDelS
c     for concurrent filters, and output as time-shift = - phase-delay.
c-----------------------------------------------------------------------
        SATmShf(i,1)  = - mSAPhDly(i,1)
        SATmShf(i,2)  = - ( mSAPhDly(i,2)  + phAdj )
        treTmShf(i,1) = - mTrePhDly(i,1)
        treTmShf(i,2) = - ( mTrePhDly(i,2) + phAdj )
      END DO
c-----------------------------------------------------------------------
c     Move full filter data from local storage to common storage.
c-----------------------------------------------------------------------
      DO i = 1,nT
        SAFlt(i,1)  = mSAFFlt(nT-i+1,1)
        SAFlt(i,2)  = mSAFFlt(nT-i+1,2)
        treFlt(i,1) = mTreFFlt(nT-i+1,1)
        treFlt(i,2) = mTreFFlt(nT-i+1,2)
      END DO
      
c-----------------------------------------------------------------------
c     Some debug output.
c-----------------------------------------------------------------------
c     filename='filterData.txt'
c     CALL OPENDEVICE(filename,8,0,ifail)
c     IF (ifail .eq. 0) THEN
c      WRITE(8,300)nT
c 300  FORMAT(' Trend and SA Filters (symmetric and concurrent)',
c    &        ' for nT = ', i4 )
c      DO i=1,nT
c       WRITE(8,301)treFlt(i,1),treFlt(i,2),SAFlt(i,1),SAFlt(i,2)
c 301   FORMAT( 4( 1x, F12.7 ) )
c      END DO
c      WRITE(8,302)
c 302  FORMAT( /, ' Trend cycles/period (1200), gain (both)',
c    &            ' and phase delay (concurrent only)' )
c      DO i=1,1200
c       WRITE(8,301)fltW(i),treGain(i,1),treGain(i,2),trePhDly(i,2)
c      END DO
c      WRITE(8,303)
c 303  FORMAT( /, ' SA cycles/period (1200), gain (both)',
c    &            ' and phase delay (concurrent only)' )
c      DO i=1,1200
c       WRITE(8,301)fltW(i),SAGain(i,1),SAGain(i,2),SAPhDly(i,2)
c      END DO
c      CALL CLOSEDEVICE(8)
c     END IF
c     iT = nT/2 + 1
c     WRITE(6,101)(SAFlt(iT-30+i,1),i=1,60)
c     WRITE(6,101)(SAFlt(i,2),i=1,60)
c     WRITE(6,101)(treFlt(iT-30+i,1),i=1,60)
c     WRITE(6,101)(treFlt(i,2),i=1,60)
c 101 FORMAT(12(5(1x,G12.5),/))
c
c     WRITE(6,102)
c     WRITE(6,102)(fltW(i),SAGain(i,1),SAPhDly(i,1),i=0,1200,10)
c     WRITE(6,102)
c     WRITE(6,102)(fltW(i),SAGain(i,2),SAPhDly(i,2),i=0,1200,10)
c     WRITE(6,102)
c     WRITE(6,102)(fltW(i),treGain(i,1),trePhDly(i,1),i=0,1200,10)
c     WRITE(6,102)
c     WRITE(6,102)(fltW(i),treGain(i,2),trePhDly(i,2),i=0,1200,10)
c     WRITE(6,102)
c 102 FORMAT(121(3(1x,G12.5),/))

      RETURN
      END