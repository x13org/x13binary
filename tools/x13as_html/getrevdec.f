      SUBROUTINE getRevDecomp( vPhiN, pn, vPhiS, ps, vThetaN, qn,
     &                         vThetaS, qs, vTheta, q, nPer, tbl53Lag,
     &                         sVarM, sVarN, sVarS, mMin, mMax,
     &                         vInfMSE, vInfMSE1, vInfMSE2, vInfMSE3,
     &                         pd1, pd2, pd3, pd4, pd5, pd6, pd7 )
c-----------------------------------------------------------------------
c     getRevDec.f, Release 1, Subroutine Version 1.0, Created 14 Nov 2005.
c-----------------------------------------------------------------------
c     This performs the Revision Decomposition in order to calculate
c     the desired MSEs using m in [mMin, mMax] past observations and 
c     infinite future observations, as described in McElroy and Findley 
c     paper "Model-Based Analysis of Signal Extraction Revision Errors".
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c vInfMSE d   Semi-infinite MSE based on m observations in the past
c               and infinite observations in the future.
c mMax    i   Maximum number of past observations in revision
c mMin    i   Minimum number of past observations in revision
c pn      i   Order of vPhiN AR polynomial for noise process
c ps      i   Order of vPhiS AR polynomial for signal process
c q       i   Order of vTheta MA polynomial for model process
c qn      i   Order of vThetaN MA polynomial for noise process
c qs      i   Order of vThetaS MA polynomial for signal process
c sVarM   d   Innovation variance for the model process
c sVarN   d   Innovation variance for the noise process
c sVarS   d   Innovation variance for the signal process
c vPhiN   d   Vector containing AR polynomial for noise process
c vPhiS   d   Vector containing AR polynomial for signal process
c vTheta  d   Vector containing MA polynomial for model process
c vThetaN d   Vector containing MA polynomial for noise process
c vThetaS d   Vector containing MA polynomial for signal process
c-----------------------------------------------------------------------
c Name   Type Description (Local Variables)
c-----------------------------------------------------------------------
c mIndex  i   Index variable used in do loop that ranges over 
c             [1,...,mMax-mMin+1].
c ishift  i   Used to calculate how much to shift vThetaS and vThetaN
c ONE     d   Parameter constant.
c sdum    i   Dummy variable used in call to cpyVec()
c sTP1    i   Size of vTP1 polynomial vector.
c sTP2    i   Size of vTP2 polynomial vector.
c vTP1    d   Temporary polynomial vector used to calculate new 
c             partial fraction decomposition for m+1 observation using 
c             existing partial fraction decomposition for m observations.
c vTP2    d   Temporary polynomial vector used like vTP1.
c ZERO    d   Parameter constant.
c-----------------------------------------------------------------------
c The following local variables are associated with developing the 
c partial fraction decomposition of the signal filter.
c-----------------------------------------------------------------------
c indS    i   indicator variable passed to GTWACF()
c sDmS0   d   First coefficient in vDmS0
c sDmS    i   Size of vDmS vector
c sDmSN   i   Size of vDmSN vector
c sHS     i   Size of vHS vector
c sNumP1S i   Size of vNumP1S vector
c sNumP2S i   Size of vNumP2S vector
c sPSMA   i   Size of vPSMA vector
c sVarPS  d   Innovation variance input to GTWACF()
c vCorS   d   Dummy argument for GTWACF()
c vCovS   d   Lag 0 autocovariance output by GTWACF()
c vDmS0R  d   Reciprocal of sDmS0
c vDmS    d   Combination polynomial: vKS x vThetaS + vGS
c vDmSN   d   Normalized vDmS
c vHS     d   One of the numerator polynomials output by getPFrac()
c vNumP1S d   vPhiN x vThetaS polynomial
c vNumP2S d   vThetaS x F^m polynomial
c vPSMA   d   MA polynomial input to GTWACF()
c-----------------------------------------------------------------------
c The following local variables are associated with developing the 
c partial fraction decomposition of the noise filter.
c-----------------------------------------------------------------------
c indN    i   indicator variable passed to GTWACF()
c sDmN0   d   First coefficient in vDmN0
c sDmN    i   Size of vDmN vector
c sDmNN   i   Size of vDmNN vector
c sHN     i   Size of vHN vector
c sNumP1N i   Size of vNumP1N vector
c sNumP2N i   Size of vNumP2N vector
c sPNMA   i   Size of vPNMA vector
c sVarPN  d   Innovation variance input to GTWACF()
c vCorN   d   Dummy argument for GTWACF()
c vCovN   d   Lag 0 autocovariance output by GTWACF()
c vDmNOR  d   Reciprocal of sDmN0
c vDmN    d   Combination polynomial: vKN x vThetaN + vGN
c vDmNN   d   Normalized vDmN
c vHN     d   One of the numerator polynomials output by getPFrac()
c vNumP1N d   vPhiS x vThetaN polynomial
c vNumP2N d   vThetaN x F^m polynomial input to getPFrac()
c vPNMA   d   MA polynomial input to GTWACF()
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER mMin, mMax, nPer, pn, ps, qn, qs, q, tbl53Lag
      DOUBLE PRECISION vPhiN(0:pn), vPhiS(0:ps), vThetaN(0:qn),
     &                 vThetaS(0:qs), vTheta(0:q)
      DOUBLE PRECISION sVarM, sVarN, sVarS
      DOUBLE PRECISION vInfMSE(72), vInfMSE1(72),
     &                 vInfMSE2(72), vInfMSE3(72)
c     ------------------------------------------------------------------
c     added by BCM to correctly dimension variables 
c     ------------------------------------------------------------------
      INTEGER pd1, pd2, pd3, pd4, pd5, pd6, pd7, pshift, psC, pp1, pp2,
     &        pp3, pp4, ppqa
c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER ishift, mIndex, sdum, sTP1, sTP2
      DOUBLE PRECISION ONE, ZERO, vTP1(pd1+1), vTP2(0:0)
      PARAMETER (ONE=1.0D0, ZERO=0.0D0)
      LOGICAL dpeq
c     ------------------------------------------------------------------
      INTEGER indS, sDmS, sDmSN, sHS,  sNumP1S, sNumP2S, sPSMA
      DOUBLE PRECISION vNumP1S(0:pn+qs), vNumP2S(0:mMin+qs)
      DOUBLE PRECISION vHS(0:pd2)
      DOUBLE PRECISION vDmS(0:pd3)
      DOUBLE PRECISION sDmS0, vDmS0R(0:0)
      DOUBLE PRECISION vDmSN(0:pd3)
      DOUBLE PRECISION vPSMA(0:pd3+qn), sVarPS
      DOUBLE PRECISION vCovS(0:nPer), vCorS(nPer)
c     ------------------------------------------------------------------
      INTEGER indN, sDmN, sDmNN, sHN, sNumP1N, sNumP2N, sPNMA
      DOUBLE PRECISION vNumP1N(0:ps+qn), vNumP2N(0:mMin+qn)
      DOUBLE PRECISION vHN(0:pd4)
      DOUBLE PRECISION vDmN(0:pd5)
      DOUBLE PRECISION sDmN0, vDmN0R(0:0)
      DOUBLE PRECISION vDmNN(0:pd5)
      DOUBLE PRECISION vPNMA(0:pd5+qs), sVarPN
      DOUBLE PRECISION vCovN(0:nPer), vCorN(nPer)
      
c     ------------------------------------------------------------------
c     Declare debug variables.
c     ------------------------------------------------------------------
      INTEGER maxI, nNum1(2), nNum2(2), nP1(2), nP2(2), sFP1, sFP2,
     &        sNum1, sNum2, sNum3, sP1, sP2, sRP1, sRP2, sRTheta
      DOUBLE PRECISION maxDif
      DOUBLE PRECISION vFP1(0:pn+qs), vFP2(0:ps+qn),
     &                 vRP1(0:pn+qs), vRP2(0:ps+qn)
      DOUBLE PRECISION vP1(0:2*pd6),
     &                 vP2(0:2*pd6)
      DOUBLE PRECISION vNum1(0:2*pd7),
     &                 vNum2(0:2*pd7),
     &                 vNum3(0:2*pd7)
      DOUBLE PRECISION vRTheta(0:q)

c-----------------------------------------------------------------------
c     Debug output and processing.
c-----------------------------------------------------------------------
c     WRITE(6,1000)pn,ps,qn,qs,q,m
c     WRITE(6,1000)
c     WRITE(6,1000)(vPhiN(i),i=0,pn)
c     WRITE(6,1000)
c     WRITE(6,1000)(vPhiS(i),i=0,ps)
c     WRITE(6,1000)
c     WRITE(6,1000)(vThetaN(i),i=0,qn)
c     WRITE(6,1000)
c     WRITE(6,1000)(vThetaS(i),i=0,qs)
c     WRITE(6,1000)
c     WRITE(6,1000)(vTheta(i),i=0,q)
c     WRITE(6,1000)
c     ------------------------------------------------------------------
c     Calculate numerator of the pseudo AGF for the ARIMA model.
c     ------------------------------------------------------------------
c     CALL polyRev( vTheta, q+1, vRTheta, sRTheta )
c     CALL CONV( vTheta, q+1, vRTheta, sRTheta, vNum1, sNum1 )
c     nNum1(1)=sNum1
c     nNum1(2)=1
c     CALL mulSca( sVarM, vNum1, nNum1 )
c     ------------------------------------------------------------------
c     Calculate numerator of the pseudo AGF for the S + N decomposition.
c     ------------------------------------------------------------------
c     CALL CONV( vPhiN, pn+1, vThetaS, qs+1, vFP1, sFP1 )
c     CALL polyRev( vFP1, sFP1, vRP1, sRP1 )
c     CALL CONV( vFP1, sFP1, vRP1, sRP1, vP1, sP1 )
c     nP1(1)=sP1
c     nP1(2)=1
c     CALL mulSca( sVarS, vP1, nP1 )
c     CALL CONV( vPhiS, ps+1, vThetaN, qn+1, vFP2, sFP2 )
c     CALL polyRev( vFP2, sFP2, vRP2, sRP2 )
c     CALL CONV( vFP2, sFP2, vRP2, sRP2, vP2, sP2 )
c     nP2(1)=sP2
c     nP2(2)=1
c     CALL mulSca( sVarN, vP2, nP2 )
c     IF ( sP1 .gt. sP2 ) THEN
c       CALL polyShft( vP2, sP2, (pn+qs)-(ps+qn), vP2, sP2 )
c     ELSE IF ( sP1 .lt. sP2 ) THEN
c       CALL polyShft( vP1, sP1, (ps+qn)-(pn+qs), vP1, sP1 )
c     END IF
c     CALL polyAdd( vP1, sP1, vP2, sP2, vNum2, sNum2 )
c     ------------------------------------------------------------------
c     Compare numerators of the two AGFs above. Should be nearly equal.
c     ------------------------------------------------------------------
c     IF ( sNum1 .gt. sNum2 ) THEN
c       CALL polyShft( vNum2, sNum2, q-max(pn+qs,ps+qn), vNum2, sNum2 )
c     ELSE IF (sNum1 .lt. sNum2 ) THEN
c       CALL polyShft( vNum1, sNum1, max(pn+qs,ps+qn)-q, vNum1, sNum1 )
c     END IF
c     nNum2(1) = sNum2
c     nNum2(2) = 1
c     CALL mulSca( -1.0d0, vNum2, nNum2 )
c     CALL polyAdd( vNum1, sNum1, vNum2, sNum2, vNum3, sNum3 )
c     maxDif = DABS( vNum3(0) )
c     maxI = 0
c     DO i=1,sNum3-1
c       IF ( DABS( vNum3(i) ) .gt. maxDif ) THEN
c         maxDif = DABS( vNum3(i) )
c         maxI = i
c       END IF
c     END DO
c     ------------------------------------------------------------------
c     Debug output
c     ------------------------------------------------------------------
c     WRITE(6,1000)(vP1(i),i=0,sP1-1)
c     WRITE(6,1000)
c     WRITE(6,1000)(vP2(i),i=0,sP2-1)
c     WRITE(6,1000)
c     WRITE(6,1000)(vNum2(i),i=0,sNum2-1)
c     WRITE(6,1000)
c     WRITE(6,1000)(vNum1(i),i=0,sNum1-1)
c     WRITE(6,1000)
c     WRITE(6,500)maxDif,maxI
c     WRITE(6,1000)
c 500 FORMAT(' MaxDif in Num is: ', G12.5, 1x, I4 )

c-----------------------------------------------------------------------
c     Calculate d(m) for signal.
c     First calculate the numerator polynomials in powers of F and B.
c-----------------------------------------------------------------------
      CALL CONV( vPhiN, pn+1, vThetaS, qs+1, vNumP1S, sNumP1S )
      pshift = max(qs+1+mMin,1)
      CALL polyShft( vThetaS, qs+1, mMin, vNumP2S, sNumP2S, pshift )

c     ------------------------------------------------------------------
c     Perform partial fraction decomposition to get the d(m) polynomial.
c     ------------------------------------------------------------------
      IF ( mMin .ge. (pn+qs) ) THEN
        pp1 = max(sNumP2S,ps+1,q)
        pp2 = max(sNumP1S,q+1)
        pp3 = max(q,ps+1)
        pp4 = max(sNumP2S-q,1)
        CALL getPFrac( vNumP2S, sNumP2S, vNumP1S, sNumP1S, 
     &                 vPhiS, ps+1, vTheta, q+1,
     &                 vDmS, sDmS, vHS, sHS, pp1, pp2, pp3, pp4 )
      ELSE
        pp1 = max(sNumP2S,ps+1,q)
        pp2 = max(sNumP1S,q+1)
        pp3 = max(sNumP2S,ps+1)
        CALL getSPFrac( vNumP2S, sNumP2S, vNumP1S, sNumP1S, 
     &                  vPhiS, ps+1, vTheta, q+1,
     &                  vDmS, sDmS, vHS, sHS, pp1, pp2, pp3 )
      END IF

c-----------------------------------------------------------------------
c     Debug output
c-----------------------------------------------------------------------
c     WRITE(6,1000)(vHS(i),i=0,sHS-1)
c     WRITE(6,1000)
c     WRITE(6,1000)(vDmS(i),i=0,sDmS-1)
c     WRITE(6,1000)

c-----------------------------------------------------------------------
c     Calculate d(m) for noise.
c     First calculate the numerator polynomials in powers of F and B.
c-----------------------------------------------------------------------
      CALL CONV( vPhiS, ps+1, vThetaN, qn+1, vNumP1N, sNumP1N )
      pshift = max(qn+1+mMin,1)
      CALL polyShft( vThetaN, qn+1, mMin, vNumP2N, sNumP2N, pshift )

c     ------------------------------------------------------------------
c     Perform partial fraction decomposition to get the d(m) polynomial.
c     ------------------------------------------------------------------
      IF ( mMin .ge. (ps+qn) ) THEN
        pp1 = max(sNumP2N,pn+1,q)
        pp2 = max(sNumP1N,q+1)
        pp3 = max(q,pn+1)
        pp4 = max(sNumP2N-q,1)
        CALL getPFrac( vNumP2N, sNumP2N, vNumP1N, sNumP1N,
     &                 vPhiN, pn+1, vTheta, q+1,
     &                 vDmN, sDmN, vHN, sHN, pp1, pp2, pp3, pp4 )
      ELSE
        pp1 = max(sNumP2N,pn+1,q)
        pp2 = max(sNumP1N,q+1)
        pp3 = max(sNumP2N,pn+1)
        CALL getSPFrac( vNumP2N, sNumP2N, vNumP1N, sNumP1N,
     &                  vPhiN, pn+1, vTheta, q+1,
     &                  vDmN, sDmN, vHN, sHN, pp1, pp2, pp3 )
      END IF

c-----------------------------------------------------------------------
c     Debug output
c-----------------------------------------------------------------------
c     WRITE(6,1000)(vHN(i),i=0,sHN-1)
c     WRITE(6,1000)
c     WRITE(6,1000)(vDmN(i),i=0,sDmN-1)
c     WRITE(6,1000)

c-----------------------------------------------------------------------
c     No need to normalize the d(m) for signal and noise since the 
c     GTWACF() routine can handle this and then calculate the innovation 
c     variances for the two processes that together provide the MSE. 
c     Note that (VarS, VarN) are already calulated relative to VarM
c     and (VarPS, VarPN) are calculated relative to VarM so that 
c     (CovS, CovN) are calculated by GTWACF() relative to VarM.
c-----------------------------------------------------------------------
      DO mIndex=1, mMax-mMin+1
        IF ( .not. dpeq(vDmS(0),ZERO) ) THEN
          sDmS0 = vDmS(0)
        ELSE
          sDmS0 = ONE
        END IF
        vDmS0R(0) = DBLE(ONE/sDmS0)
        CALL CONV( vDmS0R, 1, vDmS, sDmS, vDmSN, sDmSN )
        CALL CONV( vDmSN, sDmSN, vThetaN, qn+1, vPSMA, sPSMA )
c       sVarPS = ((sDmS0*sVarS/sVarM)**2)*sVarN
        sVarPS = ((sDmS0*sVarS)**2)*sVarN
        ppqa = max( q, sPSMA-1, nPer+1 )
        pp1 = max( q, 1 )
        CALL GTWACF( q, sPSMA-1, nPer+1, vTheta, vPSMA, sVarPS,
     &               vCovS, vCorS, indS, ppqa, pp1 )
c     ------------------------------------------------------------------
        IF ( .not. dpeq(vDmN(0),ZERO) ) THEN
          sDmN0 = vDmN(0)
        ELSE
          sDmN0 = ONE
        END IF
        vDmN0R(0) = DBLE(ONE/sDmN0)
        CALL CONV( vDmN0R, 1, vDmN, sDmN, vDmNN, sDmNN )
        CALL CONV( vDmNN, sDmNN, vThetaS, qs+1, vPNMA, sPNMA )
c       sVarPN = ((sDmN0*sVarN/sVarM)**2)*sVarS
        sVarPN = ((sDmN0*sVarN)**2)*sVarS
        ppqa = max( q, sPNMA-1, nPer+1 )
        pp1 = max( q, 1 )
        CALL GTWACF( q, sPNMA-1, nPer+1, vTheta, vPNMA, sVarPN,
     &               vCovN, vCorN, indN, ppqa, pp1 )

c-----------------------------------------------------------------------
c     Calculate the MSE relative to VarM.
c-----------------------------------------------------------------------
        IF (( indS .eq. 0 ) .and. ( indN .eq. 0 )) THEN
c         sInfMSE = (vCovS(0)+vCovN(0))/sVarM
          vInfMSE(mIndex)  = (vCovS(0)+vCovN(0))
          vInfMSE1(mIndex) = (vCovS(1)+vCovN(1))
          vInfMSE2(mIndex) = (vCovS(nPer)+vCovN(nPer))
          vInfMSE3(mIndex) = (vCovS(tbl53Lag)+vCovN(tbl53Lag))
        ELSE
          vInfMSE(mIndex)  = ZERO
          vInfMSE1(mIndex) = ZERO
          vInfMSE2(mIndex) = ZERO
          vInfMSE3(mIndex) = ZERO
        END IF
      
c-----------------------------------------------------------------------
c     Debug output
c-----------------------------------------------------------------------
c       WRITE(6,1000)DBLE(q),DBLE(sPSMA-1),sVarPS
c       WRITE(6,1000)sDmS0,sVarN,sVarS,sVarM
c       WRITE(6,1000)sDmN0,sVarN,sVarS,sVarM
c       WRITE(6,1000)DBLE(q),DBLE(sPNMA-1),sVarPN
c       WRITE(6,1000) sVarM, sVarS, sVarN
c       WRITE(6,1000) vCovS(0), vCovN(0), DBLE(indS), DBLE(indN), sInfMse
c1000   FORMAT( 100(6(G12.5,1x),/) )

c-----------------------------------------------------------------------
c     Calculate partial fraction decomposition that gives d(m+1) 
c     polynomial from partial fraction decomposition that gave 
c     d(m) polynomial.
c-----------------------------------------------------------------------
        IF ( mIndex .lt. mMax-mMin+1 ) THEN
          pshift = max(1+sDmS,1)
          CALL polyShft( vDmS, sDmS, 1, vDmS, sDmS, pshift )
          vTP2(0) = vHS(1)
          sTP2=1
          CALL CONV( vTP2, sTP2, vPhiS, ps+1, vTP1, sTP1 )
          psC = max(sDmS,sTP1)
          CALL polyAdd( vDmS, sDmS, vTP1, sTP1, vDmS, sDmS, psC )
c     ------------------------------------------------------------------
          pshift = max(sHS-1,1)
          CALL polyShft( vHS, sHS, -1, vHS, sHS, pshift )
          vTP2(0) = -vTP2(0)
          CALL CONV( vTP2, sTP2, vTheta, q+1, vTP1, sTP1 )
          psC = max(sHS,sTP1)
          CALL polyAdd( vHS, sHS, vTP1, sTP1, vHS, sHS, psC )
c     ------------------------------------------------------------------
          pshift = max(1+sDmN,1)
          CALL polyShft( vDmN, sDmN, 1, vDmN, sDmN, pshift )
          vTP2(0) = vHN(1)
c         sTP2=1
          CALL CONV( vTP2, sTP2, vPhiN, pn+1, vTP1, sTP1 )
          psC = max(sDmN,sTP1)
          CALL polyAdd( vDmN, sDmN, vTP1, sTP1, vDmN, sDmN, psC )
c     ------------------------------------------------------------------
          pshift = max(sHN-1,1)
          CALL polyShft( vHN, sHN, -1, vHN, sHN, pshift )
          vTP2(0) = -vTP2(0)
          CALL CONV( vTP2, sTP2, vTheta, q+1, vTP1, sTP1 )
          psC = max(sHN,sTP1)
          CALL polyAdd( vHN, sHN, vTP1, sTP1, vHN, sHN, psC )
        END IF
      END DO
      
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE getPFrac( vA1, sA1, vA2, sA2, vB1, sB1, vB2, sB2,
     &                     vCD, sCD, vE, sE, pp1, pp2, pp3, pp4 )
c-----------------------------------------------------------------------
c     This subroutine performs a partial fraction decomposition of the
c     rational function represented by the numerator polynomial A divided
c     by the denominator polynomial B where the denominator polynomial B
c     consists of the product of two subpolynomials B1(F) and B2(B)
c     with subpolynomial B2 is invertible and where the numerator 
c     polynomial A consists of the product of two subpolynomials 
c     A1(F) and A2(B).
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c sA1     i   Size of numerator product term 1 polynomial
c sA2     i   Size of numerator product term 2 polynomial
c sB1     i   Size of denominator product term 1 polynomial
c sB2     i   Size of denominator product term 2 polynomial
c sCD     i   Size of numerator polynomial for first partial fraction
c sE      i   Size of numerator polynomial for second partial fraction
c vA      d   Vector of numerator polynomial
c vB1     d   Vector of denominator polynomial product term 1
c vB2     d   Vector of denominator polynomial product term 2
c vCD     d   Vector of numerator polynomial for first partial fraction
c vE      d   Vector of numerator polynomial for second partial fraction
c-----------------------------------------------------------------------
c Name   Type Description (Local Variables)
c-----------------------------------------------------------------------
c i,j     i   Index variables for do loops
c jindex  i   Index used to navigate mS matrix
c k
c ONE     d   Parameter
c sA      i   Size of sA polynomial.
c sA12    i   Size of vA12 polynomial.
c sD      i   Size of vD polynomial.
c sdum    i   Dummy size variable.
c sQuot   i   Size of vQuot polynomial.
c sRA2    i   Size of vRA2 polynomial.
c sRB2    i   Size of vRB2 polynomial.
c sRem    i   Size of vRem polynomial.
c s1      i   Size of v1 polynomial.
c vA      d   Shifted version of A12 that accounts for no powers of B.
c vA12    d   Polynomial product of A1 and A2 polynomials.
c vD      d   Partial fraction numerator returned by getSPFrac().
c vQuot   d   Quotient polynomial returned by polyQuot().
c vRA2    d   Reversed version of vA2 polynomial.
c vRB2    d   Reversed version of vB2 polynomial.
c vRem    d   Vector of remainder polynomial from polynomial division 
c               for A/B.
c v1      d   Order 0 polynomial equal to 1.
c ZERO    d   Parameter
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
c     BCM added input variables pp1, pp2, pp3, pp4 to eliminate max
c     statements in dimension arguments that were causing Unix to bomb,
c     and added local variable ppp1, ppp2, and ppp3 to dimension
c     variables in getSPFrac
c-----------------------------------------------------------------------
      INTEGER sA1, sA2, sB1, sB2, sCD, sE
      INTEGER pp1, pp2, pp3, pp4, ppp1, ppp2, ppp3
      DOUBLE PRECISION vA1(sA1), vA2(sA2), vB1(sB1), vB2(sB2),
     &                 vCD(pp1), vE(pp2)
      INTEGER pshift, psC
c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i, j, jindex, k, sA, sA12, sD, sdum, sQuot, sRA2, sRB2,
     &        sRem, s1
      DOUBLE PRECISION vA(sA1), vA12(sA1+sA2-1), vD(pp3), 
     &                 vQuot(pp4), vRA2(sA2),
     &                 vRB2(sB2), vRem(sB2-1), v1(1)
      DOUBLE PRECISION ONE, ZERO
      PARAMETER (ONE=1.0D0, ZERO=0.0D0)

c     ------------------------------------------------------------------
c     Declare debug variables.
c     ------------------------------------------------------------------
      INTEGER maxI, sP1, sP2, sP3, sP4, sP5, sRE
      DOUBLE PRECISION vP1(2*(sA1+sA2-1)), vP2(2*(sA1+sA2-1)),
     &                 vP3(2*(sA1+sA2-1)), vRE(pp2)
      DOUBLE PRECISION maxDif, newDif

c-----------------------------------------------------------------------
c     Combine the A1 and A2 polynomials.
c-----------------------------------------------------------------------
      CALL polyRev( vA2, sA2, vRA2, sRA2 )
      CALL CONV( vA1, sA1, vRA2, sRA2, vA12, sA12 )
      CALL cpyVec( vA12(sA2), sA1, vA, sA )

c-----------------------------------------------------------------------
c     Reverse the B2 subpolynomial
c-----------------------------------------------------------------------
      CALL polyRev( vB2, sB2, vRB2, sRB2 )
      
c-----------------------------------------------------------------------
c     Divide polynomial A by polynomial B,
c     resulting in a quotient and remainder polynomials.
c-----------------------------------------------------------------------
      IF ( sRB2 .le. sA ) THEN
        CALL polyQuot( vA, sA, vRB2, sRB2, vQuot, sQuot, vRem, sRem )
      ELSE
        sQuot = 1
        vQuot(1) = ZERO
        CALL cpyVec( vQuot(1), max(sA-sB2,0), vQuot(2), sdum )
        CALL cpyVec( vA, sA, vRem, sRem )
      END IF
c-----------------------------------------------------------------------
c     Perform the small partial fraction decomposition.
c-----------------------------------------------------------------------
      s1 = 1
      v1(1) = ONE
      ppp1 = max(sRem,sB1,sB2-1)
      ppp2 = max(s1,sB2)
      ppp3 = max(sRem,sB1)
      CALL getSPFrac( vRem, sRem, v1, s1, vB1, sB1, vB2, sB2,
     &                vD, sD, vE, sE, ppp1, ppp2, ppp3 )

c-----------------------------------------------------------------------
c     Combine the Quot and D polynomials.
c-----------------------------------------------------------------------
      pshift = max(sQuot+sB2-1,1)
      CALL polyShft( vQuot, sQuot, sB2-1, vCD, sCD, pshift )
      psC = max(sCD,sD)
      CALL polyAdd( vCD, sCD, vD, sD, vCD, sCD, psC )

c-----------------------------------------------------------------------
c     Debug code.
c-----------------------------------------------------------------------
c     CALL CONV( vCD, sCD, vRB2, sRB2, vP1, sP1 )
c     CALL polyRev( vE, sE, vRE, sRE )
c     CALL CONV( vRE, sRE, vB1, sB1, vP2, sP2 )
c     CALL polyAdd( vP1(sB2), sP1-sB2+1, vP2(sE), sP2-sE+1, vP3, sP3 )
c     maxDif = DABS( vA(1)-vP3(1) )
c     maxI = 1
c     DO i = 2, sA
c       newDif = DABS( vA(i)-vP3(i) )
c       IF ( newDif .gt. maxDif ) THEN
c         maxDif = newDif
c         maxI = i
c       END IF
c     END DO
c     WRITE(6,1001)maxDif, maxI
c     WRITE(6,1000)(vQuot(i),i=1,sQuot)
c     WRITE(6,1000)
c     WRITE(6,1000)(vRem(i),i=1,sRem)
c     WRITE(6,1000)
c     WRITE(6,1000)(vD(i),i=1,sD)
c     WRITE(6,1000)
c     WRITE(6,1000)(vE(i),i=1,sE)
c     WRITE(6,1000)
c     WRITE(6,1000)(vA(i),i=1,sA)
c     WRITE(6,1000)
c     WRITE(6,1000)(vP1(sB2+i-1),i=1,sA)
c     WRITE(6,1000)
c     WRITE(6,1000)(vP2(sE+i-1),i=1,sA)
c     WRITE(6,1000)
c     WRITE(6,1000)(vP3(i),i=1,sP3)
c     WRITE(6,1000)
c1000 FORMAT( 100(6(G12.5,1x),/) )
c1001 FORMAT( ' getPfrac maxDif: ', G12.5, 1x, I4 )

      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE polyQuot( vA, sA, vB, sB, vQuot, sQuot, vRem, sRem )
c-----------------------------------------------------------------------
c     polyQuot, Release 1, Subroutine Version 1.0, Created 14 Nov 2005.
c-----------------------------------------------------------------------
c     This subroutine performs polynomial division of vA by vB
c     resulting in a quotient vQuot and a remainder vRem.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c sA      i   Size of numerator polynomial vector
c sB      i   Size of denominator polynomial vector
c sQuot   i   Size of quotient polynomial vector
c sRem    i   Size of remainder polynomial vector
c vA      d   Vector of numerator polynomial
c vB      d   Vector of denominator polynomial
c vQuot   d   Vector of quotient polynomial
c vRem    d   Vector of remainder polynomial
c-----------------------------------------------------------------------
c Name   Type Description (Local Variables)
c-----------------------------------------------------------------------
c i,j     i   Index variables for do loops
c ONE     d   Parameter
c sAN     i   Size of vAN polynomial
c sBN     i   Size of vBN polynomial
c sdum    i   Dummy size variable
c sW      i   Size of vW polynomial
c v1      d   Normalizing constant equal to vB(1)
c v2      d   Normalizing constant equal to 1/vB(1)
c v3      d   Constant used to facilitate polynomial subtraction
c vAN     d   vA normalized by vB(1).
c vBN     d   vB normalized by vB(1).
c vW      d   Working polynomial used to determine vQuot and vRem.
c ZERO    d   Parameter
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER sA, sB, sQuot, sRem
      DOUBLE PRECISION vA(sA), vB(sB), vQuot(*), vRem(*)
      
c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i, j, sdum, sAN, sBN, sW, psC
      DOUBLE PRECISION v1(1), v2(1), v3(1), vAN(sA), vBN(sB), vW(sA)
      DOUBLE PRECISION ONE, ZERO
      PARAMETER (ONE=1.0D0, ZERO=0.0D0)
      LOGICAL dpeq
      
c     ------------------------------------------------------------------
c     Declare debug variables.
c     ------------------------------------------------------------------
      INTEGER sP1, sP2
      DOUBLE PRECISION vP1(sA), vP2(sA)

c-----------------------------------------------------------------------
c     Normalize the vA and vB polynomials per vB(1)
c-----------------------------------------------------------------------
      v1(1) = vB(1)
      v2(1) = ONE/v1(1)
      CALL CONV( v2, 1, vA, sA, vAN, sAN )
      CALL CONV( v2, 1, vB, sB, vBN, sBN )

c-----------------------------------------------------------------------
c     If the size of the numerator vA is greater or equal
c     to the size of the denominator vB then
c-----------------------------------------------------------------------
      IF ( sA .ge. sB ) THEN
        sQuot = sA-sB+1
        CALL cpyVec( vAN, sAN, vW, sW )
        vQuot(1)=ZERO
        CALL cpyVec( vQuot(1), sQuot-1, vQuot(2), sdum )
c-----------------------------------------------------------------------
c     Calculate the quotient coefficients by using the leading 
c     coefficients in the denominator and the working polynomials 
c     and adjust the working polynomial by subtracting the denomiator
c     polynomial as weighted by the quotient. The end result in the
c     working polynomial is the remainder.
c-----------------------------------------------------------------------
        v3(1) = -ONE
        DO i = sQuot, 1, -1
          vQuot(i) = vW( sW-(sQuot-i) ) / vBN( sB )
c     ------------------------------------------------------------------
c         DO j = 0, sBN-1
c           vW( sW-j ) = vW( sW-j ) - vBN( sBN-j )*vQuot(i)
c         END DO
c         sW = sW-1
c     ------------------------------------------------------------------
          CALL CONV( vQuot, sQuot, vBN, sBN, vW, sW )
          CALL CONV( v3, 1, vW, sW, vW, sW )
          psC = max(sAN,sW)
          CALL polyAdd( vAN, sAN, vW, sW, vW, sW, psC )
        END DO
        CALL CONV( v1, 1, vW, sW, vW, sW )
        CALL cpyVec( vW, sW-sQuot, vRem, sRem )

c-----------------------------------------------------------------------
c     Else the size of the numerator vA is less than
c     the size of the denominator vB then
c-----------------------------------------------------------------------
      ELSE
        sQuot = 1
        vQuot(1) = ZERO
        CALL cpyVec( vQuot(1), sB-1, vQuot(2), sdum )
        CALL cpyVec( vA, sA, vRem, sRem )
      END IF
      
c-----------------------------------------------------------------------
c     Debug code.
c-----------------------------------------------------------------------
c     CALL CONV( vB, sB, vQuot, sQuot, vP1, sP1 )
c     CALL polyAdd( vRem, sRem, vP1, sP1, vP2, sP2 )
c     v3(1) = -ONE
c     CALL CONV( v3, 1, vP1, sP1, vP1, sP1 )
c     CALL polyAdd( vA, sA, vP1, sP1, vP1, sP1 )
c     WRITE(6,1000)(vA(i),i=1,sA)
c     WRITE(6,1000)
c     WRITE(6,1000)(vB(i),i=1,sB)
c     WRITE(6,1000)
c     WRITE(6,1000)(vQuot(i),i=1,sQuot)
c     WRITE(6,1000)
c     WRITE(6,1000)(vRem(i),i=1,sRem)
c     WRITE(6,1000)
c     WRITE(6,1000)(vP2(i),i=1,sP2)
c     WRITE(6,1000)
c     WRITE(6,1000)(vP1(i),i=1,sP1)
c     WRITE(6,1000)
c1000 FORMAT( 100(6(G12.5,1x),/) )

      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE cpyVec( vA, sA, vB, sB )
c-----------------------------------------------------------------------
c     cpyVec, Release 1, Subroutine Version 1.0, Created 14 Nov 2005.
c-----------------------------------------------------------------------
c     This subroutine copies a vector from vA to vB of size sA.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c sA      i   Size of the vA source vector
c sB      i   Size of the vB destination vector
c vA      d   Source vector of coefficients
c vB      d   Destination vector of coefficients
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c i       i   index variables for do loops
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER sA, sB
      DOUBLE PRECISION vA(*), vB(*)

c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i
      
c-----------------------------------------------------------------------
c     Establish the size of output vector.
c-----------------------------------------------------------------------
      sB = sA

c-----------------------------------------------------------------------
c     Copy the vector vA to vB,
c-----------------------------------------------------------------------
      DO i = 1, sA
        vB(i) = vA(i)
      END DO
      
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE polyRev( vA, sA, vB, sB )
c-----------------------------------------------------------------------
c     This subroutine reverses the order of the coefficients 
c     in a polynomial.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c sA      i   Size of input polynomial vA.
c sB      i   Size of output polynomial vB.
c vA      d   Input polynomial to be reversed.
c vB      d   Output polynomial after reversing vA.
c-----------------------------------------------------------------------
c Name   Type Description (Local Variables)
c-----------------------------------------------------------------------
c i       i   Index variable used in doo loops.
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER sA, sB
      DOUBLE PRECISION vA(sA), vB(sA)
c-----------------------------------------------------------------------
c     Declare Local variables.
c-----------------------------------------------------------------------
      INTEGER i
      
c-----------------------------------------------------------------------
c     Establish size of output polynomial vB.
c-----------------------------------------------------------------------
      sB = sA
c-----------------------------------------------------------------------
c    Move vA to vB in reverse order.
c-----------------------------------------------------------------------
      DO i = 1, sB
       vB( sB+1-i ) = vA(i)
      END DO
      
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE polyAdd( vA, sA, vB, sB, vC, sC, psC )
c-----------------------------------------------------------------------
c     This subroutine adds to polynomials together.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c sA      i   Size of vA polynomial.
c sB      i   Size of vB polynomial.
c sC      i   Size of vB polynomial.
c vA      d   First input polynomial to add together.
c vB      d   Second input polynomial to add together.
c vC      d   Result of adding polynomials vA and vB together.
c-----------------------------------------------------------------------
c Name   Type Description (Local Variables)
c-----------------------------------------------------------------------
c i       i   Index variable for do loops.
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER sA, sB, sC, psC
      DOUBLE PRECISION vA(sA), vB(sB), vC(psC)
c-----------------------------------------------------------------------
c     Declare Local variables.
c-----------------------------------------------------------------------
      INTEGER i
      
c-----------------------------------------------------------------------
c     Add polynomial coefficients together that exist 
c     for both polynomials.
c-----------------------------------------------------------------------
      DO i=1,min(sA,sB)
        vC(i) = vA(i) + vB(i)
      END DO
c-----------------------------------------------------------------------
c     If vA larger than vB the move the remainder of vA to the result.
c-----------------------------------------------------------------------
      IF ( sA .gt. sB ) THEN
        DO i=sB+1,sA
          vC(i) = vA(i)
        END DO
c-----------------------------------------------------------------------
c     If vA smaller than vB the move the remainder of vB to the result.
c-----------------------------------------------------------------------
      ELSE IF ( sB .gt. sA ) THEN
        DO i=sA+1,sB
          vC(i) = vB(i)
        END DO
      END IF
c-----------------------------------------------------------------------
c     Establish the size of the result polynomial.
c-----------------------------------------------------------------------
      sC=max(sA,sB)
      
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE polyShft( vA, sA, ishift, vB, sB, pshift )
c-----------------------------------------------------------------------
c     This subroutine for ishift>0 multiplies the polynomial in vA (in F) 
c     by F^ishift by shifting the vector polynomial ishift positions 
c     to the left and shifting in zeros for the lowest order coefficients.
c     For ishift<0 the polynomial in vA (in F) is multiplied 
c     by F^ishift=B^(-ishift) by shifting the vector poynomial ishift 
c     positions to the right and truncating the negative powers of F.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c ishift  i   Size of shift to perform.
c sA      i   Size of vA polynomial.
c sB      i   Size of vB polynomial.
c vA      d   Input polynomial to be shifted.
c vB      d   Output polynomial after shift operation.
c-----------------------------------------------------------------------
c Name   Type Description (Local Variables)
c-----------------------------------------------------------------------
c sC      i   Size of vC polynomial.
c vC      d   Temporary storage for vA polynomial in case vA and vB
c             have the same memory address.
c ZERO    d   Parameter.
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER ishift, sA, sB, sdum, pshift
      DOUBLE PRECISION vA(sA), vB(pshift)
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER sC
      DOUBLE PRECISION vC(sA), ZERO
      PARAMETER (ZERO=0.0d0)
      
c-----------------------------------------------------------------------
c     Shift the polynomial left if ishift larger than zero,
c     by first copying the the input polynomial vA to vC 
c     then initializing the first ishift locations in vB to zero
c     and then moving the vC polynomial to the correct location in vB.
c-----------------------------------------------------------------------
      IF ( ishift .gt. 0 ) THEN
        CALL cpyVec( vA, sA, vC, sC )
        vB(1) = ZERO
        CALL cpyVec( vB(1), ishift-1, vB(2), sdum )
        CALL cpyVec( vC, sC, vB(ishift+1), sdum )
        sB = sA+ishift

c-----------------------------------------------------------------------
c     Shift the polynomial right if ishift smaller than zero
c     and eliminate lower order coefficients.
c-----------------------------------------------------------------------
      ELSE IF ( ishift .lt. 0 ) THEN
        IF ( -ishift .lt. sA ) THEN
         CALL cpyVec( vA, sA, vC, sC )
         CALL cpyVec( vC(1-ishift), sC+ishift, vB(1), sB )
        ELSE
         vB(1)=0
         sB=0
        END IF

c-----------------------------------------------------------------------
c     Else copy the input polynomial to the output polynomial.
c-----------------------------------------------------------------------
      ELSE
        CALL cpyVec( vA, sA, vB, sB )
      END IF
      
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE getSPFrac( vA1, sA1, vA2, sA2, vB1, sB1, vB2, sB2,
     &                      vD, sD, vE, sE, ppp1, ppp2, ppp3 )
c-----------------------------------------------------------------------
c     This subroutine performs partial fraction decomposition by 
c     setting up h+k+1 system equations that are satisfied by the
c     D and E polynomials where h and k are defined in the code
c     and where E has h+1 coefficients, the first being 0, 
c     and where D has k+1 coefficients.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c sA1     i   Size of vA1 polynomial.
c sA2     i   Size of vA2 polynomial.
c sB1     i   Size of vB1 polynomial.
c sB2     i   Size of vB2 polynomial.
c sD      i   Size of vD polynomial.
c sE      i   Size of vE polynomial.
c vA1     i   First numerator polynomial in powers of F.
c vA2     i   Second numerator polynomial in powers of B.
c vB1     i   First denominator polynomial in powers of F.
c vB2     i   Second denominator polynomial in powers of B.
c vD      i   Partial fraction numerator associated with sB1.
c vE      i   Partial fraction numerator associated with sB2.
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c i,j     i   Index variables for do loops
c jindex  i   Index used to navigate mS matrix
c mInvStrS d  Matrix containing inverse of S'S
c mS      d   Matrix containing coeffieient matrix
c mStrS   d   Matrix containing S'S
c nF1     i   Size (rows,columns) of vF1 vector
c nF2     i   Size (rows,columns) of vF2 vector
c nInvStrS i  Size (rows,columns) of mInvStrS matrix
c nRem    i   Size (rows,columns) of vRem vector
c nS      i   Size (rows,columns) of mS matrix
c nStrS   i   Size (rows,columns) of mStrS matrix
c nSave   i   Memory allocation size for some local matrices
c sB      i   Size of vB polynomial
c sdum    i   Dummy size variable
c sRem    i   Size of vRem polynomial
c vB      d   Vector of combined denominator polynomials
c vF1     d   Temporary vector
c vF2     d   Vector of combined D and E polynomials
c vRem    d   Vector of remainder polynomial from polynomial division 
c               for A/B
c ZERO    d   Parameter
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER sA1, sA2, sB1, sB2, sD, sE
      INTEGER ppp1, ppp2, ppp3
      DOUBLE PRECISION vA1(sA1), vA2(sA2), vB1(sB1), vB2(sB2),
     &                 vD(ppp1), vE(ppp2)
      INTEGER pshift      
c-----------------------------------------------------------------------
c     Declare Local variables.
c-----------------------------------------------------------------------
      INTEGER i, j, jindex, h, k, nSave, sdum, sRA2, sRB2, sRE, sRes
      INTEGER nF1(2), nF2(2), nRes(2), nS(2), nStrS(2), nInvStrS(2)
      PARAMETER (nSave=500*500)
      DOUBLE PRECISION mInvStrS(nSave), mS(nSave), mStrS(nSave),
     &                 vF1(ppp3+ppp2-1),
     &                 vF2(ppp3+ppp2-1),
     &                 vRes(ppp3+ppp2-1),
     &                 vRA2(sA2), vRB2(sB2), vRE(ppp2), ZERO
      PARAMETER (ZERO=0.0d0)
      SAVE mS, mStrS
      EQUIVALENCE (mS, mInvStrS)
      
c-----------------------------------------------------------------------
c     Reverse the B2 subpolynomial.
c-----------------------------------------------------------------------
      CALL polyRev( vB2, sB2, vRB2, sRB2 )
      
c-----------------------------------------------------------------------
c     Set up the system of equations that the numerator polynomials
c     for the partial fractions satisfy.
c-----------------------------------------------------------------------
      h = max(sA2,sB2)-1
      k = max(sA1,sB1)-1
      nS(1) = h + k + 1
      nS(2) = nS(1)
      
c     ------------------------------------------------------------------
c     Zero out the matrix representation.
c     ------------------------------------------------------------------
      DO j = 1, nS(2)
        jindex = (j-1)*nS(1)+1
        mS( jindex ) = ZERO
        CALL cpyVec( mS( jindex ), nS(1)-1, mS( jindex+1 ), sdum )
      END DO
c     ------------------------------------------------------------------
c     Fill in the matrix associated with the B1 polynomial.
c     ------------------------------------------------------------------
      IF ( h .gt. 0 ) THEN
        DO j = 1, h
          jindex = (j-1)*nS(1) + j
          CALL cpyVec( vB1, sB1, mS( jindex ), sdum )
        END DO
      END IF
c     ------------------------------------------------------------------
c     Fill in the matrix associated with the B2 polynomial.
c     ------------------------------------------------------------------
      DO j = h+1, nS(2)
        jindex = (j-1)*nS(1) + j - (sB2-1)
        CALL cpyVec( vRB2, sRB2, mS( jindex ), sdum )
      END DO

c-----------------------------------------------------------------------
c     Set up the result vector vRes where vRes represents A1(F)xA2(B).
c-----------------------------------------------------------------------
      CALL polyRev( vA2, sA2, vRA2, sRA2 )
      CALL CONV( vRA2, sRA2, vA1, sA1, vRes, sRes )
      IF ( h .gt. (sA2-1) ) THEN
        pshift = max(sRes+h-(sA2-1),1)
        CALL polyShft( vRes, sRes, h-(sA2-1), vRes, sRes, pshift )
      END IF
      IF ( k .gt. (sA1-1) ) THEN
        DO i=sRes+1,h+k+1
          vRes(i)=ZERO
        END DO
        sRes = h + k + 1
      END IF

c-----------------------------------------------------------------------
c     Solve the system of equations: SxG = vRes where G = (D' E')' 
c     using G = (S'S)^(-1) x S'vRes where X' denotes transpose 
c     of matrix X.
c-----------------------------------------------------------------------
      nRes(1) = sRes
      nRes(2) = 1
      CALL mulTrMat( mS, nS, vRes, nRes, vF1, nF1 )
      CALL mulTrMat( mS, nS, mS, nS, mStrS, nStrS )
      CALL invMat( mStrS, nStrS, mInvStrS, nInvStrS )
      CALL mulMat( mInvStrS, nInvStrS, vF1, nF1, vF2, nF2 )

c-----------------------------------------------------------------------
c     Move the result from combined polynomial F for (D' E')' to the 
c     output polynomials for D and E, then add polynomials Quot and D.
c-----------------------------------------------------------------------
c     First check for D polynomial and move to output vector.
c     ------------------------------------------------------------------
      IF ( k .ge. 0 ) THEN
        CALL cpyVec( vF2(h+1), k+1, vD, sD )
      ELSE
        sD=1
        vD(1)=ZERO
      END IF
c     ------------------------------------------------------------------
c     Second check for E polynomial and move to output vector.
c     ------------------------------------------------------------------
      IF ( h .gt. 0 ) THEN
        vRE(h+1) = ZERO
        CALL cpyVec( vF2, h, vRE, sdum )
        sRE = h+1
        CALL polyRev( vRE, sRE, vE, sE )
      ELSE
        sRE=1
        vRE(1)=ZERO
        sE=1
        vE(1)=ZERO
      END IF

      RETURN
      END