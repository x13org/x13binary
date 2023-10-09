      SUBROUTINE phaseGain( filterIn, nFilter, startPoint, iPeriod,
     &                      fltLmbda, sqGain, phase, phaseDelay )
      IMPLICIT NONE
c-----------------------------------------------------------------------
c  phasegain.f, Release 1, Subroutine Version 1.2, Modified 20 Apr 2006.
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 09 Sep 2005.
c     Modified by REG, on 19 Jan 2006, to change output fltLmbda 
c       from frequencies to cycles/period.
c     Modified by REG, on 20 Apr 2006, to calculate phase delay 
c       at lambda equal to zero using derivative of arcTangent formula
c       and to lengthen output vectors by 1 for case of lambda=6.
c-----------------------------------------------------------------------
c     Modified by BCM 10 Apr 2006 - add code to handle case where
c     computation must take arctan of 0 
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
c-----------------------------------------------------------------------
c     Given an input filter, this subroutine calculates squared gain,  
c     phase, and phase delay associated with filter's Fourier transform.
c-----------------------------------------------------------------------
c Name       Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c filterIn   d    vector of input filter weights
c fltLmbda   d    vector of cycles/period set containing the following:
c                   [0, 1, ... , (grid*iPeriod/2)]/grid
c                   with grid = 200 currently
c sqGain     d    filter square gain values over the frequency interval
c iPeriod    i    seasonal period
c nFilter    i    size of input vector
c phase      d    filter phase values over the frequency interval
c phasedelay d    filter phase delay over the frequency interval
c startPoint i    index offset of first coefficient in input filter and
c                 also indicates number of future values in input filter
c-----------------------------------------------------------------------
      INTEGER nFilter, startPoint, iPeriod, grid
      PARAMETER ( grid=200 )
      DOUBLE PRECISION filterIn( 0:nFilter-1 )
      DOUBLE PRECISION fltLmbda( 0:grid*((iPeriod+1)/2) )
      DOUBLE PRECISION sqGain( 0:grid*((iPeriod+1)/2) )
      DOUBLE PRECISION phase( 0:grid*((iPeriod+1)/2) )
      DOUBLE PRECISION phaseDelay( 0:grid*((iPeriod+1)/2) )

c-----------------------------------------------------------------------
c Name       Type Description (Local Variables)
c-----------------------------------------------------------------------
c arg        d    frequency argument for partial Fourier transform
c grid       i    parameter for number of frequency points in each 
c                   interval to be evaluated
c j          i    index variables for do loops
c index      i    ranges over [0 ... grid*(oPeriod+1)/2]
c lambda     d    part of working frequency
c nOut       i    size of gain and phase vectors
c sSum0      d    sum of filterIn coefficients
c sTau0      d    used to calculate phase delay at lambda = 0.0
c TWO_PI     d    parameter for 2*PI
c vClambda   d    complex value of partial Fourier transform
c vSum       d    complex value of Fourier transform
c ZERO       d    parameter for zero
c-----------------------------------------------------------------------
      INTEGER j, index, nOut
      DOUBLE PRECISION vClambda(2), lambda, vCsum(2), ZERO, TWO_PI, arg
      DOUBLE PRECISION sSum0, sTau0
      PARAMETER ( ZERO=0.0D0, TWO_PI=2.0D0*3.14159265358979d0 )

c-----------------------------------------------------------------------
c     Modified by BCM 10 Apr 2006 - add reference to dpeq
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
c     Zero the output vectors
c-----------------------------------------------------------------------
      nOut = grid*((iPeriod+1)/2)
      DO j = 0, nOut
        sqGain(j)     = ZERO
        phase(j)      = ZERO
        phaseDelay(j) = ZERO
      END DO

c-----------------------------------------------------------------------
c     Calculate the Fourier transform over the frequency set
c-----------------------------------------------------------------------
      sSum0 = ZERO
      sTau0 = ZERO
      DO index = 0, nOut
c     ------------------------------------------------------------------
c     Choose the Fourier frequence (arg)
c     Calculate the partial Fourier transform (vClambda)
c     Sum the partial Fourier transforms (vSum)
c     ------------------------------------------------------------------
        lambda = DBLE(index)/DBLE(grid)
        vCsum(1) = ZERO
        vCsum(2) = ZERO
        DO j = 0, nFilter-1
          arg = TWO_PI*lambda*DBLE(startPoint+j)/DBLE(iPeriod)
          vClambda(1) = filterIn(nFilter-1-j)*DCOS(arg)
          vClambda(2) = filterIn(nFilter-1-j)*DSIN(-arg)
          vCsum(1) = vCsum(1) + vClambda(1)
          vCsum(2) = vCsum(2) + vClambda(2)
          IF ( index .eq. 0 ) THEN
            sSum0 = sSum0 + filterIn(nFilter-1-j)
            sTau0 = sTau0 + DBLE(startPoint+j)*filterIn(nFilter-1-j)
          END IF
        END DO
c     ------------------------------------------------------------------
c     Calculate squared gain, phase, and phase delay of Fourier transform
c     ------------------------------------------------------------------
c       fltLmbda(index) = TWO_PI*lambda/DBLE(iPeriod)
        fltLmbda(index) = lambda
        sqGain(index) = vCsum(1)*vCsum(1) + vCsum(2)*vCsum(2)
c     ------------------------------------------------------------------
c     Modified by BCM 10 Apr 2006 - if vCsum(1) or vCsum(2) = 0, set
c     value for phase and phaseDelay to DNOTST - otherwise,
c     continue with computation
c     ------------------------------------------------------------------
        if(dpeq(vCsum(1),ZERO).and.dpeq(vCsum(2),ZERO))THEN
         phase(index) = DNOTST
         phaseDelay(index) = DNOTST
        ELSE
         phase(index) = DATAN2( vCsum(2), vCsum(1) )
     &                * DBLE(iPeriod)/TWO_PI
         IF ( index .eq. 0 ) THEN
           phaseDelay(index) = sTau0/sSum0
         ELSE
           phaseDelay(index) = -phase(index)/lambda
         END IF
        END IF
      END DO
c-----------------------------------------------------------------------
      RETURN
      END