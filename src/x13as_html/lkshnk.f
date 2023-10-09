C     Last change:  BCM   6 May 2003   10:28 am
      DOUBLE PRECISION FUNCTION lkshnk(S1,S2,Sig)
      IMPLICIT NONE
C-----------------------------------------------------------------------
c     Compute estimator of likelihood associated with S1 given that the
c     true mean is S2, with variance estimated by Sig.
c     This is used in the "local" method of generating shrinkage
c     estimates developed in the paper "Shrinkage Est. of Time Series
c     Seasonal Factors and their Effect on Forecasting Accuracy",
c     Miller & Williams (2003)
C-----------------------------------------------------------------------
      DOUBLE PRECISION PI,ONE,TWO,MONE
      PARAMETER(PI=3.14159265358979d0,ONE=1D0,TWO=2D0,MONE=-1D0)
C-----------------------------------------------------------------------
      DOUBLE PRECISION S1,S2,Sig
C-----------------------------------------------------------------------
      lkshnk = (ONE/dsqrt(Sig*TWO*PI))*
     &          dexp((MONE/TWO)*(((S1-S2)*(S1-S2))/Sig))
      RETURN
      END
      