C     Last change:  BCM  15 Apr 2005   12:40 pm
      SUBROUTINE newest(Type,Uu,Vv)
      IMPLICIT NONE
C **********************************************************************
C *                                                                    *
C * COMPUTE NEW ESTIMATES OF THE QUADRATIC COEFFICIENTS USING THE      *
C *         SCALARS COMPUTED IN CALCSC.                                *
C *                                                                    *
C **********************************************************************
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'global.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION ONE,ZERO
      PARAMETER(ONE=1D0,ZERO=0D0)
C-----------------------------------------------------------------------
      DOUBLE PRECISION a4,a5,b1,b2,c2,c3,c4,temp,Uu,Vv,c1
      INTEGER Type
C-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
C-----------------------------------------------------------------------
C USE FORMULAS APPROPRIATE TO SETTING OF TYPE.
C-----------------------------------------------------------------------
      IF(Type.ne.3)THEN
       IF(Type.eq.2)THEN
        a4=(A0+G)*F+H
        a5=(F+U)*C+V0*D0
       ELSE
        a4=A0+U*B0+H*F
        a5=C+(U+V0*F)*D0
       END IF
C-----------------------------------------------------------------------
C EVALUATE NEW QUADRATIC COEFFICIENTS.
C-----------------------------------------------------------------------
       b1=-K(N)/P0(N0)
       b2=-(K(N-1)+b1*P0(N))/P0(N0)
       c1=V0*b2*A1
       c2=b1*A7
       c3=b1*b1*A3
       c4=c1-c2-c3
       temp=a5+b1*a4-c4
       IF(.not.dpeq(temp,ZERO))THEN
        Uu=U-(U*(c3+c2)+V0*(b1*A1+b2*A7))/temp
        Vv=V0*(ONE+c4/temp)
        RETURN
       END IF
      END IF
C-----------------------------------------------------------------------
C IF TYPE=3 THE QUADRATIC IS ZEROED
C-----------------------------------------------------------------------
      Uu=ZERO
      Vv=ZERO
      RETURN
      END

