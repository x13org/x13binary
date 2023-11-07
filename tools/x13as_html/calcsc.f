C     Last change:  BCM  25 Nov 97    2:45 pm
      SUBROUTINE calcsc(Type)
      IMPLICIT NONE
C **********************************************************************
C *                                                                    *
C * THIS ROUTINE CALCULATES SCALAR QUANTITIES USED TO COMPUTE THE NEXT *
C *      K POLYNOMIAL AND NEW ESTIMATES OF THE QUADRATIC COEFFICIENTS. *
C * TYPE - INTEGER VARIABLE SET HERE INDICATING HOW THE                *
C *        CALCULATIONS ARE NORMALIZED TO AVOID OVERFLOW               *
C *                                                                    *
C **********************************************************************
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'global.cmn'
C-----------------------------------------------------------------------
      DOUBLE PRECISION dabs
      INTEGER Type
C-----------------------------------------------------------------------
C SYNTHETIC DIVISION OF K BY THE QUADRATIC 1,U,V0
C-----------------------------------------------------------------------
      CALL quadsd(N,U,V0,K,Qk,C,D0)
      IF(dabs(C).le.dabs(K(N))*100D0*Eta)THEN
       IF(dabs(D0).le.dabs(K(N-1))*100D0*Eta)THEN
        Type=3
C-----------------------------------------------------------------------
C TYPE=3 INDICATES THE QUADRATIC IS ALMOST A FACTOR OF K
C-----------------------------------------------------------------------
        RETURN
       END IF
      END IF
      IF(dabs(D0).ge.dabs(C))THEN
       Type=2
C-----------------------------------------------------------------------
C TYPE=2 INDICATES THAT ALL FORMULAS ARE DIVIDED BY D
C-----------------------------------------------------------------------
       E=A0/D0
       F=C/D0
       G=U*B0
       H=V0*B0
       A3=(A0+G)*E+H*(B0/D0)
       A1=B0*F-A0
       A7=(F+U)*A0+H
       RETURN
      END IF
      Type=1
C-----------------------------------------------------------------------
C TYPE=1 INDICATES THAT ALL FORMULAS ARE DIVIDED BY C
C-----------------------------------------------------------------------
      E=A0/C
      F=D0/C
      G=U*E
      H=V0*B0
      A3=A0*E+(H/C+G)*B0
      A1=B0-A0*(D0/C)
      A7=A0+G*D0+H*F
      RETURN
      END
