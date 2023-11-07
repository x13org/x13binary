C     Last change:  BCM  25 Nov 97   11:05 am
**==quad.f    processed by SPAG 4.03F  at 09:52 on  1 Mar 1994
      SUBROUTINE quad(A,B1,C,Snr,Sni,Lr,Li)
      IMPLICIT NONE
C **********************************************************************
C *                                                                    *
C * CALCULATE THE ZEROS OF THE QUADRATIC A*Z**2+B1*Z+C.                *
C * THE QUADRATIC FORMULA, MODIFIED TO AVOID OVERFLOW, IS USED TO      *
C *     FIND THE LARGER ZERO IF THE ZEROS ARE REAL AND BOTH ZEROS      *
C *     ARE COMPLEX.                                                   *
C * THE SMALLER REAL ZERO IS FOUND DIRECTLY FROM THE PRODUCT OF        *
C *     THE ZEROS C/A.                                                 *
C *                                                                    *
C **********************************************************************
      DOUBLE PRECISION ZERO,TWO
      PARAMETER(ZERO=0D0,TWO=2D0)
C-----------------------------------------------------------------------
      DOUBLE PRECISION A,B1,C,Snr,Sni,Lr,Li,b,d,e,dabs,dsqrt
C-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
C-----------------------------------------------------------------------
      IF(dpeq(A,ZERO))THEN
       Snr=ZERO
       IF(.not.dpeq(B1,ZERO))Snr=-C/B1
       Lr=ZERO
      ELSE IF(.not.dpeq(C,ZERO))THEN
C-----------------------------------------------------------------------
C COMPUTE DISCRIMINANT AVOIDING OVERFLOW
C-----------------------------------------------------------------------
       b=B1/TWO
       IF(dabs(b).lt.dabs(C))THEN
        e=A
        IF(C.lt.0.D0)e=-A
        e=b*(b/dabs(C))-e
        d=dsqrt(dabs(e))*dsqrt(dabs(C))
       ELSE
        e=1.D0-(A/b)*(C/b)
        d=dsqrt(dabs(e))*dabs(b)
       END IF
       IF(e.lt.ZERO)GO TO 10
C-----------------------------------------------------------------------
C REAL ZEROS
C-----------------------------------------------------------------------
       IF(b.ge.ZERO)d=-d
       Lr=(-b+d)/A
       Snr=ZERO
       IF(.not.dpeq(Lr,ZERO))Snr=(C/Lr)/A
      ELSE
       Snr=ZERO
       Lr=-B1/A
      END IF
      Sni=ZERO
      Li=ZERO
      RETURN
C-----------------------------------------------------------------------
C COMPLEX CONJUGATE ZEROS
C-----------------------------------------------------------------------
   10 Snr=-b/A
      Lr=Snr
      Sni=dabs(d/A)
      Li=-Sni
      RETURN
      END
