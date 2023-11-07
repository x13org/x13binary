C     Last change:  BCM  21 Nov 97   10:42 pm
**==stvaln.f    processed by SPAG 4.03F  at 14:31 on 28 Jul 1994
      DOUBLE PRECISION FUNCTION stvaln(P)
      IMPLICIT NONE
C
C**********************************************************************
C
C     DOUBLE PRECISION FUNCTION STVALN(P)
C                    STarting VALue for Neton-Raphon
C                calculation of Normal distribution Inverse
C
C
C                              Function
C
C
C     Returns X  such that CUMNOR(X)  =   P,  i.e., the  integral from -
C     infinity to X of (1/SQRT(2*PI)) EXP(-U*U/2) dU is P
C
C
C                              Arguments
C
C
C     P --> The probability whose normal deviate is sought.
C                    P is DOUBLE PRECISION
C
C
C                              Method
C
C
C     The  rational   function   on  page 95    of Kennedy  and  Gentle,
C     Statistical Computing, Marcel Dekker, NY , 1980.
C
C**********************************************************************
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION P
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION xsign,y,z
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION xden(5),xnum(5)
C     ..
C     .. External Functions ..
      DOUBLE PRECISION devlpl
      EXTERNAL devlpl
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC log,sqrt
C     ..
C     .. Data statements ..
      DATA xnum/-0.322232431088D0,-1.000000000000D0,-0.342242088547D0,
     &     -0.204231210245D-1,-0.453642210148D-4/
      DATA xden/0.993484626060D-1,0.588581570495D0,0.531103462366D0,
     &     0.103537752850D0,0.38560700634D-2/
C     ..
C     .. Executable Statements ..
      IF(P.gt.0.5D0)THEN
 
       xsign=1.0D0
       z=1.0D0-P
      ELSE
       xsign=-1.0D0
       z=P
      END IF
      y=sqrt(-2.0D0*log(z))
      stvaln=y+devlpl(xnum,5,y)/devlpl(xden,5,y)
      stvaln=xsign*stvaln
      RETURN
 
      END
