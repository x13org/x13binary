**==dinvnr.f    processed by SPAG 4.03F  at 14:31 on 28 Jul 1994
      DOUBLE PRECISION FUNCTION dinvnr(P,Q)
      IMPLICIT NONE
C**********************************************************************
C
C     DOUBLE PRECISION FUNCTION DINVNR(P,Q)
C     Double precision NoRmal distribution INVerse
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
C     Q --> 1-P
C                    P is DOUBLE PRECISION
C
C
C                              Method
C
C
C     The  rational   function   on  page 95    of Kennedy  and  Gentle,
C     Statistical Computing, Marcel Dekker, NY , 1980 is used as a start
C     value for the Newton method of finding roots.
C
C
C                              Note
C
C
C     If P or Q .lt. machine EPS returns +/- DINVNR(EPS)
C
C**********************************************************************
C     .. Parameters ..
      INTEGER MAXIT
      PARAMETER(MAXIT=100)
      DOUBLE PRECISION EPS
      PARAMETER(EPS=1.0D-13)
      DOUBLE PRECISION R2PI
      PARAMETER(R2PI=0.3989422804014326D0)
      DOUBLE PRECISION NHALF
      PARAMETER(NHALF=-0.5D0)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION P,Q
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION strtx,xcur,cum,ccum,pp,dx
      INTEGER i
      LOGICAL qporq
C     ..
C     .. External Functions ..
      DOUBLE PRECISION stvaln
      EXTERNAL stvaln
C     ..
C     .. External Subroutines ..
      EXTERNAL cumnor
C     ..
C     .. Statement Functions ..
      DOUBLE PRECISION dennor,x
 
      dennor(x)=R2PI*exp(NHALF*x*x)
C     ..
C     .. Executable Statements ..
C
C     FIND MINIMUM OF P AND Q
C
      qporq=P.le.Q
      IF(.not.qporq)THEN
 
       pp=Q
      ELSE
       pp=P
      END IF
C
C     INITIALIZATION STEP
C
      strtx=stvaln(pp)
      xcur=strtx
C
C     NEWTON INTERATIONS
C
      DO i=1,MAXIT
       CALL cumnor(xcur,cum,ccum)
       dx=(cum-pp)/dennor(xcur)
       xcur=xcur-dx
       IF(abs(dx/xcur).lt.EPS)GO TO 10
      END DO
      dinvnr=strtx
C
C     IF WE GET HERE, NEWTON HAS FAILED
C
      IF(.not.qporq)dinvnr=-dinvnr
      RETURN
C
C     IF WE GET HERE, NEWTON HAS SUCCEDED
C
   10 dinvnr=xcur
      IF(.not.qporq)dinvnr=-dinvnr
      RETURN
 
      END
