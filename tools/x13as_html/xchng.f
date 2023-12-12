C     Last change:  BCM  26 Feb 1999    4:12 pm
**==xchng.f    processed by SPAG 4.03F  at 12:24 on 21 Jun 1994
      SUBROUTINE xchng(X,Cx,Ncol,Im,Sslen,Nchng,Ldiff)
      IMPLICIT NONE
C-----------------------------------------------------------------------
c  *****
c  *****  calculate month-to-month or year-to-year changes of the
c  *****  seasonally adjusted data (Nchng = 1 for month-to month,
c  *****  12 or 4 for year-to year)
c  *****
C-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'ssap.prm'
      INCLUDE 'notset.prm'
C-----------------------------------------------------------------------
      LOGICAL Ldiff
      DOUBLE PRECISION Cx,X
      INTEGER i,iyy,iyy2,Sslen,Ncol,Nchng,Im
      DIMENSION X(MXLEN,MXCOL),Cx(MXLEN,MXCOL)
C-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
C-----------------------------------------------------------------------
      DO i=1,Ncol
       DO iyy=1,Sslen+Im-1
        Cx(iyy,i)=DNOTST
        iyy2=iyy-Nchng
        IF(.not.dpeq(X(iyy,i),DNOTST).and.iyy2.gt.0)THEN
         IF(.not.dpeq(X(iyy2,i),DNOTST))THEN
          Cx(iyy,i)=X(iyy,i)-X(iyy2,i)
          IF(.not.Ldiff)Cx(iyy,i)=(Cx(iyy,i)/abs(X(iyy2,i)))*100D0
         END IF
        END IF
       END DO
      END DO
C-----------------------------------------------------------------------
      RETURN
      END
