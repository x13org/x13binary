C     Last change:  BCM  16 Feb 1999    3:50 pm
**==punch.f    processed by SPAG 4.03F  at 15:10 on  1 Aug 1994
      SUBROUTINE punch(X,Mfda,Mlda,Itbl,Lgraf,Lpct)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'x11opt.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION ONEHND
      PARAMETER(ONEHND=100D0)
c     ------------------------------------------------------------------
c     Argument Lpct, parameter ONEHND added to allow certain tables to
c     be printed as percentages - BCM July 2006
c     ------------------------------------------------------------------
      LOGICAL Lgraf,Lpct
      INTEGER Mfda,Mlda,Itbl,i
      DOUBLE PRECISION X,y
      DIMENSION X(*),y(PLEN)
c     INTEGER frstdt
c     DIMENSION frstdt(2)
c-----------------------------------------------------------------------
*      LOGICAL F,T
*      PARAMETER(T=.true.,F=.false.)
c-----------------------------------------------------------------------
c     Check to see if this is a revisions history, holiday, prior
c     simultaneous trading day or sliding spans seasonal adjustment run.
c     ------------------------------------------------------------------
      IF(Lhiddn)RETURN
c     ------------------------------------------------------------------
c     Otherwise, store in /rdb format, date then value.
c     First, if table is to be stored as a percentage, multiply the
c     series by one hundred - BCM July 2006
c     ------------------------------------------------------------------
      IF(Lpct)THEN
       DO i=Mfda,Mlda
        y(i)=X(i)*ONEHND
       END DO
      ELSE
       DO i=Mfda,Mlda
        y(i)=X(i)
       END DO
      END IF
c     ------------------------------------------------------------------
c      CALL savtbl(Itbl,Begbak,Mfda,Mlda,Ny,X,Serno,Nser)
      CALL savtbl(Itbl,Begbk2,Mfda,Mlda,Ny,y,Serno,Nser,Lgraf)
      RETURN
c     ------------------------------------------------------------------
      END
