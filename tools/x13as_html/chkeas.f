C     Last change:  BCM  25 Nov 97    2:39 pm
**==chkeas.f    processed by SPAG 4.03F  at 09:45 on  3 Oct 1994
      SUBROUTINE chkeas(Lmar,Llda)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Check to see if X-11 Easter adjustment can be computed.
c----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'xeastr.cmn'
c----------------------------------------------------------------------
      INTEGER i,Lmar,Llda
c-----------------------------------------------------------------------
      CALL setint(0,4,Ieast)
      DO i=Lmar,Llda,12
c-----------------------------------------------------------------------
C---- CALCULATE Number of Easters BEFORE APRIL 2
c-----------------------------------------------------------------------
       IF(Xhol(i).le.10D0)THEN
        Ieast(1)=Ieast(1)+1
c-----------------------------------------------------------------------
C---- CALCULATE Number of Easters AFTER APRIL 16
c-----------------------------------------------------------------------
       ELSE IF(Xhol(i).gt.24D0)THEN
        Ieast(2)=Ieast(2)+1
c-----------------------------------------------------------------------
C---- CALCULATE Number of Easters From APRIL 2 to April 8
c-----------------------------------------------------------------------
       ELSE IF(Xhol(i).gt.10D0.and.Xhol(i).le.17D0)THEN
        Ieast(3)=Ieast(3)+1
c-----------------------------------------------------------------------
C---- CALCULATE Number of Easters From APRIL 9 to April 15
c-----------------------------------------------------------------------
       ELSE
        Ieast(4)=Ieast(4)+1
       END IF
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
