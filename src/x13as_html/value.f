C     Last change:  BCM  25 Nov 97    2:22 pm
**==value.f    processed by SPAG 4.03F  at 09:54 on  1 Mar 1994
      SUBROUTINE value
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'chrt.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION yvalue
c-----------------------------------------------------------------------
      yvalue=Fact1*(Xyvec-Ymin)/Ydiff
      Ixy=Ifact2-int(yvalue)
c-----------------------------------------------------------------------
      RETURN
      END
