C     Last change:  BCM  25 Nov 97    9:52 am
**==setchr.f    processed by SPAG 4.03F  at 09:53 on  1 Mar 1994
      SUBROUTINE setchr(Chr,Nchr,Chrvec)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Subroutine to set each character in a character vector to chr
c-----------------------------------------------------------------------
      CHARACTER Chr*1,Chrvec*(*)
      INTEGER i,Nchr
c     ------------------------------------------------------------------
      DO i=1,Nchr
       Chrvec(i:i)=Chr
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
