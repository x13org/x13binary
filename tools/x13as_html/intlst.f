**==intlst.f    processed by SPAG 4.03F  at 09:50 on  1 Mar 1994
      SUBROUTINE intlst(Pelt,Ptrvec,Nstr)
      IMPLICIT NONE
c     -----------------------------------------------------------------
      INTEGER Nstr,Pelt,Ptrvec
      DIMENSION Ptrvec(0:Pelt)
c     -----------------------------------------------------------------
      Nstr=0
      Ptrvec(Nstr)=1
      Ptrvec(1)=1
c     -----------------------------------------------------------------
      RETURN
      END
