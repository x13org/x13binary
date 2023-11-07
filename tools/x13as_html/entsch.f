**==entsch.f    processed by SPAG 4.03F  at 16:21 on 30 Mar 1994
C***********************************************************************
c     This routine is modified code which originally appeared in X12W -
c     the seasonal adjustment program developed by the Budesbank
C***********************************************************************
      SUBROUTINE entsch(Ken,Ker,Ken1,Ker1,Iv)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      INTEGER Iv,k,Ken,Ken1,Ker,Ker1
C*** End of declarations inserted by SPAG
      k=Ken+Ker+1
      Ken1=0
      Ker1=0
      GO TO(10,20,30,40,50),k
   10 Ken1=Iv
      Ker1=2
      GO TO 50
   20 Ken1=1
      GO TO 50
   30 Ken1=Iv
      Ker1=Iv
      GO TO 50
   40 Ken1=1
      Ker1=2
   50 RETURN
      END
