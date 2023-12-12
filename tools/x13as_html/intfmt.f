**==intfmt.f    processed by SPAG 4.03F  at 09:50 on  1 Mar 1994
      SUBROUTINE intfmt(Vec,Nelt,Clwdth)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Figures out the minimum number of columns needed to print out
c a vector of integers
c-----------------------------------------------------------------------
      INTEGER Clwdth,ielt,ival,iwdth,Nelt,Vec
      DIMENSION Vec(Nelt)
c     ------------------------------------------------------------------
      Clwdth=0
      DO ielt=1,Nelt
       ival=Vec(ielt)
c     ------------------------------------------------------------------
       IF(ival.eq.0)THEN
        iwdth=1
       ELSE
        iwdth=max(0,int(log10(float(abs(ival))))+1)
       END IF
c     ------------------------------------------------------------------
       IF(Vec(ielt).lt.0)iwdth=iwdth+1
       Clwdth=max(Clwdth,iwdth)
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
