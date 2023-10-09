**==iscrfn.f    processed by SPAG 4.03F  at 09:50 on  1 Mar 1994
      SUBROUTINE iscrfn(Oprn,Scr,Avec,Nelt,Pc,Cvec)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INTEGER ADD,SUB,MULT,DIV
      PARAMETER(ADD=1,SUB=2,MULT=3,DIV=4)
      INTEGER Avec,Cvec,Oprn,Nelt,Pc,Scr
      DIMENSION Avec(Nelt),Cvec(Pc)
c     ------------------------------------------------------------------
      INTEGER i
c     ------------------------------------------------------------------
      DO i=1,Nelt
       IF(Oprn.eq.ADD)THEN
        Cvec(i)=Scr+Avec(i)
c     ------------------------------------------------------------------
       ELSE IF(Oprn.eq.SUB)THEN
        Cvec(i)=Avec(i)-Scr
c     ------------------------------------------------------------------
       ELSE IF(Oprn.eq.MULT)THEN
        Cvec(i)=Scr*Avec(i)
c     ------------------------------------------------------------------
       ELSE IF(Oprn.eq.DIV)THEN
        Cvec(i)=Avec(i)/Scr
       END IF
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
