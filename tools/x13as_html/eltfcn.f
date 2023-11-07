**==eltfcn.f    processed by SPAG 4.03F  at 09:48 on  1 Mar 1994
      SUBROUTINE eltfcn(Oprn,Avec,Bvec,Nelt,Pc,Cvec)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INTEGER ADD,SUB,MULT,DIV
      PARAMETER(ADD=1,SUB=2,MULT=3,DIV=4)
      INTEGER Oprn,Nelt,Pc
      DOUBLE PRECISION Avec,Bvec,Cvec
      DIMENSION Avec(Nelt),Bvec(Nelt),Cvec(Pc)
c     ------------------------------------------------------------------
      INTEGER i
c     ------------------------------------------------------------------
      DO i=1,Nelt
       IF(Oprn.eq.ADD)THEN
        Cvec(i)=Avec(i)+Bvec(i)
c     ------------------------------------------------------------------
       ELSE IF(Oprn.eq.SUB)THEN
        Cvec(i)=Avec(i)-Bvec(i)
c     ------------------------------------------------------------------
       ELSE IF(Oprn.eq.MULT)THEN
        Cvec(i)=Avec(i)*Bvec(i)
c     ------------------------------------------------------------------
       ELSE IF(Oprn.eq.DIV)THEN
        Cvec(i)=Avec(i)/Bvec(i)
       END IF
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
