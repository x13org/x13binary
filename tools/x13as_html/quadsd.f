**==quadsd.f    processed by SPAG 4.03F  at 09:52 on  1 Mar 1994
      SUBROUTINE quadsd(Nn,U,V,P,Q,A,B)
      IMPLICIT NONE
C **********************************************************************
C *                                                                    *
C * DIVIDES P BY THE QUADRATIC  1,U,V  PLACING THE QUOTIENT IN Q AND   *
C *         THE REMAINDER IN A,B                                       *
C *                                                                    *
C **********************************************************************
      INTEGER i,Nn
      DOUBLE PRECISION P(Nn),Q(Nn),U,V,A,B,c
      B=P(1)
      Q(1)=B
      A=P(2)-U*B
      Q(2)=A
      DO i=3,Nn
       c=P(i)-U*A-V*B
       Q(i)=c
       B=A
       A=c
      END DO
      RETURN
      END
