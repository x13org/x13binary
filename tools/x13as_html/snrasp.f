**==snrasp.f    processed by SPAG 4.03F  at 10:39 on 20 Oct 1994
      SUBROUTINE snrasp(A,B,Sxx,Frq,Sgme2,L,K,H1,Ldecbl)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      DOUBLE PRECISION A,B,cst1,g,gi1,gi2,gr1,gr2,pxx,Sgme2,t,Sxx,Frq
      INTEGER i,i1,K,k1,L,l1
      LOGICAL Ldecbl
C*** End of declarations inserted by SPAG
c-----------------------------------------------------------------------
C     THIS PROGRAM COMPUTES POWER SPECTRUM OF AN AR-MA PROCESS DEFINED B
C     X(N)=A(1)X(N-1)+...+A(L)X(N-L)+E(N)+B(1)E(N-1)+...+B(K)E(N-K),
C     WHERE E(N) IS A WHITE NOISE WITH ZERO MEAN AND VARIANCE EQUAL TO
C     SGME2.  OUTPUTS PXX(I) ARE GIVEN AT FREQUENCIES I/(2*H)
C     I=0,1,...,H.
C     REQUIRED INPUTS ARE:
C     L,K,H,SGME2,(A(I),I=1,L), AND (B(I),I=1,K).
C     0 IS ALLOWABLE AS L AND/OR K.
c-----------------------------------------------------------------------
      INTEGER H1
      DIMENSION A(101),B(101)
      DIMENSION g(H1),gr1(H1),gi1(H1),gr2(H1),gi2(H1)
      DIMENSION pxx(H1),Sxx(*),Frq(*)
c-----------------------------------------------------------------------
      DOUBLE PRECISION decibl
      EXTERNAL decibl
c-----------------------------------------------------------------------
      cst1=1.0D-00
      IF(L.gt.0)THEN
       DO i=1,L
        A(i)=-A(i)
       END DO
      END IF
      l1=L+1
      k1=K+1
      g(1)=cst1
      IF(L.gt.0)THEN
       DO i=1,L
        i1=i+1
        g(i1)=-A(i)
       END DO
      END IF
C     COMMON SUBROUTINE CALL
      CALL fouger(g,l1,gr1,gi1,H1,Frq)
      g(1)=cst1
      IF(K.gt.0)THEN
       DO i=1,K
        i1=i+1
        g(i1)=B(i)
       END DO
      END IF
C     COMMON SUBROUTINE CALL
      CALL fouger(g,k1,gr2,gi2,H1,Frq)
      DO i=1,H1
       pxx(i)=(gr2(i)**2+gi2(i)**2)/(gr1(i)**2+gi1(i)**2)*Sgme2
      END DO
      IF(Ldecbl)THEN
       DO i=1,H1
        t=pxx(i)
        IF(t.lt.0D0)t=-t
        Sxx(i)=decibl(dble(t))
       END DO
      ELSE
       DO i=1,H1
        Sxx(i)=pxx(i)
       END DO
      END IF
      RETURN
      END
