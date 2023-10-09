C     Last change:  BCM  12 Mar 98    9:48 am
**==fouger.f    processed by SPAG 4.03F  at 11:38 on  7 Nov 1994
      SUBROUTINE fouger(G,Lgp1,Fc,Fs,Lf1,Frq)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      DOUBLE PRECISION ck,ck2,cst0,Fc,Fs,G,sk,t,tk,um0,um1,um2,
     &                 Frq
      INTEGER i,i2,k,Lf1,lg,lg3,lg4,Lgp1
C*** End of declarations inserted by SPAG
C     COMMON SUBROUTINE
C     FOURIER TRANSFORM (GOERTZEL METHOD)
C     THIS SUBROUTINE COMPUTES FOURIER TRANSFORM OF G(I),I=0,1,...,LG AT
C     FREQUENCIES K/(2*LF),K=0,1,...,LF AND RETURNS COSIN TRANSFORM IN
C     FC(K) AND SIN TRANSFORM IN FS(K).
c     INTEGER lf
c     DOUBLE PRECISION alf
      DOUBLE PRECISION PI
      PARAMETER(PI=3.14159265358979d0)
      DIMENSION G(*),Fc(*),Fs(*),Frq(*)
      lg=Lgp1-1
c      lf=Lf1-1
      cst0=0.0D-00
C     REVERSAL OF G(I),I=1,...,LGP1 INTO G(LG3-I)   LG3=LGP1+1
      IF(Lgp1.gt.1)THEN
       lg3=Lgp1+1
       lg4=Lgp1/2
       DO i=1,lg4
        i2=lg3-i
        t=G(i)
        G(i)=G(i2)
        G(i2)=t
       END DO
      END IF
c      pi=3.14159265358979d0
c      alf=lf
      t=PI*2
      DO k=1,Lf1
       tk=t*Frq(k)
       ck=cos(tk)
       sk=sin(tk)
       ck2=ck+ck
       um2=cst0
       um1=cst0
       IF(lg.ne.0)THEN
        DO i=1,lg
         um0=ck2*um1-um2+G(i)
         um2=um1
         um1=um0
        END DO
       END IF
       Fc(k)=ck*um1-um2+G(Lgp1)
       Fs(k)=sk*um1
      END DO
      RETURN
      END
