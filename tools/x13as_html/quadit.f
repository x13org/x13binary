C     Last change:  BCM  25 Nov 97    2:45 pm
      SUBROUTINE quadit(Uu,Vv,Nz)
      IMPLICIT NONE
C **********************************************************************
C *                                                                    *
C * VARIABLE- SHIFT K-POLYNOMIAL ITERATION FOR A QUADRATIC FACTOR      *
C *           CONVERGES ONLY IF THE ZEROS ARE EQUIMODULAR OR NEARLY SO.*
C * UU,VV   - COEFFICIENTS OF STARTING QUADRATIC                       *
C * NZ      - NUMBER OF ZERO FOUND                                     *
C *                                                                    *
C **********************************************************************
      DOUBLE PRECISION ZERO,ONE,TWO,FOUR,FIVE,TWNTY
      PARAMETER(ZERO=0D0,ONE=1D0,TWO=2D0,FOUR=4D0,FIVE=5D0,TWNTY=20D0)
C-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'global.cmn'
C-----------------------------------------------------------------------
      DOUBLE PRECISION ui,vi,Uu,Vv,dabs
      DOUBLE PRECISION mp,omp,ee,relstp,t,zm
      INTEGER Nz,type,i,j
      LOGICAL tried
C-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
C-----------------------------------------------------------------------
      Nz=0
      tried=.false.
      U=Uu
      V0=Vv
      j=0
      DO WHILE (.true.)
C-----------------------------------------------------------------------
C MAIN LOOP
C-----------------------------------------------------------------------
       CALL quad(ONE,U,V0,Szr,Szi,Lzr,Lzi)
C-----------------------------------------------------------------------
C RETURN IF ROOTS OF THE QUADRATIC ARE REAL AND NOT CLOSE TO MULTIPLE OR
C NEARLY EQUAL AND  OF OPPOSITE SIGN
C-----------------------------------------------------------------------
       IF(dabs(dabs(Szr)-dabs(Lzr)).gt..01D0*dabs(Lzr))RETURN
C-----------------------------------------------------------------------
C EVALUATE POLYNOMIAL BY QUADRATIC SYNTHETIC DIVISION
C-----------------------------------------------------------------------
       CALL quadsd(N0,U,V0,P0,Qp,A0,B0)
       mp=dabs(A0-Szr*B0)+dabs(Szi*B0)
C-----------------------------------------------------------------------
C COMPUTE A RIGOROUS  BOUND ON THE ROUNDING ERROR IN EVALUTING P0
C-----------------------------------------------------------------------
       zm=sqrt(dabs(V0))
       ee=TWO*dabs(Qp(1))
       t=-Szr*B0
       DO i=2,N
        ee=ee*zm+dabs(Qp(i))
       END DO
       ee=ee*zm+dabs(A0+t)
       ee=(FIVE*Mre+FOUR*Are)*ee-(FIVE*Mre+TWO*Are)
     &    *(dabs(A0+t)+dabs(B0)*zm)+TWO*Are*dabs(t)
C-----------------------------------------------------------------------
C ITERATION HAS CONVERGED SUFFICIENTLY IF THE POLYNOMIAL VALUE IS LESS
C THAN 20 TIMES THIS BOUND
C-----------------------------------------------------------------------
       IF(mp.gt.TWNTY*ee)THEN
        j=j+1
C-----------------------------------------------------------------------
C STOP ITERATION AFTER 20 STEPS
C-----------------------------------------------------------------------
        IF(j.gt.20)RETURN
        IF(j.ge.2)THEN
         IF(.not.(relstp.gt..01D0.or.mp.lt.omp.or.tried))THEN
C-----------------------------------------------------------------------
C A CLUSTER APPEARS TO BE STALLING THE CONVERGENCE.
C FIVE FIXED SHIFT STEPS ARE TAKEN WITH A U,V0 CLOSE TO THE CLUSTER
C-----------------------------------------------------------------------
          IF(relstp.lt.Eta)relstp=Eta
          relstp=sqrt(relstp)
          U=U-U*relstp
          V0=V0+V0*relstp
          CALL quadsd(N0,U,V0,P0,Qp,A0,B0)
          DO i=1,5
           CALL calcsc(type)
           CALL nextk(type)
          END DO
          tried=.true.
          j=0
         END IF
        END IF
        omp=mp
C-----------------------------------------------------------------------
C CALCULATE NEXT K POLYNOMIAL AND NEW U AND V0
C-----------------------------------------------------------------------
        CALL calcsc(type)
        CALL nextk(type)
        CALL calcsc(type)
        CALL newest(type,ui,vi)
C-----------------------------------------------------------------------
C IF VI IS ZERO THE ITERATION IS NOT CONVERGING
C-----------------------------------------------------------------------
        IF(dpeq(vi,ZERO))RETURN
        relstp=dabs((vi-V0)/vi)
        U=ui
        V0=vi
       ELSE
        Nz=2
        RETURN
       END IF
      END DO
      END
