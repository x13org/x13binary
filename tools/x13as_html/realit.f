C     Last change:  BCM  25 Nov 97    2:45 pm
      SUBROUTINE realit(Sss,Nz,Iflag)
      IMPLICIT NONE
C **********************************************************************
C *                                                                    *
C * VARIABLE- SHIFT H POLYNOMIAL ITERATION FOR A REAL ZERO.            *
C * SSS     - STARTING ITERATE                                         *
C * NZ      - NUMBER OF ZERO FOUND                                     *
C * IFLAG   - FLAG TO INDICATE A PAIR OF ZEROS NEAR REAL AXIS.         *
C *                                                                    *
C***********************************************************************
C-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'global.cmn'
C-----------------------------------------------------------------------
      DOUBLE PRECISION pv,kv,t,s,Sss,dabs,ms,mp,omp,ee
      INTEGER Nz,Iflag,i,j
C-----------------------------------------------------------------------
      Nz=0
      s=Sss
      Iflag=0
      j=0
      DO WHILE (.true.)
C-----------------------------------------------------------------------
C MAIN LOOP
C-----------------------------------------------------------------------
       pv=P0(1)
C-----------------------------------------------------------------------
C EVALUATE P0 AT S
C-----------------------------------------------------------------------
       Qp(1)=pv
       DO i=2,N0
        pv=pv*s+P0(i)
        Qp(i)=pv
       END DO
       mp=dabs(pv)
C-----------------------------------------------------------------------
C COMPUTE A RIGOROUS BOUND ON THE ERROR IN EVALUATING P0
C-----------------------------------------------------------------------
       ms=dabs(s)
       ee=(Mre/(Are+Mre))*dabs(Qp(1))
       DO i=2,N0
        ee=ee*ms+dabs(Qp(i))
       END DO
C-----------------------------------------------------------------------
C ITERATION HAS CONVERGED SUFFICIENTLY IF THE
C POLYNOMIAL VALUE IS LESS THAN 20 TIMES THIS BOUND
C-----------------------------------------------------------------------
       IF(mp.gt.20D0*((Are+Mre)*ee-Mre*mp))THEN
        j=j+1
C-----------------------------------------------------------------------
C STOP ITERATION AFTER 10 STEPS
C-----------------------------------------------------------------------
        IF(j.gt.10)RETURN
        IF(j.ge.2)THEN
         IF(dabs(t).le..001D0*dabs(s-t).and.mp.gt.omp)THEN
C-----------------------------------------------------------------------
C A CLUSTER OF ZEROS NEAR THE REAL AXIS HAS BEEN ENCOUNTERED RETURN WITH
C IFLAG SET TO INITIATE A QUADRATIC ITERATION
C-----------------------------------------------------------------------
          Iflag=1
          Sss=s
          RETURN
         END IF
        END IF
C-----------------------------------------------------------------------
C RETURN IF THE POLYNOMIAL VALUE HAS INCREASED SIGNIFICANTLY
C-----------------------------------------------------------------------
        omp=mp
C-----------------------------------------------------------------------
C COMPUTE T, THE NEXT POLYNOMIAL, AND THE NEW ITERATE
C-----------------------------------------------------------------------
        kv=K(1)
        Qk(1)=kv
        DO i=2,N
         kv=kv*s+K(i)
         Qk(i)=kv
        END DO
        IF(dabs(kv).le.dabs(K(N))*10D0*Eta)THEN
C-----------------------------------------------------------------------
C USE UNSCALED FORM
C-----------------------------------------------------------------------
         K(1)=0.0D0
         DO i=2,N
          K(i)=Qk(i-1)
         END DO
        ELSE
C-----------------------------------------------------------------------
C USE THE SCALED FORM OF THE RECURRENCE IF THE VALUE
C OF K AT S IS NONZERO
C-----------------------------------------------------------------------
         t=-pv/kv
         K(1)=Qp(1)
         DO i=2,N
          K(i)=t*Qk(i-1)+Qp(i)
         END DO
        END IF
        kv=K(1)
        DO i=2,N
         kv=kv*s+K(i)
        END DO
        t=0.D0
        IF(dabs(kv).gt.dabs(K(N))*10D0*Eta)t=-pv/kv
        s=s+t
       ELSE
        Nz=1
        Szr=s
        Szi=0.D0
        RETURN
       END IF
      END DO
      END
