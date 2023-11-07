C     Last change:  BCM  25 Nov 97    3:17 pm
**==fxshfr.f    processed by SPAG 4.03F  at 09:48 on  1 Mar 1994
      SUBROUTINE fxshfr(L2,Nz)
      IMPLICIT NONE
C **********************************************************************
C *                                                                    *
C * COMPUTES UP TO  L2  FIXED SHIFT K-POLYNOMIALS, TESTING FOR         *
C *          CONVERGENCE IN THE LINEAR OR QUADRATIC CASE.              *
C * INITIATES ONE OF THE VARIABLE SHIFT ITERATIONS AND RETURNS         *
C *          WITH THE NUMBER OF ZEROS FOUND.                           *
C * L2 - LIMIT OF FIXED SHIFT STEPS                                    *
C * NZ - NUMBER OF ZEROS FOUND                                         *
C *                                                                    *
C **********************************************************************
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'global.cmn'
c     ------------------------------------------------------------------
      DOUBLE PRECISION PT25,ZERO,ONE
      LOGICAL T,NT
      PARAMETER(T=.true.,NT=.false.,PT25=0.25D0,ZERO=0D0,ONE=1D0)
c     ------------------------------------------------------------------
      DOUBLE PRECISION svu,svv,ui,vi,s
      DOUBLE PRECISION betas,betav,oss,ovv,ss,vv,ts,tv,ots,otv,tvv,tss
      INTEGER L2,Nz,rtype,i,j,iflag
      LOGICAL vpass,spass,vtry,stry
c     ------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c     ------------------------------------------------------------------
      Nz=0
      betav=PT25
      betas=PT25
      oss=Snr
      ovv=V0
C-----------------------------------------------------------------------
C EVALUATE POLYNOMIAL BY SYNTHETIC DIVISION
C-----------------------------------------------------------------------
      CALL quadsd(N0,U,V0,P0,Qp,A0,B0)
      CALL calcsc(rtype)
      DO j=1,L2
C-----------------------------------------------------------------------
C CALCULATE NEXT K POLYNOMIAL AND ESTIMATE V0
C-----------------------------------------------------------------------
       CALL nextk(rtype)
       CALL calcsc(rtype)
       CALL newest(rtype,ui,vi)
       vv=vi
C-----------------------------------------------------------------------
C ESTIMATE S
C-----------------------------------------------------------------------
       ss=ZERO
       IF(.not.dpeq(K(N),ZERO))ss=-P0(N0)/K(N)
       tv=ONE
       ts=ONE
       IF(j.eq.1.or.rtype.eq.3)GO TO 40
C-----------------------------------------------------------------------
C COMPUTE RELATIVE MEASURES OF CONVERGENCE OF S AND V0 SEQUENCES
C-----------------------------------------------------------------------
       IF(.not.dpeq(vv,ZERO))tv=abs((vv-ovv)/vv)
       IF(.not.dpeq(ss,ZERO))ts=abs((ss-oss)/ss)
C-----------------------------------------------------------------------
C IF DECREASING, MULTIPLY TWO MOST RECENT CONVERGENCE MEASURES
C-----------------------------------------------------------------------
       tvv=ONE
       IF(tv.lt.otv)tvv=tv*otv
       tss=ONE
       IF(ts.lt.ots)tss=ts*ots
C-----------------------------------------------------------------------
C COMPARE WITH CONVERGENCE CRITERIA
C-----------------------------------------------------------------------
       vpass=tvv.lt.betav
       spass=tss.lt.betas
       IF(.not.(spass.or.vpass))GO TO 40
C-----------------------------------------------------------------------
C AT LEAST ONE SEQUENCE HAS PASSED THE CONVERGENCE TEST.
C STORE VARIABLES BEFORE ITERATING
C-----------------------------------------------------------------------
       svu=U
       svv=V0
       DO i=1,N
        Svk(i)=K(i)
       END DO
       s=ss
C-----------------------------------------------------------------------
C CHOOSE ITERATION ACCORDING TO THE FASTEST CONVERGING SEQUENCE
C-----------------------------------------------------------------------
       vtry=NT
       stry=NT
       IF(spass.and.((.not.vpass).or.tss.lt.tvv))GO TO 20
   10  CALL quadit(ui,vi,Nz)
       IF(Nz.gt.0)RETURN
C-----------------------------------------------------------------------
C QUADRATIC ITERATION HAS FAILED. FLAG THAT IT HAS BEEN TRIED AND
C DECREASE THE CONVERGENCE CRITERION.
C-----------------------------------------------------------------------
       vtry=T
       betav=betav*PT25
C-----------------------------------------------------------------------
C TRY LINEAR ITERATION IF IT HAS NOT BEEN TRIED AND THE S SEQUENCE IS
C CONVERGING
C-----------------------------------------------------------------------
       IF(stry.or.(.not.spass))GO TO 30
       DO i=1,N
        K(i)=Svk(i)
       END DO
   20  CALL realit(s,Nz,iflag)
       IF(Nz.gt.0)RETURN
C-----------------------------------------------------------------------
C LINEAR ITERATION HAS FAILED. FLAG THAT IT HAS BEEN
C TRIED AND DECREASE THE CONVERGENCE CRITERION
C-----------------------------------------------------------------------
       stry=T
       betas=betas*PT25
       IF(iflag.ne.0)THEN
C-----------------------------------------------------------------------
C IF LINEAR ITERATION SIGNALS AN ALMOST DOUBLE REAL
C ZERO ATTEMPT QUADRATIC INTERATION
C-----------------------------------------------------------------------
        ui=-(s+s)
        vi=s*s
        GO TO 10
       END IF
C-----------------------------------------------------------------------
C RESTORE VARIABLES
C-----------------------------------------------------------------------
   30  U=svu
       V0=svv
       DO i=1,N
        K(i)=Svk(i)
       END DO
C-----------------------------------------------------------------------
C TRY QUADRATIC ITERATION IF IT HAS NOT BEEN TRIED
C AND THE V0 SEQUENCE IS CONVERGING
C-----------------------------------------------------------------------
       IF(vpass.and.(.not.vtry))GO TO 10
C-----------------------------------------------------------------------
C RECOMPUTE QP AND SCALAR VALUES TO CONTINUE THE SECOND STAGE
C-----------------------------------------------------------------------
       CALL quadsd(N0,U,V0,P0,Qp,A0,B0)
       CALL calcsc(rtype)
   40  ovv=vv
       oss=ss
       otv=tv
       ots=ts
      END DO
      RETURN
      END
