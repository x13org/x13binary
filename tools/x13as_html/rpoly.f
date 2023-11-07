C     Last change:  BCM  25 Nov 97    3:27 pm
      SUBROUTINE rpoly(Op,Degree,Zeror,Zeroi,Fail)
      IMPLICIT NONE
C **********************************************************************
C *                                                                    *
C * FINDS THE ZEROS OF A REAL POLYNOMIAL                               *
C *                                                                    *
C * OP     - DOUBLE PRECISION VECTOR OF COEFFICIENTS IN ORDER OF       *
C *          DECREASING POWERS.                                        *
C * DEGREE - INTEGER DEGREE OF POLYNOMIAL.                             *
C * ZEROR  - OUTPUT DOUBLE PRECISION VECTOR OF REAL PARTS OF THE       *
C *          ZEROS.                                                    *
C * ZEROI  - OUTPUT DOUBLE PRECISION VECTOR OF IMAGINARY PARTS OF      *
C *          THE ZEROS.                                                *
C * FAIL   - OUTPUT LOGICAL PARAMETER, TRUE ONLY IF LEADING            *
C *          COEFFICIENT IS ZERO OR IF RPOLY HAS FOUND FEWER THAN      *
C *          DEGREE ZEROS. IN THE LATTER CASE DEGREE IS RESET TO       *
C *          THE NUMBER OF ZEROS FOUND.                                *
C *                                                                    *
C * TO CHANGE THE SIZE OF POLYNOMIALS WHICH CAN BE SOLVED, RESET       *
C * THE DIMENSIONS OF THE ARRAYS IN THE COMMON AREA AND IN THE         *
C * FOLLOWING DECLARATIONS. THE SUBROUTINE USES SINGLE PRECISION       *
C * CALCULATIONS FOR SCALING, BOUNDS AND ERROR CALCULATIONS. ALL       *
C * CALCULATIONS FOR THE ITERATIONS ARE DONE IN DOUBLE PRECISION.      *
C *                                                                    *
C * ********************************************************************
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'global.cmn'
C-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO,TEN,ONE
      PARAMETER(ZERO=0D0,TEN=10D0,ONE=1D0)
C-----------------------------------------------------------------------
      DOUBLE PRECISION Op,temp,Zeror,Zeroi,t,aa,bb,cc,dabs,factor
      DOUBLE PRECISION lo,xmax,xmin,xx,yy,cosr,sinr,xxx,x,bnd,xm,ff,df,
     &                 dx,pt,sc,base,infin,smalno
      INTEGER Degree,cnt,nz,i,j,jj,nm1,l
      LOGICAL Fail,zerok
      DIMENSION Op(PORDER+1),temp(PORDER+1),Zeror(PORDER),
     &          pt(PORDER+1),Zeroi(PORDER)
C-----------------------------------------------------------------------
c     DOUBLE PRECISION dpmpar
c     EXTERNAL dpmpar
      LOGICAL dpeq
      EXTERNAL dpeq
C-----------------------------------------------------------------------
C THE FOLLOWING STATEMENTS SET MACHINE CONSTANTS USED IN VARIOUS PARTS
C OF THE PROGRAM. THE MEANING OF THE FOUR CONSTANTS ARE...
C ETA     THE MAXIMUM RELATIVE REPRESENTATION ERROR WHICH CAN BE
C         DESCRIBED AS THE SMALLEST POSITIVE FLOATING POINT NUMBER SUCH
C         THAT 1.D0+ETA IS GREATER THAN 1.
C INFINY  THE LARGEST FLOATING-POINT NUMBER.
C SMALNO  THE SMALLEST POSITIVE FLOATING-POINT NUMBER IF THE EXPONENT
C         RANGE DIFFERS IN SINGLE AND DOUBLE PRECISION THEN SMALNO AND
C         INFINY SHOULD INDICATE THE SMALLER RANGE.
C BASE    THE BASE OF THE FLOATING-POINT NUMBER SYSTEM USED.
C THE VALUES BELOW CORRESPOND TO THE BURROUGHS B6700
C-----------------------------------------------------------------------
C
C The following constants came from page 9 of "Numerical Computation
C Guide" by Sun Workstation
C
      base=TEN
      Eta=.5D0*base**(1-15)
      infin=1.797D30
      smalno=1.0D-38
C The following numbers correspondent to B6700
c     base=8.
c     Eta=.5*base**(1-26)
c     infin=4.3E68
c     smalno=1.0E-45
c
c      Eta=dpmpar(1)
c      infin=dpmpar(3)
c      smalno=dpmpar(2)
C-----------------------------------------------------------------------
C ARE AND MRE REFER TO THE UNIT ERROR IN + AND * RESPECTIVELY.
C THEY ARE ASSUMED TO BE THE SAME AS ETA.
C-----------------------------------------------------------------------
      Are=Eta
      Mre=Eta
      lo=smalno/Eta
C-----------------------------------------------------------------------
C INITIALIZATION OF CONSTANTS FOR SHIFT ROTATION
C-----------------------------------------------------------------------
      xx=.70710678D0
      yy=-xx
      cosr=-.069756474D0
      sinr=.99756405D0
      Fail=.false.
      N=Degree
      N0=N+1
C-----------------------------------------------------------------------
C ALGORITHM FAILS IF THE LEADING COEFFICIENT IS ZERO.
C-----------------------------------------------------------------------
      IF(.not.dpeq(Op(1),ZERO))THEN
C-----------------------------------------------------------------------
C REMOVE THE ZEROS AT THE ORIGIN IF ANY
C-----------------------------------------------------------------------
       DO WHILE (dpeq(Op(N0),ZERO))
        j=Degree-N+1
        Zeror(j)=ZERO
        Zeroi(j)=ZERO
        N0=N0-1
        N=N-1
       END DO
C-----------------------------------------------------------------------
C MAKE A COPY OF THE COEFFICIENTS
C-----------------------------------------------------------------------
       DO i=1,N0
        P0(i)=Op(i)
       END DO
      ELSE
       Fail=.true.
       Degree=0
       RETURN
      END IF
C-----------------------------------------------------------------------
C START THE ALGORITHM FOR ONE ZERO
C-----------------------------------------------------------------------
   10 IF(N.gt.2)THEN
C-----------------------------------------------------------------------
C FIND LARGEST AND SMALLEST MODULI OF COEFFICIENTS.
C-----------------------------------------------------------------------
       xmax=ZERO
       xmin=infin
c     ------------------------------------------------------------------
       DO i=1,N0
        x=abs(P0(i))
        IF(x.gt.xmax)xmax=x
        IF((.not.dpeq(x,ZERO)).and.x.lt.xmin)xmin=x
       END DO
c     ------------------------------------------------------------------
      ELSE
       IF(N.lt.1)RETURN
C-----------------------------------------------------------------------
C CALCULATE THE FINAL ZERO OR PAIR OF ZEROS
C-----------------------------------------------------------------------
       IF(N.eq.2)THEN
        CALL quad(P0(1),P0(2),P0(3),Zeror(Degree-1),Zeroi(Degree-1),
     &            Zeror(Degree),Zeroi(Degree))
c     ------------------------------------------------------------------
       ELSE
        Zeror(Degree)=-P0(2)/P0(1)
        Zeroi(Degree)=ZERO
       END IF
       RETURN
      END IF
C-----------------------------------------------------------------------
C SCALE IF THERE ARE LARGE OR VERY SMALL COEFFICIENTS COMPUTES A SCALE
C FACTOR TO MULTIPLY THE COEFFICIENTS OF THE POLYNOMIAL. THE SCALING IS
C DONE TO AVOID OVERFLOW AND TO AVOID UNDETECTED UNDERFLOW INTERFERING
C WITH THE CONVERGENCE CRITERION. THE FACTOR IS A POWER OF THE BASE
C-----------------------------------------------------------------------
      sc=lo/xmin
c     ------------------------------------------------------------------
      IF(sc.le.ONE)THEN
       IF(xmax.lt.TEN)GO TO 20
       IF(dpeq(sc,ZERO))sc=smalno
c     ------------------------------------------------------------------
      ELSE IF(infin/sc.lt.xmax)THEN
       GO TO 20
      END IF
c     ------------------------------------------------------------------
      l=idint(dlog(sc)/dlog(base)+.5D0)
      factor=(base*ONE)**l
c     ------------------------------------------------------------------
      IF(.not.dpeq(factor,ONE))THEN
       DO i=1,N0
        P0(i)=factor*P0(i)
       END DO
      END IF
C-----------------------------------------------------------------------
C COMPUTE LOWER BOUND ON MODULI OF ZEROS.
C-----------------------------------------------------------------------
   20 DO i=1,N0
       pt(i)=abs(P0(i))
      END DO
      pt(N0)=-pt(N0)
C-----------------------------------------------------------------------
C COMPUTE UPPER ESTIMATE OF BOUND
C-----------------------------------------------------------------------
      x=exp((dlog(-pt(N0))-dlog(pt(1)))/dble(N))
C-----------------------------------------------------------------------
C IF NEWTON STEP AT THE ORIGIN IS BETTER, USE IT.
C-----------------------------------------------------------------------
      IF(.not.dpeq(pt(N),ZERO))THEN
       xm=-pt(N0)/pt(N)
       IF(xm.lt.x)x=xm
      END IF
C-----------------------------------------------------------------------
C     CHOP THE INTERVAL (0,X) UNTIL FF .LE. 0
C-----------------------------------------------------------------------
      DO WHILE (.true.)
       xm=x*.1D0
       ff=pt(1)
       DO i=2,N0
        ff=ff*xm+pt(i)
       END DO
c     ------------------------------------------------------------------
       IF(ff.le.ZERO)GO TO 30
       x=xm
      END DO
C-----------------------------------------------------------------------
C     DO NEWTON ITERATION UNTIL X CONVERGES TO TWO DECIMAL PLACES
C-----------------------------------------------------------------------
   30 dx=x
      DO WHILE (abs(dx/x).gt..005D0)
       ff=pt(1)
       df=ff
c     ------------------------------------------------------------------
       DO i=2,N
        ff=ff*x+pt(i)
        df=df*x+ff
       END DO
c     ------------------------------------------------------------------
       ff=ff*x+pt(N0)
       dx=ff/df
       x=x-dx
      END DO
      bnd=x
C-----------------------------------------------------------------------
C COMPUTE THE DERIVATIVE AS THE INTIAL K POLYNOMIAL AND
C DO 5 STEPS WITH NO SHIFT
C-----------------------------------------------------------------------
      nm1=N-1
      DO i=2,N
       K(i)=dble(N0-i)*P0(i)/dble(N)
      END DO
c     ------------------------------------------------------------------
      K(1)=P0(1)
      aa=P0(N0)
      bb=P0(N)
      zerok=dpeq(K(N),ZERO)
c     ------------------------------------------------------------------
      DO jj=1,5
       cc=K(N)
       IF(zerok)THEN
C-----------------------------------------------------------------------
C USE UNSCALED FORM OF RECURRENCE
C-----------------------------------------------------------------------
        DO i=1,nm1
         j=N0-i
         K(j)=K(j-1)
        END DO
        K(1)=ZERO
        zerok=dpeq(K(N),ZERO)
       ELSE
C-----------------------------------------------------------------------
C USE SCALED FORM OF RECURRENCE IF VALUE OF K AT 0 IS NONZERO
C-----------------------------------------------------------------------
        t=-aa/cc
        DO i=1,nm1
         j=N0-i
         K(j)=t*K(j-1)+P0(j)
        END DO
        K(1)=P0(1)
        zerok=dabs(K(N)).le.dabs(bb)*Eta*TEN
       END IF
      END DO
C-----------------------------------------------------------------------
C SAVE K FOR RESTARTS WITH NEW SHIFTS
C-----------------------------------------------------------------------
      DO i=1,N
       temp(i)=K(i)
      END DO
C-----------------------------------------------------------------------
C LOOP TO SELECT THE QUADRATIC  CORRESPONDING TO EACH NEW SHIFT
C-----------------------------------------------------------------------
      DO cnt=1,20
C-----------------------------------------------------------------------
C QUADRATIC CORRESPONDS TO A DOUBLE SHIFT TO A NON-REAL POINT AND ITS
C COMPLEX CONJUGATE. THE POINT HAS MODULUS BND AND AMPLITUDE ROTATED BY
C 94 DEGREES FROM THE PREVIOUS SHIFT
C-----------------------------------------------------------------------
       xxx=cosr*xx-sinr*yy
       yy=sinr*xx+cosr*yy
       xx=xxx
       Snr=bnd*xx
       Sni=bnd*yy
       U=-2.0D0*Snr
       V0=bnd
C-----------------------------------------------------------------------
C SECOND STAGE CALCULATION, FIXED QUADRATIC
C-----------------------------------------------------------------------
       CALL fxshfr(20*cnt,nz)
       IF(nz.eq.0)THEN
C-----------------------------------------------------------------------
C IF THE ITERATION IS UNSUCCESSFUL ANOTHER QUADRATIC
C IS CHOSEN AFTER RESTORING K
C-----------------------------------------------------------------------
        DO i=1,N
         K(i)=temp(i)
        END DO
       ELSE
C-----------------------------------------------------------------------
C THE SECOND STAGE JUMPS DIRECTLY TO ONE OF THE THIRD STAGE ITERATIONS
C AND RETURNS HERE IF SUCCESSFUL. DEFLATE THE POLYNOMIAL, STORE THE ZERO
C OR ZEROS AND RETURN TO THE MAIN ALGORITHM.
C-----------------------------------------------------------------------
        j=Degree-N+1
        Zeror(j)=Szr
        Zeroi(j)=Szi
        N0=N0-nz
        N=N0-1
        DO i=1,N0
         P0(i)=Qp(i)
        END DO
        IF(nz.ne.1)THEN
         Zeror(j+1)=Lzr
         Zeroi(j+1)=Lzi
        END IF
        GO TO 10
       END IF
      END DO
C-----------------------------------------------------------------------
C RETURN WITH FAILURE IF NO CONVERGENCE WITH 20 SHIFTS
C-----------------------------------------------------------------------
      Fail=.true.
      Degree=Degree-N
      RETURN
      END
