C     Last change:  BCM  29 Sep 97    8:55 am
c-----------------------------------------------------------------------
c     lmdif.f, Release 1, Subroutine Version 1.9, Modified 17 Feb 1995.
c-----------------------------------------------------------------------
C
C     SUBROUTINE LMDIF
C
C     THE PURPOSE OF LMDIF IS TO MINIMIZE THE SUM OF THE SQUARES OF
C     M NONLINEAR FUNCTIONS IN N VARIABLES BY A MODIFICATION OF
C     THE LEVENBERG-MARQUARDT ALGORITHM. THE USER MUST PROVIDE A
C     SUBROUTINE WHICH CALCULATES THE FUNCTIONS. THE JACOBIAN IS
C     THEN CALCULATED BY A FORWARD-DIFFERENCE APPROXIMATION.
C
C     THE SUBROUTINE STATEMENT IS
C
C       SUBROUTINE LMDIF(FCN,M,N,X,FVEC,FTOL,XTOL,GTOL,MXiter,EPSFCN,
C                        DIAG,MODE,FACTOR,NPRINT,INFO,nliter,NFEV,FJAC,
C                        LDFJAC,IPVT,QTF,WA1,WA2,WA3,WA4)
C
C     WHERE
C
C       FCN IS THE NAME OF THE USER-SUPPLIED SUBROUTINE WHICH
C         CALCULATES THE FUNCTIONS. FCN MUST BE DECLARED
C         IN AN EXTERNAL STATEMENT IN THE USER CALLING
C         PROGRAM, AND SHOULD BE WRITTEN AS FOLLOWS.
c         lckinv is added to constrain the estimation inside the
c         invertibility and stationarity regions.
C
C         SUBROUTINE FCN(M,N,X,FVEC,IFLAG,lchkinv)
C         INTEGER M,N,IFLAG
C         DOUBLE PRECISION X(N),FVEC(M)
C         ----------
C         CALCULATE THE FUNCTIONS AT X AND
C         RETURN THIS VECTOR IN FVEC.
C         ----------
C         RETURN
C         END
C
C         THE VALUE OF IFLAG SHOULD NOT BE CHANGED BY FCN UNLESS
C         THE USER WANTS TO TERMINATE EXECUTION OF LMDIF.
C         IN THIS CASE SET IFLAG TO A NEGATIVE INTEGER.
C
C       M IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER
C         OF FUNCTIONS.
C
C       N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER
C         OF VARIABLES. N MUST NOT EXCEED M.
C
C       X IS AN ARRAY OF LENGTH N. ON INPUT X MUST CONTAIN
C         AN INITIAL ESTIMATE OF THE SOLUTION VECTOR. ON OUTPUT X
C         CONTAINS THE FINAL ESTIMATE OF THE SOLUTION VECTOR.
C
C       FVEC IS AN OUTPUT ARRAY OF LENGTH M WHICH CONTAINS
C         THE FUNCTIONS EVALUATED AT THE OUTPUT X.
C
C       FTOL IS A NONNEGATIVE INPUT VARIABLE. TERMINATION
C         OCCURS WHEN BOTH THE ACTUAL AND PREDICTED RELATIVE
C         REDUCTIONS IN THE SUM OF SQUARES ARE AT MOST FTOL.
C         THEREFORE, FTOL MEASURES THE RELATIVE ERROR DESIRED
C         IN THE SUM OF SQUARES.
C
C       XTOL IS A NONNEGATIVE INPUT VARIABLE. TERMINATION
C         OCCURS WHEN THE RELATIVE ERROR BETWEEN TWO CONSECUTIVE
C         ITERATES IS AT MOST XTOL. THEREFORE, XTOL MEASURES THE
C         RELATIVE ERROR DESIRED IN THE APPROXIMATE SOLUTION.
C
C       GTOL IS A NONNEGATIVE INPUT VARIABLE. TERMINATION
C         OCCURS WHEN THE COSINE OF THE ANGLE BETWEEN FVEC AND
C         ANY COLUMN OF THE JACOBIAN IS AT MOST GTOL IN ABSOLUTE
C         VALUE. THEREFORE, GTOL MEASURES THE ORTHOGONALITY
C         DESIRED BETWEEN THE FUNCTION VECTOR AND THE COLUMNS
C         OF THE JACOBIAN.
C
C       MAXFEV IS A POSITIVE INTEGER INPUT VARIABLE. TERMINATION
C         OCCURS WHEN THE NUMBER OF CALLS TO FCN IS AT LEAST
C         MAXFEV BY THE END OF AN ITERATION.  Is equal to 
C         max(Mxiter,200)*(n+1).
C
C       EPSFCN IS AN INPUT VARIABLE USED IN DETERMINING A SUITABLE
C         STEP LENGTH FOR THE FORWARD-DIFFERENCE APPROXIMATION. THIS
C         APPROXIMATION ASSUMES THAT THE RELATIVE ERRORS IN THE
C         FUNCTIONS ARE OF THE ORDER OF EPSFCN. IF EPSFCN IS LESS
C         THAN THE MACHINE PRECISION, IT IS ASSUMED THAT THE RELATIVE
C         ERRORS IN THE FUNCTIONS ARE OF THE ORDER OF THE MACHINE
C         PRECISION.
C
C       DIAG IS AN ARRAY OF LENGTH N. IF MODE = 1 (SEE
C         BELOW), DIAG IS INTERNALLY SET. IF MODE = 2, DIAG
C         MUST CONTAIN POSITIVE ENTRIES THAT SERVE AS
C         MULTIPLICATIVE SCALE FACTORS FOR THE VARIABLES.
C
C       MODE IS AN INTEGER INPUT VARIABLE. IF MODE = 1, THE
C         VARIABLES WILL BE SCALED INTERNALLY. IF MODE = 2,
C         THE SCALING IS SPECIFIED BY THE INPUT DIAG. OTHER
C         VALUES OF MODE ARE EQUIVALENT TO MODE = 1.
C
C       FACTOR IS A POSITIVE INPUT VARIABLE USED IN DETERMINING THE
C         INITIAL STEP BOUND. THIS BOUND IS SET TO THE PRODUCT OF
C         FACTOR AND THE EUCLIDEAN NORM OF DIAG*X IF NONZERO, OR ELSE
C         TO FACTOR ITSELF. IN MOST CASES FACTOR SHOULD LIE IN THE
C         INTERVAL (.1,100.). 100. IS A GENERALLY RECOMMENDED VALUE.
C
C       NPRINT IS AN INTEGER INPUT VARIABLE THAT ENABLES CONTROLLED
C         PRINTING OF ITERATES IF IT IS POSITIVE. IN THIS CASE,
C         FCN IS CALLED WITH IFLAG = 0 AT THE BEGINNING OF THE FIRST
C         ITERATION AND EVERY NPRINT ITERATIONS THEREAFTER AND
C         IMMEDIATELY PRIOR TO RETURN, WITH X AND FVEC AVAILABLE
C         FOR PRINTING. IF NPRINT IS NOT POSITIVE, NO SPECIAL CALLS
C         OF FCN WITH IFLAG = 0 ARE MADE.
C
C       INFO IS AN INTEGER OUTPUT VARIABLE. IF THE USER HAS
C         TERMINATED EXECUTION, INFO IS SET TO THE (NEGATIVE)
C         VALUE OF IFLAG. SEE DESCRIPTION OF FCN. OTHERWISE,
C         INFO IS SET AS FOLLOWS.
C
C         INFO = 0  IMPROPER INPUT PARAMETERS.
C
C         INFO = 1  BOTH ACTUAL AND PREDICTED RELATIVE REDUCTIONS
C                   IN THE SUM OF SQUARES ARE AT MOST FTOL.
C
C         INFO = 2  RELATIVE ERROR BETWEEN TWO CONSECUTIVE ITERATES
C                   IS AT MOST XTOL.
C
C         INFO = 3  CONDITIONS FOR INFO = 1 AND INFO = 2 BOTH HOLD.
C
C         INFO = 4  THE COSINE OF THE ANGLE BETWEEN FVEC AND ANY
C                   COLUMN OF THE JACOBIAN IS AT MOST GTOL IN
C                   ABSOLUTE VALUE.
C
C         INFO = 5  NUMBER OF CALLS TO FCN HAS REACHED OR
C                   EXCEEDED MAXFEV or mxiter iterations have been
c                   reached.
C
C         INFO = 6  FTOL IS TOO SMALL. NO FURTHER REDUCTION IN
C                   THE SUM OF SQUARES IS POSSIBLE.
C
C         INFO = 7  XTOL IS TOO SMALL. NO FURTHER IMPROVEMENT IN
C                   THE APPROXIMATE SOLUTION X IS POSSIBLE.
C
C         INFO = 8  GTOL IS TOO SMALL. FVEC IS ORTHOGONAL TO THE
C                   COLUMNS OF THE JACOBIAN TO MACHINE PRECISION.
C
C       NFEV IS AN INTEGER OUTPUT VARIABLE SET TO THE NUMBER OF
C         cumulative function evaluations.
C
C       FJAC IS AN OUTPUT M BY N ARRAY. THE UPPER N BY N SUBMATRIX
C         OF FJAC CONTAINS AN UPPER TRIANGULAR MATRIX R WITH
C         DIAGONAL ELEMENTS OF NONINCREASING MAGNITUDE SUCH THAT
C
C                T     T           T
C               P *(JAC *JAC)*P = R *R,
C
C         WHERE P IS A PERMUTATION MATRIX AND JAC IS THE FINAL
C         CALCULATED JACOBIAN. COLUMN J OF P IS COLUMN IPVT(J)
C         (SEE BELOW) OF THE IDENTITY MATRIX. THE LOWER TRAPEZOIDAL
C         PART OF FJAC CONTAINS INFORMATION GENERATED DURING
C         THE COMPUTATION OF R.
C
C       LDFJAC IS A POSITIVE INTEGER INPUT VARIABLE NOT LESS THAN M
C         WHICH SPECIFIES THE LEADING DIMENSION OF THE ARRAY FJAC.
C
C       IPVT IS AN INTEGER OUTPUT ARRAY OF LENGTH N. IPVT
C         DEFINES A PERMUTATION MATRIX P SUCH THAT JAC*P = Q*R,
C         WHERE JAC IS THE FINAL CALCULATED JACOBIAN, Q IS
C         ORTHOGONAL (NOT STORED), AND R IS UPPER TRIANGULAR
C         WITH DIAGONAL ELEMENTS OF NONINCREASING MAGNITUDE.
C         COLUMN J OF P IS COLUMN IPVT(J) OF THE IDENTITY MATRIX.
C
C       QTF IS AN OUTPUT ARRAY OF LENGTH N WHICH CONTAINS
C         THE FIRST N ELEMENTS OF THE VECTOR (Q TRANSPOSE)*FVEC.
C
C       WA1, WA2, AND WA3 ARE WORK ARRAYS OF LENGTH N.
C
C       WA4 IS A WORK ARRAY OF LENGTH M.
C
C     SUBPROGRAMS CALLED
C
C       USER-SUPPLIED ...... FCN
C
C       MINPACK-SUPPLIED ... DPMPAR,ENORM,FDJAC2,LMPAR,QRFAC
C
C       FORTRAN-SUPPLIED ... DABS,DMAX1,DMIN1,DSQRT,MOD
C
C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. MARCH 1980.
C     BURTON S. GARBOW, KENNETH E. HILLSTROM, JORGE J. MORE
C
C     **********
      SUBROUTINE lmdif(fcn,M,N,X,Fvec,Lauto,Gudrun,Ftol,Xtol,Gtol,
     &                 Mxiter,Epsfcn,Diag,Mode,Factor,Nprint,Info,
     &                 Nliter,Nfev,Fjac,Ldfjac,Ipvt,Qtf,Wa1,Wa2,Wa3,
     &                  Wa4)
      IMPLICIT NONE
      INCLUDE 'error.cmn'
*      INCLUDE 'units.cmn'
      INTEGER begitr,Nliter,Mxiter,oldfev
      INTEGER M,N,maxfev,Mode,Nprint,Info,Nfev,Ldfjac
      INTEGER Ipvt(N)
      LOGICAL Lauto,Gudrun,T,F,dpeq
      DOUBLE PRECISION Ftol,Xtol,Gtol,Epsfcn,Factor
      DOUBLE PRECISION X(N),Fvec(M),Diag(N),Fjac(Ldfjac,N),Qtf(N),Wa1(N)
     &                 ,Wa2(N),Wa3(N),Wa4(M)
      EXTERNAL fcn,dpeq
      INTEGER i,iflag,j,l
      DOUBLE PRECISION actred,delta,dirder,epsmch,fnorm,fnorm1,gnorm,
     &                 ONE,par,pnorm,prered,P1,P5,P25,P75,P0001,ratio,
     &                 sum,temp,temp1,temp2,xnorm,MONE,ZERO
      DOUBLE PRECISION dpmpar,enorm
      PARAMETER(ONE=1.0D0,P1=.1D0,P5=.5D0,P25=.25D0,P75=.75D0,
     &          P0001=.0001D0,MONE=-1.0D0,ZERO=0.0D0,T=.true.,F=.false.)
C
C     EPSMCH IS THE MACHINE PRECISION.
C
      epsmch=dpmpar(1)
C
      Info=0
      iflag=0
c      NFEV=0
      oldfev=Nfev
C
C     CHECK THE INPUT PARAMETERS FOR ERRORS.
C
      IF(N.gt.0.and.M.ge.N.and.Ldfjac.ge.M.and.Ftol.ge.ZERO.and.
     &   Xtol.ge.ZERO.and.Gtol.ge.ZERO.and.Mxiter.ge.0.and.
     &   Factor.gt.ZERO)THEN
c
c     Set the maximum number of function calls
c
       maxfev=max(Mxiter,200)*(N+1)
c
       IF(Mode.eq.2)THEN
        DO j=1,N
         IF(Diag(j).le.ZERO)GO TO 20
        END DO
       END IF
C
C     EVALUATE THE FUNCTION AT THE STARTING POINT
C     AND CALCULATE ITS NORM.
C
       iflag=1
       CALL fcn(M,N,X,Fvec,Lauto,Gudrun,iflag,F)
c      NFEV=1
       Nfev=Nfev+1
       IF(iflag.ge.0)THEN
        fnorm=enorm(M,Fvec)
C
C     INITIALIZE LEVENBERG-MARQUARDT PARAMETER AND ITERATION COUNTER.
C
        begitr=Nliter
        par=ZERO
        DO WHILE (T)
C
C     BEGINNING OF THE OUTER LOOP.
C
C
C        CALCULATE THE JACOBIAN MATRIX.
C
         iflag=2
         CALL fdjac2(fcn,M,N,X,Fvec,Lauto,Gudrun,Fjac,Ldfjac,iflag,
     &               Epsfcn,Wa4,F)
         Nfev=Nfev+N
c-----------------------------------------------------------------------
c     Update the parameters in the kalman filter routine to match the
c original parameter estimates, not the last displacement in the
c jacobian calculation.
c-----------------------------------------------------------------------
         CALL upespm(X)
c     ------------------------------------------------------------------
         IF(iflag.lt.0)GO TO 20
C
C        IF REQUESTED, CALL FCN TO ENABLE PRINTING OF ITERATES.
C
         IF(Nprint.gt.0.and.Nliter.gt.begitr)THEN
          CALL prtitr(Fvec,M,X,N,'ARMA',Nliter,Nfev)
          IF(Lfatal)RETURN
         END IF
C
C        COMPUTE THE QR FACTORIZATION OF THE JACOBIAN.
C
         CALL qrfac(M,N,Fjac,Ldfjac,T,Ipvt,N,Wa1,Wa2,Wa3)
C
C        ON THE FIRST ITERATION AND IF MODE IS 1, SCALE ACCORDING
C        TO THE NORMS OF THE COLUMNS OF THE INITIAL JACOBIAN.
C
         IF(Nliter.eq.begitr)THEN
          IF(Mode.ne.2)THEN
           DO j=1,N
            Diag(j)=Wa2(j)
            IF(dpeq(Wa2(j),ZERO))Diag(j)=ONE
           END DO
          END IF
C
C        ON THE FIRST ITERATION, CALCULATE THE NORM OF THE SCALED X
C        AND INITIALIZE THE STEP BOUND DELTA.
C
          DO j=1,N
           Wa3(j)=Diag(j)*X(j)
          END DO
          xnorm=enorm(N,Wa3)
          delta=Factor*xnorm
          IF(dpeq(delta,ZERO))delta=Factor
         END IF
C
C        FORM (Q TRANSPOSE)*FVEC AND STORE THE FIRST N COMPONENTS IN
C        QTF.
C
         DO i=1,M
          Wa4(i)=Fvec(i)
         END DO
         DO j=1,N
          IF(.not.dpeq(Fjac(j,j),ZERO))THEN
           sum=ZERO
           DO i=j,M
            sum=sum+Fjac(i,j)*Wa4(i)
           END DO
           temp=-sum/Fjac(j,j)
           DO i=j,M
            Wa4(i)=Wa4(i)+Fjac(i,j)*temp
           END DO
          END IF
          Fjac(j,j)=Wa1(j)
          Qtf(j)=Wa4(j)
         END DO
C
C        COMPUTE THE NORM OF THE SCALED GRADIENT.
C
         gnorm=ZERO
         IF(.not.dpeq(fnorm,ZERO))THEN
          DO j=1,N
           l=Ipvt(j)
           IF(.not.dpeq(Wa2(l),ZERO))THEN
            sum=ZERO
            DO i=1,j
             sum=sum+Fjac(i,j)*(Qtf(i)/fnorm)
            END DO
            gnorm=dmax1(gnorm,dabs(sum/Wa2(l)))
           END IF
          END DO
         END IF
C
C        TEST FOR CONVERGENCE OF THE GRADIENT NORM.
C
         IF(gnorm.le.Gtol)Info=4
*         write(mtprof,9004)' within lmdif (4):',gnorm,Gtol
         IF(Info.ne.0)GO TO 20
C
C        RESCALE IF NECESSARY.
C
         IF(Mode.ne.2)THEN
          DO j=1,N
           Diag(j)=dmax1(Diag(j),Wa2(j))
          END DO
         END IF
         DO WHILE (T)
C
C        BEGINNING OF THE INNER LOOP.
C
C
C           DETERMINE THE LEVENBERG-MARQUARDT PARAMETER.
C
          CALL lmpar(N,Fjac,Ldfjac,Ipvt,Diag,Qtf,delta,par,Wa1,Wa2,Wa3,
     &               Wa4)
C
C           STORE THE DIRECTION P AND X + P. CALCULATE THE NORM OF P.
C
          DO j=1,N
           Wa1(j)=-Wa1(j)
           Wa2(j)=X(j)+Wa1(j)
           Wa3(j)=Diag(j)*Wa1(j)
          END DO
          pnorm=enorm(N,Wa3)
C
C           ON THE FIRST ITERATION, ADJUST THE INITIAL STEP BOUND.
C
          IF(Nliter.eq.begitr)delta=dmin1(delta,pnorm)
C
C           EVALUATE THE FUNCTION AT X+P AND CALCULATE ITS NORM.
C
          iflag=1
          CALL fcn(M,N,Wa2,Wa4,Lauto,Gudrun,iflag,T)
          Nfev=Nfev+1
          IF(iflag.lt.0)GO TO 20
          fnorm1=enorm(M,Wa4)
C
C           COMPUTE THE SCALED ACTUAL REDUCTION.
C
          actred=MONE
          IF(P1*fnorm1.lt.fnorm)actred=ONE-(fnorm1/fnorm)**2
C
C           COMPUTE THE SCALED PREDICTED REDUCTION AND
C           THE SCALED DIRECTIONAL DERIVATIVE.
C
          DO j=1,N
           Wa3(j)=ZERO
           l=Ipvt(j)
           temp=Wa1(l)
           DO i=1,j
            Wa3(i)=Wa3(i)+Fjac(i,j)*temp
           END DO
          END DO
          temp1=enorm(N,Wa3)/fnorm
          temp2=(dsqrt(par)*pnorm)/fnorm
          prered=temp1**2+temp2**2/P5
          dirder=-(temp1**2+temp2**2)
C
C           COMPUTE THE RATIO OF THE ACTUAL TO THE PREDICTED
C           REDUCTION.
C
          ratio=ZERO
          IF(.not.dpeq(prered,ZERO))ratio=actred/prered
C
C           UPDATE THE STEP BOUND.
C
          IF(ratio.le.P25)THEN
           IF(actred.ge.ZERO)temp=P5
           IF(actred.lt.ZERO)temp=P5*dirder/(dirder+P5*actred)
           IF(P1*fnorm1.ge.fnorm.or.temp.lt.P1)temp=P1
           delta=temp*dmin1(delta,pnorm/P1)
           par=par/temp
          ELSE IF(dpeq(par,ZERO).or.ratio.ge.P75)THEN
           delta=pnorm/P5
           par=P5*par
          END IF
C
C           TEST FOR SUCCESSFUL ITERATION.
C
          IF(ratio.ge.P0001)THEN
C
C           SUCCESSFUL ITERATION. UPDATE X, FVEC, AND THEIR NORMS.
C
           DO j=1,N
            X(j)=Wa2(j)
            Wa2(j)=Diag(j)*X(j)
           END DO
           DO i=1,M
            Fvec(i)=Wa4(i)
           END DO
           xnorm=enorm(N,Wa2)
           fnorm=fnorm1
           Nliter=Nliter+1
c-----------------------------------------------------------------------
c     Update the parameters in the kalman filter routine to match the
c original parameter estimates before the unsuccessful step was taken.
c-----------------------------------------------------------------------
          ELSE
           CALL upespm(X)
          END IF
C
C           TESTS FOR CONVERGENCE.
C
          IF(dabs(actred).le.Ftol.and.prered.le.Ftol.and.
     &       P5*ratio.le.ONE)Info=1
*          write(mtprof,9001)' within lmdif (1):',dabs(actred),Ftol,prered,
*     &                  Ftol,P5,ratio,ONE
          IF(delta.le.Xtol*xnorm)Info=2
*          write(mtprof,9002)' within lmdif (2):',delta,Xtol,xnorm
          IF(dabs(actred).le.Ftol.and.prered.le.Ftol.and.
     &       P5*ratio.le.ONE.and.Info.eq.2)Info=3
          IF(Info.ne.0)GO TO 20
C
C           TESTS FOR TERMINATION AND STRINGENT TOLERANCES.
C
          IF(Mxiter.gt.0.and.Nliter.ge.Mxiter)Info=5
*          write(mtprof,9005)' within lmdif (5a):',Mxiter,Nliter
          IF(Nfev-oldfev.ge.maxfev)Info=5
*          write(mtprof,9006)' within lmdif (5b):',Nfev,oldfev,maxfev
          IF(dabs(actred).le.epsmch.and.prered.le.epsmch.and.
     &       P5*ratio.le.ONE)Info=6
*          write(mtprof,9007)' within lmdif (6):',dabs(actred),epsmch,
*     &                  prered,epsmch,P5,ratio,ONE
          IF(delta.le.epsmch*xnorm)Info=7
*          write(mtprof,9008)' within lmdif (7):',delta,epsmch,xnorm
          IF(gnorm.le.epsmch)Info=8
*          write(mtprof,9009)' within lmdif (8):',gnorm,epsmch
*          write(mtprof,9000)
*          write(mtprof,8000)Nliter,dabs(actred),prered,Ftol
          IF(Info.ne.0)GO TO 20
C
C           END OF THE INNER LOOP. REPEAT IF ITERATION UNSUCCESSFUL.
C
          IF(ratio.ge.P0001)GO TO 10
         END DO
C
C        END OF THE OUTER LOOP.
C
   10    CONTINUE
        END DO
       END IF
      END IF
C
C     TERMINATION, EITHER NORMAL OR USER IMPOSED.
C
   20 IF(iflag.lt.0)Info=iflag
      iflag=0
c-----------------------------------------------------------------------
c     Print out the final parameter estimates if the estimation has gone
c through at least one iteration and the last was successful.
c-----------------------------------------------------------------------
      IF(Nprint.gt.0.and.Nliter.gt.begitr.and.ratio.ge.P0001)
     &   CALL prtitr(Fvec,M,X,N,'ARMA',Nliter,Nfev)
c     ------------------------------------------------------------------
      RETURN
C
C     LAST CARD OF SUBROUTINE LMDIF.
C
* 9000 FORMAT(' -----')
* 9001 FORMAT(a,' dabs(actred).le.Ftol - dabs(actred) = ',e18.12,
*     &         ' Ftol = ',e18.12,' - ',/,
*     &       20x,'.and.prered.le.Ftol - prered = ',e18.12, 
*     &         ' Ftol = ',e18.12,' - ',/,
*     &       20x,' .and.P5*ratio.le.ONE - P5 = ',e18.12,
*     &           ' ratio = ',e18.12,' ONE = ',e18.12)
* 9002 FORMAT(a,' delta.le.Xtol*xnorm - delta = ',e18.12, 
*     &         ' Xtol = ',e18.12,' xnorm = ',e18.12)
* 9004 FORMAT(a,' gnorm.le.Gtol - gnorm = ',e18.12,' Gtol = ',e18.12)
* 9005 FORMAT(a,' Mxiter.gt.0.and.Nliter.ge.Mxiter - Mxiter = ',i10,
*     &         ' Nliter = ',i10)
* 9006 FORMAT(a,' Nfev-oldfev.ge.maxfev - Nfev = ',i10,' oldfev = ',
*     &         i10,' maxfev = ',i10)
* 9007 FORMAT(a,' dabs(actred).le.epsmch - dabs(actred) = ',e18.12,
*     &         ' epsmch = ',e18.12,' - ',/,
*     &       20x,'.and.prered.le.epsmch.and. - prered = ',e18.12,
*     &         ' epsmch = ',e18.12,' - ',/,
*     &       20x,' .and.P5*ratio.le.ONE - P5 = ',e18.12,
*     &           ' ratio = ',e18.12,' ONE = ',e18.12)
* 9008 FORMAT(a,' delta.le.epsmch*xnorm - delta = ',e18.12,' epsmch = ',
*     &       e18.12,' xnorm = ',e18.12)
* 9009 FORMAT(a,' gnorm.le.epsmch - gnorm = ',e18.12,' epsmch = ',
*     &       e18.12)
* 8000 FORMAT(i10,3(2x,e18.12))
      END

