**==euclid.f    processed by SPAG 4.03F  at 09:48 on  1 Mar 1994
      SUBROUTINE euclid(Fular,B,A,Maxpq,Mxarlg,Mxmalg,G,Err)
c-----------------------------------------------------------------------
c     Solves the scalar polynomial equation :
c           Fular(Z).F(ZINV)+F(Z).Fular(ZINV)=G(Z)
c using the Euclid algorithm .
c Fular contains AR coefficients from lag 0 to lag Mxarlg
c G contain autocovariances from the MA model from lags 0 to Mxmalg ,
c On completion the solution for F is in G from lags 0 to MAXPQ.
c Fular is unchanged.This array could be removed by calling with\
c Fular in B.
c The first loop in the subroutine merely copies Fular into B and does
c not thereafter use Fular.
c Working arrays : B holds the polynomial coefficients at
c the start, and on completion holds the stability test scalars R.
c A holds the corresponding values 1/(1-R*R). This array could also be
c removed by recomputing 1/(1-R*R) from the contents of B in the DO 200
c loop.
c ERR is set to 0 on successful exit, but if the AR coefficients do
c not satisfy the stationarity condition an exit takes place part way
c through the computation with ERR set to 1.
c     FORTRAN corrections made by Bill Bell -- 9/10/92
c 1. Integer declaration statement put before double precision
c    declaration  statement
c 2. REAL Fular(P),G(0:MAXPQ),B(Mxarlg),A(Mxarlg) changed to
c        double precision Fular(max(Mxarlg,1)),B(max(Mxarlg,1)),
c                         A(max(Mxarlg,1)) to handle case
c    where Mxarlg = 0
c FORTRAN correction sent by Granville Tunnicliffe-Wilson -- 9/21/92
c      IF (Mxarlg.GT.1)THEN   changed to   if (mxarlg.ge.1) then
c      at what is now line 99.
c Changes made:  9/21/92,  Bill Bell
c 1.  IMPLICIT NONE   statement added
c 2.  REAL type statements changed to DOUBLE PRECISION
c-----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION ONE,TWO,ZERO
      PARAMETER(ONE=1D0,TWO=2D0,ZERO=0D0)
      INTEGER i,Err,fsthlf,lsthlf,lim,Maxpq,midpt,Mxarlg,Mxmalg
      DOUBLE PRECISION G(0:Maxpq),Fular(0:Mxarlg),B(Mxarlg),A(Mxarlg),r,
     &                 s,bs,br,gs,gr
c      INCLUDE 'units.cmn'
c-----------------------------------------------------------------------
c     Initialization of coefficients of working arrays
c-----------------------------------------------------------------------
      DO i=1,Mxarlg
       B(i)=Fular(i)
      END DO
c-----------------------------------------------------------------------
c     Start of order reduction loop.
c-----------------------------------------------------------------------
      Err=0
      DO i=Maxpq,1,-1
       IF(i.le.Mxarlg)THEN
        r=B(i)
c     ------------------------------------------------------------------
c        call profiler(3,"euclid set err")
c        WRITE(Mtprof,*)'  abs(r) = ',abs(r)
        IF(abs(r).gt.ONE)THEN
         Err=1
         GO TO 10
        END IF
c     ------------------------------------------------------------------
        s=ONE/(ONE-r*r)
        A(i)=s
        midpt=i/2
c     ------------------------------------------------------------------
        DO fsthlf=1,midpt
         lsthlf=i-fsthlf
         bs=B(fsthlf)
         br=B(lsthlf)
         B(fsthlf)=(bs-br*r)*s
         B(lsthlf)=(br-bs*r)*s
        END DO
       END IF
c-----------------------------------------------------------------------
c     Correction of G by reduced coefficients
c-----------------------------------------------------------------------
       lim=i-1
       IF(i.gt.Mxarlg)lim=Mxarlg
c     ------------------------------------------------------------------
       IF(i.gt.Mxmalg)THEN
        lim=0
        G(i)=ZERO
       END IF
c     ------------------------------------------------------------------
       DO fsthlf=1,lim
        lsthlf=i-fsthlf
        G(lsthlf)=G(lsthlf)-B(fsthlf)*G(i)
       END DO
      END DO
c-----------------------------------------------------------------------
c     End of reduction loop, Start of construction loop
c-----------------------------------------------------------------------
      G(0)=G(0)/TWO
c-----------------------------------------------------------------------
c     Zero step completed
c-----------------------------------------------------------------------
      DO i=1,Mxarlg
       midpt=i/2
c     ------------------------------------------------------------------
       DO fsthlf=0,midpt
        lsthlf=i-fsthlf
        gs=G(fsthlf)
        gr=G(lsthlf)
        G(fsthlf)=(gs-B(i)*gr)*A(i)
        G(lsthlf)=(gr-B(i)*gs)*A(i)
       END DO
      END DO
c     ------------------------------------------------------------------
   10 RETURN
      END
