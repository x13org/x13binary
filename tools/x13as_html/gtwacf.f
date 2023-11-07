      SUBROUTINE GTWACF (P,Q,acvlength,PHI,THETA,SIG,ACV,ACF,IND,Ppqa,
     &                   Pp)
*      SUBROUTINE GTWACF (P,Q,acvlength,PHI,THETA,SIG,ACV,ACF,IND,Ppqa,
*     &                   Pp,G)
c
c This is Granville Tunnicliffe-Wilson's program for computing
c autocovariances for an ARMA(p,q) model.  Input is from file
c tmp.in, and output is to the screen.  Some changes were made to
c also produce autocorrelations.
c----------------------------------------------------------------------
c Changes made:  3/3/92,  Bill Bell
c   1. Some code added to compute and print out autocorrelations
c   2. Integer declarations put before double precision declarations as needed
c   3. Input file name changed to "tmp.in"
c Changes made:  9/21/92,  Bill Bell
c   1.  IMPLICIT DOUBLE PRECISION (a-h,o-z)    statement added
c   2.  REAL type statements changed to DOUBLE PRECISION
c Change made:  9/23/92,  Bill Bell
c   An initial call to subroutine xpand and associated print statements
c     were removed.  Granville Wilson informed me that this was inserted
c     in the code only for initial testing.
c CHANGE MADE:  3/2/95, Matt Kramer
c   Removed all read and write statements so subroutine can be used in Splus
c   Removed integer     ICHAN, NEXT
c           char        DATCOM
c   Made driver program into a subroutine
c   Added variables dim1 (for PHI) and dim2 (for THETA), now passed into
c      subroutines euclid.f, xpand.f, and uconv.f
c   T renamed acvlength
c CHANGE MADE:  7/25/95, Bill Bell
c   Removed variables dim1 (for PHI) and dim2 (for THETA), and dropped 
c      them from the argument lists in the calls to subroutines euclid,
c      xpand, and uconv.  This undoes a change Matt Kramer made.
c CHANGE MADE:  8/4/95, Bill Bell
c   Dimensions of arrays phi, theta, acv, and acf (subroutine arguments)
c      changed to max(p,1), max(q,1), acvlength, and acvlength, 
c      respectively (permits bounds checking)
c CHANGE MODE:  6/1/2005, Rich Gagnon
c   Changed dimensions of phi and theta to (0:max(P,1)) and (0:max(Q,1))
c     in order to handle X-13ARIMA-SEATS versions of UCONV(), XPAND(),
c     and EUCLID()
c CHANGE MADE:  9/15/2005, Rich Gagnon
c   Dimension of acf changed to acvlength-1 and dimensions of phi and 
c      theta changed to 0:p and 0:q. Also modified call to XPAND()
c      by adding new argument for size of G array.
c       
c----------------------------------------------------------------------
c Note: The AR and MA operators are expressed as
c         AR:  phi(0) + phi(1)*B + ... + phi(p)*B^p
c         MA:  theta(0) + theta(1)*B + ... + theta(q)*B^q
c----------------------------------------------------------------------
c
c     Tests input routines and matrix Euclid algorithm.
c
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Input/Output Variables
c-----------------------------------------------------------------------
      INTEGER P,Q,IND,acvlength
      DOUBLE PRECISION PHI(0:P),THETA(0:Q)
      DOUBLE PRECISION ACV(acvlength),ACF(acvlength-1), SIG
c  additional variables added by BCM      
      INTEGER Pp, Ppqa
c-----------------------------------------------------------------------
c     Local Variables
c-----------------------------------------------------------------------
c     INTEGER kmax
c     PARAMETER (kmax=1000)
*      DOUBLE PRECISION G(0:max(P,Q,acvlength)),B(max(P,1)),A(max(P,1))
      DOUBLE PRECISION G(0:Ppqa),B(Pp),A(Pp)
c
      INTEGER ONE,k,PQ
      DOUBLE PRECISION ZERO
      PARAMETER (ONE=1, ZERO=0D0)
c-----------------------------------------------------------------------
      CALL UCONV(THETA,Q,G)
*      CALL UCONV(THETA,Q,G,Ppqa)
      DO k = 0, Q
        G(k) = SIG*G(k)
      END DO
c-----------------------------------------------------------------------
      PQ=MAX(P,Q)
      CALL EUCLID(PHI,B,A,PQ,P,Q,G,IND)
c-----------------------------------------------------------------------
      CALL XPAND(PHI,P,PQ,acvlength,G,max(P,Q,acvlength))
      DO k = 1, acvlength
        ACV(k) = G(k-1)
      END DO
c-----------------------------------------------------------------------
      ACV(1)=2.0D0*ACV(1)
c-----------------------------------------------------------------------
c Compute and print out autocorrelation coefficients
c-----------------------------------------------------------------------
      IF (acvlength.gt.1) THEN
       IF(Acv(1).gt.ZERO)THEN
        DO k = 2, acvlength
          ACF(k-1) = ACV(k)/ACV(1)
        END DO
       ELSE
        DO k = 2, acvlength
          ACF(k-1) = ZERO
        END DO
       END IF
      END IF
c
      RETURN
      END
