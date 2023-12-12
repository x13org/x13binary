C     Last change:  BCM  14 May 1998    9:17 am
      SUBROUTINE roots(Thetab,Degree,Allinv,Zeror,Zeroi,Zerom,Zerof)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Find the modulus and frequency of the roots of a polynomial
c
c THETAB - double precision vector of coefficients of THETA(B) in
c          order of increasing powers
c DEGREE - maximum lag of MA model (i.e. the degree of THETA(B))
c ALLINV - output logical; true if all the zeros of the input
c              THETAB are invertible.
c ZEROR   - output double precision vector of the real parts of the
c              roots
c ZEROI   - output double precision vector of the imaginary parts of
c              the roots
c ZEROM   - output double precision vector of the modulus of the
c              roots
c ZEROF   - output double precision vector of the frequency of the
c              roots
c
c     The first two parameters are the input and also the output.
c Parameter DEGREE may change if the input leading coefficient of
c THETA(B) is near 0.0.
c-----------------------------------------------------------------------
      LOGICAL T
      PARAMETER(T=.true.)
c     -----------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'units.cmn'
c     ------------------------------------------------------------------
      DOUBLE PRECISION ZERO,ONE
      PARAMETER(ZERO=0D0,ONE=1D0)
c     ------------------------------------------------------------------
      DOUBLE PRECISION op,Zeror,Zeroi,Thetab,Zerom,Zerof
      INTEGER Degree,i,degp1
      LOGICAL fail,Allinv
      DIMENSION op(PORDER+1),Zeror(PORDER),Zeroi(PORDER),
     &          Thetab(PORDER+1),Zerom(PORDER),Zerof(PORDER)
c     ------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c     ------------------------------------------------------------------
      degp1=Degree+1
c-----------------------------------------------------------------------
c     Reverse thetab to op in order of decreasing powers
c-----------------------------------------------------------------------
      CALL revrse(Thetab,degp1,1,op)
c-----------------------------------------------------------------------
c     Check the coefficient of the highest degree to see if it is
c near zero.  If there are no roots then exit because there is nothing
c to check.
c-----------------------------------------------------------------------
      DO WHILE (dpeq(op(1),ZERO))
       IF(Degree.eq.1)THEN
        Allinv=.true.
c     ------------------------------------------------------------------
c     reduce degree of polynomial by one before exiting routine
c     BCM January 2007
c     ------------------------------------------------------------------
        Degree=Degree-1
        GO TO 10
       END IF
c     ------------------------------------------------------------------
       DO i=1,Degree
        op(i)=op(i+1)
       END DO
       Degree=Degree-1
      END DO
c-----------------------------------------------------------------------
c     Find the roots of the polynomial equation
c-----------------------------------------------------------------------
      CALL rpoly(op,Degree,Zeror,Zeroi,fail)
c-----------------------------------------------------------------------
c     Compute the modulus and frequency of each zero complex roots
c are g(i) and g(i+1)
c-----------------------------------------------------------------------
      IF(.not.fail)THEN
       Allinv=.true.
       i=0
c     ------------------------------------------------------------------
       DO WHILE (i.lt.Degree)
        i=i+1
        Zerom(i)=sqrt(Zeror(i)**2+Zeroi(i)**2)
        Zerof(i)=datan2(Zeroi(i),Zeror(i))/6.28318730707959D0
        IF((Zerom(i).lt.ONE).and.Allinv)Allinv=.false.
c     ------------------------------------------------------------------
        IF(.not.dpeq(Zeroi(i),ZERO))THEN
         i=i+1
         Zerom(i)=Zerom(i-1)
         Zerof(i)=ZERO-Zerof(i-1)
        END IF
       END DO
c     ------------------------------------------------------------------
      ELSE
       IF(.not.Lquiet)THEN
        CALL wWritln('Not all zeros of the AR or MA polynomial were '//
     &               'found.',STDERR,0,T,T)
       END IF
       WRITE(Mt1,1010)
 1010  FORMAT('<p><strong>WARNING: </strong> Not all zeros of the ',
     &        '<abbr title="autoregression">AR</abbr> or <abbr ',
     &        'title="moving average">MA</abbr> polynomial were found.',
     &        '</p>')
      END IF
c     ------------------------------------------------------------------
   10 RETURN
      END
