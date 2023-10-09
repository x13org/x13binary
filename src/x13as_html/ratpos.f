      SUBROUTINE ratpos(Nelta,Arimap,Arimal,Opr,Begopr,Endopr,Neltc,C)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c Name  Type Description
c-----------------------------------------------------------------------
c arimal  i  Input, parima long array containing the nonzero lags of the
c             arima error structure.  The structure is specified by opr
c             and mdl.
c arimap  d  Input, parma long array containing the parameter
c             estimates of the arima error structure.  The structure
c             of the vector is specified by opr and mdl.  The lags
c             associated with the estimates are in arimal.
c begelt  i  Local first element, i, of c(i) in loop
c beglag  i  Local index for the starting element in arimal and arimap
c             of the current lag operator
c begopr  i  Input index for th3 starting AR or MA operator in the
c             opr array.  Differencing is included as an AR operator
c c       d  I/O Na by nca matrix, the a matrix on input and the c,
c             expansion matrix-output.  Note that the input will be the
c             same as the output if nb=0
c endlag  i  Local index for the last element in arimal and arimap
c             of the current lag operator
c endopr  i  Local index for th3 last AR or MA operator in the
c             opr array.  Differencing is included as an AR operator
c i       i  Local do loop index
c ilag    i  Local do loop index for the current lag
c itmp    i  Local index for the current lagged element
c iopr    i  Local index for the current lag operator
c nelta   i  Local number of elements in the a matrix, ie the c matrix
c             on input
c neltc   i  Local number of elements in the c matrix
c ntmpa   i  Local number of elements in the a vector of ratpos.  On input
c             its nelta but if there are more operator the length is no
c             nolonger nelta but neltc because it has been through the
c             filter once
c opr     i  Input 3 by * array of operator specifications, The first element
c             in the specification is the pointer to its place in the coef
c             and lag vectors, second is the number of lags in the operator,
c             and third is the type of operator (this information is also
c             specified in the mdl matrix.
c sum     d  Local sum of coeffients of all the like powers of
c             a and c
c zero    d  Local PARAMETER for a double precision 0
c-----------------------------------------------------------------------
c     Type variables
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
c     ------------------------------------------------------------------
      DOUBLE PRECISION ZERO
      PARAMETER(ZERO=0D0)
c      LOGICAL T,F
c      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      INTEGER Arimal,begelt,beglag,Begopr,endlag,Endopr,i,ilag,iopr,
     &        itmp,Nelta,Neltc,ntmpa,Opr
      DOUBLE PRECISION Arimap,C,sum
      DIMENSION Arimap(*),Arimal(*),C(*),Opr(0:POPR)
c-----------------------------------------------------------------------
c     Makes a power series expansion, c(x), with nrc terms out of a rational
c function with numerator, a(x), with nra terms and a denominator, b(x),
c with nb terms.  Where a(x) is in the form
c
c     a(1)+a(2)*x+a(3)*x^2+...+a(nra)*x^(nra-1).
c
c and the b(x) is in the form
c
c     1-b(1)x^lagb(1)-b(2)x^lagb(2)- ... -b(nb)x^lagb(nb).
c
c c(x) is in the same form as a(x). In ratpos the powers of the
c demonminator, b, are positve.  Compute the expansion by solving
c
c (c(1)+c(2)x^1...+c(nrc)x^(nrc-1))(1-b(1)x^lagb(1)-...-b(nb)x^nb*lagb(nb))=
c                                         (a(1+a(2)x^1+...+a(nra)x^nra-1)
c
c for the c's by making equations of terms with like powers on both sides
c of the equation. Solve c(1) first then each suceeding one recursively.
c The summation will decrease in the b's and increase in the a's so that
c power of the products will remain constant.  Zero out the power series
c expansion array, c.  C is an nrc by nca matrix.  The first nra terms
c are initilized to the a's.
c-----------------------------------------------------------------------
c     Note c is the a matrix on input.  First
c calculate the number of elements in the c/a matrices
c-----------------------------------------------------------------------
      ntmpa=Nelta
c      CALL under0(T)
c     ------------------------------------------------------------------
      DO iopr=Begopr,Endopr
       beglag=Opr(iopr-1)
       endlag=Opr(iopr)-1
       IF(endlag.gt.beglag)THEN
        begelt=Arimal(beglag)+1
       ELSE
        begelt=1
       END IF
c     ------------------------------------------------------------------
       DO i=ntmpa+1,begelt-1
        C(i)=ZERO
       END DO
c-----------------------------------------------------------------------
c     Calculate the c(i)'s, i=lagb(nlag)+1,lag(nlag+1)
c-----------------------------------------------------------------------
       DO i=begelt,Neltc
        IF(i.le.ntmpa)THEN
         sum=C(i)
c     ------------------------------------------------------------------
        ELSE
         sum=ZERO
        END IF
c-----------------------------------------------------------------------
c     Calculate c(i)
c-----------------------------------------------------------------------
        DO ilag=beglag,endlag
         itmp=i-Arimal(ilag)
         IF(itmp.gt.0) THEN
          IF(Dabs(Arimap(ilag)).gt.1.D-150.and.Dabs(C(itmp)).gt.1.D-150)
     &      sum=sum+Arimap(ilag)*C(itmp)
         END IF
        END DO
c     ------------------------------------------------------------------
        C(i)=sum
       END DO
c     ------------------------------------------------------------------
       ntmpa=Neltc
      END DO
c      CALL under0(F)
c     ------------------------------------------------------------------
      RETURN
      END
