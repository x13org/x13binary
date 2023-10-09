      SUBROUTINE ratneg(Nelta,Arimap,Arimal,Opr,Begopr,Endopr,C)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Makes a power series expansion, c(x), with nrc terms out of a rational
c function with numerator, a(x), with nra terms and denominator, b(x), with
c nb terms.  Where a(x) is in the form
c
c     a(1)+a(2)*x+a(3)*x^2+...+a(nra)*x^(nra-1).
c
c and the b(x) is in the form
c
c     1-b(1)x^-lagb(1)-b(2)x^-lagb(2)- ... -b(nb)x^-lagb(nb).
c
c  c(x) is in the same form as a(x).  In ratneg the powers of the
c denominator are negative so c will have nra-1 positive powers and
c an infinite number of negative powers.  Compute the expansion with
c negative powers of b by solving
c
c (c(1)+c(2)x^1...+c(nra)x^(nra-1))(1-b(1)x^-lagb(1)- ...-
c           b(nb)x^-lagb(nb))=(a(1)+a(2)x^1+...+a(nra)x^nra-1)
c
c for the c's by making equations of terms with like powers.  Note that
c c(nra) is solved first and the rest are solved using the c's after.
c The routine assumes that c has no more terms than a but generally
c any coefficients higher than nra-1, a(nra), are 0.  All the a coefficients
c are needed no matter how many c coefficients are desired because the
c recursion goes from back to front.  c is a on input and is transformed
c into C which is also an nra by nca matrix.  If nb=0 this subroutine
c does nothing.
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
c opr     i  Input 2 by * array of operator specifications, The first element
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
      DOUBLE PRECISION M300,ZERO
      PARAMETER(M300=1D-300,ZERO=0D0)
c-----------------------------------------------------------------------
c      LOGICAL T,F
c      PARAMETER(T=.true.,F=.false.,PA=PLEN+PORDER,ONE=1D0)
c     ------------------------------------------------------------------
      INTEGER Arimal,beglag,Begopr,endlag,Endopr,i,ilag,iopr,itmp,Nelta,
     &        Opr
      DOUBLE PRECISION Arimap,C,sum
      DIMENSION Arimap(PARIMA),Arimal(PARIMA),C(*),Opr(0:POPR)
c-----------------------------------------------------------------------
c      Since c is the a matrix on input the c's don't need to be
c initialized at the start of the recursions.  Calculate the number
c of elements in the c matrix.
c-----------------------------------------------------------------------
c      CALL under0(T)
c      CALL reset0(C,Nelta,M300)
c     ------------------------------------------------------------------
      DO iopr=Begopr,Endopr
       beglag=Opr(iopr-1)
       endlag=Opr(iopr)-1
c-----------------------------------------------------------------------
c     Calculate the c(i)'s, i=nra-lagb(1)+1 to 1
c-----------------------------------------------------------------------
       DO i=Nelta-Arimal(beglag),1,-1
        sum=C(i)
c-----------------------------------------------------------------------
c     Calculate c(i)
c-----------------------------------------------------------------------
        DO ilag=beglag,endlag
         itmp=i+Arimal(ilag)
         IF(itmp.le.Nelta)THEN 
          sum=sum+Arimap(ilag)*C(itmp)
         END IF
        END DO
c     ------------------------------------------------------------------
        IF (abs(sum).gt.M300)THEN
         C(i)=sum
        ELSE IF(abs(sum).gt.ZERO)THEN
         C(i)=ZERO
        END IF
       END DO
      END DO
c      CALL under0(F)
c     ------------------------------------------------------------------
      RETURN
      END
