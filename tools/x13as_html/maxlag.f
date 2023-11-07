**==maxlag.f    processed by SPAG 4.03F  at 09:51 on  1 Mar 1994
      SUBROUTINE maxlag(Arimal,Opr,Begopr,Endopr,Mxlag)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Calculates the maximum lag of the product of nopr polynomials.
c-----------------------------------------------------------------------
c Changed:
c  To find the degree of a polynomial by searching the associated lags 
c    of the polynomial for the highest lag, by BCM based on changes made
c    to regCMPNT by REG on 04 Feb 2004.
c-----------------------------------------------------------------------
c Name  Type Description
c-----------------------------------------------------------------------
c arimal  i  Input, parima long array containing the nonzero lags of the
c             arima error structure.  The structure is specified by opr
c begopr  i  Input, begining operator in the opr array to find the maximum
c             lag of
c endopr  i  Local, last operator in the opr array to figure in the
c             order of the polynomial order calculations
c iopr    i  Local index for the current lag operator
c mxlag   i  Output scalar for maximum lag.
c nlag    i  Local number of lags in a given operator
c opr     i  Input * array of operator specifications, The first element
c             in the specification is the pointer to its place in the coef
c             and lag vectors, second is the number of lags in the operator,
c             and third is the type of operator (this information is also
c             specified in the mdl matrix.
c-----------------------------------------------------------------------
c     Date types
c-----------------------------------------------------------------------
      INTEGER Arimal,Begopr,Endopr,iopr,Mxlag,Opr
      INTEGER imax,i1,i2,i
      DIMENSION Arimal(*),Opr(0:*)
c     ------------------------------------------------------------------
      Mxlag=0
c     ------------------------------------------------------------------
      DO iopr=Begopr,Endopr
       imax=0
       i1=Opr(iopr-1)
       i2=Opr(iopr)-1
       IF(i2.ge.i1)THEN
        imax=Arimal(i2)
        if (i2.gt.i1) then
         DO i=i2-1,i1,-1
          if(imax.lt.Arimal(i))imax=Arimal(i)
         END DO
        END IF
        Mxlag=imax+Mxlag
       END IF
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
