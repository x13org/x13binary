C
C GRID SEARCH MINIMISATION OF A FUNCTION IN ONE DIMENSION

C
C ALSO REQUIRED IS THE FUNCTION SUBPROGRAM WHICH DEFINES THE FUNCTION
C FUNC0(X) TO BE MINIMISED
C
C    INPUT PARAMETER :
C    FMIN : THE MINIMUM OF THE FUNCTION
C    XMIN : THE POINT AT WHICH IT OCCURS
C  
C
C
      subroutine MINIM2(fmin,xmin)

C This subroutine will computes minima by searching over a grid
C Written by Donald Martin, July 2002
C -------------------------------------------------------------------
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
C.. In/Out Status: Not Read, Overwritten ..
      double precision fmin
C.. In/Out Status: Not Read, Overwritten ..
      integer xmin
C.. In/Out Status: Not Read, Overwritten ..

C
C.. Local vector..

      double precision fx(0:100000), pi, freq(0:100000)

      integer i
C
C.. External Functions ..
      double precision FUNC0
      external FUNC0
C
C.. Intrinsic Functions ..

*      include 'min.i'
C ---------------------------------------------------------------------------
C
C ... Executable Statements ...
C
C
C STEP 1: SET UP STARTING VALUES
C
      pi = 3.14159265358979d0

C
      
      fmin=120.0D0
      xmin=-1
      do 10 i=0,100000
         freq(i)=(1.0D0/100000.0d0)*dble(i)*pi
         fx(i)=FUNC0(freq(i))
c        write(6,11) fx(i)
         if (fmin .gt. fx(i)) then
            fmin=fx(i)
              xmin=i
         end if 
10    continue
c11    format(1x, f20.6)
      return
      end
      