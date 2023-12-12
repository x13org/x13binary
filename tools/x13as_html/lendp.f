C     Last change:  BCM  29 Sep 97    9:48 am
**==lendp.f    processed by SPAG 4.03F  at 09:50 on  1 Mar 1994
      SUBROUTINE lendp(X,Nx,Lenx)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Finds how many values have been input into x by finding the first
c dnotst value working back through the array.  Array must be initialized
c with notset values (dnotst for double precision) using a
c call setdp(dnotst,nx,x) call.
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INTEGER Nx,Lenx
      DOUBLE PRECISION X(*)
c     ------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c     ------------------------------------------------------------------
      DO Lenx=Nx,1,-1
       IF(.not.dpeq(X(Lenx),DNOTST))GO TO 10
      END DO
c     ------------------------------------------------------------------
   10 RETURN
      END
