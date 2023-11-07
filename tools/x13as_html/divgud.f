C     Last change:  BCM  17 Apr 2003   10:54 pm
      SUBROUTINE divgud(Result,Array1,Array2,Jfda,Jlda)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C --- THIS ROUTINE DIVIDES ARRAY1 BY ARRAY2 only for those observations
c     that are "good" for multiplicative seasonal adjustment
c     ------------------------------------------------------------------
c     written by Brian Monsell, March 2006
C-----------------------------------------------------------------------
      DOUBLE PRECISION Array1,Array2,Result
      INTEGER i,Jfda,Jlda
      DIMENSION Result(*),Array1(*),Array2(*)
C-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'goodob.cmn'
C-----------------------------------------------------------------------
      DO i=Jfda,Jlda
       IF(Gudval(i))THEN
        Result(i)=Array1(i)/Array2(i)
       ELSE
        Result(i)=DNOTST
       END IF
      END DO
C-----------------------------------------------------------------------
      RETURN
      END
