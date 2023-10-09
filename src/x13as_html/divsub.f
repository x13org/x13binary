C     Last change:  BCM  17 Apr 2003   10:54 pm
      SUBROUTINE divsub(Result,Array1,Array2,Jfda,Jlda)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C --- THIS ROUTINE DIVIDES ARRAY1 BY ARRAY2 OR SUBTRACTS ARRAY2
C --- FROM ARRAY1 DEPENDING ON WHETHER A MULTIPLICATIVE OR ADDITIVE
C --- ADJUSTMENT IS BEING MADE.
C-----------------------------------------------------------------------
      DOUBLE PRECISION Array1,Array2,Result
      INTEGER i,Jfda,Jlda
      DIMENSION Result(*),Array1(*),Array2(*)
C-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'x11opt.cmn'
C-----------------------------------------------------------------------
      IF(Muladd.eq.0)THEN
       DO i=Jfda,Jlda
        Result(i)=Array1(i)/Array2(i)
       END DO
       RETURN
      END IF
      DO i=Jfda,Jlda
       Result(i)=Array1(i)-Array2(i)
      END DO
      RETURN
      END
