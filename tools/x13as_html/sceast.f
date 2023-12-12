      DOUBLE PRECISION FUNCTION sceast(Ndays,Pdays,First,Ineast)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     function generates the Statistics Canada Easter regressor
c     ------------------------------------------------------------------
      DOUBLE PRECISION ONE,ZERO
      PARAMETER(ONE=1.0D0,ZERO=0.0D0)
c     ------------------------------------------------------------------
      LOGICAL First,Ineast
      INTEGER Ndays,Pdays
c     ------------------------------------------------------------------
      IF(First)THEN
       IF(Pdays.eq.Ndays.or.Ineast)THEN
        sceast=ONE
       ELSE
        sceast=dble(Pdays)/dble(Ndays)
       END IF
      ELSE
       IF(Pdays.eq.Ndays)THEN
        sceast=ZERO
       ELSE
        sceast=dble(Pdays-Ndays)/dble(Ndays)
       END IF
      END IF
c     ------------------------------------------------------------------
      RETURN
      END

