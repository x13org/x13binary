**==indx.f    processed by SPAG 4.03F  at 09:50 on  1 Mar 1994
      INTEGER FUNCTION indx(Str,Chr)
      IMPLICIT NONE
c     -----------------------------------------------------------------
      CHARACTER Chr*1,Str*(*)
c     -----------------------------------------------------------------
      DO indx=1,len(Str)
       IF(Chr.eq.Str(indx:indx))GO TO 10
      END DO
      indx=0
c     -----------------------------------------------------------------
   10 RETURN
      END
