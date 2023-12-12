**==clsgrp.f    processed by SPAG 4.03F  at 09:47 on  1 Mar 1994
      INTEGER FUNCTION clsgrp(Opnchr)
      IMPLICIT NONE
c----------------------------------------------------------------------
c     Return the ascii value of the closing character given the
c opening character, i.e. ) for (,] for [, and { for }.
c----------------------------------------------------------------------
      INTEGER Opnchr
c     -----------------------------------------------------------------
      IF(Opnchr.eq.40)THEN
       clsgrp=41
c     -----------------------------------------------------------------
      ELSE IF(Opnchr.eq.47)THEN
       clsgrp=47
c     -----------------------------------------------------------------
      ELSE IF(Opnchr.eq.91)THEN
       clsgrp=93
c     -----------------------------------------------------------------
      ELSE IF(Opnchr.eq.123)THEN
       clsgrp=125
c     -----------------------------------------------------------------
      ELSE
       clsgrp=-1
      END IF
c     -----------------------------------------------------------------
      RETURN
      END
