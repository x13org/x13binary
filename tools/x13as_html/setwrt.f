      SUBROUTINE setwrt(Wrt,Wrtidx)
      IMPLICIT NONE
c  set up format variable wrt for the routine fortbl.
c  the actual format used depends on the wrtidx index variable
      INTEGER Wrtidx,i
      CHARACTER*12 wrt(*),wrt0(10),wrt1(5),wrt2(4),wrt3(5),wrt4(4)
      
      data wrt0/
     $     '(6X','''DATE'',11X','''FORECAST''','11X','''SE'',13X,','N',
     $     '(''FORECAST''','11X,''SER''',',12X,','))'/
      data wrt1/'(2X,A3,','''-'',I4,2X,','N','(1X,F16.4,','1X,F16.4))'/
      data wrt2/'(25X,','N','(A13,20X','))'/
      data wrt3/'(2X,A3,','''-'',I4,5X,','N','(1X,F16.4,','14X,F16.4))'/
      data wrt4/'(20X,','N','(5X,A13,10X','))'/

      IF (Wrtidx.eq.0) THEN
       DO i = 1,10
        wrt(i) = wrt0(i)
       END DO
      ELSE IF (Wrtidx.eq.1) THEN
       DO i = 1,5
        wrt(i) = wrt1(i)
       END DO
      ELSE IF (Wrtidx.eq.2) THEN
       DO i = 1,4
        wrt(i) = wrt2(i)
       END DO
      ELSE IF (Wrtidx.eq.3) THEN
       DO i = 1,5
        wrt(i) = wrt3(i)
       END DO
      ELSE IF (Wrtidx.eq.4) THEN
       DO i = 1,4
        wrt(i) = wrt4(i)
       END DO
      END IF
       
      RETURN
      END
      