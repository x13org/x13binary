C     Last change:  BCM  25 Nov 97    2:57 pm
**==dot.f    processed by SPAG 4.03F  at 09:47 on  1 Mar 1994
      SUBROUTINE dot(Jy1,Jy2,Jx)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      INTEGER j,j1,j2,Jx,Jy1,Jy2
C*** End of declarations inserted by SPAG
      INCLUDE 'srslen.prm'
      INCLUDE 'chrt.cmn'
C*************
      j1=min(Jy1,Jy2)+1
      j2=max(Jy1,Jy2)-1
      IF(j2.ge.j1)THEN
       DO j=j1,j2
        Ia(Jx,j)=I7
       END DO
      END IF
      RETURN
      END
