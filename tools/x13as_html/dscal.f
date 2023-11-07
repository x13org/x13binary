**==dscal.f    processed by SPAG 4.03F  at 09:48 on  1 Mar 1994
      SUBROUTINE dscal(N,Da,Dx,Incx)
      IMPLICIT NONE
C
C     REPLACE DOUBLE PRECISION DX BY DOUBLE PRECISION DA*DX.
C     FOR I = 0 TO N-1, REPLACE DX(1+I*INCX) WITH  DA * DX(1+I*INCX)
C
      INTEGER i,Incx,m,mp1,N,ns
      DOUBLE PRECISION Da,Dx(*)
      IF(N.le.0)RETURN
      IF(Incx.eq.1)THEN
C
C        CODE FOR INCREMENTS EQUAL TO 1.
C
C
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5.
C
       m=mod(N,5)
       IF(m.ne.0)THEN
        DO i=1,m
         Dx(i)=Da*Dx(i)
        END DO
        IF(N.lt.5)RETURN
       END IF
      ELSE
C
C        CODE FOR INCREMENTS NOT EQUAL TO 1.
C
       ns=N*Incx
       DO i=1,ns,Incx
        Dx(i)=Da*Dx(i)
       END DO
       RETURN
      END IF
      mp1=m+1
      DO i=mp1,N,5
       Dx(i)=Da*Dx(i)
       Dx(i+1)=Da*Dx(i+1)
       Dx(i+2)=Da*Dx(i+2)
       Dx(i+3)=Da*Dx(i+3)
       Dx(i+4)=Da*Dx(i+4)
      END DO
      RETURN
      END
