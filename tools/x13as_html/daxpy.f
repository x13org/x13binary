**==daxpy.f    processed by SPAG 4.03F  at 09:47 on  1 Mar 1994
      SUBROUTINE daxpy(N,Da,Dx,Incx,Dy,Incy)
      IMPLICIT NONE
C
C     OVERWRITE DOUBLE PRECISION DY WITH DOUBLE PRECISION DA*DX + DY.
C     FOR I = 0 TO N-1, REPLACE  DY(LY+I*INCY) WITH DA*DX(LX+I*INCX) +
C       DY(LY+I*INCY), WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N,
C       AND LY IS DEFINED IN A SIMILAR WAY USING INCY.
C
      INTEGER i,Incx,Incy,ix,iy,m,mp1,N,ns
      DOUBLE PRECISION Dx(*),Dy(*),Da
      LOGICAL dpeq
      EXTERNAL dpeq
      IF(N.le.0.or.dpeq(Da,0.D0))RETURN
      IF(Incx.eq.Incy)THEN
       IF(Incx.lt.1)THEN
       ELSE IF(Incx.eq.1)THEN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 4.
C
        m=mod(N,4)
        IF(m.ne.0)THEN
         DO i=1,m
          Dy(i)=Dy(i)+Da*Dx(i)
         END DO
         IF(N.lt.4)RETURN
        END IF
        mp1=m+1
        DO i=mp1,N,4
         Dy(i)=Dy(i)+Da*Dx(i)
         Dy(i+1)=Dy(i+1)+Da*Dx(i+1)
         Dy(i+2)=Dy(i+2)+Da*Dx(i+2)
         Dy(i+3)=Dy(i+3)+Da*Dx(i+3)
        END DO
        RETURN
       ELSE
        GO TO 10
       END IF
      END IF
C
C        CODE FOR NONEQUAL OR NONPOSITIVE INCREMENTS.
C
      ix=1
      iy=1
      IF(Incx.lt.0)ix=(-N+1)*Incx+1
      IF(Incy.lt.0)iy=(-N+1)*Incy+1
      DO i=1,N
       Dy(iy)=Dy(iy)+Da*Dx(ix)
       ix=ix+Incx
       iy=iy+Incy
      END DO
      RETURN
C
C        CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS.
C
   10 ns=N*Incx
      DO i=1,ns,Incx
       Dy(i)=Da*Dx(i)+Dy(i)
      END DO
      RETURN
      END
