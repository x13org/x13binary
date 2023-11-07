C     Last change:  DH   22 May 2019
**==ddot.f    processed by SPAG 4.03F  at 09:47 on  1 Mar 1994
      DOUBLE PRECISION FUNCTION ddot(N,Dx,Incx,Dy,Incy)
      IMPLICIT NONE
C
C     RETURNS THE DOT PRODUCT OF DOUBLE PRECISION DX AND DY.
C     DDOT = SUM FOR I = 0 TO N-1 OF  DX(LX+I*INCX) * DY(LY+I*INCY)
C     WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N, AND LY IS
C     DEFINED IN A SIMILAR WAY USING INCY.
C
c
      LOGICAL UNDERFLOW
      EXTERNAL UNDERFLOW
      INTEGER i,Incx,Incy,ix,iy,m,mp1,N,ns
      DOUBLE PRECISION Dx(*),Dy(*)

      ddot=0.D0
      IF(N.le.0)RETURN
      IF(Incx.eq.Incy)THEN
       IF(Incx.eq.1)THEN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1.
C
C
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5.
C
        m=mod(N,5)
        IF(m.ne.0)THEN
         DO i=1,m
          if (.not.UNDERFLOW(Dx(i),Dy(i))) ddot=ddot+Dx(i)*Dy(i)
         END DO
         IF(N.lt.5)RETURN
        END IF
        mp1=m+1
        DO i=mp1,N,5
          if (.not.UNDERFLOW(Dx(i),Dy(i))) ddot=ddot+Dx(i)*Dy(i)
          if (.not.UNDERFLOW(Dx(i+1),Dy(i+1))) ddot=ddot+Dx(i+1)*Dy(i+1)
          if (.not.UNDERFLOW(Dx(i+2),Dy(i+2))) ddot=ddot+Dx(i+2)*Dy(i+2)
          if (.not.UNDERFLOW(Dx(i+3),Dy(i+3))) ddot=ddot+Dx(i+3)*Dy(i+3)
          if (.not.UNDERFLOW(Dx(i+4),Dy(i+4))) ddot=ddot+Dx(i+4)*Dy(i+4)
        END DO
        RETURN
       ELSE IF (Incx.gt.1) THEN
C
C         CODE FOR POSITIVE EQUAL INCREMENTS .GT.1.
C
        ns=N*Incx
        DO i=1,ns,Incx
         if (.not.UNDERFLOW(Dx(i),Dy(i))) ddot=ddot+Dx(i)*Dy(i)
        END DO
        RETURN
       END IF
      ELSE
C
C         CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS
C
        ix=1
        iy=1
        IF(Incx.lt.0)ix=(-N+1)*Incx+1
        IF(Incy.lt.0)iy=(-N+1)*Incy+1
        DO i=1,N
         if (.not.UNDERFLOW(Dx(ix),Dy(iy))) ddot=ddot+Dx(ix)*Dy(iy)
         ix=ix+Incx
         iy=iy+Incy
        END DO
        RETURN
      END IF
      END
c-----------------------------------------------------------------------
      LOGICAL FUNCTION UNDERFLOW(x,y)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Return TRUE if the multiplication x*y is an UNDERFLOW
c     or either x,y is ZERO
c     dpmpar(2) is the machine defined lower limit
c-----------------------------------------------------------------------
      DOUBLE PRECISION x,y,dpmpar,dmin,xp,ZERO
      PARAMETER(ZERO=0D0)
      EXTERNAL dpmpar
c-----------------------------------------------------------------------
      UNDERFLOW=.TRUE.
      IF (x.eq.ZERO.or.y.eq.ZERO) RETURN
      dmin=DLOG10(dpmpar(2))
      xp = DLOG10(DABS(x)) + DLOG10(DABS(y))
      IF (xp.le.dmin) RETURN
c-----------------------------------------------------------------------
      UNDERFLOW=.FALSE.
      RETURN
      END

