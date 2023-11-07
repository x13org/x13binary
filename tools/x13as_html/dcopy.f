**==dcopy.f    processed by SPAG 4.03F  at 09:47 on  1 Mar 1994
      SUBROUTINE dcopy(N,Dx,Incx,Dy,Incy)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      DOUBLE PRECISION Dx(*),Dy(*)
      INTEGER i,Incx,Incy,ix,iy,m,mp1,N
c     ------------------------------------------------------------------
      IF(N.gt.0)THEN
       IF(Incx.ne.1.or.Incy.ne.1)THEN
        ix=1
        iy=1
        IF(Incx.lt.0)ix=(1-N)*Incx+1
        IF(Incy.lt.0)iy=(1-N)*Incy+1
c     ------------------------------------------------------------------
        DO i=1,N
         Dy(iy)=Dx(ix)
         ix=ix+Incx
         iy=iy+Incy
        END DO
c     ------------------------------------------------------------------
       ELSE
        m=mod(N,7)
c     ------------------------------------------------------------------
        IF(m.ne.0)THEN
         DO i=1,m
          Dy(i)=Dx(i)
         END DO
        END IF
c     ------------------------------------------------------------------
        IF(N.ge.7)THEN
         mp1=m+1
         DO i=mp1,N,7
          Dy(i)=Dx(i)
          Dy(i+1)=Dx(i+1)
          Dy(i+2)=Dx(i+2)
          Dy(i+3)=Dx(i+3)
          Dy(i+4)=Dx(i+4)
          Dy(i+5)=Dx(i+5)
          Dy(i+6)=Dx(i+6)
         END DO
        END IF
       END IF
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
