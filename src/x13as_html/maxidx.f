      SUBROUTINE maxidx(Dx,N,Imxidx,Imxval)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     maxidx.f, Release 1, Subroutine Version 1.3, Modified 22 Sep 1994.
c-----------------------------------------------------------------------
c     Find smallest index of maximum magnitude of double precision dx.
c imxidx =  first i, i = 1 to n, to minimize  abs(dx(1-incx+i*incx))
c-----------------------------------------------------------------------
      INTEGER i,Imxidx,Imxval,N
      INTEGER Dx(*),xmag
c     -----------------------------------------------------------------
      IF(N.le.0)THEN
       Imxidx=0
       RETURN
      END IF
      Imxidx=1
c-----------------------------------------------------------------------
c     Code for increments equal to 1.
c-----------------------------------------------------------------------
      Imxval=abs(Dx(1))
      IF(N.le.1)RETURN
c     -----------------------------------------------------------------
      DO i=2,N
       xmag=abs(Dx(i))
c     -----------------------------------------------------------------
       IF(xmag.gt.Imxval)THEN
        Imxidx=i
        Imxval=xmag
       END IF
      END DO
c     -----------------------------------------------------------------
      RETURN
      END
