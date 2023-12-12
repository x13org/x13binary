      SUBROUTINE maxvec(Dx,N,Mxabvl)
c-----------------------------------------------------------------------
c     maxvec.f, Release 1, Subroutine Version 1.1, Modified 09 Feb 1995.
c-----------------------------------------------------------------------
c     Find maximum magnitude of double precision dx.
c-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER i,N
      DOUBLE PRECISION Dx(*),Mxabvl,xmag
c     -----------------------------------------------------------------
      Mxabvl=0
      IF(N.le.0)RETURN
c-----------------------------------------------------------------------
c     Code for increments equal to 1.
c-----------------------------------------------------------------------
      DO i=1,N
       xmag=abs(Dx(i))
       IF(xmag.gt.Mxabvl)Mxabvl=xmag
      END DO
c     -----------------------------------------------------------------
      RETURN
      END
