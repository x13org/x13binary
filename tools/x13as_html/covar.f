      SUBROUTINE covar(N,R,Ldr,Ipvt,Tol,Info)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     covar.f, Release 1, Subroutine Version 1.4, Modified 03 Feb 1995.
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INTEGER N,Ldr
      INTEGER Ipvt(N)
      DOUBLE PRECISION Tol
      DOUBLE PRECISION R(Ldr,N),wa(PARIMA)
c-----------------------------------------------------------------------
      INTEGER i,ii,Info,j,jj,k,km1
      LOGICAL sing
      DOUBLE PRECISION dpmpar,ONE,temp,tolr,ZERO
      PARAMETER(ONE=1D0,ZERO=0D0)
      EXTERNAL dpmpar
c-----------------------------------------------------------------------
c     Form the inverse of R in the full upper triangle of R
c-----------------------------------------------------------------------
      IF(Tol.le.ZERO)THEN
       tolr=dpmpar(1)*abs(R(1,1))
      ELSE
       tolr=Tol*abs(R(1,1))
      END IF
c-----------------------------------------------------------------------
      Info=NOTSET
      DO k=1,N
       IF(abs(R(k,k)).le.tolr)GO TO 10
       R(k,k)=ONE/R(k,k)
       km1=k-1
c-----------------------------------------------------------------------
       DO j=1,km1
        temp=R(k,k)*R(j,k)
        R(j,k)=ZERO
c-----------------------------------------------------------------------
        DO i=1,j
         R(i,k)=R(i,k)-temp*R(i,j)
        END DO
       END DO
c-----------------------------------------------------------------------
       Info=k
      END DO
c-----------------------------------------------------------------------
c     Form the full upper triangle of the inverse of R'R in the
c full upper triangle of R
c-----------------------------------------------------------------------
   10 DO k=1,Info
       km1=k-1
c-----------------------------------------------------------------------
       DO j=1,km1
        temp=R(j,k)
c-----------------------------------------------------------------------
        DO i=1,j
         R(i,j)=R(i,j)+temp*R(i,k)
        END DO
       END DO
c-----------------------------------------------------------------------
       temp=R(k,k)
       DO i=1,k
        R(i,k)=temp*R(i,k)
       END DO
      END DO
c-----------------------------------------------------------------------
c     Form the full lower triangle of the covariance matrix in the
c strict lower triangle of R and in WA
c-----------------------------------------------------------------------
      DO j=1,N
       jj=Ipvt(j)
       sing=j.gt.Info
       DO i=1,j
        IF(sing)R(i,j)=ZERO
        ii=Ipvt(i)
        IF(ii.gt.jj)THEN
         R(ii,jj)=R(i,j)
        ELSE IF(ii.lt.jj)THEN
         R(jj,ii)=R(i,j)
        END IF
       END DO
       wa(jj)=R(j,j)
      END DO
c-----------------------------------------------------------------------
c     Make R a symmetric matrix
c-----------------------------------------------------------------------
      DO j=1,N
       DO i=1,j
        R(i,j)=R(j,i)
       END DO
       R(j,j)=wa(j)
      END DO
c-----------------------------------------------------------------------
c     If the matrix is nonsingular then return info=0, otherwise
c return the number of nonsingular columns.
c-----------------------------------------------------------------------
      IF(Info.eq.N)Info=0
c-----------------------------------------------------------------------
      RETURN
      END
