**==cornom.f    processed by SPAG 4.03F  at 09:47 on  1 Mar 1994
      SUBROUTINE cornom(C,Cn,Lagh1,Cx0,Cy0)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      DOUBLE PRECISION C,Cn,cst1,Cx0,Cy0,ds
      INTEGER i,Lagh1
C*** End of declarations inserted by SPAG
C     COMMON SUBROUTINE
C     NORMALIZATION OF COVARIANCE
      DIMENSION C(*),Cn(*)
      cst1=1.0D-00
      ds=cst1/sqrt(Cx0*Cy0)
      DO i=1,Lagh1
       Cn(i)=C(i)*ds
      END DO
      RETURN
      END
