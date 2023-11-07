**==difflt.f    processed by SPAG 4.03F  at 09:47 on  1 Mar 1994
      SUBROUTINE difflt(Nr,Nc,Ndf,Nsdf,Sp,C,Nefobs)
c     ------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER i,j,lag,Nc,Ndf,Nefobs,nelt,Nr,Nsdf,Sp
      DOUBLE PRECISION C
      DIMENSION C(*)
c-----------------------------------------------------------------------
c     Regular differences
c-----------------------------------------------------------------------
      nelt=Nr*Nc
      lag=Nc
      DO j=1,Ndf
       nelt=nelt-lag
c     ------------------------------------------------------------------
       DO i=1,nelt
        C(i)=C(i+lag)-C(i)
       END DO
      END DO
c-----------------------------------------------------------------------
c     Seasonal differences
c-----------------------------------------------------------------------
      lag=Nc*Sp
      DO j=1,Nsdf
       nelt=nelt-lag
c     ------------------------------------------------------------------
       DO i=1,nelt
        C(i)=C(i+lag)-C(i)
       END DO
      END DO
c     ------------------------------------------------------------------
      Nefobs=nelt/Nc
c     ------------------------------------------------------------------
      RETURN
      END
