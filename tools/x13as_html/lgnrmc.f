      SUBROUTINE lgnrmc(Nfcst,Fctunc,Fctse,Fctcor,Ltrans)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Subroutine to perform lognormal correction of forcasts/backcasts
c     Ltrans controls if forecast is to be transformed to original scale
c-----------------------------------------------------------------------
      DOUBLE PRECISION PT5
      PARAMETER(PT5=0.5D0)
c-----------------------------------------------------------------------
      INTEGER i,Nfcst
      DOUBLE PRECISION Fctunc(*),Fctse(*),Fctcor(*),corfac
      LOGICAL Ltrans
c     ------------------------------------------------------------------
c     Compute correction factor
c     ------------------------------------------------------------------
      DO i=1,Nfcst
       corfac=Fctse(i)*Fctse(i)*PT5
       IF(Ltrans)THEN
        Fctcor(i)=DEXP(corfac+Fctunc(i))
       ELSE
        Fctcor(i)=corfac+Fctunc(i)
       END IF
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
