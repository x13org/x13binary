      SUBROUTINE resid2(Xy,Nr,Nc,Nb,Xdev,B,Rsd,Sti)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Returns the residuals from an X-11 regression model for a 
c     multiplicative seasonal adjustment when both X-11 and Holiday 
c     regressors are used.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'xtdtyp.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION Xy,B,Rsd,tmp,Sti
      INTEGER icol,irow,ir2,Nr,Nc,Nb,Xdev
      DIMENSION B(*),Rsd(*),tmp(PLEN),Xy(Nc*Nr),Sti(PLEN)
c-----------------------------------------------------------------------
      DO icol=1,Nb
       CALL daxpy(Nr,B(icol),Xy(icol),Nc,tmp,1)
      END DO
c-----------------------------------------------------------------------
      DO irow=1,Nr
       ir2=irow+Xdev-1
       Rsd(irow)=(Xnstar(ir2)*Sti(ir2))-(Xn(ir2)+tmp(irow))/Kvec(irow)
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
