C     Last change:  BCM  25 Nov 97    3:29 pm
**==trbias.f    processed by SPAG 4.03F  at 12:44 on 11 Aug 1994
      SUBROUTINE trbias(Stc,Sts,Sti,L1,L2,Biasfc,Ny)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This routine corrects for bias in a trend component genrated from
c     a log-additive seasonal adjustment (see Thompson and Ozaki, 1992)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
c-----------------------------------------------------------------------
      DOUBLE PRECISION Stc,Sts,Sti,hs,sig,Biasfc,tic23
      INTEGER i,L1,L2,Ny
      DIMENSION Stc(PLEN),Sts(PLEN),Sti(PLEN),Biasfc(PLEN),hs(PLEN)
c-----------------------------------------------------------------------
c     Evaluate characteristic function of the irregular.
c-----------------------------------------------------------------------
c     initialize sig - July 2006 BCM
c     ------------------------------------------------------------------
      sig=0D0
      DO i=L1,L2
       sig=sig+Sti(i)*Sti(i)
      END DO
      sig=exp(sig/(2D0*(L2-L1+1)))
c-----------------------------------------------------------------------
c     Smooth the seasonal factors using a 23 (or 7, if quarterly) term
c     Henderson filter.
c-----------------------------------------------------------------------
      tic23=4.5D0
      CALL hndtrn(hs,Sts,L1,L2,(2*Ny)-1,tic23,.true.,.false.)
c-----------------------------------------------------------------------
c     Perform bias correction.
c-----------------------------------------------------------------------
      DO i=L1,L2
       Biasfc(i)=sig*hs(i)
       Stc(i)=Stc(i)*Biasfc(i)
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
