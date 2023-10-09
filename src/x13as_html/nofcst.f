      SUBROUTINE nofcst(Trnsrs,Frstry,Lx11)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     This subroutine sets the number of forecasts and backcasts to 0,
c     resets data pointers to take this into account, and regenerates
c     the regression matrix.
c     Written by BCM (July 2007)
c-----------------------------------------------------------------------
      LOGICAL T
      PARAMETER(T=.true.)
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'arima.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'extend.cmn'
c     ------------------------------------------------------------------
      DOUBLE PRECISION Trnsrs(PLEN)
      LOGICAL Lx11
      INTEGER Frstry,nf2
c     ------------------------------------------------------------------
      nf2=Nfcst
      Nfcst=0
      IF(Nbcst.gt.0)Nbcst=0
      IF(Nfdrp.gt.0)Nfdrp=0
      CALL setxpt(nf2,Lx11,Fctdrp)
      CALL regvar(Trnsrs,Nobspf,Fctdrp,Nfcst,0,Userx,Bgusrx,Nrusrx,
     &            Priadj,Reglom,Nrxy,Begxy,Frstry,T,Elong)
c     ------------------------------------------------------------------
      RETURN
      END