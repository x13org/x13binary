      SUBROUTINE mdlint()
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Initialize the parameters and lag vectors.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
c     ------------------------------------------------------------------
      INTEGER PACM
      DOUBLE PRECISION ZERO
      PARAMETER(PACM=(PLEN+2*PORDER)*PARIMA,ZERO=0D0)
c     ------------------------------------------------------------------
      CALL intlst(POPR,Opr,Nopr)
      CALL intlst(POPR,Oprptr,Noprtl)
      CALL setchr(' ',POPRCR*POPR,Oprttl)
      CALL intlst(PMDL,Mdl,Nmdl)
      Mdl(AR)=1
      Mdl(MA)=1
      CALL setlg(.false.,PARIMA,Arimaf)
      CALL setint(0,PARIMA,Arimal)
      CALL setdp(ZERO,PARIMA,Arimap)
      CALL setdp(ZERO,PACM,Armacm)
      Lndtcv=ZERO
      Mxarlg=0
      Mxdflg=0
      Mxmalg=0
      Nseadf=0
      Lseadf=.false.
c-----------------------------------------------------------------------
      RETURN
      END
