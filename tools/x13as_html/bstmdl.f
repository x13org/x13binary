C     Last change:  BCM   2 Apr 98   12:55 pm
      SUBROUTINE bstmdl(Nbstds,Bstdsn,Bstptd)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Routine which stores best model found by the automatic
c     ARIMA modelling routine into temporary storage variables.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'bstmdl.cmn'
c-----------------------------------------------------------------------
      LOGICAL Bstptd
      INTEGER Nbstds
      CHARACTER Bstdsn*(132)
c-----------------------------------------------------------------------
c     Initialize temporary storage variables
c-----------------------------------------------------------------------
      CALL setlg(.false.,PARIMA,Bstafx)
      CALL setint(0,PARIMA,Bstalg)
      CALL setint(0,POPR,Bstofc)
      CALL setdp(0D0,PB,Bstb)
      CALL setdp(0D0,PARIMA,Bstap)
      CALL intlst(POPR,Bsto,Bstno)
      CALL intlst(POPR,Bstopt,Bstnot)
      CALL intlst(3*PMDL,Bstm,Bstnm)
      Bstm(AR)=1
      Bstm(MA)=1
      CALL setchr(' ',POPRCR*POPR,Bstot)
      CALL setchr(' ',132,Bstdsn)
c-----------------------------------------------------------------------
c     Copy current model variables into temporary storage variables.
c-----------------------------------------------------------------------
      CALL cpyint(Opr(0),POPR+1,1,Bsto(0))
      CALL cpyint(Oprptr(0),POPR+1,1,Bstopt(0))
      CALL cpyint(Oprfac,POPR,1,Bstofc)
      CALL cpyint(Mdl(0),3*PMDL+1,1,Bstm(0))
      CALL copy(Arimap,PARIMA,1,Bstap)
      CALL copy(B,PB,1,Bstb)
      CALL copylg(Arimaf,PARIMA,1,Bstafx)
      CALL cpyint(Arimal,PARIMA,1,Bstalg)
      Bstsdf=Lseadf
      Bstot=Oprttl
      Bstno=Nopr
      Bstnot=Noprtl
      Bstnm=Nmdl
      Bstdsn(1:Nmddcr)=Mdldsn(1:Nmddcr)
      Bnsedf=Nnsedf
      Bseadf=Nseadf
      Nbstds=Nmddcr
      Bstptd=Picktd
c-----------------------------------------------------------------------
      Bstngr=Ngrp
      Bsngrt=Ngrptl
      Bsncxy=Ncxy
      Bstnb=Nb
      Bstnct=Ncoltl
      Bstctl=Colttl
      Bstgtl=Grpttl
      CALL cpyint(Colptr(0),PB+1,1,Bclptr(0))
      CALL cpyint(Grp(0),PGRP+1,1,Bstgrp(0))
      CALL cpyint(Grpptr(0),PGRP+1,1,Bsgptr(0))
      CALL cpyint(Rgvrtp,PB,1,Bstrgv)
c-----------------------------------------------------------------------
      RETURN
      END
