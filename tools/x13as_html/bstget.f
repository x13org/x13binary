C     Last change:  BCM   3 Dec 97   11:53 am
      SUBROUTINE bstget(Nbstds,Bstdsn)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Routine which loads the best model found by the automatic
c     ARIMA modelling routine from temporary storage variables into
c     their correct locations.
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'bstmdl.cmn'
c-----------------------------------------------------------------------
      INTEGER Nbstds,i,ilag,endlag
      CHARACTER Bstdsn*(132)
c-----------------------------------------------------------------------
c     Initialize ARIMA modeling variables
c-----------------------------------------------------------------------
      CALL setlg(.false.,PARIMA,Arimaf)
      CALL setint(0,PARIMA,Arimal)
      CALL setint(0,POPR,Oprfac)
c  Bob Fay
      CALL setdp(0.D0,PB,B)
      CALL setdp(0.D0,PARIMA,Arimap)
      CALL intlst(POPR,Opr,Nopr)
      CALL intlst(POPR,Oprptr,Noprtl)
      CALL intlst(3*PMDL,Mdl,Nmdl)
      Mdl(AR)=1
      Mdl(MA)=1
      CALL setchr(' ',POPRCR*POPR,Oprttl)
c-----------------------------------------------------------------------
c     Copy temporary storage variables containing best model into 
c     current model variables.
c-----------------------------------------------------------------------
      CALL cpyint(Bsto(0),POPR+1,1,Opr(0))
      CALL cpyint(Bstopt(0),POPR+1,1,Oprptr(0))
      CALL cpyint(Bstofc,POPR,1,Oprfac)
      CALL cpyint(Bstm(0),3*PMDL+1,1,Mdl(0))
      CALL copy(Bstap,PARIMA,1,Arimap)
      CALL copy(Bstb,PB,1,B)
      CALL copylg(Bstafx,PARIMA,1,Arimaf)
      CALL cpyint(Bstalg,PARIMA,1,Arimal)
      Lseadf=Bstsdf
      Oprttl=Bstot
      Nopr=Bstno
      Noprtl=Bstnot
      Nmdl=Bstnm
      Mdldsn(1:Nbstds)=Bstdsn(1:Nbstds)
      Nnsedf=Bnsedf
      Nseadf=Bseadf
      Nmddcr=Nbstds
c-----------------------------------------------------------------------
      Ngrp=Bstngr
      Ngrptl=Bsngrt
      Ncxy=Bsncxy
      Nb=Bstnb 
      Ncoltl=Bstnct
      Colttl=Bstctl
      Grpttl=Bstgtl
      CALL cpyint(Bclptr(0),PB+1,1,Colptr(0))
      CALL cpyint(Bstgrp(0),PGRP+1,1,Grp(0))
      CALL cpyint(Bsgptr(0),PGRP+1,1,Grpptr(0))
      CALL cpyint(Bstrgv,PB,1,Rgvrtp)
c-----------------------------------------------------------------------
c     Clear model parameters so estimation of the model parameters
c     will begin from scratch
c-----------------------------------------------------------------------
      DO i=1,Nb
       B(i)=DNOTST
      END DO
      IF(Nopr.gt.0)THEN
       endlag=Opr(Nopr)-1
       DO ilag=1,endlag
        IF(.not.Arimaf(ilag))Arimap(ilag)=DNOTST
       END DO
      END IF
c-----------------------------------------------------------------------
c     Compute maximum lags for best model
c-----------------------------------------------------------------------
      CALL maxlag(Arimal,Opr,Mdl(DIFF-1),Mdl(DIFF)-1,Mxdflg)
      CALL maxlag(Arimal,Opr,Mdl(AR-1),Mdl(AR)-1,Mxarlg)
      CALL maxlag(Arimal,Opr,Mdl(MA-1),Mdl(MA)-1,Mxmalg)
c-----------------------------------------------------------------------
c     Compute the number of effective observations and initialize |G'G|
c-----------------------------------------------------------------------
      Lar=Lextar.and.Mxarlg.gt.0
      Lma=Lextma.and.Mxmalg.gt.0
c     ------------------------------------------------------------------
      IF(Lextar)THEN
       Nintvl=Mxdflg
       Nextvl=Mxarlg+Mxmalg
c     ------------------------------------------------------------------
      ELSE
       Nintvl=Mxdflg+Mxarlg
c     ------------------------------------------------------------------
       Nextvl=0
       IF(Lextma)Nextvl=Mxmalg
      END IF
c-----------------------------------------------------------------------
      RETURN
      END

