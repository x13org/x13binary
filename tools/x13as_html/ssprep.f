C     Last change:  BCM  16 Feb 1999    3:56 pm
      SUBROUTINE ssprep(Lmodel,Lx11,Lx11rg)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Prepare for sliding spans or revision analysis by storing seasonal
c     adjustment options into temporary variables.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'ssprep.cmn'
      INCLUDE 'x11opt.cmn'
c-----------------------------------------------------------------------
      INTEGER PACM
      PARAMETER(PACM=(PLEN+2*PORDER)*PARIMA)
c-----------------------------------------------------------------------
      INTEGER i
      LOGICAL Lmodel,Lx11,Lx11rg
c-----------------------------------------------------------------------
C  ****  Store selected options for seasonal adjustment in temporary
C  ****  variables.
c-----------------------------------------------------------------------
c     Store prior adjustment option
c-----------------------------------------------------------------------
      Kfm2=Kfmt
      IF(Nprtyp.eq.0.and.Kfm2.gt.0.AND.(.not.Lpradj))Kfm2=0
c-----------------------------------------------------------------------
      IF(Lx11)THEN
       DO i=1,12
        Lt2(i)=Lter(i)
       END DO
c       Lop2=Lopt
       Ktc2=Ktcopt
       Tc2=Tic
      END IF
c-----------------------------------------------------------------------
      IF(Lx11rg)THEN
c       DO i=1,7
c        Dwt2(i)=Dwt(i)
c       END DO
       Ksw2=Kswv
      END IF
c-----------------------------------------------------------------------
c  ****  Store model parameters to be saved in temporary variables
c-----------------------------------------------------------------------
      IF(Lmodel)THEN
c-----------------------------------------------------------------------
c     Reset value of Priadj if reset in tdlom subroutine.
c-----------------------------------------------------------------------
       IF(Picktd.and.Fcntyp.eq.1.and.Priadj.le.0)THEN
        Priadj=Pri2
       ELSE
        Pri2=Priadj
       END IF
c-----------------------------------------------------------------------
       Ngr2=Ngrp
       Ngrt2=Ngrptl
       Ncxy2=Ncxy
       Nbb=Nb
       Nct2=Ncoltl
       i=PCOLCR*PB
       Cttl(1:i)=Colttl(1:i)
       i=PGRPCR*PGRP
       Gttl(1:i)=Grpttl(1:i)
       CALL cpyint(Colptr(0),PB+1,1,Clptr(0))
       CALL cpyint(Grp(0),PGRP+1,1,G2(0))
       CALL cpyint(Grpptr(0),PGRP+1,1,Gptr(0))
       CALL cpyint(Rgvrtp,PB,1,Rgv2)
       CALL copy(Arimap,PARIMA,1,Ap2)
       CALL copy(B,PB,1,Bb)
       CALL copylg(Arimaf,PARIMA,1,Fxa)
       Nr2=Nrxy
       Ncusr2=Ncusrx
       Irfx2=Iregfx
       CALL copylg(Regfx,PB,1,Regfx2)
       Pktd2=Picktd
       Atd=Adjtd
       Ahol=Adjhol
       Aao=Adjao
       Als=Adjls
       Atc=Adjtc
       Aso=Adjso
       Asea=Adjsea
       Acyc=Adjcyc
       Ausr=Adjusr
       Fnhol=Finhol
       Fnao=Finao
       Fnls=Finls
       Fntc=Fintc
       Fnusr=Finusr
       Flltd=Fulltd
       Lma2=Lma
       Lar2=Lar
       Nintv2=Nintvl
       Nextv2=Nextvl
       Mxdfl2=Mxdflg
       Mxarl2=Mxarlg
       Mxmal2=Mxmalg
       V2=Var
       CALL copy(Chlxpx,PXPX,1,Chx2)
       CALL copy(Chlgpg,PGPG,1,Chg2)
       CALL copy(Armacm,PACM,1,Acm2)
       Dtcv2=Lndtcv
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
