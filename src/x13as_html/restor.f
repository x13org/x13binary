C     Last change:  BCM  16 Feb 1999    3:52 pm
      SUBROUTINE restor(Lmodel,Lx11,Lx11rg)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C     USE TEMPORARY VARIABLES TO RESET ORIGINAL SEASONAL ADJUSTMENT
C     OPTIONS for the sliding spans and revisions analysis options.
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
      INCLUDE 'lzero.cmn'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'ssprep.cmn'
      INCLUDE 'x11opt.cmn'
c-----------------------------------------------------------------------
      INTEGER PACM
      PARAMETER(PACM=(PLEN+2*PORDER)*PARIMA)
c     ------------------------------------------------------------------
      LOGICAL Lmodel,Lx11,Lx11rg
      INTEGER i
c-----------------------------------------------------------------------
      Kfmt=Kfm2
c-----------------------------------------------------------------------
      IF(Lx11)THEN
       DO i=1,12
        Lter(i)=Lt2(i)
       END DO
c       Lopt=Lop2
       Ktcopt=Ktc2
       Tic=Tc2
      END IF
      IF(Lx11rg)THEN
c       DO i=1,7
c        Dwt(i)=Dwt2(i)
c        D(i)=0.
c       END DO
       Kswv=Ksw2
       Khol=Kh2
      END IF
c-----------------------------------------------------------------------
C     USE TEMPORARY VARIABLES TO RESET ORIGINAL regARIMA OPTIONS.
c-----------------------------------------------------------------------
      IF(Lmodel)THEN
       Ngrp=Ngr2
       Ngrptl=Ngrt2
       Ncxy=Ncxy2
       Nb=Nbb
       Priadj=Pri2
       Ncoltl=Nct2
       i=PCOLCR*PB
       Colttl(1:i)=Cttl(1:i)
       i=PGRPCR*PGRP
       Grpttl(1:i)=Gttl(1:i)
       CALL cpyint(Clptr(0),PB+1,1,Colptr(0))
       CALL cpyint(G2(0),PGRP+1,1,Grp(0))
       CALL cpyint(Gptr(0),PGRP+1,1,Grpptr(0))
       CALL cpyint(Rgv2,PB,1,Rgvrtp)
       CALL copy(Ap2,PARIMA,1,Arimap)
       CALL copy(Bb,PB,1,B)
       CALL copylg(Fxa,PB,1,Arimaf)
       Nrxy=Nr2
       Iregfx=Irfx2
       CALL copylg(Regfx2,PB,1,Regfx)
       Ncusrx=Ncusr2
       Picktd=Pktd2
       Adjtd=Atd
       Adjhol=Ahol
       Adjao=Aao
       Adjls=Als
       Adjtc=Atc
       Adjso=Aso
       Adjsea=Asea
       Adjusr=Ausr
       Finhol=Fnhol
       Finao=Fnao
       Finls=Fnls
       Fintc=Fntc
       Finusr=Fnusr
       Fulltd=Flltd
       Lma=Lma2
       Lar=Lar2
       Nintvl=Nintv2
       Nextvl=Nextv2
       Mxdflg=Mxdfl2
       Mxarlg=Mxarl2
       Mxmalg=Mxmal2
       Var=V2
       CALL copy(Chx2,PXPX,1,Chlxpx)
       CALL copy(Chg2,PGPG,1,Chlgpg)
       CALL copy(Acm2,PACM,1,Armacm)
       Lndtcv=Dtcv2
      END IF
c-----------------------------------------------------------------------
      RETURN
c-----------------------------------------------------------------------
      END
