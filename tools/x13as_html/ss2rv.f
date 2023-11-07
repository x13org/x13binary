C     Last change:  BCM  23 Mar 2005    1:33 pm
      SUBROUTINE ss2rv(Lmodel,Lx11,Lx11rg,Lseats)
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
      INCLUDE 'agr.cmn'
      INCLUDE 'adj.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'lzero.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'x11reg.cmn'
      INCLUDE 'xrgmdl.cmn'
      INCLUDE 'usrxrg.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'ssprep.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'ss2rv.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'xeastr.cmn'
      INCLUDE 'seatlg.cmn'
      INCLUDE 'seatmd.cmn'
      INCLUDE 'stcfcm.cmn'
c-----------------------------------------------------------------------
      INTEGER PACM
      PARAMETER(PACM=(PLEN+2*PORDER)*PARIMA)
c-----------------------------------------------------------------------
      INTEGER i
      LOGICAL Lmodel,Lx11,Lx11rg,Lseats
c-----------------------------------------------------------------------
C  ****  Store selected options for seasonal adjustment in temporary
C  ****  variables.
c-----------------------------------------------------------------------
c     Store prior adjustment option
c-----------------------------------------------------------------------
      Kfm2rv=Kfm2
      Iagrrv=Iagr
c-----------------------------------------------------------------------
      IF(Lx11)THEN
       DO i=1,12
        Lt2rv(i)=Lt2(i)
       END DO
c       Lop2=Lopt
       Ktc2rv=Ktc2
       Tc2rv=Tc2
      END IF
c-----------------------------------------------------------------------
      IF(Lx11rg)THEN
c       DO i=1,7
c        Dwt2(i)=Dwt(i)
c       END DO
       Ksw2rv=Ksw2
c-----------------------------------------------------------------------
       Ngx2rv=Nxgrp
       Ngrx2r=Ngrptx
       Nxxy2r=Nxcxy
       Nbbxrv=Nbx
       Ncx2rv=Ncoltx
       i=PCOLCR*PB
       Cttxrv(1:i)=Colttx(1:i)
       i=PGRPCR*PGRP
       Gttxrv(1:i)=Grpttx(1:i)
       CALL cpyint(Clxptr(0),PB+1,1,Cxptrv(0))
       CALL cpyint(Grpx(0),PGRP+1,1,Gx2rv(0))
       CALL cpyint(Gpxptr(0),PGRP+1,1,Gptxrv(0))
       CALL cpyint(Rgxvtp,PB,1,Rgvx2r)
       Nxr2rv=Nxrxy
       Ncxu2r=Ncxusx
       Ifxx2r=Irgxfx
       CALL copylg(Regfxx,PB,1,Rxfx2r)
       CALL cpyint(Xbegxy,2,1,Xbxyrv)
       CALL copy(Bx,PB,1,Bbxrv)
c-----------------------------------------------------------------------
      END IF
c-----------------------------------------------------------------------
c  ****  Store model parameters to be saved in temporary variables
c-----------------------------------------------------------------------
      IF(Lmodel)THEN
c-----------------------------------------------------------------------
c     Reset value of Priadj if reset in tdlom subroutine.
c-----------------------------------------------------------------------
       Pri2rv=Pri2
c-----------------------------------------------------------------------
       Ngr2rv=Ngrp
       Ngrt2r=Ngrptl
       Ncxy2r=Ncxy
       Nbbrv=Nb
       Nct2rv=Ncoltl
       i=PCOLCR*PB
       Cttlrv(1:i)=Colttl(1:i)
       i=PGRPCR*PGRP
       Gttlrv(1:i)=Grpttl(1:i)
       CALL cpyint(Colptr(0),PB+1,1,Clptrv(0))
       CALL cpyint(Grp(0),PGRP+1,1,G2rv(0))
       CALL cpyint(Grpptr(0),PGRP+1,1,Gptrrv(0))
       CALL cpyint(Rgvrtp,PB,1,Rgv2rv)
       CALL copy(Arimap,PARIMA,1,Ap2rv)
       CALL copy(B,PB,1,Bbrv)
       CALL copylg(Arimaf,PARIMA,1,Fxarv)
       Nr2rv=Nrxy
       Ncus2r=Ncusrx
       Irfx2r=Iregfx
       CALL copylg(Regfx,PB,1,Rgfx2r)
       Pktd2r=Picktd
       Atdrv=Adjtd
       Aholrv=Adjhol
       Aaorv=Adjao
       Alsrv=Adjls
       Atcrv=Adjtc
       Asorv=Adjso
       Asearv=Adjsea
       Acycrv=Adjcyc
       Ausrrv=Adjusr
       Fnholr=Finhol
       Fnaorv=Finao
       Fnlsrv=Finls
       Fntcrv=Fintc
       Fnusrv=Finusr
       Flltdr=Fulltd
       Ltaorv=Ltstao
       Ltlsrv=Ltstls
       Lttcrv=Ltsttc
*       Ltsorv=Ltstso
       Lma2r=Lma
       Lar2r=Lar
       Nint2r=Nintvl
       Next2r=Nextvl
       Mxdf2r=Mxdflg
       Mxar2r=Mxarlg
       Mxma2r=Mxmalg
       V2r=Var
       CALL copy(Chlxpx,PXPX,1,Chx2r)
       CALL copy(Chlgpg,PGPG,1,Chg2r)
       CALL copy(Armacm,PACM,1,Acm2r)
       Dtcv2r=Lndtcv
      END IF
c-----------------------------------------------------------------------
      Nsporv=Nspobs
      Nbk2rv=Nbcst2
      Lsprv=Lsp
      Ly0rv=Ly0
      Lstyrv=Lstyr
      Lyrrv=Lyr
      CALL cpyint(Begspn,2,1,Bspnrv)
      CALL cpyint(Endspn,2,1,Espnrv)
      Frstrv=Frstsy
      CALL cpyint(Begmdl,2,1,Bmdlrv)
      CALL cpyint(Endmdl,2,1,Emdlrv)
      Ixrgrv=Ixreg
      CALL cpyint(Begxrg,2,1,Bxrgrv)
      CALL cpyint(Endxrg,2,1,Exrgrv)
      Kholrv=Khol
      Keasrv=Keastr
      Lgnxrv=Lgenx
      CALL copylg(Prttab,NTBL,1,Prtbrv)
      A1strv=Adj1st
      CALL cpyint(Begxy,2,1,Bgxyrv)
c-----------------------------------------------------------------------
      IF(Lseats)THEN
       Htrrv = Havetr
       Hsfrv = Havesf
       Hirrv = Haveir
       Hsarv = Havesa
       Hcyrv = Havecy
       Hftrrv = Havftr
       Hfsfrv = Havfsf
       Hfirrv = Havfir
       Hfsarv = Havfsa
       Hfcyrv = Havfcy
       Hsftrv = Hseftr
       Hsfsrv = Hsefsf
       Hsforv = Hsefor
       Hsfarv = Hsefsa
       Hsfcrv = Hsefcy
       Hrftrv = Hsrftr
       Hrfsrv = Hsrfsf
       Hrfarv = Hsrfsa
       Hrfcrv = Hsrfcy
       Hstarv = Hvstsa
       Hstirv = Hvstir
       Ntcnrv = Ntcnum
       Ntcdrv = Ntcden
       Nsnrv  = Nsnum 
       Nsdrv  = Nsden 
       Nsanrv = Nsanum
       Nsadrv = Nsaden
       Ntrnrv = Ntrnum
       Ntrdrv = Ntrden
       Ntcwkr = Ntcwkf
       Nsawkr = Nsawkf
       Nswkrv = Nswkf 
       Ntrwkr = Ntrwkf
       Nirwkr = Nirwkf
       Tcvrv  = Tcvar 
       Svrv   = Svar  
       Savrv  = Savar 
       Trvrv  = Trvar 
       Irrvrv = Irrvar
       Hsttrv = Hvstft
       Hstsrv = Hvstfs
       Hstorv = Hvstfo
       Hstdrv = Hvstfa
       Hstcrv = Hvstfc
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
