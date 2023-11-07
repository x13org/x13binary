C     Last change:  BCM  23 Mar 2005    1:33 pm
      SUBROUTINE rv2ss(Lmodel,Lx11,Lx11rg,Lseats)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C     USE TEMPORARY VARIABLES TO RESET ORIGINAL SEASONAL ADJUSTMENT
C     OPTIONS for the sliding spans and revisions analysis options.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'agr.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'adj.cmn'
      INCLUDE 'lzero.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'inpt.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'ssprep.cmn'
      INCLUDE 'ss2rv.cmn'
      INCLUDE 'x11reg.cmn'
      INCLUDE 'xrgmdl.cmn'
      INCLUDE 'usrxrg.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'seatlg.cmn'
      INCLUDE 'seatmd.cmn'
      INCLUDE 'stcfcm.cmn'
      INCLUDE 'orisrs.cmn'
      INCLUDE 'xeastr.cmn'
c-----------------------------------------------------------------------
      INTEGER PACM
      PARAMETER(PACM=(PLEN+2*PORDER)*PARIMA)
c-----------------------------------------------------------------------
      LOGICAL Lmodel,Lx11,Lx11rg,Lseats
      INTEGER i
c-----------------------------------------------------------------------
      Kfm2=Kfm2rv
      Iagr=Iagrrv
c-----------------------------------------------------------------------
      IF(Lx11)THEN
       DO i=1,12
        Lt2(i)=Lt2rv(i)
       END DO
       Ktc2=Ktc2rv
       Tc2=Tc2rv
      END IF
c-----------------------------------------------------------------------
      IF(Lx11rg)THEN
       Ksw2=Ksw2rv
c-----------------------------------------------------------------------
       Nxgrp=Ngx2rv
       Ngrptx=Ngrx2r
       Nxcxy=Nxxy2r
       Nbx=Nbbxrv
       Ncoltx=Ncx2rv
       i=PCOLCR*PB
       Colttx(1:i)=Cttxrv(1:i)
       i=PGRPCR*PGRP
       Grpttx(1:i)=Gttxrv(1:i)
       CALL cpyint(Cxptrv(0),PB+1,1,Clxptr(0))
       CALL cpyint(Gx2rv(0),PGRP+1,1,Grpx(0))
       CALL cpyint(Gptxrv(0),PGRP+1,1,Gpxptr(0))
       CALL cpyint(Rgvx2r,PB,1,Rgxvtp)
       Nxrxy=Nxr2rv
       Ncxusx=Ncxu2r
       Irgxfx=Ifxx2r
       CALL copylg(Rxfx2r,PB,1,Regfxx)
       CALL cpyint(Xbxyrv,2,1,Xbegxy)
       CALL copy(Bbxrv,PB,1,Bx)
c-----------------------------------------------------------------------
      END IF
c-----------------------------------------------------------------------
c  ****  Store model parameters to be saved in temporary variables
c-----------------------------------------------------------------------
      IF(Lmodel)THEN
c-----------------------------------------------------------------------
c     Reset value of Priadj if reset in tdlom subroutine.
c-----------------------------------------------------------------------
       Pri2=Pri2rv
c-----------------------------------------------------------------------
       Ngr2=Ngr2rv
       Ngrt2=Ngrt2r
       Ncxy2=Ncxy2r
       Nbb=Nbbrv
       Nct2=Nct2rv
       i=PCOLCR*PB
       Cttl(1:i)=Cttlrv(1:i)
       i=PGRPCR*PGRP
       Gttl(1:i)=Gttlrv(1:i)
       CALL cpyint(Clptrv(0),PB+1,1,Clptr(0))
       CALL cpyint(G2rv(0),PGRP+1,1,G2(0))
       CALL cpyint(Gptrrv(0),PGRP+1,1,Gptr(0))
       CALL cpyint(Rgv2rv,PB,1,Rgv2)
       CALL copy(Ap2rv,PARIMA,1,Ap2)
       CALL copy(Bbrv,PB,1,Bb)
       CALL copylg(Fxarv,PARIMA,1,Fxa)
       Nr2=Nr2rv
       Ncusr2=Ncus2r
       Irfx2=Irfx2r
       CALL copylg(Rgfx2r,PB,1,Regfx2)
       Pktd2=Pktd2r
       Atd=Atdrv
       Ahol=Aholrv
       Aao=Aaorv
       Als=Alsrv
       Atc=Atcrv
       Aso=Asorv
       Asea=Asearv
       Acyc=Acycrv
       Ausr=Ausrrv
       Fnhol=Fnholr
       Fnao=Fnaorv
       Fnls=Fnlsrv
       Fntc=Fntcrv
       Fnusr=Fnusrv
       Flltd=Flltdr
       Ltstao=Ltaorv
       Ltstls=Ltlsrv
       Ltsttc=Lttcrv
*       Ltstso=Ltsorv
       Lma2=Lma2r 
       Lar2=Lar2r
       Nintv2=Nint2r
       Nextv2=Next2r
       Mxdfl2=Mxdf2r
       Mxarl2=Mxar2r
       Mxmal2=Mxma2r
       V2=V2r
       CALL copy(Chx2r,PXPX,1,Chx2)
       CALL copy(Chg2r,PGPG,1,Chg2)
       CALL copy(Acm2r,PACM,1,Acm2)
       Dtcv2=Dtcv2r
      END IF
c-----------------------------------------------------------------------
      Nspobs=Nsporv
      Nofpob=Nspobs+Nfcst
      Nbfpob=Nspobs+Nfcst+Nbcst
      Lsp=Lsprv
      Nbcst2=Nbk2rv
      CALL setxpt(Nfcst,Lx11.or.Lseats,Fctdrp)
      Ly0=Ly0rv
      Lstyr=Lstyrv
      Lyr=Lyrrv
      CALL cpyint(Bspnrv,2,1,Begspn)
      CALL cpyint(Espnrv,2,1,Endspn)
      Frstsy=Frstrv
      Nomnfy=Nobs-Frstsy+1
      Nobspf=min(Nspobs+max(Nfcst-Fctdrp,0),Nomnfy)
      CALL cpyint(Bmdlrv,2,1,Begmdl)
      CALL cpyint(Emdlrv,2,1,Endmdl)
      Ixreg=Ixrgrv
      CALL cpyint(Bxrgrv,2,1,Begxrg)
      CALL cpyint(Exrgrv,2,1,Endxrg)
      Khol=Kholrv
      Keastr=Keasrv
      Lgenx=Lgnxrv
      CALL copy(Orig,PLEN,1,Series)
      CALL copy(Orig,PLEN,1,Stcsi)
      CALL copy(Orig,PLEN,1,Sto)
      CALL copylg(Prtbrv,NTBL,1,Prttab)
      Adj1st=A1strv
      CALL cpyint(Bgxyrv,2,1,Begxy)
c-----------------------------------------------------------------------
      IF(Lseats)THEN
       Havetr=Htrrv 
       Havesf=Hsfrv 
       Haveir=Hirrv 
       Havesa=Hsarv 
       Havecy=Hcyrv 
       Havftr =Hftrrv
       Havfsf =Hfsfrv
       Havfir =Hfirrv
       Havfsa =Hfsarv
       Havfcy=Hfcyrv
       Hseftr=Hsftrv
       Hsefsf=Hsfsrv
       Hsefor=Hsforv
       Hsefsa=Hsfarv
       Hsefcy=Hsfcrv
       Hsrftr=Hrftrv
       Hsrfsf=Hrfsrv
       Hsrfsa=Hrfarv
       Hsrfcy=Hrfcrv
       Hvstsa=Hstarv
       Hvstir=Hstirv
       Ntcnum=Ntcnrv
       Ntcden=Ntcdrv
       Nsnum=Nsnrv 
       Nsden=Nsdrv 
       Nsanum=Nsanrv
       Nsaden=Nsadrv
       Ntrnum=Ntrnrv
       Ntrden=Ntrdrv
       Ntcwkf=Ntcwkr
       Nsawkf=Nsawkr
       Nswkf=Nswkrv
       Ntrwkf=Ntrwkr
       Nirwkf=Nirwkr
       Tcvar=Tcvrv 
       Svar=Svrv  
       Savar=Savrv 
       Trvar=Trvrv 
       Irrvar=Irrvrv
       Hsttrv = Hvstft
       Hstsrv = Hvstfs
       Hstorv = Hvstfo
       Hstdrv = Hvstfa
       Hstcrv = Hvstfc
      END IF
c-----------------------------------------------------------------------
      RETURN
c-----------------------------------------------------------------------
      END
