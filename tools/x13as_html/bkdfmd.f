C     Last change:  BCM  16 Jul 2003    5:11 pm
      SUBROUTINE bkdfmd(Backup)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'tbllog.prm'
c      INCLUDE 'tbllog.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'ss2rv.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
c-----------------------------------------------------------------------
      INTEGER PACM
      PARAMETER(PACM=(PLEN+2*PORDER)*PARIMA)
c-----------------------------------------------------------------------
      LOGICAL Backup
      INTEGER i
c-----------------------------------------------------------------------
      IF(Backup)THEN
       Pri2rv=Priadj
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
c-----------------------------------------------------------------------
      ELSE
       Priadj=Pri2rv
       Ngrp=Ngr2rv
       Ngrptl=Ngrt2r
       Ncxy=Ncxy2r
       Nb=Nbbrv
       Ncoltl=Nct2rv
       i=PCOLCR*PB
       Colttl(1:i)=Cttlrv(1:i)
       i=PGRPCR*PGRP
       Grpttl(1:i)=Gttlrv(1:i)
       CALL cpyint(Clptrv(0),PB+1,1,Colptr(0))
       CALL cpyint(G2rv(0),PGRP+1,1,Grp(0))
       CALL cpyint(Gptrrv(0),PGRP+1,1,Grpptr(0))
       CALL cpyint(Rgv2rv,PB,1,Rgvrtp)
       CALL copy(Ap2rv,PARIMA,1,Arimap)
       CALL copy(Bbrv,PB,1,B)
       CALL copylg(Fxarv,PARIMA,1,Arimaf)
       Nrxy=Nr2rv
       Ncusrx=Ncus2r
       Iregfx=Irfx2r
       CALL copylg(Rgfx2r,PB,1,Regfx)
       Picktd=Pktd2r
       Adjhol=Aholrv
       Adjtd=Atdrv
       Adjao=Aaorv
       Adjls=Alsrv
       Adjtc=Atcrv
       Adjso=Asorv
       Adjsea=Asearv
       Adjusr=Ausrrv
       Finhol=Fnholr
       Finao=Fnaorv
       Finls=Fnlsrv
       Fintc=Fntcrv
       Finusr=Fnusrv
       Fulltd=Flltdr
       Ltstao=Ltaorv
       Ltstls=Ltlsrv
       Ltsttc=Lttcrv
*       Ltstso=Ltsorv
       Lma=Lma2r
       Lar=Lar2r
       Nintvl=Nint2r
       Nextvl=Next2r
       Mxdflg=Mxdf2r
       Mxarlg=Mxar2r
       Mxmalg=Mxma2r
       Var=V2r
       CALL copy(Chx2r,PXPX,1,Chlxpx)
       CALL copy(Chg2r,PGPG,1,Chlgpg)
       CALL copy(Acm2r,PACM,1,Armacm)
       Lndtcv=Dtcv2r
c-----------------------------------------------------------------------
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
      
