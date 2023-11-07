C     Last change:  BCM   2 Dec 1998   11:20 am
      SUBROUTINE loadxr(Toxreg)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Load values of the regARIMA regression variables into the X-11
c     regression variable (Toxreg=.true.) or load X-11 regression 
c     variables into regARIMA regression variables (Toxreg=.false.)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'xrgmdl.cmn'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'usrxrg.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
c-----------------------------------------------------------------------
      LOGICAL Toxreg
c-----------------------------------------------------------------------
c     Load values of the regARIMA regression variables into the X-11
c     regression variable
c-----------------------------------------------------------------------
      IF(Toxreg)THEN
       Nxgrp=Ngrp
       Ngrptx=Ngrptl
       Nxcxy=Ncxy
       Nbx=Nb
       Priadx=Priadj
       Ncoltx=Ncoltl
       Colttx=Colttl
       Grpttx=Grpttl
       CALL cpyint(Colptr(0),PB+1,1,Clxptr(0))
       CALL cpyint(Grp(0),PGRP+1,1,Grpx(0))
       CALL cpyint(Grpptr(0),PGRP+1,1,Gpxptr(0))
       CALL cpyint(Rgvrtp,PB,1,Rgxvtp)
       CALL copy(B,PB,1,Bx)
c       CALL copy(Userx,PUSERX,1,Xuserx)
c       Nrxusx=Nrusrx
c       Ncxusx=Ncusrx
       CALL cpyint(Bgusrx,2,1,Bgxusx)
       Nxrxy=Nrxy
       CALL cpyint(Begxy,2,1,Xbegxy)
       Irgxfx=Iregfx
       CALL copylg(Regfx,PB,1,Regfxx)
       Usrxfx=Userfx
       Xeasid=Easidx
       Pckxtd=Picktd
       CALL cpyint(Tddate,2,1,Xtddat)
       Xtdzro=Tdzero
       Xrgmtd=Lrgmtd
       Fulxtd=Fulltd
c-----------------------------------------------------------------------
c     Else load X-11 regression variables into regARIMA regression
c     variables (Toxreg=.false.)
c-----------------------------------------------------------------------
      ELSE
       Ngrp=Nxgrp
       Ngrptl=Ngrptx
       Ncxy=Nxcxy
       Nb=Nbx
       Priadj=Priadx
       Ncoltl=Ncoltx
       Colttl=Colttx
       Grpttl=Grpttx
       Nusrrg=Nusxrg
       CALL cpyint(Clxptr(0),PB+1,1,Colptr(0))
       CALL cpyint(Grpx(0),PGRP+1,1,Grp(0))
       CALL cpyint(Gpxptr(0),PGRP+1,1,Grpptr(0))
       CALL cpyint(Rgxvtp,PB,1,Rgvrtp)
       CALL cpyint(Usxtyp,PUREG,1,Usrtyp)
       CALL cpyint(Usrxpt(0),PUREG+1,1,Usrptr(0))
       Usrttl=Usrxtt
       CALL copy(Bx,PB,1,B)
       CALL copy(Xuserx,PUSERX,1,Userx)
       Nrusrx=Nrxusx
       Ncusrx=Ncxusx
       CALL cpyint(Bgxusx,2,1,Bgusrx)
       Nrxy=Nxrxy
       CALL cpyint(Xbegxy,2,1,Begxy)
       Iregfx=Irgxfx
       CALL copylg(Regfxx,PB,1,Regfx)
       Userfx=Usrxfx
       Picktd=Pckxtd
       Easidx=Xeasid
       CALL cpyint(Xtddat,2,1,Tddate)
       Tdzero=Xtdzro
       Lrgmtd=Xrgmtd
       Fulltd=Fulxtd
c-----------------------------------------------------------------------
c      Set variables associated with Arima models
c-----------------------------------------------------------------------
       Lma=.false.
       Lar=.false.
       Nintvl=0
       Nextvl=0
       Mxdflg=0
       Mxarlg=0
       Mxmalg=0
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
