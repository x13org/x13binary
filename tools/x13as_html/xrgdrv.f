C     Last Change: Oct,2021
C     previous change:  BCM  16 Feb 1999    3:47 pm
      SUBROUTINE xrgdrv(Lmodel,Lx11,Khl2,Lgraf)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This subroutine performs an OLS regression on the irregular
c     component of an X-11 seasonal adjustment.  The regressors have
c     been previously chosen by the user.  The irregular is calculated
c     from transparent seasonal adjustments.
c-----------------------------------------------------------------------
c     Author : Brian Monsell, February 1996
c-----------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(F=.false.,T=.true.)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'xrgmdl.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'priadj.cmn'
      INCLUDE 'priusr.cmn'
      INCLUDE 'x11reg.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'inpt.cmn'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'x11srs.cmn'
c      INCLUDE 'sspinp.cmn'
c      INCLUDE 'rev.prm'
c      INCLUDE 'rev.cmn'
      INCLUDE 'lzero.cmn'
      INCLUDE 'units.cmn'
c-----------------------------------------------------------------------
      CHARACTER ubkttl*(PCOLCR*PUREG)
      DOUBLE PRECISION base,sumf,spr,ubkx
      LOGICAL Lmodel,Lx11,pktd,lest,Lgraf
      INTEGER i,Khl2,next2,nint2,nf2,nb2,pa2,nubk,ubktyp,ubkptr
      DIMENSION spr(PLEN),ubktyp(PUREG),ubkptr(0:PUREG),ubkx(PUSERX)
      EXTERNAL sumf
c-----------------------------------------------------------------------
c      DOUBLE PRECISON adj2(PLEN)
c      INTEGER nadj2
c-----------------------------------------------------------------------
      INCLUDE 'x11opt.cmn'
c----------------------------------------------------------------------
c     Store model, seasonal adjustment parameters before transparent
c     seasonal adjustments.
c-----------------------------------------------------------------------
      CALL ssprep(Lmodel,Lx11,F)
      lest=Lestim
      Lestim=T
      nint2=Nintvl
      Nintvl=0
      next2=Nextvl
      Nextvl=0
c-----------------------------------------------------------------------
c     Make local backup copy of user defined regressors for regARIMA
c     models.
c-----------------------------------------------------------------------
      nubk=0
      IF(Ncusrx.gt.0)THEN
       CALL copy(Userx,PUSERX,1,ubkx)
       CALL cpyint(Usrtyp,PUREG,1,ubktyp)
       CALL cpyint(Usrptr(0),PUREG+1,1,ubkptr(0))
       nubk=Ncusrx
       ubkttl=Usrttl
      END IF
c-----------------------------------------------------------------------
c     Set model adjustment indicators to false
c-----------------------------------------------------------------------
      IF(Adjtd.eq.1)Adjtd=0
      IF(Adjhol.eq.1)Adjhol=0
      IF(Adjao.eq.1)Adjao=0
      IF(Adjls.eq.1)Adjls=0
      IF(Adjtc.eq.1)Adjtc=0
      IF(Adjso.eq.1)Adjso=0
      IF(Adjusr.eq.1)Adjusr=0
      IF(Adjsea.eq.1)Adjsea=0
c-----------------------------------------------------------------------
c     Reset TD logical variables, Prior adjustment factors to ensure
c     that Length of Month adjustment is not performed when TD is
c     chosen in regression spec
c-----------------------------------------------------------------------
      pktd=Picktd
      pa2=Priadj
      IF(Picktd.and.Priadj.gt.1)THEN
       Picktd=F
       Priadj=0
       CALL copy(Sprior,Posfob,-1,spr)
       base=1D0
       IF(Muladd.eq.1)base=0D0
       IF(Nprtyp.eq.0)THEN
        CALL setdp(base,PLEN,Sprior)
        Kfmt=0
       ELSE
        DO i=Pos1ob,Posfob
         Sprior(i)=base
         IF(Nustad.gt.0)THEN
          IF(Muladd.eq.1)THEN
           Sprior(i)=Sprior(i)+Usrtad(Frstat+i-Pos1ob+Lsp-1)
          ELSE
           Sprior(i)=Sprior(i)*Usrtad(Frstat+i-Pos1ob+Lsp-1)
          END IF
         END IF
         IF(Nuspad.gt.0)THEN
          IF(Muladd.eq.1)THEN
           Sprior(i)=Sprior(i)+Usrpad(Frstap+i-Pos1ob+Lsp-1)
          ELSE
           Sprior(i)=Sprior(i)*Usrpad(Frstap+i-Pos1ob+Lsp-1)
          END IF
         END IF
        END DO
       END IF
      ELSE
       pktd=F
      END IF
c-----------------------------------------------------------------------
c     Load X-11 Regression parameters into regARIMA model parameters
c-----------------------------------------------------------------------
      IF(Ixreg.gt.0)CALL loadxr(F)
c-----------------------------------------------------------------------
c     Set number of X-11 forecasts and backcasts to be zero and reset
c     X-11 pointers
c-----------------------------------------------------------------------
      IF(Nfcst.gt.0.or.Nbcst.gt.0)THEN
       nf2=Nfcst
       Nfcst=0
       Nfdrp=0
       nb2=Nbcst
       Nbcst=0
c-----------------------------------------------------------------------
c     Reset X-11 pointers
c-----------------------------------------------------------------------
       Pos1bk=Pos1ob
       Posffc=Posfob
       Nofpob=Nspobs+Nfcst
       Nbfpob=Nspobs+Nfcst+Nbcst
      ELSE
       nf2=0
       nb2=0
      END IF
c-----------------------------------------------------------------------
      IF(Ixreg.gt.0)THEN
       CALL dfdate(Endspn,Endxrg,Sp,Xdsp)
       IF(Xdsp.gt.0)THEN
        Posfob=Posfob-Xdsp
        Posffc=Posfob
        CALL cpyint(Endxrg,2,1,Endspn)
       END IF
      END IF
c-----------------------------------------------------------------------
c     Perform iterations of X-11 to get X-11 Regression
c-----------------------------------------------------------------------
      CALL x11pt1(F,F,Lgraf)
      IF(.not.Lfatal)CALL x11pt2(F,T,F,F,Lgraf)
      IF(.not.Lfatal.and.Khol.eq.1)CALL x11pt3(F,F)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
      IF(Nfcst.ne.nf2.or.Nbcst.ne.nb2)THEN
       Nfcst=nf2
       Nbcst=nb2
       Nfdrp=nf2
c-----------------------------------------------------------------------
c     Reset X-11 pointers
c-----------------------------------------------------------------------
       Pos1bk=Pos1ob-Nbcst
       Posffc=Posfob+Nfcst
       Nofpob=Nspobs+Nfcst
       Nbfpob=Nspobs+Nfcst+Nbcst
      END IF
c-----------------------------------------------------------------------
c     Call X-11 holiday estimation routines
c-----------------------------------------------------------------------
      IF(Khol.eq.1)THEN
       CALL holday(Sti,Mt1,Lgraf,Nfcst,Xdsp)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     If no holiday adjustment is to be done, set holiday pointer
c     variable to proper value.
c-----------------------------------------------------------------------
       IF(Khol.eq.0)THEN
        Khl2=0
        Axhol=F
       ELSE
        Axhol=T
       END IF
      END IF
c-----------------------------------------------------------------------
      IF((Ixreg.gt.0).and.(Xdsp.gt.0))THEN
       CALL addate(Endxrg,Sp,Xdsp,Endspn)
       Posfob=Posfob+Xdsp
       Posffc=Posfob
      END IF
c-----------------------------------------------------------------------
c     Reset X-11, Model parameters
c-----------------------------------------------------------------------
      CALL loadxr(T)
      CALL restor(Lmodel,Lx11,F)
c-----------------------------------------------------------------------
c     Reset Model based TD logical variables
c-----------------------------------------------------------------------
      IF(pktd)THEN
       Picktd=T
       Priadj=pa2
       CALL copy(spr,Posfob,-1,Sprior)
      ELSE IF(Picktd)THEN
       Picktd=F
      END IF
      Lestim=lest
      Nintvl=nint2
      Nextvl=next2
      Nfev=0
c-----------------------------------------------------------------------
c     Add user-defined regressors back to regARIMA model
c-----------------------------------------------------------------------
      IF(nubk.gt.0)THEN
       CALL copy(ubkx,PUSERX,1,Userx)
       CALL cpyint(ubktyp,PUREG,1,Usrtyp)
       CALL cpyint(ubkptr(0),PUREG+1,1,Usrptr(0))
       Ncusrx=nubk
       Usrttl=ubkttl
      END IF
c-----------------------------------------------------------------------
      IF(Ixreg.gt.0)Ixreg=3
c-----------------------------------------------------------------------
      RETURN
      END
