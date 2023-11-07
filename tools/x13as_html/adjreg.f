C     Last change:  BCM  25 Nov 1998   12:41 pm
      SUBROUTINE adjreg(Orix,Orixmv,Orixot,Ftd,Fao,Fls,Ftc,Fso,Fsea,
     &                  Fcyc,Fusr,Fmv,Fhol,Fcntyp,Lam,Nrxy,N)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c    Computes adjustment factors for trading day, holiday, outlier,
c AO outlier, and user defined regression effects.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'extend.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11fac.cmn'
      INCLUDE 'x11log.cmn'
      INCLUDE 'prior.prm'
      INCLUDE 'prior.cmn'
      INCLUDE 'inpt.cmn'
      INCLUDE 'orisrs.cmn'
      INCLUDE 'units.cmn'
c     ------------------------------------------------------------------
      DOUBLE PRECISION ZERO,ONE
      PARAMETER(ZERO=0D0,ONE=1D0)
c-----------------------------------------------------------------------
      INTEGER i,Nrxy,N,Fcntyp
      DOUBLE PRECISION Orix,Orixmv,Orixot,orixa,orixcl,Lam,Ftd,Fao,Fusr,
     &                 Fmv,Fhol,Fls,Ftc,Fso,Fsea,Fcyc,temp
      DIMENSION Orix(PLEN),Orixmv(PLEN),Orixot(PLEN),orixa(PLEN),
     &          orixcl(PLEN),Ftd(PLEN),Fao(PLEN),Fusr(PLEN),Fmv(PLEN),
     &          Fhol(PLEN),Fls(PLEN),Ftc(PLEN),Fso(PLEN),Fsea(PLEN),
     &          Fcyc(PLEN)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
c     Adjust original series (extended by forecasts and backcasts)
c     using regression effects.
c-----------------------------------------------------------------------
c      call fopen('debug.x12','unknown',IDB,fok)
c-----------------------------------------------------------------------
c     Initialize regression adjusted forecast and backcast extended 
c     original series.
c-----------------------------------------------------------------------
      CALL setdp(0D0,PLEN,orixa)
      CALL setdp(0D0,PLEN,Orixmv)
      CALL setdp(0D0,PLEN,Orixot)
      CALL setdp(0D0,PLEN,orixcl)
c-----------------------------------------------------------------------
      DO i=1,Nrxy
       orixa(i+Pos1bk-1)=Orix(i+Pos1bk-1)-Ftd(i)-Fls(i)-Fhol(i)-Fao(i)-
     &                   Ftc(i)-Fusr(i)-Fmv(i)-Fsea(i)-Fso(i)-Fcyc(i)
       Orixmv(i+Pos1bk-1)=Orix(i+Pos1bk-1)-Fmv(i)
       Orixot(i+Pos1bk-1)=Orixmv(i+Pos1bk-1)-Fao(i)-Fls(i)-Ftc(i)-Fso(i)
       orixcl(i+Pos1bk-1)=Orixmv(i+Pos1bk-1)-Ftd(i)-Fhol(i)
      END DO
*      write(Mtprof,*) ' Orix(Pos1bk) = ',Orix(Pos1bk)
c-----------------------------------------------------------------------
c      call fclose(IDB)
c-----------------------------------------------------------------------
c     Transform series back to original scale
c-----------------------------------------------------------------------
      CALL invfcn(orix(Pos1bk),Nrxy,Fcntyp,Lam,orix(Pos1bk))
      CALL invfcn(orixa(Pos1bk),Nrxy,Fcntyp,Lam,orixa(Pos1bk))
      CALL invfcn(Orixmv(Pos1bk),Nrxy,Fcntyp,Lam,Orixmv(Pos1bk))
      CALL invfcn(Orixot(Pos1bk),Nrxy,Fcntyp,Lam,Orixot(Pos1bk))
      CALL invfcn(orixcl(Pos1bk),Nrxy,Fcntyp,Lam,orixcl(Pos1bk))
c-----------------------------------------------------------------------
      IF(Posfob.eq.Posffc)THEN
       N=Nrxy+Sp
      ELSE
       N=Nrxy
      END IF
      IF(dpeq(Lam,0D0).or.dpeq(Lam,1D0))THEN
       CALL invfcn(Ftd,N,Fcntyp,Lam,Ftd)
       CALL invfcn(Fhol,N,Fcntyp,Lam,Fhol)
       CALL invfcn(Fls,N,Fcntyp,Lam,Fls)
       CALL invfcn(Ftc,N,Fcntyp,Lam,Ftc)
       CALL invfcn(Fao,N,Fcntyp,Lam,Fao)
       CALL invfcn(Fso,N,Fcntyp,Lam,Fso)
       CALL invfcn(Fsea,N,Fcntyp,Lam,Fsea)
       CALL invfcn(Fusr,N,Fcntyp,Lam,Fusr)
       CALL invfcn(Fcyc,N,Fcntyp,Lam,Fcyc)
      END IF
c     ------------------------------------------------------------------
c     copy adjusted series, adjustment factors into variables
c     properly indexed for X-11 seasonal adjustment routines
c-----------------------------------------------------------------------
      CALL copy(orixa(Pos1bk),Nrxy,1,Stcsi(Pos1bk))
*      write(Mtprof,*) ' stcsi(Pos1bk) = ',stcsi(Pos1bk)
      IF(Nbcst.gt.0)THEN
       CALL copy(Orix(Pos1bk),Nbcst,1,Series(Pos1bk))
       IF(Kfmt.gt.0)
     &    CALL addmul(Series,Series,Sprior,Pos1bk,Pos1bk+Nbcst-1)
      END IF
      IF(Nfcst.gt.0)THEN
       CALL copy(Orix(Posfob+1),Nfcst,1,Series(Posfob+1))
       IF(Kfmt.gt.0)CALL addmul(Series,Series,Sprior,Posfob+1,Posffc)
      END IF       
      IF(dpeq(Lam,0D0).or.dpeq(Lam,1D0))THEN
       IF(.not.Axrgtd.and.Adjtd.eq.1)CALL copy(Ftd,N,1,Factd(Pos1bk))
       IF(Adjhol.eq.1)CALL copy(Fhol,N,1,Fachol(Pos1bk))
       IF(Adjao.eq.1)CALL copy(Fao,N,1,Facao(Pos1bk))
       IF(Adjls.eq.1)CALL copy(Fls,N,1,Facls(Pos1bk))
       IF(Adjtc.eq.1)CALL copy(Ftc,N,1,Factc(Pos1bk))
       IF(Adjso.eq.1)CALL copy(Fso,N,1,Facso(Pos1bk))
       IF(Adjsea.eq.1)CALL copy(Fsea,N,1,Facsea(Pos1bk))
       IF(Adjusr.eq.1)CALL copy(Fusr,N,1,Facusr(Pos1bk))
       IF(Adjcyc.eq.1)CALL copy(Fcyc,N,1,Faccyc(Pos1bk))
      END IF
c-----------------------------------------------------------------------
c     Set 'extra' backcasts in series, factors to zero
c-----------------------------------------------------------------------
      IF(Nbcst2.gt.Nbcst)THEN
       temp=ZERO
       IF(Fcntyp.eq.1)temp=ONE
       DO i=1,Pos1bk-1
        IF((.not.Axrgtd).and.Adjtd.eq.1)Factd(i)=temp
        IF((.not.Axrghl).and.Adjhol.eq.1)Fachol(i)=temp
        IF(Adjao.eq.1)Facao(i)=temp
        IF(Adjls.eq.1)Facls(i)=temp
        IF(Adjtc.eq.1)Factc(i)=temp
        IF(Adjso.eq.1)Facso(i)=temp
        IF(Adjsea.eq.1)Facsea(i)=temp
        IF(Adjusr.eq.1)Facusr(i)=temp
        IF(Adjcyc.eq.1)Faccyc(i)=temp
       END DO
      END IF
c-----------------------------------------------------------------------
c     put outlier adjusted and missing value adjusted series back on
c     original scale BCM May 2004
c-----------------------------------------------------------------------
      IF(Kfmt.gt.0)THEN
       CALL addmul(Orixmv,Orixmv,Sprior,Pos1bk,Posffc)
       CALL addmul(Orixot,Orixot,Sprior,Pos1bk,Posffc)
       CALL addmul(orixcl,Orixcl,Sprior,Pos1bk,Posffc)
      END IF
c-----------------------------------------------------------------------
c     copy calendar adjusted series with forecasts into Stocal
c     BCM May 2006
c-----------------------------------------------------------------------
      CALL copy(orixcl(Pos1bk),Nrxy,1,Stocal(Pos1bk))
c-----------------------------------------------------------------------
      RETURN
      END
