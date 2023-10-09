C     Last Change: March,2021,pass Endmdl into getreg.f in order to
c     allow AOSdate-0.0 or LOSdate-0.0 format
C     Last change:  BCM   8 Dec 1998    4:04 pm
      SUBROUTINE gtmdfl(Mdlfil,Mtm,Begsrs,Endmdl,Nobs,Havsrs,Havesp,
     &                  Userx,Nrusrx,Bgusrx,Itdtst,Lmodel,Lestim,Havreg,
     &                  Leastr,Eastst,Luser,Elong,Havtca,Havhol,Rgaicd,
     &                  Lam,Fcntyp,Lomtst,Ch2tst,Chi2cv,Tlimit,Pvaic,
     &                  Lceaic,Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     read regression and/or arima specs from saved model file
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'notset.prm'
c-----------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c-----------------------------------------------------------------------
      DOUBLE PRECISION Userx,Rgaicd,Lam,Chi2cv,Tlimit,Pvaic
      CHARACTER Mdlfil*(PFILCR)
      LOGICAL Inptok,Havsrs,Havesp,Lmodel,Lestim,Havhol,Havreg,Leastr,
     &        Ch2tst,Luser,Elong,Lceaic,Havtca
      INTEGER Begsrs,Endmdl,Nobs,Itdtst,Nrusrx,Bgusrx,nfil,Mtm,iflt,
     &        begopr,endopr,iopr,beglag,endlag,begcol,endcol,ilag,icol,
     &        Lomtst,Eastst,Fcntyp,igrp
      DIMENSION Begsrs(2),Endmdl(2),Bgusrx(2),Userx(*),Rgaicd(PAICT)
c-----------------------------------------------------------------------
      INTEGER nblank,strinx
      LOGICAL getfcn,istrue
      EXTERNAL getfcn,nblank,istrue,strinx
c-----------------------------------------------------------------------
      CHARACTER SPCDIC*15
      INTEGER spcidx,spcptr,PMDSPC,spclog
      PARAMETER(PMDSPC=2)
      DIMENSION spcptr(0:PMDSPC),spclog(2,PMDSPC)
      PARAMETER(SPCDIC='regressionarima')
      DATA spcptr/1,11,16/
c-----------------------------------------------------------------------
c     Open model file
c-----------------------------------------------------------------------
      nfil=nblank(Mdlfil)
      CALL fopen(Mdlfil(1:nfil),'saved model file','OLD',Mtm,Inptok)
      IF(.not.Inptok)THEN
       CALL abend
       RETURN
      END IF
c-----------------------------------------------------------------------
c     Initialize parser input
c-----------------------------------------------------------------------
      CALL intinp(Mtm)
      IF(Lfatal)RETURN
      CALL setint(NOTSET,2*PMDSPC,spclog)
c-----------------------------------------------------------------------
c     Get the regression and ARIMA model.
c-----------------------------------------------------------------------
      DO WHILE (T)
       IF(getfcn(SPCDIC,spcptr,PMDSPC,spcidx,spclog,Inptok))THEN
        GO TO(10,20),spcidx
c     ------------------------------------------------------------------
c     Read Regression portion of model from saved file.
c     ------------------------------------------------------------------
   10   Iregfx=0
        CALL getreg(Begsrs,Endmdl,Nobs,havsrs,Havesp,Userx,Nrusrx,
     &              Bgusrx,Itdtst,Leastr,Eastst,Luser,Elong,Adjtd,Adjao,
     &              Adjls,Adjtc,Adjso,Adjhol,Adjsea,Adjcyc,Adjusr,
     &              Nusrrg,Havtca,Rgaicd,Lam,Fcntyp,Havhol,Lomtst,
     &              Ch2tst,Chi2cv,Tlimit,Pvaic,Lceaic,Inptok)
        IF(Lfatal)RETURN
        IF(.not.Lmodel)Lmodel=T
        IF(.not.Havreg)Havreg=T
        GO TO 30
c     ------------------------------------------------------------------
c     Read ARIMA portion of model from saved file.
c     ------------------------------------------------------------------
   20   Imdlfx=1
        CALL gtarma(Inptok)
        IF(Lfatal)RETURN
        Lmodel=T
        GO TO 30
       END IF
c-----------------------------------------------------------------------
c      Set variables for fixing estimation according to value of Fixmdl
c-----------------------------------------------------------------------
c       Lestim=F
c       IF(Fixmdl.eq.0)Lestim=T
       Lestim=T
       IF(Fixmdl.ge.0)THEN
        IF(Nb.eq.0)THEN
         Iregfx=0
        ELSE
         Iregfx=1
         IF(Fixmdl.ge.2)Iregfx=3
         DO icol=1,Nb
          IF(Fixmdl.ge.2)THEN
           Regfx(icol)=T
          ELSE
           Regfx(icol)=F
          END IF
         END DO
c     ------------------------------------------------------------------
c     set indicator variable for fixed user defined regressors.
c     ------------------------------------------------------------------
         IF((.not.Userfx).and.Ncusrx.gt.0.and.Iregfx.ge.2)THEN
          IF(Iregfx.eq.3)THEN
           Userfx=T
          ELSE
           igrp=strinx(F,Grpttl,Grpptr,1,Ngrptl,'User-defined')
           begcol=Grp(igrp-1)
           endcol=Grp(igrp)-1
           Userfx=istrue(Regfx,begcol,endcol)
          END IF
         END IF
        END IF
        Imdlfx=1
        IF(MOD(Fixmdl,2).eq.1)Imdlfx=3
        DO iflt=AR,MA
         begopr=Mdl(iflt-1)
         endopr=Mdl(iflt)-1
c     ------------------------------------------------------------------
         DO iopr=begopr,endopr
          beglag=Opr(iopr-1)
          endlag=Opr(iopr)-1
c     ------------------------------------------------------------------
          DO ilag=beglag,endlag
           IF(MOD(Fixmdl,2).eq.1)THEN
            Arimaf(ilag)=T
           ELSE
            Arimaf(ilag)=F
           END IF
          END DO
         END DO
        END DO
       END IF
       RETURN
   30  CONTINUE
      END DO
      END
