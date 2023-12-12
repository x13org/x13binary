C     Last change:  BCM  17 Jul 2003    6:22 pm
      SUBROUTINE prtref(Begxy,Nrxy,Fctdrp,Nfcst,Nbcst,Outdec,Ftd,Fhol,
     &                  Fao,Fls,Ftc,Fso,Fusr,Fsea,Fmv,Fcyc,Nusrrg,
     &                  Lseats,Rmcnst,Lgraf)
c     &                  Lseats,Rmcnst,Lgraf,gudrun)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     prtref.f, Release 1, Subroutine Version 1.5, Modified 20 Oct 1994.
c-----------------------------------------------------------------------
c    Computes regression adjustments for the groups of regression
c variables and the regression residuals.  The groups are:  the
c constant, seasonal effects, trading day, holiday, ao outliers,
c ls's and ramps, and user-defined effects.  The adjustments
c are done on the prior-adjusted and transformed scale.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'mdltbl.i'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'title.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'filext.prm'
      INCLUDE 'units.cmn'
c     ------------------------------------------------------------------
      LOGICAL F,T
      INTEGER PTTL
      DOUBLE PRECISION ZERO
      PARAMETER(PTTL=14,ZERO=0D0,F=.false.,T=.true.)
c     ------------------------------------------------------------------
      CHARACTER str*(PCOLCR),ttlstr*(120)
      DOUBLE PRECISION txy,Ftd,Fhol,Fao,Fls,Ftc,Fso,Fusr,Fmv,Fsea,fcyc,
     &                 dvec,trr
      LOGICAL Finao,Finhol,Finls,Fintc,Finusr,Lseats,lusr,Rmcnst,Lgraf
c      LOGICAL Finao,Finhol,Finls,Fintc,Finusr,Lseats,lusr,Rmcnst,Lgraf,
c     &        gudrun
      INTEGER Adjtd,Adjhol,Adjao,Adjls,Adjsea,Adjusr,Adjcyc,begeff,
     &        Begxy,endeff,i,icol,ieff,irgeff,nchr,neff,Nrxy,nttl,nref,
     &        regeff,rgefpt,rgeftp,ttlptr,rtype,iusr,Nusrrg,Adjso,Adjtc,
     &        ibrr,ierr,Fctdrp,Nfcst,Nbcst,Outdec
      DIMENSION Begxy(2),regeff(11),Ftd(PLEN),
     &          Fao(PLEN),Fls(PLEN),Ftc(PLEN),Fusr(PLEN),Fhol(PLEN),
     &          Fmv(PLEN),ttlptr(0:PTTL),txy(PLEN*PTTL),Fsea(PLEN),
     &          Fso(PLEN),Fcyc(PLEN),dvec(1),trr(PLEN)
c-----------------------------------------------------------------------
      COMMON /x11adj/ Adjtd,Adjhol,Adjao,Adjls,Adjtc,Adjso,Adjsea,
     &                Adjcyc,Adjusr,Finhol,Finao,Finls,Fintc,Finusr
c-----------------------------------------------------------------------
c     Get the names from
c  ../dictionary/strary REF <../dictionary/effects.dic
c-----------------------------------------------------------------------
      CHARACTER REFDIC*127
      INTEGER refptr,PREF
      PARAMETER(PREF=14)
      DIMENSION refptr(0:PREF)
      PARAMETER(REFDIC='SeasonalTrading DayLength-of-MonthLength-of-Quar
     &terLeap YearHolidayAOLS and RampTCUser-definedMissing ValueSOConst
     &antTransitory')
      DATA refptr/1,9,20,35,52,61,68,70,81,83,95,108,110,118,128/
c-----------------------------------------------------------------------
      DIMENSION rgefpt(0:PREF),rgeftp(65)
      DATA rgefpt/1,8,21,28,32,36,46,49,56,58,59,60,62,64,66/
      DATA rgeftp/
     &      PRGTSE,PRGTTS,PRGTUS,PRRTSE,PRRTTS,PRATSE,PRATTS,PRGTTD,
     &      PRGTST,PRRTTD,PRRTST,PRATTD,PRATST,PRG1TD,PRR1TD,PRA1TD,
     &      PRG1ST,PRR1ST,PRA1ST,PRGUTD,PRGTSL,PRRTSL,PRATSL,PRGTLM,
     &      PRRTLM,PRATLM,PRGULM,PRGTLQ,PRRTLQ,PRATLQ,PRGULQ,PRGTLY,
     &      PRRTLY,PRATLY,PRGULY,PRGTEA,PRGTEC,PRGTES,PRGTLD,PRGTTH,
     &      PRGTUH,PRGUH2,PRGUH3,PRGUH4,PRGUH5,PRGTAO,PRGTAA,PRGUAO,
     &      PRGTLS,PRGTRP,PRGTAL,PRGTTL,PRGTQD,PRGTQI,PRGULS,PRGTTC,
     &      PRGTAT,PRGTUD,PRGTMV,PRGTSO,PRGUSO,PRGTCN,PRGUCN,PRGCYC,
     &      PRGUCY/
c-----------------------------------------------------------------------
      INCLUDE 'filext.var'
c-----------------------------------------------------------------------
c     Initialize the titles
c-----------------------------------------------------------------------
      CALL intlst(PTTL,ttlptr,nttl)
c-----------------------------------------------------------------------
c     Find which regression effects there are.
c-----------------------------------------------------------------------
c     if removeconstant = yes, also remove constant term - adjust
c     number of regression effects accordingly (BCM - July 2008)
c-----------------------------------------------------------------------
      nref=PREF
      IF(.not.Rmcnst)nref=nref-1
      DO irgeff=1,nref
       begeff=rgefpt(irgeff-1)
       endeff=rgefpt(irgeff)-1
       DO icol=1,Nb
        DO ieff=begeff,endeff
         IF(Rgvrtp(icol).eq.rgeftp(ieff))THEN
          CALL getstr(REFDIC,refptr,PREF,irgeff,str,nchr)
          IF(.not.Lfatal)CALL insstr(str(1:nchr),nttl+1,PTTL,ttlstr,
     &                               ttlptr,nttl)
          IF(Lfatal)RETURN
          regeff(nttl)=irgeff
          GO TO 10
         END IF
        END DO
       END DO
   10  CONTINUE
      END DO
c-----------------------------------------------------------------------
c     Total regression effects
c-----------------------------------------------------------------------
      CALL insstr('Total Reg',nttl+1,PTTL,ttlstr,ttlptr,nttl)
c-----------------------------------------------------------------------
c     Regression residuals
c-----------------------------------------------------------------------
      IF(.not.Lfatal)CALL insstr('Reg Resids',nttl+1,PTTL,ttlstr,ttlptr,
     &                           nttl)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Calculate the factors.
c-----------------------------------------------------------------------
      neff=nttl-2
      dvec(1)=0.0D0
      DO i=1,neff
       CALL dcopy(Nrxy,dvec,0,txy(i),nttl)
       irgeff=regeff(i)
       begeff=rgefpt(irgeff-1)
       endeff=rgefpt(irgeff)-1
c-----------------------------------------------------------------------
c     Find the next column of the given regression effect type
c-----------------------------------------------------------------------
       DO icol=1,Nb
        DO ieff=begeff,endeff
         IF(Rgvrtp(icol).eq.rgeftp(ieff))THEN
          CALL daxpy(Nrxy,B(icol),Xy(icol),Ncxy,txy(i),nttl)
          GO TO 20
         END IF
        END DO
   20   CONTINUE
       END DO
      END DO
c-----------------------------------------------------------------------
c     Calculate the total regression effects
c-----------------------------------------------------------------------
      CALL dcopy(Nrxy,dvec,0,txy(nttl-1),nttl)
c      CALL setdp(0D0,Nrxy,txy(nttl-1))
      DO icol=1,neff
       CALL daxpy(Nrxy,1D0,txy(icol),nttl,txy(nttl-1),nttl)
      END DO
c-----------------------------------------------------------------------
c     Calculate the regression residuals
c-----------------------------------------------------------------------
      CALL dcopy(Nrxy,Xy(Ncxy),Ncxy,txy(nttl),nttl)
c      IF(gudrun)THEN
c        write(Mtprof,*)' txy(i) i=nttl,nttl*Nrxy,nttl='
c        write(Mtprof,1010)(txy(i),i=nttl,nttl*Nrxy,nttl)
c 1010   format(6f15.3)
c      END IF
      CALL daxpy(Nrxy,-1D0,txy(nttl-1),nttl,txy(nttl),nttl)
c     ------------------------------------------------------------------
      IF(Prttab(LESTRE))THEN
*       CALL genSkip(LESTRE)
       CALL prtmtx(Begxy,Sp,txy,Nrxy,nttl,ttlstr,ttlptr,nttl,
     &             'Regression Effects',tbxdic(LESTRE))
       IF(Lfatal)RETURN
      END IF
c     ------------------------------------------------------------------
      IF(Savtab(LESTRE))THEN
       CALL savmtx(LESTRE,Begxy,Sp,txy,Nrxy,nttl,ttlstr,ttlptr,nttl)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
c     If regression residuals are printed or saved, store the regression
c     residuals in a temporary variable trr.  (BCM July 2008)
c-----------------------------------------------------------------------
      IF(Prttab(LESRRS).or.Savtab(LESRRS).or.Lgraf)THEN
       ibrr=1
       ierr=Nrxy-Nbcst-max(Nfcst-Fctdrp,0)
c       if(gudrun)write(Mtprof,*)'ibrr,ierr =',ibrr,ierr
       IF(Nbcst.gt.0)THEN
        ibrr=ibrr+Nbcst
        ierr=ierr+Nfcst
       END IF
c       if(gudrun)write(Mtprof,*)'ibrr,ierr =',ibrr,ierr
c-----------------------------------------------------------------------
       DO i=ibrr,ierr
        trr(i-ibrr+1)=txy(i*nttl)
       END DO
c-----------------------------------------------------------------------
c     print or save the regression residuals.  (BCM July 2008)
c-----------------------------------------------------------------------
       IF(Prttab(LESRRS))THEN
        CALL genSkip(LESRRS)
        CALL prtshd('Residuals from the Estimated Regression Effects',
     &              Begspn,Sp,Nspobs)
        IF(Lfatal)RETURN
        CALL prttbl(Begspn,Sp,trr,Nspobs,'Data',Outdec,'xxx')
       END IF
       IF(Savtab(LESRRS))THEN
        CALL savtbl(LESRRS,Begspn,1,Nspobs,Sp,trr,Serno,Nser,F)
        IF(Lfatal)RETURN
       END IF
       IF(Lgraf)THEN
        CALL savtbl(LESRRS,Begspn,1,Nspobs,Sp,trr,Serno,Nser,T)
        IF(Lfatal)RETURN
       END IF
      END IF
c     ------------------------------------------------------------------
c     Generate prior adjustment factors.  First, initialize regression 
c     factors to zero for SEATS adjustments
c-----------------------------------------------------------------------
      IF(Lseats)CALL setdp(ZERO,PLEN,Fcyc)
c-----------------------------------------------------------------------
c     determine type of regression variable
c-----------------------------------------------------------------------
      iusr=1
      DO icol=1,Nb
       lusr=F
       rtype=Rgvrtp(icol)
       IF(Nusrrg.gt.0)THEN
        IF(rtype.eq.PRGTUD)THEN
         rtype=Usrtyp(iusr)
         iusr=iusr+1
         lusr=T
        ELSE IF((rtype.ge.PRGTUH.and.rtype.le.PRGUH5).or.
     &           rtype.eq.PRGTUS)THEN
         iusr=iusr+1
        END IF
       END IF
c-----------------------------------------------------------------------
c     Generate regARIMA trading day factors
c-----------------------------------------------------------------------
       IF(Adjtd.eq.1.and.
     &   ((rtype.eq.PRGTTD.or.rtype.eq.PRGTST.or.rtype.eq.PRRTTD.or.
     &     rtype.eq.PRRTST.or.rtype.eq.PRATTD.or.rtype.eq.PRATST.or.
     &     rtype.eq.PRG1TD.or.rtype.eq.PRR1TD.or.rtype.eq.PRA1TD.or.
     &     rtype.eq.PRG1ST.or.rtype.eq.PRR1ST.or.rtype.eq.PRA1ST).or.
     &    (rtype.eq.PRGTLM.or.rtype.eq.PRGTSL.or.rtype.eq.PRGTLQ.or.
     &     rtype.eq.PRGTLY.or.rtype.eq.PRRTLM.or.rtype.eq.PRRTSL.or.
     &     rtype.eq.PRRTLQ.or.rtype.eq.PRRTLY.or.rtype.eq.PRATLM.or.
     &     rtype.eq.PRATSL.or.rtype.eq.PRATLQ.or.rtype.eq.PRATLY).or.
     &    (rtype.eq.PRGUTD.or.rtype.eq.PRGULM.or.rtype.eq.PRGULQ.or.
     &     rtype.eq.PRGULY)))
     &    CALL daxpy(Nrxy,B(icol),Xy(icol),Ncxy,Ftd,1)
c-----------------------------------------------------------------------
c     Generate regARIMA holiday factors
c-----------------------------------------------------------------------
       IF(((Adjhol.eq.1).or.Finhol).and.
     &    (rtype.eq.PRGTEA.or.rtype.eq.PRGTEC.or.rtype.eq.PRGTES.or.
     &     rtype.eq.PRGTLD.or.rtype.eq.PRGTTH.or.
     &    (rtype.ge.PRGTUH.and.rtype.le.PRGUH5)))
     &    CALL daxpy(Nrxy,B(icol),Xy(icol),Ncxy,Fhol,1)
c-----------------------------------------------------------------------
c     Generate regARIMA User-defined regression factors 
c-----------------------------------------------------------------------
       IF(((Adjusr.eq.1).or.Finusr).and.(rtype.eq.PRGTUD))
     &    CALL daxpy(Nrxy,B(icol),Xy(icol),Ncxy,Fusr,1)
c-----------------------------------------------------------------------
c     Generate regARIMA seasonal regression factors
c     source added to generate seasonal regression factor when SEATS 
c     is specified for seasonal adjustment (BCM 04-10-05)
c-----------------------------------------------------------------------
       IF((Adjsea.eq.1).and.((rtype.eq.PRGTUS).or.(Lseats.and.
     &    (rtype.eq.PRGTSE.or.rtype.eq.PRGTTS.or.rtype.eq.PRRTSE.or.
     &     rtype.eq.PRRTTS.or.rtype.eq.PRATSE.or.rtype.eq.PRATTS))))
     &    CALL daxpy(Nrxy,B(icol),Xy(icol),Ncxy,Fsea,1)
c-----------------------------------------------------------------------
c     Generate regARIMA AO outlier factors 
c-----------------------------------------------------------------------
       IF(((Adjao.eq.1).or.Finao).and.
     &   (rtype.eq.PRGTAO.or.rtype.eq.PRGTAA.or.rtype.eq.PRGUAO))
     &    CALL daxpy(Nrxy,B(icol),Xy(icol),Ncxy,Fao,1)
c-----------------------------------------------------------------------
c     Generate regARIMA Level Change Outlier factors 
c-----------------------------------------------------------------------
       IF(((Adjls.eq.1).or.Finls).and.(rtype.eq.PRGTLS.or.
     &    rtype.eq.PRGTRP.or.rtype.eq.PRGTAL.or.rtype.eq.PRGTTL.or.
     &    rtype.eq.PRGTQI.or.rtype.eq.PRGTQD.or.rtype.eq.PRGULS))
     &    CALL daxpy(Nrxy,B(icol),Xy(icol),Ncxy,Fls,1)
c-----------------------------------------------------------------------
c     Generate regARIMA Temporary Change Outlier factors
c-----------------------------------------------------------------------
       IF(((Adjtc.eq.1).or.Fintc).and.(rtype.eq.PRGTTC.or.
     &    rtype.eq.PRGTAT))
     &    CALL daxpy(Nrxy,B(icol),Xy(icol),Ncxy,Ftc,1)
c-----------------------------------------------------------------------
c     Generate regARIMA MV outlier factors 
c-----------------------------------------------------------------------
       IF(rtype.eq.PRGTMV)CALL daxpy(Nrxy,B(icol),Xy(icol),Ncxy,Fmv,1)
c-----------------------------------------------------------------------
c     Generate regARIMA SO outlier factors 
c-----------------------------------------------------------------------
*       IF((Adjso.eq.1).and.(rtype.eq.PRGTSO.or.rtype.eq.PRGTAS))
       IF((Adjso.eq.1).and.rtype.eq.PRGTSO.or.rtype.eq.PRGUSO)
     &    CALL daxpy(Nrxy,B(icol),Xy(icol),Ncxy,Fso,1)
c-----------------------------------------------------------------------
c   Generate regARIMA transitory component for SEATS adjustments
c   (Aug 2004 - BCM)
c-----------------------------------------------------------------------
       IF(Lseats.and.lusr)THEN  
        IF(rtype.eq.PRGCYC)THEN
          CALL daxpy(Nrxy,B(icol),Xy(icol),Ncxy,Fcyc,1)
          IF(Adjcyc.eq.0)Adjcyc=1
        END IF
       END IF
c-----------------------------------------------------------------------
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
