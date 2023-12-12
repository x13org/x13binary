      SUBROUTINE chkadj(Ntd,Khol,Lseats,Lam)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     This routine checks to see if Adj and Fin variables need to be 
c     updated by seeing which types of regression effects are present
c     in the regression.
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'x11log.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'units.cmn'
c     ------------------------------------------------------------------
      INTEGER nusr,nsea,iusr,icol,rtype,Ntd,Khol,ncyc
      LOGICAL Lseats
      DOUBLE PRECISION Lam
c     ------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
      IF(Adjhol.lt.0.and.Finhol)Finhol=F
      IF(Adjusr.lt.0.and.Finusr)Finusr=F
      IF(Adjao.lt.0.and.Finao)Finao=F
      IF(Adjls.lt.0.and.Finls)Finls=F
      IF(Adjtc.lt.0.and.Fintc)Fintc=F
c-----------------------------------------------------------------------
      IF(.not.(Adjtd.ge.0.or.Adjhol.ge.0.or.Adjao.ge.0.or.Adjls.ge.0.or.
     &   Adjtc.ge.0.or.Adjso.ge.0.or.Adjsea.ge.0.or.Adjcyc.ge.0.or.
     &   Adjusr.ge.0.or.Finhol.or.Finao.or.Finls.or.Fintc.or.Finusr))
     &   RETURN
c     ------------------------------------------------------------------
c     Check to see if Adj and Fin variables need to be updated
c     First, initialize variables
c     ------------------------------------------------------------------
      nusr=0
      nsea=0
      Ntd=0
      Nao=0
      Nls=0
      Ntc=0
      Nso=0
      Nramp=0
      Nflwtd=0
      Nln=0
      Nsln=0
      Nlp=0
      Nhol=0
      Neas=0
      ncyc=0
      iusr=1
c-----------------------------------------------------------------------
c     Determine type of regression variable
c-----------------------------------------------------------------------
      DO icol=1,Nb
       rtype=Rgvrtp(icol)
       IF(Nusrrg.gt.0)THEN
        IF(rtype.eq.PRGTUD)THEN
         rtype=Usrtyp(iusr)
         iusr=iusr+1
        ELSE IF((rtype.ge.PRGTUH.and.rtype.le.PRGUH5).or.
     &           rtype.eq.PRGTUS)THEN
         iusr=iusr+1
        END IF
       END IF
c-----------------------------------------------------------------------
c     Generate regARIMA trading day factors
c-----------------------------------------------------------------------
       IF((rtype.eq.PRGTTD.or.rtype.eq.PRGTST.or.rtype.eq.PRRTTD.or.
     &    rtype.eq.PRRTST.or.rtype.eq.PRATTD.or.rtype.eq.PRATST.or.
     &    rtype.eq.PRG1TD.or.rtype.eq.PRR1TD.or.rtype.eq.PRA1TD.or.
     &    rtype.eq.PRG1ST.or.rtype.eq.PRR1ST.or.rtype.eq.PRA1ST).or.
     &    (rtype.eq.PRGTLM.or.rtype.eq.PRGTSL.or.rtype.eq.PRGTLQ.or.
     &    rtype.eq.PRGTLY.or.rtype.eq.PRRTLM.or.rtype.eq.PRRTSL.or.
     &    rtype.eq.PRRTLQ.or.rtype.eq.PRRTLY.or.rtype.eq.PRATLM.or.
     &    rtype.eq.PRATSL.or.rtype.eq.PRATLQ.or.rtype.eq.PRATLY).or.
     &    (rtype.eq.PRGUTD.or.rtype.eq.PRGULM.or.rtype.eq.PRGULQ.or.
     &    rtype.eq.PRGULY))THEN
        Ntd=Ntd+1
        IF(rtype.eq.PRGTTD.or.rtype.eq.PRRTTD.or.rtype.eq.PRATTD.or.
     &     rtype.eq.PRG1TD.or.rtype.eq.PRR1TD.or.rtype.eq.PRA1TD.or.
     &     (Isrflw.eq.0.and.rtype.eq.PRGUTD))
     &     Nflwtd=Nflwtd+1
        IF(rtype.eq.PRGTLM.or.rtype.eq.PRGTLQ.or.
     &     rtype.eq.PRRTLM.or.rtype.eq.PRRTLQ.or.
     &     rtype.eq.PRATLM.or.rtype.eq.PRATLQ.or.
     &     rtype.eq.PRGULM.or.rtype.eq.PRGULQ)
     &     Nln=Nln+1
        IF(rtype.eq.PRGTSL.or.rtype.eq.PRRTSL.or.rtype.eq.PRATSL)
     &     Nsln=Nsln+1
        IF(rtype.eq.PRGTLY.or.rtype.eq.PRRTLY.or.rtype.eq.PRATLY.or.
     &     rtype.eq.PRGULY)Nlp=Nlp+1
       END IF
c-----------------------------------------------------------------------
c     Generate regARIMA holiday factors
c-----------------------------------------------------------------------
       IF(rtype.eq.PRGTEA.or.rtype.eq.PRGTLD.or.rtype.eq.PRGTTH.or.
     &    rtype.eq.PRGTEC.or.rtype.eq.PRGTES.or.(rtype.ge.PRGTUH.and.
     &    rtype.le.PRGUH5))THEN
        Nhol=Nhol+1
        IF(rtype.eq.PRGTEA.or.rtype.eq.PRGTEC.or.rtype.eq.PRGTES)
     &     Neas=Neas+1
       END IF
c-----------------------------------------------------------------------
c     Generate regARIMA User-defined regression factors 
c-----------------------------------------------------------------------
       IF(rtype.eq.PRGTUD)nusr=nusr+1
c-----------------------------------------------------------------------
c     Generate regARIMA seasonal regression factors 
c-----------------------------------------------------------------------
       IF(rtype.eq.PRGTUS.or.(Lseats.and.(rtype.eq.PRGTSE.or.
     &    rtype.eq.PRGTTS.or.rtype.eq.PRRTSE.or.rtype.eq.PRRTTS.or.
     &    rtype.eq.PRATSE.or.rtype.eq.PRATTS)))nsea=nsea+1
c-----------------------------------------------------------------------
c     Generate regARIMA AO outlier factors 
c-----------------------------------------------------------------------
       IF(rtype.eq.PRGTAO.or.rtype.eq.PRGUAO.or.rtype.eq.PRGTAA)
     &    Nao=Nao+1
c-----------------------------------------------------------------------
c     Generate regARIMA Level Change Outlier factors 
c-----------------------------------------------------------------------
       IF(rtype.eq.PRGTLS.or.rtype.eq.PRGULS.or.rtype.eq.PRGTRP.or.
     &    rtype.eq.PRGTAL.or.rtype.eq.PRGTTL.or.rtype.eq.PRGTQD.or.
     &    rtype.eq.PRGTQI)THEN
        Nls=Nls+1
        IF(rtype.eq.PRGTRP.or.rtype.eq.PRGTQD.or.rtype.eq.PRGTQI)
     &     Nramp=Nramp+1
       END IF
c-----------------------------------------------------------------------
c     Generate regARIMA Temporary Change Outlier factors
c-----------------------------------------------------------------------
       IF(rtype.eq.PRGTTC.or.rtype.eq.PRGTAT)Ntc=Ntc+1
c-----------------------------------------------------------------------
c     Generate regARIMA So outlier factors 
c-----------------------------------------------------------------------
*       IF(rtype.eq.PRGTSO.or.rtype.eq.PRGTAS)Nso=Nso+1
       IF(rtype.eq.PRGTSO.or.rtype.eq.PRGUSO)Nso=Nso+1
c-----------------------------------------------------------------------
c     Generate regARIMA MV outlier factors 
c-----------------------------------------------------------------------
       IF(rtype.eq.PRGTMV)Nao=Nao+1
c-----------------------------------------------------------------------
c     Generate transitory component 
c-----------------------------------------------------------------------
       IF(rtype.eq.PRGCYC)ncyc=ncyc+1
c-----------------------------------------------------------------------
      END DO
c-----------------------------------------------------------------------
c     reset regression adjustment indicators if no regession effect
c     found and print warning messages.
c-----------------------------------------------------------------------
      IF(Adjtd.eq.1.and.Ntd.eq.0)Adjtd=0
      IF(Adjtd.eq.0.and.Ntd.gt.0)Adjtd=1
      IF(Adjhol.eq.1.and.Nhol.eq.0)THEN
       Adjhol=0
       IF((.NOT.(Axrghl.or.Axruhl.or.Khol.ge.1)).and.Finhol)Finhol=F
      END IF
      IF(Adjhol.eq.0.and.Nhol.gt.0)Adjhol=1
      IF(Adjsea.eq.1.and.nsea.eq.0)Adjsea=0
      IF(Adjsea.eq.0.and.nsea.gt.0)Adjsea=1
      IF(nusr.eq.0)THEN
       IF(Adjusr.eq.1)Adjusr=0
       IF(Finusr)Finusr=F
      ELSE IF(nusr.gt.0)THEN
       IF(Adjusr.eq.0)Adjusr=1
      END IF
      IF(Nao.eq.0)THEN
       IF(Adjao.eq.1)Adjao=0
       IF(Finao)Finao=F
      ELSE IF(Nao.gt.0)THEN
       IF(Adjao.eq.0)Adjao=1
      END IF
      IF(Nls.eq.0)THEN
       IF(Adjls.eq.1)Adjls=0
       IF(Finls)Finls=F
      ELSE IF(Nls.gt.0)THEN
       IF(Adjls.eq.0)Adjls=1
      END IF
      IF(Ntc.eq.0)THEN
       IF(Adjtc.eq.1)Adjtc=0
       IF(Fintc)Fintc=F
      ELSE IF(Ntc.gt.0)THEN
       IF(Adjtc.eq.0)Adjtc=1
      END IF
      IF(Adjso.eq.1.and.Nso.eq.0)Adjso=0
      IF(Adjso.eq.0.and.nso.gt.0)Adjso=1
      IF(Adjcyc.eq.1.and.ncyc.eq.0)Adjcyc=0
      IF(Adjcyc.eq.0.and.ncyc.gt.0)Adjcyc=1
c-----------------------------------------------------------------------
c     If log or no transformation not generated, then regression factors
c     will not be generated - produce warning message and reset the
c     indicator variables (BCM - December 2006)
c-----------------------------------------------------------------------
      IF(.not.(dpeq(Lam,0D0).or.dpeq(Lam,1D0)))THEN
       IF((Adjtd.eq.1).or.(Adjhol.eq.1).or.(Adjao.eq.1).or.
     &    (Adjls.eq.1).or.(Adjtc.eq.1).or.(Adjusr.eq.1).or.(Adjso.eq.1)
     &    .or.(Adjsea.eq.1).or.(Adjcyc.eq.1).or.
     &    ((.NOT.(Axrghl.or.Axruhl.or.Khol.ge.1)).and.Finhol).or.
     &    Finao.or.Fintc.or.Finusr)THEN
        CALL wWritln('regARIMA preadjustment factors are only '//
     &               'produced for either the log',STDERR,Mt2,T,F)
        CALL writln('         or no transformation.',STDERR,Mt2,F,T)
       END IF
       IF(Adjtd.eq.1)Adjtd=0
       IF(Adjhol.eq.1)Adjhol=0
       IF(Adjao.eq.1)Adjao=0
       IF(Adjls.eq.1)Adjls=0
       IF(Adjtc.eq.1)Adjtc=0
       IF(Adjusr.eq.1)Adjusr=0
       IF(Adjsea.eq.1)Adjsea=0
       IF(Adjso.eq.1)Adjso=0
       IF(Adjcyc.eq.1)Adjcyc=0
       IF((.NOT.(Axrghl.or.Axruhl.or.Khol.ge.1)).and.Finhol)Finhol=F
       IF(Finao)Finao=F
       IF(Finls)Finls=F
       IF(Fintc)Fintc=F
       IF(Finusr)Finusr=F
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
