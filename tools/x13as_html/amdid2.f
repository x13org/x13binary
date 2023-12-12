C     Last change:  SRD  31 Jan 100    7:36 am
      SUBROUTINE amdid2(Irar,Irdf,Irma,Isar,Isdf,Isma,Txy,Nelta,Lmu,Lgo)
      IMPLICIT NONE
c     ------------------------------------------------------------------
c       Estimate model for automatic model identification scheme
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.FALSE.)
c     ------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'adj.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'mdltbl.i'
c     ------------------------------------------------------------------
      DOUBLE PRECISION A,Txy,estprm
      LOGICAL inptok,Lgo,Lmu
      INTEGER ardsp,Irar,Irdf,Irma,Isar,Isdf,Isma,info,na,nefobs,Nelta,
     &        fh2
      DIMENSION a(PLEN+2*PORDER),Txy(PLEN),estprm(PARIMA)
c     ------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c     ------------------------------------------------------------------
c       Set up ARIMA model
c     ------------------------------------------------------------------
      CALL mdlint()
      CALL mdlset(Irar,Irdf,Irma,Isar,Isdf,Isma,inptok)
      IF((.not.inptok).or.Lfatal)THEN
       fh2=0
       IF (.not.Lfatal) fh2=Mt2
       CALL eWritln('Unable to set up ARIMA model for automatic '//
     &             'model identification',STDERR,fh2,T,F)
       CALL writln('        procedure for the reason(s) given above.',
     &             STDERR,fh2,F,T)
       IF (.not.Lfatal) CALL abend()
       RETURN
      END IF
      ardsp=Nnsedf+Nseadf
      Nefobs=Nspobs-Nintvl
      Armaer=0
      IF(.not.Lcalcm)Lcalcm=T
c     ------------------------------------------------------------------
c     Get initial Estimate of ARMA parameters, if requested
c-----------------------------------------------------------------------
      info=0
      Lgo=T
      IF(Hrinit)THEN
       CALL amdest(Txy,nelta,Nefobs,ardsp,Lmu,Prttab(LAUMCH),info)
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     If an estimation error is found, discontinue the routine.
c-----------------------------------------------------------------------
       IF(Armaer.eq.PSNGER.or.info.lt.0)THEN
        CALL nWritln('Estimation error found during automatic model '//
     &               'selection procedure',Mt1,Mt2,T,F)
        CALL writln(' while estimating inital values for ARIMA model'//
     &              ' coefficients.',Mt1,Mt2,F,F)
        CALL writln(' Try setting hrinitial=no.',Mt1,Mt2,F,T)
        Lgo=F
        RETURN
c-----------------------------------------------------------------------
c     If only a warning message would be printed out, reset the error
c     indicator variable to zero.
c-----------------------------------------------------------------------
       ELSE IF(Armaer.ne.0)THEN
        Armaer=0
       END IF
c-----------------------------------------------------------------------
c     Temporary printout for HR estimates...  (BCM)
c-----------------------------------------------------------------------
       IF(Prttab(LAUMDL))CALL amdprt(ardsp,Lgo,F)
       CALL setmdl(estprm,Lgo)
      END IF
c     ------------------------------------------------------------------
      IF(Lgo)THEN
       CALL rgarma(Lestim,Mxiter,Mxnlit,F,A,Na,nefobs,Lautom)
c      CALL rgarma(Lestim,Mxiter,Mxnlit,
c     &           (Prttab(LESTIT).or.Savtab(LESTIT)),A,Na,nefobs,Lautom)
       IF(.not.Lautom)CALL abend()
       IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c      Compute BIC for model
c-----------------------------------------------------------------------
       IF(Armaer.eq.0)
     &    CALL prlkhd(Y(Frstsy),Adj(Adj1st),Adjmod,Fcntyp,Lam,F,F,F)
      END IF
c-----------------------------------------------------------------------
c	Temporary printout for HR estimates...  (BCM)
c-----------------------------------------------------------------------
      IF(Lgo)Lgo=Convrg.and.Armaer.eq.0
      IF(Prttab(LAUMDL))CALL amdprt(ardsp,Lgo,Lgo)
c-----------------------------------------------------------------------
      RETURN
      END
