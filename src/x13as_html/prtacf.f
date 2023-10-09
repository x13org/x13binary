C     Last change:  BCM   9 Dec 1998    9:08 am
      SUBROUTINE prtacf(Inspc,Nefobs,A,Na,Mxlag,Lgraf,Ldiag,Ndf,Nsdf)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Calculate the ACF, PACF, and residual histogram if requested
c (Replace histogram with a QQ plot when possible)
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
c      INCLUDE 'mdltbl.i'
      INCLUDE 'acfptr.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'svllog.prm'
      INCLUDE 'svllog.cmn'
      INCLUDE 'mdlsvl.i'
c     ------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.FALSE.,T=.true.)
c     ------------------------------------------------------------------
      LOGICAL Ldiag,Lgraf,lp2,locok
      INTEGER Inspc,Mxlag,Na,Nefobs,Ndf,Nsdf,ipcp,iacp,ipcf,iacf,np,fh2,
     &        ilag,endlag
      DOUBLE PRECISION A,seacf,smpac
      DIMENSION A(PLEN),seacf(PLEN/4),smpac(PLEN/4)
c-----------------------------------------------------------------------
      INTEGER Fhacf,Fhpcf,Fhacfg,Fhpcfg
      COMMON /cfhacf/ Fhacf,Fhpcf,Fhacfg,Fhpcfg
c     ------------------------------------------------------------------
      ipcp=Inspc+LPCP
      ipcf=Inspc+LPCF
      iacp=Inspc+LACP
      iacf=Inspc+LACF
c     ------------------------------------------------------------------
      fh2=0
      IF(.not.Lquiet)fh2=STDERR
      lp2=Prttab(ipcp).or.Prttab(ipcf).or.Savtab(ipcf)
      IF(Var.le.0D0)THEN
       IF(Prttab(iacf).or.Savtab(iacf).or.Prttab(iacp).or.
     &    Svltab(LSLLBQ).or.lp2)THEN
        CALL nWritln('Can''t calculate an ACF for a model with no '//
     &               'variance.',fh2,Mt2,T,T)
       END IF
       RETURN
      END IF
c     ------------------------------------------------------------------
      IF(Prttab(iacf).or.Savtab(iacf).or.Prttab(iacp).or.Svltab(LSLLBQ)
     &   .or.lp2.or.Lgraf.or.Ldiag)THEN
       IF(Prttab(iacf))THEN
        CALL genSkip(iacf)
        CALL acfhdr(Mt1,Ndf,Nsdf,2)
       END IF
       IF(Mxlag.eq.0)THEN
        IF(Sp.eq.1)THEN
         Mxlag=10
        ELSE
         Mxlag=2*Sp
        END IF
        Mxlag=min(Mxlag,Nefobs-1)
       ELSE
c     ------------------------------------------------------------------
        Mxlag=min(Mxlag,Nefobs-1)
       END IF
c     ------------------------------------------------------------------
       np=0
       endlag=Opr(Nopr)-1
       DO ilag=1,endlag
        IF(.not.Arimaf(ilag))np=np+1
       END DO
c     ------------------------------------------------------------------
       CALL acf(A(Na-Nefobs+1),Nefobs,Nefobs,smpac,seacf,Mxlag,np,Sp,
     &          Iqtype,T,Prttab(iacf),F)
       IF(Prttab(iacf))WRITE(Mt1,1030)
 1030  FORMAT(/,'<p> The P-values approximate the probability of ',
     &          'observing a Q-value at least',
     &        /,'  this large when the model fitted is correct.  When ',
     &          'DF is positive, small',
     &        /,'  values of P, customarily those below 0.05, indicate',
     &          ' model inadequacy.</p>',/)
       IF(Savtab(iacf))THEN
        IF(Ndf.eq.NOTSET.and.Nsdf.eq.NOTSET)THEN
         CALL opnfil(T,F,iacf,Fhacf,locok)
         IF(.not.locok)THEN
          CALL abend
          RETURN
         END IF
        END IF
        CALL savacf(Fhacf,iacf,smpac,seacf,Mxlag,Ndf,Nsdf)
        IF(Lfatal)RETURN
        IF(Ndf.eq.NOTSET.and.Nsdf.eq.NOTSET)CALL fclose(Fhacf)
       END IF
       IF(Lgraf)THEN
        IF(Ndf.eq.NOTSET.and.Nsdf.eq.NOTSET)THEN
         CALL opnfil(T,Lgraf,iacf,Fhacfg,locok)
         IF(.not.locok)THEN
          CALL abend
          RETURN
         END IF
        END IF
        CALL savacf(Fhacfg,iacf,smpac,seacf,Mxlag,Ndf,Nsdf)
        IF(Lfatal)RETURN
        IF(Ndf.eq.NOTSET.and.Nsdf.eq.NOTSET)CALL fclose(Fhacfg)
       END IF
c     ------------------------------------------------------------------
       IF(Prttab(iacp))THEN
        CALL genSkip(iacp)
        CALL acfhdr(Mt1,Ndf,Nsdf,4)
        CALL corplt(smpac,seacf,Mxlag,Sp,2)
        IF(Lfatal)RETURN
       END IF
      END IF
c     ------------------------------------------------------------------
      IF(lp2.or.Lgraf)THEN
       IF(Prttab(ipcf))THEN
        CALL genSkip(ipcf)
        CALL acfhdr(Mt1,Ndf,Nsdf,1)
       END IF
       CALL pacf(Nefobs,Sp,smpac,seacf,Mxlag,Prttab(ipcf))
       IF(Savtab(ipcf))THEN
        IF(Ndf.eq.NOTSET.and.Nsdf.eq.NOTSET)THEN
         CALL opnfil(T,F,ipcf,Fhpcf,locok)
         IF(.not.locok)THEN
          CALL abend
          RETURN
         END IF
        END IF
        CALL savacf(Fhpcf,ipcf,smpac,seacf,Mxlag,Ndf,Nsdf)
        IF(Lfatal)RETURN
        IF(Ndf.eq.NOTSET.and.Nsdf.eq.NOTSET)CALL fclose(Fhpcf)
       END IF
       IF(Lgraf)THEN
        IF(Ndf.eq.NOTSET.and.Nsdf.eq.NOTSET)THEN
         CALL opnfil(T,Lgraf,ipcf,Fhpcfg,locok)
         IF(.not.locok)THEN
          CALL abend
          RETURN
         END IF
        END IF
        CALL savacf(Fhpcfg,ipcf,smpac,seacf,Mxlag,Ndf,Nsdf)
        IF(Lfatal)RETURN
        IF(Ndf.eq.NOTSET.and.Nsdf.eq.NOTSET)CALL fclose(Fhpcfg)
       END IF
c     ------------------------------------------------------------------
       IF(Prttab(ipcp))THEN
        IF(Prttab(ipcf))WRITE(Mt1,'()')
        CALL genSkip(ipcp)
        CALL acfhdr(Mt1,Ndf,Nsdf,6)
        CALL corplt(smpac,seacf,Mxlag,Sp,1)
        IF(Lfatal)RETURN
       END IF
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
