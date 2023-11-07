C     Last Change: Mar. 2021- add the Ljung-Box Q and p-value for the
C     sample ACF of the squared residuals for all seasonal lags to the
C     udg file
C     previous change:  BCM  28 Sep 1998   12:07 pm
      SUBROUTINE pracf2(Nefobs,A,Na,Mxlag,Lgraf,ldiag)
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
      INCLUDE 'mdltbl.i'
c      INCLUDE 'acfptr.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      INTEGER PR
      PARAMETER(PR=PLEN/4)
      INCLUDE 'autoq.cmn'
c     ------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.FALSE.,T=.true.)
c     ------------------------------------------------------------------
      LOGICAL Lgraf,ldiag,locok
      INTEGER i,i2,n2,Mxlag,Na,Nefobs,iacp,iacf,fhacf,fhacfg,np,endlag,
     &        ilag,fh0
      DOUBLE PRECISION A,a2,a2mu,seacf,smpac
      DIMENSION A(PLEN),a2(PLEN),seacf(PLEN/4),smpac(PLEN/4)
c-----------------------------------------------------------------------
      fh0=0
      IF(.not.Lquiet)fh0=STDERR
c-----------------------------------------------------------------------
      iacp=LCKAC2+1
      iacf=LCKAC2
      IF(.NOT.(Prttab(iacf).or.Savtab(iacf).or.Prttab(iacp).or.Lgraf))
     &   RETURN
c     ------------------------------------------------------------------
      IF(Var.le.0D0)THEN
       IF(Prttab(iacf).or.Savtab(iacf).or.Prttab(iacp))THEN
        CALL nWritln('Can''t calculate an autocorrelation function '//
     &               'of the squared residuals',fh0,Mt2,T,F)
        CALL writln('      for a model with no variance.',fh0,Mt2,F,T)
       END IF
       RETURN
      END IF
      IF(Nefobs.le.10*Sp)THEN
       IF(Prttab(iacf).or.Savtab(iacf).or.Prttab(iacp))THEN
        CALL nWritln(PRGNAM//' will not compute the autocorrelation '//
     &               'function of the',Mt1,Mt2,T,F)
        CALL writln('      squared residuals for a set of residuals '//
     &              'that is less than ten years long.',Mt1,Mt2,F,T)
       END IF
       RETURN
      END IF
c     ------------------------------------------------------------------
      IF(Prttab(iacf))THEN
       CALL genSkip(iacf)
       CALL acfhdr(Mt1,NOTSET,NOTSET,3)
      END IF
      IF(Mxlag.eq.0)THEN
       IF(Sp.eq.1)THEN
        Mxlag=10
       ELSE
        Mxlag=Sp
       END IF
       Mxlag=min(Mxlag,Nefobs-1)
      ELSE
c     ------------------------------------------------------------------
c       Mxlag=min(Mxlag,Nefobs-1,Sp)
       Mxlag=min(Mxlag,Nefobs-1)
      END IF
c     ------------------------------------------------------------------
c     Create a vector of the squared residuals (with mean removed)
c     ------------------------------------------------------------------
      a2mu=0D0
      DO i=Na-Nefobs+1,Na
       a2(i-Na+Nefobs)=A(i)*A(i)
       a2mu=a2mu+a2(i-Na+Nefobs)
      END DO
      a2mu=a2mu/DBLE(Nefobs)
      DO i=1,Nefobs
       a2(i)=a2(i)-a2mu
      END DO
c     ------------------------------------------------------------------
      np=0
      endlag=Opr(Nopr)-1
      DO ilag=1,endlag
       IF(.not.Arimaf(ilag))np=np+1
      END DO
c     ------------------------------------------------------------------
      CALL acf(a2,Nefobs,Nefobs,smpac,seacf,Mxlag,np,Sp,Iqtype,T,
     &         Prttab(iacf),T)
      IF(Prttab(iacf))WRITE(Mt1,1010)PRGNAM
 1010 FORMAT(/,'<p>The P-values approximate the probability of ',
     &         'observing a Q-value at least',
     &       /,'  this large when the model fitted is correct in a ',
     &         'way that supports the',
     &       /,'  standard interpretations of the test statistics, ',
     &         'standard errors, and',
     &       /,'  prediction intervals output by ',a,'. When ',
     &         '<abbr title="degrees of freedom">DF</abbr> is ',
     &       /,'  positive, small values of P, customarily those ',
     &         'below 0.05, suggest that model-based',
     &       /,'  inferences about statistical significance and ',
     &         'uncertainty will be less',
     &       /,'  dependable than usual.</p>',/)
      IF(Savtab(iacf).or.Lgraf)THEN
       locok=.true.
       IF(Savtab(iacf))CALL opnfil(T,F,iacf,fhacf,locok)
       IF(locok.and.Lgraf)CALL opnfil(T,Lgraf,iacf,fhacfg,locok)
       IF(.not.locok)THEN
        CALL abend
        RETURN
       END IF
       IF(Savtab(iacf))
     &    CALL savacf(fhacf,iacf,smpac,seacf,Mxlag,NOTSET,NOTSET)
       IF((.not.Lfatal).and.Lgraf)
     &    CALL savacf(fhacfg,iacf,smpac,seacf,Mxlag,NOTSET,NOTSET)
       IF(Lfatal)RETURN
       IF(Savtab(iacf))CALL fclose(fhacf)
       IF(Lgraf)CALL fclose(fhacfg)
      END IF
c     ------------------------------------------------------------------
      IF(Prttab(iacp))THEN
       CALL genSkip(iacp)
       CALL acfhdr(Mt1,NOTSET,NOTSET,5)
       CALL corplt(smpac,seacf,Mxlag,Sp,3)
       IF(Lfatal)RETURN
      END IF
c     ------------------------------------------------------------------
      IF(ldiag)THEN
       n2 = 2
       IF(n2*Sp.gt.Mxlag)n2=1
       DO i=1,n2
        i2=i*Sp
        WRITE(Nform,1020)i2,Qs(i2),Dgf(i2),Qpv(i2)
       END DO
      END IF
c     ------------------------------------------------------------------
 1020 FORMAT('acf2q$',i2.2,': ',f7.3,5x,i3,5x,f6.3)
c     ------------------------------------------------------------------
      RETURN
      END
