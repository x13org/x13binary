C     Last change:  SRD  31 Jan 100    7:33 am
      SUBROUTINE rgarma(Lestim,Mxiter,Mxnlit,Lprtit,A,Na,Nefobs,Lauto)
c-----------------------------------------------------------------------
c     rgarma.f, Release 1, Subroutine Version 1.14, Modified 07 Dec 1995.
c-----------------------------------------------------------------------
c     Regarma, reg+arma ARMA parameters estimated in the nonlinear
c routine and the regression parameters calculated by GLS at each
c function evaluation.
c-----------------------------------------------------------------------
c Name  Type Description
c-----------------------------------------------------------------------
c a       d  Output pobs na used vector of residuals
c             (r(t)'s + sqrt(log(detcov))) from the kalman filter
c apa     d  Local scalar for a'a
c estprm  d  Local parima, nestpm used, long vector of parameters
c             estimated in the nonlinear routine.
c info    i  Local integer passing convergence and error warnings form
c             the minpack nonlinear routine, lmdif.
c intflt  l  Local PARAMETER switch to initialize/calculate |G'G|
c iter    i  Local scalar for the number of overall iterations
c ipvt    i  Local work array for the nonlinear routine parima>=nestpm
c             or the number of nonlinear parameters
c convrg  l  Local flag, indicates whether the iterations call abend()
c lerr    l  Local error for X'X not positive definite (.true.).  Used
c             in the cholesky decomposition return
c lestim  l  Input logical to estimate the model
c locest  l  Local flag on whether to estimate the model on this pass
c lprtit  l  Input logical whether to print out the iteration and
c             convergence information
c mxiter  i  Input maximum number of cumulative ARMA iterations
c na      i  Output number of a(t)'s, or the number of residuals used in
c             the nonlinear routine, na=nspobs+nqstar
c nefobs  i  Input number of effective observations and the length of
c             the differenced regression variables and series less the
c             order of the DF*AR operators.
c nestpm  i  Local number of parameters estimated in the non linear
c             routine
c nfev    i  Local number of AR+MA filterings of a column of data or
c             regression effect
c nliter  i  Local number of nonlinear iterations
c nprint  i  Local flag whether lmdif should print out the iteration
c             information (=1) or not (=0)
c nrtXy   i  Local number of rows in tXy
c nrXy    i  Input length of the series, or number of rows in [X:y]
c objfcn  d  Local objective function, either the
c             deviance=^var*D**(1/nefobs).  The obective
c             function is used as the convergence criterion and to
c             calculate the likelihood.
c one     d  PARAMETER scalar for a double precision 1.0d0
c pi      d  Local PARAMETER for pi
c pt5     i  Local PARAMETER for .5d0
c pxa     i  Input PARAMETER for the number of elements in tXy,
c             >=na*ncXy
c pXy     i  Input PARAMETER for the number of elements in Xy
c             >=nefobs*ncXy
c two     d  Local PARAMETER for a double precision 2
c tXy     d  Local work pxa, nrXy by ncXy temporary matrix for the
c             regression variables and data.  Also a work array for the
c             nonlinear routine and pxa >= na*(nestpm+1)+5*nestpm
c zero    d  Local PARAMETER for 0.0d0
c-----------------------------------------------------------------------
c     Variable typing and initialization
c-----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
      LOGICAL LCLOSE
      DOUBLE PRECISION ONE,PI,TWO,ZERO,MONE
      PARAMETER(LCLOSE=T,ONE=1D0,PI=3.14159265358979d0,TWO=2D0,
     &          ZERO=0D0,MONE=-1D0)
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'mdltbl.i'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'hiddn.cmn'
c-----------------------------------------------------------------------
      INCLUDE 'series.cmn'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      INTEGER PA,PXA,PXY
      PARAMETER(PA=PLEN+2*PORDER,PXY=PLEN*(PB+1),PXA=PA*(PB+1))
c-----------------------------------------------------------------------
      INTEGER ipvt
      DOUBLE PRECISION estprm,txy,dvec
      DIMENSION estprm(PARIMA),ipvt(PARIMA),txy(PXA),dvec(1)
c-----------------------------------------------------------------------
      LOGICAL intflt,Lestim,lnxstp,locest,Lprtit,stpitr,Lauto,la,
     &        gudrun
      INTEGER flterr,i,iflag,info,inverr,iter,lstnit,Mxiter,Mxnlit,Na,
     &        Nefobs,neltxy,nprint,nrtxy,tnlitr,nchr1,nchr2,nchr3
c      INTEGER bset
      DOUBLE PRECISION A,apa,devtol,dpmpar,eps,nltolf,objfcn,tnltol
      DIMENSION A(PA)
c-----------------------------------------------------------------------
      CHARACTER str1*(10),str2*(10),str3*(10)
      INTEGER nstr1,nstr2,nstr3
c-----------------------------------------------------------------------
      DOUBLE PRECISION diag(PARIMA),qtf(PARIMA),wa1(PARIMA),wa2(PARIMA),
     &                 wa3(PARIMA),wa4(PA),tmpa(PA)
      EQUIVALENCE(diag,txy),(qtf,txy(PARIMA+1)),(wa1,txy(2*PARIMA+1)),
     &            (wa2,txy(3*PARIMA+1)),(wa3,txy(4*PARIMA+1)),
     &            (wa4,txy(5*PARIMA+1)),(tmpa,txy(5*PARIMA+PA+1))
c-----------------------------------------------------------------------
c     Changed by BCM Feb 1996 to ensure iteration information is printed
c     in multiple runs.
c     ------------------------------------------------------------------
      LOGICAL Frstcl,Scndcl
      COMMON /lgiter / Frstcl,Scndcl
c     ------------------------------------------------------------------
      LOGICAL xyzero,dpeq
      EXTERNAL dpeq,dpmpar,fcnar,stpitr
      INTEGER nelta,i2
c-----------------------------------------------------------------------
      IF(Lprtit)THEN
       Frstcl=T
       Scndcl=F
      END IF
      gudrun=Issap.LT.2.AND.Irev.lt.4
c-----------------------------------------------------------------------
c     Check the work array size
c-----------------------------------------------------------------------
      IF(Nspobs*Ncxy.gt.PXY)THEN
       nchr1=1
       CALL itoc(Nspobs,str1,nchr1)
       nchr2=1
       CALL itoc(Ncxy,str2,nchr2)
       nchr3=1
       CALL itoc(PXY,str3,nchr3)
       CALL eWritln('Work array too small, '//str1(1:(nchr1-1))//' * '//
     &              str2(1:(nchr2-1))//' > '//str3(1:(nchr3-1))//'.',
     &              STDERR,Mt2,T,T)
       IF(Lauto)THEN
        Lauto=F
       ELSE
        CALL abend
       END IF
       RETURN
      END IF
c-----------------------------------------------------------------------
c     Set some dimensions and parameters
c-----------------------------------------------------------------------
      info=1
      Armaer=0
      intflt=T
      locest=Lestim
      Lrgrsd=1D6
c-----------------------------------------------------------------------
      IF(Lprtit)THEN
       nprint=1
      ELSE
       nprint=0
      END IF
c-----------------------------------------------------------------------
c     Copy the model specifications to the common variables and
c difference Xy into Xy
c-----------------------------------------------------------------------
      CALL strtvl()
c      bset=Iregfx
c      IF(bset.ge.3.and.Nb.gt.0)bset=0
      la=.false.
      CALL setmdl(estprm,la)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
      Nefobs=Nspobs-Nintvl
*      IF(Nefobs.le.Nextvl)THEN
      IF(Nefobs.lt.Nextvl)THEN
       nchr1=1
       CALL itoc(Nefobs,str1,nchr1)
       nchr2=1
       CALL itoc(Nextvl,str2,nchr2)
       CALL eWritln('Number of observations after differencing '//
     &              'and/or conditional AR',
     &              STDERR,Mt2,T,F)
       CALL writln('        estimation is '//str1(1:(nchr1-1))//
     &             ', which is less than the minimum series length',
     &             STDERR,Mt2,F,F)
       CALL writln('        required for the model estimated, '//
     &             str2(1:(nchr2-1))//'.',
     &             STDERR,Mt2,F,T)
       IF(Lauto)THEN
        CALL writln('        Check automatic modeling options (reduce'//
     &              ' maxorder for automdl, or',Mt1,Mt2,T,F)
        CALL writln('        check models used with pickmdl) and try'//
     &              ' again.',Mt1,Mt2,F,T)
        Lauto=F
       ELSE
        CALL abend
       END IF
       RETURN
      END IF
c-----------------------------------------------------------------------
      Dnefob=dble(Nefobs)
      neltxy=Nspobs*Ncxy
c-----------------------------------------------------------------------
c    check to see if the objective function is zero
c-----------------------------------------------------------------------
      IF((Mdl(DIFF)-Mdl(DIFF-1)).gt.0)THEN
       nelta=Nspobs
       i2=0
       DO i=Ncxy,neltxy,Ncxy
        i2=i2+1
        txy(i2)=Xy(i)
       END DO
       CALL arflt(nelta,Arimap,Arimal,Opr,Mdl(DIFF-1),Mdl(DIFF)-1,txy,
     &            nelta)
       i2 = 1
       xyzero = dpeq(txy(1),ZERO)
       DO WHILE (i2.lt.nelta.and.xyzero)
        i2=i2+1
        xyzero = dpeq(txy(i2),ZERO)
       END DO
       IF(xyzero)THEN
        Armaer=POBFN0
        IF(Lauto)Lauto=F
        RETURN
       END IF
      END IF
c-----------------------------------------------------------------------
c     Input convergence tolerances are based on the log likelihood
c but the program checks for convergence of the deviance so the
c tolerances are converted
c-----------------------------------------------------------------------
      devtol=2D0/Dnefob*Tol
c-----------------------------------------------------------------------
      IF(Lar.or.Lma)THEN
       Na=Nefobs+Mxmalg
      ELSE
       Na=Nefobs
      END IF
c-----------------------------------------------------------------------
c     Nonlinear tolerances and maximum iterations are set to the overall
c if there is no regression.
c-----------------------------------------------------------------------
      eps=Stepln
c-----------------------------------------------------------------------
      IF(Nb.gt.0)THEN
       tnltol=2D0/Dnefob*Nltol0
       nltolf=2D0/Dnefob*Nltol
       tnlitr=Mxnlit
      ELSE
       tnltol=devtol
       tnlitr=Mxiter
      END IF
c-----------------------------------------------------------------------
c     Nonlinear least squares ARMA estimation.  First, check that
c  the work arrays are big enough
c-----------------------------------------------------------------------
      IF(Nestpm.gt.0)THEN
       Nlwrk=max(Na,Nspobs)*(Nestpm+1)+5*Nestpm
c-----------------------------------------------------------------------
       IF(Nlwrk.gt.PXA)THEN
        nchr1=1
        CALL itoc(Nlwrk,str1,nchr1)
        nchr2=1
        CALL itoc(PXA,str2,nchr2)
        CALL eWritln('Non linear work array too small '//
     &               str1(1:(nchr1-1))//' > '//str2(1:(nchr2-1))//'.',
     &               STDERR,Mt2,T,T)
        IF(Lauto)THEN
         Lauto=F
        ELSE
         CALL abend
        END IF
        RETURN
c-----------------------------------------------------------------------
       ELSE
        Nlwrk=max(Na,Nspobs)
       END IF
      END IF
c-----------------------------------------------------------------------
      iter=0
      Nliter=0
      Nfev=0
c-----------------------------------------------------------------------
c     Iteration loop, first do a GLS to get the regression estimates
c then send the regressor to a nonlinear least squares routine to get a
c new estimate of the ARMA parameters.
c-----------------------------------------------------------------------
c      write(Mtprof,9000)'init',iter, lnxstp, info, convrg, armaer
      DO WHILE (T)
       CALL copy(Xy,neltxy,1,txy)
       CALL armafl(Nspobs,Ncxy,intflt,F,txy,nrtxy,PXA,flterr)
c-----------------------------------------------------------------------
c     If the filter doesn't work check for invertibility and
c stationarity, report the error, and return.
c-----------------------------------------------------------------------
       IF(flterr.gt.0)THEN
        IF(Mdl(MA).gt.Mdl(DIFF))THEN
         CALL chkrt2(T,inverr,Lhiddn)
         IF(Lfatal)RETURN
c     ------------------------------------------------------------------
         IF(inverr.gt.0)THEN
          Armaer=inverr
c     ------------------------------------------------------------------
         ELSE
          Armaer=flterr
          Var=ZERO
         END IF
c-----------------------------------------------------------------------
        ELSE
         Armaer=flterr
         Var=ZERO
        END IF
c        write(Mtprof,*)'  Armaer = ',Armaer
c-----------------------------------------------------------------------
        GO TO 10
       END IF
c-----------------------------------------------------------------------
c     Calculate the regression parameters given the ARMA parameters
c from the last iteration (or initial values if it's the first
c iteration).
c-----------------------------------------------------------------------
       apa=0D0
       IF(Nb.le.0)THEN
        CALL yprmy(txy,nrtxy,apa)
        Chlxpx(1)=sqrt(apa)
c       ELSE IF(bset.gt.0)THEN
c        IF(locest.and.Nestpm.gt.0)THEN
c         bset=0
c        ELSE
c         CALL xprmx(txy,nrtxy,Ncxy,Ncxy,Chlxpx)
c         CALL dppfa(Chlxpx,Ncxy,Sngcol)
c-----------------------------------------------------------------------
c         IF(Sngcol.gt.0)THEN
c          Convrg=F
c          Armaer=PSNGER
c          GO TO 10
c         END IF
c        END IF
c-----------------------------------------------------------------------
       ELSE
        CALL olsreg(txy,nrtxy,Ncxy,Ncxy,B,Chlxpx,PXPX,Sngcol)
        IF(Lfatal)RETURN
        IF(Sngcol.gt.0)THEN
         Convrg=F
         Armaer=PSNGER
         IF(Lauto)Lauto=F
         GO TO 10
        END IF
c-----------------------------------------------------------------------
        Nfev=Nfev+Ncxy+1
       END IF
c-----------------------------------------------------------------------
c     Calculate the objective function and decide on convergence.
c Note that if there is no regression we only need to go through
c lmdif once.
c-----------------------------------------------------------------------
       CALL resid(txy,nrtxy,Ncxy,Ncxy,1,Nb,MONE,B,A)
       IF(Lfatal)RETURN
       CALL yprmy(A,nrtxy,apa)
       objfcn=apa*exp(Lndtcv/Dnefob)
c-----------------------------------------------------------------------
c     Calculate the magnitude of the largest residual needed for the
c constrained estimation in Minpack.
c-----------------------------------------------------------------------
       IF(iter.eq.0)THEN
        CALL maxvec(A,nrtxy,Lrgrsd)
        Lrgrsd=Lrgrsd*exp(Lndtcv/TWO/Dnefob)
       END IF
       lnxstp=locest.and.Nestpm.gt.0.and.
     &        stpitr(Lprier,objfcn,devtol,iter,Nliter,Mxiter,Convrg,
     &               Armaer,Lhiddn)
       iter=iter+1
c       write(Mtprof,9000)'lnxstp,iter,convrg',iter, lnxstp, info, convrg,
c     &               armaer
       IF(Lprtit)THEN
        dvec(1)=objfcn
        IF(Nliter.eq.0)CALL prtitr(dvec,1,estprm,Nestpm,'ARMA',Nliter,
     &                             Nfev)
        IF(.not.Lfatal.and.Nb.gt.0)
     &     CALL prtitr(dvec,1,B,Nb,'IGLS',iter,Nfev)
        IF(Lfatal)RETURN
       END IF
c-----------------------------------------------------------------------
c     Re-estimate the ARMA parameters.  Note that the a's are deviance
c values when returned from lmdif, not ARMA filtered residuals.
c Also, if there are no iterations in the nonlinear routine then
c it has not changed the pamameters, and the mimimization has converged.
c After the first iteration the nonlinear tolerance is changed to the
c IGLS tolerance unless it has been defined by the user.
c-----------------------------------------------------------------------
       IF(lnxstp)THEN
        IF(iter.gt.2)tnltol=nltolf
        lstnit=Nliter
        CALL resid(Xy,Nspobs,Ncxy,Ncxy,1,Nb,MONE,B,Tsrs)
        CALL lmdif(fcnar,Na,Nestpm,estprm,A,Lauto,gudrun,tnltol,ZERO,
     &             ZERO,Nliter+tnlitr,eps,diag,1,100D0,nprint,info,
     &             Nliter,Nfev,Armacm,PA,ipvt,qtf,wa1,wa2,wa3,wa4)
        IF(Lfatal)RETURN
c        write(Mtprof,9000)'after lmdif',iter, lnxstp, info, convrg, armaer
        locest=Nb.gt.0
        IF(info.ge.1.and.info.le.8.and.Nliter.gt.lstnit)GO TO 20
       END IF
c-----------------------------------------------------------------------
c     Print out any errors in the convergence of the MINPACK nonlinear
c estimation routine.  Let the program go on because the user may be
c able to spot and error from the parameter estimates.  Info= 1 to 4
c means the estimation has converged and that will be printed out later.
c-----------------------------------------------------------------------
       IF(Lestim.and.Nestpm.gt.0)THEN
        IF(Nb.eq.0)THEN
         Convrg=Convrg.and.(info.ge.1.and.info.le.4).or.
     &          (info.ge.6.and.info.le.8)
        ELSE
         Convrg=Convrg.and.info.ge.1.and.info.le.8
        END IF
c        write(Mtprof,9000)'convrg=',iter, lnxstp, info, convrg, armaer
c-----------------------------------------------------------------------
        IF(info.lt.0)THEN
         Armaer=PUNKER
         Convrg=F
         Var=ZERO
         GO TO 10
c-----------------------------------------------------------------------
        ELSE IF(info.eq.0)THEN
         Armaer=PINPER
c-----------------------------------------------------------------------
        ELSE IF(info.eq.5.or.(Nliter.ge.Mxiter.and..not.Convrg))THEN
         IF(Nliter.ge.Mxiter)THEN
          Armaer=PMXIER
         ELSE
          Armaer=PMXFER
         END IF
c-----------------------------------------------------------------------
        ELSE IF(info.ge.1.and.info.le.4)THEN
         Armaer=0
c-----------------------------------------------------------------------
        ELSE
         Armaer=info
        END IF
c        write(Mtprof,9000)'armaer=',iter, lnxstp, info, convrg, armaer
       END IF
c-----------------------------------------------------------------------
c     Calculate the maximum likelihood variance and the likelihood
c-----------------------------------------------------------------------
       Var=apa/Dnefob
       IF(Var.lt.TWO*dpmpar(1))Var=ZERO
       IF(dpeq(Var,ZERO))THEN
        Lnlkhd=ZERO
       ELSE
        Lnlkhd=-(Lndtcv+Dnefob*(log(TWO*PI*Var)+ONE))/TWO
       END IF
c-----------------------------------------------------------------------
c     Calculate the covariance of the ARMA parameters from the
c information from the minpack routine only if the model is estimated
c so we know the parameters are at their MLE's
c-----------------------------------------------------------------------
       IF(Lestim.and.Nestpm.gt.0.and.Convrg)THEN
        CALL resid(Xy,Nspobs,Ncxy,Ncxy,1,Nb,MONE,B,Tsrs)
c        CALL fcnar(Na,tmpa,estprm,tmpa,Lauto,info,F)
        CALL fcnar(Na,Nestpm,estprm,tmpa,Lauto,gudrun,info,F)
        IF(Lfatal)RETURN
c        write(Mtprof,9000)'after fcnar',iter, lnxstp, info, convrg, armaer
        iflag=2
        CALL fdjac2(fcnar,Na,Nestpm,estprm,tmpa,Lauto,gudrun,Armacm,PA,
     &              iflag,0d0,wa4,F)
        CALL upespm(estprm)
c-----------------------------------------------------------------------
        IF(iflag.ge.0)THEN
         CALL qrfac(Na,Nestpm,Armacm,PA,T,ipvt,Nestpm,wa1,wa2,wa3)
         DO i=1,Nestpm
          Armacm(i,i)=wa1(i)
c          qtf(i)=wa4(i)
         END DO
c-----------------------------------------------------------------------
         CALL covar(Nestpm,Armacm,PA,ipvt,tnltol,info)
c         write(Mtprof,9000)'after covar',iter, lnxstp, info, convrg, armaer
         Lcalcm=info.eq.0
         IF(.not.Lcalcm)Armaer=PACSER
        ELSE
         Lcalcm=F
        END IF
c        write(Mtprof,9000)'lcalcm=',iter, lnxstp, info, convrg, armaer
       END IF
c-----------------------------------------------------------------------
       IF(Savtab(LESTIT))THEN
        dvec(1)=ZERO
        CALL savitr(LCLOSE,iter,iter,ZERO,dvec,1)
       END IF
c-----------------------------------------------------------------------
   10  RETURN
   20  CONTINUE
      END DO
c 9000 FORMAT(a27,' iter = ',i3,' lnxstp = ',l2,' info = ',i3,
c     &       ' convrg = ',l2,' armaer = ',i3)
      END

