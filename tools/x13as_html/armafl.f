C     Last change:      REG  15 Sep 2005
C     Previous change:  BCM   9 Oct 97   10:36 am
      SUBROUTINE armafl(Nr,Nc,Linit,Lckrts,Mata,Na,Nata,Info)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     armafl.f, Release 1, Subroutine Version 1.8, Modified 15 Sep 2005.
c-----------------------------------------------------------------------
c Changes:
c     Modified 15 Sep 2005, by REG, to add PORDER (size of acv vector)
c       to XPAND() argument list (new input variable),
c-----------------------------------------------------------------------
c     Exact ARMA filter
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
c      INCLUDE 'units.cmn'
c     ------------------------------------------------------------------
      LOGICAL F
      DOUBLE PRECISION ONE,ZERO
      INTEGER PMATD
      PARAMETER(ONE=1D0,ZERO=0D0,F=.false.,PMATD=(PLEN+PORDER)*PORDER)
c     ------------------------------------------------------------------
      LOGICAL chkrts,Lckrts,Linit
      INTEGER begopr,endopr,i,ielt,ilag,Info,j,k,lastlg,maxpq,Na,nacv,
     &        Nc,nelta,neltd,neltwp,nextma,nfular,nfulma,Nr,qprow,row,
     &        Nata
      DOUBLE PRECISION acv,ldtvwp,ddot,fular,fulma,Mata,psiwgt,tmp
      DIMENSION acv(0:PORDER),fular(0:PORDER),fulma(0:PORDER),
     &          Mata(Nata),psiwgt(0:PORDER)
      EXTERNAL chkrts,ddot
c-----------------------------------------------------------------------
      SAVE nextma
c-----------------------------------------------------------------------
c     No ARMA model to filter
c-----------------------------------------------------------------------
      Info=0
      IF(Nopr.eq.0)THEN
       Na=Nr
c-----------------------------------------------------------------------
c     Check that the MA coeficients are invertibile, and the AR roots
c are stationary if they are calculated exactly.  If filtering
c conditionally it will never reach the roots check.  If the roots are
c invertible and stationary then initialize G'G, D, and Var(w_p|z)
c-----------------------------------------------------------------------
      ELSE
       IF(Lar)THEN
        begopr=Mdl(AR-1)
       ELSE
        begopr=Mdl(MA-1)
       END IF
       endopr=Mdl(MA)-1
c     ------------------------------------------------------------------
       IF(Lckrts.and.chkrts(begopr,endopr))THEN
        Info=PINVER
        GO TO 10
       END IF
c     ------------------------------------------------------------------
       IF(Linit.and.(Lma.or.Lar))THEN
        nextma=Nr-Mxdflg-Mxarlg+Mxmalg
        CALL intgpg(nextma,Info)
c     ------------------------------------------------------------------
        IF(Info.gt.0)THEN
         Info=PGPGER
         GO TO 10
        END IF
c-----------------------------------------------------------------------
c     Expand the MA operator and put in a vector with all the zero
c coefficients included.  Do this by AR filtering a vector of zero's
c with 1 at the first time point. \theta(B)\Theta(B)*1=fullMA(B).
c-----------------------------------------------------------------------
        IF(Lar)THEN
         fulma(0)=ONE
         nfulma=Mxmalg+1
         CALL mltpos(1,Arimap,Arimal,Opr,Mdl(MA-1),Mdl(MA)-1,nfulma,
     &               fulma)
c     ------------------------------------------------------------------
         CALL copy(fulma,nfulma,1,psiwgt)
         CALL ratpos(nfulma,Arimap,Arimal,Opr,Mdl(AR-1),Mdl(AR)-1,
     &               nfulma,psiwgt)
c     ------------------------------------------------------------------
         maxpq=max(Mxarlg,Mxmalg)
         CALL uconv(fulma,Mxmalg,acv)
*         CALL uconv(fulma,Mxmalg,acv,PORDER)
c-----------------------------------------------------------------------
c     Expand the AR operator and put in a vector with all the zero
c coefficients included.  Do this by AR filtering a vector of zero's
c with 1 at the first time point. \phi(B)\Phi(B)*1=fullAR(B).
c-----------------------------------------------------------------------
         fular(0)=ONE
         nfular=Mxarlg+1
         CALL mltpos(1,Arimap,Arimal,Opr,Mdl(AR-1),Mdl(AR)-1,nfular,
     &               fular)
c matd is used as work space
c         call profiler(2,"euclid enter, armafl")
         CALL euclid(fular,Matd,Matd(Mxarlg+1),maxpq,Mxarlg,Mxmalg,acv,
     &               Info)
c         call profiler(2,"euclid exit")
c         WRITE(Mtprof,*)' Info = ',Info
c     ------------------------------------------------------------------
         IF(Info.gt.0)THEN
          Info=PACFER
          GO TO 10
         END IF
c     ------------------------------------------------------------------
         nacv=Mxarlg
c         CALL ratpos(maxpq,Arimap,Arimal,Opr,Mdl(AR-1),Mdl(AR)-1,nacv,
c     &               acv)
         CALL xpand(fular,Mxarlg,maxpq,nacv,acv,PORDER)
         acv(0)=2D0*acv(0)
        END IF
c-----------------------------------------------------------------------
c     Calculate D
c Note, that only the last min(p,q) columns are nonzero so only need to
c creat and filter those.  GITWF.
c-----------------------------------------------------------------------
        IF(Lar.and.Lma)THEN
         neltd=(Nr-Mxdflg-Mxarlg)*Mxarlg
         CALL setdp(ZERO,neltd,Matd)
c     ------------------------------------------------------------------
         DO row=1,Mxmalg
          qprow=Mxmalg+row
          tmp=ZERO
          DO k=row,Mxmalg
           tmp=tmp+fulma(qprow-k)*psiwgt(Mxmalg-k)
          END DO
c     ------------------------------------------------------------------
          ielt=Mxarlg*row
          DO k=max(1,Mxarlg-row+1),Mxarlg
           Matd(ielt)=tmp
           ielt=ielt-Mxarlg-1
          END DO
         END DO
c-----------------------------------------------------------------------
c     Multiply the lags by the number of columns
c so a matrix can be filtered the same as a vector
c-----------------------------------------------------------------------
         lastlg=Opr(endopr)-1
c     ------------------------------------------------------------------
         DO ilag=1,lastlg
          Arimal(ilag)=Mxarlg*Arimal(ilag)
         END DO
c     ------------------------------------------------------------------
         CALL exctma(Mxarlg,Matd,neltd,PMATD)
c-----------------------------------------------------------------------
c     Put the vector length and the lags back on a row basis
c-----------------------------------------------------------------------
         nextma=neltd/Mxarlg
c     ------------------------------------------------------------------
         DO ilag=1,lastlg
          Arimal(ilag)=Arimal(ilag)/Mxarlg
         END DO
c     ------------------------------------------------------------------
         CALL xprmx(Matd,nextma,Mxarlg,Mxarlg,Chlvwp)
c-----------------------------------------------------------------------
c     Calculate chol(\Sigma_p-D'D)
c-----------------------------------------------------------------------
         ielt=0
         DO j=1,Mxarlg
          DO i=1,j
           ielt=ielt+1
           Chlvwp(ielt)=acv(j-i)-Chlvwp(ielt)
          END DO
         END DO
c-----------------------------------------------------------------------
c     Calculate chol(\Sigma_p-D'D)
c-----------------------------------------------------------------------
        ELSE IF(Lar)THEN
         ielt=0
         DO j=1,Mxarlg
          DO i=1,j
           ielt=ielt+1
           Chlvwp(ielt)=acv(j-i)
          END DO
         END DO
        END IF
c-----------------------------------------------------------------------
c     Calculate the cholesky decomposition and determinate of var(w_p|z)
c-----------------------------------------------------------------------
        IF(Lar)THEN
         CALL dppfa(Chlvwp,Mxarlg,Info)
c     ------------------------------------------------------------------
         IF(Info.gt.0)THEN
          Info=PVWPER
          GO TO 10
c     ------------------------------------------------------------------
         ELSE
          CALL logdet(Chlvwp,Mxarlg,ldtvwp)
          Lndtcv=Lndtcv+ldtvwp
         END IF
        END IF
       ELSE IF(Lma.or.Lar)THEN
        nextma=Nr-Mxdflg-Mxarlg+Mxmalg
       END IF
c-----------------------------------------------------------------------
c     Multiply the series length and the lags by the number of columns
c so a matrix can be filtered the same as a vector
c-----------------------------------------------------------------------
       nelta=Nr*Nc
c     ------------------------------------------------------------------
       endopr=Mdl(MA)-1
       lastlg=Opr(endopr)-1
c     ------------------------------------------------------------------
       DO ilag=1,lastlg
        Arimal(ilag)=Nc*Arimal(ilag)
       END DO
c-----------------------------------------------------------------------
c     Difference the data, then if doing conditional AR AR filter also.
c Note that nelta is reduced by differencing order, d*nc, and
c conditional AR order, p*nc.
c-----------------------------------------------------------------------
       CALL arflt(nelta,Arimap,Arimal,Opr,Mdl(DIFF-1),Mdl(DIFF)-1,Mata,
     &            nelta)
c-----------------------------------------------------------------------
c     Put \vecwp in first then \hat\veca
c-----------------------------------------------------------------------
       IF(Lar)THEN
        neltwp=Mxarlg*Nc
        CALL copy(Mata,nelta,-1,Mata(neltwp+1))
       ELSE
        neltwp=0
       END IF
c-----------------------------------------------------------------------
c      Conditionally filter the remaining data
c-----------------------------------------------------------------------
       CALL arflt(nelta,Arimap,Arimal,Opr,Mdl(AR-1),Mdl(AR)-1,
     &            Mata(neltwp+1),nelta)
c-----------------------------------------------------------------------
c     Exact MA filter the data
c-----------------------------------------------------------------------
       CALL exctma(Nc,Mata(neltwp+1),nelta,Nata-neltwp)
c-----------------------------------------------------------------------
c     Multiply \vecwp-\matD'\hat\veca. Need LAPACK HERE
c Need to check if this underflow is OK here.
c-----------------------------------------------------------------------
       IF(Lar.and.Lma)THEN
c        CALL under0(T)
        ielt=0
        DO i=1,Mxarlg
         DO j=1,Nc
          ielt=ielt+1
          Mata(ielt)=Mata(ielt)
     &               -ddot(nextma,Matd(i),Mxarlg,Mata(neltwp+j),Nc)
         END DO
        END DO
c        CALL under0(F)
       END IF
c-----------------------------------------------------------------------
c     \chol(\condexpwp)\vecx=\vecwp-\matD'\hat\veca. Need LAPACK HERE
c-----------------------------------------------------------------------
       IF(Lar)THEN
        CALL dsolve(Chlvwp,Mxarlg,Nc,F,Mata)
        nelta=nelta+neltwp
       END IF
c-----------------------------------------------------------------------
c     Put the series length and the lags back on a row basis
c-----------------------------------------------------------------------
       Na=nelta/Nc
c     ------------------------------------------------------------------
       DO ilag=1,lastlg
        Arimal(ilag)=Arimal(ilag)/Nc
       END DO
      END IF
c     ------------------------------------------------------------------
   10 RETURN
      END

