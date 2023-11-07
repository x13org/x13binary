C     Last change:      REG  27 Apr 2006
C     Previous change:  REG  04 Apr 2006, 28 Feb 2006, 31 Aug 2005, 15 Sep 2005
C     Previous change:  BCM  19 May 2003    8:51 am
C  THIS SUBROUTINE COMPUTES THE AUTOCORRELATION FUNCTION OF THE COMPONENT
C  ESTIMATOR AND ESTIMATE (STATIONARY TRANSFORMATION) AND THE
C  WIENER-KOLMOGOROV FILTER
C
C      INPUT PARAMETERS
C        TREND : TREND COMPONENT
C       TRENDS : NOSTATIONARY TREND ESTIMATOR
C           SA : SEASONALLY ADJUSTED SERIES
C           SC : SEASONAL COMPONENT
C          SCS : NOSTATIONARY SEASONAL ESTIMATOR
C        CYCLE : CYCLE COMPONENT
C       CYCLES : NOSTATIONARY CYCLE ESTIMATOR
C           IR : IRREGULAR COMPONENT
C        WVARA : ****** NOT USED *******
C       WVARNP : INNOVATIONS VARIANCE OF TREND
C       WVARNS : INNOVATIONS VARIANCE OF SEASONAL
C       WVARNA : INNOVATIONS VARIANCE OF SEASONALLY ADJUSTED
C       WVARNC : INNOVATIONS VARIANCE OF CYCLE
C          QT1 : INNOVATIONS VARIANCE OF IRREGUALAR
C           PG : 0 FILES FOR GRAPH, 1 NO FILES
C          OUT : TO CONTROL THE PRINTOUT
C           MQ : FREQUENCY
C        TITLE : NAME OF THE SERIES
C      NOSERIE : 1 THEORETICAL ACFs. SET TO ZERO THE ONE FOR ESTIMATE.
C          SQF : STANDARD ERROR OF THE RESIDUALS
C         ITER : iter NMLSTS param 
C
C
      subroutine AUTOCOMP(oz,z,trend,trends,sa,sc,scs,cycle,cycles,ir,
     $                    wvara,varwnp,varwns,varwna,varwnc,phi,nphi,
     $                    theta,nth,psieps,psiess,psiecs,psiue,nfl,qt1,
     $                    pg,out,mq,title,noserie,sqf,ncycth,lamd,psiep,
     $                    psies,psiec,psieas,lf,iter,IsCloseToTD)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
      include 'units.cmn'
      integer mc
      parameter (mc = 1000)
      real*8 t_ACF,ZERO
      parameter (t_ACF=2.58d0,ZERO=0.0d0)
C
C.. Formal Arguments ..
      integer nphi,nth,nfl,pg,out,mq,noserie,ncycth,lamd,lf,iter
      character title*80
      real*8 oz(mpkp),z(mpkp),trend(mpkp),trends(mpkp),sa(mpkp),
     $       sc(mpkp),scs(mpkp),cycle(mpkp),cycles(mpkp),
     $       ir(mpkp),wvara,varwnp,varwns,varwna,varwnc,phi(*),
     $       theta(*),psieps(*),psiess(*),psiecs(*),psiue(*),qt1,sqf,
     $       psiep(*),psies(*),psiec(*),psieas(*),dvec(1)
      logical IsCloseToTD
C
C.. Local Scalars ..
      integer i,j,k,mq2,mqo,n,ndum,ndum1,ndum2,ndum3,ntd,nus,nvn
      integer mserror,nztr,nzs,nzsa,nstar
      character fname*30,subtitle*50, auxformat*80, ColsWk*4
      real*8 tmean,varpas,vz,spurMarg
      data spurMarg /1.0D-4/
c      integer nzlen
C
C.. Local Arrays ..
      real*8 dum(80),dum1(80),dum2(mpkp),dum3(80),imz(1000),
     $       rez(0:1000),sas(mpkp),us(50),vn(64),wkcyc(mpkp),
     $       wkir(mpkp),wks(mpkp),wksa(mpkp),wktrend(mpkp)
C
C.. External Calls ..
      external BFAC, CONV, USRENTRY
C
C.. Intrinsic Functions ..
      intrinsic DBLE
      include 'acfst.i'
C  Added by REG on 31 Aug 2005 for include file.
      include 'acfast.i'
      include 'estb.i'
      include 'hspect.i'
      include 'models.i'
      include 'sform.i'
      include 'stream.i'
      include 'bartlett.i'
*      include 'indhtml.i'
      include 'transcad.i'
      include 'htmlout.cmn'
C
C ... Executable Statements ...
C
      mq2 = 2 * mq
      mserror = 1000
      if (mq2 .gt. 24) then
       mq2 = 24
      end if
*      write(Mtprof,*)'  sqf, wvara = ', sqf, wvara
C
C
C COMPUTE THE ACF OF COMPONENTS,ESTIMATORS,ESTIMAT (STATION. TRANSF.)
C
C DUM() AND VN() US() ARE USED AS DUMMY TO COMPUTE THE ARRAYS TO BE PASSED
C TO BFAC. IMZ AND REZ ARE USED FOR GAM AND G (NOT NEEDED).
C
C
C      ***TREND***
C
C
      ntd = 60
      colsWk='1111'
      if (out .eq. 0) then
       CALL genSkip(1003)
       CALL writTagOneLine(Nio,'h3','@',
     $                     'WIENER-KOLMOGOROV FILTERS (ONE SIDE)')
      end if
      if (Nchi .eq. 1) then
       do i = 0,mq2
        Acfpth(i) = ZERO
        Acfper(i) = ZERO
        Acfpem(i) = ZERO
       end do
       do i = 1, mp
        wktrend(i) = ZERO
       end do
       ColsWk(1:1)='0'
      else
C
C  ACF OF THEORETICAL COMPONENT
C
       do i = 1,Nchis-1
        dum(i) = -Chis(i+1)
       end do
       do i = 1,Nthetp-1
        vn(i) = -Thetp(i+1)
       end do
       ndum = Nchis - 1
       if (ndum.lt.0) ndum = 0
       nvn = Nthetp - 1
c      WRITE(Mtprof,*)'  subroutine AUTOCOMP, call 1, ndum = ', ndum
       call BFAC(dum,vn,ndum,nvn,mq2,rez,Acfpth,vz,varwnp,imz,mq2)
       Acfpth(0) = vz
C
C  ACF OF ESTIMATOR
C
       call CONV(Chis,Nchis,Thstr0,Qstar0,dum,ndum)
       do i = 1,ndum-1
        dum(i) = -dum(i+1)
       end do
       call CONV(Thetp,Nthetp,Thetp,Nthetp,vn,nvn)
       call CONV(Psi,Npsi,Cycs,Ncycs,us,nus)
       call CONV(us,nus,vn,nvn,vn,nvn)
C
C**********************************************************
C
       call CONV(us,nus,Thetp,Nthetp,dum2,ndum2)
       do i = 1,Qstar0-1
        dum3(i) = -Thstr0(i+1)
       end do
       do i = 1,ndum2-1
        dum1(i) = -dum2(i+1)
       end do
       ndum3 = Qstar0 - 1
       ndum1 = ndum2 - 1
c      WRITE(Mtprof,*)'  subroutine AUTOCOMP, call 2, ndum3 = ',ndum3
       call BFAC(dum3,dum1,ndum3,ndum1,ntd,wktrend,rez,vz,varwnp,imz,mq2
     $          )
       if (out .eq. 0) then
        CALL prtwkf(Nio,wktrend,ntd,'TREND-CYCLE COMPONENT           ',
     $              21,'wk.trendcycle           ',13)
*        if ((pg .eq. 0).and.(iter.eq.0)) then
*         fname = 'FILTT.T4'
*         subtitle = 'TREND-CYCLE FILTER (T.D.)'
*         call PLOTFLT(fname,subtitle,wktrend,ntd,4,10)
*        end if
       end if
C   LINES OF CODE ADDED FOR X-13A-S : 3
c Usrentry routines added by BCM to facilitate saving
c models of the components  July 2000, revised May 2001
       CALL USRENTRY(wktrend,1,NTD,1,mpkp,2014)
C   END OF CODE BLOCK
*       call CONV(us,nus,vn,nvn,vn,nvn)
       do i = 1,nvn-1
        vn(i) = -vn(i+1)
       end do
       ndum = ndum - 1
       nvn = nvn - 1
       varpas = varwnp**2
c      WRITE(Mtprof,*)'  subroutine AUTOCOMP, call 3, ndum = ',ndum
       call BFAC(dum,vn,ndum,nvn,mserror,rez,Acfper,vz,varpas,imz,
     &           mserror)
       Acfper(0) = vz
C
C   ACF OF ESTIMATE
C
       if (noserie .eq. 0) then
        tmean = ZERO
        do i = 1,Nz-Nchins+1
         trends(i) = ZERO
         do j = 1,Nchins
*          write(Mtprof,*)' trends(',i,') = ',trends(i),' Chins(',
*     &               j,') = ',Chins(j), ' trend(',i+Nchins-j,') = ',
*     &               trend(i+Nchins-j)
          trends(i) = trends(i) + Chins(j)*trend(i+Nchins-j)
*          write(Mtprof,*)'  trends(',i,') = ',trends(i)
         end do
         tmean = tmean + trends(i)
        end do
        n = Nz - Nchins + 1
        tmean = tmean / DBLE(n)
*        write(Mtprof,*)'  tmean = ', tmean
        do k = 0,mq2
         Acfpem(k) = ZERO
         do i = k+1,n
          Acfpem(k) = Acfpem(k) + (trends(i)-tmean)*(trends(i-k)-tmean)
         end do
         Acfpem(k) = Acfpem(k) / DBLE(n)
        end do
        do i = 1,mq2
         Acfpem(i) = Acfpem(i) / Acfpem(0)
        end do
        Acfpem(0) = Acfpem(0) / ((sqf**2)*wvara)
       end if
      end if
C
C
C
C   ***SEASONALLY ADJUSTED***
C
C
      if ((ncycth.eq.0) .and. (Nchcyc.eq.1)) then
       do i = 0,mq2
        Acfath(i) = ZERO
        Acfaer(i) = ZERO
        Acfaem(i) = ZERO
       end do
       do i = 1, mp
        wksa(i) = ZERO
       end do
       ColsWk(2:2)='0'
      else
C
C  ACF OF THEORETICAL COMPONENT
C
       do i = 1,Nadjs-1
        dum(i) = -Adjs(i+1)
       end do
       do i = 1,Nthadj-1
        vn(i) = -Thadj(i+1)
       end do
       ndum = Nadjs - 1
       nvn = Nthadj - 1
c      WRITE(Mtprof,*)'  subroutine AUTOCOMP, call 4, ndum = ',ndum
       call BFAC(dum,vn,ndum,nvn,mq2,rez,Acfath,vz,varwna,imz,mq2)
       Acfath(0) = vz
C
C  ACF OF ESTIMATOR
C
       call CONV(Adjs,Nadjs,Thstr0,Qstar0,dum,ndum)
       do i = 1,ndum-1
        dum(i) = -dum(i+1)
       end do
       call CONV(Thadj,Nthadj,Thadj,Nthadj,vn,nvn)
       if (isCloseToTD) then
         call CONV(cyc,Ncyc,vn,nvn,vn,nvn)
         call CONV(Psi,Npsi,vn,nvn,vn,nvn)
       else
         call CONV(Psi,Npsi,vn,nvn,vn,nvn)
       end if
C
C**********************************************************
       call CONV(Thadj,Nthadj,Psi,Npsi,dum2,ndum2)
       do i = 1,Qstar0-1
        dum3(i) = -Thstr0(i+1)
       end do
       do i = 1,ndum2-1
        dum1(i) = -dum2(i+1)
       end do
       ndum3 = Qstar0 - 1
       ndum1 = ndum2 - 1
c      WRITE(Mtprof,*)'  subroutine AUTOCOMP, call 5, ndum3 = ',ndum3
       call BFAC(dum3,dum1,ndum3,ndum1,ntd,wksa,rez,vz,varwna,imz,mq2)
       if (out .eq. 0) then
        CALL prtwkf(Nio,wksa,ntd,'SEASONALLY ADJUSTED SERIES      ',26,
     &              'wk.seadj                ',8)
*        if ((pg .eq. 0).and.(iter.eq.0)) then
*         fname = 'FILTADJ.T4'
*         subtitle = 'SA SERIES FILTER (T.D.)'
*         call PLOTFLT(fname,subtitle,wksa,ntd,4,10)
*        end if
       end if
C   LINES OF CODE ADDED FOR X-13A-S : 3       
c Usrentry routines added by BCM to facilitate saving
c models of the components  July 2000, revised May 2001
       CALL USRENTRY(wksa,1,NTD,1,mpkp,2015)
C   END OF CODE BLOCK
       do i = 1,nvn-1
        vn(i) = -vn(i+1)
       end do
       ndum = ndum - 1
       nvn = nvn - 1
       varpas = varwna**2
c      WRITE(Mtprof,*)'  subroutine AUTOCOMP, call 6, ndum = ',ndum
       call BFAC(dum,vn,ndum,nvn,mserror,rez,Acfaer,vz,varpas,imz,
     &           mserror)
       Acfaer(0) = vz
C
C  ACF OF ESTIMATE
C
       if (noserie .eq. 0) then
        tmean = ZERO
        do i = 1,Nz-Nadjns+1
         sas(i) = ZERO
         do j = 1,Nadjns
          sas(i) = sas(i) + Adjns(j)*sa(i+Nadjns-j)
         end do
         tmean = tmean + sas(i)
        end do
        n = Nz - Nadjns + 1
        tmean = tmean / DBLE(n)
        do k = 0,mq2
         Acfaem(k) = ZERO
         do i = k+1,n
          Acfaem(k) = Acfaem(k) + (sas(i)-tmean)*(sas(i-k)-tmean)
         end do
         Acfaem(k) = Acfaem(k) / DBLE(n)
        end do
        do i = 1,mq2
         Acfaem(i) = Acfaem(i) / Acfaem(0)
        end do
        Acfaem(0) = Acfaem(0) / ((sqf**2)*wvara)
       end if
      end if
C
C
C      ***SEASONAL***
C
C
      if (Npsi .eq. 1) then
       do i = 0,mq2
        Acfsth(i) = ZERO
        Acfser(i) = ZERO
        Acfsem(i) = ZERO
       end do
       do i = 1,mp
        wks(i) = ZERO
       end do
       ColsWk(3:3)='0'
      else
C
C  ACF OF THEORETICAL COMPONENT
C
       do i = 1,Npsis-1
        dum(i) = -Psis(i+1)
       end do
       do i = 1,Nthets-1
        vn(i) = -Thets(i+1)
       end do
       ndum = Npsis - 1
       if (ndum.lt.0) ndum=0
       nvn = Nthets - 1
c      WRITE(Mtprof,*)'  subroutine AUTOCOMP, call 7, ndum = ', ndum
       call BFAC(dum,vn,ndum,nvn,mq2,rez,Acfsth,vz,varwns,imz,mq2)
       Acfsth(0) = vz
C
C  ACF OF ESTIMATOR
C
       call CONV(Psis,Npsis,Thstr0,Qstar0,dum,ndum)
       do i = 1,ndum-1
        dum(i) = -dum(i+1)
       end do
       call CONV(Thets,Nthets,Thets,Nthets,vn,nvn)
       call CONV(Chi,Nchi,vn,nvn,vn,nvn)
       call CONV(Cycs,Ncycs,vn,nvn,vn,nvn)
C
C**********************************************************
       call CONV(Thets,Nthets,Chi,Nchi,dum1,ndum1)
       call CONV(dum1,ndum1,Cyc,Ncyc,dum2,ndum2)
       do i = 1,Qstar0-1
        dum3(i) = -Thstr0(i+1)
       end do
       do i = 1,ndum2-1
        dum1(i) = -dum2(i+1)
       end do
       ndum3 = Qstar0 - 1
       ndum1 = ndum2 - 1
c      WRITE(Mtprof,*)'  subroutine AUTOCOMP, call 8, ndum3 = ',ndum3
       call BFAC(dum3,dum1,ndum3,ndum1,ntd,wks,rez,vz,varwns,imz,mq2)
       if (out .eq. 0) then
        CALL prtwkf(Nio,wks,ntd,'SEASONAL COMPONENT              ',18,
     &              'wk.seasonal             ',11)
*        if ((pg .eq. 0).and.(iter.eq.0)) then
*         fname = 'FILTS.T4'
*         subtitle = 'SEASONAL COMP. FILTER (T.D.)'
*         call PLOTFLT(fname,subtitle,wks,ntd,4,10)
*        end if
       end if
C   LINES OF CODE ADDED FOR X-13A-S : 3
c Usrentry routines added by BCM to facilitate saving
c models of the components  July 2000, revised May 2001
       CALL USRENTRY(wks,1,NTD,1,mpkp,2016)
C   END OF CODE BLOCK
       do i = 1,nvn-1
        vn(i) = -vn(i+1)
       end do
       ndum = ndum - 1
       nvn = nvn - 1
       varpas = varwns**2
c      WRITE(Mtprof,*)'  subroutine AUTOCOMP, call 9, ndum = ',ndum
       call BFAC(dum,vn,ndum,nvn,mserror,rez,Acfser,vz,varpas,imz,
     &           mserror)
       Acfser(0) = vz
C
C  ACF OF ESTIMATE
C
       if (noserie .eq. 0) then
        tmean = ZERO
        do i = 1,Nz-Npsins+1
         scs(i) = ZERO
         do j = 1,Npsins
          scs(i) = scs(i) + Psins(j)*sc(i+Npsins-j)
         end do
         tmean = tmean + scs(i)
        end do
        n = Nz - Npsins + 1
        tmean = tmean / DBLE(n)
        do k = 0,mq2
         Acfsem(k) = ZERO
         do i = k+1,n
          Acfsem(k) = Acfsem(k) + (scs(i)-tmean)*(scs(i-k)-tmean)
         end do
         Acfsem(k) = Acfsem(k) / DBLE(n)
        end do
        do i = 1,mq2
         Acfsem(i) = Acfsem(i) / Acfsem(0)
        end do
        Acfsem(0) = Acfsem(0) / ((sqf**2)*wvara)
       end if
      end if
C
C
C      ***CYCLE***
C
C
      if (varwnc.lt.1.0d-10 .or.((ncycth.eq.0) .and. (Ncyc.eq.1))) then
       do i = 0,mq2
        Acfcth(i) = ZERO
        Acfcer(i) = ZERO
        Acfcem(i) = ZERO
       end do
       do i = 1,mp
        wkcyc(i) = ZERO
       end do
       ColsWk(4:4)='0'
      else
C
C  ACF OF THEORETICAL COMPONENT
C
       do i = 1,Ncycs-1
        dum(i) = -Cycs(i+1)
       end do
       ndum = Ncycs - 1
       do i = 1,Nthetc-1
        vn(i) = -Thetc(i+1)
       end do
       nvn = Nthetc - 1
c       WRITE(Mtprof,*)'  subroutine AUTOCOMP, call 10, ndum = ',ndum
       call BFAC(dum,vn,ndum,nvn,mq2,rez,Acfcth,vz,varwnc,imz,mq2)
       Acfcth(0) = vz
C
C  ACF OF ESTIMATOR
C
       call CONV(Cycs,Ncycs,Thstr0,Qstar0,dum,ndum)
       do i = 1,ndum-1
        dum(i) = -dum(i+1)
       end do
       call CONV(Thetc,Nthetc,Thetc,Nthetc,vn,nvn)
       call CONV(Psi,Npsi,Chi,Nchi,us,nus)
       call CONV(us,nus,vn,nvn,vn,nvn)
       call CONV(Cycns,Ncycns,vn,nvn,vn,nvn)
C
C**********************************************************
       call CONV(Chi,Nchi,Psi,Npsi,dum1,ndum1)
       call CONV(dum1,ndum1,Thetc,Nthetc,dum2,ndum2)
       do i = 1,Qstar0-1
        dum3(i) = -Thstr0(i+1)
       end do
       do i = 1,ndum2-1
        dum1(i) = -dum2(i+1)
       end do
       ndum3 = Qstar0 - 1
       ndum1 = ndum2 - 1
c      WRITE(Mtprof,*)'  subroutine AUTOCOMP, call 11, ndum3 = ',ndum3
       call BFAC(dum3,dum1,ndum3,ndum1,ntd,wkcyc,rez,vz,varwnc,imz,mq2)
       if (out .eq. 0) then
        If (IsCloseToTD) then
         CALL prtwkf(Nio,wkcyc,ntd,
     &               'TRADING DAY STOCHASTIC COMPONENT',32,
     &               'wk.tradingday.stochastic',24)
        else
         CALL prtwkf(Nio,wkcyc,ntd,
     &               'TRANSITORY COMPONENT            ',20,
     &               'wk.transitory           ',13)
        end if
       end if
C   LINES OF CODE ADDED FOR X-13A-S : 1
c Usrentry routines added by BCM to facilitate saving
c models of the components  July 2000, revised May 2001
      CALL USRENTRY(wkcyc,1,NTD,1,mpkp,2017)
C   END OF CODE BLOCK
       do i = 1,nvn-1
        vn(i) = -vn(i+1)
       end do
       ndum = ndum - 1
       nvn = nvn - 1
       varpas = varwnc**2
c      WRITE(Mtprof,*)'  subroutine AUTOCOMP, call 12, ndum=',ndum
       call BFAC(dum,vn,ndum,nvn,mserror,rez,Acfcer,vz,varpas,imz,
     &           mserror)
       Acfcer(0) = vz
C
C  ACF OF ESTIMATE
C
       if (noserie .eq. 0) then
        tmean = ZERO
        do i = 1,Nz-Ncycns+1
         cycles(i) = ZERO
         do j = 1,Ncycns
          cycles(i) = cycles(i) + Cycns(j)*cycle(i+Ncycns-j)
         end do
         tmean = tmean + cycles(i)
        end do
        n = Nz - Ncycns + 1
        tmean = tmean / DBLE(n)
        do k = 0,mq2
         Acfcem(k) = ZERO
         do i = k+1,n
          Acfcem(k) = Acfcem(k) + (cycles(i)-tmean)*(cycles(i-k)-tmean)
         end do
         Acfcem(k) = Acfcem(k) / DBLE(n)
        end do
        do i = 1,mq2
         Acfcem(i) = Acfcem(i) / Acfcem(0)
        end do
        Acfcem(0) = Acfcem(0) / ((sqf**2)*wvara)
       end if
      end if
c  RREGULAR
C     IF (NOADMISS.EQ.2) GOTO 96
      do i = 1,mq2
       Acfith(i) = ZERO
      end do
      Acfith(0) = qt1
      if (qt1.ne.ZERO) then
C
C  ACF OF ESTIMATOR
C
       do i = 1,Qstar0-1
        dum(i) = -Thstr0(i+1)
       end do
       do i = 1,Ntotd-1
        vn(i) = -Totden(i+1)
       end do
       ndum = Qstar0 - 1
       nvn = Ntotd - 1
C
C**********************************************************
c      WRITE(Mtprof,*)'  subroutine AUTOCOMP, call 13, ndum =',ndum
       call BFAC(dum,vn,ndum,nvn,ntd,wkir,rez,vz,qt1,imz,mq2)
       if (out .eq.0) then
        CALL prtwkf(Nio,wkir,ntd,
     &              'IRREGULAR COMPONENT                   ',19,
     &              'wk.irregular            ',12)
*       if ((pg .eq. 0).and.(iter.eq.0)) then
*        fname = 'FILTI.T4'
*        subtitle = 'IRREGULAR COMP. FILTER (T.D.)'
*        call PLOTFLT(fname,subtitle,wkir,ntd,4,10)
*       end if
       end if
C   LINES OF CODE ADDED FOR X-13A-S : 3      
c Usrentry routines added by BCM to facilitate saving
c models of the components  July 2000, revised May 2001
       CALL USRENTRY(wkir,1,NTD,1,mpkp,2018)
C   END OF CODE BLOCK
       varpas = qt1**2
c      WRITE(Mtprof,*)'  subroutine AUTOCOMP, call 14, ndum = ',ndum
       call BFAC(dum,vn,ndum,nvn,mserror,rez,Acfier,vz,varpas,imz,
     &           mserror)
       Acfier(0) = vz
C
C  ACF OF ESTIMATE
C
       if (noserie .eq. 0) then
        tmean = ZERO
        do i = 1,Nz
         tmean = tmean + ir(i)
        end do
        n = Nz
        tmean = tmean / DBLE(n)
        do k = 0,mq2
         Acfiem(k) = ZERO
         do i = k+1,n
          Acfiem(k) = Acfiem(k) + (ir(i)-tmean)*(ir(i-k)-tmean)
         end do
         Acfiem(k) = Acfiem(k) / DBLE(n)
        end do
        do i = 1,mq2
         Acfiem(i) = Acfiem(i) / Acfiem(0)
        end do
        Acfiem(0) = Acfiem(0) / ((sqf**2)*wvara)
       end if
      else
       do i = 1,mq2
        Acfiem(i) =ZERO
        acfith(i)=ZERO
        acfier(i)=ZERO
       enddo
       do i=1,mp+kp 
        wkir(i)=ZERO
       enddo
      end if
      if (out.eq.0) then
       call plotWkFilters(wktrend,wksa,wks,wkcyc,wkir,ntd,nio)
      end if
C
C
C HERE INTRODUCE THE NEW CONTRIBUTION TABLE
C
      if (out .eq. 0) then
*        iTab=iTab+1
*        iId=iId+1
       CALL genSkip(1190)
       CALL writTagOneLine(Nio,'h3','@',
     $                     'CONTRIBUTION OF ORIGINAL SERIES AND '//
     $                     'OF ITS INNOVATIONS TO THE ESTIMATOR '//
     $                     'OF THE COMPONENTS FOR THE PRESENT PERIOD.')
       CALL mkTableTag(Nio,'w80',
     $                     'CONTRIBUTION OF ORIGINAL SERIES AND '//
     $                     'OF ITS INNOVATIONS TO THE ESTIMATOR '//
     $                     'OF THE COMPONENTS FOR THE PRESENT PERIOD.')
       CALL makColgroup(Nio,0)
       CALL makColgroup(Nio,2)
       CALL makColgroup(Nio,2)
       CALL makColgroup(Nio,2)
       CALL writTag(Nio,'<thead>')
       CALL writTag(Nio,'<tr>')
       CALL mkTableCellSpan(Nio,'row',2,'head',
     $                      'COMPONENT'//Cbr//'CONTRIBUTION OF')
       CALL mkHeaderCellScope(Nio,0,2,'colgroup','@','TREND-CYCLE')
       CALL mkHeaderCellScope(Nio,0,2,'colgroup','@','SEASONAL')
       CALL mkHeaderCellScope(Nio,0,2,'colgroup',
     $                        'Transitory plus Irregular','TRANS+IRREG')
       CALL writTag(Nio,'</tr>')
       CALL writTag(Nio,'<tr>')
       WRITE(Nio,1010)
 1010  FORMAT(3('<th scope="col">OBSERVATION</th>',
     &          '<th scope="col">INNOVATION</th>'))
       CALL writTag(Nio,'</tr>')
       CALL writTag(Nio,'</thead>')
       CALL writTag(Nio,'<tbody>')
 1020  FORMAT('<tr><th scope="row">',a,'</th>',
     $        3('<td class="center">',F9.3,'</td>',
     $          '<td class="center">',F9.3,'</td>'),'</tr>')
       write (Nio,1020)'LAST PERIOD',
     $        wktrend(1), psiep(lf+1), wks(1), psies(lf+1),
     $        wkcyc(1)+wkir(1), psiue(lf+1)+psiec(lf+1)
       write (Nio,1020)'NEXT PERIOD',
     $        wktrend(2), psiep(lf), wks(2), psies(lf), 
     $        wkcyc(2)+wkir(2), psiue(lf)+psiec(lf)
       write (Nio,1020)'1 YEAR AHEAD',
     $        wktrend(mq+1), psiep(lf+1-mq), wks(mq+1), 
     $        psies(lf+1-mq), wkcyc(mq+1)+wkir(mq+1),
     $        psiue(lf+1-mq)+psiec(lf+1-mq)
       write (Nio,1020)'2 YEAR AHEAD',
     $        wktrend(2*mq+1), psiep(lf+1-2*mq), wks(2*mq+1),
     $        psies(lf+1-2*mq), wkcyc(2*mq+1)+wkir(2*mq+1),
     $        psiue(lf+1-2*mq)+psiec(lf+1-2*mq)
       CALL writTag(Nio,'</tbody>')
       CALL writTag(Nio,'</table>')
       CALL mkPOneLine(Nio,'@','&nbsp;')
       CALL mkPOneLine(Nio,'@','Check :')
       CALL writTagClass(Nio,'ul','indent')
       CALL writTagOneLine(Nio,'li','@',
     $                     'The sum of the 3 weights associated '//
     $                     'with the observation, for the last '//
     $                     'period, should be 1.0.')
       CALL writTagOneLine(Nio,'li','@',
     $                     'The same should happen with the 3 '//
     $                     'weights associated with the innovations '//
     $                     'for the last period.')
       CALL writTagOneLine(Nio,'li','@',
     $                     'The sum of the 3 weights associated '//
     $                     'with the innovation, for future period,'//
     $                     'should be zero.')
       CALL writTag(Nio,'</ul>')
       CALL mkPOneLine(Nio,'@','<strong>Note :</strong> some examples')
       CALL writTagClass(Nio,'ul','indent')
       CALL writTagOneLine(Nio,'li','@',
     $   'If the last observation on the series has a '//
     $   'relatively large weight for the seasonal '//
     $   'component, the series contains a relatively '//
     $   'important seasonal component.')
       CALL writTagOneLine(Nio,'li','@',
     $   'If the next period innovation has a relatively '//
     $   'large weight for the trend-cycle '//
     $   'component, the estimator of this component '//
     $   'will be strongly affected by the '//
     $   'next period forecast error (i.e., the first '//
     $   'revision of the concurrent estimator will be large).')
       CALL writTagOneLine(Nio,'li','@',
     $   'If the weight for some component, associated '//
     $   'with the innovation two-year into '//
     $   'the future is large, this would indicate that '//
     $   'the estimator, after two years of '//
     $   'revisions is still far from convergence.')
       CALL writTag(Nio,'</ul>')
      end if
C
C
C
C
C HERE INTRODUCE THE NEW VARIANCES TABLES
*       if (noserie .eq. 0) then
*        CALL VARIANCES(OZ,Z,TREND,SA,SC,CYCLE,IR,NZ,LAMD,OUT,
*     $                     QT1,VARWNP,VARWNS,VARWNC,
*     $                     THSTR0,QSTAR0,PSIEPS,PSIESS,
*     $                     PSIECS,PSIUE,PSIEA,NFL)
*       end if
C
C
C
C   OUTPUT ACF OF COMPONENTS
C
      if (out .eq. 0) then
       CALL genSkip(1005)
       CALL writTagOneLine(Nio,'h3','@',
     $                     'DISTRIBUTION OF COMPONENT, THEORETICAL '//
     $                     'ESTIMATOR AND EMPIRICAL ESTIMATE'//Cbr//
     $                     'AUTOCORRELATION FUNCTION OF COMPONENTS '//
     $                     '(STATIONARY TRANSFORMATION)')
*        call AddIdx2(Nidx,Nio,'ACF of Components (ST)',
*     $  'AUTOCORRELATION FUNCTION OF COMPONENTS '//
*     $  '(STATIONARY TRANSFORMATION)',0,4)
      end if
      mqo = mq
      nztr = nz - Nchins+1
      nzs = nz - Npsins+1
      nzsa = nz- Nadjns+1
      nstar = 0
      do i = 0,24
       bsetr(i) = ZERO
       bses(i) = ZERO
       bsesa(i) = ZERO
       bsecyc(i) = ZERO
       bseir(i) = ZERO
      end do
      if (noserie .eq. 0) then
       call SEBARTLETTACF (nz,nztr,nzs,nzsa,mserror,mqo,bsetr,bses,
     &                           bsesa,bsecyc,bseir,qt1)
      end if
*      if ((pg .eq. 0).and.(iter.eq.0).and.(out.eq.0)) then
*       fname = 'ACFTTRE.T2'
*       subtitle = 'THEOR. COMP.: ACF OF TREND-CYCLE (ST)'
*       call PLOTACF0(fname,subtitle,Acfpth,mq2,0,0)
*       fname = 'ACFRTRE.T2'
*       subtitle = 'THEOR. EST.: ACF OF TREND-CYCLE (ST)'
*       call PLOTACF0(fname,subtitle,Acfper,mq2,0,0)
*       fname = 'ACFETRE.T2'
*       subtitle = 'ESTIMATE: ACF OF TREND-CYCLE (ST)'
*       call PLOTACF0(fname,subtitle,Acfpem,mq2,0,0)
*       fname = 'ACFTSADJ.T2'
*       subtitle = 'THEOR. COMP.: ACF OF SA SERIES (ST)'
*       call PLOTACF0(fname,subtitle,Acfath,mq2,0,0)
*       fname = 'ACFRSADJ.T2'
*       subtitle = 'THEOR. EST.: ACF OF SA SERIES (ST)'
*       call PLOTACF0(fname,subtitle,Acfaer,mq2,0,0)
*       fname = 'ACFESADJ.T2'
*       subtitle = 'ESTIMATE: ACF OF SA SERIES (ST)'
*       call PLOTACF0(fname,subtitle,Acfaem,mq2,0,0)
*      end if
*      mqo = mq
C       IF (MQ.LT.4) MQO=2*MQ
      if (out .eq. 0) then
 6412  format('<thead><tr><td class="head">&nbsp;</td>',
     $        '<th scope="col">COMPONENT</th>',
     $        '<th scope="col">ESTIMATOR</th>',
     $        '<th scope="col">ESTIMATE</th>',
     $        '<th scope="col"><abbr title="standard error">SE</abbr>',
     $        '</th></tr></thead>')
 6001  format ('<tr><th scope="row">LAG ',i4,'</th>',
     $         4('<td class="center">',f11.3,'</td>'),'</tr>')
 6011  format ('<tr><th scope="row">LAG ',i4,'</th>',
     $         3('<td class="center">',f11.3,'</td>'),
     $         '<td class="center">(***)</td></tr>')
 6002  format ('<tfoot><tr><th scope="row">VARIANCE(*)</th>',
     $         4('<td class="center">',f11.3,'</td>'),'</tr></tfoot>')
 6012  format ('<tfoot><tr><th scope="row">VARIANCE(*)</th>',
     $         3('<td class="center">',f11.3,'</td>'),
     $         '<td class="center">(***)</td></tr></tfoot>')
       if (Nthetp .gt. 1) then
        CALL mkTableTag(Nio,'w60','Autocorrelation Function of the '//
     $                  'Trend-Cycle Component')
        CALL mkCaption(Nio,'<abbr title="autocorrelation function">'//
     $                      'ACF</abbr> OF TREND-CYCLE')
c thead
        write (Nio,6412)
        CALL writTag(nio,'<tbody>')
        do i = 1,mqo
         if (bsetr(i) .lt. ZERO) then
          nstar = nstar + 1
             write (Nio,6011)
     $          i, Acfpth(i), Acfper(i), Acfpem(i)
         else
          write (Nio,6001)
     $          i, Acfpth(i), Acfper(i), Acfpem(i), bsetr(i) 
         endif
        end do
        CALL writTag(Nio,'</tbody>')
c tfoot     
        if (bsetr(0) .lt. ZERO) then
         nstar = nstar + 1
         write (Nio,6012)
     $        Acfpth(0), Acfper(0), Acfpem(0)
        else
         write (Nio,6002)
     $       Acfpth(0), Acfper(0), Acfpem(0), bsetr(0)
        endif
c
        CALL writTag(Nio,'</table>')
        CALL mkPOneLine(Nio,'@','&nbsp;')
       end if
 6413   format('<thead><tr><td class="head">&nbsp;</td>',
     $         '<th scope="col">COMPONENT</th>',
     $         '<th scope="col">ESTIMATOR</th>',
     $         '<th scope="col">ESTIMATE</th>',
     $         '<th scope="col"><abbr title="standard error">SE</abbr>',
     $         '</th></tr></thead>')
       if (Nthadj .gt. 1) then
*         iTab=iTab+1
        CALL mkTableTag(Nio,'w60','Autocorrelation Function of the '//
     $                  'Trend-Cycle Component')
        CALL mkCaption(Nio,'<abbr title="autocorrelation function">'//
     $                      'ACF</abbr> OF <abbr title="seasonally '//
     $                      'adjusted">SA</abbr> SERIES')
         write (Nio,6413)
         CALL writTag(nio,'<tbody>')
         do i = 1,mqo
          if (bsesa(i) .lt. ZERO) then
           nstar = nstar + 1
             write (Nio,6011)
     $           i, Acfath(i), Acfaer(i),Acfaem(i)
          else
           write (Nio,6001)
     $         i, Acfath(i), Acfaer(i),Acfaem(i),bsesa(i)
          end if
         end do
        CALL writTag(Nio,'</tbody>')
         if (bsesa(0) .lt. ZERO) then
          nstar = nstar + 1
          write (Nio,6012)
     $       Acfath(0), Acfaer(0), Acfaem(0)
         else
          write (Nio,6002)
     $       Acfath(0), Acfaer(0), Acfaem(0), bsesa(0)
         end if
        CALL writTag(Nio,'</table>')
        CALL mkPOneLine(Nio,'@','&nbsp;')
       end if
 6414   format('<thead><tr><td class="head">&nbsp;</td>',
     $         '<th scope="col">COMPONENT</th>',
     $         '<th scope="col">ESTIMATOR</th>',
     $         '<th scope="col">ESTIMATE</th>',
     $         '<th scope="col"><abbr title="standard error">SE</abbr>',
     $         '</th></tr></thead>')
       if (Nthets .gt. 1) then
        CALL mkTableTag(Nio,'w60','Autocorrelation Function of the '//
     $                  'Seasonal Component')
        CALL mkCaption(Nio,'<abbr title="autocorrelation function">'//
     $                      'ACF</abbr> OF SEASONAL')
*         iTab=iTab+1
c   thead
         write (Nio,6414)
         CALL writTag(nio,'<tbody>')
         do i = 1,mqo
          if (bses(i) .lt. ZERO) then
           nstar = nstar + 1
             write (Nio,6011)
     $          i, Acfsth(i), Acfser(i), Acfsem(i)
          else
           write (Nio,6001)
     $          i, Acfsth(i), Acfser(i), Acfsem(i), bses(i)
          end if
         end do
        CALL writTag(Nio,'</tbody>')
c   tfoot
         if (bses(0) .lt. ZERO) then
          nstar = nstar + 1
          write (Nio,6012)
     $       Acfsth(0), Acfser(0), Acfsem(0)
         else
          write (Nio,6002)
     $       Acfsth(0), Acfser(0), Acfsem(0), bses(0)
         end if
        CALL writTag(Nio,'</table>')
        CALL mkPOneLine(Nio,'@','&nbsp;')
       end if
* 6002  format ('<tfoot><tr><th scope="row">VAR.(*)</th>',
*     $         4('<td>',f11.3,'</td>'),'</tr></tfoot>')
* 6012  format ('<tfoot><tr><th scope="row">VAR.(*)</th>',
*     $         3('<td>',f11.3,'</td>'),
*     $         '<td>(***)</td></tr></tfoot>')
*       if ((Acfpth(0).le.Acfper(0)).or.(Acfpth(0).le.Acfpem(0))) then
*         call setCvar('E')
*       end if
*       if ((Acfath(0).le.Acfaer(0)).or.(Acfath(0).le.Acfaem(0))) then
*         call setCvar('E')
*       end if
C
C COMMENT OUTPUT ACF CYCLE
C
c      if ((ncycth.eq.0) .and. (Ncyc.eq.1)) then
       if (Nthetc .gt. 1 .and. varwnc.gt.1.0d-10 ) then
 6401   format('<thead><tr><td class="head">&nbsp;</td>',
     $        '<th>COMPONENT</th><th>ESTIMATOR</th>',
     $        '<th>ESTIMATE</th><th>SE</th></tr></thead>')
        if (out.eq.0) then
*         if ((pg.eq.0).and.(iter.eq.0)) then
*          fname = 'ACFTTRA.T2'
*          write(subtitle,'("THEOR. COMP.: ACF OF ",A,
*     $          ". COMPONENT (ST)")') transCad(1:nTransCad)
*          call PLOTACF0(FNAME,SUBTITLE,ACFCTH,MQ2,0,0)
*          fname = 'ACFRTRA.T2'
*          write(subtitle,'("THEOR. EST.: ACF OF ",A,
*     $          ". COMPONENT (ST)")') transCad(1:nTransCad)
*          call PLOTACF0(FNAME,SUBTITLE,ACFCER,MQ2,0,0)
*          fname = 'ACFECYC.T2'
*          write(subtitle,'("ESTIMATE: ACF OF ",A,
*     $          ". COMPONENT (ST)")') transCad(1:nTransCad)
*          call PLOTACF0(FNAME,SUBTITLE,ACFCEM,MQ2,0,0)
*         end if
         CALL mkTableTag(Nio,'w60','@')
*          iTab=iTab+1
         if (IsCloseToTD) then
           CALL mkCaption(Nio,
     $                    '<abbr title="autocorrelation function">'//
     $                    'ACF</abbr> OF STOCHASTIC TRADING DAY')
         else
           CALL mkCaption(Nio,
     $                    '<abbr title="autocorrelation function">'//
     $                    'ACF</abbr> OF TRANSITORY')
         end if
         write (nio, 6401)
         CALL writTag(nio,'<tbody>')
         do i=1,mqo
          if (bsecyc(i) .lt. ZERO) then
           nstar = nstar + 1
           write (nio,6011) i,acfcth(i),acfcer(i),acfcem(i)
          else
           write (nio,6001) i,acfcth(i),acfcer(i),acfcem(i),bsecyc(i)
          end if
         end do
         CALL writTag(Nio,'</tbody>')
         if (bsecyc(0) .lt. ZERO) then
          nstar = nstar + 1
          write (nio,6012) acfcth(0),acfcer(0),acfcem(0)
         else
          write (nio,6002) acfcth(0),acfcer(0),acfcem(0),bsecyc(0)
         end if
         CALL writTag(Nio,'</table>')
         CALL mkPOneLine(Nio,'@','&nbsp;')
        end if
       end if
 6402  format('<tr><td>',i4,'</td>',
     $        4('<td class="center">',f11.3,'</td>'),'</tr>')
 6404  format('<tr><th>VAR.(*)</th>',
     $        4('<td class="center">',f11.3,'</td>'),'</tr>')
*       write (Nio,7003)
      end if
      if (noserie.eq.0) then
       if ((QT1.gt.ZERO .or.NPSI.gt.1 .or. NTHETS.gt.1) .and. 
     $   ((Acfpth(0).le. (Acfper(0)-spurMarg)).or.
     $    (abs(Acfper(0)-Acfpem(0)).gt.t_ACF*bsetr(0)))) then
        call setCvar('E')
      end if
       if ( (NPSI.gt.1 .or. NTHETS.gt.1)  .and.
     $    ((Acfath(0).le. (Acfaer(0)-spurMarg)).or.
     $    (abs(Acfaer(0)-Acfaem(0)).gt.t_ACF*bsesa(0)))) then
        call setCvar('E')
      end if
       if ((Acfith(0).le. (Acfier(0)-spurMarg)).or.
     $    (abs(Acfier(0)-Acfiem(0)).gt.t_ACF*bseir(0))) then
       call setCvar('E')
       end if
       if ( (NPSI.gt.1 .or. NTHETS.gt.1)  .and.
     $    (Acfsth(0).le. (Acfser(0)-spurMarg)).or.
     $    (abs(Acfser(0)-Acfsem(0)).gt.t_ACF*bses(0))) then
       call setCvar('E')
       end if
      end if
*      if ((out.eq.0).and.(iter.eq.0).and.(pg .eq. 0)) then
*       fname = 'ACFRIR.T2'
*       subtitle = 'THEOR. EST.: ACF OF IRREGULAR (ST)'
*       call PLOTACF0(fname,subtitle,Acfier,mq2,0,0)
*       fname = 'ACFEIR.T2'
*       subtitle = 'ESTIMATE: ACF OF IRREGULAR (ST)'
*       call PLOTACF0(fname,subtitle,Acfiem,mq2,0,0)
*       fname = 'ACFTSEAS.T2'
*       subtitle = 'THEOR. COMP.: ACF OF SEASONAL (ST)'
*       call PLOTACF0(fname,subtitle,Acfsth,mq2,0,0)
*       fname = 'ACFRSEAS.T2'
*       subtitle = 'THEOR. EST.: ACF OF SEASONAL (ST)'
*       call PLOTACF0(fname,subtitle,Acfser,mq2,0,0)
*       fname = 'ACFESEAS.T2'
*       subtitle = 'ESTIMATE: ACF OF SEASONAL (ST)'
*       call PLOTACF0(fname,subtitle,Acfsem,mq2,0,0)
*      end if
      dvec(1)=Acfpth(0)
      call USRENTRY(dvec,1,1,1,1,1910)
      dvec(1)=Acfper(0)          
      call USRENTRY(dvec,1,1,1,1,1911)
      dvec(1)=Acfpem(0)          
      call USRENTRY(dvec,1,1,1,1,1912)
      dvec(1)=Acfath(0)          
      call USRENTRY(dvec,1,1,1,1,1913)
      dvec(1)=Acfaer(0)          
      call USRENTRY(dvec,1,1,1,1,1914)
      dvec(1)=Acfaem(0)          
      call USRENTRY(dvec,1,1,1,1,1915)
      dvec(1)=Acfith(0)          
      call USRENTRY(dvec,1,1,1,1,1916)
      dvec(1)=Acfier(0)          
      call USRENTRY(dvec,1,1,1,1,1108)
      dvec(1)=Acfiem(0)          
      call USRENTRY(dvec,1,1,1,1,1109)
      dvec(1)=Acfsth(0)          
      call USRENTRY(dvec,1,1,1,1,1917)
      dvec(1)=Acfser(0)          
      call USRENTRY(dvec,1,1,1,1,1918)
      dvec(1)=Acfsem(0)          
      call USRENTRY(dvec,1,1,1,1,1919)
      if (out .eq. 0) then
       if (qt1.ne.ZERO) then
 6415   format('<thead><tr><td class="head">&nbsp;</td>',
     $         '<th scope="col">COMPONENT</th>',
     $         '<th scope="col">ESTIMATOR</th>',
     $         '<th scope="col">ESTIMATE</th>',
     $         '<th scope="col"><abbr title="standard error">SE</abbr>',
     $         '</th></tr></thead>')
        CALL mkTableTag(Nio,'w60','@')
        CALL mkCaption(Nio,'IRREGULAR')
*        iTab=iTab+1
        write (Nio,6415)
        CALL writTag(nio,'<tbody>')
        do i = 1,mqo
         if (bseir(i) .lt. ZERO) then
          nstar = nstar + 1
          write (Nio,6011)
     $         i, Acfith(i), Acfier(i), Acfiem(i)
         else
          write (Nio,6001)
     $          i, Acfith(i), Acfier(i), Acfiem(i), bseir(i)
         end if
        end do
        CALL writTag(nio,'</tbody>')
        if (bseir(0) .lt. ZERO) then
         nstar = nstar + 1
         write (Nio,6012)
     $         Acfith(0), Acfier(0), Acfiem(0)
        else
         write (Nio,6002)
     $        Acfith(0), Acfier(0), Acfiem(0), bseir(0)
        end if
        CALL writTag(nio,'</table>')
        CALL mkPOneLine(Nio,'@','&nbsp;')
        if (nstar .gt. 0) then
         CALL mkPOneLine(Nio,'bold','(***) : Unreliable SE estimate.')
        end if
       endif
       CALL mkPOneLine(Nio,'bold','(*) IN UNITS OF VAR(A)')
      endif
C
C END COMMENT
C
C
      end
C
C  THIS SUBPROGRAM COMPUTES THE STANDARD ERROR IN LEVELS FOR THE
C  COMPONENTS AND THEIR FORECAST
C
C    INPUT PARAMETERS
C      Z : ORIGINAL SERIES + FORECAST
C  TREND : TREND COMPONENT + FORECAST
C     SC : SEASONAL COMPONENT + FORECAST
C  CYCLE : CYCLICAL COMPONENT + FORECAST
C     SA : SEASONALLY ADJUSTED SERIES + FORECAST
C   NCHI : DIMENSION OF THE TREND DENOMINATOR MODEL
C   NPSI : DIMENSION OF THE SEASONAL DENOMINATOR MODEL
C   NCYC : DIMENSION OF THE CYCLE DENOMINATOR MODEL
C     NZ : DIMENSION OF THE SERIES AND COMPONENTS
C     MQ : FREQUENCY
C
      subroutine SERRORL(z,trend,sc,cycle,sa,nchi,npsi,ncyc,ncycth,nz,
     $                   sqf,lfor,alpha,IsCloseToTD,varwnc,out)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      INCLUDE 'dimensions.i'
      integer n12
      LOGICAL T
      real*8 ZERO,TWO,ONEHND
      parameter (n12 = 12, T = .true., ZERO = 0.0d0, TWO = 2.0d0,
     &           ONEHND = 100.0d0)
C
C.. Formal Arguments ..
      integer nchi,npsi,ncyc,ncycth,nz,lfor,out
      real*8 z(*),trend(*),sc(*),cycle(*),sa(*),sqf,alpha,varwnc
      logical IsCloseToTD
C
C.. Local Scalars ..
      integer i,j,mq2
      real*8 rfactse,sminus,splus
C
C.. Local Arrays ..
      real*8 sic(kp),sip(kp),sis(kp),sisa(kp),siz(kp),
     $       tmp(kp),stmp(kp),tz(kp),ttrn(kp),
     &       tcy(kp),tsa(kp),tsf(kp)
C
C.. Intrinsic Functions ..
      intrinsic EXP, LOG
      include 'sfcast.i'
      include 'serrlev.i'
      include 'sesfcast.i'
      include 'stream.i'
      include 'transcad.i'
C
C ... Executable Statements ...
C
      mq2 = lfor
*      if (mq2 .gt. 24) then
*       mq2 = 24
*      end if
      
      CALL setdp(ZERO,2*n12,sic)
      CALL setdp(ZERO,2*n12,sip)
      CALL setdp(ZERO,2*n12,sis)
      CALL setdp(ZERO,2*n12,sisa)
      CALL setdp(ZERO,2*n12,siz)
      
      if (nchi .gt. 1) then
       do i = nz+1,nz+mq2
        splus = LOG(trend(i)) + alpha*Setp(i-nz)
        sminus = LOG(trend(i)) - alpha*Setp(i-nz)
        sip(i-nz) = (EXP(splus)-EXP(sminus)) / (TWO * alpha)
       end do
      end if
      if (npsi .gt. 1) then
       do i = nz+1,nz+mq2
        splus = LOG(sc(i)/ONEHND) + alpha*Sets(i-nz)
        sminus = LOG(sc(i)/ONEHND) - alpha*Sets(i-nz)
        sis(i-nz) = (EXP(splus)-EXP(sminus)) / (TWO * alpha)
       end do
      else
       do i = nz+1,nz+mq2
        sis(i-nz) = ZERO
       end do
      end if
      if (varwnc.gt.1.0D-10 .and. ((ncycth.eq.1) .or. (ncyc.gt.1))) then
       do i = nz+1,nz+mq2
        splus = LOG(cycle(i)/ONEHND) + alpha*Setc(i-nz)
        sminus = LOG(cycle(i)/ONEHND) - alpha*Setc(i-nz)
        sic(i-nz) = (EXP(splus)-EXP(sminus)) / (TWO * alpha)
       end do
      else
       do i = nz+1,nz+mq2
        sic(i-nz) = ZERO
       end do
      end if
      if ((nchi+ncyc+ncycth) .gt. 2) then
       do i = nz+1,nz+mq2
        splus = LOG(sa(i)) + alpha*Seta(i-nz)
        sminus = LOG(sa(i)) - alpha*Seta(i-nz)
        sisa(i-nz) = (EXP(splus)-EXP(sminus)) / (TWO * alpha)
       end do
      else
       do i = nz+1,nz+mq2
        sisa(i-nz) = ZERO
       end do
      end if
      if (Nsfcast .eq. 0) then
       do i = nz+1,nz+mq2
        splus = z(i) + alpha*Seser(i-nz)
        sminus = z(i) - alpha*Seser(i-nz)
        siz(i-nz) = (EXP(splus)-EXP(sminus)) / (TWO * alpha)
       end do
      else
       do i = 1,mq2
        splus = Sfcast(i) + alpha*Sesfcast(i)
        sminus = Sfcast(i) - alpha*Sesfcast(i)
        Sesfcast(i) = (EXP(splus)-EXP(sminus)) / (TWO * alpha)
       end do
      end if
      if (Nsfcast .eq. 0) then
       do i = 1,mq2
        j = nz + i
        tmp(i) = EXP(z(j))
        stmp(i) = siz(i)
       end do
      else
       rfactse = Sqfsave / sqf
       do i = 1,mq2
        j = nz + i
        tmp(i) = EXP(Sfcast(i))
        stmp(i) = Sesfcast(i)
       end do
      end if
      call usrentry(tmp,1,mq2,1,kp,1205)
      call usrentry(stmp,1,mq2,1,kp,1206)
      if (Nsfcast .eq. 0) then
        call usrentry(sip,1,mq2,1,kp,1256)
        call usrentry(sisa,1,mq2,1,kp,1257)
      else
        rfactse = Sqfsave / sqf
        do i=1,mq2
          tmp(i) = sip(i)*rfactse
        enddo
        call usrentry(tmp,1,mq2,1,kp,1256)
        call usrentry(sisa,1,mq2,1,kp,1257)
      endif
      if (npsi .gt. 1) then
       do i=1,mq2
         tmp(i) = sis(i)*ONEHND
       enddo
       call usrentry(tmp,1,mq2,1,kp,1258)
      endif
      if (varwnc.gt.1.0D-10 .and. ((ncycth.eq.1) .or. (ncyc.gt.1))) then
       if (Nsfcast .eq. 0) then
        call usrentry(sic,1,mq2,1,kp,1259)
       else
         do i=1,mq2
           tmp(i) = sic(i)*ONEHND
         enddo
         call usrentry(tmp,1,mq2,1,kp,1259)
       endif
      endif
      if (out .ne.0) then
        return
      endif
 6000 format ('<thead><tr><th rowspan="2">&nbsp;</th>',
     $ '<th colspan="2" scope="colgroup">SERIES</th>',
     $ '<th colspan="2" scope="colgroup">TREND-CYCLE</th>',
     $ '<th colspan="2" scope="colgroup">ADJUSTED</th></tr>',
     $ '<tr><th scope="col">FORECAST</th>',
     $ '<th scope="col"><abbr title="Standard error">S.E.',
     $ '</abbr></th><th scope="col">FORECAST</th>',
     $ '<th scope="col"><abbr title="Standard error">S.E.',
     $ '</abbr></th><th scope="col">FORECAST</th>',
     $ '<th scope="col"><abbr title="Standard error">S.E.</abbr>',
     $ '</th></tr></thead>')
 6001 format ('<tr><th scope="row">Period ',i4,'</th>',
     $           6('<td>',g11.4,'</td>'),'</tr>')        
      CALL genSkip(1006)
      CALL mkTableTag(Nio,'w80','@')
      CALL mkCaption(Nio,
     $          'FORECAST OF STOCHASTIC SERIES AND COMPONENTS (LEVELS)')
      CALL makColgroup(Nio,0)
      CALL makColgroup(Nio,2)
      CALL makColgroup(Nio,2)
      CALL makColgroup(Nio,2)
      write (Nio,6000)
      CALL writTag(nio,'<tbody>')
      if (Nsfcast .eq. 0) then
       do i = 1,mq2
        j = nz + i
        tmp(i) = EXP(z(j))
        stmp(i) = siz(i)
         write (Nio,6001)
     $         i, EXP(z(j)), siz(i), trend(j), sip(i), sa(j), sisa(i)
        tz(i)=EXP(z(j))
        ttrn(i)=trend(j)
        tsa(i)=sa(j)
       end do
      else
       rfactse = Sqfsave / sqf
       do i = 1,mq2
        j = nz + i
        tmp(i) = EXP(Sfcast(i))
        stmp(i) = Sesfcast(i)
         write (Nio,6001)
     $         i, EXP(Sfcast(i)), Sesfcast(i),
     $         EXP(LOG(trend(j))*Rfact(i)), sip(i)*rfactse,
     $         EXP(Sfcast(i))-EXP(LOG(sc(j)/ONEHND)), sisa(i)
        tz(i)=EXP(Sfcast(i))
        ttrn(i)=EXP(LOG(trend(j))*Rfact(i))
        tsa(i)=EXP(Sfcast(i))-EXP(LOG(sc(j)/ONEHND))
       end do
      end if
      CALL writTag(nio,'</tbody>')
      CALL writTag(nio,'</table>')
      CALL mkPOneLine(Nio,'@','&nbsp;')
      if (varwnc.lt.1.0D-10 .or.(ncycth.eq.0) .and. (ncyc.eq.1)) then
 6002  format ('<thead><tr><th rowspan="2" scope="col">&nbsp;</th>',
     $         '<th colspan="2" scope="colgroup">',
     $         'SEASONAL FACTORS</th></tr><tr>',
     $         '<th scope="col">FORECAST</th>',
     $         '<th scope="col">STANDARD ERROR</th></tr></thead>')
        CALL genSkip(1007)
        CALL mkTableTag(Nio,'w50','@')
        CALL mkCaption(Nio,'FORECAST OF SEASONAL FACTORS')
        CALL makColgroup(Nio,0)
        CALL makColgroup(Nio,2)
        write (Nio,6002)
        CALL writTag(nio,'<tbody>')
        if (Nsfcast .eq. 0) then
         do i = 1,mq2
          j = nz + i
 6003     format ('<tr><th scope="row">',i4,
     $            '</th><td>',g11.4,
     $            '</td><td>',g11.4,
     $            '</td></tr>')
          write (Nio,6003) i, sc(j), sis(i)*ONEHND
          tsf(i)=sc(j)/ONEHND
         end do
        else
         do i = 1,mq2
          j = nz + i
          write (Nio,6003)
     $          i, EXP(LOG(sc(j)/ONEHND))*ONEHND, sis(i)*ONEHND
          tsf(i)=EXP(LOG(sc(j)/ONEHND))
         end do
        end if
        CALL writTag(nio,'</tbody>')
        CALL writTag(nio,'</table>')
        CALL mkPOneLine(Nio,'@','&nbsp;')
        if (Nsfcast .eq. 1) then
         CALL nWritln('THE APPROXIMATION WILL LIKELY INDUCE'//
     $                ' NOZERO IRREGULAR FORECASTS, '//
     $                'AND HENCE THE FORECAST OF THE ADJUSTED '//
     $                'SERIES WILL NOT BE THAT OF THE TREND-CYCLE',
     &                Nio,0,T,T)
        end if
      else
 6004   format ('<thead><tr><th scope="col" rowspan="2">&nbsp;</th>',
     $   '<th colspan="2" scope="colgroup">SEASONAL FACTORS</th>',
     $   '<th colspan="2" scope="colgroup">TRANSITORY COMPONENT</th>',
     $   '</tr>',/,7x,'<tr><th scope="col">FORECAST</th>',
     $   '<th scope="col">STANDARD ERROR</th>',
     $   '<th scope="col">FORECAST</th>',
     $   '<th scope="col">STANDARD ERROR</th></tr></thead>')
 6014   format ('<thead><tr><th scope="col" rowspan="2">&nbsp;</th>',
     $   '<th colspan="2" scope="colgroup">SEASONAL FACTORS</th>',
     $   '<th colspan="2" scope="colgroup">TRADING DAY STOCHASTIC ',
     $   'COMPONENT</th></tr>',/,7x,'<tr><th scope="col">FORECAST</th>',
     $   '<th scope="col">STANDARD ERROR</th>',
     $   '<th scope="col">FORECAST</th>',
     $   '<th scope="col">STANDARD ERROR</th></tr></thead>')
        CALL mkTableTag(Nio,'w80','@')
        CALL mkCaption(Nio,
     $          'FORECAST OF SEASONAL FACTORS AND TRANSITORY COMPONENT')
        CALL makColgroup(Nio,0)
        CALL makColgroup(Nio,2)
        CALL makColgroup(Nio,2)
        if (IsCloseToTD) then
          write (Nio,6014)
        else
          write (Nio,6004)
        end if
        CALL writTag(nio,'<tbody>')
        if (Nsfcast .eq. 0) then
         do i = 1,mq2
          j = nz + i
 6005     format ('<tr><td scope="row">',i4,
     $            '</td><td>',g11.4,
     $            '</td><td>',g11.4,
     $            '</td><td>',g11.4,
     $            '</td><td>',g11.4,
     $            '</td></tr>')
          write (Nio,6005) i, sc(j), sis(i)*ONEHND, cycle(j), sic(i)
          tsf(i)=sc(j)/ONEHND
          tcy(i)=cycle(j)/ONEHND
         end do
        else
         do i = 1,mq2
          j = nz + i
          write (Nio,6005)
     $          i, EXP(LOG(sc(j)/ONEHND))*ONEHND, sis(i)*ONEHND,
     $          EXP(LOG(cycle(j)/ONEHND))*ONEHND, sic(i)*ONEHND
          tsf(i)=EXP(LOG(sc(j)/ONEHND))
          tcy(i)=EXP(LOG(cycle(j)/ONEHND))
         end do
        end if
       CALL writTag(nio,'</tbody>')
       CALL writTag(nio,'</table>')
       CALL mkPOneLine(Nio,'@','&nbsp;')
        if (Nsfcast .eq. 1) then
         CALL nWritln('DUE TO THE APPROXIMATION, THE STANDARD ERROR '//
     $                'OF THE COMPONENT MAY BE UNRELIABLE',Nio,0,T,T)
        end if
      end if
      if ((ncycth.eq.0) .and. (ncyc.eq.1)) then
       call USRENTRY(tz,1,mq2,1,kp,3203)
       call USRENTRY(ttrn,1,mq2,1,kp,3201)
       call USRENTRY(tsa,1,mq2,1,kp,3204)
       call USRENTRY(tsf,1,mq2,1,kp,3202)
      ELSE
       call USRENTRY(tz,1,mq2,1,kp,3203)
       call USRENTRY(ttrn,1,mq2,1,kp,3201)
       call USRENTRY(tsa,1,mq2,1,kp,3204)
       call USRENTRY(tsf,1,mq2,1,kp,3202)
       call USRENTRY(tcy,1,mq2,1,kp,3205)
      END IF
      end
C
C
C
      subroutine BIASCORR(forbias,forsbias,fortbias,trend,sc,z,cycle,ir,
     $                    sa,mq,lfor,npsi,noC)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
      integer kf
      real*8 ZERO,ONE,TWO,THREE,ONEHND
      parameter (kf = 20, ZERO = 0.0d0, ONE = 1.0d0, TWO = 2.0d0,
     &           THREE = 3.0d0, ONEHND = 100.0d0)
C
C.. Formal Arguments ..
      integer mq,lfor,npsi
      real*8 forbias(kp),forsbias(kp),fortbias(kp),trend(mpkp),
     $       sc(mpkp),z(mpkp),cycle(mpkp),ir(mpkp),sa(mpkp)
      logical noC
C
C.. Local Scalars ..
      integer i,itf,j,j0,jf,jj0,jl,k,nf,nind,nt,cont1,cont
      real*8 a,dn0,dp0,zln0,zlp0,zmn0,zmp0,zmx0
C
C.. Local Arrays ..
      real*8 dln(mpkp+kf),dlp(mpkp+kf),dn(mpkp),dp(mpkp),
     $       satmp(mpkp+kf),trtmp(mpkp+kf),zln(mpkp),zlp(mpkp),
     $       zmn(mpkp),zmp(mpkp),zmx(mpkp),zsave(mpkp)
C
C.. Intrinsic Functions ..
      intrinsic DBLE, EXP, INT, MOD
C   LINES OF CODE ADDED FOR X-13A-S : 2
      logical dpeq
      external dpeq
C   END OF CODE BLOCK
      include 'sform.i'
C
C ... Executable Statements ...
C
      j0 = 0
      do i = 1,Nz+lfor
       zsave(i) = z(i)
      end do
      if (Nper .ne. 1) then
       j0 = mq + 1 - Nper
      end if
      jf = Nz - j0 - (INT(Nz-j0)/mq)*mq
      jl = ((lfor/mq)+1)*mq - lfor - jf
      itf = lfor + 2*mq + jl
      nf = INT((jf+itf)/mq)
      nt = nf + INT((Nz-j0)/mq)
      do i = 1,Nz+lfor
       trend(i) = EXP(trend(i))
       cycle(i) = EXP(cycle(i)) * ONEHND
*       if (npsi .gt. 1) then
       sa(i) = EXP(sa(i))
*       end if
       z(i) = EXP(z(i))
      end do
      do i = 1,kp
       fortbias(i) = EXP(fortbias(i))
       if (npsi .gt. 1) then
        forsbias(i) = EXP(forsbias(i))
       end if
       forbias(i) = EXP(forbias(i))
      end do
      if (npsi .gt. 1) then
       do i = 1,kp
        forsbias(i) = forbias(i) / forsbias(i)
       end do
      end if
      if (j0 .ne. 0) then
       zmx0 = ZERO
       zmn0 = ZERO
       zmp0 = ZERO
       do i = 1,j0
        zmx0 = zmx0 + z(i)
        zmn0 = zmn0 + sa(i)
        zmp0 = zmp0 + trend(i)
       end do
       zmx0 = zmx0 / DBLE(j0)
       zmn0 = zmn0 / DBLE(j0)
       zmp0 = zmp0 / DBLE(j0)
       zln0 = zmx0 - zmn0
       zlp0 = zmx0 - zmp0
      end if
      do i = 1,nt
       zmx(i) = ZERO
       zmp(i) = ZERO
       zmn(i) = ZERO
      end do
      do i = 1,nt
       do j = 1,mq
        k = j0 + (i-1)*mq + j
        if (k .gt. Nz) then
         zmx(i) = zmx(i) + forbias(k-Nz)
         zmp(i) = zmp(i) + fortbias(k-Nz)
         zmn(i) = zmn(i) + forsbias(k-Nz)
        else
         zmx(i) = zmx(i) + z(k)
         zmp(i) = zmp(i) + trend(k)
         zmn(i) = zmn(i) + sa(k)
        end if
       end do
       zmx(i) = zmx(i) / DBLE(mq)
       zmp(i) = zmp(i) / DBLE(mq)
       zmn(i) = zmn(i) / DBLE(mq)
C
C DEBUG
C
C         WRITE(NIO,'(2X,''ANNUAL MEAN'',/''SERIES TREND-CYCLE SA'')')
C         WRITE(NIO,'(2X,3G18.6)')ZMX(I),ZMP(I),ZMN(I)
      end do
      do i = 1,nt
       zln(i) = zmx(i) - zmn(i)
       zlp(i) = zmx(i) - zmp(i)
      end do
      do i = 1,nt-1
       dn(i) = zln(i) - zln(i+1)
       dp(i) = zlp(i) - zlp(i+1)
      end do
      dn0 = zln0 - zln(1)
      dp0 = zlp0 - zlp(1)
C
C WE OBTAIN THE PRELIMINARY CORRECTION FOR THE FIRST J0 OBS.
C
      if (j0 .ne. 0) then
       if (j0 .eq. 1) then
C
C PROBLEMS WITH FORECAST TREND 25-07-96
C
C           SATMP(1)=SA(I)
C           TRTMP(1)=TREND(I)
        satmp(1) = sa(1)
        trtmp(1) = trend(1)
       else
        jj0 = j0 / 2
        do i = 1,j0
         if (MOD(j0,2) .eq. 0) then
          satmp(i) = sa(i) + (zln0+(dn0/(2*jj0))*(jj0-i+1)-dn0/(2*j0))
          trtmp(i) =
     $      trend(i) + (zlp0+(dp0/(2*jj0))*(jj0-i+1)-dp0/(2*j0))
         else
          satmp(i) = sa(i) + (zln0+(dn0/(2*jj0))*(jj0-i+1))
          trtmp(i) = trend(i) + (zlp0+(dp0/(2*jj0))*(jj0-i+1))
         end if
        end do
       end if
      end if
C
C
      if (j0 .eq. 0) then
       a = dn(1) / DBLE((mq+1))
       dln(1) = zln(1) + (dn(1)/TWO) - a
       do j = 2,mq
        dln(j) = dln(j-1) - a
       end do
      else if (((dn0.gt.ZERO).and.(dn(1).gt.ZERO)) .or.
     $        ((dn0.lt.ZERO).and.(dn(1).lt.ZERO)) .or.
     $        (dpeq(dn0,ZERO).and.dpeq(dn(1),ZERO))) then
       a = dn(1) / DBLE(mq+1)
       dln(1) = zln(1) + (dn(1)/TWO) - a
       do j = 2,mq
        dln(j) = dln(j-1) - a
       end do
      else
       a = dn(1) / DBLE((mq/TWO)+ONE)
       dln(1) = zln(1) - dn(1)/TWO + a
       do j = 2,mq/2
        dln(j) = dln(j-1) + a
       end do
       dln((mq/2)+1) = dln(mq/2)
       do j = (mq/2)+2,mq
        dln(j) = dln(j-1) - a
       end do
      end if
      do i = 2,nt-1
       if (((dn(i).gt.ZERO).and.(dn(i-1).gt.ZERO)) .or.
     $     ((dn(i).lt.ZERO).and.(dn(i-1).lt.ZERO)) .or.
     $     (dpeq(dn(i),ZERO).and.dpeq(dn(i-1),ZERO))) then
        a = dn(i) / DBLE((mq+1))
        dln((i-1)*mq+1) = zln(i) + (dn(i)/TWO) - a
        do j = 2,mq
         dln((i-1)*mq+j) = dln((i-1)*mq+j-1) - a
        end do
       else
        a = dn(i) / DBLE((mq/TWO)+ONE)
        dln((i-1)*mq+1) = zln(i) - dn(i)/TWO + a
        do j = 2,mq/2
         dln((i-1)*mq+j) = dln((i-1)*mq+j-1) + a
        end do
        dln((i-1)*mq+(mq/2)+1) = dln((i-1)*mq+mq/2)
        do j = mq/2+2,mq
         dln((i-1)*mq+j) = dln((i-1)*mq+j-1) - a
        end do
       end if
      end do
C
C
C
      if (j0 .eq. 0) then
       a = dp(1) / DBLE((mq+1))
       dlp(1) = zlp(1) + (dp(1)/TWO) - a
       do j = 2,mq
        dlp(j) = dlp(j-1) - a
       end do
      else if (((dp0.gt.ZERO).and.(dp(1).gt.ZERO)) .or.
     $        ((dp0.lt.ZERO).and.(dp(1).lt.ZERO)) .or.
     $        (dpeq(dp0,ZERO).and.dpeq(dp(1),ZERO))) then
       a = dp(1) / DBLE((mq+1))
       dlp(1) = zlp(1) + (dp(1)/TWO) - a
       do j = 2,mq
        dlp(j) = dlp(j-1) - a
       end do
      else
       a = dp(1) / DBLE((mq/TWO)+ONE)
       dlp(1) = zlp(1) - dp(1)/TWO + a
       do j = 2,mq/2
        dlp(j) = dlp(j-1) + a
       end do
       dlp((mq/2)+1) = dlp(mq/2)
       do j = mq/2+2,mq
        dlp(j) = dlp(j-1) - a
       end do
      end if
      do i = 2,nt-1
       if (((dp(i).gt.ZERO).and.(dp(i-1).gt.ZERO)) .or.
     $     ((dp(i).lt.ZERO).and.(dp(i-1).lt.ZERO)) .or.
     $     (dpeq(dp(i),ZERO).and.dpeq(dp(i-1),ZERO))) then
        a = dp(i) / DBLE((mq+1))
        dlp((i-1)*mq+1) = zlp(i) + (dp(i)/TWO) - a
        do j = 2,mq
         dlp((i-1)*mq+j) = dlp((i-1)*mq+j-1) - a
        end do
       else
        a = dp(i) / DBLE((mq/TWO)+ONE)
        dlp((i-1)*mq+1) = zlp(i) - dp(i)/TWO + a
        do j = 2,mq/2
         dlp((i-1)*mq+j) = dlp((i-1)*mq+j-1) + a
        end do
        dlp((i-1)*mq+(mq/2)+1) = dlp((i-1)*mq+mq/2)
        do j = mq/2+2,mq
         dlp((i-1)*mq+j) = dlp((i-1)*mq+j-1) - a
        end do
       end if
      end do
C
C
C
      if (npsi.ne.1) then
        cont1=(nt-1)*mq
      else
        cont1=(nt-1-nf)*mq
      endif
      if (npsi.ne.1.or..not.noC)then
        cont=(nt-1)*mq
      else
        cont=(nt-1-nf)*mq
      endif
      do i = j0+1,cont1
       if (i .le. Nz) then
        sa(i) = sa(i) + dln(i-j0)
       else
        sa(i) = forsbias(i-Nz) + dln(i-j0)
       end if
      end do
      do i = j0+1,cont
       if (i .le. Nz) then
        trtmp(i) = trend(i) + dlp(i-j0)
       else
        trtmp(i) = fortbias(i-Nz) + dlp(i-j0)
       end if
      end do
C
C
C       TEST
C
C       DO 100 I=1,NT-1
C         ZZT=0.0D0
C         ZZS=0.0D0
C         DO 101 J=1,MQ
C           ZZT=ZZT+TRTMP((I-1)*MQ+J+J0)
C           ZZS=ZZS+SATMP((I-1)*MQ+J+J0)
C 101    CONTINUE
C        ZZT=ZZT/MQ
C        ZZS=ZZS/MQ
C        WRITE(*,*)ZZT,ZZS,I,(NT-1)*MQ
C        READ(*,*)
C 100  CONTINUE
C
C  SMOOTING THE TREND
C
      if (mq .eq. 12) then
       trend(1) = trtmp(1)
       trend(2) = (trtmp(1)+trtmp(2)+trtmp(3)) / THREE
       do i = 3,cont-2
        if (i .le. Nz+lfor) then
         trend(i) =
     $     (trtmp(i-2)+trtmp(i-1)+trtmp(i)+trtmp(i+1)+trtmp(i+2)) /
     $     5.0d0
         if (i .gt. Nz) fortbias(i-Nz) = trend(i)
        else
         fortbias(i-Nz) =
     $     (trtmp(i-2)+trtmp(i-1)+trtmp(i)+trtmp(i+1)+trtmp(i+2)) /
     $     5.0d0
        end if
       end do
      else
       trend(1) = trtmp(1)
       do i = 2,cont-2
        if (i .le. Nz+lfor) then
         trend(i) = (trtmp(i-1)+trtmp(i)+trtmp(i+1)) / THREE
         if (i .gt. Nz) fortbias(i-Nz) = trend(i)
        else
         fortbias(i-Nz) = (trtmp(i-1)+trtmp(i)+trtmp(i+1)) / THREE
        end if
       end do
      end if
      nind = cont - 1
      fortbias(nind-Nz) =
     $  (trtmp(nind-1)+trtmp(nind)+trtmp(nind+1)) / THREE
      fortbias(nind-Nz+1) = trtmp(nind+1)
      do i=cont,nz+lfor
        fortbias(i-nz)=trend(i)
      enddo
      do i = Nz+1,(nt-1)*mq
       forsbias(i-Nz) = sa(i)
      end do
      do i = 1,Nz+lfor
       sc(i) = (z(i)/sa(i)) * ONEHND
       ir(i) = sa(i) / (trend(i)*cycle(i)/ONEHND) * ONEHND
      end do
      do i = 1,Nz+lfor
       z(i) = zsave(i)
      end do
      end
C
C
      subroutine DETCOMP(hptmp,hptrtmp,hpcycle,psiep,psiea,sqf,ilen,oz,
     $                   bz,z,trend,sa,sc,ir,cycle,pread,a,na,osa,ot,
     $                   ftr,fsa,ncyc,ncycth,out,ndec,pg,nz,mq,lamd,
     $                   title,npsi,nchi,iter,ioneout,fortr,lfor,
     $                   nreestimated,itable,tabtables,nper,nyer,
     $                   IsCloseToTD,varwnc)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
      integer nfl
      logical T
      parameter (nfl = mp*2, T = .true.)
C   LINES OF CODE ADDED FOR X-13A-S : 5
      INCLUDE 'lzero.cmn'
      INCLUDE 'priadj.cmn'
      INCLUDE 'priusr.cmn'
      DOUBLE PRECISION ZERO,ONE,SMALL,ONEHND
      PARAMETER(ZERO=0.0D0,SMALL=1.0d-12,ONEHND=100.0d0,ONE=1.0d0)
C   END OF CODE BLOCK 
C
C.. Formal Arguments ..
      integer hpcycle,ilen,ncyc,ncycth,out,ndec,pg,nz,mq,lamd,npsi,nchi,
     $        iter,ioneout,fortr,lfor,nreestimated,itable,nper,nyer,na
      character title*80
      real*8 hptmp(mpkp),psiep(nfl),psiea(nfl),sqf,oz(mpkp),
     $       bz(mp+2*kp),
     $       z(mpkp),trend(mpkp),sa(mpkp),sc(mpkp),a(mpkp),
     $       ir(mpkp),cycle(mpkp),pread(mpkp),osa(mpkp),
     $       ot(mpkp),ftr(-kp:kp),fsa(-kp:kp),hptrtmp(mpkp),varwnc
      character tabtables*100
      logical IsCloseToTD
C
C.. Local Scalars ..
      integer i,i2,j,nf,nfor,ntitle,nyr,thisDate(2)
      character fname*30,subtitle*50,cad8*50,cad9*50
      real*8  bias1,bias2,bias3,sum
c      integer jadd,maxfat,maxfst,maxfxt,maxsat,maxsxt,zmax
c      real*8 mufat,mufst,mufxt,musat,musxt
c     real*8 aggsxt,aggfxt,aggsat,aggfat,aggfst
c     real*8 aavsxt,aavfxt,aavsat,aavfst,aavfat
c      real*8 sumfat,sumsxt,sumsat,sumfxt,sumfst
c     real*8 pplevfxt,pplevsat,pplevsxt,pplevfat,pplevfst
c      real*8 fat(mpkp),fst(mpkp),fxt(mpkp)
c      real*8 sat(mpkp),sxt(mpkp)
c      real*8 tmp1(mpkp)
      logical bool
C
C.. Local Arrays ..
      real*8 ceff(mpkp),fcyc(-kp:kp),fir(-kp:kp),fo(-kp:kp),
     $       freg(-kp:kp),fs(-kp:kp),ftmp(-kp:kp),
     $       ocyc(mpkp),oir(mpkp),osc(mpkp),
     $       sieaf(kl),sieafl(kl),siepf(kl),siepfl(kl),
     $       tmp(mpkp),fosa(mpkp)
C
C.. External Functions ..
      character*60 PERIODH
      external PERIODH
      real*8 DMEAN
      real*8 DMU
      integer ISTRLEN
      external DMEAN, DMU, ISTRLEN
      character GETCMTS
      external GETCMTS
      character GETCMTTC
      external GETCMTTC
      character GETCMTTS
      external GETCMTTS
      character GETCMTIR
      external GETCMTIR
C.. External Calls ..
      external FINALSE, FORTBL, OUTTABFOR, OUTTABLE, TABLE, USRENTRY
C
C.. Intrinsic Functions ..
      intrinsic ABS, DBLE, EXP, LOG, MAX, MOD
      include 'preadtr.i'
      include 'sfcast.i'
      include 'sesfcast.i'
      include 'stream.i'
      include 'titl.i'
      include 'bench.i'
      include 'force.cmn'
*      include 'indhtml.i'
C
C ... Executable Statements ...
C
C
C
c      nfor = MAX(lfor,MAX(8,2*mq))
      nfor=lfor
      ntitle = ISTRLEN(title)
      thisDate(1) = Nyer
      thisDate(2) = Nper
      if (Nsfcast .eq. 1) then
       if (lamd .eq. 0) then
        do i = 1,nfor
         ir(i+nz) = sa(i+nz) / (trend(i+nz)*(cycle(i+nz)/ONEHND))
        end do
       else
        do i = 1,nfor
         ir(i+nz) = sa(i+nz) - (trend(i+nz)+cycle(i+nz))
        end do
       end if
      end if
      if (Tramo .eq. 1) then
C
       if (out .eq. 0) then
        CALL genSkip(1008)
        CALL writTagOneLine(Nio,'h3','@',
     $                     'DETERMINISTIC COMPONENT (from regARIMA)')
       end if
C
C
C
       if (lamd .eq. 1) then
C
        if (nreestimated .eq. 1) then
         do i = nz+1,nz+MAX(lfor,MAX(8,2*mq))
          sum = ZERO
          do j = 0,5
           sum = sum + Pareg(i,j)
          end do
          sum = sum + Pareg(i,7)
          Tram(i) =
     $      z(i) + Paoutr(i) + Paouir(i) + Paous(i) + Paeast(i) + 
     $      Patd(i) + sum
         end do
        end if
        if (Noutr .eq. 1) then
         call USRENTRY(Paoutr,1,nz+lfor,1,mpkp,1300)
         if (out .eq. 0) then
          CALL genSkip(1123)
          call prttbl(thisDate,Mq,Paoutr,nz,'LEVEL SHIFT',Ndec,
     &                'ls.outlier.effects')
         end if
*         if ((pg.eq.0) .and. (out.le.2).and. (iter.eq.0)) then
*          fname = 'PAOTRF.T'
*          subtitle = 'LEVEL SHIFT'
*          call PLOTSERIES(fname,subtitle,Paoutr,nz,1,0.0d0)
*         end if
        end if
        if (Nouir .eq. 1) then
         call USRENTRY(Paouir,1,nz+lfor,1,mpkp,1301)
         if (out .eq. 0) then
          CALL genSkip(1124)
          call prttbl(thisDate,Mq,Paouir,nz,'TRANSITORY OUTLIERS',
     &                ndec,'tc.outlier.effects')
         end if
*         if ((pg.eq.0) .and. (out.le.2).and. (iter.eq.0)) then
*          fname = 'PAOIRF.T'
*          subtitle = 'TRANSITORY OUTLIERS'
*          call PLOTSERIES(fname,subtitle,Paouir,nz,1,0.0d0)
*         end if
        end if
        if (Nous .eq. 1) then
         call USRENTRY(Paous,1,nz+lfor,1,mpkp,1298)
         if (out .eq. 0) then
          CALL genSkip(1125)
          call prttbl(thisDate,Mq,Paouir,nz,'SEASONAL OUTLIERS',ndec,
     &                'seasonal.outlier.effects')
         end if
*         if ((pg.eq.0) .and. (out.le.1).and. (iter.eq.0)) then
*          fname = 'PAOSF.T'
*          subtitle = 'SEASONAL OUTLIERS'
*          call PLOTSERIES(fname,subtitle,Paous,nz,1,0.0d0)
*         end if
        end if
        if (Neast .eq. 1) then
         call USRENTRY(Paeast,1,nz+lfor,1,mpkp,1302)
         if (out .eq. 0) then
          CALL genSkip(1126)
          call prttbl(thisDate,Mq,Paeast,nz,'EASTER EFFECT',ndec,
     &                'easter.regression.effects')
         end if
*         if ((pg.eq.0) .and. (out.le.2).and. (iter.eq.0)) then
*          fname = 'PAEASF.T'
*          subtitle = 'EASTER EFFECT'
*          call PLOTSERIES(fname,subtitle,Paeast,nz,1,0.0d0)
*         end if
        end if
        if (Npatd .gt. 0) then
         call USRENTRY(Patd,1,nz+lfor,1,mpkp,1303)
         if (out .eq. 0) then
          CALL genSkip(1127)
          call prttbl(thisDate,Mq,Patd,nz,
     &                'DETERMINISTIC TRADING DAY EFFECT',ndec,
     &                'td.regression.effects')
         end if
*         if ((pg.eq.0) .and. (out.le.2).and. (iter.eq.0)) then
*          fname = 'PATDF.T'
*          subtitle = 'DETERMINISTIC TRADING DAY EFFECT'
*          call PLOTSERIES(fname,subtitle,Patd,nz,1,0.0d0)
*         end if
        end if
        if (NDS .gt. 0) then
         if (out .eq. 0) then
           CALL genSkip(1128)
           CALL makDivId(Nio,'deterministic.seasonal.component','@')
           CALL mkTableTag(Nio,'w60','DETERMINISTIC SEASONAL COMPONENT')
           CALL writTag(Nio,'<tr>')
           do i=1,mq
            CALL mkHeaderCellScope(Nio,0,0,'col','@',PeriodH(i,mq))
           end do
           CALL writTag(Nio,'</tr>')
           CALL writTag(Nio,'<tr>')
           do i=1,mq
            write (nio,100)DetSeas(i)
 100        format('<td scope="row">',G12.5,'</td>')
           end do
           CALL writTag(Nio,'</tr>')
           CALL writTag(Nio,'</table></div>')
           CALL mkPOneLine(Nio,'@','&nbsp;')
         end if
        end if
        if (Npareg .eq. 1) then
         if (Neff(2) .eq. 1) then
          do i = 1,nz+nfor
           tmp(i) = Pareg(i,2)
          end do
          call USRENTRY(tmp,1,nz+lfor,1,mpkp,1304)
          if (out .eq. 0) then
           CALL genSkip(1129)
           call prttbl(thisDate,Mq,tmp,nz,'CALENDAR REGRESSION EFFECT',
     &                 ndec,'cal.regression.effects')
          end if
*          if ((pg.eq.0) .and. (out.le.2).and. (iter.eq.0)) then
*           fname = 'SREGC.T'
*           subtitle = 'CALENDAR REGRESSION EFFECT'
*           call PLOTSERIES(fname,subtitle,tmp,nz,1,0.0d0)
*          end if
         end if
         if (Neff(1) .eq. 1) then
          do i = 1,nz+nfor
           tmp(i) = Pareg(i,1)
          end do
          call USRENTRY(tmp,1,nz+lfor,1,mpkp,1305)
          if (out .eq. 0) then
           CALL genSkip(1130)
           call prttbl(thisDate,Mq,tmp,nz,
     &                 'TREND-CYCLE REGRESSION EFFECT',ndec,
     &                 'trend.regression.effects')
          end if
*          if ((pg.eq.0) .and. (out.le.2).and. (iter.eq.0)) then
*           fname = 'TREGC.T'
*           subtitle = 'TREND-CYCLE REGRESSION EFFECT'
*           call PLOTSERIES(fname,subtitle,tmp,nz,1,0.0d0)
*          end if
         end if
         if (Neff(7) .eq. 1) then
          do i = 1,nz+nfor
           tmp(i) = Pareg(i,7)
          end do
          call USRENTRY(tmp,1,nz+lfor,1,mpkp,1315)
          if (out.eq.0) then
           CALL genSkip(1131)
           call prttbl(thisDate,Mq,tmp,nz,
     &                 'BUSINESS CYCLE REGRESSION EFFECT',ndec,
     &                 'bcycle.regression.effects')
          end if
*          if ((pg.eq.0) .and. (out.le.1).and. (iter.eq.0)) then
*           fname = 'BCREGC.T'
*           subtitle = 'BUSINESS CYCLE REGRESSION EFFECT'
*           call PLOTSERIES(fname,subtitle,tmp,nz,1,0.0d0)
*          end if
         end if
         if (Neff(3) .eq. 1) then
          do i = 1,nz+nfor
           tmp(i) = Pareg(i,3)
          end do
          call USRENTRY(tmp,1,nz+lfor,1,mpkp,1306)
          if ((Tramo .eq. 1).and.(out.eq.0)) then
           CALL genSkip(1132)
           call prttbl(thisDate,Mq,tmp,nz,
     &                 'IRREGULAR REGRESSION EFFECT',ndec,
     &                 'irr.regression.effects')
          end if
*          if ((pg.eq.0) .and. (out.le.2).and. (iter.eq.0)) then
*           fname = 'IREGC.T'
*           subtitle = 'IRREGULAR REGRESSION EFFECT'
*           call PLOTSERIES(fname,subtitle,tmp,nz,1,0.0d0)
*          end if
         end if
         if (Neff(5) .eq. 1) then
          do i = 1,nz+nfor
           tmp(i) = Pareg(i,5)
          end do
          call USRENTRY(tmp,1,nz+lfor,1,mpkp,1307)
          if (out .eq. 0) then
           CALL genSkip(1133)
           call prttbl(thisDate,Mq,tmp,nz,
     &                 'TRANSITORY COMPONENT REGRESSION EFFECT',ndec,
     &                 'trans.regression.effects')
          end if
*          if ((pg.eq.0) .and. (out.le.2).and. (iter.eq.0)) then
*           fname = 'TRAREGC.T'
*           subtitle = 'TRANSITORY COMPONENT REGRESSION EFFECT'
*           call PLOTSERIES(fname,subtitle,tmp,nz,1,0.0d0)
*          end if
         end if
         if (Neff(4) .eq. 1) then
          do i = 1,nz+nfor
           tmp(i) = Pareg(i,4)
          end do
          call USRENTRY(tmp,1,nz+lfor,1,mpkp,1308)
          if (out .eq. 0) then
           CALL genSkip(1134)
           call prttbl(thisDate,Mq,tmp,nz,
     &                 'OTHER REGRESSION EFFECT IN SEASONALLY '//
     &                 'ADJUSTED SERIES',ndec,
     &                 'other.regression.effects')
          end if
*          if ((pg.eq.0) .and. (out.le.2).and. (iter.eq.0)) then
*           fname = 'SAREGC.T'
*           subtitle = 'OTHER SA REGRESSION EFFECT'
*           call PLOTSERIES(fname,subtitle,tmp,nz,1,0.0d0)
*          end if
         end if
        end if
        if (out .eq. 0) then
         CALL genSkip(1009)
         CALL writTagOneLine(Nio,'h3','@','FINAL DECOMPOSITION')
        end if
        call USRENTRY(sa,1,nz,1,mpkp,1309)
        call USRENTRY(trend,1,nz,1,mpkp,1310)
        call USRENTRY(sc,1,nz+lfor,1,mpkp,1311)
        call USRENTRY(ir,1,nz,1,mpkp,1312)
        if ((ncycth.eq.1) .or. (ncyc.gt.1)) then
         call USRENTRY(cycle,1,nz,1,mpkp,1313)
        end if
        if ((Npareg.eq.1) .and. (Neff(0).eq.1)) then
         do i = 1,nz
          bz(i) = Pareg(i,0)
         end do
         if (out .eq. 0) then
          CALL genSkip(1135)
          call prttbl(thisDate,Mq,bz,nz,'SEPARATE REGRESSION EFFECT',
     &                ndec,'separate.regression.effects')
         end if
*         if (pg .eq. 0) then
*          fname = 'SPREGC.T'
*          subtitle = 'SEPARATE REGRESSION EFFECT'
*          call PLOTSERIES(fname,subtitle,bz,nz,1,0.0d0)
*         end if
        end if
        if (out .eq. 0) then
         CALL genSkip(2135)
         CALL writTagOneLine(Nio,'h3','@','FINAL COMPONENT')
        end if
C
C FINAL SEASONALLY ADJUSTED
C
*       if ((Neast.ne.0).or.(Neff(2).ne.0).or.(Npatd .ne.0).or.
*     $      (Neff(0).ne.0).or.(Nous.ne.0)) then
*          call setCmtSA('Y')
*        end if
        if (npsi.ne.1 .or.  Neff(1).ne.0 .or. 
     $        Neff(3).ne.0 .or. Neff(4).ne.0 .or. Neff(5).ne.0 .or.
     $        Noutr.ne.0 .or. Nouir.ne.0 .or. Nuspad.gt.0) then
         IF(Nuspad.gt.0)THEN
          do i = 1,nz+lfor
           i2 = Frstap + i - 1
           osa(i) = Tram(i) - 
     $              (sc(i)+Paeast(i)+Paous(i)+Patd(i)+Usrpad(i2)+
     $                     Pareg(i,2)+Pareg(i,0))
           if (isCloseToTD) then
            osa(i)=osa(i)-(cycle(i)+pareg(i,5))
           end if
           fosa(i) = osa(i)
          end do
         ELSE
          do i = 1,nz+lfor
           osa(i) =
     $     Tram(i) - (sc(i)+Paeast(i)+Paous(i)+Patd(i)+
     $                Pareg(i,2)+Pareg(i,0))
           if (isCloseToTD) then
            osa(i)=osa(i)-(cycle(i)+pareg(i,5))
           end if
           fosa(i) = osa(i)
          end do
         END IF
         call USRENTRY(osa,1,nz,1,mpkp,1309)
         if (out .eq. 0) then
          CALL genSkip(2136)
          call prttbl(thisDate,Mq,osa,nz,
     &                'FINAL SEASONALLY ADJUSTED SERIES',ndec,
     &                'sa.seats.final')
         end if
*         if (pg.eq.0) then
*          if(iter.ne.0) then
*           if ((ioneout.eq.0) .and. (out.lt.2)) then
*            fname = title(1:ntitle) // '.SA'
*            subtitle = 'FINAL SEASONALLY ADJUSTED SERIES'
*            call PLOTSERIES(fname,subtitle,osa,nz,1,0.0d0)
*            write (17,'(A)') fname
*           end if
*          else
*           if (out.lt.3) then
*            fname = 'SAFIN.T'
*            subtitle = 'FINAL SA SERIES'
*            call PLOTSERIES(fname,subtitle,osa,nz,1,0.0d0)
*           end if
*          end if 
*         end if
cc
c Benchmark
cc
         if (((MQ.eq.4) .or. (MQ.eq.12)) .and. (bcMark.eq.1)) then 
          Lamda = Blamda
          Mid = Bmid
          Rol = Brol
          IF (rol.gt.0.99999D00) THEN
           if (MQ .eq.12) then
             rol = 0.9d0
           else
            rol = 0.729d0 
           end if
          end if
          Iftrgt = Bserie
          if (Bserie .eq. 0) then
           do i=1,nz+lfor
             tmp(i)=Tram(i)
           end do
          else if (Bserie .eq. 1) then
           do i=1,nz+lfor
             tmp(i)=Tram(i)-Paeast(i) - Patd(i) - Pareg(i,6)
           end do
          else if (Bserie .eq. 2) then
           do i=1,nz+lfor
             tmp(i)=z(i) + Paeast(i) + Patd(i) + Pareg(i,6)
           end do
          else if (Bserie .eq. 3) then
           do i=1,nz+lfor
             tmp(i)=z(i)
           end do
          end if
          Begyrt = 1
          call qmap2(tmp,osa,fosa,1,nz+lfor,mq,0)
          if (out .eq. 0) then
           CALL genSkip(2137)
           call prttbl(thisDate,Mq,osa,nz,
     &           'FINAL SEASONALLY ADJUSTED SERIES WITH REVISED YEARLY',
     &                 ndec,'sa.seats.final.rev')
          end if
*          if (pg .eq. 0) then
*           if (iter.ne.0) then 
*            if ((ioneout.eq.0) .and. (out.eq.0)) then
*             fname = title(1:ntitle) // '.SAR'
*             subtitle = 'FINAL SA SERIES WITH REVISED YEARLY'
*             call PLOTSERIES(fname,subtitle,fosa,nz,1,0.0d0)
*             write (17,'(A)') fname
*            end if
*           else
*            if (out.lt.2) then
*             fname = 'FSAFIN.T'
*             subtitle = 'FINAL SA SERIES WITH REVISED YEARLY'
*             call PLOTSERIES(fname,subtitle,fosa,nz,1,0.0d0)
*            end if
*           end if
*          end if
          call USRENTRY(fosa,1,nz,1,mpkp,1314)
         end if
cc
c
cc
        else
         do i = 1,nz
          osa(i) =Tram(i)
         end do
         call USRENTRY(osa,1,nz,1,mpkp,1309)
        end if
C
C FINAL TREND
C
        if ((Noutr.ne.0).or.(Neff(1).ne.0).or.(Neff(7).ne.0)) then
         call setCmtTc('Y')
        end if
        if (nchi.ne.1 .or. Noutr.ne.0 .or. Neff(1).ne.0 .or.
     $      Neff(7).ne.0) then
         do i = 1,nz
          ot(i) = trend(i) + Paoutr(i) + Pareg(i,1) + Pareg(i,7)
         end do
         call USRENTRY(ot,1,nz,1,mpkp,1310)
         if (out .eq. 0) then
          CALL genSkip(2138)
          call prttbl(thisDate,Mq,ot,nz,'FINAL TREND-CYCLE',ndec,
     &                'trend.seats.final')
         end if
*         if (pg .eq. 0) then
*          if (iter.ne.0) then 
*            if ((ioneout.eq.0) .and. (out.lt.2)) then
*             fname = title(1:ntitle) // '.TRE'
*             subtitle = 'FINAL TREND-CYCLE'
*             call PLOTSERIES(fname,subtitle,ot,nz,1,0.0d0)
*             write (17,'(A)') fname
*            end if
*          else
*            if (out.lt.3) then
*             fname = 'TRFIN.T'
*             subtitle = 'FINAL TREND-CYCLE'
*             call PLOTSERIES(fname,subtitle,ot,nz,1,0.0d0)
*            end if
*          end if   
*         end if
        else
         do i = 1,nz
          ot(i) = ZERO
         end do
        end if
C
C FINAL SEASONAL
C
        if ((Neast.ne.0).or.(Neff(2).ne.0).or.(Npatd.ne.0) .or.
     $      (Nous .ne. 0).or.(IsCloseToTD.and.neff(5).ne.0)) then
         call setCmtS('Y')
        end if
        if (npsi.ne.1 .or.( Neast.ne.0 .or. Neff(2).ne.0 .or. 
     $      Npatd.ne.0.or. Nous.ne.0 .or. IsCloseToTD))THEN
         do i = 1,nz+lfor
          osc(i) = sc(i) + Paeast(i) + Patd(i) + Pareg(i,2) + Paous(i)
          if (isCloseToTD) then
            osc(i)=osc(i)+cycle(i)+Pareg(i,5)
          end if
         end do
         call USRENTRY(osc,1,nz+lfor,1,mpkp,1311)
         if (npsi.ne.1 .and.( Neast.ne.0 .or. Neff(2).ne.0 .or. 
     $      Npatd.ne.0.or. Nous.ne.0 .or. IsCloseToTD))
     $     then
          if (out .eq. 0) then
           CALL genSkip(2139)
           call prttbl(thisDate,Mq,osc,nz,'FINAL SEASONAL',ndec,
     &                 'seasonal.seats.final')
          end if
*         if (pg .eq. 0) then
*          if (iter.eq.0) then
*           if (out.lt.3) then
*            fname = 'SFIN.T'
*            subtitle = 'FINAL SEASONAL'
*            call PLOTSERIES(fname,subtitle,osc,nz,1,0.0d0)
*           end if
*          else
*           if (out.lt.2) then
*            fname = title(1:ntitle) // '.sf'
*            subtitle = 'FINAL SEASONAL'
*            call PLOTSERIES(fname,subtitle,osc,nz,1,0.0d0)
*            write (17,'(A)') fname
*           end if
*          end if
*         end if
         endif
        else
         do i = 1,nz
          osc(i) = ZERO
         end do
        end if
C
C FINAL CYCLE or Final TD
C
        do i = 1,nz
         ocyc(i) = cycle(i) + Pareg(i,5)
        end do
        if (isCloseToTD) then
         do i=1,nz
           ocyc(i)=ocyc(i)+patd(i)
         end do
        end if
        if (Neff(5) .eq. 1 .or. (iSCloseToTD.and.Npatd.ne.0)) then
         call setCmtTs('Y')
        end if
        if ((varwnc.gt.1.0D-10 .and.((ncycth.eq.1) .or. (ncyc.gt.1)))
     $        .or. (Neff(5).eq.1).or.
     $       (isCloseTotD.and.Npatd.ne.0)) then
         if (isCloseToTD) then
           cad8='FINAL TD COMPONENT'
           CALL genSkip(2140)
           call USRENTRY(ocyc,1,nz,1,mpkp,1316)
         else
           cad8='FINAL TRANSITORY COMPONENT'
           CALL genSkip(2141)
         end if
C               WRITE(NIO,'(//,2X,''FINAL TRANSITORY COMPONENT'',/)')
         call USRENTRY(ocyc,1,nz,1,mpkp,1313)
*         if (pg .eq. 0) then
*          if (iter.ne.0) then 
*           if ((ioneout.eq.0) .and. (out.lt.0)) then
*            fname = title(1:ntitle) // '.CYC'
*            call PLOTSERIES(fname,cad8,ocyc,nz,1,0.0d0)
*            write (17,'(A)') fname
*           end if
*          else
*           if (out.lt.3) then
*            fname = 'TRAFIN.T'
*            call PLOTSERIES(fname,cad8,ocyc,nz,1,0.0d0)
*           end if 
*          end if
*         end if 
        end if
C
C FINAL IRREGULAR
C
        do i = 1,nz
         oir(i) = ir(i) + Paouir(i) + Pareg(i,3)
        end do
        call USRENTRY(oir,1,nz,1,mpkp,1312)
        if ((Nouir.ne.0) .or. (Neff(3).ne.0)) then
         call setCmtIR('Y')
C             IF (OUT.LT.2) THEN
C               WRITE(NIO,'(//,2X,''FINAL IRREGULAR'',/)')
C               CALL TABLE(OIR)
C             end if
*         if (pg .eq. 0) then
*          if (iter.eq.0) then
*           if (out.lt.3) then
*            fname = 'IRFIN.T'
*            subtitle = 'FINAL IRREGULAR'
*            call PLOTSERIES(fname,subtitle,oir,nz,1,0.0d0)
*           end if
*          else 
*           if (out.lt.2) then
*            fname = title(1:ntitle) // '.FIR'
*            subtitle = 'FINAL IRREGULAR'
*            call PLOTSERIES(fname,subtitle,oir,nz,1,0.0d0)
*            write (17,'(A)') fname
*           end if
*          end if
*         end if
        end if
        call SETCMTSA(GETCMTS())
        call SETCMTSA(GETCMTTC())
        call SETCMTSA(GETCMTTS())
        call SETCMTSA(GETCMTIR())
        if (NEFF(4).ne.0) then
         call SETCMTSA('Y')
        end if
        if (out.eq.0) then        
         if ((varwnc.gt.1.0D-10 .and.((ncycth.eq.1) .or. (ncyc.gt.1)))
     $      .or. (Neff(5).eq.1) .or.
     $      (Nouir.ne.0) .or. (Neff(3).ne.0).or.
     $      (isCloseToTD.and.NpaTD.ne.0)) then
          do i = 1,nz
           tmp(i) = ocyc(i) + oir(i)
          end do
          CALL genSkip(2142)
          call prttbl(thisDate,Mq,tmp,nz,
     &                'FINAL TRANSITORY-IRREGULAR COMPONENT',Ndec,
     &                'transirr.seats.final')
         else
*           itab=itab+1
          CALL writTagOneLine(Nio,'h3','@','FINAL IRREGULAR FACTORS')
          CALL mkPOneLine(Nio,'bold',
     &                   'The same as the stochastic irregular.')
         end if
        end if
C
C
C
        nf = lfor
        if (Nsfcast .eq. 0) then
         do i = (-nf),nf
          ftr(i) = trend(nz+i) + Paoutr(nz+i) + Pareg(nz+i,1)
     $            + Pareg(nz+i,7)
         end do
         do i = (-nf),nf
          fir(i) = Paouir(nz+i) + ir(nz+i) + Pareg(nz+i,3)
         end do
         do i = (-nf),nf
          fs(i) = sc(nz+i) + Paeast(nz+i) + Paous(nz+i) + Patd(nz+i) + 
     $            Pareg(nz+i,2)
          if (isCloseTotD) then
            fs(i)=fs(i)+cycle(nz+i)+Pareg(nz+i,5)
          end if
         end do
         do i = (-nf),nf
          fcyc(i) = cycle(nz+i) + Pareg(nz+i,5)
          if (isCloseToTD) then
            fcyc(i)=fcyc(i)+PaTD(nz+i)
          end if
         end do
         do i = (-nf),nf
          fsa(i) =
     $      Tram(nz+i) -
     $      (sc(nz+i)+Paeast(nz+i)+Patd(nz+i)+Pareg(nz+i,2)+
     $       Pareg(nz+i,0)+Paous(nz+i))
          if (isCloseToTD) then
            fsa(i)=fsa(i)-(cycle(nz+i)+Pareg(nz+i,5))
          end if
         end do
         if (fortr .eq. 1) then
          do i = 1,nf
           if (isCloseToTD) then
            ftr(i) = fsa(i) - fir(i)
           else
            ftr(i) = fsa(i) - fcyc(i) - fir(i)
           end if
          end do
         end if
         do i = (-nf),nf
          freg(i) = Pareg(nz+i,0)
          fo(i) = Tram(nz+i)
         end do
        else
         do i = (-nf),nf
          if (i .gt. 0) then
           ftr(i) = trend(nz+i)*Rfact(i) + Paoutr(nz+i) + Pareg(nz+i,1)
     $             +Pareg(nz+i,7)
          else
           ftr(i) = trend(nz+i) + Paoutr(nz+i) + Pareg(nz+i,1)
     $             +Pareg(nz+i,7)
          end if
         end do
         do i = (-nf),nf
          fir(i) = Paouir(nz+i) + ir(nz+i) + Pareg(nz+i,3)
         end do
         do i = (-nf),nf
          fs(i) = sc(nz+i) + Paeast(nz+i) + Patd(nz+i) + Paous(nz+i) + 
     $            Pareg(nz+i,2)
          if (isCloseToTD) then
            fs(i)=fs(i)+cycle(nz+i)+Pareg(nz+i,5)
          end if
         end do
         do i = (-nf),nf
          fcyc(i) = cycle(nz+i) + Pareg(nz+i,5)
         end do
         do i = (-nf),nf
          fsa(i) =
     $      Tram(nz+i) -
     $      (sc(nz+i)+Paeast(nz+i)+Patd(nz+i)+Pareg(nz+i,2)+
     $       Pareg(nz+i,0)+Paous(nz+i))
          if (isCloseToTD) then
            fsa(i)=fsa(i)-(cycle(nz+i)+Pareg(nz+i,5))
          end if
         end do
         if (fortr .eq. 1) then
          do i = 1,nf
           if (isCloseToTD) then
            ftr(i) = fsa(i) - fir(i) 
           else
            ftr(i) = fsa(i) - fir(i) - fcyc(i)
           end if
           ftr(i) = ftr(i) * Rfact(i)
          end do
         end if
         do i = (-nf),nf
          freg(i) = Pareg(nz+i,0)
          fo(i) = Tram(nz+i)
         end do
        end if
        if (nreestimated .eq. 1.and.tramo.eq.0)then 
*         if(out.eq.0)then
*          CALL nWritln('SINCE SEATS HAS RE-ESTIMATED AND CHANGED THE'//
*     $                 ' MODEL, THE FORECAST OF THE ORIGINAL '//
*     &                 '(UNCORRECTED) SERIES WILL DIFFER FROM THAT '//
*     &                 'IN regARIMA output.',Nio,0,T,T)
*         end if
         call USRENTRY(Tram,1,nz+nf,1,mpkp,213)
        end if
        if (itable .eq. 1) then
         do i = 1,nz+nfor
          ceff(i) = Paeast(i) + Patd(i) + Pareg(i,6)
         end do
         if (ITER .gt. 2) then
          call ProcTables(tabtables)
         end if
         call OUTTABLE2(titleg,Tram,ot,osa,osc,oir,ocyc,pread,ceff,
     $                 eresid,numEresid,hptmp,hptrtmp,hpcycle,lamd,1,
     $                 nz,mq,2,kunits,nf,trend,sa,fosa,IsCloseToTD)
         call OUTTABFOR(ftr,fsa,fs,fir,fcyc,pread,ceff,hptmp,
     $                  hptrtmp,hpcycle,lamd,1,nf,nz,mq,trend,sa,fosa)
        end if
C
C TABLES WITH THE SE OF FINAL COMPONENTS
C
        if (out .eq. 0) then
C Modified by REG, on 28 Feb 2006, to add out to FINALSE parameter list.
         call FINALSE(psiep,psiea,trend,sa,siepf,siepfl,sieaf,sieafl,
     $                sqf,ilen,mq,lfor,lamd,out,ndec)
C
C
C
         CALL genSkip(1011)
         CALL writTagOneLine(Nio,'h3','@','FORECAST OF FINAL COMPONENT')
         call HTMLFORTBL(fo,freg,ftr,fsa,fs,fcyc,fir,Tse,siepf,siepfl,
     $               sieaf,sieafl,Neff,mq,Nouir,Noutr,Npatd,Neast,nchi,
     $               npsi,ncyc,ncycth,lamd,nper,nyer,nz,nf,isCloseToTD,
     $               varwnc)
         if (Nsfcast .ne. 0) then
          CALL mkPOneLine(Nio,'bold','THE FORECAST OF THE IRREGULAR '//
     $                    'ABSORBS THE EFFECT OF THE APPROXIMATION.')
         end if
        end if
        do i=1,nf
         tmp(i)=fsa(i)
        end do
        call USRENTRY(tmp,1,nf,1,mpkp,1409)
        do i=1,nf
         tmp(i)=ftr(i)
        end do
        call USRENTRY(tmp,1,nf,1,mpkp,1410)
        do i=1,nf
         tmp(i)=fs(i)
        end do
        call USRENTRY(tmp,1,nf,1,mpkp,1411)
        do i=1,nf
         tmp(i)=fir(i)
        end do
        call USRENTRY(tmp,1,nf,1,mpkp,1412)
        if (varwnc.gt.1.0D-10 .and.((ncycth.eq.1).or.(ncyc.gt.1)))then
         do i=1,nf
          tmp(i)=fcyc(i)
         end do
         call USRENTRY(tmp,1,nf,1,mpkp,1413)
        end if
*        if ((pg.eq.0).and.(iter.eq.0)) then
*         if (out.lt.2) then
*           if (Npareg .eq. 1) then
*            fname = 'FREG.T5'
*            subtitle = 'FORECAST TOTAL REGRESSION EFFECT'
*            call PLOTFCAST1(fname,subtitle,freg,nf,nz,0)
*           end if
*           if (Neff(0).eq. 1) then
*            fname = 'SPREGF.T5'
*            subtitle = 'FORECAST SEPARATE REG. EFFECT'
*            do i = (-nf),nf
*             ftmp(i) = Pareg(nz+i,0) * 100.0d0
*            end do
*            call PLOTFCAST1(fname,subtitle,ftmp,nf,nz,0)
*           end if
*           if ((Neff(5).eq.1) .or. 
*     $        (varwnc.gt.1.0D-10 .and.((ncycth.eq.1) .or. (ncyc.gt.1)))
*     $          .or.(isCloseTotD.and.Npatd.ne.0)) then
*            fname = 'FTRAFIN.T5'
*            if (IsCloseToTD) then
*             subtitle = 'FORECAST FINAL TD COMPONENT'        
*            else
*             subtitle = 'FORECAST FINAL TRANSITORY COMPONENT'
*            end if
*            call PLOTFCAST1(fname,subtitle,fcyc,nf,nz,0)
*           end if
*           if ((Neff(3).eq.1) .or. (Nouir.eq.1)) then
*            fname = 'FIRFIN.T5'
*            subtitle = 'FORECAST FINAL IRREGULAR'
*            call PLOTFCAST1(fname,subtitle,fir,nf,nz,0)
*           end if
*         end if
*         if (out.lt.3) then
*           fname = 'FUNORIG.T5'
*           subtitle = 'FORECAST OF SERIES'
*           call PLOTFCAST1(fname,subtitle,fo,nf,nz,0)
*           if (npsi.ne.1 .or. (Neast+Neff(2)+Npatd).ne.0) then
*            fname = 'FSAFIN.T5'
*            subtitle = 'FORECAST FINAL SA SERIES'
*            call PLOTFCAST1(fname,subtitle,fsa,nf,nz,0)
*            fname = 'FSFIN.T5'
*            subtitle = 'FORECAST FINAL SEASONAL'
*            call PLOTFCAST1(fname,subtitle,fs,nf,nz,0)
*           end if
*           if (nchi.ne.1 .or. Noutr.ne.0 .or. Neff(1).ne.0 .or.
*     $         Neff(7).ne.0) then
*            fname = 'FTRFIN.T5'
*            subtitle = 'FORECAST FINAL TREND-CYCLE'
*            call PLOTFCAST1(fname,subtitle,ftr,nf,nz,0)
*           end if          
*         end if
*        end if
       else
C
C LAMDA EQUAL TO ZERO
C
        if (nreestimated .eq. 1) then
         do i = nz+1,nz+MAX(lfor,MAX(8,2*mq))
          sum = ONE
          do j = 0,5
           sum = sum * Pareg(i,j)
          end do
          sum = sum * Pareg(i,7)
          Tram(i) =
     $      EXP(z(i)) * Paoutr(i) * Paouir(i) * Paeast(i) * Patd(i) *
     $      sum
         end do
        end if
C      IF ((OUT.LT.2).OR.(OUT.EQ.3)) THEN
        if (Noutr .eq. 1) then
         do i = 1,nz+nfor
          bz(i) = Paoutr(i) * ONEHND
         end do
         call USRENTRY(bz,1,nz+nfor,1,mpkp,1300)
         if (out .eq. 0) then
          CALL genSkip(1143)
          if (Ndec.eq.0)THEN
           call prttbl(thisDate,Mq,bz,nz,'LEVEL SHIFT (X 100)',2,
     &                 'ls.regression.effects')
          else 
           call prttbl(thisDate,Mq,bz,nz,'LEVEL SHIFT (X 100)',Ndec,
     &                 'ls.regression.effects')
          end if
         end if
*         if ((pg.eq.0) .and. (out.le.2).and. (iter.eq.0)) then
*          fname = 'PAOTRF.T'
*          subtitle = 'LEVEL SHIFT FACTORS'
*          call PLOTSERIES(fname,subtitle,bz,nz,1,888.0d0)
*         end if
        end if
        if (Nouir .eq. 1) then
         do i = 1,nz+nfor
          bz(i) = Paouir(i) * ONEHND
         end do
         call USRENTRY(bz,1,nz+nfor,1,mpkp,1301)
         if (out .eq. 0) then
          CALL genSkip(1144)
          if (Ndec.eq.0)THEN
           call prttbl(thisDate,Mq,bz,nz,'TRANSITORY OUTLIERS (X100)',
     &                 2,'tc.regression.effects')
          else
           call prttbl(thisDate,Mq,bz,nz,'TRANSITORY OUTLIERS (X100)',
     &                 Ndec,'tc.regression.effects')
          end if
         end if
        else
*         if ((pg.eq.0) .and. (out.le.2).and. (iter.eq.0)) then
*          fname = 'PAOIRF.T'
*          subtitle = 'TRANSITORY OUTLIERS FACTORS'
*          call PLOTSERIES(fname,subtitle,bz,nz,1,888.0d0)
*         end if
        end if
        if (Neast .eq. 1) then
         do i = 1,nz+nfor
          bz(i) = Paeast(i) * ONEHND
         end do
         call USRENTRY(bz,1,nz+nfor,1,mpkp,1302)
         if (out .eq. 0) then
          CALL genSkip(1146)
          if (Ndec.eq.0)THEN
           call prttbl(thisDate,Mq,bz,nz,'EASTER EFFECT (X 100)',2,
     &                'easter.regression.effects')
          else
           call prttbl(thisDate,Mq,bz,nz,'EASTER EFFECT (X 100)',Ndec,
     &                'easter.regression.effects')
          end if
         end if
*         if ((pg.eq.0) .and. (out.le.2).and. (iter.eq.0)) then
*          fname = 'PAEASF.T'
*          subtitle = 'EASTER EFFECT FACTORS'
*          call PLOTSERIES(fname,subtitle,bz,nz,1,888.0d0)
*         end if
        end if
        if (Npatd .gt. 0) then
         do i = 1,nz+nfor
          bz(i) = Patd(i) * ONEHND
         end do
         call USRENTRY(bz,1,nz+nfor,1,mpkp,1303)
         if (out .eq. 0) then
          CALL genSkip(1147)
          if (Ndec.eq.0)THEN
           call prttbl(thisDate,Mq,bz,nz,'TRADING DAY EFFECT (X 100)',
     &                 2,'td.regression.effects')
          else
           call prttbl(thisDate,Mq,bz,nz,'TRADING DAY EFFECT (X 100)',
     &                 Ndec,'td.regression.effects')
          end if
         end if
*         if ((pg.eq.0) .and. (out.le.2).and. (iter.eq.0)) then
*          fname = 'PATDF.T'
*          subtitle = 'TRADING DAY EFFECT FACTORS'
*          call PLOTSERIES(fname,subtitle,bz,nz,1,888.0d0)
*         end if
        end if
        if (NDS .gt. 0) then
         if (out .eq. 0) then
           CALL genSkip(1148)
           CALL mkTableTag(Nio,'w60','DETERMINISTIC SEASONAL COMPONENT')
           CALL writTag(Nio,'<tr>')
           do i=1,mq
            CALL mkHeaderCellScope(Nio,0,0,'col','@',PeriodH(i,mq))
           end do
           CALL writTag(Nio,'</tr>')
           CALL writTag(Nio,'<tr>')
           do i=1,mq
            write (nio,100)DetSeas(i)*ONEHND
           end do
           CALL writTag(Nio,'</tr>')
           CALL writTag(Nio,'</table>')
         end if
        end if
        if (Npareg .eq. 1) then
         if (Neff(2) .eq. 1) then
          do i = 1,nz+nfor
           tmp(i) = Pareg(i,2) * ONEHND
          end do
          call USRENTRY(tmp,1,nz+nfor,1,mpkp,1304)
          if (out .eq. 0) then
           CALL genSkip(1155)
           if (Ndec.eq.0)THEN
            call prttbl(thisDate,Mq,tmp,nz,
     &                  'SEASONAL REGRESSION EFFECT (X 100)',2,
     &                  'seasonal.regression.effects')
           else
            call prttbl(thisDate,Mq,tmp,nz,
     &                  'SEASONAL REGRESSION EFFECT (X 100)',Ndec,
     &                  'seasonal.regression.effects')
           end if
          end if
*          if ((pg.eq.0) .and. (out.le.2).and. (iter.eq.0)) then
*           fname = 'SREGF.T'
*           subtitle = 'SEASONAL REGRESSION EFFECT FACTORS'
*           call PLOTSERIES(fname,subtitle,tmp,nz,1,888.0d0)
*          end if
         end if
         if (Neff(1) .eq. 1) then
          do i = 1,nz+nfor
           tmp(i) = Pareg(i,1) * ONEHND
          end do
          call USRENTRY(tmp,1,nz+nfor,1,mpkp,1305)
          if (out .eq. 0) then
           CALL genSkip(1150)
           if (Ndec.eq.0)THEN
            call prttbl(thisDate,Mq,bz,nz,
     &                  'TREND-CYCLE REGRESSION EFFECT (X 100)',2,
     &                  'trend.regression.effects')
           else
            call prttbl(thisDate,Mq,bz,nz,
     &                  'TREND-CYCLE REGRESSION EFFECT (X 100)',Ndec,
     &                  'trend.regression.effects')
           end if
          end if
*          if ((pg.eq.0) .and. (out.le.2).and. (iter.eq.0)) then
*           fname = 'TREGF.T'
*           subtitle = 'TREND-CYCLE REGRESSION EFFECT FACTORS'
*           call PLOTSERIES(fname,subtitle,tmp,nz,1,888.0d0)
*          end if
         end if
         if (Neff(7) .eq. 1) then
          do i = 1,nz+nfor
           tmp(i) = Pareg(i,7) * ONEHND
          end do
          call USRENTRY(tmp,1,nz+nfor,1,mpkp,1315)
          if (out .eq. 0) then
           CALL genSkip(1151)
           if (Ndec.eq.0)THEN
            call prttbl(thisDate,Mq,bz,nz,
     &                  'BUSINESS CYCLE REGRESSION EFFECT FACTORS '//
     &                  '(X 100)',2,'bcycle.regression.effects')
           else
            call prttbl(thisDate,Mq,bz,nz,
     &                  'BUSINESS CYCLE REGRESSION EFFECT FACTORS '//
     &                  '(X 100)',Ndec,'bcycle.regression.effects')
           end if
          end if
*          if ((pg.eq.0) .and. (out.lt.2).and.(iter.eq.0)) then
*           fname = 'BCREGF.T'
*           subtitle = 'BUSINESS CYCLE REGRESSION EFFECT FACTORS'
*           call PLOTSERIES(fname,subtitle,tmp,nz,1,888.0d0)
*          end if
         end if
         if (Neff(3) .eq. 1) then
          do i = 1,nz+nfor
           tmp(i) = Pareg(i,3) * ONEHND
          end do
          call USRENTRY(tmp,1,nz+nfor,1,mpkp,1306)
          if ((Tramo .eq. 1).and.(out.eq.0)) then
           CALL genSkip(1152)
           if (Ndec.eq.0)THEN
            call prttbl(thisDate,Mq,tmp,nz,
     &                  'IRREGULAR REGRESSION EFFECT (X 100)',2,
     &                  'irr.regression.effects')
           else
            call prttbl(thisDate,Mq,tmp,nz,
     &                  'IRREGULAR REGRESSION EFFECT (X 100)',Ndec,
     &                  'irr.regression.effects')
           end if
          end if
*          if ((pg.eq.0) .and. (out.lt.2).and. (iter.eq.0)) then
*           fname = 'IREGF.T'
*           subtitle = 'IRREGULAR REGRESSION EFFECT FACTORS'
*           call PLOTSERIES(fname,subtitle,tmp,nz,1,888.0d0)
*          end if
         end if
         if (Neff(4) .eq. 1) then
          do i = 1,nz+nfor
           tmp(i) = Pareg(i,4) * ONEHND
          end do
          call USRENTRY(tmp,1,nz+nfor,1,mpkp,1308)
          if (out .eq. 0) then
           CALL genSkip(1154)
           if (Ndec.eq.0)THEN
            call prttbl(thisDate,Mq,tmp,nz,
     &                  'OTHER REGRESSION EFFECT FACTORS IN '//
     &                  'SEASONALLY ADJUSTED SERIES (X100)',2,
     &                  'other.regression.effects')
           else
            call prttbl(thisDate,Mq,tmp,nz,
     &                  'OTHER REGRESSION EFFECT FACTORS IN '//
     &                  'SEASONALLY ADJUSTED SERIES (X100)',Ndec,
     &                  'other.regression.effects')
           end if
          end if
*          if ((pg.eq.0) .and. (out.le.2).and. (iter.eq.0)) then
*           fname = 'SAREGF.T'
*           subtitle = 'OTHER REG. EFFECT FACTORS IN SA SERIES'
*           call PLOTSERIES(fname,subtitle,tmp,nz,1,888.0d0)
*          end if
         end if
         if (Neff(5) .eq. 1) then
          do i = 1,nz+nfor
           tmp(i) = Pareg(i,5) * ONEHND
          end do
          call USRENTRY(tmp,1,nz+nfor,1,mpkp,1307)
          if (out .eq. 0) then
           CALL genSkip(1153)
           if (Ndec.eq.0)THEN
            call prttbl(thisDate,Mq,tmp,nz,
     &                  'TRANSITORY REGRESSION EFFECT FACTORS (X 100)',
     &                  2,'trans.regression.effects')
           else
            call prttbl(thisDate,Mq,tmp,nz,
     &                  'TRANSITORY REGRESSION EFFECT FACTORS (X 100)',
     &                  Ndec,'trans.regression.effects')
           end if
          end if
*          if ((pg.eq.0) .and. (out.le.2).and. (iter.eq.0)) then
*           fname = 'TRAREGF.T'
*           subtitle = 'TRANSITORY REGRESSION EFFECT FACTORS'
*           call PLOTSERIES(fname,subtitle,tmp,nz,1,0.0d0)
*          end if
         end if
        end if
C      end if
        if (out .eq. 0) then
         CALL genSkip(1009)
         CALL writTagOneLine(Nio,'h3','@','FINAL DECOMPOSITION')
        end if
C
C COMPUTE THE FACTOR FOR THE BIAS=1 CORRECTION
C
        bias1 = ZERO
        bias2 = ZERO
        nyr = (nz/mq) * mq
        do i = 1,nz
         if (i .le. nyr) then
          if (isCloseToTD) then
            bias1=bias1+((sc(i)/ONEHND)*(cycle(i)/ONEHND)*Paeast(i)*
     $           Patd(i)*Pareg(i,2)*Pareg(i,5))
          else
            bias1 = bias1 + (sc(i)/ONEHND*Paeast(i)*Patd(i)*Pareg(i,2))
          end if
         end if
         bias2 = bias2 + (ir(i)/ONEHND*Paouir(i)*Pareg(i,3))
        end do
        bias1 = bias1 / nyr
        bias2 = bias2 / nz
        bias3 = bias1 * bias2
C
C Set Gianluca 16-02-2001 casino non torna piu' il prod delle componenti
C
C Cazzo questo e' da verificare
C
        bias3=ONE
        bias2=ONE
        bias1=ONE
C
C
        call USRENTRY(sa,1,nz,1,mpkp,1309)
        call USRENTRY(trend,1,nz,1,mpkp,1310)
        call USRENTRY(sc,1,nz+lfor,1,mpkp,1311)
        call USRENTRY(ir,1,nz,1,mpkp,1312)
        if (varwnc.gt.1.0D-10 .and.((ncycth.eq.1) .or.(ncyc.gt.1))) then
         call USRENTRY(cycle,1,nz,1,mpkp,1313)
        end if
        if ((Npareg.eq.1) .and. (Neff(0).eq.1)) then
         do i = 1,nz
          bz(i) = Pareg(i,0) * ONEHND
         end do
         if (out .eq. 0) then
          CALL genSkip(1156)
          IF (Ndec.eq.0) THEN
           call prttbl(thisDate,Mq,bz,nz,
     &                 'SEPARATE REGRESSION EFFECT FACTORS (X 100)',
     &                 Ndec,'separate.regression.effects')
          ELSE
           call prttbl(thisDate,Mq,bz,nz,
     &                 'SEPARATE REGRESSION EFFECT FACTORS (X 100)',2,
     &                 'separate.regression.effects')
          END IF
         end if
*         if ((pg .eq. 0).and.(iter.eq.0).and.(out.lt.2)) then
*          fname = 'SPREGF.T'
*          subtitle = 'SEPARATE REGRESSION EFFECT FACTORS'
*          call PLOTSERIES(fname,subtitle,bz,nz,1,888.0d0)
*         end if
        end if
        if (out .eq. 0) then
         CALL genSkip(1010)
         CALL writTagOneLine(Nio,'h3','@','FINAL COMPONENT')
        end if
C
C FINAL SEASONALLY ADJUSTED
C
*       if ((Neast.ne.0).or.(Neff(2).ne.0).or.(Npatd .ne.0).or.
*     $     (Neff(0).ne.0).or.(Nous.ne.0)) then
*         call setCmtSA('Y')
*       end if
        if (npsi.ne.1 .or.  Neff(1).ne.0 .or. 
     $        Neff(3).ne.0 .or. Neff(4).ne.0 .or. Neff(5).ne.0 .or.
     $        Noutr.ne.0 .or. Nouir.ne.0  .or. Nuspad.gt.0) then
         IF(Nuspad.gt.0)THEN
          do i = 1,nz+lfor
           i2 = Frstap + i - 1
           osa(i) =
     $      Tram(i) /
     $     (((sc(i)/ONEHND)*Paeast(i)*Paous(i)*Patd(i)*Pareg(i,2)*
     $         Usrpad(i2)*bias1*Pareg(i,0)))
           if (isCloseToTD) then
             osa(i)=osa(i)/(Pareg(i,5)*cycle(i)/ONEHND)
           end if
           fosa(i) = osa(i)
          end do
         ELSE
          do i = 1,nz+lfor
           osa(i) =
     $      Tram(i) /
     $     (((sc(i)/ONEHND)*Paeast(i)*Paous(i)*Patd(i)*Pareg(i,2)*
     $         bias1*Pareg(i,0)))
           if (isCloseToTD) then
             osa(i)=osa(i)/(Pareg(i,5)*cycle(i)/ONEHND)
           end if
           fosa(i) = osa(i)
          end do
         END IF
         call USRENTRY(osa,1,nz,1,mpkp,1309)
         if (out .eq. 0) then
          CALL genSkip(2136)
          call prttbl(thisDate,Mq,osa,nz,
     &                'FINAL SEASONALLY ADJUSTED SERIES',ndec,
     &                'sa.seats.final')
         end if
*         if (pg.eq.0) then
*          if (iter.ne.0) then
*            if ((ioneout.eq.0).and.(out.lt.2)) then
*             fname = title(1:ntitle) // '.SA'
*             subtitle = 'FINAL SEASONALLY ADJUSTED SERIES'
*             call PLOTSERIES(fname,subtitle,osa,nz,1,0.0d0)
*             write (17,'(A)') fname
*            end if
*          else
*            if (out.lt.3) then
*             fname = 'SAFIN.T'
*             subtitle = 'FINAL SA SERIES'
*             call PLOTSERIES(fname,subtitle,osa,nz,1,0.0d0)
*            end if 
*          end if
*         end if
cc
c Benchmark
cc
         if (((MQ.eq.4) .or. (MQ.eq.12)) .and. (BcMark .eq. 1)) then 
          Lamda = Blamda
          Mid = Bmid
          Rol = Brol
          IF (rol.gt.0.99999D00) THEN
            if (MQ .eq.12) then
             rol = 0.9d0
            else
             rol = 0.729d0 
            end if
          end if
          Iftrgt = Bserie
          if (Bserie .eq. 0) then
           do i=1,nz+lfor
             tmp(i)=Tram(i)
           end do
          else if (Bserie .eq. 1) then
           do i=1,nz+lfor
             tmp(i)=Tram(i) / (Paeast(i) * Patd(i) * Pareg(i,6))
           end do
          else if (Bserie .eq. 2) then
           do i=1,nz+lfor
             tmp(i)=z(i) * Paeast(i) * Patd(i) * Pareg(i,6)
           end do
          else if (Bserie .eq. 3) then
           do i=1,nz+lfor
             tmp(i)=z(i)
           end do
          end if
          Begyrt = 1
          call qmap2(tmp,osa,fosa,1,nz+lfor,mq,0)
          if (out .eq. 0) then
           CALL genSkip(2137)
           call prttbl(thisDate,Mq,fosa,nz,
     &    'FINAL SEASONALLY ADJUSTED SERIES WITH REVISED YEARLY TOTALS',
     &                 ndec,'sa.seats.final.rev')
          end if
          call USRENTRY(fosa,1,nz,1,mpkp,1314)
*          if (pg .eq. 0) then
*           if (iter.ne.0) then
*            if ((ioneout.eq.0) .and. (out.lt.1)) then
*             fname = title(1:ntitle) // '.SAR'
*             subtitle = 'FINAL SA SERIES WITH REVISED YEARLY'
*             call PLOTSERIES(fname,subtitle,fosa,nz,1,0.0d0)
*             write (17,'(A)') fname
*            end if
*           else
*            if (out.eq.0) then
*             fname = 'FSAFIN.T'
*             subtitle = 'FINAL SA SERIES WITH REVISED YEARLY'
*             call PLOTSERIES(fname,subtitle,fosa,nz,1,0.0d0)
*            end if
*           end if
*          end if      
         end if
cc
c
cc
        else
         do i = 1,nz
          osa(i) =Tram(i)
         end do
         call USRENTRY(osa,1,nz,1,mpkp,1309)
        end if
C
C FINAL TREND
C
        if ((Noutr.ne.0).or.(Neff(1).ne.0).or.(Neff(7).ne.0)) then
         call setCmtTc('Y')
        end if
        if (nchi.ne.1 .or. Noutr.ne.0 .or. Neff(1).ne.0 .or. 
     $      Neff(7).ne.0) then
         do i = 1,nz
          ot(i) = trend(i) * Paoutr(i) * Pareg(i,1) *Pareg(i,7)* bias3
         end do
         call USRENTRY(ot,1,nz,1,mpkp,1310)
         if (out.eq.0) then
          CALL genSkip(2138)
          call prttbl(thisDate,Mq,ot,nz,'FINAL TREND-CYCLE',ndec,
     &                'trend.seats.final')
         end if
*         if (pg .eq. 0) then
*          if (iter.ne.0) then
*           if ((ioneout.eq.0) .and. (out.lt.2)) then
*            fname = title(1:ntitle) // '.TRE'
*            subtitle = 'FINAL TREND-CYCLE'
*            call PLOTSERIES(fname,subtitle,ot,nz,1,0.0d0)
*            write (17,'(A)') fname
*           end if
*          else
*           if (out.lt.3) then
*            fname = 'TRFIN.T'
*            subtitle = 'FINAL TREND-CYCLE'
*            call PLOTSERIES(fname,subtitle,ot,nz,1,0.0d0)
*           end if
*          end if
*         end if
        else
         do i = 1,nz
          ot(i) = ONE
         end do
        end if
C
C FINAL SEASONAL
C
        if ((Neast.ne.0).or.(Neff(2).ne.0).or.(Npatd.ne.0) .or. 
     $      (Nous .ne.0).or. (IsCloseToTD.and.neff(5).ne.0)) then
         call setCmtS('Y')
        end if
        if (npsi.ne.1 .or. Neast.ne.0 .or. Neff(2).ne.0 .or. Npatd.ne.0 
     $      .or. Nous.ne.0 .or. isCloseToTD) then
         do i = 1,nz+lfor
          osc(i) = (sc(i)*Paeast(i)*Patd(i)*Paous(i)*Pareg(i,2)) / bias1
         end do
         if (isCloseToTD) then
           osc(i)=osc(i)*cycle(i)*Pareg(i,5)/(ONEHND)
         end if
         call USRENTRY(osc,1,nz+lfor,1,mpkp,1311)
         if (out .eq. 0) then
          CALL genSkip(2139)
          call prttbl(thisDate,Mq,osc,nz,'FINAL SEASONAL',ndec,
     &                'seasonal.seats.final')
         end if
*         if (pg .eq. 0) then
*          if (iter.eq.0) then
*           if (out.lt.3) then
*            fname = 'SFIN.T'
*            subtitle = 'FINAL SEASONAL FACTORS'
*            call PLOTSERIES(fname,subtitle,osc,nz,1,888.0d0)
*           end if
*          else
*           if (out.lt.2) then
*            fname = title(1:ntitle) // '.sf'
*            subtitle = 'FINAL SEASONAL FACTORS'
*            call PLOTSERIES(fname,subtitle,osc,nz,1,888.0d0) 
*            write (17,'(A)') fname
*           end if
*          end if
*         end if
        else
         do i = 1,nz
          osc(i) = ONEHND
         end do
        end if
C
C FINAL CYCLE
C
        do i = 1,nz
         ocyc(i) = cycle(i) * Pareg(i,5)
         if (IsCloseToTD) then
          ocyc(i)=ocyc(i)*Patd(i)
         end if
        end do
        if (Neff(5) .eq. 1) then
         call setCmtTs('Y')
        end if
        if ((varwnc.gt.1.0D-10 .and.((ncycth.eq.1).or.(ncyc.gt.1)))
     $      .or. (Neff(5).eq.1).or. 
     $      (iscloseToTD.and.Npatd.ne.0)) then
          if (isCloseToTD) then
            cad9='FINAL TD FACTORS'
            CALL genSkip(2140)
            call USRENTRY(ocyc,1,nz,1,mpkp,1316)
          else
            cad9='FINAL TRANSITORY FACTORS'
          CALL genSkip(2141)
          end if
          call USRENTRY(ocyc,1,nz,1,mpkp,1313)
*          if (pg .eq. 0) then  
*           if (iter.ne.0) then 
*            if ((ioneout.eq.0) .and. (out.eq.0)) then
*             fname = title(1:ntitle) // '.CYC'
*             call PLOTSERIES(fname,cad9,ocyc,nz,1,0.0d0)
*             write (17,'(A)') fname
*            end if
*           else
*            if (out.lt.3) then
*             fname = 'TRAFIN.T'
*             call PLOTSERIES(fname,cad9,ocyc,nz,1,888.0d0) 
*            end if
*           end if
*          end if        
C            IF (OUT.eq.0) THEN
C               WRITE(NIO,'(//,2X,''FINAL TRANSITORY FACTORS'',/)')
C               CALL TABLE(OCYC)
C             end if
         end if
C
C FINAL IRREGULAR
C
        do i = 1,nz
         oir(i) = ir(i) * Paouir(i) * Pareg(i,3)
        end do
        call USRENTRY(oir,1,nz,1,mpkp,1312)
        if ((Nouir.ne.0) .or. (Neff(3).ne.0)) then
         call setCmtIR('Y')
C             IF (OUT.LT.2) THEN
C               WRITE(NIO,'(//,2X,''FINAL IRREGULAR FACTORS'',/)')
C               CALL TABLE(OIR)
C             end if
*         if (pg .eq. 0) then
*          if (iter.eq.0) then
*           if (out.lt.3) then
*            fname = 'IRFIN.T'
*            subtitle = 'FINAL IRREGULAR FACTORS'
*            call PLOTSERIES(fname,subtitle,oir,nz,1,888.0d0)
*           end if
*          else
*           if (out.lt.2 .and. ioneout.eq.0) then
*            fname = title(1:ntitle) //'.FIR'
*            subtitle = 'FINAL IRREGULAR FACTORS'
*            call PLOTSERIES(fname,subtitle,oir,nz,1,888.0d0)
*            write (17,'(A)') fname
*           end if
*          end if  
*         end if
        end if
        call SETCMTSA(GETCMTS())
        call SETCMTSA(GETCMTTC())
        call SETCMTSA(GETCMTTS())
        call SETCMTSA(GETCMTIR())
        if (NEFF(4).ne.0) then
         call SETCMTSA('Y')
        end if
C
        if ((out.eq.0).and.
     $ ((varwnc.gt.1.0D-10 .and.((ncycth.eq.1).or.(ncyc.gt.1))).or.
     $      (Neff(5).eq.1) .or.(Nouir.ne.0) .or. (Neff(3).ne.0).or.
     $       (isCloseToTD.and.NpaTD.ne.0))) then
         do i = 1,nz
          tmp(i) = (ocyc(i)*oir(i)) / ONEHND
         end do
         CALL genSkip(2142)
         call prttbl(thisDate,Mq,tmp,nz,'FINAL TRANSITORY-IRREGULAR',
     $               ndec,'transirr.seats.final')
        else if(out.eq.0)THEN
         CALL writTagOneLine(Nio,'h3','@','FINAL IRREGULAR COMPONENT')
         CALL mkPOneLine(Nio,'@',
     $                   'The same as the stochastic irregular.')
        end if
C
C
C
        nf = MAX(lfor,MAX(8,2*mq))
        if (Nsfcast .eq. 0) then
         do i = (-nf),nf
          ftr(i) = trend(nz+i) * Paoutr(nz+i) * Pareg(nz+i,1) 
     $              *Pareg(nz+i,7)* bias3
         end do
         do i = (-nf),nf
          if (i .le. 0) then
           fir(i) = Paouir(nz+i) * Pareg(nz+i,3) * ir(nz+i)
          else
           fir(i) = Paouir(nz+i) * Pareg(nz+i,3)
          end if
         end do
         do i = (-nf),nf
          fcyc(i) = cycle(nz+i) * Pareg(nz+i,5)
          if (isCloseToTD) then
            fcyc(i)=fcyc(i)*PaTD(nz+i)
          end if
         end do
         do i = (-nf),nf
          fsa(i) =
     $      Tram(nz+i) /
     $      ((sc(nz+i)/ONEHND*Paeast(nz+i)*Patd(nz+i)*Pareg(nz+i,2)*
     $       Paous(nz+i))/bias1*Pareg(nz+i,0))
          if (iscloseTotD) then
            fsa(i)=fsa(i)/(Pareg(nz+i,5)*cycle(nz+i)/ONEHND)
          end if
         end do
         if (fortr .eq. 1) then
          do i = 1,nf
           if (isCloseTotD) then
             ftr(i) = (fsa(i)/fir(i))
           else
             ftr(i) = (fsa(i)/fir(i)) / (fcyc(i)/ONEHND)
           end if
          end do
         end if
         do i = (-nf),nf
          fs(i) =
     $      sc(nz+i) * Paeast(nz+i) * Patd(nz+i) * Paous(nz+i) * 
     $      Pareg(nz+i,2) / bias1
          if (isCloseToTD) then
            fs(i)=fs(i)*PaReg(nz+i,5)*cycle(nz+i)/ONEHND
          end if
         end do
         do i = (-nf),nf
          freg(i) = Pareg(nz+i,0)
          fo(i) = Tram(nz+i)
         end do
        else
         do i = (-nf),nf
          if (i .gt. 0) then
           ftr(i) =
     $       EXP(LOG(trend(nz+i))*Rfact(i)) * Paoutr(nz+i) *
     $       Pareg(nz+i,1) * bias3
          else
           ftr(i) = trend(nz+i) * Paoutr(nz+i) * Pareg(nz+i,1) * bias3
          end if
         end do
         do i = (-nf),nf
          fir(i) = Paouir(nz+i) * Pareg(nz+i,3) * ir(nz+i)
         end do
         do i = (-nf),nf
          fcyc(i) = cycle(nz+i) * Pareg(nz+i,5)
         end do
         do i = (-nf),nf
          fsa(i) =
     $      Tram(nz+i) /
     $      ((sc(nz+i)/ONEHND*Paeast(nz+i)*Patd(nz+i)*Pareg(nz+i,2)*
     $        Paous(nz+i))/bias1*Pareg(nz+i,0))
         end do
         if (fortr .eq. 1) then
          do i = 1,nf
           ftr(i) = (fsa(i)/fir(i)) / (fcyc(i)/ONEHND)
          end do
         end if
         do i = (-nf),nf
          fs(i) =
     $      sc(nz+i) * Paeast(nz+i) * Patd(nz+i) * Paous(nz+i) *
     $      Pareg(nz+i,2) / bias1
         end do
         do i = (-nf),nf
          freg(i) = Pareg(nz+i,0) * ONEHND
          fo(i) = Tram(nz+i)
         end do
        end if
        do i = 1,nf
         fir(i) = fir(i) * ONEHND
        end do
        if (nreestimated .eq. 1 .and. tramo.eq.0) then
*         if (out.eq.0) then
*          CALL nWritln('SINCE SEATS HAS RE-ESTIMATED AND CHANGED '//
*     $          'THE MODEL, THE FORECAST OF THE ORIGINAL '//
*     $          '(UNCORRECTED) SERIES WILL DIFFER FROM THAT IN '//
*     $          'regARIMA output.',Nio,0,T,T)
*         end if
         call USRENTRY(Tram,1,nz+nf,1,mpkp,213)
        end if
C         call USRENTRY(Tram,nz+1,nz+nf,1409)
C         call USRENTRY(trend,nz+1,nz+nf,1410)
C         call USRENTRY(sc,nz+1,nz+nf,1411)
C         call USRENTRY(ir,nz+1,nz+nf,1412)
        do i=1,nf
         tmp(i)=fsa(i)
        end do
        call USRENTRY(tmp,1,nf,1,mpkp,1409)
        do i=1,nf
         tmp(i)=ftr(i)
        end do
        call USRENTRY(tmp,1,nf,1,mpkp,1410)
        do i=1,nf
         tmp(i)=fs(i)
        end do
        call USRENTRY(tmp,1,nf,1,mpkp,1411)
        do i=1,nf
         tmp(i)=fir(i)
        end do
        call USRENTRY(tmp,1,nf,1,mpkp,1412)
        if ((ncycth.eq.1) .or. (ncyc.gt.1)) then
         do i=1,nf
          tmp(i)=fcyc(i)
         end do
         call USRENTRY(tmp,1,nf,1,mpkp,1413)
        end if
        if (itable .eq. 1) then
         do i = 1,nz+nfor
          ceff(i) = Paeast(i) * Patd(i) * Pareg(i,6)
         end do
         if (ITER .gt. 2) then
          call ProcTables(tabtables)
         end if
         call OUTTABLE2(titleg,Tram,ot,osa,osc,oir,ocyc,pread,ceff,
     $                 eresid,numEresid,hptmp,hptrtmp,hpcycle,lamd,1,
     $                 nz,mq,2,kunits,nf,trend,sa,fosa,IsCloseToTD)
         call OUTTABFOR(ftr,fsa,fs,fir,fcyc,pread,ceff,hptmp,
     $                  hptrtmp,hpcycle,lamd,1,nf,nz,mq,trend,sa,fosa)
*         CALL writTag(36,'</table>')
         CALL mkPOneLine(36,'@','&nbsp;')
        end if
C
C TABLES WITH THE SE OF FINAL COMPONENTS
C
        if (out .eq. 0) then
C Modified by REG, on 28 Feb 2006, to add out to FINALSE parameter list.
         call FINALSE(psiep,psiea,trend,sa,siepf,siepfl,sieaf,sieafl,
     $                sqf,ilen,mq,lfor,lamd,out,ndec)
C
         CALL genSkip(1011)
         CALL writTagOneLine(Nio,'h4','@','FORECAST OF FINAL COMPONENT')
         call HTMLFORTBL(fo,freg,ftr,fsa,fs,fcyc,fir,Tse,siepf,siepfl,
     $               sieaf,sieafl,Neff,mq,Nouir,Noutr,Npatd,Neast,nchi,
     $        npsi,ncyc,ncycth,lamd,nper,nyer,nz,nf,isCloseToTD,varwnc)
         if (Nsfcast .ne. 0) then
          CALL mkPOneLine(Nio,'bold',
     $                    'THE FORECAST OF THE IRREGULAR ABSORBS '//
     $                    'THE EFFECT OF THE APPROXIMATION.')
         end if
        end if
        do i=1,nf
         tmp(i)=fsa(i)
        end do
        call USRENTRY(tmp,1,nf,1,mpkp,1409)
        do i=1,nf
         tmp(i)=ftr(i)
        end do
        call USRENTRY(tmp,1,nf,1,mpkp,1410)
        do i=1,nf
         tmp(i)=fs(i)
        end do
        call USRENTRY(tmp,1,nf,1,mpkp,1411)
        do i=1,nf
         tmp(i)=fir(i)
        end do
        call USRENTRY(tmp,1,nf,1,mpkp,1412)
        if (varwnc.gt.1.0D-10 .and.((ncycth.eq.1).or.(ncyc.gt.1))) then
         do i=1,nf
          tmp(i)=fcyc(i)
         end do
         call USRENTRY(tmp,1,nf,1,mpkp,1413)
        end if
*        if ((pg .eq. 0).and.(iter.eq.0)) then
*         if (out.lt.2) then
*           if (Npareg .eq. 1) then
*            fname = 'FREG.T5'
*            subtitle = 'FORECAST REGRESSION EFFECT'
*            call PLOTFCAST1(fname,subtitle,freg,nf,nz,0)
*           end if
*           if (Neff(0) .eq. 1) then
*            fname = 'SPREG.T5'
*            subtitle = 'FORECAST SEPARATE REG. EFFECT'
*            do i = (-nf),nf
*             ftmp(i) = Pareg(nz+i,0)
*            end do
*            call PLOTFCAST1(fname,subtitle,ftmp,nf,nz,0)
*           end if
*           if ((Neff(5).eq.1) .or.
*     $      (varwnc.gt.1.0D-10.and.((ncycth.eq.1).or.(ncyc.gt.1)))) then
*            fname = 'FTRAFIN.T5'
*            if (isCloseToTD) then
*             subtitle = 'FORECAST FINAL TD COMPONENT'
*            else
*             subtitle = 'FORECAST FINAL TRANSITORY COMPONENT'
*            end if
*            call PLOTFCAST1(fname,subtitle,fcyc,nf,nz,0)
*           end if
*           if ((Neff(3).eq.1) .or. (Nouir.eq.1)) then
*            fname = 'FIRFIN.T5'
*            subtitle = 'FORECAST FINAL IRREGULAR'
*            call PLOTFCAST1(fname,subtitle,fir,nf,nz,0)
*           end if
*         end if
*         if (out.lt.3) then
*           fname = 'FUNORIG.T5'
*           subtitle = 'FORECAST OF SERIES'
*           call PLOTFCAST1(fname,subtitle,fo,nf,nz,0)
*           if (npsi.ne.1 .or. (Neast+Neff(2)+Npatd).ne.0) then
*            fname = 'FSAFIN.T5'
*            subtitle = 'FORECAST FINAL SA SERIES'
*            call PLOTFCAST1(fname,subtitle,fsa,nf,nz,0)
*            fname = 'FSFIN.T5'
*            subtitle = 'FORECAST FINAL SEASONAL FACTORS'
*            call PLOTFCAST1(fname,subtitle,fs,nf,nz,0)
*           end if
*           if (nchi.ne.1 .or. Noutr.ne.0 .or. Neff(1).ne.0 .or.
*     $       Neff(7).ne.0) then
*            fname = 'FTRFIN.T5'
*            subtitle = 'FORECAST FINAL TREND-CYCLE'
*            call PLOTFCAST1(fname,subtitle,ftr,nf,nz,0)
*           end if
*         end if
*        end if
       end if
C
C end if LAMDA=0
C
C
C HERE INTRODUCE THE CHECK ON THE AGGREGATE
C
*       if (out .ne. 2) then
*        if (lamd .eq. 1) then
*         aavsxt = 0.0d0
*         aavfxt = 0.0d0
*         aavsat = 0.0d0
*         aavfat = 0.0d0
*         aavfst = 0.0d0
*         musxt = 0.0d0
*         mufxt = 0.0d0
*         musat = 0.0d0
*         mufat = 0.0d0
*         mufst = 0.0d0
*         maxsxt = 1
*         maxsat = 1
*         maxfxt = 1
*         maxfat = 1
*         maxfst = 1
*         do i = 1,nz+lfor
*          sxt(i) = z(i) - sa(i) - sc(i)
*          sat(i) = sa(i) - trend(i) - cycle(i) - ir(i)
*          if ((sxt(i)-sxt(maxsxt)) .gt. 1.0d-8) then
*           maxsxt = i
*          end if
*          if ((sat(i)-sat(maxsat)) .gt. 1.0d-8) then
*           maxsat = i
*          end if
*          aavsxt = aavsxt + ABS(sxt(i))
*          aavsat = aavsat + ABS(sat(i))
*          musxt = musxt + sxt(i)
*          musat = musat + sat(i)
*         end do
*         do i = 1,nz
*          fxt(i) = Tram(i) - osa(i) - osc(i) - Pareg(i,0)
*          fat(i) = osa(i) - ot(i) - ocyc(i) - oir(i) - Pareg(i,4)
*          fst(i) = osc(i) - sc(i) - Patd(i) - Paeast(i) - Pareg(i,2) - 
*     $             Paous(i)
*          if ((fxt(i)-fxt(maxfxt)) .gt. 1.0d-8) then
*           maxfxt = i
*          end if
*          if ((fat(i)-fat(maxfat)) .gt. 1.0d-8) then
*           maxfat = i
*          end if
*          if ((fst(i)-fst(maxfst)) .gt. 1.0d-8) then
*           maxfst = i
*          end if
*          aavfxt = aavfxt + ABS(fxt(i))
*          aavfat = aavfat + ABS(fat(i))
*          aavfst = aavfst + ABS(fst(i))
*          mufxt = mufxt + fxt(i)
*          mufat = mufat + fat(i)
*          mufst = mufst + fst(i)
*         end do
*         do i = 1,lfor
*          fxt(nz+i) = Tram(nz+i) - fsa(i) - fs(i) - Pareg(nz+i,0)
*          fat(nz+i) = fsa(i) - ftr(i) - fcyc(i) - fir(i) - Pareg(nz+i,4)
*          fst(nz+i) =
*     $      fs(i) - sc(nz+i) - Patd(nz+i) - Paeast(nz+i) - Pareg(nz+i,2)
*     $      - Paous(nz+i)
*          if ((fxt(nz+i)-fxt(maxfxt)) .gt. 1.0d-8) then
*           maxfxt = nz + i
*          end if
*          if ((fat(nz+i)-fat(maxfat)) .gt. 1.0d-8) then
*           maxfat = nz + i
*          end if
*          if ((fst(nz+i)-fst(maxfst)) .gt. 1.0d-8) then
*           maxfst = nz + i
*          end if
*          aavfxt = aavfxt + ABS(fxt(nz+i))
*          aavfat = aavfat + ABS(fat(nz+i))
*          aavfst = aavfst + ABS(fst(nz+i))
*          mufxt = mufxt + fxt(nz+i)
*          mufat = mufat + fat(nz+i)
*          mufst = mufst + fst(nz+i)
*         end do
*         aavsxt = aavsxt / DBLE(nz+lfor)
*         aavfxt = aavfxt / DBLE(nz+lfor)
*         aavsat = aavsat / DBLE(nz+lfor)
*         aavfat = aavfat / DBLE(nz+lfor)
*         aavfst = aavfst / DBLE(nz+lfor)
*         musxt = musxt / DBLE(nz+lfor)
*         mufxt = mufxt / DBLE(nz+lfor)
*         musat = musat / DBLE(nz+lfor)
*         mufat = mufat / DBLE(nz+lfor)
*         mufst = mufst / DBLE(nz+lfor)
*         aggsxt = sxt(maxsxt)
*         aggfxt = fxt(maxfxt)
*         aggsat = sat(maxsat)
*         aggfat = fat(maxfat)
*         aggfst = fst(maxfst)
*         sumsxt = sa(maxsxt) + sc(maxsxt)
*         sumsat = trend(maxsat) + cycle(maxsat) + ir(maxsat)
*         if (maxfxt .le. nz) then
*          sumfxt = osa(maxfxt) + osc(maxfxt) + Pareg(maxfxt,0)
*         else
*          sumfxt = fsa(maxfxt-nz) + fs(maxfxt-nz) + Pareg(maxfxt,0)
*         end if
*         if (maxfat .le. nz) then
*          sumfat = ot(maxfat) + ocyc(maxfat) + oir(maxfat) +
*     $             Pareg(maxfat,4)
*         else
*          sumfat = ftr(maxfat-nz) + fcyc(maxfat-nz) + fir(maxfat-nz) +
*     $             Pareg(maxfat,4)
*         end if
*         if (maxfst .le. nz) then
*          sumfst = sc(maxfst) + Patd(maxfst) + Paeast(maxfst) +
*     $             Pareg(maxfst,2) + Paous(maxfst)
*         else
*          sumfst = fs(maxfst-nz) + Patd(maxfst) + Paeast(maxfst) +
*     $             Pareg(maxfst,2) + Paous(maxfst)
*         end if
*         IF(ABS(Tram(maxsxt)).le.SMALL)THEN
*          pplevsxt = (ABS(aggsxt)/ABS(Tram(maxsxt))) * 100.0d0
*         ELSE
*          pplevsxt = ZERO
*         END IF
*         IF(ABS(Tram(maxsat)).le.SMALL)THEN
*          pplevsat = (ABS(aggsat)/ABS(Tram(maxsat))) * 100.0d0
*         ELSE
*          pplevsat = ZERO
*         END IF
*         IF(ABS(Tram(maxfxt)).le.SMALL)THEN
*          pplevfxt = (ABS(aggfxt)/ABS(Tram(maxfxt))) * 100.0d0
*         ELSE
*          pplevfxt = ZERO
*         END IF
*         IF(ABS(Tram(maxfat)).le.SMALL)THEN
*          pplevfat = (ABS(aggfat)/ABS(Tram(maxfat))) * 100.0d0
*         ELSE
*          pplevfat = ZERO
*         END IF
*         IF(ABS(Tram(maxfst)).le.SMALL)THEN
*          pplevfst = (ABS(aggfst)/ABS(Tram(maxfst))) * 100.0d0
*         ELSE
*          pplevfst = ZERO
*         END IF
*         if (HTML .eq. 1) then
*          write (Nio,'(''<br><br><u><b>DIFFERENCE BETWEEN AGGREGATE'',
*     $               '' AND AGGREGATE OF COMPONENTS</b></u>'')')
*          write (nio,'(''<TABLE BORDER="0" CELLPADDING="6" '',
*     &                 ''CELLSPACING="0" ALIGN="JUSTIFY">'')')
*          write (Nio,'(''<tr><th></th><th align=right>SX</th>'',
*     $                 ''<th align=right>SA</th>'',
*     $                 ''<th align=right>FX</th>'',
*     $                 ''<th align=right>FA</th>'',
*     $                 ''<th align=right>FS</th></tr>'')')
*          write (Nio,'(''<tr><th align=right>MEAN</th>'',
*     $                 5(''<td align=right>'',G12.4,''</td>''),
*     $                 ''</tr>'')')
*     $          musxt, musat, mufxt, mufat, mufst
*          write (Nio,'(''<tr><th align=right>AAV</th>'',
*     $                 5(''<td align=right>'',G12.4,''</td>''),
*     $                 ''</tr>'')')
*     $          aavsxt, aavsat, aavfxt, aavfat, aavfst
*          write (Nio,'(''<tr><th align=right>MAX DIFF.</th>'',
*     $                 5(''<td align=right>'',G12.4,''</td>''),
*     $                 ''</tr>'')')
*     $          sxt(maxsxt), sat(maxsat), fxt(maxfxt), fat(maxfat),
*     $          fst(maxfst)
*          write (Nio,'(''<tr><th align=right>MAX DIFF. '',
*     $                 ''AS % OF LEVEL</th>'',
*     $                 5(''<td align=right>'',G12.4,''</td>''),
*     $                 ''</tr>'')')
*     $          pplevsxt, pplevsat, pplevfxt, pplevfat, pplevfst
*          write (Nio,'(''<tr><th align=right>PERIOD</th>'',
*     $                 5(''<td align=right>'',G12.4,''</td>''),
*     $                 ''</tr>'')')
*     $          maxsxt, maxsat, maxfxt, maxfat, maxfst
*          write (Nio,'(''<tr><th align=right>AGGREGATE</th>'',
*     $                 5(''<td align=right>'',G12.4,''</td>''),
*     $                 ''</tr>'')')
*     $          aggsxt, aggsat, aggfxt, aggfat, aggfst
*          write (Nio,'(''<tr><th align=right>THROUGH COMP.</th>'',
*     $                 5(''<td align=right>'',G12.4,''</td>''),
*     $                 ''</tr>'')')
*     $          sumsxt, sumsat, sumfxt, sumfat, sumfst
*          write (Nio,'("</table><br>")')
*         else
*          write (Nio,'(//,12x,''DIFFERENCE BETWEEN AGGREGATE'',/,12x,
*     $              ''AND AGGREGATE OF COMPONENTS'',//)')
*          write (Nio,'(34x,''SX'',16x,''SA'',16x,''FX'',16x,
*     $     ''FA'',16x,''FS'')')
*          write (Nio,'(4X,''MEAN'',20X,5(G12.4,6X),/)')
*     $         musxt, musat, mufxt, mufat, mufst
*          write (Nio,'(4X,''AAV'',21X,5(G12.4,6X),/)')
*     $         aavsxt, aavsat, aavfxt, aavfat, aavfst
*          write (Nio,'(4X,''MAX DIFF.'',15X,5(G12.4,6X),/)')
*     $         sxt(maxsxt), sat(maxsat), fxt(maxfxt), fat(maxfat),
*     $         fst(maxfst)
*          write (Nio,'(4X,''MAX DIFF. AS % '')')
*          write (Nio,'(4X,''OF LEVEL'',16X,5(G12.4,6X),/)')
*     $         pplevsxt, pplevsat, pplevfxt, pplevfat, pplevfst
*          write (Nio,'(4X,''PERIOD'',15X,5(I12,6X),/)')
*     $         maxsxt, maxsat, maxfxt, maxfat, maxfst
*          write (Nio,'(4X,''AGGREGATE'',15X,5(G12.4,6X),/)')
*     $         aggsxt, aggsat, aggfxt, aggfat, aggfst
*          write (Nio,'(4X,''THROUGH COMP.'',11X,5(G12.4,6X),/)')
*     $         sumsxt, sumsat, sumfxt, sumfat, sumfst
*         end if
*        else
*         aavsxt = 0.0d0
*         aavfxt = 0.0d0
*         aavsat = 0.0d0
*         aavfat = 0.0d0
*         aavfst = 0.0d0
*         musxt = 0.0d0
*         mufxt = 0.0d0
*         musat = 0.0d0
*         mufat = 0.0d0
*         mufst = 0.0d0
*         maxsxt = 1
*         maxsat = 1
*         maxfxt = 1
*         maxfat = 1
*         maxfst = 1
**         OPEN(66,file='z.txt',STATUS='UNKNOWN')
*         OPEN(66,file=Cursrs(1:Nfilcr)//'.tbz',STATUS='UNKNOWN')
*         do i = 1,nz
*          write(66,*)z(i),sa(i),sc(i),trend(i),cycle(i),ir(i)
*          sxt(i) = EXP(z(i)) / (sa(i)*(sc(i)/100.0d0))
*          sat(i) = sa(i) / (trend(i)*(cycle(i)/100.0d0)*(ir(i)/100.0d0))
*          if (ABS(sxt(i)-sxt(maxsxt)) .gt. 1.0d-8) then
*           maxsxt = i
*          end if
*          if (ABS(sat(i)-sat(maxsat)) .gt. 1.0d-8) then
*           maxsat = i
*          end if
*          aavsxt = aavsxt + ABS(sxt(i))
*          aavsat = aavsat + ABS(sat(i))
*          musxt = musxt + sxt(i)
*          musat = musat + sat(i)
*         end do
*         do i = nz+1,nz+lfor
*          write(66,*)z(i),sa(i),sc(i),trend(i),cycle(i),ir(i)
*          sxt(i) = EXP(z(i)) / (sa(i)*(sc(i)/100.0d0))
*          sat(i) = sa(i) / (trend(i)*(cycle(i)/100.0d0))
*          if ((sxt(i)-sxt(maxsxt)) .gt. 1.0d-8) then
*           maxsxt = i
*          end if
*          if ((sat(i)-sxt(maxsat)) .gt. 1.0d-8) then
*           maxsat = i
*          end if
*          aavsxt = aavsxt + ABS(sxt(i))
*          aavsat = aavsat + ABS(sat(i))
*          musxt = musxt + sxt(i)
*          musat = musat + sat(i)
*         end do
*         close(66)
*         OPEN(66,file=Cursrs(1:Nfilcr)//'.tbo',STATUS='UNKNOWN')
*         do i = 1,nz
*          write(66,*)Tram(i),osa(i),osc(i),ot(i),ocyc(i),oir(i)
*          fxt(i) = Tram(i) / (osa(i)*(osc(i)/100.0d0)*Pareg(i,0))
*          fat(i) =
*     $      osa(i) /
*     $      (ot(i)*(ocyc(i)/100.0d0)*(oir(i)/100.0d0)*Pareg(i,4))
*          fst(i) =
*     $      (osc(i)/((sc(i)/100.0d0)*Patd(i)*Paeast(i)*Paous(i)*
*     $       Pareg(i,2))) / 100.0d0
*          if ((fxt(i)-fxt(maxsxt)) .gt. 1.0d-8) then
*           maxfxt = i
*          end if
*          if ((fat(i)-fat(maxfat)) .gt. 1.0d-8) then
*           maxfat = i
*          end if
*          if ((fst(i)-fst(maxfst)) .gt. 1.0d-8) then
*           maxfst = i
*          end if
*          aavfxt = aavfxt + ABS(fxt(i))
*          aavfat = aavfat + ABS(fat(i))
*          aavfst = aavfst + ABS(fst(i))
*          mufxt = mufxt + fxt(i)
*          mufat = mufat + fat(i)
*          mufst = mufst + fst(i)
*         end do
*         close(66)
*         OPEN(66,file=Cursrs(1:Nfilcr)//'.tbf',STATUS='UNKNOWN')
*         do i = 1,lfor
*          write(66,*)Tram(nz+i),fsa(i),fs(i),ftr(i),fcyc(i),fir(i)
*          fxt(nz+i) =
*     $      Tram(nz+i) / (fsa(i)*(fs(i)/100.0d0)*Pareg(nz+i,0))
*          fat(nz+i) =
*     $      fsa(i) /
*     $      (ftr(i)*(fcyc(i)/100.0d0)*(fir(i)/100.0d0)*Pareg(nz+i,4))
*          fst(nz+i) =
*     $      (fs(i)/
*     $       ((sc(nz+i)/100.0d0)*Patd(nz+i)*Paeast(nz+i)*Pareg(nz+i,2)*
*     $         Paous(nz+i))) / 100.0d0
*          if ((fxt(i)-fxt(maxsxt)) .gt. 1.0d-8) then
*           maxfxt = nz + i
*          end if
*          if ((fat(i)-fat(maxfat)) .gt. 1.0d-8) then
*           maxfat = nz + i
*          end if
*          if ((fst(i)-fst(maxfst)) .gt. 1.0d-8) then
*           maxfst = nz + i
*          end if
*          aavfxt = aavfxt + ABS(fxt(nz+i))
*          aavfat = aavfat + ABS(fat(nz+i))
*          aavfst = aavfst + ABS(fst(nz+i))
*          mufxt = mufxt + fxt(nz+i)
*          mufat = mufat + fat(nz+i)
*          mufst = mufst + fst(nz+i)
*         end do
*         close(66)
*         aavsxt = aavsxt / DBLE(nz+lfor)
*         aavfxt = aavfxt / DBLE(nz+lfor)
*         aavsat = aavsat / DBLE(nz+lfor)
*         aavfat = aavfat / DBLE(nz+lfor)
*         aavfst = aavfst / DBLE(nz+lfor)
*         musxt = musxt / DBLE(nz+lfor)
*         mufxt = mufxt / DBLE(nz+lfor)
*         musat = musat / DBLE(nz+lfor)
*         mufat = mufat / DBLE(nz+lfor)
*         mufst = mufst / DBLE(nz+lfor)
*         aggsxt = sxt(maxsxt)
*         aggfxt = fxt(maxfxt)
*         aggsat = sat(maxsat)
*         aggfat = fat(maxfat)
*         aggfst = fst(maxfst)
*         sumsxt = sa(maxsxt) * (sc(maxsxt)/100.0d0)
*         sumsat = trend(maxsat) * (cycle(maxsat)/100.0d0) *
*     $            (ir(maxsat)/100.0d0)
*         if (maxfxt .le. nz) then
*          sumfxt = osa(maxfxt) * (osc(maxfxt)/100.0d0) * Pareg(maxfxt,0)
*         else
*          sumfxt = fsa(maxfxt-nz) * (fs(maxfxt-nz)/100.0d0) *
*     $             Pareg(maxfxt,0)
*         end if
*         if (maxfat .le. nz) then
*          sumfat = ot(maxfat) * (ocyc(maxfat)/100.0d0) *
*     $             (oir(maxfat)/100.0d0) * Pareg(maxfat,4)
*         else
*          sumfat = ftr(maxfat-nz) * (fcyc(maxfat-nz)/100.0d0) *
*     $             (fir(maxfat-nz)/100.0d0) * Pareg(maxfat,4)
*         end if
*         if (maxfst .le. nz) then
*          sumfst = (sc(maxfst)/100.0d0) * Patd(maxfst) * Paeast(maxfst)
*     $             * Pareg(maxfst,2) * Paous(maxfst)
*         else
*          sumfst = (fs(maxfst-nz)/100.0d0) * Patd(maxfst) * 
*     $              Paeast(maxfst) * Pareg(maxfst,2) * 
*     $              Paous(maxfst)
*         end if
*C   LINES OF CODE COMMENTED FOR X-13A-S : 5
*C         pplevsxt = (ABS(aggsxt)/ABS(Tram(maxsxt))) * 100.0d0
*C         pplevsat = (ABS(aggsat)/ABS(Tram(maxsat))) * 100.0d0
*C         pplevfxt = (ABS(aggfxt)/ABS(Tram(maxfxt))) * 100.0d0
*C         pplevfat = (ABS(aggfat)/ABS(Tram(maxfat))) * 100.0d0
*C         pplevfst = (ABS(aggfst)/ABS(Tram(maxfst))) * 100.0d0
*C   END OF CODE BLOCK 
*C   LINES OF CODE ADDED FOR X-13A-S : 25
*         IF(ABS(Tram(maxsxt)).le.SMALL)THEN
*          pplevsxt = (ABS(aggsxt)/ABS(Tram(maxsxt))) * 100.0d0
*         ELSE
*          pplevsxt = ZERO
*         END IF
*         IF(ABS(Tram(maxsat)).le.SMALL)THEN
*          pplevsat = (ABS(aggsat)/ABS(Tram(maxsat))) * 100.0d0
*         ELSE
*          pplevsat = ZERO
*         END IF
*         IF(ABS(Tram(maxfxt)).le.SMALL)THEN
*          pplevfxt = (ABS(aggfxt)/ABS(Tram(maxfxt))) * 100.0d0
*         ELSE
*          pplevfxt = ZERO
*         END IF
*         IF(ABS(Tram(maxfat)).le.SMALL)THEN
*          pplevfat = (ABS(aggfat)/ABS(Tram(maxfat))) * 100.0d0
*         ELSE
*          pplevfat = ZERO
*         END IF
*         IF(ABS(Tram(maxfst)).le.SMALL)THEN
*          pplevfst = (ABS(aggfst)/ABS(Tram(maxfst))) * 100.0d0
*         ELSE
*          pplevfst = ZERO
*         END IF
*C   END OF CODE BLOCK
*         if (HTML .eq. 1) then
*          write (Nio,'(''<br><br><u><b>DIFFERENCE BETWEEN AGGREGATE'',
*     $                 '' AND AGGREGATE OF COMPONENTS</b></u>'')')
*          write (nio,'(''<TABLE BORDER="0" CELLPADDING="6" '',
*     &           ''CELLSPACING="0" ALIGN="JUSTIFY">'')')
*          write (Nio,'(''<tr><th></th><th align=right>SX</th>'',
*     $                 ''<th align=right>SA</th>'',
*     $                 ''<th align=right>FX</th>'',
*     $                 ''<th align=right>FA</th>'',
*     $                 ''<th align=right>FS</th></tr>'')')
*          write (Nio,'(''<tr><th align=right>MEAN</th>'',
*     $                 5(''<td align=right>'',G12.4,''</td>''),
*     $                 ''</tr>'')')
*     $          musxt, musat, mufxt, mufat, mufst
*          write (Nio,'(''<tr><th align=right>AAV</th>'',
*     $                 5(''<td align=right>'',G12.4,''</td>''),
*     $                 ''</tr>'')')
*     $          aavsxt, aavsat, aavfxt, aavfat, aavfst
*          write (Nio,'(''<tr><th align=right>MAX DIFF.</th>'',
*     $                 5(''<td align=right>'',G12.4,''</td>''),
*     $                 ''</tr>'')')
*     $          sxt(maxsxt), sat(maxsat), fxt(maxfxt), fat(maxfat),
*     $          fst(maxfst)
*          write (Nio,'(''<tr><th align=right>MAX DIFF. '',
*     $                 ''AS % OF LEVEL</th>'',
*     $                 5(''<td align=right>'',G12.4,''</td>''),
*     $                 ''</tr>'')')
*     $          pplevsxt, pplevsat, pplevfxt, pplevfat, pplevfst
*          write (Nio,'(''<tr><th align=right>PERIOD</th>'',
*     $                 5(''<td align=right>'',G12.4,''</td>''),
*     $                 ''</tr>'')')
*     $          maxsxt, maxsat, maxfxt, maxfat, maxfst
*          write (Nio,'(''<tr><th align=right>AGGREGATE</th>'',
*     $                 5(''<td align=right>'',G12.4,''</td>''),
*     $                 ''</tr>'')')
*     $          aggsxt, aggsat, aggfxt, aggfat, aggfst
*          write (Nio,'(''<tr><th align=right>THROUGH COMP.</th>'',
*     $                 5(''<td align=right>'',G12.4,''</td>''),
*     $                 ''</tr>'')')
*     $          sumsxt, sumsat, sumfxt, sumfat, sumfst
*          write (Nio,'("</table><br>")')
*         else
*          write (Nio,'(//,12x,''DIFFERENCE BETWEEN AGGREGATE'',/,12x,
*     $              ''AND AGGREGATE OF COMPONENTS'',//)')
*          write (Nio,'(34x,''SX'',16x,''SA'',16x,''FX'',16x,
*     $     ''FA'',16x,''FS'')')
*          write (Nio,'(4X,''MEAN'',20X,5(G12.4,6X),/)')
*     $         musxt, musat, mufxt, mufat, mufst
*          write (Nio,'(4X,''AAV'',21X,5(G12.4,6X),/)')
*     $         aavsxt, aavsat, aavfxt, aavfat, aavfst
*          write (Nio,'(4X,''MAX DIFF.'',15X,5(G12.4,6X),/)')
*     $         sxt(maxsxt), sat(maxsat), fxt(maxfxt), fat(maxfat),
*     $         fst(maxfst)
*          write (Nio,'(4X,''MAX DIFF. AS % '')')
*          write (Nio,'(4X,''OF LEVEL'',16X,5(G12.4,6X),/)')
*     $         pplevsxt, pplevsat, pplevfxt, pplevfat, pplevfst
*          write (Nio,'(4X,''PERIOD'',15X,5(I12,6X),/)')
*     $         maxsxt, maxsat, maxfxt, maxfat, maxfst
*          write (Nio,'(4X,''AGGREGATE'',15X,5(G12.4,6X),/)')
*     $         aggsxt, aggsat, aggfxt, aggfat, aggfst
*          write (Nio,'(4X,''THROUGH COMP.'',11X,5(G12.4,6X),/)')
*     $         sumsxt, sumsat, sumfxt, sumfat, sumfst
*         end if
*        end if
*       end if
C
C CHECK ON THE MEAN
C
*       jadd = MOD(nz,mq)
*       if (jadd .gt. 0) then
*        jadd = mq - jadd
*       end if
       if (lamd .eq. 0) then
        do i=nz+1,nz+lfor
         oz(i) = Dexp(z(i))
        end do 
       else
        do i=nz+1,nz+lfor
         oz(i) = z(i)
        end do 
       end if
*       if (HTML .eq. 1) then
*        write (Nio,'(''<br><br><u><b>COMPARISON OF MEANS</b></u>'')')
*        write (nio,'(''<TABLE BORDER="0" CELLPADDING="6" '',
*     &              ''CELLSPACING="0" ALIGN="JUSTIFY">'')')
*        write (Nio,'(''<tr><th></th><th align=center colspan=2>'',
*     $               ''STOCHASTIC COMPONENT</th>'',
*     $               ''<th align=center colspan=2>'',
*     $               ''FINAL COMPONENT</th></tr>'')')
*        write (Nio,'(''<tr><th></th>'',
*     $               2(''<th align=right>IN SAMPLE</th>'',
*     $                 ''<th align=right>FORECAST</th>''),
*     $                 ''</tr>'')')
*        write (Nio,'(''<tr><th align=right>SERIES</th>'',
*     $               2(''<td align=right>'',G13.4,
*     $                 ''</td><td align=right>'',G13.4,
*     $                 ''</td>''),''</tr>'')')
*     $        DMEAN(nz+jadd,oz), DMU(oz,nz+1,nz+lfor),
*     $        DMEAN(nz+jadd,Tram), DMU(Tram,nz+1,nz+lfor)
*       else
*        write (Nio,'(/,45x,''COMPARISON OF MEANS'',/,28x,
*     $   ''STOCHASTIC'',32x,''FINAL'',/,28x,''COMPONENT'',33x,
*     $   ''COMPONENT'',/)')
*        write (Nio,'(13X,2(9X,''IN SAMPLE'',8X,''FORECAST'',6X))')
*        write (Nio,'(4X,''SERIES'',6X,2(4X,G13.4,3X,G13.4,8X),/)')
*c     $       DMEAN(nz+jadd,oz), DMU(oz,nz+1,nz+lfor),
*     $       DMEAN(nz,oz), 0.0D0,
*     $       DMEAN(nz+jadd,Tram), DMU(Tram,nz+1,nz+lfor)
*       end if
       do i = nz+1,nz+lfor
        osc(i) = fs(i-nz)
        osa(i) = fsa(i-nz)
        ocyc(i) = fcyc(i-nz)
        oir(i) = fir(i-nz)
        ot(i) = ftr(i-nz)
       end do
*       if ((pg.eq.0).and.(iter.ne.0).and.(ioneout.eq.0)) then
*        if (out.le.1) then
*         if (tramo.eq.0) then
*          fname = title(1:ntitle) // '.FX'
*          subtitle = 'FORECAST OF SERIES(MCS)'
*          call PLOTFCAST1(fname,subtitle,fo,nf,nz,0)
**          write (27,'(A)') fname
*         end if
*        end if
*         if (nchi.ne.1 .or. Noutr.ne.0 .or. Neff(1).ne.0 
*     $           .or. Neff(7).ne.0) then
*         fname = title(1:ntitle) // '.FTR'
*         subtitle = 'FORECAST FINAL TREND-CYCLE'
*         call PLOTFCAST1(fname,subtitle,ftr,nf,nz,0)
**         write (27,'(A)') fname
*        end if
*       end if
*       if (out.eq.0) then
*        if (npsi.ne.1 .or. (Neast+Neff(2)+Npatd).ne.0) then
*         fname = title(1:ntitle) // '.FSA'
*         subtitle = 'FORECAST FINAL SA SERIES'
*         call PLOTFCAST1(fname,subtitle,fsa,nf,nz,0)
**         write (27,'(A)') fname
*        end if
*         if ((Neff(5).eq.1) .or.
*     $      (varwnc.gt.1.0D-10 .and.(ncycth.eq.1.or.ncyc.gt.1)))then
*          fname = title(1:ntitle) // '.FCY'
*          if (isCloseToTD) then
*            subtitle = 'FORECAST FINAL TD COMPONENT'
*          else
*            subtitle = 'FORECAST FINAL TRANSITORY COMPONENT'
*          end if
*          call PLOTFCAST1(fname,subtitle,fcyc,nf,nz,0)
**          write (27,'(A)') fname
*         end if
*       end if
*      end if
      end if
      end
C
C
      subroutine ABIASC(mq,lfor,oz,trend,z,sc,forbias,forsbias,fortbias,
     $                  bias1,bias3,xx,npsi,noC)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
      real*8 ZERO,ONEHND
      parameter(ZERO=0.0d0,ONEHND=100.0d0)
C
C.. Formal Arguments ..
      integer mq,lfor,nPSI
      real*8 oz(*),trend(*),z(*),sc(*),forbias(*),forsbias(*),
     $       fortbias(*),bias1,bias3,xx
      logical noC
C
C.. Local Scalars ..
      integer i,itf,j,j0,jf,jl,nf,nt
      real*8 sabsdif1,sfull2,sum1,sum2,sum3
C
C.. Local Arrays ..
      real*8 forstemp(Kp),fortemp(Kp),forttemp(Kp),stemp(mpkp),
     $       ttemp(mpkp)
C
C.. Intrinsic Functions ..
      intrinsic ABS, DBLE, EXP
      include 'sform.i'
C
C ... Executable Statements ...
C
      do i = 1,Nz+2*mq
       ttemp(i) = EXP(trend(i)) * bias3
       stemp(i) = EXP(sc(i)) / bias1
       stemp(i) = EXP(z(i)) / stemp(i)
      end do
*      do i = 1,59
      do i = 1,Kp
       if (NPSI.ne.1) then
         forstemp(i) = EXP(forbias(i)) / (EXP(forsbias(i))/bias1)
       else
         forstemp(i) = EXP(forbias(i)) / (EXP(forsbias(i)))
       endif
       IF (NPSI.ne.1 .or. .not. noC)then
         forttemp(i) = EXP(fortbias(i)) * bias3
       else
         forttemp(i) = EXP(fortbias(i)) 
       endif
       fortemp(i) = EXP(forbias(i))
      end do
      j0 = 0
      if (Nper .ne. 1) then
       j0 = mq + 1 - Nper
      end if
      jf = Nz - j0 - ((Nz-j0)/mq)*mq
      jl = ((lfor/mq)+1)*mq - lfor - jf
      itf = lfor + 2*mq + jl
      nf = (jf+itf) / mq
      nt = nf + (Nz-j0)/mq
      sfull2 = ZERO
      sabsdif1 = ZERO
      do i = 1,nt-2
       sum1 = ZERO
       sum2 = ZERO
       sum3 = ZERO
       do j = 1,mq
        if (((i-1)*mq+j+j0) .le. Nz) then
         sum1 = sum1 + oz((i-1)*mq+j+j0)
         sum2 = sum2 + stemp((i-1)*mq+j+j0)
         sum3 = sum3 + ttemp((i-1)*mq+j+j0)
        else if (((i-1)*mq+j+j0-Nz).le.Kp) then
         sum1 = sum1 + fortemp((i-1)*mq+j+j0-Nz)
         sum2 = sum2 + forstemp((i-1)*mq+j+j0-Nz)
         sum3 = sum3 + forttemp((i-1)*mq+j+j0-Nz)
        end if
       end do
       sum1 = sum1 / DBLE(mq)
       sum2 = sum2 / DBLE(mq)
       sum3 = sum3 / DBLE(mq)
       sfull2 = sfull2 + sum2
       sabsdif1 = sabsdif1 + (ABS(sum1-sum2))
      end do
      sfull2 = sfull2 / DBLE(nt-2)
      sabsdif1 = sabsdif1 / DBLE(nt-2)
      if (ABS(sfull2) .lt. 1.0d-8) then
       sfull2 = 1.0d-6
      end if
      xx = (sabsdif1/sfull2) * ONEHND
      end
C
C
      subroutine FORTBL(fo,freg,ftr,fsa,fs,fcyc,fir,tse,siepf,siepfl,
     $                  sieaf,sieafl,neff,mq,nouir,noutr,npatd,neast,
     $                  nchi,npsi,ncyc,ncycth,lamd,nper,nyer,nz,lfor,
     $                  isCloseToTD,varwnc)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      INCLUDE 'dimensions.i'
      real*8 ONEHND
      parameter (ONEHND = 100.0d0)
C
C.. Formal Arguments ..
      integer neff(0:7),mq,nouir,noutr,npatd,neast,nchi,npsi,ncyc,
     $        ncycth,lamd,nper,nyer,nz, lfor
      real*8 fo(-kp:kp),freg(-kp:kp),ftr(-kp:kp),fsa(-kp:kp),fs(-kp:kp),
     $       fcyc(-kp:kp),fir(-kp:kp),tse(kl),siepf(kl),siepfl(kl),
     $       sieaf(kl),sieafl(kl),varwnc
      logical isCloseToTD
C
C.. Local Scalars ..
      integer i,j,jnlastper,jnlastyear,ncols,nf,nlastper,nlastyear,nse
C
C.. Local Arrays ..
      character fn(0:12)*12,fstline(7)*16,mth(12)*4,scnline(7)*16,
     $          srt(11)*4,thrline(7)*16,wrt(10)*12,wrt1(5)*12,
     $          wrt2(4)*12,wrt3(5)*12,wrt4(4)*12
      real*8 formatrix(kp,14),tmp(kp)
C
C.. External Calls ..
      external USRENTRY
C
C.. Intrinsic Functions ..
      intrinsic MAX, MOD
      include 'stream.i'
C
C.. Data Declarations ..
C       DATA WRT/'(3X','''DATE'',10X','N','(''FORECAST''','6X',
C     $          '''SE'',8X','))'/
      data fn/'0','1','2','3','4','5','6','7','8','9','10','11','12'/
      data mth/
     $     'JAN ','FEB ','MAR ','APR ','MAY ','JUN','JUL','AUG ','SEP',
     $     'OCT ','NOV ','DEC '/
      data srt/
     $     '1ST','2ND','3RD','4TH','5TH','6TH','7TH','8TH','9TH','10TH',
     $     '11TH'/
C
C ... Executable Statements ...
C
c   initialize wrt format variables so they are the same for each call
c   of the subroutine  (BCM, JAN 2003)
      CALL setwrt(wrt,0)
      CALL setwrt(wrt1,1)
      CALL setwrt(wrt2,2)
      CALL setwrt(wrt3,3)
      CALL setwrt(wrt4,4)
c   end of change  BCM
      ncols = 1
      nse = 1
      nf = MAX(lfor,MAX(8,2*mq))
      do i = 1,nf
       formatrix(i,ncols) = fo(i)
       formatrix(i,ncols+1) = tse(i)
      end do
      ncols = ncols + 1
      fstline(ncols-nse) = 'ORIGINAL'
      scnline(ncols-nse) = '(UNCORRECTED)'
      thrline(ncols-nse) = 'SERIES'
      if ((nchi.gt.1) .or. (noutr.eq.1) .or. (neff(1).eq.1) 
     $     .or. (neff(7).eq.1)) then
       nse = nse + 1
       ncols = ncols + 1
       do i = 1,nf
        formatrix(i,ncols) = ftr(i)
        tmp(i) = ftr(i)
       end do
       ncols = ncols + 1
       if (lamd .eq. 0) then
        do i = 1,nf
         formatrix(i,ncols) = siepfl(i)
        end do
        call usrentry(siepfl,1,nf,1,kl,1256)
       else
        do i = 1,nf
         formatrix(i,ncols) = siepf(i)
        end do
        call usrentry(siepf,1,nf,1,kl,1256)
       end if
       call USRENTRY(tmp,1,nf,1,kp,1410)
       fstline(ncols-nse) = 'TREND-CYCLE'
       scnline(ncols-nse) = ' '
       thrline(ncols-nse) = ' '
      end if
      if ((npsi.gt.1) .or. (neast.eq.1) .or. (neff(2).eq.1) .or.
     $    (npatd.eq.1)) then
       nse = nse + 1
       ncols = ncols + 1
       do i = 1,nf
        formatrix(i,ncols) = fsa(i)
        tmp(i) = fsa(i)
       end do
       ncols = ncols + 1
       if (lamd .eq. 0) then
        do i = 1,nf
         formatrix(i,ncols) = sieafl(i)
        end do
        call usrentry(sieafl,1,nf,1,kl,1257)
       else
        do i = 1,nf
         formatrix(i,ncols) = sieaf(i)
        end do
        call usrentry(sieaf,1,nf,1,kl,1257)
       end if
       call USRENTRY(tmp,1,nf,1,kp,1409)
       fstline(ncols-nse) = 'SA SERIES'
       scnline(ncols-nse) = 'SERIES'
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
C       thrline(ncols-nse) = ''
C   END OF CODE BLOCK       
C   LINES OF CODE ADDED FOR X-13A-S : 1
       thrline(ncols-nse) = ' '
C   END OF CODE BLOCK
      else if (neff(0) .eq. 1) then
       nse = nse + 1
       ncols = ncols + 1
       do i = 1,nf
        formatrix(i,ncols) = fsa(i)
        tmp(i) = fsa(i)
       end do
       ncols = ncols + 1
       if (lamd .eq. 0) then
        do i = 1,nf
         formatrix(i,ncols) = sieafl(i)
        end do
        call usrentry(sieafl,1,nf,1,kl,1257)
       else
        do i = 1,nf
         formatrix(i,ncols) = sieaf(i)
        end do
        call usrentry(sieaf,1,nf,1,kl,1257)
       end if
       call USRENTRY(tmp,1,nf,1,kp,1409)
       fstline(ncols-nse) = 'SA SERIES'
       scnline(ncols-nse) = 'SERIES'
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
C       thrline(ncols-nse) = ''
C   END OF CODE BLOCK       
C   LINES OF CODE ADDED FOR X-13A-S : 1
       thrline(ncols-nse) = ' '
C   END OF CODE BLOCK
      end if
      if (neff(0) .eq. 1) then
       ncols = ncols + 1
       do i = 1,nf
        formatrix(i,ncols) = freg(i)
       end do
       fstline(ncols-nse) = 'SEPARATE'
       scnline(ncols-nse) = 'REGRESSION'
       thrline(ncols-nse) = 'EFFECT'
      end if
      if ((npsi.gt.1) .or. (neast.eq.1) .or. (neff(2).eq.1) .or.
     $    (npatd.eq.1)) then
       ncols = ncols + 1
       do i = 1,nf
        formatrix(i,ncols) = fs(i)
        tmp(i) = fs(i)
       end do
       call USRENTRY(tmp,1,nf,1,kp,1411)
       fstline(ncols-nse) = 'SEASONAL'
       if (lamd .eq. 0) then
        scnline(ncols-nse) = 'FACTORS'
       else
        scnline(ncols-nse) = 'COMPONENT'
       end if
       thrline(ncols-nse) = ' '
      end if
C      IF ((NCYCTH.EQ.1).OR.(NCYC.GT.1).OR.(NEFF(5).EQ.1)) THEN
C        NCOLS=NCOLS+1
C         DO 50 I=1,NF
C           FORMATRIX(I,NCOLS)=FCYC(I)
C           TMP(I)=FCYC(I)
C 50     CONTINUE
C        CALL USRENTRY(TMP,1,NF,1413)
C        FSTLINE(NCOLS)='TRANSITORY'
C        IF (LAMD.EQ.0) THEN
C          SCNLINE(NCOLS)='FACTORS'
C        ELSE
C          SCNLINE(NCOLS)='COMPONENT'
C        end if
C        THRLINE(NCOLS)=' '
C      end if
      if ((neff(3).eq.1) .or. (nouir.eq.1) .or.
     $    (varwnc.gt.1.0D-10 .and.(ncycth.eq.1.or.ncyc.gt.1))
     $      .or. (neff(5).eq.1)) then
       ncols = ncols + 1
       if (lamd .eq. 1) then
        do i = 1,nf
         formatrix(i,ncols) = fir(i) + fcyc(i)
         tmp(i) = fir(i)
        end do
       else
        do i = 1,nf
         formatrix(i,ncols) = (fir(i)*fcyc(i)) / ONEHND
         tmp(i) = fir(i)
        end do
       end if
       call USRENTRY(tmp,1,nf,1,kp,1412)
       if (isCloseToTD) then
         fstline(ncols-nse) = 'TDfinal.-IRREG.'
       else
         fstline(ncols-nse) = 'TRANS.-IRREG.'
       end if
       if (lamd .eq. 0) then
        scnline(ncols-nse) = 'FACTORS'
       else
        scnline(ncols-nse) = ' '
       end if
       thrline(ncols-nse) = ' '
      end if
      nlastper = nper
      nlastyear = nyer
      do i = 2,nz
       if (MOD(nlastper,mq) .eq. 0) then
        nlastyear = nlastyear + 1
        nlastper = 0
       end if
       nlastper = nlastper + 1
      end do
      nlastper = nlastper + 1
      if (nlastper .gt. mq) then
       nlastper = 1
       nlastyear = nlastyear + 1
      end if
      jnlastper = nlastper
      jnlastyear = nlastyear
C
C 100   FORMAT(9X,A13,11X,A13,11X,A13,11X,A13,11X,
C     $        A13,11X,A13,11X,A13)
C 110   FORMAT(2X,A3,'-',I4,4X,F13.4,4X,F13.4,3X,F13.4,3X,F13.4,5X,
C     $          F13.4,3X,F13.4,4X,F13.4)
      write (Nio,'(//)')
      wrt2(2) = fn(nse)
      write (Nio,wrt2) (fstline(i), i = 1,nse)
      write (Nio,wrt2) (scnline(i), i = 1,nse)
      write (Nio,wrt2) (thrline(i), i = 1,nse)
      if (nse.eq.1)THEN
       wrt(6) = '1x)'
       DO i = 7,10
        wrt(i) = ' '
       END DO 
      else
       wrt(6) = fn(nse-1)
      end if
      wrt1(3) = fn(nse)
      write (Nio,*)
      write (Nio,wrt)
      write (Nio,*)
      if (mq .eq. 12) then
       do i = 1,nf
        write (Nio,wrt1)
     $        mth(nlastper), nlastyear, (formatrix(i,j), j = 1,nse*2)
        if (nlastper .eq. mq) then
         nlastper = 1
         nlastyear = nlastyear + 1
        else
         nlastper = nlastper + 1
        end if
       end do
      else
       do i = 1,nf
        write (Nio,wrt1)
     $        srt(nlastper), nlastyear, (formatrix(i,j), j = 1,nse*2)
        if (nlastper .eq. mq) then
         nlastper = 1
         nlastyear = nlastyear + 1
        else
         nlastper = nlastper + 1
        end if
       end do
      end if
      if (nse*2 .lt. ncols) then
       write (Nio,'(/)')
       nlastper = jnlastper
       nlastyear = jnlastyear
       wrt4(2) = fn(ncols-2*nse)
       write (Nio,wrt4) (fstline(i), i = nse+1,ncols-nse)
       write (Nio,wrt4) (scnline(i), i = nse+1,ncols-nse)
       write (Nio,wrt4) (thrline(i), i = nse+1,ncols-nse)
C   LINES OF CODE COMMENTED FOR X-13A-S : 1       
c       wrt(3) = fn(ncols-2*nse)
C   END OF CODE BLOCK
       wrt3(3) = fn(ncols-2*nse)
       if (mq .eq. 12) then
        do i = 1,nf
         write (Nio,wrt3)
     $         mth(nlastper), nlastyear,
     $         (formatrix(i,j), j = nse*2+1,ncols)
         if (nlastper .eq. mq) then
          nlastper = 1
          nlastyear = nlastyear + 1
         else
          nlastper = nlastper + 1
         end if
        end do
       else
        do i = 1,nf
         write (Nio,wrt3)
     $         srt(nlastper), nlastyear,
     $         (formatrix(i,j), j = nse*2+1,ncols)
         if (nlastper .eq. mq) then
          nlastper = 1
          nlastyear = nlastyear + 1
         else
          nlastper = nlastper + 1
         end if
        end do
       end if
      end if
      write (Nio,'(//,2x,''SE  : STANDARD ERROR OF THE OBSERVED '',
     $  ''SERIES FORECAST.''/,2x,''SER : STANDARD ERROR OF THE '',
     $  ''REVISION.'',//,2x,''Note 1 : SINCE THE COMPONENT IS '',
     $  ''NEVER OBSERVED,THE FORECAST ERROR IS OF LITTLE'',/,2x,
     $  ''APPLIED INTEREST. WHAT IS OF INTEREST '',
     $  ''IS THE SE OF THE REVISION THE FORECAST'',/,2x,
     $  ''OF THE COMPONENT WILL UNDERGO (UNTIL IT BECOMES '',
     $  ''THE FINAL OR HISTORICAL ESTIMATOR).'',/)')
      write (Nio,'(2x,''Note 2 : SER(Seasonal) = SER (SA Series)'',/)')
      end
C
C
      subroutine RATES(tram,otr,osa,fo,ftr,fsa,nchi,npsi,lfor,nfinal)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
      real*8 ONEHND
      parameter(ONEHND=100.0d0)
C
C.. Formal Arguments ..
      integer nchi,npsi,lfor,nfinal
      real*8 tram(*),otr(*),osa(*),fo(-kp:kp),ftr(-kp:kp),fsa(-kp:kp)
C
C.. Local Scalars ..
      integer i,k,nper2,nrg,nyer2,nzs
C
C.. Local Arrays ..
      real*8 rg(mpkp),temp(mpkp)
      integer thisDate(2)
C
C.. External Calls ..
      external TABLE
C
C.. Intrinsic Functions ..
      intrinsic EXP
      include 'sig.i'
      include 'sform.i'
      include 'stream.i'
C
C ... Executable Statements ...
C
      nzs = Nz
      nyer2 = Nyer
      nper2 = Nper
C
C     RATES OF ORIGINAL SERIES
C
      if (nfinal .eq. 1) then
       do i = 1,Nz
        temp(i) = tram(i)
       end do
       do i = 1,lfor
        temp(Nz+i) = fo(i)
       end do
      else
       do i = 1,Nz+lfor
        temp(i) = EXP(tram(i))
       end do
      end if
      k = Nz - lfor
      nrg = (Nz+lfor/2) - (Nz-lfor) + 1
      do i = Nz-lfor,Nz+lfor/2
       rg(i-k+1) = ((temp(i)-temp(i-1))/temp(i-1)) * ONEHND
      end do
      Nz = nrg
      Nper = Nper + nzs - lfor - 1
      do while (Nper.gt.Nfreq .and. Nfreq.ne.0)
       Nper = Nper - Nfreq
       Nyer = Nyer + 1
      end do
      if (nfinal .eq. 1) then
       CALL genSkip(1157)
       call prttbl(thisDate,Nfreq,rg,nrg,
     &             'ORIGINAL SERIES (from regARIMA)',ndec,
     &             'original.rates.final')
      else
       CALL genSkip(1158)
       call prttbl(thisDate,Nfreq,rg,nrg,
     &             'ORIGINAL SERIES',ndec,'original.rates')
      end if
      Nz = nzs
      if (npsi .gt. 1) then
       if (nfinal .eq. 1) then
        do i = 1,Nz
         temp(i) = osa(i)
        end do
        do i = 1,lfor
         temp(Nz+i) = fsa(i)
        end do
       else
        do i = 1,Nz+lfor
         temp(i) = osa(i)
        end do
       end if
       k = Nz - lfor
       nrg = (Nz+lfor/2) - (Nz-lfor) + 1
       do i = Nz-lfor,Nz+lfor/2
        rg(i-k+1) = ((temp(i)-temp(i-1))/temp(i-1)) * ONEHND
       end do
       Nz = nrg
       if (nfinal .eq. 1) then
        CALL genSkip(1159)
        call prttbl(thisDate,Nfreq,rg,nrg,
     &              'FINAL SEASONALLY ADJUSTED SERIES',ndec,
     &              'sa.rates.final')
       else
        CALL genSkip(1160)
        call prttbl(thisDate,Nfreq,rg,nrg,
     &              'SEASONALLY ADJUSTED SERIES',ndec,'sa.rates')
       end if
       Nz = nzs
      end if
      if (nchi .gt. 1) then
       if (nfinal .eq. 1) then
        do i = 1,Nz
         temp(i) = otr(i)
        end do
        do i = 1,lfor
         temp(Nz+i) = ftr(i)
        end do
       else
        do i = 1,Nz+lfor
         temp(i) = otr(i)
        end do
       end if
       k = Nz - lfor
       nrg = (Nz+lfor/2) - (Nz-lfor) + 1
       do i = Nz-lfor,Nz+lfor/2
        rg(i-k+1) = ((temp(i)-temp(i-1))/temp(i-1)) * ONEHND
       end do
       Nz = nrg
       if (nfinal .eq. 1) then
        CALL genSkip(1161)
        call prttbl(thisDate,Nfreq,rg,nrg,'FINAL TREND-CYCLE',ndec,
     &              'trend.rates.final')
       else
        CALL genSkip(1162)
        call prttbl(thisDate,Nfreq,rg,nrg,'TREND-CYCLE',ndec,
     &              'trend.rates')
       end if
       Nz = nzs
      end if
      Nyer = nyer2
      Nper = nper2
      end
C
C
C
      subroutine VARIANCES(oz,z,trend,sa,sc,cycle,ir,nz,lamda,out,qt1,
     $                     varwnp,varwns,varwnc,theta,nth,psieps,psiess,
     $                     psiecs,psiue,psieas,nfl)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
      integer nfilt
c      parameter (kl = PFCST, kp = PFCST, mp = POBS, nfilt = 1200)
      parameter (nfilt = mp * 4)
      real*8 ZERO,ONE,ONEHND
      parameter (ZERO = 0.0d0, ONE = 1.0d0, ONEHND = 100.0d0)
C
C.. Formal Arguments ..
      integer nz,lamda,out,nth,nfl
      real*8 oz(mpkp),z(mpkp),trend(mpkp),sa(mpkp),sc(mpkp),
     $       cycle(mpkp),ir(mpkp),qt1,varwnp,varwns,varwnc,
     $       theta(*),psieps(*),psiess(*),psiecs(*),psiue(*),psieas(*)
C
C.. Local Scalars ..
      integer i,j,ndum,ndum1,nlenght
      real*8 aa,ac,ap,as,au,bias1,bias3,dmfcyc,dmfir,dmfsa,dmfsea,
     $       dmftre,ga,gc,gp,gs,gu,gx,vc,vcycle,vfcycle,vfir,vfsa,
     $       vfsc,vftrend,vir,vp,vs,vsa,vsc,vtrend,vu,vx,vxlin,vxorig
C
C.. Local Arrays ..
      real*8 dum(nfilt+kp*2),dum1(40),fcyc(mpkp),fir(mpkp),
     $       flcyc(mpkp),flir(mpkp),flsa(mpkp),flsea(mpkp),
     $       fltre(mpkp),fsa(mpkp),fsea(mpkp),ftre(mpkp),temp(mpkp)
C
C.. External Functions ..
      real*8 DMEAN
      real*8 DVAR
      LOGICAL dpeq
      external DMEAN, DVAR, dpeq
C
C.. External Calls ..
      external CONV, GETTHVARIANCE
C
C.. Intrinsic Functions ..
      intrinsic EXP, LOG, SQRT
      include 'cxfinal.i'
      include 'estb.i'
      include 'hspect.i'
      include 'models.i'
      include 'preadtr.i'
      include 'stream.i'
      include 'transcad.i'
C
C ... Executable Statements ...
C
C
C
      if (Tramo .eq. 1) then
       if (lamda .eq. 1) then
        vtrend = DVAR(nz,trend)
        vsa = DVAR(nz,sa)
        vsc = DVAR(nz,sc)
        vir = DVAR(nz,ir)
        vcycle = DVAR(nz,cycle)
        do i = 1,nz
         ftre(i) = trend(i) + Paoutr(i) + Pareg(i,1) +Pareg(i,7)
        end do
        vftrend = DVAR(nz,ftre)
        do i = 1,nz
         fsa(i) =
     $     Tram(i) - (sc(i)+Paeast(i)+Patd(i)+Pareg(i,2)+Pareg(i,0))
        end do
        vfsa = DVAR(nz,fsa)
        do i = 1,nz
         fsea(i) = sc(i) + Paeast(i) + Patd(i) + Pareg(i,2)
        end do
        vfsc = DVAR(nz,fsea)
        do i = 1,nz
         fir(i) = ir(i) + Paouir(i) + Pareg(i,3)
        end do
        vfir = DVAR(nz,fir)
        do i = 1,nz
         fcyc(i) = cycle(i) + Pareg(i,5)
        end do
        vfcycle = DVAR(nz,fcyc)
       else
C
C VEDERE COSA FARE CON IL BIAS CORRECTION
C
        bias3 = ONE
        do i = 1,nz
         ftre(i) = EXP(trend(i)) * Paoutr(i) * Pareg(i,1) * 
     $            Pareg(i,7) * bias3
        end do
        vftrend = DVAR(nz,ftre)
        bias1 = ONE
        do i = 1,nz
         fsa(i) =
     $     Tram(i) /
     $     (EXP(sc(i))*Paeast(i)*Patd(i)*(Pareg(i,2)/bias1)*Pareg(i,0))
        end do
        vfsa = DVAR(nz,fsa)
        bias1 = ONE
        do i = 1,nz
         fsea(i) = (EXP(sc(i))*Paeast(i)*Patd(i)*Pareg(i,2)) / bias1
        end do
        vfsc = DVAR(nz,fsea)
        do i = 1,nz
         fir(i) = EXP(ir(i)) * Paouir(i) * Pareg(i,3)
        end do
        vfir = DVAR(nz,fir)
        do i = 1,nz
         fcyc(i) = EXP(cycle(i)) * Pareg(i,5)
        end do
        vfcycle = DVAR(nz,fcyc)
        do i = 1,nz
         temp(i) = EXP(trend(i))
        end do
        vtrend = DVAR(nz,temp)
        do i = 1,nz
         temp(i) = EXP(sa(i))
        end do
        vsa = DVAR(nz,temp)
        do i = 1,nz
         temp(i) = EXP(sc(i))
        end do
        vsc = DVAR(nz,temp)
        do i = 1,nz
         temp(i) = EXP(ir(i))
        end do
        vir = DVAR(nz,temp)
        do i = 1,nz
         temp(i) = EXP(cycle(i))
        end do
        vcycle = DVAR(nz,temp)
       end if
       vxorig = DVAR(nz,Tram)
       vxlin = DVAR(nz,oz)
C
C COMPUTE CROSS-CORRELATION OF FINAL ADDITIVE COMPONENT
C
       if (lamda .eq. 1) then
        dmfsa = DMEAN(nz,fsa)
        dmfsea = DMEAN(nz,fsea)
        dmftre = DMEAN(nz,ftre)
        dmfir = DMEAN(nz,fir)
        dmfcyc = DMEAN(nz,fcyc)
        Crssa = ZERO
        Crtsa = ZERO
        Crts = ZERO
        Crirsa = ZERO
        Crirs = ZERO
        Crirt = ZERO
        Crcycsa = ZERO
        Crcycs = ZERO
        Crcyct = ZERO
        Crcycir = ZERO
        do i = 1,nz
         Crssa = (fsa(i)-dmfsa)*(fsea(i)-dmfsea) + Crssa
         Crtsa = (ftre(i)-dmftre)*(fsa(i)-dmfsa) + Crtsa
         Crts = (ftre(i)-dmftre)*(fsea(i)-dmfsea) + Crts
         Crirsa = (fir(i)-dmfir)*(fsa(i)-dmfsa) + Crirsa
         Crirs = (fir(i)-dmfir)*(fsea(i)-dmfsea) + Crirs
         Crirt = (fir(i)-dmfir)*(ftre(i)-dmftre) + Crirt
         if (Ncyc .gt. 1) then
          Crcycsa = (fcyc(i)-dmfcyc)*(fsa(i)-dmfsa) + Crcycsa
          if (NPSI .gt. 1) then
           Crcycs = (fcyc(i)-dmfcyc)*(fsea(i)-dmfsea) + Crcycs
          else
           Crcycs = ZERO
          end if
          if (NCHI .gt. 1) then
           Crcyct = (fcyc(i)-dmfcyc)*(ftre(i)-dmftre) + Crcyct
          else
           Crcyct = ZERO
          end if
          Crcycir = (fcyc(i)-dmfcyc)*(fir(i)-dmfir) + Crcycir
         end if
        end do
       else
        do i = 1,nz
         flsa(i) = LOG(fsa(i))
         flsea(i) = LOG(fsea(i))
         fltre(i) = LOG(ftre(i))
         flcyc(i) = LOG(fcyc(i))
         flir(i) = LOG(fir(i))
        end do
        dmfsa = DMEAN(nz,flsa)
        dmfsea = DMEAN(nz,flsea)
        dmftre = DMEAN(nz,fltre)
        dmfir = DMEAN(nz,flir)
        dmfcyc = DMEAN(nz,flcyc)
        Crssa = ZERO
        Crtsa = ZERO
        Crts = ZERO
        Crirsa = ZERO
        Crirs = ZERO
        Crirt = ZERO
        Crcycsa = ZERO
        Crcycs = ZERO
        Crcyct = ZERO
        Crcycir = ZERO
        do i = 1,nz
         Crssa = (flsa(i)-dmfsa)*(flsea(i)-dmfsea) + Crssa
         Crtsa = (fltre(i)-dmftre)*(flsa(i)-dmfsa) + Crtsa
         Crts = (fltre(i)-dmftre)*(flsea(i)-dmfsea) + Crts
         Crirsa = (flir(i)-dmfir)*(flsa(i)-dmfsa) + Crirsa
         Crirs = (flir(i)-dmfir)*(flsea(i)-dmfsea) + Crirs
         Crirt = (flir(i)-dmfir)*(fltre(i)-dmftre) + Crirt
         if (Ncyc .gt. 1) then
          Crcycsa = (flcyc(i)-dmfcyc)*(flsa(i)-dmfsa) + Crcycsa
          Crcycs = (flcyc(i)-dmfcyc)*(flsea(i)-dmfsea) + Crcycs
          Crcyct = (flcyc(i)-dmfcyc)*(fltre(i)-dmftre) + Crcyct
          Crcycir = (flcyc(i)-dmfcyc)*(flir(i)-dmfir) + Crcycir
         end if
        end do
       end if
       if ((NADJS .gt. 1) .and. (NPSI .gt. 1)) then
        Crssa = Crssa / (SQRT(vfsa)*SQRT(vfsc))
       else
        Crssa = ZERO
       end if
       if ((NADJS .gt. 1) .and. (NCHI .gt. 1)) then
        Crtsa = Crtsa / (SQRT(vftrend)*SQRT(vfsa))
       else
        Crtsa = ZERO
       end if
       if ((NPSI .gt. 1) .and. (NCHI .gt. 1)) then
        Crts = Crts / (SQRT(vftrend)*SQRT(vfsc))
       else
        Crts = ZERO
       end if
       if (NADJS .gt. 1) then 
        Crirsa = Crirsa / (SQRT(vfir)*SQRT(vfsa))
       else
        Crirsa = ZERO
       end if
       if (NPSI .gt. 1) then
        Crirs = Crirs / (SQRT(vfir)*SQRT(vfsc))
       else
        Crirs = ZERO
       end if
       if (NCHI .gt. 1) then
        Crirt = Crirt / (SQRT(vfir)*SQRT(vftrend))
       else
        Crirt = ZERO
       end if
       if (Ncyc .gt. 1) then
        IF(NADJS .gt. 1 .and. (.not.dpeq(vfcycle,0D0)))THEN
         Crcycsa = Crcycsa / (SQRT(vfcycle)*SQRT(vfsa))
        ELSE        
         Crcycsa = ZERO
        end if
        IF(NPSI .gt. 1 .and. (.not.dpeq(vfcycle,0D0)))THEN
         Crcycs = Crcycs / (SQRT(vfcycle)*SQRT(vfsc))
        ELSE        
         Crcycs = ZERO
        end if
        IF(NCHI .gt. 1 .and. (.not.dpeq(vfcycle,0D0)))THEN
         Crcyct = Crcyct / (SQRT(vfcycle)*SQRT(vftrend))
        ELSE        
         Crcyct = ZERO
        end if
        IF(.not.dpeq(vfcycle,0D0))THEN
         Crcycir = Crcycir / (SQRT(vfcycle)*SQRT(vfir))
        ELSE
         Crcycir = ZERO
        end if
       end if
C
C OUTPUT VARIANCES
C
c  rober revisar esta parte
c
       if (out .eq. 0) then
         CALL genSkip(1012)
         call mkPOneLine(Nio,'ub','DECOMPOSITION OF VARIANCE (IN %)')
         call mkPOneLine(Nio,'bold',
     $                   'A) SAMPLE VARIANCE FOR ORIGINAL SERIES')
         CALL mkTableTag(Nio,'w80','@')
         CALL mkCaption(Nio,'SAMPLE VARIANCE FOR ORIGINAL SERIES')
         write (Nio,1010)
 1010    FORMAT('<tr><th>&nbsp;</th><th scope="col">FINAL</th>',
     $          '<th scope="col">STOCHASTIC</th></tr>')
         write (Nio,1020)'SEASONAL COMPONENT',
     $         (vfsc/vxorig)*ONEHND, (vsc/vxlin)*ONEHND
 1020    FORMAT('<tr><th scope="row">',a,'</th>',
     $          '<td>',F12.4,'</td><td>',F12.4,'</td></tr>')
         write (Nio,1020)'TREND-CYCLE',
     $         (vftrend/vxorig)*ONEHND, (vtrend/vxlin)*ONEHND
         write (Nio,1020)'IRREGULAR COMPONENT',
     $         (vfir/vxorig)*ONEHND, (vir/vxlin)*ONEHND
         if (Ncyc .gt. 1) then
          write (Nio,1020)'TRANSITORY COMPONENT',
     $          (vfcycle/vxorig)*ONEHND, (vcycle/vxlin)*ONEHND
         end if
         write (Nio,1020)'TOTAL',
     $         ((vfir+vfcycle+vftrend+vfsc)/vxorig)*ONEHND,
     $         ((vir+vcycle+vtrend+vsc)/vxlin)*ONEHND
         write (Nio,1020)'SEASONALLY ADJUSTED SERIES',
     $         (vfsa/vxorig)*ONEHND, (vsa/vxlin)*ONEHND
         CALL writTag(nio,'</table>')
         CALL mkPOneLine(Nio,'@','&nbsp;')
       end if
      end if
C
C THEORETICAL VARIANCE
C
C
C SERIES
C
      call CONV(Chis,Nchis,Psis,Npsis,dum,ndum)
      call CONV(dum,ndum,Cyc,Ncyc,dum1,ndum1)
      call GETTHVARIANCE(dum1,ndum1,theta,nth,ONE,vx)
C
C THEORETICAL VARIANCE
C TREND
C
      call CONV(Psins,Npsins,Thetp,Nthetp,dum,ndum)
      call GETTHVARIANCE(Chis,Nchis,dum,ndum,varwnp,vp)
C
C THEORETICAL VARIANCE
C SEASONAL
C
      call CONV(Chins,Nchins,Thets,Nthets,dum,ndum)
      call GETTHVARIANCE(Psis,Npsis,dum,ndum,varwns,vs)
C
C THEORETICAL VARIANCE
C IRREGULAR
C
      call CONV(Chins,Nchins,Psins,Npsins,dum,ndum)
      dum1(1) = ONE
      ndum1 = 1
      call GETTHVARIANCE(dum1,ndum1,dum,ndum,qt1,vu)
C
C THEORETICAL VARIANCE
C CYCLE
C
      call CONV(Chins,Nchins,Psins,Npsins,dum,ndum)
      call CONV(dum,ndum,Thetc,Nthetc,dum1,ndum1)
      call GETTHVARIANCE(Cyc,Ncyc,dum1,ndum1,varwnc,vc)
C
C       CHECK
C
C        WRITE(*,*)'VX ',VX
C        WRITE(*,*)'VU ',VU
C        WRITE(*,*)'VS ',VS
C        WRITE(*,*)'VP ',VP
C        WRITE(*,*)VX-VC-VU-VS-VP,' SHOULD BE ZERO'
C        READ(*,*)
C
C MMSE THEORETICAL VARIANCE
C TREND
C
      call CONV(psieps,nfl,Psins,Npsins,dum,ndum)
      ap = ZERO
      do i = 1,ndum
       ap = ap + dum(i)*dum(i)
      end do
C        AP=DSQRT(AP/(NDUM*ONE))
C
C MMSE THEORETICAL VARIANCE
C SEASONAL
C
      call CONV(psiess,nfl,Chins,Nchins,dum,ndum)
      as = ZERO
      do i = 1,ndum
       as = as + dum(i)*dum(i)
      end do
C        AS=DSQRT(AS/(NDUM*ONE))
C
C MMSE THEORETICAL VARIANCE
C IRREGULAR
C
      call CONV(Chins,Nchins,Psins,Npsins,dum1,ndum1)
      call CONV(psiue,nfl,dum1,ndum1,dum,ndum)
      au = ZERO
      do i = 1,ndum
       au = au + dum(i)*dum(i)
      end do
C        AR=DSQRT(AR/(NDUM*ONE))
C
C MMSE THEORETICAL VARIANCE
C CYCLE
C
      call CONV(psiecs,nfl,dum1,ndum1,dum,ndum)
      ac = ZERO
      do i = 1,ndum
       ac = ac + dum(i)*dum(i)
      end do
C
C MMSE THEORETICAL VARIANCE
C SA
C
      call CONV(psieas,nfl,adjns,nadjns,dum,ndum)
      aa = ZERO
      do i = 1,ndum
       aa = aa + dum(i)*dum(i)
      end do
C
C DECOMPOSITION OF THE VARIANCE OF THE STATIONARY SERIES IN TERMS
C OF THE ESTIMATES OBTAINED BY SEATS.
C
C DUM1 IS CHINS*PSINS
C
C TREND
C
      nlenght = nz - ndum1 + 1
      do i = 1,nlenght
       dum(i) = ZERO
       do j = 1,ndum1
        dum(i) = dum(i) + dum1(j)*trend(i+ndum1-j)
       end do
      end do
      gp = DVAR(nlenght,dum)
C
C SEASONAL
C
      do i = 1,nlenght
       dum(i) = ZERO
       do j = 1,ndum1
        dum(i) = dum(i) + dum1(j)*sc(i+ndum1-j)
       end do
      end do
      gs = DVAR(nlenght,dum)
C
C IRREGULAR
C
      do i = 1,nlenght
       dum(i) = ZERO
       do j = 1,ndum1
        dum(i) = dum(i) + dum1(j)*ir(i+ndum1-j)
       end do
      end do
      gu = DVAR(nlenght,dum)
C
C CYCLE
C
      do i = 1,nlenght
       dum(i) = ZERO
       do j = 1,ndum1
        dum(i) = dum(i) + dum1(j)*cycle(i+ndum1-j)
       end do
      end do
      gc = DVAR(nlenght,dum)
C
C ADJUSTED
C
      do i = 1,nlenght
       dum(i) = ZERO
       do j = 1,ndum1
        dum(i) = dum(i) + dum1(j)*sa(i+ndum1-j)
       end do
      end do
      ga = DVAR(nlenght,dum)
C
C SERIES
C
      do i = 1,nlenght
       dum(i) = ZERO
       do j = 1,ndum1
        dum(i) = dum(i) + dum1(j)*z(i+ndum1-j)
       end do
      end do
      gx = DVAR(nlenght,dum)
C
C CHECK
C
C        WRITE(*,*) ((VP+VS+VU+VC)/VX)*100.0D0,' SHOULD BE 100'
C
      if (out .eq. 0) then
       if (Tramo .ne. 1) then
         CALL genSkip(1012)
         call mkPOneLine(Nio,'ub','DECOMPOSITION OF VARIANCE (IN %)')
       end if
       call mkPOneLine(Nio,'bold',
     $                 'B) VARIANCE OF THE STATIONARY SERIES')
       CALL mkTableTag(Nio,'w80','@')
       CALL mkCaption(Nio,'B) VARIANCE OF THE STATIONARY SERIES')
       write (Nio,1030)
 1030  FORMAT('<tr><th>&nbsp;</th>',
     $        '<th scope="col">THEORETICAL COMPONENT</th>',
     $        '<th scope="col">MMSE ESTIMATOR</th>',
     $        '<th scope="col">ESTIMATED COMPONENT</th></tr>')
       write (Nio,1040)'SEASONAL COMPONENT',
     $       (vs/vx)*ONEHND, (as/vx)*ONEHND, (gs/gx)*ONEHND
 1040  FORMAT('<tr><th scope="row">',a,'</th>',
     $        '<td>',F12.4,'</td><td>',F12.4,'</td><td>',F12.4,
     $        '</td></tr>')
       write (Nio,1040)'TREND-CYCLE',
     $       (vp/vx)*ONEHND, (ap/vx)*ONEHND, (gp/gx)*ONEHND
       write (Nio,1040)'IRREGULAR COMPONENT',
     $       (vu/vx)*ONEHND, (au/vx)*ONEHND, (gu/gx)*ONEHND
       if (Ncyc .gt. 1) then
        write (Nio,1040)'TRANSITORY COMPONENT',
     $         (vc/vx)*ONEHND, (ac/vx)*ONEHND, (gc/gx)*ONEHND
       end if
       write (Nio,1040)'TOTAL',
     $       ((vp+vs+vc+vu)/vx)*ONEHND, ((as+ap+au+ac)/vx)*ONEHND,
     $       ((gs+gp+gu+gc)/gx)*ONEHND
       write (Nio,1040)'SEASONALLY ADJUSTED SERIES',
     $        ((vp+vu+vc)/vx)*ONEHND, ((ac+ap+au)/vx)*ONEHND, 
     $        (ga/gx)*ONEHND
       CALL writTag(nio,'</table>')
       CALL mkPOneLine(Nio,'@','&nbsp;')
      end if
      end
C
C
      subroutine GETTHVARIANCE(phi,nphi,theta,ntheta,varinn,var)
C
C.. Implicits ..
      implicit none
      include 'units.cmn'
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 phi(*)
C.. In/Out Status: Read, Not Written ..
      integer nphi
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 theta(*)
C.. In/Out Status: Read, Not Written ..
      integer ntheta
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 varinn
C.. In/Out Status: Not Read, Overwritten ..
      real*8 var
C
C.. Local Scalars ..
      integer i,nbjphi,nbjtheta,ndum,ndum3
c      integer ndum1
C
C.. Local Arrays ..
      real*8 bjphi(32),bjtheta(32),dum(0:50),dum1(0:50),dum3(0:50)
C
C.. External Calls ..
      external BFAC
C
C ... Executable Statements ...
C
      do i = 1,nphi-1
       bjphi(i) = -phi(i+1)
      end do
      nbjphi = nphi - 1
      do i = 1,ntheta-1
       bjtheta(i) = -theta(i+1)
      end do
      nbjtheta = ntheta - 1
*      ndum1 = 24
      ndum = 24
      ndum3 = 24
c      WRITE(Mtprof,*)'  subroutine GETTHVARIANCE, call 1,nbjphi = ',
c     &               nbjphi
      call BFAC(bjphi,bjtheta,nbjphi,nbjtheta,ndum,dum,dum1,var,varinn,
     $          dum3,ndum3)
      end
C
C FUNCTION TO COMPUTE THE MEAN OF X SERIES
C
      double precision function DMU(x,nstart,nend)
C
C.. Implicits ..
      implicit none
      real*8 ZERO
      parameter(ZERO=0.0d0)
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 x(*)
C.. In/Out Status: Read, Not Written ..
      integer nstart
C.. In/Out Status: Read, Not Written ..
      integer nend
C
C.. Local Scalars ..
      integer i
C
C.. Intrinsic Functions ..
      intrinsic DBLE
C
C ... Executable Statements ...
C
      DMU = ZERO
      do i = nstart,nend
       DMU = DMU + x(i)
      end do
      DMU = DMU / DBLE(nend-nstart+1)
      end
CC
C
CC
      subroutine SEBARTLETTACF (nz,nztr,nzs,nzsa,acflen,nzlen,bsetr,
     &                          bses,bsesa,bsecyc,bseir,qt1)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      integer mc,kp
      real*8 ZERO,ONE,TWO
      parameter(ZERO = 0.0d0, ONE = 1.0d0, TWO = 2.0d0, mc = 1000,
     &          kp = PFCST)
C
C.. Formal Arguments ..
      integer nzlen,acflen,nz,nztr,nzs,nzsa
      real*8 bsetr(0:kp),bses(0:kp),bsesa(0:kp),bsecyc(0:kp),
     &       bseir(0:kp),qt1
C
C.. Local Scalars ..
      integer j,i
c      integer iminusj,iplusj
      real*8 sum
      real*8 star
C
C.. Local Arrays ..
      real*8 dum(-mc : mc)
C
C..
      include 'acfst.i'
      include 'models.i'
cc
c Compute Bartelett SE for Trend ACF
cc
      star = -9999
      sum = ZERO
      do j = 0, nzlen
       bsetr(j) = ZERO
      end do
      if (Nthetp .gt. 1) then
       do j = 1,acflen
        sum = sum + Acfper(j)*Acfper(j)
       end do
       sum = (sum * TWO + ONE) * Acfper(0) * Acfper(0) * TWO 
       sum = sum / dble(nztr)
       if (sum .lt. ZERO) then
        bsetr(0) = star
       else
        bsetr(0) = sqrt(sum)
       end if
       do j = 1, acflen
        dum(-j) = Acfper(j)
        dum(j) = Acfper(j)
       end do
       dum(0) = ONE
       do j = 1, nzlen
        sum = ZERO
        do i = -acflen+j, acflen-j
         sum = dum(i)*dum(i) + dum(i+j)*dum(i-j) + TWO*dum(j)*dum(j)*
     &         dum(i)*dum(i) - 4.0d0*dum(j)*dum(i)*dum(i-j) + sum
        end do
        sum = sum / dble(nztr)
        if (sum .lt. ZERO) then
         bsetr(j) = star
        else
         bsetr(j) = sqrt(sum)
        end if
       end do
      end if
cc
c Compute Bartelett SE for Seasonal ACF
cc
      do j = 0, nzlen
       bses(j) = ZERO
      end do
       sum = ZERO
       do j = 1,acflen
        sum = sum + Acfser(j)*Acfser(j)
       end do
       sum = (sum * TWO + ONE) * Acfser(0) * Acfser(0) * TWO 
       sum = sum / dble(nzs)
       if (sum .lt. ZERO) then
        bses(0) = star
       else
        bses(0) = sqrt(sum)
       end if
       do j = 1, acflen
        dum(-j) = Acfser(j)
        dum(j) = Acfser(j)
       end do
       dum(0) = ONE
       do j = 1, nzlen
        sum = ZERO
        do i = -acflen+j, acflen-j
         sum = dum(i)*dum(i) + dum(i+j)*dum(i-j) + TWO*dum(j)*dum(j)*
     &         dum(i)*dum(i) - 4.0d0*dum(j)*dum(i)*dum(i-j) + sum
        end do
        sum = sum / dble(nzs)
        if (sum .lt. ZERO) then
         bses(j) = star
        else
         bses(j) = sqrt(sum)
        end if
       end do
cc
c Compute Bartelett SE for SA ACF
cc
      do j = 0, nzlen
       bsesa(j) = ZERO
      end do
      if (Nthets .gt. 1) then
       sum = ZERO
       do j = 1,acflen
        sum = sum + Acfaer(j)*Acfaer(j)
       end do
       sum = (sum * TWO + ONE) * Acfaer(0) * Acfaer(0) * TWO 
       sum = sum / dble(nzsa)
       if (sum .lt. ZERO) then
        bsesa(0) = star
       else
        bsesa(0) = sqrt(sum)
       end if
       do j = 1, acflen
        dum(-j) = Acfaer(j)
        dum(j) = Acfaer(j)
       end do
       dum(0) = ONE
       do j = 1, nzlen
        sum = ZERO
        do i = -acflen+j, acflen-j
         sum = dum(i)*dum(i) + dum(i+j)*dum(i-j) + TWO*dum(j)*dum(j)*
     &         dum(i)*dum(i) - 4.0d0*dum(j)*dum(i)*dum(i-j) + sum
        end do
        sum = sum / dble(nzsa)
        if (sum .lt. ZERO) then
         bsesa(j) = star
        else
         bsesa(j) = sqrt(sum)
        end if
       end do
      end if
cc
c Compute Bartelett SE for IRREGULAR ACF
cc
      do j = 0, nzlen
       bseir(j) = ZERO
      end do
      if (qt1.ne.ZERO) then
       sum = ZERO
       do j = 1,acflen
        sum = sum + Acfier(j)*Acfier(j)
       end do
       sum = (sum * TWO + ONE) * Acfier(0) * Acfier(0) * TWO 
       sum = sum / dble(nz)
       if (sum .lt. ZERO) then
        bseir(0) = star
       else
        bseir(0) = sqrt(sum)
       end if
       do j = 1, acflen
        dum(-j) = Acfier(j)
        dum(j) = Acfier(j)
       end do
       dum(0) = ONE
       do j = 1, nzlen
        sum = ZERO
        do i = -acflen+j, acflen-j
         sum = dum(i)*dum(i) + dum(i+j)*dum(i-j) + TWO*dum(j)*dum(j)*
     &         dum(i)*dum(i) - 4.0d0*dum(j)*dum(i)*dum(i-j) + sum
        end do
        sum = sum / dble(nz)
        if (sum .lt. ZERO) then
         bseir(j) = star
        else
         bseir(j) = sqrt(sum)
        end if
       end do
      endif
cc
c Compute Bartelett SE for TRANSITORY ACF
cc
      do j = 0, nzlen
       bsecyc(j) = ZERO
      end do
      if (Nthetc .gt. 1) then
       sum = ZERO
       do j = 1,acflen
        sum = sum + Acfcer(j)*Acfcer(j)
       end do
       sum = (sum * TWO + ONE) * Acfcer(0) * Acfcer(0) * TWO 
       sum = sum / dble(nz)
       if (sum .lt. ZERO) then
        bsecyc(0) = star
       else
        bsecyc(0) = sqrt(sum)
       end if
       do j = 1, acflen
        dum(-j) = Acfcer(j)
        dum(j) = Acfcer(j)
       end do
       dum(0) = ONE
       do j = 1, nzlen
        sum = ZERO
        do i = -acflen+j, acflen-j
         sum = dum(i)*dum(i) + dum(i+j)*dum(i-j) + TWO*dum(j)*dum(j)*
     &         dum(i)*dum(i) - 4.0d0*dum(j)*dum(i)*dum(i-j) + sum
        end do
        sum = sum / dble(nz)
        if (sum .le. 0d0) then
          sum=1.0d-8
        end if
        bsecyc(j) = sqrt(sum)
       end do
      end if
      return
      end
C
CC
C
CC
      subroutine SEBARTLETTCC (nzlen,acflen,crpsem,crpcem,crpiem,
     &                         crscem,crsiem,crciem,bseps,bsepc,
     &                         bsepi,bsesc,bsesi,bseci,qt1,numSer)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      integer mc
      real*8 ZERO,ONE,TWO
      parameter(ZERO = 0.0d0, ONE = 1.0d0, TWO = 2.0d0, mc = 1000)
C
C.. Formal Arguments ..
      integer acflen,nzlen,numSer
      real*8 crpsem (-mc:mc),crpcem(-mc:mc),crpiem(-mc:mc),
     &       crscem(-mc:mc),crsiem(-mc:mc),crciem(-mc:mc)
      real*8 bseps,bsepc,bsepi,bsesc,bsesi,bseci,qt1
C
C.. Local Scalars ..
      integer j,i
c      integer iminusj,iplusj
      real*8 sum
      real*8 star
C
C.. Local Arrays ..
      real*8 dum(-mc : mc)
      real*8 dum1(-mc : mc)
C
C..
      include 'acfst.i'
      include 'models.i'
cc
c Compute Bartelett SE for Trend ACF
cc
      star = -9999
      bseps = ZERO
      bsepc = ZERO
      bsepi = ZERO
      bsesc = ZERO
      bsesi = ZERO
      bseci = ZERO
      if (Nthetp .gt. 1) then 
cc
c Compute SE Trend-Seasonal
cc
       if (Nthets .gt. 1) then
        do j = 1, acflen
         dum(-j) = Acfper(j)
         dum(j) = Acfper(j)
         dum1(-j) = Acfser(j)
         dum1(j) = Acfser(j)
        end do
        dum(0) = ONE
        dum1(0) = ONE
        sum = ZERO
        do i = -acflen, acflen 
         sum = sum + dum(i)*dum1(i) + crpsem(i)*crpsem(-i) +
     &         crpsem(0)*crpsem(0) * (crpsem(i)*crpsem(i) + 
     &         (dum1(i)*dum1(i)) / TWO + (dum(i)*dum(i)) /TWO ) -
     &         TWO*crpsem(0)* (dum(i)*crpsem(i)+dum1(i)*crpsem(-i))
        end do
        if (sum .lt. ZERO) then
         bseps = star
        else
         bseps = sqrt(sum/dble(nzlen))
        end if
       end if
cc
c Compute SE Trend-Transitory
cc
       if (Nthetc .gt. 1) then
        do j = 1, acflen
         dum(-j) = Acfper(j)
         dum(j) = Acfper(j)
         dum1(-j) = Acfcer(j)
         dum1(j) = Acfcer(j)
        end do
        dum(0) = ONE
        dum1(0) = ONE
        sum = ZERO
        do i = -acflen, acflen 
         sum = sum + dum(i)*dum1(i) + crpcem(i)*crpcem(-i) +
     &         crpcem(0)*crpcem(0) * (crpcem(i)*crpcem(i) + 
     &         (dum1(i)*dum1(i)) / TWO + (dum(i)*dum(i)) /TWO) -
     &         TWO*crpcem(0)* (dum(i)*crpcem(i)+dum1(i)*crpcem(-i))
        end do
        if (sum .lt. ZERO) then
         bsepc = star
        else
         bsepc = sqrt(sum/dble(nzlen))
        end if
       end if
cc
c Compute SE Trend-Irregular
cc
       if (qt1.ne.ZERO) then
        do j = 1, acflen
         dum(-j) = Acfper(j)
         dum(j) = Acfper(j)
         dum1(-j) = Acfier(j)
         dum1(j) = Acfier(j)
        end do
        dum(0) = ONE
        dum1(0) = ONE
        sum = ZERO
        do i = -acflen, acflen 
         sum = sum + dum(i)*dum1(i) + crpiem(i)*crpiem(-i) +
     &         crpiem(0)*crpiem(0) * (crpiem(i)*crpiem(i) + 
     &         0.5d0*dum1(i)*dum1(i) + 0.5d0*dum(i)*dum(i))-
     &         TWO*crpiem(0)* (dum(i)*crpiem(i)+dum1(i)*crpiem(-i))
        end do
        if (sum .lt. ZERO) then
         bsepi = star
        else
         bsepi = sqrt(sum/dble(nzlen))
        end if
       end if
      end if
cc
c
cc
      if (Nthets .gt. 1.and.numser.le.5) then 
cc
c Compute SE Seasonal-Transitory
cc
       if (Nthetc .gt. 1) then
        do j = 1, acflen
         dum(-j) = Acfser(j)
         dum(j) = Acfser(j)
         dum1(-j) = Acfcer(j)
         dum1(j) = Acfcer(j)
        end do
        dum(0) = ONE
        dum1(0) = ONE
        sum = ZERO
        do i = -acflen, acflen 
         sum = sum + dum(i)*dum1(i) + crscem(i)*crscem(-i) +
     &         crscem(0)*crscem(0) * (crscem(i)*crscem(i) + 
     &         0.5d0*dum1(i)*dum1(i) + 0.5d0*dum(i)*dum(i)) -
     &         TWO*crscem(0)* (dum(i)*crscem(i)+dum1(i)*crscem(-i))
        end do
        if (sum .lt. ZERO) then
         bsesc = star
        else
         bsesc = sqrt(sum/dble(nzlen))
        end if
       end if
cc
c Compute SE Seasonal-Irregular
cc
       if (qt1.ne.ZERO) then
        do j = 1, acflen
         dum(-j) = Acfser(j)
         dum(j) = Acfser(j)
         dum1(-j) = Acfier(j)
         dum1(j) = Acfier(j)
        end do
        dum(0) = ONE
        dum1(0) = ONE
        sum = ZERO
        do i = -acflen, acflen 
         sum = sum + dum(i)*dum1(i) + crsiem(i)*crsiem(-i) +
     &         crsiem(0)*crsiem(0) * (crsiem(i)*crsiem(i) + 
     &         (dum1(i)*dum1(i)) / TWO + (dum(i)*dum(i)) /TWO) -
     &         TWO*crsiem(0)* (dum(i)*crsiem(i)+dum1(i)*crsiem(-i))
        end do
        if (sum .lt. ZERO) then
         bsesi = star
        else
         bsesi = sqrt(sum/dble(nzlen))
        end if
       end if
      end if
cc
c
cc
      if ((Nthetc .gt. 1).and.(numSer.le.5).and.(qt1.ne.ZERO)) then 
cc
c Compute SE Transitory-Irregular
cc
       do j = 1, acflen
        dum(-j) = Acfcer(j)
        dum(j) = Acfcer(j)
        dum1(-j) = Acfier(j)
        dum1(j) = Acfier(j)
       end do
       dum(0) = ONE
       dum1(0) = ONE
       sum = ZERO
       do i = -acflen, acflen 
        sum = sum + dum(i)*dum1(i) + crciem(i)*crciem(-i) +
     &        crciem(0)*crciem(0) * (crciem(i)*crciem(i) + 
     &        (dum1(i)*dum1(i)) / TWO + (dum(i)*dum(i)) /TWO) -
     &        TWO*crciem(0)* (dum(i)*crciem(i)+dum1(i)*crciem(-i))
       end do
       if (sum .lt. ZERO) then
        bseci = star
       else
        bseci = sqrt(sum/dble(nzlen))
       end if
      end if

      return
      end
C
C..  Extensively modified by REG on 31 Aug 2005 in order to reduce
C    the amount of repeated code. A new subroutine getUnderOverClass 
C    was added to handle the repeated code. Comments were also added.
      subroutine UnderOverTest(Mq,bseps,bsepc,bsepi,bsesc,bsesi,bseci,
     $                         qt1,numSer)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      integer kp, mc
      real*8 ZERO
      parameter (ZERO = 0.0d0, kp = PFCST, mc = 1000)
C
C.. Formal Arguments ..
      integer Mq,numSer
      real*8 bseps,bsepc,bsepi,bsesc,bsesi,bseci,qt1
C.. Local Scalars ..
      character uotest*6, uotest1*6, uotest2*6
      integer nstar,ncomp,inc
C
C.. Common include
      include 'stream.i'
      include 'models.i'
      include 'acfst.i'
      include 'bartlett.i'
      include 'cross.i'
      include 'transcad.i'
      include 'htmlout.cmn'
cc
cc Output over/under title
cc
      CALL genSkip(1000)
      CALL writTagOneLine(Nio,'h3','@','SECOND ORDER MOMENTS '//
     $                     'OF THE (STATIONARY) COMPONENTS '//
     $                      'OVER / UNDER ESTIMATION TESTS')
      CALL writTagOneLine(Nio,'h4','@','1. VARIANCE')
      CALL mkTableTag(Nio,'w50','@')
      CALL mkCaption(Nio,'1. VARIANCE')
cc
cc Output subtest title for Variance test
cc
cc
c Trend Variance Over/Under estimation
cc
      nstar = 0
      call getUnderOverClass( nstar, Acfpem(0), Acfper(0), bsetr(0),
     $                                    uotest )
      if (nthetp .gt. 1) then
       write (Nio,1010)'TREND-CYCLE',uotest
 1010  FORMAT('<tr><th>',a,'</th><td class="center">',a,'</td></tr>')
      end if
cc
c Seasonal Variance Over/Under estimation
cc
      call getUnderOverClass( nstar, Acfsem(0), Acfser(0), bses(0),
     $                                    uotest )
      if (nthets .gt. 1) then
       write (Nio,1010)'SEASONAL',uotest
      end if
cc
c Transitory Variance Over/Under estimation
cc
      call getUnderOverClass( nstar, Acfcem(0), Acfcer(0), bsecyc(0),
     $                                    uotest )
      if (nthetc .gt. 1) then
       write (Nio,1010)transLcad(1:nTransLcad),uotest
      end if
cc
c Irregular Variance Over/Under estimation
cc
      if (qt1.ne.ZERO) then 
       call getUnderOverClass( nstar, Acfiem(0), Acfier(0), bseir(0),
     $                                    uotest )
       write (Nio,1010)'IRREGULAR',uotest
      end if
      CALL writTag(nio,'</table>')
      CALL mkPOneLine(Nio,'@','&nbsp;')
cc
cc Output table of class definitions
cc
      CALL writTagClass(Nio,'ul','nob')
      write (Nio,1020)'+&nbsp;+','Overestimation of component.'//
     $                     ' Strong evidence (t&gt;3).'
      write (Nio,1020)' +','Overestimation of component.'//
     $                     ' Mild evidence (2&lt;t&lt;3).'
      write (Nio,1020)'-&nbsp;-','Underestimation of component.'//
     $                     ' Strong evidence (t&lt;-3).'
      write (Nio,1020)' -','Underestimation of component.'//
     $                     ' Mild evidence (-3&lt;t&lt;-2).'
      if (nstar .gt. 0) then
       write (Nio,1020)'**','unreliable test.'
      endif
      CALL writTag(nio,'</ul>')
 1020 FORMAT('<li><em>',a,' :</em> ',a,'</li>')
cc
cc Output subtest title for Autocorrelation test
cc
      CALL writTagOneLine(Nio,'h4','@','2. AUTOCORRELATION')
      CALL mkTableTag(Nio,'w50','@')
      CALL mkCaption(Nio,'2. AUTOCORRELATION')

      write (Nio,1030)Cbr,Cbr
 1030 FORMAT('<tr><th>&nbsp;</th><th scope="col">FIRST ORDER ',a,
     $       ' AUTOCORRELATION</th>',/,
     $       '<th scope="col">SEASONAL ORDER ',a,' AUTOCORRELATION',
     $       '</th></tr>')
cc
c Trend first order
cc
      nstar = 0
      call getUnderOverClass( nstar, Acfpem(1), Acfper(1), bsetr(1),
     $                                    uotest )
cc
c Trend seasonal order
cc
      call getUnderOverClass( nstar, Acfpem(Mq), Acfper(Mq), bsetr(Mq),
     $                                    uotest1 )
      if (nthetp .gt. 1) then
       write (Nio, 1040) 'TREND-CYCLE', uotest, uotest1
 1040  FORMAT('<tr><th scope="row">',a,'</th><td class="center">',a,
     &        '</td><td class="center">',a,'</td></tr>') 
      end if
cc
c Seasonal first order
cc
      call getUnderOverClass( nstar, Acfsem(1), Acfser(1), bses(1),
     $                                    uotest )
cc
c Seasonal seasonal order
cc
      call getUnderOverClass( nstar, Acfsem(Mq), Acfser(Mq), bses(Mq),
     $                                    uotest1 )
      if (nthets .gt. 1) then
       write (Nio, 1040) 'SEASONAL', uotest, uotest1
      end if
cc
c Transitory first order
cc
      call getUnderOverClass( nstar, Acfcem(1), Acfcer(1), bsecyc(1),
     $                                    uotest )
cc
c Transitory seasonal order
cc
      call getUnderOverClass( nstar, Acfcem(Mq), Acfcer(Mq), bsecyc(Mq),
     $                                    uotest1 )
      if (nthetc .gt. 1) then
       write (Nio, 1040) transLcad(1:nTransLcad), uotest, uotest1
      end if
      if (qt1.ne.ZERO) then
cc
c Irregular first order
cc
       call getUnderOverClass( nstar, Acfiem(1), Acfier(1), bseir(1),
     $                                    uotest )
cc
c Irregular seasonal order
cc
       call getUnderOverClass( nstar, Acfiem(Mq), Acfier(Mq), bseir(Mq),
     $                                    uotest1 )
cc
cc Output table of class definitions
cc
       write (Nio, 1040) 'IRREGULAR', uotest, uotest1
      end if
      CALL writTag(nio,'</table>')
      CALL mkPOneLine(Nio,'@','&nbsp;')
      CALL writTagClass(Nio,'ul','nob')
      write (Nio, 1020) '+&nbsp;+','Too much positive correlation.'//
     $                       ' Strong evidence (t&gt;3).'
      write (Nio, 1020) '+ ','Too much positive correlation.'//
     $                       ' Mild evidence (2&lt;t&lt;3).'
      write (Nio, 1020) '-&nbsp;-','Too much negative correlation.'//
     $                       ' Strong evidence (t&lt;-3).'
      write (Nio, 1020) '- ','Too much negative correlation.'//
     $                       ' Mild evidence (-3&lt;t&lt;-2).'
      if (nstar .gt. 0) then
       write (Nio, 1020) '**','unreliable test.'
      endif
      CALL writTag(nio,'</ul>')
cc
cc Output subtest title for Cross-correlation test
cc
      nstar = 0
c contar numero de componentes 
      ncomp = 0
      if (Nthetc .gt.1) then
       ncomp = 1
      end if
      if (Nthets .gt.1) then
       ncomp = ncomp+1
      end if
      if (Nthetp.gt.1) then
       ncomp = ncomp+1
      end if
      if(qt1.ne.0.d0) then
       ncomp = ncomp+1
      end if
      if (ncomp.gt.1) then
       CALL writTagOneLine(Nio,'h4','@','3. CROSSCORRELATION')
       CALL mkTableTag(Nio,'w50','@')
       CALL mkCaption(Nio,'3. CROSSCORRELATION')
       CALL writTag(Nio,'<tr>')
       CALL mkTableCell(Nio,'head','&nbsp;')
       inc=1
       if (Nthets .gt. 1) then
        CALL mkHeaderCellScope(Nio,0,0,'col','@','SEASONAL')
        inc=inc+1
       endif
       if (Nthetc .gt. 1) then
        CALL mkHeaderCellScope(Nio,0,0,'col','@','TRANSITORY')
        inc=inc+1
       endif
       if (qt1.ne.ZERO) then
        CALL mkHeaderCellScope(Nio,0,0,'col','@','IRREGULAR')
        inc=inc+1
       endif
       CALL writTag(Nio,'</tr>')
       nstar = 0
       if (Nthetp .gt. 1) then
        if (Nthets .gt. 1) then
         call getUnderOverClass( nstar, crpsem(0), crpser(0), bseps,
     $                                      uotest )
        endif
        if (Nthetc .gt. 1 .and. numSer.le.5) then
         call getUnderOverClass( nstar, crpcem(0), crpcer(0), bsepc,
     $                                      uotest2 )
        endif
        if (qt1.ne.ZERO) then
         call getUnderOverClass( nstar, crpiem(0), crpier(0), bsepi,
     $                                     uotest1 )
        endif
        CALL writTag(Nio,'<tr>')
        CALL mkHeaderCellScope(Nio,0,0,'row','@','TREND-CYCLE')
        if (Nthets .gt. 1) CALL mkTableCell(Nio,'center',uotest)
        if (Nthetc .gt. 1) CALL mkTableCell(Nio,'center',uotest2)
        if (qt1.ne.ZERO)  CALL mkTableCell(Nio,'center',uotest1)
        CALL writTag(Nio,'</tr>')
       endif

       if (Nthets .gt. 1) then
        if (Nthetc .gt. 1 .and. numSer.le.5) then
         call getUnderOverClass( nstar, crscem(0), crscer(0), bsesc,
     $                                      uotest )
        endif
        if (qt1.ne.ZERO) then
         call getUnderOverClass( nstar, crsiem(0), crsier(0), bsesi,
     $                                     uotest1 )
        endif
c Modified by REG on 27 Apr 2006 to correct output bug.
        CALL writTag(Nio,'<tr>')
        CALL mkHeaderCellScope(Nio,0,0,'row','@','SEASONAL')
        if (Nthetc .gt. 1 .and. numser.le.5)
     &     CALL mkTableCell(Nio,'center',uotest)
        if (qt1.ne.ZERO) CALL mkTableCell(Nio,'center',uotest1)
        CALL mkTableCell(Nio,'center','&nbsp;')
        CALL writTag(Nio,'</tr>')
       endif
      
c Modified by REG on 27 Apr 2006 to correct output bug.
       if (Nthetc .gt. 1 .and. numser.le.5) then
        CALL writTag(Nio,'<tr>')
        CALL mkHeaderCellScope(Nio,0,0,'col','@','TRANSITORY')
        call getUnderOverClass( nstar, crciem(0), crcier(0), bseci,
     $                                     uotest )
        CALL mkTableCell(Nio,'center',uotest)
        CALL mkTableCell(Nio,'center','&nbsp;')
        CALL mkTableCell(Nio,'center','&nbsp;')
        CALL writTag(Nio,'</tr>')
       endif 
cc
cc Output table of class definitions
cc
       CALL writTag(nio,'</table>')
       CALL mkPOneLine(Nio,'@','&nbsp;')
       CALL writTagClass(Nio,'ul','nob')
       write (Nio,1020) '++','Too much positive crosscorrelation.'//
     $                       ' Strong evidence (t&gt;3).'
       write (Nio,1020) ' +','Too much positive crosscorrelation.'//
     $                       ' Mild evidence (2&lt;t&lt;3).'
       write (Nio,1020) '--','Too much negative crosscorrelation.'//
     $                       ' Strong evidence (t&lt;-3).'
       write (Nio,1020) ' -','Too much negative crosscorrelation.'//
     $                       ' Mild evidence (-3&lt;t&lt;-2).'
       if (nstar .gt. 0) write (Nio,1020)'**','unreliable test.'
       CALL writTag(nio,'</ul>')
      end if
      return
      end

c-----------------------------------------------------------------------
c     The following subroutine supports the UnderOverTest subroutine
c     by determining the class (one of six) of an estimate 
c     with respect to its estimator (expected value of estimate)
c     and bse (Bartlett standard error).
c-----------------------------------------------------------------------
      subroutine getUnderOverClass( nstar, estimate, estimator, bse,
     $                                            uotest )
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c bse       d     Bartlett standard error
c estimate d      estimate to be classified
c estimator d     expected value of estimate
c nstart    i     incremented to indicate that class could not be determined
c                  since bse is negative
c uotest    c     output class from '(**)', 'ok', '-', '+', '--', '++'
c-----------------------------------------------------------------------
      implicit none
      real*8 ZERO,TWO,THREE
      parameter(ZERO=0.0d0,TWO=2.0d0,THREE=3.0d0)
      integer nstar
      real*8 bse, estimate, estimator
      character uotest*6

c-----------------------------------------------------------------------
c     The following commented code has been converted to the code below.
c-----------------------------------------------------------------------
c     if (bsetr(0) .lt. ZERO) then
c      nstar = nstar + 1
c      uotest = ' (**) '
c     else
c      if ((Acfpem(0) .ge. Acfper(0) - 2.0d0 * bsetr(0)) .and.
c    $    (Acfpem(0) .le. Acfper(0) + 2.0d0 * bsetr(0))) then
c       uotest = ' OK '
c      else if ((Acfpem(0) .gt. Acfper(0) + 2.0d0 * bsetr(0)) .and.
c    $         (Acfpem(0) .le. Acfper(0) + 3.0d0 * bsetr(0))) then
c       uotest = '  +  '
c      else if ((Acfpem(0) .ge. Acfper(0) - 3.0d0 * bsetr(0)) .and.
c    $         (Acfpem(0) .lt. Acfper(0) - 2.0d0 * bsetr(0))) then
c       uotest = '  -  '
c      else if (Acfpem(0) .gt. Acfper(0) + 3.0d0 * bsetr(0)) then
c       uotest = '  ++  '
c      else if (Acfpem(0) .lt. Acfper(0) - 3.0d0 * bsetr(0)) then
c       uotest = '  --  '
c      end if
c     end if
c-----------------------------------------------------------------------

      if (bse .lt. ZERO) then
       nstar = nstar + 1
       uotest = ' (**) '
      else
       if ((estimate .ge. estimator - TWO * bse) .and.
     $     (estimate .le. estimator + TWO * bse)) then
        uotest = ' OK '
       else if ((estimate .gt. estimator + TWO * bse) .and.
     $          (estimate .le. estimator + THREE * bse)) then
        uotest = '  +  '
       else if ((estimate .ge. estimator - THREE * bse) .and.
     $          (estimate .lt. estimator - TWO * bse)) then
        uotest = '  -  '
       else if (estimate .gt. estimator + THREE * bse) then
        uotest = '  ++  '
       else if (estimate .lt. estimator - THREE * bse) then
        uotest = '  --  '
       end if
      end if
      end
c-----------------------------------------------------------------------
c
cc
*      subroutine DSOUT(nio,freq,DetSeas,lamda)
*C 
*C.. Implicits .. 
*       implicit none
*C 
*C.. Formal Arguments .. 
*       integer nio,freq,lamda
*      real*8 DetSeas(freq)
*C 
*C.. Local Variables .. 
*      character frmt(7)*3,num(12)*3,frmt1(8)*3
*C 
*C.. Local Arrays .. 
*       integer i
*       character*4 Month(12),Per(12)
*       data Month/
*     &     'Jan ','Feb ','Mar ','Apr ','May ','Jun','Jul','Aug ','Sep',
*     &     'Oct ','Nov ','Dec '/
*       data Per/
*     &     '1st','2nd','3rd','4th','5th','6th','7th','8th','9th','10th',
*     &     '11th','12th'/
*       data frmt/'(','N2','X,','N1','(6x','a4','))'/
*       data frmt1/'(','N2','X,','N1','(x,','F9','.3','))'/
*       data num/'1','2','3','4','5','6','7','8','9','10','11','12'/
*       frmt(2)='4'
*       frmt(4)=num(freq)
*       if (freq .eq.12) then
*        write(nio,frmt)(Month(i),i=1,freq)
*       else
*        write(nio,frmt)(Per(i),i=1,freq)
*       end if
*       frmt1(2)='4'
*       frmt1(4)=num(freq)
*       if (lamda .eq.1) then
*         write(nio,frmt1)(DetSeas(i),i=1,freq)
*       else
*         write(nio,frmt1)(Dexp(DetSeas(i))*100.0d0,i=1,freq)
*       end if
*       return
*      end
c-----------------------------------------------------------------------
