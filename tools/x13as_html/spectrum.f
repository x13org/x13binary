c     change variable name test to test2 to avoid conflit with common
c     block name test in test.i --Mar. 2021
C     Last change:      REG  22 Dec 2005
C     Previous change:  BCM  16 May 2003    4:26 pm
C       THIS SUBROUTINE COMPUTES THE HARMONIC FUNCTIONS FOR THE COMPONENTS,
C       THE FILTER DENOMINATORS, THE NUMERATOR OF THE COMPONENT MODELS
C     AND THEIR INNOVATION VARIANCES :
C
C       CT : trend filter
C       CS : seasonal filter
C       CC : cycle filter
C
C
C  SET UP FACTORS OF SPECTRUM
C  THE PARAMETER "PRFRA"  HAS BEEN DELETED
C
C        ***************HARMONIC FUNCTIONS*****************
C
C     FF = THSTAR X THSTAR = MOVING AVERAGE
C
C     FT = CHI X CHI =  TREND
C     FC = CYC X CYC =  CYCLE
C     FS = PSI X PSI =  SEASONAL
C     FN = FT * FC   =  CYCLE-TREND (NONSEASONAL)
C     FH = FN * FS   =  TOTAL DENOMINATOR
C
C     INPUT PARAMETERS
C       NOADMISS : TO APPROXIMATE THE MODEL IF IRREGULAR SPECTRUM IS
C                  NEGATIVE. 1 APPROXXIMATION, O NO APPROXIMATION
C         THSTAR : NUMERATOR OF THE MODEL
C          QSTAR : DIMENSION OF THSTAR
C            CHI : DENOMINATOR OF TREND MODEL
C           NCHI : DIMENSION OF CHI
C            CYC : DENOMINATOR OF CYCLE MODEL
C           NCYC : DIMENSION OF CYC
C            PSI : DENOMINATOR OF SEASONAL MODEL
C           NPSI : DIMENSION OF PSI
C          CHCYC : DENOMINATOR OF TREND*CYCLE MODEL
C         NCHCYC : DIMENSION OF CHCYC
C          PSCYC : DENOMINATOR OF SEASONAL*CYCLE MODEL (ADDED DEKM 6 Feb 2003)
C         NPSCYC : DIMENSION OF PSCYC
C          CHPSI :  DENOMINATOR OF TREND*SEASONAL MODEL (ADDED DEKM 20 Feb 2003)
C         NCHPSI : DIMENSION OF CHPSI
C          PSTAR : DIMENSION OF THE MODEL DENOMINATOR
C             MQ : FREQUENCY
C          TITLE : NAME OF THE SERIES
C             HS : HEIGH OF SPECTRUM (FOR GRAPHS)
C             BD : DELTA^MQ DIMENSION
C             D  : DELTA DIMENSION
C             CT : TREND DENOMINATOR FILTER (OUTPUT)
C             CS : SEASONAL DENOMINATOR FILTER (OUTPUT)
C             CC : CYCLE DENOMINATOR FILTER (OUTPUT)
C            QT1 : SPECTRUM OF IRREGULAR (VARIANCE) (OUTPUT)
C            SQG : STANDARD ERROR OF RESIDUALS
C            OUT : TO CONTROL THE PRINTOUT
C          WVARA :  ****** NOT USED ******
C          THETP : NUMERATOR OF TREND MODEL (OUTPUT)
C         NTHETP : DIMENSION OF THETP (OUTPUT)
C          THETS : NUMERATOR OF SEASONAL MODEL (OUTPUT)
C         NTHETS : DIMENSION OF THETS (OUTPUT)
C          THETC : NUMERATOR OF CYCLE MODEL (OUTPUT)
C         NTHETC : DIMENSION OF THETC (OUTPUT)
C          THADJ : NUMERATOR OF SEASONALLY ADJUSTED MODEL (OUTPUT)
C         NTHADJ : DIMENSION OF THADJ (OUTPUT)
C          THTHA : NUMERATOR OF TREND ADJUSTED MODEL (OUTPUT) (ADDED by DEKM FEB 6 2003)
C         NTHTHA : DIMENSION OF THTHA (OUTPUT)
C          THCYA : NUMERATOR OF CYCLE ADJUSTED MODEL (OUTPUT) (ADDED by DEKM FEB 20 2003)
C         NTHCYA : DIMENSION OF THCYA (OUTPUT)
C         VARWNP : INNOVATIONS VARIANCE OF TREND (OUTPUT)
C         VARWNS : INNOVATIONS VARIANCE OF SEASONAL (OUTPUT)
C         VARWNC : INNOVATIONS VARIANCE OF CYCLE (OUTPUT)
C         VARWNA : INNOVATIONS VARIANCE OF SEASONALLY ADJUSTED (OUTPUT)
C         VARWNT : INNOVATIONS VARIANCE OF TREND ADJUSTED (OUTPUT) (ADDED by DEKM FEB 6 2003)
C         VARWCA : INNOVATIONS VARIANCE OF CYCLE ADJUSTED (OUTPUT) (ADDED by DEKM FEB 20 2003)
C
C
C       SUBROUTINE SPECTRUM(NOADMISS,THSTAR,QSTAR,CHI,NCHI,CYC,
C     $NCYC,PSI,NPSI,CHCYC,NCHCYC,PSTAR,MQ,TITLE,HS,BD,
C     $D,CT,CS,CC,QT1,SQG,PG,OUT,WVARA,NCYCTH,
C     $      THETP,NTHETP,THETS,NTHETS,THETC,NTHETC,THADJ,NTHADJ,
C     $      VARWNP,VARWNS,VARWNC,VARWNA,BUFF2,SMTR,HAR,*)
      subroutine SPECTRUM(noadmiss,OutNA,thstar,qstar,
     $              chi,nchi,cyc,ncyc,psi,npsi,
     $              chcyc,nchcyc,pscyc,npscyc,chpsi,nchpsi,
C added arguments pscyc, npscyc (seasonal-cycle denominator and dimension), 
C ththa, nththa (trend adjusted numerator and it's dimension), 
C varwnt (innovations variance for trend adjusted component)
C DEKM 6 Feb 2003
C added arguments chpsi, nchpsi (trend-seasonal denominator and dimension), 
C thcya, nthcya (cycle adjusted numerator and it's dimension), 
C varwca (innovations variance for cycle adjusted component)
C DEKM 20 Feb 2003
C
     $              pstar,mq,bd,d,ct,cs,cc,qt1,
     $              sqg,pg,out,ndec,ncycth,thetp,nthetp,thets,nthets,
     $              thetc,nthetc,thadj,nthadj,thtra, nthtra,thcya,
     $              nthcya,varwnp,varwns,
     $              varwnc,varwna,varwnt,varwca,
     $              buff2,har,chis,nchis,psis,npsis,cycs,ncycs,
     $              adjs,nadjs,noserie,iter,sqf,
c             Para OutSeats
     $              IOUT,Ndevice,
     $              printBack,back,sr,SQSTAT,SDF,SSE,mAuto,nfreq,
     $              n_1,n0,tvalRUNS,
     $              Qstat,DF,Pstat1,spstat1,
     $              wnormtes,wsk,skewne,test1,wkk,rkurt,test2,r,SEa,
     $              Resid,flagTstu,it,iper,iyear,
     $           rmean,rstd,DW,KEN,RTVAL,SumSres,F_seats,Nyer1,Nper1,
     $              Pstar_seats,Qstar_seats,
c             Para OutDenC
     $              Titleg,init,
     $              p,q,bp,bq,theta,nTh,Btheta,nBth,
     $              phi,nPhi,Bphi,nBphi,
     $              Chins,
     $              Cycns,
     $              Psins,
     $              Adjns,
     $              Totden,nTot,InputModel,
c             Para OutPar.m
     $              niter,mattitle,Lgraf,
c             Para indicar raices reales
     $              root0c,rootPIc,rootPIs,isUgly,
     $              IsCloseToTD,
c             Para OutPart2
     $              ImeanOut,Wdif,WdifCen,nwDif,WmDifXL,VdifXL,
     $              QstatXL,rXL,seRxl,partACF,sePartACF,model,
     $              PicosXL,tstmean,Wm,seMean,nx,Cmatrix,
     $              sePHI,seTH,seBPHI,seBTH,ph,TH,bph,
     $              MArez,MAimz,MAmodul,MAar,MApr,
     $              rez,imz,modul,ar,pr,
     $              Z,nz,ILam,Nper,Nyer,Zvar,M,BTH,
c             Para OutSearch
     $              ItnSearch,IfnSearch,nxSearch,Esearch,
     $              FIsearch,xSearch,status,NAfijado,tramo,
     $              Lsgud,*)
C
C.. Implicits ..
      implicit none
      logical T,F
      integer n10,n12,n1
      parameter(n1=1,n10=10,n12=12,T=.true.,F=.false.)
c-----------------------------------------------------------------------
c    add include files to define print and save logical vectors, pointer
c    variables  BCM May 2003
c-----------------------------------------------------------------------
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      include 'seattb.i'
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
      include 'stream.i'
      include 'stdio.i'
c-----------------------------------------------------------------------
c    add include file to allow program to print error message to
c    error file BCM Oct 2005
c-----------------------------------------------------------------------
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      real*8 ONE,ZERO
      parameter(ONE=1D0,ZERO=0D0)
c     INPUT PARAMETERS OUTSEATS
      logical printBack,root0c,rootPIc,rootPIs,IsCloseToTD
      integer IOUT,mAuto,DF,SDF
      real*8 resid(mpkp),back(mpkp),Qstat,Pstat1,spstat1
      real*8 sr(50),SQstat,SSE(50),tvalRUNS
      integer n_1,n0,tramo
      real*8 wnormtes,wsk,skewne,test1,wkk,rkurt,test2,r(50),SEa(50)
      integer flagTstu,NDEVICE,IPER,IYEAR,it,Nper1,nYer1
      integer Pstar_seats,Qstar_seats,nfreq
      real*8 Rmean,Rstd,DW,KEN,RTVAL,F_seats,SumSres
c     INPUT PARAMETERS OUTDENC
      integer init,p,q,bp,bq,nTh,nBth,
     $                  nPhi,nBphi,nTot
      character Titleg*80
      real*8 theta(4),Btheta(25),phi(4),Bphi(13),
     $       Chins(8),
     $       Cycns(5),Psins(27),
     $       Adjns(8),Totden(40)
c     InPUT OutPar.m
      character mattitle*180
      character status
      integer niter
c     INPUT/OUTPUT PARAMETERS
      integer InputModel
c
c   INPUT/OUTPUT PARAMETER OutPart2
c   INPUT
      integer ImeanOut,nwDif,model                  
      real*8 Wdif(*),WdifCen(*),WmDifXL,VdifXL
      real*8 QstatXL,rXL(5*n10),seRxl(5*n10),partACF(5*n10),sePartACF
      character PicosXL(7)*2
      integer tstmean,nx
      real*8 Wm,seMean,Cmatrix(n10,n10),
     $       sePHI(n10),seTH(n10),seBPHI(n10),seBTH(n10),
     $       MArez(5*n12+n12/3),MAimz(5*n12+n12/3),MAmodul(5*n12+n12/3),
     $       MAar(5*n12+n12/3),MApr(5*n12+n12/3),
     $       TH(3*n1),pH(3*n1),bph(3*n1)
      real*8 rez(5*n12+n12/3),imz(5*n12+n12/3),modul(5*n12+n12/3),
     $       ar(5*n12+n12/3),pr(5*n12+n12/3),Z(*),Zvar,BTH(3*n1)
      integer nz,ILam,Nper,Nyer,M
      real*8 tmp
      logical Lsgud
C    INPUT PARAMETER OutSearch
      integer ItnSearch,IfnSearch,nxSearch,Esearch(n10)
      real*8 FIsearch,xSearch(n10)
c   OUTPUT
*      integer Itab,IID
c     OUTPUT PARAMETERS
      logical isUgly
C
C.. Formal Arguments ..
      integer noadmiss,OUTna,qstar,nchi,ncyc,npsi,nchcyc,pstar,mq,bd,d,
     $        sqg,pg,out,ndec,ncycth,nthetp,nthets,nthetc,nthadj,npscyc, 
     $        nthtra,har,nchpsi,nthcya,nchis,
     $        npsis,ncycs,nadjs,noserie,iter,NAfijado
C added arguments npscyc, nthtra DEKM Feb 6, 2003
C added arguments nchpsi, nthcya DEKM Feb 20, 2003
      character buff2*80
      real*8 thstar(maxTh),chi(8),cyc(17),psi(27),chcyc(20),
     $       ct(32),cs(32),cc(32),qt1,thetp(8),thets(27),
     $       thetc(32),thadj(32),varwnp,varwns,varwnc,varwna,chis(17),
     $       psis(16),cycs(17),adjs(17),sqf, 
C adding variables (DEKM Feb 6, 2003)
     $       pscyc(32), varwnt, thtra(32),
C adding variables (DEKM Feb 20, 2003)
     $       chpsi(32), varwca, thcya(32)
      LOGICAL Lgraf
C
C.. Local Scalars ..
      integer nn,j,nus,nvn,NoDecompOut,NAiter
c     character fname*30,subtitle*50
c     character auxS*350
c      real*8 arg,x
      real*8 enoc,enot,estar,
     $       qmin
cc
c     (Roberto Lopez: 01/2006. 
c      OUTPUT'S of MinimGrid)      
cc
      integer doMinimGrid
      parameter (doMinimGrid = 1)
C
C.. Local Arrays ..
      real*8 fn(50),us(50),vn(80),dvec(1)
c     $       ,utf(8),y(300)
      character linePol*(MaxLineLength),strPol*(MaxStrlength)
cc
c Used for the new Model Approx.
cc
      real*8 Res(mpkp),thstar1(maxTh),rt(32),qt(32)
      integer Na,i,nrt,nqt
      logical isVa0,svudg
cc
c
cc
C
C.. External Functions ..
      real*8 FUNC0,getSdt,getSds,getSdc,getSdi,getSdsa
      external FUNC0,getSdt,getSds,getSdc,getSdi,getSdsa
      integer ISTRLEN,KnownApprox
      external ISTRLEN,KnownApprox
C
C.. External Calls ..
      external CONJ, CONV, DIVFCN, MAK1, MINIM, MULTFN, PARFRA,
     $         SPCEST, USRENTRY, GLOBALMINIM, MINIMGRID
C
C.. Intrinsic Functions ..
      intrinsic ABS, INT, MAX, MIN
      include 'func.i'
      include 'func2.i'
      include 'func3.i'
      include 'func4.i'
      include 'func5.i'
      include 'hspect.i'
      include 'min.i'
      include 'test.i'
      include 'buffers.i'
      include 'spectra.i'
      include 'dirs.i'
      include 'strmodel.i'
      include 'polynom.i'
      include 'htmlout.cmn'
      include 'hiddn.cmn'
C
c      OUTna=0
CUNX#ifdef PROFILER
!DEC$ IF DEFINED (PROFILER)
*      call profiler(3,'Pre Spectrum')
!DEC$ ENDIF             
CUNX#endif
      svudg=.false.
      IF(Lsumm.gt.0)svudg=.true.
      isUgly=.false.
      isVa0=.false.
      do i=1,qstar
        thstar1(i)=thstar(i)
      enddo
*      call profiler(3,'Pre SPECTRU')
      call SPECTRU(thstar,qstar,chi,nchi,cyc,ncyc,psi,
     $                    npsi,pstar,mq,bd,d,
     $                    qt1,out,ncycth,
     $                    har,
     $                    Fn,NN,estar,enot,enoc,Us,nUS,Vn,nVn,
     $                    root0c,rootPIc,rootPIs,isUgly)
cc
c New experimental Model Approximation
cc
cc  Chequeamos si es uno de los modelos con region de admisibilidad conocida
cc  en cuyo caso fijamos los coeficientes.
      if (Lsgud) 
     $    call showFirstNA(nio,InputModel,p,d,q,bp,bd,bq,theta,
     $                     Btheta,nbth,phi,Bphi,nbphi,imeanout,tramo)	 
      if ((qt1 .lt. ZERO).and.(noadmiss.ne.0)) then
        NAfijado=KnownApprox(p,q,d,bd,bp,bq,init,noadmiss,
     $                       th,bth,ph,bph,mq,status)  
*        write(*,*) 'Within Spectrum, Noadmiss, NAfijado = ', Noadmiss, 
*     &             NAfijado
      end if
      if (Noadmiss.eq.-1 .and. qt1.ge.ZERO)then
        dvec(1)=ZERO
        call USRENTRY(dvec,1,1,1,1,1019)
      else if (((qt1 .lt. ZERO) .and. (isUgly)).and.
     $          (Noadmiss.eq.-1)) then
        if (NAfijado.eq.0) then
        if (out .eq. 0) then
          call wWritln('DECOMPOSITION INVALID, VARIANCE OF '//
     $     'IRREGULAR IS NEGATIVE. THE MODEL IS APPROXIMATED '//
     $     'BY SETTING VAR(irreg)=0. RESIDUALS ARE RE-COMPUTED',
     $                Nio,Mt2,T,T)
        end if       
        call CONJ(thstar,qstar,thstar,qstar,Ff,Nf)
        call CONJ(Totden,Pstar,Totden,Pstar,Fh,Nh)
        do i=nf+1,nh
          Ff(i)=ZERO
        enddo
        do i = 1,Nh
         Ff(i) = Ff(i) + (1.0d-9 -qt1)*Fh(i)
        end do
*        call profiler(3,'Pre MAK1')
        call MAK1 (Ff,max(nf,nh),Thstar,Qstar,qt1,nio,0,
     $             'APPROXIMATED MODEL MA',21,tmp,'approx.ma',9)
*          itab=itab+1
         q=qstar-1
         bq=0
         call setNmq(q)
         call setNmbq(bq)
         isVa0=.true.
        CALL genSkip(1203)
        CALL writTagOneLine(Nio,'h4','@',
     $          '<abbr title="moving average">MA'//
     $          '</abbr> APPROXIMATE MODEL')
        call strPolyn('B    ',THstar,Qstar,1.0D-6,strPol,LinePol)
        call AppendLine(strPol,linePol)
        CALL mkPOneLine(Nio,'@',strPol(1:istrlen(strPol)))
*        call profiler(3,'Pre CALCRES')
        call CALCRES(phi,nPhi,Bphi,nBphi,WDifCen,nfreq,ImeanOut,Totden,
     $               Pstar,Thstar,Qstar,Z,Nz,Resid,back,na,sqf,rmean,
     $               rstd,rtval,wnormtes,skewne,test1,rkurt,test2,
     $               sumSres,dw,F_seats,mAuto,nfreq,r,sea,Qstat,DF,D,
     $               BD,KEN,n_1,n0,tvalRUNS,sr,SQSTAT,SDF,SSE,out)
*        call profiler(3,'Pre SPECTRU')
        call SPECTRU(thstar,qstar,chi,nchi,cyc,ncyc,psi,
     $                      npsi,pstar,mq,bd,d,
     $                      qt1,out,ncycth,
     $                      har,
     $                      Fn,NN,estar,enot,enoc,Us,nUS,Vn,nVn,
     $                      root0c,rootPIc,rootPIs,isUgly)
        call setAna('Y')
        call setTmcs('Y')
        dvec(1)=ONE
c   arreglo provisional
         if (abs(qt1).le.1.0d-2) then
          qt1=ZERO  
         end if
c    
        end if 
      end if
      isUgly=.FALSE. 
c    isUgly=.FALSE. comentar esta línea para activar el quitar componentes con mínimo en 0 o PI
c   (ver rel 369) Spectru puede devolver ISugly=TRUE si hay algun componente con espectro raro.
c
c      qt1=ZERO  ! PAra que trague una descomposición no admisible
      qmin = qt1
      if ((qmin.ge.ZERO .or. OUTna.eq.1)) then
c   De esta manera sólo se escribe el espectro si la descomposición es admisible
c    o es el modelo de entrada
*        call profiler(3,'Pre OutSearch')
*      write(Mtprof,*)' nio = ',nio
*      write(Mtprof,*)' out = ',out
*      write(Mtprof,*)' ItnSearch = ',ItnSearch
*      write(Mtprof,*)' IfnSearch = ',IfnSearch
*      write(Mtprof,*)' FIsearch = ',FIsearch
*      write(Mtprof,*)' nxSearch = ',nxSearch
*      do j=1,nxSearch
*       write(Mtprof,*)' xSearch(',j,'), Esearch(',j,') = ',xSearch(j),
*     *              Esearch(j)
*      end do
       call OutSearch(nio,out,ItnSearch,IfnSearch,FIsearch,
     $                xSearch,nxSearch,Esearch)
*        call profiler(3,'Pre outPart2')
       call OutPart2(nio,z,nz,ILam,ImeanOut,noserie,Pg,Out,Ndec,
     $               iter,p,D,q,bp,BD,bq,Nper,Nyer,mq,
     $               Wdif,WdifCen,nwDif,WmDifXL,Zvar,VdifXL,
     $               QstatXL,df,rXL,seRxl,M,partACF,sePartACF,model,
     $               PicosXL,init,tstmean,Wm,seMean,nx,Cmatrix,
     $               PH,TH,BPH,BTH,sePHI,seTH,seBPHI,seBTH,
     $               MArez,MAimz,MAmodul,MAar,MApr,
     $               rez,imz,modul,ar,pr,THstar,isVa0)
       if (noserie.ne.1) then
*        call profiler(3,'Pre OutSeats')
        Call OutSeats(IOUT,Nio,Ndevice,
     $                printBack,back,sr,SQSTAT,SDF,SSE,mAuto,nfreq,
     $                n_1,n0,tvalRUNS,Qstat,DF,Pstat1,spstat1,
     $                wnormtes,wsk,skewne,test1,wkk,rkurt,test2,r,SEa,
     $                Resid,flagTstu,it,iper,iyear,
     $                rmean,rstd,DW,KEN,RTVAL,SumSres,F_seats,Nyer1,
     $                Nper1,Pstar_seats,Qstar_seats,D,BD)
       end if
*        call profiler(3,'Pre OutDenC1')
       call OutDenC1(Out,Nio,Titleg,
     $               p,d,q,bp,bd,bq,theta,nTh,Btheta,nBth,
     $               phi,nPhi,Bphi,nBphi)
       if ((qmin.ge.ZERO .or. OUTna.eq.1) .and. Lsgud) then
*        call profiler(3,'Pre OutDenCN')
        call OutDenCN(Out,Nio,init,pstar,ThStar,Qstar,
     $                Chis,nChis,Chins,nChins,Chi,nChi,
     $                Cycs,nCycs,Cycns,nCycns,Cyc,nCyc,
     $                Psis,nPsis,Psins,nPsins,Psi,nPsi,
     $                Adjs,nAdjs,Adjns,nAdjns,Chcyc,nChcyc,
     $                Totden,nTot)
       end if
      else
        if (Out.eq.0) then
*        call profiler(3,'Pre ShowNA')
          call ShowNA(Nio,InputModel)
        end if
      end if
*      write(*,*)' qmin = ', qmin
      if (qmin .ge. ZERO) then
       if (IsUgly) then
        p=p-1
        if (q.lt.3) then
         q=q+1
        end if
        if (out .eq. 0) then
          CALL wWritln('DECOMPOSITION INVALID'//Cbr//
     $                 'THE MODEL IS APPROXIMATED',Nio,Mt2,T,T)
        end if       
       else
        if (iter.ne.0) then
*        call profiler(3,'Pre OutPara')
         call OutPara(74,niter,mattitle,NAiter,ImeanOut,
     $          p,d,q,bp,bd,bq,phi,bphi,nbphi,theta,btheta,nbth,
     $          qstat,wm,1)
        end if
*        call profiler(3,'Pre DecompSpectrum')
*        write(*,*) 'Before DecompSpectrum, Noadmiss = ', Noadmiss
        call DecompSpectrum(NOADMISS,NOSERIE,
     $         CHI,nCHI,PSI,nPSI,CYC,nCYC,CHIS,nCHIS,
     $         PSIS,nPSIS,ADJS,nADJS,CYCS,nCYCS,THSTAR,QSTAR,SQF,
     $         ct,cs,cc,Qt1,
     $         SQG,mq,bd,d,PG,OUT,ITER,
     $         estar,enot,enoc,Us,nUS,Vn,nVn,
     $         ncycth,
     $         THETP,nTHETP,THETS,nTHETS,THETC,nTHETC,THADJ,nTHADJ,
     $         CHCYC,nCHCYC,
     $         VarWNP,varwns,varwnc,varwna,buff2, 
     $         pscyc, varwnt, thtra, npscyc, nthtra,
     $         chpsi, varwca, thcya, nchpsi, nthcya, NoDecompOut,
     $         Lsgud,IsCloseToTD, svudg)
C   LINES OF CODE ADDED FOR X-13A-S : 1
        IF(Lfatal)RETURN
*        write(*,*) 'After DecompSpectrum, Noadmiss = ', Noadmiss
C   END OF CODE BLOCK
        if (NOdecompOut.eq.1) then
CUNX#ifdef PROFILER
!DEC$ IF DEFINED (PROFILER)
*      call profiler(3,'Spectrum')
!DEC$ ENDIF             
CUNX#endif
c         if (Lsumm.gt.0.and.Lsgud.and.svudg)
c     &       WRITE(Nform,1000)'seatsdecomp: no'
         return 1
        end if
       end if
      else if ((noadmiss.eq.1) .or. (noadmiss.eq.2)) then
       if (OUTna.eq.1) then
*        call profiler(3,'Pre ShowInvalDecomp')
        call ShowInvalDecomp(Out,nio,buff2,
     $                       chi,nchi,enot,psi,npsi,estar,
     $                       cyc,ncyc,ncycth,enoc,
     $                       chcyc,nchcyc,thstar,qstar,qt1,Lsgud,
     $                       IsCloseTOTD,svudg)
       end if
 1000  format(a)
       InputModel=InputModel+1
       if (nafijado.eq.0) then
         noadmiss = 3
*         write(*,*) 'Within Spectrum, Noadmiss, NAfijado = ', Noadmiss, 
*     &              NAfijado
       end if
*       if (out .eq. 0) then
*        if (HTML .eq. 1) then
*         call SWarn(Nio)
*         write (Nio,'("<br>DECOMPOSITION INVALID<br>",
*     $              "THE MODEL IS APPROXIMATED")')
*         call EWarn(Nio)
*        else
* 7051    format (
*     $  ////,' DECOMPOSITION INVALID,IRREGULAR SPECTRUM NEGATIVE'//,10x,
*     $  '*****************************',/,12x,
*     $  'THE MODEL IS APPROXIMATED',/,10x,
*     $  '*****************************',/)
*         write (Nio,7051)
*        end if
*       end if
      else
*        call profiler(3,'Pre ShowInvalDecomp')
       call ShowInvalDecomp(Out,nio,buff2,
     $                   chi,nchi,enot,psi,npsi,estar,
     $                   cyc,ncyc,ncycth,enoc,
     $                   chcyc,nchcyc,thstar,qstar,qt1,Lsgud,
     $                   ISCloseToTD,svudg)
       call wWritln('DECOMPOSITION INVALID, IRREGULAR '//
     $              'SPECTRUM NEGATIVE.'//Cbr//'TRY ANOTHER MODEL '//
     $              'OR, FOR AN APPROXIMATION, SET NOADMISS=YES.',
     $              Nio,Mt2,T,T)
       WRITE(STDERR,1010)
 1010  FORMAT(/,'  Warning: Decomposition invalid, irregular spectrum ',
     $        'negative.',/,
     $        '           Try another model or, for an approximation, ',
     $        'set NOADMISS=YES.')
CUNX#ifdef PROFILER
!DEC$ IF DEFINED (PROFILER)
*      call profiler(3,'Spectrum')
!DEC$ ENDIF             
CUNX#endif
c       if (Lsumm.gt.0.and.Lsgud.and.svudg) 
c     &     WRITE(Nform,1000)'seatsdecomp: no'
       return 1
      end if
CUNX#ifdef PROFILER
!DEC$ IF DEFINED (PROFILER)
*      call profiler(3,'Spectrum')
!DEC$ ENDIF             
CUNX#endif
      end
c
c
c
      subroutine SPECTRU(thstar,qstar,chi,nchi,cyc,ncyc,psi,
     $                    npsi,pstar,mq,bd,d,
     $                    qt1,out,ncycth,
     $                    har,
     $                    Fn,NN,estar,enot,enoc,Us,nUS,Vn,nVn,
     $                    root0c,rootPIc,rootPIs,isUgly)
C
C.. Implicits ..
      implicit none
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
C
c-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO,ONE
      PARAMETER(ZERO=0D0,ONE=1D0)
c-----------------------------------------------------------------------
C.. Formal Arguments ..
      integer qstar,nchi,ncyc,npsi,pstar,mq,bd,d,
     $        out,ncycth,har
      real*8 thstar(maxTh),chi(8),cyc(17),psi(27),qt1,fn(*)
      logical root0c,rootPIc,rootPIs,isUgly
C
C.. Local Scalars ..
      integer i,ipipp,j,jmq,jsfix,nn,nqt,nrt,nu,
     $        nus,nvn,ixmin,n_step
c     character fname*30,subtitle*50
c     character auxS*350
c      real*8 arg,x
      real*8 ce1,ce2,cexmin1,cexmin2,e1,e2,enoc,enot,estar,
     $       exmin1,exmin2,pi,lb,ub,e3,ce3,exmin3,
     $       cexmin3,exmin7
cc
c     (Roberto Lopez: 01/2006. 
c      OUTPUT'S of MinimGrid)      
cc
      integer doMinimGrid
      parameter (doMinimGrid = 1)
C
C.. Local Arrays ..
      integer iconv(7),jconv(4)
      real*8 efmin(7),exmin(7),qt(32),rt(32),u(22),us(50),
     $       vn(80),dvec(1)
*     $       utf(8),vf(27),vn(80),y(300)
C
C.. External Functions ..
      real*8 FUNC0,getSdt,getSds,getSdc,getSdi,getSdsa
      logical istrue
      external FUNC0,istrue,getSdt,getSds,getSdc,getSdi,getSdsa
      integer ISTRLEN
      external ISTRLEN
C
C.. External Calls ..
      external CONJ, CONV, DIVFCN, MAK1, MINIM, MULTFN, PARFRA,
     &         SPCEST, USRENTRY, GLOBALMINIM, MINIMGRID
C
C.. Intrinsic Functions ..
      intrinsic ABS, INT, MAX, MIN
      include 'func.i'
      include 'func2.i'
      include 'func3.i'
      include 'func4.i'
      include 'func5.i'
      include 'hspect.i'
      include 'min.i'
      include 'stream.i'
      include 'test.i'
      include 'buffers.i'
      include 'spectra.i'
      include 'dirs.i'
      include 'strmodel.i'
      include 'transcad.i'
C   LINES OF CODE ADDED FOR X-13A-S : 2
      include 'error.cmn'
C   END OF CODE BLOCK
C Debug added by REG
      integer np1a, np2a, np3a, np1b, np2b, np3b, np4b
      double precision fp1a(100), fp2a(100), fp3a(100)
      double precision fp1b(100), fp2b(100), fp3b(100), fp4b(100)
      double precision qt1a(1)
C
C ... Executable Statements ...
C

      jsfix = 0
      ncycth = 0
      Nuc = 0
      do i=1,32
       Uc(i)=ZERO
      end do
      pi = 3.14159265358979d0
      call CONJ(thstar,qstar,thstar,qstar,Ff,Nf)
      call CONJ(chi,nchi,chi,nchi,Ft,Nt)
      call CONJ(cyc,ncyc,cyc,ncyc,Fc,Nc)
      call CONJ(psi,npsi,psi,npsi,Fs,Ns)
      call MULTFN(Ft,Nt,Fc,Nc,fn,nn)
      call MULTFN(fn,nn,Fs,Ns,Fh,Nh)
C
C TO REACTIVATE THE PRINTOUT OF THE HARMONIC FUNCTIONS CHANGE HAR PARAMETER
C
      if ((out .eq.0).and.(har.eq.1)) then
c rober Aqui hay que meter un encabezado!!! ¿h3 o h4?
       CALL mkPOneLine(Nio,'ub','HARMONIC FUNCTIONS')
       CALL mkPOneLine(Nio,'bold','F(X) / H(X) = F(X) / T(X)C(X)S(X)')

       CALL mkHrTable(Nio,Ff,Nf,1)
       CALL mkHrTable(Nio,Ft,Nt,2)
       CALL mkHrTable(Nio,Fc,Nc,3)
       CALL mkHrTable(Nio,Fs,Ns,4)
       CALL mkHrTable(Nio,fn,nn,5)
       CALL mkHrTable(Nio,Fh,Nh,6)

      end if
C correzione per eliminare i picchi stagionalita' quando non c'e'
      if (bd .eq. 1) then
       jmq = mq / 2
      else
       jmq = 0
      end if
C
C  PLOT SERIES SPECTRUM
C
c*******
c Ahora calculamos el espectro de la serie de tramo al principio del programa 
c      if (noadmiss.lt.2) then
c      call SPC(Ff,nf,Fh,nh,1.0D0,spect)
c      end if
C******
C
C                      IDENTIFY COMPONENTS
C
C  QT    TO THE IRREGULAR
C  UT/FT TO TREND
C  UC/FC TO CYCLE
C   V/FS TO SEASONAL
C   U/FN IS THE CYCLE-TREND
C
C   FF/FH = QT + RT/FH
C
C   RT/FH = U/FN  + V/FS
C    U/FN = UT/FT + UC/FC
C
C    QT1  = QT + ENOT + ESTAR + ENOC
C   ENOT  = MINIMUM OF TREND SPECTRUM
C   ENOC  = MINIMUM OF CYCLE SPECTRUM
C   ESTAR = MINIMUM MINIMORUM OF SEASONAL SPEC
C
C   TREND = PRELIMINARY TREND - ENOT
C   CICLE = PRELIMINARY CYCLE - ENOC
C   SEAS. = PRELIMINARY SEAS. - ESTAR
C
C FORM QUOTIENT QT(X) IF NUMERATOR IS OF HIGHER DEGREE THAN DENOMINATOR
C OTHERWISE SET QT(X) = 0 AND REMAINDER RT(X) = FF(X)
C
C      IF(QSTAR.GT.PSTAR) THEN
C      WRITE(NIO,*)' PTOT > QTOT NO DECOMPSITION IS PERFORMED'
C      RETURN 1
C      end if
      if (qstar .lt. pstar) then
       nqt = 1
       qt(1) = ZERO
       do i = 1,qstar
        rt(i) = Ff(i)
       end do
       j = qstar + 1
       do i = j,pstar
        rt(i) = ZERO
       end do
       nrt = pstar
      else
       call DIVFCN(Ff,Nf,Fh,Nh,qt,nqt,rt,nrt)
C FF (total numerator)/FH (total denominator) = QT (quotient) + RT/FH (remainder) 
c comment added DEKM 20 Feb 03
      end if
      if ((out .eq.0).and.(har.eq.1)) then
       CALL mkPOneLine(Nio,'bold','F(X) / H(X) = QT(X) / H(X) + RT(X)')

       CALL mkTableTag(Nio,'@','Harmonic function qt(x) QUOTIENT')
       CALL mkCaption(Nio,'QT(X) QUOTIENT')
 7010  format ('<tr>',8('<td>',f11.4,'</td>'),'</tr>')
       write (Nio,7010) (qt(i), i = 1,nqt)
       CALL writTag(Nio,'</table>')
       CALL mkPOneLine(Nio,'@','&nbsp;')

       CALL mkTableTag(Nio,'@','Harmonic function RT(X)  REMAINDER')
       CALL mkCaption(Nio,'RT(X)  REMAINDER')
       write (Nio,7010) (rt(i), i = 1,nrt)
       CALL writTag(Nio,'</table>')
       CALL mkPOneLine(Nio,'@','&nbsp;')
      end if
      if (npsi .eq. 1) then
       jsfix = 1
      end if
      if (mq .eq. 1) then
       jsfix = 1
      end if
C
C  8484 IS THE END OF THE COMPUTATION OF NUMERATORS OF SPECTRA COMPONENTS
C
C If trend but no seasonal, cycle or transitory, put remainder rt(i) in trend ut(i)  
c comment added DEKM 20 Feb 03
      if (jsfix.eq.1 .and. ncyc.eq.1 .and. ncycth.eq.0 .and. nchi.gt.1)
     $   then
       do i = 1,nrt
        Ut(i) = rt(i)
       end do
       Nut = nrt
       estar = ZERO
       enoc = ZERO
C
C If seasonal but no trend, cycle or transitory, put remainder rt(i) in seasonal v(i)  
c comment added DEKM 20 Feb 03
      else if (jsfix.ne.1 .and. ncyc.eq.1 .and. ncycth.eq.0 .and.
     $        nchi.eq.1) then
       do i = 1,nrt
        V(i) = rt(i)
       end do
       Nv = nrt
       enot = ZERO
       enoc = ZERO
C
C If cycle but no seasonal, trend or transitory, put remainder rt(i) in cycle uc(i)  
c comment added DEKM 20 Feb 03
      else if (jsfix.eq.1 .and. ncycth.eq.0 .and. ncyc.gt.1 .and.
     $        nchi.eq.1) then
       do i = 1,nrt
        Uc(i) = rt(i)
       end do
       Nuc = nrt
       estar = ZERO
       enot = ZERO
C
C
C If seasonal, cycle and trend  
c comment added DEKM 20 Feb 03
      else if (jsfix.ne.1 .and. ncycth.eq.0 .and. ncyc.gt.1 .and.
     $        nchi.gt.1) then
C
C  FIND H.C.F OF FN(X) AND FS(X)
C
C********************************************************************
C ACTIVATE WHEN THE TREND COMPONENT IS MUCH BIGGER THAN THE SEASONAL
C AND THERE MAY BE PROBLEMS WITH PARFRA (I.E WHEN MQ IS SMALL)
C
C       CALL PARFRA(RT,NRT,FS,NS,FN,NN,V,NV,U,NU)
C       IF(I.EQ.I) GO TO 2901
C
C********************************************************************
C
C partial fraction decomposition remainder/(trend-cycle denom*seasonal denom)=(u/trend-cycle denom) + (v/seas denom)  
c comment added DEKM 20 Feb 03
       call PARFRA(rt,nrt,fn,nn,Fs,Ns,u,nu,V,Nv)
       if ((out .eq.0).and.(har.eq.1)) then
        CALL mkHrTable(Nio,u,nu,7)
        CALL mkHrTable(Nio,V,Nv,8)
       end if
       call MULTFN(u,nu,Fs,Ns,us,nus)
       call MULTFN(V,Nv,fn,nn,vn,nvn)
       do i = 1,nus
        Dum(i) = rt(i) - us(i) - vn(i)
       end do
       if ((out .eq.0).and.(har.eq.1)) CALL mkHrTable(Nio,Dum,nus,9)
C
C
C  FIND H.C.F OF FT(X) AND FC(X)
C
       ipipp = ncyc + nchi - 1
       do i = nu+1,ipipp
        u(i) = ZERO
       end do
       nu = ipipp
C partial fraction decomposition to split cycle and trend  
c comment added DEKM 20 Feb 03
       call PARFRA(u,nu,Fc,Nc,Ft,Nt,Uc,Nuc,Ut,Nut)
cc
c Correzione 04.05.2006 MAKE IT SENSE ?????
cc
       if ((Nuc .eq. 1) .and. (abs(Uc(Nuc)) .lt.1.0d-15)) then
        Uc(Nuc) = 1.0d-15
       end if
       if ((out .eq.0).and.(har.eq.1)) then
        CALL mkHrTable(Nio,Ut,Nut,10)
        CALL mkHrTable(Nio,Uc,Nuc,11)
       end if
       call MULTFN(Uc,Nuc,Ft,Nt,us,nus)
       call MULTFN(Ut,Nut,Fc,Nc,vn,nvn)
       do i = 1,nu
        Dum(i) = u(i) - us(i) - vn(i)
       end do
       if ((out .eq.0).and.(har.eq.1)) then
        CALL mkHrTable(Nio,Dum,nus,12)
       end if
C
c if trend and cycle but no seasonal or transitory split cycle and trend 
c comment added DEKM 20 Feb 03
      else if (jsfix.eq.1 .and. ncycth.eq.0 .and. ncyc.gt.1 .and.
     $        nchi.gt.1) then
C
       call PARFRA(rt,nrt,Ft,Nt,Fc,Nc,Ut,Nut,Uc,Nuc)
       if ((out .eq.0).and.(har.eq.1)) then
        CALL mkHrTable(Nio,Ut,Nut,10)
        CALL mkHrTable(Nio,Uc,Nuc,11)
       end if
       call MULTFN(Uc,Nuc,Ft,Nt,us,nus)
       call MULTFN(Ut,Nut,Fc,Nc,vn,nvn)
       do i = 1,nus
        Dum(i) = rt(i) - us(i) - vn(i)
       end do
       if ((out .eq.0).and.(har.eq.1)) then
        CALL mkHrTable(Nio,Dum,nus,12)
       end if
       estar = ZERO
c if seasonal and cycle but no trend and transitory split cycle and seasonal 
c comment added DEKM 20 Feb 03
      else if (jsfix.ne.1 .and. ncycth.eq.0 .and. ncyc.gt.1 .and.
     $        nchi.eq.1) then
       call PARFRA(rt,nrt,Fc,Nc,Fs,Ns,Uc,Nuc,V,Nv)
       if ((out .eq.0).and.(har.eq.1)) then
        CALL mkHrTable(Nio,V,Nv,8)
        CALL mkHrTable(Nio,Uc,Nuc,11)
       end if
       call MULTFN(Uc,Nuc,Fs,Ns,us,nus)
       call MULTFN(V,Nv,Fc,Nc,vn,nvn)
       do i = 1,nus
        Dum(i) = rt(i) - us(i) - vn(i)
       end do
       if ((out .eq.0).and.(har.eq.1)) then
        CALL mkHrTable(Nio,Dum,nus,13)
       end if
       enot = ZERO
c if trend and seasonal but no cycle or transitory split seasonal and trend 
c comment added DEKM 20 Feb 03
      else if (jsfix.ne.1 .and. ncycth.eq.0 .and. ncyc.eq.1 .and.
     $        nchi.gt.1) then
       call PARFRA(rt,nrt,Ft,Nt,Fs,Ns,Ut,Nut,V,Nv)
       if ((out .eq.0).and.(har.eq.1)) then
        CALL mkHrTable(Nio,Ut,Nut,10)
        CALL mkHrTable(Nio,V,Nv,8)
       end if
       call MULTFN(V,Nv,Ft,Nt,us,nus)
       call MULTFN(Ut,Nut,Fs,Ns,vn,nvn)
       do i = 1,nus
        Dum(i) = rt(i) - us(i) - vn(i)
       end do
       if ((out .eq.0).and.(har.eq.1)) then
        CALL mkHrTable(Nio,Dum,nus,14)
       end if
       enoc = ZERO
      end if
C
C
      if (qstar .gt. pstar) then
       call MULTFN(qt,nqt,Fc,Nc,Dum,Ndum)
C
C..   Modified by REG on 12/22/2005
       call ADDJ(Uc,Nuc,ONE,Dum,NDum,ONE,Uc,Nuc)
       ncycth = 1
      end if
C Debug added by REG on 12/22/2005
      if ((out .eq.0).and.(har.eq.1)) then
       do i=1,Nf
        fp1b(i)=ZERO
        fp2b(i)=ZERO
        fp3b(i)=ZERO
        fp4b(i)=ZERO
       end do
       CALL mkHrTable(Nio,Ut,Nut,10)
       CALL mkHrTable(Nio,V,Nv,8)
       CALL mkHrTable(Nio,Uc,Nuc,11)

       call MULTFN(V,Nv,Ft,Nt,fp1a,np1a)
       call MULTFN(Ut,Nut,Fs,Ns,fp2a,np2a)
       call MULTFN(fp1a,np1a,Fc,Nc,fp1b,np1b)
       call MULTFN(fp2a,np2a,Fc,Nc,fp2b,np2b)
       if ( Nuc .gt. 0 ) then
        call MULTFN(Uc,Nuc,Fs,Ns,fp3a,np3a)
        call MULTFN(fp3a,np3a,Ft,Nt,fp3b,np3b)
       end if
       if (Nf .eq. Nh) then
         call MULTFN(qt,nqt,Fh,Nh,fp4b,np4b)
       end if
       do i = 1,Nf
        Dum(i) = Ff(i) - fp1b(i) - fp2b(i) - fp3b(i) - fp4b(i)
       end do
       CALL mkHrTable(Nio,Dum,Nf,15)
      end if
C
C
      if (nchi .ne. 1) then
C
C  FIND MINIMUM OF TREND SPECTRUM AND PLOT
C
       Ifunc = 2
       Dstop = 0.000005d0
       Step = 0.01d0
       Start = pi
C Changed (by Donald Martin, 7/23/02) set lb and ub, pass to minim.
       lb = 0.5d0 * pi
       ub = pi
*       call MINIM(e1,exmin1,lb,ub,jconv(1))
       n_step=12
       call GlobalMINIM(e1,exmin1,lb,ub,jconv(1),n_step,d+bd,mq,2)
       Start = 0.5d0 * pi
C Changed (by Donald Martin, 7/23/02) set lb and ub, pass to minim.
       lb = 0D0
       ub = 0.5d0 * pi
       call GlobalMINIM(e2,exmin2,lb,ub,jconv(2),n_step,d+bd,mq,2)
       if (abs(exmin2).lt.1.0d-3) then
         isUgly=.TRUE.
       end if
*       call MINIM(e2,exmin2,lb,ub,jconv(2))
       enot = MIN(e1,e2)
       if ((doMinimGrid.gt.0) .and. (ut(1)-enot*ft(1).lt.ZERO)) then
        call minimGrid(e3,exmin3,mq,2,2)
        if (e3 .lt. enot) then
         exmin2 = exmin3
         enot = e3
         e2 = e3
        end if
       end if
c
c MY ADDITION (Donald Martin, July 2002) TO 'SPECTRUM' OF TREND USING GRID SEARCH ALSO
c
*       if (Newmdl.gt.0) THEN
*        call minim2(e3, ixmin)
*        exmin3 = dble(float(ixmin))
*        if (e3 .lt. enot) enot = e3
*       end if
      end if
C
      if (ncycth.ne.0 .or. ncyc.ne.1) then
C
C  FIND MINIMUM OF CYCLE SPECTRUM AND PLOT
C
       Ifunc = 3
       Dstop = 0.000005d0
       Step = 0.01d0
       Start = pi
C Changed (by Donald Martin, 7/23/02) set lb and ub, pass to minim.
       lb=0.5d0*pi
       ub = pi
*       call MINIM(ce1,cexmin1,lb,ub,jconv(3))
       n_step=12
       call GlobalMINIM(ce1,cexmin1,lb,ub,jconv(3),n_step,d+bd,mq,2)
       if (abs(cexmin1-pi).lt.1.0D-3 .and. rootPIc) then
        isUgly=.TRUE.
       end if
       Start = 0.5d0 * pi
C Changed (by Donald Martin, 7/23/02) set lb and ub, pass to minim.
       lb = 0D0
       ub = 0.5d0*pi
*       call MINIM(ce2,cexmin2,lb,ub,jconv(4))
       call GlobalMINIM(ce2,cexmin2,lb,ub,jconv(4),n_step,d+bd,mq,2)
       if (abs(cexmin2).lt.1.0D-3 .and. root0c) then
        isUgly=.TRUE.
       end if
       enoc = MIN(ce1,ce2)
       if ((doMinimGrid.gt.0) .and. (uc(1)-enoc*fc(1).lt.ZERO)) then
        call minimGrid(ce3,cexmin3,mq,2,3)
        if (ce3 .lt. enoc) then 
         enoc = ce3
         cexmin2 = cexmin3
         ce2 = ce3
        end if  
       end if
*       if (Newmdl.gt.0) THEN
*c
*c MY ADDITION (DONALD MARTIN, JULY 2002) TO FIND MINIMUM OF
*c 'SPECTRUM' OF CYCLE
*c
*        call minim2(ce3,ixmin)
*        cexmin3 = dble(float(ixmin))
*        if (ce3 .lt. enoc) enoc = ce3
*       end if
      end if
C
      if (jsfix .ne. 1) then
C
C  FIND MINIMUM OF SEASONAL SPECTRUM AND PLOT
C
       Ifunc = 1
       Dstop = 0.000005d0
       Step = 0.01d0
       Start = ZERO
       jmq = mq / 2
C Changed (by Donald Martin, 7/23/02) set lb and ub, pass to minim.
       lb = 0D0
       ub = pi / dble(jmq)
       call MINIM(efmin(1),exmin(1),lb,ub,iconv(1))
       if (abs(efmin(1)-pi).lt.1.0D-3 .and. rootPIs) then
        isUgly=.TRUE.
       end if
       do i = 2,jmq
        Start = (dble(i-0.5d0) * pi) / dble(jmq)
C Changed (by Donald Martin, 7/23/02) set lb and ub, pass to minim.
        lb = (dble(i-1) * pi) / dble(jmq)
        ub = (dble(i) * pi) / dble(jmq)
        call MINIM(efmin(i),exmin(i),lb,ub,iconv(i))
        if (abs(efmin(i)-pi).lt.1.0D-3 .and.rootPIs) then
         isUgly=.TRUE.
        end if
       end do
       estar = 10.0d0
       do i = 1,jmq
        if (efmin(i) .lt. estar) then
         estar = efmin(i)
        end if
       end do

*       if (Newmdl.gt.0) THEN
*        call minim2(efmin(7),ixmin)
*        exmin7 = dble(float(ixmin))
*        if (efmin(7) .lt. estar) estar = efmin(7)
*       end if
       if ((doMinimGrid.gt.0) .and. (v(1)- estar*fs(1).lt.ZERO)) then
        call minimGrid(efmin(jmq+1), exmin(jmq+1),mq,2,1)  
        if (efmin(jmq+1) .lt. estar) then
         estar = efmin(jmq+1)
        end if
       end if  
      end if
C
      if ((out .eq.0).and.(har.eq.1)) then
       if (jsfix .ne. 1) then
        CALL mkTableTag(Nio,'w60','SEASONAL SPECTRUM LOCAL MINIMA')
        CALL mkCaption(Nio,'SEASONAL SPECTRUM LOCAL MINIMA')
 7027   format ('<tr><th scope="col">LOCAL MINIMA</th>',
     $             '<th scope="col">FREQUENCY (RADIANS)</th>',
     $             '<th scope="col">CONVERGENCE TEST</th></tr>')
        write (Nio,7027)
        do i = 1,jmq
 7029    format ('<tr><td>',f9.6,'</td>','<td>',f7.4,'</td>',
     $           '<td>',i2,'</td></tr>')
         write (Nio,7029) efmin(i), exmin(i), iconv(i)
        end do
        CALL writTag(Nio,'</table>')
        CALL mkPOneLine(Nio,'@','&nbsp;')
 7030   format (/,'<p><strong>MINIMUM MINIMORUM :</strong> ',f11.6,
     $            '</p>')
        write (Nio,7030) estar
       end if
C
       if (ncycth.ne.0 .or. ncyc.ne.1) then
        CALL mkTableTag(Nio,'w60',
     $              transLcad(1:ntransLCad)//' SPECTRUM SIMPLE MINIMUM')
        CALL mkCaption(Nio,transLcad(1:ntransLCad)//
     $                 ' SPECTRUM SIMPLE MINIMUM')
        write (Nio,7027)
        write (Nio,7029) ce1, cexmin1, jconv(3)
         write (Nio,'("<tr><td>",f9.6,"</td>",
     $               "<td>",f7.4,"</td>",
     $               "<td>",i2,"</td></tr>")') 
     $               ce1, cexmin1, jconv(3)
        if ((ABS(cexmin1-cexmin2)) .ge. 0.0001d0) then
         write (Nio,7029) ce2, cexmin2, jconv(4)
        end if
        CALL writTag(Nio,'</table>')
        CALL mkPOneLine(Nio,'@','&nbsp;')
       end if
       if (nchi .ne. 1) then
        CALL mkTableTag(Nio,'w60','TREND-CYCLE SPECTRUM SIMPLE MINIMUM')
        CALL mkCaption(Nio,'TREND-CYCLE SPECTRUM SIMPLE MINIMUM')
        write (Nio,7027)
        write (Nio,7029) e1, exmin1, jconv(1)
        if ((ABS(exmin1-exmin2)) .ge. 0.0001d0) then
         write (Nio,7029) e2, exmin2, jconv(2)
        end if
        CALL writTag(Nio,'</table>')
        CALL mkPOneLine(Nio,'@','&nbsp;')
       end if
      end if
C
C
C CHECK IF DECOMPOSITION VALID
C
      qt1 = qt(1) + enot + estar + enoc
      if (qstar .gt. pstar) then
       qt1 = enot + estar + enoc
      end if
cc
c  Used for new Model Approximation
cc
       if (qt1 .lt.ZERO) then
         isUgly = .true.
       end if
c
cc
      end
cc
c
cc
      subroutine CALCRES(phi,nPhi,Bphi,nBphi,WDifCen,nWdif,ImeanOut,
     $                   Phistar,nPhistar,Thstar,nThstar,Z,Nz,A,back,
     $                   Na,sqf,rmean,rstd,rtval,wnormtes,skewne,
     $                   test1,rkurt,test2,sumSres,dw,rvar,mAuto,nfreq,
     $                   r,se,Qstat, DF,D,BD,KEN,n_1,n0,tvalRUNS,Sr,
     $                   Sqstat,Sdf,Sse,out)
C
C
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
      integer n12,n10,n1
      parameter (n1 = 1, n10 = 10, n12 = 12)
      real*8 ZERO,TWO
      parameter (ZERO = 0.0D0, TWO = 2.0D0)
C
C.. Formal Arguments ..
      real*8 A(mpkp),back(mpkp)
      real*8 Z(mpkp)
      real*8 WDifCen(mpkp)
      real*8 Phistar(*),Thstar(*),phi(4),Bphi(13)
      real*8 sqf,KEN
      integer nPhistar,nThstar,Nz, Na, out, nPhi,nBphi
      real*8 rkurt,skewne,wnormtes,test1,test2
      real*8 rmean,rstd,rtval,rvar,dw,sumSres
      integer mAuto,nfreq,DF,Sdf,D,BD,n_1,n0,nWdif,ImeanOut
      real*8 Qstat,Sqstat,tvalRUNS
      real*8 r(*),se(*),Sr(*),Sse(*)
C
C.. External Functions ..
      real*8 KENDALLS
      real*8 DMED
C
C.. Local Scalars ..
      integer i,j,Np,q1,nna,iauto,wnw,kd,Ierr
      real*8 sum,sumb,f,xmed,ws
      character Errext*180
C
C.. Local Arrays ..
      real*8 u(mpkp),ub(mpkp)
      real*8 ba(mpkp),bz(mpkp)
      real*8 Pstar(27),Qstar(maxTh)
      integer nPstar,nQstar
      include 'stream.i'
*      include 'indhtml.i'
      include 'pinno.i'
C
C ... Executable Statements ...
C
cc
c Switch the model to BJ signs and compute the residuals using CLS
cc
      do i=1,nThstar-1
        Qstar(i) = -Thstar(i+1)
      enddo
      nQstar = nThstar - 1
      if (ImeanOut .eq.0) then
        do i=1,nz
          bz(Nz-i+1) = z(i)
        enddo
        do i=1,nPhistar
          Pstar(i) = -Phistar(i+1)
        enddo
        nPstar = nPhistar - 1   
        Np = Nz-nPstar
        do i = 1,Np
         sum = Z(i+nPstar)
         sumb = bz(i+nPstar)
          do j = 1,nPstar
           sum = sum - Pstar(j)*Z(i+nPstar-j)
           sumb = sumb - Pstar(j)*bz(i+nPstar-j)
          end do
         u(i) = sum
         ub(i) = sumb
        end do
      else
cc
c     Compute PHIST = Phi * Bphi
cc
        call conv(Phi,nPhi,Bphi,nBphi,Pstar,nPstar)
        do i=2,nPstar
          Pstar(i-1) = -Pstar(i)
        enddo
        nPstar = nPstar - 1   
        kd = (-1)**(BD+D)
        j = nWdif
        do i = 1,nWdif
          ws = WDifCen(i) * kd
          bz(i) = WDifCen(nWdif-i+1) * kd
          bz(nWdif-i+1) = ws
          j = j - 2
          if (j .le. 0) goto 1
        end do
 1      do i = 1,NWdif
         sum = WDifCen(i+nPstar)
         sumb = bz(i+nPstar)
          do j = 1,nPstar
           sum = sum - Pstar(j)*WDifCen(i+nPstar-j)
           sumb = sumb - Pstar(j)*bz(i+nPstar-j)
          end do
         u(i) = sum
         ub(i) = sumb
        end do
        Np = NWdif
      end if
      do i = 1, nQstar
        a(i) = ZERO
        back(i) = ZERO
      enddo
      q1 = nQstar + 1
      do i = q1,Nz
       sum = u(i-nQstar)
       sumb = ub(i-nQstar)
        do j = 1,nQstar
         sum = sum + Qstar(j)*a(i-j)
         sumb = sumb + Qstar(j)*back(i-j)
        end do
       a(i) = sum
       back(i) = sumb
      end do
cc
c Residuals statistics computation
cc
      rmean = ZERO
      rvar = ZERO
      Na = Np
      do i = nQstar+1,Na
        rvar = rvar + a(i)*a(i)
        rmean = rmean + a(i)
      end do
      sumSres = rvar
      rvar = rvar / (Na-nQstar)
      sqf = dsqrt(sumSres / DBLE(Na-nQstar) )
      call setSD(sqf)
      rmean = rmean / (Na-nQstar)
      rstd = (sumSres/Na)**0.5d0
      rtval = rmean / rstd
      skewne = ZERO
      rkurt = ZERO
      nna = Na - nPstar
      do i = 1,Na
        skewne = skewne + ((a(i)-rmean)**3)/(rvar**1.50d0*nna)
        rkurt = rkurt + ((a(i)-rmean)**4)/(rvar**TWO*nna)
      end do
      test1 = SQRT(6.0d0/Na)
      test2 = SQRT(24.0d0/Na)
      wnormtes = (skewne**2)/(test1**2) +
     $           ((rkurt-3)**2)/(test2**2)
      dw = ZERO
      do i = 2,Na
        dw = dw + (a(i)-a(i-1))**2
      end do
      dw = dw / sumSres
cc
c KENDALL Test
cc
      Ken = kendalls(a,Na,Nfreq)
cc
c Compute ACF of residuals
cc
      iauto = 1
      wNw = Nz - D - Nfreq*BD
      call AUTO(Na,A,mAuto,r,Iq,wNw,0,Nfreq,iauto,
     $          Qstat,df,se,Ierr,Errext)
cc
c Test RUNS on Residuals
cc
      xmed = DMED(a,Na)
      call RACES(a,Na,xmed,1,tvalRUNS,n_1,n0)
cc
c Squared Residuals
cc
      do i = 1,Na
        ba(i) = a(i)**2
      end do
      call AUTO(Na,ba,mAuto,Sr,Iq,wNw,0,Nfreq,iauto,
     $          SQstat,Sdf,Sse,Ierr,Errext)
cc
c OUTPUT Section
cc
C
C
      end
c
      subroutine DecompSpectrum(NOADMISS,NOSERIE,
     $         CHI,nCHI,PSI,nPSI,CYC,nCYC,CHIS,nCHIS,
     $         PSIS,nPSIS,ADJS,nADJS,CYCS,nCYCS,THSTAR,QSTAR,SQF,
     $         ct,cs,cc,Qt1,
     $         SQG,mq,bd,d,PG,OUT,ITER,
     $         estar,enot,enoc,Us,nUS,Vn,nVn,
     $         ncycth,
     $         THETP,nTHETP,THETS,nTHETS,THETC,nTHETC,THADJ,nTHADJ,
     $         CHCYC,nCHCYC,
     $         VarWNP,varwns,varwnc,varwna,buff2, 
     $         pscyc, varwnt, thtra, npscyc, nthtra,
     $         chpsi, varwca, thcya, nchpsi, nthcya, NoDecompOut,
     $         Lsgud, IsCloseToTD, svudg)
      implicit none
c-----------------------------------------------------------------------
      DOUBLE PRECISION ONE,ZERO,TWO
      LOGICAL T,F
      PARAMETER(ONE=1D0,ZERO=0D0,TWO=2D0,T=.true.,F=.false.)
c-----------------------------------------------------------------------
c INPUT PARAMETERS
      integer NOADMISS,nchi,npsi,nCYC,SQG,MQ,bd,d,PG,OUT,ITER,NOSERIE
      integer nChis,nADJS,nPSIS,nCYCS
      real*8 ct(32),cs(32),cc(32),Cyc(17),CYCs(17),
     $       chi(8),CHIS(5),PSI(27),PSIS(16),SQF
      real*8 ADJS(17)
      include 'func.i'
      include 'func2.i'
      include 'func3.i'
      include 'func4.i'
      include 'func5.i'
      include 'test.i'
      include 'buffers.i'
      include 'spectra.i'
      include 'dirs.i'
      include 'stream.i'
      include 'error.cmn'
      include 'htmlout.cmn'
      include 'transcad.i'
      include 'stdio.i'
      include 'units.cmn'
      integer nUS,nVn,ncycth,Qstar, npscyc, nthtra, 
     $        nchpsi, nthcya
      integer nounit
      real*8 qt1,estar,enot,enoc,Us(50),Vn(80),THstar(27), 
     $       pscyc(32), varwnt, thtra(32),
     $       chpsi(32), varwca, thcya(32)
      logical Lsgud, IsCloseToTD, svudg
c OUTPUT PARAMETERS
      include 'strmodel.i'
      integer nTHETP,nTHETS,nTHETC,NTHADJ,NCHCYC,NoDecompOut
      real*8 thetp(8),thets(27),thetc(32),thadj(32),chcyc(20)
      real*8 varWnp,varwns,varwnc,varwna
      character buff2*80
c LOCAL PARAMETERS
      real*8 Qmin,utf(8),x,pi
      real*8 arg,y(300),vf(27),UCF(32),toterr,dvec(1)
      integer I,J,IOUT,nsaltos
      character fname*30,subtitle*50,auxs*350,caption0*(1),id0*(1)
      logical isopen
c External Functions
      real*8 FUNC0
      integer ISTRLEN
      external FUNC0,ISTRLEN
      intrinsic abs
c -----------------
      pi = 3.14159265358979d0
      Qmin=Qt1
      NoDecompOut=0
      nounit = 0
C
C SUBTRACT MINIMA AND SET UP FILTERS NUMERATORS
C
       do i = 1,32
        ct(i) = ZERO
        cs(i) = ZERO
        cc(i) = ZERO
       end do
C
       do i = 1,Nf
        Dum1(i) = Ff(i)
       end do
       Ndum1 = Nf
       if (nchi .ne. 1) then
        Ut(Nt) = ZERO
        Nut = Nt
        do i = 1,Nut
         utf(i) = Ut(i) - enot*Ft(i)
        end do
        call SPC(Utf,Nut,Ft,Nt,ONE,spectt)
C**********************************************************
        call MULTFN(utf,Nut,Fc,Nc,vn,nvn)
        call MULTFN(vn,nvn,Fs,Ns,us,nus)
C
C**********************************************************
        do i = 1,nus
         Dum(i) = us(i)
        end do
        Ndum = nus
        Ifunc = 5
        do i = 0,120
         x = (ONE/120.0d0) * pi * i
         arg = FUNC0(x)
         y(i+1) = arg
         if (sqg .eq. 1) then
          y(i+1) = y(i+1)**2
         end if
        end do
C
C GC 08/07/98
        if (d.ne.0 .or. bd.ne.0) then
         y(1) = ONE
        end if
*        if ((pg.eq.0) .and. (out.eq.0).and.(iter.eq.0)) then
*         fname = 'FILTFT.T4F'
*         if (sqg .eq. 1) then
*          subtitle = 'SQUARED GAIN OF TREND-CYCLE FILTER'
*         else
*          subtitle = 'FILTER for TREND-CYCLE (F.D.)'
*         end if
*         call PLOTFILTERS(fname,subtitle,y,121,mq,ZERO,pi,1)
*        end if
C
C**********************************************************
C
C**********************************************************
        do i = 1,Nh
         Dum(i) = Fh(i)
        end do
        Ndum = Nh
        Ifunc = 5
        do i = 0,120
         x = (ONE/120.0d0) * pi * i
         arg = FUNC0(x)
         y(i+1) = arg * qt1
         if (sqg .eq. 1) then
          y(i+1) = y(i+1)**2
         end if
        end do
*        if ((pg.eq.0) .and. (out.eq.0).and.(iter.eq.0)) then
*         fname = 'FILTFI.T4F'
*         if (sqg .eq. 1) then
*          subtitle = 'SQUARED GAIN OF IRREGULAR FILTER'
*         else
*          subtitle = 'FILTER for IRREGULAR (F.D.)'
*         end if
*         call PLOTFILTERS(fname,subtitle,y,121,mq,ZERO,pi,1)
*        end if
C
C**********************************************************
        ct(1) = us(1)
        do j = 2,nus
         ct(j) = 0.5d0 * us(j)
        end do
       end if
C
       if (npsi .ne. 1) then
        V(Ns) = ZERO
        do i = 1,Ns
         vf(i) = V(i) - estar*Fs(i)
        end do
        call SPC(Vf,Ns,Fs,Ns,ONE,spectS)
C**********************************************************
        call MULTFN(vf,Ns,Fc,Nc,vn,nvn)
        call MULTFN(vn,nvn,Ft,Nt,us,nus)
C
C**********************************************************
        do i = 1,nus
         Dum(i) = us(i)
        end do
        Ndum = nus
        Ifunc = 5
        do i = 0,120
         x = (ONE/120.0d0) * pi * i
         arg = FUNC0(x)
         y(i+1) = arg
         if (sqg .eq. 1) then
          y(i+1) = y(i+1)**2
         end if
        end do
*        if ((pg.eq.0) .and. (out.eq.0).and.(iter.eq.0)) then
*         fname = 'FILTFS.T4F'
*         if (sqg .eq. 1) then
*          subtitle = 'SQUARED GAIN OF SEASONAL FILTER'
*         else
*          subtitle = 'FILTER for SEASONAL (F.D.)'
*         end if
*         call PLOTFILTERS(fname,subtitle,y,121,mq,ZERO,pi,1)
*        end if
C
C**********************************************************
        cs(1) = us(1)
        do j = 2,nus
         cs(j) = 0.5d0 * us(j)
        end do
       end if
       if (ncycth.ne.0 .or. ncyc.ne.1) then
C
C CORREZIONE DI GIANLUCA 06-09-95 TOP-HEAVY CYCLE
C
        if (ncycth .eq. 0) then
         do i=Nuc+1,Nc
          Uc(i) = ZERO
         end do
         Nuc = Nc
        else
         do i = Nc+1,Nuc
          Fc(i) = ZERO
         end do
         Nc = Nuc
        end if
        do i = 1,Nuc
         ucf(i) = Uc(i) - enoc*Fc(i)
        end do
        call SPC(Ucf,Nuc,Fc,nc,ONE,specty)
C**********************************************************
        call MULTFN(ucf,Nuc,Fs,Ns,vn,nvn)
        call MULTFN(vn,nvn,Ft,Nt,us,nus)
C
C**********************************************************
        do i = 1,nus
         Dum(i) = us(i)
        end do
        Ndum = nus
        Ifunc = 5
        do i = 0,120
         x = (ONE/120.0d0) * pi * i
         arg = FUNC0(x)
         y(i+1) = arg
         if (sqg .eq. 1) then
          y(i+1) = y(i+1)**2
         end if
        end do
*        if ((pg.eq.0) .and. (out.eq.0).and.(iter.eq.0)) then
*         fname = 'FILTFY.T4F'
*         if (sqg .eq. 1) then
*          subtitle = 'SQUARED GAIN OF '//transLCad(1:nTransLCad)//
*     &               ' FILTER'
*         else
*          subtitle = 'FILTER for '//transLCad(1:nTransLCad)//
*     &               ' (F.D.)'
*         end if
*         call PLOTFILTERS(fname,subtitle,y,121,mq,ZERO,pi,1)
*        end if
C
C**********************************************************
        cc(1) = us(1)
        do j = 2,nus
         cc(j) = 0.5d0 * us(j)
        end do
       end if
C Debug added by REG on 12/22/2005
*      if (out .eq. nnohar) then
*       do i=1,Nf
*        fp1b(i)=ZERO
*        fp2b(i)=ZERO
*        fp3b(i)=ZERO
*        fp4b(i)=ZERO
*       end do
*       write (Nio,8003) 'UTF(X)', (Utf(i), i = 1,Nut)
*       write (Nio,8003) 'VF(X)',  (Vf(i), i = 1,Ns)
*       write (Nio,8003) 'UCF(X)', (Ucf(i), i = 1,Nuc)
*       write (Nio,8003) 'I(X)', qt1
*c8003  format( //, 1x, a, //, 10(8(f11.4,1x),/) )
*       call MULTFN(Vf,Ns,Ft,Nt,fp1a,np1a)
*       call MULTFN(Utf,Nut,Fs,Ns,fp2a,np2a)
*       call MULTFN(fp1a,np1a,Fc,Nc,fp1b,np1b)
*       call MULTFN(fp2a,np2a,Fc,Nc,fp2b,np2b)
*       if ( Nuc .gt. 0 ) then
*        call MULTFN(Ucf,Nuc,Fs,Ns,fp3a,np3a)
*        call MULTFN(fp3a,np3a,Ft,Nt,fp3b,np3b)
*       end if
*       qt1a(1)=qt1
*       call MULTFN(qt1a,1,Fh,Nh,fp4b,np4b)
*       do i = 1,Nf
*        Dum(i) = Ff(i) - fp1b(i) - fp2b(i) - fp3b(i) - fp4b(i)
*       end do
* 8028  format ( ///,
*     $ ' DUM(X) = F(X)-VF(X)T(X)C(X)-UTF(X)S(X)C(X)-UCF(X)S(X)T(X)',
*     $ '-I(X)H(X).',' THIS SHOULD BE ZERO', //, 10(8(g12.5,1x),/) )
*       write (Nio,8028) (Dum(i), i = 1,Nf)
*      end if
C
C  FIND THE MA REPRESENTATION OF THE THREE NUMERATORS
C
       nthetp = 1
       nthets = 1
       nthetc = 1
       nthadj = 1
       thetp(1) = ONE
       thets(1) = ONE
       thetc(1) = ONE
       thadj(1) = ONE
C
C      SPECTRUM OF IRREGULAR ESTIMATOR
C
*       if (pg .eq. 0) then
*        call SPC(Fh,Nh,Ff,Nf,Qt1*Qt1,spectei)
*       end if
       if (out.eq.0) then
        iout=0
       else
        iout=1
       end if
       call MAspectrum(iout,nio,buff2,
     $            chi,nchi,utf,nut,thetp,nthetp,varwnp,
     $            npsi,vf,ns,thets,nthets,varwns,
     $            cyc,ncyc,ncycth,ucf,nuc,thetc,nthetc,varwnc,
     $            chcyc,nchcyc,thstar,qstar,thadj,nthadj,varwna,
     $            us,nus,qt1,IsCloseToTD)
C   LINES OF CODE ADDED FOR X-13A-S : 1
       IF(Lfatal)RETURN
C   END OF CODE BLOCK
C                ****  TREND  ****
C
*       if (nchi .ne. 1) then
*        if (pg .eq. 0) then
*         call SPCEST(utf,Nut,Fs,Ns,Fc,Nc,Ft,Nt,Ff,Nf,spectet)
*        end if
*       end if
C
*       if (npsi .ne. 1) then
*C
*C                ****  SEAS.  ****
*C
*        if (pg .eq. 0) then
*         call SPCEST(vf,Ns,Ft,Nt,Fc,Nc,Fs,Ns,Ff,Nf,specteS)
*        end if
*       end if
C
       if (ncycth.ne.0 .or. ncyc.ne.1) then
C
C                 ****  CYCLE  ****
C
        if (varwnc .lt.ZERO) then
         if ((noadmiss.eq.1) .or. (noadmiss.eq.2)) then
          noadmiss = 3
          if (out.eq.0) then
           CALL wWritln('DECOMPOSITION INVALID'//Cbr//
     $                  'THE MODEL IS APPROXIMATED',Nio,Mt2,T,T)
          end if
          return
         else
          if (out.eq.0) then
           call wWritln('DECOMPOSITION INVALID,IRREGULAR SPECTRUM '//
     $                  'NEGATIVE'//Cbr//'TRY ANOTHER MODEL OR, '//
     $                  'FOR AN APPROXIMATION, SET NOADMISS=YES.',
     $                  Nio,Mt2,T,T)
          end if
          NoDecompOut=1
          return
         end if
        end if
*        if (pg .eq. 0) then
*         call SPCEST(ucf,Nuc,Fs,Ns,Ft,Nt,Fc,Nc,Ff,Nf,spectey)
*        end if
       end if
C
       if (nchcyc.ne.1 .or. ncycth.ne.0) then
        if (npsi .eq. 1) then
         do i = 1,qstar
          thadj(i) = thstar(i)
         end do
         do i = qstar+1,nchcyc
          thadj(i) = ZERO
         end do
c         nthadj = MAX(qstar,nchcyc)
         nthadj=qstar
         varwna = ONE
        else
C
C
C  FIND MA REPRESENTATION OF SEASONALLY ADJUSTED SERIES
C
         if (IsCloseToTD) then
*           if (pg .eq. 0) then
*             call SPCEST(us,nus,Fs,Ns,Fc,nc,Ft,nt,Ff,Nf,specteSA)
*           end if
         else
           call MULTFN(Ft,Nt,Fc,Nc,vn,nvn)
*           if (pg .eq. 0) then
*             call SPCEST(us,nus,Fs,Ns,ONE,1,vn,nvn,Ff,Nf,specteSA)
*           end if
         end if
         call getSpectrum(thadj,nthadj,chcyc,nchcyc,spectSA)
         do i=1,Lspect
           spectSA(i)=varwna*spectSA(i)/(TWO*pi)
         enddo
         if (IsCloseToTD) then
           call MULTFN(us,nus,Fs,Ns,Dum1,Ndum1)
           call MULTFN(dum1,nDum1,Fc,Nc,Dum,Ndum)
         else   
           call MULTFN(us,nus,Fs,Ns,Dum,Ndum)
         end if
         do i = 1,Nf
          Dum1(i) = Ff(i)
         end do
         Ndum1 = Nf
         Ifunc = 5
         do i = 0,120
          x = (ONE/120.0d0) * pi * i
          arg = FUNC0(x)
          y(i+1) = arg
          if (sqg .eq. 1) then
           y(i+1) = y(i+1)**2
          end if
         end do
C
C GC 08/07/98
         if (d.ne.0 .or. bd.ne.0) then
          y(1) = ONE
         end if
*         if ((pg.eq.0) .and. (out.eq.0).and.(iter.eq.0)) then
*          fname = 'FILTFADJ.T4F'
*          if (sqg .eq. 1) then
*           subtitle = 'SQUARED GAIN OF SA SERIES FILTER'
*          else
*           subtitle = 'FILTER for TREND-CYCLE (F.D.)'
*          end if
*          call PLOTFILTERS(fname,subtitle,y,121,mq,ZERO,pi,1)
*         end if
        end if
       end if
       
C
C added by DEKM Feb 6 2003 to compute trend adjusted component

       varwnt = ZERO
       if (npscyc.ne.1 .or. ncycth.ne.0) then
        if (nchi .eq. 1) then
         do i = 1,qstar
          thtra(i) = thstar(i)
         end do
         do i = qstar+1,npscyc
          thtra(i) = ZERO
         end do
         nthtra = MAX(qstar,npscyc)
         varwnt = ONE
        else
C
C
C  FIND MA REPRESENTATION OF TREND ADJUSTED SERIES
C
         call CONJ(pscyc,npscyc,pscyc,npscyc,us,nus)
         do i = 1,nus
          us(i) = us(i) * qt1
         end do
C
C..   Modified by REG on 12/22/2005
         if (npsi .ne. 1) then
          call CONV(thets,nthets,cyc,ncyc,vn,nvn)
          call CONJ(vn,nvn,vn,nvn,Dum,Ndum)
C
C..   Modified by REG on 12/22/2005
          call ADDJ(us,nus,ONE,Dum,NDum,varwns,us,nus)
         end if
         if (ncycth.ne.0 .or. ncyc.ne.1) then
          call CONV(thetc,nthetc,psi,npsi,vn,nvn)
          call CONJ(vn,nvn,vn,nvn,Dum,Ndum)
C
C..   Modified by REG on 12/22/2005
          call ADDJ(us,nus,ONE,Dum,NDum,varwnc,us,nus)
         end if
         iout = 1
         if (out .eq. 1) then
          CALL writTagOneLine(Nio,'h3','@',
     $                        'MA ROOTS OF TREND ADJUSTED SERIES')
          iout = 0
         end if
c Here we do spectral factorization to get trend adjusted numerator (thtra) 
c comment added DEKM 20 Feb 03 
         caption0=' '
         id0=' '
         call MAK1(us,nus,thtra,nthtra,varwnt,nounit,iout,caption0,0,
     &             toterr,id0,0)
C   LINES OF CODE ADDED FOR X-13A-S : 1
         IF(Lfatal)RETURN
C   END OF CODE BLOCK
         call CONJ(thtra,nthtra,thtra,nthtra,vn,nvn)
         if (nus .ne. nvn) then
 7034    format (
     $   /,'<p>THE LENGTH OF THE MA DOESN''T MATCH WITH THE ACF</p>')
          write (Nio,7034)
         end if
         toterr = ZERO
         do i = 1,nvn
          toterr = toterr + (vn(i)*varwnt-us(i))**2
         end do
         dvec(1)=toterr
         call USRENTRY(dvec,1,1,1,1,1903)
         if (toterr .gt. 1.0d-2) then
          call setSf('E')
          buff2 =
     $      'THE SPECIFICATION OF SOME OF THE MODELS MAY BE UNRELIABLE'
         end if
         if (out .eq. 1) then
 7035     format ('<p>TOTAL SQUARED ERROR=',d15.7,'</p>')
          write (Nio,7035) toterr
         end if
        end if
       else
        nthtra=1
        thtra(1)=1D0
       end if
C
C added by DEKM 1 May 2003 to compute cycle adjusted component

C
C
      varwca = ZERO
      if (nchpsi.ne.1 .or. ncycth.ne.0) then
C..   Modified by REG on 12/22/2005
        if ((ncyc .eq. 1) .and. (ncycth. eq. 0)) then
         do i = 1,qstar
          thcya(i) = thstar(i)
         end do
         do i = qstar+1,nchpsi
          thcya(i) = ZERO
         end do
         nthcya = MAX(qstar, nchpsi)
         varwca = ONE
        else
C
C
C  FIND MA REPRESENTATION OF CYCLE ADJUSTED SERIES
C
         call CONJ(chpsi,nchpsi,chpsi,nchpsi,us,nus)
         do i = 1,nus
          us(i) = us(i) * qt1
         end do
C
         if (nchi .ne. 1) then
          call CONV(thetp,nthetp,psi,npsi,vn,nvn)
          call CONJ(vn,nvn,vn,nvn,Dum,Ndum)
C
C..   Modified by REG on 12/22/2005
          call ADDJ(us,nus,ONE,Dum,NDum,varwnp,us,nus)
         end if
C..   Modified by REG on 12/22/2005
         if (npsi.ne.1) then
          call CONV(thets,nthets,chi,nchi,vn,nvn)
          call CONJ(vn,nvn,vn,nvn,Dum,Ndum)
C
C..   Modified by REG on 12/22/2005
          call ADDJ(us,nus,ONE,Dum,NDum,varwns,us,nus)
         end if
         iout = 1
         if (out .eq. 1) then
          CALL writTagOneLine(Nio,'h3','@',
     $                        'MA ROOTS OF CYCLE ADJUSTED SERIES')
          iout = 0
         end if
c Here we do spectral factorization to get cycle adjusted numerator (thcya) 
c comment added DEKM 20 Feb 03
         caption0=' '
         id0=' '
         call MAK1(us,nus,thcya,nthcya,varwca,nounit,iout,caption0,0,
     &             toterr,id0,0)
C   LINES OF CODE ADDED FOR X-13A-S : 1
         IF(Lfatal)RETURN
C   END OF CODE BLOCK
  
         call CONJ(thcya,nthcya,thcya,nthcya,vn,nvn)
         if (nus .ne. nvn) then
          write (Nio,7034)
         end if
C..   Modified by REG on 12/22/2005
         toterr = ZERO
         do i = 1,nvn
          toterr = toterr + (vn(i)*varwca-us(i))**2
         end do
         dvec(1)=toterr
         call USRENTRY(dvec,1,1,1,1,1903)
         if (toterr .gt. 1.0d-2) then
          call setSf('E')
          buff2 =
     $      'THE SPECIFICATION OF SOME OF THE MODELS MAY BE UNRELIABLE'
         end if
         if (out .eq. 1) then
          write (Nio,7035) toterr
         end if

C
C
C GC 08/07/98
c        if (d.ne.0 .or. bd.ne.0) then
c          y(1) = hs
c         end if
c         if (pg .eq. 0) then
c          fname = 'SPECTSA.T3'
c          subtitle = 'SPECTRUM SA SERIES'
c          call PLOTSPECTRUM(fname,subtitle,y,300,600/mq,hs)
c         end if
c         call MULTFN(us,nus,Fs,Ns,Dum,Ndum)
c         do i = 1,Nf
c          Dum1(i) = Ff(i)
c         end do
c         Ndum1 = Nf
c         Ifunc = 5
c         do i = 1,120
c          x = (ONE/120.0d0) * pi * i
c          arg = F(x)
c          y(i) = arg
c          if (sqg .eq. 1) then
c           y(i) = y(i)**2
c          end if
c         end do
C
C GC 08/07/98
c         if (d.ne.0 .or. bd.ne.0) then
c          y(1) = ONE
c         end if
c         if ((pg.eq.0) .and. (out.eq.1)) then
c          fname = 'FILTFADJ.T4F'
c          if (sqg .eq. 1) then
c           subtitle = 'SQUARED GAIN OF SA SERIES FILTER'
c          else
c           subtitle = 'FILTER for TREND-CYCLE (F.D.)'
c          end if
c          call PLOTFILTERS(fname,subtitle,y,120,240/mq,ZERO)
c         end if

        end if
       end if 


C       OUTPUT COMPONENTS
C
c rober
       lu61=' '
       lu62=' '
       lu63=' '
       lu64=' '
       lu64I=' '
       if ((noadmiss.eq.1) .or. (noadmiss.eq.2) .or. (noadmiss.eq.0)
     &     .and. (noserie.eq.1)) then
c       call WriteLinCompMatrix()
         inquire(file=Cursrs(1:Nfilcr)//'_sum.html',opened=IsOpen)
         if (isopen) then
          if (varwna.gt.1.0d-20) then
c trend-cycle model
           if (nchis.gt.1) then
            if (chis(2).gt.0) then
             write(lu61,6100) chis(2) 
            else
             write(lu61,6110) chis(2)
            end if
            do i=3, nchis
             if (chis(i).gt.0) then
              write(lu61,6120) lu61(1:istrlen(lu61)),chis(i),i-1
             else 
              write(lu61,6130) lu61(1:istrlen(lu61)),chis(i),i-1
             end if
            end do
            lu61=lu61(1:istrlen(lu61))//') '
           end if
           if (bd+d.gt.0) then 
            if (bd+d.eq.1) then
             lu61=lu61(1:istrlen(lu61))//' &nabla;'
            else
             write(lu61,6140) lu61(1:istrlen(lu61)),bd+d
            end if 
           end if
           lu61=lu61(1:istrlen(lu61))//' p<sub>t</sub> = ' 
           if (nthetp.gt.1) then
            if (thetp(2).gt.0) then
             write(lu61,6150) lu61(1:istrlen(lu61)),thetp(2)
            else
             write(lu61,6160) lu61(1:istrlen(lu61)),thetp(2)
            end if
              do i=3, nthetp
             if (thetp(i).gt.0) then
              write(lu61,6120) lu61(1:istrlen(lu61)),thetp(i),i-1
             else  
              write(lu61,6130) lu61(1:istrlen(lu61)),thetp(i),i-1
             end if
            end do
            lu61=lu61(1:istrlen(lu61))//')'  
           end if
           lu61=lu61(1:istrlen(lu61))//' a<sub>pt</sub>,   <span>'//
     &            'a<sub>pt</sub>&#8764;N(0,'   
c          write(lu61,'(A,f12.6,")  niid</span>")') 
c     &           lu61(1:istrlen(lu61)),varwnp
           write(lu61,6280) lu61(1:istrlen(lu61)), varwnp*sqf*sqf
          end if
c seasonal model
          if (varwns.gt.1.0d-20) then
           if (npsis.gt.1) then
            if (psis(2) .gt.0) then
             write(lu62,6100) psis(2)
            else
             write(lu62,6110) psis(2)
            end if 
            do i=3, npsis
             if (psis(i) .gt.0) then             
              write(lu62,6120) lu62(1:istrlen(lu62)),psis(i),i-1
             else
              write(lu62,6130) lu62(1:istrlen(lu62)),psis(i),i-1
             end if
            end do
            lu62=lu62(1:istrlen(lu62))//')'
           end if
           if (bd.gt.0) then 
            lu62=lu62(1:istrlen(lu62))//' S s<sub>t</sub> = '
           else
            lu62=lu62(1:istrlen(lu62))//' s<sub>t</sub> = '
           end if 
           if (nthets.gt.1) then
            if (thets(2) .gt.0) then
             write(lu62,6150) lu62(1:istrlen(lu62)),thets(2)
            else
             write(lu62,6160) lu62(1:istrlen(lu62)),thets(2) 
            end if  
            do i=3,nthets
             if (thets(i) .gt.0) then  
              write(lu62,6120) lu62(1:istrlen(lu62)),thets(i) ,i-1
             else
              write(lu62,6130) lu62(1:istrlen(lu62)),thets(i) ,i-1
             end if 
            end do
            lu62=lu62(1:istrlen(lu62))//')'  
           end if
           lu62=lu62(1:istrlen(lu62))//' a<sub>st</sub>  ,<span>'//
     &            'a<sub>st</sub>&#8764;N(0,'   
           write(lu62,6280) lu62(1:istrlen(lu62)),varwns*sqf*sqf
          end if
c seasonally adjusted
          if (varwna.gt.1.0d-20) then
           if (nadjs.gt.1) then
            if (adjs(2).gt.0) then
             write(lu63,6100) adjs(2) 
            else if (adjs(2).lt.0) then
             write(lu63,6110) adjs(2)
            else
             write(lu63,6302)
            end if
            do i=3, nadjs
             if (adjs(i).gt.0) then 
              write(lu63,6120) lu63(1:istrlen(lu63)),adjs(i),i-1
             else if (adjs(i).lt.0) then
              write(lu63,6130) lu63(1:istrlen(lu63)),adjs(i),i-1
             end if
            end do
            lu63=lu63(1:istrlen(lu63))//')'
           end if
           if (bd+d.gt.0) then 
            if (bd+d.eq.1) then
             lu63=lu63(1:istrlen(lu63))//' &nabla;'
            else
c            lu63=lu63(1:istrlen(lu63))//' &nabla;<sup>'//
c     &             '</sup>'
             write(lu63,6141) lu63(1:istrlen(lu63)),bd+d,'</sup>'
            end if 
           end if
           lu63=lu63(1:istrlen(lu63))//' n<sub>t</sub> = ' 
           if (nthadj.gt.1) then
            if (thadj(2).gt.0) then
             write(lu63,6150) lu63(1:istrlen(lu63)),thadj(2)
            else if (thadj(2).lt.0) then
             write(lu63,6160) lu63(1:istrlen(lu63)),thadj(2)
            else         
             write(lu63,6332) lu63(1:istrlen(lu63))
            end if
            do i=3, nthadj
             if (thadj(i).gt.0) then
              write(lu63,6120) lu63(1:istrlen(lu63)),thadj(i),i-1
             else if (thadj(i).lt.0) then
              write(lu63,6130) lu63(1:istrlen(lu63)),thadj(i),i-1
             end if
            end do
            lu63=lu63(1:istrlen(lu63))//')'  
           end if
           lu63=lu63(1:istrlen(lu63))//' a<sub>nt</sub>  ,<span>'//
     &            'a<sub>nt</sub>&#8764;N(0,'   
           write(lu63,6280) lu63(1:istrlen(lu63)),varwna*sqf*sqf
          end if
c transitorio
          if (varwnc.gt.1.0d-20) then
           if (ncycs.gt.1) then
            if (cycs(2).gt.0) then
             write(lu64,6100) cycs(2)
            else if (cycs(2).lt.0) then
             write(lu64,6110) cycs(2)
            else
             write(lu64,6302)
            end if
            do i=3, ncycs
             if (cycs(i).gt.0) then
              write(lu64,6120) lu64(1:istrlen(lu64)),cycs(i),i-1
             else if (cycs(i).lt.0) then
              write(lu64,6130) lu64(1:istrlen(lu64)),cycs(i),i-1
             end if
            end do
            lu64=lu64(1:istrlen(lu64))//')'
           end if
           lu64=lu64(1:istrlen(lu64))//' c<sub>t</sub> = ' 
           if (nthetc.gt.1) then
            if (thetc(2).gt.0) then  
             write(lu64,6150) lu64(1:istrlen(lu64)),thetc(2)
            else if (thetc(2).lt.0) then 
             write(lu64,6160) lu64(1:istrlen(lu64)),thetc(2)
            else
             write(lu64,6332) lu64(1:istrlen(lu64))
            end if
            do i=3, nthetc
             if (thetc(i).gt.0) then  
              write(lu64,6430) lu64(1:istrlen(lu64)),thetc(i),i-1
             else if (thetc(i).lt.0) then
              write(lu64,6431) lu64(1:istrlen(lu64)),thetc(i),i-1
             end if
            end do
            lu64=lu64(1:istrlen(lu64))//')'  
           end if
           lu64=lu64(1:istrlen(lu64))//' a<sub>ct</sub>   ,<span>'//
     &            'a<sub>ct</sub>&#8764;N(0,'   
           write(lu64,6280) lu64(1:istrlen(lu64)),varwnc*sqf*sqf
          end if
          if (qt1.gt.1.0d-20) then
           write(lu64I,6450) qt1*sqf*sqf
          end if
         else
          inquire(61,opened=IsOpen) 
          if (Isopen) then
           write(61,6000)buffs
           write (61,6010) bd+d
           do i=2, nchis
            write (61,6020) chis(i)
           end do
           do i=nchis+1,5
            write (61,6010) 0
           end do
           do i=2, nthetp    
            write (61,6020) thetp(i)
           end do 
           do i=nthetp+1,8
            write (61,6010) 0
           end do
           write (61,6030) varwnp
           CALL writTag(61,'</tr>')
          end if
          inquire(63,opened=IsOpen) 
          if (Isopen) then
           write(63,6000)buffs
           write (63,6010)  d+bd
           do i=2, nadjs
            write (63,6020) adjs(i)
           end do
           do i=nadjs+1,17
            write (63,6010) 0
           end do
           do i=2, nthadj    
            write (63,6020) thadj(i)
           end do 
           do i=nthadj+1,18
            write (63,6010) 0
           end do
           write (63,6030) varwna*sqf*sqf
           CALL writTag(63,'</tr>')
          end if
c       
          inquire(62,opened=IsOpen) 
          if (Isopen) then
           write(62,6000)buffs
           write (62,6010)  bd
           do i=2, npsis
            write (62,6020) psis(i)
           end do
           do i=npsis+1,15
            write (62,6010) 0
           end do
           do i=2, nthets    
            write (62,6020) thets(i)
           end do 
           do i=nthets+1,26
            write (62,6010) 0
           end do
           write (62,6030) varwns*sqf*sqf
           CALL writTag(62,'</tr>')
          end if
c
          inquire(64,opened=IsOpen) 
          if (Isopen) then
           write(64,6000)buffs
           do i=2, ncycs
            write (64,6010) cycs(i)
           end do
           do i=ncycs+1,16
            write (64,6020) 0
           end do
           do i=2, nthetc    
            write (64,6010) thetc(i)
           end do 
           do i=nthetc+1,16
            write (64,6020) 0
           end do
           write (64,6030) varwnc*sqf*sqf
           write (64,6030) qt1*sqf*sqf
           CALL writTag(64,'</tr>')
          end if
         end if
       end if
      IF(Lsgud)call ShowComp(out,buff2,nio,
     $             chi,nchi,thetp,nthetp,varwnp,
     $             psi,nPSI,thets,nthets,varwns,
     $             ncycth,cyc,ncyc,thetc,nthetc,varwnc,qt1,
     $             chcyc,nchcyc,thadj,nthadj,varwna,svudg)
c-----------------------------------------------------------------------
 6100 format('(1+',f5.2,'B')
 6110 format('(1',f5.2,'B')
 6120 format(A,'+',f5.2,'B<sup>',i2,'</sup>') 
 6130 format(A,f5.2,'B<sup>',i2,'</sup>') 
 6140 format(A,'&nabla;<sup>',i1,'</sup>')
 6141 format(A,' &nabla;<sup>',i1,a6)
 6150 format(A,' (1+',f5.2,'B') 
 6160 format(A,' (1',f5.2,'B') 
 6280 format(A,f12.6,')  niid</span>')
 6302 format('(1')                 
 6332 format(A,' (1')
 6430 format(A,'+',f5.2,'B<sup>',i2,'</sup>')
 6431 format(A,f5.2,'B<sup>',i2,'</sup>') 
 6450 format('u<sub>t</sub> = N(0,',G13.6,')  niid')
 6000 format(A)
 6010 format('<td>',i5,'</td>') 
 6020 format('<td>',f5.2,'</td>') 
 6030 format('<td>',G13.6,'</td>')           
c----------------------------------------------------------------------
      return
      end
cc
c
cc
      subroutine ShowInvalDecomp(Out,nio,buff2,
     $                   chi,nchi,enot,psi,npsi,estar,
     $                   cyc,ncyc,ncycth,enoc,
     $                   chcyc,nchcyc,thstar,qstar,qt1,Lsgud,
     $                   IsCloseToTD,svudg)
      implicit none
c-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO
      PARAMETER(ZERO=0D0)
c-----------------------------------------------------------------------
      include 'func.i'
      include 'func2.i'
      include 'func3.i'
      include 'error.cmn'
c     INPUT PARAMETERS
      integer Out,nio,nchi,npsi,
     $       ncyc,ncycth,nchcyc,qstar
      real*8 chi(8),cyc(17),chcyc(20),thstar(27),
     $         qt1,psi(27),enot,estar,enoc
      logical Lsgud, IsCloseToTD, svudg
c     LOCAL PARAMETERS
      integer Noprint,nthetp,nthets,nthetc,nthadj,nus,i
      real*8 thetp(8),varwnp,thets(27),varwns,vf(27),ucf(32),
     $       thetc(32),varwnc,thadj(32),varwna,us(50),utf(8)
      character buff2*80
c ------------------------------------------
      if (nchi .ne. 1) then
        Ut(Nt) = ZERO
        Nut = Nt
        do i = 1,Nut
         utf(i) = Ut(i) - enot*Ft(i)
        end do
      end if
      if (npsi .ne. 1) then
        V(Ns) = ZERO
        do i = 1,Ns
         vf(i) = V(i) - estar*Fs(i)
        end do
      end if
      if (ncycth.ne.0 .or. ncyc.ne.1) then
        if (ncycth .eq. 0) then
         do i=Nuc+1,Nc
          Uc(i) = ZERO
         end do
         Nuc = Nc
        else
         do i = Nc+1,Nuc
          Fc(i) = ZERO
         end do
         Nc = Nuc
        end if
        do i = 1,Nuc
         ucf(i) = Uc(i) - enoc*Fc(i)
        end do
      end if
      if (out.eq.0) then
        Noprint=0
      else
        Noprint=1
      end if
c      Noprint=1
      call MAspectrum(Noprint,nio,buff2,
     $              chi,nchi,utf,nut,thetp,nthetp,varwnp,
     $              npsi,vf,ns,thets,nthets,varwns,
     $            cyc,ncyc,ncycth,ucf,nuc,thetc,nthetc,varwnc,
     $            chcyc,nchcyc,thstar,qstar,thadj,nthadj,varwna,
     $            us,nus,qt1,IsCloseToTD)
C   LINES OF CODE ADDED FOR X-13A-S : 1
      IF(Lfatal)RETURN
C   END OF CODE BLOCK
      buff2='NO ADMISSIBLE'
      IF(Lsgud)call ShowComp(out,buff2,nio,
     $             chi,nchi,thetp,nthetp,varwnp,
     $             psi,nPSI,thets,nthets,varwns,
     $             ncycth,cyc,ncyc,thetc,nthetc,varwnc,qt1,
     $             chcyc,nchcyc,thadj,nthadj,varwna,svudg)
      end
cc
c
cc
      subroutine ShowComp(out,buff2,nio,
     $             chi,nchi,thetp,nthetp,varwnp,
     $             psi,nPSI,thets,nthets,varwns,
     $             ncycth,cyc,ncyc,thetc,nthetc,varwnc,qt1,
     $             chcyc,nchcyc,thadj,nthadj,varwna,svudg)
      implicit none
c-----------------------------------------------------------------------
      real*8 ONE,ZERO
      parameter(ONE=1D0,ZERO=0D0)
c-----------------------------------------------------------------------
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
*      include 'indhtml.i'
      include 'transcad.i'
      INCLUDE 'hiddn.cmn'
c-----------------------------------------------------------------------
c     INPUT PARAMETERS
      integer out,nio,nchi,nthetp,nPSI,nthets,
     $        ncycth,ncyc,nthetc,nchcyc,nthadj
      character buff2*80
      real*8 chi(8),thetp(8),varwnp,psi(27),thets(27),varwns,dvec(1),
     $       cyc(17),thetc(32),varwnc,chcyc(20),thadj(32),varwna,qt1
      logical svudg
c     LOCAL PARAMETERS
      integer i
c---------------------------------
       if (out .eq. 0) then
        if (buff2(8:8) .eq. ' ') then
         CALL mkPOneLine(Nio,'bold',
     $                   'DERIVATION OF THE COMPONENT MODELS :'//buff2)
        else
         CALL mkPOneLine(Nio,'bold',
     $                   'DERIVATION OF THE COMPONENT MODELS : "'//
     $                   buff2//'"')
        end if
c roberto: 
c  ¡¡¡¡continuar por aqui
c         call AddIdx(Nio,'Models for the Components','0028',0,28)
c         write(Nio,'('</div><h3>MODELS FOR THE COMPONENTS</h3>')')
        CALL genSkip(1204)     
        CALL writTagOneLine(Nio,'h3','@','MODELS FOR THE COMPONENTS')
        if (nchi .ne. 1) then
         CALL writTagOneLine(Nio,'h4','@','TREND-CYCLE')
*          itab=itab+1
*         CALL writTagOneLine(Nio,'h5','@',
*     &                       'TREND-CYCLE NUMERATOR (MOVING AVERAGE '//
*     &                       '<abbr title="polynomial">POL.</abbr>)')
         call WrTabHtmPol(thetp,nthetp,nio,31)
c
*         itab=itab+1
*         CALL writTagOneLine(Nio,'h5','@','TREND-CYCLE DENOMINATOR '//
*     &                     '(AUTOREGRESSIVE <abbr title="polynomial">'//
*     &                       'POL.</abbr>)')
         call WrTabHtmPol(chi,nchi,nio,32)
c
*          iTab=iTab+1
*         write (Nio,7040) 'trendCycle',varwnp
* 7040    format('<p><strong>INNOVATION VARIANCE <abbr title="',
*     $         'Innovation variance of ',a,' in units of VAR(A)">',
*     $         '(*)</abbr></strong>', f12.6,'</p>')
         CALL USRENTRY(THETP,1,NTHETP,1,8,2001)
         CALL USRENTRY(CHI,1,NCHI,1,8,2002)
         dvec(1)=Varwnp
         call USRENTRY(dvec,1,1,1,1,2003)
         call WrTabHtmPol(dvec,1,nio,41)
C   END OF CODE BLOCK
         IF(varwnp.gt.ONE.or.varwnp.lt.ZERO)THEN
          CALL mkPOneLine(Nio,'bold','(*)   IN UNITS OF VAR(A)')
          IF(varwnp.gt.ONE)THEN
           WRITE (Nio,9000)'<p>','trend','greater than one','.</p>',
     &                    '<p>','.</p>'
           WRITE (Mt2,9000)'<p>','trend','greater than one','.</p>',
     &                    '<p>','.</p>'
          ELSE
           WRITE (Nio,9000)'<p>','trend','less than zero','.</p>',
     &                    '<p>','.</p>'
           WRITE (Mt2,9000)'<p>','trend','less than zero','.</p>',
     &                    '<p>','.</p>'
          END IF
          Lfatal=.true.
          if (Lsumm.gt.0.and.svudg) THEN
           WRITE(Nform,9001)'seatsadj: no'
           svudg=.false.
          END IF
          RETURN
         END IF
        end if
 9000   FORMAT(/,a,'The innovation variance of the ',a,' is ',a,',',/,
     &          '  an indication that the model is not suitable for ',
     &          'signal extraction',a,/,
     &          a,'Examine the arima model used for this ',
     &          'decomposition for possible unit roots,',/,
     &          '  and try another model',a) 
 9001   FORMAT(a)
c  resume here at difference number 97
        if (npsi .ne. 1) then
         CALL writTagOneLine(Nio,'h4','@','SEASONAL') 
*         CALL writTagOneLine(Nio,'h5','@',
*     &         '<abbr title="Seasonal">SEAS.</abbr> '//
*     $         'NUMERATOR (MOVING AVERAGE <abbr title="polynomial">'//
*     $         'POL.</abbr>)')
         call WrTabHtmPol(thets,nthets,nio,33)
*         CALL writTagOneLine(Nio,'h5','@',
*     &         '<abbr title="Seasonal">SEAS.</abbr> '//
*     $         'DENOMINATOR (AUTOREGRESSIVE <abbr title="polynomial">'//
*     $         'POL.</abbr>)')
         call WrTabHtmPol(psi,npsi,nio,34)
*         write (Nio,7040)'Seasonal',varwns
C   LINES OF CODE ADDED FOR X-13A-S : 5
c Usrentry routines added by BCM to facilitate saving
c models of the components  July 2000
        CALL USRENTRY(THETS,1,NTHETS,1,27,2004)
        CALL USRENTRY(PSI,1,NPSI,1,27,2005)
        dvec(1)=Varwns
        call USRENTRY(dvec,1,1,1,1,2006)
         call WrTabHtmPol(dvec,1,nio,42)
C   END OF CODE BLOCK
        IF(Varwns.gt.ONE.or.Varwns.lt.ZERO)THEN
         CALL mkPOneLine(Nio,'bold','(*)   IN UNITS OF VAR(A)')
         IF(varwnp.gt.ONE)THEN
          WRITE (Nio,9000)'<p>','seasonal','greater than one','.</p>',
     &                    '<p>','.</p>'
          WRITE (Mt2,9000)'<p>','seasonal','greater than one','.</p>',
     &                    '<p>','.</p>'
         ELSE
          WRITE (Nio,9000)'<p>','seasonal','less than zero','.</p>',
     &                    '<p>','.</p>'
          WRITE (Mt2,9000)'<p>','seasonal','less than zero','.</p>',
     &                    '<p>','.</p>'
         END IF
         if (Lsumm.gt.0.and.svudg) THEN
          WRITE(Nform,9001)'seatsadj: no'
          svudg=.false.
         END IF
         Lfatal=.true.
         RETURN 
        END IF
       end if
       if (ncycth.ne.0 .or. ncyc.ne.1) then
        CALL writTagOneLine(Nio,'h4','@',transLcad(1:nTransLcad)) 
*        CALL writTagOneLine(Nio,'h5','@',
*     &         transLcad(1:nTransLcad)//' NUMERATOR '//
*     $         '(MOVING AVERAGE <abbr title="polynomial">'//
*     $         'POL.</abbr>)')
*         itab=itab+1
        call WrTabHtmPol(thetc,nthetc,nio,35)
*         itab=itab+1
*        CALL writTagOneLine(Nio,'h5','@',
*     &         transLcad(1:nTransLcad)//' DENOMINATOR '//
*     $         '(AUTOREGRESSIVE <abbr title="polynomial">'//
*     $         'POL.</abbr>)')
        call WrTabHtmPol(cyc,ncyc,nio,36)
*        write (Nio,7040)transLcad(1:nTransLcad),varwnc
C   LINES OF CODE ADDED FOR X-13A-S : 5
c Usrentry routines added by BCM to facilitate saving
c models of the components  July 2000
        CALL USRENTRY(THETC,1,NTHETC,1,32,2007)
        CALL USRENTRY(CYC,1,NCYC,1,17,2008)
        dvec(1)=Varwnc
        call USRENTRY(dvec,1,1,1,1,2009)
        call WrTabHtmPol(dvec,1,nio,43)
C   END OF CODE BLOCK
        IF(Varwnc.gt.ONE.or.Varwnc.lt.ZERO)THEN
         CALL mkPOneLine(Nio,'bold','(*)   IN UNITS OF VAR(A)')
         IF(varwnp.gt.ONE)THEN
          WRITE (Nio,9000)'<p>','transitory','greater than one',
     &                    '.</p>','<p>','.</p>'
          WRITE (Mt2,9000)'<p>','transitory','greater than one',
     &                    '.</p>','<p>','.</p>'
         ELSE
          WRITE (Nio,9000)'<p>','transitory','less than zero','.</p>',
     &                    '<p>','.</p>'
          WRITE (Mt2,9000)'<p>','transitory','less than zero','.</p>',
     &                    '<p>','.</p>'
         END IF
         if (Lsumm.gt.0.and.svudg) then 
          WRITE(Nform,9001)'seatsadj: no'
          svudg=.false.
         END IF
         Lfatal=.true.
         RETURN 
        END IF
       end if
c       if (smtr .ne. 1) then
*          iTab=iTab+1
       CALL writTagOneLine(Nio,'h4','@','IRREGULAR') 
*       write (Nio,7047) qt1
* 7047  format('<p><strong>VARIANCE (*) </strong>  ',f12.6,'</p>')
C   LINES OF CODE ADDED FOR X-13A-S : 1
       dvec(1)=qt1
       call USRENTRY(dvec,1,1,1,1,2010)
       call WrTabHtmPol(dvec,1,nio,45)
C   END OF CODE BLOCK
c       end if
       CALL writTagOneLine(Nio,'h4','@','SEASONALLY ADJUSTED')
*         itab=itab+1
*       CALL writTagOneLine(Nio,'h5','@','SEASONALLY ADJUSTED '//
*     &                       'NUMERATOR (MOVING AVERAGE POLYNOMIAL)')
       call WrTabHtmPol(thadj,nthadj,nio,37)
*         itab=itab+1
*       CALL writTagOneLine(Nio,'h5','@','SEASONALLY ADJUSTED '//
*     &                       'DENOMINATOR (AUTOREGRESSIVE POLYNOMIAL)')
       call WrTabHtmPol(chcyc,nchcyc,nio,38)
*         iTab=iTab+1
*       write (Nio,7040)'SEASONALLY ADJUSTED',varwna
*       CALL mkPOneLine(Nio,'bold','(*)   IN UNITS OF VAR(A)')
C   LINES OF CODE ADDED FOR X-13A-S : 5
c Usrentry routines added by BCM to facilitate saving
c models of the components  July 2000
       CALL USRENTRY(THADJ,1,NTHADJ,1,32,2011)
       CALL USRENTRY(CHCYC,1,NCHCYC,1,20,2012)
       dvec(1)=Varwna
       call USRENTRY(dvec,1,1,1,1,2013)
       call WrTabHtmPol(dvec,1,nio,44)
C   END OF CODE BLOCK
C
C
       IF(Varwna.gt.ONE.or.Varwna.lt.ZERO)THEN
        CALL mkPOneLine(Nio,'bold','(*)   IN UNITS OF VAR(A)')
        IF(varwnp.gt.ONE)THEN
         WRITE (Nio,9000)'<p>','seasonal adjustment',
     &                   'greater than one','.</p>','<p>','.</p>'
         WRITE (Mt2,9000)'<p>','seasonal adjustment',
     &                   'greater than one','.</p>','<p>','.</p>'
        ELSE
         WRITE (Nio,9000)'<p>','seasonal adjustment','less than zero',
     &                   '.</p>','<p>','.</p>'
         WRITE (Mt2,9000)'<p>','seasonal adjustment','less than zero',
     &                   '.</p>','<p>','.</p>'
        END IF
        if (Lsumm.gt.0.and.svudg) THEN 
         WRITE(Nform,9001)'seatsadj: no'
         Svudg=.false.
        END IF
        Lfatal=.true.
        RETURN 
       END IF
*        CALL mkPOneLine(Nio,'bold','(*)   IN UNITS OF VAR(A) - 6')
      end if
      end
cc
c
cc
      subroutine MAspectrum(Noprint,nio,buff2,
     $              chi,nchi,utf,nut,thetp,nthetp,varwnp,
     $              npsi,vf,ns,thets,nthets,varwns,
     $            cyc,ncyc,ncycth,ucf,nuc,thetc,nthetc,varwnc,
     $            chcyc,nchcyc,thstar,qstar,thadj,nthadj,varwna,
     $            us,nus,qt1,IsCloseToTD)
      implicit none
c-----------------------------------------------------------------------
      DOUBLE PRECISION ONE,ZERO
      PARAMETER(ONE=1D0,ZERO=0D0)
c-----------------------------------------------------------------------
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
c     INPUT PARAMETERS
      integer Noprint,nio,nchi,nut,npsi,ns,
     $       ncyc,ncycth,nuc,nchcyc,qstar
      real*8 chi(8),utf(8),vf(27),cyc(17),ucf(32),chcyc(20),thstar(27),
     $         qt1
      logical IsCloseToTD
c     OUTPUT PARAMETERS
      integer nthetp,nthets,nthetc,nthadj,nus
      real*8 thetp(8),varwnp,thets(27),varwns,dvec(1),
     $       thetc(32),varwnc,thadj(32),varwna,us(50)
      character buff2*80,caption0*(60),id0*(60)
c     LOCAL PARAMETERS
      real*8 toterrP,toterrS,toterrC,toterrSA,Dum(80),Vn(80)
      integer nounit,nDum,nVn,i
C                ****  TREND  ****
C
      varwnp = ZERO
      caption0=' '
      id0=' '
      if (noprint.ne.1) CALL genSkip(1205)
       nounit = 0
       if (nchi .ne. 1) then
        caption0(1:23)='MA ROOTS OF TREND-CYCLE'
        id0(1:19)='trendcycle.ma.roots'
        call MAK1(utf,Nut,thetp,nthetp,varwnp,nounit,Noprint,
     $            caption0,23,toterrP,id0,19)
C   LINES OF CODE ADDED FOR X-13A-S : 1
        IF(Lfatal)RETURN
C   END OF CODE BLOCK
        dvec(1)=toterrP
        call USRENTRY(dvec,1,1,1,1,1900)
        if (noprint.ne.1) then
         if (toterrP .gt. 1.0d-2) then
           call setSf('E')
           buff2 =
     $     'THE SPECIFICATION OF SOME OF THE MODELS MAY BE UNRELIABLE'
         end if
        end if
       end if
C
       varwns = ZERO
       if (npsi .ne. 1) then
C
C                ****  SEAS.  ****
C
        caption0(1:20)='MA ROOTS OF SEASONAL'
        id0(1:17)='seasonal.ma.roots'
        call MAK1(vf,Ns,thets,nthets,varwns,nounit,noprint,
     $            caption0,20,toterrS,id0,17)
C   LINES OF CODE ADDED FOR X-13A-S : 1
        IF(Lfatal)RETURN
C   END OF CODE BLOCK
        dvec(1)=toterrS
        call USRENTRY(dvec,1,1,1,1,1901)
        if (noprint.ne.1) then
          if (toterrS .gt. 1.0d-2) then
            call setSf('E')
            buff2 =
     $     'THE SPECIFICATION OF SOME OF THE MODELS MAY BE UNRELIABLE'
          end if
        end if
       end if
C
       varwnc = ZERO
       if (ncycth.ne.0 .or. ncyc.ne.1) then
C
C                 ****  CYCLE  ****
C
        if (isCloseToTD) then
          call MAK1(ucf,Nuc,thetc,nthetc,varwnc,nounit,noprint,
     $              'MA ROOTS OF TD-STOCHASTIC',25,toterrC,
     $              'td.stochastic.ma.roots',22)
        else
          call MAK1(ucf,Nuc,thetc,nthetc,varwnc,nounit,noprint,
     $              'MA ROOTS OF TRANSITORY',22,toterrC,
     $              'transitory.ma.roots',19)
        endif
C   LINES OF CODE ADDED FOR X-13A-S : 1
        IF(Lfatal)RETURN
C   END OF CODE BLOCK
        dvec(1)=toterrC
        call USRENTRY(dvec,1,1,1,1,1902)
        if (noprint.ne.1) then
          if (toterrC .gt. 1.0d-2) then
            call setSf('E')
            buff2 =
     $     'THE SPECIFICATION OF SOME OF THE MODELS MAY BE UNRELIABLE'
          end if
        end if
       end if
C
       varwna = ZERO
       if (nchcyc.ne.1 .or. ncycth.ne.0) then
        if (npsi .eq. 1) then
         do i = 1,qstar
          thadj(i) = thstar(i)
         end do
         do i = qstar+1,nchcyc
          thadj(i) = ZERO
         end do
c         nthadj = MAX(qstar,nchcyc)
         nthadj=qstar
         varwna = ONE
        else
C
C
C  FIND MA REPRESENTATION OF SEASONALLY ADJUSTED SERIES
C
         if (isCloseToTD) then
           call CONJ(chi,nchi,chi,nchi,us,nus)
         else
           call CONJ(chcyc,nchcyc,chcyc,nchcyc,us,nus)
         endif
         do i = 1,nus
          us(i) = us(i) * qt1
         end do
         do i=nus+1,50
          us(i)=0
         end do
C
         if (nchi .ne. 1) then
          if (isCloseToTD) then
            call CONJ(thetp,nthetp,thetp,nthetp,Dum,Ndum)
          else
            call CONV(thetp,nthetp,cyc,ncyc,vn,nvn)
            call CONJ(vn,nvn,vn,nvn,Dum,Ndum)
          endif
          do i = 1,Ndum
           us(i) = us(i) + varwnp*Dum(i)
          end do
          nus = MAX(nus,Ndum)
         end if
         if (.not. IsCloseToTD) then
          if (ncycth.ne.0 .or. ncyc.ne.1) then
           call CONV(thetc,nthetc,chi,nchi,vn,nvn)
           call CONJ(vn,nvn,vn,nvn,Dum,Ndum)
           do i = 1,Ndum
            us(i) = us(i) + varwnc*Dum(i)
           end do
           nus = MAX(nus,Ndum)
          end if
         end if
         caption0(1:38)='MA ROOTS OF SEASONALLY ADJUSTED SERIES'
         id0(1:20)='seasonaladj.ma.roots'
         call MAK1(us,nus,thadj,nthadj,varwna,nounit,noprint,
     $             caption0,38,toterrSA,id0,20)
C   LINES OF CODE ADDED FOR X-13A-S : 1
         IF(Lfatal)RETURN
C   END OF CODE BLOCK
         dvec(1)=toterrSA
         call USRENTRY(dvec,1,1,1,1,1903)
         if (noprint.ne.1) then
           if (toterrSA .gt. 1.0d-2) then
             call setSf('E')
             buff2 =
     $      'THE SPECIFICATION OF SOME OF THE MODELS MAY BE UNRELIABLE'
           end if
         end if
        end if
      end if
      if (noprint.eq.0) then
        if (varwnc.lt.1.0d-10.and.(ncycth.ne.0 .or. ncyc.ne.1)) then
          call m_vc_is0(nio)
        end if
      endif
      end      
cc
c
cc
      subroutine PLOTOrigSpectrum(p,d,q,bp,bd,bq,mq,Th,Phi,BTh,BPhi)
      implicit none
c-----------------------------------------------------------------------
      DOUBLE PRECISION ONE,ZERO
      PARAMETER(ONE=1D0,ZERO=0D0)
c-----------------------------------------------------------------------
      integer n1,n12,lspect,d,bd
      parameter (n12 = 12, n1 = 1,Lspect=300)
c parametros formales
      integer p,q,bp,bq,mq    
      real*8  PHI(3*N1),TH(3*N1),BPHI(3*N1),BTH(3*N1),Output(Lspect)            
c locales   
      real*8 PHIST(2*N12+5),THSTAR(2*N12+3*N1),polDifs(2*N12+3*N1),
     $       polAR(2*N12+3*N1),fMA(32),fAR(32)
      integer i,j,k,grPhist,grThstar,fMAdim,fARdim,grpolAR,grPolDifs 
      character fname*30,subtitle*50
cc
      grpolAR = P + Bp*Mq+1
      grthstar = Q + Bq*Mq+1
      do i = 2,2*N12+3*N1
        polAR(i) = ZERO
      end do
       polAR(1) = ONE
      if (P .ne. 0) then
       do i = 1,P
         polAR(i+1) = -Phi(i)
       end do
      end if
      if (Bp .ne. 0) then
       do i = 1,Bp
        j = i * Mq+1
         polAR(j) = -Bphi(i)
        if (P .ne. 0) then
         do k = 1,P
           polAR(k+j) = Phi(k)*Bphi(i)
         end do
        end if
       end do        
      end if
c Los delta (1-B)^d 
c
      grPolDifs=bd*mq+d+1
      polDifs(1)=1
      do i = 2,2*N12+3*N1
       polDifs(i) = ZERO
      end do
      if (d.eq.0) then
       if (bd.eq.1) then
        poldifs(mq+1)=-1
       end if
      else if(d.eq.1) then
       polDifs(2)=-1
       if (bd.ne.0) then
        polDifs(mq+1)=-1
        polDifs(mq+2)=1 
       end if 
      else if (d.eq.2) then
       polDifs(2)=-2
       polDifs(3)=1
       if (bd.ne.0) then 
        polDifs(mq+1)=polDifs(mq+1)-1
        polDifs(mq+2)=2
        polDifs(mq+3)=-1
       end if
      end if
      do i = 1,2*N12+5
       phist(i)=0
      end do
      call CONV(polAR,grpolAR,polDifs,grPolDifs,phist,grPhist)
      thstar(1)=ONE
      do i = 2,2*N12+3*N1
       Thstar(i) = ZERO
      end do
      if (Q .ne. 0) then
       do i = 1,Q
        Thstar(i+1) = -Th(i)
       end do
      end if
      if (Bq .ne. 0) then
       do i = 1,Bq
        j = i * Mq+1
        Thstar(j) = -Bth(i)
        if (Q .ne. 0) then
         do k = 1,Q
          Thstar(k+j) = Th(k)*Bth(i)
         end do
        end if
       end do
      end if
c     prueba 
      call CONJ(thstar,grthstar,thstar,grthstar,fMA,fMAdim)
      call CONJ(phist,grPhist,phist,grPhist,fAR,fARdim)
      call SPC(fMA,fMAdim,fAR,fARdim,1.d0,Output)     
c generamos el fichero
cdos
*      fname='MODEL\\SPECT.T3'
cunix      
cunix      fname='MODEL/SPECT.T3'
*      subtitle='SPECTRUM MODEL SERIES'
*      call PlotSpectrum(fname,subtitle,Output,dble(Lspect),mq,1.5d0,1)
      end
cc
c
cc
      logical function TDSpectCrit(pico)
      implicit none 
      character pico(7)*2
      if (pico(7).eq.'AT') then
       TDSpectCrit=.true.
      else
       TDSpectCrit=.false.
      end if
      end
cc
c
cc
cc    Last Change Jan. 2021,sa(mpkp/2)->sa(mpkp),called by sigex.f
      integer function ResidualSeasTest(d,bd,crQS,crSNP,crpicos,nz,sa,
     $                         picSA,totalSeasSA,mq,imprimir,nio)
      implicit none
C.. Parameters ..
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
      integer mq,nz,imprimir,nio,d,bd,totalSeasSA
      character picSA(7)*2
      real*8 sa(mpkp)
c
c variables locales
      real*8 aux(mpkp),QS,SNP,media
      integer i,k,OverTest,crQs,crSNP,crpicos,ndif,j
c funciones llamadas
      logical SeasSpectCrit2
      real*8 calcQS3,kendalls,dvec(1)
      external SeasSpectCrit2,calcQS3,kendalls
c   
      OverTest=0 
      ndif=max(min(2,d+bd),1)  
      do i=1,nz
       aux(i)=sa(i)
      end do
      k=nz
      do j=1,ndif
       k=k-1
       do i=1,k
        aux(i)=aux(i+1)-aux(i)
       end do            
      end do
      media=0
      do i=1,k
       media=media+aux(i) 
      end do 
      media=media/k
      do i=1,k
       aux(i)=aux(i)-media
      end do      
*      QS=calcQS(aux,nz,mq)
*      SNP=kendalls(aux,nz,mq)
      QS=calcQS3(aux,k,mq)
      SNP=kendalls(aux,k,mq)
      if (QS.gt.9.21d0) then    
       OverTest=OverTest+1
       crQs=1
      else
       crQS=0
      end if
      if (SNP.gt.24.73d0.and.mq.eq.12.or.
     $         SNP.gt.11.35d0.and.mq.eq.4) then        
       OverTest=OverTest+1
       crSNP=1
      else
       crSNP=0 
      end if
      if (seasSpectCrit2(picSA,mq)) then
       OverTest=OverTest+1
       crpicos=1
      else
       crpicos=0
      end if
      if (imprimir.gt.0) then
       call WrResidSeasTest(OverTest,crQs,crSNP,crpicos,nio) 
      end if
      ResidualSeasTest=OverTest
      dvec(1)=OverTest*1.d0
      call usrentry(dvec,1,1,1,1,1604)
      return
      end
cc
c
cc
      subroutine WrResidSeasTest(OST,crQs,crSNP,crPeaks,nio)   
      implicit none
      integer OST,crQs,crSNP,crPeaks,nio
c           
      character spicos*3,sqs*3,sSNP*3
c
      if (crQS.eq.1) then       
       sQs='YES'
      else
       sQS='NO '
      end if
      if (crSNP.eq.1) then
       sSNP='YES'
      else 
       sSNP='NO '
      end if
      if (crPeaks.eq.1) then
       spicos='YES'
      else
       spicos='NO '
      end if
      if (nio.ne.65) then
       CALL writTagOneLine(Nio,'h3','@',
     $                     'OVERALL TEST FOR RESIDUAL SEASONALITY')
         
       CALL mkTableTag(Nio,'w60','residual seasonality test results')
      else
       CALL mkTableTag(Nio,'w60','residual seasonality test results')
       CALL mkCaption(Nio,'OVERALL TEST FOR RESIDUAL SEASONALITY')
      end if
      CALL writTag(Nio,'<tr>')
      CALL mkHeaderCellScope(Nio,0,0,'row','@',
     &                       'AUTOCORRELATION FUNCTION EVIDENCE')
      CALL mkTableCell(Nio,'@',sQs)
      CALL writTag(Nio,'</tr>')
      CALL writTag(Nio,'<tr>')
      CALL mkHeaderCellScope(Nio,0,0,'row','@',
     &                       'NON-PARAMETRIC EVIDENCE')
      CALL mkTableCell(Nio,'@',sSNP)
      CALL writTag(Nio,'</tr>')
      CALL writTag(Nio,'<tr>')
      CALL mkHeaderCellScope(Nio,0,0,'row','@','SPECTRAL EVIDENCE')
      CALL mkTableCell(Nio,'@',sPicos)
      CALL writTag(Nio,'</tr>')
      CALL writTag(Nio,'</table>')
      CALL mkPOneLine(Nio,'@','&nbsp;')

      If (OST.gt.1) then
       CALL mkPOneLine(Nio,'center','RESIDUAL SEASONALITY DETECTED '//
     $                 'IN SEASONALLY ADJUSTED SERIES')
      else if (OST .eq.1) then
       CALL mkPOneLine(Nio,'center','MILD EVIDENCE OF RESIDUAL '//
     $                 'SEASONALITY DETECTED IN SEASONALLY ADJUSTED '//
     $                 'SERIES')
      else
       CALL mkPOneLine(Nio,'center','NO RESIDUAL SEASONALITY DETECTED'//
     $                 ' IN SEASONALLY ADJUSTED SERIES')
      end if
      end

C
C
C     THIS SUBROUTINE CALCULATES C,THE SUM OF D1*A(Z) AND D2*B(Z)
C
C      INPUT  PARAMETER
C       A : FIRST POLYNOMIAL (true signs) A(1) + A(2)*COS(W) + ... +
C                                         A(MPLUS1)*COS((MPLUS1-1)*W)
C  MPLUS1 : DIMENSION  OF A
C       B : SECOND POLYNOMIAL (true signs) "    "     "       "
C  NPLUS1 : DIMENSION OF B
C       C : SUM OF A + B (true signs)  "    "     "       "
C  LPLUS1 : DIMENSION OF C
C
C     This subroutine added by REG on 12/22/2005
C
      subroutine ADDJ(a,mplus1,d1,b,nplus1,d2,c,lplus1)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Maybe Written if c=a or c=b
      real*8 a(*), b(*)
C.. In/Out Status: Read, Maybe Written if lplus1=mplus1 or lplus1=nplus1
      integer mplus1, nplus1
C.. In/Out Status: Maybe Read if c=a or c=b, Written ..
      real*8 c(*)
C.. In/Out Status: Not Read, Overwritten ..
      integer lplus1
C.. In/Out Status: Read ..
      real*8 d1, d2
C
C.. Local Scalars ..
      integer i,j,k,num
C
C.. Intrinsic Functions ..
      intrinsic MAX, MIN
C
C ... Executable Statements ...
C
C     Add the common part of the polynomials
      if (min(mplus1,nplus1) .gt. 0) then
       do i=1,min(mplus1,nplus1)
        c(i) = d1*a(i)+d2*b(i)
       end do
      end if
C
C     For degree of A > degree of B
      if (mplus1 .gt. nplus1) then
       do i=nplus1+1,mplus1
        c(i)=d1*a(i)
       end do
C
C     For degree of A V degree of B
      else if (mplus1 .lt. nplus1) then
       do i=mplus1+1,nplus1
        c(i)=d2*b(i)
       end do
      end if
C
C     Set length=degree+1 of C
      lplus1=max(mplus1,nplus1)
      
      return
      end
      
