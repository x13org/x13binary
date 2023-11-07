C     Last change:  BCM   4 Oct 2002    2:44 pm
c          ANSUB5.F
c
c
c
c     PlotSpectra: plot all the spectra that were previously estimated
c      INPUT: 
c            INCLUDE Spectra.i: where we keep all the spectra
c            MQ: the annual frequency 
c            HS: the maximum value ploted in the graphs
c            nchi: length of chi where is keep the trend AR polynomial (1=>no trend)
c            ncycth: length-1 of Transitory MA (>0 => transitory)
c            ncyc:    length of Transitory AR polynomial (>1=>Transitory)
c            nchcyc:   length of Transitory+trend AR polynomial 
c            npsi: length of Seasonal AR polynomial (>1 => Seasonal)
c            hpcycle: if the user wanted HP cycle (>0=>BC and long Term Trend)
c      OUTPUT: FILES graph\spectra\*.T3
*      subroutine plotSpectra(MQ,HS,nchi,ncycth,ncyc,nchcyc,npsi,hpcycle,
*     $                       varwnc)
*      implicit none
*      include 'spectra.i'
*      integer MQ,nchi,ncycth,ncyc,nchcyc,npsi,hpcycle
*      real*8 hs,varwnc
*c     LOCAL VARIABLES
*      include 'transcad.i'
*      character fname*30,subtitle*50,longTermCad*22
*      integer i
*      character SCM
*      real*8 Ymaxgraph
*C
*      character gettmcs
*      integer ISTRLEN
*      EXTERNAL gettmcs,ISTRLEN
*c   
*      Ymaxgraph=1.5d0
*      If (HPcycle.eq.1) then
*        LongTermCad='LONG TERM TREND'
*      else if (HPcycle.eq.2) then
*        LongTermCad='SA series without BC'
*      else
*        LongTermCad='Series without BC'
*      end if
*      i=Lspect
*c Ahora lo calculamos al comienzo porque aqui sale el modelo que ha cambiado seats
*c si lo cambia antes de entrar a la descomposicion
*c      fname='SPECT.T3'
*c     subtitle='SPECTRUM MODEL SERIES'
*c     call PlotSpectrum(fname,subtitle,spect,dble(Lspect),mq,hs,1)
*      SCM=gettmcs()
*      if (SCM.eq.'Y') then
*
*       fname='SPECTSE.T3'
*       subtitle='SPECTRUM MODEL SERIES SEATS'
*       call PlotSpectrum(fname,subtitle,spectse,dble(Lspect),mq,hs,1)
*      end if
*      fname='SPECTEI.T3'
*      subtitle='SPECTRUM MODEL: IRREGULAR ESTIMATOR'
*      call PlotSpectrum(fname,subtitle,spectei,dble(Lspect),mq,-10d0,1)
*      if (nchi.ne.1) then
*        fname='SPECTT.T3'
*        subtitle='SPECTRUM MODEL TREND CYCLE'
*        call PlotSpectrum(fname,subtitle,spectt,
*     $                    dble(Lspect),mq,Ymaxgraph,0)
*        fname='SPECTET.T3'
*        subtitle='SPECTRUM MODEL: TREND-CYCLE ESTIMATOR'
*        call PlotSpectrum(fname,subtitle,spectet,dble(Lspect),mq,
*     $                    Ymaxgraph,0)
*      end if
*      if (varwnc.gt.1.0D-10 .and.(ncycth.ne.0 .or. ncyc.ne.1)) then
*        fname='SPECTY.T3'
*        write(subtitle,1001)transLcad(1:nTransLcad)
* 1001   FORMAT('SPECTRUM MODEL ',A)
*        call PlotSpectrum(fname,subtitle,specty,dble(Lspect),mq,-10D0,1)
*        fname='SPECTEY.T3'
*        write(subtitle,1001)transLcad(1:nTransLcad)//' ESTIMATOR'
*        call PlotSpectrum(fname,subtitle,spectey,dble(Lspect),
*     $                    mq,-10D0,1)
*      end if
*      if (mq.ne.1 .and. npsi.ne.1) then
*        fname='SPECTS.T3'
*        subtitle='SPECTRUM MODEL SEASONAL'
*        call PlotSpectrum(fname,subtitle,spects,dble(Lspect),mq,
*     $                     Ymaxgraph,1)
*        fname='SPECTES.T3'
*        subtitle='SPECTRUM MODEL: SEASONAL ESTIMATOR'
*        call PlotSpectrum(fname,subtitle,spectes,dble(Lspect),mq,
*     $                    Ymaxgraph,1)
*      end if
*      if ((varwnc.gt.1.0D-10 .and.(ncycth.ne.0 .or. nchcyc.ne.1))
*     $    .and. npsi.ne.1) then
*        fname='SPECTSA.T3'
*        subtitle='SPECTRUM MODEL SA SERIES'
*        call PlotSpectrum(fname,subtitle,spectsa,dble(Lspect),mq,
*     $                    Ymaxgraph,1)
*        fname='SPECTESA.T3'
*        subtitle='SPECTRUM MODEL: SA SERIES ESTIMATOR'
*        call PlotSpectrum(fname,subtitle,spectesa,dble(Lspect),mq,
*     $                    Ymaxgraph,1)
*      end if
*      if (hpcycle.gt.0) then
*        fname='SPECTBC.T3'
*        subtitle='SPECTRUM MODEL BUSINESS CYCLE'
*        call PLOTSPECTRUM(fname,subtitle,spectbc,dble(Lspect),mq,hs,0)
*        fname='SPECTM.T3'
*        subtitle='SPECTRUM MODEL '//LongTermCad(1:istrlen(LongTermCad))
*        call PLOTSPECTRUM(fname,subtitle,spectm,dble(Lspect),mq,hs,0)
*        fname='SPECTEBC.T3'
*      subtitle='SPECTRUM MODEL: BUSINESS CYCLE ESTIMATOR'
*        call PLOTSPECTRUM(fname,subtitle,spectebc,dble(Lspect),mq,hs,0)
*        fname='SPECTEM.T3'
*        subtitle='SPECTRUM MODEL: '//
*     $          LongTermCad(1:istrlen(LongTermCad))//' ESTIMATOR'
*        call PLOTSPECTRUM(fname,subtitle,spectem,dble(Lspect),mq,hs,0)  
*      end if
*      end
c     SPC:  computes the spectrum of a component
c   INPUT PARAMETERS
c      A: cosine transform of MA of the model
c      na: dimension of A
c      E: cosine transform of AR of the model
c      ne: dimmension of E
c      Var: variance of innovations of the model
c   OUTPUT PARAMETERS
c      spect(1:Lspect): theoretical spectrum of the model for w=(1:Lspect)*pi/Lspect
      subroutine SPC(a,na,e,ne,Var,spect)
      implicit none
      include 'spectrum.i'
      include 'func5f1.i'
      include 'testf1.i'
      real*8 pi
      parameter (pi=3.14159265358979d0)
      real*8 ONE
      parameter (ONE=1.0d0)
c     INPUT PARAMETERS
      integer na,ne
      real*8 A(na),E(ne),Var
c     OUTPUT PARAMETERS
      real*8 spect(Lspect)
c     EXTERNAL
      external FUNC1
      real*8 FUNC1
c     LOCAL VARIABLES
      integer i
      real*8 x
c 
      Ifunc1=5
      do i=1,na
        Dumf1(i)=a(i)
      endDo
      Ndumf1=na
      do i=1,ne
        Dum1f1(i)=e(i)
      enddo
      nD1f1=ne
      do i=1,Lspect
        x=(ONE/Lspect)*pi*i
        spect(i)=FUNC1(x)
        if (spect(i) .lt. -1.0D-2) then
          spect(i)=1000.0d0
        end if
        spect(i)=Var*spect(i)/(2*pi)
      enddo
      end
C
C SPCEST COMPUTES THE SPECTRUM OF THE ESTIMATOR OF THE COMPONENT
C
C   INPUT PARAMETERS
C      A : MA in cosine transform OF THE COMPONENT
C     NA : DIMENSION OF A
C      B : AR in cosine transform OF OTHER COMPONENT 1
C     NB : DIMENSION OF B
C      C : AR in cosine transform OF OTHER COMPONENT 2
C    NCC : DIMENSION OF C
C      D : AR is cosine transform OF THE COMPONENT
C     ND : DIMENSION OF D
C      E : MA in cosine transform OF THE FULL MODEL
C     NE : DIMENSION OF E
c   OUTPUT PARAMETERS
c    spect(1:Lspect): theoretical spectrum of the estimator for w=(1:lspect)*pi/Lspect 
C
C
      subroutine SPCEST(a,na,b,nb,c,ncc,d,nd,e,ne,spect)
C
C.. Implicits ..
      implicit none
      include 'spectrum.i'
      real*8 ONE
      parameter (ONE=0.0d0)
C
C.. INPUT Arguments ..
      integer na,nb,ncc,nd,ne
      real*8 a(na),b(nb),c(ncc),d(nd),e(ne)
c
c   OUTPUT ARGUMENTS
      real*8 spect(Lspect)
C
C.. Local Scalars ..
      integer i,ndum2,ndum5
      real*8 arg,pi,x
C
C.. Local Arrays ..
      real*8 dum2(170),dum5(180)
C
C.. External Functions ..
      real*8 FUNC1
      external FUNC1
C
C.. External Calls ..
      external MULTFN
      include 'func5f1.i'
      include 'hspect.i'
      include 'spe.i'
      include 'testf1.i'
C
C ... Executable Statements ...
C
      pi = 3.14159265358979d0
       call MULTFN(a,na,a,na,dum2,ndum2)
       call MULTFN(d,nd,e,ne,Dum1f1,ND1f1)
       call MULTFN(dum2,ndum2,b,nb,dum5,ndum5)
       if (ncc .eq. 1) then
        do i = 1,ndum5
         Dumf1(i) = dum5(i)
        end do
        Ndumf1 = ndum5
       else
        call MULTFN(dum5,ndum5,c,ncc,Dumf1,Ndumf1)
       end if
       Ifunc1 = 5
       do i = 1,Lspect
        x = (ONE/Lspect) * pi * i
        arg = FUNC1(x)
        if (arg .lt. -1.0d-2) then
         arg = 10.0d10
        end if
        spect(i) = arg
        spect(i) = spect(i) / (2*pi)
       end do
      end
c
c
c     PHIbc(B)Bc=THETbc(B)Abt  Abt~niid(0,Vbc)   Bc:Business Cycle Component
c     PHIm(B)Mt=THETm(B)Amt    Amt~niid(0,Vm)    Mt:Long Term Trend Component
c     THstar(1:qstar) :MA of the serie
c     PHInc(1:nPHInc)*PHIc(B): AR serie, where PHIc:is PHIp(if hpcycle=1)
c                                                   is PHIsa(if hpcycle=2)
c                                              PHInc=1 (if HPcycle=3)
c     (Vm/Vc)*1/HPTH(B)HPTH(F):Is the filter over C(P,SA or Xt) to obtain M(long term trend)
c     MQ: the frequency of the observations.
      subroutine getBcSpectra(PHIbc,nPHIbc,THETbc,nTHETbc,Vbc,
     $                      PHIm,nPHIm,THETm,nTHETm,Vm,
     $                      THstar,qstar,PHI,p,d,bphi,bp,bd,MQ)
      implicit none
      INCLUDE 'srslen.prm'
      include 'func5f1.i'
      include 'component.i'
      include 'spectra.i'
      include 'dimensions.i'
      real*8 TWO
      parameter (TWO=2.0d0)
c     INPUT PARAMETERS
      real*8 PHIbc(MaxCompDim),THETbc(MaxCompDim),Vbc,
     $      PHIm(MaxCompDim),THETm(MaxCompDim),Vm,
     $      THstar(maxTH),PHI(*),bphi(*)
      integer nPHIbc,nTHETbc,nPHIm,nTHETm,qstar,p,d,bp,bd,MQ
c     EXTERNAL CALLS
      external getAR,getSpectrum
c     CONSTANT PARAMETERS
      real*8 pi
      parameter(pi=3.14159265358979d0)
c     LOCAL PARAMETERS
      real*8 AR(MaxCompDim),spectX(300)
      integer nAR,i
c
      call getSpectrum(THETbc,nTHETbc,PHIbc,nPHIbc,SpectBC)
      call getSpectrum(THETm,nTHETm,PHIm,nPHIm,SpectM)
      call getAR(phi,p,d,bphi,bp,bd,MQ,AR,nAR)
      call getSpectrum(THstar,qstar,AR,nAR,SpectX)
      do i=1,300
        spectBC(i)=spectBC(i)*Vbc
        spectM(i)=spectM(i)*Vm
        specteBC(i)=spectBC(i)*spectBC(i)/(TWO*pi*spectX(i))
        specteM(i)=spectM(i)*spectM(i)/(TWO*pi*spectX(i))
        spectBC(i)=spectBC(i)/(TWO*pi)
        spectM(i)=spectM(i)/(TWO*pi)
      end do
      end
c
c
c    GetSpectrum given a model Numerator(B)Mt=Denominator(B), 
c          return its spectrum in Spect(1:300) for frequencies pi*(1:300)/300
c
      subroutine getSpectrum(Numerator,nNum,Denominator,nDen,spect)
      implicit none
      include 'component.i'
      include 'func5f1.i'
      include 'testf1.i'
      include 'spectrum.i'
c     INPUT PARAMETERS
      real*8 Numerator(*),Denominator(*),spect(*)
      integer nNum,nDen
c     CONSTANT PARAMETERS
      real*8 pi
      parameter(pi=3.14159265358979D0)
      real*8 ONE
      parameter (ONE=1.0d0)
c     LOCAL PARAMETERS
      integer i
      real*8 x,arg
c     EXTERNAL
      external CONJ,FUNC1
      real*8 FUNC1
c
      call CONJ(Denominator,nDen,Denominator,nDen,Dum1f1,nD1f1)
      call CONJ(Numerator,nNum,Numerator,nNum,Dumf1,Ndumf1)
      Ifunc1=5
      DO i=1,Lspect
        x=(ONE/Lspect)*pi*i
        arg=Func1(x)
        if (arg .lt. -1.0d-2) then
         arg = 10.0d10
        end if
        spect(i)=abs(arg)
      END DO
      end
C     getAR: return in AR(B)=phi(B)*bphi(B^mq)*[(1-B)^d]*[(1-B^mq)^bd]
      subroutine getAR(phi,p,d,bphi,bp,bd,mq,AR,nAR)
      implicit none
      include 'component.i'
      real*8 ZERO,ONE,MONE
      parameter (ZERO=0.0d0,ONE=1.0d0,MONE=-1.0d0)
c     INPUT PARAMETERS
      real*8 phi(*),bphi(*)
      integer p,d,bp,bd,mq
c     OUTPUT PARAMETERS
      real*8 AR(*)
      integer nAR
c     LOCAL PARAMETERS
      real*8 Delta(2),Deltas(13),tmp(60),tmp2(60),bphiE(60)
      integer i,j,ntmp,ntmp2
c
      External CONV
c
      Delta(1)=ONE
      Delta(2)=-1.0D0
      tmp(1)=ONE
      ntmp=1
      ntmp2=1
      tmp2(1)=ONE
      Do i=1,p+1
        tmp(i)=phi(i)
      end do
      ntmp=p+1
      Do j=1,d
        Call Conv(Delta,2,tmp,ntmp,tmp2,ntmp2);
        Do i=1,ntmp2
          tmp(i)=tmp2(i)
        end do
        ntmp=ntmp2
      end do
      if (bp.gt.0) then
        do i=1,mq*bp+1
          bphiE(i)=ZERO
        end do
        do i=1,bp
          bphiE(i*mq+1)=bphi(i+1)
        end do
        call conv(tmp,ntmp,bphi,bp*mq+1,tmp2,ntmp2)
        do i=1,ntmp2
          tmp(i)=tmp2(i)
        end do
        ntmp=ntmp2
      end if
      if (bd.gt.0) then
        Deltas(1)=ONE
        do i=2,mq
          Deltas(i)=ZERO
        end do
        Deltas(mq+1)=-1.0d0
        Do j=1,bd
          call CONV(tmp,ntmp,Deltas,mq+1,tmp2,ntmp2)
          do i=1,ntmp2
            tmp(i)=tmp2(i)
          end do
          ntmp=ntmp2
        end do
      end if
      do i=1,ntmp
        AR(i)=tmp(i)
      end Do
      nAR=ntmp
      end
c     TruncaSpectra: para los espectros teóricos trunca
c         todos los valores próximos a los picos estacionales y W=0
      subroutine truncaSpectra(d,bd,mq,maxValue,nchi,
     $                ncycth,nchcyc,npsi,HpCycle,varwnc)
      implicit none
      include 'spectra.i'
c     INPUT PARAMETERS
      integer d,bd,mq,nchi,ncycth,nchcyc,npsi,HpCycle
      real*8 maxValue,varwnc
c     LOCAL VARIABLES
      integer iTO,wide
c--------------------------------------------------------------------
      wide=Lspect/MQ
      iTO=1
      if ((d+bd).gt.0) then
        call truncaValor(spect,Lspect,wide,iTO,maxValue)
        call truncaValor(spectse,Lspect,wide,iTO,maxvalue)
      end if
      if (bd.gt.0) then
        call truncaSeasSpect(spect,mq,maxValue)
        call truncaSeasSpect(spectSE,mq,maxValue)
      end if
      if (nchi.ne.1 .and. (d+bd).gt.0) then
        call truncaValor(spectt,Lspect,wide,iTO,maxvalue)
        call truncaValor(spectet,Lspect,wide,iTO,maxValue)
      end if
      if (mq.ne.1 .and. npsi.ne.1 .and. bd.ge.1) then
        call truncaSeasSpect(spects,mq,maxValue)
        call truncaSeasSpect(spectes,mq,maxValue)
      end if
      if ((varwnc.gt.1.0D-10 .and.(ncycth.ne.0 .or. nchcyc.ne.1)) 
     $       .and. npsi.ne.1 .and. (bd+d).gt.0) then
        call truncaValor(spectSA,Lspect,wide,iTO,maxValue)
        call truncaValor(specteSA,LSpect,wide,iTO,maxValue)
      end if
      if (HPCYCLE.ge.1 .and. (d+bd).gt.0) then
        call truncaValor(spectM,Lspect,wide,iTO,maxValue)
        call TruncaValor(specteM,Lspect,wide,iTO,maxValue)
      end if
      if (HPCYCLE.ge.1 .and. (d+bd).ge.3) then
        call truncaValor(spectBC,Lspect,wide,iTO,maxValue)
        call truncaValor(specteBC,Lspect,wide,iTO,maxValue)
      end if
      if (HPCYCLE.eq.3 .and .bd.ge.1) then
        call truncaSeasSpect(spectM,mq,maxValue)
        call truncaSeasSpect(specteM,mq,maxValue)
        call truncaSeasSpect(spectBc,mq,maxValue)
        call truncaSeasSpect(specteBC,mq,maxValue)
      end if
      end
c     TruncaSeasSpect: trunca a un valor máximo en la proximidad
c              de las frecuencias estacionales del espectro.
      subroutine truncaSeasSpect(spect,mq,maxValue)
      implicit none
      include 'spectrum.i'
c   INPUT PARAMETERS
      real*8 maxValue
      integer mq
c   INPUT/OUTPUT parameters
      real*8 spect(Lspect)
c   EXTERNAL
      external truncaValor
c   LOCAL parameters
      integer i,iTo,wide
c -----------------------------------
      wide=Lspect/mq
      do i=1,mq/2
        iTo=2*i*wide
        call truncaValor(spect,Lspect,iTo-wide,iTo,maxValue)
        if (iTo.lt.Lspect) then
          call truncaValor(spect,Lspect,iTo+wide,iTo,maxValue)
        end if
      enddo
      end
c
      subroutine truncaValor(arr,nArr,ifrom,iTo,maxVal)
      implicit none
c     INPUT PARAMETERS
      integer narr,ifrom,ito
      real*8 maxVal
c     INPUT/OUTPUT
      real*8 arr(nArr)
c     LOCAL VARIABLES
      integer i,i1,foundMax
c
      foundMax=0
      i1=iFrom
      if (iFrom.lt.iTo) then
        do while((foundMax.eq.0) .and. (i1.le.iTo))
          if (arr(i1).gt.maxVal) then
            foundMax=1
          else
            i1=i1+1
          end if
        endDo
        do i=i1,iTo
          arr(i)=maxVal
        endDo
      else
        do while((foundMax.eq.0) .and. (i1.ge.ito))
          if (arr(i1).gt.maxVal) then
            foundMax=1
          else
            i1=i1-1
          end if
        endDo
        do i=i1,iTo,-1
          arr(i)=MaxVal
        endDo
      end if
      end
*c
*c     maxValT nos devuelve el mayor valor no infinito de los máximos de los espectros
*      real*8 function maxValT(ncycth,ncyc,npsi,Hpcycle,d,bd,mq,varwnc)
*      implicit none
*      include 'spectra.i'
*c     INPUT PARAMETERS
*      integer ncycth,ncyc,npsi,HPcycle,d,bd,mq
*      real*8 varWnc
*c     EXTERNAL FUNCTIONS
*      external maxVspec
*      real*8 maxVspec
*c     LOCAL VARIABLES
*      real*8 maxVal1,mVal2
*c
*      mVal2=maxVspec(spectei,Lspect)
*      if ((d+bd).eq.0) then
*        maxVal1=maxVspec(spectse,Lspect)
*        if (maxVal1.gt.mVal2) mVal2=maxVal1
*      end if
*      if (varwnc.gt.1.0D-10 .and.(ncycth.ne.0 .or. ncyc.ne.1)) then
*        maxVal1=maxVspec(specty,Lspect)
*        if (maxVal1.gt.mVal2) mVal2=maxVal1
*      end if
*      if (mq.ne.1 .and. npsi.ne.1 .and. bd.eq.0) then
*        maxVal1=maxVspec(spects,Lspect)
*        if (maxVal1.gt.mVal2) mVal2=maxVal1
*      end if
*      if ((bd+d).le.2 .and. (HPCYCLE.eq.1 .or.  HPCYCLE.eq.2) .or.
*     $     (HPCYCLE.eq.3 .and. bd.eq.0)) then
*         maxVal1=maxVspec(spectBC,Lspect)
*        if (maxVal1.gt.mVal2) mVal2=maxVal1
*      else if (HPCYCLE.eq.3 .and. (d+bd).le.2)then
*        maxVal1=maxVspec(spectBC,Lspect/MQ)
*        if (maxVal1.gt.mVal2) mVal2=maxVal1
*      end if
*      maxValT=mVal2;
*      end
c
      real*8 function maxVspec(arr,nArr)
      implicit none
c     INPUT PARAMETERS
      integer nArr
      real*8 arr(nArr)
c     LOCAL VARIABLES
      integer i
      real*8 max1
c
      max1=-1.0D30
      do i=1,nArr
        if (max1.lt.arr(i)) then
          max1=arr(i)
        end if
      endDo
      maxVspec=max1
      end
c
c     getSGmfilter: obtain the Squared Gain filter for Long Term Trend
c     INPUT:
c         ARnc(1:nARnc): polynomial of the roots that are in AR of series but not in ARp (Trend,SA or AR serie depending on HPcycle) 
c         MAm(1:nMAm): Moving average  of the long term trend
c         THhp(1:nTHhp): the THhp that only depend on the landa of HP filter
c         THstar(1:Qstar): the MA of the series
c         Vc: tha variance of the Long Term Trend in terms of Va
c         nSG: the number of values plus one that will output in SG
c         SQG: 1:squared gain, ELSE: Gain of the filter
c     OUTPUT:
c         SG: the squared Gain of the filter
c         SG(1:nSG+1)=Fourier(Vm*ACF(ARnc(B)*MAm(B)/[THhp(B)*THstar(B)], w=pi*(0:nSG)/nSG).^2
      subroutine getSGmfilter(ARnc,nARnc,MAm,nMAm,THhp,nTHhp,
     $                    THstar,Qstar,Vc,SG,nSG,SQG)
      implicit none
      include 'component.i'
      include 'func5f1.i'
      include 'testf1.i'
c     CONSTANT PARAMETERS
      real*8 pi
      parameter(pi=3.14159265358979D0)
      real*8 ONE,ONE20
      parameter (ONE=1.0d0,ONE20=120.0d0)
c     INPUT PARAMETERS
      integer nARnc,nMAm,nTHhp,Qstar,nSG,SQG
      real*8 ARnc(*),MAm(*),THhp(*),THstar(*),Vc
c     OUTPUT PARAMETERS
      real*8 SG(*)
c     EXTERNAL
      external CONJ,CONV,FUNC0
      real*8 FUNC0
C     LOCAL PARAMETERS
      real*8 arg,x,nume(MaxCompDim),Deno(MaxCompDim)
      integer i,nNume,nDeno
c ---------------------------------------------------------
      call CONV(ARnc,nARnc,MAm,nMAm,Nume,nNume)
      call CONV(THhp,nTHhp,THstar,Qstar,Deno,nDeno)
      call CONJ(Nume,nNume,Nume,nNume,Dumf1,Ndumf1)
      call CONJ(Deno,nDeno,Deno,nDeno,Dum1f1,nD1f1)
      Ifunc1=5
      do i=0,nSG
        x=(ONE/ONE20)*pi*i
        arg=Vc*FUNC0(x)
        if (SQG.eq.1) then
          SG(i+1)=arg*arg
        else
          SG(i+1)=arg
        end if
      enddo
      end
c
c
c
c     HPSGfilters: obtain the Squared Gain filter for Long Term Trend
c     INPUT:
c         ARnp(1:nARnp): polynomial of the roots that are in AR of series but not in ARp (Trend,SA or AR serie depending on HPcycle) 
c         MAm(1:nMAm): Moving average  of the long term trend
c         Vm: the variance of the Long Term Trend in terms of Va
c         MAbc(1:nMAbc): Moving average  of the Business Cycle
c         Vbc: the variance of the Business Cycle in terms of Va
c         THhp(1:nTHhp): the THhp that only depend on the landa of HP filter
c         THstar(1:Qstar): the MA of the series
c         d,bd,mq
c         SQG: 1: squared gain of the filter, ELSE: Gain of the Filter
c         plotg: 0 -->call PlotFilters
c     OUTPUT:
c         SGm: the squared Gain of the LONG TERM TREND filter
c         SGm(1:120+1)=Fourier(Vm*ACF(ARnp(B)*MAm(B)/[THhp(B)*THstar(B)], w=pi*(0:120)/120).^2
c         SGbc: the squared Gain of the LONG TERM TREND filter
c         SGbc(1:120+1)=Fourier(Vbc*ACF(ARnp(B)*MAbc(B)/[THhp(B)*THstar(B)*Delta^min(2,d+bd)], w=pi*(0:120)/120).^2
      subroutine HPSGfilters(HPcycle,ARnp,nARnp,MAm,nMAm,MAbc,nMAbc,Vbc,
     $              HPth,nHPth,Vm,THstar,Qstar,d,bd,mq,SQG,plotG)
      implicit none
      include 'component.i'
c     CONSTANT PARAMETERS
      real*8 pi
      integer nSG
      parameter(pi=3.14159265358979D0,nSG=120)
      real*8 ONE
      parameter (ONE=1.0d0)
c     INPUT PARAMETERS
      integer HPcycle
      integer nARnp,nMAm,nMAbc,nHPth,Qstar,d,bd,mq,SQG,plotG
      real*8 ARnp(*),MAm(*),MAbc(*),HPth(*),THstar(*),Vbc,Vm
c     EXTERNAL
      integer istrlen
      external CONV,getSGmFilter,istrlen
c     LOCAL VARIABLES
      integer nMAmDelta
      real*8 MAmDelta(MaxCompDim),
     $       Delta(3),SGm(nSG+1),SGbc(nSG+1)
      character fname*30,subtitle*50,LongTermCad*22
c ---------------------------------------------------------
      If (HPcycle.eq.1) then
        LongTermCad='LONG TERM TREND'
      else if (HPcycle.eq.2) then
        LongTermCad='SA series without BC'
      else
        LongTermCad='Series without BC'
      end if
      call getSGmFilter(ARnp,nARnp,MAm,nMAm,
     $               HPth,nHPth,THstar,Qstar,Vm,SGm,nSG,SQG)
      if ((d+bd).ge.1) then
        SGm(1)=ONE
      end if
*      if (plotg.eq.0) then
*       fname='FILTFM.T4F'
*       if (SQG.eq.1) then
*        subtitle='SQUARED GAIN OF '//
*     $        LongTermCad(1:istrlen(Long TermCad))
*       else
*        subtitle='GAIN OF '//LongTermCad(1:istrlen(LongTermCad))
*     $        //' FILTER'
*       end if
*       call PlotFilters(fname,subtitle,SGm,nSG+1,mq,0.0d0,pi,0)
*      end if
      Delta(1)=ONE
      Delta(2)=-2.0D0
      Delta(3)=ONE
      call Conv(Delta,3,MAm,nMAm,MAmDelta,nMAmDelta)
      call getSGmfilter(ARnp,nARnp,MAmDelta,nMAmDelta,
     $      HPth,nHPth,THstar,Qstar,Vbc,SGbc,nSG,SQG)
*      if (plotg.eq.0) then 
*       fname='FILTFBC.T4F'
*       if (SQG.eq.1) then
*        subtitle='SQUARED GAIN OF BUSINESS CYCLE'
*       else
*        subtitle='GAIN OF BUSINESS CYCLE FILTER'
*       end if
*       call PLOTFILTERS(fname,subtitle,SGBC,nSG+1,mq,0.0d0,pi,0)
*      end if
      end
C
C
C   LINES OF CODE COMMENTED FOR X-13A-S : 65
C      subroutine GETCOMMLINE(nover,ioneout,outdir,graphdir,infile,
C     $                       outfile,itable)
CC
CC.. Implicits ..
C      implicit none
CC
CC.. Formal Arguments ..
C      integer nover,ioneout,itable
C      character outdir*180,graphdir*180,infile*180,outfile*180
CC.. Local Scalars ..
C      integer i
C      integer*4 nar
C      character buff*180
CC
CC.. External Functions ..
C      integer IARGC
C      external IARGC
CC
CC.. External Calls ..
C      external GETARG
CC
CC ... Executable Statements ...
CC
CC
CC
C      outdir = 'output'
C      graphdir = 'graph'
C      infile = 'serie'
C      outfile=''
C      nover = 0
C      ioneout = 0
C      itable = 0
C      nar = IARGC()
C      if (nar .lt. 1) return
C      i = 1
C      do while (.true.)
C       call GETARG(i,buff)
C       if (buff .eq. '-s') then
C        nover = 1
C       end if
C       if (buff .eq. '-t') then
C        itable = 1
C       end if
C       if (buff .eq. '-i') then
C        i = i + 1
C        call GETARG(i,infile)
C       end if
C       if (buff .eq. '-o') then
C        i = i + 1
C        call GETARG(i,outdir)
C       end if
C       if (buff .eq. '-g') then
C        i = i + 1
C        call GETARG(i,graphdir)
C       end if
C       if (buff .eq. '-OF') then
C        ioneout = 1
C        i = i + 1
C        call GETARG(i,outfile)
C       end if
C       i = i + 1
C       if (nar .lt. i) return
C      end do
C      end
C   END OF CODE BLOCK
C
C
C
      subroutine PROUT1(mq,lam,type,ioneout,nz,titleg,tramo,interp,init,
     $                  p,d,q,bd,bp,bq,out,npers,nyer,npread)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
      integer mq,lam,type,ioneout,nz,tramo,interp,init,p,d,q,bd,bp,bq,
     $        out,npers,nyer,npread
      character titleg*80
C
C.. Local Scalars ..
      character buff*80,buff1*80
C
C.. External Functions ..
      integer ISTRLEN
      external ISTRLEN
      include 'stream.i'
      include 'htmlout.cmn'
C
C ... Executable Statements ...
C
*      if (out .eq. 2) then
*       write (buff,'(I2,2X,A)') mq, 'OBS. PER YEAR'
*       if (mq .eq. 12) then
*        buff = 'MONTHLY'
*       end if
*       if (mq .eq. 4) then
*        buff = 'QUARTERLY'
*       end if
*       if (mq .eq. 1) then
*        buff = 'ANNUAL'
*       end if
*       if (ioneout .eq. 1) then
*        if (HTML .eq. 1) then
*         write (Nio,'("<br><br><br>")')
*        else
*         write (Nio,'(///)')
*        end if
*       end if
*       if (HTML .eq. 1) then
*        write (nio,'(''<TABLE BORDER="0" CELLPADDING="6" '',
*     &              ''CELLSPACING="0" ALIGN="JUSTIFY">'')')
*        write (Nio,'("<tr><th>",A,"</th><th>",A,"</th><th>",I3,2X,A,
*     &               "</th><th>",A,2X,I2,A,I4,"</th></tr>")')
*     $        titleg(1:ISTRLEN(titleg)), buff(1:ISTRLEN(buff)), nz,
*     $        'OBSERVATIONS', 'STARTS :', npers, '/', nyer
*        write (Nio,'("</table>")')
*        if (npread .eq. 1) then
*         write (Nio,'("<br><b>",A,"</b>")') 'PREADJUSTED WITH regARIMA'
*        end if
*        if ((tramo.eq.1) .and. (interp.eq.1)) then
*         write (Nio,'(''<br><b>MISSING OBSERVATIONS IN ORIGINAL'',
*     $                '' SERIES HAVE BEEN INTERPOLATED</b>'')')
*       end if
*       else
*       write (Nio,'(A,4X,A,2X,I3,2X,A,4X,A,2X,I2,A,I4)')
*     $       titleg(1:ISTRLEN(titleg)), buff(1:ISTRLEN(buff)), nz,
*     $       'OBSERVATIONS', 'STARTS :', npers, '/', nyer
*       if (npread .eq. 1) then
*C   LINES OF CODE COMMENTED FOR X-13A-S : 1
*        write (Nio,'(2X,A)') 'PREADJUSTED WITH TRAMO'
*C   END OF CODE BLOCK 
*C   LINES OF CODE ADDED FOR X-13A-S : 1
*        write (Nio,'(2X,A)') 'PREADJUSTED WITH regARIMA'
*C   END OF CODE BLOCK 
*       end if
*       if ((tramo.eq.1) .and. (interp.eq.1)) then
*        write (Nio,'(2x,"MISSING OBSERVATIONS IN ORIGINAL SERIES",
*     $           1x,"HAVE BEEN INTERPOLATED")')
*       end if
*       end if
*       buff1 = 'MULTIPLICATIVE'
*       if (lam .eq. 1) then
*        buff1 = 'ADDITIVE'
*       end if
*       if ((init.eq.2) .and. (tramo.eq.1)) then
*C   LINES OF CODE COMMENTED FOR X-13A-S : 1
*C        buff = 'MAX. LIKE. FROM TRAMO'
*C   END OF CODE BLOCK 
*C   LINES OF CODE ADDED FOR X-13A-S : 1
*        buff = 'MAX. LIKE. FROM regARIMA'
*C   END OF CODE BLOCK 
*       else if ((init.eq.2) .and. (tramo.eq.0)) then
*        buff = 'PARAMETERS FIXED'
*       else if ((init.eq.2) .and. (tramo.eq.-1)) then
*C   LINES OF CODE COMMENTED FOR X-13A-S : 1
*C        buff = 'FROM TRAMO'
*C   END OF CODE BLOCK 
*C   LINES OF CODE ADDED FOR X-13A-S : 1
*        buff = 'FROM regARIMA'
*C   END OF CODE BLOCK 
*       else if (type .eq. 0) then
*        buff = 'MAXIMUM LIKELIHOOD'
*       else if (type .eq. 1) then
*        buff = 'CONSTRAINED LEAST SQUARES'
*       end if
*       if (HTML .eq. 1) then
*        write (Nio,'("<br><b>",A,"</b>",A,2X,"<b>",A,2X,A,"</b>")')
*     $        'TYPE OF DECOMPOSITION : ', buff1(1:ISTRLEN(buff1)),
*     $        ',TYPE OF ESTIMATION : ', buff(1:ISTRLEN(buff))
*        write (buff,'(''<b>(</b>'',1x,i1,'','',2x,i1,'','',2x,
*     $                i1,'','',1x,''<b>)</b>'',4x,''<b>(</b>'',
*     $                1x,i1,'','',2x,i1,'','',2x,i1,1x,
*     $                ''<b>)</b>'',4x,i2)') p, d, q, bp, bd, bq, mq
*       else
*       write (Nio,'(A,2X,A,2X,A,2X,A)')
*     $       'TYPE OF DECOMPOSITION :', buff1(1:ISTRLEN(buff1)),
*     $       ',TYPE OF ESTIMATION :', buff(1:ISTRLEN(buff))
*       write (buff,
*     $'("(",1x,i1,",",2x,i1,",",2x,i1,",",1x,")",4x,"(",
*     $1x,i1,",",2x,i1,",",2x,i1,1x,")",4x,i2)') p, d, q, bp,
*     $                                          bd, bq, mq
*       end if
*       if ((p.eq.0) .and. (bp.eq.0) .and. (d.eq.1) .and. (bd.eq.1) .and.
*     $     (q.eq.1) .and. (bq.eq.1)) then
*        buff = 'DEFAULT'
*       end if
*       if (HTML .eq. 1) then
*        if (lam .eq. 0) then
*         write (Nio,'(''<br><b>ARIMA MODEL : </b>'',2X,
*     $                ''LOGS'',/,17X,A)') buff
*        else
*         write (Nio,'(''<br><b>ARIMA MODEL : </b>'',2X,
*     $                ''LEVELS'',/,17X,A)') buff
*        end if
*       else
*       if (lam .eq. 0) then
*        write (Nio,'(2X,"ARIMA MODEL :",2X,"LOGS",/,17X,A)') buff
*       else
*        write (Nio,'(2X,"ARIMA MODEL :",2X,"LEVELS",/,17X,A)') buff
*       end if
*       end if
*      else
      if (out.eq.0) then 
       if (ioneout .eq. 1) then
        write (Nio,'(///)')
       end if
       CALL mkPOneLine(Nio,'@',
     &                 Cbr//'<strong>SERIES TITLE : </strong>'//titleg)
       buff = 'NO'
       if (npread .eq. 1) then
        buff = 'YES'
       end if
       CALL mkPOneLine(Nio,'@',
     &                 '<strong>PREADJUSTED WITH regARIMA : </strong>'//
     &                 buff)
      end if
      end
C
C
      subroutine FITMODEL(bjstat1,bjstatsave,sigsave,qmax,ntry,sqf,th,d,
     $                    bd,p,bp,q,bq,imean,init,*)
C
C.. Implicits ..
      implicit none
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
      logical T
      parameter(T = .true.)
C
C.. Formal Arguments ..
      integer qmax,ntry,d,bd,p,bp,q,bq,imean,init
      real*8 bjstat1,bjstatsave(3),sigsave(3),sqf,th
C
C.. Local Scalars ..
      integer nmax
      real*8 sigmax,sigmin
C
C.. Intrinsic Functions ..
      intrinsic DBLE
      include 'fitmod.i'
      include 'stream.i'
C   LINES OF CODE ADDED FOR X-13A-S : 1
      include 'units.cmn'
C   END OF CODE BLOCK
C
C ... Executable Statements ...
C
      if (ntry .eq. 1) then
       if (bjstat1 .lt. DBLE(qmax)) then
C   LINES OF CODE COMMENTED FOR X-13A-S : 2
C        close (12)
c        Nio = 16
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 2
        close (42)
        Nio = Mt1
C   END OF CODE BLOCK
        ntry = 0
        return 1
       else if (-th .gt. -.1d0) then
        d = 2
        q = 2
        p = 0
        bd = 1
        bq = 1
        bp = 0
        imean = 0
        init = 0
        ntry = 2
        return 1
       else
        p = 3
        d = 1
        bd = 1
        q = 1
        bq = 1
        bp = 0
        imean = 1
        init = 0
        ntry = 3
        return 1
       end if
      else if (ntry .eq. 2) then
       if ((bjstat1.lt.DBLE(qmax)) .and. (sqf.lt.(1.1d0*Sisv1))) then
C   LINES OF CODE COMMENTED FOR X-13A-S : 2
C        close (12)
c        Nio = 16
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 2
        close (42)
        Nio = Mt1
C   END OF CODE BLOCK
        d = 2
        q = 2
        p = 0
        bd = 1
        bq = 1
        bp = 0
        imean = 0
        init = 0
        ntry = 0
        CALL wWritln('TO PROVIDE A BETTER FIT, SEATS HAS CHANGED '//
     &               'THE MODEL',Nio,0,T,T)
        return 1
       else
        p = 3
        d = 1
        bd = 1
        q = 1
        bq = 1
        bp = 0
        imean = 1
        init = 0
        ntry = 3
        return 1
       end if
      else
       if (ntry .ne. 3) return
       if ((bjstat1.lt.DBLE(qmax)) .and. (sqf.lt.(1.1d0*Sisv1))) then
C   LINES OF CODE COMMENTED FOR X-13A-S : 2
C        close (12)
c        Nio = 16
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 2
        close (42)
        Nio = Mt1
C   END OF CODE BLOCK
        CALL wWritln('TO FIT BETTER THE SERIES, SEATS HAS CHANGED '//
     &               'THE MODEL',Nio,0,T,T)
        ntry = 0
        p = 3
        d = 1
        bd = 1
        q = 1
        bq = 1
        bp = 0
        imean = 1
        init = 0
C           IF (P.EQ.3) TYPE=1
        return 1
       end if
       sigmax = sigsave(1)
       nmax = 1
       if (sigmax .lt. sigsave(2)) then
        sigmax = sigsave(2)
        nmax = 2
       end if
       if (sigmax .lt. sigsave(3)) then
        sigmax = sigsave(3)
        nmax = 3
       end if
       sigmin = sigsave(1)
       if ((sigmin.gt.sigsave(2)) .and. (sigsave(2).gt.ZERO)) then
        sigmin = sigsave(2)
       end if
       if ((sigmin.gt.sigsave(3)) .and. (sigsave(3).gt.ZERO)) then
        sigmin = sigsave(3)
       end if
       if ((bjstatsave(3).lt.bjstatsave(2)) .and.
     $     (bjstatsave(3).lt.bjstatsave(1)) .and.
     $     ((sigmax-sigmin).lt.(1.0d-1*sigmin))) then
        p = 3
        d = 1
        bd = 1
        q = 1
        bq = 1
        bp = 0
        imean = 1
        init = 0
        ntry = 0
C   LINES OF CODE COMMENTED FOR X-13A-S : 2
C        close (12)
c        Nio = 16
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 2
        close (42)
        Nio = Mt1
C   END OF CODE BLOCK
        CALL wWritln('TO FIT BETTER THE SERIES, SEATS HAS CHANGED '//
     &               'THE MODEL',Nio,0,T,T)
        return 1
       end if
       if ((bjstatsave(2).lt.bjstatsave(3)) .and.
     $     (bjstatsave(2).lt.bjstatsave(1)) .and.
     $     ((sigmax-sigmin).lt.(1.0d-1*sigmin))) then
        d = 2
        q = 2
        p = 0
        bq = 1
        bp = 0
        bd = 1
        imean = 0
        init = 0
        ntry = 0
C   LINES OF CODE COMMENTED FOR X-13A-S : 2
C        close (12)
c        Nio = 16
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 2
        close (42)
        Nio = Mt1
C   END OF CODE BLOCK
        CALL wWritln('TO FIT BETTER THE SERIES, SEATS HAS CHANGED '//
     &               'THE MODEL',Nio,0,T,T)
        return 1
       end if
       if (((bjstatsave(2).lt.bjstatsave(1)).and.(nmax.eq.3)) .or.
     $     ((bjstatsave(2).lt.bjstatsave(3)).and.(nmax.eq.1))) then
        d = 2
        q = 2
        p = 0
        bq = 1
        bp = 0
        bd = 1
        imean = 0
        init = 0
        ntry = 0
C   LINES OF CODE COMMENTED FOR X-13A-S : 2
C        close (12)
c        Nio = 16
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 2
        close (42)
        Nio = Mt1
C   END OF CODE BLOCK
        CALL wWritln('TO FIT BETTER THE SERIES, SEATS HAS CHANGED '//
     &               'THE MODEL',Nio,0,T,T)
        return 1
       else if (((bjstatsave(3).lt.bjstatsave(2)).and.(nmax.eq.1)) .or.
     $         ((bjstatsave(3).lt.bjstatsave(1)).and.(nmax.eq.2))) then
        p = 3
        d = 1
        bd = 1
        q = 1
        bq = 1
        bp = 0
        imean = 1
        init = 0
        ntry = 0
C   LINES OF CODE COMMENTED FOR X-13A-S : 2
C        close (12)
c        Nio = 16
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 2
        close (42)
        Nio = Mt1
C   END OF CODE BLOCK
        CALL wWritln('TO FIT BETTER THE SERIES, SEATS HAS CHANGED '//
     &               'THE MODEL',Nio,0,T,T)
        return 1
       else
C   LINES OF CODE COMMENTED FOR X-13A-S : 2
C        close (12)
c        Nio = 16
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 2
        close (42)
        Nio = Mt1
C   END OF CODE BLOCK
        ntry = 100
        return 1
       end if
      end if
      end
C
C
C
*      subroutine SMOOTHING(p,d,q,bp,bd,bq,mq,smtr,thlim,bthlim,
*     $                     ths,th,bth,bths,thstar)
*C
*C.. Implicits ..
*      implicit none
*C
*C.. Formal Arguments ..
*C.. In/Out Status: Read, Maybe Written ..
*      integer p,d,q,bp,bd,bq,mq,smtr
*C.. In/Out Status: Maybe Read, Maybe Written ..
*      real*8 thlim,bthlim
*C.. In/Out Status: Maybe Read, Maybe Written ..
*      real*8 ths(*),thstar(*)
*C.. In/Out Status: Not Read, Maybe Written ..
*      real*8 th(*),bth(*),bths(*)
*C
*C.. Local Scalars ..
*      integer nthstar,i
*      include 'stream.i'
*C
*C ... Executable Statements ...
*C
*      smtr = 0
*      if ((p.ne.0).or.(d.ne.1).or.(q.ne.1) .or.
*     $    (bp.ne.0).or.(bd.ne.1).or.(bq.ne.1)) then
*       if (HTML .eq. 1) then
*        call SNote(Nio)
*        write (Nio,'(''<br>SINCE MODEL IS NOT THE DEFAULT ONE,'',
*     $       '' THE SMOOTHER CANNOT BE COMPUTED;'')')
*        call ENote(Nio)
*       else
*        write (Nio,'(//8x,''SINCE MODEL IS NOT THE DEFAULT ONE,'',
*     $             /,8x,'' THE SMOOTHER CANNOT BE COMPUTED;'')')
*       end if
*       return
*      end if
*      if (thlim .lt. 0.0d0) then
*       if (ths(2) .le. thlim) then
*       if (HTML .eq. 1) then
*        call SNote(Nio)
*         write (Nio,'(''<br>SINCE TH(1) <= THLIM, NO FURTHER'',
*     $   '' SMOOTHING IS NEEDED'')')
*        call ENote(Nio)
*       else
*         write (Nio,'(2x,''SINCE TH(1) <= THLIM, NO FURTHER'',/,2x,
*     $  "SMOOTHING IS NEEDED")')
*       end if
*       else
*         ths(2) = thlim
*         th(1) = -thlim
*         smtr = 1
*         if (HTML .eq. 1) then
*          write (Nio,'(''<p><strong>'',
*     $              ''THE TREND-CYCLE HAS BEEN MODIFIED TO'',
*     $              '' BECOME SMOOTHER;<br>'',
*     $              ''THE IRREGULAR COMPONENT'',
*     $              '' IS LIKELY TO PRESENT (LOW ORDER)<br>'',
*     $              ''AUTOCORRELATION, AND A LARGER VARIANCE.'',
*     $              ''</strong><p>'')')
*         else
*          write (Nio,'(///,2x,''THE TREND-CYCLE HAS BEEN MODIFIED TO'',
*     $           '' BECOME SMOOTHER;'',/,2x,''THE IRREGULAR COMPONENT'',
*     $           '' IS LIKELY TO PRESENT (LOW ORDER)'',/,2x,
*     $           ''AUTOCORRELATION, AND A LARGER VARIANCE'',//)')
*          end if
*      end if
*      end if
*      if (bthlim .lt. 0.0d0) then
*       if (-bth(1) .le. bthlim) then
*        if (HTML .eq. 1) then
*         call SNote(Nio)
*         write (Nio,'(''<br>SINCE BTH(1) <= BTHLIM, NO FURTHER'',
*     $    '' SMOOTHING IS NEEDED'')')
*         call ENote(Nio)
*        else
*         write (Nio,'(2x,''SINCE BTH(1) <= BTHLIM, NO FURTHER'',/,2x,
*     $    ''SMOOTHING IS NEEDED'')')
*        end if
*       else
*         bth(1) = -bthlim
*         smtr = 1
*      if (HTML .eq. 1) then
*          write (Nio,'(''<p><strong>'',
*     $              ''THE SEASONAL HAS BEEN MODIFIED TO'',
*     $              '' BECOME SMOOTHER;<br>'',
*     $              ''THE IRREGULAR COMPONENT'',
*     $              '' IS LIKELY TO PRESENT (LOW ORDER)<br>'',
*     $              ''AUTOCORRELATION, AND A LARGER VARIANCE.'',
*     $              ''</strong><p>'')')
*      else
*          write (Nio,'(///,2x,''THE SEASONAL HAS BEEN MODIFIED TO'',
*     $        " BECOME SMOOTHER;",/,2x,"THE IRREGULAR COMPONENT",
*     $        " IS LIKELY TO PRESENT (LOW ORDER)",/,2x,
*     $        "AUTOCORRELATION, AND A LARGER VARIANCE",//)')
*      end if
*       end if
*      end if
*      do i=1,mq+1
*       bths(i)=0.0d0
*      enddo 
*      bths(1)=1.0d0
*      bths(mq+1)=-bth(1)
*      call CONV(ths,q+1,bths,MQ+1,thstar,nthstar)
*      do i=2,nthstar
*       thstar(i-1)=-thstar(i)
*      enddo 
*      return
*      end
C
C
*      subroutine OPENFILE(iter,title,titleg,tout,ioneout,out,opened,
*     $                    outdir,outfile,noserie,itable,niter,numser)
*C
*C.. Implicits ..
*      implicit none
*      integer maxLen
*      parameter(maxLen=80)
*C
*      INCLUDE 'stdio.i'
*C.. Formal Arguments ..
*      integer iter,ioneout,out,noserie,itable,niter,ilen,numser
*      character title*80,tout*80,outdir*180,outfile*180
*      character titleg*80
*      character pkindex*80,Tidx*80,Tmain*80,
*     $          Tfname*80,PFix*4
*      character TMainName*180,TidxName*180
*      logical opened
*C
*C.. Local Scalars ..
*      integer i,ifail,ii,jj,noutdir,noutfile
*      character fname*180,seqname*19,FilenameC*180
*C
*C.. External Functions ..
*      integer ISTRLEN
*      external ISTRLEN
*C
*C.. External Calls ..
*      external OPENDEVICE, OPENDEVSCRATCH
*      include 'stream.i'
*      include 'build.i'
*C
*C ... Executable Statements ...
*C
*      PFix='.out'
*      TmainName = ''
*      if (HTML .eq. 1) then
*       PFix='.htm'
*      end if
*      noutfile = ISTRLEN(outfile)
*      noutdir = ISTRLEN(outdir)
*      if (iter .eq. 1) then
*       write (seqname,'(I12)') niter
*       i = 1
*       do while (seqname(i:i).ne.'1' .and. seqname(i:i).ne.'2' .and.
*     $           seqname(i:i).ne.'3' .and. seqname(i:i).ne.'4' .and.
*     $           seqname(i:i).ne.'5' .and. seqname(i:i).ne.'6' .and.
*     $           seqname(i:i).ne.'7' .and. seqname(i:i).ne.'8' .and.
*     $           seqname(i:i).ne.'9')
*        i = i + 1
*       end do
*       ii = i
*       i = ii
*       do while (seqname(i:i) .ne. ' ')
*        i = i + 1
*       end do
*       jj = i - 1
*       title = 'MODEL' // seqname(ii:jj)
*       titleg = title
*C   LINES OF CODE ADDED FOR X-13A-S : 5
*       if (noutdir.gt.0) THEN
*        tout = outdir(1:noutdir) // 's_model' // seqname(ii:jj) // PFix
*       else
*        tout = 's_model' // seqname(ii:jj) // PFix
*       end if 
*       Tfname = '\s_model' // seqname(ii:jj) // PFix
*C   END OF CODE BLOCK         
*C   LINES OF CODE COMMENTED FOR X-13A-S : 1         
*C       tout = outdir(1:noutdir) // '\MODEL' // seqname(ii:jj) // PFix
*C   END OF CODE BLOCK         
*       if (HTML .eq. 1) then
*        TidxName = outdir(1:noutdir) // '\s_idx' // seqname(ii:jj) 
*     $             // PFix
*        TmainName = outdir(1:noutdir) // '\s_main' // seqname(ii:jj)
*     $              // PFix
*       end if
*       if (ioneout .eq. 0) then
*        if (out .ne. 0) then
*C   LINES OF CODE COMMENTED FOR X-13A-S : 1         
*C         call OPENDEVSCRATCH(16)
*C   END OF CODE BLOCK         
*C   LINES OF CODE ADDED FOR X-13A-S : 1
*         call OPENDEVSCRATCH(46)
*C   END OF CODE BLOCK
*        else
*         fname = tout
*C   LINES OF CODE COMMENTED FOR X-13A-S : 1         
*C         call OPENDEVICE(fname,16,0,ifail)
*C   END OF CODE BLOCK         
*C   LINES OF CODE ADDED FOR X-13A-S : 1
*         call OPENDEVICE(fname,46,0,ifail)
*C   END OF CODE BLOCK
*        end if
*       else if (.not. opened) then
*        if (out .ne. 0) then
*C   LINES OF CODE COMMENTED FOR X-13A-S : 1         
*C         call OPENDEVSCRATCH(16)
*C   END OF CODE BLOCK         
*C   LINES OF CODE ADDED FOR X-13A-S : 1
*         call OPENDEVSCRATCH(46)
*C   END OF CODE BLOCK
*        else
*         if (noutfile .gt. 1) then
*C   LINES OF CODE ADDED FOR X-13A-S : 5
*          if (noutdir.gt.0) then
*           fname = outdir(1:noutdir) // outfile(1:noutfile) // PFix
*          else
*           fname = outfile(1:noutfile) // PFix
*          end if
*C   END OF CODE BLOCK
*          Tfname = outfile(1:noutfile) // PFix
*          if (HTML .eq. 1) then
*           TmainName = outdir(1:noutdir) // 'main' //  PFix
*           TidxName = outdir(1:noutdir) // 'idx' //  PFix
*          end if
*C   LINES OF CODE COMMENTED FOR X-13A-S : 6
*ccdos
*c          fname = outdir(1:noutdir) // '\\' // outfile(1:noutfile) //
*c     $            '.OUT'
*ccunix
*cc          fname = outdir(1:noutdir) // '/' // outfile(1:noutfile) //
*cc     $            '.OUT'         
*C   END OF CODE BLOCK
*         else
*          fname = tout
*         end if
*C   LINES OF CODE COMMENTED FOR X-13A-S : 1         
*C         call OPENDEVICE(fname,16,0,ifail)
*C   END OF CODE BLOCK         
*C   LINES OF CODE ADDED FOR X-13A-S : 1
*         call OPENDEVICE(fname,46,0,ifail)
*C   END OF CODE BLOCK
*        end if 
*       end if
*      else
*       tout = title
*       if ((iter.eq.2) .or. (iter.eq.3)) then
*        jj = 1
*        do while (tout(jj:jj) .eq. ' ' .and. jj.le.maxLen)
*         jj = jj + 1
*        end do
*        ii = jj
*        do while (tout(ii:ii) .ne. ' ' .and. ii.le.maxLen)
*         ii = ii + 1
*        end do
*        ii = ii - 1
*        if (ii-jj+1 .gt. 8) then
*         ii = jj + 7
*        end if
*        do i = jj,ii
*         if ((tout(i:i).eq.'"') .or. (tout(i:i).eq.'\\') .or.
*     $       (tout(i:i).eq.'/') .or. (tout(i:i).eq.'[') .or.
*     $       (tout(i:i).eq.']') .or. (tout(i:i).eq.'<') .or.
*     $       (tout(i:i).eq.'>') .or. (tout(i:i).eq.'+') .or.
*     $       (tout(i:i).eq.';') .or. (tout(i:i).eq.',') .or.
*     $       (tout(i:i).eq.'*') .or. (tout(i:i).eq.'?') .or.
*     $       (tout(i:i).eq.':') .or. (tout(i:i).eq.'=')) then
*          tout(i:i) = '-'
*         end if
*        end do
*        write(pkindex,'(i4)')niter
*        call LEFTTRIM(pkindex)
*        pkindex = 'S'//pkindex(1:ISTRLEN(pkindex))
*        ilen = istrlen(pkindex)
*        ilen = 8 - (ii-jj+1)-ilen-1
*        if (ilen .ge.0) then
*         ilen = -1
*        else
*          ilen = -ilen
*        end if
*        title = pkindex(1:istrlen(pkindex))//'_'//tout(jj:ii-ilen)
*        tout = pkindex(1:istrlen(pkindex))//'_'//tout(jj:ii-ilen)
*        tout = tout(1:ISTRLEN(tout)) // PFix         
*        call STRTOLOW(title)
*        call STRTOLOW(tout)
*        Tfname = tout
*        Tmain = pkindex(1:istrlen(pkindex))//'_main' // PFix
*        Tidx = pkindex(1:istrlen(pkindex))//'_idx' // PFix
*C   LINES OF CODE COMMENTED FOR X-13A-S : 4
*ccdos
*c        tout = outdir(1:noutdir) // '\\' // tout(jj:ii) // PFix
*ccunix
*cc        tout = outdir(1:noutdir) // '/' // tout(jj:ii) // PFix
*C   END OF CODE BLOCK         
*        if (ioneout .eq. 0) then
*         if (out .ne. 0) then
*C   LINES OF CODE COMMENTED FOR X-13A-S : 1         
*C          call OPENDEVSCRATCH(16)
*C   END OF CODE BLOCK         
*C   LINES OF CODE ADDED FOR X-13A-S : 1
*          call OPENDEVSCRATCH(46)
*C   END OF CODE BLOCK
*         else
*cdos  backslash for directory
*          fname = outdir(1:noutdir) // '\\' // tout(1:istrlen(tout))
*cunix forward slash for directory
*cunix          fname = outdir(1:noutdir) // '/' // tout(1:istrlen(tout))
*C   LINES OF CODE COMMENTED FOR X-13A-S : 1         
*C          call OPENDEVICE(fname,16,0,ifail)
*C   END OF CODE BLOCK         
*C   LINES OF CODE ADDED FOR X-13A-S : 1
*          call OPENDEVICE(fname,46,0,ifail)
*          if (HTML .eq. 1) then
*           TmainName = outdir(1:noutdir) // '\' // 
*     $                 Tmain(1:istrlen(Tmain))
*           TidxName = outdir(1:noutdir) // '\' // Tidx(1:istrlen(Tidx))
*          end if
*         end if
*        else if (.not. opened) then
*         if (out .ne. 0) then
*C   LINES OF CODE COMMENTED FOR X-13A-S : 1         
*C          call OPENDEVSCRATCH(16)
*C   END OF CODE BLOCK         
*C   LINES OF CODE ADDED FOR X-13A-S : 1
*          call OPENDEVSCRATCH(46)
*C   END OF CODE BLOCK
*         else
*          call STRTOLOW(outfile)
*          if (noutfile .gt. 1) then
*C   LINES OF CODE ADDED FOR X-13A-S : 5
*           if (noutdir .gt. 0) then
*            fname = outdir(1:noutdir) // outfile(1:noutfile) // PFix
*           else
*            fname = outfile(1:noutfile) // PFix
*           end if
*           Tfname = outfile(1:noutfile) // PFix
*C   END OF CODE BLOCK
*C   LINES OF CODE COMMENTED FOR X-13A-S : 6
*ccdos
*c           fname = outdir(1:noutdir) // '\\' // outfile(1:noutfile) //
*c     $              PFix
*cccunix
*cc           fname = outdir(1:noutdir) // '/' // outfile(1:noutfile) //
*cc     $             PFix         
*C   END OF CODE BLOCK
*          else
*           if (noutdir .gt. 0) then
*            fname = outdir(1:noutdir) // tout
*           else
*            fname = tout
*           end if
*          end if
*          if (HTML .eq. 1) then
*           TmainName = outdir(1:noutdir) // 'main' //  PFix
*           TidxName = outdir(1:noutdir) // 'idx' //  PFix
*          end if
*C   LINES OF CODE COMMENTED FOR X-13A-S : 1         
*C          call OPENDEVICE(fname,16,0,ifail)
*C   END OF CODE BLOCK         
*C   LINES OF CODE ADDED FOR X-13A-S : 1
*          call OPENDEVICE(fname,46,0,ifail)
*C   END OF CODE BLOCK
*         end if      
*        end if
*       else
*        if (noserie .eq. 1) then
*         title = 'noserie'
*        end if
*        tout = title
*        jj = 1
*        do while (tout(jj:jj) .eq. ' ')
*         jj = jj + 1
*        end do
*        ii = jj
*        do while (tout(ii:ii) .ne. ' ')
*         ii = ii + 1
*        end do
*        ii = ii - 1
*        if (ii-jj+1 .gt. 8) then
*         ii = jj + 7
*        end if
*        write(pkindex,'(i4)')niter
*        call LEFTTRIM(pkindex)
*        pkindex = 'S'//pkindex(1:ISTRLEN(pkindex))
*        ilen = istrlen(pkindex)
*        ilen = 8 - (ii-jj+1)-ilen-1
*        if (ilen .ge.0) then
*         ilen = -1
*        else
*         ilen = -ilen
*        end if
*        title = pkindex(1:istrlen(pkindex))//'_'//tout(jj:ii-ilen)
*        tout = pkindex(1:istrlen(pkindex))//'_'//tout(jj:ii-ilen)
*        tout = tout(1:ISTRLEN(tout)) // PFix
*        call STRTOLOW(title)
*        call STRTOLOW(tout)
*        Tfname = tout
*        Tmain = pkindex(1:istrlen(pkindex))//'_main' // PFix
*        Tidx = pkindex(1:istrlen(pkindex))//'_idx' // PFix
*        do i = 1,istrlen(tout)
*         if ((tout(i:i).eq.'"') .or. (tout(i:i).eq.'\\') .or.
*     $       (tout(i:i).eq.'/') .or. (tout(i:i).eq.'[') .or.
*     $       (tout(i:i).eq.']') .or. (tout(i:i).eq.'<') .or.
*     $       (tout(i:i).eq.'>') .or. (tout(i:i).eq.'+') .or.
*     $       (tout(i:i).eq.';') .or. (tout(i:i).eq.',') .or.
*     $       (tout(i:i).eq.'*') .or. (tout(i:i).eq.'?') .or.
*     $       (tout(i:i).eq.':') .or. (tout(i:i).eq.'=')) then
*          tout(i:i) = '-'
*         end if      
*        end do
*        if (ioneout .eq. 0) then
*         if ((out .ne. 0)) then
*C   LINES OF CODE COMMENTED FOR X-13A-S : 1         
*C          call OPENDEVSCRATCH(16)
*C   END OF CODE BLOCK         
*C   LINES OF CODE ADDED FOR X-13A-S : 1
*          call OPENDEVSCRATCH(46)
*C   END OF CODE BLOCK
*         else
*          call STRTOLOW(tout)
*C   LINES OF CODE ADDED FOR X-13A-S : 5
*          if (noutdir .gt. 0) then
*cdos  backslash for directory
*           fname = outdir(1:noutdir) // '\\' // tout(1:istrlen(tout))
*cunix forward slash for directory
*cunix           fname = outdir(1:noutdir) // '/' // tout(1:istrlen(tout))
*          else
*           fname = tout(1:istrlen(tout))
*          end if
*          Tfname = tout(1:istrlen(tout))
*          if (HTML .eq. 1) then
*           TmainName = outdir(1:noutdir) // '\\' // 
*     $                 Tmain(1:istrlen(Tmain))
*           TidxName = outdir(1:noutdir) // '\\' // Tidx(1:istrlen(Tidx))
*          end if
*C   END OF CODE BLOCK
*C   LINES OF CODE COMMENTED FOR X-13A-S : 4         
*ccdos
*c          fname = outdir(1:noutdir) // '\\' // tout(jj:ii+4)
*ccunix
*cc          fname = outdir(1:noutdir) // '/' // tout(jj:ii+4)
*C   END OF CODE BLOCK
*C   LINES OF CODE COMMENTED FOR X-13A-S : 1         
*C          call OPENDEVICE(fname,16,0,ifail)
*C   END OF CODE BLOCK         
*C   LINES OF CODE ADDED FOR X-13A-S : 1
*          call OPENDEVICE(fname,46,0,ifail)
*C   END OF CODE BLOCK
*         end if
*        else if (.not. opened) then
*         if (out .eq. 3) then
*C   LINES OF CODE COMMENTED FOR X-13A-S : 1         
*C          call OPENDEVSCRATCH(16)
*C   END OF CODE BLOCK         
*C   LINES OF CODE ADDED FOR X-13A-S : 1
*          call OPENDEVSCRATCH(46)
*C   END OF CODE BLOCK
*         else
*          call STRTOLOW(outfile)
*          if (noutfile .gt. 1) then
*C   LINES OF CODE ADDED FOR X-13A-S : 5
*           if (noutdir .gt. 0) then
*            fname = outdir(1:noutdir) // outfile(1:noutfile) // PFix
*           else
*            fname = outfile(1:noutfile) // PFix
*           end if
*C   END OF CODE BLOCK         
*C   LINES OF CODE COMMENTED FOR X-13A-S : 6
*ccdos
*c           fname = outdir(1:noutdir) // '\\' // outfile(1:noutfile) //
*c     $             '.OUT'
*ccunix
*cc           fname = outdir(1:noutdir) // '/' // outfile(1:noutfile) //
*cc     $             '.OUT'
*C   END OF CODE BLOCK         
*          else
*           fname = tout
*           Tfname = tout(1:istrlen(tout))
*          end if
*          if (HTML .eq. 1) then
*           TmainName = outdir(1:noutdir) // '\' // 
*     $                 Tmain(1:istrlen(Tmain))
*           TidxName = outdir(1:noutdir) // '\' // Tidx(1:istrlen(Tidx))
*          end if
*C   LINES OF CODE COMMENTED FOR X-13A-S : 1         
*C          call OPENDEVICE(fname,16,0,ifail)
*C   END OF CODE BLOCK         
*C   LINES OF CODE ADDED FOR X-13A-S : 1
*          call OPENDEVICE(fname,46,0,ifail)
*C   END OF CODE BLOCK
*         end if
*        end if
*       end if
*      end if
*      if (itable .eq. 1) then
*       call OpenFileTables(ifail,iter,niter,title,numser)
*       if ((iter .eq.0).or.(niter.le.1)) then
*        call OpenFilePsie(ifail)
*       end if        
*      end if
*      if (HTML .eq. 1) then
*       if (out .ne. 0) then       
*        call OPENDEVSCRATCH(71)
*        call OPENDEVSCRATCH(70)
*       else
*       call OPENDEVICE(TmainName,70,0,ifail)
*       call FrameHead(70,Tidx,Tfname)
*       call OPENDEVICE(TidxName,71,0,ifail)
*       call IdxHead(71,Tfname)
*       call HeadHtmlMeta(46,1,title(1:istrlen(title)),0)
*       end if
*      end if
*      end
C
C
C   THIS SUBPROGRAM EVALUATES THE HARMONIC FUNCTION IN THE COMMON
C   "FUNC5F1"
C
C     INPUT ARGUMENT
C
C      X : THE VALUE AT WHICH THE FUNCTION MUST BE EVALUATED
C
      double precision function FUNC1(x)
C
C
C
C.. Implicits ..
      implicit none
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 x
C
C.. Local Scalars ..
      integer i,l
      real*8 denom,numer,w
C
C.. Local Arrays ..
      real*8 c(250)
C
C.. Intrinsic Functions ..
      intrinsic ABS, COS, MAX, SIGN
      include 'func5f1.i'
C
C ... Executable Statements ...
C
C
      w = ZERO
      numer = ZERO
      denom = ZERO
      l = MAX(Ndumf1,Nd1f1)
      do i = 1,l
       c(i) = COS(w)
       w = w + x
      end do
      do i = 1,Ndumf1
       numer = numer + Dumf1(i)*c(i)
      end do
      do i = 1,Nd1f1
       denom = denom + Dum1f1(i)*c(i)
      end do
      if (ABS(denom) .lt. 1.0d-13) then
       denom = SIGN(1.0d-13,denom)
      end if
      FUNC1 = numer / denom
      end
C
C THIS SUBPROGRAM COMPUTES THE STANDARD ERROR OF A COMPONENT
C
C    INPUT PARAMETERS
C    SE : STANDARD ERROR OF THE COMPONENT
C    NZ : DIMENSION OF COMP AND SE
C  PSIE : PSI-WEIGHTS OF THE COMPOPNENT (B,F)
C NFILT : DIMENSION OF PSIE
C   FEE : THEORETICAL VARIANCE OF THE MODEL FOR THE COMPONENT
C   SQF : STANDARD ERROR OF RESIDUALS
C  COMP : COMPONENT
C  LAMD : 0 LOGS, 1 NO LOGS
C
C
      subroutine SERROR(se,nz,psie,nfilt,fee,sqf,comp,lamd)
C
C.. Implicits ..
      implicit none
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
C.. Parameters ..
*      INCLUDE 'srslen.prm'
*      integer nfl,mp,kp
*      parameter (kp = PFCST, mp = POBS, nfl = mp*2) 
C
C.. Formal Arguments ..
      integer nz,nfilt,lamd
      real*8 se(*),psie(*),fee,sqf,comp(*)
C
C.. Local Scalars ..
      integer i,k,mq2
      real*8 sminus,splus
C
C.. Intrinsic Functions ..
      intrinsic EXP, MOD, SQRT
C
C ... Executable Statements ...
C
C
C
      mq2 = nz / 2
      if (MOD(nz,2) .eq. 1) then
       mq2 = mq2 + 1
      end if
      se(mq2) = ZERO
      do i = 1,nfilt-mq2
       se(mq2) = se(mq2) + psie(i)*psie(i)
      end do
      i = mq2
C      if (MOD(nz,2) .eq. 1 ) then
C       do k = mq2,nz
C        i = i - 1
C        se(k) = se(k-1) + psie(nfilt-i)*psie(nfilt-i)
C       end do
C      else
       do k = mq2+1,nz
        i = i - 1
        se(k) = se(k-1) + psie(nfilt-i)*psie(nfilt-i)
       end do
C      end if
      do k = mq2,nz
       se(k) = SQRT(fee+se(k)) * sqf
      end do
      do k = 1,mq2-1
       se(k) = se(nz-k+1)
      end do
      if (lamd .eq. 0) then
       do i = 1,nz
        splus = comp(i) + 1.96d0*se(i)
        sminus = comp(i) - 1.96d0*se(i)
        se(i) = (EXP(splus)-EXP(sminus)) / (2.0d0 * 1.96d0)
       end do
      end if
      end
C
C
C THIS SUBPROGRAM COMPUTES THE STANDARD ERROR OF A COMPONENT
C
C    INPUT PARAMETERS
C    SE : STANDARD ERROR OF THE COMPONENT
C    NZ : DIMENSION OF COMP AND SE
c   lFor: number of forecast
C  PSIE : PSI-WEIGHTS OF THE COMPOPNENT (B,F)
C NFILT : DIMENSION OF PSIE
C   FEE : THEORETICAL VARIANCE OF THE MODEL FOR THE COMPONENT
C   SQF : STANDARD ERROR OF RESIDUALS
C  COMP : COMPONENT
C  LAMD : 0 LOGS, 1 NO LOGS
C
C
      subroutine SERRORF(se,nz,lfor,psie,nfilt,fee,sqf,comp,lamd)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      integer nfl,mp,kp
      parameter (kp = PFCST, mp = POBS, nfl = mp*2)
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
C.. Formal Arguments ..
      integer nz,lfor,nfilt,lamd
      real*8 se(*),psie(*),fee,sqf,comp(*)
C
C.. Local Scalars ..
      integer i,k,mq2
      real*8 sminus,splus
C
C.. Intrinsic Functions ..
      intrinsic EXP, MOD, SQRT
C
C ... Executable Statements ...
C
C
C
      mq2 = (nz/2) + 1
      se(mq2) = ZERO
      do i = 1,nfilt-mq2
       se(mq2) = se(mq2) + psie(i)*psie(i)
      end do
      i = mq2
C      if (MOD(nz,2) .eq. 1 ) then
C       do k = mq2,nz
C        i = i - 1
C        se(k) = se(k-1) + psie(nfilt-i)*psie(nfilt-i)
C       end do
C      else
       do k = mq2+1,nz+lfor
        i = i - 1
        se(k) = se(k-1) + psie(nfilt-i)*psie(nfilt-i)
       end do
C      end if
      do k = mq2,nz+lfor
       se(k) = SQRT(fee+se(k)) * sqf
      end do
      do k = 1,mq2-1
       se(k) = se(nz-k+1)
      end do
      if (lamd .eq. 0) then
       do i = 1,nz+lfor
        splus = comp(i) + 1.96*se(i)
        sminus = comp(i) - 1.96*se(i)
        se(i) = (EXP(splus)-EXP(sminus)) / (2.0d0*1.96d0)
       end do
      end if
      end
C
C
C THIS SUBROUTINE COMPUTES THE PSEUDO-INNOVATIONS OF THE COMPONENTS
C
C        INPUT PARAMETERS
C     THI : NUMERATOR OF THE MODEL FOR THE COMPONENT
C    NTHI : DIMENSION OF THI
C   PHINI : DENOMINATOR OF THE MODEL FOR THE COMPONENT
C  NPHINI : DIMENSION OF PHINI
C  THSTAR : NUMERATOR OF THE MODEL
C   QSTAR : DIMENSION OF THSTAR
C       A : RESIDUALS
C      NA : DIMENSION OF A
C    NDEC : NUMBER OF DIGITS IN THE TABLES
C  VARPSE : INNOVATIONS VARIANCE OF THE COMPONENT
C      PG : 0 FILES FOR GRAPH, 1 NO FILES
C    COMP : NAME OF THE COMPONENT
C   TITLE : TITLE OF THE SERIES
C
      subroutine PINNOV(thi,nthi,phini,nphini,thstar,qstar,a,na,ndec,
     $                  varpse,pg,comp)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
C.. Formal Arguments ..
      integer nthi,nphini,qstar,na,pg,ndec
      character comp*32
      real*8 thi(*),phini(*),thstar(*),a(*),varpse
C
C.. Local Scalars ..
      integer i,iavant,indiet,j,nfnum
      character fname*30,subtitle*50
      real*8 sum
C
C.. Local Arrays ..
      real*8 fnum(55),pse(mpkp)
      character tabtit*120
      integer thisDate(2)
C
C.. External Functions ..
      integer ISTRLEN
      external ISTRLEN
C
C.. External Calls ..
      external CONV, TABLE
      include 'sform.i'
      include 'stream.i'
      include 'transcad.i'
C
C ... Executable Statements ...
C
C
C
C VARPSE=VARIANCE OF PSEUDO-INN. (IN UNITS OF VARIANCE OF A)
C
C INIT.COND. PSE(NZ+1),..,PSE(NZ+QSTAR),..,A(NZ+1),.,.A(NZ+FNUM)=0
C
C THE VECTORS A(NA) AND NA ARE NOT CHANGED
C
      call CONV(thi,nthi,phini,nphini,fnum,nfnum)
      do i = 1,nfnum
       fnum(i) = fnum(i) * varpse
      end do
C
C NUMERATOR FILTERING
C
      iavant = Nz - na
      if (iavant .lt. 0) then
       iavant = 0
      end if
      indiet = 0
      if (Nz-na .le. 0) then
       indiet = na - Nz
      end if
      do i = 1,Nz
       sum = ZERO
       do j = 1,nfnum
        if (i+j-1.gt.iavant .and. i+j-1.le.Nz) then
         sum = sum + fnum(j)*a(i+j-1+indiet-iavant)
        end if
       end do
       pse(i) = sum
      end do
C
C DENOMINATOR FILTERING
C
      do i = 1,Nz
       sum = pse(Nz+1-i)
       do j = 2,qstar
        if (Nz-i+j .le. Nz) then
         sum = sum - thstar(j)*pse(Nz-i+j)
        end if
       end do
       pse(Nz+1-i) = sum
      end do
      thisDate(1) = Nyer
      thisDate(2) = Nper
      CALL genSkip(1163)
      CALL prttbl(thisDate,Nfreq,pse,nz,'Innovations',Ndec,
     &            'innovations.seats')
C      PSEMEAN=DMEAN(NZ,PSE)
C      PSEVAR=DVAR(NZ,PSE)
C      RSTD=0.0D0
C      RSTD=PSEVAR**0.5D0
C      SKEWNE=0.0D0
C      RKURT=0.0D0
C        DO 25 I=1,NZ
C        SKEWNE=SKEWNE+((PSE(I)-PSEMEAN)**3)/(PSEVAR**1.50D0*NZ)
C 25     RKURT=RKURT+((PSE(I)-PSEMEAN)**4)/(PSEVAR**2.0D0*NZ)
C        DO 26 I=1,NZ
C        PSE(I)=PSE(I)/RSTD
C        IF (MOD(I,NFREQ).EQ.0) THEN
C        IYEAR=I/NFREQ
C        IYEAR=NYER+IYEAR-1
C        IPER=NFREQ
C        ELSE
C        IYEAR=I/NFREQ
C        IPER=I-IYEAR*NFREQ
C        IYEAR=NYER+IYEAR
C        end if
C        IPER=IPER+NPER-1
C        IF (IPER.GT.NFREQ) THEN
C        IYEAR=IYEAR+1
C        IPER=IPER-NFREQ
C        end if
C 26   CONTINUE
C      NN=0
C      IMEAN=1
C      WRITE(NIO,'(/,4X,"ACF OF ESTIMATED INNOVATIONS IN ",A,/)')COMP
C      CALL AUTO(NZ,PSE,M,R,IQ,NZ,NN,IMEAN,NFREQ,1,1)
C        DO 126 I=1,NZ
C  126   PSE(I)=PSE(I)**2
C      WRITE(NIO,'(/,4X,"ACF OF SQUARED ESTIMATED INNOVATIONS IN ",
C     & A,/)')COMP
C      CALL AUTO(NZ,PSE,M,R,IQ,NZ,NN,IMEAN,NFREQ,1,1)
C 1015 FORMAT(/,1H ,'OUTLIER OF  ',F8.4,'  AT T=',I3,
C     ,       4X,'(',I2,1X,I4,')')
C      RSTD=RSTD/DSQRT(DBLE(NZ))
C      WRITE(NIO,30)PSEMEAN,RSTD,PSEVAR,SKEWNE,RKURT
C  30  FORMAT(//,1H ,'           MEAN=',D12.4,/
C     $           '    STAND. DEV.=',D12.4,/
C     $           '      VARIANCE =',D12.4,/
C     $           '       SKEWNESS=',F8.4,/
C     $           '       KURTOSIS=',F8.4,/)
C
*      if (pg .eq. 0) then
*       if (comp .eq. 'TREND-CYCLE') then
*        fname = 'PITREND.T'
*       end if
*       if (comp .eq. 'SEASONAL') then
*        fname = 'PISEAS.T'
*       end if
*       if (comp .eq. transLcad(1:nTransLcad)) then
*        fname = 'PITRANS.T'
*       end if
*       if (comp .eq. 'SEASONALLY ADJUSTED SERIES') then
*        fname = 'PISA.T'
*       end if
*       subtitle = 'PSEUDO INNOVATIONS IN ' // comp(1:ISTRLEN(comp))
*       call PLOTSERIES(fname,subtitle,pse,Nz,0,999.0d0)
*      end if
C   LINES OF CODE ADDED FOR X-13A-S : 9
      if (comp .eq. 'TREND-CYCLE') then
       CALL usrentry(pse,1,Nz,1,mpkp,2021)
      end if
      if (comp .eq. 'SEASONAL') then
       CALL usrentry(pse,1,Nz,1,mpkp,2022)
      end if
      if (comp .eq. 'TRANSITORY') then
       CALL usrentry(pse,1,Nz,1,mpkp,2023)
      end if
      if (comp .eq. 'SEASONALLY ADJUSTED SERIES') then
       CALL usrentry(pse,1,Nz,1,mpkp,2024)
      end if
C   END OF CODE BLOCK          
      end
CC
CC
CC
C
      subroutine APPROXIMATE(p,q,d,bd,bp,bq,rez,imz,init,noadmiss,
     $ imean,type,th,bth,phi,bphi,mq,status,out,fixparam,remMeanMCS,*,*)
C
C.. Implicits ..
      implicit none
      integer n10,n1
      parameter(n10=10,n1=1)
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
C.. Formal Arguments ..
      integer p,q,d,bd,bp,bq,init,noadmiss,imean,type,mq,out
      character status
      real*8 rez(*),imz(*),th(*),bth(*),phi(*),bphi(*)
      logical remMeanMCS
      integer fixparam(n10)
C
C.. Local Scalars ..
      integer i,nth,flagRmod,aux,difsOrig
      real*8 rdroot
C
C.. Local Arrays ..
      real*8 dar(64),dimz(64),dmodul(64),dpr(64),drez(64),ths(4)
C
C.. External Functions ..
      real*8 POLYVAL
      external POLYVAL
      integer KnownApprox
      external KnownApprox
C   LINES OF CODE ADDED FOR X-13A-S : 2
      logical dpeq
      external dpeq
C   END OF CODE BLOCK
C
C.. External Calls ..
      external RPQ
C
C.. Intrinsic Functions ..
      intrinsic MAX, MIN
      include 'stream.i'
C   LINES OF CODE ADDED FOR X-13A-S : 1
      include 'error.cmn'
C   END OF CODE BLOCK
c
c
*      integer pBak,dBak,qBak,bpBak,bdBak,bqBak 
*      real*8 phiBak(3*n1),thBak(3*n1),bphiBak(3*n1),bthBak(3*n1)
*      common /ARMAbak/ phiBak,thBak,bphiBak,bthBak,pBak,
*     $                 dBak,qBak,bpBak,bdBak,bqBak

C
C ... Executable Statements ...
C
c       Th y bth entran con sus coeficientes *(-1)
      flagRmod=0
      difsOrig=d+bd
      if (remMeanMCS) then
       imean=0
      end if
      if (status.eq.'X') then
c         Si nos sale no admisible no mantenemos el TD stoch, dejamos libertad a Seats
          if (q.gt.0) then
             q=q-1
          end if
          init=0
          do i=1,n10
            fixParam(i)=0
          enddo
          status = 'Z'
        noadmiss=2
        return 1
      else
        if (status .eq. 'G') then
         d = 1
         q = 1
         bd = 1
         bq = 1
         init = 0
         type = 1
         noadmiss = 2
         status = 'F'
         return 1
        end if
        if (status .eq. 'I') then
         q = 1
         type = 1
         init = 0
         noadmiss = 2
         status = 'H'
         return 1
        end if
        if ((p.gt.3) .or. (q.gt.3) .or. (d.gt.2) .or. (bp.gt.1) .or.
     $      (bd.gt.1) .or. (bq.gt.1)) then
         if (p .gt. 3) then
          p = 3
         end if
         if (q .gt. 3) then
          q = 3
         end if
         if (d .gt. 2) then
          d = 2
         end if
         if (bp .gt. 1) then
          bp = 1
         end if
         if (bd .gt. 1) then
          bd = 1
         end if
         if (bq .gt. 1) then
          bq = 1
         end if
         noadmiss = 2
         init = 0
         return 1
C
C P > 0 REDUCE THE VALUE OF P
C
        else if (p .gt. 0) then
         rdroot = ZERO
         do i = 1,p
          if ((rez(i).gt.ZERO) .and. dpeq(imz(i),ZERO)) then
           rdroot = rez(i)
          end if
         end do
         do i = 1,p
          if ((rez(i).gt.rdroot) .and. dpeq(imz(i),ZERO)) then
           rdroot = rez(i)
          end if
         end do
         if (rdroot .gt. 5.0d-1) then
          d = MIN(d+1,2)
          p = p - 1
          q = MIN(q+1,p+d,3)
          init = 0
          if (difsOrig.lt.d+bd) then
           imean=0
          end if
          noadmiss = 2
          return 1
         else
          p = p - 1
          q = MIN(q+1,p+d,3)
          init = 0
          noadmiss = 2
          return 1
         end if
        else
C
C BP > 0 REDUCE THE VALUE OF BP   (aqui siempre p=0)
C
         if (bp .gt. 0) then
          if (bphi(1).ge.ZERO.or.bd.gt.0) then
           bp = 0
           bd = MIN(bd+1,1)
           bq = MIN(bq+1,1)
           if (bd.eq.0) then
            imean=0
           end if
          else if (d.ne.1.or.q.ne.1) then
           if (d.eq.0) then
            imean=0
           end if
           d=1
           q=1
          else if (bq.eq.1) then
           bq=0
          else
           bp=0
          end if
          type = 1
          init = 0
          noadmiss = 2
          return 1
         end if
         if (bd .eq. 0) then
          if (bq .gt. 0) then
            bq = 0
            type = 1
            init = 0
            noadmiss = 2
            return 1
C
C
C CASE A) SEE DOCUMENTATION AGUSTIN
c                   (BD=0 BQ=0 P=0 BP=0)
C
          else if (d .eq. 2) then
            q = MAX(q-1,1)
            init = 0
            noadmiss = 2
            return 1
          else if (d .eq. 1) then
           q = MAX(q-1,1)
           init = 0
           noadmiss = 2
           return 1
          end if
C
C ELSE BD.EQ.0
C
C
C CASE B) SEE DOCUMENTATION AGUSTIN (BD=1,P=0,BP=0)
C
         else if (d .eq. 0) then
          if (q .ge. 2) then
           q = q-1
           type = 1
           noadmiss = 2
           init = 0
           return 1
          else
           d = 1
           q = 1
           bd = 1
           bq = 1
           imean = 0
           type = 1
           init = 0
           noadmiss = 2
           return 1
          end if
         else if (d .eq. 1) then
          if (q .gt. 2) then
           q = 2
           type = 1
           noadmiss = 2
           init = 0
           status = 'G'
           return 1
          else
           q = 1
           init = 0
           noadmiss = 2
           return 1
          end if
         else
          d = 2
          q = q-1
          bd = 1
          type = 1
          init = 0
          noadmiss = 2
          return 1
         end if
        end if
      end if
      end
C
C
      subroutine OUTTABLE(titleg,oz,trend,sa,sc,ir,cycle,pread,ceff,
     $                    a,na,hpcyc,hptrend,hpcycle,lamd,nstart,nzs,
     $                    mq,nex,nunits,ilen)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
      real*8 ONEHND, ZERO
      parameter (ONEHND=100.0d0, ZERO=0.0d0)
C
C.. Formal Arguments ..
      integer hpcycle,lamd,nstart,nzs,mq,nex,na,nunits,ilen,EndLoop
      character titleg*80
      real*8 oz(*),trend(*),sa(*),sc(*),ir(*),cycle(*),pread(*),ceff(*),
     $       hpcyc(*),a(*),hptrend(*)
C
C.. Local Scalars ..
      integer i,nperiod,nyear,k,Ncol,nblank
      real*8 DONE
      character LongTermCad*22, LongTermAbb*50
C
C.. Local Arrays ..
      real*8 eresid(mpkp)
      real*8 Matrix(mpkp,14)
      character testo*1280
      character*(100) GetTokenIdx
C
C.. External Functions ..
      integer ISTRLEN
      external ISTRLEN,GetTokenIdx
C
C.. Intrinsic Functions ..
      intrinsic EXP, MAX
      include 'sform.i'
      include 'preadtr.i'
      include 'prtous.i'
      include 'htmlout.cmn'
      include 'stream.i'
C
C ... Executable Statements ...
C
      If (HPcycle.eq.1) then
        LongTermCad='LONG TERM TREND'
        LongTermAbb='@'
      else if (HPcycle.eq.2) then
        LongTermCad='SA series without BC'
        LongTermAbb='Seasonally Adjusted series without Business Cycle'
      else
        LongTermCad='Series without BC'
        LongTermAbb='Series without Business Cycle'
      end if
      testo = ' '
      nperiod = Nper
      nyear = Nyer
      if ((NZ .eq. 1) .and. (NYER+NPER+NFREQ .eq. 0)) then
       DONE = -1.0d0
       CALL writTagOneLine(36,'h2','center',titleg(1:ISTRLEN(titleg)))
       CALL mkTableTag(36,'x11','Failed')
       write (36,3601)
 3601  FORMAT('<tr><th>DATE</th><th>Failed</th></tr>')
       WRITE (36,3602)0, 0, DONE
 3602  FORMAT('<tr><td>',I2,'-',I4,'</td><td>',G18.9,'</td></tr>')
       CALL writTag(36,'</table>')
       return
      end if
      if (nunits .ne. 0) then
       write (36,3603) '<h2 class="center">',titleg(1:ISTRLEN(titleg)),
     &  Cbr,'(Series in input file has been multiplied by 10**',
     &  3*nunits,').','</h2>'
 3603  FORMAT(2A,2x,3A,i3,2A)
      else
       CALL writTagOneLine(36,'h2','center',titleg(1:ISTRLEN(titleg)))
      end if
      Ncol = 0
      if (xotab.eq.1) Ncol = Ncol + 1
      if (ptab .eq. 1) ncol = ncol + 1
      if (ntab .eq.1) ncol = ncol + 1
      if (stab .eq. 1) ncol = ncol + 1
      if (caltab .eq. 1) ncol = ncol + 1
      if (patab .eq. 1) ncol = ncol + 1
      if (hpcycle .ge. 1) then   
       if (cytab .eq. 1) ncol = ncol + 1
       if (ltptab .eq. 1) ncol = ncol + 1
      end if
      if (ertab .eq. 1) ncol=ncol+1
      if (rg0tab .eq. 1) ncol=ncol+1
      CALL genTableTag(36,'Matrix of Series',nCol,.true.)
      CALL writTag(36,'<thead>')
      CALL writTag(36,'<tr>')
      CALL mkTableCell(36,'head','&nbsp;')
      if (nex .eq. 1) then
       EndLoop = nzs+ilen
      else
       endLoop =nzs
      end if
      Ncol = 0
      DO i = 1, 14
       DO k = 1, mpkp
        Matrix(k,i) = ZERO
       END DO
      END DO
cc
c Print Original Series
cc
      if (xotab.eq.1) then
       ncol=ncol+1
       if (nex .eq. 1) then
        if (lamd .eq. 0) then
         do i=nstart, endLoop
           Matrix(i,Ncol) = EXP(oz(i))
         end do
        else
         do i=nstart, endLoop
          Matrix(i,Ncol) =oz(i)
         end do
        end if
       else 
        do i=nstart, endLoop
         Matrix(i,Ncol) =oz(i)
        end do
       end if
       CALL mkHeaderCellScope(36,0,0,'col','@','Series')
      end if
cc
c Print Trend
cc
      if (ptab .eq. 1) then
       ncol=ncol+1
       do i=nstart, endLoop
        Matrix(i,Ncol) =trend(i)
       end do    
       CALL mkHeaderCellScope(36,0,0,'col','@','Trend-Cycle')
      end if
cc
c Print SA Series
cc
      if (ntab .eq.1) then
       ncol=ncol+1
       do i=nstart, endLoop
        Matrix(i,Ncol) = sa(i)
       end do
       CALL mkHeaderCellScope(36,0,0,'col',
     &                        'Seasonally Adjusted Series','SA Series')
      end if
cc
c Print Seasonal Series
cc
      if (stab .eq. 1) then
       ncol=ncol+1
       do i=nstart, endLoop
        Matrix(i,Ncol) = sc(i)
       end do
       CALL mkHeaderCellScope(36,0,0,'col','@','Seasonal')
      end if
cc
c Print Calendar Effect
cc
      if (caltab .eq. 1) then
       ncol=ncol+1
       if (lamd .eq. 0) then
        do i=nstart, endLoop
         Matrix(i,Ncol) = ceff(i)*ONEHND
        end do   
       else
        do i=nstart, endLoop
         Matrix(i,Ncol) = ceff(i)
        end do   
       end if
       CALL mkHeaderCellScope(36,0,0,'col','@','Calendar')
      end if
cc
c Print Peradjusted components
cc    
      if (patab .eq. 1) then
       ncol=ncol+1
       if (nex .eq. 1) then
        if (lamd .eq. 0) then
         do i=nstart, endLoop
          Matrix(i,ncol) = ONEHND
         enddo
        else
         do i=nstart, endLoop
          Matrix(i,ncol) = ZERO
         enddo 
        end if
       else
        do i=nstart, endLoop
         Matrix(i,ncol) = pread(i)
        enddo 
       end if
       CALL mkHeaderCellScope(36,0,0,'col','@','Preadjust')
      end if
cc
c If HPCYCLE >= 1 Print Long Term Trend  and Business Cycle
cc    
      if (hpcycle .ge. 1) then   
cc
c Print Business Cycle
cc       
       if (cytab .eq. 1) then
        ncol=ncol+1
        if (nex .eq. 1) then
         do i = nstart,nzs+ilen/2
          Matrix(i,ncol) = hpcyc(i)
         enddo
         do i=nzs+ilen/2+1,ilen
          Matrix(i,ncol) = ZERO
         enddo
        else
         do i= nstart,nzs
          Matrix(i,ncol) = hpcyc(i) 
         enddo 
        end if  
        CALL mkHeaderCellScope(36,0,0,'col','@','Cycle')
       end if
cc
c Print Long Term Trend
cc
       if (ltptab .eq. 1) then
        ncol=ncol+1
        if (nex .eq. 1) then
         do i = nstart,nzs+ilen/2
          Matrix(i,ncol) = hptrend(i)
         enddo
         do i=nzs+ilen/2+1,ilen
          Matrix(i,ncol) = ZERO
         enddo
        else
         do i= nstart,nzs
          Matrix(i,ncol) = hptrend(i) 
         enddo 
        end if  
        CALL mkHeaderCellScope(36,0,0,'col',
     $                         LongTermAbb(1:Istrlen(LongTermAbb)),
     $                         LongTermCad(1:Istrlen(LongTermCad)))
       end if
      end if
cc
c Print Extended residuals
cc    
      if (ertab .eq. 1) then
       ncol=ncol+1
       k = nzs - na
       if (k .ge.0) then
        do i = 1, k
         eresid(i)=ZERO
        end do
        do i = 1, na
         eresid(k+i)=a(i)
        end do
        do i = na+k+1, nzs+ilen
         eresid(i) = ZERO
        end do
       else
        do i = -k+1, na
         eresid (i+k) = a(i)
        end do
        do i = nzs+1, nzs+ilen
         eresid(i) = ZERO
        end do
       end if
       do i=nstart, endLoop
        Matrix(i,ncol) = eresid(i)
       enddo 
       CALL mkHeaderCellScope(36,0,0,'col','@','E-Residuals')
      end if
cc
c Print Separate Effect Regression Component
cc    
      if (rg0tab .eq. 1) then
       ncol=ncol+1
       if (lamd .eq. 1) then
        do i=nstart, endLoop
         Matrix(i,ncol) = pareg(i,0)
        enddo 
       else
        do i=nstart, endLoop
         Matrix(i,ncol) = pareg(i,0)*ONEHND
        enddo 
       end if
       CALL mkHeaderCellScope(36,0,0,'col',
     &                 'Separate Regression Component','Sep. REG Comp.')
      end if
cc
c Print SA Effect Regression Component
cc
      if (rgsatab .eq. 1) then
       ncol=ncol+1
       if (lamd .eq. 1) then
        do i=nstart, endLoop
         Matrix(i,ncol) = pareg(i,4)
        enddo 
       else
        do i=nstart, endLoop
         Matrix(i,ncol) = pareg(i,4)*ONEHND
        enddo 
       end if
       CALL mkHeaderCellScope(36,0,0,'col',
     &             'Regression Component in Seasonally Adjusted Series',
     &                        'REG Comp. in SA')
      end if
cc
c Print all the matrix
cc    
      call LEFTTRIM(testo)
      CALL writTag(36,'</tr>')
      CALL writTag(36,'</thead>')
      CALL writTag(36,'<tbody>')
      do I = 1,endLoop
        CALL writTag(36,'<tr>')
        write (36,3604)nperiod, nyear
 3604   format('<th scope="row">',I2,'-',I4,'</th>')
        DO K = 1,Ncol
          write(36,3605)Matrix(I,K)
 3605     format('<td>',G18.9,'</td>')
        END DO
        CALL writTag(36,'</tr>')
        nperiod = nperiod + 1
        if (nperiod .gt. mq) then
         nperiod = 1
         nyear = nyear + 1
        end if
       end do
      CALL writTag(36,'</tbody>')
      CALL writTag(36,'</table>')
      return
      end
C
C
      subroutine OUTTABFOR(ftr,fsa,fs,fir,fcyc,pread,ceff,hpcyc,
     $                     hptrend,hpcycle,lamd,nstart,nf,nzs,mq,
     $                     strend,ssa,fosa)
C
C.. Implicits ..
      implicit none
      INCLUDE 'srslen.prm'
      INCLUDE 'dimensions.i'
      real*8 ZERO,ONEHND
      parameter (ZERO=0.0d0,ONEHND=100.0d0)
C
C.. Formal Arguments ..
      integer hpcycle,lamd,nstart,nf,nzs,mq
      real*8 ftr(-kp:kp),fsa(-kp:kp),fs(-kp:kp),fir(-kp:kp),
     $       fcyc(-kp:kp),pread(*),ceff(*),hpcyc(*),hptrend(*),
     $       strend(*),ssa(*),fosa(*)
C
C.. Local Scalars ..
      integer i,nperiod,nyear,Ncol,k
      include 'sform.i'
      include 'preadtr.i'
      include 'prtous.i'
      include 'stream.i'
      include 'bench.i'
C
C.. Local Arrays ..
      real*8 Matrix(Kp,1:18)
C
C ... Executable Statements ...
C
      DO i = 1, 18
       DO k = 1, kp
        Matrix(k,i) = ZERO
       END DO
      END DO
      nperiod = Nper
      nyear = Nyer
      do i = 1,nzs
       nperiod = nperiod + 1
       if (nperiod .gt. mq) then
        nperiod = 1
        nyear = nyear + 1
       end if
      end do
      Ncol = 0
cc
c Print Original Series Forecast
cc
      if (xotab .eq. 1) then
       ncol=ncol+1
       do i=nstart, nf
        Matrix(i,Ncol) = tram(nzs+i)
       end do
      end if
cc
c Print Trend Forecast
cc
      if (ptab .eq. 1) then
       ncol=ncol+1
       do i=nstart, nf
        Matrix(i,Ncol) =ftr(i)
       end do    
      end if
c Real-Time Trend Estimator
cc
      if (rtptab .eq. 1) then
       ncol=ncol+1
       do i=nstart, nf
        Matrix(i,Ncol) =ZERO
       end do    
      end if
cc
cc
c Print SA Series Forecast
cc
      if (ntab .eq.1) then
        ncol=ncol+1
        do i=nstart, nf
         Matrix(i,Ncol) = fsa(i)
        end do    
      end if    
cc
c Real-Time SA Series Estimator
cc
      if (rtsatab .eq. 1) then
       ncol=ncol+1
       do i=nstart, nf
        Matrix(i,Ncol) =ZERO
       end do    
      end if
cc
c Print SA Yearly Revised Series Forecast
cc
      if ((BcMark.eq.1).and.((Mq.eq.4).or.(Mq.eq.12))) then
       if (ntab .eq.1) then
         ncol=ncol+1
         do i=nstart, nf
          Matrix(i,Ncol) = fosa(nzs+i)
         end do    
       end if
      end if
cc
c Print Seasonal Forecast
cc
      if (stab .eq. 1) then
       ncol=ncol+1
       do i=nstart, nf
        Matrix(i,Ncol) = fs(i)
       end do    
      end if
cc
c Print Calendar Forecast
cc
      if (caltab .eq. 1) then
       ncol=ncol+1
       if (lamd .eq. 0) then
        do i=nstart, nf
         Matrix(i,Ncol) = ceff(nzs+i)*ONEHND
        end do
       else
        do i=nstart, nf
         Matrix(i,Ncol) = ceff(nzs+i)
        end do
       end if
      end if
cc
c Print Ir Forecast
cc
      if (utab .eq. 1) then
        ncol=ncol+1
        do i=nstart,nf
          Matrix(i,ncol)=fir(i)
        end do
      end if
cc
c Print Transtory 
cc
      if (ctab .eq. 1) then
        ncol=ncol+1
        do i=nstart,nf
          Matrix(i,ncol)=fcyc(i)
        end do
      end if
cc
c Print Preadjusted Forecast
cc
      if (patab .eq. 1) then
       ncol=ncol+1
       do i=nstart, nf
        Matrix(i,ncol) = pread(nzs+i)
       enddo 
      end if
cc
c If HPCYCLE >= 1 Print Business Cycle and Long Trend Term
cc      
      if (hpcycle .ge. 1) then 
cc
c Print Business Cycle forecast
cc
       if (cytab .eq. 1) then
        ncol=ncol+1
        do i = nstart,nf
         Matrix(i,ncol) = hpcyc(nzs+i)
        enddo
       end if
cc
c Print Long Term Trend Forecast
cc
       if (ltptab .eq. 1) then
        ncol=ncol+1
        do i= nstart,nf
         Matrix(i,ncol) = hptrend(Nzs+i) 
        enddo 
       end if
      end if
cc 
c Extended residuals forecast (all zero)
cc
      if (ertab .eq. 1) then
       ncol=ncol+1
       do i=nstart, nf
        Matrix(i,ncol) = ZERO
       enddo 
      end if
cc
c Print Separate Regression Effect Forecast
cc
      if (rg0tab .eq. 1) then
       ncol=ncol+1
       if (lamd .eq. 0) then
        do i=nstart, nf
         Matrix(i,ncol) = pareg(nzs+i,0)*ONEHND
        enddo 
       else
        do i=nstart, nf
         Matrix(i,ncol) = pareg(nzs+i,0)
        enddo 
       end if
      end if
cc
c Print SA Regression Effect Forecast
cc
      if (rgsatab .eq. 1) then
       ncol=ncol+1
       if (lamd .eq. 0) then
        do i=nstart, nf
         Matrix(i,ncol) = pareg(nzs+i,4)*ONEHND
        enddo 
       else
        do i=nstart, nf
         Matrix(i,ncol) = pareg(nzs+i,4)
        enddo 
       end if
      end if
cc
c Print Stochastic Trend-Cycle Forecast
cc

      if (stptab .eq. 1) then
       ncol=ncol+1
       do i=nstart, nf
        Matrix(i,ncol) = strend(nzs+i)
       enddo 
      end if
cc
c Print Stochastic SA Forecast
cc
      if (stntab .eq. 1) then
       ncol=ncol+1
       do i=nstart, nf
        Matrix(i,ncol) = ssa(nzs+i)
       enddo 
      end if 




c      write(tstmp,'(''("<tr><td>",I2,"-",I4,"</td>"'',I2,
c     &               ''("<td>",G18.9,"</td>"),"</tr>")'')') ncol
*       tstmp='(''<tr><td scope="row" axis="forecast" class="h">'','
*     &         //'I2,''-'',I4,''</td>'','//fdec(ncol) //
*     &         '(''<td>'',G18.9,''</td>''),''</tr>'')'
       CALL writTagClass(36,'tbody','f')
       do I = 1,nf
        CALL writTag(36,'<tr>')
        WRITE(36,1010)nperiod, nyear
 1010   FORMAT('<th scope="row"> ',I2,'-',I4,'</th>')
        DO k = 1, Ncol
         write (36,1020) Matrix(I,K)
 1020    format('<td> ',g18.9,' </td>')
        END DO
        nperiod = nperiod + 1
        if (nperiod .gt. mq) then
         nperiod = 1
         nyear = nyear + 1
        end if
        CALL writTag(36,'</tr>')
       end do
       CALL writTag(36,'</tbody>')
       CALL writTag(36,'</table>')
      return
      end  
CC
C
CC
      subroutine CheckLen(OZ,NZ,Mq,IsOk)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
      real*8 OZ(*)
      integer NZ,Mq,IsOk
C
C.. Local Scalars ..
      integer i,nfirst,nlast,nzmin,noa,kk1,kk2
C
C.. Intrinsic Functions ..
      intrinsic MAX
C   LINES OF CODE ADDED FOR X-13A-S : 2
      logical dpeq
      external dpeq
C   END OF CODE BLOCK
C
C ..
       nfirst=0
       nlast=0
       do i=1,NZ 
        if ((.not.dpeq(OZ(i),-99999.0d0)) .and. (nfirst .eq. 0)) then
         nfirst=i
         nlast=i
        end if
        if ((.not.dpeq(OZ(i),-99999.0d0)) .and. (nfirst .ne. 0)) then
         nlast=i
        end if
       end do
       nzmin = 0
       noa = 0
       do i=nfirst,nlast
        if (.not.dpeq(OZ(i),-99999.0d0)) then
         nzmin = nzmin + 1
        else
         noa = noa + 1
        end if
       end do
       if (Mq .eq. 12) then
        kk1 = 36
        kk2 = 30
       else
        kk1 = max(12, 4*Mq)
        kk2 = max (8, 3*Mq)
       end if
       if ((nzmin .ge. kk1) .and. (nzmin-noa .ge. kk2)) then
        IsOk = 1
       end if
       return
      end
CC
c
CC
      subroutine LEFTTRIM(string)
C
C.. Implicits ..
C 
C.. Implicits .. 
      implicit none
C 
C.. Formal Arguments .. 
C.. In/Out Status: Maybe Read, Maybe Written ..
      character*(*) string
C 
C.. Local Scalars .. 
      integer i,jlen,fnoblank
C 
C.. External Functions .. 
      integer ISTRLEN
      external ISTRLEN
C 
C ... Executable Statements ...
C 
CC
C
CC
      jlen = ISTRLEN(string)
      fnoblank = 0
      do i = 1,jlen
       if (string(i:i).ne.' ') then
      fnoblank = i
      goto 10
       end if
      end do
 10   if (fnoblank .gt. 1) then
        do i=fnoblank,jlen
        string(i-fnoblank+1:i-fnoblank+1) = string(i:i)
      end do
      do i=jlen-fnoblank+2,jlen
       string(i:i)= ' '
      end do
      end if
      end
CC
C
CC

      subroutine OUTTABLE2(titleg,oz,trend,sa,sc,ir,cycle,pread,ceff,
     $                    a,na,hpcyc,hptrend,hpcycle,lamd,nstart,nzs,
     $                   mq,nex,nunits,ilen,strend,ssa,fosa,IsCloseToTD)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
      real*8 ZERO,ONEHND
      parameter (ZERO=0.0d0,ONEHND=100.0d0)
C
C.. Formal Arguments ..
      integer hpcycle,lamd,nstart,nzs,mq,nex,na,nunits,ilen,EndLoop
      character titleg*80
      real*8 oz(*),trend(*),sa(*),sc(*),ir(*),cycle(*),pread(*),ceff(*),
     $       hpcyc(*),a(*),hptrend(*),ssa(*),strend(*),fosa(*)
      logical IsCloseToTD
C
C.. Local Scalars ..
      integer i,nperiod,nyear,k,Ncol,nblank
      real*8 DONE
      character LongTermCad*22, LongTermAbb*50
      character cad1*80
C
C.. Local Arrays ..
      real*8 eresid(mpkp)
      real*8 Matrix(mpkp,18)
      character testo*1280
      character*(100) GetTokenIdx
      character*100 lineFormat
C
C.. External Functions ..
      integer ISTRLEN
      external ISTRLEN,GetTokenIdx
C
C.. Intrinsic Functions ..
      intrinsic EXP, MAX
      include 'sform.i'
      include 'preadtr.i'
      include 'rtestm.i'
      include 'prtous.i'
      include 'bench.i'
      include 'htmlout.cmn'
C
C ... Executable Statements ...
C
      DO i = 1, 18
       DO k = 1, mpkp
        Matrix(k,i) = ZERO
       END DO
      END DO
      If (HPcycle.eq.1) then
        LongTermCad='LONG TERM TREND'
        LongTermAbb='@'
      else if (HPcycle.eq.2) then
        LongTermCad='SA series without BC'
        LongTermAbb='Seasonally Adjusted series without Business Cycle'
      else
        LongTermCad='Series without BC'
        LongTermAbb='Series without Business Cycle'
      end if
      testo = ' '
      nperiod = Nper
      nyear = Nyer
      if ((NZ .eq. 1) .and. (NYER+NPER+NFREQ .eq. 0)) then
       DONE = -1.0d0
       CALL writTagOneLine(36,'h2','center',titleg(1:ISTRLEN(titleg)))
       CALL mkTableTag(36,'x11','Failed')
       write (36,3601)
 3601  FORMAT('<tr><th>DATE</th><th>Failed</th></tr>')
       WRITE (36,3602)0, 0, DONE
 3602  FORMAT('<tr><td>',I2,'-',I4,'</td><td>',G18.9,'</td></tr>')
       CALL writTag(36,'</table>')
       return
      endif
      if (nunits .ne. 0) then
       write (36,3603) '<h2 class="center">',titleg(1:ISTRLEN(titleg)),
     &  Cbr,'(Series in input file has been multiplied by 10**',
     &  3*nunits,').','</h2>'
 3603  FORMAT(2A,2x,A,/,A,i3,2A)
      else
       CALL writTagOneLine(36,'h2','center',titleg(1:ISTRLEN(titleg)))
      end if
      Ncol = 0
      if (xotab.eq.1) Ncol = Ncol + 1
      if (ptab .eq. 1) ncol = ncol + 1
      if (rtptab .eq. 1) ncol = ncol + 1
      if (ntab .eq.1) ncol = ncol + 1
      if (rtsatab .eq. 1) ncol = ncol + 1
      if (stab .eq. 1) ncol = ncol + 1
      if (caltab .eq. 1) ncol = ncol + 1
      if (utab .eq. 1) ncol = ncol + 1
      if (ctab .eq. 1) ncol = ncol + 1
      if (patab .eq. 1) ncol = ncol + 1
      if (hpcycle .ge. 1) then   
       if (cytab .eq. 1) ncol = ncol + 1
       if (ltptab .eq. 1) ncol = ncol + 1
      end if
      if (ertab .eq. 1) ncol=ncol+1
      if (rg0tab .eq. 1) ncol=ncol+1
      if (rgsatab .eq. 1) ncol=ncol+1
      if (stptab .eq. 1) ncol=ncol+1
      if (stntab .eq. 1) ncol=ncol+1
      CALL genTableTag(36,'Matrix of Series',nCol,.true.)
      CALL writTag(36,'<thead>')
      CALL writTag(36,'<tr>')
      CALL mkTableCell(36,'head','&nbsp;')
      if (nex .eq. 1) then
       EndLoop = nzs+ilen
      else
       endLoop =nzs
      end if    
      Ncol = 0
cc
c Print Original Series
cc
      if (xotab.eq.1) then
       ncol=ncol+1
       if (nex .eq. 1) then
        if (lamd .eq. 0) then
         do i=nstart, endLoop
           Matrix(i,Ncol) = EXP(oz(i))
         end do
        else
         do i=nstart, endLoop
          Matrix(i,Ncol) =oz(i)
         end do  
        end if    
       else 
        do i=nstart, endLoop
         Matrix(i,Ncol) =oz(i)
        end do      
       end if
       CALL mkHeaderCellScope(36,0,0,'col','@','Series')
      end if
cc
c Print Trend
cc
      if (ptab .eq. 1) then
       ncol=ncol+1
       do i=nstart, endLoop
        Matrix(i,Ncol) =trend(i)
       end do    
       CALL mkHeaderCellScope(36,0,0,'col','@','Trend-Cycle')
      end if
cc
c Print RealTimeTrend
cc
      if (rtptab .eq. 1) then
       ncol=ncol+1
       do i=nstart, nzs-nrt-1
        Matrix(i,Ncol) =ZERO
       end do    
       do i=1,nrt
        Matrix(nzs-nrt-1+i,Ncol) = RTtre(i)
       end do    
       do i=nzs,nzs+ilen
        Matrix(i,Ncol) =ZERO
       end do    
       CALL mkHeaderCellScope(36,0,0,'col','@','Real-Time Trend-Cycle')
      end if
cc
c Print SA Series
cc
      if (ntab .eq.1) then
       ncol=ncol+1
       do i=nstart, endLoop
        Matrix(i,Ncol) = sa(i)
       end do    
       CALL mkHeaderCellScope(36,0,0,'col',
     &                        'Seasonally Adjusted Series','SA Series')
      end if
cc
c Print RealTime SA Series
cc
      if (rtsatab .eq. 1) then
       ncol=ncol+1
       do i=nstart, nzs-nrt-1
        Matrix(i,Ncol) =ZERO
       end do    
       do i=1,nrt
        Matrix(nzs-nrt-1+i,Ncol) = RTsa(i)
       end do    
       do i=nzs,nzs+ilen
        Matrix(i,Ncol) =ZERO
       end do    
       CALL mkHeaderCellScope(36,0,0,'col',
     &                        'Real-Time Seasonally Adjusted Series',
     &                        'Real-Time SA Series')
      end if
cc
c Print SA Series Yearly Revised
cc
      if ((BcMark .eq. 1) .and. ((Mq .eq.4).or.(Mq .eq.12))) then
       if (ntab .eq.1) then
        ncol=ncol+1
        do i=nstart, endLoop
         Matrix(i,Ncol) = fosa(i)
        end do    
        CALL mkHeaderCellScope(36,0,0,'col',
     &          'Seasonally Adjusted Series with Revised Yearly Totals',
     &                         'Y. Revised SA')
       end if
      end if
cc
c Print Seasonal Series
cc
      if (stab .eq. 1) then
       ncol=ncol+1
       do i=nstart, endLoop
        Matrix(i,Ncol) = sc(i)
       end do    
       CALL mkHeaderCellScope(36,0,0,'col','@','Seasonal')
      end if
cc
c Print Calendar Effect
cc
      if (caltab .eq. 1) then
       ncol=ncol+1
       if (lamd .eq. 0) then
        do i=nstart, endLoop
         Matrix(i,Ncol) = ceff(i)*ONEHND
        end do   
       else
        do i=nstart, endLoop
         Matrix(i,Ncol) = ceff(i)
        end do   
       end if
       CALL mkHeaderCellScope(36,0,0,'col','@','Calendar')
      end if
cc
c Print Irregular component
cc
      if (utab .eq. 1) then
        ncol=ncol+1
        do i=nstart, endLoop 
         Matrix(i,ncol) = ir(i) 
        end do 
       CALL mkHeaderCellScope(36,0,0,'col','@','Irregular')
      end if
cc
c Print Transitory component
cc
      if (ctab .eq. 1) then
       ncol=ncol+1
       if (IsCloseToTD) then
         cad1='final TD'
       else
         cad1='Transitory'
       end if
        do i=nstart, endLoop
         Matrix(i,ncol) = cycle(i)
        end do
       CALL mkHeaderCellScope(36,0,0,'col','@',cad1(1:istrlen(cad1)))
      end if
cc
c Print Peradjusted components
cc    
      if (patab .eq. 1) then
       ncol=ncol+1
       if (nex .eq. 1) then
        if (lamd .eq. 0) then
         do i=nstart, endLoop
          Matrix(i,ncol) = ONEHND
         enddo
        else
         do i=nstart, endLoop
          Matrix(i,ncol) = ZERO
         enddo 
        end if
       else
        do i=nstart, endLoop
         Matrix(i,ncol) = pread(i)
        enddo 
       end if
       CALL mkHeaderCellScope(36,0,0,'col','Preadjusted Factor',
     &                        'Preadjust')
      end if
cc
c If HPCYCLE >= 1 Print Long Term Trend  and Business Cycle
cc    
      if (hpcycle .ge. 1) then   
cc
c Print Long Term Trend
cc       
       if (cytab .eq. 1) then
        ncol=ncol+1
        if (nex .eq. 1) then
         do i = nstart,nzs+ilen/2
          Matrix(i,ncol) = hpcyc(i)
         enddo
         do i=nzs+ilen/2+1,nzs+ilen
          Matrix(i,ncol) = ZERO
         enddo
        else
         do i= nstart,nzs
          Matrix(i,ncol) = hpcyc(i) 
         enddo 
        end if  
        CALL mkHeaderCellScope(36,0,0,'col','@','Cycle')
       end if
cc
c Print Business Cycle
cc
       if (ltptab .eq. 1) then
        ncol=ncol+1
        if (nex .eq. 1) then
         do i = nstart,nzs+ilen/2
          Matrix(i,ncol) = hptrend(i)
         enddo
         do i=nzs+ilen/2+1,nzs+ilen
          Matrix(i,ncol) = ZERO
         enddo
        else
         do i= nstart,nzs
          Matrix(i,ncol) = hptrend(i) 
         enddo 
        end if  
        CALL mkHeaderCellScope(36,0,0,'col',
     &                         LongTermAbb(1:istrlen(LongTermAbb)),
     &                         LongTermCad(1:istrlen(LongTermCad)))
       end if
      end if
cc
c Print Extended residuals
cc    
      if (ertab .eq. 1) then
       ncol=ncol+1
       k = nzs - na
       if (k .ge.0) then
        do i = 1, k
         eresid(i)=ZERO
        end do
        do i = 1, na
         eresid(k+i)=a(i)
        end do
        do i = na+k+1, nzs+ilen
         eresid(i) = ZERO
        end do
       else
        do i = -k+1, na
         eresid (i+k) = a(i)
        end do
        do i = nzs+1, nzs+ilen
         eresid(i) = ZERO
        end do
       end if
       do i=nstart, endLoop
        Matrix(i,ncol) = eresid(i)
       enddo 
       CALL mkHeaderCellScope(36,0,0,'col','@','E-Resid')
      end if
cc
c Print Separate Effect Regression Component
cc    
      if (rg0tab .eq. 1) then
       ncol=ncol+1
       if (lamd .eq. 1) then
        do i=nstart, endLoop
         Matrix(i,ncol) = pareg(i,0)
        enddo 
       else
        do i=nstart, endLoop
         Matrix(i,ncol) = pareg(i,0)*ONEHND
        enddo 
       end if
       CALL mkHeaderCellScope(36,0,0,'col',
     &                        'Separate REGression Component',
     &                        'Sep. REG Comp.')
      end if
cc
c Print SA Effect Regression Component
cc
      if (rgsatab .eq. 1) then
       ncol=ncol+1
       if (lamd .eq. 1) then
        do i=nstart, endLoop
         Matrix(i,ncol) = pareg(i,4)
        enddo 
       else
        do i=nstart, endLoop
         Matrix(i,ncol) = pareg(i,4)*ONEHND
        enddo 
       end if
       CALL mkHeaderCellScope(36,0,0,'col',
     &             'REGression Component in Seasonally Adjusted series',
     &                        'REG Comp. in SA')
      end if
cc rober
cc
c Print Stochastic Trend-Cycle
cc

      if (stptab .eq. 1) then
       ncol=ncol+1
       do i=nstart, endLoop
        Matrix(i,ncol) = strend(i)
       enddo 
       CALL mkHeaderCellScope(36,0,0,'col','Stochastic Trend Cycle',
     &                        'Stoch. TrendCycle')
      end if
cc
c Print Stochastic SA 
cc
      if (stntab .eq. 1) then
       ncol=ncol+1
       do i=nstart, endLoop
        Matrix(i,ncol) = ssa(i)
       enddo 
       CALL mkHeaderCellScope(36,0,0,'col',
     &                        'Stochastic Seasonally Adjusted Series',
     &                        'Stoch. SA Series')
      end if

cc
c Print all the matrix
cc    
      CALL writTag(36,'</tr>')
      CALL writTag(36,'</thead>')
      CALL writTag(36,'<tbody>')
c      write(lineFormat,'(''("<tr scope="row"><td>",I2,"-",I4,"</td>"'',
c     &               I2,''("<td>",G18.9,"</td>"),"</tr>")'')') ncol
c       write(lineFormat,'(''I2,''-'',I4,''</td>'''',
c     &             I2,''(''<td>'',G18.9,''</td>''),''</tr>'')'')') ncol
       do I = 1,endLoop
        CALL writTag(36,'<tr>')
        write (36,3604) nperiod, nyear
 3604   format('<th scope="row">',I2,'-',I4,'</th>')
        do K = 1, Ncol
         write(36,3605)Matrix(I,K)
 3605    format('<td> ',G18.9,' </td>')
        end do
        nperiod = nperiod + 1
        if (nperiod .gt. mq) then
         nperiod = 1
         nyear = nyear + 1
        end if
        CALL writTag(36,'</tr>')
       end do
       CALL writTag(36,'</tbody>')
       if (nex .ne. 2) then
        CALL writTag(36,'</table>')
       end if
      return
      end
C
cc
c
cc
      integer function KnownApprox(p,q,d,bd,bp,bq,init,noadmiss,
     $                             th,bth,phi,bphi,mq,status)
C
C.. Implicits ..
      implicit none
c parameters      
      integer n1
      parameter (n1=1)
      real*8 ZERO,ONE
      parameter (ZERO=0.0D0,ONE=1.0d0)
C
C.. Formal Arguments ..
      integer p,q,d,bd,bp,bq,init,noadmiss,mq
      character status
      real*8 th(*),bth(*),phi(*),bphi(*)
C
C
      integer pBak,dBak,qBak,bpBak,bdBak,bqBak 
      real*8 phiBak(3*n1),thBak(3*n1),bphiBak(3*n1),bthBak(3*n1)
      common /ARMAbak/ phiBak,thBak,bphiBak,bthBak,pBak,
     $                 dBak,qBak,bpBak,bdBak,bqBak
c locals
      integer i
C
C ... Executable Statements ...
C
c       Th y bth entran con sus coeficientes *(-1)
      KnownApprox=0
      if (status.ne.'J') then
       do i=1,3
        thBak(i)=th(i)
        bthBak(i)=bth(i)
        phiBak(i)=phi(i)
        bphibak(i)=bphi(i)
        pBak=p
        dBak=d
        qBak=q
        bpBak=bp  
        bdBak=bd
        bqBak=bq
       enddo
       if ((bp.eq.0).and.(bd.eq.1)) then
c  (001) (011) mq=4  
        if ((p.eq.0).and.(d.eq.0).and.(q.le.1).and.(mq.eq.4)) then
         init=2
         bq=0
         Bth(1) = ZERO
         noadmiss=2
         KnownApprox=2        
c (110)(010) mq=12         
        else if((p.eq.1).and.(d.eq.1).and.(q.eq.0).and.(bq.eq.0) 
     &      .and. (mq.eq.12)) then 
            phi(1)=0.55
          noadmiss=2
          KnownApprox=2
            init=2   
c (110)(011) o (010)(011) mq=12
        else if((p.le.1).and.(d.eq.1).and.(q.eq.0).and.(bq.eq.1)) then
         if (mq.eq.12) then
          if (phi(1).le.0.15d0) then
           bth(1)=0
           bq=0
          else if (phi(1).le.0.33d0) then     
           if (bth(1).lt.ZERO) then 
            bth(1)=ZERO
            bq=0
           else
            phi(1)=0.15d0 
           end if
          else if(phi(1).le.0.5d0) then
           phi(1)=0.5d0          
           if (bth(1).lt.ZERO) then 
            bth(1)=ZERO
            bq=0
           end if 
          else
           bth(1)=phi(1)+phi(1)-1   
          end if 
          noadmiss=2 
          init = 2
          status='J'
          KnownApprox= 2
c (110)(011) mq=4
         else if (mq.eq.4) then
          if (phi(1).le.-0.5d0) then
           bth(1)=-0.35d0
          else if (phi(1).le.-0.15d0) then
           bth(1)= -phi(1)-0.4d0
           if (abs(bth(1)).lt.1.d-5) then
            bth(1)=ZERO
            bq=0 
           end if
          else if (phi(1).le.0.15d0) then
           bth(1)=-0.25d0
          else if (phi(1).le.0.33d0) then
           phi(1)=0.15d0
           if (bth(1).lt.-0.2d0) then
            bth(1)=-0.2d0
           end if
          else if (phi(1).lt.0.5d0) then
           phi(1)=0.5d0
           if (bth(1).lt.ZERO) then 
            bq=0
            bth(1)=ZERO
           end if
          else
           bth(1)=-0.3d0
          end if
          noadmiss=2 
          init = 2
          status='J'
          KnownApprox=2   
         end if 
c
c  mq=12 (001)(011) o (100)(011)     
        else if ((mq.eq.12).and.(d.eq.0).and.(p+q.le.1)) then
         bq=0
         bth(1)=ZERO 
         noadmiss=2 
         init = 2
         if (p.eq.1) then
          status='J'
         end if
         KnownApprox= 2             
c***     dejamos que lo controle approximate
c mq=4 (120)(011)
*        else if ((mq.eq.4).and.(p.eq.1).and.(d.eq.2).and.(q.eq.0)) then
*         if (phi(1).le.-0.5d0) then
*          bth(1)=-0.4d0
*         else if (phi(1).lt.-0.35d0) then
*          bth(1)=-0.15d0
*         else if (phi(1).le.0.1d0) then
*          if (bth(1).gt.0.95d0) then
*           bth(1)=0.95d0
*          else
*           bth(1)=-0.35d0
*          end if
*         else if (phi(1).lt.0.3d0) then
*          bq=0
*          bth(1)=0.d0
*         else if (phi(1).le.0.5d0) then
*          phi(1)=0.5d0
*          if (bth(1).lt.-0.2d0) then
*           bth(1)=-0.2d0    
*          end if
*         else
*          bq=0
*          bth(1)=0.d0 
*         end if
*         init=2
*         noadmiss=2
*         status='J'
*         KnownApprox= 2  
c mq=4 (100)(011)
        else if ((mq.eq.4).and.(p.eq.1).and.(d.eq.0).and.(q.eq.0)) then
         if (bth(1) .gt.0.95d0) then
          bth(1)=0.95
         else
          if ((phi(1).lt.0.45d0).and.(phi(1).gt.-0.65)) then
           bth(1)=0
           bq=0
          else if (phi(1).le.-0.65) then
           bth(1)=-0.15d0
          else if (phi(1).ge.0.45) then
           bth(1)=-0.3d0
          end if
         end if
         init=2
         noadmiss=2
         status='J'
         KnownApprox= 2        
c lineas aereas
        else if ((p.eq.0).and.((q.eq.1).or.(q.eq.0))
     &           .and.((d.eq.1).or.(d.eq.2))) then
         init = 2
         if (d .eq. 2) then
          if (mq .eq. 12) then
           if (-bth(1) .gt. -1.0d-1) then
            bth(1) = 1.0d-1
           end if
           if (-th(1) .gt. ((-5.0d0/9.0d0)*(-bth(1)+ONE))) then
            th(1) = 5.0d0 / 9.0d0 * (-bth(1)+ONE)
            q=1
           end if
          else if (mq .eq. 4) then
           if (-bth(1) .gt. -1.0d-1) then
            bth(1) = 1.0d-1
           end if
           if (-th(1) .gt. ((-3.0d0/11.0d0)*(-bth(1)+.1d0)+0.6d0)) then
            th(1) = 3.0d0/11.0d0*(-bth(1)+.1d0) + .6d0
            q=1
           end if
          end if
         end if
         if (abs(th(1)) .gt. ONE) then
          init = 1
         endif
         noadmiss = 2
         KnownApprox= 2
         if (bth(1) .lt. ZERO) then
          BQ=0
          KnownApprox= 1
         end if         
        end if
       end if
      else 
       noadmiss=-1
       do i=1,3
        th(i)=thBak(i)
        bth(i)=bthBak(i)
        phi(i)=phiBak(i)
        bphi(i)=bphibak(i)
        p=pBak
        dBak=d
        q=qBak
        bp=bpBak  
        bd=bdBak
        bq=bqBak
       enddo
c       if (html.eq.1) then
c luego ya pondremos un mensaje mas explicativo
c       write(nio,'(''<p>NOADMISS changed to -1</p>'')')
c      else 
c       write(nio,*)
c       write(nio,*) 'NOADMISS changed to -1'
c       end if
       KnownApprox=1
      end if
      end
      
