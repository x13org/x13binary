C     Last change:      Jan. 2021 change line 2165 lower case
C     previous change:      REG  29 Jun 2006, 26 May 2006
C     Previous change:  REG  21 Apr 2006, 28 Feb 2006, 30 Aug 2005
C     Previous change:  BCM  19 Jun 2002    5:38 pm
      subroutine HPPARAM(mq,hplan,hpPer,hpPar,hpth,km,kc,g,h)
C     IN mq
C     IN/OUT HPlan,HPper
C     OUT  HPpar:(0:HPLan and HPper by default; 1 HPper set by user;2 HPlan set by user)
C
C.. Implicits ..
      implicit none
      real*8 ZERO,ONE,TWO,ONEHND,MONE
      parameter (ZERO=0.0d0,ONE=1.0d0,TWO=2.0d0,ONEHND=100.0d0,
     &           MONE=-1.0d0)
      include 'units.cmn'
C
C.. Formal Arguments ..
      integer mq,HPpar
      real*8 hplan,hpPer,hpth(3),km,kc,g(3),h(4,5)
C
C.. Local Scalars ..
      integer alen,blen,clen,i,j,nmat,nsys
      real*8 a,b,m1,m2,n1,n2,r,s,sum,vb,z,freq,pi
      complex*16 r1,r2
C
C.. Local Arrays ..
      real*8 am(60,66),hmat(4,4),mat(3,4)
      complex*16 apol(2),bpol(2),c(3)
C
C.. External Functions ..
      complex*16 SELROOT
      external SELROOT
C
C.. External Calls ..
      external CONVC, MLTSOL
C
C.. Intrinsic Functions ..
      intrinsic DBLE, DCMPLX, SQRT,ACOS,COS
C
C.. Data Declarations ..
      data ((mat(i,j), j = 1,4), i = 1,3)/
     $     1.0d0,0.0d0,0.0d0,0.0d0,0.0d0,1.0d0,0.0d0,0.0d0,0.0d0,0.0d0,
     $     2.0d0,1.0d0/
C
      data ((hmat(i,j), j = 1,4), i = 1,4)/
     $     1.0d0,-2.0d0,1.0d0,0.0d0,0.0d0,1.0d0,-2.0d0,1.0d0,1.0d0,
     $     0.0d0,0.0d0,0.0d0,0.0d0,1.0d0,0.0d0,0.0d0/
C
C ... Executable Statements ...
C
C
      pi=acos(MONE)
      if (hpPer.ge.TWO) then
       HPpar=1  ! HPPER set by user
       freq=2*pi/hpPer
       hpLan=.25d0/((ONE-cos(freq))**2)
      else if (hplan.lt.0.0625) then
       HPpar=0  !HPper and HPlan by default
       hpPer=10*MQ  ! We choose the period of 10 Years
       freq=2*pi/hpPer
       hpLan=.25d0/((ONE-cos(freq))**2)
      else
       HPpar=2  !HPLAN set by user
       freq=acos(ONE-0.5d0/sqrt(hplan))
       hpPer=2*pi/freq
      end if
      a = TWO
      b = ONE / SQRT(hplan)
      s = 2 * a * b
      z = SQRT((ONE/(TWO*hplan))*(ONE+SQRT(ONE+16.0d0*hplan)))
      r = s / (TWO*z)
      m1 = (-a+r) / TWO
      n1 = (z-b) / TWO
      m2 = (-a-r) / TWO
      n2 = (-z-b) / TWO
      r1 = SELROOT(m1,n1,m2,n2)
      b = -b
      z = -z
      n1 = (z-b) / TWO
      n2 = (-z-b) / TWO
      r2 = SELROOT(m1,n1,m2,n2)
      apol(1) = DCMPLX(ONE,ZERO)
      apol(2) = r1
      bpol(1) = DCMPLX(ONE,ZERO)
      bpol(2) = r2
      alen = 2
      blen = 2
      clen = alen + blen - 1
      call CONVC(apol,alen,bpol,blen,c,clen)
      sum = ZERO
      do i = 1,clen
       hpth(i) = DBLE(c(i))
       sum = sum + hpth(i)*hpth(i)
      end do
      vb = (ONE+6.0d0*hplan) / sum
      km = ONE / vb
      kc = hplan / vb
      mat(2,1) = hpth(2)
      mat(3,1) = hpth(3) + hpth(3)
      mat(3,2) = hpth(2) + hpth(2)
      mat(2,2) = ONE + hpth(3)
      mat(2,3) = hpth(2)
      mat(1,3) = hpth(3)
      nsys = 1
      nmat = 3
      do i = 1,nmat
       do j = 1,nmat+nsys
        am(i,j) = mat(i,j)
       end do
      end do
*      WRITE(Mtprof,*)'  subroutine HPPARAM, call 1'
      call MLTSOL(am,nmat,nsys,60,66)
      do i = 1,nmat
       g(nmat-i+1) = am(i,4)
      end do
      hmat(3,2) = hpth(2)
      hmat(3,3) = hpth(3)
      hmat(4,3) = hpth(2)
      hmat(4,4) = hpth(3)
      do i = 1,4
       do j = 1,4
        h(i,j) = hmat(i,j)
       end do
      end do
      end
C
C
      complex*16 function SELROOT(m1,n1,m2,n2)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
C.. In/Out Status: Read, Not Written ..
      real*8 m1
C.. In/Out Status: Read, Not Written ..
      real*8 n1
C.. In/Out Status: Read, Not Written ..
      real*8 m2
C.. In/Out Status: Read, Not Written ..
      real*8 n2
C
C.. Local Scalars ..
      real*8 mod1,mod2
      complex*16 res
C
C.. Intrinsic Functions ..
      intrinsic DCMPLX
C
C ... Executable Statements ...
C
      mod1 = (m1*m1) + (n1*n1)
      mod2 = (m2*m2) + (n2*n2)
      if (mod1 .le. mod2) then
       res = DCMPLX(m1,n1)
      else
       res = DCMPLX(m2,n2)
      end if
      SELROOT = res
      end
C
C
      subroutine CONVC(a,alen,b,blen,c,clen)
C
C.. Implicits ..
      implicit none
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
C.. Formal Arguments ..
C.. In/Out Status: Read, Not Written ..
      integer alen
C.. In/Out Status: Read, Not Written ..
      integer blen
C.. In/Out Status: Read, Overwritten ..
      integer clen
C.. In/Out Status: Maybe Read, Not Written ..
      complex*16 a(alen)
C.. In/Out Status: Maybe Read, Not Written ..
      complex*16 b(blen)
C.. In/Out Status: Not Read, Maybe Written ..
      complex*16 c(clen)
C
C.. Local Scalars ..
      integer i,j,l,num
C
C.. Local Arrays ..
      complex*16 e(60)
C.. Intrinsic Functions ..
      intrinsic DCMPLX
C
C ... Executable Statements ...
C
      l = alen + blen - 1
      do i = 1,l
       e(i) = DCMPLX(ZERO,ZERO)
      end do
      do i = 1,alen
       do j = 1,blen
        num = i + j - 1
        e(num) = e(num) + a(i)*b(j)
       end do
      end do
      do i = 1,l
       c(i) = e(i)
      end do
      clen = l
      end
C
C
      subroutine HPTRCOMP(tr,nz,nf,hptrend,hpcycle,hpth,km,g,h)
c      Lamda:  (no used)
c      TR: the component to apply business cycle with NF forecast
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
      include 'units.cmn'
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
C.. Formal Arguments ..
      integer nz,nf
      real*8 tr(*),hptrend(mpkp),hpcycle(mpkp),hpth(3),km,g(3),h(4,5)
C
C.. Local Scalars ..
      integer i,j,lenx,nmat,nsys,lf,lenext
      real*8 wm
C
C.. Local Arrays ..
      real*8 am(60,66),exb(-3:mpkp),exf(-3:mpkp),
     $       y(mpkp),z(-1:mpkp)
      real*8 trend(-3:mpkp)
c
C
C.. External Calls ..
      external MLTSOL
C
C.. Intrinsic Functions ..
*      intrinsic LOG
C
C ... Executable Statements ...
C
      lenx = nz + nf
      lf=4
      call extendHP(tr,lenx,hpTH,lf,wm,trend)
c      if (lamda .eq. 1) then
c       do i = 1,len
c        trend(i) = tr(i)
c       end do
c      else
c       do i = 1,len
c        trend(i) = LOG(tr(i))
c       end do
c      end if
c      do i = 1,4
c       trend(1-i) = 2.0d0*trend(2-i) - trend(3-i)
c      end do
c      do i = 1,4
c       trend(len+i) = 2.0d0*trend(len+i-1) - trend(len+i-2)
c      end do
c
c
      lenext=lenx+2*lf-8
      do i = 1,lenext+2
       y(i) =  km * (g(1)*trend(i) + g(2)*trend(i+1) + g(3)*trend(i+2))
      end do
      h(1,5) = ZERO
      h(2,5) = ZERO
      h(3,5) = y(lenext+1)
      h(4,5) = y(lenext+2)
      nmat = 4
      nsys = 1
      do i = 1,nmat
       do j = 1,nmat+nsys
        am(i,j) = h(i,j)
       end do
      end do
*      WRITE(Mtprof,*)'  subroutine HPTRCOMP, call 1'
      call MLTSOL(am,nmat,nsys,60,66)
      do i = 1,4
       exf(lenext+i) = am(i,5)
      end do
      do i = 1,lenext
       j = lenext - i + 1
       exf(j) = -hpth(2)*exf(j+1) - hpth(3)*exf(j+2) + y(j)
      end do
      do j = -1,lenext+4
       z(j) =Km* (g(1)*trend(j) +g(2)*trend(j-1)+g(3)*trend(j-2))
      end do
      h(1,5) = wm
      h(2,5) = wm
      h(3,5) = z(0)
      h(4,5) = z(-1)
      nmat = 4
      nsys = 1
      do i = 1,nmat
       do j = 1,nmat+nsys
        am(i,j) = h(i,j)
       end do
      end do
*      WRITE(Mtprof,*)'  subroutine HPTRCOMP, call 2'
      call MLTSOL(am,nmat,nsys,60,66)
      exb(0) = am(1,5)
      exb(-1) = am(2,5)
      exb(-2) = am(3,5)
      exb(-3) = am(4,5)
      do i = 1,lenext+4
       exb(i) = -hpth(2)*exb(i-1) - hpth(3)*exb(i-2) + z(i)
      end do
      do i = 1,lenx
       hptrend(i) =(exf(i)+exb(i))
       hpcycle(i) = trend(i) - hptrend(i)
      end do
      end
C
C
C
c      Subroutine ErrorBcf
c      This subroutine is a more faster and direct way to obtain the variance of final error of BC
c      See that in this case Var final error BC=Var final error M
c      BUT SUPPOSE Error of Trend uncorrelated with error of extracting Business Cycle
c       and Error of Trend is correlated with error of estractiion BC from Trend
c      DO NOT USE this subroutine because THE SUPPOSITION OF FINAL ERROR uncorrelated is not correct
c      OUTPUT
c        VfBc: Var of final error of BC and M(long Term Trend)
c      INPUT
c         HPth: parte AR del modelo del filtro HP
c         Km: Variance of Long Term Trend innovation in units of Vp
c         Kc: Variance of Business Cycle innovation in units of Vp
c         Vp: variance of trend innovations
c         THETbc(1:nTHETbc): MA of Business Cycle component
c         PHIbc(1:nPHIbc): AR of Business Cycle Component
c         Vfp: var of final error of Trend
      subroutine ErrorBcF(HPth,Km,Kc,Vp,THETbc,nTHETbc,PHIbc,nPHIbc,
     $                  Vfp,VfBc)
      implicit none
      include 'component.i'
      include 'units.cmn'
c     INPUT PARAMETERS
      real*8 HPth(3),Km,Kc,Vp,THETbc(*),PHIbc(*),Vfp
      integer nTHETbc,nPHIbc
c     OUTPUT PARAMETERS
      real*8 Vfbc
c     LOCAL PARAMETERS
      real*8 Vfbcp,HP_PHIbc(MaxCompDim)
      integer nHP_PHIbc,i
      real*8 gam(0:1),rho(0:1),g(0:1)
      real*8 bHP_PHIbc(MaxCompDim),bTHETbc(MaxCompDim)
c     
      call CONV(HPth,3,PHIbc,nPHIbc,HP_PHIbc,nHP_PHIbc)
      DO i=1,nHP_PHIbc-1
        bHP_PHIbc(i)=-HP_PHIbc(i+1)
      endDO
      DO i=1,nTHETbc-1
        bTHETbc(i)=-THETbc(i+1)
      endDo
c      WRITE(Mtprof,*)'  subroutine ErrorBcF, call 1, nHP_PHIbc-1 =',
c     $               nHP_PHIbc-1
      call BFAC(bHP_PHIbc,bTHETbc,nHP_PHIbc-1,nTHETbc-1,0,
     $        gam,rho,Vfbcp,Km*Kc*Vp,g,0)
      Vfbc=Vfbcp+Vfp
      end    
c         
c
c
c     Subroutine RevErrorBc
c     Output:VrcM,VrcBc: the concurrent revision errors in units of Va
c            PSIEm(0:2pk+1): are the weights of the innovations for Long Term Trend Filter
c                        where PSIEm(pk+i) is the weight of the innovation B^i
c            PSIEbc(0:2pk+1):are the weights of the innovations for Business Cycle
c     INPUT as global variables of 'model.i'
c      PSI(nPSI),THETs(nTHETs) AR and MA of Seas component(no used if HPcycle>=3)
c      Cyc(nCyc),THETc(nTHETc) AR and MA of Transitory (no used if HPcycle>=2)
c      THstar(qstar0): MA of original serie
c     INPUT parameters:
c      HPcycle:(1: business Cycle extracted of Trend,
c               2: business Cycle extracted of SA,
c               3: business Cycle extracted of original serie)
c      varwns: innovations variance of Seas in units of Va  (no used if HPcycle>=3)
c      qt1: innovations variance of Irregular in units of Va (no used if HPcycle>=2)
c      varwnc: innovations variance of Transitory in units of Va(no used if HPcycle>=2)
c      d_bd: d+bd
c      pk: a constant to define the size of PSIEs
c      PHIm(nPHIm)  AR of Long Term Trend
c      THETm(nTHETm) MA of Long Term Trend
c      Vm: variance innovations of Long Term Trend in units of Va
c      PHIbc(nPHIbc) AR of Business Cycle
c      THETbc(nTHETbc) MA of business Cycle
c      Vbc: variance innovations of Business Cycle in units of Va 
      subroutine RevErrorBc(HpCycle,HPth,varwns,qt1,varwnc,d_bd,
     $                           pk,
     $                           PHIm,nPHIm,THETm,nTHETm,Vm,
     $                           PHIbc,nPHIbc,THETbc,nTHETbc,Vbc,
     $                           VrcM,VrcBc,PSIEm,PSIEbc)
      implicit none
      include 'component.i'
      include 'polynom.i'
      include 'stream.i'
c     INPUT
      include 'estb.i'
      include 'models.i'
      include 'units.cmn'
      real*8 ONE,MONE
      parameter (ONE=1.0d0,MONE=-1.0d0)
      integer HpCycle
      real*8 HPth(3)
      real*8 varwns,qt1,varwnc
      integer d_bd,pk
      real*8 PHIm(MaxCompDim),THETm(MaxCompDim),Vm,
     $       PHIbc(MaxCompDim),THETbc(MaxCompDim),Vbc
      integer nTHETm,nPHIm,nTHETbc,nPHIbc
c     OUTPUT
      real*8 VrcBc,VrcM,PSIEm(0:2*pk+1),PSIEbc(0:2*pk+1)
c     LOCAL VARIABLES
c        Components that added produce the complementary component
c               to component to which the HP filter is applied (nP)
      real*8 VSnP(MaxComp),ARnP(MaxComp,MaxCompDim),
     $    MAnP(MaxComp,MaxCompDim)
      integer ARnPDim(MaxComp),MAnPDim(MaxComp),nCompNp
c        model complementary to the component to which the HP filter is applied (nP)
      real*8 PHInP(MaxCompDim),THETnP(MaxCompDim),VnP
      integer nPHInP,nTHETnP
cc     Convolution(TH,HPth) and its Box-Jenkins representation (b*)
      real*8 TH_HPth(MaxCompDim),bTH_HPth(MaxCompDim-1)
      integer nTH_HPth
cc     Box-Jenkins representation of PHInp,PHIm,THETm
      real*8 bPHInP(MaxCompDim-1),bPHIm(MaxCompDim-1),
     $        bTHETm(MaxCompDim-1)
cc     Box-Jenkins representation of PHIbc,THETbc
      real*8 bPHIbc(MaxCompDim-1),bTHETbc(MaxCompDim-1)
cc     The concurrent revision error are Hm/(TH*HPth)arm  arm~niid(0,VrM) for M
      real*8 Hm(MaxCompDim),VrM,Em(0:maxCompDim)
      integer lHm,lEm
cc     The concurrent revision error are Hbc/(TH*HPth)arbc  arm~niid(0,VrBc) for Bc
      real*8 Hbc(MaxCompDim),VrBc,Ebc(0:maxCompDim)
      integer lHbc,lEbc
c        Local dummy variables to BFAC
      real*8 gam(0:1),rho(0:1),g(0:1),dvec(1)
c        Local dummy variables to DECFB
      real*8 Rce(0:12)
c
      real*8 delta(2),tmp(MaxCompDim)
      integer i,j,min_2_dbd,nTmp
cc    To check the exactness in getting components
      real*8 toterrNP
cc
ccc   For debugging purposes
c      character strNp*(MaxStrLength)
c
c      EXTERNAL ISTRLEN
c      integer ISTRLEN
c
      delta(1)=ONE
      delta(2)=MONE
      nCompNp=0
c
c     Step 1: getting the component complementary to P (to the component used to apply the HP filter)
c
      if (HPcycle.eq.1) then
c       call AddComp(CHI,nCHI,THETp,nTHETp,varwnp,         !Añadiendo esto tenemos la serie original en lugar de nP
c     $          ARnP,ARnPDim,MAnP,MAnPDim,VSnP,nCompNP)
        call AddComp(PSI,nPSI,THETs,nTHETs,varwns,
     $          ARnP,ARnPDim,MAnP,MAnPDim,VSnP,nCompNP)
        dvec(1)=ONE
        call AddComp(dvec,1,dvec,1,qt1,
     $          ARnP,ARnPDim,MAnP,MAnPDim,VSnP,nCompNP)
        call AddComp(Cyc,nCyc,THETc,nTHETc,varwnc,
     $          ARnP,ARnPDim,MAnP,MAnPDim,VSnP,nCompNP)
      else if (HPCycle.eq.2) then
        call AddComp(PSI,nPSI,THETs,nTHETs,varwns,
     $          ARnP,ARnPDim,MAnP,MAnPDim,VSnP,nCompNP)
      end if
c
c     Step 2: getting the component complementary to M and the complementary to BC
c
      call GetComp(ARnP,ARnPdim,MAnP,MAnPdim,VSnP,nCompNp,
     $                PHInP,nPHInP,THETnP,nTHETnP,VnP,toterrNp)
cc    For debugging purposes
c      write(nio,'(///,"Model nP computed with getComp")')
c      call ShowModel(PHInp,nPHInp,THETnP,nTHETnP,VnP,'nP',strnP)
c      write(nio,'(//,A)') strNP(1:ISTRLEN(StrNP))
c      write(nio,'(//,"TOTAL SQUARED ERROR NP = ",G10.4)') toterrNP
cc    End debugging block
c
c     Step 4: Obtaining the concurrent revision errors and innovation weights for M and BC
c
      call Conv(THSTR0,qstar0,HPth,3,TH_HPth,nTH_HPth)
      do i=1,nTH_HPth-1
        bTH_HPth(i)=-TH_HPth(i+1)
      endDo
      do i=1,nPHInP-1
        bPHInP(i)=-PHInP(i+1)
      endDo
      do i=1,nPHIm-1
        bPHIm(i)=-PHIm(i+1)
      enddo
      do i=1,nTHETm-1
        bTHETm(i)=-THETm(i+1)
      enddo
      call DECFB(bPHIm,bTH_HPth,nPHIm-1,nTH_HPth-1,
     $        bTHETm,bPHInP,nTHETm-1,nPHInP-1,Vm,
     $        PSIEm,pk,Rce,Hm,lHm,Vrm,Em,lEm)
c      WRITE(Mtprof,*)'  subroutine RevErrorBc, call 1, nTH_HPth-1 = ',
c     $        nTH_HPth-1
      call BFAC(bTH_HPth,Hm,nTH_HPth-1,lHm,
     $        1,gam,rho,VrcM,VrM,g,1)
      min_2_dbd=min(2,d_bd)
      DO i=1,min_2_dbd
        call CONV(PHInp,nPHInP,Delta,2,tmp,ntmp)
        DO j=1,ntmp
          PHInp(j)=tmp(j)
        enddo
        nPHInp=ntmp
      endDo
      DO i=1,nPHInP-1
        bPHInP(i)=-PHInP(i+1)
      endDo
      DO i=1,nPHIbc-1
        bPHIbc(i)=-PHIbc(i+1)
      endDo
      DO i=1,nTHETbc-1
        bTHETbc(i)=-THETbc(i+1)
      endDo
      call DECFB(bPHIbc,bTH_HPth,nPHIbc-1,nTH_HPth-1,
     $        bTHETbc,bPHInP,nTHETbc-1,nPHInP-1,Vbc,
     $        PSIEbc,pk,Rce,Hbc,lHbc,Vrbc,Ebc,lEbc)
c      WRITE(Mtprof,*)'  subroutine RevErrorBc, call 2, nTH_HPth-1 = ', 
c     $        nTH_HPth-1
      call BFAC(bTH_HPth,Hbc,nTH_HPth-1,lHbc,
     $        1,gam,rho,VrcBc,VrBc,g,1)
      end
c         
c
c
c     Subroutine GetErrorBc
c     Output:
c            VfcBc: variance of final error of Business Cycle in units of Va (0 if WithoutVf)
c            VfcM: variance of final error of Long Term Trend in units of Va (0 if WithoutVf)
c            VrcBc: variance of Revision error of Business Cycle for concurrent in units of Va
c            VrcM: variance of Revision error of Long Term Trend for concurrent in units of Va
c            PSIEm(0:2pk+1): are the weights of the innovations for Long Term Trend Filter
c                        where PSIEm(pk+i) is the weight of the innovation B^i
c            PSIEbc(0:2pk+1):are the weights of the innovations for Business Cycle
c            PHInp(1:nPHInp) the AR of the component complementary to P,SA or Series according to hpcycle
c     INPUT/OUTPUT
c            WithoutVf: 1 if variance of final error is infinite(d+bd>2 or ns>0, 
c                               or there are roots too close to 1)
c     INPUT as global variables of 'model.i'
c      PSI(nPSI),THETs(nTHETs) AR and MA of Seas component(no used if HPcycle>=3)
c      Cyc(nCyc),THETc(nTHETc) AR and MA of Transitory (no used if HPcycle>=2)
c      THstar(qstar): MA of original serie
c     INPUT parameters:
c      HPcycle:(1: business Cycle extracted of Trend,
c               2: business Cycle extracted of SA,
c               3: business Cycle extracted of original serie)
c      varwns: innovations variance of Seas in units of Va  (no used if HPcycle>=3)
c      qt1: innovations variance of Irregular in units of Va (no used if HPcycle>=2)
c      varwnc: innovations variance of Transitory in units of Va(no used if HPcycle>=2)
c      d_bd: d+bd
c      pk: a constant to define the size of PSIEs
c      PHIm(nPHIm)  AR of Long Term Trend
c      THETm(nTHETm) MA of Long Term Trend
c      Vm: variance innovations of Long Term Trend in units of Va
c      PHIbc(nPHIbc) AR of Business Cycle
c      THETbc(nTHETbc) MA of business Cycle
c      Vbc: variance innovations of Business Cycle in units of Va 
      subroutine getErrorBc(HpCycle,HPth,varwns,qt1,varwnc,d_bd,
     $                           pk,
     $                           PHIm,nPHIm,THETm,nTHETm,Vm,
     $                           PHIbc,nPHIbc,THETbc,nTHETbc,Vbc,
     $                           VfcM,VfcBc,VrcM,VrcBc,PSIEm,PSIEbc,
     $                           WithoutVf,PHInp,nPHInp)
      implicit none
      include 'component.i'
      include 'polynom.i'
      include 'stream.i'
c     INPUT
      include 'estb.i'
      include 'models.i'
      include 'units.cmn'
      real*8 ZERO,ONE,MONE
      parameter (ZERO=0.0d0,ONE=1.0d0,MONE=-1.0d0)
      integer HpCycle
      real*8 HPth(3)
      real*8 varwns,qt1,varwnc
      integer d_bd,pk
      real*8 PHIm(MaxCompDim),THETm(MaxCompDim),Vm,
     $       PHIbc(MaxCompDim),THETbc(MaxCompDim),Vbc
      integer nTHETm,nPHIm,nTHETbc,nPHIbc
c     OUTPUT
      real*8 VfcM,VfcBc,VrcBc,VrcM,PSIEm(0:2*pk+1),PSIEbc(0:2*pk+1)
      integer withoutVf
      real*8 PHInP(MaxCompDim)
      integer nPHInp
c     LOCAL VARIABLES
c        Components that added produce the complementary component
c               to component to which the HP filter is applied (nP)
      real*8 VSnP(MaxComp),ARnP(MaxComp,MaxCompDim),
     $    MAnP(MaxComp,MaxCompDim)
      integer ARnPDim(MaxComp),MAnPDim(MaxComp),nCompNp
c        Components that added produce the complementary to Business Cycle (nBc)
      real*8 VSnBc(MaxComp),ARnBc(MaxComp,MaxCompDim),
     $    MAnBc(MaxComp,MaxCompDim)
      integer ARnBcDim(MaxComp),MAnBcDim(MaxComp),nCompNbc
c        Components that added produce the complementary to Long Term Trend (nM)
      real*8 VSnM(MaxComp),ARnM(MaxComp,MaxCompDim),
     $    MAnM(MaxComp,MaxCompDim)
      integer ARnMdim(MaxComp),MAnMdim(MaxComp),nCompNm
c        model complementary to the component to which the HP filter is applied (nP)
      real*8 PHInpDelta(MaxCompDim),THETnP(MaxCompDim),VnP
      integer nPHInpDelta,nTHETnP
c        model complementary to Long Term Trend (nM)
      real*8 PHInM(MaxCompDim),THETnM(MaxCompDim),VnM
      integer nPHInM,nTHETnM
c        model complementary to Business Cycle (nBc)
      real*8 PHInBc(MaxCompDim),THETnBc(MaxCompDim),VnBc
      integer nPHInBc,nTHETnBc
cc     Convolution(THn,THnm) and its Box-Jenkins representation (b*)
      real*8 THmTHnm(2*MaxCompDim),bTHmTHnm(2*MaxCompDim-1)
      integer nTHmTHnm
cc     Convolution(TH,HPth) and its Box-Jenkins representation (b*)
      real*8 TH_HPth(MaxCompDim),bTH_HPth(MaxCompDim-1)
      integer nTH_HPth
cc     Convolution(TH,HPth,PHIbc) and its Box-Jenkins representation (b*)
      real*8 TH_HPth_PHIbc(2*MaxCompDim),bTH_HPth_PHIbc(2*MaxCompDim-1)
      integer nTH_HPth_PHIbc
cc     Box-Jenkins representation of PHInp,PHInpDelta,PHIm,THETm
      real*8 bPHInPDelta(MaxCompDim-1),bPHInP(MaxCompDim-1),
     $       bPHIm(MaxCompDim-1),bTHETm(MaxCompDim-1)
cc     Box-Jenkins representation of PHIbc,THETbc
      real*8 bPHIbc(MaxCompDim-1),bTHETbc(MaxCompDim-1)
cc     The concurrent revision error are Hm/(TH*HPth)arm  arm~niid(0,VrM) for M
      real*8 Hm(MaxCompDim),VrM,Em(0:MaxCompDim)
      integer lHm,lEm
cc     The concurrent revision error are Hbc/(TH*HPth)arbc  arm~niid(0,VrBc) for Bc
      real*8 Hbc(MaxCompDim),VrBc,Ebc(0:maxCompDim)
      integer lHbc,lEbc
c        Local dummy variables to BFAC
      real*8 gam(0:1),rho(0:1),g(0:1)
c        Local dummy variables to DECFB
      real*8 Rce(0:12)
c
      real*8 delta(2),tmp(MaxCompDim),dvec(1)
      integer i,j,min_2_dbd,nTmp
cc    To check the exactness in getting components
c      real*8 toterrTest
      real*8 toterrNP,toterrNM,toterrNBC
cc
ccc   For debugging purposes
cccc  Representation of nBc and nM
c      character strNp*(MaxStrLength),strNbc*(MaxStrLength),
c     $        strnM*(MaxStrLength)
cccc  Testing the complementary components
c      real*8 VStest(MaxComp),ARtest(MaxComp,MaxCompDim),
c     $        MAtest(MaxComp,MaxCompDim)
c     integer ARtestDim(MAxComp),MAtestDim(MaxComp),nTestComp
c     real*8 PHItest(MaxCompDim),THtest(MAxCompDim),Vtest
c     integer nPHItest,nTHtest
c     character StrTest*MaxStrLength
ccc   End declarations for debugging purposes
c
      EXTERNAL ISTRLEN
      integer ISTRLEN
c
      delta(1)=ONE
      delta(2)=MONE
      nCompNbc=0
      nCompNm=0
      nCompNp=0
c
c     Step 1: getting the component complementary to P (to the component used to apply the HP filter)
c
      if (HPcycle.eq.1) then
c       call AddComp(CHI,nCHI,THETp,nTHETp,varwnp,         !Añadiendo esto tenemos la serie original en lugar de nP
c     $          ARnP,ARnPDim,MAnP,MAnPDim,VSnP,nCompNP)
        call AddComp(PSI,nPSI,THETs,nTHETs,varwns,
     $          ARnP,ARnPDim,MAnP,MAnPDim,VSnP,nCompNP)
        dvec(1)=ONE
        call AddComp(dvec,1,dvec,1,qt1,
     $          ARnP,ARnPDim,MAnP,MAnPDim,VSnP,nCompNP)
        call AddComp(Cyc,nCyc,THETc,nTHETc,varwnc,
     $          ARnP,ARnPDim,MAnP,MAnPDim,VSnP,nCompNP)
      else if (HPCycle.eq.2) then
        call AddComp(PSI,nPSI,THETs,nTHETs,varwns,
     $          ARnP,ARnPDim,MAnP,MAnPDim,VSnP,nCompNP)
      end if
c
c     Step 2: getting the component complementary to M and the complementary to BC
c
      call CopyAddComp(ARnP,ARnPDim,MAnP,MAnPDim,VSnP,nCompNp,
     $          ARnM,ARnMdim,MAnM,MAnMdim,VSnM,nCompNm)
      call CopyAddComp(ARnP,ARnPDim,MAnP,MAnPDim,VSnP,nCompNp,
     $          ARnBc,ARnBcDim,MAnBc,MAnBcDim,VSnBc,nCompNbc)
      call AddComp(PHIm,nPHIm,THETm,nTHETm,Vm,
     $          ARnBc,ARnBcDim,MAnBc,MAnBcDim,VSnBc,nCompNbc)
      call AddComp(PHIbc,nPHIbc,THETbc,nTHETbc,Vbc,
     $          ARnM,ARnMdim,MAnM,MAnMdim,VSnM,nCompNm)
      call GetComp(ARnP,ARnPdim,MAnP,MAnPdim,VSnP,nCompNp,
     $                PHInP,nPHInP,THETnP,nTHETnP,VnP,toterrNp)
      call GetComp(ARnBc,ARnBcDim,MAnBc,MAnBcDim,VSnBc,nCompNbc,
     $          PHInBc,nPHInBc,THETnBc,nTHETnBc,VnBc,toterrNBC)
      call GetComp(ARnM,ARnMdim,MAnM,MAnMdim,VSnm,nCompnM,
     $          PHInM,nPHInM,THETnM,nTHETnM,VnM,toterrNM)
cc    For debugging purposes
c      write(nio,'(///,"Model nP computed with getComp")')
c      call ShowModel(PHInp,nPHInp,THETnP,nTHETnP,VnP,'nP',strnP)
c     write(nio,'(//,A)') strNP(1:ISTRLEN(StrNP))
c     write(nio,'(//,"TOTAL SQUARED ERROR NP = ",G10.4)') toterrNP
c      write(nio,'(//,"Componentes complementarios al BC", 
c     $            " y Long Term Trend para depuracion")')
c      call ShowModel(PHInm,nPHInm,THETnM,nTHETnM,VnM,'nM',strnM)
c     write(nio,'(//,A)') strNM(1:ISTRLEN(StrNM))
c     write(nio,'(//,"TOTAL SQUARED ERROR NM = ",G10.4)') toterrNM
c      call ShowModel(PHInBc,nPHInBc,THETnBc,nTHETnBc,VnBc,'nBc',strnBc)
c     write(nio,'(//,A,///)') strNBc(1:ISTRLEN(StrNBc))
c     write(nio,'(//,"TOTAL SQUARED ERROR NBc = ",G10.4)') toterrNBc
cc    Testing nM and nBc
c      nTestComp=0
c     call AddComp(PHIm,nPHIm,THETm,nTHETm,Vm,
c     $           ARtest,ARtestDim,MAtest,MAtestDim,VStest,nTestComp)
c     call AddComp(PHInm,nPHInm,THETnm,nTHETnm,Vnm,
c     $           ARtest,ARtestDim,MAtest,MAtestDim,VStest,nTestComp)
c      call GetComp(ARtest,ARtestDim,MAtest,MAtestDim,VStest,nTestComp,
c     $      PHItest,nPHItest,THtest,nTHtest,Vtest,toterrTest)
c      call ShowModel(PHItest,nPHItest,THtest,nTHtest,Vtest,
c     $      'M+nM',strTest)
c     write(nio,'(//,A)') strTest(1:ISTRLEN(StrTest))
c      write(nio,'(//,"Should be 1 Vtest(M)=",G10.4)') Vtest
c      nTestComp=0
c     call AddComp(PHIbc,nPHIbc,THETbc,nTHETbc,Vbc,
c     $      ARtest,ARtestDim,MAtest,MAtestDim,VStest,nTestComp)
c     call AddComp(PHInbc,nPHInbc,THETnbc,nTHETnbc,Vnbc,
c     $      ARtest,ARtestDim,MAtest,MAtestDim,VStest,nTestComp)
c      call GetComp(ARtest,ARtestDim,MAtest,MAtestDim,VStest,nTestComp,
c     $      PHItest,nPHItest,THtest,nTHtest,Vtest,toterrTest)
c      call ShowModel(PHItest,nPHItest,THtest,nTHtest,Vtest,
c     $      'Bc+nBc',strTest)
c     write(nio,'(//,A)') strTest(1:ISTRLEN(StrTest))
c      write(nio,'(/,"Should be 1 Vtest(BC)=",G10.4,///)') Vtest
cc    End debugging Block
c
c     Step 3: Obtaining the final error of M and BC
c
      call Conv(THSTR0,qstar0,HPth,3,TH_HPth,nTH_HPth)
      if (withoutVf.eq.0) then
        call Conv(THETm,nTHETm,THETnM,nTHETnM,THmTHnm,nTHmTHnm)
        call Conv(TH_HPth,nTH_HPth,PHIbc,nPHIbc,
     $        TH_HPth_PHIbc,nTH_HPth_PHIbc)
        do i=1,nTH_HPth_PHIbc-1
          bTH_HPth_PHIbc(i)=-TH_HPth_PHIbc(i+1)
        enddo
        do i=1,nTHmTHnm-1
          bTHmTHnm(i)=-THmTHnm(i+1)
        enddo
c      WRITE(Mtprof,*)'  subroutine getErrorBc, call 1, ', 
c     $               'nTH_HPth_PHIbc-1 = ',nTH_HPth_PHIbc-1 
        call BFAC(bTH_HPth_PHIbc,bTHmTHnm,nTH_HPth_PHIbc-1,nTHmTHnm-1,
     $      0,gam,rho,VfcM,Vm*Vnm,g,0)
        call Conv(THETbc,nTHETbc,THETnBc,nTHETnBc,THmTHnm,nTHmTHnm)
        do i=1,nTHmTHnm-1
          bTHmTHnm(i)=-THmTHnm(i+1)
        enddo
c      WRITE(Mtprof,*)'  subroutine getErrorBc, call 2, ', 
c     $               'nTH_HPth_PHIbc-1 = ',nTH_HPth_PHIbc-1 
        call BFAC(bTH_HPth_PHIbc,bTHmTHnm,nTH_HPth_PHIbc-1,nTHmTHnm-1,0,
     $        gam,rho,VfcBc,Vbc*Vnbc,g,0)
        if ((VfcM.lt.ZERO).or.(VfcBc.lt.ZERO)) then
          withoutVf=2
        end if
      end if
      if (withoutVf.ne.0) then
        VfcM=ZERO
        VfcBc=ZERO
      end if
c
c     Step 4: Obtaining the concurrent revision errors and innovation weights for M and BC
c
      do i=1,nTH_HPth-1
        bTH_HPth(i)=-TH_HPth(i+1)
      endDo
      do i=1,nPHInP-1
        bPHInP(i)=-PHInP(i+1)
      endDo
      do i=1,nPHIm-1
        bPHIm(i)=-PHIm(i+1)
      enddo
      do i=1,nTHETm-1
        bTHETm(i)=-THETm(i+1)
      enddo
      call DECFB(bPHIm,bTH_HPth,nPHIm-1,nTH_HPth-1,
     $        bTHETm,bPHInP,nTHETm-1,nPHInP-1,Vm,
     $        PSIEm,pk,Rce,Hm,lHm,Vrm,Em,lEm)
c      WRITE(Mtprof,*)'  subroutine getErrorBc, call 3, nTH_HPth-1 =', 
c     $               nTH_HPth-1
      call BFAC(bTH_HPth,Hm,nTH_HPth-1,lHm,
     $        1,gam,rho,VrcM,VrM,g,1)
      min_2_dbd=min(2,d_bd)
      Do i=1,nPHInp
        PHInpDelta(i)=PHInp(i)
      enddo
      nPHInpDelta=nPHInp
      DO i=1,min_2_dbd
        call CONV(PHInpDelta,nPHInpDelta,Delta,2,tmp,ntmp)
        DO j=1,ntmp
          PHInpDelta(j)=tmp(j)
        enddo
        nPHInpDelta=ntmp
      endDo
      DO i=1,nPHInPDelta-1
        bPHInpDelta(i)=-PHInPDelta(i+1)
      endDo
      DO i=1,nPHIbc-1
        bPHIbc(i)=-PHIbc(i+1)
      endDo
      DO i=1,nTHETbc-1
        bTHETbc(i)=-THETbc(i+1)
      endDo
      call DECFB(bPHIbc,bTH_HPth,nPHIbc-1,nTH_HPth-1,
     $        bTHETbc,bPHInpDelta,nTHETbc-1,nPHInpDelta-1,Vbc,
     $        PSIEbc,pk,Rce,Hbc,lHbc,Vrbc,Ebc,lEbc)
c      WRITE(Mtprof,*)'  subroutine getErrorBc, call 4, nTH_HPth-1=',
c     $               nTH_HPth-1
      call BFAC(bTH_HPth,Hbc,nTH_HPth-1,lHbc,
     $        1,gam,rho,VrcBc,VrBc,g,1)
      end
c
c
      subroutine getBcycleComp(d_bd,mq,nS,
     $                    PHIp,nPHIp,PHIps,nPHIps,THETp,nTHETp,Vp,
     $                    HPth,Km,Kc,
     $                    PHIbc,nPHIbc,THETbc,nTHETbc,Vbc,
     $                    PHIm,nPHIm,THETm,nTHETm,Vm,WithoutVf)
c     Given THhp, and the model of the component (P) to which 
c      we apply the HP filter (Km/ACF(HPth)) and (kc*ACF((1-B)^2)/ACF(HPth))
c     we obtain the models of Long Term Trend (M) and Business Cycle (Bc)
c     Model of P:   PHIps(B)*(S(mq)^ns)*(1-B)^(d_bd) P= THETp(B)Apt   Apt~niid(0,Vp)
c                   where S(mq)=ones(1,mq)
c     OTHER INPUT/OUTPUT
c            WithoutVf: 1 if variance of final error is infinite(d+bd>2 or ns>0, 
c                               or there are roots too close to 1)
      implicit none
      include 'component.i'
      real*8 ZERO,ONE,MONE
      parameter (ZERO=0.0d0,ONE=1.0d0,MONE=-1.0d0)
c     INPUT/OUTPUT
      integer withoutVf
c     INPUT
      integer d_bd,ns,mq     
      real*8 PHIp(*),PHIps(*),THETp(*),Vp,HPth(3),Kc,Km
      integer nPHIp,nPHIps,nTHETp
c     OUTPUT
      real*8 PHIbc(MaxCompDim),THETbc(MaxCompDim),Vbc
      integer nPHIbc,nTHETbc
      real*8 PHIm(MaxCompDim),THETm(MaxCompDim),Vm
      integer nPHIm,nTHETm
c     LOCAL PARAMETERS
      real*8 Delta(2),S(12),tmp(MaxCompDim)
      integer i,j,ntmp
c
      logical dpeq
      external dpeq
c
      if ((d_bd.gt.2).or.(ns.ne.0))then 
        withoutVf=1
      else
        withoutVf=0
      end if
      call CONV(PHIp,nPHIp,HPth,3,PHIm,nPHIm)
      DO i=1,nTHETp
        THETbc(i)=THETp(i)
        THETm(i)=THETp(i)
      endDo
      nTHETbc=nTHETp
      do while(dpeq(THETbc(nTHETbc),ZERO))
        nTHETbc=nTHETbc-1
      enddo
      nTHETm=nTHETbc
      call CONV(PHIps,nPHIps,HPth,3,PHIbc,nPHIbc)
      Delta(1)=ONE
      Delta(2)=MONE
      if (d_bd.ge.2) then
        do i=1,(d_bd-2)
          call CONV(PHIbc,nPHIbc,Delta,2,tmp,ntmp)
          Do j=1,ntmp
            PHIbc(j)=tmp(j)
          EndDo
          nPHIbc=ntmp
        endDO
      else
        do i=1,(2-d_bd)
          call CONV(THETbc,nTHETbc,Delta,2,tmp,ntmp)
          DO j=1,ntmp
            THETbc(j)=tmp(j)
          endDo
          nTHETbc=ntmp
        endDo
      end if
      if (ns.ge.1) then
        Do i=1,mq
          S(i)=ONE
        endDo
        Do i=1,ns
          call CONV(PHIbc,nPHIbc,S,mq,tmp,ntmp)
          Do j=1,ntmp
            PHIbc(j)=tmp(j)
          endDo
          nPHIbc=ntmp
        endDo
      end if
      Vbc=Kc*Vp
      Vm=Km*Vp
      end
c
      subroutine HPOUTPUT(lamda,compHP,hptrend,hpcyc,hpregt,hpregc,
     $   totcyc,ireg,nfor,out,ndec,HPper,HPlam,HPpar,HPcycle,km,HPth,
     $   varw,VfcBc,VfcM,VfBc,WithoutVf,seBc,seM,MQ,DBD)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
      integer pk
      parameter (pk = 550)
      real*8 ZERO,ONEHND,MONE
      parameter (ZERO=0.0d0,ONEHND=100.0d0,MONE=-1.0d0)
C
C.. Formal Arguments ..
      real*8 VfcBc,VfcM,VfBc,seBc(2*pk+2),seM(2*pk+2)
      integer WithoutVf,DBD,HPpar
      integer lamda,ireg,nfor,out,ndec,pg,HPcycle,iter,MQ
      real*8 compHP(mpkp),hptrend(mpkp),hpcyc(mpkp),hpregt(mpkp),
     $       hpregc(mpkp),totcyc(mpkp),HPper,HPlam,Km,HPth(1:3),
     $       varw
C
C.. Local Scalars ..
c      integer j
c      character fname*30
c      real*8 splot(2*kp+1,3)
      integer i,nf,thisDate(2),nLongTermCad,nidCad,baseCode
      character subtitle*50,LongTermCad*50,idCad*16
      real*8 kons,sum0,sum1
C
C.. Local Arrays ..
      real*8 temp(mpkp)
C
C.. External Calls ..
c      integer ISTRLEN
c      external ISTRLEN
C
C.. Intrinsic Functions ..
      intrinsic EXP
      include 'sform.i'
      include 'stream.i'
      include 'models.i'
      include 'polynom.i'
      character ModelStrCt*(MaxStrLength),ModelStrMt*(maxStrLength)
C
C ... Executable Statements ...
C
C
C
      LongTermCad=' '
      idCad=' '
      If (HPcycle.eq.1) then
        LongTermCad='LONG TERM TREND'
        nLongTermCad=15
        idCad='long.trend'
        nidCad=10
        baseCode=2500
      else if (HPcycle.eq.2) then
        LongTermCad='SA series without Business Cycle'
        nLongTermCad=32
        idCad='sa.wo.bcycle'
        nidCad=12
        baseCode=2600
      else
        LongTermCad='Series without Business Cycle'
        nLongTermCad=29
        idCad='series.wo.bcycle'
        nidCad=16
        baseCode=2700
      end if
      thisDate(1) = Nyer
      thisDate(2) = Nper
      call PresentaHP(HPth,HPcycle,Km,HPlam,varw,
     $         ModelStrCt,ModelStrMt)
c      nf = nfor / 2
      nf = nfor 
      if (lamda .eq. 0) then
       sum0 = ZERO
       sum1 = ZERO
       do i = 1,Nz+nf
        sum0 = sum0 + compHP(i)
        sum1 = sum1 + Exp(hptrend(i))
       end do
       kons = sum0 / sum1
      end if
      if (out.eq.0) then
        call OutHeadHP(ModelStrCt,ModelStrMt,HPth,Km,HPper,HPlam,
     $           HPpar,HPcycle,VfcBc,VfcM,VfBc,WithoutVf,MQ,DBD,varw)
      end if
*      if ((pg .eq. 0).and.(iter.eq.0).and.(out.lt.2)) then
*       if (lamda .eq. 1) then
*        if (ireg .eq. 1) then
*         fname = 'HPcBCs.T'
*         subtitle = 'STOCHASTIC BUSSINES CYCLE'
*         call PLOTSERIESCI(fname,subtitle,hpcyc,seBc,Nz,1,-666.0d0)
*c
*         fname = 'HPcBCr.T' 
*         subtitle = 'REGRESSION CYCLICAL COMPONENT'
*         call PLOTSERIES(fname,subtitle,hpregc,Nz,1,0.0d0)
*c         
*         fname = 'HPcBCt.T'   
*         subtitle = 'TOTAL BUSSINES CYCLE'
*         do i = 1,Nz
*          temp(i) = hpcyc(i) + hpregc(i)
*         end do
*         call PLOTSERIES(fname,subtitle,temp,Nz,1,0.0d0)
*c
*         fname = 'HPcLTs.T'
*         subtitle = 'STOCHASTIC '//LongTermCad(1:istrlen(LongTermCad))
*         call PLOTSERIESCI(fname,subtitle,hptrend,seM,Nz,1,-666.0d0)
*c
*         fname='HPcLTr.T'
*         subtitle = 'REGRESSION '//LongTermCad(1:istrlen(LongTermCad))
*         call PLOTSERIES(fname,subtitle,hpregt,Nz,1,0.0d0)
*c
*         fname='HPcLTt.T'  
*         subtitle = 'TOTAL '//LongTermCad(1:istrlen(LongTermCad))
*         do i = 1,Nz
*          temp(i) = hptrend(i) + hpregt(i)
*         end do
*         call PLOTSERIES(fname,subtitle,temp,Nz,1,0.0d0)
*c                LAM=1 IREG=0        
*        ELSE
*c
*         fname = 'HPcBCt.T'
*         subtitle = 'BUSINESS CYCLE'
*         call PLOTSERIESCI(fname,subtitle,hpcyc,seBc,Nz,1,-666.0d0)        
*C        
*         fname = 'HPcLTt.T'
*         subtitle = LongTermCad(1:istrlen(LongTermCad))
*         call PLOTSERIESCI(fname,subtitle,hptrend,seM,Nz,1,-666.0d0)
*        end if
*c            LAM=0 IREG>0    
*       else if (ireg .eq. 1) then
*c                  logs          
*        fname='HPcBCs.T'
*        subtitle = 'STOCHASTIC CYCLICAL COMPONENT'
*        call PLOTLSERIES(fname,subtitle,hpcyc,Nz,1,0.0d0)
*c
*        fname='HPcBCr.T'
*        subtitle = 'REGRESSION CYCLICAL COMPONENT'
*        call PLOTLSERIES(fname,subtitle,hpregc,Nz,1,0.0d0)
*c        
*        fname='HPcBCt.T'
*        subtitle = 'TOTAL CYCLICAL COMPONENT'
*        do i = 1,Nz
*         temp(i) = hpcyc(i) + hpregc(i)
*        end do
*        call PLOTLSERIES(fname,subtitle,temp,Nz,1,0.0d0)
*c
*        fname='HPcLTsLO.T' 
*        subtitle = 'STOCHASTIC '//LongTermCad(1:istrlen(LongTermCad))
*        call PLOTLSERIES(fname,subtitle,hptrend,Nz,1,0.0d0)
*c
*        fname = 'HPcLTrLO.T' 
*        subtitle = 'REGRESSION '//LongTermCad(1:istrlen(LongTermCad))
*        call PLOTLSERIES(fname,subtitle,hpregt,Nz,1,0.0d0)
*c
*        do i = 1,Nz
*         temp(i) = hptrend(i) + hpregt(i)
*        end do
*        fname = 'HPcLTtLO.T' 
*        subtitle = 'TOTAL '//LongTermCad(1:istrlen(LongTermCad))
*        call PLOTLSERIES(fname,subtitle,temp,Nz,1,0.0d0)
*c                  levels
*c
*        fname = 'HPfBCs.T'
*        subtitle = 'STOCHASTIC BUSINESS CYCLE FACTORS'       
*        do i=1,nz
*         temp(i)=100.0d0 * (compHP(i)/(kons*EXP(hptrend(i))))
*        end do
*        call PLOTSERIESCI(fname,subtitle,temp,seBc,Nz,1,-666.0d0)
*c
*        fname='HPfBCr.T'
*        subtitle = 'REGRESSION CYCLICAL FACTOR'
*        do i = 1,Nz
*         temp(i) = 100.0d0 * EXP(hpregc(i))
*        end do
*        call PLOTSERIES(fname,subtitle,temp,Nz,1,0.0d0)
*c
*        fname='HPfBCt.T'
*        subtitle = 'TOTAL CYCLICAL FACTOR'
*        do i = 1,Nz
*         temp(i) =
*     $     100.0d0 * (compHP(i)/(kons*EXP(hptrend(i)))) * exp(hpregc(i))
*        end do
*        call PLOTSERIES(fname,subtitle,temp,Nz,1,0.0d0)
*c
*c
*        fname = 'HPcLTsLE.T'
*        subtitle = 'STOCHASTIC '//LongTermCad(1:istrlen(LongTermCad))
*        do i = 1,Nz
*         temp(i) = kons * EXP(hptrend(i))
*        end do
*        call PLOTSERIES(fname,subtitle,temp,Nz,1,0.0d0)
*c      
*        fname = 'HPcLTrLE.T' 
*        subtitle = 'REGRESSION '//LongTermCad(1:istrlen(LongTermCad))
*        do i = 1,Nz
*         temp(i) = EXP(hpregt(i))
*        end do
*        call PLOTSERIES(fname,subtitle,temp,Nz,1,0.0d0)
*        do i = 1,Nz
*         temp(i) = kons * EXP(hptrend(i)) * (hpregt(i))
*        end do
*        fname = 'HPcLTtLE.T' 
*        subtitle = 'TOTAL '//LongTermCad(1:istrlen(LongTermCad))
*        call PLOTSERIES(fname,subtitle,temp,Nz,1,0.0d0)
*c
*c               LAM=0 IREG=0           
*       else
*c
*cc      fname='HPcBCs.T'
*        fname='HPcBCt.T'
*        subtitle = 'BUSINESS CYCLE'
*        call PLOTLSERIES(fname,subtitle,hpcyc,Nz,1,0.0d0)
*c
*c        fname = 'HPfBCs.T'
*        fname = 'HPfBCt.T'
*        subtitle = ' BUSINESS CYCLE FACTORS'        
*        do i = 1,Nz
*         temp(i) = 100.0d0 * (compHP(i)/(kons*EXP(hptrend(i))))
*        end do
*        call PLOTSERIESCI(fname,subtitle,temp,seBc,Nz,1,-666.0d0)
*c
*cc        fname='HPcLTsLO.T'      
*        fname='HPcLTtLO.T'      
*        subtitle = LongTermCad(1:istrlen(LongTermCad))//' COMPONENT'
*        call PLOTLSERIES(fname,subtitle,hptrend,Nz,1,0.0d0)
*c
*c        fname = 'HPcLTsLE.T'
*        fname = 'HPcLTtLE.T'
*        subtitle = LongTermCad(1:istrlen(LongTermCad))        
*        do i = 1,Nz
*         temp(i) = kons * EXP(hptrend(i))
*        end do
*        call PLOTSERIES(fname,subtitle,temp,Nz,1,0.0d0)
*       end if
*c
*c  FORECAST
*       do i = 1,2*kp+1
*        do j = 1,3
*         splot(i,j) = 0.0d0
*        end do
*       end do
*       if (lamda.eq.0) then
*        do i = kp-nf,kp+nf
*         splot(i,3) = exp(hptrend(nz-kp+i))*kons
*c         splot(i,3) = exp(hptrend(nz-kp+i))
*        end do
*       else
*        do i = kp-nf,kp+nf
*         splot(i,3) = hptrend(nz-kp+i)
*        end do
*       end if
*       do i = -nf,nf
*        splot(kp+i,1) = splot(kp+i,3) - 1.96*seM(Nz+i)
*        splot(kp+i,2) = splot(kp+i,3) + 1.96*seM(Nz+i)
*       end do
*       fname = 'LTTFCI.T5'
*       subtitle = LongTermCad(1:istrlen(LongTermCad))//
*     $      ' Forecast with Confidence Intervals'
*       call PLOTFCAST2(fname,subtitle,splot,nf,nz,1)
*       if (lamda.eq.0) then
*        do i = kp-nf,kp+nf
*c          splot(i,3) = 100*exp(hpcyc(nz-kp+i))
*         splot(i,3)=100.0d0*(compHP(nz-kp+i)
*     $                      /(kons*EXP(hptrend(nz-kp+i))))
*        end do
*        subtitle=
*     $      'BUSINESS CYCLE FACTORS Forecast with Confidence Intervals'
*       else
*        do i = kp-nf,kp+nf
*          splot(i,3) = hpcyc(nz-kp+i)
*        end do
*        subtitle = 'BUSINESS CYCLE Forecast with Confidence Intervals'
*       end if
*       do i = -nf,nf
*        splot(kp+i,1) = splot(kp+i,3) - 1.96*seBc(Nz+i)
*        splot(kp+i,2) = splot(kp+i,3) + 1.96*seBc(Nz+i)
*       end do
*       fname = 'BCFCI.T5'
*       call PLOTFCAST2(fname,subtitle,splot,nf,nz,1)
*cc
*      end if
C
C OUTPUT FILE
C
c      write(*,*)' out = ',out,' print out long term trend'
c      write(*,*)' ireg = ',ireg
      if (out.eq.0) then  
c      write(*,*)' lamda = ',lamda,' print out long term trend'
       if (lamda .eq. 1) then
        if (ireg .eq. 1) then
C
C CYCLE
C
         do i = 1,Nz+nf
          temp(i) = hpcyc(i) + hpregc(i)
          totcyc(i) = hpcyc(i) + hpregc(i)
         end do
         subtitle(1:29)='STOCHASTIC CYCLICAL COMPONENT'
         CALL genSkip(1104)
         CALL prttbl1(thisDate,Nfreq,hpcyc,Nz,subtitle,29,ndec,nf,
     $                'stochastic.cyclical.component')
         subtitle(1:29)='REGRESSION CYCLICAL COMPONENT'
         CALL genSkip(1105)
         CALL prttbl1(thisDate,Nfreq,hpregc,Nz,subtitle,29,ndec,nf,
     $                'regression.cyclical.component')
         subtitle(1:24)='TOTAL CYCLICAL COMPONENT'
         CALL genSkip(1106)
         CALL prttbl1(thisDate,Nfreq,temp,Nz,subtitle,24,ndec,nf,
     $                'total.cyclical.component')
         call USRENTRY(temp,1,Nz+nf,1,mpkp,2501)
         CALL genSkip(1107)
         if (WithoutVf.ne.0) then
          subtitle(1:36)='Revision error of CYCLICAL COMPONENT'
          CALL prttbl1(thisDate,Nfreq,seBc,Nz,subtitle,24,ndec,nf,
     $                 'cyclical.component.revision.error')
         else
c           call HTMLTABLE1('Total error of CYCLICAL COMPONENT',
          subtitle(1:36)='Revision error of CYCLICAL COMPONENT'
          CALL prttbl1(thisDate,Nfreq,seBc,Nz,subtitle,24,ndec,nf,
     $                 'cyclical.component.revision.error')
         end if
C
C LONG TERM TREND
C
c         write(*,*) ' stochastic long trem trend printed out'
         CALL genSkip(baseCode+1)
         subtitle(1:11+nLongTermCad)='STOCHASTIC '//
     $                longTermCad(1:nLongTermCad)
         CALL prttbl1(thisDate,Nfreq,hptrend,Nz,subtitle,
     $                11+nLongTermCad,ndec,nf,
     $                'stochastic.'//idCad(1:nidCad))
c         write(*,*) ' regression long trem trend printed out'
         CALL genSkip(baseCode+2)
         subtitle(1:11+nLongTermCad)='REGRESSION '//
     $                longTermCad(1:nLongTermCad)
         CALL prttbl1(thisDate,Nfreq,hpregt,Nz,subtitle,11+nLongTermCad,
     &                ndec,nf,'regression.'//idCad(1:nidCad))
         do i = 1,Nz+nf
           temp(i) = hptrend(i) + hpregt(i)
         end do
         CALL genSkip(baseCode+3)
         subtitle(1:6+nLongTermCad)='TOTAL '//
     $                longTermCad(1:nLongTermCad)
         CALL prttbl1(thisDate,Nfreq,temp,Nz,subtitle,6+nLongTermCad,
     $                ndec,nf,'total.'//idCad(1:nidCad))
         call USRENTRY(temp,1,Nz+nf,1,mpkp,2502)
c         write(*,*) '  long trem trend saved'
         CALL genSkip(baseCode+4)
         subtitle(1:18+nLongTermCad)='Revision error of '//
     $                longTermCad(1:nLongTermCad)
         if (WithoutVf.ne.0) then
           CALL prttbl1(thisDate,Nfreq,seM,Nz,subtitle,18+nLongTermCad,
     $                  ndec,nf,idCad(1:nidCad)//'.revision.error')
         else
c           call HTMLTABLE1('Total error of LONG TERM TREND',
           CALL prttbl1(thisDate,Nfreq,seM,Nz,subtitle,18+nLongTermCad,
     $                  ndec,nf,idCad(1:nidCad)//'.revision.error')
c            Because we pass fee=0 to SErrorF
         end if
        else
C
C CYCLE
C
         do i = 1,Nz
          totcyc(i) = hpcyc(i)
         end do
         subtitle(1:18)='CYCLICAL COMPONENT'
         CALL genSkip(1112)
         CALL prttbl1(thisDate,Nfreq,hpcyc,Nz,subtitle,18,ndec,nf,
     $                'cyclical.component')
         call USRENTRY(hpcyc,1,Nz+nf,1,mpkp,2501)
         CALL genSkip(1107)
         if (WithoutVf.ne.0) then
          subtitle(1:36)='Revision error of CYCLICAL COMPONENT'
          CALL prttbl1(thisDate,Nfreq,seBc,Nz,subtitle,36,ndec,nf,
     $                 'cyclical.component.revision.error')
         else
c           call HTMLTABLE1('Total error of CYCLICAL COMPONENT',
          subtitle(1:36)='Revision error of CYCLICAL COMPONENT'
          CALL prttbl1(thisDate,Nfreq,seBc,Nz,subtitle,36,ndec,nf,
     $                 'cyclical.component.revision.error')
         end if
C
C LONG TERM TREND
C
c         write(*,*) '  ireg = ', ireg
c         write(*,*) '  long trem trend printed out'
         CALL genSkip(baseCode)
         subtitle(1:nLongTermCad)=LongTermCad
         CALL prttbl1(thisDate,Nfreq,hptrend,Nz,
     $                subtitle,nLongTermCad,ndec,
     $                nf,idCad(1:nidCad))
         call USRENTRY(hptrend,1,Nz+nf,1,mpkp,2502)
c         write(*,*) '  long trem trend saved'
         CALL genSkip(baseCode+4)
         subtitle(1:18+nLongTermCad)='Revision error of '//LongTermCad
         if (WithoutVf.ne.0) then
           CALL prttbl1(thisDate,Nfreq,seM,Nz,subtitle,18+nLongTermCad,
     $                  ndec,nf,idCad(1:nidCad)//'.revision.error')
         else
c           call HTMLTABLE1('Total error of LONG TERM TREND',
           CALL prttbl1(thisDate,Nfreq,seM,Nz,subtitle,18+nLongTermCad,
     $                  ndec,nf,idCad(1:nidCad)//'.revision.error')
c            Because we pass fee=0 to SErrorF
         end if
        end if
       else if (ireg .eq. 1) then
c        write(*,*)' ireg = ',ireg,' print out long term trend'
C
C CYCLE
C
        do i = 1,Nz+nf
         temp(i) = hpcyc(i) + hpregc(i)
         totcyc(i) = hpcyc(i) + hpregc(i)
        end do
        CALL genSkip(1104)
        subtitle(1:29)='STOCHASTIC CYCLICAL COMPONENT'
        CALL prttbl1(thisDate,Nfreq,hpcyc,Nz,subtitle,29,ndec,nf,
     $              'stochastic.cyclical.component')
        CALL genSkip(1105)
        subtitle(1:29)='REGRESSION CYCLICAL COMPONENT'
        CALL prttbl1(thisDate,Nfreq,hpregc,Nz,subtitle,29,ndec,nf,
     $              'regression.cyclical.component')
        CALL genSkip(1106)
        subtitle(1:24)='TOTAL CYCLICAL COMPONENT'
        CALL prttbl1(thisDate,Nfreq,hpregc,Nz,subtitle,24,ndec,nf,
     $              'total.cyclical.component')
        do i = 1,Nz+nf
         temp(i) = ONEHND * (compHP(i)/(kons*EXP(hptrend(i))))
        end do
        CALL genSkip(1108)
        subtitle(1:26)='STOCHASTIC CYCLICAL FACTOR'
        CALL prttbl1(thisDate,Nfreq,temp,Nz,subtitle,26,ndec,nf,
     $              'stochastic.cyclical.factors')
        do i = 1,Nz+nf
         temp(i) = ONEHND * EXP(hpregc(i))
        end do
        CALL genSkip(1109)
        subtitle(1:26)='REGRESSION CYCLICAL FACTOR'
        CALL prttbl1(thisDate,Nfreq,temp,Nz,subtitle,26,ndec,nf,
     $              'regression.cyclical.factors')
        do i = 1,Nz+nf
         temp(i) =
     $      ONEHND * (compHP(i)/(kons*EXP(hptrend(i))))*exp(hpregc(i))
         totcyc(i) =
     $      ONEHND * (compHP(i)/(kons*EXP(hptrend(i))))*exp(hpregc(i))
        end do
        CALL genSkip(1110)
        subtitle(1:21)='TOTAL CYCLICAL FACTOR'
        CALL prttbl1(thisDate,Nfreq,temp,Nz,subtitle,21,ndec,nf,
     $              'total.cyclical.factors')
        call USRENTRY(temp,1,Nz+nf,1,mpkp,2501)
        CALL genSkip(1111)
        if (WithoutVf.ne.0) then
         subtitle(1:33)='Revision error of CYCLICAL FACTOR'
         CALL prttbl1(thisDate,Nfreq,seBc,Nz,subtitle,33,ndec,nf,
     $                'cyclical.factors.revision.error')
        else
c           call HTMLTABLE1('Total error of CYCLICAL FACTOR',
         subtitle(1:33)='Revision error of CYCLICAL FACTOR'
         CALL prttbl1(thisDate,Nfreq,seBc,Nz,subtitle,33,ndec,nf,
     $                'cyclical.factors.revision.error')
        end if
C
C LONG TERM TREND
C
        CALL genSkip(baseCode+5)
        subtitle(1:11+nLongTermCad)='STOCHASTIC '//
     $     LongTermCad(1:nLongTermCad)
        CALL prttbl1(thisDate,Nfreq,hptrend,Nz,subtitle,11+nLongTermCad,
     $     ndec,nf,'stochastic.'//idCad(1:nidCad)//'.component')
        CALL genSkip(baseCode+6)
        subtitle(1:11+nLongTermCad)='REGRESSION '//
     $     LongTermCad(1:nLongTermCad)
        CALL prttbl1(thisDate,Nfreq,hpregt,Nz,subtitle,11+nLongTermCad,
     $     ndec,nf,'regression.'//idCad(1:nidCad)//'.component')
        do i = 1,Nz+nf
          temp(i) = hptrend(i) + hpregt(i)
        end do
        CALL genSkip(baseCode+7)
        subtitle(1:6+nLongTermCad)='TOTAL '//
     $     LongTermCad(1:nLongTermCad)
        CALL prttbl1(thisDate,Nfreq,temp,Nz,subtitle,6+nLongTermCad,
     $     ndec,nf,'total.'//idCad(1:nidCad)//'.component')
        do i = 1,Nz+nf
          temp(i) = kons * EXP(hptrend(i))
        end do
        CALL genSkip(baseCode+9)
        subtitle(1:11+nLongTermCad)='STOCHASTIC '//
     $     LongTermCad(1:nLongTermCad)
        CALL prttbl1(thisDate,Nfreq,temp,Nz,subtitle,11+nLongTermCad,
     $     ndec,nf,'stochastic.'//idCad(1:nidCad)//'.factors')
        do i = 1,Nz+nf
          temp(i) = EXP(hpregt(i))
        end do
        CALL genSkip(baseCode+10)
        subtitle(1:11+nLongTermCad)='REGRESSION '//
     $     LongTermCad(1:nLongTermCad)
        CALL prttbl1(thisDate,Nfreq,temp,Nz,subtitle,11+nLongTermCad,
     $     ndec,nf,'regression.'//idCad(1:nidCad)//'.factors')
        do i = 1,Nz+nf
          temp(i) = kons * EXP(hptrend(i)) * exp(hpregt(i))
        end do
        CALL genSkip(baseCode+11)
        subtitle(1:6+nLongTermCad)='TOTAL '//
     $     LongTermCad(1:nLongTermCad)
        CALL prttbl1(thisDate,Nfreq,temp,Nz,subtitle,6+nLongTermCad,
     &              ndec,nf,'total.'//idCad(1:nidCad)//'.factors')
        call USRENTRY(temp,1,Nz+nf,1,mpkp,2502)
c        write(*,*) ' total long trem trend saved'
        CALL genSkip(baseCode+12)
        subtitle(1:18+nLongTermCad)='Revision error of '//
     $     LongTermCad(1:nLongTermCad)
        if (WithoutVf.ne.0) then
         CALL prttbl1(thisDate,Nfreq,seM,Nz,subtitle,18+nLongTermCad,
     $                ndec,nf,idCad(1:nidCad)//'.revision.error')
        else
c           call HTMLTABLE1('Total error of LONG TERM TREND',
         CALL prttbl1(thisDate,Nfreq,seM,Nz,subtitle,18+nLongTermCad,
     $                ndec,nf,idCad(1:nidCad)//'.revision.error')
        end if
       else
c        write(*,*)' ireg = ',ireg,' print out long term trend'
C
C CYCLE
C
        do i = 1,Nz+nf
         temp(i) = ONEHND * (compHP(i)/(kons*EXP(hptrend(i))))
         totcyc(i) = ONEHND * (compHP(i)/(kons*EXP(hptrend(i))))
        end do
        CALL genSkip(1112)
        subtitle(1:18)='CYCLICAL COMPONENT'
        CALL prttbl1(thisDate,Nfreq,hpcyc,Nz,subtitle,18,ndec,nf,
     $              'cyclical.component')
        CALL genSkip(1113)
        subtitle(1:16)='CYCLICAL FACTORS'
        CALL prttbl1(thisDate,Nfreq,temp,Nz,subtitle,16,ndec,nf,
     $              'cyclical.factors')
        call USRENTRY(temp,1,Nz+nf,1,mpkp,2501)
        CALL genSkip(1114)
        if (WithoutVf.ne.0) then
         subtitle(1:34)='Revision error of CYCLICAL FACTORS'
         CALL prttbl1(thisDate,Nfreq,seBc,Nz,subtitle,34,ndec,nf,
     $               'cyclical.factors.revision.error')
        else
c         call HTMLTABLE1('Total error of CYCLICAL FACTOR',
         subtitle(1:34)='Revision error of CYCLICAL FACTORS'
         CALL prttbl1(thisDate,Nfreq,seBc,Nz,subtitle,34,ndec,nf,
     $               'cyclical.factors.revision.error')
        end if
C
C LONG TERM TREND
C
        CALL genSkip(baseCode+13)
        subtitle(1:10+nLongTermCad)=LongTermCad(1:nLongTermCad)//
     $                           ' COMPONENT'
c        write(*,*)' print out long term trend component'
        CALL prttbl1(thisDate,Nfreq,hptrend,Nz,subtitle,
     $              10+nLongTermCad,ndec,nf,
     $              idCad(1:nidCad)//'.component')
        do i = 1,Nz+nf
          temp(i) = kons * EXP(hptrend(i))
        end do
        CALL genSkip(baseCode)
        subtitle(1:nLongTermCad)=LongTermCad(1:nLongTermCad)
        CALL prttbl1(thisDate,Nfreq,temp,Nz,subtitle,
     $              nLongTermCad,ndec,nf,idCad(1:nidCad))
        call USRENTRY(temp,1,Nz+nf,1,mpkp,2502)
c         write(*,*) '  long trem trend component saved'
        CALL genSkip(baseCode+8)
        subtitle(1:18+nLongTermCad)='Revision error of '//
     $                             LongTermCad(1:nLongTermCad)
        if (WithoutVf.ne.0) then
         CALL prttbl1(thisDate,Nfreq,seM,Nz,subtitle,18+nLongTermCad,
     $                ndec,nf,idCad(1:nidCad)//'.revision.error')
        else
c           call HTMLTABLE1('Total error of LONG TERM TREND',
         CALL prttbl1(thisDate,Nfreq,seM,Nz,subtitle,18+nLongTermCad,
     $                ndec,nf,idCad(1:nidCad)//'.revision.error')
        end if
       end if
      end if
      end
*C
*C
*C
*      subroutine TABLE1(datax,nfor)
*C
*C.. Implicits ..
*      implicit none
*C
*C.. Formal Arguments ..
*C.. In/Out Status: Maybe Read, Not Written ..
*      real*8 datax(*)
*C.. In/Out Status: Read, Overwritten ..
*C.. In/Out Status: Read, Not Written ..
*      integer nfor
*C
*C.. Local Scalars ..
*      integer i,i1,i2,ifact,j,jfact,kfreq,ndecp,nfreq1,nnper,
*     $        nper1,nx,ny,nyr
*      integer*4 yr,krest
*      real*8 sum,zz
*      integer*4 decp
*C
*C.. Local Arrays ..
*      character fdecp1(7)*8,fn1(12)*8,fn2(12)*8,fnfreq(3)*8,mth(12)*4,
*     $          srt(11)*4,srt0(4)*4,srt1(6)*4,wrt0(8)*8,wrt2(7)*8,
*     $          wrt99(7)*8
*C
*C.. Intrinsic Functions ..
*      intrinsic ABS, INT, LOG10
*      include 'sform.i'
*      include 'stream.i'
*C
*C.. Data Declarations ..
*      data mth/
*     $     'JAN ','FEB ','MAR ','APR ','MAY ','JUN','JUL','AUG ','SEP',
*     $     'OCT ','NOV ','DEC '/
*      data srt/
*     $     '1ST','2ND','3RD','4TH','5TH','6TH','7TH','8TH','9TH','10TH',
*     $     '11TH'/
*      data srt0/'1ST','2ND','1ST','2ND'/
*      data srt1/'1ST','2ND','3RD','1ST','2ND','3RD'/
*      data wrt2/'(1H ,I4,','N2','X,','N1','(F10','.DECP','))'/
*      data wrt0/
*     $     '(1H ,I4,','"-", I4,','N2','X,','N1','(F10','.DECP','))'/
*      data wrt99/'(/,1X,','"YEAR"','2X,','N2','(6X,','A4','))'/
*      data fdecp1/'.0','.1','.2','.3','.4','.5','.6'/
*      data fn1/'1','2','3','4','5','6','7','8','9','10','11','12'/
*      data fn2/
*     $     '2','12','22','32','42','52','62','72','82','092','102','112'
*     $     /
*      data fnfreq/'4','12','6'/
*C
*C ... Executable Statements ...
*C
*      decp = 3
*      kfreq = Nfreq
*      if (kfreq .lt. 4) then
*       if (Nfreq .eq. 3) then
*        kfreq = 6
*       else
*        kfreq = 4
*       end if
*      end if
*      nnper = Nper
*      if (Nper .gt. Nfreq) then
*       Nper = Nfreq
*      end if
*      ndecp = decp
*      if (decp .ge. 6) then
*       decp = 6
*      end if
*c      if (decp .ne. 0) then
*c       mdecp = 10 - decp
*c       a = 0.00999999 * 10**mdecp
*c       do i = 1,Nz
*c        if (datax(i) .ge. a) then
*c         decp = decp - 1
*c        end if
*c       end do
*c      end if
*      zz = LOG10(ABS(datax(1))+.0000000001d0)
*      sum = ABS(zz)
*      do i = 2,Nz+nfor
*       if (zz .gt. 0.0d0) then
*        sum = 0.0d0
*        goto 5000
*       else
*        zz = LOG10(ABS(datax(i))+.0000000001d0)
*        if ((ABS(zz).lt.sum) .and. (zz.lt.0.0d0)) then
*         sum = ABS(zz)
*        end if
*       end if
*      end do
* 5000 if (zz .gt. 0.0d0) then
*       sum = 0.0d0
*      end if
*      ifact = 0
*      if (sum .gt. 1.0d0) then
*       ifact = INT(sum)
*       if (ifact .gt. 6) then
*        ifact = 6
*       end if
*       if (ifact .gt. 0) then
*        write (Nio,'(4X, "X  10.0D",I2,/)') -ifact
*       end if
*      end if
*      jfact = 0
*      zz = LOG10(ABS(datax(1))+.0000000001d0)
*      sum = zz
*      do i = 2,Nz+nfor
*       zz = LOG10(ABS(datax(i))+.0000000001d0)
*       if ((zz.gt.sum) .and. (zz.gt.0.0d0)) then
*        sum = zz
*       end if
*      end do
*      if (sum .gt. 4.0d0) then
*       jfact = INT(sum) - 2
*       if (jfact .gt. 0) then
*        write (Nio,'(4X, "X  10.0D",I2,/)') jfact
*       end if
*      end if
*      yr = Nyer
*      if (Nfreq .eq. 12) then
* 7000  format (/,1x,'YEAR',2x,12(6x,a4)/)
*       write (Nio,7000) (mth(i), i = 1,12)
*C      ELSE IF (NFREQ.EQ.4) THEN
*C        WRITE(NIO,2002) (QRT(I),I=1,4)
*C      ELSE IF (NFREQ.EQ.6) THEN
*C        WRITE(NIO,2003) (SRT(I),I=1,6)
*      else if (Nfreq .eq. 3) then
* 7001  format (/,3x,'YEAR',5x,6(6x,a4)/)
*       write (Nio,7001) (srt1(i), i = 1,6)
*      else if (Nfreq .eq. 2) then
* 7002  format (/,3x,'YEAR',5x,4(6x,a4)/)
*       write (Nio,7002) (srt0(i), i = 1,4)
*      else if (Nfreq .eq. 1) then
*       write (Nio,7002) (srt(i), i = 1,4)
*      else
*       wrt99(4) = fn1(Nfreq)
*       write (Nio,wrt99) (srt(i), i = 1,Nfreq)
*      end if
*      nyr = (Nz-(Nfreq-Nper+1)) / Nfreq
*      ny = (Nz-(Nfreq-Nper+1)) - nyr*Nfreq
*      if (ny .ne. 0) then
*       nyr = nyr + 1
*      end if
*      nyr = nyr + 1
*      wrt2(6) = fdecp1(decp+1)
*      do i = 1,nyr
*       i1 = (i-1)*kfreq - (Nper-2)
*       i2 = i*kfreq - (Nper-1)
*       krest = 0
*       if (i2 .ge. Nz) then
*        krest = i2-Nz
*        i2 = Nz
*       end if
*       if (Nfreq .ge. 4) then
*        wrt2(2) = fn2(1)
*        wrt2(4) = fn1(kfreq)
*       else
*        wrt0(3) = fn2(1)
*        wrt0(5) = fn1(kfreq)
*        wrt0(7) = fdecp1(decp+1)
*       end if
*       if (i .eq. 1) then
*        if (Nfreq .ge. 4) then
*         wrt2(4) = fn1(kfreq-Nper+1)
*         wrt2(2) = fn2(Nper)
*        else
*         wrt0(3) = fn2(Nper)
*         wrt0(5) = fn1(kfreq-Nper+1)
*        end if
*        i1 = 1
*       end if
*       if (Nfreq .lt. 4) then
*        if (ifact .gt. 0) then
*         write (Nio,wrt0)
*     $         yr, (yr+kfreq/Nfreq-1),
*     $         (datax(j)*(10.0d0**ifact), j = i1,i2)
*        else
*         write (Nio,wrt0)
*     $         yr, (yr+kfreq/Nfreq-1),
*     $         (datax(j)*(10.0d0**(-jfact)), j = i1,i2)
*        end if
*       else if (ifact .gt. 0) then
*        write (Nio,wrt2) yr, (datax(j)*(10.0d0**ifact), j = i1,i2)
*       else
*        write (Nio,wrt2) yr, (datax(j)*(10.0d0**(-jfact)), j = i1,i2)
*       end if
*       if (Nfreq .lt. 4) then
*        yr = yr + kfreq/Nfreq - krest/Nfreq
*       else
*        yr = yr + 1
*       end if
*       if (i2 .ge. Nz) goto 5001
*      end do
* 5001 decp = ndecp
*      Nper = nnper
*C
*C OUTPUT THE FORECAST
*C
*      nfreq1 = Nfreq
*      nper1 = Nper
*c     nper1 = Kfreq - Krest + 1
*      nx = Nz / nfreq1
*      nx = Nz - nx*nfreq1
*      if (nx .gt. 0) then
*       nper1 = nper1 + nx
*       if (nper1 .gt. nfreq1) then
*        nper1 = nper1 - nfreq1
*       end if
*      end if
*      write (Nio,'(1X,"FORECAST : ")')
*      nyr = (nfor-(nfreq1-nper1+1)) / nfreq1
*      ny = (nfor-(nfreq1-nper1+1)) - nyr*nfreq1
*      if (ny .ne. 0) then
*       nyr = nyr + 1
*      end if
*      nyr = nyr + 1
*      do i = 1,nyr
*       i1 = (i-1)*kfreq - (Nper1-2)
*       i2 = i*kfreq - (Nper1-1)
*       if (Nz+i2 .ge. Nz+nfor) then
*        i2 = nfor
*       end if
*       wrt2(2) = fn2(1)
*       wrt2(4) = fnfreq(1)
*       if (Nfreq .ge. 4) then
*        wrt2(2) = fn2(1)
*        wrt2(4) = fn1(kfreq)
*       else
*        wrt0(3) = fn2(1)
*        wrt0(5) = fn1(kfreq)
*        wrt0(7) = fdecp1(decp+1)
*       end if
*       if (i .eq. 1) then
*        if (Nfreq .ge. 4) then
*         wrt2(4) = fn1(kfreq-Nper1+1)
*         wrt2(2) = fn2(Nper1)
*        else
*         wrt0(3) = fn2(Nper1)
*         wrt0(5) = fn1(kfreq-Nper1+1)
*        end if
*        i1 = 1
*       end if
*       if (Nfreq .lt. 4) then
*        if (ifact .gt. 0) then
*         write (Nio,wrt0)
*     $         yr, (yr+kfreq/Nfreq-1),
*     $         (datax(Nz+j)*(10.0d0**ifact), j = i1,i2)
*        else
*         write (Nio,wrt0)
*     $         yr, (yr+kfreq/Nfreq-1),
*     $         (datax(Nz+j)*(10.0d0**(-jfact)), j = i1,i2)
*        end if
*       else if (ifact .gt. 0) then
*        write (Nio,wrt2) yr, (datax(Nz+j)*(10.0d0**ifact), j = i1,i2)
*       else
*        write (Nio,wrt2) yr, (datax(Nz+j)*(10.0d0**(-jfact)), j = i1,i2)
*       end if
*       if (Nfreq .lt. 4) then
*        yr = yr + kfreq/Nfreq
*       else
*        yr = yr + 1
*       end if
*       if (i2 .ge. nfor) goto 5002
*      end do
* 5002 decp = ndecp
*      Nfreq = nfreq1
**      Nper = nper1
*      end
C
C
C
      subroutine RATESGROWTH(mq,lam,sqf,oz,trend,sa,nz,sigpt1,
     $                       sigat1,nlen,sigptac,sigatac,sigptaf,
     $                       sigataf,sigptmq,sigatmq,rcetre,rceadj,
     $                       teetre,teeadj,psiep,psiea,psitot,lf,nyer,
     $                       nper,rogtable,iter,title,out,
     $                       THstar,lTHstar,HFp,lHp0,Vrp,HFsa,lHFsa,
     $                       Vrsa)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
C Modified by REG, on 21 Apr 2006
      INCLUDE 'tbl5x.i'
      INCLUDE 'htmlout.cmn'
      real*8 sCoef
      integer nfl,mp,kp,nOutPar
      parameter (kp = PFCST, mp = POBS, nfl = 2*mp, nOutPar=36)
      real*8 ZERO,ONE,TWO,ONEHND
      parameter (ZERO=0.0d0,ONE=1.0d0,TWO=2.0d0,ONEHND=100.0d0)
C
C.. Formal Arguments ..
      integer mq,lam,nz,nlen,lf,nyer,nper,pg,rogtable,iter,out,lTHstar,
     $        lHFsa,lHp0
      integer Reverse
      character title*80
      real*8 sqf,oz(*),trend(*),sa(*),sigpt1(0:kp),sigat1(0:kp),
     $       sigptac(kp),sigatac(kp),sigptaf(kp),sigataf(kp),sigptmq(2),
     $       sigatmq(2),rcetre(0:12),rceadj(0:12),teetre(0:12),
     $       teeadj(0:12),psiep(nfl),psiea(nfl),psitot(nfl),HFp(59),Vrp,
     $       THstar(27),HFsa(59),Vrsa
C
C.. Local Scalars ..
c      integer finbucle,ifail,k,kmq
c      real*8 wvalue
c      integer Nyer2, Nper2
      integer i,ifact,j,jfact,kfact,lagr,nlastper,nlastpsave,nlastyear,
     $        nlastysave,nroga1,nrogamq,nrogp1,nrogpmq,nrogx1,nrogxmq,
     $        nsdrev
C   LINES OF CODE ADDED FOR X-13A-S : 1
c      integer noutdir
C   END OF CODE BLOCK         
c      character filename*180,fname*30,subtitle*50
c      real*8 suma1,suma2,sump1,sump2
      real*8 a,b,c,d,e,f,g,h,o,racca,raccp,raccx,rmqa1,rmqa2,rmqp1,
     $       rmqp2,rmqx1,rmqx2,sdatac,sdatac1,sdatac2,sdchecka,sdcheckp,
     $       sdptac,sdptac1,sdptac2,sdrmqx1,sdrmqx2,sum,sum1,suma,sump
      real*8 sumx1,sumx2,varf,vart,vprf,vprt,vramq,vrpmq,zz
C
C.. Local Arrays ..
      character mth(12)*4,amth(12)*9,srt(8)*4,asrt(8)*7,cq*1,str*3
      real*8 roga1(kp),rogamq(kp),rogp1(kp),rogpmq(kp),rogx1(kp),
     $       rogxmq(kp),
     $       sdreva1tmp(kp),
     $       sdrevp1tmp(kp),tmp(8),tmpsa(8),
     $       tmpser(8),tmptr(8)
      integer ipos
      real*8 SDrev_p(nOutPar), !SDrev(i):component revision SE of Trend
     $       SDR1_p(nOutPar),  !SDR1(i):revision SE  T(1,1) of Trend
     $       SDR1f_p,            !SDR1f:revision SE (1-F) of Trend
     $       SDRmq_p(nOutPar), !SDRmq(i):revision SE  T(1,mq) of Trend
     $       SDRmqF_p, !revision SE of (1-F^mq) for concurrent of Trend
     $       SDRmqC_p,   !revision SE of (B^(mq/2-1)-F^(mq/2)) of Trend
     $       SDRmqC2_p,!revision SE of (B^(mq/2-2)-F^(mq/2-1)) of Trend
     $       SDRmqPf,  !revision SE of annual rate for the present year
     $       SDrev_SA(nOutPar), !SDrev(i):component revision SE of SA
     $       SDR1_SA(nOutPar),  !SDR1(i):revision SE  T(1,1) of SA
     $       SDR1f_SA,            !SDR1f:revision SE (1-F) of SA
     $       SDRmq_SA(nOutPar), !SDRmq(i):revision SE  T(1,mq) of SA
     $       SDRmqF_SA,   !revision SE of (1-F^mq) for concurrent of SA
     $       SDRmqC_SA,         !revision SE of (B^(mq/2-1)-F^(mq/2))
     $       SDRmqC2_SA,        !revision SE of (B^(mq/2-2)-F^(mq/2-1))
     $       SDRmqSAf  !revision SE of annual rate for the present year

C
C.. External Functions ..
      integer ISTRLEN
      external ISTRLEN
C
C.. External Calls ..
c      external CLOSEDEVICE, OPENDEVICE
      external ROGEST, ROGSIG
C
C.. Intrinsic Functions ..
      intrinsic ABS, INT, LOG10, MOD, SQRT
      include 'dirs.i'
      include 'stream.i'
      include 'seatop.cmn'
      include 'units.cmn'
C
C.. Data Declarations ..
      data mth/
     $     'JAN ','FEB ','MAR ','APR ','MAY ','JUN','JUL','AUG ','SEP',
     $     'OCT ','NOV ','DEC '/
      DATA amth/'January  ','February ','March    ','April    ',
     &          '@        ','June     ','July     ','August   ',
     &          'September','October  ','November ','December '/
      data srt/'1ST','2ND','3RD','4TH','5TH','6TH','7TH','8TH'/
      data asrt/'first  ','second ','third  ','fourth ',
     &          'fifth  ','sixth  ','seventh','eighth '/
      data cq/'"'/
C
C ... Executable Statements ...
C
      nlastper = nper
      nlastyear = nyer
      do i = 2,nz
       if (MOD(nlastper,mq) .eq. 0) then
        nlastyear = nlastyear + 1
        nlastper = 0
       end if
       nlastper = nlastper + 1
      end do
      nlastpsave = nlastper
      nlastysave = nlastyear
      racca=0D0
      raccp=0D0
      raccx=0D0
c     write(nio,'("SErates of TREND",I3,G11.3,G11.3)') nOutPar,Sqf,Vrp
c      WRITE(Mtprof,*)' Before SErates, lHp0,lTHstar = ', lHp0,lTHstar
      call SErates(HFp,lHp0,THstar,lTHstar,PSIEP,lf,Vrp,Sqf*Sqf,mq,
     $             nLastPer,nOutPar,
     $             SDrev_p,SDR1_p,SDR1f_p,SDRmqF_p,SDRmqC_p,SDRmqPf,
     $             SDRmq_P,SDRmqC2_p)
c     call SEratesOut(SDrev_P,SDR1_P,SDR1f_P,SDRmq_P,SDRmqF_P,
c     $                      SDRmqC_P,SDRmqPf,SDRmqC2_P,nOutPar,nio)

c     write(nio,'("SErates of SA")')
c      WRITE(Mtprof,*)' Before SErates, lHFsa = ', lHFsa
      call SErates(HFsa,lHFsa,THstar,lTHstar,PSIEA,lf,Vrsa,Sqf*Sqf,mq,
     $       nLastPer,nOutPar,
     $       SDrev_SA,SDR1_SA,SDR1f_SA,SDRmqF_SA,SDRmqC_SA,SDRmqSAf,
     $       SDRmq_SA,SDRmqC2_SA)
c     call SEratesOut(SDrev_SA,SDR1_SA,SDR1f_SA,SDRmq_SA,SDRmqF_SA,
c     $                 SDRmqC_SA,SDRmqSAf,SDRmqC2_SA,nOutPar,nio)

      lagr = 1
      a = 0D0
      b = 0D0
      c = 0D0
      d = 0D0
      e = 0D0
      f = 0D0
      g = 0D0
      h = 0D0
      o = 0D0
      call ROGEST(oz,nz,rogx1,nrogx1,mq,lam,lagr)
      call ROGEST(sa,nz,roga1,nroga1,mq,lam,lagr)
      call ROGEST(trend,nz,rogp1,nrogp1,mq,lam,lagr)
*      do k = 1,nroga1
*       suma = 0.0d0
*       sump = 0.0d0
*       do j = k,lf-1
*        suma = suma + (psiea(lf+1-j)-psiea(lf+1-j-1))**2
*        sump = sump + (psiep(lf+1-j)-psiep(lf+1-j-1))**2
*       end do
*       sdreva1(k) = suma
*       sdrevp1(k) = sump
*      end do
      if (lam .eq. 0) then
       do j = 1,nroga1
         SDR1_SA(j)=SDR1_SA(j)*100D0
         SDR1_P(j)=SDR1_P(j)*100D0
       end do
      end if
      if (out.eq.0) then
       CALL genSkip(1050)
       CALL writTagOneLine(Nio,'h2','@','PART 5 : RATES OF GROWTH')
       CALL writTagClass(Nio,'ul','indent')
       if (lam .eq. 0) then
        write (nio,1010)'THE RATE-OF-GROWTH','IN PERCENT POINTS AS',
     $                  '[ (Z(t2) / Z(t1)) -1] * 100'
 1010   FORMAT('<li>',a,' OF SERIES Z(t) OVER THE PERIOD (t1,t2) IS ',
     &        'EXPRESSED ',a,/,1x,a,'.</li>')
        write (nio,1020)
 1020   FORMAT('<li>ALL STANDARD ERRORS REPORTED FOR THE ',
     $        'RATES-OF GROWTH IN THE FOLLOWING TABLES ARE ',/,1x,
     $        'COMPUTED USING LINEAR APPROXIMATION TO THE RATES.',/,1x,
     $        'WHEN PERIOD-TO-PERIOD CHANGES ARE LARGE, ',
     $        'THESE STANDARD ERRORS SHOULD BE INTERPRETED',/,1x,
     $        'AS BROAD APPROXIMATIONS, THAT WILL TEND TO ',
     $        'UNDERESTIMATE THE TRUE VALUES.</li>')
        write (nio,1030)
 1030   FORMAT('<li>THE ERROR VARIANCES ARE BASED ON THE ',
     $        'ESTIMATION ERROR OF THE STOCHASTIC TREND AND SA',/,1x,
     $       'SERIES, AND THE ERRORS IN THE PARAMETER ESTIMATES ',
     $       'ARE NOT CONSIDERED.',/,1x,'GIVEN THAT THE ',
     $       'VARIANCES OF THE LATTER GO TO ZERO AS t BECOMES ',
     $       'LARGE, THEY WILL TYPICALLY',/,1x,'BE DOMINATED ',
     $       'BY THE ESTIMATION ERROR VARIANCE OF THE STOCHASTIC ',
     $       'COMPONENTS.',/,1x,'(THIS DOMINANCE WILL BE ',
     $       'WEAKEST IN THE VICINITY OF OUTLIERS.)</li>')
       else
        write (nio,1010)'GROWTH','AS','[ Z(t2) / Z(t1)]'
        write (nio,1030)
        write (nio,1040)cq,cq,cq,cq,Cbr,PRGNAM
 1040   FORMAT('<li>SINCE THE SERIES IS MODELLED IN LEVELS',
     $        ' AND ITS DECOMPOSITION IS ADDITIVE, THE',/,1x,a,
     $        'RATES OF GROWTH',a,' ARE SIMPLY DENOTED ',a,
     $        'GROWTH',a,' OF THE SERIES IN QUESTION.',/,1x,
     $        'This growth can be transformed easily into a rate ',
     $        '(dividing by the value at the',/,3x,
     $        'starting period and multiplying by 100).',a,/,3x,
     $        'Alternatively, a usually good approximation can be ',
     $        'obtained by re-running',/,3x,
     $        a,' with <em>function=log</em> in the <em>transform</em>',
     $        ' spec, the same model, and reestimating the parameters.',
     $        '</li>')
       end if
      else
       CALL writTagClass(Nio,'ul','indent')
       write (nio,1010)'GROWTH','AS','[ Z(t2) / Z(t1) ]'
       write (nio,1030)
       write (nio,1040)cq,cq,cq,cq,Cbr,PRGNAM
      end if
      CALL writTag(Nio,'</ul>')
      CALL mkPOneLine(Nio,'ub','IN THE TABLES THAT FOLLOW :')
      CALL mkPOneLine(Nio,'@','<em>ORIGINAL SERIES</em> DENOTES THE '//
     $                'OBSERVED SERIES, UNLESS THERE ARE MISSING '//
     $                'VALUES, IN WHICH CASE IT DENOTES THE '//
     $                '<em>INTERPOLATED SERIES</em>.')
      CALL mkPOneLine(Nio,'@','<em>TREND-CYCLE</em> AND '//
     $                '<em>SA SERIES</em> DENOTE THE FINAL '//
     $                'ESTIMATORS, WITH DETERMINISTIC EFFECTS '//
     $                '(IF PRESENT) INCLUDED.')
      CALL writTagOneLine(Nio,'h2','@',
     $       'A. PERIOD-TO-PERIOD RATE-OF-GROWTH OF THE SERIES. T(1,1)')
*      end if  
C
C TABLE 5.1
C
C Modified by REG, on 21 Apr 2006, to select between SEATS version 
C of table 5.1, and an alternate finite sample version
      IF ( .not.Lfinit ) THEN
       tmp(1) = sigpt1(nlen+1)**2
       tmp(2) = sigat1(nlen+1)**2
       tmp(3) = SDR1_P(1)**2
       tmp(4) = SDR1_SA(1)**2
       tmp(5) = sigpt1(nlen+1)**2 + SDR1_P(1)**2
       tmp(6) = sigat1(nlen+1)**2 + SDR1_SA(1)**2
       tmp(7) = SQRT(sigpt1(nlen+1)**2+SDR1_P(1)**2)
       tmp(8) = SQRT(sigat1(nlen+1)**2+SDR1_SA(1)**2)
      ELSE
C Modified by REG, on 26 May 2006, to give percentage GR SEs for lam=0
       if ( lam .eq. 0 ) then
        sCoef = 10000D0
       else
        sCoef = ONE
       end if
       tmp(1) = vTbl51(1)*sCoef
       tmp(2) = vTbl51(2)*sCoef
       tmp(3) = vTbl51(3)*sCoef
       tmp(4) = vTbl51(4)*sCoef
       tmp(5) = vTbl51(5)*sCoef
       tmp(6) = vTbl51(6)*sCoef
       tmp(7) = SQRT( vTbl51(5)*sCoef )
       tmp(8) = SQRT( vTbl51(6)*sCoef )
      END IF
      call setT11t(tmp(7))
      call setT11sa(tmp(8))
      zz = LOG10(ABS(tmp(1)+.0000000001d0))
      sum = ABS(zz)
      do i = 2,8
       if (zz .gt. ZERO) then
        sum = ZERO
        goto 5000
       else
        zz = LOG10(ABS(tmp(i)+.0000000001d0))
        if ((ABS(zz).lt.sum) .and. (zz.lt.ZERO)) then
         sum = ABS(zz)
        end if
       end if
      end do
 5000 if (sum .gt. ONE) then
       ifact = INT(sum)
       if (ifact .gt. 9) then
        ifact = 9
       end if
      end if
      jfact = 0
      zz = LOG10(ABS(tmp(1)+.0000000001d0))
      sum = zz
      do i = 2,8
       zz = LOG10(ABS(tmp(i)+.0000000001d0))
       if ((zz.gt.sum) .and. (zz.gt.ZERO)) then
        sum = zz
       end if
      end do
      if (sum .gt. 4.0d0) then
       jfact = INT(sum) - 2
      end if
      ifact = -jfact
      if (out.eq.0) then
       CALL genSkip(1051)
       CALL mkTableTag(Nio,'w80',
     &                'RATE T(1,1) : ESTIMATION ERROR VARIANCE')
       if (ABS(ifact) .gt. 0) then
        ipos=1
        CALL itoc(ifact,str,ipos)
        CALL mkCaption(Nio,
     &            'TABLE 5.1 RATE T(1,1) : ESTIMATION ERROR VARIANCE '//
     &            '(x 1.0D'//str(1:(ipos-1))//')')
       ELSE
        CALL mkCaption(Nio,
     &              'TABLE 5.1 RATE T(1,1) : ESTIMATION ERROR VARIANCE')
       END IF
       write (nio,1050)
       write(nio,1060)'FINAL ESTIMATION ERROR',
     $       tmp(1)*(10.0d0**ifact), tmp(2)*(10.0d0**ifact)
       write(nio,1060)'REVISION ERROR',
     $       tmp(3)*(10.0d0**ifact), tmp(4)*(10.0d0**ifact)
       write(nio,1060)'TOTAL ESTIMATION ERROR',
     $       tmp(5)*(10.0d0**ifact), tmp(6)*(10.0d0**ifact)
       write(nio,1060)'(SD)',
     $       tmp(7)*(10.0d0**ifact), tmp(8)*(10.0d0**ifact)
       CALL writTag(Nio,'</table>')
       CALL mkPOneLine(Nio,'@','&nbsp;')
      end if
 1050 FORMAT('<tr><th scope="col">CONCURRENT ESTIMATOR</th>',
     $       '<th scope="col">TREND-CYCLE</th>',/,'<th scope="col">',
     $       '<abbr title="seasonally adjusted">SA</abbr> SERIES</th>',
     $       '</tr>')
 1060 FORMAT('<tr><th scope="row">',a,'</th>',2('<td>',F9.3,'</td>'),
     $       '</tr>')
C
C TABLE 5.2
C
      if (out.eq.0) then

       CALL writTag(Nio,'<ul>')
       CALL writTagOneLine(Nio,'li','@','As mentioned before, '//
     $           'for applied purposes, the relevant error is the '//
     $           'full revision the measurement will undergo. '//
     $           'Accordingly, the standard errors appearing in '//
     $           'most of the next tables are the ones implied by the'//
     $           ' revision error.')
       CALL writTagOneLine(Nio,'li','@',
     $           'These <abbr title="standard error">S.E.</abbr> '//
     $           'can be used to build confidence intervals around '//
     $           'the concurrent or, in general, preliminary '//
     $           'estimators, that indicate a likely range for the '//
     $           'eventual final estimator.')
       CALL writTagOneLine(Nio,'li','@',
     $           'The <abbr title="standard error">S.E.</abbr> can '//
     $           'also be used to test for specific hypothesis. '//
     $           'For example in TABLE 5.2 (below), let RC(t) be '//
     $           'the concurrent estimator of a rate for period t. '//
     $           'If : <em>| RC(t)/SE[RC(t)] | &gt; 1.645</em>, '//
     $           'we can reject (at the 90% level) that the eventual '//
     $           'final estimator of the rate for period t could be '//
     $           'zero.')
       CALL writTag(Nio,'</ul>')

       CALL genSkip(1052)
       if (lam .eq. 0) then
        CALL mkTableTag(Nio,'x11','PERIOD-TO-PERIOD RATE T(1,1) FOR '//
     &                  'THE MOST RECENT PERIODS '//
     &                  'With associated SE in Percent points.')
        CALL mkCaption(Nio,'TABLE 5.2 '//Cbr//
     &                 'PERIOD-TO-PERIOD RATE T(1,1) '//
     &                 'FOR THE MOST RECENT PERIODS'//Cbr//'With '//
     &                 'associated <abbr title="standard error">'//
     &                 'SE</abbr> in Percent points.')
       else
        CALL mkTableTag(Nio,'x11','PERIOD-TO-PERIOD GROWTH T(1,1) '//
     $                  'FOR THE MOST RECENT PERIODS With '//
     &                  'associated SE')
        CALL mkCaption(Nio,'TABLE 5.2 '//Cbr//
     &                 'PERIOD-TO-PERIOD GROWTH T(1,1) '//
     $                 'FOR THE MOST RECENT PERIODS '//Cbr//
     &                 'With associated <abbr title="standard error">'//
     &                 'SE</abbr>')
       end if
       CALL makColgroup(Nio,0)
       CALL makColgroup(Nio,0)
       CALL makColgroup(Nio,2)
       CALL makColgroup(Nio,2)
       CALL writTag(Nio,'<thead>')
       CALL writTag(Nio,'<tr>')
       CALL mkTableCellSpan(Nio,'row',2,'head','&nbsp;')
       CALL mkHeaderCellScope(Nio,2,0,'col','@','ORIGINAL'//Cbr//
     $                        'SERIES')
       CALL mkHeaderCellScope(Nio,0,2,'colgroup','@','TREND-CYCLE')
       CALL mkHeaderCellScope(Nio,0,2,'colgroup',
     $                        'seasonally adjusted series','SA SERIES')
       CALL writTag(Nio,'</tr>')
       CALL writTag(Nio,'<tr>')
       DO i=1,2
        CALL mkHeaderCellScope(Nio,2,0,'col','@','ESTIMATE')
        CALL mkHeaderCellScope(Nio,2,0,'col','standard error','SER')
       END DO
       CALL writTag(Nio,'</tr>')
       CALL writTag(Nio,'</thead>')
       CALL writTag(Nio,'<tbody>')
       
      if (mq .eq. 12) then
       do i = 1,nroga1
C Modified by REG, on 21 Apr 2006, to select between SEATS version 
C of table 5.2, and an alternate finite sample version
        if ( .not. Lfinit ) then
          if (nlastper.eq.5) THEN
           write (nio,1080)
     $         mth(nlastper), nlastyear, rogx1(i), rogp1(i), SDR1_P(i),
     $         roga1(i), SDR1_SA(i)
          else
           ipos=istrlen(amth(nlastper))
           write (nio,1081)amth(nlastper)(1:ipos),
     $         mth(nlastper), nlastyear, rogx1(i), rogp1(i), SDR1_P(i),
     $         roga1(i), SDR1_SA(i)
          end if
 1080     FORMAT('<tr><th scope="row">',a3,'-',i4,'</th>',/,
     $           5('<td class="center">',g11.3,'</td>'),/,'</tr>')
 1081     FORMAT('<tr><th scope="row"><abbr title="',a,'">',a3,
     $           '</abbr>-',i4,'</th>',/,
     $           5('<td class="center">',g11.3,'</td>'),/,'</tr>')
        else if ( i .le. nTreGRSE1(1) ) then
C Modified by REG, on 26 May 2006, to give percentage GR SEs for lam=0
         if ( lam .eq. 0 ) then
          sCoef = 100D0
         else
          sCoef = ONE
         end if
         if(nlastper.eq.5)THEN
          write (nio,1080)
     $         mth(nlastper), nlastyear, rogx1(i), rogp1(i),
     $         vTreGRSE1(i)*sCoef, roga1(i), vSeaGRSE1(i)*sCoef
          else
           ipos=istrlen(amth(nlastper))
           write (nio,1081)amth(nlastper)(1:ipos),
     $         mth(nlastper), nlastyear, rogx1(i), rogp1(i),
     $         vTreGRSE1(i)*sCoef, roga1(i), vSeaGRSE1(i)*sCoef
          end if
        end if
        if (nlastper .eq. 1) then
         nlastper = mq
         nlastyear = nlastyear - 1
        else
         nlastper = nlastper - 1
        end if
       end do
      else
       do i = 1,nroga1
C Modified by REG, on 21 Apr 2006, to select between SEATS version 
C of table 5.2, and an alternate finite sample version
        if ( .not. Lfinit ) then
          ipos=istrlen(asrt(nlastper))
          write (nio,1081)asrt(nlastper)(1:ipos), 
     $         srt(nlastper), nlastyear, rogx1(i), rogp1(i), SDR1_P(i),
     $         roga1(i), SDR1_SA(i)
        else if ( i .le. nTreGRSE1(1) ) then
C Modified by REG, on 26 May 2006, to provide percentage GRs for lam=0
         if ( lam .eq. 0 ) then
          sCoef = 100D0
         else
          sCoef = ONE
         end if
         ipos=istrlen(asrt(nlastper))
         write (nio,1081)asrt(nlastper)(1:ipos), 
     $         srt(nlastper), nlastyear, rogx1(i), rogp1(i), 
     $         vTreGRSE1(i)*sCoef, roga1(i), vSeaGRSE1(i)*sCoef
        end if
        if (nlastper .eq. 1) then
         nlastper = mq
         nlastyear = nlastyear - 1
        else
         nlastper = nlastper - 1
        end if
       end do
      end if
       CALL writTag(Nio,'</tbody>')
       CALL writTag(Nio,'</table>')
       CALL mkPOneLine(Nio,'@','&nbsp;')
      end if
c
      if (rogtable .eq. 1) then
       if (iter .ne. 0) then
        CALL mkPOneLine(54,'bold',title)
       end if
       CALL mkTableTag(54,'x11','Estimates of Rates of Growth')
       CALL mkCaption(54,'Rates of Growth')
       write (54,5401)
       write (54,5402)'T11 RATE : ', rogx1(1), rogp1(1), roga1(1)
      end if
 5401 FORMAT('<tr><td class="head">&nbsp;</td>',
     $       '<th scope="col">ORIGINAL SERIES</th>',
     $       '<th scope="col">TREND-CYCLE</th>',
     $       '<th scope="col">SEASONALLY ADJUSTED SERIES</th></tr>')
 5402 FORMAT('<tr><th scope="row">',a,'</th>',3('<td>',g10.3,'</td>'),
     $       '</tr>')
C
C HERE INTRODUCE THE GRAPH FOR T11 RATE
C
*      if ((pg .eq. 0).and.(iter.eq.0).and.(out.lt.2)) then
*       Nper2 = Nper
*       Nyer2 = Nyer
*       Nper = nlastpsave
*       Nyer = nlastysave
*       Reverse = 1
*c  cambiamos el sentido del arra1 Xi<-->Xn+1-i
*c      
*CUNX#ifdef TSW
*!DEC$ IF DEFINED (TSW)
*       reverse = 0 
*       finbucle=nroga1-1
*       Do i=1,finbucle
*        nper=nper-1
*        if (nper.eq.0) then
*         nyer = nyer-1
*         nper = mq
*        end if
*       end do 
*       finbucle = int(nroga1/2)  
*       Do i=1, finbucle
*        wvalue = rogx1(i)
*        rogx1(i) = rogx1(nroga1+1-i)
*        rogx1(nroga1+1-i) = wvalue
*        wvalue = rogp1(i)
*        rogp1(i) = rogp1(nroga1+1-i)
*        rogp1(nroga1+1-i) = wvalue
*        wvalue = roga1(i)
*        roga1(i) = roga1(nroga1+1-i)
*        roga1(nroga1+1-i) = wvalue
*       end do 
*!DEC$ END IF
*CUNX#end if
*       fname = 'ROGX1.T'
*       subtitle = 'T(1,1) RATE ORIGINAL SERIES'
*       call PLOTRSERIES(fname,subtitle,rogx1,nroga1,1,999.0d0)
*       fname = 'ROGP1.T'
*       subtitle = 'T(1,1) RATE TREND-CYCLE'
*       call PLOTRSERIES(fname,subtitle,rogp1,nroga1,1,999.0d0)
*       fname = 'ROGA1.T'
*       subtitle = 'T(1,1) RATE SA SERIES'
*       call PLOTRSERIES(fname,subtitle,roga1,nroga1,1,999.0d0)
*       Reverse = 0
*       Nper = Nper2
*       Nyer = Nyer2
*      end if
C
C NOW INSERT THE CHECK ON THE ABOVE TABLE 5.2
C
      call ROGSIG(sigat1,nlen+1,sdreva1tmp,nsdrev)
      call ROGSIG(sigpt1,nlen+1,sdrevp1tmp,nsdrev)
      sdcheckp = TWO*rcetre(0)*(ONE-rcetre(1))-psiep(lf)**2 
      if (sdcheckp.gt.0) then
       sdcheckp = sqf * SQRT(sdcheckp)
      end if
      sdchecka = TWO*rceadj(0)*(ONE-rceadj(1))-psiea(lf)**2
      if (sdchecka.gt.0) then 
       sdchecka = sqf * SQRT(sdchecka)
      end if
      if (lam .eq. 0) then
       sdcheckp = sdcheckp * ONEHND
       sdchecka = sdchecka * ONEHND
      end if
C
C END THE CHECK ON THE TABLE 5.2
C
C
C TABLE 5.3
C
C
C FIRST METHOD
C
      if (mq .ne.1) then
       if (out.eq.0) then
        CALL writTagOneLine(Nio,'h3','@',
     $                      'B. ACCUMULATED RATE OF GROWTH DURING '//
     $                      'THE PRESENT YEAR.')
       end if
       if (lam .eq. 0) then
        raccx = (oz(nz)/oz(nz-nlastpsave)-ONE) * ONEHND
        raccp = (trend(nz)/trend(nz-nlastpsave)-ONE) * ONEHND
        racca = (sa(nz)/sa(nz-nlastpsave)-ONE) * ONEHND
        sump = ZERO
        suma = ZERO
        do i = 1,lf-nlastpsave
         suma = suma + (psiea(lf+1-i)-psiea(lf+1-i-nlastpsave))**2
         sump = sump + (psiep(lf+1-i)-psiep(lf+1-i-nlastpsave))**2
        end do
        sdptac = sqf * SQRT(sump) * ONEHND
        sdatac = sqf * SQRT(suma) * ONEHND
        if (out.eq.0) then
         CALL genSkip(1053)
         CALL mkTableTag(Nio,'x11','ACCUMULATED RATE OF GROWTH '//
     $                   'DURING THE PRESENT YEAR.')
         CALL mkCaption(Nio,'TABLE 5.3'//Cbr//
     $                  'ACCUMULATED RATE OF GROWTH '//
     $                  'DURING THE PRESENT YEAR.'//Cbr//
     $                  '(In percent points)')

         if (mq .eq. 12) then
          if (nlastpsave.eq.5)THEN
           write (nio,1090) mth(nlastpsave), nlastysave
          else
           ipos=istrlen(amth(nlastpsave))
           write (nio,1091) amth(nlastpsave)(1:ipos), mth(nlastpsave),
     $                      nlastysave
          end if
         else
          ipos=istrlen(asrt(nlastpsave))
          write (nio,1091) asrt(nlastpsave)(1:ipos), srt(nlastpsave), 
     $                     nlastysave
         end if
 1090    FORMAT('<tr><th>',a3,'-',i4,'</th>',
     $         '<th scope="col">ESTIMATE</th><th scope="col">',
     $         '<abbr title="standard error">SER</abbr></th></tr>')
 1091    FORMAT('<tr><th><abbr title="',a,'">',a3,'</abbr>-',i4,'</th>',/,
     $         '<th scope="col">ESTIMATE</th><th scope="col">',
     $         '<abbr title="standard error">SER</abbr></th></tr>')
         write (nio,1100)'ORIGINAL SERIES', raccx
 1100    FORMAT('<tr><th scope="row">',a,'</th><td class="center">',
     $          g11.3,'</td><td class="center">-</td></tr>')
C Modified by REG, on 21 Apr 2006, to select between SEATS version 
C of table 5.3, and an alternate finite sample version
         if(.not. Lfinit)THEN
          write (nio,1110)'TREND-CYCLE', raccp, sdptac
          write (nio,1110)'SEASONALLY ADJUSTED SERIES', racca, sdatac
         else
          write (nio,1110)'TREND-CYCLE', raccp, vTbl53(1)*sCoef
          write (nio,1110)'SEASONALLY ADJUSTED SERIES',
     $                    racca, vTbl53(2)*sCoef
         end if
 1110    FORMAT('<tr><th scope="row">',a,'</th>',
     $          2('<td class="center">',g11.3,'</td>'),'</tr>')
         CALL writTag(Nio,'</table>')
         CALL mkPOneLine(Nio,'@','&nbsp;')
        end if
       else
        raccx = oz(nz) - oz(nz-nlastpsave)
        raccp = trend(nz) - trend(nz-nlastpsave)
        racca = sa(nz) - sa(nz-nlastpsave)
        sump = ZERO
        suma = ZERO
        do i = 1,lf-nlastpsave
         suma = suma + (psiea(lf+1-i)-psiea(lf+1-i-nlastpsave))**2
         sump = sump + (psiep(lf+1-i)-psiep(lf+1-i-nlastpsave))**2
        end do
        sdptac = sqf * SQRT(sump)
        sdatac = sqf * SQRT(suma)
        if (out.eq.0) then
         CALL genSkip(1053)
         CALL mkTableTag(Nio,'x11','ACCUMULATED RATE OF GROWTH '//
     $                  'DURING THE PRESENT YEAR.')
         CALL mkCaption(Nio,'TABLE 5.3'//Cbr//
     $                  'ACCUMULATED RATE OF GROWTH '//
     $                  'DURING THE PRESENT YEAR.')
          if (nlastpsave.eq.5)THEN
           write (nio,1090) mth(nlastpsave), nlastysave
          else
           ipos=istrlen(amth(nlastpsave))
           write (nio,1091) amth(nlastpsave)(1:ipos), mth(nlastpsave),
     $                      nlastysave
          end if
         else
          ipos=istrlen(asrt(nlastpsave))
          write (nio,1091) asrt(nlastpsave)(1:ipos), srt(nlastpsave), 
     $                     nlastysave
         end if
         write (nio,1100)'ORIGINAL SERIES', raccx
C Modified by REG, on 21 Apr 2006, to select between SEATS version 
C of table 5.3, and an alternate finite sample version
         if ( .not. Lfinit ) THEN
           write (nio,1110)'TREND-CYCLE', raccp, sdptac
           write (nio,1110)'SEASONALLY ADJUSTED SERIES', racca, sdatac
         else
           write (nio,1110)'TREND-CYCLE', raccp, vTbl53(1)*sCoef
           write (nio,1110)'SEASONALLY ADJUSTED SERIES',
     $                   racca, vTbl53(2)*sCoef
         end if
         CALL writTag(Nio,'</table>')
         CALL mkPOneLine(Nio,'@','&nbsp;')
        end if
       end if
       if (rogtable .eq. 1) then
        write (54,5402)'ACCUMULATED RATE :', raccx, raccp, racca
       end if
C
C CHECK ON THE SE COMPUTATION, SECOND METHOD,THIRD METHOD
C
C SECOND METHOD
       sdptac1 = (sigptac(nlastpsave)**2-sigptaf(nlastpsave)**2)
       sdatac1 = (sigatac(nlastpsave)**2-sigataf(nlastpsave)**2)
       if (sdptac1 .lt. ZERO) then
        sdptac1 = ZERO
       end if
       if (sdatac1 .lt. ZERO) then
        sdatac1 = ZERO
       end if
       sdptac1 = SQRT(sdptac1) * ((nlastpsave*ONE)/(mq*ONE))
       sdatac1 = SQRT(sdatac1) * ((nlastpsave*ONE)/(mq*ONE))
C THIRD METHOD
       sump = ZERO
       suma = ZERO
       do i = 1,lf-nlastpsave
        suma = suma + (psiea(lf+1-i)-psiea(lf+1-i-nlastpsave))**2
        sump = sump + (psiep(lf+1-i)-psiep(lf+1-i-nlastpsave))**2
       end do
       sdptac2 = sqf * SQRT(sump)
       sdatac2 = sqf * SQRT(suma)
       if (lam .eq. 0) then
        sdptac2 = sdptac2 * ONEHND
        sdatac2 = sdatac2 * ONEHND
       end if
      
C
C TABLE 5.4
C
       if (out.eq.0) then       
        if (lam .eq. 0) then
         CALL writTagOneLine(Nio,'h3','@',
     $                       'C. RATES OF ANNUAL GROWTH T(1,MQ)')
        else
         CALL writTagOneLine(Nio,'h3','@','C. ANNUAL GROWTH T(1,MQ)')
        end if
       end if
       lagr = mq
       call ROGEST(oz,nz,rogxmq,nrogxmq,mq,lam,lagr)
       call ROGEST(sa,nz,rogamq,nrogamq,mq,lam,lagr)
       call ROGEST(trend,nz,rogpmq,nrogpmq,mq,lam,lagr)
*       do k = 1,nrogamq
*        suma = 0.0d0
*        sump = 0.0d0
*        do j = k,lf-mq
*         suma = suma + (psiea(lf+1-j)-psiea(lf+1-j-mq))**2
*         sump = sump + (psiep(lf+1-j)-psiep(lf+1-j-mq))**2
*        end do
*        sdrevamq(k) = suma
*        sdrevpmq(k) = sump
*       end do
       vrpmq = SDRmq_p(1)**2
       vramq = SDRmq_sa(1)**2
       if (lam .eq. 0) then
        do j = 1,nrogamq
          SDRmq_p(j) =SDRmq_p(j)*100
          SDRmq_sa(j)=SDRmq_sa(j)*100
*         sdrevamq(j) = SQRT(sdrevamq(j)) * sqf * ONEHND
*         sdrevpmq(j) = SQRT(sdrevpmq(j)) * sqf * ONEHND
*        end do
*       else
*        do j = 1,nrogamq
*         sdrevamq(j) = SQRT(sdrevamq(j)) * sqf
*         sdrevpmq(j) = SQRT(sdrevpmq(j)) * sqf
        end do
       end if
       vprt = TWO * teetre(0) * (ONE-teetre(mq))
       vart = TWO * teeadj(0) * (ONE-teeadj(mq))
       suma = ZERO
       sump = ZERO
       do i = 1,mq
        suma = suma + psiea(lf+1-i)**2
        sump = sump + psiep(lf+1-i)**2
       end do
       vprt = (vprt - sump)*(sqf**2)
       vart = (vart - suma)*(sqf**2)
       vprf = vprt - vrpmq
       varf = vart - vramq
       if (out.eq.0) then
         CALL genSkip(1054)
       end if
*       if (out.eq.0) then
*        if (HTML .ne. 1) then
*         if (lam .eq. 0) then
*          write (nio,
*     $      '(/,6x,''TABLE 5.4 ESTIMATION ERROR VARIANCE :'',/,6x,
*     $      ''-------------------------------------'',/,8x,
*     $      ''Rate of annual growth T(1,MQ), not-centered and'',/,8x,
*     $      ''dated at last observation.'')')
*         else
*          write (nio,
*     $      '(/,6x,''TABLE 5.4 ESTIMATION ERROR VARIANCE :'',/,6x,
*     $      ''-------------------------------------'',/,8x,
*     $      ''Annual growth T(1,MQ), not-centered and dated '',/,8x,
*     $      ''at last observation.'')')
*         end if
*        end if
*       end if
c      end if
C
C HERE IT HAS TO BE INTRODUCED THE CODE TO RESCALE THE VALUES
C
C Modified by REG, on 21 Apr 2006, to select between SEATS version 
C of table 5.4, and an alternate finite sample version
       if ( out .eq. 1 ) then
        tmp(1) = vprf 
        tmp(2) = varf 
        tmp(3) = vrpmq 
        tmp(4) = vramq 
        tmp(5) = tmp(1) + tmp(3)
        tmp(6) = tmp(2) + tmp(4)
        if (tmp(5) .lt. 0) then  !To avoid crashes
         tmp(5)=0
        end if
        if (tmp(6) .lt. 0) then  !to avoid crashes
         tmp(6)=0 
        end if
        tmp(7) = SQRT(tmp(5))
        tmp(8) = SQRT(tmp(6))
       else
        tmp(1) = vTbl54(1)
        tmp(2) = vTbl54(2)
        tmp(3) = vTbl54(3)
        tmp(4) = vTbl54(4)
        tmp(5) = vTbl54(5)
        tmp(6) = vTbl54(6)
        if (tmp(5) .lt. 0) then  !To avoid crashes
         tmp(5)=0
        end if
        if (tmp(6) .lt. 0) then  !to avoid crashes
         tmp(6)=0 
        end if
        tmp(7) = SQRT(tmp(5))
        tmp(8) = SQRT(tmp(6))
       end if
       ifact = 0
       jfact = 0
       zz = LOG10(ABS(tmp(1)+.0000000001d0))
       sum = ABS(zz)
       do i = 2,6
        if (zz .gt. ZERO) then
         sum = ZERO
         goto 5001
        else
         zz = LOG10(ABS(tmp(i)+.0000000001d0))
         if ((ABS(zz).lt.sum) .and. (zz.lt.ZERO)) then
          sum = ABS(zz)
         end if
        end if
       end do
 5001  if (sum .gt. ONE) then
        ifact = INT(sum)
        if (ifact .gt. 9) then
         ifact = 9
        end if
       end if
       zz = LOG10(ABS(tmp(7)+.0000000001d0))
       sum = ABS(zz)
       if (zz .gt. ZERO) then
        sum = ZERO
       else
        zz = LOG10(ABS(tmp(8)+.0000000001d0))
        if ((ABS(zz).lt.sum) .and. (zz.lt.ZERO)) then
         sum = ABS(zz)
        end if
       end if
       if (sum .gt. ONE) then
        jfact = INT(sum)
        if (jfact .gt. 9) then
         jfact = 9
        end if
       end if
       kfact = 0
       zz = LOG10(ABS(tmp(1)+.0000000001d0))
       sum = zz
       do i = 2,6
        zz = LOG10(ABS(tmp(i)+.0000000001d0))
        if ((zz.gt.sum) .and. (zz.gt.ZERO)) then
         sum = zz
        end if
       end do
       if (sum .gt. 4.0d0) then
        kfact = INT(sum) - 2
       end if
       if (ifact .eq. 0) then
        ifact = -kfact
       end if
       if (out.eq.0) then
        if (ABS(ifact) .gt. 0) then
         ipos=1
         CALL itoc(ifact,str,ipos)
        END IF
        if (lam .eq. 0) then
         CALL mkTableTag(Nio,'x11','ESTIMATION ERROR VARIANCE : '//
     $          'Rate of annual growth T(1,MQ), '//
     $          'not-centered and dated at last observation.')
         if (ABS(ifact) .gt. 0) then
          CALL mkCaption(Nio,'TABLE 5.4 ESTIMATION ERROR VARIANCE :'//
     $          Cbr//'<em>Rate of annual growth T(1,MQ), '//
     $          'not-centered and dated at last observation.</em>'//
     $          Cbr//'(X  1.0D'//str(1:(ipos-1))//')')
         ELSE
          CALL mkCaption(Nio,'TABLE 5.4 ESTIMATION ERROR VARIANCE :'//
     $          Cbr//'<em>Rate of annual growth T(1,MQ), '//
     $          'not-centered and dated at last observation.</em>')
         END IF
        ELSE
         CALL mkTableTag(Nio,'x11','ESTIMATION ERROR VARIANCE : '//
     $          'Annual growth T(1,MQ), not-centered '//
     $          'and dated at last observation.')
         if (ABS(ifact) .gt. 0) then
          CALL mkCaption(Nio,'TABLE 5.4 ESTIMATION ERROR VARIANCE :'//
     $          Cbr//'<em>Annual growth T(1,MQ), not-centered '//
     $          'and dated at last observation.</em>'//
     $          Cbr//'(X  1.0D'//str(1:(ipos-1))//')')
         ELSE
          CALL mkCaption(Nio,'TABLE 5.4 ESTIMATION ERROR VARIANCE :'//
     $          Cbr//'<em>Annual growth T(1,MQ), not-centered '//
     $          'and dated at last observation.</em>')
         END IF
        ENDIF
        write(nio,1120)
 1120   FORMAT('<tr><th scope="col">CONCURRENT ESTIMATOR</th>',
     $         '<th scope="col">TREND-CYCLE</th>',
     $         '<th scope="col"><abbr title="seasonally adjusted">SA',
     $         '</abbr> SERIES</th></tr>')
        write(nio,1130)'FINAL ESTIMATION ERROR',
     $        tmp(1)*(10.0d0**ifact), tmp(2)*(10.0d0**ifact)
 1130   FORMAT('<tr><th scope="row">',a,'</th>',/,
     $        2('<td class="center">',f9.3,'</td>'),'</tr>')
        write(nio,1130)'REVISION ERROR',
     $        tmp(3)*(10.0d0**ifact), tmp(4)*(10.0d0**ifact)
        write(nio,1130)'TOTAL ESTIMATION ERROR',
     $        tmp(5)*(10.0d0**ifact), tmp(6)*(10.0d0**ifact)
        write(nio,1130)'(<abbr title="standard deviation">SD</abbr>'//
     $        ' x 1.0D'//str(1:(ipos-1))//')',tmp(7)*(10.0d0**jfact),
     $        tmp(8)*(10.0d0**jfact)
        CALL writTag(Nio,'</table>')
        CALL mkPOneLine(Nio,'@','&nbsp;')
       end if
C
C TABLE 5.5
C
       if (out.eq.0) then
        CALL genSkip(1055)
        if (lam .eq. 0) then
         CALL mkTableTag(Nio,'x11','INTERANNUAL RATE OF GROWTH :'//
     $         'Rate T(1,MQ), not-centered and dated at last '//
     $         'observation, FOR THE MOST RECENT PERIODS. This '//
     $         'rate measures the rate of growth with respect to '//
     $         '1-year ago. With standard errors. In Percent points.')
         CALL mkCaption(Nio,'TABLE 5.5 INTERANNUAL RATE OF GROWTH :'//
     $         Cbr//'<em>Rate T(1,MQ), not-centered and dated at last'//
     $         ' observation, FOR THE MOST RECENT PERIODS.'//Cbr// 
     $         'This rate measures the rate of growth with respect '//
     $         'to 1-year ago.'//Cbr//'With standard errors. '//
     $         'In Percent points.</em>')
        else
         CALL mkTableTag(Nio,'x11','INTERANNUAL RATE OF GROWTH: '//
     $         'Growth T(1,MQ), not-centered and dated at last '//
     $         'observation, FOR THE MOST RECENT PERIODS. This rate '//
     $         'measures the growth with respect to 1-year ago. '//
     $         'With standard errors.')
         CALL mkCaption(Nio,'TABLE 5.5 INTERANNUAL RATE OF GROWTH :'//
     $         Cbr//'<em>Growth T(1,MQ), not-centered and dated at '//
     $         'last observation, FOR THE MOST RECENT PERIODS.'//Cbr//
     $         'This rate measures the growth with respect to 1-year'//
     $         ' ago.'//Cbr//'With standard errors.</em>')
        end if
        CALL makColgroup(Nio,0)
        CALL makColgroup(Nio,0)
        CALL makColgroup(Nio,2)
        CALL makColgroup(Nio,2)
        write (nio,1140)Cbr
 1140   FORMAT('<thead><tr><th rowspan="2" scope="col">&nbsp;</th>',
     $   '<th rowspan="2" scope="col">ORIGINAL',a,'SERIES</th>',
     $   '<th colspan="2" scope="colgroup">TREND-CYCLE</th>',/
     $   '<th colspan="2" scope="colgroup">',
     $   '<abbr title="seasonally adjusted">SA</abbr> SERIES</th></tr>')
        write(nio,1141)
 1141   format('<tr>',2('<th scope="col">ESTIMATE</th><th scope="col">',
     $   '<abbr title="standard error">SER</abbr></th>'),/,
     $   '</tr></thead>')
        nlastper = nlastpsave
        nlastyear = nlastysave
        CALL writTag(Nio,'<tbody>')
        if (mq .eq. 12) then
         do i = 1,nrogamq
          if (nlastper.eq.5) then
           if ( .not. Lfinit ) then
            write (nio,1150)
     $          mth(nlastper), nlastyear, rogxmq(i), rogpmq(i),
     $          SDRmq_p(i), rogamq(i), SDRmq_sa(i)
           else
            write (nio,1150)
     $          mth(nlastper), nlastyear, rogxmq(i), rogpmq(i),
     $          vTreGRSE2(i)*sCoef, rogamq(i), vSeaGRSE2(i)*sCoef
           end if
          else
           ipos=istrlen(amth(nlastper))
           if ( .not. Lfinit ) then
            write (nio,1151) amth(nlastper)(1:ipos), 
     $          mth(nlastper), nlastyear, rogxmq(i), rogpmq(i),
     $          SDRmq_p(i), rogamq(i), SDRmq_sa(i)
           else
            write (nio,1151) amth(nlastper)(1:ipos), 
     $          mth(nlastper), nlastyear, rogxmq(i), rogpmq(i),
     $          vTreGRSE2(i)*sCoef, rogamq(i), vSeaGRSE2(i)*sCoef
           end if
          end if
          if (nlastper .eq. 1) then
           nlastper = mq
           nlastyear = nlastyear - 1
          else
           nlastper = nlastper - 1
          end if
         end do
        else
         do i = 1,nrogamq
          ipos=istrlen(asrt(nlastper))
          if ( .not. Lfinit ) then
           write (nio,1151) asrt(nlastper)(1:ipos), 
     $          srt(nlastper), nlastyear, rogxmq(i), rogpmq(i),
     $          SDRmq_p(i), rogamq(i), SDRmq_sa(i)
          else
           write (nio,1151) asrt(nlastper)(1:ipos), 
     $          srt(nlastper), nlastyear, rogxmq(i), rogpmq(i),
     $          vTreGRSE2(i)*sCoef, rogamq(i), vSeaGRSE2(i)*sCoef
          end if
          if (nlastper .eq. 1) then
           nlastper = mq
           nlastyear = nlastyear - 1
          else
           nlastper = nlastper - 1
          end if
         end do
        end if
        CALL writTag(Nio,'</tbody>')
        CALL writTag(Nio,'</table>')
        CALL mkPOneLine(Nio,'@','&nbsp;')
       end if
 1150  FORMAT('<tr><th scope="row">',a3,'-',i4,'</th>',/,
     $        5('<td class="center">',g11.3,'</td>'),/,'</tr>')
 1151  FORMAT('<tr><th scope="row"><abbr title="',a,'">',a3,'</abbr>-',
     $        i4,'</th>',/,
     $        5('<td class="center">',g11.3,'</td>'),/,'</tr>')
       if (rogtable .eq. 1) then
        write (54,5402)'INTERANNUAL RATE :'//Cbr//'(non_centered)',
     $                 rogxmq(1), rogpmq(1), rogamq(1)
       end if
       if (out.eq.0) then
        CALL writTag(Nio,'<ul>')
        CALL writTagOneLine(Nio,'li','@',
     $         'The annual rate of growth in table 5.5 measures '//
     $         'growth with respect to one-year ago because it is '//
     $         'not centered, the measure induces an important phase '//
     $         'effect, and can be strongly influenced by the '//
     $         'irregular and moving seasonal components. It is thus '//
     $         'a poor indicator of the present rate of annual '//
     $         'growth, useful in short-term analysis.')
        CALL writTagOneLine(Nio,'li','@',
     $         'Assessments on the present rate of annual growth '//
     $         'should be preferably be based on the centered '//
     $         'measurement of TABLE 5.6 below, which requires '//
     $         '<em>half-a-year of forecast.</em> This centering '//
     $         'minimizes phase effect and is less affected by the '//
     $         'irregular or seasonal innovations.')
        CALL writTag(Nio,'</ul>')
       end if
C
C HERE INTRODUCE THE GRAPH FOR ANNUAL GROWTH
C
*       if ((pg .eq. 0).and.(iter.eq.0).and.(out.lt.2)) then
*        kmq = mq
*        mq = 0
*        Nper2 = Nper
*        Nyer2 = Nyer
*        Nper = nlastpsave
*        Nyer = nlastysave
*CUNX#ifdef TSW
*!DEC$ IF DEFINED (TSW)
*        mq=kmq
*        reverse = 0 
*        finbucle=nrogamq-1
*        Do i=1,finbucle
*         nper=nper-1
*         if (nper.eq.0) then
*          nyer = nyer-1
*          nper = mq
*         end if
*        end do 
*        finbucle = int(nrogamq/2)  
*        Do i=1, finbucle
*         wvalue = rogxmq(i)
*         rogxmq(i) = rogxmq(nrogamq+1-i)
*         rogxmq(nrogamq+1-i) = wvalue
*         wvalue = rogpmq(i)
*         rogpmq(i) = rogpmq(nrogamq+1-i)
*         rogpmq(nrogamq+1-i) = wvalue
*         wvalue = rogamq(i)
*         rogamq(i) = rogamq(nrogamq+1-i)
*         rogamq(nrogamq+1-i) = wvalue
*        end do 
*!DEC$ end if
*CUNX#end if
*        fname = 'ROGXMQ.T'
*        subtitle = 'ANNUAL RATE-OF-GROWTH ORIGINAL SERIES'
*        call PLOTRSERIES(fname,subtitle,rogxmq,nrogamq,1,999.0d0)
*        fname = 'ROGPMQ.T'
*        subtitle = 'ANNUAL RATE-OF-GROWTH TREND-CYCLE'
*        call PLOTRSERIES(fname,subtitle,rogpmq,nrogamq,1,999.0d0)
*        fname = 'ROGAMQ.T'
*        subtitle = 'ANNUAL RATE-OF-GROWTH SA SERIES'
*        call PLOTRSERIES(fname,subtitle,rogamq,nrogamq,1,999.0d0)
*        Nper = Nper2
*        Nyer = Nyer2
*        mq = kmq
*       end if
       if ((mq.eq.4) .or. (mq.eq.6) .or. (mq.eq.12)) then
        if (lam .eq. 0) then
         rmqx1 = (oz(nz+mq/2)/oz(nz-mq/2)-ONE) * ONEHND
         rmqp1 = (trend(nz+mq/2)/trend(nz-mq/2)-ONE) * ONEHND
         rmqa1 = (sa(nz+mq/2)/sa(nz-mq/2)-ONE) * ONEHND
         rmqx2 = (oz(nz+mq/2-1)/oz(nz-mq/2-1)-ONE) * ONEHND
         rmqp2 = (trend(nz+mq/2-1)/trend(nz-mq/2-1)-ONE) * ONEHND
         rmqa2 = (sa(nz+mq/2-1)/sa(nz-mq/2-1)-ONE) * ONEHND
        else
         rmqx1 = oz(nz+mq/2) - oz(nz-mq/2)
         rmqp1 = trend(nz+mq/2) - trend(nz-mq/2)
         rmqa1 = sa(nz+mq/2) - sa(nz-mq/2)
         rmqx2 = oz(nz+mq/2-1) - oz(nz-mq/2-1)
         rmqp2 = trend(nz+mq/2-1) - trend(nz-mq/2-1)
         rmqa2 = sa(nz+mq/2-1) - sa(nz-mq/2-1)
        end if
c        sump1 = ZERO
c        sump2 = ZERO
        sumx1 = ONE
        sumx2 = ONE
        do i = 1,mq/2-1
         sumx1 = sumx1 + psitot(lf+1+i)**2
        end do
        do i = 1,mq/2-2
         sumx2 = sumx2 + psitot(lf+1+i)**2
        end do
        if (lam .eq. 0) then
         sdrmqx1 = sqf * SQRT(sumx1) * ONEHND
         sdrmqx2 = sqf * SQRT(sumx2) * ONEHND
         SDRmqC_P =100*SDRmqC_P
         SDRmqC_SA=100*SDRmqC_SA
         SDRmqC2_P =100*SDRmqC2_P
         SDRmqC2_SA=100*SDRmqC2_SA
        else
         sdrmqx1 = sqf * SQRT(sumx1)
         sdrmqx2 = sqf * SQRT(sumx2)
*         sdrmqp1 = sqf * SQRT(sump1)
*         sdrmqa1 = sqf * SQRT(suma1)
*         sdrmqp2 = sqf * SQRT(sump2)
*         sdrmqa2 = sqf * SQRT(suma2)
         end if
c        call setT112x(rmqx1)
c        call setT112t(rmqp1)
c        call setT112Sa(rmqa1)
        call setT112x(SDrmqx1)
        call setT112t(sqrt(sigptmq(2)**2+SDrmqC_P**2))
        call setT112Sa(sqrt(sigatmq(2)**2+SDRmqC_SA**2))
c        call setT112x(rmqx1)
c        call setT112t(rmqp1)
c        call setT112Sa(rmqa1)
        call setT112x(SDrmqx1)
        call setT112t(sqrt(sigptmq(2)**2+SDrmqC_P**2))
        call setT112Sa(sqrt(sigatmq(2)**2+SDRmqC_SA**2))
c
        if (out.eq.0) then
          CALL genSkip(1056)
          if (lam .eq. 0) then
           CALL mkTableTag(Nio,'x11','PRESENT RATE OF ANNUAL GROWTH :'//
     $          ' Rate T(1,MQ), centered and dated. Annual rate '//
     $          'computed as the rate of growth over the last (MQ/2) '//
     $          'observed periods and the next (MQ/2) forecasts '//
     $          'at last observed period. With associated standard '//
     $          'errors.  In Percent points.')
           CALL mkCaption(Nio,'TABLE 5.6 PRESENT RATE OF ANNUAL '//
     $          'GROWTH :'//Cbr//'<em>Rate T(1,MQ), centered and '//
     $          'dated. Annual rate computed as the rate of growth '//
     $          'over the last (MQ/2) observed periods'//Cbr//' and '//
     $          'the next (MQ/2) forecasts at last observed period'//
     $          Cbr//'With associated standard errors. In Percent '//
     $          'points.</em>')
          else
           CALL mkTableTag(Nio,'x11','PRESENT RATE OF ANNUAL GROWTH :'//
     $          ' Rate T(1,MQ), centered and dated. Annual rate '//
     $          'computed as the rate of growth over the last (MQ/2) '//
     $          'observed periods and the next (MQ/2) forecasts at '//
     $          'last observed period. With associated standard '//
     $          'errors. In Percent points.')
           CALL mkCaption(Nio,'TABLE 5.6 PRESENT RATE OF ANNUAL '//
     $          'GROWTH :'//Cbr//'<em>Rate T(1,MQ), centered and '//
     $          'dated. Annual rate computed as the rate of growth '//
     $          'over the last (MQ/2) observed periods '//Cbr//'and '//
     $          'the next (MQ/2) forecasts at last observed period.'//
     $          Cbr//'With associated standard errors. In Percent '//
     $          'points.</em>')
          end if
          write (nio,1160)Cbr
 1160     FORMAT('<tr><th>&nbsp;</th><th>&nbsp;</th>',
     $         '<th scope="col">CENTERED RATE OF',a,
     $         'ANNUAL GROWTH</th>',/,'<th scope="col">',
     $         '<abbr title="standard error">SER</abbr></th>',
     $         '<th scope="col">TSE</th></tr>')
          if (Lfinit) then
            call mkTable56Row(Nio, 'ORIGINAL SERIES', 
     &         nlastpsave, nlastysave, Mq,
     $         rmqx1, vTbl56(1,1)*sCoef, vTbl56(1,2)*sCoef, 
     $         rmqx2, vTbl56(2,1)*sCoef, vTbl56(2,2)*sCoef)
            call mkTable56Row(Nio, 'TREND-CYCLE', 
     &         nlastpsave, nlastysave, Mq, 
     $         rmqp1, vTbl56(3,1)*sCoef, vTbl56(3,2)*sCoef,
     $         rmqp2, vTbl56(4,1)*sCoef, vTbl56(4,2)*sCoef)
            call mkTable56Row(Nio, 'SEASONALLY ADJUSTED SERIES',
     &         nlastpsave, nlastysave, Mq, 
     $         rmqa1, vTbl56(5,1)*sCoef, vTbl56(5,2)*sCoef,
     $         rmqa2, vTbl56(6,1)*sCoef, vTbl56(6,2)*sCoef)
c     -----------------------------------------------------------------
          else
            call mkTable56Row(Nio, 'ORIGINAL SERIES', 
     &         nlastpsave, nlastysave, Mq,
     $         rmqx1, sdrmqx1, sdrmqx1, rmqx2, sdrmqx2, sdrmqx2)
            call mkTable56Row(Nio, 'TREND-CYCLE', 
     &         nlastpsave, nlastysave, Mq,
     $         rmqp1, SDRmqC_p, SQRT(sigptmq(2)**2+SDRmqC_P**2),
     $         rmqp2, SDRmqC2_P, SQRT(sigptmq(2)**2+SDRmqC2_P**2))
            call mkTable56Row(Nio, 'SEASONALLY ADJUSTED SERIES', 
     &         nlastpsave, nlastysave, Mq,
     $         rmqa1, SDRmqC_sa, SQRT(sigatmq(2)**2+SDRmqC_sa**2),
     $         rmqa2, SDRmqC2_sa, SQRT(sigatmq(2)**2+SDRmqC2_sa**2))
          end if
c     ------------------------------------------------------------------
        end if
        CALL writTag(Nio,'</table>')
        CALL mkPOneLine(Nio,'@','&nbsp;')
        if (rogtable .eq. 1) then
         write (54,5402)'PRESENT ANNUAL RATE :'//Cbr//'(centered)',
     $                  rmqx1, rmqp1, rmqa1
        end if
      end if
      if (out.eq.0) then
       CALL writTagOneLine(Nio,'h3','@','D. FORECAST')
      end if
      if (lam .eq. 0) then
C
C ORIGINAL SERIES
C
        tmpser(1) = (oz(nz+1)/oz(nz)-ONE) * ONEHND
        a = tmpser(1)
        tmpser(2) = sqf * ONEHND
        tmpser(3) = (oz(nz+mq)/oz(nz)-ONE) * ONEHND
        d = tmpser(3)
        sumx1 = ONE
        do i = 1,mq-1
         sumx1 = sumx1 + psitot(lf+1+i)**2
        end do
        tmpser(4) = SQRT(sumx1) * sqf * ONEHND
        tmpser(5) = (oz(nz+mq-nlastpsave)/oz(nz-nlastpsave)-ONE) *
     &               ONEHND
        g = tmpser(5)
        if (nlastpsave .eq. mq) then
         tmpser(6) = ZERO
        else
         sum1 = ONE
         do i = 1,mq-nlastpsave-1
          sum1 = sum1 + psitot(lf+1+i)**2
         end do
         tmpser(6) = SQRT(sum1) * sqf * ONEHND
        end if
C
C TREND
C
        tmptr(1) = (trend(nz+1)/trend(nz)-ONE) * ONEHND
        b = tmp(1)
        tmptr(2)=SDR1F_P*ONEHND
        tmptr(3) = (trend(nz+mq)/trend(nz)-ONE) * ONEHND
        e = tmptr(3)
        tmptr(4)=SDRmqF_P*ONEHND
        tmptr(5) =
     $    (trend(nz+mq-nlastpsave)/trend(nz-nlastpsave)-ONE) * ONEHND
        h = tmptr(5)
        tmptr(6) = SDRmqPF*ONEHND
C
C SA SERIES
C
        tmpsa(1) = (sa(nz+1)/sa(nz)-ONE) * ONEHND
        c = tmpsa(1)
        tmpsa(2)=SDR1F_SA*ONEHND
        tmpsa(3) = (sa(nz+mq)/sa(nz)-ONE) * ONEHND
        f = tmpsa(3)
        tmpsa(4)=SDRmqF_SA*ONEHND
        tmpsa(5) = (sa(nz+mq-nlastpsave)/sa(nz-nlastpsave)-ONE) * ONEHND
        o = tmpsa(5)
        tmpsa(6)=SDRmqSAF*ONEHND
       else
C
C ORIGINAL SERIES
C
        tmpser(1) = oz(nz+1) - oz(nz)
        tmpser(2) = sqf
        tmpser(3) = oz(nz+mq) - oz(nz)
        sumx1 = ONE
        do i = 1,mq-1
         sumx1 = sumx1 + psitot(lf+1+i)**2
        end do
        tmpser(4) = SQRT(sumx1) * sqf
        tmpser(5) = oz(nz+mq-nlastpsave) - oz(nz-nlastpsave)
        if (nlastpsave .eq. mq) then
         tmpser(6) = ZERO
        else
         sum1 = ONE
         do i = 1,mq-nlastpsave-1
          sum1 = sum1 + psitot(lf+1+i)**2
         end do
         tmpser(6) = SQRT(sum1) * sqf
        end if
C
C TREND
C
        tmptr(1) = trend(nz+1) - trend(nz)
        tmptr(2)=SDR1F_P
        tmptr(3) = trend(nz+mq) - trend(nz)
        tmptr(4) = SDRmqF_P
        tmptr(5) = trend(nz+mq-nlastpsave) - trend(nz-nlastpsave)
        tmptr(6) = SDRmqPF
C
C SA SERIES
C
        tmpsa(1) = sa(nz+1) - sa(nz)
        tmpsa(2) = SDR1F_SA
        tmpsa(3) = sa(nz+mq) - sa(nz)
        tmpsa(4) = SDRmqF_SA
        tmpsa(5) = sa(nz+mq-nlastpsave) - sa(nz-nlastpsave)
        tmpsa(6) = SDRmqSAF
       end if
       if (out.eq.0) then
        CALL genSkip(1057)
        CALL mkTableTag(Nio,'x11','RATES OF GROWTH FORECASTS : '//
     $                  'In Percent points.')
        CALL mkCaption(Nio,'TABLE 5.7 RATES OF GROWTH FORECASTS : '//
     $                 '<em>In Percent points.</em>')
        if(nlastpsave.eq.5)then
         write (nio,1190)Cbr, mth(nlastpsave), nlastysave
        else
         ipos = istrlen(amth(nlastpsave))
         write (nio,1191)Cbr, amth(nlastpsave)(1:ipos), mth(nlastpsave),
     $                   nlastysave
        end if
 1190   FORMAT('<tr><th>FORECAST ORIGIN :',a,a3,'-',i4,'</th>',/
     $         '<th scope="col">ORIGINAL SERIES</th>',
     $         '<th scope="col">TREND-CYCLE</th>',/
     $         '<th scope="col"><abbr title="seasonally adjusted">',
     $         'SA</abbr> SERIES</th></tr>')
 1191   FORMAT('<tr><th>FORECAST ORIGIN :',a,'<abbr title="',a,'">',a3,
     $         '</abbr>-',i4,'</th>',/
     $         '<th scope="col">ORIGINAL SERIES</th>',
     $         '<th scope="col">TREND-CYCLE</th>',/
     $         '<th scope="col"><abbr title="seasonally adjusted">',
     $         'SA</abbr> SERIES</th></tr>')
        ipos=1
        CALL itoc(mq,str,ipos)
        if ( Lfinit ) then
         write (nio,1200)
     $        'ONE-PERIOD-AHEAD FORECAST'//Cbr//'TO PERIOD RATE T(1,1)',
     $        tmpser(1), vTbl57(1,1)*sCoef, tmptr(1), vTbl57(1,2)*sCoef,
     $        tmpsa(1), vTbl57(1,3)*sCoef
         write (nio,1200)'FORECAST OF ANNUAL RATE'//Cbr//
     $        'OF GROWTH OVER THE NEXT'//Cbr//str(1:(ipos-1))//
     $        ' PERIODS (one year horizon)',
     $        tmpser(3), vTbl57(2,1)*sCoef, tmptr(3), vTbl57(2,2)*sCoef,
     $        tmpsa(3), vTbl57(2,3)*sCoef
         write (nio,1200)'FORECAST OF ANNUAL RATE'//Cbr//
     $        'OF GROWTH FOR THE PRESENT YEAR',
     $        tmpser(5), vTbl57(3,1)*sCoef, tmptr(5), vTbl57(3,2)*sCoef,
     $        tmpsa(5), vTbl57(3,3)*sCoef
        else
         write (nio,1200)'ONE-PERIOD-AHEAD FORECAST'//Cbr//
     $        'TO PERIOD RATE T(1,1)',
     $        tmpser(1),tmpser(2),tmptr(1),tmptr(2),tmpsa(1),tmpsa(2)
         write (nio,1200)'FORECAST OF ANNUAL RATE'//Cbr//
     $        'OF GROWTH OVER THE NEXT'//Cbr//
     $        str(1:(ipos-1))//' PERIODS (one year horizon)',
     $        tmpser(3),tmpser(4),tmptr(3),tmptr(4),tmpsa(3),tmpsa(4)
         write (nio,1200)'FORECAST OF ANNUAL RATE'//Cbr//
     $        'OF GROWTH FOR THE PRESENT YEAR',
     $        tmpser(5),tmpser(6),tmptr(5),tmptr(6),tmpsa(5),tmpsa(6)
        end if
        CALL writTag(Nio,'</table>')
        CALL mkPOneLine(Nio,'@','&nbsp;')
       end if
 1200  FORMAT('<tr><th scope="row">',a,'</th>',/,
     $        3('<td class="center">',g11.2,
     &        ' (<abbr title="standard error">SER</abbr>=',g11.2,
     &        ')</td>'),/,'</tr>')
C
C
C
       if (rogtable .eq. 1) then
        write (54,5402)'FORECAST OF T11 RATE :', a, b, c
        write (54,5402)'FORECAST 1-year ahead :', d, e, f
        write (54,5402)'FORECAST for present year :', g, h, o
        CALL writTag(54,'</table>')
        CALL writTag(54,'</body>')
        CALL writTag(54,'</html>')
       end if
      end
C
C
C
      subroutine ROGEST(series,nz,rog,nrog,mq,lam,lagr)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      integer kp
      parameter (kp = PFCST)
      real*8 ONE,ONEHND
      parameter (ONE=1.0d0,ONEHND=100.0d0)
C
C.. Formal Arguments ..
      integer nz,nrog,mq,lam,lagr
      real*8 series(*),rog(kp)
C
C.. Local Scalars ..
      integer j
C
C ... Executable Statements ...
C
C
      if (mq .eq. 12) then
       nrog = 36
      else if (mq .eq. 6) then
       nrog = 18
      else if (mq .eq. 4) then
       nrog = 12
      else
       nrog = 8
      end if
      if ((nrog+1) .gt. nz) then
       nrog = nz - 1
      end if
      if (lam .eq. 0) then
       do j = 1,nrog-lagr+1
        rog(j) = ((series(nz-j+1)/series(nz-j+1-lagr))-ONE) * ONEHND
       end do
      else
       do j = 1,nrog-lagr+1
        rog(j) = series(nz-j+1) - series(nz-j+1-lagr)
       end do
      end if
      nrog = nrog - lagr + 1
      end
C
C
C
      subroutine ROGSIG(s,ns,sdrev,nsdrev)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      integer kp
      parameter (kp = PFCST)
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 s(0:kp)
C.. In/Out Status: Read, Not Written ..
      integer ns
C.. In/Out Status: Maybe Read, Maybe Written ..
      real*8 sdrev(kp)
C.. In/Out Status: Not Read, Overwritten ..
      integer nsdrev
C
C.. Local Scalars ..
      integer i
c      character htmtit*120
C
C.. Intrinsic Functions ..
      intrinsic SQRT
C
C ... Executable Statements ...
C
      nsdrev = ns - 1
      do i = 0,ns-1
       sdrev(i+1) = s(i)**2 - s(ns)**2
       if (sdrev(i+1) .le. ZERO) then
        sdrev(i+1) = ZERO
       else
        sdrev(i+1) = SQRT(sdrev(i+1))
       end if
      end do
      end
C
C
C Modified by REG, on 28 Feb 2006, to add out to FINALSE parameter list.
C Modified by BCM, on 7 May 2010, to replace out with Lfinit in FINALSE
c parameter list.
      subroutine FINALSE(psiep,psiea,trend,sa,siepf,siepfl,sieaf,sieafl,
     $                   sqf,ilen,mq,lfor,lam,out,ndec)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
      INCLUDE 'revs.i'
      integer nfl
      parameter (nfl = mp*2)
C   LINES OF CODE ADDED FOR X-13A-S : 2
      DOUBLE PRECISION ZERO
      parameter (ZERO=0D0)
C   END OF CODE BLOCK 
C
C.. Formal Arguments ..
      integer ilen,mq,lfor,lam,out,ndec
      real*8 psiep(nfl),psiea(nfl),trend(mpkp),sa(mpkp),siepf(kl),
     $       siepfl(kl),sieaf(kl),sieafl(kl),sqf
C
C.. Local Scalars ..
      integer i,nfreqs,nlastper,nlastyear,npers,nse,nyers,nzs
      real*8 tempbm
      character htmtit*120
C
C.. Local Arrays ..
      real*8 siea(kl),sieal(kl),siep(kl),siepl(kl),tmp(kl),tmp1(kl)
      integer thisDate(2)
C
C.. External Calls ..
      real*8 RAIZ
      integer nblank
      external RAIZ,nblank
C      external TABLE
C
C.. Intrinsic Functions ..
      intrinsic LOG, MOD
      include 'sform.i'
      include 'stream.i'
      include 'seatop.cmn'
C
C ... Executable Statements ...
C
C
C
C BACKUP SFORM COMMON PARAMETERS
C
      nzs = Nz
      nyers = Nyer
      npers = Nper
      nfreqs = Nfreq
      nse = 5 * mq
      if (nse .gt. Nz) then
       nse = Nz
      end if
      tmp(1) = ZERO
      tmp1(1) = ZERO
      do i = 1,ilen
       tmp(1) = tmp(1) + psiep(i)*psiep(i)
       tmp1(1) = tmp1(1) + psiea(i)*psiea(i)
      end do  
      siep(1) = RAIZ(tmp(1)) * sqf 
      siea(1) = RAIZ(tmp1(1)) * sqf
      do i = 2,nse
       tempbm = (psiep(ilen+2-i)*psiep(ilen+2-i))
       tmp(i) = tmp(i-1) - tempbm
       tmp1(i) = tmp1(i-1) - (psiea(ilen+2-i)*psiea(ilen+2-i))
       siep(i) = RAIZ(tmp(i)) * sqf
       siea(i) = RAIZ(tmp1(i)) * sqf
      end do
      tmp(1) = tmp(1) + (psiep(ilen+1)*psiep(ilen+1))
      tmp1(1) = tmp1(1) + (psiea(ilen+1)*psiea(ilen+1))
      siepf(1) = RAIZ(tmp(1)) * sqf
      sieaf(1) = RAIZ(tmp1(1)) * sqf
      do i = 2,lfor
       tmp(i) = tmp(i-1) + (psiep(ilen+i)*psiep(ilen+i))
       tmp1(i) = tmp1(i-1) + (psiea(ilen+i)*psiea(ilen+i))
       siepf(i) = RAIZ(tmp(i)) * sqf
       sieaf(i) = RAIZ(tmp1(i)) * sqf
      end do
      if (lam .eq. 1) then
       if (nse .lt. Nz) then
         if (.not.Lfinit) then
          htmtit = 'STANDARD ERROR OF REVISION IN TREND-CYCLE '//
     &             'ESTIMATOR (LAST 5 YEARS)'
          CALL genSkip(1115)
         else
          htmtit = 'FINITE SAMPLE STANDARD ERROR OF REVISION IN '//
     &             'TREND-CYCLE ESTIMATOR (LAST 5 YEARS)'
          CALL genSkip(1116)
         end if
       else
         if (.not.Lfinit) then
          htmtit = 'STANDARD ERROR OF REVISION IN TREND-CYCLE '//
     &             'ESTIMATOR (LAST YEARS)'
          CALL genSkip(1117)
         else
          htmtit = 'FINITE SAMPLE STANDARD ERROR OF REVISION IN '//
     &             'TREND-CYCLE ESTIMATOR (LAST YEARS)'
          CALL genSkip(1118)
         end if
       end if
       do i = 1,nse
C Modified by REG, on 28 Feb 2006, to select between SEATS output and
C alternative standard error of revision developed by getDiag().
        if (.not.Lfinit) then
         tmp(nse-i+1) = siep(i)
        else
         tmp(i)=seRevs(i,2)
        end if
       end do
       nlastper = npers
       nlastyear = nyers
       do i = 2,Nz-nse+1
        if (MOD(nlastper,mq) .eq. 0) then
         nlastyear = nlastyear + 1
         nlastper = 0
        end if
        nlastper = nlastper + 1
       end do
       Nyer = nlastyear
       Nper = nlastper
       Nz = nse
       thisDate(1)=Nyer
       thisDate(2)=Nper
       call prttbl(thisDate,Mq,tmp,Nz,htmtit(1:nblank(htmtit)),ndec,
     &             'std.error.revision.trend')
       if (nse .lt. nzs) then
         if (Lfinit) then
          htmtit = 'STANDARD ERROR OF REVISION IN SA SERIES ESTIMATOR'//
     &             ' (LAST 5 YEARS)'
          CALL genSkip(1119)
         else
          htmtit = 'FINITE SAMPLE STANDARD ERROR OF REVISION IN '//
     &             'SA SERIES ESTIMATOR (LAST 5 YEARS)'
          CALL genSkip(1120)
         end if
       else
         if (Lfinit) then
          htmtit = 'STANDARD ERROR OF REVISION IN SA SERIES ESTIMATOR'//
     &             ' (LAST YEARS)'
          CALL genSkip(1121)
         else
          htmtit = 'FINITE SAMPLE STANDARD ERROR OF REVISION IN '//
     &             'SA SERIES ESTIMATOR (LAST YEARS)'
          CALL genSkip(1122)
         end if
       end if
       do i = 1,nse
C Modified by REG, on 28 Feb 2006, to select between SEATS output and
C alternative standard error of revision developed by getDiag().
        if (out .eq. 1) then
         tmp(nse-i+1) = siea(i)
        else
         tmp(i) = seRevs(i,1)
        end if
       end do
       thisDate(1)=Nyer
       thisDate(2)=Nper
       call prttbl(thisDate,Mq,tmp,Nz,htmtit(1:nblank(htmtit)),ndec,
     &             'std.error.revision.sa')
       Nz = nzs
      else
       do i = 1,nse
        siepl(i) = siep(i) * trend(Nz-i+1)
        sieal(i) = siea(i) * sa(Nz-i+1)
       end do
       do i = 1,lfor
        siepfl(i) = siepf(i) * trend(Nz+i)
        sieafl(i) = sieaf(i) * sa(Nz+i)
       end do
       if (nse .lt. Nz) then
         htmtit = 'STANDARD ERROR OF REVISION IN TREND-CYCLE '//
     &            'ESTIMATOR (LAST 5 YEARS)'
         CALL genSkip(1115)
       else
         htmtit = 'STANDARD ERROR OF REVISION IN TREND-CYCLE '//
     &            'ESTIMATOR (LAST YEARS)'
         CALL genSkip(1117)
       end if
       do i = 1,nse
        tmp(nse-i+1) = siepl(i)
       end do
       nlastper = npers
       nlastyear = nyers
       do i = 2,Nz-nse+1
        if (MOD(nlastper,mq) .eq. 0) then
         nlastyear = nlastyear + 1
         nlastper = 0
        end if
        nlastper = nlastper + 1
       end do
       Nyer = nlastyear
       Nper = nlastper
       Nz = nse
       thisDate(1)=Nyer
       thisDate(2)=Nper
       call prttbl(thisDate,Mq,tmp,Nz,htmtit(1:nblank(htmtit)),ndec,
     &             'std.error.revision.trend')
       if (nse .lt. nzs) then
         htmtit = 'STANDARD ERROR OF REVISION IN SA SERIES ESTIMATOR'//
     &            ' (LAST 5 YEARS)'
          CALL genSkip(1119)
       else
         htmtit = 'STANDARD ERROR OF REVISION IN SA SERIES ESTIMATOR'//
     &            ' (LAST YEARS)'
          CALL genSkip(1121)
       end if
       do i = 1,nse
        tmp(nse-i+1) = sieal(i)
       end do
       call prttbl(thisDate,Mq,tmp,Nz,htmtit(1:nblank(htmtit)),ndec,
     &             'std.error.revision.sa')
       Nz = nzs
      end if
C
C RESTORE SFORM COMMON PARAMETERS
C
      Nz = nzs
      Nyer = nyers
      Nper = npers
      Nfreq = nfreqs
      end
C
C
      subroutine NMOut(Type,Init,Lam,Imean,P,D,Q,Bp,Bd,Bq,Sqg,Mq,M,
     $           iqm,maxit,fh,noserie,Pg,Out,seas,
     $           Noadmiss,StochTD,
     $           qmax,Har,Bias,model,Noutr,
     $           Nouir,Nous,Npatd,Npareg,interp,Rsa,Fortr,Neast,
     $           epsiv,Epsphi,ta,Xl,Rmod,blqt,tmu,Phi,Th,
     $           Bphi,thlim,bthlim,crmean,hplan,hpcycle,rogtable,
     $           centrregs,statseas,units,
     $           kunits,acfe,posbphi,Nochmodel,printphtrf,
     $           tabtables,d_tabtables,psieinic,psiefin,
     $           firstobs,lastobs,HPper,maxSpect,brol,blamda,
     $           bserie,bmid,bcMark,Nz)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      integer n1
      parameter (n1 = 1)
C
C.. Formal Arguments ..
c      integer modelsumm,OutNA,iter,tramo
c      real*8 bth(3*n1)
      integer bd,bias,bp,bq,d,fh,fortr,har,hpcycle,imean,
     $        init,interp,iqm,lam,m,maxit,model,mq,
     $        neast,noadmiss,StochTD,
     $        noserie,nouir,noutr,npareg,npatd,out,
     $        p,pg,q,qmax,rogtable,rsa,statseas,units,kunits
      integer seas,sqg,type,crmean,acfe,posbphi,Nous,Nochmodel
      integer printphtrf,centrregs,Nz
      integer psieinic,psiefin
      real*8 blqt,epsiv,epsphi,hplan,rmod,
     $       ta,thlim,bthlim,tmu,xl,HPper,maxSpect,brol,blamda
      integer bserie,bmid,bcMark
      real*8 bphi(3*n1),phi(3*n1),th(3*n1)
      character tabtables*100, d_tabtables*100
      character firstobs*7,lastobs*7
C
C.. Local Scalars ..
c      character tst*80, testo*1280
      integer l_type,l_init,l_lam,l_imean,l_p,l_d,l_q,l_bp,l_bd,l_bq,
     $        l_sqg,l_mq,l_m,l_iqm,l_maxit,l_fh,l_noserie,
     $        l_pg,l_out,l_seas,l_noadmiss,l_OutNA,L_StochTD,l_iter,
     $        l_qmax,l_har,l_bias,l_tramo,l_model,l_noutr,l_nouir,
     $        l_npatd,l_npareg,l_interp,l_rsa,l_fortr,l_neast
      integer l_hpcycle,l_rogtable,l_statseas,
     $        l_units,l_kunits,l_crmean,l_acfe,l_posbphi,l_nous
      integer l_psieinic,l_psiefin
      integer l_Nochmodel,l_printphtrf,l_centrregs,l_modelsumm
      real*8 l_epsiv,l_epsphi,l_ta,l_xl,l_rmod,l_blqt,
     $       l_tmu,l_thlim,l_bthlim,l_hplan,
     $       l_HPper,l_maxSpect,l_brol,l_blamda
      integer l_bserie,l_bmid,l_bcMark
      integer CounterLine,ifail,i
      character l_tabtables*100
      character l_firstobs*7,l_lastobs*7,l_Odate*7
      integer l_Olen,l_nds,indx
C.. Added by REG on 30 Aug 2005 to create local variable l_nfixed
      integer l_nfixed
C
C.. Local Arrays ..
      real*8 l_bphi(3*n1),l_bth(3*n1),l_phi(3*n1),l_th(3*n1)
      real*8 l_DetSeas(12*n1)
C
C.. External Functions ..
c      integer ISTRLEN
c      external ISTRLEN
      include 'stream.i'
C   LINES OF CODE ADDED FOR X-13A-S : 2
c      logical dpeq
c      external dpeq
C   END OF CODE BLOCK
C
C
      CounterLine=0
C   LINES OF CODE COMMENTED FOR X-13A-S : 2
C      testo="
C      tst="
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 2
c      testo=' '
c      tst=' '
C   END OF CODE BLOCK
C.. Modified by REG on 30 Aug 2005 to add l_nfixed to NMLSTS parameter list
      call NMLSTS(l_Nochmodel,l_Type,l_Init,l_Lam,l_Imean,
     $      l_P,l_D,l_Q,l_Bp,l_Bd,l_Bq,l_Sqg,l_Mq,l_M,l_iqm,
     $      l_maxit,l_fh,l_noserie,l_Pg,l_modelsumm,l_Out,
     $      l_seas,l_Noadmiss,l_OutNA,l_StochTD,
     $      l_Iter,l_qmax,l_Har,l_Bias,l_Tramo,
     $      l_model,l_Noutr,l_Nouir,l_Nous,l_Npatd,l_Npareg,
     $      l_interp,l_Rsa,l_Fortr,l_Neast,l_epsiv,
     $      l_Epsphi,l_ta,l_Xl,l_Rmod,l_blqt,
     $      l_tmu,l_Phi,l_Th,l_Bphi,l_Bth,l_thlim,l_bthlim,
     $      l_crmean,l_hplan,l_hpcycle,l_rogtable,
     $      l_centrregs,l_statseas,l_units,l_kunits,
     $      l_acfe,l_posbphi,l_printphtrf,l_tabtables,
     $      l_psieinic,l_psiefin,
     $      l_firstobs,l_lastobs,l_HPper,l_maxSpect,
     $      l_brol,l_blamda,l_bserie,l_bmid,l_bcMark,
     $      l_Odate,l_Olen,l_DetSeas,l_nds,Nz,l_nfixed,0,ifail)
       CALL mkTableTag(65,'x11','INPUT')
       CALL mkCaption(65,'INPUT')
       indx=1
       CALL NMOuti(bd,l_bd,'bd',indx,CounterLine)
       CALL NMOuti(bias,l_bias,'bias',indx,CounterLine)
       CALL NMOuti(acfe,l_acfe,'acfe',indx,CounterLine)
       CALL NMOuti(posbphi,l_posbphi,'posbphi',indx,CounterLine)
       CALL NMOuti(printphtrf,l_printphtrf,'printphtrf',indx,
     &             CounterLine)
       CALL NMOutA(Firstobs,l_Firstobs,'Firstobs',indx,CounterLine)
       CALL NMOutA(Lastobs,l_Lastobs,'Lastobs',indx,CounterLine)
       CALL NMOuti(bp,l_bp,'bp',indx,CounterLine)
       CALL NMOuti(bq,l_bq,'bq',indx,CounterLine)
       CALL NMOuti(centrregs,l_centrregs,'centrregs',indx,CounterLine)
       CALL NMOuti(d,l_d,'d',indx,CounterLine)
       CALL NMOuti(fh,l_fh,'fh',indx,CounterLine)
       CALL NMOuti(fortr,l_fortr,'fortr',indx,CounterLine)
       CALL NMOuti(har,l_har,'har',indx,CounterLine)
       CALL NMOuti(hpcycle,l_hpcycle,'hpcycle',indx,CounterLine)
       CALL NMOuti(imean,l_imean,'imean',indx,CounterLine)
       CALL NMOuti(init,l_init,'imean',indx,CounterLine)
       CALL NMOuti(interp,l_interp,'interp',indx,CounterLine)
       CALL NMOuti(iqm,l_iqm,'iqm',indx,CounterLine)
       CALL NMOuti(lam,l_lam,'lam',indx,CounterLine)
       CALL NMOuti(m,l_m,'m',indx,CounterLine)
       CALL NMOuti(maxit,l_maxit,'maxit',indx,CounterLine)
       CALL NMOuti(model,l_model,'model',indx,CounterLine)
       CALL NMOuti(mq,l_mq,'mq',indx,CounterLine)
       CALL NMOuti(nochmodel,l_nochmodel,'nochmodel',indx,CounterLine)
       CALL NMOuti(neast,l_neast,'neast',indx,CounterLine)
       CALL NMOuti(noadmiss,l_noadmiss,'noadmiss',indx,CounterLine)
c       if (OutNA .ne. l_OutNA) then
c        write (65,'(''<th id="i_35">OutNA=</th><td headers="i_87">'',
c     $              I2,''</td>'')') OutNA
c        CounterLine=CounterLine+1
c       end if
c       if (CounterLine .eq. 5) then
c        CALL writTag(65,'</tr><tr>')
c        CounterLine=0
c       end if
       CALL NMOuti(StochTD,l_StochTD,'StochTD',indx,CounterLine)
       CALL NMOuti(noserie,l_noserie,'noserie',indx,CounterLine)
       CALL NMOuti(nouir,l_nouir,'nouir',indx,CounterLine)
       CALL NMOuti(nous,l_nous,'nous',indx,CounterLine)
       CALL NMOuti(noutr,l_noutr,'noutr',indx,CounterLine)
       CALL NMOuti(npareg,l_npareg,'npareg',indx,CounterLine)
       CALL NMOuti(npatd,l_npatd,'npatd',indx,CounterLine)
       CALL NMOuti(out,l_out,'nout',indx,CounterLine)
       CALL NMOutA(tabtables,d_tabtables,'tabtables',indx,CounterLine)
       CALL NMOuti(p,l_p,'p',indx,CounterLine)
       CALL NMOuti(pg,l_pg,'pg',indx,CounterLine)
       CALL NMOuti(q,l_q,'q',indx,CounterLine)
       CALL NMOuti(qmax,l_qmax,'qmax',indx,CounterLine)
       CALL NMOuti(rogtable,l_rogtable,'rogtable',indx,CounterLine)
       CALL NMOuti(rsa,l_rsa,'rsa',indx,CounterLine)
       CALL NMOuti(statseas,l_statseas,'statseas',indx,CounterLine)
       CALL NMOuti(units,l_units,'units',indx,CounterLine)
       CALL NMOuti(kunits,l_kunits,'kunits',indx,CounterLine)
       CALL NMOuti(seas,l_seas,'seas',indx,CounterLine)
       CALL NMOuti(sqg,l_sqg,'sqg',indx,CounterLine)
       CALL NMOuti(sqg,l_sqg,'sqg',indx,CounterLine)
*       if (tramo .ne. l_tramo) then
*        write (65,'(''<th id="i_57">tramo=</th><td headers="i_57">'',
*     $              I2,''</td>'')') tramo
*        CounterLine=CounterLine+1
*       end if
*       if (CounterLine .eq. 5) then
*        CALL writTag(65,'</tr><tr>')
*        CounterLine=0
*       end if
       CALL NMOuti(type,l_type,'type',indx,CounterLine)
       CALL NMOutDP(blqt,l_blqt,'blqt',indx,CounterLine)
       CALL NMOuti(crmean,l_crmean,'crmean',indx,CounterLine)
       CALL NMOutDP(epsiv,l_epsiv,'epsiv',indx,CounterLine)
       CALL NMOutDP(epsphi,l_epsphi,'epsphi',indx,CounterLine)
       CALL NMOutDP(hplan,l_hplan,'hplan',indx,CounterLine)
       CALL NMOutDP(hpper,l_hpper,'hpper',indx,CounterLine)
       CALL NMOutDP(rmod,l_rmod,'rmod',indx,CounterLine)
       CALL NMOutDP(ta,l_ta,'ta',indx,CounterLine)
       CALL NMOutDP(thlim,l_thlim,'thlim',indx,CounterLine)
       CALL NMOutDP(bthlim,l_bthlim,'bthlim',indx,CounterLine)
       CALL NMOutDP(tmu,l_tmu,'tmu',indx,CounterLine)
       CALL NMOutDP(xl,l_xl,'xl',indx,CounterLine)
       CALL NMOutDP(thlim,l_thlim,'thlim',indx,CounterLine)
       do i=1,3
         CALL NMOutDPi(phi(i),l_phi(i),'phi',i,indx,CounterLine)
       end do
       do i=1,3
         CALL NMOutDPi(th(i),l_th(i),'th',i,indx,CounterLine)
       end do
       CALL NMOutDPi(bphi(1),l_bphi(1),'bphi',1,indx,CounterLine)
       CALL NMOuti(psieinic,l_psieinic,'psieinic',indx,CounterLine)
       CALL NMOuti(psiefin,l_psiefin,'psiefin',indx,CounterLine)
       CALL NMOutDP(MaxSpect,l_MaxSpect,'MaxSpect',indx,CounterLine)
       CALL NMOutDP(brol,l_brol,'brol',indx,CounterLine)
       CALL NMOutDP(blamda,l_blamda,'blamda',indx,CounterLine)
       CALL NMOuti(bserie,l_bserie,'bserie',indx,CounterLine)
       CALL NMOuti(bmid,l_bmid,'bmid',indx,CounterLine)
       CALL NMOuti(bcMark,l_bcMark,'bcMark',indx,CounterLine)
       if (CounterLine .gt. 0) then
        DO i=1,5-CounterLine
         CALL mkTableCell(65,'@','&nbsp;')
         CALL mkTableCell(65,'@','&nbsp;')
        END DO
        CALL writTag(65,'</tr>')
       endif
       CALL writTag(65,'</table>')
       CALL mkPOneLine(65,'@','&nbsp;')
      return
      end
CC
C
CC
      subroutine NMOutI(ivar,lvar,cvar,indx,CounterLine)
      implicit none
      integer ivar,lvar,indx,CounterLine
      character cvar*(*)
      if (ivar .ne. lvar) then
        CounterLine=CounterLine+1
        if (CounterLine .eq. 1) then
          CALL writTag(65,'<tr>')
        end if
        write (65,6501)indx,cvar,indx,ivar
        indx=indx+1
        if (CounterLine .eq. 5) then
          CALL writTag(65,'</tr>')
          CounterLine=0
        endif
      endif
 6501 format('<th id="i_',i4.4,'">',a,'=</th><td headers="i_',i4.4,
     $       '" class="center">',I5,'</td>')
      return
      end
CC
C
CC
      subroutine NMOutA(avar,lvar,cvar,indx,CounterLine)
      implicit none
      integer indx,CounterLine
      character avar*(*),cvar*(*),lvar*(*)
      if (avar .ne. lvar) then
        CounterLine=CounterLine+1
        if (CounterLine .eq. 1) then
          CALL writTag(65,'<tr>')
        end if
        write (65,6501)indx,cvar,indx,avar
        indx=indx+1
        if (CounterLine .eq. 5) then
          CALL writTag(65,'</tr>')
          CounterLine=0
        endif
      endif
 6501 format('<th id="i_',i4.4,'">',a,'=</th><td headers="i_',i4.4,
     $       '" class="center">',A,'</td>')
      return
      end
CC
C
CC
      subroutine NMOutDP(dvar,lvar,cvar,indx,CounterLine)
      implicit none
      double precision dvar,lvar
      integer indx,CounterLine
      character cvar*(*)
C   LINES OF CODE ADDED FOR X-13A-S : 2
      logical dpeq
      external dpeq
C   END OF CODE BLOCK
      if (.not.dpeq(dvar,lvar)) then
        CounterLine=CounterLine+1
        if (CounterLine .eq. 1) then
          CALL writTag(65,'<tr>')
        end if
        write (65,6502)indx,cvar,indx,dvar
        indx=indx+1
        if (CounterLine .eq. 5) then
          CALL writTag(65,'</tr>')
          CounterLine=0
        endif
      endif
 6502 format('<th id="i_',i4.4,'">',a,'=</th><td headers="i_',i4.4,
     $       '">',f8.3,'</td>')
      return
      end
CC
C
CC
      subroutine NMOutDPi(dvar,lvar,cvar,i,indx,CounterLine)
      implicit none
      double precision dvar,lvar
      integer i,indx,CounterLine
      character cvar*(*)
C   LINES OF CODE ADDED FOR X-13A-S : 2
      logical dpeq
      external dpeq
C   END OF CODE BLOCK
      if (.not.dpeq(dvar,lvar)) then
        CounterLine=CounterLine+1
        if (CounterLine .eq. 1) then
          CALL writTag(65,'<tr>')
        end if
        write (65,6503)indx,cvar,i,indx,dvar
        indx=indx+1
        if (CounterLine .eq. 5) then
          CALL writTag(65,'</tr>')
          CounterLine=0
        endif
      endif
 6503  format('<th id="i_',i4.4,'">',a,'(',i1,')=</th><td headers="i_',
     $        i4.4,'">',f8.3,'</td>')
      return
      end
CC
C
CC
      subroutine NMCHECK (Type,Init,Lam,Imean,P,D,Q,Bp,Bd,Bq,Sqg,Mq,M,
     $           iqm,maxit,fh,noserie,Pg,Out,seas,
     $           Noadmiss,OutNA,StochTD,
     $           Iter,qmax,Har,Bias,Tramo,model,Noutr,Nouir,
     $           Nous,Npatd,Npareg,interp,Rsa,Fortr,Neast,
     $           epsiv,Epsphi,Xl,Rmod,thlim,bthlim,crmean,hplan,hpcycle,
     $           rogtable,centrregs,statseas,units,
     $           acfe,posbphi,nochmodel,
     $           tabtables,d_tabtables,psieinic,psiefin,
     $           firstobs,lastobs,HPper,brol,blamda,
     $           bserie,bmid,bcMark,Nz)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      integer n1
      logical T,F
      parameter (n1 = 1, T = .true., F = .false.)
      real*8 ZERO,ONE,TWO,ONEHND,MONE,TEN
      parameter (ZERO=0.0d0,ONE=1.0d0,TWO=2.0d0,ONEHND=100.0d0,
     &           MONE=-1.0d0,TEN=10.0d0)
C.. Formal Arguments ..
c      integer modelsumm,kunits,printphtrf
c      real*8 blqt,ta,tmu,maxSpect
c      real*8 bphi(3*n1),bth(3*n1),phi(3*n1),th(3*n1)
      integer bd,bias,bp,bq,d,fh,fortr,har,hpcycle,imean,
     $        init,interp,iqm,iter,lam,m,maxit,model,mq,
     $        neast,noadmiss,OutNA,StochTD,
     $        noserie,nouir,noutr,npareg,npatd,out,
     $        p,pg,q,qmax,rogtable,rsa,statseas,units
      integer seas,sqg,tramo,type,crmean,Nous,acfe,posbphi
      integer nochmodel,centrregs
      integer psieinic,psiefin,Nz
      real*8 epsiv,epsphi,hplan,rmod,thlim,bthlim,xl,HPper
      character tabtables*100, d_tabtables*100
      character firstobs*7,lastobs*7
      integer lobs
      real*8 brol,blamda
      integer bserie,bmid,bcMark
C
C.. Local Scalars ..
c      real*8 l_ur
c      integer i
      real*8 perTolan,wpi 
      integer l_type,l_init,l_lam,l_imean,l_p,l_d,l_q,l_bp,l_bd,l_bq,
     $        l_sqg,l_mq,l_m,l_iqm,l_maxit,l_fh,l_noserie,
     $        l_pg,l_out,l_seas,l_noadmiss,l_OutNA,l_stochTD,l_iter,
     $        l_qmax,l_har,l_bias,l_tramo,l_model,l_noutr,l_nouir,
     $        l_npatd,l_npareg,l_interp,l_rsa,l_fortr,l_neast
      integer l_hpcycle,l_rogtable,l_statseas,
     $        l_units,l_kunits,l_crmean,l_acfe,l_posbphi,l_Nous,ifail
      integer l_nochmodel,l_printphtrf,l_centrregs
      integer l_psieinic,l_psiefin
      real*8 l_epsiv,l_epsphi,l_ta,l_xl,l_ur,l_rmod,l_blqt,
     $       l_tmu,l_thlim,l_bthlim,l_hplan,l_HPper,l_maxSpect
      character l_tabtables*100
      character l_firstobs*7,l_lastobs*7,l_Odate*7
      integer l_Olen,l_nds
      real*8 l_brol,l_blamda
      integer l_bserie,l_bmid,l_bcMark
      integer l_modelsumm
C.. Added by REG on 30 Aug 2005 to create local variable l_nfixed
      integer l_nfixed
C
C.. Local Arrays ..
      real*8 l_bphi(3*n1),l_bth(3*n1),l_phi(3*n1),l_th(3*n1)
      real*8 l_DetSeas(12*n1)
      integer ValidTables
      external ValidTables
      integer Date2Idx
      external Date2Idx
      character*7 Idx2Date
      external Idx2Date
      logical dpeq
      external dpeq
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'htmlout.cmn'
      include 'stream.i'
c-----------------------------------------------------------------------
      parameter (wpi = 3.14159265358979d0)
C
C
C.. Modified by REG on 30 Aug 2005 to add l_nfixed to NMLSTS parameter list
      call NMLSTS(l_Nochmodel,l_Type,l_Init,l_Lam,l_Imean,
     $       l_P,l_D,l_Q,l_Bp,l_Bd,l_Bq,l_Sqg,l_Mq,l_M,l_iqm,
     $       l_maxit,l_fh,l_noserie,l_Pg,l_modelsumm,l_Out,l_seas,
     $       l_Noadmiss,l_OutNA,l_stochTD,
     $       l_Iter,l_qmax,l_Har,l_Bias,l_Tramo,
     $       l_model,l_Noutr,l_Nouir,l_Nous,l_Npatd,l_Npareg,
     $       l_interp,l_Rsa,l_Fortr,l_Neast,l_epsiv,
     $       l_Epsphi,l_ta,l_Xl,l_Rmod,l_blqt,
     $       l_tmu,l_Phi,l_Th,l_Bphi,l_Bth,l_thlim,l_bthlim,
     $       l_crmean,l_hplan,l_hpcycle,l_rogtable,
     $       l_centrregs,l_statseas,l_units,
     $       l_kunits,l_acfe,l_posbphi,l_printphtrf,
     $       l_tabtables,l_psieinic,l_psiefin,
     $       l_firstobs,l_lastobs,l_HPper,l_maxSpect,
     $       l_brol,l_blamda,l_bserie,l_bmid,l_bcMark,
     $       l_Odate,l_Olen,l_DetSeas,l_nds,Nz,l_nfixed,0,
     $       ifail)
cc       
c      
       if ((acfe .lt. 0) .or. (acfe .gt. 999)) then
        CALL CheckMSG(Nio,'ACFE','[0, 999]')
        acfe=l_acfe
       endif
       if ((posbphi .lt. 0) .or. (posbphi .gt. 999)) then
        CALL CheckMSG(Nio,'POSBPHI','[0, 1]')
        posbphi=l_posbphi
       endif
       lobs = Date2Idx(Firstobs)
       if (lobs .eq. -1) then 
        lobs = 1
       end if
       if (lobs .lt. 0) then
        CALL CheckMSG(Nio,'Firstobs','['//Idx2Date(1)//', '//
     &                Idx2Date(2)//']')
        Firstobs=l_Firstobs
       end if
       lobs=Date2Idx(Lastobs)
       if (lobs.gt.Nz) then
        CALL CheckMSG(Nio,'Lastobs','['//Idx2Date(1)//', '//
     &                Idx2Date(2)//']')
        Lastobs=l_Lastobs
       end if
       if ((Date2Idx(Firstobs) .ge. Date2Idx(Lastobs)) .and. 
     &     (Date2Idx(lastobs) .ne. -1))then 
        CALL CheckMSG(Nio,'Firstobs","Lastobs','['//Idx2Date(1)//', '//
     &                Idx2Date(2)//']')
        Firstobs=l_Firstobs
        Lastobs=l_Lastobs
       end if
       if ((bd .ne. 0) .and. (bd .ne. 1)) then
        CALL CheckMSG(Nio,'BD','[0, 1]')
        bd=l_bd
       end if
       if ((bias .lt. -1) .or. (bias .gt. 1)) then
        CALL CheckMSG(Nio,'BIAS','[-1, 0, 1]')
        bias=l_bias
       end if
       if ((bp .ne. 0) .and. (bp .ne. 1)) then
        CALL CheckMSG(Nio,'BP','[0, 1]')
        bp=l_bp
       end if
       if ((bq .ne. 0) .and. (bq .ne. 1)) then
        CALL CheckMSG(Nio,'BQ','[0, 1]')
        bq=l_bq
       end if
       if ((centrregs .ne. 0) .and. (centrregs .ne. 1)) then
        CALL CheckMSG(Nio,'CENTERREGS','[0, 1]')
        centrregs=l_centrregs
       end if
       if ((d .lt. 0) .or. (d .gt. 3)) then
        CALL CheckMSG(Nio,'D','0 &#8804; d &#8804; 3')
        d=l_d
       end if
       if (fh .lt. 0) then
        CALL CheckMSG(Nio,'FH','fh &gt; 0')
        fh=l_fh
       end if
       if ((fortr .ne. 0) .and. (fortr .ne. 1)) then
        CALL CheckMSG(Nio,'FORTR','[0, 1]')
        fortr=l_fortr
       end if
       if ((har .ne. 0) .and. (har .ne. 1)) then
        CALL CheckMSG(Nio,'HAR','[0, 1]')
        har=l_har
       end if
       if ((hpcycle .lt. -1) .or. (hpcycle .gt.3)) then
        CALL CheckMSG(Nio,'HPCYCLE','[-1, 0, 1, 2, 3]')
        hpcycle=l_hpcycle
       end if
       if (hplan .lt. 0.0625d0 .and. (.not.dpeq(hpLan,l_hplan))) then 
        CALL CheckMSG(Nio,'HPLAN',' &gt; 0.0625')
        hplan=l_hplan
       end if
       if (hpPer .lt. TWO .and. (.not.dpeq(hpPer, l_hpPer))) then 
        CALL CheckMSG(Nio,'HPPER',' &gt; 2.0')
        hpper=l_hpPer
       end if
       if ((HPlan .ge. 0.0625d0) .and. (HPper.gt.TWO)) then 
        perTolan=1/(4*(1-(cos(2*wpi/HPper)) **2)) 
        if (abs(HPlan-perTolan).lt.TEN**(-8)) then
         CALL wWritln('You have to choose between setting "HPper"'//Cbr,
     &                Nio,0,T,F)
         CALL writln(' &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; or setting '//
     &               '"HPlan", you cannot set both.'//Cbr,Nio,0,F,F)
         CALL writln(' &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; HPlan set to '//
     &               'the default value.'')',Nio,0,F,T)
         hpLan=l_hpLan
        end if  
       end if
       if ((imean .ne. 0) .and. (imean .ne.1)) then
        CALL CheckMSG(Nio,'IMEAN','[0, 1]')
        imean=l_imean
       end if
       if ((init .ne. 0) .and. (init .ne. 1) .and. (init .ne.2)) then
        CALL CheckMSG(Nio,'INIT','[0, 1, 2]')
        init=l_init
       end if
       if ((interp .ne. 0) .and. (interp .ne. 1) .and. (interp .ne.2))
     &                                                            then
        CALL CheckMSG(Nio,'INTERP','[0, 1, 2]')
        interp=l_interp
       end if
       if (iqm .lt. 0) then
        CALL CheckMSG(Nio,'IQM','iqm &#8805; 0')
        iqm=l_iqm
       end if
       if ((iter .lt. 0) .or. (iter .gt. 3)) then
        CALL CheckMSG(Nio,'ITER','[0, 1, 2, 3]')
        iter=l_iter
       end if
       if ((lam .ne. 0) .and. (lam .ne. 1)) then
        CALL CheckMSG(Nio,'LAM','[0, 1]')
        lam=l_lam
       end if
       if ((nochmodel .ne. 0) .and. (nochmodel .ne. 1)) then
        CALL CheckMSG(Nio,'NOCHMODEL','[0, 1]')
        nochmodel=l_Nochmodel
       end if
       if (m .lt. 0) then
        CALL CheckMSG(Nio,'M','m &#8805; 0')
        m=l_m
       end if
       if (maxit .lt. 1) then
        CALL CheckMSG(Nio,'MAXIT','maxit &#8805; 0')
        maxit=l_maxit
       end if
       if ((model .ne. 0) .and. (model .ne. 1)) then
        CALL CheckMSG(Nio,'MODEL','[0, 1]')
        model=l_model
       end if
       if ((mq .ne. 1) .and. (mq .ne. 2) .and. (mq .ne. 4) .and.
     &    (mq .ne. 6) .and. (mq .ne. 12)) then
        CALL CheckMSG(Nio,'MQ','[1, 2, 4, 6, 12]')
        mq=l_mq
       end if
       if ((neast .ne. 0) .and. (neast .ne. 1)) then
        CALL CheckMSG(Nio,'NEAST','[0, 1]')
        neast=l_neast
       end if
       if ((noadmiss .ne. 0) .and. (noadmiss .ne. 1).and.
     $     (noadmiss .ne.-1)) then
        CALL CheckMSG(Nio,'NOADMISS','[-1, 0, 1]')
        noadmiss=l_noadmiss
       end if
       if ((OutNA .ne. 0) .and. (OutNA .ne. 1)) then
        CALL CheckMSG(Nio,'OUTNA','[0, 1]')
        OUTNA=l_OUTNA
       end if
       if ((StochTD .ne. 0) .and. (StochTD .ne. 1).and.
     &      (StochTD.ne.-1)) then
        CALL CheckMSG(Nio,'STOCHTD','[-1, 0, 1]')
        stochTD=l_stochTD
       end if
       if ((noserie .ne. 0) .and. (noserie .ne. 1)) then
        CALL CheckMSG(Nio,'NOSERIE','[0, 1]')
        noserie=l_noserie
       end if
       if ((nouir .ne. 0) .and. (nouir .ne. 1)) then
        CALL CheckMSG(Nio,'NOUIR','[0, 1]')
        nouir=l_nouir
       end if
       if ((nous .ne. 0) .and. (nous .ne. 1)) then
        CALL CheckMSG(Nio,'NOUS','[0, 1]')
        nous=l_nous
       end if
       if ((noutr .ne. 0) .and. (noutr .ne. 1)) then
        CALL CheckMSG(Nio,'NOUTR','[0, 1]')
        noutr=l_noutr
       end if
       if ((npareg .ne. 0) .and. (npareg .ne. 1)) then
        CALL CheckMSG(Nio,'NPAREG','[0, 1]')
        npareg=l_npareg
       end if
       if ((npatd .ne. 0) .and. (npatd .ne. 1) .and.
     &     (npatd .ne. 2) .and. (npatd .ne. 6) .and.
     &     (npatd .ne. 7)) then
        CALL CheckMSG(Nio,'NPATD','[0, 1, 2, 6, 7]')
        npatd=l_npatd
       end if
       if ((out .lt. -1) .or. (out .gt. 3)) then
        CALL CheckMSG(Nio,'OUT','[0, 1, 2, 3]')
        out=l_out
       end if
       if (validTables(tabtables) .eq. 0) then
        CALL CheckMSG(Nio,'TABTABLES',CNOTST)
        tabtables=d_tabtables
       end if
       if ((p .lt. 0) .or. (p .gt. 3)) then
        CALL CheckMSG(Nio,'P','0 &#8804; p &#8804; 3')
        p=l_p
       end if
       if ((pg .ne. 1) .and. (pg .ne. 0)) then
        CALL CheckMSG(Nio,'PG','[0, 1]')
        pg=l_pg
       end if
       if ((q .lt. 0) .or. (q .gt. 3)) then
        CALL CheckMSG(Nio,'Q','0 &#8804; q &#8804; 3')
        q=l_q
       end if
       if (qmax .lt. 0) then
        CALL CheckMSG(Nio,'QMAX','qmax &#8805; 0')
        qmax=l_qmax
       end if
       if ((rogtable .ne. 0) .and. (rogtable .ne. 1)) then
        CALL CheckMSG(Nio,'ROGTABLE','[ 0, 1 ]')
        rogtable=l_rogtable
       end if
       if ((rsa .lt. 0) .and. (rsa .gt. 2)) then
        CALL CheckMSG(Nio,'RSA','[ 0, 1, 2 ]')
        rsa=l_rsa
       end if
       if ((statseas .ne. 0) .and. (statseas .ne. 1) .and. 
     &     (statseas.ne.-1)) then
        CALL CheckMSG(Nio,'STATSEAS','[ -1, 0, 1 ]')
        statseas=l_statseas
       end if
       if ((units .ne. 0) .and. (units .ne. 1) 
     &     .and.(units .ne. -1)) then
        CALL CheckMSG(Nio,'UNITS','[-1, 0, 1]')
        units=l_units
       end if
       if ((seas .ne. 0) .and. (seas .ne. 1)) then
        CALL CheckMSG(Nio,'SEAS','[0, 1]')
        seas=l_seas
       end if
       if ((sqg .ne. 0) .and. (sqg .ne. 1)) then
        CALL CheckMSG(Nio,'SQG','[0, 1]')
        sqg=l_sqg
       end if
       if ((tramo .lt. -1) .and. (tramo .gt. 1)) then
        CALL CheckMSG(Nio,'TRAMO','[-1, 0, 1]')
        tramo=l_tramo
       end if
       if ((type .ne. 0) .and. (type .ne. 1)) then
        CALL CheckMSG(Nio,'TYPE','[0, 1]')
        type=l_type
       end if
       if ((crmean .ne. 0) .and. (crmean .ne. 1)) then
        CALL CheckMSG(Nio,'CRMEAN','[0, 1]')
        crmean=l_crmean
       end if
       if (epsiv .le. ZERO) then
        CALL CheckMSG(Nio,'EPSIV','epsiv &gt; 0')
        epsiv=l_epsiv
       end if
       if (epsphi .lt. ZERO) then
        CALL CheckMSG(Nio,'EPSPHI','epsiv &gt; 0.0')
        epsphi=l_epsphi
       end if
       if ((rmod .lt. ZERO) .or. (rmod .gt. ONE)) then
        CALL CheckMSG(Nio,'RMOD','0.0 &#8804; rmod &#8804; 1.0')
        rmod=l_rmod
        end if
       if ((thlim .le. MONE) .or. (thlim .gt. ZERO)) then
        CALL CheckMSG(Nio,'THLIM','-1.0 &lt; thlim &lt; 0.0')
        thlim=l_thlim
       end if
       if ((bthlim .le. MONE) .or. (bthlim .gt. ZERO)) then
        CALL CheckMSG(Nio,'BTHLIM','-1.0 &lt; bthlim &lt; 0.0')
        bthlim=l_bthlim
       end if
       if ((xl .le. ZERO) .or. (xl .ge. ONE)) then
        CALL CheckMSG(Nio,'XL','0.0 &lt; xl &#8804; 1.0')
        xl=l_xl
       end if
       if ((psieinic .gt. -24) .or. (psieinic .lt. -300))then
        CALL CheckMSG(Nio,'Psieinic','[-300:-24]')
        psieinic=l_psieinic
       end if
       if ((psiefin .lt. -1) .or. (psiefin .gt. 36))then
        CALL CheckMSG(Nio,'Psiefin','[-1:36]')
        psiefin=l_psiefin
       end if
       if  ((Brol .lt. ZERO) .or. (Brol .gt. ONE)) then
        CALL CheckMSG(Nio,'Brol','[0:1.0]')
        Brol=l_Brol
       end if
       if  ((Blamda .lt. -3.0d0) .or. (Blamda .gt. 3.0d0)) then
        CALL CheckMSG(Nio,'Blamda','[-3.0:3.0]')
        Blamda=l_Blamda
       end if
       if  ((Bserie .lt. 0) .or. (Bserie .gt. 3)) then
        CALL CheckMSG(Nio,'Bserie','[0,1,2,3]')
        Bserie=l_Bserie
       end if
       if  ((BMid .ne. 0) .and. (BMid .ne. 1)) then
        CALL CheckMSG(Nio,'BMid','[0,1]')
        BMid=l_BMid
       end if
       if  ((BcMark .ne. 0) .and. (BcMark .ne. 1)) then
        CALL CheckMSG(Nio,'BcMark','[0,1]')
        BcMark=l_BcMark
       end if
      return
      end
cc
c
cc
      subroutine SEATSLOG(Infile,TotalNum)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
      character Infile*180
      integer TotalNum
C
C.. Local Scalars ..
      integer I,nrterr,nlenerr,ntrterr,ntlenerr,ntnmerr
c      real*8 tmp
C
C.. Local Arrays ..
c      integer ITnmerr(50000)
      integer Irterr(50000),Ilenerr(50000),ITrterr(50000),
     &        ITlenerr(50000)
C
C.. External Calls ..
      logical dpeq
      external dpeq
      include 'logtrace.i'
      nrterr=0
      nlenerr=0
      ntrterr=0
      ntlenerr=0
      ntnmerr=0
      do i=1,ntrace-1
       if (dpeq(Dstdres(i), -99999.99D0)) then
        nrterr=nrterr+1
        Irterr(nrterr) = i
       else if (dpeq(Dstdres(i), -88888.88D0)) then
        nlenerr=nlenerr+1
        Ilenerr(nlenerr) = i
       else if (dpeq(Dstdres(i), -11111.11D0)) then
        ntrterr=ntrterr+1
        ITrterr(ntrterr) = i
       else if (dpeq(Dstdres(i), -22222.22D0)) then
        ntlenerr=ntlenerr+1
        ITlenerr(ntlenerr) = i
       else if (dpeq(Dstdres(i), -33333.33D0)) then
        ntnmerr=ntnmerr+1
c        ITnmerr(ntnmerr) = i
       end if
      end do
      write (44,'(//)')
!DEC$ IF DEFINED (DOS)
CUNX#ifdef DOS
      write (44,'(6x,''Name of the series set: '',a)')Infile
      write (44,'(/)')
CUNX#end if
!DEC$ end if
      write (44,'(6x,''Total number of the series in the set :'',
     &       i5.5)') TotalNum
      write (44,'(//)')
      write (44,'(6x,''Number of series not treated because not '',
     & ''enough observations, too many'',/,6x,''zeros, too many '',
     & ''constant values at the end, or too many missing'',/,
     & 6x,''observations :'',i5.5)') nlenerr
      if (nlenerr .gt. 0) then
       write (44,'(/)')
       do i=1, nlenerr
         write (44,'(12x,a)') TrTitle(Ilenerr(i))(1:32)
       end do
      end if
      write (44,'(//)')
      write (44,'(6x,''Number of series that produced '',
     $       ''a Run-Time EXCEPTION :'',i5.5)')nrterr
      if (nrterr .gt. 0) then
       write (44,'(/)')
       do i=1, nrterr
         write (44,'(12x,a)') TrTitle(Irterr(i))(1:32)
       end do
      end if
      if (ntrterr .gt. 0) then
        write (44,'(//)')
        write (44,'(6x,''Number of series that produced '',
     $         ''a Run-Time EXCEPTION in TRAMO :'',i5.5)')ntrterr
        write (44,'(/)')
        do i=1, ntrterr
          write (44,'(12x,a)') TrTitle(ITrterr(i))(1:32)
        end do
      end if
      if (ntlenerr .gt. 0) then
        write (44,'(//)')
        write (44,'(6x,''Number of series not treated because not '',
     &   ''enough observations, too many'',/,6x,''zeros, too many '',
     &   ''constant values at the end, or too many missing'',/,
     &   6x,''observations in TRAMO :'',i5.5)') ntlenerr
        write (44,'(/)')
        do i=1, ntlenerr
          write (44,'(12x,a)') TrTitle(ITlenerr(i))(1:32)
        end do
      end if
      end
cc
c
cc
CC
C Return the number of token in the string.
C The valid token separator are blank,comma,tab
CC
      integer function GetTokenNum(Line)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      character*(*) Line
C
C.. Local Scalars ..
      integer i,numtok,intok
C
C.. External Functions ..
      integer ISTRLEN
      logical IsSeparator
      external ISTRLEN,IsSeparator
      numtok = 0
      intok = 0
      do i=1, ISTRLEN(Line)
       if ((intok .eq. 0) .and. .not. IsSeparator(Line(i:i))
     &      ) then
        numtok = numtok + 1
        intok = 1
       end if
       if (IsSeparator(Line(i:i))) then
        intok = 0
       end if
      end do
      GetTokenNum = numtok
      return
      end
cc
c Return the Token(index) in the string Line
c If index &gt; GetTokenNum return a void string
cc
      character*(*) function GetTokenidx(Line,index)
C.. Implicits ..
      implicit none
C.. Parameters ..
      integer index
      character*(*) Line
C.. Local Scalars ..
c      integer iend
      integer i,numtok,intok,istart,LineLen
C.. Local Arrays ..
      character*(1000) LocLine
C.. External Functions ..
      integer ISTRLEN
      logical IsSeparator
      external ISTRLEN,IsSeparator
      LocLine = Line
      numtok = 0
      intok = 0
      GetTokenidx = ''
      istart = 0
      LineLen = ISTRLEN(LocLine)
      if ((ichar(LocLine(LineLen+1:LineLen+1)) .ne. 9) .and.
     &    (ichar(LocLine(LineLen+1:LineLen+1)) .ne. 44)) then
      LocLine(LineLen+1:LineLen+1) = ','
      LineLen = LineLen + 1
      end if
      do i=1, LineLen
       if ((intok .eq. 0) .and. .not. IsSeparator(LocLine(i:i))
     &      ) then
        numtok = numtok + 1
        intok = 1
       end if
       if (IsSeparator(LocLine(i:i))) then
        intok = 0
       end if
       if ((numtok .eq. index) .and. (istart .eq. 0)) then
        istart = i
       end if
       if ((numtok .eq. index) .and. (intok .eq. 0)) then
        GetTokenidx = LocLine(istart:i-1)
        return
       end if
      end do
      return
      end
cc
c Return 1 if the syntax of the tabtablet is ok
cc
      integer function  Validtables(tabtables)
      implicit none
      character*100 tabtables
C.. Local Scalars ..
      character*100 GetTokenidx
      integer nTokens,tokenLen,i
      character token*100
C.. External Functions ..
      external GetTokenNum,GetTokenidx,istrlen
      integer GetTokenNum,istrlen
c...   
      nTokens=GetTokenNum(tabtables)
      do i=1,nTokens
       token= GetTokenidx(tabtables,i)
       tokenLen=istrlen(token)
       if ('all'.ne. token(1:tokenLen) .and.
     &     'xo' .ne. token(1:tokenLen) .and.
     &     'p' .ne. token(1:tokenLen) .and.
     &     'n' .ne. token(1:tokenLen) .and.
     &     's' .ne. token(1:tokenLen) .and.
     &     'cal' .ne. token(1:tokenLen) .and.
     &     'uc' .ne. token(1:tokenLen) .and.
     &     'pa' .ne. token(1:tokenLen) .and.     
     &     'cy' .ne. token(1:tokenLen) .and.
     &     'ltp' .ne. token(1:tokenLen) .and.
     &     'er' .ne. token(1:tokenLen) .and.
     &     'rg0' .ne. token(1:tokenLen) .and.
     &     'rgsa' .ne. token(1:tokenLen) .and.
     &     'stp' .ne. token(1:tokenLen) .and.
     &     'stn' .ne. token(1:tokenLen)  ) then
        Validtables=0
        return
       end if
      end do
      Validtables=1
      return
      end
cc
c
cc
      integer function  IsSubstr (str,substr)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
      character*(*) substr
      character*100 str
C
C.. Local Scalars ..
c      integer i,imat
      integer l1,l2,j
      logical again
C.. External Functions ..
      external istrlen,IsSeparator
      integer istrlen
      logical IsSeparator
c...   
      IsSubstr = 0
      l1 = max(1,istrlen(str))
      l2 = istrlen(substr)
c      imat = 0
      j = 1
      again = .true.
      do while (j .le. l1) 
       do while ((j .le. l1) .and. (again))
        if (str(j:j) .eq. substr(1:1)) then
         again = .false.
         if (j .gt. 1) then
          again = .not. IsSeparator (str(j-1:j-1))
          if (again) then
           J = J + 1
          end if
         end if
        else
         j = j + 1
        end if
       enddo
       if ((str(j:j+l2-1) .eq. substr(1:l2)) .and.
     &     IsSeparator(str(j+l2:j+l2))) then
         IsSubstr = 1
         return
       else
         j = j + 1
         again = .true.
       end if
      enddo
      return
      end
cc
c
cc
      logical function IsSeparator(char)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      character char
C
C..
      IsSeparator = ((ichar(char).eq.9) .or. 
     &               (ichar(char).eq.44) .or. 
     &                (char .eq. ' '))
      return
      end
cc
c
cc
cc
c
cc
      subroutine ProcTables(tabtables)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
      character tabtables*100
C
C.. Local Scalars ..
      include 'prtous.i'
C.. External Functions ..
      external IsSubstr
      integer IsSubstr
c...   
C..
      xotab = 1
      ptab = 1
      ntab = 1
      stab = 1
      caltab = 1
      patab = 1
      cytab = 1
      ltptab = 1
      ertab = 1
      rg0tab = 1
      rgsatab = 1
      stptab =1
      stntab =1
      utab=1
      ctab=1
      rtptab=0
      rtsatab=0
      if (IsSubstr(tabtables,'all') .eq. 0) then
       xotab = IsSubstr(tabtables,'xo')   
       ptab = IsSubstr(tabtables,'p')
       ntab = IsSubstr(tabtables,'n')
       stab = IsSubstr(tabtables,'s')
       caltab = IsSubstr(tabtables,'cal')
       patab = IsSubstr(tabtables,'pa')
       cytab = IsSubstr(tabtables,'cy')
       ltptab = IsSubstr(tabtables,'ltp')
       ertab = IsSubstr(tabtables,'er')
       rg0tab = IsSubstr(tabtables,'rg0')
       rgsatab = IsSubstr(tabtables,'rgsa')
       stptab = IsSubstr(tabtables,'stp')
       stntab = IsSubstr(tabtables,'stn')
       utab = IsSubstr(tabtables,'u')
       ctab = IsSubstr(tabtables,'c')
       rtptab=IsSubstr(tabtables,'rtp')
       rtsatab=IsSubstr(tabtables,'rtsa')
      end if
      return
      end
c
c
c
      integer function Date2Idx(strdate)
        implicit none
        character*7 strdate
        character*2 tok1
        character*4 tok2
        integer period,year,retval
        integer lper,lyear,idx
        include 'date.i'
        logical IsInteger
        external IsInteger
        retval=-1
        tok1=strdate(1:2)
        if (.not. IsInteger(tok1)) then
          Date2Idx=retval
            return
        end if
        read (tok1,'(i2)') period
        tok2=strdate(4:7)
        if (.not. IsInteger(tok2)) then
          Date2Idx=retval
          return
        end if
        read (tok2,'(i4)') year
        idx=1
        lper=Dperiod
        lyear=Dyear
        do while ((idx.le.Dlen).and.
     $         ((lper.ne.period).or.(lyear.ne.year)))
         idx=idx+1
         lper=lper+1
         if (lper .gt. Dfreq) then
           lper=1
           lyear=lyear+1
         end if
        end do
        if (idx.le.Dlen) then
          retval=idx
        end if
        Date2Idx=retval
        return
      end
c
c
c
      character*7 function Idx2Date(idx)
        implicit none
        integer idx
        character*7 strdate
        integer k,sp,sy
        include 'date.i'
        strdate='00-0000'
        sp=Dperiod
        sy=Dyear
        if (idx .gt. Dlen) then
          Idx2Date=strdate
          return
        end if
        do k=2,idx
         sp=sp+1
         if (sp .gt. Dfreq) then
          sp=1
          sy=sy+1
         end if
        enddo
        write (strdate,'(i2.2,"-",i4.4)')sp,sy
        Idx2Date=strdate
        return
      end
cc
c
cc
      logical function isInteger (Txt)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      character*(*) Txt
C
C.. Local Scalars ..
      integer iflag,Inum
      read (txt,'(i12)',iostat=iflag) Inum
      if (iflag .gt. 0) then
        isInteger = .false.
      else
        isInteger = .true.
      end if
      return
      end
cc
c
cc
      integer function LostB()
        implicit none
        character*2 tok1
        character*4 tok2
        integer period,year,retval
        integer lper,lyear,idx
        integer OCommDate,ACommDate
        include 'date.i'
        logical IsInteger
        external IsInteger
        retval=0
        tok1=Odate(1:2)
        if (.not. IsInteger(tok1)) then
          LostB=retval
            return
        end if
        read (tok1,'(i2)') period
        tok2=Odate(4:7)
        if (.not. IsInteger(tok2)) then
          LostB=retval
          return
        end if
        read (tok2,'(i4)') year
        OCommDate=year*100+period
        ACommDate=Dyear*100+Dperiod
        if ((OCommDate .eq. 0) .or. (ACommDate .eq.0)) then
          retval=0
          LostB=retval
          return
        end if
        if (OCommDate .ge. ACommDate) then
          retval=0
          LostB=retval
          return
        end if
        idx=1
        lper=period
        lyear=year
        do while ((lper.ne.Dperiod).or.(lyear.ne.Dyear))
         idx=idx+1
         lper=lper+1
         if (lper .gt. Dfreq) then
           lper=1
           lyear=lyear+1
         end if
        end do
        retval=idx-1
        LostB=retval
        return
      end
cc
c
cc
      integer function LostE()
        implicit none
        character*2 tok1
        character*4 tok2
        integer period,year,retval
c        integer llper,llyear
        integer lper,lyear,idx
        integer OCommDate,ACommDate
        include 'date.i'
        logical IsInteger
        external IsInteger
        retval=0
        tok1=Odate(1:2)
        if (.not. IsInteger(tok1)) then
          LostE=retval
            return
        end if
        read (tok1,'(i2)') period
        tok2=Odate(4:7)
        if (.not. IsInteger(tok2)) then
          LostE=retval
          return
        end if
        read (tok2,'(i4)') year
        idx=1
        do while (idx .lt. Olen) 
         idx=idx+1
         period=period+1
         if (period .gt. Dfreq) then
           period=1
           year=year+1
         end if
        end do
        OCommDate=year*100+period
        lper=Dperiod
        lyear=Dyear
        idx=1
        do while (idx .lt. Dlen) 
         idx=idx+1
         lper=lper+1
         if (lper .gt. Dfreq) then
           lper=1
           lyear=lyear+1
         end if
        end do
        ACommDate=lyear*100+lper
        if ((OCommDate .eq. 0) .or. (ACommDate .eq.0)) then
          retval=0
          LostE=retval
          return
        end if
        if (ACommDate .ge. OCommDate) then
          retval=0
          LostE=retval
          return
        end if
        idx=1
        do while ((lper.ne.period).or.(lyear.ne.year))
         idx=idx+1
         lper=lper+1
         if (lper .gt. Dfreq) then
           lper=1
           lyear=lyear+1
         end if
        end do
        retval=idx-1
        LostE=retval
        return
      end
cc
c
cc
      Subroutine Index2Date(index,sp,sy,nper,nyear,nfreq,lenx)
        implicit none
        integer index,sp,sy,nper,nyear,nfreq,lenx
        integer k
        sp=nper
        sy=nyear
        if (index .gt. lenx) then
          return
        end if
        do k=2,index
         sp=sp+1
         if (sp .gt. nfreq) then
          sp=1
          sy=sy+1
         end if
        enddo
        return
      end
c
c
c     ExtendZ: this subroutine extends Z (backast and forecast) with a given model
      subroutine extendHP(Z,nz,THhp,lf,wm,eZ)
      implicit none
      include 'srslen.prm'
      include 'dimensions.i'
      integer n1,n10,n12
      parameter (n1=1,n10=10,n12=12)
      real*8 ZERO,TWO,MONE
      parameter (ZERO=0.0d0,TWO=2.0d0,MONE=-1.0d0)
c   INPUT PARAMETERS
c      integer nTHhp
      integer nz,lf
      real*8 Z(*),THhp(*)
c   OUTPUT PARAMETERS
      real*8 eZ(*),wm
c   Common
      include 'calc.i'
      include 'calfor.i'
      include 'sesfcast.i'
      include 'xarr.i'
c   Local variables
c      integer tmpBPstar
      integer i,j,nd,tmplf
      real*8 sum
      real*8 tmpTH(3),tmpTHstar(40),tmpPHIST(30),BPHIST(60),
     $       tmpSESfcast(kp)
      integer tmpQ,tmpQstar,tmpPstar,tmpBQ,tmpP,tmpBP,tmpINIT
      integer Ierr,dummInt
      character ErrExt*180
      real*8 a(mp+2*kp),ba(mp+2*kp),bz(mp+3*kp)
      integer na,BPSTAR
      real*8 f,forbias(kp)
c ----------------------------------------------------------------------
      do i=1,nz
        eZ(i)=Z(i)
        wd(i)=z(i)
        bz(i)=z(nz-i+1)
      enddo
      nw=nz
      nd=2
      dummInt=3
      BPHIST(1)=TWO
      BPHIST(2)=MONE
      do j=1,nd
        do i=1,nw-1
          wd(i)=wd(i+1)-wd(i)
        enddo
        nw=nw-1
      enddo
      do i=1,kp
        tmpSESfcast(i)=SESfcast(i)
      enddo
      do i=1,Q
        tmpTH(i)=TH(i)
      enddo
      tmpQ=Q
      Q=2
      TH(1)=-THhp(2)
      TH(2)=-THhp(3)
      do i=1,Qstar
        tmpTHstar(i)=THstar(i)
      enddo
      tmpQstar=Qstar
      Qstar=2
      do i=1,Pstar
        tmpPHIST(i)=PHIST(i)
      enddo
      tmpPstar=Pstar
      tmpBQ=BQ
      tmpP=P
      tmpBP=BP
      Pstar=0
      BQ=0
      P=0
      BP=0
      tmpINIT=INIT
      INIT=2
      Ierr=0
      Na = Nw-Pstar+Qstar
      call calcFx(nx,x,f,na,a,Ierr,ErrExt,dummInt,*1000)
      do i=1,na
        a(i)=a(i)/Detpri
      enddo
      do i=1,INT(nw/2)
        sum=wd(i)
        wd(i)=wd(nw-i+1)
        wd(nw-i+1)=sum
      enddo
      if (nd.ne.INT(nd/2)*2) then
        do i=1,nw
          wd(i)=-wd(i)
        enddo
      end if
      sum=ZERO
      do i=1,nw
        sum=sum+wd(i)
      enddo
      wm=sum/nw
      BPSTAR=0
      tmplf=lf
      call Fcast(PHIST,THstar,BPHIST,BPstar,eZ,nz,wm,a,na,-1,f,1,nd,0,
     $           0,wm,tmplf,0,-300,forbias,1,1.645d0)
      call calcFx(nx,x,f,na,ba,Ierr,ErrExt,dummInt,*1000)
      do i=1,na
        ba(i)=ba(i)/Detpri
      enddo
      tmplf=lf
      call Fcast(PHIST,THstar,BPHIST,BPstar,bz,nz,wm,ba,na,-1,f,1,nd,0,
     $           0,wm,tmplf,0,-300,forbias,1,1.645d0)
      do i=nz+lf,1,-1
        eZ(lf+i)=ez(i)
      enddo
      do i=1,lf
        eZ(lf-i+1)=bz(nz+i)
      enddo
 1000 if (Ierr.ne.0) then
        return
      end if
      INIT=tmpINIT
      BP=tmpBP
      P=tmpP
      BQ=tmpBQ
      Pstar=tmpPstar
      do i=1,Pstar
        PHIST(i)=tmpPHIST(i)
      enddo
      Qstar=tmpQstar
      do i=1,Qstar
        THstar(i)=tmpTHstar(i)
      enddo
      q=tmpQ
      do i=1,Q
        TH(i)=tmpTH(i)
      enddo
      do i=1,kp
        SESfcast(i)=tmpSESfcast(i)
      enddo
      end

      SUBROUTINE CheckMSG(Nio,cvar,cvalue)
      implicit none
c-----------------------------------------------------------------------
      INTEGER Nio
      CHARACTER cvar*(*),cvalue*(*)
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      WRITE(Nio,1010)cvar,Cbr
 1010 FORMAT('<p><strong>WARNING:</strong> &nbsp; ',
     &       'Wrong value for the parameter "',a,a)
      IF(cvalue(1:1).ne.CNOTST)then
       WRITE(Nio,1020)cvalue,Cbr
 1020  FORMAT(' &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Admissible value : ',a,a)
      END IF
      WRITE(Nio,1030)cvar
 1030 FORMAT(' &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;',a,
     &       ' set to the default value.</p>')
c-----------------------------------------------------------------------
      RETURN
      END
	  
