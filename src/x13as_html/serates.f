c             SErates.F
c        To compute the Se of the Rates of growth
c

c      SErates conpute the Se of the rates of growth
c     INPUT PARAMETERS
c      real*8    H(0:lH)     :Numerator of revision error
c     $         THr(1:lTHr)     :MA of the serie in real signs, also denominator of the revision error
c     $         PSIE(0:2*pk+1):innovations weights of the component filter for B^pk to F^pk+1,PSIE(pk) is the PSIE of concurrent
c     $         Vr=(Vc*H_F(1))^2  : Variance of revision error model in terms of Va
c     $         Va            :Variance of residuals
c     integer    mq           :number of observations per year
c     $         LastPer    :the Last period which period of the year is
c     $         nOutPar     :number of SDrev,SDR1 and of SDRmq

c     OUTPUT PARAMETERS
c      real*8 SDrev(nOutPar), !SDrev(i):component revision SE for B^(i-1)
c     $       SDR1(nOutPar),  !SDR1(i):revision SE of rate T(1,1) for B^(i-1)
c     $       SDR1f,            !SDR1f:revision SE of rate (1-F) for concurrent
c     $       SDRmq(nOutPar), !SDRmq(i):revision SE of interannual rate T(1,mq)
c     $       SDRmqF,           !revision SE of (1-F^mq) for concurrent
c     $       SDRmqC,           !revision SE of (B^(mq/2-1)-F^(mq/2))
c     $       SDRmqC2,          !revision SE of (B^(mq/2-2)-F^(mq/2-1))
c     $       SDRmqPf           !revision SE of annual rate for the present year
      subroutine SErates(H,lH,THr,lTHr,PSIE,pK,Vr,Va,mq,LastPer,nOutPar,
     $             SDrev,SDR1,SDR1f,SDRmqF,SDRmqC,SDRmqPf,SDRmq,SDRmqC2)
                      
      implicit none
      include 'units.cmn'
c     INPUT PARAMETERS
      integer  lH, !LH+1:length of H_F
     $         lTHr, !length of THr
     $         pK,  !PSIE(pk) is the PSIE of concurrent
     $         mq, !number of observations per year
     $         LastPer, !the Last period which period of the year is
     $         nOutPar   !number of SDrev,SDR1 and of SDRmq

      real*8   H(*), !Numerator of revision error
     $         THr(*),    !TH of the serie in BOX-Jenkins
     $         PSIE(0:2*pk+1), !innovations weights 
     $         Vr,        !Variance of revision error model in units Va
     $         Va         !Variance of residuals

c     INTRINSIC FUNCTIONS
      intrinsic ABS
c     OUTPUT PARAMETERS
      real*8 SDrev(nOutPar), !SDrev(i):component revision SE
     $       SDR1(nOutPar),  !SDR1(i):revision SE  T(1,1)
     $       SDR1f,            !SDR1f:revision SE (1-F) 
     $       SDRmq(nOutPar), !SDRmq(i):revision SE  T(1,mq)
     $       SDRmqF,       !revision SE of (1-F^mq) for concurrent
     $       SDRmqC,     !revision SE of (B^(mq/2-1)-F^(mq/2))
     $       SDRmqC2,    !revision SE of (B^(mq/2-2)-F^(mq/2-1))
     $       SDRmqPf  !revision SE of annual rate for the present year

c     LOCAL PARAMETERS
      integer i
      real*8 TH(60)


      do i=1,lTHr-1
        TH(i)=-THr(i+1)
      end do
c      write(Mtprof,*)'Before serev, lH, lTHr-1 =',lH,lTHr-1
      call seRev(H,TH,lH,lTHr-1,PSIE,pK,Vr,Va,nOutPar,SDrev)
      call seT11(H,TH,lH,lTHr-1,PSIE,pK,Vr,Va,nOutPar,SDR1,SDR1f)
      call seT1mq(H,TH,lH,lTHr-1,PSIE,pk,Vr,Va,mq,LastPer,nOutPar,
     $            SDRmq,SDRmqF,SDRmqC,SDRmqPf,SDRmqC2)
       
      end subroutine

      subroutine seRev(H,TH,lH,lTH,PSIE,pK,Vr,Va,nOutPar,SDrev)
      implicit none
      include 'units.cmn'
c     INPUT PARAMETERS
      integer lH,lTH,pK,nOutPar
      real*8 H(*),TH(*),PSIE(0:2*pk+1),Vr,Va

C     OUTPUT PARAMETERS
      real*8 SDrev(nOutPar)

c     INTRINSIC FUNCTIONS
      intrinsic ABS
c     LOCAL PARAMETERS
      real*8 cov(0:0),rho(0:0),g(0:0),Ve,Vrev(nOutPar)
      integer i

c      WRITE(Mtprof,*)'  subroutine seRev, call 1, lTH = ',lTH
      call BFAC(TH,H,lTH,lH,0,cov,rho,Ve,Vr,g,0)
      Vrev(1)=cov(0)
      if ((Vrev(1)) .lt. 1.0D-14) then
        Vrev(1)=0.0D0
      end if
      Do i=2,nOutPar
        Vrev(i)=Vrev(i-1)-PSIE(1-i+pk)*PSIE(1-i+pk)
        if ((Vrev(i)) .lt. 1.0E-14) then
          Vrev(i)=0.0d0
        end if
      end do
      do i=1,nOutPar
        SDrev(i)=sqrt(Vrev(i)*Va)
      end do

      end subroutine


      subroutine seT11(H,TH,lH,lTH,PSIE,pk,Vr,Va,nOutPar,SDR1,SDR1f)
      implicit none
      include 'units.cmn'
c     INPUT PARAMETERS
      integer lH,lTH,pK,nOutPar
      real*8 H(*),TH(*),PSIE(0:2*pk+1),Vr,Va

C     OUTPUT PARAMETERS
      real*8 SDR1(nOutPar),SDR1f

c     INTRINSIC FUNCTIONS
      intrinsic ABS
C     LOCAL PARAMETERS
      integer i,lHc1
      real*8  delta1(1),Hc1(60),cov(0:0),rho(0:0),Ve,g(0:0),Hc1r(60),
     $        Vr1(nOutPar),Vr1f,THr(60),PSIE1(50+1)

      delta1(1)=1
      call MPBBJ(H,delta1,lH,1,Hc1)
      lHc1=lH+1
c      WRITE(Mtprof,*)'  subroutine seT11, call 1, lTH = ',lTH
      call BFAC(TH,Hc1,lTH,lHc1,0,cov,rho,Ve,Vr,g,0)

      THr(1)=1
      do i=1,lTH
       THr(i+1)=-TH(i) !THr in real signs
      end do
      Hc1r(1)=1
      do i=2,lHc1+1
        Hc1r(i)=-Hc1(i-1) !Hc1r in real signs
      end do
      call getPSIE(Hc1r,lHc1,THr,lTH,sqrt(Vr),50,PSIE1)
      Vr1(1)=(cov(0)-PSIE1(1)*PSIE1(1))
      if ((Vr1(1)) .lt. 1.0E-14) then
        Vr1(1)=0.0d0
      end if
      do i=2,nOutPar
        Vr1(i)=Vr1(i-1)-PSIE1(i)*PSIE1(i)
        if ((Vr1(i)) .lt. 1.0E-14) then
          Vr1(i)=0.0d0
        end if
      end do
      do i=1,nOutPar
        SDR1(i)=sqrt(Vr1(i)*Va)        
      end do
      Vr1f=Vr1(1)*Va+(PSIE(pk)-PSIE(pk-1))*(PSIE(pk)-PSIE(pk-1))*Va
      SDR1f=sqrt(Vr1f)

      end subroutine


      subroutine seT1mq(H,TH,lH,lTH,PSIE,pk,Vr,Va,mq,lastPer,nOutPar,
     $                  SDRmq,SDRmqF,SDRmqC,SDRmqPf,SDRmqC2)
      implicit none
      include 'units.cmn'
c     INPUT PARAMETERS
      integer lH,lTH,pK,mq,lastPer,nOutPar
      real*8 H(*),TH(*),PSIE(0:2*pk+1),Vr,Va

c     OUTPUT PARAMETERS
      real*8 SDRmq(nOutPar),SDRmqF,SDRmqC,SDRmqPf,SDRmqC2

c     INTRINSIC FUNCTIONS
      intrinsic ABS
c     LOCAL PARAMETERS
      real*8 cov(0:0),rho(0:0),g(0:0),Ve,Vrmq(nOutPar),THr(60),
     $       HpMQ(60),HpMQr(60),PSIEmq(50),a(12),VrmqF,VrmqC,VrmqC2,
     $       VrmqPf,DeltaMQ(12)
      integer i,lHpMQ,Iper

      THr(1)=1
      DO i=1,lTH
        THr(i+1)=-TH(i)
      end do
      do i=1,mq-1
        DeltaMQ(i)=0
      end do
      DeltaMQ(mq)=1 !(1-B^mq) in Box-Jenkins notation

      call MPBBJ(H,DeltaMQ,lH,mq,HpMQ)
      lHpMQ=lH+mq
c      WRITE(Mtprof,*)'  subroutine seT1mq, call 1, lTH = ',lTH
      call BFAC(TH,HpMQ,lTH,lHpMQ,0,cov,rho,Ve,Vr,g,0)
      HpMQr(1)=1
      Do i=1,lHpMQ
        HpMQr(i+1)=-HpMQ(i)
      end do
      call getPSIE(HpMQr,lHpMQ,THr,lTH,sqrt(Vr),50,PSIEmq)

      Vrmq(1)=cov(0)
      do i=1,mq
        Vrmq(1)=Vrmq(1)-PSIEmq(i)*PSIEmq(i)
      end do
      if ((Vrmq(1)) .lt. 1.0D-14) then
        Vrmq(1)=0.0D0
      end if
      Do i=2,NoutPar
        Vrmq(i)=Vrmq(i-1)-PSIEmq(i+mq-1)*PSIEmq(i+mq-1)
        if ((Vrmq(i)) .lt. 1.0D-14) then
          Vrmq(i)=0.0D0
        end if
      end do
      do i=1,nOutPar
        SDRmq(i)=sqrt(Vrmq(i)*Va)
      end do
      do i=1,mq
        a(i)=PSIE(pk+mq-i)-PSIE(pk-i)
      end do
      
      VrMQf=VrMQ(1)*Va
      do i=1,mq
        VrMQf=VrMQf+a(i)*a(i)*Va
      end Do
      SDrMQf=sqrt(VrMQf)

      Iper=lastPer+1
      VrMQpf=VrMQ(1)*Va
      do i=Iper,mq
        VrMQpf=VrMQpf+a(i)*a(i)*Va
      end do
      SDRmqPf=sqrt(VrMQpf)
      VrMQc=VrMQ(1)*Va
      do i=(mq/2)+1,mq
        VrMQc=VrMQc+a(i)*a(i)*Va
      end do
      SDRmqC=sqrt(VrMQc)
      VrMQc2=VrMQ(1)*Va
      do i=(mq/2)+2,mq
        VrMQc2=VrMQc2+a(i)*a(i)*Va
      end do
      SDRmqC2=sqrt(VrMQc2)

      end subroutine


      subroutine SEratesOut(SDrev,SDR1,SDR1f,SDRmq,SDRmqF,
     $                      SDRmqC,SDRmqPf,SDRmqC2,nOutPar,nio)
      implicit none
c     INPUT PARAMETERS
      integer nOutPar,
     $        nio !FileIdentifier
      real*8 SDRev(nOutPar),SDR1(nOutPar),SDR1f,
     $       SDRmq(nOutPAr),SDRmqF,SDRmqC,SDRmqPf,SDRmqC2
c     EXTERNAL FUNCTIONS
      integer istrlen
      external istrlen
c     LOCAL PARAMETERS
      integer i
      character cad*1000,cad2*1000

      write(nio,'("  N   SDRev        SDR1        SDRmq")')
      do i=1,nOutPar
         write(nio,'(I3," ",G11.3," ",G11.3," ",G11.3)') 
     $         i,SDRev(i),SDR1(i),SDRmq(i)
      end do

*      write(nio,'("SDR1f=",G11.3)') SDR1f
      write(nio,'("SDRmqF=",G11.3)') SDRmqF
      write(nio,'("SDRmqC=",G11.3)') SDRmqC
      write(nio,'("SDRmqC2=",G11.3)') SDRmqC2
      write(nio,'("SDRmqPf=",G11.3)') SDRmqpf
      end subroutine
      