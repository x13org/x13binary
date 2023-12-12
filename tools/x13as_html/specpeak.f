cc
c New Spectrum 
cc
C     Last change:  Domingo Perez  14 September 2004    
      SUBROUTINE GetPeaks(z,nz,mq,Szz,w,
     $             TDpeaks,nTDpeaks,pTDpeaks,
     $             SeasPeaks,nSeasPeaks,pSeaspeaks,differ)
      IMPLICIT NONE
      include 'spectrum.i'
c
c     INPUT PARAMETERS
c
      integer nz,mq
      double precision z(nz)
      integer differ
c
c     OUTPUT PARAMETERS
c
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
c      integer nw
c	parameter (nw=61)
cc
c
cc
      integer nTDpeaks,!number of peaks detected at TD frequencies
     $        nSeasPEaks
                 !number of peaks detected at Seasonal frequencies
      integer TDpeaks(6),  !peaks detected at TD frequencies
     $	  Seaspeaks(6) !peaks detected at Seasonal frequencies
      double precision w(nfrq),     !frequencies
     $                 Szz(nfrq)   !Spectrum of (1-B)z at w frequencies
     	real*8  pTDpeaks(6),  !chance of peaks detected at TD frequencies
     $	  pSeaspeaks(6) !chance of peaks detected at Seasonal frequencies
c
c     INTERNAL PARAMETERS
c
      integer i,N1
      real*8 pi2
      double precision diffz(nz)  !(1-B)z
c   Initialize
      nTDpeaks=0
      nSeasPEaks=0
      do i=1,6
       TDpeaks(i)=0
       Seaspeaks(i)=0
       pTDpeaks(i)=0d0
       pSeaspeaks(i)=0d0
      end do
c
      pi2=2.0d0*acos(-1.0d0)
c     120=12*10  cogemos los últimos 10 años para el AR(30) en el caso MQ=12
      if (nz.gt.SPECLENGTH) then
       N1=nz-SPECLENGTH+1
      else
       N1=1
      end if
      if (differ .eq. 1) then
        do i = N1,nz-1
           diffz(i-N1+1)=z(i+1)-z(i)
        end do
c	  Vz=getVar(diffz,nz-1)
        call GetPeak1(diffz,nz-N1,mq,Szz,w,
     $            TDpeaks,nTDpeaks,pTDpeaks,
     $            SeasPeaks,nSeasPeaks,pSeasPeaks)
      else
c	  Vz=getVar(z,nz)
        do i=N1,nz
          diffz(i-N1+1)=z(i)
        enddo
        call GetPeak1(diffz,nz-N1+1,mq,Szz,w,
     $            TDpeaks,nTDpeaks,pTDpeaks,
     $            SeasPeaks,nSeasPeaks,pSeasPeaks)
      end if
      do i=1,nfrq
         Szz(i) = exp(Szz(i)*log(10.0d0)/10.0d0)
      end do
c	 sSzz=0
c	 do i=2,nfrq
c	   sSzz=sSzz+Szz(i)
c	 end do
c       sSzz=sSzz/(nfrq-1)
c	 do i=1,nfrq
c	   Szz(i)=Szz(i)*Vz/sSzz
c	 end do
      do i = 1,nfrq
       w(i) = w(i) * pi2
      end do
      end
c
      SUBROUTINE GetPeak1(diffz,ndiffz,mq,Szz,frq,
     $                TDpeaks,nTDpeaks,pTDpeaks,
     $                SeasPeaks,nSeasPeaks,pSeasPeaks)
      IMPLICIT NONE      
c
c     INPUT PARAMETERS
c
      integer ndiffz,mq
      double precision diffz(ndiffz)  ! the differenced serie (1-B)Z
c
c     OUTPUT PARAMETERS
c
      INCLUDE 'srslen.prm'
      include 'dimensions.i'     
      include 'rho.cmn'     
c      integer nfrq
c     parameter (nfrq=61)
cc
c
cc
      integer nTDpeaks,nSeasPeaks
      double precision frq(nfrq),   !frequencies
     $                 Szz(nfrq)    !Spectrum of (1-B)z at w frequencies
      integer          TDpeaks(6),  !peaks detected at TD frequencies
     $                 SeasPeaks(6) !peaks detected at Seas frequencies
      real*8     pTDpeaks(6),  !prob peaks detected at TD frequencies
     $           pSeasPeaks(6) !prob peaks detected at Seas frequencies
c
c     INTERNAL PARAMETERS
c
      integer i,frqidx,N1,k
      double precision LIMSPC,pklim,tmpSzz(nfrq),Rango
      logical good
c antiguo
c     parameter(LIMSPC=6D0)
c nuevo
c      parameter(LIMSPC=4D0)
c
c     Functions
c
c      integer ispeak,ispeak2
c     external ispeak,ispeak2
      integer pARpeak
      external pARpeak
c-----------------------------------------------------------------------
c      inicializando freq y frqidx
c-----------------------------------------------------------------------
      DO i = 1,61
       frq(i)=dble(i-1)/120.0
      END DO
      IF(mq.eq.12)THEN
       frq(42)=.3482-frq(2)
       frq(43)=.3482
       frq(44)=.3482+frq(2)
       frq(52)=.432-frq(2)
       frq(53)=.432
       frq(54)=.432+frq(2)
       frqidx=1
c      ELSE !IF(mq.eq.4)THEN
c       frq(5)=.0446-frq(2)
c       frq(6)=.0446
c       frq(7)=.0446+frq(2)
c       frq(11)=.0893-frq(2)
c       frq(12)=.0893
c       frq(13)=.0893+frq(2)
      ELSE !IF(mq.eq.4)THEN
       frq(35)=.29465-frq(2)
       frq(36)=.29465
       frq(37)=.29465+frq(2)
       frq(41)= 0.3393-frq(2)
       frq(42)= 0.3393
       frq(43)= 0.3393+frq(2)
       frqidx=2
           !ELSE   Error('solo hacemos spectral plots para mq=12 ó 4')
           !   RETURN
           !END IF
      END IF
c-----------------------------------------------------------------------
      good=.true.
      n1=1
      call spgrh(diffz,Szz,frq,0D0,n1,ndiffz,nfrq,mq,Mxarsp,Ldecbl,good)
c      k=min(30,ndiffz/2)   !to avoid underdetermined system  
c      call getSpect(diffz,ndiffz,frq,nfrq,k,Szz,good)
      do i = 1,nfrq
       tmpSzz(i)=Szz(i)
      end do
      nTDpeaks=0
      nSeaspeaks=0
      call shlsrt(nfrq,tmpSzz)   !sort the array tmpSzz
      Rango=(tmpSzz(nfrq)-tmpSzz(1))
      if ((mq .eq. 12) .or. (mq .eq. 4)) then
c        pklim=Rango*(LIMSPC/52D0)
c       nTDpeaks=ispeak(Szz,frqidx,pklim,tmpSzz(31),TDpeaks)
c       nSeaspeaks=ispeak(Szz,2+frqidx,pklim,tmpSzz(31),Seaspeaks)
        nTDpeaks=pARpeak(Szz,frqidx,Rango,tmpSzz(31),
     $                   pTDpeaks,TDpeaks)
        nSeaspeaks=pARpeak(Szz,2+frqidx,Rango,tmpSzz(31),
     $                   pSeasPeaks,SeasPeaks)
      else 
        if (mq.eq.6) then
c         nSeaspeaks=ispeak(Szz,5,pklim,tmpSzz(31),Seaspeaks)
          nSeaspeaks=pARpeak(Szz,5,Rango,tmpSzz(31),
     $                   pSeasPeaks,SeasPeaks)
        else if (mq.eq.2) then
c         nSeaspeaks=ispeak(Szz,6,pklim,tmpSzz(31),Seaspeaks)
          nSeaspeaks=pARpeak(Szz,6,Rango,tmpSzz(31),
     $                   pSeasPeaks,SeasPeaks)
        else if (mq.eq.3) then
c         nSeaspeaks=ispeak(Szz,7,pklim,tmpSzz(31),Seaspeaks)
          nSeaspeaks=pARpeak(Szz,7,Rango,tmpSzz(31),
     $                   pSeasPeaks,SeasPeaks)
        else
          nSeasPeaks=0
        end if
      end if
c
      end subroutine
c
c
cc
c
cc
      logical function SeasSpectCrit2(pico,mq) 
      implicit none
C Test al 99%
      integer mq
      character pico(7)*2
c local
      integer i,ipicos,idoble
      ipicos=0
      idoble=0
      if (mq.eq.4) then	 
       do i=1,2
        if ((pico(i)(1:1).eq.'A').or.(pico(i)(2:2).eq.'T')) then
         ipicos=ipicos+1 
        end if
       end do
       if (pico(1).eq.'AT') then
        SeasSpectCrit2=.true.
       else if (ipicos.eq.2) then 
        SeasSpectCrit2=.true.
       else
        SeasSpectCrit2=.false.
       end if
      else if (mq.eq.12) then
       SeasSpectCrit2=.false.
       do i=1,6
        if (pico(i).eq.'AT') then
         idoble=idoble+1
         ipicos=ipicos+1
        else if ((pico(i)(1:1).eq.'A').or.(pico(i)(2:2).eq.'T')) then
         ipicos=ipicos+1
        end if	     	  
       end do
       SELECT CASE (ipicos)
        CASE (4,5,6) 
          SeasSpectCrit2=.true.
        CASE (3)
          if (((pico(6)(1:1).ne.'A').and.(pico(6)(2:2).ne.'T')).or.
     $       (idoble.ge.1))then
            SeasSpectCrit2=.true.
          end if
        CASE (2)
         if (pico(6).eq.'AT') then
          if (idoble.eq.2)  then
           SeasSpectCrit2=.true.
          end if
         else if (pico(6)(1:1).ne.'A'.or.pico(6)(2:2).ne.'T') then
          if (idoble.ge.1) then
           SeasSpectCrit2=.true.
          end if  
         end if
        CASE DEFAULT
          SeasSpectCrit2=.false. 
       END SELECT  
      else
       SeasSpectCrit2=.false. 
      end if
      end
cc
c
cc
      logical function SeasSpectCrit(pico,mq)
      implicit none
C      Test al 95% 
      integer mq
      character pico(7)*2
c local
      integer i,ipicos,idoble
      ipicos=0
      idoble=0
      if (mq.eq.4) then	 
       do i=1,2
        if ((pico(i).ne.'--').and.(pico(i).ne.'nc')) then
         ipicos=ipicos+1 
        end if
       end do
       if ((pico(1).eq.'AT'))then
        SeasSpectCrit=.true.
       else if (ipicos.eq.2) then 
        SeasSpectCrit=.true.
       else
        SeasSpectCrit=.false.
       end if
      else if (mq.eq.12) then
       SeasSpectCrit=.false.
       do i=1,6
        if (pico(i).eq.'AT') then
         idoble=idoble+1
         ipicos=ipicos+1
        else if ((pico(i).ne.'--').and.(pico(i).ne.'nc')) then
         ipicos=ipicos+1
        end if	     	  
       end do
       SELECT CASE (ipicos)
        CASE (4,5,6) 
          SeasSpectCrit=.true.
        CASE (3)
          if ((pico(6).eq.'--').or.(pico(6).eq.'nc').or.
     $       (idoble.ge.1))then
            SeasSpectCrit=.true.
          end if
        CASE (2)
         if ((pico(6)(1:1).eq.'A'.or.pico(6)(1:1).eq.'a').and.
     $      (pico(6)(2:2).eq.'T'.or.pico(6)(2:2).eq.'t')) then
          if (idoble.eq.2)  then
           SeasSpectCrit=.true.
          end if
         else if (pico(6).eq.'--') then
          if (idoble.ge.1) then
           SeasSpectCrit=.true.
          end if  
         end if
        CASE DEFAULT
          SeasSpectCrit=.false. 
       END SELECT  
      else
        SeasSpectCrit=.false.
      end if
      end
c
c
c
c
c
c
      subroutine dfPeaks(m,nz,df1,df2,df3,df4)
      implicit none
c    INPUT
      integer nz,m
c    OUTPUT
      real*8 df1,df2,df3,df4
c    LOCAL
      real*8 n100,n_100,df(3,4)
c----------------------------------------------
      if (m.eq.120) then
        df(1,1)=0.317d0
        df(2,1)=2.7706d0
        df(3,1)=2.6516d0
        df(1,2)=2.0934d0
        df(2,2)=7.0464d0
        df(3,2)=10.5217d0
        df(1,3)=-.4336d0
        df(2,3)=1.4463d0
        df(3,3)=3.0668d0
        df(1,4)=0.6411d0
        df(2,4)=3.6073d0
        df(3,4)=7.9892d0
      else if (m.eq.112) then
        df(1,1)=0.5463d0
        df(2,1)=2.9303d0
        df(3,1)=2.2042d0
        df(1,2)=1.1329d0
        df(2,2)=7.6924d0
        df(3,2)=10.8795d0
        df(1,3)=-.3492d0
        df(2,3)=1.533d0
        df(3,3)=2.7696d0
        df(1,4)=0.9829d0
        df(2,4)=3.8217d0
        df(3,4)=6.9345d0
      else if (m.eq.44) then
        df(1,1)=1.3779d0
        df(2,1)=7.2620d0
        df(3,1)=0.3725d0
        df(1,2)=3.1495d0
        df(2,2)=18.0654d0
        df(3,2)=3.5564d0
        df(1,3)=0.2504d0
        df(2,3)=3.6616d0
        df(3,3)=0.7929d0
        df(1,4)=0.504d0
        df(2,4)=9.7201d0
        df(3,4)=3.0605d0
      else if (m.eq.79) then
        df1=6.35251d0
        df2=19.6308d0
        df3=2.29316d0
        df4=6.55412d0
      end if
      if (m.eq.112 .or. m.eq.44 .or. m.eq.120) then
        n100=dble(nz)/100.0d0
        n_100=100.d0/dble(nz)
        df1=df(1,1)+df(2,1)*n100+df(3,1)*n_100
        df2=df(1,2)+df(2,2)*n100+df(3,2)*n_100
        df3=df(1,3)+df(2,3)*n100+df(3,3)*n_100
        df4=df(1,4)+df(2,4)*n100+df(3,4)*n_100
      end if
      end
c
c
c     Tpeaks2: retorna la probabilidad de picos en el espectro ACF windowing usando Tukey
c           Solo esta calculado para las ventanas m=112, 79 y 44
      subroutine Tpeaks2(H,m,MQ,nz,pTDpeaks,pSpeaks,mv)
      implicit none
c     INPUT PARAMETERS
      integer m,MQ,nz
      real*8 H(*)
c     OUTPUT PARAMETERS
      real*8 pTDpeaks,pSpeaks(6)
      real*8 mv(14)
c     LOCAL PARAMETERS
      real*8 incH,mv1,mv2,mv3,vA1,vA2
      real*8 df1,df2,df3,df4
      integer i,indM(5),nIndM,indTD,indPI,k
c    EXTERNAL
      real*8 Fcdf
      external Fcdf
c-------------------------------------------------
      do i=1,6
        pSpeaks(i)=0.0d0
      enddo
      pTDpeaks=0.0d0
      call dfPeaks(m,nz,df1,df2,df3,df4)
      select case(m)
       case(120)
        indM(1)=11
        indM(2)=21
        indM(3)=31
        indM(4)=41
        indM(5)=51
        nIndM=5
        indTD=43
        indPI=61
       case(112)
        indM(1)=10
        indM(2)=20
        indM(3)=29
        indM(4)=38
        indM(5)=48
        nindM=5
        indTD=40
        indPI=57
       case(79)
        indM(1)=8
        indM(2)=14
        indM(3)=21
        indM(4)=27
        indM(5)=34
        nindM=5
        indTD=29
        indPI=40
       case default
        indTD=-1
        indPI=22
        nindM=0
        select case(mq)
        case(6)
          indM(1)=8
          indM(2)=15
          nindM=2
        case(4)
c	    indTD=3
          indTD=14
          indM(1)=12
          nindM=1
        case(3)
          indPI=-1
          indM(1)=15
          nindM=1
        case(1)
          indPI=-1
        end select
      end select
      do i=1,nIndM
        IncH=2*H(indM(i))
        incH=incH/(H(indM(i)+1)+H(indM(i)-1))
        pSpeaks(i)=Fcdf(incH,df1,df2)
c  Obtencion de valores de test que tiene en cuenta picos anchos
        mv1=H(indM(i))/H(indM(i)-1)
        mv2=H(indM(i))/H(indM(i)+1)
        vA1=2*H(indM(i))/(H(indM(i)-1)+H(indM(i)-2))
        vA2=2*H(indM(i))/(H(indM(i)+1)+H(indM(i)+2))
        if (mV1.lt.mV2) then
          mv(i*2-1)=mv1
        else
          mv(i*2-1)=mv2
        endif
        if (VA1.lt.mv1) then
          VA1=mv1
        endif
        if (VA2.lt.mv2) then
          VA2=mv2
        endif
        if (vA1.lt.vA2) then
          mV3=VA1
        else
          mv3=VA2
        endif
        if ((MQ.eq.12.and.i.eq.4).or.MQ.eq.4) then
          mv(i*2)=VA1
        else
          mv(i*2)=mv3
        endif
      enddo
      k=nIndM
      if (indPI.gt.0) then
        IncH=H(indPI)/H(indPI-1)
        pSpeaks(MQ/2)=Fcdf(incH,df3,df4)
c  Obtencion de valores de test que tiene en cuenta picos anchos
        k=k+1
        mv(k*2-1)=H(indPI)/H(indPI-1)
        VA1=2*H(indPI)/(H(indPI-1)+H(indPI-2))
        if (VA1.gt.mv(2*k-1)) then
          mv(2*k)=VA1
        else
          mv(2*k)=mv(k*2-1)
        endif
      endif
      if (indTD.gt.0) then
        incH=2*H(indTD)/(H(indTD+1)+H(indTD-1))
        pTDpeaks=Fcdf(incH,df1,df2)
c  Obtencion de valores de test que tiene en cuenta picos anchos
        k=k+1
        mv1=H(indTD)/H(indTD-1)
        mv2=H(indTD)/H(indTD+1)
        VA2=2*H(indTD)/(H(indTD+1)+H(indTD+2))
        if (mv1.lt.mv2) then
          mv(k*2-1)=mv1
        else
          mv(k*2-1)=mv2
        endif
        if (VA2.lt.mv2) then
          VA2=mv2
        endif
        if (VA2.lt.mv1) then
          mv(k*2)=VA2
        else
          mv(k*2)=mv1
        endif
      end if
      end
c
c
c     getTPeaks: given a series(serie(1:nserie)) with its MQ 
c          return its spectrum(H), the choosen Tukey window size(m) and 
c      the probability of TD peaks (pTDpeak) and of seasonal peaks(Speaks(1:nSpeaks))
c      Besides: wSpeaks(i): 1:if thereis a Seasonal spectral peak for w=i*pi/6 Radians; 0 If there is not a peaks
      subroutine getTPeaks(serie,nz,mq,H,m,pTDpeak,pSpeaks,mv)
      implicit none
      INCLUDE 'srslen.prm'
      INCLUDE 'dimensions.i'
      INCLUDE 'rho.cmn'
c     INPUT
      real*8 serie(*),mv(14)
      integer nz,mq
c     OUTPUT
      real*8 H(*),pTDpeak,pSpeaks(6)
      integer m
c     LOCAL
      real*8 window(0:120)
      integer iwindow,i
c
      iwindow=2 !Tukey
      if (Ltk120.and.nz.ge.120.and.mq.eq.12) then
c      if (Ltk120) then
        m=120
      else if ((MQ.ne.12).and.(nz.ge.60)) then
        m=44
      else if ((nz.ge.120).and.(mq.eq.12)) then
        m=112
      else if ((nz.ge.80).and.(mq.eq.12)) then
        m=79
      else
        pTDpeak=0.0d0
        do i=1,6
          pSpeaks(i)=0.0d0
        enddo
        m=-1
        return
      end if
      call getWind(iWindow,m,window)
      call covWind(H,m,serie,nz,window,nw2)
      call Tpeaks2(H,m,MQ,nz,pTDpeak,pSpeaks,mv)
      end
cc
C     Last change:  BCM  12 Nov 1998   10:53 am
**==ispeak.f    processed by SPAG 4.03F  at 14:16 on 28 Sep 1994
c  INPUT Sxx(1:61): AR(30) pseudo-spectrum
c        PKidx=1 analyze TD freq for MQ=12
c        PKidx=2 analyze TD freq for MQ=4
c        PKidx=3 analyze Seasonal frequencies for MQ=12
c        PKidx=4 analyze Seasonal frequencies for MQ=4
c        PKidx=5 analyze Seasonal frequencies for MQ=6
c        PKidx=6 analyze Seasonal frequencies for MQ=2
c        PKidx=7 analyze Seasonal frequencies for MQ=3
c        Rango=max(Sxx)-min(Sxx)
c        Mlimit=mediana(Sxx)
c  OUTPUT
c        peaks(1:pARpeak): probabilidad acumulada de los diferentes 
c            picos supuesto que son espectro de una serie de ruido 
c            blanco diferenciada una vez.
      integer function pARpeak(Sxx,Pkidx,Rango,Mlimit,
     $                        ppeaks,peaks)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Function that flags possible trading day or seasonal peaks in a
c     given set of spectral estimates.  Peak must be greater than the
c     median of the spectral estimates computed (Mlimit).  The peaks of
c     interest are defined in the vector pkvec.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
c    INPUT PARAMETERS
      INTEGER Pkidx
      Double precision Sxx(61),Rango,Mlimit
c    OUTPUT PARAMETERS
      real*8 ppeaks(6)
      integer peaks(6)
c    LOCAL PARAMETERS
      integer j,i,freq,cont
      real*8 incH,incH2
c-----------------------------------------------------------------------
      integer pkvec(18-1),pkptr(0:7)
      DATA pkvec/43,53,36,42,11,21,31,41,51,61,31,61,21,41,61,61,41/
      DATA pkptr/1,3,5,11,13,16,17,18/
c-----------------------------------------------------------------------
      real*8 silHf(100),silHm(100)
      DATA silHf/0.0696,0.0705,0.0715,0.0726,0.0735,
     &     0.0746,0.0756,0.0768,0.0778,0.0788,
     &     0.0801,0.0812,0.0822,0.0834,0.0845,
     &     0.0857,0.0869,0.0881,0.0895,0.0909,
     &     0.0923,0.0935,0.0948,0.0961,0.0975,
     &     0.0988,0.1003,0.1018,0.1034,0.1047,
     &     0.1062,0.1076,0.1090,0.1106,0.1125,
     &     0.1143,0.1161,0.1175,0.1192,0.1208,
     &     0.1224,0.1245,0.1263,0.1282,0.1301,
     &     0.1324,0.1342,0.1360,0.1381,0.1404,
     &     0.1428,0.1452,0.1477,0.1500,0.1523,
     &     0.1548,0.1573,0.1598,0.1620,0.1645,
     &     0.1674,0.1702,0.1731,0.1756,0.1790,
     &     0.1820,0.1852,0.1883,0.1922,0.1958,
     &     0.1991,0.2030,0.2069,0.2108,0.2154,
     &     0.2198,0.2243,0.2296,0.2347,0.2396,
     &     0.2447,0.2510,0.2572,0.2640,0.2717,
     &     0.2787,0.2863,0.2952,0.3043,0.3147,
     &     0.3259,0.3385,0.3522,0.3658,0.3849,
     &     0.4040,0.4314,0.4590,0.4958,0.5485/
      DATA silHm/0.0023,0.0029,0.0036,0.0042,0.0048,
     &     0.0055,0.0061,0.0068,0.0074,0.0080,
     &     0.0089,0.0095,0.0102,0.0109,0.0116,
     &     0.0123,0.0131,0.0139,0.0147,0.0154,
     &     0.0162,0.0169,0.0178,0.0186,0.0195,
     &     0.0203,0.0212,0.0221,0.0230,0.0240,
     &     0.0249,0.0259,0.0270,0.0280,0.0290,
     &     0.0300,0.0309,0.0319,0.0330,0.0342,
     &     0.0353,0.0364,0.0376,0.0388,0.0400,
     &     0.0411,0.0424,0.0435,0.0448,0.0459,
     &     0.0474,0.0488,0.0502,0.0515,0.0529,
     &     0.0544,0.0559,0.0572,0.0589,0.0606,
     &     0.0624,0.0640,0.0658,0.0676,0.0695,
     &     0.0714,0.0732,0.0751,0.0768,0.0790,
     &     0.0814,0.0839,0.0864,0.0889,0.0912,
     &     0.0939,0.0968,0.0995,0.1027,0.1057,
     &     0.1093,0.1127,0.1163,0.1202,0.1239,
     &     0.1280,0.1332,0.1379,0.1430,0.1490,
     &     0.1551,0.1620,0.1705,0.1794,0.1901,
     &     0.2025,0.2173,0.2380,0.2661,0.3120/
c     la probabilidad de un pico espureo sea mayor que 80+i*0.2% 
c      se da si supera en Plimit a las frecuencias adjacentes
c       donde   Plimit=silHf(i+1)*Rango (para pico en frecuencia pi radianes)
c               Plimit=silHm(i+1)*Rango (para frecuencia no pi radianes ni 0 radianes)
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c   EXTERNAL
      integer indexGE
      external indexGE
c-----------------------------------------------------------------------
      do i=1,6
        ppeaks(i)=0.0d0
      enddo
      cont=0
      DO i=pkptr(Pkidx-1),pkptr(Pkidx)-1
       j=i-pkptr(Pkidx-1)+1
       freq=pkvec(i)
       ppeaks(j)=0.0d0
       IF(Sxx(freq).gt.Mlimit)THEN
         incH=(Sxx(freq)-Sxx(freq-1))/Rango
         if (freq.ne.61) then
           incH2=(Sxx(freq)-Sxx(freq+1))/Rango
           if (incH.gt.incH2) then
             incH=incH2
           end if
           if (incH.gt.0.0d0) then
             ppeaks(j)=0.8+0.002*indexGE(incH,silHm,100)
           end if
         else
           if (incH.gt.0.0d0) then
             ppeaks(j)=0.8+0.002*indexGE(incH,silHf,100)
           end if
         end if
         if (pPeaks(j).ge.0.90d0)then
           cont=cont+1
           Peaks(cont)=freq
         end if
       END IF
      END DO
      pARpeak=cont
      RETURN
      END
c
      subroutine rellPico2(pARpeaks_s,pARpeaks_TD,pTpeaks_s,pTpeaks_TD,
     $                   mv,mq,dm,peaksARseas,peaksTseas,pico)
      implicit none
      real*8 prob1,prob2,probA
      parameter(prob1=0.99d0,prob2=0.90d0,probA=0.79d0)
c     INPUT VARIABLES
      integer mq,dm
      real*8 pTpeaks_s(6),pTpeaks_TD,pARPeaks_TD(6),pARpeaks_S(6),
     $       mv(14)
c     OUTPUT VARIABLES
      character pico(7)*2
      integer peaksARseas,peaksTseas
c     LOCAL VARIABLES
      integer i,tmp,peaksTtd,peaksARtd
c     external
      integer nPicosAnchos,picosAnchosTD
      external nPicosAnchos,picosAnchosTD
c--------------------
      do i=1,7
        pico(i)='--'
      enddo
      if ((pARPeaks_TD(1).ge.prob1)) then
        pico(7)(1:1)='A'
      else if ((pARPeaks_TD(1).ge.prob2)) then
        pico(7)(1:1)='a'
      end if
      if (pTpeaks_TD.ge.prob1) then
        pico(7)(2:2)='T'
      else if (pTpeaks_TD.ge.prob2) then
        pico(7)(2:2)='t'
      end if
      do i=1,6
        if (pTpeaks_s(i).ge.prob1) then
          pico(i)(2:2)='T'
        else if (pTpeaks_s(i).ge.prob2) then
          pico(i)(2:2)='t'
        end if
        if (pARpeaks_s(i).ge.prob1) then
          pico(i)(1:1)='A'
        else if (pARpeaks_s(i).ge.prob2) then
          pico(i)(1:1)='a'
        end if
      enddo
      end subroutine
c      end 
c
c
      integer function nPicosAnchos(mv,m,MQ,picos)
      implicit none
c  INPUT PARAMETERS
        real*8 mv(14)
        integer m,MQ
c  OUTPUT PARAMETERS
        character*2 picos(7)
c  LOCAL PARAMETERS
        integer i,cont
        real*8 mC,mC1,mC2,mC3,mC4,mC5,mC6,mCe4
c -------------------------------------------
      cont=0
      if (MQ.eq.12.and.m.eq.112) then
        mC=3.0d0
        mCe4=3.0d0
        mC1=1.76d0
        mC2=1.77d0
        mC3=2.05d0
        mC4=3.01d0
        mC5=1.76d0
        mC6=2.29d0
      elseif (MQ.eq.12.and.m.eq.79) then
        mC=3.0d0
        mCe4=2.81d0
        mC1=1.64d0
        mC2=1.78d0
        mC3=1.67d0
        mC4=2.82d0
        mC5=1.85d0
        mC6=1.97d0
      else
c  No se han calculado los valores críticos optimos para otros M
        nPicosAnchos=0
        return
      endif
      do i=1,6
        picos(i)(2:2)='-'
      enddo
c     if (m.ne.79)then
        if (mv(1).ge.mC.or.(mv(1).gt.1.0d0.and.mv(2).ge.mC1))then
         cont=cont+1
         picos(1)(2:2)='T'
        endif
c     endif
      if (mv(3).ge.mC.or.(mv(3).gt.1.0d0.and.mv(4).ge.mC2))then
         cont=cont+1
         picos(2)(2:2)='T'
      endif
      if (mv(5).ge.mC.or.(mv(5).gt.1.0d0.and.mv(6).ge.mC3))then
         cont=cont+1
         picos(3)(2:2)='T'
      endif
      if (mv(7).ge.mCe4.or.(mv(7).gt.1.0d0.and.mv(8).ge.mC4))then
         cont=cont+1
         picos(4)(2:2)='T'
      endif
      if (mv(9).ge.mC.or.(mv(9).gt.1.0d0.and.mv(10).ge.mC5))then
         cont=cont+1
         picos(5)(2:2)='T'
      endif
      if (mv(11).ge.mC.or.(mv(11).gt.1.0d0.and.mv(12).ge.mC5))then
         cont=cont+1
         picos(6)(2:2)='T'
      endif
      nPicosAnchos=cont
      end
c
c
      integer function picosAnchosTD(mv,m,MQ)
        implicit none
c  INPUT PARAMETERS
        real*8 mv(14)
        integer m,MQ
c  LOCAL PARAMETERS
        integer i,cont
        real*8 mC,mC1,mC2,mC3,mC4,mC5,mC6,mCe4
c -------------------------------------------
      cont=0
      if (MQ.eq.12.and.m.eq.112) then
        mCe4=3.0d0
        mC4=3.01d0
      elseif (MQ.eq.12.and.m.eq.79) then
        mCe4=2.81d0
        mC4=2.82d0
      else
c  No se han calculado los valores críticos optimos para otros M
        picosAnchosTD=0
        return
      endif
      if (mv(MQ+1).ge.mCe4.or.
     $     (mv(MQ+1).gt.1.0d0.and.mv(MQ+2).ge.mC4))then
         cont=cont+1
      endif
      picosAnchosTD=cont
      end
