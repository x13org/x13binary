c           Special.f
c              Here we calculate the mathematical functions:
c              LogGamma(x):  log(Gamma(x))
c              BetaInc: the beta cumulative distribution 
c              Fcdp:    F cumulative distribution
c              tcdp:    t cumulative distribution
c              suma:    sum of values
c              suma2:   squared sum of values

      real*8 function LogGamma(a)
c     INPUT PARAMETERS
      real*8 a
c     LOCAL PARAMETERS
      integer j
      real*8 suma,s2pi,tmp,y,c0,c(6)
      SAVE c
      parameter (c0=1.000000000190015d0)
      DATA c /76.18009172947146d0,-86.50532032941677d0,
     &       24.01409824083091d0,-1.231739572450155d0,
     &       .1208650973866179d-2,-.5395239384953d-5/
      parameter (s2pi=2.5066282746310005d0)

      y=a
      tmp=a+5.5d0
      tmp=(a+.5d0)*log(tmp)-tmp
      suma=c0
      do j=1,6
        y=y+1.0d0
        suma=suma+c(j)/y
      end do
      LogGamma=tmp+log(s2pi*suma/a)
      return
      end

      real*8 function BetaInc(x,a,b)
c     Return the probability of value<=x for distribution Beta(a,b)
      implicit none
      real*8 a,b,x,e
      external LogGamma,BetaCfra
      real*8 bt,LogGamma,BetaCfra
C     INTRINSIC FUNCTIONS
      intrinsic LOG,EXP,MAX

      if (x .le. 0.0d0) then
        BetaInc=0.0d0
        return
      elseif (x .ge. 1.0d0) then
        BetaInc=1.0d0
        return
      end if
      e=LogGamma(a+b)-LogGamma(a)-LogGamma(b)
     &               +a*LOG(x)+b*LOG(1.0d0-x)
      bt=EXP(max(e,-500.0D0)) !To avoid underflow exception
      if (x .lt. (a+1.0d0)/(a+b+2.0d0)) then 
        BetaInc=bt*BetaCfra(a,b,x)/a
      else
        BetaInc=1.0d0-bt*BetaCfra(b,a,1.0d0-x)/b
      end if
      end

      real*8 function BetaCfra(a,b,x)
c     continued fraction evaluation for Beta Incomplete function
c      using Lentz's method
      implicit none
      real*8 a,b,x
c     LOCAL PARAMETERS
      integer MaxItera
      real*8 EPS,FP_MIN
      parameter(MaxItera=1000,EPS=1.0d-7,FP_MIN=1.0D-78)
      integer m,m2
      real*8 aa,c,d,f,del

      d=1.0-(a+b)*x/(a+1)
      if (abs(d) .le. FP_MIN) d=FP_MIN
      d=1.0/d
      c=1.0
      f=d
      do m=1,MaxItera
c       even step
        m2=2*m
        aa=m*(b-m)*x/((a-1+m2)*(a+m2))
        d=1.0d0+aa*d 
        if (abs(d) .le. FP_MIN) d=FP_MIN
        d=1.0d0/d
        c=1.0d0+aa/c
        if (abs(c) .le. FP_MIN) c=FP_MIN
        f=f*d*c
c       odd step
        aa=-(a+m)*(a+b+m)*x/((a+m2)*(a+1+m2))
        d=1.0d0+aa*d
        if (abs(d) .lt. FP_MIN) d=FP_MIN
        d=1.0d0/d
        c=1.0d0+aa/c
        if (abs(c) .lt. FP_MIN) c=FP_MIN
        del=d*c
        f=f*d*c
        if (abs(del-1.0d0) .lt. EPS) then
          BetaCfra=f
          return
        end if
      end do
      BetaCfra=f
      write(*,*)'Error MaxItera too small in BetaCfra'
      return
      end


      real*8 function Fcdf(F,x,y)
c     returns the probability of F(x,y)<=F for a given F value
      real*8 F,x,y
      real*8 BetaInc
      
      Fcdf=1-BetaInc(y/(y+x*F),y/2,x/2)
      return
      end

      real*8 function tcdf(t,df)
c     returns the probability of t(df)<=t for a given t value 
      real*8 t,df
      real*8 BetaInc

      if (t .ge. 0) then
        tcdf=1-.5*BetaInc(df/(df+t*t),df/2,.5d0)
      else
        tcdf=.5*betainc(df/(df+t*t),df/2,.5d0)
      end if
      return
      end


      real*8 function suma(a,n1,n2)
      implicit none
      real*8 a(*)
      integer n1,n2,i
      real*8 tmp
      tmp=0
      do i=n1,n2
        tmp=tmp+a(i)
      end do
      suma=tmp
      end
      
      
      real*8 function suma2(a,n1,n2)
      implicit none
      real*8 a(*)
      integer n1,n2,i
      real*8 tmp
      tmp=0
      do i=n1,n2
        tmp=tmp+a(i)*a(i)
      end do
      suma2=tmp
      end

      real*8 function variance(a,n1,n2)
      implicit none
      real*8 a(*),m,va
      integer n1,n2,i
      external suma,suma2
      real*8 suma,suma2
      
      m=suma(a,n1,n2)/DBLE(n2-n1+1)
      va=suma2(a,n1,n2)/DBLE(n2-n1+1) - m*m
      variance=(DBLE(n2-n1+1)/DBLE(n2-n1))*va
      end

      real*8 function calcQS3(Z,nz,mq)
C
C      THIS SUBROUTINE CALCULATES THE PIERCE qs STATISTIC OF THE
C       Z SERIES 
C       NZ : NUMBER OF OBSERVATIONS OF THE SERIES
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      integer n10
      parameter (n10 = 10)
C
C.. Formal Arguments ..
      integer nz,nw,mq
      real*8 z(*)
C
C.. Local Scalars ..
      integer i,j,k,nr
      real*8 c0,QS
C
C.. Local Arrays ..
      real*8 c(5*n10), r(24)
C ... Executable Statements ...
      c0 = 0.0d0
      do i = 1,nz
       c0 = c0 + z(i)*z(i)
      end do
      c0 = c0 / nz
      nr=MQ+MQ
      do k = 1,nr
       c(k) = 0.0d0
       j = k + 1
       do i = j,nz
        c(k) = c(k) + z(i)*z(i-k)
       end do
       c(k) = c(k) / nz
       r(k) = c(k) / c0
      end do  
      QS = 0.0d0   
      if (mq.ne.1 .and. r(mq).gt.0.0d0) then       
       do j = 1,2
        k = j * mq
        if (r(k).gt.0d0) then
         QS = QS + (r(k)*r(k))/(nz-k)
        end if
       end do
       QS = QS * nz * (nz+2)
      end if   
      calcQS3=QS
      return   
      end
c
c
c
      integer function indexGE(val,arrVal,nArrVal)
      implicit none
c    INPUT
      integer nArrVal
      real*8 Val,arrVal(nArrVal)
c    LOCAL
      integer i
c----------------------------------------------------
      i=0
      do while(i.lt.nArrVal)
        if (arrVal(i+1).le.val) then
          i=i+1
        else
          indexGE=i
          return
        end if
      enddo
      indexGE=i
      end 
c
c
c
      subroutine acuArea(Arr,nArr,acuArr)
      implicit none
c   INPUT PARAMETERS
      integer nArr
      real*8 Arr(nArr)
c   OUTPUT
      real*8 acuArr(nArr)
c   LOCAL
      integer i
      real*8 tmpAcu
c---------------------------------------------------
      tmpAcu=0.0d0
      do i=1,nArr
        tmpAcu=tmpAcu+arr(i)
        acuArr(i)=tmpAcu
      enddo
      end subroutine
c
c
c
      integer function Median(arr,nArr)
      implicit none
      include 'spectrum.i'
c   INPUT
      real*8 arr(Lspect)
      integer nArr
c   LOCAL
      real*8 acuArr(Lspect)
c   EXTERNAL
      integer indexGE
      external indexGE
c---------------------------------------------------
      call acuArea(arr,nArr,acuArr)
      median=indexGE(acuArr(nArr)/2.0d0,AcuArr,nArr)
      end function
c
c
c
      integer function indexMax(Arr,nArr)
      implicit none
      include 'spectrum.i'
c   INPUT
      integer nArr
      real*8 arr(Lspect)
c   LOCAL
      integer ind,i
      real*8 val
c --------------------------------------------------
      ind=1
      val=arr(1)
      do i=2,nArr
        if (arr(i).gt.val) then
          ind=i
          val=arr(i)
        end if
      enddo
      indexMax=ind
      end function
c
cc
c
      real*8 function MEANxI(arr,nArr)
      implicit none
c   INPUT
      integer nArr
      real*8 arr(nArr)
c   LOCAL
      integer i
      real*8 sum,sum2
c --------------------------------------------------
      sum=0.0d0
      sum2=0.0d0
      do i=1,nArr
        sum=sum+DBLE(i)*Arr(i)
        sum2=sum2+Arr(i)
      enddo
      sum=sum/sum2
      MEANxI=sum
      end function
c
c
c     Writes MODE period, MEAN period and Median period of a spectrum
      subroutine areaStat(Arr,nArr,MQ,Caption,DBD)
      implicit none
      include 'stream.i'
      include 'htmlout.cmn'
      real*8 pi,tol
      parameter (pi = 3.14159265358979d0,tol=1.0d-8)
c   INPUT PARAMETERS
      integer nArr,MQ,DBD
      integer inicP
      real*8 arr(nArr)
      character Caption*(*)
c   LOCAL PARAMETERS
      integer iMode,iMedian,lCap
      real*8 fMode,fMean,fMedian
c   EXTERNAL
      integer istrlen,indexMax,Median
      real*8 MEANxI
      external MEANxI,istrlen,indexMax,Median
c ---------------------------------------------------
      lCap=istrLen(Caption)
      if (DBD.ge.3) then
        fmode=0.0d0
        fmean=0.0d0
        fmedian=0.0d0
      else
        iMode=indexMax(Arr,nArr)
        fMean=MEANxI(Arr,nArr)
        iMedian=Median(Arr,nArr)
        fMode=DBLE((imode)*MQ)/DBLE((nArr)*2)
        fMean=(fMean)*DBLE(MQ)/DBLE((nArr)*2)
        fMedian=DBLE((iMedian)*MQ)/DBLE((nArr)*2)
      end if
*        call addIdx2(Nidx,nio,Caption(1:lCap),Caption(1:lCap),0,3)
      CALL mkPOneLine(Nio,'bold',Caption(1:lCap))
        if (fMode.gt.tol) then
          write(nio,1010)'<p>','MODE',1.0d0/fMode
        else
          write(nio,1020)'<p>','MODE','INF'
        end if
        if (fMean.gt.tol) then
          write(nio,1010)Cbr,'MEAN',1.0d0/fMean
        else
          write(nio,1020)Cbr,'MEAN','INF'
        end if
        if (fMedian.gt.tol) then
          write(nio,1010)Cbr,'MEDIAN',1.0d0/fMedian
        else
          write(nio,1020)Cbr,'MEAN','MEDIAN'
        end if
      CALL writTag(nio,'</p>')
 1010 FORMAT(a,'<em>',a,'   = </em>',F12.2,' years cycle')
 1020 FORMAT(a,'<em>',a,'   = </em>   ',a,' years cycle')
      end subroutine
      