C     Last change:  BCM  15 Nov 2002    1:54 pm
C
C     THE SUBROUTINE ESTBUR ESTIMATES THE COMPONENTS USING THE
C     METHOD OF  BURMAN(1980).
C     THE FILTERS NUMERATOR ARE PASSED:
C     CT=TREND; CS=SEASONAL; CC=CYCLE;
C
C       CT(B,F)           GT(B)    GT(F)
C      -----------  =    ------ + ------    (TUNICLIFFE-WILSON ALGOR.)
C      TH(B)*TH(F)        TH(B)    TH(F)
C
C    THE COMMON "ESTB" CONTAINS :
C             NCHI : THE DIMENSION OF TREND NUMERATOR MODEL
C             NCYC : THE DIMENSION OF CYCLE NUMERATOR MODEL
C
C SEE THAT THE DIMENSION OF SEASONAL NUMERATOR MODEL IS PASSED
C
C
C      INPUT PARAMETERS
C     BZ : THE REVERSED ORIGINAL SERIES AND THE BACKCAST
C TOTDEN : TOTAL DENOMINATOR OF THE MODEL (true signs)
C  PSTAR : DIMENSION OF TOTDEN
C THSTAR : TOTAL NUMERATOR OF THE MODEL (true signs)
C  QSTAR : DIMENSION OF THSTAR
C     CT : NUMERATOR TREND FILTER
C     CS : NUMERATOR SEASONAL FILTER
C     CC : NUMERATOR CYCLE FILTER
C     MQ : FREQUENCY
C    ZAF : FORECAST SERIES (COMPUTED IN FCAST)
C    ZAB : BACKCAST SERIES (COMPUTED IN FCAST)
C   NPSI : DIMENSION OF THE SEASONAL NUMERATOR MODEL
C      D : DELTA OF MODEL
C     BD : DELTA^MQ OF THE MODEL
c   nCycTH   : MA dimension -1 of the Cycle component
C   varwnc   : Innovation variance of Cycle component
c    IMEAN   : If mean is choosen in the Seats model
c IsCloseToTD: If the Transitory component is a stochastic Td component
c
c     OUTPUT PARAMETERS
C    TREND  : THE TREND COMPONENT including forecast
C       SC  : THE SEASONAL COMPONENT including forecast
C    CYCLE  : THE CYCLE COMPONENT including forecast
C       SA  : Seasonal Adjusted Component including forecast
C       IR  : Irregular component including forecast
C  ForBias  : Forecast of Z
C  ForTbias : Forecast of Trend
C  ForSBias : Forecast of SC
C
c     INPUT/OUTPUT PARAMETERS
C      Z : THE ORIGINAL SERIES AND THE FORECAST (At the end of the function is the forecast of Tramo)
C     LF : NUMBER OF FORECAST AND BACKCAST, If TRAMO<>0 NF=NF-MQ/2
C
C
      subroutine ESTBUR(z,bz,totden,pstar,thstar,qstar,ct,cs,cc,mq,zaf,
     $                  zab,trend,sc,cycle,sa,ir,npsi,d,bd,lf,forbias,
     $                  fortbias,forsbias,ncycth,varwnc,imean,
     $                  iscloseToTD)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
      include 'units.cmn'
      integer np
      REAL*8 ZERO
      parameter (np = 60, ZERO=0D0)
C
C.. Formal Arguments ..
      logical isCloseToTD
      integer pstar,qstar,mq,npsi,d,bd,lf,ncycth,imean
      real*8 z(*),bz(*),totden(*),thstar(*),ct(*),cs(*),cc(*),zaf,zab,
     $       trend(*),sc(*),cycle(*),sa(*),ir(*),forbias(kp),
     $       fortbias(kp),forsbias(kp),varwnc
C
C.. Local Scalars ..
      integer i,iqrow,irow,j,k,l1,m,maxpq,n,nqst1,nz1,l2
      real*8 sum,sum1,sum2,sum3,sum4,sum5,sum6,wmb,wmf,zmean,maxZ,sum1b
C
C.. Local Arrays ..
      real*8 am(60,66),bxc(mpkp+np),bxs(mpkp+np),
     $       bxt(mpkp+np),byc(mpkp+np),
     $       bys(mpkp+np),byt(mpkp+np),
     $       fxc(mpkp+np),fxs(mpkp+np),
     $       fxt(mpkp+np),fyc(mpkp+np),
     $       fys(mpkp+np),fyt(mpkp+np),
     $       extZ(mpkp),d1(mpkp)
C
C.. External Functions ..
      real*8 DMEAN
      external DMEAN
C
C.. External Calls ..
      external MLTSOL
C
C.. Intrinsic Functions ..
      intrinsic MAX,abs
      include 'estb.i'
      include 'sform.i'
      include 'estgc.i'
      include 'stream.i'
      include 'sig.i'
      include 'preadtr.i'
C
C ... Executable Statements ...
C
      CALL setdp(ZERO,mpkp,extZ)
      maxpq = MAX(pstar,qstar)
      l1= MAX(maxpq+Qstar,Mq+Mq)
      l1=max(l1,lf)
      l2=min(l1,qstar+maxpq-2)
      l2=max(l2,lf)
*      if (l.gt.lf) l = lf
*      write(Mtprof,*)'  extz   bz '
      do i=1,nz+l2
        extZ(i)=z(i)
      enddo
*      write(Mtprof,*)'  -----'
*      write(Mtprof,*)' maxpq=',maxpq,' Qstar=',Qstar,' l=',l
*      write(Mtprof,*)' l2=',l2,' nz+l2=',nz+l2
      if ((d+bd .eq. 0).and.(imean.eq.1)) then
       zmean = DMEAN(Nz,z)
       do i = 1,Nz+l2
        extz(i) = extz(i) - zmean
        bz(i) = bz(i) - zmean
       end do
       do i = 1,kp
        forbias(i) = forbias(i) - zmean
       end do
       zab = ZERO
       zaf = ZERO
      else
       zmean = ZERO
      end if
      do i = 1,kp
       fortbias(i) = ZERO
       forsbias(i) = ZERO
      end do
C
C PARTITION EACH FILTER INTO TWO 1-SIDED FILTERS
C
      if (pstar .ne. qstar) then
       nqst1 = qstar + 1
       do i = nqst1,pstar
        thstar(i) = ZERO
       end do
      end if
C
C SET UP MATRIX
C
      do i = 1,maxpq
       do j = i,maxpq
        am(i,j) = ZERO
       end do
       do j = 1,i
        am(i,j) = thstar(i-j+1)
       end do
       m = maxpq - i + 1
       do j = m,maxpq
        am(i,j) = am(i,j) + thstar(maxpq-j+m)
       end do
       k = maxpq - i + 1
       am(i,maxpq+1) = ct(k)
       am(i,maxpq+2) = cs(k)
       am(i,maxpq+3) = cc(k)
      end do
      m = 3
*      WRITE(Mtprof,*)'  subroutine ESTBUR, call 1'
      call MLTSOL(am,maxpq,m,60,66)
      do i = 1,maxpq
       k = maxpq - i + 1
       gt(k) = am(i,maxpq+1)
       gs(k) = am(i,maxpq+2)
       gc(k) = am(i,maxpq+3)
      end do
*      WRITE(NIO,2003) 'GT(B)'//'<br>'
*      WRITE(NIO,2004)(GT(I),I=1,MAXPQ)
*      WRITE(NIO,*)'</p>'
*      WRITE(NIO,2003) 'GS(B)'//'<br>'
*      WRITE(NIO,2004) (GS(I),I=1,MAXPQ)
*      WRITE(NIO,*)'</p>'
*      WRITE(NIO,2003) 'GC(B)'//'<br>'
*      WRITE(NIO,2004) (GC(I),I=1,MAXPQ)
*      WRITE(NIO,*)'</p>'
* 2003 FORMAT (//'<p> NUMERATOR OF 1-SIDED FILTER ',a)
* 2004 FORMAT (12F12.5)
C
C RE-ARRANGE COEFFICIENTS FROM ESTIMATION PROGRAM
C
      l1 = maxpq + qstar - 2
C      M = NZ+1
C      K = NZ+L1
C      WRITE(NIO,2005) (Z(I),I=M,K)
C      WRITE(NIO,2006) (BZ(I),I=M,K)
C 2005 FORMAT (//'0FORECAST OF Z-SERIES'/(12F12.4))
C 2006 FORMAT (//'0BACKCAST OF Z-SERIES'/(12F12.4))
C
C APPLY FILTERS GT AND GS TO FORWARD AND BACKWARDS
C SERIES TO OBTAIN Y-SERIES
C
      n = Nz + qstar - 1
      do i = 1,n
       sum1 = ZERO
       sum2 = ZERO
       sum3 = ZERO
       sum4 = ZERO
       sum5 = ZERO
       sum6 = ZERO
*       write(Mtprof,*)' i= ',i
       do j = 1,maxpq
        m = i + j - 1
        sum1 = sum1 + gt(j)*extz(m)
        sum2 = sum2 + gt(j)*bz(m)
        sum3 = sum3 + gs(j)*extz(m)
        sum4 = sum4 + gs(j)*bz(m)
        sum5 = sum5 + gc(j)*extz(m)
        sum6 = sum6 + gc(j)*bz(m)
*        write(Mtprof,*)'  extz(',m,')= ',extz(m),
*     &             '  gt(',j,')= ',gt(j)
*        write(Mtprof,*)'  gt(',j,')*extz(',m,')= ',gt(j)*extz(m)
       end do
*       write(Mtprof,*)'  sum1 ',sum1
*       write(Mtprof,*)'  -----'
       fyt(i) = sum1
       byt(i) = sum2
       fys(i) = sum3
       bys(i) = sum4
       fyc(i) = sum5
       byc(i) = sum6
      end do
      if (qstar .eq. 1) then
*       write(Mtprof,*)'   j     fxt(j)'
       do j = 1,Nz
        fxt(j) = fyt(j)
        bxt(j) = byt(j)
        fxs(j) = fys(j)
        bxs(j) = bys(j)
        fxc(j) = fyc(j)
        bxc(j) = byc(j)
*         write(Mtprof,*)'   ',j,'   ',fxt(j)
       end do
*       write(Mtprof,*)'  -----'
      else
C
C DERIVE (PSTAR+QSTAR) TERMS OF X-SERIES BY SOLVING EQUATIONS
C
       irow = pstar + qstar - 2
       wmf = 0.5 * zaf
       wmb = 0.5 * zab
*       write(Mtprof,*)' irow = ',irow,' wmf = ',wmf,' wmb = ',wmb
       do i = 1,irow
        do j = 1,irow
         am(i,j) = ZERO
        end do
       end do
       n = Nz + qstar - pstar
       iqrow = qstar - 1
*       write(Mtprof,*)' iqrow = ',iqrow,' pstar = ',pstar
       do i = 1,iqrow
*        write(Mtprof,*)' i = ',i
        do j = 1,pstar
         m = i + j - 1
         am(i,m) = totden(j)
*         write(Mtprof,*)' j = ',j,' totden(j) = ',totden(j)
        end do
        am(i,irow+1) = wmf
        am(i,irow+2) = wmb
        do j = 3,6
         am(i,irow+j) = ZERO
        end do
       end do
       do i = qstar,irow
*        write(Mtprof,*)' i = ',i
        do j = 1,qstar
         m = i - j + 1
         am(i,m) = thstar(j)
*         write(Mtprof,*)' j = ',j,' thstar(j) = ',thstar(j)
        end do
        k = n + irow - i + 1
        am(i,irow+1) = fyt(k)
        am(i,irow+2) = byt(k)
        am(i,irow+3) = fys(k)
        am(i,irow+4) = bys(k)
        am(i,irow+5) = fyc(k)
        am(i,irow+6) = byc(k)
*        write(Mtprof,*)' k = ',k,' fyt(k) = ',fyt(k),' byt(k) = ',byt(k)
*        write(Mtprof,*)' fys(k) = ',fys(k),' bys(k) = ',bys(k)
*        write(Mtprof,*)' fyc(k) = ',fyc(k),' byc(k) = ',byc(k)
       end do
       m = 6
*       write(Mtprof,8999)'R/C',(i,i=1,irow+m)
* 8999  format(3x,a3,6(5x,i5),/,4(6x,6(5x,i5),/),6x,3(5x,i5))
*       do i = 1, irow
*        write(Mtprof,9000)i, (am(i,j), j = 1, irow+m)
* 9000   format(i6,6f10.6,/,4(6x,6f10.6,/),6x,3f10.6)
*       end do
*       write(Mtprof,*)'------'
*       call profiler(3,'subroutine MLTSOL, call 2')
       call MLTSOL(am,irow,m,60,66)
       do i = 1,irow
        k = n + irow - i + 1
        fxt(k) = am(i,irow+1)
        bxt(k) = am(i,irow+2)
        fxs(k) = am(i,irow+3)
        bxs(k) = am(i,irow+4)
        fxc(k) = am(i,irow+5)
        bxc(k) = am(i,irow+6)
*        write(Mtprof,9001)(am(i,j),j=irow+1,irow+6)
 9001   format(6f12.6)
       end do
*       write(Mtprof,*)'------'

c OBTAIN REST OF X-SERIES BY RECURRENCE AND
c COMBINE X-SERIES TO GIVE SC AND TREND

*       write(Mtprof,*)
*     &       ' sum1b     j     thstar(j)    k     fxt(k)     sum1'
       do i = 1,n
        m = n - i + 1
        sum1 = fyt(m)
        sum2 = byt(m)
        sum3 = fys(m)
        sum4 = bys(m)
        sum5 = fyc(m)
        sum6 = byc(m)
        do j = 2,qstar
         k = m + j - 1
         sum1b = sum1
         sum1 = sum1 - thstar(j)*fxt(k)
         sum2 = sum2 - thstar(j)*bxt(k)
         sum3 = sum3 - thstar(j)*fxs(k)
         sum4 = sum4 - thstar(j)*bxs(k)
         sum5 = sum5 - thstar(j)*fxc(k)
         sum6 = sum6 - thstar(j)*bxc(k)
*         write(Mtprof,*)'  ',sum1b,'  ',j,'  ',thstar(j),'  ',k,'  ',
*     &              fxt(k),'  ',sum1
        end do
        fxt(m) = sum1
        bxt(m) = sum2
        fxs(m) = sum3
        bxs(m) = sum4
        fxc(m) = sum5
        bxc(m) = sum6
       end do
*       write(Mtprof,*)'------'
      end if
      do i = 1,Nz
       trend(i) = fxt(i) + bxt(Nz-i+1)
*       write(Mtprof,*)' trend(',i,') = ',trend(i),' fxt(',i,') = ', 
*     &            fxt(i),' bxt(',Nz-i+1,') = ',bxt(Nz-i+1)
       sc(i) = fxs(i) + bxs(Nz-i+1)
       cycle(i) = fxc(i) + bxc(Nz-i+1)
      end do
C
C IF MODEL TOPHEAVY,CREATE WHITE NOISE IRREGULAR COMPONENT
C
C
      nz1 = Nz+lf 
      do i = Nz+1,nz1
       sc(i) = ZERO
       trend(i) = ZERO
       cycle(i) = ZERO
      end do
C
      if (npsi .ne. 1) then
C
C FORECAST SEASONALS
C
       k = 2*qstar - 1
       if (k .le. kp) then
        do i = k,kp
         sum = ZERO
         do j = 2,pstar
          sum = sum - totden(j)*fxs(Nz+i-j+1)
         end do
         fxs(Nz+i) = sum
        end do
       end if
       do i = 1,qstar
        bxs(Nz-i+1) = bxs(i)
       end do
       do i = 1,kp
        sum = ZERO
        k = Nz + i
        do j = 1,maxpq
         if ((k-j+1) .gt. (Nz+lf)) then
          sum = sum + gs(j)*forbias(k-j+1-Nz)
         else
          sum = sum + gs(j)*extZ(k-j+1)
         end if
        end do
        if (qstar .ne. 1) then
         do j = 2,qstar
          sum = sum - thstar(j)*bxs(k-j+1)
         end do
        end if
        bxs(k) = sum
        if (k .le. Nz+lf) then
         sc(k) = fxs(k) + bxs(k)
         forsbias(k-Nz) = fxs(k) + bxs(k)
        else
         forsbias(k-Nz) = fxs(k) + bxs(k)
        end if
       end do
      end if
C
C      FORECAST TREND
C
      if (Nchi .ne. 1) then
       k = 2*qstar - 1
       if (k .le. kp) then
        do i = k,kp
         sum = ZERO
         do j = 2,pstar
          sum = sum - totden(j)*fxt(Nz+i-j+1)
         end do
         fxt(Nz+i) = sum
        end do
       end if
       do i = 1,qstar
        bxt(Nz-i+1) = bxt(i)
       end do
       do i = 1,kp
        sum = ZERO
        k = Nz + i
        do j = 1,maxpq
         if ((k-j+1) .gt. (Nz+lf)) then
          sum = sum + gt(j)*forbias(k-j+1-Nz)
         else
          sum = sum + gt(j)*extZ(k-j+1)
         end if
        end do
        byt(k) = sum
        if (qstar .ne. 1) then
         do j = 2,qstar
          sum = sum - thstar(j)*bxt(k-j+1)
         end do
        end if
        bxt(k) = sum
        if (k .le. Nz+lf) then
         trend(k) = fxt(k) + bxt(k)
         fortbias(k-Nz) = fxt(k) + bxt(k)
        else
         fortbias(k-Nz) = fxt(k) + bxt(k)
        end if
       end do
      end if
C
C
C
      if (varwnc.gt.1.0d-10  .and. (ncycth.ne.0 .or. Ncyc.ne.1)) then
C
C FORECAST CYCLE
C
       k = 2*qstar - 1
       if (k .le. lf) then
        do i = k,lf
         sum = ZERO
         do j = 2,pstar
          sum = sum - totden(j)*fxc(Nz+i-j+1)
         end do
         fxc(Nz+i) = sum
        end do
       end if
       do i = 1,qstar
        bxc(Nz-i+1) = bxc(i)
       end do
       do i = 1,lf
        sum = ZERO
        k = Nz + i
        do j = 1,maxpq
         sum = sum + gc(j)*extZ(k-j+1)
        end do
        if (qstar .ne. 1) then
         do j = 2,qstar
          sum = sum - thstar(j)*bxc(k-j+1)
         end do
        end if
        bxc(k) = sum
        cycle(k) = fxc(k) + bxc(k)
       end do
      end if
C       DO 123 I=NZ+1,NZ+MQ
C 123   TREND(I)=Z(I)-CYCLE(I)-SC(I)
C       WRITE(NIO,1998)
C 1998 FORMAT(////50(1H*),' IT''S A VERY TEMPORARY OUTPUT ',50(1H*)//
C     $' FORECAST OF:',10X,'TRANSF. SERIES',10X,'TREND-CYCLE',4X,
C     $'SEAS.',10X,'CYCLE',10X,'DIFFERENCES'/)
C      DO 1999 I=NZ+1,NZ+MQ2
C      A=Z(I)-CYCLE(I)-TREND(I)-SC(I)
C 1999 WRITE(NIO,800) I,SC(I),FORSBIAS(I-NZ),TREND(I),FORTBIAS(I-NZ)
C  800 FORMAT(3X,I5,16X,4(D15.8),5X,D15.8)
      if ((d+bd .eq. 0).and.(imean.eq.1)) then
       do i = 1,Nz+lf
        bz(i) = bz(i) + zmean
       enddo
       do i = 1,Nz+lf
        extz(i) = extz(i) + zmean
        trend(i) = trend(i) + zmean
       end do
       do i = 1,kp
        forbias(i) = forbias(i) + zmean
        fortbias(i) = fortbias(i) + zmean
       end do
      end if
c
c             Z=SA+SC   (Linealized observed series are equal to stochastic SA+SC)
c             Z=Trend+Cycle+Sc+IR   (Linealized observed series are equal to stochastic Trend+Cycle+Sc+Ir)
c
      nz1 = Nz + lf
      if (Npsi .eq. 1) then	            
       do i = 1,nz1
        Sc(i) = 0.0d0
        if (i .le. Nz) then	   
         if (noadmiss.eq.-1) then  
          ir(i) =0.0d0
         else
          ir(i) = z(i) - trend(i) - cycle(i)
         end if
         if (iscloseToTD) then
          sa(i) = z(i) - cycle(i)
         else
          sa(i) = z(i)
         endif
        end if
       end do
      else
       if (isCloseToTD) then
        do i = 1,Nz
         sa(i) = z(i) - sc(i) - cycle(i)
         if (noadmiss.eq.-1) then  
          ir(i) =0.0d0
         else
          ir(i) = z(i) - sc(i) - trend(i) - cycle(i)
         end if
        enddo
      else
        do i = 1,Nz
         sa(i) = z(i) - sc(i)
         if (noadmiss.eq.-1) then  
          ir(i) =0.0d0
         else
          ir(i) = z(i) - sc(i) - trend(i) - cycle(i)
         end if
        enddo
      endif
      end if
      do i = Nz+1,nz1
       ir(i) = 0.0d0
      end do
c
c     To make Z=TramLin (Predictions of Tramo are equal to the ones in Seats)
c             Z=SA+SC   (Predictions of Tramo are equal to SA+SC)
c             Z=Trend+Cycle+Sc+IR   (Predictions of Tramo are equal to Trend+Cycle+Sc+Ir)
c
      if (Tramo.ne.0) then
       do i=1,nz
        d1(i)=0.0d0
       enddo
       if (ILAM.eq.0) then
        do i=nz+1,nz1
         z(i)=trend(i)+sc(i)+cycle(i)+ir(i)
         d1(i)=LOG(TramLin(i))-Z(i)
         z(i)=LOG(TramLin(i))
         forbias(i-nz)=z(i)
        enddo
       else
        do i=nz+1,nz1
         z(i)=trend(i)+sc(i)+cycle(i)+ir(i)
         d1(i)=TramLin(i)-Z(i)
         z(i)=TramLin(i)
        enddo
       endif
       lf=lf-mq/2
       if (mq.ne.3)then 
c       CASE    MQ=2,4,6,12
        if (npsi.gt.1)then
         if (nchi.gt.1) then
          do i=Nz+1,nz+lf
           sc(i)=sc(i)+d1(i)-(d1(i+mq/2)+d1(i-mq/2))/dble(2*mq)
           do j=1-mq/2,mq/2-1 
            SC(i)=sc(i)-d1(i+j)/dble(mq)
           enddo
          enddo
         else
          do i=Nz+1,nz+lf
           sc(i)=sc(i)+d1(i)
          enddo
         endif
        elseif (.not.isCloseToTD .and. varwnc.gt.1.0d-10  .and.
     &         (ncycth.ne.0 .or. Ncyc.ne.1)) then
         if (nchi.gt.1) then           
          do i=Nz+1,nz+lf
           cycle(i)=cycle(i)+d1(i)-(d1(i+mq/2)+d1(i-mq/2))/dble(2*mq)
           do j=1-mq/2,mq/2-1 
            cycle(i)=cycle(i)-d1(i+j)/dble(mq)
           enddo
          enddo
         else
          do i=NZ+1,nz+lf
           cycle(i)=cycle(i)+d1(i)
          enddo
         endif
        else
         if (nchi.gt.1) then
          do i=Nz+1,nz+lf
           ir(i)=d1(i)-(d1(i+mq/2)+d1(i-mq/2))/dble(2*mq)
           do j=1-mq/2,mq/2-1 
            ir(i)=ir(i)-d1(i+j)/dble(mq)
           enddo
          enddo
         else
          do i=NZ+1,nz+lf
           ir(i)=d1(i)
          enddo
         endif
        endif
       else
c      CASE  MQ=3
        if (npsi.gt.1)then
         if (nchi.gt.1) then
          do i=Nz+1,nz+lf
           sc(i)=sc(i)+d1(i)-(d1(i-1)+d1(i)+d1(i+1))/dble(3)
          enddo
         else
          do i=NZ+1,nz+lf
           sc(i)=sc(i)+d1(i)
          enddo
         endif
        elseif (.not.isCloseToTD .and. varwnc.gt.1.0d-10  .and.
     &         (ncycth.ne.0 .or. Ncyc.ne.1)) then 
         if (nchi.gt.1) then          
          do i=Nz+1,nz+lf
           cycle(i)=cycle(i)+d1(i)-(d1(i-1)+d1(i)+d1(i+1))/dble(3)
          enddo
         else
          do i=Nz+1,Nz+lf
           cycle(i)=cycle(i)+d1(i)
          enddo
         endif
        else
         if (nchi.gt.1) then
          do i=Nz+1,nz+lf
           ir(i)=d1(i)-(d1(i+mq/2)+d1(i-mq/2))/dble(2*mq)
           do j=1-mq/2,mq/2-1 
            ir(i)=ir(i)-d1(i+j)/dble(mq)
           enddo
          enddo
         else
          do i=NZ+1,nz+lf
           ir(i)=d1(i)
          enddo
         endif
        endif
       endif
      endif
      maxZ=0.0d0
      do i=1,nz
       if (abs(z(i)).gt.maxZ) then
        maxZ=abs(Z(i))
       endif
      enddo
      do i = Nz+1,nz1
       trend(i) = z(i) - sc(i) - cycle(i)-ir(i)
       if (abs(trend(i)).lt.(1.0D-15*maxZ))then
        trend(i)=0.0d0
       endif
       fortbias(i-nz)=trend(i)
      end do
      if (isCloseToTD) then
       do i = Nz+1,nz1
        sa(i) = z(i) - sc(i) - cycle(i)
        forsbias(i-nz)=sc(i)
       end do
      else
       do i = Nz+1,nz1
        sa(i) = z(i) - sc(i)
        forsbias(i-nz)=sc(i)
       end do
      endif
      end
C
C  THIS SUBROUTINE COMPUTES THE DECOMPOSITION OF A FRACTION
C                       P (B,F)
C                      ----------
C                       Q (B,F)
C
C  WHERE P(B,F), Q(B,F) ARE POLYNOMIALS IN B AND F IN THE SUM OF TWO
C
C    P(B,F) AND Q(B,F) ARE COMPUTED AS :
C
C     P(B,F)= THc(B)*THc(F)*PHInc(F)
C
C     Q(B,F)= PHIc(B)*TH(F)
C
C
C               R(B)     T(F)
C  FRACTION    ------ + ------
C              PHIc(B)   TH(F)
C
C NOTE : ALL INPUT POLYNOMIAL ARE IN B
C
c     DECFB given the model of a component and the ARIMA model return the PSIES of the component estimator
c     INPUT 
c          PHIc(1:lPHIc) AR of component in Box-Jenkins signs
c          THc(1:lTHc)   MA of component in Box-Jenkins signs
c          Vc:           Variance of component in units of Va(variance of residuals)
c          TH(1:lTH)     MA of ARIMA model of serie in Box-Jenkins sign
c          PHInc(1:lPHInc) Conv(PHInc,PHIc)=AR of ARIMA model in Box-Jenkins sings
c     OUTPUT
c          PSI(0:2pk) PSIEs of component estimator from F^pk to B^pk-1
c          Rce(0):   variance of concurrent revision error of component estimator
c          Rce(1:12): correlations 1 to 12 of concurrent revision error
c          H(1:lH): the MA of the revision Error
c          Vr:      the variance of the innovations of the revision error
c          E_B(0:lB): MA of concurrent estimator model
      subroutine DECFB(PHIc,TH,lPHIc,lTH,THc,PHInc,lTHc,lPHInc,Vc,
     $                 PSI,pk,Rce,H,lH,Vr,E_B,lB)
C
      implicit none
      real*8 ZERO
      parameter (ZERO=0.0d0)
      include 'units.cmn'
c     INPUT PARAMETERS
      integer lPHIc,lTHc,lTH,lPHInc,pk
      real*8  PHIc(*),THc(*),TH(*),PHInc(*),Vc
c     OUTPUT PARAMETERS
      real*8 PSI(0:2*pk+1),rce(0:12),Vr,H(60-1),E_B(0:60-1)
      integer lH,lB
c     LOCAL PARAMETERS
      integer lF,i,lM
      real*8 M(60-1),eM(60),eTH(60),ePHIc(60),eTHc(60),
     $       H_F(0:60-1),PSIE_B(1:pk+1),PSIE_F(1:pk+1),
     $       H1,TH1(60),Ve,g(60),rho(0:12)
C
C.. External Calls ..
      external CHBJB, MPBBJ, SeparaBF, getPSIE
C
C ... Executable Statements ...
C
C
C CALCULO DE ALFA POR FISp
C
      call MPBBJ(PHInc,THc,lPHInc,lTHc,M)
      lM=lPHInc+lTHc
      call CHBJB(M,lM,eM)
      call CHBJB(TH,lTH,eTH)
      call CHBJB(THc,lTHc,eTHc)
      call CHBJB(PHIc,lPHIc,ePHIc)
      call SeparaBF(eTHc,lTHc,eM,lM,ePHIc,lPHIc,eTH,lTH,
     $             E_B,lB,H_F,lF)
      call getPSIE(E_B,lB,ePHIc,lPHIc,Vc,pk+1,PSIE_B)
      call getPSIE(H_F,lF,eTH,lTH,Vc,pk+1,PSIE_F)
      DO i=pk,1,-1
        PSI(pk-i)=PSIE_F(i+1)
      end do
      do i=0,pk
        PSI(pk+i)=PSIE_B(i+1)
      end do
c      DO i=2,lF
c       H(i-1)=-H_F(i)/H_F(1)
c     end do
c      lH=lF-1
c     Vr=abs(H_F(1))*Vc
      Do i=0,LF
        H(i+1)=H_F(i)
      end do
      lH=LF+1
      DO while ((H(1) .eq. 0) .and. (lH .gt. 0))
        lH=lH-1
        Do i=1,lH
          H(i)=H(i+1)
        end do
      end do
      if (lH .eq. 0) then
        Vr=0
      else
        H1=H(1)
        Vr=abs(H1)*Vc
        lH=lH-1
        Do i=1,lH
          H(i)=-H(i+1)/H1
        end do
      end if
      Vr=Vr*Vr
c      WRITE(Mtprof,*)'  subroutine DECFB, call 1,lTH = ',lTH
      call BFAC(TH,H,lTH,lH,12,rce,rho,Ve,Vr,g,12)
      if (abs(rce(0)) .lt. 1.0D-20) then
        rce(0)=ZERO
      end if
      do i=1,12
        if (rce(0) .eq. ZERO) then
          rce(i)=0
        else
          rce(i)=rce(i)/rce(0)
       end if
      end do
      end subroutine
C
C  THIS SUBROUTINE CHANGES THE SIGNS OF A POLYNOMIAL
C   TRUE SIGN  ----->  B-J SIGNS
C
C       INPUT PARAMETER
C      A : POLYNOMIAL TRUE SIGN
C      N : DIMENSION OF A
C      B : POLYNOMIAL B-J SIGN
C
      subroutine CHBJB(a,n,b)
C
C.. Implicits ..
      implicit none
      real*8 ONE
      parameter (ONE=1.0d0)
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 a(*)
C.. In/Out Status: Read, Not Written ..
      integer n
C.. In/Out Status: Not Read, Maybe Written ..
      real*8 b(0:*)
C
C.. Local Scalars ..
      integer i
C
C ... Executable Statements ...
C
      b(0) = ONE
      do i = 1,n
       b(i) = -a(i)
      end do
      end
C
C  THIS SUBROUTINE COMPUTES THE PSI-WEIGHTS (B,F) OF A MODEL
C
C     INPUT PARAMETERS
C    FI : DENOMINATOR OF THE MODEL B-J SIGN
C   THE : NUMERATOR OF THE MODEL B-J SIGN
C    NP : DIMENSION OF FI
C    NQ : DIMENSION OF THE
C NLONG : DIMENSION OF PSI-WEIGHTS(B,F)
C   PSI : PSI-WEIGHTS
C    VA : VARIANCE OF THE INNOVATION OF THE MODEL
C
      subroutine DPSI(fi,the,np,nq,nlong,psi,va)
C
C.. Implicits ..
      implicit none
      real*8 ONE
      parameter (ONE=1.0d0)
C
C.. Formal Arguments ..
      integer np,nq,nlong
      real*8 fi(np),the(nq),psi(0:*),va
C
C.. Local Scalars ..
      integer i
C
C.. Local Arrays ..
      real*8 a(0:50),b(0:50)
C
C.. External Calls ..
      external INPOL, MPB
C
C ... Executable Statements ...
C
      a(0) = ONE
      do i = 1,np
       a(i) = -fi(i)
      end do
      call INPOL(a,np,nlong,psi)
      b(0) = va
      do i = 1,nq
       b(i) = -the(i)*va
      end do
      call MPB(b,psi,nq,nlong,psi)
      end
C
C  THIS SUBROUTINE COMPUTES THE PRODUCT OF TWO POLYNOMIALS IN B
C  WITH B-J SIGNS.
C
C      INPUT PARAMETERS
C     A : FIRST POLYNOMIAL IN B
C     B : SECOND POLYNOMIAL IN B
C     N : DIMENSION OF A
C     M : DIMENSION OF B
C     E : PRODUCT A * B
C
      subroutine MPBBJ(a,b,n,m,e)
C
C.. Implicits ..
      implicit none
      real*8 ZERO,ONE
      parameter (ZERO=0.0d0,ONE=1.0d0)
C
C.. Formal Arguments ..
C.. In/Out Status: Read, Not Written ..
      integer n
C.. In/Out Status: Read, Not Written ..
      integer m
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 a(n)
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 b(m)
C.. In/Out Status: Not Read, Maybe Written ..
      real*8 e(n+m)
C
C.. Local Scalars ..
      integer i
C
C.. Local Arrays ..
      real*8 aa(0:50),bb(0:50)
C
C.. External Calls ..
      external MPB
C
C ... Executable Statements ...
C
      aa(0) = ONE
      bb(0) = ONE
      do i = 1,50
       aa(i) = ZERO
       bb(i) = ZERO
      end do
      if ((n+m) .gt. 0) then
       do i = 1,n+m
        e(i) = ZERO
       end do
      end if
      if (n .ge. 1) then
       do i = 1,n
        aa(i) = -a(i)
       end do
      end if
      if (m .ge. 1) then
       do i = 1,m
        bb(i) = -b(i)
       end do
      end if
      call MPB(aa,bb,n,m,aa)
      if ((n+m) .ge. 1) then
       do i = 1,n+m
        e(i) = -aa(i)
       end do
      end if
      end
C
C  THIS SUBROUTINE COMPUTES THE PRODUCT OF TWO POLYNOMIALS IN B
C  WITH TRUE SIGNS (ATTENTION TO THE DIMENSION OF THE POLYNOMIALS)
C
C      INPUT PARAMETERS
C     A : FIRST POLYNOMIAL IN B  (true signs)
C     B : SECOND POLYNOMIAL IN B (true signs)
C     N : DIMENSION OF A
C     M : DIMENSION OF B
C     E : PRODUCT A * B
C
      subroutine MPB(a,b,n,m,e)
C
C.. Implicits ..
      implicit none
      INCLUDE 'srslen.prm'
      integer nfl
      parameter (nfl = 2*POBS)
      real*8 ZERO,MTINY,TINY
      parameter (ZERO=0.0d0)
C
C.. Formal Arguments ..
C.. In/Out Status: Read, Not Written ..
      integer n
C.. In/Out Status: Read, Not Written ..
      integer m
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 a(0:n)
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 b(0:m)
C.. In/Out Status: Maybe Read, Maybe Written ..
      real*8 e(0:*)
C
C.. Local Scalars ..
      integer i,j,k
C
C.. Local Arrays ..
      real*8 vv(0:nfl),ww(0:nfl)
C
C.. Intrinsic Functions ..
      intrinsic MAX
C
C ... Executable Statements ...
C
      TINY=10.d0**(-30.0d0)
      MTINY=-(10.d0**(-30.d0))
      do i = 0,MAX(m,n)
       vv(i) = ZERO
       ww(i) = ZERO
      end do
      do i = 0,m
       if (b(i).le.MTINY .or. b(i).ge.TINY)then
        ww(i) = b(i)
       end if
      end do
      do i = 0,n
       if (a(i).le.MTINY .or. a(i).ge.TINY)then
        vv(i) = a(i)
       end if
      end do
      do i = 0,n+m
       e(i) = ZERO
      end do
      do i = 0,n
       do j = 0,m
        k = i + j
        e(k) = e(k) + vv(i)*ww(j)
       end do
      end do
      do k=1,m+n
        if (abs(e(k)) .lt. 1.0D-28) then
           e(k)=ZERO
        end if
      end do
      end
C
C  THIS SUBROUTINE COMPUTES THE PRODUCT OF TWO POLYNOMIALS THE FIRST IN B
C  THE SECOND IN F WITH TRUE SIGNS
C
C      INPUT PARAMETERS
C     A : POLYNOMIAL IN B (true signs)
C     B : POLYNOMIAL IN B (true signs)
C     N : DIMENSION OF A
C     M : DIMENSION OF B
C     E : PRODUCT A * B
C
      subroutine MPBF(a,b,n,m,e)
C
C.. Implicits ..
      implicit none
      INCLUDE 'srslen.prm'
      integer nfl
      parameter (nfl = 2*POBS)
C
C.. Formal Arguments ..
C.. In/Out Status: Read, Not Written ..
      integer n
C.. In/Out Status: Read, Not Written ..
      integer m
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 a(0:n)
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 b(0:m)
C.. In/Out Status: Maybe Read, Maybe Written ..
      real*8 e(0:n+m)
C
C.. Local Scalars ..
      integer i
C
C.. Local Arrays ..
      real*8 vv(0:nfl),ww(0:nfl)
C
C.. External Calls ..
      external MPB
C
C ... Executable Statements ...
C
      do i = 0,n
       vv(i) = a(i)
      end do
      do i = 0,m
       ww(i) = b(i)
      end do
      do i = 0,m
       e(m-i) = b(i)
      end do
      do i = 0,m
       ww(i) = e(i)
      end do
      call MPB(vv,ww,n,m,e)
      end
C
C THIS SUBROUTINE COMPUTE THE INVERSE OF A POLYNOMIAL IN B
C
C    INPUT PARAMETERS
C
C       A : POLYNOMIAL IN B (true signs)
C       N : DIMENSION OF A
C       M : DIMENSION OF E
C       E : INVERSE POLYNOMIAL (true signs)
C
      subroutine INPOL(a,n,m,e)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
C.. In/Out Status: Read, Not Written ..
      integer n
C.. In/Out Status: Read, Not Written ..
      integer m
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 a(0:n)
C.. In/Out Status: Maybe Read, Maybe Written ..
      real*8 e(0:m)
C
C.. Local Scalars ..
      integer i,j
      real*8 TINY,MTINY
C
C ... Executable Statements ...
C
      TINY=10.d0**(-30.0d0)
      MTINY=-(10.d0**(-30.d0))
      e(0) = 1.d0
      do i = 1,m
       e(i) = 0.d0
      end do
      if (m .gt. n) then
       do i = 1,n
        e(i) = e(i) - a(i)
        do j = 1,i-1
         if ((a(j).le.MTINY .or. a(j).ge.TINY) .and.
     $       (e(i-j).le.MTINY .or. e(i-j).ge.TINY)) then
          e(i) = e(i) - a(j)*e(i-j)
         end if
        end do
       end do
       do i = n+1,m
        do j = 1,n
         if ((a(j).le.MTINY .or. a(j).ge.TINY) .and.
     $       (e(i-j).le.MTINY .or. e(i-j).ge.TINY)) then
          e(i) = e(i) - a(j)*e(i-j)
         end if
        end do
       end do
      else
       do i = 1,m
        e(i) = e(i) - a(i)
        do j = 1,i-1
         if ((a(j).le.MTINY .or. a(j).ge.TINY) .and.
     $       (e(i-j).le.MTINY .or. e(i-j).ge.TINY)) then
          e(i) = e(i) - a(j)*e(i-j)
         end if
        end do
       end do
      end if
      end
C
C  THIS SUBROUTINE COMPUTES AUTOCORRELATION, AUTOCOVARIANCE
C  PSI-WEIGHTS AND THEORETICAL VARIANCE OF AN ARIMA MODEL
C
C      INPUT PARAMETERS
C    PHI : AUTOREGRESSIVE PART OF THE MODEL  B-J SIGN
C     TH : MOVING AVERAGE PART OF THE MODEL  B-J SIGN
C     NP : DIMENSION OF PHI
C     NQ : DIMENSION OF TH
C  NLONG : DIMENSION OF GAM
C    GAM : AUTOCOVARIANCE
C    RHO : AUTOCORRELATIONS
C     VZ : THEORETICAL VARIANCE OF THE MODEL
C     VA : VARIANCE OF THE INNOVATIONS
C      G : DUMMY
C   NRHO : DIMENSION OF RHO
C
      subroutine BFAC(phi,th,np,nq,nlong,gam,rho,vz,va,g,nrho)
C
C.. Implicits ..
      implicit none
      real*8 ZERO
      parameter (ZERO=0.0d0)
      include 'units.cmn'
C
C.. Formal Arguments ..
      integer np,nq,nlong,nrho
      real*8 phi(np),th(nq),gam(0:nlong),rho(0:nrho),vz,va,g(0:*)
C
C.. Local Scalars ..
      integer i,imaxpq,j,m,npst1,nq2,pstar,qstar,sizeg
C
C.. Local Arrays ..
c      real*8 a(0:82),aa(0:82),am(100,100),e(0:999)
      real*8 a(0:100),aa(0:100),am(60,66),e(0:9999)
C
C.. External Calls ..
      external INPOL, MLTSOL, MPB, MPBF
C
C.. Intrinsic Functions ..
      intrinsic ABS, MAX, MIN
C
C ... Executable Statements ...
C
      do i = 1,nlong
       gam(i) = ZERO
      end do
      do i = 0,MAX(np,nq)
       a(i) = ZERO
       aa(i) = ZERO
      end do
      a(0) = 1.0d0
      do i = 1,np
       a(i) = -phi(i)
      end do
      aa(0) = 1.0d0
      do i = 1,nq
       aa(i) = -th(i)
      end do
      call MPBF(aa,aa,nq,nq,e)
      if (np .eq. 0) then
       do i = 0,nlong
        gam(i) = ZERO
       end do
       nq2 = MIN(nq,nlong)
       do i = 0,nq2
        gam(i) = e(nq-i) * va
        if (i .le. nrho) then
         if (ABS(va) .lt. 1.0d-13) then
          rho(i) = ZERO
         else
          rho(i) = gam(i) / gam(0)
         end if
        end if
       end do
       vz = gam(0)
       do i = nq2+1,nrho
        rho(i) = ZERO
       end do
      else
       pstar = np + 1
       qstar = nq + 1
       imaxpq = MAX(pstar,qstar)
       if (qstar .lt. pstar) then
        do i = 0,nq
         e(np-i) = e(nq-i)
        end do
        do i = 0,np-nq-1
         e(i) = ZERO
        end do
       else if (qstar .ne. pstar) then
        npst1 = pstar + 1
        do i = npst1,qstar
         a(i) = ZERO
        end do
       end if
C
C SET UP MATRIX
C
       do i = 1,imaxpq
        do j = 1,imaxpq
         am(i,j) = ZERO
        end do
        do j = 1,i
         am(i,j) = a(i-j)
        end do
        m = imaxpq - i + 1
        do j = m,imaxpq
         am(i,j) = am(i,j) + a(imaxpq-j+m-1)
        end do
        am(i,imaxpq+1) = e(i-1)
       end do
*      WRITE(Mtprof,*)'  subroutine BFAC, call 1'
       call MLTSOL(am,imaxpq,1,60,66)
       do i = 0,imaxpq-1
        aa(i) = am(imaxpq-i,imaxpq+1)
*        g(i) = aa(i)
       end do
c      write(Mtprof,*)'  imaxpq-1,np = ',imaxpq-1,np
       call getPSIE(aa,imaxpq-1,a,np,va,nlong+1,gam)
       gam(0) = 2 * gam(0)
       vz = gam(0)
       do i = 1,min(nlong,nrho)
        if (i .le. nrho) then
         if (ABS(va) .lt. 1.0d-13) then
          rho(i) = ZERO
         else
          rho(i) = gam(i) / gam(0)
         end if
        end if
       end do
      end if
      end
C
C THIS SUBROUTINE COMPUTES THE CROSS CORRELATION BETWEEN TWO SERIES
C
C    INPUT PARAMETERS
C     SER1 : FIRST SERIES
C     SER2 : SECOND SERIES
C       N1 : DIMENSION OF SER1
C       N2 : DIMENSION OF SER2
C        J : NUMBER OF CROSSCORRELATIONS
C        R : CROSSCORRELATIONS DIMENSION R(-J:J)
C
      subroutine CROSS(ser1,ser2,n1,n2,j,r)
C
C.. Implicits ..
      implicit none
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      integer mp,kp,mc
      parameter (kp = PFCST, mp = POBS, mc = 1000)
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 ser1(*)
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 ser2(*)
C.. In/Out Status: Read, Not Written ..
      integer n1
C.. In/Out Status: Maybe Read, Not Written ..
      integer n2
C.. In/Out Status: Maybe Read, Not Written ..
      integer j
C.. In/Out Status: Not Read, Maybe Written ..
      real*8 r(-mc:mc)
C
C.. Local Scalars ..
      integer i,in,is,k,nn
      real*8 chiqui,maser1,maser2,var1,var2,zmed1,zmed2
C
C.. Local Arrays ..
      real*8 c(-mc:mc),w1(mp+2*kp),w2(mp+2*kp)
C
C.. Intrinsic Functions ..
      intrinsic ABS, MIN, SQRT
C
C ... Executable Statements ...
C
      chiqui = 1.0d-15
      maser1 = ZERO
      maser2 = ZERO
      do i = 1,n1
       if (ABS(ser1(i)) .ge. maser1) then
        maser1 = ABS(ser1(i))
       end if
      end do
      if (maser1.ge.ZERO .and. maser1.le.chiqui) return
      do i = 1,n2
       if (ABS(ser2(i)) .ge. maser2) then
        maser2 = ABS(ser2(i))
       end if
      end do
      maser2 = ABS(maser2)
      if (maser2.ge.ZERO .and. maser2.le.chiqui) return
      do i = 1,n1
       w1(i) = ser1(i)
      end do
      do i = 1,n2
       w2(i) = ser2(i)
      end do
      nn = MIN(n1,n2)
      if (n1 .lt. n2) then
       in = n2 - n1
       do i = 1,nn
        w2(i) = w2(i+in)
       end do
      else if (n1 .ne. n2) then
       in = n1 - n2
       do i = 1,nn
        w1(i) = w1(i+in)
       end do
      end if
      zmed1 = ZERO
      zmed2 = ZERO
      do i = 1,nn
       zmed1 = zmed1 + w1(i)
       zmed2 = zmed2 + w2(i)
      end do
      zmed1 = zmed1 / nn
      zmed2 = zmed2 / nn
      do i = 1,nn
       w1(i) = w1(i) - zmed1
       w2(i) = w2(i) - zmed2
      end do
      var1 = ZERO
      var2 = ZERO
      do i = 1,nn
       var1 = var1 + w1(i)*w1(i)
       var2 = var2 + w2(i)*w2(i)
      end do
      var1 = var1 / nn
      var2 = var2 / nn
      do i = 0,j
       c(i) = ZERO
       is = i + 1
       do k = is,nn
        c(i) = c(i) + w1(k)*w2(k-i)
       end do
       c(i) = c(i) / nn
       r(i) = c(i) / (SQRT(var1*var2))
      end do
      do i = -j,-1
       c(i) = ZERO
       is = nn + i
       do k = 1,is
        c(i) = c(i) + w1(k)*w2(k-i)
       end do
       c(i) = c(i) / nn
       r(i) = c(i) / (SQRT(var1*var2))
      end do
      end
C
C TO COMPUTE SQUARE ROOT, CONTROL IF ZERO
C
      double precision function RAIZ(a)
C
C.. Implicits ..
      implicit none
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
C.. Formal Arguments ..
C.. In/Out Status: Read, Not Written ..
      real*8 a
C
C.. Local Scalars ..
      real*8 peque
C
C.. Intrinsic Functions ..
      intrinsic SQRT
*      include 'stream.i'
C
C ... Executable Statements ...
C
      peque = -1.0d-08
      if (a .lt. ZERO) then
       RAIZ = ZERO
       return
      end if
      if ((a.lt.ZERO) .and. (a.lt.peque)) then
       RAIZ = ZERO
      else
       RAIZ = SQRT(a)
      end if
      end

c
c
c     GetPSIE return the first nval PSIEs of Va*N(B)/D(B)=PSIE(1)+PSIE(2)B+PSIE(3)B^2+PSIE(4)B^3+... 
c      where N(B)=N(0)+N(1)B+N(2)B^2+...+N(lN)B^lN
c            D(B)=D(0)+D(1)B+D(2)B^2+...+D(lD)B^lD   with the aditional condition that D(0)<>0
c            Va: is an escalar
      Subroutine getPSIE(N,lN,D,lD,Va,nval,PSIE)
      implicit none
      real*8 ZERO
      parameter (ZERO=0.0d0)
c
c     INPUT PARAMETERS
      real*8 N(0:60-1),D(0:60-1),Va
      integer nVal,lN,lD

c
c     OUTPUT PARAMETERS
      real*8  PSIE(*)

c
c     Intrinsic functions
      intrinsic MAX

c
c     LOCAL PARAMETERS
c      real*8 k,D1(60),tmpPSIE(max(nval,lD))
      real*8 k,D1(60),tmpPSIE(max(nval+lD,1))
      integer i,j

      D1(1)=1
      do i=1,lD
        D1(i+1)=D(i)/D(0)
      end do
      tmpPSIE(1)=n(0)
      do j=2,lD
        k=0
        do i=1,j-1
          k=k-tmpPSIE(j-i)*D1(i+1)
        end do
        if (j .gt. lN+1) then
          tmpPSIE(j)=k
        else
          tmpPSIE(j)=k+N(j-1)
        end if
      end do
      do j=lD+1,nval
        k=0
        do i=1,lD
          k=k-tmpPSIE(j-i)*D1(i+1)
        end do
        if (abs(k).lt.1.0d-60) then
          k=ZERO
        end if
        if (j .gt. lN+1) then
          tmpPSIE(j)=k
        else
          tmpPSIE(j)=k+N(j-1)
        end if
      end do
      do j=1,nval
        PSIE(j)=Va*tmpPSIE(j)/D(0)
      end do
      end subroutine


c     SeparaBF decompose in fraction in B and fraction in F
c     given N(B)M(F)/D1(B)Q1(F)=E_B(B)/D1(B)+H_F(F)/Q1(F)
c     INPUT
c       N(0:lN)=> N(0)+N(1)B+N(2)B^2+...+N(lN)B^lN
c       M(0:lM)=> M(0)+M(1)F+M(2)F^2+...+M(lM)F^lM
c       D1(0:lD1)
c       Q1(0:lQ1)
c     OUTPUT
c       E_B(1:lB+1)=>E_B(1)+E_B(2)B+E_B(3)B^2+...+E_B(lB+1)B^lB
c       H_F(1:lF+1)=>H_F(1)+H_F(2)F+H_F(3)F^2+...+H_F(lF+1)F^lF
c
      Subroutine SeparaBF(N,lN,M,lM,D1,lD1,Q1,lQ1,E_B,lB,H_F,lF)
      implicit none
      include 'units.cmn'
c     INPUT PARAMETERS
      integer lN,lM,lD1,lQ1,nval
      real*8 N(0:lN),M(0:lM),D1(0:lD1),Q1(0:lQ1)

c     OUTPUT PARAMETERS
      integer lB,lF
      real*8 E_B(60),H_F(60)

c     LOCAL PARAMETERS
      integer i,j,lmQD
      real*8 mQD(60,61),MN(0:60-1),M0(0:60-1),N0(0:60-1)

      if (lN .ge. lD1) then
        lB=lN
      else
c       lB=lD1 !because H_F(1)=0 =>E_B(lD1+1)=0
        lB=lD1-1
      end if
      if (lM .ge. lQ1) then
        lF=lM
      else
        lF=lQ1
      end if
      lmQD=lF+lB+2

c     Preparing the matrix that define the linear equations system
      do j=1,lmQD
        do i=1,lB+1
          if (((j-(lB+2)+i) .ge. 0) .and. ((j-(lB+2)+i) .le. lQ1)) then
            mQD(j,i)=Q1(j-(lB+2)+i)
          else
            mQD(j,i)=0
          end if
        end do
        do i=1,lF+1
          if (((lB-j+i) .ge. 0) .and. ((lB-j+i) .le. lD1)) then
            mQD(j,i+lB+1)=D1(lB-j+i)
          else
            mQD(j,i+lB+1)=0
          end if
        end do
      end do
      mQD(2+lB+lF,lB+2)=1;

c     Preparing the non homogeneus part of the equations system
      call MPBF(M,N,lM,lN,MN)  
c       NM^(1)B^lM+...+NM(lM)B+NM(lM+1)+NM(lM+2)F+...NM(lM+lN+1)F^lN
      DO j=-lB,lF+1
        if (((j+lN) .ge. 0) .and. (j .le. lM)) then
          mQD(j+lB+1,lmQD+1)=MN(j+lN)
        else
          mQD(j+lB+1,lmQD+1)=0
        end if
      end do


*      WRITE(Mtprof,*)'  subroutine SeparaBF, call 1'
      call MLTSOL(mQD,lmQD,1,60,61)

      do j=0,lB
        E_B(j+1)=mQD(j+1,lmQD+1)
      end DO
      do j=0,lF
        H_F(j+1)=mQD(2+j+lB,lmQD+1)
      end do
      end subroutine
      