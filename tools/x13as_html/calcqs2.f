c
cc 
      subroutine calcQS2(Z,nz,mq,QS,PosCorr)
C
C      THIS SUBROUTINE CALCULATES THE PIERCE QS STATISTIC OF THE
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
C.. Input parameters
      integer nz,mq
      real*8 z(*)
C
C.. Output parameters
      real*8 QS
      integer PosCorr
C
C.. Local Scalars ..
      integer i,j,k,nr
      real*8 c0
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
      posCorr=1
      if (mq.gt.4) then
       if (r(mq).le.0.0d0) then
        posCorr=0
       else
        do i=1,4
          if (r(i).le.0.0d0) then 
            posCorr=0
          endif
        enddo
       endif
      else
       do i=1,mq
         if (r(i).le.0.2d0) then
           posCorr=0
         endif
       enddo
      endif
      QS = 0.0d0   
      if (mq.ne.1 .and. r(mq).gt.0.0d0) then       
       do j = 1,2
        k = j * mq
        if (r(k).gt.0) then
         QS = QS + (r(k)*r(k))/(nz-k)
        end if
       end do
       QS = QS * nz * (nz+2)
      end if   
      return   
      end
c
