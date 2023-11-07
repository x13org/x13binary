c
cc 
      real*8 function calcQS(Z,Iconce,nz,mq)
C
C      THIS SUBROUTINE CALCULATES THE PIERCE QS STATISTIC OF THE
C       Z(Iconce+1:nz) SERIES 
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
      integer Iconce,nz,mq
      real*8 z(*)
C
C.. Local Scalars ..
      integer i,j,k,nr
      real*8 c0,QS
C
C.. Local Arrays ..
      real*8 c(2), r(2)
c    r(1):autocorr(mq), r(2):autocorr(2*mq)
C ... Executable Statements ...
      c0 = 0.0d0
      nr=nz-Iconce
      do i = Iconce+1,nz
       c0 = c0 + z(i)*z(i)
      end do
      c0 = c0 / nr
      do k = 1,2
       c(k) = 0.0d0
       j = k*mq + 1+ICONCE
       do i = j,nz
        c(k) = c(k) + z(i)*z(i-k*mq)
       end do
       c(k) = c(k) / nr
       r(k) = c(k) / c0
      end do  
      QS = 0.0d0   
      if (mq.ne.1 .and. r(1).gt.0.0d0) then       
       do k = 1,2
         if (r(k).gt.0) then
         QS = QS + (r(k)*r(k))/(nr-k*mq)
        end if
       end do
       QS = QS * nr * (nr+2)
      end if   
      calcQS=QS
      return   
      end
c
