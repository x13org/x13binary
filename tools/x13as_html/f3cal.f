C     Last change:  BCM  16 Feb 1999    3:38 pm
      SUBROUTINE f3cal(Sts,Ifail)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C --- THIS SUBROUTINE CALCULATES THE QUALITY CONTROL STATISTICS IN TABLE
C --- F3.
C-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'inpt2.cmn'
      INCLUDE 'work2.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'tests.cmn'
C-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO,ONE,TWO,THREE,TEN
      PARAMETER(ZERO=0D0,ONE=1D0,TWO=2D0,THREE=3D0,TEN=10D0)
C-----------------------------------------------------------------------
      DOUBLE PRECISION ave,ave1,ave2,ave3,ave4,count,ct,ct1,ct2,diff,
     &                 dsmic,fn,rmcd,sd,sdev,Sts,Temp,twt,twt2,wt
      INTEGER i,i1,Ifail,j,k,klda,kny,l,n
C-----------------------------------------------------------------------
      DIMENSION wt(11),Temp(PLEN),Sts(PLEN)
C-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
C-----------------------------------------------------------------------
      COMMON /work  / Temp
C-----------------------------------------------------------------------
      DATA wt/10D0,11D0,10D0,8D0,11D0,10D0,18D0,7D0,7D0,4D0,4D0/
C-----------------------------------------------------------------------
      Ifail=0
      kny=Ny/4
      Qu(1)=(Isq(kny)/(ONE-Psq(kny)))/0.10D0
      kny=12/Ny
      IF(kny.lt.1)kny=1
      Qu(2)=(Vi/dabs(100D0-Vp))/0.10D0
      Qu(3)=(Ratic*kny-1D0)/TWO
      fn=Posfob-Pos1bk+1
      Qu(4)=dabs(3D0*(fn-1D0)/Adri-2D0*fn+1D0)/
     &      (dsqrt(1.6D0*fn-2.9D0)*2.577D0)
      IF(Mcd.eq.1)THEN
       rmcd=1+(Smic(1)-ONE)/(Smic(1)-Smic(2))
       IF(rmcd.lt.0.5D0)rmcd=0.5D0
       IF(rmcd.gt.ONE)rmcd=ONE
      ELSE
       dsmic=Smic(Mcd-1)-Smic(Mcd)
       IF((dsmic.lt.ZERO.or.dpeq(dsmic,ZERO)).and.Mcd.eq.Ny)THEN
        rmcd=kny*15.5D0
       ELSE
        rmcd=Mcd+(Smic(Mcd)-ONE)/dsmic
       END IF
      END IF
      Qu(5)=(rmcd*kny-0.5D0)/5D0
      IF(Kfulsm.lt.2)Qu(6)=dabs(Ratis-4.0D0)/2.5D0
      Qu(7)=dsqrt((Test1+Test2)/TWO)
      Nyrs=(Posfob-Pos1bk+1)/Ny
      Nn=7
      IF((.not.Lstabl).and.Nyrs.ge.6.and.Kfulsm.lt.2)THEN
       Nn=11
       n=2-Muladd
       sd=sdev(Sts,Pos1bk,Posfob,1,n)
       ave=1-Muladd
       DO i=Pos1bk,Posfob
        Temp(i)=(Sts(i)-ave)/sd
       END DO
       ct1=ZERO
       ct2=ZERO
       count=ZERO
       ave1=ZERO
       ave2=ZERO
       ave3=ZERO
       ave4=ZERO
       klda=Pos1bk+Ny-1
       DO i=Pos1bk,klda
        ct=ZERO
        i1=i+Ny
        DO j=i1,Posfob,Ny
         ct=ct+ONE
         count=count+ONE
         diff=dabs(Temp(j)-Temp(j-Ny))
         k=j
         ave1=ave1+diff
        END DO
        ave2=ave2+dabs(Temp(k)-Temp(i))/ct
        k=k-2*Ny
        j=k-3*Ny
        IF(j.ge.Pos1bk)THEN
         ave3=ave3+dabs(Temp(k)-Temp(j))/THREE
         ct1=ct1+ONE
         j=j+Ny
         DO l=j,k,Ny
          i1=l-Ny
          IF(i1.ge.Pos1bk)THEN
           ct2=ct2+ONE
           diff=dabs(Temp(l)-Temp(i1))
           ave4=ave4+diff
          END IF
         END DO
        END IF
       END DO
       ave1=ave1/count
       ave2=ave2/DBLE(Ny)
       IF(.not.dpeq(ct1,ZERO))THEN
        ave3=ave3/ct1
        ave4=ave4/ct2
       END IF
       Qu(8)=TEN*ave1
       Qu(9)=TEN*ave2
       Qu(10)=TEN*ave4
       Qu(11)=TEN*ave3
      END IF
C-----------------------------------------------------------------------
      Qual=ZERO
      twt=ZERO
      DO i=1,11
       IF(i.le.Nn)THEN
        IF(Qu(i).lt.ZERO)Qu(i)=ZERO
        IF(Qu(i).gt.THREE)Qu(i)=THREE
        IF(Qu(i).ge.ONE)Ifail=Ifail+1
        IF(((.not.L3x5).or.Kfulsm.eq.2).and.i.eq.6)GO TO 10
        Qual=Qual+Qu(i)*wt(i)
       ELSE IF(i.lt.10)THEN
        Qual=Qual+Qu(7)*wt(i)
       ELSE IF(i.eq.10)THEN
        Qual=Qual+Qu(1)*wt(i)
       ELSE
        Qual=Qual+Qu(2)*wt(i)
       END IF
       twt=twt+wt(i)
   10  CONTINUE
      END DO
      Qual=Qual/twt
      Kfail=Ifail
c-----------------------------------------------------------------------
c     Calculate value of Q without M2
c-----------------------------------------------------------------------
      twt2=11D0
      IF(Lstabl.or.Nyrs.lt.6.or.Kfulsm.eq.2)twt2=15D0
      Q2m2=((Qual*twt)-(Qu(2)*twt2))/(twt-twt2)
C-----------------------------------------------------------------------
      RETURN
      END
