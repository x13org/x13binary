C     Last change:  BCM  15 Apr 2005   12:12 pm
**==easter.f    processed by SPAG 4.03F  at 09:45 on  3 Oct 1994
      SUBROUTINE easter(Yhat,Khol,Kkhol,Kh2,Llda,Ihol,Nfcst)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'xeastr.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION TWOHND,TEN,TWO,ZERO,TWNTY4,ONE,SVNTN,ELEVEN,FOUR,
     &                 ONEHND,TWNTY1,FOURTN,TWNTY5,SEVEN
      PARAMETER(TWOHND=200D0,TEN=10D0,TWO=2D0,ZERO=0D0,TWNTY4=24D0,
     &          ONE=1D0,SVNTN=17D0,ELEVEN=11D0,FOUR=4D0,TWNTY1=21D0,
     &          FOURTN=14D0,ONEHND=100D0,TWNTY5=25D0,SEVEN=7D0)
c-----------------------------------------------------------------------
      DOUBLE PRECISION dif,dif1,dif2,sd28,sd915,sda,sdm,sum0,sum1,sum2,
     &                 suma,suma1,suma1e,suma2,suma2e,sumae,sumd1,sumd2
      DOUBLE PRECISION sumd3,summ,summe,var28,var915,vara,varm,yl1,yl2,
     &                 smfac,mfac,mfreq,Yhat
      INTEGER i,Ihol,Kh2,Khol,Kkhol,kk,ll,Llda,mm,nsuaa,nsuaa1,nsuaa2,
     &        nsum,nsum1,nsum2,nsuma,nsuma1,nsuma2,Nfcst
      DIMENSION Yhat(PLEN),mfac(0:34),mfreq(0:34)
c-----------------------------------------------------------------------
      DATA(mfreq(i),i=0,34)/
     & 0.0100D0,0.0150D0,0.0050D0,0.0175D0,0.0300D0,0.0325D0,0.0250D0,
     & 0.0300D0,0.0300D0,0.0400D0,0.0375D0,0.0350D0,0.0250D0,0.0275D0,
     & 0.0425D0,0.0425D0,0.0275D0,0.0300D0,0.0225D0,0.0400D0,0.0425D0,
     & 0.0325D0,0.0300D0,0.0350D0,0.0300D0,0.0425D0,0.0375D0,0.0350D0,
     & 0.0300D0,0.0250D0,0.0350D0,0.0300D0,0.0100D0,0.0100D0,0.0100D0/
c-----------------------------------------------------------------------
      ll=Llda
      IF(Lgenx)THEN
       CALL chkeas(Khol,Llda)
       IF((Ieast(1)*Ieast(2)*Ieast(3)*Ieast(4)).eq.0)THEN
        Ihol=0
        RETURN
       END IF
      END IF
      mm=ll+Nfcst
c      kk = Khol - 12
      kk=Kkhol
c-----------------------------------------------------------------------
C---- INVERT APRIL
c-----------------------------------------------------------------------
      DO i=Khol,ll-1,12
       Yhol(i+1)=TWOHND-Yhol(i+1)
      END DO
c-----------------------------------------------------------------------
C---- CALCULATE AVERAGES OF IRREGULARS BEFORE APRIL 2
c-----------------------------------------------------------------------
      summ=ZERO
      sum0=ZERO
      DO i=Khol,ll,12
       IF(Xhol(i).le.TEN)THEN
        summ=summ+Yhol(i)
        sum0=sum0+ONE
        IF(i.lt.ll)THEN
         summ=summ+Yhol(i+1)
         sum0=sum0+ONE
        END IF
       END IF
      END DO
      summ=summ/sum0
c-----------------------------------------------------------------------
C---- CALCULATE STANDARD DEVIATION OF OBS BEFORE APRIL 2
c-----------------------------------------------------------------------
      summe=ZERO
      nsum=0
      varm=ZERO
      DO i=Khol,ll,12
       IF(Xhol(i).le.TEN)THEN
        varm=(Yhol(i)-summ)**2+varm
        IF(i.lt.ll)varm=(Yhol(i+1)-summ)**2+varm
       END IF
      END DO
      sdm=sqrt(varm)/sqrt(sum0)*TWO
c-----------------------------------------------------------------------
C---- GET RID OF EXTREMES
c-----------------------------------------------------------------------
      DO i=Khol,ll,12
       IF(Xhol(i).le.TEN)THEN
        yl1=Yhol(i)
        dif=abs(Yhol(i)-summ)
        nsum1=1
        IF(dif.ge.sdm)THEN
         nsum1=0
         yl1=ZERO
        END IF
        yl2=ZERO
        nsum2=0
        IF(i.lt.ll)THEN
         dif=abs(Yhol(i+1)-summ)
         IF(dif.lt.sdm)THEN
          yl2=Yhol(i+1)
          nsum2=1
         END IF
        END IF
        summe=yl1+yl2+summe
        nsum=nsum1+nsum2+nsum
       END IF
      END DO
      summ=summe/nsum
c-----------------------------------------------------------------------
C---- CALCULATE AVERAGE AFTER APRIL 16TH
c-----------------------------------------------------------------------
      suma=ZERO
      sum0=ZERO
      DO i=Khol,ll,12
       IF(Xhol(i).gt.TWNTY4)THEN
        suma=suma+Yhol(i)
        sum0=sum0+ONE
        IF(i.lt.ll)THEN
         suma=suma+Yhol(i+1)
         sum0=sum0+ONE
        END IF
       END IF
      END DO
      suma=suma/sum0
c-----------------------------------------------------------------------
C---- CALCULATE STANDARD DEVIATION OF OBS AFTER APRIL 16
c-----------------------------------------------------------------------
      sumae=ZERO
      nsum=0
      vara=ZERO
      DO i=Khol,ll,12
       IF(Xhol(i).gt.TWNTY4)THEN
        vara=(Yhol(i)-suma)**2+vara
        IF(i.lt.ll)vara=(Yhol(i+1)-suma)**2+vara
       END IF
      END DO
      sda=sqrt(vara)/sqrt(sum0)*TWO
c-----------------------------------------------------------------------
C---- GET RID OF EXTREMES
c-----------------------------------------------------------------------
      DO i=Khol,ll,12
       IF(Xhol(i).gt.TWNTY4)THEN
        yl1=Yhol(i)
        dif=abs(Yhol(i)-suma)
        nsum1=1
        IF(dif.ge.sda)THEN
         nsum1=0
         yl1=ZERO
        END IF
        yl2=ZERO
        nsum2=0
        IF(i.lt.ll)THEN
         dif=abs(Yhol(i+1)-suma)
         IF(dif.lt.sda)THEN
          yl2=Yhol(i+1)
          nsum2=1
         END IF
        END IF
        sumae=yl1+yl2+sumae
        nsum=nsum1+nsum2+nsum
       END IF
      END DO
      suma=sumae/dble(nsum)
      sum1=ZERO
      sum2=ZERO
      suma1=ZERO
      suma2=ZERO
      DO i=Khol,ll,12
       IF((Xhol(i).gt.TEN).and.(Xhol(i).lt.TWNTY5))THEN
c-----------------------------------------------------------------------
C---- DO PERIOD APRIL 2 TO 8, 9 TO 15
c-----------------------------------------------------------------------
        IF(Xhol(i).le.SVNTN)THEN
         suma1=suma1+Yhol(i)
         sum1=sum1+ONE
         IF(i.lt.ll)THEN
          suma1=suma1+Yhol(i+1)
          sum1=sum1+ONE
         END IF
        ELSE
         suma2=suma2+Yhol(i)
         sum2=sum2+ONE
         IF(i.lt.ll)THEN
          suma2=suma2+Yhol(i+1)
          sum2=sum2+ONE
         END IF
        END IF
       END IF
      END DO
      suma1=suma1/sum1
      suma2=suma2/sum2
      DO i=Khol,mm,12
       IF(Xhol(i).lt.ELEVEN)THEN
        Yhat(i)=summ
        Yhat(i+1)=TWOHND-Yhat(i)
       ELSE IF(Xhol(i).gt.TWNTY4)THEN
        Yhat(i)=suma
        Yhat(i+1)=TWOHND-Yhat(i)
       ELSE IF(Xhol(i).le.FOURTN)THEN
        sumd1=FOURTN-Xhol(i)
        Yhat(i)=suma1+sumd1*(summ-suma1)/FOUR
       ELSE IF(Xhol(i).le.TWNTY1)THEN
        sumd2=TWNTY1-Xhol(i)
        Yhat(i)=suma2+sumd2*(suma1-suma2)/SEVEN
       ELSE
        sumd3=TWNTY5-Xhol(i)
        Yhat(i)=suma+sumd3*(suma2-suma)/FOUR
       END IF
      END DO
c-----------------------------------------------------------------------
C---- CALCULATE STANDARD ERRS FOR PERIODS APRIL 2-8,9-15
c-----------------------------------------------------------------------
      var28=ZERO
      var915=ZERO
      DO i=Khol,ll,12
       IF((Xhol(i).gt.TEN).and.(Xhol(i).lt.TWNTY5))THEN
        IF(Xhol(i).gt.SVNTN)THEN
         var915=(Yhol(i)-Yhat(i))**2+var915
         IF(i.lt.ll)var915=(Yhol(i+1)-Yhat(i))**2+var915
        ELSE
         var28=(Yhol(i)-Yhat(i))**2+var28
         IF(i.lt.ll)var28=(Yhol(i+1)-Yhat(i))**2+var28
        END IF
       END IF
      END DO
      sd28=sqrt(var28)/sqrt(sum1)*TWO
      sd915=sqrt(var915)/sqrt(sum2)*TWO
      nsuma=0
      suma1e=ZERO
      nsuaa=0
      suma2e=ZERO
c-----------------------------------------------------------------------
C---- THROW OUT EXTREMES BEYOND 2 STANDARD ERRORS FOR PERIODS APR 2-8
C---- AND APRIL 9-15
c-----------------------------------------------------------------------
      DO i=Khol,ll,12
       yl1=Yhol(i)
       IF(i.lt.ll)yl2=Yhol(i+1)
       IF((Xhol(i).gt.TEN).and.(Xhol(i).lt.TWNTY5))THEN
        IF(Xhol(i).gt.SVNTN)THEN
c-----------------------------------------------------------------------
C---- GET RID OF EXTREMES APRIL 9-15
c-----------------------------------------------------------------------
         dif2=abs(Yhol(i)-Yhat(i))
         IF(dif2.lt.sd915)THEN
          nsuaa1=1
         ELSE
          yl1=ZERO
          nsuaa1=0
         END IF
         IF(i.ge.ll)GO TO 20
         dif2=abs(Yhol(i+1)-Yhat(i))
         IF(dif2.ge.sd915)GO TO 20
         nsuaa2=1
         GO TO 30
        ELSE
c-----------------------------------------------------------------------
C---- APRIL 2-8
c-----------------------------------------------------------------------
         dif1=abs(Yhol(i)-Yhat(i))
         IF(dif1.lt.sd28)THEN
          nsuma1=1
         ELSE
          yl1=ZERO
          nsuma1=0
         END IF
         IF(i.lt.ll)THEN
          dif1=abs(Yhol(i+1)-Yhat(i))
          IF(dif1.lt.sd28)THEN
           nsuma2=1
           GO TO 10
          END IF
         END IF
         yl2=ZERO
         nsuma2=0
        END IF
   10   suma1e=yl1+yl2+suma1e
        nsuma=nsuma1+nsuma2+nsuma
       END IF
       GO TO 40
   20  yl2=ZERO
       nsuaa2=0
   30  suma2e=yl1+yl2+suma2e
       nsuaa=nsuaa1+nsuaa2+nsuaa
   40  CONTINUE
      END DO
      IF(nsuma.ne.0)suma1=suma1e/dble(nsuma)
      IF(nsuaa.ne.0)suma2=suma2e/dble(nsuaa)
c-----------------------------------------------------------------------
C---- RECALCULATE FIT FOR PERIODS APR 2-8,9-15 WITH EXTREMES REMOVED
c-----------------------------------------------------------------------
      DO i=kk,mm,12
       IF((Xhol(i).gt.TEN).and.(Xhol(i).lt.TWNTY5))THEN
        IF(Xhol(i).le.FOURTN)THEN
         sumd1=FOURTN-Xhol(i)
         Yhat(i)=suma1+sumd1*(summ-suma1)/FOUR
         Yhat(i+1)=TWOHND-Yhat(i)
        ELSE IF(Xhol(i).le.TWNTY1)THEN
         sumd2=TWNTY1-Xhol(i)
         Yhat(i)=suma2+sumd2*(suma1-suma2)/SEVEN
         Yhat(i+1)=TWOHND-Yhat(i)
        ELSE
         sumd3=TWNTY5-Xhol(i)
         Yhat(i)=suma+sumd3*(suma2-suma)/FOUR
         Yhat(i+1)=TWOHND-Yhat(i)
        END IF
       END IF
      END DO
c-----------------------------------------------------------------------
c     Compute seasonal component within easter effect for March.
c-----------------------------------------------------------------------
      smfac=ZERO
c      di=ZERO
      DO i=0,34
c       di=di+ONE
       IF(i.le.10)THEN
        mfac(i)=summ
       ELSE IF(i.gt.10.and.i.le.14)THEN
        sumd1=FOURTN-i
        mfac(i)=(suma1+sumd1*(summ-suma1)/FOUR)
       ELSE IF(i.gt.14.and.i.lt.21)THEN
        sumd2=TWNTY1-i
        mfac(i)=(suma2+sumd2*(suma1-suma2)/SEVEN)
       ELSE IF(i.ge.21.and.i.le.24)THEN
        sumd3=TWNTY5-i
        mfac(i)=(suma+sumd3*(suma2-suma)/FOUR)
       ELSE
        mfac(i)=suma
       END IF
       smfac=smfac+(mfreq(i)*mfac(i))
      END DO
c-----------------------------------------------------------------------
c     Divide out seasonal effect from March, April values.
c-----------------------------------------------------------------------
      DO i=kk,mm,12
       Yhat(i)=(Yhat(i)*ONEHND)/smfac
       Yhat(i+1)=(Yhat(i+1)*ONEHND)/(TWOHND-smfac)
      END DO
c-----------------------------------------------------------------------
      IF(Kh2.eq.0)RETURN
      IF(Xhol(Kh2).le.TEN)THEN
       Yhat(Kh2+1)=TWOHND-summ
      ELSE IF(Xhol(Kh2).gt.TEN.and.Xhol(Kh2).le.FOURTN)THEN
       sumd1=FOURTN-Xhol(Kh2)
       Yhat(Kh2+1)=TWOHND-(suma1+sumd1*(summ-suma1)/FOUR)
      ELSE IF(Xhol(Kh2).gt.FOURTN.and.Xhol(Kh2).le.TWNTY1)THEN
       sumd2=TWNTY1-Xhol(Kh2)
       Yhat(Kh2+1)=TWOHND-(suma2+sumd2*(suma1-suma2)/SEVEN)
      ELSE IF(Xhol(Kh2).gt.TWNTY1.and.Xhol(Kh2).le.TWNTY4)THEN
       sumd3=TWNTY5-Xhol(Kh2)
       Yhat(Kh2+1)=TWOHND-(suma+sumd3*(suma2-suma)/FOUR)
      ELSE
       Yhat(Kh2+1)=TWOHND-nsuma
      END IF
      Yhat(Kh2+1)=(Yhat(Kh2+1)*ONEHND)/(TWOHND-smfac)
c-----------------------------------------------------------------------
      RETURN
      END
