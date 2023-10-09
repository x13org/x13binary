C     Last change:  BCM  19 Jun 2002    5:56 pm
**==sicp2.f    processed by SPAG 4.03F  at 09:53 on  1 Mar 1994
      SUBROUTINE sicp2(Cyy,L1,N1,N2,Coef,Moar,Osd,Oaic)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      DOUBLE PRECISION a,aic,am,b,Coef,d,d2,Oaic,Osd
      DOUBLE PRECISION cst01,cst1,cst2,Cyy,sd,sdr,se
      INTEGER i,im,l,L1,lm,m,Moar,mp1,n,N1,N2
C*** End of declarations inserted by SPAG
C     COMMON SUBROUTINE
C     THIS SUBROUTINE FITS AUTOREGRESSIVE MODELS OF SUCCESSIVELY
C     INCREASING ORDER UP TO L(=L1-1).
C     INPUT:
C     CYY(I),I=0,L1  AUTOCOVARIANCE SEQUENCE
C     L1: L1=L+1, L IS THE UPPER LIMIT OF THE MODEL ORDER
C     N    LENGTH OF ORIGINAL DATA
C     OUT PUT:
C     COEF : AR-COEFFICIENTS
C     MOAR: ORDER OF AR
C     OSD: INNOVATION VARIANCE
C     OAIC: VALUE OF AIC
      DIMENSION Cyy(L1),Coef(L1)
      DIMENSION a(101),b(101)
      DOUBLE PRECISION an
c     CHARACTER*1 f(41),ax,bl,sta,ffff
c     DATA(f(i),i=1,41)/41*' '/
c     DATA ax,bl,sta/' ',' ','*'/
      n=N2-N1+1
c      cst0=0.0D-00
      cst1=1.0D-00
      cst2=2.0D-00
c      cst20=20.0D-00
c      cst05=0.05D-00
      cst01=0.00001D-00
      l=L1-1
      sd=Cyy(1)
      an=dble(n)
      Oaic=an*log(sd)
      Osd=sd
      Moar=0
C     INITIAL CONDITION PRINTOUT
c      ran=cst1/sqrt(an)
c      scalh=cst20
c      jj0=scalh+cst1
c      ian=scalh*(ran+cst05)
c      ian1=ian+jj0
c      ian2=2*ian+jj0
c      lan1=-ian+jj0
c      lan2=-2*ian+jj0
c      f(jj0)=ax
c      f(ian1)=ax
c      f(ian2)=ax
c      f(lan1)=ax
c      f(lan2)=ax
      se=Cyy(2)
C     ITERATION START
      DO m=1,l
       sdr=sd/Cyy(1)
       IF(sdr.lt.cst01)GO TO 10
       mp1=m+1
       d=se/sd
       a(m)=d
       d2=d*d
       sd=(cst1-d2)*sd
       am=m
       aic=an*log(sd)+cst2*am
       IF(m.ne.1)THEN
C     A(I) COMPUTATION
        lm=m-1
        DO i=1,lm
         a(i)=a(i)-d*b(i)
        END DO
       END IF
       DO i=1,m
        im=mp1-i
        b(i)=a(im)
       END DO
C     M,SD,AIC  PRINTOUT
c       IF(a(m).lt.cst0)THEN
c        nfc=scalh*(a(m)-cst05)
c       ELSE
c        nfc=scalh*(a(m)+cst05)
c       END IF
c       anfc=nfc
c       jj=int(anfc+scalh+cst1)
c       ffff=f(jj)
c       f(jj)=sta
c       f(jj)=ffff
C
C     ----- 5/15/80 -----
C
       IF(Oaic.ge.aic)THEN
        Oaic=aic
        Osd=sd
        Moar=m
       END IF
       IF(m.ne.l)THEN
        se=Cyy(m+2)
        DO i=1,m
         se=se-b(i)*Cyy(i+1)
        END DO
       END IF
      END DO
C     ----- 5/15/80 -----
   10 Oaic=aic
      Osd=sd
      Moar=l
      DO i=1,l
       Coef(i)=-a(i)
      END DO
c      f(jj0)=bl
c      f(ian1)=bl
c      f(ian2)=bl
c      f(lan1)=bl
c      f(lan2)=bl
      RETURN
      END
