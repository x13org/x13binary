C     Last change:  BCM  25 Nov 97    2:01 pm
**==grzlst.f    processed by SPAG 4.03F  at 09:49 on  1 Mar 1994
      SUBROUTINE grzlst(Ibeg,Iend,Lyr,A,B,Nmon,Nyc,Ifac)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      DOUBLE PRECISION BIG
      PARAMETER(BIG=10D16)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'chrt.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION A,B
      INTEGER i,Ibeg,Iend,Ifac,k,l,Lyr,Nmon,Nyc
      DIMENSION A(*),B(*)
c-----------------------------------------------------------------------
      Ienda=Iend
      Ymin=BIG
      Ymax=0D0-BIG
      Ibeg2=Iend-Nmon+1
      IF(Ibeg2.lt.Ibeg.or.Nmon.eq.0)Ibeg2=Ibeg
      l=0
      Ifrst=mod(Ibeg2-1,Nyc)+1
      Last=mod(Iend-1,Nyc)+1
c-----------------------------------------------------------------------
      DO k=Ibeg2,Iend
       Npts=Npts+1
       l=l+1
       Y1(l)=A(k)
       Y2(l)=B(k)
       IF(Ifac.eq.1)Y1(l)=A(k)*100.0D0
       IF(Ifac.eq.1)Y2(l)=B(k)*100.0D0
       Ymin=dmin1(Ymin,Y1(l),Y2(l))
       Ymax=dmax1(Ymax,Y1(l),Y2(l))
      END DO
c-----------------------------------------------------------------------
      Llyr=Lyr+(Ibeg2-1)/Nyc
      Lastyr=Lyr+(Iend+Nyc-1)/Nyc-1
      Nyr=Lastyr-Llyr+1
      N1=Iend-Ibeg2+1
c-----------------------------------------------------------------------
      DO i=1,61
       IF(Lyr.eq.0)THEN
        Xdata3(i)=0.D0
       ELSE
        Xdata3(i)=dble(float(Llyr+i-1))
       END IF
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
