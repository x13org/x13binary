**==spgrh.f    processed by SPAG 4.03F  at 10:39 on 20 Oct 1994
      SUBROUTINE spgrh(Yy,Sxx,Frq,Thtapr,N1,N2,Nspfrq,Ny,Mxarsp,
     &                 Ldecbl,Good)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C*** Start of declarations inserted by SPAG
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'notset.prm'
c-----------------------------------------------------------------------
      DOUBLE PRECISION PI,ZERO,ONE,TEN
      PARAMETER(PI=3.14159265358979d0,ZERO=0D0,ONE=1D0,TEN=10D0)
c-----------------------------------------------------------------------
      LOGICAL Ldecbl,Good
      DOUBLE PRECISION Yy,Sxx,an,x,y,a,b,cxx,oaic,sgme2,Frq,Thtapr,c2,
     &                 s2,dj,pxx
      INTEGER h,h1,i,ifpl,ifpl1,k,l,lagh,lagh1,n,N1,N2,Nspfrq,Ny,Mxarsp
c-----------------------------------------------------------------------
C*** End of declarations inserted by SPAG
c     replace dimension length for yy (BCM May 2007)
c-----------------------------------------------------------------------
      DIMENSION y(PLEN),Yy(*),x(PLEN),cxx(Nspfrq),a(101),b(101),Sxx(*),
     &          Frq(*),pxx(76)
c-----------------------------------------------------------------------
      DOUBLE PRECISION decibl
      EXTERNAL decibl
c-----------------------------------------------------------------------
      n=N2-N1+1
c-----------------------------------------------------------------------
      DO i=N1,N2
       y(i)=Yy(i)
      END DO
c-----------------------------------------------------------------------
      h=Nspfrq-1
c-----------------------------------------------------------------------
      h1=Nspfrq
c-----------------------------------------------------------------------
      DO i=N1,N2
       x(i)=y(i)
      END DO
c-----------------------------------------------------------------------
C     AUTO COVARIANCE COMPUTATION.
c-----------------------------------------------------------------------
      lagh=n-1
      lagh=min0(lagh,h)
      lagh1=lagh+1
      CALL sautco(x,cxx,N1,N2,n,lagh1,Thtapr,Good)
      IF(.not.Good)RETURN
      an=dble(n)
      ifpl=idint(3.0D-00*sqrt(an))
c-----------------------------------------------------------------------
      ifpl=min0(ifpl,50,lagh)
c-----------------------------------------------------------------------
C     ----- 5/15/80 -----
c-----------------------------------------------------------------------
      IF(Mxarsp.eq.NOTSET)THEN
       ifpl=30*Ny/12
*       ifpl=30
      ELSE
       ifpl=Mxarsp
      END IF
      ifpl=min0(ifpl,n-1)
c-----------------------------------------------------------------------
C     ----- 5/15/80 -----
c-----------------------------------------------------------------------
      ifpl1=ifpl+1
      CALL sicp2(cxx,ifpl1,N1,N2,a,l,sgme2,oaic)
C      k=0
C      CALL snrasp(a,b,Sxx,Frq,sgme2,l,k,h1,Ldecbl)
c-----------------------------------------------------------------------
C     ----- 10/1/2010 -----
c-----------------------------------------------------------------------
      DO i=1,h1
       c2=ONE
       DO k=1,l
        dj=dble(2*k)*PI*Frq(i)
        c2=c2+(a(k)*cos(dj))
       END DO
       s2=ZERO
       DO k=1,l
        dj=dble(2*k)*PI*Frq(i)
        s2=s2+(a(k)*sin(dj))
       END DO
       pxx(i)=sgme2/(c2**2 + s2**2)
      END DO
c-----------------------------------------------------------------------
      IF(Ldecbl)THEN
       DO i=1,H1
        dj=pxx(i)
        IF(dj.lt.ZERO)dj=-dj
        Sxx(i)=decibl(dble(dj))
       END DO
      ELSE
       DO i=1,H1
        Sxx(i)=pxx(i)
       END DO
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
