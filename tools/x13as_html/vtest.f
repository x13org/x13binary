C     Last change:  BCM  29 Oct 97    7:39 am
      SUBROUTINE vtest(X,I1,Ib,Ie)
      IMPLICIT NONE
C***********************************************************************
c     This routine is modified code which originally appeared in X12W -
c     the seasonal adjustment program developed by the Budesbank
C***********************************************************************
      INCLUDE 'srslen.prm'
      INCLUDE 'x11opt.cmn'
C-----------------------------------------------------------------------
      INTEGER i,I1,Ib,Ie,j,n1,nmin
      DOUBLE PRECISION s,smax,st,t,tw,X,t4,tt
      DIMENSION X(*),t(40),t4(40),s(PSP)
C-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
C-----------------------------------------------------------------------
      DATA t/
     & .5410D0,.3934D0,.3264D0,.2880D0,.2624D0,.2439D0,.2299D0,.2187D0,
     & .2098D0,.2020D0,.1980D0,.194D0,.186D0,.182D0,.178D0,.174D0,.17D0,
     & .166D0,.162D0,.158D0,.15D0,.15D0,.15D0,.15D0,.15D0,.15D0,.15D0,
     & .15D0,.15D0,.15D0,.15D0,.15D0,.15D0,.15D0,.15D0,.1403D0,.14D0,
     & .14D0,.14D0,.14D0/
      DATA t4/
     & .9065D0,.7679D0,.6841D0,.6287D0,.5895D0,.5598D0,.5365D0,.5175D0,
     & .5017D0,.4884D0,.480D0,.471D0,.463D0,.454D0,.445D0,.4366D0,
     & .433D0,.430D0,.427D0,.424D0,.421D0,.417D0,.414D0,.411D0,.408D0,
     & .404D0,.401D0,.398D0,.395D0,.391D0,.388D0,.385D0,.382D0,.379D0,
     & .375D0,.3720D0,.369D0,.366D0,.362D0,.359D0/
C***********************************************************************
c     This routine performs Cochran's test to determine if the months
c     are heteroskedastic.
C***********************************************************************
      tw=0D0
      I1=0
      smax=-10.D0
      nmin=100
      st=1D0
      IF(Muladd.eq.1)st=0D0
      DO i=1,Ny
       n1=1
       j=Ib+i-1
       s(i)=0D0
       DO WHILE (.true.)
        s(i)=s(i)+(X(j)-st)**2D0
        j=j+Ny
        n1=n1+1
        IF(j.gt.Ie)THEN
         nmin=min0(nmin,n1-2)
         s(i)=s(i)/dble(n1-1)
         smax=dmax1(smax,s(i))
         tw=tw+s(i)
         GO TO 10
        END IF
       END DO
   10  CONTINUE
      END DO
      IF(.not.dpeq(tw,0D0))tw=smax/tw
      IF(nmin.gt.40)nmin=40
      tt=t(nmin)
      IF(Ny.eq.4)tt=t4(nmin)
      IF(tw.ge.tt)I1=1
      RETURN
      END
