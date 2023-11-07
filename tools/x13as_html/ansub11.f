C     Last change:  BCM  30 Sep 2005   12:38 pm
cc
c
cc
c    La subrutina Afilter es equivalente a CalculaFilter.m de MAtlab junto con ErrorTotalAf.m
c    esta subroutine devuelve los pesos del filtro asimetrico asi como su fase y su funcion de Transferencia
c    se supone Xt=St+Nt   
      Subroutine Afilter(alpha,transf,phase,phaseD,w,c,h,
     $                   m,th,q,phis,ps,ths,qs,Vs,
     $                   phin,pn)
      implicit none
      integer mx,mw
      parameter(mx=300,mw=1200)
      real*8 ZERO,ONE,TWO,MONE
      parameter (ZERO=0.0d0,ONE=1.0d0,TWO=2.0d0,MONE=-1.0d0)
c
c     INPUT PARAMETERS
c
      integer m   !filter of t-m when the serie Xt is of -Inf:t 
      integer q,qs,  !length of MA(Xt),MA(St),MA(Nt)
     $        ps,pn     !length of AR(St),AR(Nt)
      double precision th(0:q),phis(0:ps),ths(0:qs),
     $                 phin(0:pn),
     $                 Vs    ! Variance of components noise where Va=1
c    Note:   seasonal operators are assumed to be stored in nonseasonal form,
c              e.g., if th(B) = 1 - .8*B^12, the corresponding coefficient 
c              array is th = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -.8]'.
c      

c
c     OUTPUT PARAMETERS
c
      double precision alpha(0:2*mx)   !weights of asymmetric filter
      double precision transf(0:mw),phase(0:mw),w(0:mw),phased(0:mw)
c                               ! phase and transfer at different w values
      double precision c(0:mx) !Ignored part of asymmetric filter

c
c     INTERNAL PARAMETERS
c

      integer h,k,shft,lda,ipvt(mx),info,job
      double precision acgths(-mx:mx),acgth(-mx:mx),g(mx),a(mx,mx),
     $             tmp(-mx:mx),d(0:mx),thetil(0:mx),numer(0:mx),
     $             pi,rephin(0:mw),retheta(0:mw),
     $             imphin(0:mw),red(0:mw),imd(0:mw),imtheta(0:mw),
     $             refm(0:mw),imfm(0:mw),requot(0:mw),imquot(0:mw)
      integer i,j,ii           !loop counters

      do i=0,mw
       transf(i)=ZERO
       phase(i) = ZERO
       w(i) = ZERO
      end do
      do i=0,2*mx
       alpha(i) = ZERO
      end do
      pi=dacos(MONE)
      lda=mx

      call mult0(ths,qs,qs,ths,0,qs,0,qs,acgths,mx,mx)
      call mult0(th,q,q,th,0,q,0,q,acgth,mx,mx)

      k= max(ps-1,qs+m)
      h= max(q,pn+qs-m)
      

c----------------------------------------------------------------------------
c Compute g(B) = phin(F)*ths(F)*ths(B).  Store coefficients of F^h, ... , B^k, 
c where k = max(ps,qs) in (g(1), ... , g(h+k+1))'.  Note that some of the first 
c coefficients are zero if h > pn+qs, and some of the last coefficients are 
c zero if k > qs.  The first nonzero computed coefficient of phis(F)*ACGF(B) 
c is stored in g(shft) where shft = h-(pn+qs).
c
      do 115 i=1,h+k+1
         g(i)=ZERO
115   continue

      call mult0(phin, pn, pn, acgths, mx,mx, qs, qs, tmp, mx,mx)
      shft = h + m - (pn+qs)
      do 120 i = shft+1, shft+1+pn+2*qs
        g(i) = tmp(i-shft-1-pn-qs)
120   continue



c----------------------------------------------------------------------------
c Set up and solve linear equations for c(q),...,c(0),d(1),...,d(k) where 
c c(F) = c(0) + c(1)*F + ... + c(h)*F^h, d(B) = 1 + d(1)*B + ... + d(k)*B^k, 
c and c(F)*phis(B) + th(F)*d(B) = g(B).  Write linear equations as Ax = g.
c

c
c Set up (h+k+1) by (h+k+1) matrix A for linear equations
c
      
      do 130 j=1,mx
         do 131 i=1,mx
            a(i,j) = ZERO
131      continue
130   continue
      if (h. gt. 0) then
        do 140 j = 1, h
           do 141 i = j, ps+j
        if (i .eq. j) then
           a(i,j) = ONE
             else
                a(i,j) = phis(i-j)
             end if
141         continue
140     continue
      end if
      do 142 j = h+1, h+k+1
           do 144 i = j, j-q, -1
             if (i .eq. j) then
               a(i,j) = ONE
             else
               a(i,j) = th(j-i)
             end if
144         continue
142   continue

c
c Solve linear equations Ax = g for x = (c(h),...,c(1),d(0),...,d(k))'
c Note that solution (as well as input) is stored in g
 
      
      call dgefa(a,lda,h+k+1,ipvt,info)
      job=0
      call dgesl(a,lda,h+k+1,ipvt,g,job)

c
c fill d vector
c
      do 200 i=0,k
         d(i)=g(i+h+1)
200   continue

c
c fill c vector
c
      c(0)=ZERO
      do 201 i=h,1, -1
         c(i)=g(h-i+1)
201   continue
c -----------------------------------------------------------------------------
c
c get coefficients of alpha(B)=(F**m)*phin(B)*d(B)/theta(B)
c
c first get coefficients of numer(B)=phin(B)*d(B)
c
      call mult1(phin,pn,pn,d,mx,k,numer,mx)
c
c now get coefficients of thetil(B)=1/th(B)
c only up to order mx
c
      thetil(0)=ONE
      do 205 j=1,mx
         thetil(j)=ZERO
         ii=min(j,q)
         do 206 i=1,ii
            thetil(j)=thetil(j)-th(i)*thetil(j-i)
206      continue
205   continue
c
c now get coefficients (up to mx) of alpha(B) (for m = 0) by taking numer*thetil
c first compute numer*thetil
c then multiply by F**m (shift by m), and multiply by variance ratio
c
      call mult1(numer,mx,pn+k,thetil,mx,mx,alpha,2*mx)

      do 210 i=0,2*mx
        alpha(i)=Vs*alpha(i)
210   continue
c
c alpha:  pesos del filtro concurrent
c--------------------------------------------------------------------------

c-----------------------------------------------------------------------------
c The transfer and phase functions are computed in this section
c
C
C Compute frequencies w(i)
C
      do 238 i = 0,mw
         w(i) = pi*dble(i)/dble(mw)
238   continue
 
C
C Compute real and imaginary parts of polynomials evaluated at B = exp(-iw(j))
C
      do 240 j = 0,mw
         rephin(j)=phin(0)
         imphin(j)=ZERO
         red(j)=d(0)
         imd(j)=ZERO
         retheta(j)=th(0)
         imtheta(j)=ZERO
         if (pn .gt. 0) then
           do 241 ii = 1,pn
              rephin(j)=rephin(j)+phin(ii)*dcos(w(j)*dble(ii))
              imphin(j)=imphin(j)-phin(ii)*dsin(w(j)*dble(ii))
241        continue
         else
              rephin(j)=rephin(j)
         end if
            
         if (k .gt. 0) then
            do 245 ii=1,k
           red(j)=red(j)+d(ii)*dcos(w(j)*dble(ii))
           imd(j)=imd(j)-d(ii)*dsin(w(j)*dble(ii))
245         continue
         else
              red(j)=red(j)
         end if

         if (q .gt. 0) then
           do 250 ii = 1,q
              retheta(j)=retheta(j)+th(ii)*dcos(w(j)*dble(ii))
              imtheta(j)=imtheta(j)-th(ii)*dsin(w(j)*dble(ii))
250        continue
         else
              retheta(j)=retheta(j)
         end if
C
C replacing F^^m by exp[i*cos(w(j))*m]
C
         refm(j)=dcos(w(j)*dble(m))
         imfm(j)=dsin(w(j)*dble(m))
240   continue


c compute transfer and phase functions
c first compute  real and imaginary parts of phin*d/th
c store in requot and imquot
c then multiply by exp(i*m*w(j)) (in place of F^^m) and re-store results
c 
c
      do 300 i=0,mw
         requot(i)=retheta(i)*(rephin(i)*red(i)-imphin(i)*imd(i))+
     1             imtheta(i)*(rephin(i)*imd(i)+red(i)*imphin(i))
         imquot(i)=retheta(i)*(rephin(i)*imd(i)+red(i)*imphin(i))-
     1             imtheta(i)*(rephin(i)*red(i)-imphin(i)*imd(i))
         requot(i)=requot(i)/(retheta(i)**2.0D0+imtheta(i)**2.0D0)
         imquot(i)=imquot(i)/(retheta(i)**2.0D0+imtheta(i)**2.0D0)
         requot(i)=requot(i)*refm(i)-imquot(i)*imfm(i)
         imquot(i)=imquot(i)*refm(i)+imfm(i)*requot(i)
C
C multiplying by square of variance ratio now
C
         if (abs(requot(i)).lt.1E-10) then
           requot(i)=0
         end if
         if (abs(imquot(i)).lt.1E-10) then
           imquot(i)=0
         end if
         requot(i)=requot(i)*Vs
         imquot(i)=imquot(i)*Vs
300   continue

c
c compute squared gain
c
      do 320 i=0,mw
         transf(i)=requot(i)**2.0D0+imquot(i)**2.0D0
         
c 
c compute phase shift
c using arg function
c
         if ((requot(i) .eq. ZERO) .and. (imquot(i) .gt. ZERO)) then
            phase(i)=pi/TWO
         else if ((requot(i) .eq. ZERO) .and. (imquot(i) .lt. ZERO))
     1             then
            phase(i)=-pi/TWO
         else if ((requot(i) .eq. ZERO) .and. (imquot(i) .eq. ZERO))
     1             then
            phase(i)=-pi/TWO
         else if ((requot(i) .lt. ZERO) .and. (imquot(i) .ge. ZERO)) 
     1              then
            phase(i)=datan(imquot(i)/requot(i))+pi
         else if ((requot(i) .lt. ZERO) .and. (imquot(i) .lt. ZERO)) 
     1              then
            phase(i)=datan(imquot(i)/requot(i))-pi
         else
            phase(i)=datan(imquot(i)/requot(i))
         end if
c 
c the following piece is to take out discontinuities of phase when going from
c quadrant 3 to 2 or quandrant 2 to 3
c
c        if ((i .ge. 2) .and. (requot(i-1) .lt. ZERO) .and. 
c     1      (imquot(i-1) .lt. ZERO) .and. (requot(i) .lt. ZERO)
c     1      .and. (imquot(i) .gt. ZERO)) then
c                 phase(i)=phase(i)-TWO*pi
c         else if ((i .ge. 2) .and. (requot(i-1) .lt. ZERO) .and. 
c     1      (imquot(i-1) .gt. ZERO) .and. (requot(i) .lt. ZERO)
c     1      .and. (imquot(i) .lt. ZERO)) then
c                 phase(i)=phase(i)+TWO*pi
c        else
c             phase(i)=phase(i)
c        end if
                  

c the following is to make the phase "continuous" (other than "holes" at undefined points)
         
cc       if (jj .ne. 2) then
c           if ((201 .le. i) .and. (400 .ge. i)) then
c                phase(i)=phase(i)-pi
c           else if ((401 .le. i) .and. (600 .ge. i)) then
c                phase(i)=phase(i)-TWO*pi 
c           else if ((601 .le. i) .and. (800 .ge. i)) then
c                phase(i)=phase(i)-3.0d0*pi 
c           else if ((801 .le. i) .and. (1000 .ge. i)) then
c                phase(i)=phase(i)-4.0d0*pi 
c           else if ((1001 .le. i) .and. (1200 .ge. i)) then
c                phase(i)=phase(i)-5.0d0*pi
c             else
c                phase(i)=phase(i)
c             end if
cc       end if 
         if (i .eq. 0) then
            phased(i)=ZERO
         else
            phased(i)=-phase(i)/w(i)
         end if
c
c write phase in units of cycles per year
c  
c      phase(i)=12.0d0*phase(i)/(TWO*pi)

320   continue

c
c write transfer and phase functions at frequencies w(i)
c



c  end loop for m
c
95    continue


5     continue
      end

C      Subroutine ErrorTAf
C      Equivalent to Matlab subroutine ErrorTotalAf
C      Va=1
C      INPUT
C       C: part of asymmetric filter that needs the future 
C       h:=max(q,pn+qs-m), is the maximum i such C(i)<>0
C       th: theta of serie Xt
C       ths,thn: theta of St and Nt  (Xt=St+Nt)
C       Vs,Vn:   Variance of innovations of St and Nt in units of Va
C      OUTPUT
C       GMEM: autovariances(-2mx:2mx) of Total Error of Asymmetric filter
      subroutine ErrorTAf(C,h,th,q,ths,qs,thn,qn,Vs,Vn,
     $                    GMEM)
      implicit none
C     INPUT PARAMETERS
      integer mx,mw
      parameter(mx=300,mw=1200)
      real*8 ZERO,ONE,TWO
      parameter (ZERO=0.0d0,ONE=1.0d0,TWO=2.0d0)
      integer h,q,qs,qn
      double precision C(0:mx),th(0:q),ths(0:qs),thn(0:qn),Vs,Vn
C     INTERNAL PARAMETERS
      double precision acgths(-mx:mx),thetil(0:mx),
     $             acgthn(-mx:mx),gmeinf(-2*mx:2*mx),
     $             acgtil(-mx:mx),
     $             gminfn(-mx:mx),tmp2(-mx:mx)
      integer i,j,k           !loop counters
C     OUTPUT PARAMETERS
      double precision gmem(-2*mx:2*mx)
      
      call mult0(ths,mx,qs,ths,0,mx,0,qs,acgths,mx,mx)
      call mult0(thn,mx,qn,thn,0,mx,0,qn,acgthn,mx,mx)
c -----------------------------------------------------------------------------
c
c first get coefficients of thetil(B)=1/th(B)
c only up to order mx
c
      thetil(0)=ONE
      do 1205 j=1,mx
         thetil(j)=ZERO
         k=min(j,q)
         do 1206 i=1,k
            thetil(j)=thetil(j)-th(i)*thetil(j-i)
1206     continue
1205  continue
c-------------------------------------------------------------------
c now compute mean square error
c compute autocovariance generation function gamma(eps,m)
c first compute gamma(eps,infinity)
c see Bell and Martin, formula (38)
c
c compute numerator 

      call mult2(acgths,mx, mx, qs, qs, acgthn,mx,mx,qn, qn, 
     1   gminfn, mx,mx)
c
c compute denominator   
           
      call mult0(thetil,mx,mx,thetil,0,mx,0,mx,acgtil,mx,mx)

c now multiply to get gmeinf
      
      call mult2(gminfn,mx, mx, qs+qn, qs+qn, acgtil,mx,mx,mx,mx, 
     1           gmeinf, 2*mx, 2*mx)
c
c multiply by variances
c     
      do 219 j= -(mx+qs+qn), mx+qs+qn
       gmeinf(j)=gmeinf(j) * Vs * Vn
 219  continue

c
c compute rest of gmem = gamma(eps,m) formula (43)
c
      call mult0(C, mx, h , C, 0, mx, 0, h, tmp2, mx,mx)
      call mult2(tmp2, mx, mx, h, h, acgtil,mx,mx,mx,mx,
     1        gmem, 2*mx,2*mx)

      do 220 j=-(mx+h),mx+h
       gmem(j)=gmem(j)*(Vs**TWO)
 220  continue

c
c Now add coefficients of gamma(eps, inf) from above (equation 43)
c
      do 228 j=-mx,mx
       gmem(j)=gmem(j)+gmeinf(j)
 228  continue
      end
cc
c
cc
      Subroutine FinitoFilter(ct,cs,cc,nz,nlen,mq,out,IsCloseToTD,
     $             FDelayp,FDelaySA,pg_iter)
      implicit none
C
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
C.. Parameters ..
      integer np
      parameter (np = 60)
      real*8 ZERO,ONE,TWO,MONE
      parameter (ZERO=0.0d0,ONE=1.0d0,TWO=2.0d0,MONE=-1.0d0)
      integer m,   !the filter will be of observation nz-m
     $        nz,  !Number of observations
     $        nlen, !length of w
     $        mq,out,pg_iter
      real*8 ct(32),cs(32),cc(32)
      logical IsCloseToTD
C
C.. Local Scalars ..
      integer i,j
      double precision pi,phmaxx
C
C.. Local Arrays ..
      double precision w(nlen+1)  !frecuencies to which we calculate:
c                                ! Transf*,phase* and PhaseDelay* 
c
c     OUTPUT PARAMETERS
c
      double precision transp(nlen),
c!Gain of p filter to each w(i) value
     $                 transSA(nlen),!Gain of SA filter to each w(i)
     $                 phasep(nlen),  !phase of p filter to each w(i)
     $                 phaseSA(nlen), !phase of SA filter to each w(i)
     $                 FDelayp(nlen),!phase delay of p filter
     $                 FDelaySA(nlen)
c!phase delay of SA filter to each w(i)
      double precision tmp_Xlin(mpkp)
      double precision weightSA(0:nz-1), !weights of finite SA filter
     $                 weightp(0:nz-1)   !weights of finite Trend filter


      character fname*30,subtitle*50
      include 'models.i'
      m = 0
      pi=acos(MONE)
      phmaxx = pi
      if (mq .gt. 1) then
       phmaxx = TWO*pi/dble(mq)
      end if
      do i=1,nlen
        w(i)=pi*dble(i-1)/dble(nlen)
      end do
      do i=0,nz-1
        do j=1,nz+kp
         tmp_Xlin(j)=ZERO
        end do
        tmp_Xlin(nz-i)=ONE
        call GetStochWeight(nz-m,tmp_Xlin,nz,
     $                      weightSA(i),weightp(i),ct,cs,cc,IsCloseToTD)
      end do
      call GetPhase(weightSA,nz,w,nlen,transSA,phaseSA,FDelaySA)
*      if (out.eq.0 .and. pg_iter.eq.0) then
*       fname = 'SQAFSA.T4F'
*       subtitle = 'SQUARED GAIN OF FINITE SA FILTER'
*       call PLOTFILTERS(fname,subtitle,transSA,nlen,Mq,-10.0d0,pi,1)
*       fname = 'PHAFSA.T4F'
*       subtitle = 'PHASE DELAY OF FINITE SA FILTER'
*       call PLOTFILTERS(fname,subtitle,FDelaySA,nlen,Mq,-10.0d0,
*     $                 phmaxx,1)
*      end if
      if (nthetp .eq. 1) then
       do i=1,nlen
        transp(i) = ZERO
        FDelayP(i) = ZERO
       end do
*       if (out.eq.0 .and. pg_iter.eq.0) then
*        fname = 'SQAFTR.T4F'
*        subtitle = 'SQUARED GAIN OF FINITE TREND FILTER'
*        call PLOTFILTERS(fname,subtitle,transp,nlen,Mq,-10.0d0,pi,1)
*        fname = 'PHAFTR.T4F'
*        subtitle = 'PHASE DELAY OF FINITE TREND FILTER'
*        call PLOTFILTERS(fname,subtitle,FDelayP,nlen,Mq,-10.0d0,
*     $                  phmaxx,1)
*       end if
      else
       call GetPhase(weightp,nz,w,nlen,transp,phasep,FDelayp)
*       if (out.eq.0 .and. pg_iter.eq.0) then
*        fname = 'SQAFTR.T4F'
*        subtitle = 'SQUARED GAIN OF FINITE TREND FILTER'
*        call PLOTFILTERS(fname,subtitle,transp,nlen,Mq,-10.0d0,pi,1)
*        fname = 'PHAFTR.T4F'
*        subtitle = 'PHASE DELAY OF FINITE TREND FILTER'
*        call PLOTFILTERS(fname,subtitle,FDelayP,nlen,Mq,-10.0d0,
*     $                  phmaxx,1)
*       end if
      end if
      end
cc
c
cc
      subroutine GetPhase(weights,nweights,w,nw,
     $                  transf,phase,PhaseDelay)
      implicit none
      real*8 ZERO,TWO
      parameter (ZERO=0.0d0,TWO=2.0d0)
c
c     INPUT PARAMETERS
c
      integer nw,nweights
      double precision w(nw),weights(nweights)
c
c     OUTPUT PARAMETERS
c
      double precision transf(nw),phase(nw),PhaseDelay(nw)
c
c     INTERNAL PARAMETERS
c
      integer k,l
      double precision freal(nw),fimag(nw),pi

      pi=dacos(-1.0D00)
      do k=1,nw
        fimag(k)=0
        freal(k)=0
        do l=0,nweights-1
          fimag(k)=fimag(k)-weights(l+1)*sin(l*w(k))
          freal(k)=freal(k)+weights(l+1)*cos(l*w(k))
        enddo 
        transf(k)=fimag(k)**2.0D0+freal(k)**2.0D0
        if (abs(fimag(k)).lt.1.0D-10) then
          fimag(k)=0
        end if
        if (abs(freal(k)).lt.1.0D-10) then
          freal(k)=0
        end if
        if ((freal(k) .eq. ZERO) .and. (fimag(k) .gt. ZERO)) then
          phase(k)=pi/TWO
        else if ((freal(k) .eq. ZERO) .and. (fimag(k) .lt. ZERO))
     $             then
          phase(k)=-pi/TWO
        else if ((freal(k) .eq. ZERO) .and. (fimag(k) .eq. ZERO))
     $             then
          phase(k)=-pi/TWO
        else if ((freal(k) .lt. ZERO) .and. (fimag(k) .ge. ZERO)) 
     $              then
          phase(k)=datan(fimag(k)/freal(k))+pi
        else if ((freal(k) .lt. ZERO) .and. (fimag(k) .lt. ZERO)) 
     $              then
          phase(k)=datan(fimag(k)/freal(k))-pi
        else
          phase(k)=datan(fimag(k)/freal(k))
        end if
        PhaseDelay(k) = ZERO
        if ( w(k) .gt. 1.0d-16) then
         PhaseDelay(k)=-phase(k)/w(k)
        end if
      end do
      return
      end
cc
c
cc
      subroutine GetStochWeight(ind,z,nz,weightSa,weightP,ct,cs,cc,
     $                          IscloseToTD)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
C
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
      include 'units.cmn'
C.. Parameters ..
      integer np
      parameter (np = 60)
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
C.. Formal Arguments ..
      integer ind,nz
      real*8 z(mpkp),weightSa, weightP
      real*8 ct(32),cs(32),cc(32)
      logical IsCloseToTD
C
C.. Local Scalars ..
      integer i,iqrow,irow,j,k,m,maxpq,n,nqst1
      real*8 sum1,sum2,sum3,sum4,sum5,sum6,zaf,zab
C
C.. Local Arrays ..
      real*8 am(60,66),bxt(mpkp+np),bxs(mpkp+np),bxc(mpkp+np),
     $       byt(mpkp+np),bys(mpkp+np),byc(mpkp+np),
     $       fxt(mpkp+np),fxs(mpkp+np),fxc(mpkp+np),
     $       fyt(mpkp+np),fys(mpkp+np),fyc(mpkp+np),
     $       bz(mp+2*kp)
      include 'estgc.i'
      include 'models.i'

C APPLY FILTERS GT AND GS TO FORWARD AND BACKWARDS
C SERIES TO OBTAIN Y-SERIES
C
      maxpq = MAX(ntotd,qstar0)
cc
c Here I have to extend the series with forecast,
c compute the backward series and extend the series with backcast
cc
      
      call ExtendSeries(z,bz,zaf,zab,nz)
cc
c
cc
      if (ntotd .ne. qstar0) then
       nqst1 = qstar0 + 1
       do i = nqst1,ntotd
        THSTR0(i) = ZERO
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
        am(i,j) = THSTR0(i-j+1)
       end do
       m = maxpq - i + 1
       do j = m,maxpq
        am(i,j) = am(i,j) + THSTR0(maxpq-j+m)
       end do
       k = maxpq - i + 1
       am(i,maxpq+1) = ct(k)
       am(i,maxpq+2) = cs(k)
       am(i,maxpq+3) = cc(k)
      end do
      m = 3
*      WRITE(Mtprof,*)'  subroutine GetStochWeight, call 1'
      call MLTSOL(am,maxpq,m,60,66)
      do i = 1,maxpq
       k = maxpq - i + 1
       gt(k) = am(i,maxpq+1)
       gs(k) = am(i,maxpq+2)
       gc(k) = am(i,maxpq+3)
      end do
      n = Nz + qstar0 - 1
      do i = 1,n
       sum1 = ZERO
       sum2 = ZERO
       sum3 = ZERO
       sum4 = ZERO
       sum5 = ZERO
       sum6 = ZERO
       do j = 1,maxpq
        m = i + j - 1
        sum1 = sum1 + gt(j)*z(m)
        sum2 = sum2 + gt(j)*bz(m)
        sum3 = sum3 + gs(j)*z(m)
        sum4 = sum4 + gs(j)*bz(m)
        sum5 = sum5 + gc(j)*z(m)
        sum6 = sum6 + gc(j)*bz(m)
       end do
       fyt(i) = sum1
       byt(i) = sum2
       fys(i) = sum3
       bys(i) = sum4
       fyc(i) = sum5
       byc(i) = sum6
      end do
      if (qstar0 .eq. 1) then
       do j = 1,Nz
        fxt(j) = fyt(j)
        bxt(j) = byt(j)
        fxs(j) = fys(j)
        bxs(j) = bys(j)
        fxc(j) = fyc(j)
        bxc(j) = byc(j)
       end do
      else
C
C DERIVE (PSTAR+QSTAR) TERMS OF X-SERIES BY SOLVING EQUATIONS
C
       irow = ntotd + qstar0 - 2
       do i = 1,irow
        do j = 1,irow
         am(i,j) = ZERO
        end do
       end do
       n = Nz + qstar0 - ntotd
       iqrow = qstar0 - 1
       do i = 1,iqrow
        do j = 1,ntotd
         m = i + j - 1
         am(i,m) = totden(j)
        end do
        am(i,irow+1) = 0.5d0*zaf
        am(i,irow+2) = 0.5d0*zab
        do j = 3,6
         am(i,irow+j) = ZERO
        end do
       end do
       do i = qstar0,irow
        do j = 1,qstar0
         m = i - j + 1
         am(i,m) = THSTR0(j)
        end do
        k = n + irow - i + 1
        am(i,irow+1) = fyt(k)
        am(i,irow+2) = byt(k)
        am(i,irow+3) = fys(k)
        am(i,irow+4) = bys(k)
        am(i,irow+5) = fyc(k)
        am(i,irow+6) = byc(k)
       end do
       m = 6
*      WRITE(Mtprof,*)'  subroutine GetStochWeight, call 2'
       call MLTSOL(am,irow,m,60,66)
       do i = 1,irow
        k = n + irow - i + 1
        fxt(k) = am(i,irow+1)
        bxt(k) = am(i,irow+2)
        fxs(k) = am(i,irow+3)
        bxs(k) = am(i,irow+4)
        fxc(k) = am(i,irow+5)
        bxc(k) = am(i,irow+6)
       end do
C
C OBTAIN REST OF X-SERIES BY RECURRENCE AND
C COMBINE X-SERIES TO GIVE SC AND TREND
C
       do i = 1,n
        m = n - i + 1
        sum1 = fyt(m)
        sum2 = byt(m)
        sum3 = fys(m)
        sum4 = bys(m)
        sum5 = fyc(m)
        sum6 = byc(m)
        do j = 2,qstar0
         k = m + j - 1
         sum1 = sum1 - THSTR0(j)*fxt(k)
         sum2 = sum2 - THSTR0(j)*bxt(k)
         sum3 = sum3 - THSTR0(j)*fxs(k)
         sum4 = sum4 - THSTR0(j)*bxs(k)
         sum5 = sum5 - THSTR0(j)*fxc(k)
         sum6 = sum6 - THSTR0(j)*bxc(k)
        end do
        fxt(m) = sum1
        bxt(m) = sum2
        fxs(m) = sum3
        bxs(m) = sum4
        fxc(m) = sum5
        bxc(m) = sum6
       end do
      end if
      if (IscloseToTD) then
       weightSa = z(ind) - fxs(ind) - bxs(Nz-ind+1)
     $                  - fxc(ind) - bxc(Nz-ind+1)
      else
       weightSa = z(ind) - fxs(ind) - bxs(Nz-ind+1)
      end if
      weightP = fxt(ind) + bxt(Nz-ind+1)
      return
      end
cc
c
cc
      subroutine ExtendSeries(z,bz,zaf,zab,nz)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
C
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
C.. Parameters ..
      integer n1,n12,n10,np
      parameter (n10 = 10, n12 = 12, n1 = 1, np = 60)
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
C.. Formal Arguments ..
      integer nz
      real*8 z(mpkp),bz(mp+2*kp),zaf,zab
C
C.. Local Scalars ..
      integer i,j,ierr,jdd,kd,jmean,dummInt
      integer na,jfac,dof,lsig,lam,bias,out,fh,bpstar
      real*8 s,wm,alpha,f,ws
C

C.. Local Arrays ..
      real*8 a(mpkp),forbias(kp),bphist(6*n10)
      character Errext*180
C
C.. Common
      include 'calc.i'
      include 'calfor.i'
      include 'xarr.i'
cc
c
cc
      jmean = imean
cc
c We want all the time imean = 0
cc
      imean = 0
      dummInt=3
      alpha = 1.645d0
      lsig = -1
      lam = 1
      bias = 0
      out = 2
      fh = -1
      do i=1,nz
       Wd(i) = z(i)
      enddo
      nw = nz
      if (Bd .ne. 0) then
       do i = 1,Bd
        Nw = Nw - Mq
        do j = 1,Nw
         Wd(j) = Wd(j+Mq) - Wd(j)
        end do
       end do
      end if
      if (D .ne. 0) then
       do i = 1,D
        Nw = Nw - 1
        do j = 1,Nw
         Wd(j) = Wd(j+1) - Wd(j)
        end do
       end do
      end if
      wm = ZERO
      do i=1,nw
        wm = wm + wd(i)
      end do
      wm = wm /dble(nw)
      if (imean .eq. 1) then
       do i=1,nw
         wd(i) = wd(i) - wm
       end do
      end if
      Jfac = 1
      Na = Nw - Pstar + Qstar
      Dof = Nw - Pstar - nx - Imean
      call CALCFX(nx,x,s,Na,a,Ierr,Errext,dummInt,*10)
 10   continue
      f = s / Dof
      do i = 1,Na
       a(i) = a(i) / Detpri
      end do
      call FCAST(Phist,Thstar,bphist,bpstar,z,Nz,wm,a,Na,
     $           lsig,f,lam,D,Bd,Imean,zaf,fh,Out,Bias,
     $           forbias,0,alpha)
cc
c Reverse the series and compute the backward residuals
cc
      do i = 1,Nz
       bz(Nz-i+1) = z(i)
      end do
cc
c Reverse the differenced series
cc
      jdd = D + Bd
      kd = (-1)**jdd
      j = Nw
      do i = 1,Nw
       ws = Wd(i) * kd
       Wd(i) = Wd(Nw-i+1) * kd
       Wd(Nw-i+1) = ws
       j = j - 2
       if (j .le. 0) goto 5009
      end do
 5009 zab = zaf * kd
C
C Generate backwards residuals and remove factor detpri
C
      Jfac = 1
      call CALCFX(Bpq,x,s,Na,a,Ierr,Errext,dummInt,*5010)
 5010 do i = 1,Na
       a(i) = a(i) / Detpri
      end do
      call FCAST(Phist,Thstar,bphist,bpstar,bz,Nz,wm,a,Na,
     $           lsig,f,Lam,D,Bd,Imean,zab,fh,Out,-300,
     $           forbias,0,alpha)
      imean = jmean
      return
      end
c     smoothH make the smoothing of the histogram Transf(lTransf)
c     
      subroutine smoothH(Transf,lTransf,m,Stransf)
c
c     INPUT PARAMETERS
c
      integer lTransf,m
      double precision Transf(0:lTransf)
c
c     OUTPUT PARAMETERS
c
      double precision STransf(0:lTransf)
c
c     INTERNAL PARAMETERS
c
      integer k,j
      double precision window(0:m),ww

      do k=0,m-1
       window(k)=1D0
      end do
      window(m)=.5D0
      do k=0,lTransf
       Stransf(k)=0D0
      end do
      do j=0,m-1
        ww=0
        do k=0,j
          Stransf(j)=Stransf(j)+Transf(j-k)*window(k)
          ww=ww+window(k)
        end do
        do k=1,m
          Stransf(j)=Stransf(j)+Transf(j+k)*window(k)
          ww=ww+window(k)
        end do
        Stransf(j)=Stransf(j)/ww
      end do

      ww=window(0)
      do j=1,m
        ww=ww+2*window(j)
      end do
      do j=m,lTransf-m
        do k=1,m
          Stransf(j)=Stransf(j)+Transf(j-k)*window(k)
        end do
        do k=0,m
          Stransf(j)=Stransf(j)+Transf(j+k)*window(k)
        end do
        Stransf(j)=Stransf(j)/ww
      end do

      do j=lTransf-m+1,lTransf
        ww=0
        do k=1,m
          Stransf(j)=Stransf(j)+Transf(j-k)*window(k)
          ww=ww+window(k)
        end do
        do k=0,lTransf-j
          Stransf(j)=Stransf(j)+Transf(j+k)*window(k)
          ww=ww+window(k)
        end do
        Stransf(j)=Stransf(j)/ww
      end do

      end
cc
c
cc
c
c     Windowing of Data or windowing of AutoVariances
c     INPUT:
c     wtype   (0 windowing of Data, 1 windowing of autovariances)
c     iwindow (0=>square,1=>welch,2=>Tukey,3=>Bartlett,4=>Hamming,5=>Parzen)
c     m        (2m-1 is the width of the window)
c     x(nz)    (the serie to obtain its spectrum)
c     ovrlap   (if wtype=0, .TRUE.=>overlap windows, .FALSE.=>don't overlap)
c     OUTPUT:
c     p(0:m-1)   Spectrum Estimator
*      SUBROUTINE Windowin(wtype,iwindow,p,m,x,nz,ovrlap)
*
*c
*c     INPUT PARAMETERS
*c
*      integer wtype,iwindow,m,nz
*     integer ovrlap
*      double precision x(nz)
*c
*c     OUTPUT PARAMETERS
*c
*     double precision p(0:m)
*c
*c     INTERNAL PARAMETERS    
*c
*      double precision window(0:m)
*
*     call getWind(iwindow,m,window)
*     if (wtype .eq. 0) then
*        call DataWind(p,m,x,nz,window,ovrlap)
*     else
*        call covwind(p,m,x,nz,window)
*     end if
*
*      return
*     end 

c
c     DataWind applies Data windowing to obtain the histogram.
c      Data windowing is applied over Time domain 
c          and fft is calculated over each window
*      subroutine DataWind(p,m,x,nz,window,ovrlap)
*
*c
*c     INPUT PARAMETERS
*c
*     integer m,nz
*     double precision x(nz),window(0:m)
*     integer ovrlap
*c
*c     OUTPUT PARAMETERS
*c
*      double precision p(0:m)
*c
*c     INTERNAL PARAMETERS
*c
*      integer j,k,mk,redu,Inc,Ini
*      double precision w1(2*m+1),sumw,ffr(0:m),ffi(0:m)
*
*c
*      do j=0,m
*       p(j)=0D0
*      end do
*     if (ovrlap .eq. 2) then
*       Inc=1
*       Ini=m
*     else
*       Inc=m
*       Ini=1
*     end if
*     mk=nz/Inc
*     sumw=window(0)*window(0)
*     DO j=1,m
*       sumw=sumw+2*window(j)*window(j)
*     end do
*
*      
*      redu=0
*     do k=Ini,mk-1
*       if (ovrlap .ge. 1) then
*         if ((k*Inc+1+m) .gt. nz) then
*            redu=nz-(k*Inc+1+m)
*         end if
*         do j=0,m
*           w1(m-j+1)=x(k*Inc+1-j+redu)*window(j)
*           w1(m+j+1)=x(k*Inc+1+j+redu)*window(j)
*         end do
*       else 
*         if ((k*(2*m+1)+1+m) .gt. nz) then
*           redu=nz-(k*(2*m+1)+1+m)
*         end if
*         do j=0,m
*           w1(m-j+1)=x(k*(2*m+1)+1-j+redu)*window(j)
*            w1(m+j+1)=x(k*(2*m+1)+1+j+redu)*window(j)
*         end do
*       end if
*       call fft(w1,2*m+1,ffr,ffi)
*       p(0)=p(0)+ffr(0)*ffr(0)
*       do j=1,m
*         p(j)=p(j)+ffr(j)*ffr(j)+ffi(j)*ffi(j)
*       end do
*     end do
*      do j=0,m
*       p(j)=p(j)/(mk*sumw)
*      end do
*
*     end

c     covWind  apply Fourier to the windowed ACF
      subroutine covwind(p,m,x,nz,window,pm)
c
c     INPUT PARAMETERS
c
      integer m,nz,pm
      double precision window(0:120),x(*)
c
c     OUTPUT PARAMETERS
c
      double precision p(0:pm)
c
c     INTERNAL PARAMETERS
c
      integer j,k,mm
      double precision c(0:m),pi2
      parameter(pi2=6.28318530717959d0)
      
      call crosco(x,x,1,nz,nz,c,m+1)
      mm=min(pm,m)
      do j=0,mm
        p(j)=0D0
        p(0)=p(0)+c(j)*window(j)
      end do

      do k=1,m/2+1
        p(k)=c(0)*window(0)
        do j=1,m
          p(k)=p(k)+2*c(j)*window(j)*cos(pi2*j*k/m)
        end do
      end do

      end

      subroutine getWind(ind,m,window)
c
c     INPUT PARAMETERS
c
      integer ind,m
c
c     OUTPUT PARAMETERS
c
      double precision window(0:m)
c
c     internal parameters
c
      integer j
      double precision square,Welch,Tukey,Bartlett,Hamming,Parzen,pi
      parameter(pi=3.14159265358979d0)

      square(j)=1.
      welch(j)=1-(j/m)**2
      Tukey(j)=.5*(1+cos(pi*j/m))
      Bartlett(j)=1-j/m
      Hamming(j)=.54+.46*cos(pi*j/m)
      

      if (ind .eq. 1) then
        do j=0,m
          window(j)=welch(j)
        end do
      else if (ind .eq. 2) then
        do j=0,m
          window(j)=Tukey(j)  !this is equivalent to Hanning
        end do
      else if (ind .eq. 3) then
        do j=0,m
          window(j)=Bartlett(j)
        end do
      else if (ind .eq. 4) then
        do j=0,m
          window(j)=Hamming(j)
        end do
      else if (ind .eq. 5) then
        do j=0,m
          window(j)=Parzen(j,m)
        end do
      else
        do j=0,m
          window(j)=square(j)
        end do
      end if  

      end

      double precision function Parzen(j,m)
      integer j,m
      double precision parzen1,parzen2

      parzen1(j)=1-6*(j/m)**2+6*(j/m)**3
      parzen2(j)=2*(1-j/m)**3

      if (j .le. m/2) then
         Parzen=parzen1(j)
      else
         Parzen=parzen2(j)
      end if

      end



      character*(*) function getWindN(ind)
      integer ind
      integer max_wind
      parameter(max_wind=5)
      character*16 windName(0:max_wind)
      data windName /'square','Welch','Tukey',
     $              'Bartlett','Hamming','Parzen'/

      if ((ind .ge. 1) .and. (ind .le. 5)) then 
        getWindN=windName(ind)
      else
        getWindN=windName(0)
      end if

      end

      character*(*) function getWindT(wtype)
      integer wtype

      if (wtype .eq. 0) then
        getWindT='windData'
      else
        getWindT='windCovar'
      end if

      end

cc
c
cc
c     getSpect subroutine that obtains the Spectrum of a given a serie
      subroutine getSpect(z,nz,Freq,nFreq,nAR,Sxx,Good)
C
C.. Implicits ..
      implicit none
c-----------------------------------------------------------------------
      DOUBLE PRECISION PI,ZERO,ONE,TEN
      PARAMETER(PI=3.14159265358979d0,ZERO=0D0,ONE=1D0,TEN=10D0)
c-----------------------------------------------------------------------
c     INPUT PARAMETERS
      integer nz,nFreq,nAR
      double precision z(nz),Freq(nFreq)
c     OUTPUT PARAMETERS
      LOGICAL Good
      double precision Sxx(nFreq)
c     INTERNAL PARAMETERS
      integer na,i,ifail,k,h1
      DOUBLE PRECISION c2,s2,dj,pxx(nFreq)
      real*8 aic,Vz
      real*8 AR(nAR),b(nAR),tmpAR(nAR)
c-----------------------------------------------------------------------
      real*8 getVar
      external getVar
c-----------------------------------------------------------------------
      na = nz-nAR
      if (na .gt. 0) then
       call arfit(z,nz,nAR,AR,aic,ifail)
       if (ifail .eq. 1) then 
        return
       end if
       DO i=1,nAR
        tmpAR(i)=-AR(i)
       END DO
*       call snrasp(tmpAR,b,Sxx,Freq,aic/na,nAR,0,nFreq,.true.)
       Vz=getVar(z,nz)
c-----------------------------------------------------------------------
       h1=nFreq
       DO i=1,h1
        c2=ONE
        DO k=1,nAR
         dj=dble(2*k)*PI*Freq(i)
         c2=c2+(tmpAR(k)*cos(dj))
        END DO
        s2=ZERO
        DO k=1,nAR
         dj=dble(2*k)*PI*Freq(i)
         s2=s2+(tmpAR(k)*sin(dj))
        END DO
        pxx(i)=Vz/(c2**2 + s2**2)
       END DO
c-----------------------------------------------------------------------
*      do i=1,nFreq
*        Sxx(i) = exp(pxx(i)*log(10.0d0)/10.0d0)
*      end do
*      sSxx=0
*      do i=2,nFreq
*        sSxx=sSxx+Sxx(i)
*      end do
*       sSxx=sSxx/(nFreq-1)
       do i=1,nFreq
        Sxx(i)=pxx(i)
       end do
      end if
      return
      end

      double precision function KENDALLS(x,nz,mq)
C
C.. Implicits ..
      implicit none
C
      INCLUDE 'srslen.prm'
C.. Parameters ..
      integer Nmod,Np,Mp,Kp,loopmax
      real*8 dbl_max
      parameter (kp = PFCST, mp = POBS, Np = 84, Nmod = 5,
     $           dbl_max = 1.0d307,loopmax = 1000)
      real*8 ZERO,TWO
      parameter (ZERO=0.0d0,TWO=2.0d0)
C
C.. Formal Arguments ..
      integer nz,mq
      real*8 x(*)
C
C.. Local Scalars ..
      integer ny,res,i,j,k,ind,maxloop
      real*8 tmp,sum,min_val,value
C
C.. Local Arrays ..
      integer found(mq)
      real*8 obs(mq),m(mq),r(nz/mq,mq)
C
C.. External Functions ..
      real*8 AMIN
      external AMIN
      if (mq.le.1) then
        kendalls=ZERO
        return
      end if
      ny = nz/mq
      res = nz - ny*mq
      do i = 1,ny
       do j = 1,mq
        obs(j) = x(res+(i-1)*mq+j)
       end do
       ind = 1
       maxloop = 0
       do while ((ind .le. mq).and.(maxloop .lt.loopmax))
        maxloop = maxloop +1
        min_val = AMIN(obs,mq)
        k =  0
        do j = 1,mq
         found(j) = 0
         if (abs(obs(j)-min_val) .lt. 1.0d-20) then
          k = k + 1
          found(j) = 1
         end if
        end do
        value = ind + (k-1)/TWO
        do j = 1,mq
         if (found(j) .eq. 1) then
          obs(j) = dbl_max
          r(i,j) = value
         end if
        end do
        ind = ind + k
       end do
       if (maxloop .gt. loopmax) then
        kendalls = ZERO
        return
       end if
      end do
      do i = 1,mq
       sum = ZERO
       do j = 1,ny
        sum = sum + r(j,i)
       end do
       m(i) = sum
      end do
      tmp = ZERO
      do i = 1,mq
       tmp = tmp + (m(i)-ny*(mq+1)/TWO) * (m(i)-ny*(mq+1)/TWO)
      end do
      kendalls = 12.0d0*tmp / ((mq+1)*mq*ny)
      return
      end


      subroutine FFT(x,lx,ffr,ffi)
c     initialice and call FFTr
c
c     INPUT PARAMETERS
c
      integer lx
      double precision x(lx)
      EXTERNAL FFTr
c
c     OUTPUT PARAMETERS
c
      double precision ffr(0:lx/2),!real part of fourier Transf
     $                 ffi(0:lx/2) !Imag part of Fourier Transform

      call initWg(lx)
      call FFTr(x,lx,ffr,ffi)
      return
      end

      subroutine FFTr(x,lx,ffr,ffi)
c
c     INPUT PARAMETERS
c
      integer lx
      double precision x(lx)
      double precision ffr(0:lx/2),!real part of fourier Transf
     $                 ffi(0:lx/2) !Imag part of Fourier Transform
      include "fft.i"
      call sFourier(x,lx,ffr,ffi)
      return
      end
            
      double precision function getVar(z,nz)
      integer nz,j
      double precision z(nz),media,ss,zm

      media=0
      do j=1,nz
        media=media+z(j)
      end do
      media=media/nz
      ss=0
      do j=1,nz
        zm=z(j)-media
        ss=ss+zm*zm
      end do
      getVar=ss/nz
      return
      end


            
c     getHist return the histogram values of x
      subroutine getHist(x,lx,transf)
c
c     INPUT PARAMETERS
c
      integer lx
      double precision x(lx)
c
c     OUTPUT PARAMETERS
c
      double precision transf(0:lx/2)
c
c     INTERNAL PARAMETERS
c
      double precision ffr(0:lx/2),ffi(0:lx/2)
      integer k

      call FFT(x,lx,ffr,ffi)
      do k=0,lx/2
        transf(k)=(ffr(k)*ffr(k)+ffi(k)*ffi(k))/lx
      end do
      return
      end

cc
c
cc
c               Fast Fourier routines

      Subroutine initWg(lx)
c     initialize the common block FFT_block
      integer lx,k
      double precision w
      include "fft.i"

      nz=lx
      w=pi2/nz

      wgr(0)=1
      wgi(0)=0
      do k=1,(nz-1)
        wgr(k)=cos(w*k)
        wgi(k)=sin(w*k)
      end do

      end
         
c     slowFourier performs Fourier with O(N^2) computation
      Subroutine sFourier(x,lx,ffr,ffi)
c
c     INPUT PARAMETERS
c
      integer lx
      double precision x(lx)
      include "fft.i"
c
c     OUTPUT PARAMETERS
c
      double precision ffr(0:(lx-1)/2), !real part of fourier Transf
     $                 ffi(0:(lx-1)/2)  !Imag part of Fourier Transform
c
c     Internal PARAMETERS
c
      integer lx2,lx1,j,k
      double precision w

      if (lx .eq. 1) then
         ffr(0)=x(1)
         ffi(0)=0
         return
      end if
      lx1=lx-1
      lx2=(lx-1)/2
      w=pi2/lx
      DO k=0,lx2
        ffr(k)=0
        ffi(k)=0
        DO j=1,lx
          ffr(k)=ffr(k)+x(j)*cos(w*k*(j-1))
          ffi(k)=ffi(k)+x(j)*sin(w*k*(j-1))
        end DO
        !ffr(lx1-k)=ffr(k) !por simetria de la transformada al ser x real
        !ffr(lx1-k)=ffi(k) !por simetria de la FT al ser x real
      end DO

      end
cc
c
cc
c
      subroutine rellenarPico(pico,peakSA_S,nPeakSA_S,nPeakSA_TD,
     $  peakSA_TD,peakRes_S,nPeakRes_S,nPeakRes_TD,peakRes_TD)
      character pico(16)
      integer nPeakSA_S, nPeakSA_TD,nPeakRes_S, nPeakRes_TD
      integer PeakSA_S(16),peakSA_TD(16),PeakRes_S(16),peakRes_TD(16)
c     LOCAL VARIABLES
      integer i
c     include 'peaks.i'
c     
      do i=1,16
       pico(i)='-'         
      end do
      do i=1,nPeakSA_S
       select case (peakSA_S(i))
        case (11)
         pico(1)='Y' 
        case (21)
         pico(2)='Y' 
        case (31)
         pico(3)='Y'  
        case (41)
         pico(4)='Y'
        case (51)
         pico(5)='Y'
        case (61)
         pico(6)='Y'
       end select  
      end do 
      do i=1,nPeakSA_TD
       select case (peakSA_TD(i))
        case (43)
         pico(7)='Y' 
        case (53)
         pico(8)='Y' 
        case (6)
         pico(7)='Y' 
        case (12)
         pico(8)='Y'        
       end select  
      end do 
      do i=1,npeakRes_S
       select case (peakRes_S(i))
        case (11)
         pico(9)='Y' 
        case (21)
         pico(10)='Y' 
        case (31)
         pico(11)='Y'  
        case (41)
         pico(12)='Y'
        case (51)
         pico(13)='Y'
        case (61)
         pico(14)='Y'
       end select  
      end do 
      do i=1,npeakRes_TD
       select case (peakRes_TD(i))
        case (43)
         pico(15)='Y' 
        case (53)
         pico(16)='Y' 
        case (6)
         pico(15)='Y' 
        case (12)
         pico(16)='Y'          
       end select  
      end do 
      end

C     Last change:  BCM  12 Nov 1998   10:53 am
**==ispeak2.f    processed by SPAG 4.03F  at 14:16 on 28 Sep 1994
      INTEGER FUNCTION ispeak2(Sxx,Lsa,Peaks,Lowlim,Uplim,Npeaks,Plimit,
     &                         Mlimit,Ny,Freq,Plocal,Ldecbl,ipeaks)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Function that flags possible trading day or seasonal peaks in a
c     given set of spectral estimates.  Peak must be greater than the
c     median of the spectral estimates computed (Mlimit).  The peaks of
c     interest are defined in the vector pkvec.
c-----------------------------------------------------------------------
      DOUBLE PRECISION Mlimit,Sxx,slimit,Plimit,Freq,f0,f1,f2,Plocal 
      INTEGER ipeaks
      LOGICAL Lsa,Ldecbl
      INTEGER i,ifreq,Peaks,Lowlim,Uplim,Peakwd,Npeaks,i2,Ny,k,k0,k1,k2
      DIMENSION Sxx(*),Freq(*),Peaks(*),Lowlim(*),Uplim(*),ipeaks(6)
c-----------------------------------------------------------------------
      ispeak2=0
      i2=Npeaks
      IF(Lsa.and.Ny.eq.12)i2=i2-1
c-----------------------------------------------------------------------
      DO i=1,i2
       ifreq=Peaks(i)
       IF(Sxx(ifreq).gt.Mlimit)THEN
c-----------------------------------------------------------------------
        k=0
        k1=Lowlim(i)+1
        IF(Lsa.and.Ny.eq.12)THEN
         k2=ifreq-1
        ELSE
         k2=Uplim(i)-1
        END IF
        IF(k2.gt.k1)THEN
         f1=Freq(ifreq)-Plocal
         f2=Freq(ifreq)+Plocal
         DO k0=k1,k2
          IF(k0.ne.ifreq)THEN
           f0=Freq(k0)
           IF((f0.lt.f1.or.f0.gt.f2).and.(Sxx(k0).gt.Sxx(ifreq)))k=k+1
          END IF
         END DO
        END IF
c-----------------------------------------------------------------------
        IF(k.eq.0)THEN
         IF(Ldecbl)THEN
          slimit=Sxx(ifreq)-Plimit
          IF(Sxx(Lowlim(i)).lt.slimit)THEN
           IF(Lsa.and.(i.eq.Npeaks))THEN
            ispeak2=ispeak2+1
            ipeaks(ispeak2)=ifreq
           ELSE
            IF(Sxx(Uplim(i)).lt.slimit)THEN
             ispeak2=ispeak2+1
             ipeaks(ispeak2)=ifreq
           END IF
           END IF
          END IF
         ELSE
          slimit=Sxx(ifreq)/Sxx(Lowlim(i))
          IF(slimit.ge.Plimit)THEN
           IF(Lsa.and.(i.eq.i2))THEN
            ispeak2=ispeak2+1
            ipeaks(ispeak2)=ifreq
           ELSE
            slimit=Sxx(ifreq)/Sxx(Uplim(i))
            IF(slimit.ge.Plimit)THEN
             ispeak2=ispeak2+1
             ipeaks(ispeak2)=ifreq
            END IF
           END IF
          END IF
         END IF
        END IF
       END IF
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
