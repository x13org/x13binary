C     Last change:  BCM  19 May 2003    7:50 am
c This subroutine will compute signal extraction filter weights for ARIMA component models 
c The squared gain and phase delay of the filter will also be computed
c See Bell and Martin (2003) for more details 
c With appropriate definitions of S(t) and N(t) the program can be used for signal + noise, 
c  seasonal + nonseasonal, and trend + (seasonal + irregular), etc.  The general model form is 
c
c       z(t)  =  S(t) + N(t)
c
c       phi(B) z(t) = theta(B) a(t)
c
c       phis(B) S(t) = thetas(B) b(t)
c
c       phin(B) N(t) = thetan(B) e(t)
c
c The filter computed is alpha(B), the signal extraction filter for S(t) 
c The corresponding filter for N(t) is 1 - alpha(B). (true if m >= 0)
c The filter is based on observations up to Z(t+m), for arbitrary m 
c
c The phi(B), phis(B), and phin(B) operators include both AR and differencing 
c operators.  It is assumed that phis(B) and phin(B) have no common zeroes 
c on or inside the unit circle.  Any operators in the model with 
c multiplicative structure are assumed to have been multiplied out.  
c Operators are taken to be of the following general form
c
c      phis(B) = phis(0) + phis(1)*B + ... + phis(ps)*B^ps,
c
c and their coefficients are stored in corresponding arrays as follows:
c
c      phis = [phis(0), phis(1), ... , phis(ps)]'.
c
c Analogous definitions apply for the other operators in the model.
c Notice (1) the operators are written with + signs between terms
c        (2) the array of coefficients includes the lag-0 coefficient
c              (which will always be 1)
c        (3) seasonal operators are assumed to be stored in nonseasonal form,
c              e.g., if theta(B) = 1 - .8*B^12, the corresponding coefficient 
c              array is theta = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -.8]'.
c
c The program is currently set up to take vectors and dimensions from subroutine spectrum.forarguments print out 
c spectrum.for does the spectral decomposition to obtain MA operators for the component models
c We currently print out (file trans.out) the squared gain and phase delay for the seasonally adjusted, seasonal, trend and cycle components
c To do this, we must form (in sigex.for) and/or capture seasonally adjusted, seasonal, trend, non-trend, cycle and noncycle components from spectrum.for 
c Also printed are filter weights (file weights.out)
c-----------------------------------------------------------------------
  
      SUBROUTINE CONCURRENT(thstar, qstar, pstar, thets, nthets, psi, 
     1 npsi, varwns, thadj, nthadj, chcyc, nchcyc, varwna, thetp,
     1 nthetp, chi, nchi, varwnp, thtra, nthtra, pscyc, npscyc, 
     1 varwnt, cyc, ncyc, thetc, nthetc, varwnc, 
     1 thcya, nthcya, chpsi, nchpsi, varwca, Lgraf)
c-----------------------------------------------------------------------
c input argument variables and arrays (from SEATS subroutine SPECTRUM)
c
c thstar(1,...qstar) -- coefficients of MA operator for z(t)  
c thets(1,nthets) -- seasonal numerator
c psi(1,npsi) -- seasonal denominator 
c thadj(1,nthadj) -- seasonally adjusted numerator
c chcyc(1,nchcyc) -- trend-cycle denominator 
c thetp(1,nthetp) -- trend numerator
c chi(1,nchi) -- trend denominator
c thtra(1,nthtra) -- trend adjusted numerator
c pscyc(1,npscyc) -- seasonal-cycle denominator
c thcya(1,nthcya) -- cycle adjusted numerator
c chpsi(1,nchpsi) -- cycle adjusted denominator
c varwns -- variance ratio for seasonal component
c varwna -- variance ratio for seasonally adjusted component
c varwnp -- variance ratio for trend component
c varwnt -- variance ratio for the trend adjusted component
c varwca -- variance ratio for the cycle adjusted component
c-----------------------------------------------------------------------
c Internal variables and arrays
c    (note not all model operators need be input)
c
c     p, ps, pn, q, qs, qn = model orders
c     phi(0:p) = coefficients of AR + differencing operator for Z(t) 
c     phis(0:ps) = coefficients of AR + differencing operator for S(t)
c     phin(0:pn) = coefficients of AR + differencing operator for N(t)
c     theta(0:q) = coefficients of MA operator for z(t)
c     thetan(0:qn) = coefficients of MA operator for N(t)
c     thetas(0:qs) = coefficients of MA operator for S(t)
c     sigrts     = Var(b(t))/Var(a(t)), the variance ratio for S(t)
c     sigrtn     = Var(e(t))/Var(a(t)), the variance ratio for N(t)
c     mx is some order greater than what is needed for a general problem
c     kk = number of subdivisions of the interval [0,pi] for transfer
c             and phase delay functions
c     m is number of "future" observations available on Z
c             (or steps ahead if m is negative)
c
c ACGTHT(-qs:qs)= autocovariance generating function of MA side of model
c                 (without variance ratio)  
c   for S(t), that is, thetas(B)*thetas(F). 
c g = vector (h+k+1 by 1) containing coefficients of B^(-h),...B^0,
c     B^1,..B^k
c (g also holds the coefficients c(h),...c(1), d(0), ...d(k) as
c     output of subroutine)
c a is matrix (h+k+1 by h+k+1) containing proper coefficients of
c     phin and theta
c d is vector (k+1 by 1) containing coefficients of B^0, ... B^k
c     (part of output g)
c w is vector of frequencies for computing squared gain
c rephis is real part of phis(exp(i*w(j)), similarly for red, retheta
c imphis is imaginary part of phis(exp(i*w(j)),
c     similarly for imd, imtheta
c requot is real part of (F^m)*phis*d/theta
c     (evaluated at exp(i*w(j)) in place of B)
c imquot is imaginary part of (F^m)*phis*d/theta
c     (evaluated at exp(i*w(j)) in place of B)
c transf is the transfer function (requot^2+imquot^2) times square of
c     phased is phase delay function 
c                  
      implicit none
c-----------------------------------------------------------------------
c    add include files to define print and save logical vectors, pointer
c    variables  BCM May 2003
c    add include files for notset parameters BCM April 2004
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'cchars.i'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'error.cmn'
      include 'seattb.i'
c-----------------------------------------------------------------------
c
c declare input variables
c
      integer qstar,pstar,nthets,npsi,nchcyc,nthadj,nthetp,nchi,
     1        nthtra, npscyc, nchpsi, nthcya, ncyc, nthetc
      double precision thstar(qstar), thets(nthets),
     1   psi(npsi), varwns, thadj(nthadj), chcyc(nchcyc),
     1   varwna, thetp(nthetp), chi(nchi), varwnp, thtra(nthtra), 
     1   pscyc(npscyc), varwnt, chpsi(nchpsi), thcya(nthcya), varwca,
     1   cyc(ncyc), thetc(nthetc), varwnc
c
c declare internal variables
c
      character outstr*(110)
      integer i, j, ii, jj, k, p, q, ps, qs, pn, qn, shft, lda, job, h, 
     1        kk, info, mx, lb, ub, m, ipos
      double precision sigrts, sigrtn
      parameter (mx = 300, kk=1200)
      double precision phis(0:mx),phin(0:mx),acgths(-mx:mx), 
     1 acgth(-mx:mx), acgthn(-mx:mx), thetan(0:mx), 
     1 gmeinf(-2*mx:2*mx), gmem(-2*mx:2*mx), gminfn(-mx:mx), 
     1 acgtil(-mx:mx),
     1 theta(0:mx), g(mx), a(mx,mx),thetas(0:mx), d(0:mx),
     1 w(0:kk), rephin(0:kk), imphin(0:kk), c(0:mx), 
     1 red(0:kk), imd(0:kk), retheta(0:kk), imtheta(0:kk),
     1 transf(0:kk), pi, requot(0:kk), imquot(0:kk), phase(0:kk),
     1 numer(0:mx), thetil(0:mx),alpha(0:2*mx), refm(0:kk), imfm(0:kk),
     1 tmp(-mx:mx), tmp2(-mx:mx), phased(0:kk), svalph(0:60,4)
      integer ipvt(mx)
C   LINES OF CODE ADDED FOR X-13A-S : 2
      logical dpeq
      external dpeq
C   END OF CODE BLOCK
c-----------------------------------------------------------------------
c    file handles for signal extraction weights (fhw) and spectral 
c    estimates (fhs) and logical variables for whether file is opened
c    successfully (locok) and parameters for true and false (T, F)
c    added by BCM, May 2003
c-----------------------------------------------------------------------
      INTEGER fhw, fhs, fhc
      LOGICAL Lgraf, lsvsew, lsvcsp, lsvcsc
      LOGICAL locok, T, F
      PARAMETER(T=.TRUE.)
*      LOGICAL F
*      PARAMETER(F=.false.)
c-----------------------------------------------------------------------
c      open (unit=6, file='trans.out')
c     open (unit=7, file='weights.out')
c
c trans.out contains squared gain, phase and phase delay functions
c weights.out contains the filter weights
c
c-----------------------------------------------------------------------
c change to opening separate files with filename, bcm May 2003
c-----------------------------------------------------------------------
*      IF(Lgraf)THEN
*       lsvsew=T
*       lsvcsp=T
*      ELSE
*       lsvsew=Savtab(LSESEW)
*       lsvcsp=Savtab(LSECSP)
*      END IF
*      locok=T
*      IF(lsvsew)call opnfil(T,Lgraf,LSESEW,fhw,locok)
*      if (locok.and.lsvcsp)call opnfil(T,Lgraf,LSECSP,fhs,locok)
*      IF (.not.locok) THEN
*       CALL abend
*       RETURN
*      END IF
c-----------------------------------------------------------------------
c   end of change BCM
c-----------------------------------------------------------------------
      p=pstar-1
      q=qstar-1
      pi=dacos(-1.0D0)
      lda=mx
      lb=0
      ub=0
      do 1 i=0,q
         theta(i)=thstar(i+1)
 1    continue
        do i=q+1,mx
           theta(i)=0.0D0
        end do
      
c
c jj=1 seasonal adjustment filter (S(t) = nonseasonal)
c jj=2 seasonal filter (S(t) = seasonal)
c jj=3 trend filter (S(t) = trend)
c jj=4 cycle filter (S(t) is cycle)
c

      do 5 jj=1,4

*         lsvcsc=Lgraf
*         IF(.not.lsvcsc)lsvcsc=Savtab(LSECSA+jj-1)

c seasonal adjustment filter

         if (jj .eq. 1) then
            ps=nchcyc-1
            pn=npsi-1
            qs=nthadj-1
            qn=nthets-1
            sigrts=varwna
            sigrtn=varwns
            do 6 i=0,ps
               phis(i)=chcyc(i+1)
 6          continue
            do 7 i=0,pn
               phin(i)=psi(i+1)
 7          continue
            do 8 i=0,qs
               thetas(i)=thadj(i+1)
 8          continue
            do 9 i=0,qn
               thetan(i)=thets(i+1)
 9          continue
         end if
c
c set up for seasonal filter
c
         if (jj .eq. 2) then
            ps=npsi-1
            pn=nchcyc-1
            qs=nthets-1
            qn=nthadj-1
            sigrts=varwns
            sigrtn=varwna
            do 10 i=0,ps
               phis(i)=psi(i+1)
 10          continue
            do 11 i=0,pn
               phin(i)=chcyc(i+1)
 11          continue
            do 12 i=0,qs
               thetas(i)=thets(i+1)
 12          continue
            do 13 i=0,qn
               thetan(i)=thadj(i+1)
 13          continue
         end if

C
C set up for trend filter
C
         if (jj .eq. 3) then
            ps=nchi-1
            pn=npscyc-1
            qs=nthetp-1
            qn=nthtra-1
            sigrts=varwnp
            sigrtn=varwnt
            do 14 i=0,ps
               phis(i)=chi(i+1)
 14         continue
            do 15 i=0,pn
               phin(i)=pscyc(i+1)
 15         continue
            do 16 i=0,qs
               thetas(i)=thetp(i+1)
 16         continue
            do 17 i=0,qn
               thetan(i)=thtra(i+1)
 17         continue
         end if
c
c set up for cycle filter
c
         if (jj .eq. 4) then
            ps=ncyc-1
            pn=nchpsi-1
            qs=nthetc-1
            qn=nthcya-1
            sigrts=varwnc
            sigrtn=varwca
            do 21 i=0,ps
               phis(i)=cyc(i+1)
 21         continue
            do 22 i=0,pn
               phin(i)=chpsi(i+1)
 22         continue
            do 23 i=0,qs
               thetas(i)=thetc(i+1)
 23         continue
            do 24 i=0,qn
               thetan(i)=thcya(i+1)
 24         continue
         end if

c ----------------------------------------------------------------------
c
*         IF(lsvcsp)write(fhs,96)
         
         if (jj .eq. 1) then
            if (ps .eq. 0 ) then 
*               IF(lsvcsp)write(fhs,960)
*               IF(lsvsew)THEN
*c                write(fhw,960)
*                DO i=0,60
*                 svalph(i,jj)=DNOTST
*                END DO
*               END IF
               goto 5
            else
*               IF(lsvcsp)write(fhs, 330)
*               IF(lsvcsc)THEN
*                call opnfil(T,Lgraf,LSECSA,fhc,locok)
*                IF (.not.locok) THEN
*                 CALL abend
*                 RETURN
*                END IF
*               END IF
            end if
         else if (jj .eq. 2) then
            if (ps .eq. 0) then 
*               IF(lsvcsp)write(fhs,961)
*               IF(lsvsew)THEN
*c                write(fhw,961)
*                DO i=0,60
*                 svalph(i,jj)=DNOTST
*                END DO
*               END IF
               goto 5
            else 
**               IF(lsvcsp)write(fhs,340)
*               IF(lsvcsc)THEN
*                call opnfil(T,Lgraf,LSECSS,fhc,locok)
*                IF (.not.locok) THEN
*                 CALL abend
*                 RETURN
*                END IF
*               END IF
            end if
         else if (jj .eq. 3) then
            if (ps .eq. 0) then 
*               IF(lsvcsp)write(fhs,962)
*               IF(lsvsew)THEN
*c                write(fhw,962)
*                DO i=0,60
*                 svalph(i,jj)=DNOTST
*                END DO
*               END IF
               goto 5
            else 
*               IF(lsvcsp)write(fhs,350)
*               IF(lsvcsc)THEN
*                call opnfil(T,Lgraf,LSECST,fhc,locok)
*                IF (.not.locok) THEN
*                 CALL abend
*                 RETURN
*                END IF
*               END IF
            end if
         else
            if (ps .eq. 0) then
*               IF(lsvcsp)write(fhs,963)
*               IF(lsvsew)THEN
*c                write(fhw,963)
*                DO i=0,60
*                 svalph(i,jj)=DNOTST
*                END DO
*               END IF
               goto 5
            else
*               IF(lsvcsp)write(fhs,360)
*               IF(lsvcsc)THEN
*                call opnfil(T,Lgraf,LSECSC,fhc,locok)
*                IF (.not.locok) THEN
*                 CALL abend
*                 RETURN
*                END IF
*               END IF
            end if
         end if
*         IF(lsvcsp)write(fhs,96)
c
c write variables
c
         IF (lsvcsp)THEN
            write (fhs,50) p, ps, pn,q, qs, qn, sigrts, sigrtn
            write (fhs,60) (phis(i),  i = 0,ps)
            write (fhs,70) (phin(i),  i = 0,pn)
            write (fhs,80) (thetas(i), i=0,qs)
            write (fhs,85) (thetan(i), i=0,qn)
            write (fhs,90) (theta(i), i = 0,q)
         endif

c
c compute thetas(F)*thetas(B), note,
c     will multiply by variance ratio later)
c compute thetan(F)*thetan(B) 
c compute theta(F)*theta(B)
      
         call mult0(thetas,mx,qs,thetas,0,mx,0,qs,acgths,mx,mx)
         call mult0(thetan,mx,qn,thetan,0,mx,0,qn,acgthn,mx,mx)
         call mult0(theta,mx,q,theta,0,mx,0,q,acgth,mx,mx)

c      IF(lsvcsp)THEN
c     write(fhs,96)
      
c     write(fhs,101)
c 101 format(1x, 'thetan(F)*thetan(B)')

c     do 100 j=-qn,qn
c        write(fhs, 102) acgthn(j)
c 100 continue
c 102 format(1x, f6.3)

c     write(fhs,96)

c     write(fhs,103)
c 103 format(1x, 'thetas(F)*thetas(B)')

c     do 104 j=-qs,qs
c        write(fhs, 102) acgths(j)
c 104 continue

c     write(fhs,96)

c     write(fhs,105)
c 105 format(1x, 'theta(F)*theta(B)')

c     do 106 j=-q,q
c        write(fhs, 102) acgth(j)
c 106 continue
c      end if

c ----------------------------------------------------------------------
c
c start loop for m

c compute h and k

         do 95 m=lb,ub

            k= max(ps-1,qs+m)
            h= max(q,pn+qs-m)
*      
*            IF(lsvcsp)THEN
*               write(fhs,96)
c     write(fhs,97) m
c 97  format(1x, 'm is', 1x, i5)
c     write(fhs,96)
*               write(fhs,99) h,k
*            END IF

c-----------------------------------------------------------------------
c Compute g(B) = phis(F)*thetas(F)*thetas(B).
c Store coefficients of F^h, ... , B^k, where k = max(ps,qs) in
c (g(1), ... , g(h+k+1))'.  Note that some of the first 
c coefficients are zero if h > pn+qs, and some of the last coefficients
c are zero if k > qs.  The first nonzero computed coefficient of
c phis(F)*ACGF(B) is stored in g(shft) where shft = h-(pn+qs).
c
            do 115 i=1,h+k+1
               g(i)=0.0D0
 115        continue

            call mult0(phin, mx, pn, acgths, mx,mx, qs, qs, tmp, mx,mx)
            shft = h + m - (pn+qs)
            do 120 i = shft+1, shft+1+pn+2*qs
               g(i) = tmp(i-shft-1-pn-qs)
 120        continue

c      IF(lsvcsp)write (fhs,125) (g(i),  i = 1, h+k+1)
c 125   format(/1x, 'g vector',1x, 16(f12.6))


c-----------------------------------------------------------------------
c Set up and solve linear equations for c(q),...,c(0),d(1),...,d(k)
c where c(F) = c(0) + c(1)*F + ... + c(h)*F^h, d(B) =
c              1 + d(1)*B + ... + d(k)*B^k, 
c and c(F)*phis(B) + theta(F)*d(B) = g(B).
c Write linear equations as Ax = g.
c

c
c Set up (h+k+1) by (h+k+1) matrix A for linear equations
c
      
            do 130 j=1,mx
               do 131 i=1,mx
                  a(i,j) = 0.0D0
 131           continue
 130        continue
            if (h. gt. 0) then
               do 140 j = 1, h
                  do 141 i = j, ps+j
                     if (i .eq. j) then
                        a(i,j) = 1.0D0
                     else
                        a(i,j) = phis(i-j)
                     endif
 141              continue
 140           continue
            endif
            do 142 j = h+1, h+k+1
               do 144 i = j, j-q, -1
                  if (i .eq. j) then
                     a(i,j) = 1.0D0
                  else
                     a(i,j) = theta(j-i)
                  endif
 144           continue
 142        continue
c
c write A matrix
c
c      IF(lsvcsp)THEN
c       write(fhs,150)
c 150   format(/1x, 'The "A" matrix')
c       do 160 i = 1,h+k+1
c        write (fhs,170) (a(i,j),  j = 1,h+k+1)
c 170    format(/1x, 16(f8.2))
c 160   continue
c      end if

c
c Solve linear equations Ax = g for x = (c(h),...,c(1),d(0),...,d(k))'
c Note that solution (as well as input) is stored in g
 
      
            call dgefa(a,lda,h+k+1,ipvt,info)
            job=0
            call dgesl(a,lda,h+k+1,ipvt,g,job)
c
c write coefficients g = (c(h),...,c(1),d(0),...,d(k))'
c

c      IF(lsvcsp)THEN
c       write(fhs,180)
c 180   format(/1x, 'coefficients c(h) .. c(1)')
c       do 190 i=1,h
c        write(fhs,195) g(i)
c 190   continue 
c 195   format(1x, f14.6)

c     write(fhs,196)
c 196   format(/1x, 'coefficients d(0) .. d(k)')
c       do 197 i=h+1,h+k+1
c        write(fhs,198) g(i)
c 197   continue 
c 198   format(1x, f14.6)
c      endif
c
c fill d vector
c
            do 200 i=0,k
               d(i)=g(i+h+1)
 200        continue

c
c fill c vector
c
            c(0)=0.0D0
            do 201 i=h,1, -1
               c(i)=g(h-i+1)
 201        continue
c ----------------------------------------------------------------------
c get coefficients of alpha(B)=(F**m)*phin(B)*d(B)/theta(B)
c
c first get coefficients of numer(B)=phin(B)*d(B)
c
            call mult1(phin,mx,pn,d,mx,k,numer,mx)
c
c now get coefficients of thetil(B)=1/theta(B)
c only up to order mx
c
            thetil(0)=1.0D0
            do 205 j=1,mx
               thetil(j)=0.0D0
               do 206 i=1,j
                  thetil(j)=thetil(j)-theta(i)*thetil(j-i)
 206           continue
 205        continue
c
c now get coefficients (up to mx) of alpha(B) (for m = 0)
c by taking numer*thetil
c first compute numer*thetil
c then multiply by F**m (shift by m), and multiply by variance ratio
c
            call mult1(numer,mx,pn+k,thetil,mx,mx,alpha,2*mx)
c
c store coefficients of F^m, F^m+1,....in alpha(0), alpha(1),...
c shifted by m
c multiply by variance ratio

c     do 210 i=0,mx
c        alpha(i)=sigrts*alpha(i)
c 210 continue
c
c write as many filter coefficients as desired
c
c      IF(lsvcsp)THEN
c     write(fhs,96)

c     write(fhs,212) m
c 212 format(1x, 'some coefficients of B^i alpha filter for m =  ', i3)
c     write(fhs,211)
c     write(fhs,217) mx
c 217   format(1x, 'mx is', i5)
c      end if
c
*            IF(lsvsew)THEN
*               write(fhw,96)
*               if (jj .eq. 1) then
*                  write(fhw, 213)
*               else if (jj .eq. 2) then
*                  write(fhw,214)
*               else if (jj .eq. 3) then
*                  write(fhw,215)
*               else
*                  write(fhw,216)
*               end if
*               write(fhw,96)
*               write(fhw,217)
*               write(fhw,96)
*               do 290 i=0,60 
*                  write(fhw,218) i-m, alpha(i)
c        write(fhw,218) alpha(i)
*                  svalph(i,jj)=alpha(i)
* 290           continue
*               write(fhw,96)
*            END IF
c 218 format(1x, f10.6)
c     end if
c-----------------------------------------------------------------------
c compute mean square error
c compute autocovariance generation function gamma(eps,m)
c first compute gamma(eps,infinity)
c see Bell and Martin, formula (38)
c
c compute numerator 

            call mult2(acgths, mx, mx, qs, qs, acgthn, mx, mx, qn, qn, 
     1                 gminfn, mx,mx)
c
c compute denominator   
           
            call mult0(thetil,mx,mx,thetil,0,mx,0,mx,acgtil,mx,mx)

c now multiply to get gmeinf
      
            call mult2(gminfn,mx, mx, qs+qn, qs+qn, acgtil, mx, mx, mx,
     1                 mx, gmeinf, 2*mx, 2*mx)
c
c multiply by variances
c     
            do 219 j=-(mx+qs+qn),mx+qs+qn
               gmeinf(j)=gmeinf(j)*sigrts*sigrtn
 219        continue

c
c compute rest of gmem = gamma(eps,m) formula (43)
c
            call mult0(c, mx, h, c, 0, mx, 0, h, tmp2, mx,mx)
            call mult2(tmp2, mx, mx, h, h, acgtil, mx, mx, mx, mx,
     1                 gmem, 2*mx,2*mx)

            do 220 j=-(mx+h),mx+h
               gmem(j)=gmem(j)*(sigrts**2.0D0)
 220        continue

c      IF(lsvcsp)THEN
c     write(fhs,96)
c     write(fhs,224)
c 224 format(1x, 'coefficients of c(F)*c(B)')
c     do 225 j=-h,h
c        write(fhs,226) tmp2(j)
c 225 continue
c 226 format(1x, f10.3)
c      END IF
c
c Now add coefficients of gamma(eps, inf) from above (equation 43)
c
            do 228 j=-mx,mx
               gmem(j)=gmem(j)+gmeinf(j)
 228        continue

c      IF(lsvcsp)THEN
c     write(fhs,96)
c     write(fhs,229)
c 229 format(1x, 
c     1 'coefficients of autocovariance gen. funct. gamma(eps,m)')
c     do 230 j=0,0
c        write(fhs,232)  gmem(j)
c 230 continue
c 232 format(1x, f13.7)
c      END IF

c-----------------------------------------------------------------------
c The transfer and phase functions are computed in this section
c
C
C Compute frequencies w(i)
C
            do 238 i = 0,kk
               w(i) = pi*dble(i)/dble(kk)
 238        continue
 
C
C Compute real and imaginary parts of polynomials evaluated at
c B = exp(-iw(j))
C
            do 240 j = 0,kk
               rephin(j)=phin(0)
               imphin(j)=0.0D0
               red(j)=d(0)
               imd(j)=0.0D0
               retheta(j)=theta(0)
               imtheta(j)=0.0D0
               if (pn .gt. 0) then
                  do 241 ii = 1,pn
                     rephin(j)=rephin(j)+phin(ii)*dcos(w(j)*dble(ii))
                     imphin(j)=imphin(j)-phin(ii)*dsin(w(j)*dble(ii))
 241              continue
               else
                  rephin(j)=rephin(j)
               endif
            
               do 245 ii=1,k
                  red(j)=red(j)+d(ii)*dcos(w(j)*dble(ii))
                  imd(j)=imd(j)-d(ii)*dsin(w(j)*dble(ii))
 245           continue
           
               if (q .gt. 0) then
                  do 250 ii = 1,q
                     retheta(j)=retheta(j)+theta(ii)*dcos(w(j)*dble(ii))
                     imtheta(j)=imtheta(j)-theta(ii)*dsin(w(j)*dble(ii))
 250              continue
               else
                  retheta(j)=retheta(j)
               endif
               refm(j)=dcos(w(j)*dble(m))
               imfm(j)=dsin(w(j)*dble(m))
 240        continue
        

c compute transfer and phase functions
c first compute  real and imaginary parts of phis*d/theta
c store in requot and imquot
c then multiply by exp(i*m*wj) and store result in imquot
c 
c
            do 300 i=0,kk
               requot(i)=retheta(i)*(rephin(i)*red(i)-imphin(i)*imd(i))+
     1                   imtheta(i)*(rephin(i)*imd(i)+red(i)*imphin(i))
               imquot(i)=retheta(i)*(rephin(i)*imd(i)+red(i)*imphin(i))-
     1                   imtheta(i)*(rephin(i)*red(i)-imphin(i)*imd(i))
               requot(i)=requot(i)/(retheta(i)**2.0D0+imtheta(i)**2.0D0)
               imquot(i)=imquot(i)/(retheta(i)**2.0D0+imtheta(i)**2.0D0)
               requot(i)=requot(i)*refm(i)-imquot(i)*imfm(i)
               imquot(i)=imquot(i)*refm(i)+imfm(i)*requot(i)
 300        continue

            do 320 i=0,kk
               transf(i)=requot(i)**2.0D0+imquot(i)**2.0D0
C
C multiplying by square of variance ratio now
C
               transf(i)=transf(i)*(sigrts**2.0D0)
c 
c compute phase shift
c
               if (dpeq(requot(i),0.0D0).and.(imquot(i).gt.0.0D0)) then
                  phase(i)=pi/2.0D0
               else if (dpeq(requot(i),0.0D0).and.(imquot(i).lt.0.0D0)) 
     1              then
                  phase(i)=-pi/2.0D0
               else if ((requot(i).lt.0.0D0) .and. (imquot(i).ge.0.0D0))
     1              then
                  phase(i)=datan(imquot(i)/requot(i))+pi
               else if ((requot(i).lt.0.0D0) .and. (imquot(i).lt.0.0D0))
     1              then
                  phase(i)=datan(imquot(i)/requot(i))-pi
               else if (requot(i) .gt. 0.0D0) then
                  phase(i)=datan(imquot(i)/requot(i))           
               else
                  phase(i)=9999.0D0
               end if
               if (i .eq. 0) then
                  phased(i)=0.0D0
               else
                  phased(i)=-phase(i)/w(i)
               end if
 320        continue

c
c write transfer and phase functions at frequencies w(i)
c

*            IF(lsvcsp)write(fhs,96)
            
*            IF(lsvcsc)THEN
*            
*               write(fhc,1000)'freq',TABCHR,'transferfnc',TABCHR,
*     &                        'phaseshift',TABCHR,'phasedelay'
*               WRITE(fhc,1000)'----------------------',
*     &                        (TABCHR,'----------------------',k=1,3)
*
*               do 400 i=0,kk
*                  ipos=1
*                  CALL setchr(' ',110,outstr)
*                  CALL dtoc(w(i),outstr,ipos)
*                  IF(Lfatal)RETURN
*                  outstr(ipos:ipos)=TABCHR
*                  ipos=ipos+1
*                  CALL dtoc(transf(i),outstr,ipos)
*                  IF(Lfatal)RETURN
*                  outstr(ipos:ipos)=TABCHR
*                  ipos=ipos+1
*                  CALL dtoc(phase(i),outstr,ipos)
*                  IF(Lfatal)RETURN
*                  outstr(ipos:ipos)=TABCHR
*                  ipos=ipos+1
*                  CALL dtoc(phased(i),outstr,ipos)
*                  IF(Lfatal)RETURN
*                  write(fhc,1002)outstr(1:(ipos-1))
* 400           continue
*               CALL fclose(fhc)
*            END IF
      
c 351   format(1x, i4, 1x, f8.3, 1x, 2(f12.6, 1x))
c  end loop for m
c
 95      continue
  5   continue
c-----------------------------------------------------------------------
c     write out saved filter weights, if weights are to be saved.
c     (BCM April 2004)
c-----------------------------------------------------------------------
*      IF(lsvsew)THEN
*       WRITE(fhw,1001)'lag',TABCHR,'sadj',TABCHR,'seasonal',TABCHR,
*     &                'trend',TABCHR,'cycle'
*       WRITE(fhw,1001)'------',(TABCHR,'----------------------',k=1,4)
c-----------------------------------------------------------------------
*       DO i=0,60
*        ipos=1
*        CALL setchr(' ',110,outstr)
*        CALL itoc(i,outstr,ipos)
*        IF(Lfatal)RETURN
*        DO k=1,4
*         outstr(ipos:ipos)=TABCHR
*         ipos=ipos+1
*         CALL dtoc(svalph(i,k),outstr,ipos)
*         IF(Lfatal)RETURN
*        END DO
c-----------------------------------------------------------------------
*        WRITE(fhw,1002)outstr(1:(ipos-1))
*       END DO
c-----------------------------------------------------------------------
*      END IF
c-----------------------------------------------------------------------
c     Close files opened during routine (BCM, May 2003)
c-----------------------------------------------------------------------
*      IF(locok)THEN
*         IF(lsvcsp)CALL fclose(fhs)
*         IF(lsvsew)CALL fclose(fhw)
*      END IF
c-----------------------------------------------------------------------
 50   format(1x, 'p =', i3, 1x, 'ps =', i3, 1x, 'pn =', i3, 1x,
     1       'q =', i3, 1x, 'qs =', i3, 1x, 'qn =', i3, 1x,  
     1       'variance ratio for s =', f12.6, 1x,
     1       'variance ratio for n =', f12.6)
 60   format(/1x, 'phis coefficients',1x, 50(f21.15))
 70   format(/1x, 'phin coefficients',1x, 50(f21.15))
 80   format(/1x, 'thetas coefficients',1x, 50(f21.15))
 85   format(/1x, 'thetan coefficients',1x, 50(f21.15))
 90   format(/1x, 'theta coefficients',1x, 50(f21.15))
 96   format(1x, ' ')
 99   format(1x, 'h =', i3, 1x, 'k =', i3)
c-----------------------------------------------------------------------
 213  format(1x,
     1 'weights (for infinite concurrent seasonal adjustment filter)')
 214  format(1x, 'weights (for infinite concurrent seasonal filter)')
 215  format(1x, 'weights (for infinite concurrent trend filter)')
 216  format(1x, 'weights (for infinite concurrent cycle filter)')
 217  format(1x, '   i   alpha(i)')
 218  format(1x, i4, 1x, f10.6)
c-----------------------------------------------------------------------
 960  format(1x, ' there is no seasonally adjusted component')
 961  format(1x, ' there is no seasonal  component')
 962  format(1x, ' there is no trend component')
 963  format(1x, ' there is no cycle component')
c-----------------------------------------------------------------------
 330  format(1x, 'infinite concurrent seasonal adjustment filter')
 340  format(1x, 'infinite concurrent seasonal filter')
 350  format(1x, 'infinite concurrent trend filter')
 360  format(1x, 'infinite concurrent cycle filter')
c-----------------------------------------------------------------------
 1000 format(7a)
 1001 format(9a)
 1002 format(a)
c-----------------------------------------------------------------------
      return
      end
      