C     Last change:      REG  23 Dec 2005
C     Previous change:  BCM   4 Oct 2002    3:08 pm
C
C
C SEARCH SUBROUTINE
C FINDS THE VALUES OF THE PARAMETERS FOR WHICH THE VALUE OF A
C          FUNCTION IN THESE PARAMETERS IS MINIMISED.
C PROGRAM VARIABLES INPUT FROM CALLING PROGRAM:-
C VARIABLE NAME      PURPOSE
C
C   NX          NUMBER OF PARAMETERS.
C   X           ARRAY OF STARTING VALUES OF PARAMETERS.
C   XMIN        ARRAY OF LOWER BOUNDS OF PARAMETERS.
C   XMAX        ARRAY OF UPPER BOUNDS OF PARAMETERS.
C   EPSIV       CONVERGENCE LIMIT FOR BOUNDED PARAMETERS.
C   EPSIF       CONVERGENCE LIMIT FOR FUNCTION VALUES.
C   E           ARRAY, -1 FOR PARAMETERS WITHOUT BOUNDS,
C                       0 FOR PARAMETERS WITH BOUNDS.
C                      +1 FOR PARAMETERS TO BE FIXED.
C   CONV        ARRAY OF CONVERGENCE TESTS FOR UNBOUNDED PARAMETERS.
C   N           NUMBER OF OBSERVATIONS IN DATA.
C   F           ARRAY OF ERRORS.
C   FI          SUM OF SQUARES OF ERRORS (TO BE MINIMISED).
C   MAXIT       MAXIMUM NUMBER OF ITERATIONS ALLOWED.
C   MAXF        MAXIMUM NUMBER OF FUNCTION VALUES ALLOWED.
C   IPRINT      1 FOR EXTRA PRINTING, 0 OTHERWISE.
C   SET         ARRAY OF STANDARD ERRORS OF PARAMETERS.
C   CE          CORRELATION MATRIX OF PARAMETERS.
C   FIXED       RETURN 1 IF SOME PARAMETERS ARE FIXED 0 OTHERWISE
C   FIXVAL      IS THE DIMENSION OF AUTOREGRESSIVE PART
C   UR          THE VALUES TO WHICH THE AR PARAMETERS ARE FIXED
C   OUT         1,2 NO PRINT ARE PERFORMED
C   *           RETURN STATEMENT IF PARAMETERS ARE OUT OF BOUNDS.
C
C
C SUBROUTINES CALLED BY SEARCH:
C      FEASI   (SUPPLIED WITH SEARCH).
C      CALCFX  (SUPPLIED BY THE USER).
C      CALCFX IS USED TO CALCULATE THE VALUE OF THE FUNCTION TO BE
C      MINIMISED. THE ARGUMENTS PASSED TO CALCFX ARE NX,X,FI,N,F
C      DEFINED ABOVE (BUT X WILL CONTAIN THE CURRENT VALUES OF THE
C      PARAMETERS).
C
C
      subroutine SEARCH(nx,x,xmin,xmax,epsiv,e,convc,n,f,fi,maxit,maxf,
     $ iprint,set,ce,fixed,fixval,ipr,ur,out,itn,bd,dr,Ierr,Errext,*)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
      integer n10
      parameter (n10 = 10)
      logical T
      parameter (T = .true.)
      real*8 ZERO,ONE,TWO,ONEHND
      parameter (ZERO=0.0d0,ONE=1.0d0,TWO=2.0d0,ONEHND=100.0d0)
C
      INCLUDE 'htmlout.cmn'
      INCLUDE 'units.cmn'
C
C.. Formal Arguments ..
      integer nx,e(*),n,maxit,maxf,iprint,fixed,fixval,out,Ierr,dr,bd,
     $        ipr
      real*8 x(*),xmin(*),xmax(*),epsiv,convc(*),f(*),fi,set(*),
     $       ce(n10,n10),ur
      character Errext*180
C
C.. Local Scalars ..
      integer i,icom,ip,isp,itn,j,ji,jp,jsum,k,kl0,l,m,nv,nvc,itmp,
     $        nUnitReg
      real*8 b,ceps,cgam,cotb,det,dgam,fbt,fib,fit,fitt,gam,lam,minmu,q,
     $       qth,qtheti,r,range,red,rf,sa,sg,sgam,sp,st,stg,stj,sx,
     $       theta,ttheta,v,xtest
      character*10 ctmp
C
C.. Local Arrays ..
      real*8 a(2*n10,2*n10),bx(2*n10),c(2*n10,2*n10),d(2*n10),
     $       del(2*n10),ds(2*n10),f1(mpkp),fia(2*n10),g(2*n10),
     $       gs(2*n10),h(2*n10),mu(n10),p(mpkp,2*n10),qtheta(2*n10),
     $       s(2*n10,2*n10),se(2*n10),sig(2*n10),ss(2*n10),tv(2*n10),
     $       ts(2*n10)
C
C.. External Calls ..
      external CALCFX, FEASI
C   LINES OF CODE ADDED FOR X-13A-S : 2
      logical dpeq
      external dpeq
C   END OF CODE BLOCK
C
C.. Intrinsic Functions ..
      intrinsic ABS, ACOS, ATAN, SIN, SQRT
      include 'count.i'
      include 'stream.i'
C
C ... Executable Statements ...
C
      Icomm = 0
      fit = ZERO
      ceps = 1.0d-12
C
      do i = 1,nx
       if (e(i) .eq. 0) then
        if (x(i).lt.xmin(i) .or. x(i).gt.xmax(i)) then
         if (out.eq.0) THEN
          CALL wWritln('STARTING VALUES OUT OF RANGE OR BOUNDS IN '//
     $                 'THE WRONG ORDER',Nio,0,T,T)
         end if
         return 1
        end if
       end if
      end do
C
C FIND INITIAL FUNCTION VALUE FI AND INITIAL ERROR ARRAY F
      Ifn = 0
      call CALCFX(nx,x,fi,n,f,Ierr,Errext,out,*5021)
      if (Ierr.ne.0) then
       return
      end if
      Ifn = 1
      itn = 1
      mu(1) = ZERO
      mu(2) = 0.1d0
      do j = 3,n10
       mu(j) = 2.d0 * mu(j-1) / (1.d0+mu(j-1))
      end do
C
C SET UP ARRAY DEL, USED TO CALCULATE DERIVATIVES
      do j = 1,nx
       if (e(j) .lt. 0) then
        del(j) = 0.1d0 * convc(j)
       else if (e(j) .eq. 0) then
        range = xmax(j) - xmin(j)
        del(j) = range * 0.0001d0
        convc(j) = range * epsiv
       end if
      end do
C
C START OF ITERATION PROCEDURE.
C TEST TO SEE IF MAXIMUM NUMBER OF ITERATIONS OR FUNCTION VALUES
C EXCEEDED
C
      do 15 while (itn.lt.maxit .and. Ifn.lt.maxf)
C
       if ((iprint .gt. 0).and. (out.eq.0)) then
 6001    format ('<p><em>ITERATION </em>',i5,i12,
     $           '<em> FUNCTION VALUES F = </em>',e17.8,
     $           6(a,e20.6),'</p>')
         write (Nio,6001) itn, Ifn, fi, (Cbr, x(j), j = 1,nx)
       end if
*       write(Mtprof,7001) itn, Ifn, fi, (x(j), j = 1,nx)
 7001    format (
     $   /,' '/' ITERATION ',i5,i12,' FUNCTION VALUES       F = ',
     $   e17.8/(6e20.6))
*       CALL outARMAParam()
       do j = 1,nx
        bx(j) = x(j)
       end do
C
C TEST FUNCTION VALUE FOR ABSOLUTE MAGNITUDE.
C
       if (fi .lt. 1.0d-9)THEN
*        call profiler(2,'**GO TO 5020**, line 165')
        goto 5020
       END IF
C
C
       fib = fi
       nv = nx
C
C IF A BOUNDED PARAMETER IS WITHIN 1 PERCENT OF ITS BOUNDARY,
C FIX IT AT THE BOUNDARY. THE BOUNDARY IS "UR" FOR AUTOREGRESSIVE
C PARAMETERS AND XMAX(.) FOR MOVING AVERAGE PARAMETERS
C
       nUnitReg=0
       do 10 j = 1,nx
        if (e(j) .lt. 0) then
*         call profiler(2,'**GO TO 10**, line 172')
         goto 10
        else if (e(j) .eq. 0) then
         xtest = (x(j)-xmin(j)) / (xmax(j)-xmin(j))
         if (xtest .ge. 0.01d0) then
          if (xtest .gt. 0.99d0) then
           if (j .le. fixval) then
            bx(j) = 0.99d0
           else
            bx(j) = xmax(j)
           end if
          else
*           call profiler(2,'**GO TO 10**, line 184')
           goto 10
          end if
         else if (j .le. fixval) then
          if (j.gt.ipr) then 
           if (bd.eq.0) then
            bx(j) = -ur
           else
*              call profiler(2,'**GO TO 10**, line 192')
            goto 10
           end if
          else
           if ((dr+nUnitReg).lt.2) then
            bx(j)=-ur
            nUnitReg=nUnitReg+1
           else
*            call profiler(2,'**GO TO 10**, line 201')
            goto 10
           end if
          end if 
         else
          bx(j) = xmin(j)
         end if
         e(j) = 1
*         call profiler(2,'entering CALCFX, line 208')
         call CALCFX(nx,bx,fi,n,f,Ierr,Errext,out,*5021)
*         call profiler(2,'exiting CALCFX')
         if (Ierr.ne.0) then
          return
         end if
         Ifn = Ifn + 1
         if ((iprint.gt.0) .and. (out.eq.0)) then
 6002      format ('<p><em>PARAMETER </em>',i2,'<em> FIXED</em></p>')
           write (Nio,6002) j
         end if
        end if
        nv = nv - 1
 10    continue
C
C NV IS THE NUMBER OF PARAMETERS WHICH ARE NOT YET FIXED.
C
       if (nv .le. 0) THEN
*        call profiler(2,'**GO TO 5018**, line 226')
        goto 5018
       END IF
C
C CALCULATE FIRST DERIVATIVES
       k = 1
       do j = 1,nx
        if (e(j) .ne. 1) then
*         write(Mtprof,*) ' bx(',j,'), del(',j,') = ', bx(j), del(j)
         bx(j) = bx(j) + del(j)
*         write(Mtprof,*) ' bx = ', (bx(i), i = 1, nx)
*         call profiler(2,'entering CALCFX, line 235')
         call CALCFX(nx,bx,fi,n,f1,Ierr,Errext,out,*5021)
*         call profiler(2,'exiting CALCFX')
         if (Ierr.ne.0) then
          return
         end if
         Ifn = Ifn + 1
         do i = 1,n
          p(i,k) = (f(i)-f1(i)) / del(j)
         end do
         bx(j) = bx(j) - del(j)
         k = k + 1
        end if
       end do
C
C CALCULATE MATRIX A=P'.P
       do j = 1,nv
        do k = 1,j
         sa = ZERO
         do i = 1,n
          sa = sa + p(i,j)*p(i,k)
         end do
         a(j,k) = sa
        end do
       end do
C
C CALCULATE ARRAY G=F.P , $ CHECK THAT G HAS A NON-ZERO ELEMENT
       do j = 1,nv
        sg = ZERO
        do i = 1,n
         sg = sg + f(i)*p(i,j)
        end do
        g(j) = sg
       end do
       do j = 1,nv
        if (ABS(g(j)) .gt. ceps) goto 5000
       end do
*       call profiler(2,'**GO TO 5020**, line 272')
       goto 5020
C
 5000  if (nv .gt. 1) then
C
C ROUTINE IF MORE THAN ONE PARAMETER STILL NOT FIXED.
C
C STANDARDISE MATRIX A AND ARRAY G.
        do j = 1,nv
         sig(j) = SQRT(a(j,j))
         if (sig(j).lt.1.0D-30) then
           sig(j)=1.0D-30
         end if 
         gs(j) = g(j) / sig(j)
         kl0 = j - 1
         do k = 1,kl0
          a(j,k) = a(j,k) / (sig(j)*sig(k))
          a(k,j) = a(j,k)
         end do
         a(j,j) = ONE
        end do
C
C
C INVERT MATRIX A.
        det = ONE
        do i = 1,nv
         det = det * a(i,i)
         r = 10.d0**(-nv-3)
         if (det .lt. r) then
          if (det .gt. 0) then
           if ((iprint .gt. 0).and.(out.eq.0)) then
            CALL wWritln(' MATRIX SINGULAR ',Nio,0,T,T)
           end if
          else
           if (out.eq.0) then
            CALL mkPOneLine(Nio,'strong','MODEL DEGENERATE - DET &lt 0')
           end if
           Ierr = 1
           Errext = 'SEARCH : Model degenerate DET(A) <=0'
           return
          end if
         end if
         if (a(i,i) .le. 1.d-10) goto 5019
         b = 1 / a(i,i)
         a(i,i) = b
         do j = 1,nv
          if (j .lt. i) then
           v = b * a(j,i)
           do k = j,nv
            if (k .lt. i) then
             a(j,k) = a(j,k) + v*a(k,i)
            end if
            if (k .gt. i) then
             a(j,k) = a(j,k) - v*a(i,k)
            end if
           end do
           a(j,i) = -v
          else if (j .ne. i) then
           v = b * a(i,j)
           do k = j,nv
            a(j,k) = a(j,k) - v*a(i,k)
           end do
           a(i,j) = v
          end if
         end do
        end do
        do j = 1,nv
         m = j - 1
         do k = 1,m
          a(j,k) = a(k,j)
         end do
        end do
C
C CALCULATE T*
        st = ZERO
        sg = ZERO
        sp = ZERO
        do j = 1,nv
         stj = ZERO
         do k = 1,nv
          stj = stj + a(j,k)*gs(k)
         end do
         ts(j) = stj
         st = st + ts(j)*ts(j)
         sg = sg + gs(j)*gs(j)
         sp = sp + gs(j)*ts(j)
        end do
C
C CALCULATE GAMMA (THE ANGLE BETWEEN THE TAYLOR POINT DIRECTION
C  $ THE DIRECTION OF STEEPEST DESCENT.)
        cgam = sp / SQRT(st*sg)
        gam = ACOS(cgam)
        dgam = gam * 57.2957795d0
        if ((iprint .gt. 0).and.(out.eq.0)) then
 7005    format (/,'<p class="em">GAMMA = ',f5.1,'</p>')
         write (Nio,7005) dgam
        end if
        sgam = SIN(gam)
        stg = SQRT(st/sg)
C
C DESCALE T AND D
        do j = 1,nv
         tv(j) = ts(j) / sig(j)
         ds(j) = gs(j) * stg
         d(j) = ds(j) / sig(j)
        end do
C EXPAND T AND D TO VECTORS OF LENGTH NX WITH ZEROS FOR FIXED
C  PARAMETER SUBSCRIPTS.  T IS THE TAYLOR POINT, D IS THE POINT
C  0F STEEPEST DESCENT.
        nvc = nv
        do j = 1,nx
         ji = nx - j + 1
         if (e(ji) .gt. 0) then
          tv(ji) = ZERO
          d(ji) = ZERO
         else
          tv(ji) = tv(nvc)
          d(ji) = d(nvc)
          nvc = nvc - 1
         end if
        end do
C
C SET SPIRAL COUNTERS
        icom = 0
        isp = 1
        q = 1
        do j = 1,nx
         x(j) = bx(j) + tv(j)
        end do
C
C IF TAYLOR POINT IS OUT OF BOUNDS FOR BOUNDED PARAMETERS, SCALE
C  IT TO BRING IT WITHIN BOUNDS.
        call FEASI(nx,x,bx,e,xmin,xmax,red)
        if (ABS(red-ONE) .ge. ceps) then
         rf = red * 0.8d0
         do j = 1,nx
          tv(j) = rf * tv(j)
          d(j) = rf * d(j)
         end do
*         call profiler(2,'**GO TO 5010**, line 413')
         goto 5010
        end if
C     .. Head of LOOP ..
 5006   continue
C
C IF THE FUNCTION VALUE IS SMALLER AT THE TAYLOR POINT, THE
C ITERATION IS COMPLETE.
        do j = 1,nx
         if (ABS(tv(j)) .gt. convc(j)) goto 5007
        end do
*        call profiler(2,'**GO TO 5020**, line 424')
        goto 5020
* 5007   call profiler(2,'entering CALCFX, line 426')
 5007   call CALCFX(nx,x,fi,n,f,Ierr,Errext,out,*5021)
*        call profiler(2,'exiting CALCFX, line 427')
        if (Ierr.ne.0) then
         return
        end if
        Ifn = Ifn + 1
        if (fi .lt. fib) THEN
*          call profiler(2,'**GO TO 5011**, line 433')
          goto 5011
        END IF
C
C
        if (isp .gt. 1) then
         fbt = fib*0.75d0 + fit*0.25d0
         if (fi .lt. fbt) then
C
C INTERPOLATE ALONG OT.
          fitt = fi
          if ((iprint.gt.0).and.(out.eq.0)) then
           CALL mkPOneLine(Nio,'bold','INTERPOLATION ALONG OT')
          end if
          lam = (fbt-fitt) / (fib*0.5d0+fit*0.5d0-fitt)
          do j = 1,nx
           x(j) = bx(j) + lam*tv(j)
          end do
*          call profiler(2,'entering CALCFX, line 453')
          call CALCFX(nx,x,fi,n,f,Ierr,Errext,out,*5021)
*          call profiler(2,'exiting CALCFX, line 455')
          if (Ierr.ne.0) then
           return
          end if
          Ifn = Ifn + 1
C
C IF INTERPOLATION ALONG OT IS SUCCESSFUL, THE ITERATION IS COMPLETE.
          if (fi .lt. fib) then
*            call profiler(2,'**GO TO 5012**, line 460')
            goto 5012
          END IF
C
C IF INTERPOLATION ALONG OT IS UNSUCCESSFUL, ADOPT LAMBDA (IF NECESSARY)
C    $ START ON SPIRAL.
          if (fi .ge. fitt) then
           fit = fitt
           if ((iprint .gt. 0).and.(out.eq.0)) then
            CALL wWritln('UNSUCCESSFUL',Nio,0,T,T)
           end if
          else
           if ((iprint .gt. 0).and.(out.eq.0)) then
 7008       format (/,'<p class="bold">LAMBDA ADOPTED ',f12.3,'</p>')
            write (Nio,7008) lam
           end if
           do j = 1,nx
            tv(j) = tv(j) * lam
            d(j) = d(j) * lam
           end do
           q = q * lam
           fit = fi
           cotb = ONE / sgam
          end if
*          call profiler(2,'**GO TO 5008**, line 486')
          goto 5008
         end if
        end if
        fit = fi
        cotb = ONE / sgam
C
C START OF SPIRAL
 5008   fia(1) = fit
        if ((iprint .gt. 0).and.(out.eq.0)) then
 7009    format (/,'<p class="bold">SPIRAL NUMBER ',i4,'</p>')
         write (Nio,7009) isp
        end if
C
C TRY SEVEN POINTS ON THE SPIRAL.
        do j = 2,8
         if (icom .ne. 1) then
          ttheta = mu(j) * sgam / (1.d0-mu(j)+mu(j)*cgam)
          theta = ATAN(ttheta)
          qth = (1.d0-gam*cotb) * (theta/gam)**2
          qtheta(j-1) =
     $      SIN(theta) * (1.d0-theta*cotb-qth) / (mu(j)*sgam)
          do k = 1,nx
           s(j,k) = qtheta(j-1) * (mu(j)*d(k)+(1.d0-mu(j))*tv(k))
          end do
         end if
C
         do k = 1,nx
          x(k) = bx(k) + s(j,k)*q
         end do
         if (icom .ne. 1) then
          call FEASI(nx,x,bx,e,xmin,xmax,red)
          if (ABS(red-ONE) .ge. ceps) then
           jp = j - 1
*           call profiler(2,'**GO TO 5009**, line 520')
           goto 5009
          end if
         end if
*         call profiler(2,'entering CALCFX, line 524')
         call CALCFX(nx,x,fi,n,f,Ierr,Errext,out,*5021)
*         call profiler(2,'exiting CALCFX, line 526')
         if (Ierr.ne.0) then
          return
         end if
         Ifn = Ifn + 1
         jp = j - 1
C
C IF THE FUNCTION VALUE IS SMALLER, THE ITERATION IS COMPLETE.
         if (fi .lt. fib) THEN
*           call profiler(2,'**GO TO 5013**, line 535')
           goto 5013
         END IF
C
C INTERPOLATE BETWEEN POINTS ON THE SPIRAL.
         fia(j) = fi
         h(j) = (fia(j)-fia(j-1)) / (mu(j)-mu(j-1))
         if (j .ge. 3) then
          if (h(j) .gt. h(j-1)) then
           minmu = h(j)*(mu(j-1)+mu(j-2)) - h(j-1)*(mu(j)+mu(j-1))
           minmu = minmu / (2.d0*(h(j)-h(j-1)))
           if (minmu.lt.mu(j) .and. minmu.gt.mu(j-2)) then
            if ((iprint .gt. 0).and.(out.eq.0)) then
 7010         format (/,'<p class="bold">SPIRAL NUMBER ',i4,'</p>')
              write (Nio,7010) jp
            end if
            ttheta = minmu * sgam / (1.d0-minmu+minmu*cgam)
            theta = ATAN(ttheta)
            qtheti = (1.d0-gam*cotb) * (theta/gam)**2
            qtheti = SIN(theta) * (1.d0-theta*cotb-qtheti) /
     $               (minmu*sgam)
            do k = 1,nx
             ss(k) = qtheti * (minmu*d(k)+(1.d0-minmu)*tv(k))
             x(k) = bx(k) + ss(k)
            end do
            if (icom .ne. 1) then
             call FEASI(nx,x,bx,e,xmin,xmax,red)
             if (red .lt. ONE) goto 5009
            end if
*            call profiler(2,'entering CALCFX, line 566')
            call CALCFX(nx,x,fi,n,f,Ierr,Errext,out,*5021)
*            call profiler(2,'exiting CALCFX, line 568')
            if (Ierr.ne.0) then
             return
            end if
            Ifn = Ifn + 1
C
C IF FUNCTION VALUE IS SMALLER, THE ITERATION IS COMPLETE.
            if (fi .ge. fib) then
             if ((iprint .gt. 0).and.(out.eq.0)) then
              CALL wWritln('UNSUCCESSFUL',Nio,0,T,T)
             end if
            else
*             call profiler(2,'**GO TO 5014**, line 576')
             goto 5014
            end if
           end if
          end if
         end if
        end do
C
C SCALE DOWN THE TAYLOR POINT $ POINT OF STEEPEST DESCENT, & TRY
C   ANOTHER SPIRAL  (5 SPIRALS ALLOWED).
        icom = 1
 5009   if (isp .ge. 5) THEN
*          call profiler(2,'**GO TO 5015**, line 588')
          goto 5015
        END IF
        do j = 1,nx
         tv(j) = tv(j) * 0.5d0
         d(j) = d(j) * 0.5d0
        end do
        q = q * 0.5d0
        isp = isp + 1
 5010   do j = 1,nx
         x(j) = bx(j) + tv(j)
        end do
C     .. End of LOOP ..
*        call profiler(2,'**GO TO 5006**, line 601')
        goto 5006
 5011   if ((iprint .gt. 0).and.(out.eq.0)) then
         CALL mkPOneLine(Nio,'bold',' TAYLOR POINT ')
        end if
        itn = itn + 1
*        call profiler(2,' TAYLOR POINT ')
*        call profiler(2,'**GO TO 15**, line 609')
        goto 15
 5012   if ((iprint .gt. 0).and.(out.eq.0)) then
         CALL mkPOneLine(Nio,'bold','SUCCESSFUL')
        end if
        itn = itn + 1
*        call profiler(2,' SUCCESSFUL ')
*        call profiler(2,'**GO TO 15**, line 617')
        goto 15
 5013   if ((iprint .gt. 0).and.(out.eq.0)) then
 7013    format (/,'<p class="bold">SUCCESSFUL AT STEP ',i4,'</p>')
         write (Nio,7013) jp
        end if
        itn = itn + 1
*          write (Mtprof,6013)jp
* 6013     format (/,' ',' SUCCESSFUL AT STEP ',i4)
*        call profiler(2,'**GO TO 15**, line 625')
        goto 15
 5014   if ((iprint .gt. 0).and.(out.eq.0)) then
         CALL mkPOneLine(Nio,'bold','SUCCESSFUL')
        end if
        itn = itn + 1
*        call profiler(2,' SUCCESSFUL ')
*        call profiler(2,'**GO TO 15**, line 632')
        goto 15
C
C STEEPEST DESCENT.
 5015   do j = 1,nx
         d(j) = d(j) * 0.5d0
         x(j) = bx(j) + d(j)
        end do
        call FEASI(nx,x,bx,e,xmin,xmax,red)
        if (red .lt. ONE) then
         do j = 1,nx
          d(j) = 0.9d0 * red * d(j)
          x(j) = bx(j) + d(j)
         end do
        end if
        do while (.true.)
         if ((iprint .gt. 0).and.(out.eq.0)) then
          CALL mkPOneLine(Nio,'bold','STEEPEST DESCENT ')
         end if
         do j = 1,nx
          if (ABS(d(j)) .gt. convc(j)*0.5) THEN
*            call profiler(2,'**GO TO 5016**, line 654')
            goto 5016
          END IF
         end do
*         call profiler(2,'**GO TO 5020**, line 658')
         goto 5020
* 5016    call profiler(2,'entering CALCFX, line 664')
 5016    call CALCFX(nx,x,fi,n,f,Ierr,Errext,out,*5021)
*         call profiler(2,'entering CALCFX, line 666')
         if (Ierr.ne.0) then
          return
         end if
         Ifn = Ifn + 1
C
C IF FUNCTION VALUE IS SMALLER THE ITERATION IS COMPLETE.
         if (fi .ge. fib) then
          do j = 1,nx
           d(j) = d(j) * 0.5d0
           x(j) = bx(j) + d(j)
          end do
         else
*          call profiler(2,'**GO TO 5017**, line 673')
          goto 5017
         end if
        end do
 5017   itn = itn + 1
       else
C
C END OF SPIRAL.
C
C
C ROUTINE IF ONLY ONE PARAMETER IS NOT FIXED.
C IP IS THE PARAMETER NOT YET FIXED
        do j = 1,nx
         tv(j) = ZERO
         if (e(j) .ne. 1) then
          ip = j
          tv(j) = g(1) / a(1,1)
         end if
         x(j) = bx(j) + tv(j)
        end do
        isp = 1
        call FEASI(nx,x,bx,e,xmin,xmax,red)
        if (ABS(red-ONE) .ge. ceps) then
         tv(ip) = 0.8 * red * tv(ip)
*          call profiler(2,'**GO TO 5003**, line 697')
         goto 5003
        end if
C     .. Head of INNER_LOOP ..
 5001   continue
        if (ABS(tv(ip)) .lt. convc(ip)) THEN
*          call profiler(2,'**GO TO 5020**, line 703')
          goto 5020
        END IF
*        call profiler(2,'before CALCFX, line 712')
        call CALCFX(nx,x,fi,n,f,Ierr,Errext,out,*5021)
*        call profiler(2,'exiting CALCFX, line 714')
        if (Ierr.ne.0) then
         return
        end if
        Ifn = Ifn + 1
        if (fi .lt. fib) THEN
*          call profiler(2,'**GO TO 5004**, line 720')
          goto 5004
        END IF
        if (isp .gt. 1) then
         fbt = fib*0.75d0 + fit*0.25d0
         if (fi .lt. fbt) then
          fitt = fi
          if ((iprint .gt. 0).and.(out.eq.0)) then
           CALL mkPOneLine(Nio,'bold','INTERPOLATION ALONG OT')
          end if
          lam = (fbt-fitt) / (fib*0.5d0+fit*0.5d0-fitt)
          x(ip) = bx(ip) + lam*tv(ip)
*          call profiler(2,'before CALCFX, line 732')
          call CALCFX(nx,x,fi,n,f,Ierr,Errext,out,*5021)
*          call profiler(2,'exiting CALCFX, line 734')
          if (Ierr.ne.0) then
           return
          end if
          Ifn = Ifn + 1
          if (fi .lt. fib) THEN
*            call profiler(2,'**GO TO 5005**, line 740')
            goto 5005
          END IF
          if (fi .ge. fitt) then
           fit = fitt
           if (iprint .gt. 0) then
            CALL wWritln('UNSUCCESSFUL',Nio,0,T,T)
           end if
*           call profiler(2,'UNSUCCESSFUL')
*           call profiler(2,'**GO TO 5002**, line 749')
           goto 5002
          else
           tv(ip) = lam * tv(ip)
          end if
         end if
         fit = fi
        end if
 5002   tv(ip) = 0.5d0 * tv(ip)
        isp = isp + 1
 5003   x(ip) = bx(ip) + tv(ip)
C     .. End of INNER_LOOP ..
*        call profiler(2,'**GO TO 5001**, line 761')
        goto 5001
 5004   if ((iprint .gt. 0).and.(out.eq.0)) then
         CALL mkPOneLine(Nio,'bold',' TAYLOR POINT ')
        end if
        itn = itn + 1
*           call profiler(2,'TAYLOR POINT')
*           call profiler(2,'**GO TO 15**, line 768')
        goto 15
 5005   if ((iprint .gt. 0).and.(out.eq.0)) then
         CALL mkPOneLine(Nio,'bold','SUCCESSFUL')
        end if
*           call profiler(2,'SUCCESSFUL')
*           call profiler(2,'**GO TO 15**, line 776')
        itn = itn + 1
       end if
 15   continue
C
*      if (out .eq. 0) then
*         if (HTML .eq. 1) then
* 6015  format('<p>',i4,' <strong>ITERATIONS COMPLETED.</strong><br>',i6,
*     $ ' <strong>FUNCTION VALUES F = </strong>',e17.8,6('<br>',e20.6))
*          write (Nio,6015) itn, Ifn, fi, (x(j), j = 1,nx)
*          write (Nio,'(''</p>'')')
*         else
* 7015  format (
*     $ /,' ',i4,' ITERATIONS COMPLETED '/i6,
*     $ ' FUNCTION VALUES       F = ',e17.8/(6e20.6))
*          write (Mtprof,7015) itn, Ifn, fi, (x(j), j = 1,nx)
*         end if
*      end if
*       CALL outARMAParam()
*       call profiler(2,'**GO TO 5021**, line 793')
      goto 5021
 5018 do j = 1,nx
       x(j) = bx(j)
      end do
      if ((iprint .gt. 0).and.(out.eq.0)) then
 6016    format ('<p><strong>ALL PARAMETERS AT LIMITS F = </strong>',
     $        e17.8,6(a,e20.6),'</p>')
         write (Nio,6016) fi, (Cbr, x(j), j = 1,nx)
      end if
*      write(Mtprof,7016) fi, (x(j), j = 1,nx)
* 7016    format (
*     $   /,' ',' ALL PARAMETERS AT LIMITS ',10x,'F = ',e17.8/(6e20.6))
      fixed = 1 + fixed
      return
 5019 continue
      if (out.eq.0) then
 6017   format('<p><strong>WARNING:</strong> MODEL DEGENERATE - ',
     $         'DIAGONAL ELEMENT ',i4,' SMALL</p>')
        write (Nio,6017) i
        CALL writTag(Nio,'<p>')
        do k = 1,nv
         if (a(k,k) .le. 1.d-10) then
 6018     format (a,'<em>ELEMENT </em>',i4,5x,f10.6)
          write (Nio,6018) Cbr, k, a(k,k)
         end if
        end do
        CALL writTag(Nio,'</p>')
      end if
      Ierr = 1
      Errext = 'SEARCH : Model degenerate Diagonal Element too small'
      return
C
C END OF ITERATION PROCEDURE.
C
C CALCULATE FINAL FUNCTION VALUE.
 5020 do j = 1,nx
       x(j) = bx(j)
      end do
*      call profiler(2,'before CALCFX, line 827')
      call CALCFX(nx,x,fi,n,f,Ierr,Errext,out,*5021)
*      call profiler(2,'exiting CALCFX, line 829')
      if (Ierr.ne.0) then
       return
      end if
      Ifn = Ifn + 1
*      if (out .eq. 0) then
*       if (HTML .eq. 1) then
* 6019   format ('<p><em>CONVERGED AFTER </em>',i2,
*     $          '<em> ITERATIONS AND </em>',i3,
*     $          '<em> FUNCTION VALUES    F = </em>',e17.8,
*     $          6('<br>',e20.6))
*        write (Nio,6019) itn, Ifn, fi, (x(j), j = 1,nx)
*        write (Nio,'(''</p>'')')
*       else
* 7019   format (
*     $  /,' ',' CONVERGED AFTER ',i2,' ITERATIONS AND ',i3,
*     $  ' FUNCTION VALUES    F =',e17.8/(6e20.6))
*        write (Nio,7019) itn, Ifn, fi, (x(j), j = 1,nx)
*       end if
*      end if
 5021 if (n .gt. nx) then
C
C INDICATE WHICH PARAMETERS HAVE BEEN FIXED.
       sx = fi / (n-nv)
*       if (out .ne. 2) then
*        if (HTML .eq. 1) then
* 6020    format ('<p><strong>PARAMETERS FIXED</strong></p>')
*         write (Nio,6020)
*        else
* 7020    format (/,' ','PARAMETERS FIXED ')
*         write (Nio,7020)
*        end if
*       end if
       jsum = 0
       do j = 1,nx
        if (e(j) .ne. -1) then
         jsum = jsum + e(j)
         if (e(j) .eq. 1) then
*          if (out .eq. 0) then
*           if (HTML .eq. 1) then
* 6021       format ('<br>',i6)
*            write (Nio,6021) j
*           else
* 7021      format (i6)
*           write (Nio,7021) j
*           end if
*          end if
          fixed = 1 + fixed
         end if
        end if
       end do
*       if ((out.eq.0) .and. (jsum.eq.0)) then
*        write (Nio,7021) jsum
*       end if
C
C CALCULATE STANDARD ERRORS OF PARAMETERS AND CORRELATION MATRIX.
       if (nv .ne. 1) then
        if ((iprint .gt. 0).and.(out.eq.0)) then
         CALL mkPOneLine(Nio,'bold','CORRELATION MATRIX ')
         CALL mkTableTag(Nio,'w90','@')
         CALL mkCaption(Nio,'CORRELATION MATRIX')
         CALL writTag(Nio,'<tr>')
         CALL mkTableCell(Nio,'@','&nbsp;')
         DO j = 1,nv
          itmp=1
          CALL itoc(j,ctmp,itmp)
          CALL mkHeaderCellScope(Nio,0,0,'col','@',ctmp(1:itmp-1))
         END DO
         CALL writTag(Nio,'</tr>')
        end if
        do j = 1,nv
         do k = 1,j
          c(j,k) = a(j,k) / SQRT(a(j,j)*a(k,k))
         end do
CC
C Changed the computation os SE 02/11/2001
C Restored the old one, to be checked 23-10-2002
CC
         se(j) = SQRT(a(j,j)*sx) / sig(j)
C         se(j) = SQRT(a(j,j)*sx)/a(j,j)
         if ((iprint .gt. 0).and.(out.eq.0)) then
          CALL writTag(Nio,'<tr>')
 7023     format ('<td>',f14.6,'</td>')
 6023     format ('<td class="nowrap">',f14.6,'</td>')
          itmp=1
          CALL itoc(j,ctmp,itmp)
          CALL mkHeaderCellScope(Nio,0,0,'row','@',ctmp(1:itmp-1))
          do k=1,j
           if (c(j,k).lt.0D0)THEN
            write (Nio,6023) c(j,k)
           else
            write (Nio,7023) c(j,k)
           end if
          end do
          if (j.lt.nv) THEN
           do k=j+1,nv
            CALL mkTableCell(Nio,'@','&nbsp;')
           end do
          END IF
          CALL writTag(Nio,'</tr>')
         end if
        end do
        if ((iprint .gt. 0).and.(out.eq.0)) CALL writTag(Nio,'</table>')
       else
C **********************************************************************
        if (ABS(a(1,1)) .lt. 1.0d-13) then
         a(1,1) = 1.0d-6
        end if
C HO AGGIUNTO L'IF PERCHE' DAVA LO ZERODIVIDE
C *******************************************************************
        se(1) = SQRT(sx/a(1,1))
        c(1,1) = 1.d0
       end if
       if ((iprint .gt. 0).and.(out.eq.0)) then
 7024   format (/,'<p><strong>STANDARD ERRORS OF PARAMETERS',
     &            '</strong>',a)
 6024   format (1x,7f14.6)
        write (Nio,7024) Cbr
        write (Nio,6024) (se(j), j = 1,nv)
        CALL writTag(Nio,'</p>')
       end if
C
C SET UP ARRAYS SET,CE.
C   SET(I)=CE(I,J)=CE(J,I)=100 IF PARAMETER I IS FIXED.
       do i = 1,nx
        do j = 1,i
         ce(j,i) = ONEHND
         ce(i,j) = ONEHND
        end do
        set(i) = ONEHND
       end do
       k = 1
       do i = 1,nx
        l = 1
        if (e(i) .le. 0) then
         set(i) = se(k)
         do j = 1,i
          if (e(j) .le. 0) then
           ce(i,j) = c(k,l)
           ce(j,i) = ce(i,j)
           l = l + 1
          end if
         end do
         k = k + 1
        end if
       end do
      else if ((iprint .gt. 0).and.(out.eq.0)) then
       CALL mkPOneLine(Nio,'bold',' EXACT FIT ')
      end if
      end
C
C
C THIS SUBROUTINE CALCULATES THE VALUE OF THE SUM OF SQUARES FUNCTION
C WHICH IS BEING MINIMISED.
C THE ALGORITHIM USED FOR ML IS BASED ON OSBORN
C
C PARAMETERS IN CALLING STATEMENT ARE
C     NX = NO OF TRANSFORMED PARAMETERS
C      X = ARRAY CONTAINING VALUES OF TRANSFORMED PARAMETERS
C      F = VALUE OF SUM OF SQUARES
C      N = NO OF TERMS IN SS
C      A = ESTIMATE OF ERROR TERMS USED IN SS
C
      subroutine CALCFX(nx,x,f,n,a,Ierr,Errext,out,*)
C
C
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..

      INCLUDE 'srslen.prm'
      include 'dimensions.i'
      integer n12,n10,n1
      parameter (n1 = 1, n10 = 10, n12 = 12)
      logical T
      parameter (T = .true.)
      real*8 ZERO,ONE
      parameter (ZERO=0.0d0,ONE=1.0d0)
C
C.. Formal Arguments ..
      integer nx
      real*8 x(*)
      real*8 f
      integer n,out
      real*8 a(mpkp)
      integer Ierr
      character Errext*180
C
C.. Local Scalars ..
      integer i,i1,i2,iflag,iproot,iqroot,iroot,j,j1,k,l,m,nith,np,nq1,
     $        nq2,p1,q1
      real*8 ceps,det,detbnp,e,g,h,small,sum,sum1,sum2,sum3,diffpq
C
C.. Local Arrays ..
      integer ith(maxTH),jcol(3*n10)
      real*8 am(mpkp,2*n12+3*n1),ap(3*n1),aq(3*n1),
     $       b(2*n12+3*n1,2*n12+3*n1),r(3*n1),u(mpkp),v(2*n12+3*n1),
     $       y(4*n10)
C
C.. External Calls ..
      external TRANSC
C   LINES OF CODE ADDED FOR X-13A-S : 2
      logical dpeq
      external dpeq
C   END OF CODE BLOCK
C
C.. Intrinsic Functions ..
      intrinsic ABS
      include 'calc.i'
      include 'calfor.i'
      include 'calshr.i'
      include 'count.i'
      include 'stream.i'
      include 'units.cmn'
C
C.. Save Declarations ..
      save iproot, iqroot, ith, nith, y
C
C ... Executable Statements ...
C
      small = 10.d-10
      ceps = 1.0d-12
C
C NP = NO OF PERIODS WHEN DATA FOR FULL MODEL EXISTS
C
      np = Nw - Pstar
      if (Init .ne. 2) then
       if (Ifn .gt. 0) goto 5000
      end if
      iproot = 0
      iqroot = 0
      do i = 1,nx
       y(i) = x(i) + 0.01
      end do
C
C  INDICES OF NON-ZERO THSTAR TO ITH
C
      if (Q .ne. 0) then
       do i = 1,Q
        ith(i) = i
       end do
      end if
      if (Bq .ne. 0) then
       k = Q
       do i = 1,Bq
        k = k + 1
        j = i * Mq
        ith(k) = j
        if (Q .ne. 0) then
         do l = 1,Q
          k = k + 1
          ith(k) = j + l
         end do
        end if
       end do
      end if
      nith = Q*Bq + Q + Bq
c        write(*,*)'  Q,Bq,nith = ',Q,Bq,nith
C
C     PART 1
C     CALCULATE PHI(I) AND BPHI(I) FROM TRANSFORMED PARAMETERS X(J)
C
 5000 iflag = 0
      if (P .ne. 0) then
C
C CHANGE PHI ONLY IF SOME X(J)'S HAVE CHANGED, IF NOT IFLAG =1
C
       do i = 1,P
        if (.not.dpeq(y(i),x(i))) goto 5001
       end do
       goto 5002
 5001  if (Init .lt. 2) then
*        write(Mtprof,*) ' phi = ',(phi(i),i=1,P)
        call TRANSC(x,0,P,Phi,iproot,ap)
*        if (iproot.gt.0) THEN
*          write(Mtprof,*) ' ap = ',(ap(i),i=1,iproot)
*        ELSE
*          write(Mtprof,*) ' iproot = ',iproot
*        END IF
       end if
       goto 5003
      end if
 5002 iflag = 1
C
 5003 if (Bp .ne. 0) then
       p1 = P + 1
C
C CHANGE BPHI(I) ONLY IF X(J) HAS CHANGED
C
       do i = p1,Pbp
        if (.not.dpeq(y(i),x(i))) goto 5004
       end do
       goto 5005
 5004  if (Init .lt. 2) then
        call TRANSC(x,P,Pbp,Bphi,iroot,r)
       end if
       goto 5006
      end if
 5005 if (iflag .eq. 1) goto 5007
C     IROOT AND V ARE NOT USED
C
C     STEP 1B
C     CALCULATE PHIST = FI * BFI
C
 5006 do i = 1,Pstar
       Phist(i) = ZERO
      end do
      if (P .ne. 0) then
       do i = 1,P
        Phist(i) = Phi(i)
       end do
      end if
      if (Bp .ne. 0) then
       do i = 1,Bp
        j = i * Mq
        Phist(j) = Bphi(i)
        if (P .ne. 0) then
         do k = 1,P
          Phist(k+j) = -Phi(k)*Bphi(i)
         end do
        end if
       end do
      end if
C
C     STEP 2
C     CALCULATE U(I),I=1,NW-PSTAR
C
 5007 do i = 1,np
       sum = Wd(i+Pstar)
       if (Pstar .ne. 0) then
        do j = 1,Pstar
         sum = sum - Phist(j)*Wd(i+Pstar-j)
        end do
       end if
       u(i) = sum
      end do
      detbnp = ONE
c        write(*,*)'  step 2, qstar=',qstar
C
C STEP 3
C
C CALCULATE TH(I) AND BTH(I) FROM TRANSFORMED PARAMETERS X(J)
C
C IF QSTAR=0 NO PAST ESTIMATES OF ERRORS REQUIRED
C
      if (Qstar .ne. 0) then
       iflag = 0
       if (Q .ne. 0) then
        p1 = Pbp + 1
C
C CHANGE TH(I) ONLY IF SOME X(J)'S HAVE CHANGED. IF NOT IFLAG =1
C
        do i = p1,Pq
         if (.not.dpeq(y(i),x(i))) goto 5008
        end do
        goto 5009
 5008   if (Init .lt. 2) then
         call TRANSC(x,Pbp,Pq,Th,iqroot,aq)
        end if
        goto 5010
       end if
 5009  iflag = 1
 5010  if (Bq .ne. 0) then
        p1 = Pq + 1
C
C CHANGE BTH(I) ONLY IF SOME X(J)'S HAVE CHANGED
C
        do i = p1,Bpq
         if (.not.dpeq(y(i),x(i))) goto 5011
        end do
        goto 5012
 5011   if (Init .lt. 2) then
         call TRANSC(x,Pq,Bpq,Bth,iroot,r)
        end if
        goto 5013
       end if
 5012  if (iflag .eq. 1) goto 5014
C     IROOT AND V ARE NOT USED
C
C     STEP 3B
C CALCULATE THSTAR=TH * BTH
C
 5013  do i = 1,Qstar
        Thstar(i) = ZERO
       end do
       if (Q .ne. 0) then
        do i = 1,Q
         Thstar(i) = Th(i)
        end do
       end if
       if (Bq .ne. 0) then
        do i = 1,Bq
         j = i * Mq
         Thstar(j) = Bth(i)
         if (Q .ne. 0) then
          do k = 1,Q
           Thstar(k+j) = -Th(k)*Bth(i)
          end do
         end if
        end do
       end if
* 5014  write(Mtprof,*)' iproot, iqroot, Jfac = ', iproot, iqroot, Jfac
 5014  if (iproot.ne.0 .and. iqroot.ne.0 .and. Jfac.ne.1) then
*        CALL outARMAParam()
        j = 2*nx + 3
        if (Ifn .gt. j) then
         do i = 1,iproot
          do j = 1,iqroot
           diffpq=ABS(ap(i)-aq(j))
*           write(Mtprof,*)' ap(',i,'), aq(',j,'), diffpq = ',
*     &                      ap(i), aq(j), diffpq
           if ((ABS(ap(i)-aq(j)).lt.0.10d0)) THEN
*             call profiler(2,'**GO TO 5015**, line 1201')
             goto 5015
           END IF
          end do
         end do
         goto 5016
 5015    Ifac = 1
         if (Icomm .eq. 0) then
          if ((Ipr .ne. 2).and.(out.eq.0)) then
           CALL wWritln('<abbr title="autoregression">AR</abbr> '//
     &                  'AND <abbr title="moving average">MA</abbr> '//
     &                  'HAVE COMMON FACTORS',Nio,0,T,T)
           RETURN 1
          end if
          Icomm = 1
         end if
        end if
       end if
C       RETURN 1
C
C
C STEP 4
C ESTIMATE CONSTRAINED RESIDUALS
C SET UP FIRST Q* ROWS OF MATRICES
C K=AM AND K'K=B.ZEROISE VECTOR V
C
 5016  do i = 1,Qstar
        a(i) = ZERO
        do j = 1,Qstar
         b(i,j) = ZERO
        end do
        b(i,i) = ONE
        v(i) = ZERO
       end do
       do j = 1,Qstar
        do i = 1,n
         am(i,j) = ZERO
        end do
        am(j,j) = ONE
       end do
C
C FOR CONSTRAINED LEAST SQUARES GO TO STEP 8
C
       if (Type .ne. 1) then
C
C STEP 5
C RECURRENCE FORMULAE FOR CONSTRAINED RESIDUALS
C MATRICES K=AM AND K'K, AND VECTOR V
C
        q1 = Qstar + 1
        do l = q1,n
         sum1 = u(l-Qstar)
c        write(*,*)'  nith = ',nith
         do i = 1,nith
          j = ith(i)
          sum1 = sum1 + Thstar(j)*a(l-j)
         end do
         a(l) = sum1
        end do
C
C FIRST Q COLUMNS OF BLOCKS OF MQ IN MATRIX K (FOR SEASONAL MODELS)
C
        nq2 = Q + 2
        nq1 = Bq + 1
        if (Q .eq. 0) then
         nq1 = Bq
        end if
        do i1 = 1,nq1
         k = (i1-1) * Mq
         if (Q .ne. 0) then
          do i2 = 1,Q
           i = k + i2
           jcol(i) = 0
           do l = q1,n
            sum2 = ZERO
            do j = 1,nith
             j1 = ith(j)
             sum2 = sum2 + Thstar(j1)*am(l-j1,i)
            end do
            if (ABS(sum2) .le. small) then
             sum2 = ZERO
            end if
            am(l,i) = sum2
           end do
          end do
          if (i .eq. Qstar) goto 5018
         end if
C
C (Q+1)TH COLUMN OF EACH BLOCK HAS NON-ZEROS EVERY MQ ROWS
C
         i2 = Q + 1
         i = k + i2
         jcol(i) = 1
         l = q1
         do while (.true.)
          sum2 = ZERO
          do j = 1,Bq
           j1 = j * Mq
           sum2 = sum2 + Thstar(j1)*am(l-j1,i)
          end do
          am(l,i) = sum2
          l = l + Mq
          if (l .gt. n) goto 5017
         end do
C
C REMAINING COLUMNS OF EACH BLOCK OBTAINED BY SHIFTING ELEMENTS OF
C (Q+1)TH COLUMN
C
 5017    do 10 i2 = nq2,Mq
          i = k + i2
          jcol(i) = i2 - Q
          l = q1 + i2 - nq2 + 1
          do while (.true.)
           am(l,i) = am(l-1,i-1)
           l = l + Mq
           if (l .gt. n) goto 10
          end do
 10      continue
        end do
C
C FORM K'K AND VECTOR V
C
 5018   do 20 i = 1,Qstar
         do 15 j = 1,i
          if ((jcol(i)+jcol(j)) .gt. 0) then
           if ((jcol(i)*jcol(j)) .gt. 0) then
            if (jcol(i) .ne. jcol(j)) goto 15
            l = Qstar + jcol(i)
           else
            l = Qstar + jcol(i) + jcol(j)
           end if
           sum3 = b(j,i)
           do while (.true.)
            sum3 = sum3 + am(l,i)*am(l,j)
            l = l + Mq
            if (l .gt. n) goto 5019
           end do
 5019      b(j,i) = sum3
          else
           sum3 = b(j,i)
           do l = q1,n
            sum3 = sum3 + am(l,i)*am(l,j)
           end do
           b(j,i) = sum3
          end if
 15      continue
         if (jcol(i) .gt. 0) then
          l = Qstar + jcol(i)
          do while (.true.)
           v(i) = v(i) + am(l,i)*a(l)
           l = l + Mq
           if (l .gt. n) goto 20
          end do
         else
          do l = q1,n
           v(i) = v(i) + am(l,i)*a(l)
          end do
         end if
 20     continue
C
C     STEP 6
C     COMPUTE INVERSE OF B AND DETB (B IS SYMMETRIC AND SPARSE)
C    B**-1 = (K'K)**-1
C
        if (Qstar .gt. 1) then
         det = ONE
         e = 10.d0**(-Qstar-3)
         do i = 1,Qstar
          det = det * b(i,i)
C   IF DETB ZERO OR NEGATIVE STOP
          if (det .lt. e) then
           if(out.eq.0) then
            CALL wWritln('DETB ZERO OR NEGATIVE',Nio,0,T,T)
           end if
           Ierr = 1
           Errext = 'CALCFX : Error DET(B) Zero or Negative'
           return
          end if
          g = 1 / b(i,i)
          b(i,i) = g
          do j = 1,Qstar
           if (j .lt. i) then
            if (ABS(b(j,i)) .ge. ceps) then
             h = g * b(j,i)
             do k = j,Qstar
              if (k .lt. i) then
               b(j,k) = b(j,k) + h*b(k,i)
              else if (k .ne. i) then
               b(j,k) = b(j,k) - h*b(i,k)
              end if
             end do
             b(j,i) = -h
            end if
           else if (j .ne. i) then
            if (ABS(b(i,j)) .ge. ceps) then
             h = g * b(i,j)
             do k = j,Qstar
              b(j,k) = b(j,k) - h*b(i,k)
             end do
             b(i,j) = h
            end if
           end if
          end do
         end do
         do j = 1,Qstar
          m = j - 1
          do k = 1,m
           b(j,k) = b(k,j)
          end do
         end do
        else
         det = b(1,1)
         b(1,1) = 1 / b(1,1)
        end if
C
C    DETBNP = (DET(K'K))**(1/2(N-P))
C
        detbnp = det**(0.5d0/np)
C
C STEP 7
C COMPUTE ML VALUES OF FIRST QSTAR A(I)
C
        do i = 1,Qstar
         sum = ZERO
         do j = 1,Qstar
          sum = sum - b(i,j)*v(j)
         end do
         a(i) = sum
        end do
       end if
      end if
C
C
C
C     PART 2
C
C
C     STEP 8
C     F0RM A(I), I=QSTAR+1,NW+QSTAR-PSTAR
C THE VALUES OF THE ERRORS WITHIN PERIOD UNDER INVESTIGATION
C
C
C
      q1 = Qstar + 1
      do i = q1,n
       sum = u(i-Qstar)
       if (Qstar .ne. 0) then
        do j = 1,nith
         j1 = ith(j)
         sum = sum + Thstar(j1)*a(i-j1)
        end do
       end if
       a(i) = sum
      end do
C
C     STEP 9
C     CALCULATE FUNCTION TO BE MINIMISED
C
      f = ZERO
      Detpri = detbnp
      do i = 1,n
       a(i) = Detpri * a(i)
       f = f + a(i)*a(i)
      end do
C
C STORE PRESENT VALUES OF TRANSFORMED PARAMETERS FOR TESTING IN
C THE NEXT ROUND
C
      do i = 1,nx
       y(i) = x(i)
      end do
      end
C
C FEASI SUBROUTINE
C CHECKS THAT PARAMETERS IN SEARCH ARE NOT OUT OF
C           BOUNDS.
C INPUT/OUTPUT:  PASSED FROM/RETURNED TO SEARCH.
C THE ARRAY DIMENSIONS MUST CORRESPOND WITH THOSE IN SEARCH.
      subroutine FEASI(nx,x,bx,e,xmin,xmax,red)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
*      integer n10
*      parameter (n10 = 10)
      real*8 ONE
      parameter (ONE=1.0d0)
C
C.. Formal Arguments ..
      integer nx,e(*)
      real*8 x(*),bx(*),xmin(*),xmax(*),red
C
C.. Local Scalars ..
      integer j
      real*8 rj
C
C ... Executable Statements ...
C
      red = ONE
      do 10 j = 1,nx
       if (e(j) .eq. 0) then
        if (x(j) .le. xmax(j)) then
         if (x(j) .ge. xmin(j)) goto 10
         rj = (bx(j)-xmin(j)) / (bx(j)-x(j))
        else
         rj = (xmax(j)-bx(j)) / (x(j)-bx(j))
        end if
        if (rj .lt. red) then
         red = rj
        end if
       end if
 10   continue
      end
C THIS SUBROUTINE CALCULATES VARIANCES OF MODEL PARAMETERS
C
C   X = FINAL VALUES OF TRANSFORMED PARAMETRS
C   A = COVARIANCE MATRIX FOR FINAL TRANSFORMED PARAMETERS
C  PQ = NO OF TRANSFORMED PARAMETERS WITHIN GROUP
C         (E.G. NO OF TRANSFORMED THETA PARAMETERS)
C VFT = STANDARD ERROR OF TRANSFORMED PARAMETERS
C  IB = POSITION OF FIRST TRANSFORMED PARAMETER IN GROUP WITHIN X
C  IE = POSITION OF LAST TRANSFORMED PARAMETER IN GROUP WITHIN X
C
C PROGRAM RETURNS WITH
C   VFT = STANDARD ERRORS OF MODEL PARAMETERS
C
      subroutine VARMP(x,a,pq,vft,ib,ie)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      integer n10
      parameter (n10 = 10)
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Not Written ..
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
C      real*8 x(3)
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 1
      real*8 x(*)
C   END OF CODE BLOCK
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 a(n10,n10)
C.. In/Out Status: Read, Not Written ..
      integer pq
C.. In/Out Status: Maybe Read, Maybe Written ..
      real*8 vft(n10)
C.. In/Out Status: Read, Not Written ..
      integer ib
C.. In/Out Status: Maybe Read, Not Written ..
      integer ie
C
C.. Local Scalars ..
      integer i,im
      real*8 a1,a2,fx1,fx2,fx3,gx1,gx2,gx3,v
C
C.. Intrinsic Functions ..
      intrinsic SQRT,ABS
C
C ... Executable Statements ...
C
C
      if (pq .gt. 1) then
       if (pq .gt. 2) then
        im = ib + 1
        fx1 = (3.d0+x(im)) * (1.d0+x(ie)) * 0.5d0
        gx1 = -fx1
        a1 = (1.d0-x(ie)) * 0.5d0
        a2 = x(ib) * (1.d0+x(ie)) * 0.5d0
        fx2 = a1 + a2
C
        gx2 = a1 - a2
        a1 = -(1.d0+x(im))*0.5d0
        a2 = x(ib) * (3.d0+x(im)) * 0.5d0
        fx3 = a1 + a2
        gx3 = a1 - a2
        v = a(ib,ib)*fx1*fx1 + a(im,im)*fx2*fx2 + a(ie,ie)*fx3*fx3
        vft(1) =
     $    v + (a(im,ib)*fx1*fx2+a(ie,ib)*fx1*fx3+a(ie,im)*fx2*fx3)*2.d0
        v = a(ib,ib)*gx1*gx1 + a(im,im)*gx2*gx2 + a(ie,ie)*gx3*gx3
        vft(2) =
     $    v + (a(im,ib)*gx1*gx2+a(ie,ib)*gx1*gx3+a(ie,im)*gx2*gx3)*2.d0
        vft(3) = a(ie,ie)
       else
        fx1 = 1.d0 - x(ie)
        fx2 = -x(ib)
        vft(1) =
     $    a(ib,ib)*fx1*fx1 + a(ie,ie)*fx2*fx2 + 2.d0*a(ie,ib)*fx1*fx2
        vft(2) = a(ie,ie)
       end if
      else
       vft(1) = a(ib,ib)
      end if
      do i = 1,pq
       vft(i) = SQRT(abs(vft(i)))
      end do
      end
C
C
C
      subroutine RATF(th,q,bphi,p,ps,l,ipr)
C
C THIS SUBROUTINE CALCULATES THE POWER SERIES EXPANSION OF A RATIONAL
C     LAG FUNCTION
C
C
C.. Implicits ..
      implicit none
      real*8 ZERO,ONE
      parameter (ZERO=0.0d0,ONE=1.0d0)
C
C.. Formal Arguments ..
      integer q,p,l,ipr
      real*8 th(*),bphi(*),ps(*)
C
C.. Local Scalars ..
      integer i,iq,j,k
      real*8 sum
C
C.. Intrinsic Functions ..
      intrinsic MIN
C
C ... Executable Statements ...
C
C
      ps(1) = ONE
      if (q .ne. 0) then
       do i = 1,q
        ps(i+1) = -th(i)
       end do
      end if
      iq = q + 2
      if (iq .le. l) then
       do i = iq,l
        ps(i) = ZERO
       end do
      end if
      if (p .eq. 0) return
      do i = 2,l
       sum = ps(i)
       k = MIN(i-1,p)
       do j = 1,k
        if (j .eq. i) goto 5000
        sum = sum + bphi(j)*ps(i-j)
       end do
 5000  ps(i) = sum
      end do
      end
C
C SUBROUTINE TRANSFORMS PARAMETER VALUES WITHIN GROUPS
C
C PHITH = ARRAY OF MODEL PARAMETERS E.G.THETA PARAMETERS
C   PQ = NO OF ELEMENTS IN FITH
C    X = ARRAY OF TRANSFORMED PARAMETERS
C XMIN = MINIMUM BOUNDS FOR X
C XMAX = MAXIMUM BOUNDS FOR X
C   IB = POSITION OF FIRST TRANSFORMED MODEL PARAMETER WITHIN X
C   IE = POSITION OF LAST TRANSFORMED MODEL PARAMETER WITHIN X
C
C
      subroutine TRANS1(phith,pq,x,xmin,xmax,ib,ie,out)
C
C.. Implicits ..
      implicit none
      real*8 ONE
      parameter (ONE=1.0d0)
C
C.. Formal Arguments ..
      integer pq,ib,ie,out
      real*8 phith(*),x(*),xmin(*),xmax(*)
C
C.. Local Scalars ..
      integer i
      real*8 x1,x2
C
C.. Intrinsic Functions ..
      intrinsic ABS, SIGN
      include 'stream.i'
C
C ... Executable Statements ...
C
C
      if (pq .gt. 1) then
       if (pq .gt. 2) then
        if (ABS(phith(3)-ONE) .lt. 1.0d-9) then
         phith(3) = SIGN(0.9999999d0,phith(3))
        end if
        x(ib) = 0.5d0 * ((phith(1)+phith(2))/(ONE-phith(3))+ONE)
        x(ib+1) = (ONE+(phith(1)-phith(2))/(ONE+phith(3)))
        if (ABS(x(ib)+ONE) .lt. 1.0d-9) then
         x(ib) = -.9999999d0
        end if
        x(ib+1) = x(ib+1)/(ONE+x(ib)) - ONE
        x(ie) = phith(3)
       else
        if (ABS(ONE-phith(2)) .lt. 1.0d-9) then
         phith(2) = .9999999d0
        end if
        x(ib) = phith(1) / (ONE-phith(2))
        x(ie) = phith(2)
       end if
      else
       x(ib) = phith(1)
      end if
      do i = ib,ie
       x1 = 0.95*xmin(i) + 0.05*xmax(i)
       if (x(i) .lt. x1) then
        x(i) = x1
        if (out.eq.0) then
 7000    format ('<p><strong>PARAMETER SET AWAY FROM BOUNDARY,',
     &           'I=</strong>',i2,'</p>')
         write (Nio,7000) i
        end if
       end if
       x2 = 0.05*xmin(i) + 0.95*xmax(i)
       if (x(i) .gt. x2) then
        x(i) = x2
       end if
      end do
      end
cc
c
cc      
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
C      subroutine AUTO(n,z,m,r,iq,nw,nx,imean,nfreq,sk,out)
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 1
      subroutine AUTO(n,z,m,r,iq,nw,nx,nfreq,sk,
     $                Qstat,df,se,Ierr,Errext)
C   END OF CODE BLOCK      
C
C      THIS SUBROUTINE CALCULATES THE FIRST M AUTOCORRELATIONS OF THE
C       Z SERIES AND THEIR STANDARD ERRORS.THE LJUNG-BOX Q-VALUE
C       AND THE PIERCE QS VALUE ARE ALSO OUTPUT.
C   INPUT PARAMETERS:
C       N : NUMBER OF OBSERVATIONS OF THE SERIES
C       Z : SERIES
C       M : NUMBER OF AUTOCORRELATIONS
C       R : AUTOCORRELATIONS
C      IQ : NUMBER OF AUTOCORRELATIONS ON WHICH LJUNG-BOX AND PIERCE
C      NW : NUMBER OF OBSERVATIONS OF DIFFERENCED SERIES TO COMPUTE
C           STATISTICS
C      NX : DIMENSION OF THE MODEL
C      SK : 1 MEAN CORRECTION OF THE SERIES, 0 NO MEAN CORRECTION
C
C      THE INPUT IS MEAN CORRECTED AND THEN RESTORED AS IT WAS
c
c       OUTPUT r(1:M) Autocorrelations
c              se(1:M) SE of Autocorrelations
c              Qstat
c              DF: the Qstat is distributed as CHI-Squared of DF (degrees of freedom)
c       OUTPUT IN COMMON: 
c                BJstat1=QSTAT
c                BJSTAT2=DF
c                PSTAT1=QS
c                PSTAT2: degrees of freedom of CHI-SQUARED that behaves like QS.
C
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      integer n10
      parameter (n10 = 10)
      real*8 ZERO,ONE,TWO
      parameter (ZERO=0.0d0,ONE=1.0d0,TWO=2.0d0)
C
C.. Formal Arguments ..
      integer n,m,iq,nw,nx,nfreq,sk
      real*8 z(*),r(*),se(*)
C
C.. Local Scalars ..
      integer df,i,is,j,k,me,ms,mp,mr
      real*8 c0,qs,qstat,rn,sr,zmean
C
C.. Local Arrays ..
      real*8 c(5*n10)
C
C.. Intrinsic Functions ..
      intrinsic MOD, SQRT
      include 'eee.i'
      include 'dets.i'
      include 'calfor.i'
      include 'stream.i'
      include 'htmlout.cmn'
C   LINES OF CODE ADDED FOR X-13A-S : 4
      CHARACTER Errext*(180)
      INTEGER Ierr
      LOGICAL dpeq
      EXTERNAL dpeq
C   END OF CODE BLOCK      
C
C ... Executable Statements ...
C
C
C     MEAN CORRECTION
C
      mp=m/12
      if (MOD(m,12).eq.0) then
        mp=mp*12
      else
        mp=(mp+1)*12
      end if
      do i=m+1,mp
        r(i)=ZERO
        se(i)=ZERO
      enddo
      if (sk .ne. 0) then
       zmean = ZERO
       do i = 1,n
        zmean = zmean + z(i)
       end do
       zmean = zmean / n
       do i = 1,n
        z(i) = z(i) - zmean
       end do
      end if
C
C     CALCULATE AC'S
C
      c0 = ZERO
      do i = 1,n
       c0 = c0 + z(i)**2
      end do
C   LINES OF CODE ADDED FOR X-13A-S : 6
      IF (dpeq(c0,0D0)) THEN
       Errext='AUTO: Cannot generate autocorrelations from a series of z
     &eros.'
       Ierr=1
       RETURN
      END IF
C   END OF CODE BLOCK
      c0 = c0 / n
      do k = 1,m
       c(k) = ZERO
       is = k + 1
       do i = is,n
        c(k) = c(k) + z(i)*z(i-k)
       end do
       c(k) = c(k) / n
       r(k) = c(k) / c0
      end do
      rn = n
      se(1) = ONE / SQRT(rn)
      sr = ZERO
      me = m - 1
      do i = 1,me
       sr = sr + r(i)*r(i)
       se(i+1) = SQRT((ONE+TWO*sr)/n)
      end do
      if ((SeasCheck .gt. 0) .and. (nround .eq. 1)) then
       Acf1 = r(mq)
       Seacf1 = se (mq)
      end if
C
C     IF   IQ=0    THE TESTS ARE NOT COMPUTED
C
      qstat=-1
      if (iq .ne. 0) then
C
C     CALCULATE Q AND QS VALUES FOR TESTING FIT OF MODEL
C
       qstat = ZERO
       do j = 1,iq
        qstat = qstat + r(j)**2/(nw-j)
       end do
       if (SeasCheck .gt. 0) then
        if (nround .eq. 0) then
         Jb0 = qstat
        else
         Jb1 = qstat
        end if
       end if
       qstat = qstat * nw * (nw+2)
cc
c Changed 22-10-2002 removed the mean
cc
c       df = iq - nx - imean
       df = iq - nx
       Bjstat1 = qstat
       Bjstat2 = df
C      MS = IQ / NFREQ
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
C       if (2*nfreq.lt.m .and. nfreq.ne.1) then
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 1
       if (2*nfreq.lt.m .and. 2*nfreq.lt.nw .and. nfreq.ne.1) then
C   END OF CODE BLOCK
        ms = 2
        qs = ZERO
        do j = 1,ms
         k = j * nfreq
         qs = qs + r(k)**2/(nw-k)
        end do
        qs = qs * nw * (nw+2)
        Pstat1 = qs
        Pstat2 = ms
c        if ((out.eq.3) .or. (out.eq.1)) then
c 7003    format (
c     $   ' THE PIERCE QS VALUE IS ',f12.2,' AND IF RESIDUALS ARE',
c     $   ' RANDOM IT SHOULD BE DISTRIBUTED AS CHI-SQUARE (',i2,')')
c         write (Nio,7003) qs, ms
c        end if
       end if
      end if
c     Call OutAuto(OUT,Nio,Icode,Qstat,df,r,se,M)
C
C     THE ORIGINAL INPUT IS RESTORED
C
      if (sk .ne. 0) then
       do i = 1,n
        z(i) = z(i) + zmean
       end do
      end if
      end
C
      subroutine PartAuto(n,m,r,fi,sep,i)
c     n:numero de observaciones usado para calcular las autocorrelaciones(r)
c     m:numero de autocorrelaciones calculadas.
c     r(1:m): las autocorrelaciones de 1 a m
c     fi(1:m): las autocorrelaciones paraciales
c     Sep:error estandar de las autocorrelaciones
C     i= m si todo fue bien;
C          0<i<m si se paro por a(i,i)<0;
C          i=-1 Error aumentar el tamaño de arrays.
      implicit none
      integer maxAutoCorr
      parameter(maxAutoCorr=50)
      real*8 ZERO,ONE
      parameter (ZERO=0.0d0,ONE=1.0d0)
c   INPUT PARAMETERS
      integer m,n
      real*8 r(*)
c   OUTPUT PARAMETERS
      real*8 sep,fi(maxAutoCorr)
      integer i
C   LOCAL PARAMETERS
      integer ma,me,j,je,js,k
      real*8 b,c,a(maxAutoCorr,maxAutoCorr)
c   Intrinsic
      intrinsic REAL,SQRT
C CALCULATION OF PARTIAL CORRELATION
C
C SOLVES YULE-WALKER EQUATIONS
C NOTE DIVISOR IN CALCULATION OF R(K) IS N
C
      if (m.gt.maxAutoCorr) then
        i=-1  !Aumentar el tamaño de los arrays maxAutoCorr
        return
      end if
      ma = m + 1
      me = m - 1
      sep = 1 / SQRT(REAL(n))
      do i = 1,me
       je = m - i
       do j = 1,je
        a(i,(j+i)) = r(j)
       end do
      end do
      do i = 1,m
       a(i,ma) = r(i)
       a(i,i) = ONE
      end do
      do i = 1,m
       b = ONE / a(i,i)
       a(i,i) = b
       js = i + 1
       do j = js,ma
        c = b * a(i,j)
        if (j .ne. ma) then
         do k = j,ma
          a(j,k) = a(j,k) - c*a(i,k)
         end do
        end if
        a(i,j) = c
       end do
       if (a(i,i) .lt. ZERO) return
       fi(i) = a(i,ma)
      end do
      end
C
C
C  THIS SUBROUTINE COMPUTES THE PARTIAL AUTOCORRELATION
C  OF THE SERIES
C    INPUT PARAMETERS
C       N : NUMBER OF OBSERVATION OF THE SERIES
C       M : NUMBER OF AUTOCORRELATION
C       R : AUTOCORRELATION
C     OUT : <> 1 NO PRINT OUT, 1 PRINTOUT
C     withSE: 1:print the associated SE; 0:do not print the SE
C
      subroutine PART(n,m,r,out,fi,sep)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      integer n10
      parameter (n10 = 10)
C
C.. Formal Arguments ..
C.. In/Out Status: INPUT.
      integer n
      integer m
      real*8 r(*)
      integer out
C.. In/Out Status: Output
      real*8 fi(5*n10)
      real*8 sep
cc
      integer ISTRLEN
      external ISTRLEN
C
C.. Local Scalars ..
      integer i,ie,j,k,mp,mr
C
C.. Intrinsic Functions ..
      intrinsic MOD
      include 'stream.i'
*      include 'indhtml.i'
C
C ... Executable Statements ...
C
C CALCULATION OF PARTIAL CORRELATION
C
C SOLVES YULE-WALKER EQUATIONS
C NOTE DIVISOR IN CALCULATION OF R(K) IS N
C
      call PartAuto(n,m,r,fi,sep,i)
      if (i.lt.0) then
       if (out.eq.0) then
         write(nio,'("!!!!Error en PartAuto aumentar maxAutoCorr")')
       end if
       return
      end if
      if (i.lt.m .and. i.gt.0)then
       do j = i,m
        fi(j) = 1000.0d0
      end do
       sep = 1000.0d0
      end if
      return
      end
C
C THIS SUBROUTINE COMPUTES THE FORECAST  OF THE SERIES
C INPUT PARAMETERS:
C      PHIST : NON-SEASONAL AR PART OF THE MODEL B-J SIGN
C     THSTAR : THE FULL MA PART OF THE MODEL B-J SIGN
C     BPHIST : FULL AR PART OF THE MODEL (OUTPUT) TRUE SIGN
C     BPSTAR : THE DIMENSION OF BPHIST
C          Z : SERIES
C         NZ : DIMENSION OF Z
C         WM : MEAN OF DIFFERENCED SERIES
C          A : RESIDUALS
C         NA : DIMENSION OF A
C          L : NUMBER OF FORECAST
C       LSIG : -1 COMPUTE FORECAST BPHIST TO BE COMPUTED,
C              -2 COMPUTE BACKCAST BPHIST ALREADY COMPUTED
C          F : VARIANCE OF RESIDUALS
C        LAM : 0 LOGS, 1 NO LOGS OF DATA
C          D : DELTA OF THE MODEL
C         BD : DELTA*MQ OF THE MODEL
C      IMEAN : 0 NOMEAN, 1 MEAN
C         ZA : AMOUNT OF MEAN ADDED A FUNCTION OF THE MODEL (OUTPUT)
C         LF : MINIMUM NUMBER OF FORECAST
C        OUT : <> 1 NO PRINT OUT, 1 PRINTOUT
C
      subroutine FCAST(phist,thstar,bphist,bpstar,z,nz,wm,a,na,lsig,f,
     $                 lam,d,bd,imean,za,lf,out,bias,forbias,
     $                 noadmiss,alpha)
C
C      THIS SUBROUTINE CALCULATES L FORECASTS FOR THE Z SERIES.
C
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      INCLUDE 'dimensions.i'
      integer n12,n10
      parameter (n10 = 15, n12 = 12)
      real*8 ZERO,ONE
      parameter (ZERO=0.0d0,ONE=1.0d0)
C
C.. Formal Arguments ..
      integer bpstar,nz,na,l,lsig,lam,d,bd,imean,lf,out,bias,noadmiss
      real*8 phist(*),thstar(*),bphist(*),z(*),wm,a(*),f,za,alpha,
     $       forbias(5*n12)
C
C.. Local Scalars ..
      integer i,j,k,kk,lp,maxpq
      real*8 cl,sz,vz,zaa,zexp,zlexp,zuexp
C
C.. Local Arrays ..
      real*8 zl(3*n10),zu(3*n10)
C
C.. External Calls ..
      external RATF
C
C.. Intrinsic Functions ..
      intrinsic EXP, MAX, SQRT
      include 'calfor.i'
      include 'cse.i'
      include 'sesfcast.i'
      include 'stream.i'
      include 'htmlout.cmn'
C
C ... Executable Statements ...
C
C       COMMON /CBLOCK/ TEMP9,ZL,ZU,TEMP10
C
      Ffc = f
C
      if (lsig .ne. -2) then
C
C
C CALCULATE BPHIST COEFFICIENTS WHERE
C             BPHIST=PHIST*(1-B)**D*(1-B**MQ)**BD
C
       bphist(1) = ONE
       if (Pstar .ne. 0) then
        do i = 1,Pstar
         bphist(i+1) = -phist(i)
        end do
       end if
       bpstar = Pstar + 1
       if (d .ne. 0) then
        do i = 1,d
         bphist(bpstar+1) = ZERO
         do j = 1,bpstar
          k = bpstar - j + 2
          bphist(k) = bphist(k) - bphist(k-1)
         end do
         bpstar = bpstar + 1
        end do
       end if
       if (bd .ne. 0) then
        do i = 1,bd
         do j = 1,Mq
          bphist(bpstar+j) = ZERO
         end do
         do j = 1,bpstar
          k = bpstar - j + Mq + 1
          bphist(k) = bphist(k) - bphist(k-Mq)
         end do
         bpstar = bpstar + Mq
        end do
       end if
       bpstar = bpstar - 1
       do i = 1,bpstar
        bphist(i) = -bphist(i+1)
       end do
      end if
      if (lsig .lt. 0) then
C
C  FORECAST PARAMETERS FOR SIGEX
C***********************************************
        maxpq = MAX(bpstar,Qstar)
        l = MAX(maxpq+Qstar,Mq+Mq)
        if ((lsig.lt.0) .and. (l.gt.lf)) then
         lf = MAX(lf,MAX(8,2*Mq))
        end if
        if ((lsig.lt.0) .and. (l.lt.lf)) then
         l = lf
        end if
        if (lsig .eq. -2) goto 5000
      end if
C***********************************************
C
      za = ZERO
      if (imean .ne. 0) then
        if (Pstar .ne. 0) then
         zaa = ONE
         do j = 1,Pstar
          zaa = zaa - phist(j)
         end do
         za = zaa * wm
        else
C
C FIND SUM OF PHIST COEFFICIENTS AND MULTIPLY BY MEAN IF SERIES
C    WAS MEAN CORRECTED
C
         za = wm
        end if
      end if
C
C  FORECAST SERIES
C
 5000 do i = 1,l
       k = na + i
       kk = nz + i
       a(k) = ZERO
       sz = za
       if (Qstar .ne. 0) then
        do j = 1,Qstar
         sz = sz - thstar(j)*a(k-j)
        end do
       end if
       if (bpstar .ne. 0) then
        do j = 1,bpstar
         sz = sz + bphist(j)*z(kk-j)
        end do
       end if
       z(kk) = sz
      end do
C
C COMPUTE THE FORECAST FOR THE BIAS CORRECTION AND ANNUAL AVERAGE
C EVERY TIME COMPUTE 59 FORECASTS (THE MAXIMUM NEEDED)
C
      if ((lam.eq.0) .and. (bias.ne.-300)) then
       do i = 1,59
        k = na + i
        kk = nz + i
        sz = za
        if (Qstar .ne. 0) then
         do j = 1,Qstar
          if ((k-j) .le. na) then
           sz = sz - thstar(j)*a(k-j)
          end if
         end do
        end if
        if (bpstar .ne. 0) then
         do j = 1,bpstar
          if ((kk-j) .le. nz) then
           sz = sz + bphist(j)*z(kk-j)
          else
           sz = sz + bphist(j)*forbias(i-j)
          end if
         end do
        end if
        forbias(i) = sz
       end do
      end if
C     IF (ISFIX.EQ.0) GO TO 61
C ****** TILL 74 DOES NOT DO MUCH *********************
C IT'S THE PART WERE THE SEAS. MEAN WAS READ T Z(I)
C******************************************************
C  COMMENTED BY GIANLUCA
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C      K=NZ/MQ
C      NZOD=NZ-K*MQ
C      NZ1=NZ+1
C      NZL=NZ+L
C      J=NZOD
C      DO 74 I=NZ1,NZL
C      J=J+1
C      Z(I)=Z(I)
C      IF (J.GE.MQ) J=0
C   74 CONTINUE
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C CALCULATE COEFFICIENTS FOR ESTIMATING BOUNDS ON FORECASTS
C
      if ((noadmiss.eq.1) .and. (lsig.eq.-1)) then
       lp = MAX(lf,MAX(8,2*Mq))
       lp = MIN(lp,5*N12-N12/3)
       call RATF(thstar,Qstar,bphist,bpstar,Ps,lp,1)
       vz = ZERO
       do i = 1,lp
        vz = vz + Ps(i)*Ps(i)
        Sesfcast(i) = SQRT(vz*f)
       end do
      end if
      if (lsig .eq. -2) return
      lp = MAX(l,Qstar+1)
      lp = MIN(lp,5*N12-N12/3)
      call RATF(thstar,Qstar,bphist,bpstar,Ps,lp,1)
      if (lsig .lt. 0) return
      if (lam .eq. 0) then
       if (out .eq. 0) then
        CALL mkPOneLine(Nio,'bold','FORECAST OF TRANSFORMED SERIES')
       end if
      else if (out .eq. 0) then
        CALL mkPOneLine(Nio,'bold','FORECAST OF ACTUAL SERIES')
      end if
      if (l .gt. 3*n10) then
       write (Nio,7001) 3*n10, Cbr, 3*n10
 7001  format('<p><strong>NOTE:</strong> ',
     $        'MAXIMUM ALLOWED VALUE FOR L IS ',i2,a,/,
     $         2x,'L CHANGED TO ',i2,'</p>')
       l = 3 * n10
      end if
      if (out .ne. 2) then
       CALL mkTableTag(Nio,'w60','@')
       if (lam .eq. 0) then
        CALL mkCaption(Nio,'FORECAST OF TRANSFORMED SERIES')
       ELSE
        CALL mkCaption(Nio,'FORECAST OF ACTUAL SERIES')
       END IF
       CALL writTag(Nio,'<thead>')
       CALL writTag(Nio,'<tr>')
       WRITE(Nio,7002)
 7002  FORMAT('<th scope="col">LOWER LIMIT</th>',
     &        '<th scope="col">FORECAST</th>',
     &        '<th scope="col">UPPER LIMIT</th>')
       CALL writTag(Nio,'</tr>')
       CALL writTag(Nio,'</thead>')
       CALL writTag(Nio,'<tbody>')
      end if
      vz = ZERO
      do i = 1,l
       vz = vz + Ps(i)*Ps(i)
       cl = vz * f
C      WRITE(*,*)DSQRT(VZ*F)
       cl = alpha * SQRT(cl)
C
C ZL AND ZU ARE THE LOWER AND UPPER BOUNDS ON Z(I)
C
       kk = nz + i
       zl(i) = z(kk) - cl
       zu(i) = z(kk) + cl
       if (out .ne. 2) then
 7003   format ('<tr>',3('<td>',f27.6,'</td>'),'</tr>')
        write (Nio,7003) zl(i), z(kk), zu(i)
       end if
      end do
      if (out .ne. 2) then
       CALL writTag(Nio,'</tbody>')
       CALL writTag(Nio,'</table>')
      end if
C
C CALCULATE EXPONENTIAL OF SERIES IF LOG TRANSFORMED
C
      if (lam .eq. 1) return
      if (out .ne. 2) then
        CALL mkPOneLine(Nio,'bold','FORECAST OF ACTUAL SERIES')
      end if
       if (out .ne. 2) then
        CALL mkTableTag(Nio,'w60','@')
        CALL mkCaption(Nio,'FORECAST OF ACTUAL SERIES')
       end if
       do j = 1,l
        kk = nz + j
        zexp = EXP(z(kk))
        zlexp = EXP(zl(j))
        zuexp = EXP(zu(j))
        if (out .ne. 2) then
         write (Nio,7003) zlexp, zexp, zuexp
        end if
       end do
       if (out .ne. 2) then
        CALL writTag(Nio,'</tbody>')
        CALL writTag(Nio,'</table>')
       end if
      end
C
C  TO TRANSFORM SEARCH PARAMETERS INTO MODEL PARAMETERS
C  INPUT PARAMETERS
C     X : PARAMETERS OF THE MODEL
C     M : M VALUE VALUE IN X TO TRANSFORM
C     N : LAST VALUE IN X TO TRANSFORM
C     C : TRANSFORMED PARAMETERS
C IROOT : NUMBER OF ROOTS OF THE TRANSFORMED PARAMETERS
C  ALPH : ROOTS OF TRANSFORMED PARAMETERS
C
C
C
      subroutine TRANSC(x,m,n,c,iroot,alph)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      integer n1
      parameter (n1 = 1)
      logical T
      parameter (T = .true.)
      real*8 ZERO,ONE,TWO
      parameter (ZERO=0.0d0,ONE=1.0d0,TWO=2.0d0)
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 x(*)
C.. In/Out Status: Read, Not Written ..
      integer m
C.. In/Out Status: Read, Not Written ..
      integer n
C.. In/Out Status: Maybe Read, Maybe Written ..
      real*8 c(3*n1)
C.. In/Out Status: Not Read, Overwritten ..
      integer iroot
C.. In/Out Status: Maybe Read, Maybe Written ..
      real*8 alph(3*n1)
C
C.. Local Scalars ..
      integer i,icount,j
      real*8 d,delta,disc,e,s,y
C
C.. Intrinsic Functions ..
      intrinsic ABS, SQRT
      include 'stream.i'
      include 'units.cmn'
C
C ... Executable Statements ...
C
      if (n.le.0) return
      j = n - m
      iroot = j
      if (j .lt. 2) then
       c(1) = x(n)
       alph(1) = c(1)
*       write(Mtprof,*)' first alph(1) = ', alph(1)
      else if (j .eq. 2) then
       c(1) = x(m+1) * (ONE-x(n))
       c(2) = x(n)
       disc = c(1)**2 + 4.0d0*c(2)
       if (disc .ge. ZERO) then
        disc = SQRT(disc)
        alph(1) = 0.5d0 * (c(1)+disc)
        alph(2) = 0.5d0 * (c(1)-disc)
*        write(Mtprof,*)' alph(1), alph(2) = ', alph(1), alph(2)
       else
        iroot = 0
       end if
      else
       s = (TWO*x(m+1)-ONE) * (ONE-x(n))
       d = (ONE+x(n)) * ((ONE+x(m+1))*(ONE+x(m+2))-ONE)
       c(1) = 0.5d0 * (s+d)
       c(2) = 0.5d0 * (s-d)
       c(3) = x(n)
*       write(Mtprof,*)' s, d, c = ', s, d, c(1), c(2), c(3)
C
C  TO FIND REAL ROOTS OF X**3-C(1)*X**2-C(2)*X-C(3)=0.
C  PUT X=Y+C(1)/3. EQUATION BECOMES Y**3-D*Y-E=0
C  FIND ROOT BY NEWTON-RAPHSON
C
       d = c(1)*c(1)/3.0d0 + c(2)
       e = (TWO*c(1)**3+9.0d0*c(1)*c(2))/27.0d0 + c(3)
       disc = 4.0d0*d**3 - 27.0d0*e**2
*       write(Mtprof,*)' d, e, disc = ', d, e, disc
       if (disc .gt. ZERO) then
        y = -e/d
*        write(Mtprof,*)' y = ', y, '(1)'
       else if (e .gt. ZERO) then
        y = 1 - c(1)/3
*        write(Mtprof,*)' y = ', y, '(2)'
       else
        y = -1 - c(1)/3
*        write(Mtprof,*)' y = ', y, '(3)'
       end if
       icount = 0
       do while (.true.)
        delta = (y**3-d*y-e) / (3.0d0*y*y-d)
        y = y - delta
*        write(Mtprof,*)' icount, delta, y = ', icount, ABS(delta), y
        if (ABS(delta) .le. 0.00005d0) goto 5000
        icount = icount + 1
        if (icount .gt. 10) then
         CALL wWritln('CUBIC ITERATIONS EXCEEDED',Nio,0,T,T)
         goto 5000
        end if
       end do
 5000  alph(1) = y
*       write(Mtprof,*)' alph(1) = ', alph(1)
C
C  TEST IF ALL ROOTS ARE REAL
C
       if (disc .ge. ZERO) then
C
C  ROOTS REAL.DIVIDE BY (Y-ALPH(1))
C  Y**2+ALPH(1)*Y+E/ALPH(1)=0
C
        disc = SQRT(alph(1)**2-4.0d0*e/alph(1))
        alph(2) = 0.5d0 * (-alph(1)+disc)
        alph(3) = 0.5d0 * (-alph(1)-disc)
*        write(Mtprof,*)' disc, alph(2), alph(3) = ',
*     &                 disc, alph(2), alph(3)
       else
        iroot = 1
       end if
       do i = 1,iroot
        alph(i) = alph(i) + c(1)/3.0d0
*        write(Mtprof,*)' alph(',i,') = ', alph(i)
       end do
      end if
      end
C
C THIS SUBROUTINE COMPUTES THE STARTING VALUES OF MODEL PARAMETER
C
C      INPUT PARAMETERS
C       P : DIMENSION OF NON-SEASONAL AR PART
C       Q : DIMENSION OF NON-SEASONAL MA PART
C      BP : DIMENSION OF SEASONAL AR PART
C      BQ : DIMENSION OF SEASONAL MA PART
C     PHI : NON-SEASONAL AR MODEL
C      TH : NON-SEASONAL MA MODEL
C    BPHI : SEASONAL AR MODEL
C     BTH : SEASONAL MA MODEL
C       R : AUTOCORRELATIONS OF DIFFERENCED SERIES
C      MQ : FREQUENCY
C     MQ2 : 2*MQ
C
      subroutine STAVAL(p,q,bp,bq,phi,th,bphi,bth,r,mq,mq2)
C
C.. Implicits ..
      implicit none
      real*8 ZERO,ONE
      parameter (ZERO=0.0d0,ONE=1.0d0)
C
C.. Formal Arguments ..
      integer p,q,bp,bq,mq,mq2
      real*8 phi(*),th(*),bphi(*),bth(*),r(*)
C
C.. Local Scalars ..
      real*8 a,b,b1,b2,b3,c,r0,rmq,rp1,rp2,x
      integer i
C
C.. Intrinsic Functions ..
      intrinsic ABS, SIGN, SQRT
      real*8 Dividecheck
      external Dividecheck
C
C ... Executable Statements ...
C
C
C PROVIDES START VALUES FOR MODEL PARAMETERS
C
      if (p .ne. 0) then
       if (q .eq. 0) then
        b1 = r(q+1)
       else
        b1 = r(q+1) / r(q)
       end if
       if ((mq.eq.12) .or. (mq.eq.0)) then
          b2 = r(q+2) / Dividecheck(r(q+1))
          b3 = r(q+3) / Dividecheck(r(q+2))
       else
          b2 = r(mq+q+2) / Dividecheck(r(mq+q+1))
          b3 = r(mq2+q+2) / Dividecheck(r(mq2+q+1))
       end if
       if ((b1*b2.le.0) .or. (b2*b3.le.0)) then
        phi(1) = 0.2
       else
        phi(1) = (b1+b2+b3) / 3
        if (phi(1) .ge. ONE) then
         phi(1) = 0.9d0
        end if
       end if
       if (p .ge. 2) then
        phi(2) = 0.50d0 * phi(1)
        if (p .eq. 3) then
         phi(3) = 0.5d0 * phi(2)
        else
         do i=3,p
          phi(i)=phi(i-1)*0.5d0
         end do
        end if
       end if
       c = 1 + phi(1)**2
       r0 = c - (2*phi(1)*r(1))
       rp1 = c*r(1) - phi(1)*(1+r(2))
       rp2 = c*r(2) - phi(1)*(r(1)+r(3))
       r(1) = rp1 / Dividecheck(r0)
       r(2) = rp2 / Dividecheck(r0)
       if (bq .ne. 0) then
        rmq = c*r(mq) - phi(1)*(r(mq-1)+r(mq+1))
        r(mq) = rmq / Dividecheck(r0)
       end if
      end if
      if (q .ne. 0) then
       if (q .ne. 1) then
        if (ABS(r(2)) .ge. 0.5d0) then
         r(2) = SIGN(0.45d0,r(2))
        end if
        b = 1 + 2*r(2)
        if (r(2) .gt. 0.16666d0) then
         a = r(2) * (1-2*r(2))
         if (r(1)**2 .ge. 4*a) then
          r(1) = SIGN(1.8d0*SQRT(a),r(1))
         end if
        else if (ABS(r(1)) .ge. 0.5d0*b) then
         r(1) = SIGN(b*0.45d0,r(1))
        end if
        x = (r(2)+r(2)-1-SQRT(b**2-4*r(1)**2)) / Dividecheck(2*r(2))
        th(2) = 0.5d0 * x * (1-SQRT(1-4/Dividecheck(x**2)))
        th(1) = r(1) * th(2) / Dividecheck(r(2)*(1-th(2)))
        if (q .eq. 3) then
         th(3) = ZERO
        end if
       else if (ABS(r(1)) .lt. 0.5d0) then
        th(1) = (SQRT(1-4*r(1)**2)-1) / Dividecheck(2*r(1))
       else
        th(1) = -SIGN(0.9d0,r(1))
       end if
      end if
      if (bq .ne. 0) then
       if (ABS(r(mq)) .lt. 0.5d0) then
        bth(1) = (SQRT(1-4*r(mq)**2)-1) / Dividecheck(2*r(mq))
C  START VALUE IS WEIGHTED AVERAGE OF ESTIMATE AND 0.83
        bth(1) = 0.50d0 + 0.40d0*bth(1)
       else
        bth(1) = -SIGN(0.9d0,r(mq))
       end if
       if (bq .eq. 2) then
        bth(2) = ZERO
       end if
      end if
      if (bp .ne. 0) then
       bphi(1) = r(mq2) / Dividecheck(r(mq))
       if (bphi(1) .ge. 0.8d0) then
        bphi(1) = 0.8d0
       end if
       if (bphi(1) .le. ZERO) then
        bphi(1) = ZERO
       end if
       if (bp .ne. 1) then
        bphi(2) = 0.25d0
       end if
      end if
      end
C
C FUNCTION TO COMPUTE THE VARIANCE
C
      double precision function DVAR(n,x)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
C.. In/Out Status: Read, Not Written ..
      integer n
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 x(*)
C
C.. Local Scalars ..
      integer i
      real*8 dmean2,ym2
C
C.. External Functions ..
      real*8 DMEAN
      external DMEAN
C
C ... Executable Statements ...
C
      dmean2 = (DMEAN(n,x)**2)
      ym2 = 0.d0
      do i = 1,n
       ym2 = ym2 + x(i)**2
      end do
      ym2 = ym2 / n
      DVAR = ym2 - dmean2
      IF (DVAR.lt.0D0) DVAR = 0D0
      end
C
C COMPUTE THE VARIANCE OF X SERIES
C
      double precision function DVARMS(n,x)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
C.. Formal Arguments ..
C.. In/Out Status: Read, Not Written ..
      integer n
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 x(mpkp)
C
C.. Local Scalars ..
      integer i
      real*8 dmean2,ym2
C
C.. External Functions ..
      real*8 DMEAN
      external DMEAN
C
C ... Executable Statements ...
C
      dmean2 = DMEAN(n,x)
      ym2 = ZERO
      do i = 1,n
       ym2 = ym2 + (x(i)-dmean2)**2
      end do
      ym2 = ym2 / n
      DVARMS = ym2
      end
C
C THIS  SUBROUTINE COMPUTES TEST OF RUNS ON AUTOCORRELATIONS / RESIDUALS
C
C     INPUT PARAMETERS
C      X : RESIDUALS OR AUTOCORRELATIONS
C      N : DIMENSION OF X
C   XMED : MEAN OF X
C  ITEST : <> 0 APPROXIMATE TEST IS COMPUTED
C
      subroutine RACES(x,n,xmed,itest,tval,n1,n0)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 x(*)
C.. In/Out Status: Read, Not Written ..
      integer n
C.. In/Out Status: Read, Not Written ..
      real*8 xmed
C.. In/Out Status: Read, Not Written ..
      integer itest
C.. In/Out Status: Not Read, Maybe Written ..
      real*8 tval
c   OUTPUT PARAMETERS
      integer n0,n1
C
C.. Local Scalars ..
      integer i,l,ll,nr
      real*8 runm,runstd,xn0,xn1
C
C.. Intrinsic Functions ..
      intrinsic DBLE, SQRT
      include 'stream.i'
C
C ... Executable Statements ...
C
      nr = 1
      l = 0
      if (x(1) .ge. xmed) then
       l = 1
      end if
      n1 = l
      n0 = 1 - l
      do i = 2,n
       ll = 0
       if (x(i) .ge. xmed) then
        ll = 1
       end if
       n1 = n1 + ll
       n0 = n0 + 1 - ll
       if (ll .ne. l) then
        l = ll
        nr = nr + 1
       end if
      end do
      if (itest .ne. 0) then
       xn0 = DBLE(n0)
       xn1 = DBLE(n1)
       runm = 1 + 2*xn1*xn0/(xn1+xn0)
       runstd = 2 * xn1 * xn0 * (2*xn1*xn0-xn1-xn0)
       runstd = runstd / (((xn1+xn0)**2)*(xn1+xn0-1))
       runstd = SQRT(runstd)
       tval = (nr-runm) / runstd
      end if
      end
C
C  THIS FUNCTION COMPUTES THE MEDIAN OF X SERIES
C
      double precision function DMED(x,n)
C
C.. Implicits ..
      implicit none
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 x(*)
C.. In/Out Status: Read, Not Written ..
      integer n
C
C.. Local Scalars ..
      integer i,ipippo,j
      real*8 dmed1,sum,sumabs
C
C.. Intrinsic Functions ..
      intrinsic ABS
C
C ... Executable Statements ...
C
C
      ipippo = 0
      dmed1 = 0.d0
      sumabs = 1.0d12
      do while (.true.)
       do i = 1,n
        sum = ZERO
        do j = 1,n
         sum = sum + ABS(x(j)-x(i))
        end do
        if (sum.le.sumabs .and. ABS(x(i)-dmed1).ge.1.0d-12) then
         sumabs = sum
         DMED = x(i)
        end if
       end do
       if (ipippo .ne. 0) goto 5000
       if (2*(n/2) .ne. n) return
       ipippo = 1
       dmed1 = DMED
      end do
 5000 DMED = DMED/2 + dmed1/2
      end
C
C   THIS FUNCTION COMPUTES THE S.E. OF THE MEAN
C
C      INPUT PARAMETER
C       VM : VARIANCE OF DIFFERENCED SERIES
C       WM : MEAN         "   "     "    "
C       NW : NUMBER OBSERVATIONS OF DIFFERENCED SERIES
C      PHI : NON-SEASONAL AR PART OF THE MODEL B-J SIGN
C        P : DIMENSION OF PHI
C     BPHI : SEASONAL AR PART OF THE MODEL B-J SIGN
C       BP : DIMENSION OF BPHI
C       TH : NON-SEASONAL MA PART OF THE MODEL B-J SIGN
C        Q : DIMENSION OF TH
C      BTH : SEASONAL MA PART OF THE MODEL B-J SIGN
C       BQ : DIMENSION OF BTH
C       MQ : FREQUENCY
C       SE : STANDARD ERROR OF THE MEAN
C
      subroutine CHECK(vm,wm,nw,phi,p,bphi,bp,th,q,bth,bq,mq,se)
C
C.. Implicits ..
      implicit none
      include 'units.cmn'
C
C.. Parameters ..
      integer n12,n10,n1
*      parameter (n1 = 10, n10 = 10, n12 = 12)
      parameter (n1 = 1, n10 = 10, n12 = 12)
      real*8 ZERO,ONE
      parameter (ZERO=0.0d0,ONE=1.0d0)
C
C.. Formal Arguments ..
      integer nw,p,bp,q,bq,mq
      double precision vm,wm,phi(3*n1),bphi(3*n1),th(3*n1),bth(3*n1),se
C
C.. Local Scalars ..
      integer i,lll,lll1,m,mm,nbphi,nbth,nphi,nth
      double precision vc,vz,xx
C
C.. Local Arrays ..
      double precision bphi1(5*n10),bth1(5*n10),g(0:5*n10),gam(0:3*n12),
     $                 phi1(4*n1),rho(0:3*n12),th1(4*n1)
C
C.. External Calls ..
      external BFAC, CONV
C
C.. Intrinsic Functions ..
      intrinsic SQRT
C
C ... Executable Statements ...
C
*      call profiler(2,'subroutine CHECK')
      m = 24
      phi1(1) = ONE
      do i = 1,p
       phi1(i+1) = -phi(i)
*       write(Mtprof,*) 'phi1(',i+1,') = ', phi1(i+1)
      end do
      bphi1(1) = ONE
      if (bp .gt. 0) then
       do i = 1,bp*mq
        bphi1(i+1) = ZERO
       end do
       do i = 1,bp
        bphi1(i*mq+1) = -bphi(i)
*        write(Mtprof,*) 'bphi1(',i*mq+1,') = ', bphi1(i*mq+1)
       end do
      end if
      nphi = p + 1
      nbphi = 1 + bp*mq
*      call profiler(2,'subroutine CONV')
      call CONV(phi1,nphi,bphi1,nbphi,bphi1,lll)
      if (lll .gt. 1) then
       do i = 2,lll
        bphi1(i-1) = -bphi1(i)
*        write(Mtprof,*) 'bphi1(',i-1,') = ', bphi1(i-1)
       end do
      end if
      lll = lll - 1
      th1(1) = ONE
      do i = 1,q
       th1(i+1) = -th(i)
*       write(Mtprof,*) 'th1(',i+1,') = ', th1(i+1)
      end do
      bth1(1) = ONE
      if (bq .gt. 0) then
       do i = 1,bq*mq
        bth1(i+1) = ZERO
       end do
       do i = 1,q
        bth1(i*mq+1) = -bth(i)
*        write(Mtprof,*) 'bth1(',i*mq+1,') = ', bth1(i*mq+1)
       end do
      end if
      nth = q + 1
      nbth = 1 + bq*mq
*      call profiler(2,'subroutine CONV')
      call CONV(th1,nth,bth1,nbth,bth1,lll1)
      do i = 2,lll1
       bth1(i-1) = -bth1(i)
*       write(Mtprof,*) 'bth1(',i-1,') = ', bth1(i-1)
      end do
      lll1 = lll1 - 1
      mm = m
      vz = ONE
c      WRITE(Mtprof,*)'  subroutine check, call 1, lll =',lll
      call BFAC(bphi1,bth1,lll,lll1,m,gam,rho,vc,vz,g,m)
      xx = ZERO
      do i = 1,mm
       xx = xx + (1-(i*ONE)/(nw*ONE))*rho(i)
*       write(Mtprof,*) 'rho(i), xx = ', rho(i), xx
      end do
      xx = 1 + 2*xx
*      write(Mtprof,*) 'xx = ', xx
      xx = (vm/nw) * xx
*      write(Mtprof,*) 'vm, nw, xx = ', vm, nw, xx
      if (xx .gt. ZERO) then
       se = SQRT(xx)
      end if
      end
C
C
C   THIS FUNCTION COMPUTES THE MINIMUM BTHETA SUCH THAT GIVEN THETA
C   THE MODEL HAS A VALID DECOMPOSITION FOR MONTHLY AIRLINE MODEL,
C   QUARTERLY AIRLINE MODEL, AND MONTHLY AND QUARTERLY AIRLINE MODEL
C   WITH D^2
C
C   INPUT PARAMETER
C    THETA : THE VALUE OF THETA B-J SIGN
C       MQ : FREQUENCY
C        D : DELTA OF THE MODEL
C
      real*8 function POLYVAL(theta,mq,d)
C
C.. Implicits ..
      implicit none
      real*8 ZERO,ONEHND
      parameter (ZERO=0.0d0,ONEHND=100.0d0)
C
C.. Formal Arguments ..
C.. In/Out Status: Maybe Read, Not Written ..
      real*8 theta
C.. In/Out Status: Read, Not Written ..
      integer mq
C.. In/Out Status: Read, Not Written ..
      integer d
C
C.. Local Scalars ..
      integer i,nairm,nairq,nd2m,nd2q
C
C.. Local Arrays ..
      real*8 airm(37),airq(36),d2m(28),d2q(25)
C
C.. Intrinsic Functions ..
      intrinsic ABS, ANINT, SIGN
C
C.. Data Declarations ..
      data airq/
     $     -1.50568861051561d+07,-4.07864271885349d+06,
     $     1.25495713468045d+08,3.72893488076512d+07,
     $     -4.79316449488926d+08,-1.55227766622531d+08,
     $     1.11199260604581d+09,3.89926250516196d+08,
     $     -1.75105862443012d+09,-6.60203090340804d+08,
     $     1.98087443551278d+09,7.96938928504665d+08,
     $     -1.66146607023159d+09,-7.07321965792925d+08,
     $     1.05100610295520d+09,4.69148293267340d+08,
     $     -5.04888372223982d+08,-2.33997977625452d+08,
     $     1.83900005449306d+08,8.76043491537447d+07,
     $     -5.02818844050958d+07,-2.43866695225120d+07,
     $     1.01154607380367d+07,4.95920421687316d+06,
     $     -1.44719924386675d+06,-7.16551068193267d+05,
     $     1.38950643779603d+05,7.05870266796903d+04,
     $     -7.97845888823479d+03,-4.45727946957010d+03,
     $     1.83672933301260d+02,1.61704844615717d+02,
     $     6.28102914263037d+00,-1.85013363024192d+00,
     $     -4.67101412373999d-01,-3.14388900223822d-01/
      data airm/
     $     1.95347563065735d+07,1.40869407022632d+07,
     $     -1.82924843962725d+08,-1.24837916382162d+08,
     $     7.84012309810082d+08,5.06656270824311d+08,
     $     -2.03824923561787d+09,-1.24723236243656d+09,
     $     3.59083883274152d+09,2.07891569193133d+09,
     $     -4.53585473370324d+09,-2.48015536619207d+09,
     $     4.23918446756410d+09,2.18248985662395d+09,
     $     -2.98178573585705d+09,-1.43853103116210d+09,
     $     1.59018446079529d+09,7.13771251305322d+08,
     $     -6.42824470238530d+08,-2.65666810599684d+08,
     $     1.95564418343889d+08,7.33014344799244d+07,
     $     -4.41316525032554d+07,-1.46797381353294d+07,
     $     7.21747845147075d+06,2.06510268195992d+06,
     $     -8.25912908016856d+05,-1.94312700336529d+05,
     $     6.26921279186573d+04,1.13487961429655d+04,
     $     -2.90128263818433d+03,-3.64564339836437d+02,
     $     7.07021116258866d+01,5.00986705245536d+00,
     $     -8.06732206713255d-01,-1.29571848439844d-01,
     $     -1.50316939442371d-01/
      data d2q/
     $     -7.36886387334525d+03,5.71043080817024d+03,
     $     4.89655041459870d+04,-2.72040336684033d+04,
     $     -1.40067644904055d+05,5.38068353807028d+04,
     $     2.26611284963096d+05,-5.62886010543245d+04,
     $     -2.28695251634143d+05,3.21568573231266d+04,
     $     1.49568754383030d+05,-8.38954361805799d+03,
     $     -6.37582467027026d+04,-4.93256763478040d+02,
     $     1.73902925238205d+04,9.03967061027686d+02,
     $     -2.91111942281865d+03,-2.25701468391660d+02,
     $     2.77900060158067d+02,2.36818663168160d+01,
     $     -1.32508103079524d+01,-9.99194312958687d-01,
     $     2.39589706532146d-01,1.13994409953707d-02,
     $     -6.95571335187068d-04/
      data d2m/
     $     -6.16086458289453d+05,-1.93205965815733d+06,
     $     1.88840463291657d+06,9.78701910995365d+06,
     $     -1.11444188423980d+06,-2.21556751224427d+07,
     $     -3.17747695176425d+06,2.95610373631368d+07,
     $     7.13230914290129d+06,-2.57929135874340d+07,
     $     -6.93754106056724d+06,1.54167965673704d+07,
     $     3.96437423488546d+06,-6.41657328297885d+06,
     $     -1.41686692105580d+06,1.84861784825349d+06,
     $     3.14705412928108d+05,-3.58340765118489d+05,
     $     -4.10545347750240d+04,4.41702532829458d+04,
     $     2.79277887146530d+03,-3.15413411558056d+03,
     $     -7.90998715967224d+01,1.14532961893102d+02,
     $     -6.90866544602480d-01,-9.20391422924575d-01,
     $     -5.95788710264705d-01,-6.82655329201659d-01/
C
C ... Executable Statements ...
C
      POLYVAL = ZERO
      nairq = 36
      nairm = 37
      nd2q = 25
      nd2m = 28
      if (d .eq. 2) then
       if (mq .eq. 4) then
        do i = 1,nd2q
         POLYVAL = POLYVAL + d2q(i)*(-theta)**(nd2q-i)
        end do
        POLYVAL = -POLYVAL + .075d0
        if (ABS(POLYVAL) .gt. .98d0) then
         POLYVAL = SIGN(.98d0,POLYVAL)
        end if
       end if
C      POLYVAL=(DNINT(POLYVAL*ONEHND+1)/ONEHND)
       if (mq .eq. 12) then
        do i = 1,nd2m
         POLYVAL = POLYVAL + d2m(i)*(-theta)**(nd2m-i)
        end do
        POLYVAL = -POLYVAL + .075d0
        if (ABS(POLYVAL) .gt. .98d0) then
         POLYVAL = SIGN(.98d0,POLYVAL)
        end if
       end if
      else
       if (mq .eq. 4) then
        do i = 1,nairq
         POLYVAL = POLYVAL + airq(i)*theta**(nairq-i)
        end do
       end if
       if (mq .eq. 12) then
        do i = 1,nairm
         POLYVAL = POLYVAL + airm(i)*theta**(nairm-i)
        end do
       end if
       POLYVAL = POLYVAL + .01d0
       POLYVAL = (ANINT(POLYVAL*ONEHND+1)/ONEHND)
      end if
      end
C
C
C
      subroutine CHMODEL(x,se,nx,p,q,bp,bq,d,bd,w,nw,wm,vm,mq,ur,xl,phi,
     $                   tst,imean,seas,pbp,pq,bpq,pstar,z,nz,out,*)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
      integer n10,n1
      parameter (n1 = 1, n10 = 10)
      logical T,F
      parameter (T = .true., F = .false.)
      real*8 ZERO
      parameter (ZERO=0.0d0)
C
C.. Formal Arguments ..
      integer nx,p,q,bp,bq,d,bd,nw,mq,tst,imean,seas,pbp,pq,bpq,pstar,
     $        nz,out
      real*8 x(n10),se(n10),w(mpkp),wm,vm,ur,xl,phi(3*n1),z(mpkp)
C
C.. Local Scalars ..
      integer i,iproot,j,nbp,nchanged,np
      logical ltest
C
C.. Local Arrays ..
      real*8 ap(3*n1)
C
C.. External Calls ..
      external TRANSC
C
C.. Intrinsic Functions ..
      intrinsic ABS, DBLE
      include 'stream.i'
      include 'htmlout.cmn'
C
C ... Executable Statements ...
C
      nchanged = 0
C
C..   Modified by REG on 12/23/2005 to access X in the next statement
C     only after if conditions are met.
      if ((seas.eq.0) .and. (bq.eq.1) .and. (bd.gt.0)) then
       if (ABS(x(p+bp+q+bq)-xl).lt.1.0d-12) then
C
C PER FARE QUESTO CONTROLLO MOLTO PROBABILMENETE BISOGNA
C CALCOLARE LE RADICI DI PHI(P) E CONTROLLARE SE SONO RADICI
C STAGIONALI (COME NELLA SUBROUTINE FIRST)
C
C CHIEDERE AD AGUSTIN SE NE VALE LA PENA
C
C          NTST=0
C          DO 80 I=1,P
C 80      IF (DABS(X(I)).EQ.UR) NTST=1
C         IF (NTST.EQ.0) THEN
C            WRITE(NIO,'(//,8X,A,/,8X,A)')'THE SERIES DOES NOT CONTAIN',
C     &                              'SIGNIFICANT SEASONALITY'
C         ELSE
C            WRITE(NIO,'(//,8X,A,/,8X,A)')'THE SERIES ONLY CONTAINS',
C     &                              'SOME STATIONARY SEASONALITY'
C         end if
       bd = bd - 1
       bq = bq - 1
       imean = 1
       if (out.eq.0) then
        CALL wWritln(' ',Nio,0,T,F)
        write (Nio,1000)'BD', bd, Cbr
        write (Nio,1000)'BQ', bq, '</p>'
 1000   FORMAT(a,' CHANGED TO ',I1,a)
        CALL mkPOneLine(Nio,'@','THE MODEL IS CHANGED AND RE-ESTIMATED')
       end if
       nw = nz
       do i = 1,nz
        w(i) = z(i)
       end do
       do i = 1,d
        nw = nw - 1
        do j = 1,nw
         w(j) = w(j+1) - w(j)
        end do
       end do
       wm = ZERO
       do i = 1,nw
        wm = wm + w(i)
       end do
       wm = wm / DBLE(nw)
       do i = 1,nw
        w(i) = w(i) - wm
       end do
       pbp = p + bp
       pq = pbp + q
       bpq = p + q + bp + bq
       pstar = p + bp*mq
       return 1
       end if
      else
C
C..   Modified by REG on 12/23/2005 to access X in the next statement
C     only after if conditions are met.
       if ((seas.eq.0) .and. (bp.eq.1) .and. (bd.eq.0)) then
        if (ABS(x(p+bp)-ur).lt.1.0d-12) then
C
C PER FARE QUESTO CONTROLLO MOLTO PROBABILMENETE BISOGNA
C CALCOLARE LE RADICI DI PHI(P) E CONTROLLARE SE SONO RADICI
C STAGIONALI (COME NELLA SUBROUTINE FIRST)
C
C CHIEDERE AD AGUSTIN SE NE VALE LA PENA
C
C          NTST=0
C          DO 180 I=1,P
C 180     IF (DABS(X(I)).EQ.UR) NTST=1
C         IF (NTST.EQ.0) THEN
C            WRITE(NIO,'(//,8X,A,/,8X,A)')'THE SERIES DOES NOT CONTAIN',
C     &                              'SIGNIFICANT SEASONALITY'
C         ELSE
C            WRITE(NIO,'(//,8X,A,/,8X,A)')'THE SERIES ONLY CONTAINS',
C     &                              'SOME STATIONARY SEASONALITY'
C         end if
        bp = bp - 1
        bq = bq - 1
        imean = 1
        if (out.eq.0) then
         CALL wWritln(' ',Nio,0,T,F)
         write (Nio,1000)'BP', bp, Cbr
         write (Nio,1000)'BQ', bq, '</p>'
         CALL mkPOneLine(Nio,'@',
     &                   'THE MODEL IS CHANGED AND RE-ESTIMATED')
        end if
        pbp = p + bp
        pq = pbp + q
        bpq = p + q + bp + bq
        pstar = p + bp*mq
        return 1
        end if
       end if
       ltest=.false.
       if ((p+bp) .gt. 0) then
        ltest = ABS(x(p+bp)-ur).le.1.0d-8
       end if 
       if ((seas.ne.0.or.bp.ne.1.or.bd.ne.0.or.ltest) .and. seas.ne.1)
     &    return
       np = p
       nbp = bp
       if (p .gt. 0) then
        i = 1
        do while (.true.)
         if ((ABS(x(i)-ur).lt.1.0d-12) .and. (d.lt.2)) then
          p = p - 1
          d = d + 1
          if (out.eq.0) then
           CALL wWritln(' ',Nio,0,T,F)
           write (Nio,1000)'P', p, Cbr
           write (Nio,1000)'D', d, '</p>'
           CALL mkPOneLine(Nio,'@',
     &                     'THE MODEL IS CHANGED AND RE-ESTIMATED')
          end if
          nchanged = 1
          nw = nw - 1
          do j = 1,nw
           w(j) = w(j+1) - w(j)
          end do
          do j = nx,i+1,-1
           se(j-1) = se(j)
           x(j-1) = x(j)
          end do
          nx = nx - 1
          tst = tst - 1
         end if
         i = i + 1
         if (i .gt. p) goto 5000
        end do
       end if
C
 5000  if (bp .gt. 0) then
        do i = p+1,p+bp
         if ((ABS(x(i)-ur).lt.1.0d-10) .and. (bd.lt.1)) then
          bp = bp - 1
          bd = bd + 1
          if (out.eq.0) then
           CALL wWritln(' ',Nio,0,T,F)
           write (Nio,1000)'BP', bp, Cbr
           write (Nio,1000)'BD', bd, '</p>'
           CALL mkPOneLine(Nio,'@',
     &                     'THE MODEL IS CHANGED AND RE-ESTIMATED')
          end if
          nchanged = 1
          nw = nw - mq
          do j = 1,nw
           w(j) = w(j+mq) - w(j)
          end do
          do j = i,nx
           se(j) = se(j+1)
           x(j) = x(j+1)
          end do
          nx = nx - 1
          tst = tst - 1
         end if
        end do
       end if
       if ((np.ne.p) .or. (nbp.ne.bp)) then
        wm = ZERO
        do i = 1,nw
         wm = wm + w(i)
        end do
        wm = wm / DBLE(nw)
        vm = ZERO
        do i = 1,nw
         vm = vm + w(i)*w(i)
        end do
        vm = vm / nw
        call TRANSC(x,0,p,phi,iproot,ap)
       end if
       if (nchanged .eq. 1) then
        return 1
       end if
      end if
      end
C
C
      subroutine TRANS1I2(p,x,ib,ie,xl,ur)
C
C.. Implicits ..
      implicit none
      real*8 ONE
      parameter (ONE=1.0d0)
C
C.. Formal Arguments ..
      integer ib,ie
      real*8 p(*),x(*),xl,ur
C
C.. Local Scalars ..
      integer i,j,npq
      real*8 xmax,xmin,xtest
C
C.. Local Arrays ..
      real*8 phith(3)
C
C.. Intrinsic Functions ..
      intrinsic ABS, SIGN
      xmin = -xl
      xmax = xl
      npq = ie - ib + 1
      do i = 1,npq
       phith(i) = -p(i)
      end do
      if (npq .le. 1) then
       x(ib) = phith(1)
      else if (npq .le. 2) then
       if (abs(ONE-phith(2)) .lt. 1.0d-9) then
        phith(2) = ur
       end if
       x(ib) = phith(1) / (ONE-phith(2))
       x(ie) = phith(2)
      else
       if (ABS(phith(3)-ONE) .lt. 1.0d-9) then
        phith(3) = SIGN(ur,phith(3))
       end if
       x(ib) = 0.5d0 * ((phith(1)+phith(2))/(ONE-phith(3))+ONE)
       x(ib+1) = (ONE+(phith(1)-phith(2))/(ONE+phith(3)))
       if (abs(x(ib)+ONE) .lt. 1.0d-9) then
        x(ib) = -ur
       end if
       x(ib+1) = x(ib+1)/(ONE+x(ib)) - ONE
       x(ie) = phith(3)
      end if
      do j = ib,ie
       xtest = (x(j)-xmin) / (xmax-xmin)
       if (xtest .lt. 0.01d0) then
        x(j) = -ur
       end if
       if (xtest .gt. xmax) then
        x(j) = ur
       end if
      end do
      end
C
C
      subroutine TRANSCI2(p,x,m,n)
C
C.. Implicits ..
      implicit none
      LOGICAL T
      PARAMETER (T = .true.)
      real*8 ZERO,ONE,TWO
      parameter (ZERO=0.0d0,ONE=1.0d0,TWO=2.0d0)
C
C.. Formal Arguments ..
      integer m,n
      real*8 p(*),x(*)
C
C.. Local Scalars ..
      integer i,icount,iroot,j
      real*8 d,delta,disc,e,s,y
c
c.. local arrays ..
      real*8 alph(3),c(3)
c
c.. intrinsic functions ..
      intrinsic ABS, SQRT
c
c.. Common Variables
      include 'stream.i'
c
c  to transform search parameters into model  parameters
c
      j = n - m
      iroot = j
      if (j .lt. 2) then
       c(1) = x(n)
       alph(1) = c(1)
      else if (j .eq. 2) then
       c(1) = x(m+1) * (ONE-x(n))
       c(2) = x(n)
       disc = c(1)**2 + 4.0d0*c(2)
       if (disc .ge. ZERO) then
        disc = SQRT(disc)
        alph(1) = 0.5d0 * (c(1)+disc)
        alph(2) = 0.5d0 * (c(1)-disc)
       else
        iroot = 0
       end if
      else
       s = (TWO*x(m+1)-ONE) * (ONE-x(n))
       d = (ONE+x(n)) * ((ONE+x(m+1))*(ONE+x(m+2))-ONE)
       c(1) = 0.5d0 * (s+d)
       c(2) = 0.5d0 * (s-d)
       c(3) = x(n)
c
c  to find real roots of x**3-c(1)*x**2-c(2)*x-c(3)=0.
c  put x=y+c(1)/3. equation becomes y**3-d*y-e=0
c  find root by newton-raphson
c
       d = c(1)*c(1)/3.0d0 + c(2)
       e = (TWO*c(1)**3+9.0d0*c(1)*c(2))/27.0d0 + c(3)
       disc = 4.0d0*d**3 - 27.0d0*e**2
       if (disc .gt. ZERO) then
        y = -e/d
       else if (e .gt. ZERO) then
        y = 1 - c(1)/3
       else
        y = -1 - c(1)/3
       end if
       icount = 0
       do while (.true.)
        delta = (y**3-d*y-e) / (3.0d0*y*y-d)
        y = y - delta
        if (ABS(delta) .le. 0.00005d0) goto 1000
        icount = icount + 1
        if (icount .gt. 10) then
         CALL wWritln('CUBIC ITERATIONS EXCEEDED',Nio,0,T,T)
         goto 1000
        end if
       end do
       goto 1005
 1000  alph(1) = y
c
c  test if all roots are real
c
       if (disc .ge. ZERO) then
c
c  roots real.divide by (y-alph(1))
c  y**2+alph(1)*y+e/alph(1)=0
c
        disc = sqrt(alph(1)**2-4.0d0*e/alph(1))
        alph(2) = 0.5d0 * (-alph(1)+disc)
        alph(3) = 0.5d0 * (-alph(1)-disc)
       else
        iroot = 1
       end if
       do i = 1,iroot
        alph(i) = alph(i) + c(1)/3.0d0
       end do
      end if
 1005 do i = 1,n-m
       p(i) = -c(i)
      end do
      end
C
C
      subroutine CHECKI2(p,x,m,n,xl,ur)
      integer m,n
      real*8 p(*),x(*),xl,ur
      call TRANS1I2(p,x,m,n,xl,ur)
      call TRANSCI2(p,x,m-1,n)
      return
      end

C
C
      subroutine UnitsCheck(oz,nz,k)
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
C
C.. Formal Arguments ..
      integer nz
      real*8 oz(*)
C
C.. Local Scalars ..
      integer zlen,i,k
C
C.. Local Arrays ..
      real*8 z(mpkp),zmin,zmax
C
C.. External Functions ..
      real*8 AMIN,AMAX
      external AMIN,AMAX
C   LINES OF CODE ADDED FOR X-13A-S : 2
      logical dpeq
      external dpeq
C   END OF CODE BLOCK
C
C..
      zlen=0
      k=0
      do i=1,nz
       if (.not.dpeq(oz(i), -99999.0d0)) then
        z(i)=DABS(oz(i))
        zlen=zlen+1
       end if
      end do
      zmin=AMIN(z,zlen)
      if (zmin .ge. 10.0d4) then
       k=-1
       zmin = zmin*10.0d-3
       do while (zmin .gt. 10.0d3)
        k=k-1
        zmin=zmin*10.0d-3
       end do
       do i=1,nz
        if (.not.dpeq(oz(i), -99999.0d0)) then
         oz(i)=oz(i)*(10.0d0**(3*k))
        end if
       end do
      end if
      zmax = AMAX(z,zlen)
      if (zmax .lt. 10.0d-3) then
       zmax=zmax*10.0d3
       k=1
       do while (zmax .le. 10.0d-1)
        k=k+1
        zmax=zmax*10.0d3
       end do
       do i=1,nz
        if (.not.dpeq(oz(i), -99999.0d0)) then
         oz(i)=oz(i)*(10.0d0**(3*k))
        end if
       end do
      end if
      return
      end
C
C
      double precision function AMAX(z,nz)
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
      integer nz
      real*8 z(*)
C
C.. Local Scalars ..
      real*8 zmax
      integer i
C
C..
      zmax = z(1)
      do i=1,nz
       if (z(i).gt.zmax) then
        zmax=z(i)
       end if
      end do
      AMAX=zmax
      return
      end
C
C
      double precision function AMIN(z,nz)
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
      integer nz
      real*8 z(*)
C
C.. Local Scalars ..
      real*8 zmin
      integer i
C
C..
      zmin = z(1)
      do i=1,nz
       if (z(i).lt.zmin) then
        zmin=z(i)
       end if
      end do
      AMIN=zmin
      return
      end
C
      Integer function getLastPeriod(Nz,Nper,Nyear,Mq)
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
      integer Nz,Nper,Nyear,Mq
C
C.. Local Scalars ..
      integer i,lper,lyear
      lper=Nper
      lyear=Nyear
      do i=2,Nz
       lper=lper+1
       if (lper .gt. Mq) then
        lper=1
        lyear=lyear+1
       end if
      end do
      getLastPeriod=lper
      return
      end
C
      Integer function getLastYear(Nz,Nper,Nyear,Mq)
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
      integer Nz,Nper,Nyear,Mq
C
C.. Local Scalars ..
      integer i,lper,lyear
      lper=Nper
      lyear=Nyear
      do i=2,Nz
       lper=lper+1
       if (lper .gt. Mq) then
        lper=1
        lyear=lyear+1
       end if
      end do
      getLastYear=lyear
      return
      end


CC
C
CC
      SUBROUTINE TITLECK (STRING)
        CHARACTER*(80) STRING
        CHARACTER CHAR
        INTEGER I,J,ISTRLEN
        INTEGER*4 IASC,ICHAR
        J=ISTRLEN(STRING)
        DO 10 I=1,J
          IASC = ICHAR(STRING(I:I))
          IF (IASC .GT. 126) THEN
           IF ((IASC .ge. 192) .and. (IASC .le. 198)) THEN
            STRING(I:I) = CHAR(65)
           ELSE IF ((IASC .ge. 200) .and. (IASC .le. 203)) THEN
            STRING(I:I) = CHAR(67)
           ELSE IF ((IASC .ge. 204) .and. (IASC .le. 207)) THEN
            STRING(I:I) = CHAR(73)
           ELSE IF (IASC .eq. 208) THEN
            STRING(I:I) = CHAR(68)
           ELSE IF (IASC .eq. 209) THEN
            STRING(I:I) = CHAR(78)
           ELSE IF ((IASC .ge. 210) .and. (IASC .le. 216)) THEN
            STRING(I:I) = CHAR(79)
           ELSE IF ((IASC .ge. 217) .and. (IASC .le. 220)) THEN
            STRING(I:I) = CHAR(85)
           ELSE IF (IASC .eq. 221) THEN
            STRING(I:I) = CHAR(89)
           ELSE IF ((IASC .ge. 224) .and. (IASC .le. 230)) THEN
            STRING(I:I) = CHAR(97)
           ELSE IF (IASC .eq. 231) THEN
            STRING(I:I) = CHAR(99)
           ELSE IF ((IASC .ge. 232) .and. (IASC .le. 235)) THEN
            STRING(I:I) = CHAR(101)
           ELSE IF ((IASC .ge. 236) .and. (IASC .le. 239)) THEN
            STRING(I:I) = CHAR(105)
           ELSE IF (IASC .eq. 241) THEN
            STRING(I:I) = CHAR(110)
           ELSE IF ((IASC .ge. 242) .and. (IASC .le. 246)) THEN
            STRING(I:I) = CHAR(111)
           ELSE IF ((IASC .ge. 249) .and. (IASC .le. 252)) THEN
            STRING(I:I) = CHAR(117)
           ELSE IF (IASC .eq. 253) THEN
            STRING(I:I) = CHAR(121)
           ELSE
             STRING(I:I)=ACHAR(45)
           end if
          end if
 10     CONTINUE
        RETURN
      END
CC
C
CC
C      
C FUNCTION TO COMPUTE THE VARIANCE
C
      double precision function DIVIDECHECK(x)
C 
C.. Implicits .. 
      implicit none
C 
C.. Formal Arguments .. 
      real*8 x
       DIVIDECHECK = x
       if (abs(x) .lt. 1.0d-9) then
         DIVIDECHECK = 1.0d-9
       end if
      return
      end

cc
c
cc
      integer function ChangeModel(nio,init,nochmodel,
     $           statseas,posbphi,rmod,p,d,q,bp,bd,bq,th,bth,phi,
     $           bphi,imean,remMeanMCS,out,tramo,inputModel)
C.. Parameters ..
      integer n1
      logical T,F
      parameter (n1 = 1, T = .true., F = .false.)
      real*8 ZERO
      parameter (ZERO=0.0d0)
c
      integer nio,init,nochmodel,statseas,posbphi,p,d,q,bp,bd,
     $        bq,out,imean,tramo,inputModel,
     $        oP,oD,oQ,oBp,oBd,oBq,oImean
      real*8 phi(3*n1),bphi(3*n1),th(3*n1),bth(3*n1),rmod
      logical remMeanMCS
c
      integer cambiado,difsOrig,origInit
      INCLUDE 'htmlout.cmn'
c
      cambiado=0
      origInit=init
      oP=p
      oD=d
      oQ=q
      oBp=bp
      oBd=bd
      oBQ=bq
      oImean=imean
      difsOrig=d+bd 
      if (nochmodel.eq.0) then  
       if (bd.eq.0) then
        if ((bp.eq.1).and.(bq.eq.1).and.
     $      (abs(bphi(1)).lt.abs(bth(1))))then
         if ((bphi(1).gt.ZERO).and.(bth(1).lt.ZERO).and.
     $            (statseas.eq.1)) then
          bd=1
          bp=0
          cambiado=1
         else if ((bphi(1).gt.ZERO).and.(bth(1).gt.ZERO)) then  
          cambiado=1
          bq=0 
         end if
        else if ((bp.eq.1).and.(bq.eq.0).and.(statseas.eq.1)) then
         if (bphi(1).gt.rmod-0.2d0) then
          bp=0
          bq=1
          bd=1
          cambiado=1  
         else if ((bphi(1).le.ZERO).and.(bphi(1).ge.-rmod+0.2d0)) then
          cambiado=1
          bp=0
         end if
        end if    
       else
        if ((posbphi.eq.1).and.(bp.eq.1).and.(bphi(1).le.ZERO)) then
         bp=0
         bq=1
         cambiado=1 
        end if 
       end if
       if (d.eq.0) then 
        if ((p.eq.1).and.(q.eq.1)) then
         if (abs(phi(1)).lt.abs(th(1))) then
          if ((phi(1).gt.ZERO).and.(th(1).lt.0.d0).and.
     $           (statseas.eq.1)) then
           d=1
           p=0
           cambiado=1
          end if 
         end if
        else if ((p.eq.1).and.(statseas.eq.1).and.
     $           (phi(1).gt.rmod)) then  
         p=0
         q=1
         d=1
         cambiado=1           
        else if ((p.eq.0).and.(q.eq.1).and.(bp.eq.0).and.(bd.eq.0)
     $    .and.(bq.eq.0)) then
         q=0
         cambiado=4
        end if
       end if
       if (difsOrig.lt.d+bd) then
        imean=0
       end if   
       ChangeModel=cambiado
       if (cambiado.ne.0) then
        if (remMeanMCS) then
         imean=0
        end if
        if ((cambiado.ne.2).and.(cambiado.ne.3)) then
         init=0
        end if
        call setTmcs('Y')
        if (out.eq.0) then     
c
         if (inputModel.eq.1) then  
          call ShowFirstModel(Nio,oP,oD,oQ,oBp,oBd,oBq,th,
     $          Bth,phi,Bphi,oImean,tramo,origInit) 
         end if
         if (cambiado.eq.2) then
          CALL wWritln('The negative seasonal correlation - '//
     $         'possibly induced by seasonal adjustment - is '// 
     $         'ignored.  Model from regARIMA has been modified by '//
     $         'setting BTH=0.'//Cbr,Nio,0,T,F) 
          CALL writln('MODEL CHANGED TO : ',Nio,0,F,F)
         else if (cambiado.eq.3) then
          CALL wWritln('A pure seasonal MA(1) does not yield '//
     $         'a proper seasonal component. Model from regARIMA '//
     $         'has been modified by setting BTH=0'//Cbr,Nio,0,T,F)
          CALL writln('MODEL CHANGED TO : ',Nio,0,F,F)
         ELSE
          CALL writln('MODEL CHANGED TO : ',Nio,0,T,F)
         end if
         write (Nio,100)p,d,q,bp,bd,bq
  100    format('(',1x,i1,',',2x,i1,',',2x,i1,',',1x,')',4x,
     $          '(',1x,i1,',',2x,i1,',',2x,i1,1x,')</p>')                 
        end if
       end if 
      end if
      return
      end
cc
c
cc
