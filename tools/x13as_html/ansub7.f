C
C
C  REMOVED AMI SUBROUTINE - BCM June 2001
C
C
C
      subroutine MOCOMPARE(p,q,d,bd,bq,bp,imean,prec)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
      integer p,q,d,bd,bq,bp,imean
      real*8 prec
C
C.. Local Scalars ..
      integer i,ndvmin,nstart
      real*8 dvmin,qxmin,sgmin
C
C.. Local Arrays ..
      real*8 dvalue(4)
      include 'amic.i'
C
C ... Executable Statements ...
C
      if ((0.0d0.lt.prec) .or. (1.0d0.gt.prec)) then
       prec = 0.5d0
      end if
      qxmin = 1.0d12
      do i = 1,4
       nstart = (i-1)*2 + 1
       if (Statistics(nstart) .lt. qxmin) then
        qxmin = Statistics(nstart)
        sgmin = Statistics(nstart+1)
       end if
      end do
      do i = 1,4
       nstart = (i-1)*2 + 1
       Statistics(nstart) = Statistics(nstart) / sgmin
       Statistics(nstart+1) = Statistics(nstart+1) / sgmin
      end do
      do i = 1,4
       nstart = (i-1)*2 + 1
       dvalue(i) =
     $   prec*Statistics(nstart+1) + (1-prec)*Statistics(nstart)
      end do
      dvmin = 1.0d12
      do i = 1,4
       if (dvalue(i) .lt. dvmin) then
        dvmin = dvalue(i)
        ndvmin = i
       end if
      end do
C
C NOW WE RETORE THE BEST MODEL
C
      nstart = (ndvmin-1)*7 + 1
      p = Models(nstart)
      d = Models(nstart+1)
      q = Models(nstart+2)
      bp = Models(nstart+3)
      bd = Models(nstart+4)
      bq = Models(nstart+5)
      imean = Models(nstart+6)
      end
C
C
C
      subroutine SETMODEL(p,d,q,bp,bd,bq,imean,nmodel)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
      integer p,d,q,bp,bd,bq,imean,nmodel
C
C.. Local Scalars ..
      integer nstart
C
C.. Local Arrays ..
      integer models(28)
C
C.. Data Declarations ..
C THE STRUCTURE OF THE MATRIX IS :
C     P,D,Q,BP,BD,BQ,IMEAN
C
      data models/
     $0,1,1,0,1,1,1,0,2,2,0,1,1,0,3,2,2,0,1,1,0,3,1,1,0,1,1,1/
C First Model
C Second Model
C Third Model
C
C ... Executable Statements ...
C
C Fourth Model
C
      nstart = (nmodel-1)*7 + 1
      p = models(nstart)
      d = models(nstart+1)
      q = models(nstart+2)
      bp = models(nstart+3)
      bd = models(nstart+4)
      bq = models(nstart+5)
      imean = models(nstart+6)
      end
C
      subroutine STOREMODEL(p,d,q,bp,bd,bq,imean,sqf,qbox,nmodel)
C
C.. Implicits ..
      implicit none
C
C.. Formal Arguments ..
      integer p,d,q,bp,bd,bq,imean,nmodel
      real*8 sqf,qbox
C
C.. Local Scalars ..
      integer nstart
      include 'amic.i'
C
C ... Executable Statements ...
C
C THE STRUCTURE OF THE ARRAY MODELS IS :
C     P,D,Q,BP,BD,BQ,IMEAN
C
      nstart = (nmodel-1)*7 + 1
      Models(nstart) = p
      Models(nstart+1) = d
      Models(nstart+2) = q
      Models(nstart+3) = bp
      Models(nstart+4) = bd
      Models(nstart+5) = bq
      Models(nstart+6) = imean
      nstart = (nmodel-1)*2 + 1
      Statistics(nstart) = qbox
      Statistics(nstart+1) = sqf
      end
C
C
      integer function CHECKADM(p,q,bp,bq,d,bd,th,bth,phi,bphi,mq,rmod,
     $                          epsphi,varwnc,out)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
      integer n1,n12
      parameter (n12 = 12, n1 = 1)
C
C.. Formal Arguments ..
      integer p,q,bp,bq,d,bd,mq,out
      real*8 th(*),bth(*),phi(*),bphi(*),rmod,epsphi,varwnc
C
C.. Local Scalars ..
      integer i,j,jk,k,nbths,nchi,nchins,nchis,ncyc,ncycns,ncycs,nphis,
     $        npsi,npsins,npsis,nths,pstar,qstar,dumInt
      real*8 cmu,dplusd
      logical root0c,rootPIc,rootPIs,IsCloseToTD
C
C.. Local Arrays ..
      real*8 ar(5*n12+n12/3),bths(2*n12+1),chi(8),chins(8),chis(5),
     $       cyc(17),cycns(5),cycs(17),dum(80),imz(5*n12+n12/3),
     $       modul(5*n12+n12/3),phis(4*n1),pr(5*n12+n12/3),psi(27),
     $       psins(27),psis(16),rez(5*n12+n12/3),ths(4*n1),thstar(40)
C
C.. External Functions ..
      integer CHKSPCT
      external CHKSPCT
C
C.. External Calls ..
      external CONV, F1RST, RPQ
C
C.. Intrinsic Functions ..
      intrinsic ABS
C   LINES OF CODE ADDED FOR X-13A-S : 1
      include 'error.cmn'
C   END OF CODE BLOCK
C
C ... Executable Statements ...
C
C
C
C
      dumInt=3
      if (p .gt. 0) then
       phis(1) = 1.d0
       do i = 1,p
        phis(i+1) = -phi(i)
       end do
       nphis = p + 1
       call RPQ(phis,nphis,rez,imz,modul,ar,pr,0,out)
C   LINES OF CODE ADDED FOR X-13A-S : 1
       IF(Lfatal)RETURN
C   END OF CODE BLOCK
      end if
      ths(1) = 1.0d0
      do i = 1,q
       ths(i+1) = -th(i)
      end do
      nths = q + 1
      bths(1) = 1.0d0
      do i = 1,bq
       j = i*mq + 1
       bths(j) = -bth(i)
       do k = 2,mq
        jk = k + mq*(i-1)
        bths(jk) = 0.0d0
       end do
      end do
      nbths = bq*mq + 1
C
C COMPUTE THE MODEL FOR THE COMPONENTS
C
      pstar = p + d + mq*(bd+bp) + 1
      call CONV(ths,nths,bths,nbths,thstar,qstar)
      chins(1) = 1.0d0
      dplusd = d + bd
      do i = 1,INT(dplusd)
       chins(i+1) = 0.0d0
       do j = 1,i
        k = i - j + 2
        chins(k) = chins(k) - chins(k-1)
       end do
      end do
      nchins = dplusd + 1
      chis(1) = 1.0d0
      nchis = 1
      if (bp .ne. 0) then
       cmu = (-bphi(mq+1))**(1.0d0/mq)
       dum(1) = 1.0d0
       dum(2) = -cmu
       if (ABS(1.0d0-cmu) .lt. 1.0d-13) then
        call CONV(dum,2,chins,nchins,chins,nchins)
       else
        call CONV(dum,2,chis,nchis,chis,nchis)
       end if
      end if
      psins(1) = 1.0d0
      do i = 2,27
       psins(i) = 0.0d0
       psi(i) = 0.0d0
      end do
      npsins = 1
      psis(1) = 1.0d0
      npsis = 1
      if (bd .ne. 0) then
       do i = 1,mq
        dum(i) = 1.0d0
       end do
       call CONV(dum,mq,psins,npsins,psins,npsins)
       if (bd .ne. 1) then
        call CONV(dum,mq,psins,npsins,psins,npsins)
        goto 5000
       end if
      end if
      if (bp .ne. 0) then
       dum(1) = 1.0d0
       do i = 2,mq
        dum(i) = cmu * dum(i-1)
       end do
       if (ABS(1.0d0-cmu) .lt. 1.0d-13) then
        call CONV(dum,mq,psins,npsins,psins,npsins)
       else
        call CONV(dum,mq,psis,npsis,psis,npsis)
       end if
      end if
 5000 cycs(1) = 1.0d0
      cycns(1) = 1.0d0
      ncycs = 1
      ncycns = 1
C
C COMPUTATION OF THE STATIONARY AND NON-STATIONARY (IF UNIT ROOTS)
C DENOMINATOR OF THE COMPONENTS
C
      IsCloseToTD=.FALSE.
      call F1RST(p,imz,rez,ar,epsphi,mq,cycns,ncycns,psins,npsins,cycs,
     $           ncycs,chins,nchins,chis,nchis,modul,psis,npsis,rmod,
     $           root0c,rootPIc,rootPIs,IsCloseToTD)
C
C
      call CONV(chis,nchis,chins,nchins,chi,nchi)
      call CONV(psis,npsis,psins,npsins,psi,npsi)
      call CONV(cycs,ncycs,cycns,ncycns,cyc,ncyc)
C
C
      CHECKADM = CHKSPCT(thstar,qstar,chi,nchi,cyc,ncyc,psi,npsi,pstar,
     $                   mq,d,bd,varwnc)
      end
C
C
C
C
      integer function CHKSPCT(thstar,qstar,chi,nchi,cyc,ncyc,psi,npsi,
     $                         pstar,mq,d,bd,varwnc)
C
C.. Implicits ..
      implicit none
      include 'srslen.prm'
      include 'dimensions.i'
C
C.. Formal Arguments ..
      integer qstar,nchi,ncyc,npsi,pstar,mq,d,bd
      real*8 thstar(maxTh),chi(8),cyc(5),psi(27),varwnc
C
C.. Local Scalars ..
      integer i,ipipp,j,jmq,jsfix,ncycth,ndum,nn,nqt,nrt,nu,nus,nvn
      real*8 ce1,ce2,cexmin1,cexmin2,e1,e2,enoc,enot,estar,exmin1,
     $       exmin2,pi,qmin,qt1,lb,ub,exmin7
cc
c     (Roberto Lopez: New 01/2006
cc     outputs minimGrid)      
      real*8 ce3,cexmin3,e3,exmin3
      integer doMinimGrid,n_Step
      parameter (doMinimGrid = 1)
cc
c
cc
      integer ixmin
C
C.. Local Arrays ..
      integer iconv(7),jconv(4)
      real*8 dum(80),efmin(7),exmin(7),fn(8),qt(32),rt(32),u(22),us(50),
     $       vn(80)
C
C.. External Calls ..
      external CONJ, DIVFCN, MINIM, MULTFN, PARFRA, GLOBALMINIM,
     $         MINIMGRID
C
C.. Intrinsic Functions ..
      intrinsic MAX, MIN
      include 'func.i'
      include 'func2.i'
      include 'func3.i'
      include 'func4.i'
      include 'min.i'
      include 'test.i'
C
C ... Executable Statements ...
C
      jsfix = 0
      ncycth = 0
      pi = 3.14159265358979d0
      call CONJ(thstar,qstar,thstar,qstar,Ff,Nf)
      call CONJ(chi,nchi,chi,nchi,Ft,Nt)
      call CONJ(cyc,ncyc,cyc,ncyc,Fc,Nc)
      call CONJ(psi,npsi,psi,npsi,Fs,Ns)
      call MULTFN(Ft,Nt,Fc,Nc,fn,nn)
      call MULTFN(fn,nn,Fs,Ns,Fh,Nh)
      if (qstar .lt. pstar) then
       nqt = 1
       qt(1) = 0.0d0
       do i = 1,qstar
        rt(i) = Ff(i)
       end do
       j = qstar + 1
       do i = j,pstar
        rt(i) = 0.0d0
       end do
       nrt = pstar
      else
       call DIVFCN(Ff,Nf,Fh,Nh,qt,nqt,rt,nrt)
      end if
      if (npsi .eq. 1) then
       jsfix = 1
      end if
      if (mq .eq. 1) then
       jsfix = 1
      end if
C
C  8484 IS THE END OF COMPUTATION OF NUMERATORS OF SPECTRA COMPONENTS
C
      if (jsfix.eq.1 .and. ncyc.eq.1 .and. ncycth.eq.0 .and. nchi.gt.1)
     $   then
       do i = 1,nrt
        Ut(i) = rt(i)
       end do
       Nut = nrt
       estar = 0.0d0
       enoc = 0.0d0
C
      else if (jsfix.ne.1 .and. ncyc.eq.1 .and. ncycth.eq.0 .and.
     $        nchi.eq.1) then
       do i = 1,nrt
        V(i) = rt(i)
       end do
       Nv = nrt
       enot = 0.0d0
       enoc = 0.0d0
C
      else if (jsfix.eq.1 .and. ncycth.eq.0 .and.
     $        (varwnc.gt.1.0D-10 .and. ncyc.gt.1 )
     $        .and.nchi.eq.1) then
       do i = 1,nrt
        Uc(i) = rt(i)
       end do
       Nuc = nrt
       estar = 0.0d0
       enot = 0.0d0
C
C
      else if (jsfix.ne.1 .and. ncycth.eq.0 .and.
     $        varwnc.gt.1.0D-10 .and.  ncyc.gt.1 .and.nchi.gt.1) then
       call PARFRA(rt,nrt,fn,nn,Fs,Ns,u,nu,V,Nv)
       call MULTFN(u,nu,Fs,Ns,us,nus)
       call MULTFN(V,Nv,fn,nn,vn,nvn)
       do i = 1,nus
        dum(i) = rt(i) - us(i) - vn(i)
       end do
C
C  FIND H.C.F OF FT(X) AND FC(X)
C
       ipipp = ncyc + nchi - 1
       do i = nu+1,ipipp
        u(i) = 0.0d0
       end do
       nu = ipipp
       call PARFRA(u,nu,Fc,Nc,Ft,Nt,Uc,Nuc,Ut,Nut)
       call MULTFN(Uc,Nuc,Ft,Nt,us,nus)
       call MULTFN(Ut,Nut,Fc,Nc,vn,nvn)
       do i = 1,nu
        dum(i) = u(i) - us(i) - vn(i)
       end do
C
      else if (jsfix.eq.1 .and. ncycth.eq.0 .and. 
     $        varwnc.gt.1.0D-10 .and.ncyc.gt.1 .and.nchi.gt.1) then
C
       call PARFRA(rt,nrt,Ft,Nt,Fc,Nc,Ut,Nut,Uc,Nuc)
       call MULTFN(Uc,Nuc,Ft,Nt,us,nus)
       call MULTFN(Ut,Nut,Fc,Nc,vn,nvn)
       do i = 1,nus
        dum(i) = rt(i) - us(i) - vn(i)
       end do
       estar = 0.0d0
      else if (jsfix.ne.1 .and. ncycth.eq.0 .and. 
     $        varwnc.gt.1.0D-10 .and.ncyc.gt.1 .and.nchi.eq.1) then
       call PARFRA(rt,nrt,Fc,Nc,Fs,Ns,Uc,Nuc,V,Nv)
       call MULTFN(Uc,Nuc,Fs,Ns,us,nus)
       call MULTFN(V,Nv,Fc,Nc,vn,nvn)
       do i = 1,nus
        dum(i) = rt(i) - us(i) - vn(i)
       end do
       enot = 0.0d0
      else if (jsfix.ne.1 .and. ncycth.eq.0 .and.
     $        varwnc.gt.1.0D-10 .and. ncyc.eq.1 .and.nchi.gt.1) then
       call PARFRA(rt,nrt,Ft,Nt,Fs,Ns,Ut,Nut,V,Nv)
       call MULTFN(V,Nv,Ft,Nt,us,nus)
       call MULTFN(Ut,Nut,Fs,Ns,vn,nvn)
       do i = 1,nus
        dum(i) = rt(i) - us(i) - vn(i)
       end do
       enoc = 0.0d0
      end if
C
C
      if (qstar .gt. pstar) then
       call MULTFN(qt,nqt,Fc,Nc,dum,ndum)
       Nuc = MAX(ndum,Nuc)
       do i = 1,Nuc
        Uc(i) = dum(i) + Uc(i)
       end do
       ncycth = 1
      end if
C
C
      if (nchi .ne. 1) then
C
C  FIND MINIMUM OF TREND SPECTRUM AND PLOT
C
       Ifunc = 2
       Dstop = 0.000005d0
       Step = 0.01d0
       n_Step = 12
       Start = pi
C Changed (by Donald Martin, 7/23/02) set lb and ub, pass to minim.
       lb = 0.5d0 * 2D0
       ub = pi
C       call MINIM(e1,exmin1,lb,ub,jconv(1))
       call GlobalMINIM(e1,exmin1,lb,ub,jconv(1),n_step,d+bd,mq,2)
       Start = 0.5d0 * pi
C Changed (by Donald Martin, 7/23/02) set lb and ub, pass to minim.
       lb = 0D0
       ub = 0.5d0 * 2D0
c       call MINIM(e2,exmin2,lb,ub,jconv(2))
       call GlobalMINIM(e2,exmin2,lb,ub,jconv(2),n_step,d+bd,mq,2)
       enot = MIN(e1,e2)
c
c MY ADDITION (Donald Martin, July 2002) TO 'SPECTRUM' OF TREND USING GRID SEARCH ALSO
c
c       if (Newmdl.gt.0) THEN
c        call minim2(e3, ixmin)
c        exmin3 = dble(float(ixmin))
c       end if
       if ((doMinimGrid.gt.0) .and. (ut(1)-enot*ft(1).lt.0.0d0)) then
        call minimGrid(e3,exmin3,mq,2,2)
        if (e3 .lt. enot) enot = e3
       end if
       
      end if
C
      if (varwnc.gt.1.0D-10 .and.(ncycth.ne.0 .or. ncyc.ne.1)) then
C
C  FIND MINIMUM OF CYCLE SPECTRUM AND PLOT
C
       Ifunc = 3
       Dstop = 0.000005d0
       Step = 0.01d0
       Start = pi
C Changed (by Donald Martin, 7/23/02) set lb and ub, pass to minim.
       lb = 0.5d0*pi
       ub = pi
c       call MINIM(ce1,cexmin1,lb,ub,jconv(3))
       call GlobalMINIM(ce1,cexmin1,lb,ub,jconv(3),n_step,d+bd,mq,2)
       Start = 0.5d0 * pi
C Changed (by Donald Martin, 7/23/02) set lb and ub, pass to minim.
       lb = 0D0
       ub = 0.5d0 * pi
c       call MINIM(ce2,cexmin2,lb,ub,jconv(4))
       call GlobalMINIM(ce2,cexmin2,lb,ub,jconv(4),n_step,d+bd,mq,2)
       enoc = MIN(ce1,ce2)
       if ((doMinimGrid.gt.0) .and. (uc(1)-enoc*fc(1).lt.0.0d0)) then
        call minimGrid(ce3,cexmin3,mq,2,1)
        if (ce3 .lt. enoc) enoc = ce3
       end if
      end if
C
      if (jsfix .ne. 1) then
C
C  FIND MINIMUM OF SEASONAL SPECTRUM AND PLOT
C
       Ifunc = 1
       Dstop = 0.000005d0
       Step = 0.01d0
       Start = 0.0d0
       jmq = mq / 2
C Changed (by Donald Martin, 7/23/02) set lb and ub, pass to minim.
       lb = 0D0
       ub = pi / dble(jmq)
       call MINIM(efmin(1),exmin(1),lb,ub,iconv(1))
       do i = 2,jmq
        Start = (dble(i-0.5d0) * pi) / dble(jmq)
C Changed (by Donald Martin, 7/23/02) set lb and ub, pass to minim.
        lb = (dble(i-1) * pi) / dble(jmq)
        ub = (dble(i) * pi) / dble(jmq)
        call MINIM(efmin(i),exmin(i),lb,ub,iconv(i))
       end do
       estar = 10.0d0
       do i = 1,jmq
        if (efmin(i) .lt. estar) then
         estar = efmin(i)
        end if
       end do
       if ((doMinimGrid.gt.0) .and. (v(1)- estar*fs(1) .lt. 0.0d0)) THEN
        call minimGrid(efmin(jmq+1), exmin(jmq+1),mq,2,1)  
        if (efmin(jmq+1) .lt. estar) then
         estar = efmin(jmq+1)
        end if  
       end if
      end if
C
C CHECK DECOMPOSITION VALID
C
C
      qt1 = qt(1) + enot + estar + enoc
      if (qstar .gt. pstar) then
       qt1 = enot + estar + enoc
      end if
C
C
      qmin = qt1
      if (qmin .lt. 0.0d0) then
       CHKSPCT = 0
      else
       CHKSPCT = 1
      end if
      end
C
C
