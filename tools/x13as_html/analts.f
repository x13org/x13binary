C     Last change Mar. 2021if there is sliding span or history,
C     not write to .rog
C     previous change:  BCM  30 Sep 2005   11:59 am
C     Previous change:  BCM   4 Oct 2002    3:03 pm
C   LINES OF CODE COMMENTED FOR X-13A-S : 35
C      program
CC
CC.. Implicits ..
C      implicit none
CC
CC.. Local Scalars ..
C      integer ione,itbl,nver,Ierr
C      character graphd*180,infile*180,outd*180,outfile*180,
C     $          Errext*180
CC
CC.. External Calls ..
C      external GETCOMMLINE, SEATS
CC
CC ... Executable Statements ...
CC
CC
CC
CC THIS SUBROUTINE MASK THE FLOATING POINT INTERRUPT
CC
C      call MASK()
C      call GETCOMMLINE(nver,ione,outd,graphd,infile,outfile,itbl)
C      call rmgraph(graphd,outd)
CC
CC All time run with itbl=1
CC
CC      itbl = 1
C      call SEATS(infile,outfile,outd,graphd,nver,ione,itbl,Ierr,Errext)
C      if (Ierr.ne.0) then
C       write (*,'(6x,A)')Errext
C       stop 'Program Aborted'
C      else
C       stop 'Processing Completed'
C      end if
C      end
C   END OF CODE BLOCK
C
      subroutine SEATS(infil,outfile,outd,graphd,ione,nver,Ierr,
     $                 Errext,Lgraf)
C
C.. Implicits ..
      implicit none
C
C.. Parameters ..
C   LINES OF CODE COMMENTED FOR X-13A-S : 3 
C      integer ndevice,nidevice,n1,n12,n10,mp,kp,kl
C      parameter (kl = PFCST, kp = 50, mp = 600, n10 = 10, n12 = 12, n1 = 1,
C     $           ndevice = 16, nidevice = 71)
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 2
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      include 'dimensions.i'
      integer n1,n12,n10
      LOGICAL T,F
      parameter (n10 = 10, n12 = 12, n1 = 1, 
     &           T = .true., F = .false.)
C   END OF CODE BLOCK
      real*8 ceps,ONE,ZERO,TWO,THREE,ONEHND,MONE
      parameter (ceps = 1.0d-13, ONE = 1.0d0, ZERO = 0.0d0, TWO = 2.0d0,
     &           THREE = 3.0d0, ONEHND = 100.0d0, MONE = -1.0d0)
C
C.. Formal Arguments ..
      integer nver,ione,Ierr
      character infil*180,outfile*180,outd*180,graphd*180,Errext*180
      logical Lgraf
C
C.. Local Scalars ..
      logical IsCloseToTD,lptag,gudrun
      real*8 varwnc,TramDet
      integer bpstar,fh,hpcycle,i,iauto,ifail,iioneout,ilsave,
     $        imeansave,inover,interp,iout,iper,iprint,iqm,it,
     $        itt,iyear,j,j0,jdd,jk,k,kd,kkp,kq,lll,lll1,lp,lsig,maxf,
     $        maxit,model,mq2,ncen,ncrazy,nf,units,crmean,nouts,
     $        centrregs,smtr
      integer niosave,nk,nn,nna,noretry,noserie,nout,nper1,
     $        nper2,nphi,npread,nprova,nsavebd,nsavebp,nsavebq,
     $        nsaved,nsavep,nsaveq,nsr,nth,ntltst,nyer1,nyer2,
     $        nz1,qmax,rogtable,seas,tst,statseas
      integer qbqmq,fhi,NAiter
      integer totalSeasXL,totalSeasRes
c      integer niter
      integer NumSer
c      integer ntry
      integer nochmodel,modelsumm
      integer acfe,posbphi,printphtrf
C.. Added by REG on 30 Aug 2005 to create nfixed local variable
      integer nfixed
      integer Nzsave,Nzread,Nzorig
      integer ifault
      integer IsOk,CkLen,auxInt
      integer nTDpeaks,nSEASpeaks
      integer firstobs,lastobs
      character StrFobs*7,StrLobs*7
      character auxS*6
      character buff*180,filename*180,fname*30,sgraphdir*180,
     $          soutdir*180,status,subtitle*50,soutfile*180,
     $          outf*180,FilenameC*180
      character SerSet*180
      common /SerieSet/ SerSet
c     character mattitle*180
      character cname*50,shortName*2, errch*2
      character tabtables*100, d_tabtables*100,htmtit*120
      logical opened,saved,matopened,Momopened,bool,remMeanMCS
      real*8 DONE
      real*8 blqt,dof1,dw,epsiv,f0,first,hplan,prec,rkurt,
     $       rmean,rstd,rtval,rvar,s,s2save,sbjstat1,sbjstat2,second,
     $       sfd,sigq,skewne,spstat1,sum,ta,test,test1,
     $       thlim,bthlim,tmean,tmu,tvalRUNS,wkk,wm,wnormtes,
     $       hpper,maxSpect
      real*8 ws,wsk,xmed,seMean,zab,zaf,zm,zerr
      real*8 Fbar,alpha, Ken
      real*8 wrmqx1,wrmqa1,aux1,aux2
      
      integer flagTstu
      integer DF,sDF
      real*8 Qstat,SR(5*n10),sSE(5*n10),
     $       sea(5*n10),sQstat,SumSres
      integer n_1,n0
      integer InputModel
      integer outNA,stochTD
      integer ItnSearch,IfnSearch,nxSearch,Esearch(n10)
      real*8 FIsearch,xSearch(n10)
c para sumSeats
      integer totMCS,totNA,totS,totCyc,totStocTD,totSpecFac,totACF,
     $        totCCF,totUnsSA,totUnrSA,totRevSA,totSnotSig,totBias,
     $        totCrQS,totCrSNP,totCrPeaks
      integer lost
C
C   LINES OF CODE ADDED FOR X-13A-S : 1
      integer ndevice,nidevice,noutdir
C   END OF CODE BLOCK
C
      integer inicbucle,endbucle
C.. Local Arrays ..
      integer e(n10),fixParam(n10),TDpeaks(6),SEASpeaks(6),ipos,
     &        thisDate(2)
      real*8 xtmp(n10)
      real*8 Szz(61),ow(61)
      character picosXl(7)*2,str*(5)
      real*8 a(mpkp),ba(mpkp),bphi1(4*n10),
     $       bphis(n12+1),bphist(6*n10),bth1(4*n10),bths(2*n12+1),
     $       bz(mp+2*kp),conv1(n10),dum(n10),forbias(kp),
     $       oz(mpkp),phi1(4*n1),
     $       phis(4*n1),ps(5*n10+n12/2),r(5*n10),
     $       se(n10),th1(4*n1),ths(4*n1),
     $       xmax(n10),xmin(n10),z(mpkp),aa(mpkp),dvec(1),
     $       temp(mpkp),trtemp(mpkp),th0(3*n1),bth0(3*n1),
     $       satemp(mpkp),stemp(mpkp),caltemp(mpkp),pretemp(mpkp),
     $       irtemp(mpkp),backOZ(mpkp),Resid(mpkp),TRstoch(mpkp),
     $       seasStoch(mpkp),irStoch(mpkp)
      real*8 sePHI(n10),seTH(n10),seBPHI(n10),seBTH(n10)
      real*8 rez(5*n12+n12/3),imz(5*n12+n12/3),modul(5*n12+n12/3),
     $       ar(5*n12+n12/3),pr(5*n12+n12/3)
      real*8 MArez(5*n12+n12/3),MAimz(5*n12+n12/3),MAmodul(5*n12+n12/3),
     $       MAar(5*n12+n12/3),MApr(5*n12+n12/3)
      real*8 c(n10,n10),cMatrix(n10,n10)
      real*8 seRxl(5*n10),rXL(5*n10)
      integer tstMean
      real*8 wmDifXL,VdifXL
      real*8 Wdif(mpkp),WdifCen(mpkp)
      integer nWdif
      real*8 partAcf(5*n10),SEpartAcf,QstatXL
      integer ImeanOut,whtml
      integer qstar_seats,pstar_seats
      logical printBack
C
C.. External Functions ..
      real*8 DMEAN
      real*8 DMED
      real*8 DVAR
      integer ISTRLEN
      real*8 POLYVAL
      real*8 KENDALLS
      logical ISOPEN
      external DMEAN, DMED, DVAR, ISTRLEN, POLYVAL, KENDALLS, ISOPEN
      integer getNmmu,getNmp,getNmd,getNmq,getNmBp,getNmBd,getNmBq,
     $        getSsh,getSSp2,getSSf
      character getPat,getTmcs,getAna,getSf,getCvar,getCcc,getCmtTc,
     $          getCmtS,getCmtIR,getCmtTs,getCmtSA
      real*8  getSd
      external getPat,getTmcs,getAna,getNmmu,getNmp,getNmd,getNmq,
     $         getNmBp,getNmBd,getNmBq,getSf,getCvar,getCcc,getCmtTc,
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
C     $         getCmtS,getCmtIR,getCmtTs,getCmtSA,getSd,getSsh,getSSp,
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 1
     $         getCmtS,getCmtIR,getCmtTs,getCmtSA,getSd,getSsh,getSSp2,
C   END OF CODE BLOCK
     $         getSSf
      real*8 getSdt,getSds,getSdc,getSdi,getSdsa,getSeCect,getSeCecSa,
     $       getRseCect,getRseCecSa,getCovt1,getCovsa1,getCovt5,
     $       getCovsa5,getT11t,getT11sa,getT112t,
     $       getT112sa,getT112x,getDaat,getDaasa
      external getSdt,getSds,getSdc,getSdi,getSdsa,getSeCect,getSeCecSa,
     $       getRseCect,getRseCecSa,getCovt1,getCovsa1,getCovt5,
     $       getCovsa5,getT11t,getT11sa,getT112t,getT112sa,getT112x,
     $       getDaat,getDaasa
      integer getLastPeriod,getLastYear,ChangeModel
      external getLastPeriod,getLastYear,ChangeModel
      integer Date2Idx,LostB,LostE
      external Date2Idx,LostB,LostE
C   LINES OF CODE ADDED FOR X-13A-S : 2
      logical dpeq
      external dpeq
C   END OF CODE BLOCK
C
C.. External Calls ..
      integer SerCount
      external SerCount
      external AUTO, CALCFX, CHECK, CHMODEL, CLOSEDEVICE,
     $         CLOSEINFILE, CONV, FCAST, GETSERIES, GETSERIENAMES,
     $         NMLSTS, OPENDEVICE, OPENDEVSCRATCH, OPENINFILE,
     $         PART, PROUT1, RACES, RATF, RPQ,
     $         SEARCH, SIGEX, STAVAL, TABLE,
     $         TAKEDETTRAMO, TRANS1, USRENTRY, VARMP, CHECKLEN
c      external SETTIME
C
C.. Intrinsic Functions ..
      intrinsic ABS, EXP, LOG, MAX, MIN, MOD, SQRT
      include 'calc.i'
      include 'calfor.i'
      include 'calshr.i'
      include 'count.i'
      include 'dirs.i'
      include 'eee.i'
      include 'dets.i'
      include 'hdflag.i'
      include 'pinno.i'
      include 'preadtr.i'
      include 'sesfcast.i'
      include 'sfcast.i'
      include 'sform.i'
      include 'sig.i'
      include 'sig1.i'
      include 'stream.i'
      include 'peaks.i'
      include 'titl.i'
      include 'unitmak.i'
c      include 'xxxs.i'
      include 'xarr.i'
      include 'logtrace.i'
      include 'buffers.i'
      include 'strmodel.i'
      include 'bench.i'
      include 'seastest.i'
      include 'date.i'
      include 'nsums.i'
      include 'sername.i'
      include 'seatserr.i'
      integer nOutPar
      common /outPar/ nOutPar

C   LINES OF CODE ADDED FOR X-13A-S : 3
      include 'error.cmn'
      include 'title.cmn'
      include 'units.cmn'
      include 'hiddn.cmn'
      include 'htmlout.cmn'
c
      integer*2 control
      include 'build.i'
C   END OF CODE BLOCK
C
C ... Executable Statements ...
C
*      call profiler(1,'in SEATS')
*      HTML = 0
      i=0
      DONE = MONE
      do i=1,kp
        forbias(i)=ZERO
      enddo
      do i=1,4*n1
        phis(i)=ZERO
      enddo
cc
c SEK, alpha removed as parameter, used like local variable
cc
      sek=THREE
      alpha=1.645d0
      nOutPar=0
      ntrace = 1
      do i=1,n10
       do j=1,n10
        c(i,j)=ZERO
       enddo
      enddo
      noretry = 0
      iprint = 0
      outf=outfile
      soutfile=outf
      Outdir = outd
      noutdir = ISTRLEN(Outdir)
      Graphdir = graphd
      Nover = nver
      Ioneout = ione
      Itable = 1
      status = 'Z'
C   LINES OF CODE COMMENTED FOR X-13A-S :  2
C      Nio = ndevice
C      Nidx = Nidevice
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 2
      Nio = Mt1
      ndevice = Mt1
*      Nprof = Mtprof
*      Nidx = 0
      nidevice = 0
*      neresid = 0
C   END OF CODE BLOCK
      Nsfcast = 0
      Nsfcast1 = 0
      opened = F
      Handle = 0
      soutdir = Outdir
      sgraphdir = Graphdir
      inover = Nover
      iioneout = Ioneout
      matopened = F
      Momopened = F
      Ierr = 0
      ntitle=0
c totales para sumSeats
      totMCS = 0
      totNA = 0
      totS = 0
      totCyc = 0
      totStocTD = 0
      totSpecFac = 0
      totACF = 0
      totCCF = 0
      totUnsSA = 0
      totUnrSA = 0
      totRevSA = 0
      totSnotSig = 0
      totBias = 0
      totCrQS = 0
      totCrSNP = 0
      totCrPeaks = 0
      auxInt=0
      remMeanMCS=F
*      call inicSumS()
*     ntltst=0
*      TtlSet=''
      call PicosReset(picosSA)
      call PicosReset(picosIr)
      call PicosReset(picosTr)
      SerSet=' '  
      Errext = ' '
      lu61=' '
C   LINES OF CODE COMMENTED FOR X-13A-S : 8
C      if (Nover .eq. 0) then
C       call SETTIME 
*CUNX#ifdef DOS
*!DEC$ IF DEFINED (DOS)      
*       write (*,'(25X,A,2X,A/)') 'SEATS Build Date :', Compdate
*CUNX#end if
*!DEC$ end if
C      end if
C   END OF CODE BLOCK
      niter = 1
      itnSearch = 0
      haveError=0
      CountError=0
      saved = F
      IsCloseToTD = F
      gudrun=Issap.lt.2.and.Irev.lt.4
C   LINES OF CODE COMMENTED FOR X-13A-S : 7
c      Time = X05BAF()
c      call OPENINFILE(infile,ifail)
c      if (ifail .ne. 0) then
c        Ierr = 1
c        Errext = 'Error opening input file'
c        go to 5028
c      end if
C   END OF CODE BLOCK
C
C READ IN DATA
C
 25   continue
      Reverse = 0
      NumSer = SerCount()
      SerSet=Infile
*        if (ifail .ne. 0) then
*         goto 5028
*        end if
*      call profiler(2,'before GETSERIENAMES')
      call GETSERIENAMES(Ttlset,Nz,Nyer,Nper,Nfreq,ifail)
      Dperiod=Nper
      Dyear=Nyer
      Dfreq=Nfreq
      numEresid = 0 
*        smtr = 0
      call LEFTTRIM(Ttlset)
      call TITLECK(Ttlset)
CC
CC
      j = ISTRLEN(Ttlset)
      do i = 1,j
        if ((Ttlset(i:i) .eq. '.') .or. (Ttlset(i:i) .eq. '"')) then
          Ttlset(i:i) = '_'
        end if
      end do
CC
C
CC
*      call profiler(2,'before Mtx1Reset')
      call Mtx1Reset()
      call Mtx2Reset()
      nround = -1
      Titleg = Ttlset
      mattitle='"'//Titleg(1:min(istrlen(Titleg),20))//'"'
      haveError=0
      if (Nz .gt. mp) goto 5026
        do i=1,mpkp
          oz(i)=ZERO
        enddo
c  note - change from SEATS 2002 - BCM
        Nzread = Nz
        Nzsave = Nz
        Nzorig = Nz
*        call profiler(2,'before GETSERIES')
        call GETSERIES(oz,Nzread,ifail)
        if (ifail .ne. 0) then
          write (*,'(//,6X,''INCORRECT NUMBER OF OBSERVATIONS'')')
          write (*,'(6X,''FOR THE SERIES : '',A,//)') Titleg
          Ierr = 1
          Errext = 'Incorrect number of observations'
          go to 6000
        end if
        Dlen=Nzread
        CkLen=0
        if ((Nz .lt. Nzread) .and. (Nz .ne. -1)) then
          CkLen=-1
        else if ((Nz.gt. Nzread) .and. (Nz .ne. -1)) then
          CkLen=1
        end if
C
C Commented in order to permit the ENTRY Handle_Point
C        do 20 while (.true.)
C
C DEFAULTS FOR NAMELIST INPUT
C
 20     if (.not. saved) then
C Modified by REG on 30 Aug 2005 to add nfixed to NMLSTS parameter list
*          call profiler(2,'before NMLSTS')
          call NMLSTS(Nochmodel,Type,Init,Ilam,Imean,P,D,Q,Bp,Bd,Bq,
     $            Sqg,Mq,M,iqm,maxit,fh,noserie,Pg,modelsumm,
     $            Out,seas,Noadmiss,OutNA,StochTD,
     $            Iter,qmax,Har,Bias,Tramo,
     $            model,Noutr,Nouir,Nous,Npatd,Npareg,interp,Rsa,
     $            Fortr,Neast,epsiv,Epsphi,ta,Xl,Rmod,
     $            blqt,tmu,Phi,Th,Bphi,Bth,thlim,bthlim,crmean,hplan,
     $            hpcycle,rogtable,centrregs,
     $            statseas,units,kunits,acfe,posbphi,printphtrf,
     $            tabtables,psieinic,psiefin,
     $            StrFobs,StrLobs,HPper,maxSpect,brol,blamda,
     $            bserie,bmid,bcMark,ODate,OLen,DetSeas,
     $            nds,Nz,nfixed,0,ifail)
          IF(Lfatal)RETURN
        end if
        InputModel=1
        if ((NumSer .gt. 250) .and. (NumSer .le. 1000)) then
          tabtables = 'p,n,s,er'
        end if
        if ((NumSer .gt. 1000) .and. (NumSer .le. 5000)) then
          tabtables = 'p,n,er'
        end if
        d_tabtables = tabtables
        nprova = 0
C
C READ IN NAMELIST INPUT
C
        if (Iter .ne. 2) then
          if (Iter .ne. 1) then
            SeasCheck = 0
C Modified by REG on 30 Aug 2005 to add nfixed to NMLSTS parameter list
*            call profiler(2,'before NMLSTS')
            call NMLSTS(Nochmodel,Type,Init,Ilam,Imean,P,D,Q,Bp,Bd,Bq,
     $            Sqg,Mq,M,iqm,maxit,fh,noserie,Pg,modelsumm,
     $            Out,seas,Noadmiss,OutNA,StochTD,
     $            Iter,qmax,Har,Bias,Tramo,
     $            model,Noutr,Nouir,Nous,Npatd,Npareg,interp,Rsa,
     $            Fortr,Neast,epsiv,Epsphi,ta,Xl,Rmod,
     $            blqt,tmu,Phi,Th,Bphi,Bth,thlim,bthlim,crmean,hplan,
     $            hpcycle,rogtable,centrregs,
     $            statseas,units,kunits,
     $            acfe,posbphi,printphtrf,
     $            tabtables,psieinic,psiefin,
     $            StrFobs,StrLobs,HPper,maxSpect,brol,blamda,
     $            bserie,bmid,bcMark,ODate,OLen,DetSeas,
     $            nds,Nz,nfixed,1,ifail)
            IF(Lfatal)RETURN
            if ((tramo .eq.0) .or. (Tramo .eq. 999))then
               FirstObs=Date2Idx(StrFobs)
            if (FirstObs .eq. -1) then
              FirstObs=1
            end if
            LastObs=Date2Idx(StrLobs)
          else
            FirstObs=1
            LastObs=-1
          end if
          if (bias.eq.-1 .and. MQ.ne.12) then
            bias=1
          end if
           if (bias.eq.-1 .and. FH.gt.30) then
c            FH=30
          end if
          if (iter .eq. 1) then
            do i = 1,NZ
              backOZ(i)=oz(i)
            end do
          end if
          if (iter .eq. 1) then
            do i = 1,NZ
              backOZ(i)=oz(i)
            end do
          end if
          if (OUT .eq. -1) then
            if (ITER .ge. 2) then
              if (NumSer .le. 5) Then
                out=0
              else if (numser .le. 25) then
                out=1
              else   
                OUT=2
              end if 
            else
              OUT=0
            end if
          end if
          if (ifail .ne. 0) then
            Ierr = 1
            Errext = 'Error reading SEATS parameters'
*            call profiler(2,'**GO TO 5024**, line 535')
            goto 5024
          end if
        else
          nz=nzsave
          do  i=1,nz
            oz(i)=backoz(i)
          end do
          SeasCheck = 0
C Modified by REG on 30 Aug 2005 to add nfixed to NMLSTS parameter list
*          call profiler(2,'before NMLSTS')
          call NMLSTS(Nochmodel,Type,Init,Ilam,Imean,P,D,Q,Bp,Bd,Bq,
     $            Sqg,Mq,M,iqm,maxit,fh,noserie,Pg,modelsumm,
     $            Out,seas,Noadmiss,OutNA,StochTD,
     $            Iter,qmax,Har,Bias,Tramo,
     $            model,Noutr,Nouir,Nous,Npatd,Npareg,interp,Rsa,
     $            Fortr,Neast,epsiv,Epsphi,ta,Xl,Rmod,
     $            blqt,tmu,Phi,Th,Bphi,Bth,thlim,bthlim,crmean,hplan,
     $            hpcycle,rogtable,centrregs,
     $            statseas,units,kunits,
     $            acfe,posbphi,printphtrf,
     $            tabtables,psieinic,psiefin,
     $            StrFobs,StrLobs,HPper,maxSpect,brol,blamda,
     $            bserie,bmid,bcMark,ODate,OLen,DetSeas,
     $            nds,Nz,nfixed,1,ifail)
          IF(Lfatal)RETURN
          if ((tramo .eq.0) .or. (Tramo .eq. 999))then
            FirstObs=Date2Idx(StrFobs)
            if (FirstObs .eq. -1) then
              FirstObs=1
            end if
            LastObs=Date2Idx(StrLobs)
          else
            FirstObs=1
            LastObs=-1
          end if
          if (bias.eq.-1 .and. MQ.ne.12) then
            bias=1
          end if
           if (bias.eq.-1 .and. FH.gt.30) then
c            FH=30
          end if
          if (ITER .ge. 2) then
            if (NumSer .le. 5) Then
              out=0
            else if (numser .le. 25) then
              out=1
            else
              OUT=2
            end if 
          else
            OUT=0
          end if
        end if
        if (ifail .ne. 0) then
*           call profiler(2,'**GO TO 5028**, line 590')
           goto 5028
        end if
      end if
c aqui hacemos validaciones de parametros y cortamos la serie en funcion de
c firstobs y lastobs
      if ((NumSer .gt. 25).and.(modelsumm.eq.-1)) then
        modelsumm = 1
      end if
      if (firstObs .lt. 1) then
        firstobs=1
*           write (7,'(/,2x,
*     &          ''FirstObs lower than 1!"'',/,
*     &          2x,''Firstobs and lastobs set to 1'')')
      end if 
      if (lastobs .ne. -1) then
        if (Firstobs .ge. Lastobs) then
c            write (nio,'(/,2x,
c     &          ''FirstObs greater than lastobs"'',/,
c     &          2x,''Firstobs and lastobs set to the default value'')')
c           StrFobs='00-0000'
c           StrLobs='00-0000'
          firstobs = 1
          lastobs =-1         
        end if
      end if
C         end if
c    cortamos la serie   
      inicBucle = firstObs
      endBucle = lastObs
      if ((nz .lt. lastobs) .or. (lastobs .eq. -1)) then
        endbucle = nz
      end if
      if (firstobs .lt.1)  then
        inicBucle= 1
      end if
      if (inicBucle .ge. endBucle) then
        inicBucle = 1
        endbucle = nz
      end if
      do i=inicBucle,endBucle
        OZ(i-inicBucle+1) = Oz(i)
      enddo
      Nz = endBucle - inicBucle + 1
*         Nzsave = Nz
*         Nzread = Nz
         nper = nper + inicBucle - 1
      do while (nper .gt. nfreq)
        nper = nper - nfreq
        nyer = nyer + 1
      end do
c        
      if ((tabtables .eq. d_tabtables) .and. (hpcycle .gt. 0)) then
        if ((NumSer .gt. 250) .and. (NumSer .le. 1000)) then
          tabtables = 'p,n,s,er,cy'
        end if
        if ((NumSer .gt. 1000) .and. (NumSer .le. 5000)) then
          tabtables = 'p,n,er,cy'
        end if
        d_tabtables = tabtables
      end if

cc
c Set Default value for TabTables
cc      
      Mq = Nfreq
      if ((Ilam .ne. 0) .and. (Ilam .ne. 1)) then
        Ilam=1
      end if
      if (Mq .ge. 4) then
        Nfreq = Mq
      else
C ORIGINALLY
C        NFREQ=4
        Nfreq = Mq
      end if
C
C Comment the next 3 lines in TSW
C
      if ((Iter.ne.0) .and. (Out.eq.3)) then
        Ioneout = 1
      end if
      Nreestimated = 0
      if (Init .ne. 2) then
        Nreestimated = -1
      end if
      if ((Tramo.eq.999) .or. (Tramo.eq.0)) then
        Tramo = 0
        npread = 0
      else
        npread = 1
      end if
      if (npread .ge. 1) then
        call setPat('Y')
      else
        call setPat('N')
      end if
      Ipr = Out
      if (Mq .eq. 1) then
        Bd = 0
        Bq = 0
      end if
      if (Rsa .gt. 0) then
        if (Noadmiss .eq.0) then
          Noadmiss = 1
        end if
        Rsa = 0
      end if
C
C HERE INTRODUCE THE PART TO READ THE DETERMINISTIC COMPONENT FROM TRAMO
C
*      itab=0
*      iId=0
*      iAkey=1
        if (nz.gt.1 .and. Tramo.ne.0)then
          nf = MAX(fh,MAX(8,2*Mq+6))
*        call profiler(2,'before TAKEDETTRAMO')
          call TAKEDETTRAMO(Tram,Paoutr,Paouir,Paous,Paeast,Patd,Neff,
     $                    Pareg,Tse,Npareg,Nz,nf,Ilam,ifail)
          call usrentry(Tram,1,Nz,1,mpkp,1213)
C   LINES OF CODE COMMENTED FOR X-13A-S : 5
*          if (ifail .eq. 1) then
*           Ierr = 1
*           Errext = 'Error reading Preadjustment components from TRAMO'
*           goto 5024
*          end if
C   END OF CODE BLOCK 
*            write (nio,9999)
* 9999       format(/,2x,'<p>Set TramLin and TramDet</p>')
          if (ILam.eq.0) then
           do i=1,NZ+nf
            TramDet=PaOuTr(i)*PaOuIr(i)*PaOus(i)*PaEast(i)*PaTD(i)
            do j=0,7
             TramDet=TramDet*PaReg(i,j)
            enddo
            TramLin(i)=Tram(i)/TramDet
           enddo
          else 
           do i=1,NZ+nf
            TramDet=PaOuTr(i)+PaOuIr(i)+PaOus(i)+PaEast(i)+PaTD(i)
            do j=0,7
             TramDet=TramDet+PaReg(i,j)
            enddo
            TramLin(i)=Tram(i)-TramDet
           enddo
          endif
        endif
        if (Tramo .gt. 0) then
         if (Neff(6) .eq. 1) then
          if (Ilam .eq. 0) then
            do i = 1,Nz+nf
              Pareg(i,2) = Pareg(i,2) * Pareg(i,6)
            end do
          else
            do i = 1,Nz+nf
              Pareg(i,2) = Pareg(i,2) + Pareg(i,6)
            end do
          end if
          Neff(2) = 1
        end if
        j0 = 1
        if (Nper .ne. 1) then
          j0 = Mq + 2 - Nper
        end if
        nk = (Nz+nf-j0+1) / Mq
        tmean = ZERO
*          if (Neast .eq. 1) then
*           sum = 0.0d0
*           if (Ilam .eq. 0) then
*            do i = j0,nk*Mq+j0-1
*             sum = sum + LOG(Paeast(i))
*            end do
*           else
*            do i = j0,nk*Mq+j0-1
*             sum = sum + Paeast(i)
*            end do
*           end if
*           sum = sum / (nk*Mq)
*           if (Ilam .eq. 0) then
*            do i = 1,Nz+nf
*             Paeast(i) = EXP(LOG(Paeast(i))-sum)
*            end do
*           else
*            do i = 1,Nz+nf
*             Paeast(i) = Paeast(i) - sum
*            end do
*           end if
*           tmean = tmean + sum
*          end if
C
*          if (Npatd .eq. 1) then
*           sum = 0.0d0
*           if (Ilam .eq. 0) then
*            do i = j0,nk*Mq+j0-1
*             sum = sum + LOG(Patd(i))
*            end do
*           else
*            do i = j0,nk*Mq+j0-1
*             sum = sum + Patd(i)
*            end do
*           end if
*           sum = sum / (nk*Mq)
*           if (Ilam .eq. 0) then
*            do i = 1,Nz+nf
*             Patd(i) = EXP(LOG(Patd(i))-sum)
*            end do
*           else
*            do i = 1,Nz+nf
*             Patd(i) = Patd(i) - sum
*            end do
*           end if
*           tmean = tmean + sum
*          end if
          if (Neff(2) .eq. 1) then
            if(centrregs .eq. 1) then
              sum = ZERO
              if (Ilam .eq. 0) then
                do i = j0,nk*Mq+j0-1
                sum = sum + LOG(Pareg(i,2))
              end do
            else
              do i = j0,nk*Mq+j0-1
                sum = sum + Pareg(i,2)
              end do
            end if
            sum = sum / (nk*Mq)
            if (Ilam .eq. 0) then
              do i = 1,Nz+nf
                Pareg(i,2) = EXP(LOG(Pareg(i,2))-sum)
              end do
            else
              do i = 1,Nz+nf
                Pareg(i,2) = Pareg(i,2) - sum
              end do
            end if
            tmean = tmean + sum
          else
            remMeanMCS=T
          end if  
        end if
C
C CENTER THE REGRESSION CALENDAR EFFECT
C
        if (Neff(6) .eq. 1) then
          sum = ZERO
          if (Ilam .eq. 0) then
            do i = j0,nk*Mq+j0-1
              sum = sum + LOG(Pareg(i,6))
            end do
          else
            do i = j0,nk*Mq+j0-1
              sum = sum + Pareg(i,6)
            end do
          end if
          sum = sum / (nk*Mq)
          if (Ilam .eq. 0) then
            do i = 1,Nz+nf
              Pareg(i,6) = EXP(LOG(Pareg(i,6))-sum)
            end do
          else
            do i = 1,Nz+nf
              Pareg(i,6) = Pareg(i,6) - sum
            end do
          end if
        end if
C
C
C
        if (ABS(tmean) .gt. 1.0d-8) then
          Imean = 1
          if (Ilam .eq. 0) then
             do i = 1,Nz
               oz(i) = EXP(LOG(oz(i))+tmean)
             end do
          else
            do i = 1,Nz
              oz(i) = oz(i) + tmean
            end do
          end if
        end if
      else
        neff(0) = 0
        neff(1) = 0
        neff(2) = 0
        neff(3) = 0
        neff(4) = 0
        neff(5) = 0
        neff(6) = 0
        neff(7) = 0
        if (Ilam .eq. 0) then
          do i = 1,nz+fh
            do j = 0,7
              pareg(i,j) = ONE
            end do
            PaouTR(i)=ONE
            PaouIR(i)=ONE
            PaouS(i)=ONE
            PaEast(i)=ONE
            PaTD(i)=ONE
          end do
        else
          do i = 1,nz+fh
            do j = 0,7
              pareg(i,j) = ZERO
            end do
            PaouTR(i)=ZERO
            PaouIR(i)=ZERO
            PaouS(i)=ZERO
            PaEast(i)=ZERO
            PaTD(i)=ZERO
          end do
        end if
        call usrentry(Oz,1,Nz,1,mpkp,1213)
      end if
C
C CHANGE THE SIGN OF INPUT PARAMETERS FROM TRUE SIGN IN B-J SIGN
C
C
C WHEN ITER=2 WE SAVE THE NAMELIST INPUT IN AN INTERNAL FILE
C IN ORDER TO RE-READ IT. WHEN THE INTERNAL FILE IS CLOSED IT IS
C AUTOMATICALLY DELETED
C
*      if ((Iter.eq.2) .and. (.not.saved)) then
*C Modified by REG on 30 Aug 2005 to add nfixed to NMLSTS parameter list
*        call NMLSTS(Nochmodel,Type,Init,Ilam,Imean,P,D,Q,Bp,Bd,Bq,
*     $              Sqg,Mq,M,iqm,maxit,fh,noserie,Pg,modelsumm,
*     $              Out,seas,Noadmiss,OutNA,StochTD,Iter,qmax,Har,Bias,
*     $              Tramo,model,Noutr,Nouir,Nous,Npatd,Npareg,interp,
c     $              Rsa,Fortr,Neast,epsiv,Epsphi,ta,Xl,Rmod,
c     $              blqt,tmu,Phi,Th,Bphi,Bth,thlim,bthlim,
c     $              crmean,hplan,hpcycle,rogtable,
c     $              centrregs,statseas,units,kunits,
c     $              acfe,posbphi,printphtrf,tabtables,psieinic,
c     $              psiefin,
*     $              StrFobs,StrLobs,HPper,maxSpect,brol,blamda,
c     $              bserie,bmid,bcMark,ODate,OLen,DetSeas,
*     $              nds,Nz,nfixed,2,ifail)
*        IF(Lfatal)RETURN
*          saved = .true.
*        end if
CC
C
CC
        IsOk=0
        if ((NOSERIE .eq. 0) .and. 
     $   .not. ((NZ .eq. 1) .and. (Nyer+Nper+Nfreq .eq. 0))) then
*          call profiler(2,'before CheckLen')
          call CheckLen(OZ,NZ,Mq,IsOk)
        else
          IsOk=1
        end if
        if  (IsOk .eq. 0) then
           Dstdres(ntrace) = -88888.88
           ntrace = ntrace + 1
           TrTitle(ntrace) = titleg
*           call profiler(2,'**GO TO 5119**, line 943')
           goto 5119
        end if
C
C Open the matrix files
C
        if ((Iter .gt. 0) .and. (.not. Momopened)) then
          filename=Outdir(1:ISTRLEN(Outdir)) //'\\moments\\acfes.html'
          call OPENDEVICE (filename,80,0,ifail)
          filename=Outdir(1:ISTRLEN(Outdir)) //'\\moments\\vares.html'
          call OPENDEVICE (filename,81,0,ifail)
          filename=Outdir(1:ISTRLEN(Outdir)) //'\\moments\\ccfes.html'
          call OPENDEVICE (filename,82,0,ifail)
          Momopened = T
        end if
        if ((Iter .eq. 0) .and. (.not.matopened) .and. gudrun) then
          nouts=ISTRLEN(Outdir)
          filename=' '
c          if (nouts.gt.0) THEN
c             filename=Outdir(1:ISTRLEN(Outdir)) // '\\summarys.html'
c          else
             filename = Cursrs(1:Nfilcr)//'_sum.html'
c          end if
          call OpenSummary(65,filename,ifail)
c             call OPENDEVICE (filename,65,0,ifail)
          if (ifail.gt.0) THEN
            Ierr=ifail
            Errext='Error opening '//filename
            RETURN
          END IF
c          call Mtx1Reset()
c          call Mtx2Reset()
          CALL writTagOneLine(65,'h1','center',mattitle(1:22))
          write (65, 6501)Nz,Nper,Nyer,
     $                  getLastPeriod(Nz,Nper,Nyer,Mq),
     $                  getLastYear(Nz,Nper,Nyer,Mq),Mq
 6501     FORMAT('<p><strong>NZ =</strong>',I3.3,'<strong>;</strong>',
     $           ' <strong>PERIOD=</strong>',I2.2,'-',I4.4,'/',I2.2,'-',
     $           I4.4,'; <strong>MQ=</strong>',I2.2,
     $           '<strong>;</strong></p>')
*            call profiler(2,'before NMOut')
          call NMOut(Type,Init,Ilam,Imean,P,D,Q,Bp,Bd,Bq,Sqg,Mq,M,iqm,
     $               maxit,fh,noserie,Pg,Out,seas,
     $               Noadmiss,StochTD,
     $               qmax,Har,Bias,model,Noutr,
     $               Nouir,Nous,Npatd,Npareg,interp,Rsa,Fortr,
     $               Neast,epsiv,Epsphi,ta,Xl,Rmod,blqt,tmu,
     $               Phi,Th,Bphi,thlim,bthlim,crmean,hplan,hpcycle,
     $               rogtable,centrregs,statseas,
     $               units,kunits,acfe,posbphi,Nochmodel,
     $               printphtrf,tabtables,d_tabtables,psieinic,psiefin,
     $               StrFobs,StrLobs,HPper,maxSpect,
     $               brol,blamda,bserie,bmid,bcMark,Nzorig)
          matopened=T
        else if ((Iter .gt. 0).and. (.not.matopened)) then
c nuevos ficheros para componentes          
            call OpenCompMatrix(ifail,12)
            call OpenClasicMatrix(ifail)
            if ((mq.eq.12) .or. (mq.eq.4)) then
              call openPeakHtm2(ifail)          
            end if
          matopened=T
        end if
        if (niter.eq.1) then
          wSrmod=rmod
          wSposBphi=posBphi  
          wSxl=xl
          wSstochTD=StochTD
          wSstatseas=statseas
        else
          if (wSrmod.ne.rmod) then
            wSrmod=-9.99
          end if
          if (wSposBphi.ne.posBphi) then 
            wSposBphi=-9
          end if 
          if (wSxl.ne.xl) then
            wSxl=-9.99
          end if
          if (wSstochTD.ne.StochTD) then
            wSstochTD=-9
          end if
          if (wSstatseas.ne.statseas) then
            wSstatseas=-9          
          end if
        end if
        do i = 1,P
          Phi(i) = -Phi(i)
        end do
        do i = 1,Q
          Th(i) = -Th(i)
        end do
        if (Bq .gt. 0) then
          Bth(1) = -Bth(1)
        end if
        if (Bp .gt. 0) then
          Bphi(1) = -Bphi(1)
        end if
C

C   LINES OF CODE COMMENTED FOR X-13A-S : 8
C         if ((Iter.ne.0) .and. (Ioneout.eq.0) .and. (out.ne.3)) then
C          filename = Graphdir(1:ISTRLEN(Graphdir)) //
C     $               '\SERIES\GRAPH.LST'
CC          call OPENDEVICE(filename,17,0,ifail)
C          filename = Graphdir(1:ISTRLEN(Graphdir)) //
C     $               '\FORECAST\GRAPH.LST'
C          call OPENDEVICE(filename,27,0,ifail)
C         end if
C   END OF CODE BLOCK          
c   el fichero rogtable escribia a fichero cerrado con out=3
c change Jan.20201 if there is sliding span or history,not write to .rog
         if ((out.eq.3).or.(.not. gudrun)) then
          rogtable=0
         end if
c
        if ((rogtable.eq.1).and. (out.lt.3).and. 
     &     ((Iter.eq.0) .or. (niter.le.1))) then
C   LINES OF CODE ADDED FOR X-13A-S : 5
          filename = Cursrs(1:Nfilcr)//'_rog.html'
          noutdir = ISTRLEN(filename)
C   END OF CODE BLOCK
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
C          filename = Outdir(1:ISTRLEN(Outdir)) // '\ROGTABLE.OUT'
C   END OF CODE BLOCK
          call OPENDEVICE(filename,54,0,ifail)
          CALL mkHead(54,filename,'Rates of Growth Matrix',F,1,1,F)
        end if
        if ((iter.ne.0).and.(OUT .eq. 3)) then
          Itable = 0
        end if
        if ((tabtables(1:1) .eq. '')
     $       .or.((iter.gt.0).and.(out.eq.3))) then
          itable = 0
        end if
C   LINES OF CODE COMMENTED FOR X-13A-S : 2
C         call OPENFILE(Iter,Ttlset,tout,Ioneout,Out,opened,Outdir,
C     $                 outf,noserie,Itable,niter)
C   END OF CODE BLOCK          
        if ((itable .eq. 1) .and. (ITER .le. 2)) then
          call ProcTables(tabtables)
        end if
C   LINES OF CODE ADDED FOR X-13A-S : 9
        if (Out.eq.3) then
          CALL opendevscratch(99)
          Nio=99
          Ndevice=99
        end if
        if (Itable .eq. 1) then
C   LINES OF CODE ADDED FOR X-13A-S : 5
          filename = Cursrs(1:Nfilcr)//'_tbs.html'
C   END OF CODE BLOCK
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
C          filename = outdir(1:ISTRLEN(Outdir)) // '\TABLE-S.OUT'
C   END OF CODE BLOCK
          call OPENDEVICE(filename,36,0,ifail)
          CALL mkHead(36,filename,'Table-s.out',F,1,1,F)
        end if
C   END OF CODE BLOCKCUNX #ifdef TSW
!DEC$ IF DEFINED (TSW)
        if ((ITER .eq. 1) .and. (TRAMO .eq.0) ) then
          if (niter .gt. 1) then
            write (TITLEG,'(''M'',i4.4,a)')niter,
     $      TITLEG(6:ISTRLEN(TITLEG))
          else
            write (TITLEG,'(''M'',i4.4,a)')niter,
     $      TITLEG(1:ISTRLEN(TITLEG))
          end if
        end if
CUNX #end if
!DEC$ END IF 

CUNX#ifdef DOS
!DEC$ IF DEFINED (DOS)      
c         write (*,'(a)') titleg
c         if ((Iter.ne.0) .and. (Nover.eq.0)) then
c          write (*,'(4X,''ITERATION : '',I3)') niter
c         end if
CUNX#end if
!DEC$ end if
C
c inicializamos itab,iId
*        itab=0
*        iId=0       
        if ((NZ .eq. 1) .and. (Nyer+Nper+Nfreq .eq. 0)) then
          TrTitle(ntrace) = titleg
          if (oz(1) .eq. ONE) then
            Dstdres(ntrace) = -11111.11
          else if (oz(1) .eq. TWO) then
            Dstdres(ntrace) = -22222.22
          else
            Dstdres(ntrace) = -33333.33
          end if
          ntrace = ntrace + 1
          if ((matopened) .and. (iter .gt. 0)) then
            noTratadas=noTratadas+1
            if (oz(1) .eq. ONE) then
              errch = '*T'
*              call usrentry(oz,1,1,-3)
*              call ErrorLog(HTML,
*     &                 'Series No Treated in Tramo for Error',1)
            else if (oz(1) .eq. TWO) then
              errch = '$T'
*              call usrentry(oz,1,1,-3)
*              call ErrorLog(HTML,'Series No Treated in Tramo',1)
            else
              errch = '#T'
*              call usrentry(oz,1,1,-3)
*              call ErrorLog(HTML,
*     $                 'Model especification error detected in Tramo',1)
            end if
*            call profiler(2,'before MTX1RESET')
            call MTX1RESET
            call MTX2RESET

              write (65,6600)
     $          niter, errch, mattitle(1:22),'u','u', 'u', 
     $          -1, -1, -1, -1, -1, -1, -1,DONE, DONE,'u','u','u','u',
     $          'u', 'u', 'u', 'u'
 6600         FORMAT('<tr><td>',i4,a,'</td>',
     $               4('<td>',a,'</td>'),7('<td>',i2,'</td>'),
     $               2('<td>',f9.0,'</td>'),
     $               8('<td>',a,'</td>'),'</tr>' )
*            call profiler(2,'before OutNoPar')
              call OutNoPar(74,niter,mattitle)
              write (66,6601)
     $          niter, errch, mattitle(1:22), DONE, 
     $          DONE, DONE, DONE, DONE, DONE, DONE,DONE,DONE,
     $          DONE, DONE, DONE, DONE, DONE
 6601         FORMAT('<tr><td>',i4,a,'</td><td>',a,'</td>',
     $               14('<td>',f9.0,'</td>'),'</tr>')
              write (67,6701)
     $          niter, errch, mattitle(1:22),DONE, 
     $          DONE, DONE, DONE, -1, -1, -1, DONE, DONE
 6701         FORMAT('<tr><td>',i4,a,'</td><td>',a,'</td>',
     $               4('<td>',f9.0,'</td>'),3('<td>',i2,'</td>'),
     $               2('<td>',f9.0,'</td>'),'</tr>')
              if ((mq.eq.12) .or. (mq.eq.4)) then 
*                call profiler(2,'before PicosReset')
                call PicosReset(picosSA)
                call wrLnTabPeaks(69,niter,matTitle,picosSA,1)
                call PicosReset(picosIr)
                call wrLnTabPeaks(72,niter,matTitle,picosIr,1)
                call PicosReset(picosTr)
                call wrLnTabPeaks(73,niter,matTitle,picosTr,1)
              end if
          end if
          if (itable .eq.1 ) then
*            call profiler(2,'before OUTTABLE2')
            call OUTTABLE2(Titleg,tram,trtemp,satemp,stemp,irtemp,temp,
     $                  pretemp,caltemp,eresid,numEresid,temp,temp,0,
     $                  ilam,1,NZ,mq,2,SUNITS,0,trtemp,satemp,satemp,
     $                  F)
          end if
          niter = niter +1
          itnSearch = 0
          if (Iter .eq. 1) then
*            call profiler(2,'**GO TO 20**, line 1199')
            goto 20
          else 
*            call profiler(2,'**GO TO 25**, line 1202')
            goto 25
          end if
        end if
      
        if ((Noadmiss.ne.0) .and. (Noadmiss.ne.1) .and. 
     $      (Noadmiss.ne.-1)) then
          Noadmiss = 0
        end if
*         if (fh .gt. 24) then
*          fh = 24
*         end if
        if (noserie .eq. 1) then
          Init = 2
          Noadmiss = 0
          Ilam = 1
        end if
*        if ((Pg .eq. 0) .and. (Noserie .eq. 0)) then
*          if (iter.eq.0) then
*            if (out.lt.3) then
*              fname = 'XLIN.T'
*              if (Tramo.eq.0) then
*                subtitle = 'ORIGINAL SERIES'
*              else
*                subtitle = 'LINEARIZED SERIES'
*              end if
*              call PLOTSERIES(fname,subtitle,oz,Nz,1,0.0d0)
*            end if
*          else
*            if ((Ioneout.eq.0) .and.(out.le.1)) then    
*              ntltst = ISTRLEN(Ttlset)
*              if (Tramo .eq. 0) then
*                if (out.lt.2) then
*                  fname = Ttlset(1:ntltst) // '.SER'
*                  subtitle = 'ORIGINAL SERIES'
*                  inquire (FILE = fname,EXIST = bool)
*                  if (.not.bool) then
*                    call PLOTSERIES(fname,subtitle,oz,Nz,1,0.0d0)
*                    write (17,'(A)') fname
*                  end if
*                end if
*c             else      Y se escribe en tramo
*c             if (out.eq.0) then
*c               fname = Title(1:ntltst) // '.SER'
*c             subtitle = 'ORIGINAL SERIES FROM TRAMO'
*c             inquire (FILE = fname,EXIST = bool)
*c             if (.not.bool) then
*c               call PLOTSERIES(fname,subtitle,Tram,Nz,1,0.0d0)
*c               write (17,'(A)') fname
*c             end if
*c             end if
*              end if
*            end if
*          end if
*        end if
C
        maxf = maxit * 10
C
C OUTPUT TITLE AND CHOOSE TYPE OF ESTIMATION PROCEDURE
C
        if ((.not.opened) .and. (Out.eq.0)) then
C      IF ((.NOT.OPENED).AND.(OUT.EQ.3)) WRITE(NOU3,901)
*            call profiler(2,'before IntrodH')
            call IntrodH(nio)
        end if
        if (fh .ge. NZ-1) then
          fh = NZ-2
          ipos=1
          CALL itoc(fh,str,ipos)
          CALL nWritln('FORECAST HORIZON &gt; NZ. FH set to '//
     &                 str(1:(ipos-1))//'.',Nio,0,T,T)
        end if
        if (CkLen .lt.0) then
          ipos=1
          CALL itoc(Nz,str,ipos)
          CALL wWritln('The value entered for NZ is smaller '//
     $         'than the number of observations in the series.'//Cbr,
     $         Nio,0,T,F)
          CALL writln('The program will use '//str(1:(ipos-1))//
     $                ' observations.',Nio,0,F,T)
        else if (CkLen .gt.0) then
          ipos=1
          CALL itoc(Nz,str,ipos)
          CALL wWritln('The value entered for NZ is greater '//
     $         'than the number of observations in the series.'//Cbr,
     $         Nio,0,T,F)
          CALL writln('The program will use '//str(1:(ipos-1))//
     $                ' observations.',Nio,0,F,T)
        end if
        if (Ioneout .eq. 1) then
          opened = T
        end if
C         if ((Iter.ne.0) .and. (Ioneout.eq.1)) then
CC   LINES OF CODE ADDED FOR X-13A-S : 6
C          if (noutdir.gt.0) then
C           filename = Outdir(1:ISTRLEN(Outdir)) // outf(1:ISTRLEN(outf))
C     $                // '.CMP'
C          ELSE
C           filename = outf(1:ISTRLEN(outf))//'.cmp'
C          END IF
CC   END OF CODE BLOCK
CC   LINES OF CODE COMMENTED FOR X-13A-S : 6
Cccdos
Cc           filename = Outdir(1:ISTRLEN(Outdir)) // '\\' //
Cc     $                outf(1:ISTRLEN(outf)) // '.CMP'
Cccunix
Ccc           filename = Outdir(1:ISTRLEN(Outdir)) // '/' //
Ccc     $                outf(1:ISTRLEN(outf)) // '.CMP'
CC   END OF CODE BLOCK
C          call OPENDEVICE(filename,22,0,ireturn)
C         end if
        if (Out .eq. 0) then
*            itab=itab+1
          CALL genSkip(1001)
          CALL writTagOneLine(Nio,'h2','@','PART 1 : ARIMA ESTIMATION')
        end if
C
         if ((Iter.eq.2) .and. (.not.saved)) then
*         call profiler(2,'before NMCHECK')
          call NMCHECK (Type,Init,ILam,Imean,P,D,Q,Bp,Bd,Bq,Sqg,Mq,M,
     $          iqm,maxit,fh,noserie,Pg,Out,seas,
     $          Noadmiss,OutNA,StochTD,
     $          Iter,qmax,Har,Bias,Tramo,model,Noutr,Nouir,
     $          Nous,Npatd,Npareg,interp,Rsa,Fortr,Neast,
     $          epsiv,Epsphi,Xl,Rmod,thlim,bthlim,crmean,hplan,hpcycle,
     $          rogtable,centrregs,statseas,units,
     $          acfe,posbphi,nochmodel,
     $          tabtables,d_tabtables,psieinic,psiefin,
     $          StrFobs,StrLobs,HPper,brol,blamda,
     $          bserie,bmid,bcMark,Nzorig)
C WHEN ITER=2 WE SAVE THE NAMELIST INPUT IN AN INTERNAL FILE
C IN ORDER TO RE-READ IT. WHEN THE INTERNAL FILE IS CLOSED IT IS
C AUTOMATICALLY DELETED
C
*         call profiler(2,'before NMLSTS')
         call NMLSTS(Nochmodel,Type,Init,Ilam,Imean,P,D,Q,Bp,Bd,Bq,
     $            Sqg,Mq,M,iqm,maxit,fh,noserie,Pg,modelsumm,
     $            Out,seas,Noadmiss,OutNA,StochTD,
     $            Iter,qmax,Har,Bias,Tramo,
     $            model,Noutr,Nouir,Nous,Npatd,Npareg,interp,Rsa,
     $            Fortr,Neast,epsiv,Epsphi,ta,Xl,Rmod,
     $            blqt,tmu,Phi,Th,Bphi,Bth,thlim,bthlim,crmean,hplan,
     $            hpcycle,rogtable,centrregs,
     $            statseas,units,kunits,
     $            acfe,posbphi,printphtrf,
     $            tabtables,psieinic,psiefin,
     $            StrFobs,StrLobs,HPper,maxSpect,brol,blamda,
     $            bserie,bmid,bcMark,ODate,OLen,DetSeas,
     $            nds,Nz,nfixed,2,ifail)
          saved = T
         else
*         call profiler(2,'before NMCHECK')
          call NMCHECK (Type,Init,ilam,Imean,P,D,Q,Bp,Bd,Bq,Sqg,Mq,M,
     $          iqm,maxit,fh,noserie,Pg,Out,seas,
     $          Noadmiss,OutNA,StochTD,
     $          Iter,qmax,Har,Bias,Tramo,model,Noutr,Nouir,
     $          Nous,Npatd,Npareg,interp,Rsa,Fortr,Neast,
     $          epsiv,Epsphi,Xl,Rmod,thlim,bthlim,crmean,hplan,hpcycle,
     $          rogtable,centrregs,statseas,units,
     $          acfe,posbphi,nochmodel,
     $          tabtables,d_tabtables,psieinic,psiefin,
     $          StrFobs,StrLobs,HPper,brol,blamda,
     $          bserie,bmid,bcMark,Nzorig)
         end if
*        call profiler(2,'before PROUT1')
        call PROUT1(Mq,Ilam,Type,Ioneout,Nz,Titleg,Tramo,interp,Init,P,
     $               D,Q,Bd,Bp,Bq,Out,Nper,Nyer,npread)
        if (Type .eq. 0) then
          if (Out .eq. 0) then
           CALL mkPOneLine(Nio,'em','METHOD: MAXIMUM LIKELIHOOD')
          end if
        else if (Type .eq. 1) then
          if (Out .eq. 0) then
           CALL mkPOneLine(Nio,'em','METHOD: CONSTRAINED LEAST SQUARES')
          end if
        else
*          call profiler(2,'**GO TO 5025**, line 1380')
          goto 5025
        end if
        if (Out.eq.0) then
          if (noserie.ne.1) then
            write (Nio,1010) Nz
 1010       format ('<p><em>NO OF OBSERVATIONS =</em>',I3,'</p>')
          end if
          if (Firstobs .gt. 1) then
            write(Nio,1020)'FirstObs'
            write(Nio,1030)'First',Firstobs-1
 1020       format('<p><em>Due to ',a,' parameter: ')
 1030       format(a,'(',i3.3,') observations in the original ',
     $             ' series have been removed.</em></p>')
          end if
          if ((lastobs.ne.-1).and.
     $        (LastObs .gt. 0) .and.((Dlen-LastObs).gt.0)) then
            write(Nio,1020)'LastObs'
            write(Nio,1030)'Last',Dlen-LastObs
          end if
          if ((Tramo .ne.0) .and. (Tramo .ne.999)) then
           lost = LostB()
           if (lost .gt.0) then           
            write(Nio,1020)'FirstObs'
            write(Nio,1030)'First',lost
          end if
           lost = LostE()
           if (lost .gt.0) then           
            write(Nio,1020)'LastObs'
            write(Nio,1030)'Last',lost
           end if
        end if
c        
c        
        if ((Tramo.eq.1) .and. (interp.eq.1)) then
            call nWritln('MISSING OBSERVATIONS IN ORIGINAL SERIES'//
     $                   ' HAVE BEEN INTERPOLATED',Nio,0,T,T)
        end if
C   LINES OF CODE COMMENTED FOR X-13A-S : 8
C         if (Out .eq. 0) then
C          if (kunits .ne. 0) then
C           write (Nio, '(''<p>'',A,A,i2,A)')
C     $        'TRAMO modified the original ',
C     $        'series by multiplying them by 10**',3*kunits,'.'
C            write (Nio,'(A)')
C     $       '<br>SEATS will preserve this modification.</p>'
Cc            call Enote(nio)
C          end if
C         end if
C   END OF CODE BLOCK 
        if ((Tramo.eq.1) .and. (noserie.eq.0)) then
*            call profiler(2,'before prttbl(Tram)')
          thisDate(1)=Nyer
          thisDate(2)=Nper
          CALL genSkip(1100)
          if(Ndec.eq.0)THEN
           call prttbl(thisDate,Mq,Tram,Nz,
     $                 ' ORIGINAL UNCORRECTED SERIES (from regARIMA)',
     $                 3,'b1.seats')
          else
           call prttbl(thisDate,Mq,Tram,Nz,
     $                 ' ORIGINAL UNCORRECTED SERIES (from regARIMA)',
     $                 ndec,'b1.seats')
          end if
          CALL genSkip(1101)
          if (Ilam .eq. 0) then
            do i = 1,Nz
              bz(i) = (Tram(i)/oz(i)) * ONEHND
            end do
*              call profiler(2,'before prttbl(bz)')
            if(Ndec.eq.0)THEN
             call prttbl(thisDate,Mq,bz,Nz,
     $                  'PREADJUSTMENT FACTORS Outliers and ' //
     $                  'Other Deterministic Effects',3,'adj.seats')
            else
             call prttbl(thisDate,Mq,bz,Nz,
     $                   'PREADJUSTMENT FACTORS Outliers and ' //
     $                   'Other Deterministic Effects',ndec,'adj.seats')
            end if
          else
            do i = 1,Nz
              bz(i) = Tram(i) - oz(i)
            end do
*              call profiler(2,'before prttbl(bz)')
            if(Ndec.eq.0)THEN
             call prttbl(thisDate,Mq,bz,Nz,
     $                   'PREADJUSTMENT FACTORS Outliers and ' //
     $                   'Other Deterministic Effects',3,'adj.seats')
            else
             call prttbl(thisDate,Mq,bz,Nz,
     $                   'PREADJUSTMENT FACTORS Outliers and ' //
     $                   'Other Deterministic Effects',ndec,'adj.seats')
            end if
          end if
        end if
      end if
      if ((Tramo .eq. 0 ) .and. (Units .eq. 1)) then
        sunits=0
*          call profiler(2,'before UNITSCHECK')
        call UNITSCHECK(oz,nz,sunits)
        if ((sunits .gt.0). and. (Out .eq. 0)) then
           ipos=1
           CALL itoc(3*sunits,str,ipos)
           CALL nWritln('Units in input series are too small.',
     &                  Nio,0,T,F)
           CALL writln('It is recommended that the series be '//
     &                 'multiplied by 10**'//str(1:(ipos-1))//';',
     &                  Nio,0,F,F)
           CALL writln('the program will do it automatically. '//
     &                 '(If correction is not desired, set UNITS=0).',
     &                  Nio,0,F,T)
           CALL mkPOneLine(Nio,'@',
     &                     'The output of the program refers to '//
     &                     'series multiplied by 10**'//
     &                     str(1:(ipos-1))//'.')
        end if
        if ((sunits .lt.0) .and. (Out .eq. 0)) then
           ipos=1
           CALL itoc(-3*sunits,str,ipos)
           CALL nWritln('Units in input series are too large.',
     &                  Nio,0,T,F)
           CALL writln('It is recommended that the series be '//
     &                 'multiplied by 10**'//str(1:(ipos-1))//';',
     &                  Nio,0,F,F)
           CALL writln('the program will do it automatically. '//
     &                 '(If correction is not desired, set UNITS=0).',
     &                  Nio,0,F,T)
           CALL mkPOneLine(Nio,'@',
     &                     'The output of the program refers to '//
     &                     'series multiplied by 10**'//
     &                     str(1:(ipos-1))//'.')
        end if
      end if
      if ((Out.eq.0) .and. (noserie.eq.0) .and. (Tramo.eq.0)) then
          htmtit = 'ORIGINAL SERIES'
          CALL genSkip(1102)
      end if
      if ((Out.eq.0) .and. (noserie.eq.0) .and. (Tramo.ne.0)) then
          htmtit = 'ARIMA SERIES (Corrected by regARIMA)' //
     $     ' ''Original Series'' FOR SEATS'
          CALL genSkip(1103)
      end if
C
      if ((noserie.eq.0) .and. (Out.eq.0)) then
        ncen = 0
*          call profiler(2,'before prttbl(oz)')
        if(Ndec.eq.0)THEN
         call prttbl(thisDate,Mq,oz,Nz,htmtit(1:ISTRLEN(htmtit)),3,
     $               'a1.seats')
        else
         call prttbl(thisDate,Mq,oz,Nz,htmtit(1:ISTRLEN(htmtit)),ndec,
     $               'a1.seats')
        end if
        if ((Neff(2) .eq. 1).and.(centrregs.eq.1)) then
           CALL nWritln('DETERMINISTIC EFFECTS ASSIGNED TO THE '//
     $                  'SEASONAL COMPONENT HAVE BEEN CENTERED.',
     $                  Nio,0,T,T)
            ncen = 1
        end if
        if ((Neff(2) .eq. 1).and.(centrregs.eq.0)) then
           CALL nWritln('DETERMINISTIC EFFECTS ASSIGNED TO THE '//
     $                  'SEASONAL COMPONENT HAVE NOT BEEN CENTERED.',
     $                  Nio,0,T,T)
        end if
        if ((Nouir.eq.1).or.(Neff(3).eq.1)) then
           CALL nWritln('DETERMINISTIC EFFECTS ASSIGNED TO THE '//
     $                  'IRREGULAR COMPONENT HAVE NOT BEEN CENTERED.',
     $                  Nio,0,T,T)
        end if
        if (Neff(5).eq.1) then
           CALL nWritln('DETERMINISTIC EFFECTS ASSIGNED TO THE '//
     $                  'TRANSITORY COMPONENT HAVE NOT BEEN CENTERED.',
     $                  Nio,0,T,T)
        end if
      end if
C
C PRINT OUT INPUT PARAMETERS
C
      if (Bias .eq. 0) then
        Bias = 1
        if (Out .eq. 0) then
            CALL nWritln('BIAS SET EQUAL TO 1',Nio,0,T,T)
        end if
      end if
      if (iqm .eq. 999) then
        if ((Mq.ne.12) .and. (Mq.ne.6) .and. (Mq.ne.4) .and.
     $      (Mq.ne.3) .and. (Mq.ne.2) .and. (Mq.ne.1) .and.
     $      (Mq.gt.12)) then
          iqm = 24
        end if
        if ((Mq.eq.12) .or. (Mq.eq.6)) then
          iqm = 24
        end if
        if (Mq .eq. 4) then
          iqm = 16
        end if
        if (Mq .eq. 3) then
          iqm = 12
        end if
        if ((Mq.eq.1) .or. (Mq.eq.2)) then
          iqm = 8
        end if
      end if
      if ((Out .eq. 0) .or. (Noserie .eq. 1)) then
          CALL genSkip(1002)
          CALL writTagOneLine(Nio,'h3','@','INPUT PARAMETERS')
          CALL mkTableTag(Nio,'w80','SEATS INPUT PARAMETERS')
          CALL mkCaption(Nio,'SEATS INPUT PARAMETERS')
          
          CALL writTag(Nio,'<tr>')
          WRITE(Nio,1050)'lam','LAM','lam',Ilam
          WRITE(Nio,1050)'imean','IMEAN','imean',Imean
          WRITE(Nio,1050)'rsa','RSA','rsa',Rsa
          WRITE(Nio,1050)'mq','MQ','mq',Mq
          CALL writTag(Nio,'</tr>')

          CALL writTag(Nio,'<tr>')
          WRITE(Nio,1050)'p','P','p',P
          WRITE(Nio,1050)'bp','BP','bp',Bp
          WRITE(Nio,1050)'q','Q','q',Q
          WRITE(Nio,1050)'bq','BQ','bq',Bq
          CALL writTag(Nio,'</tr>')
          
          CALL writTag(Nio,'<tr>')
          WRITE(Nio,1050)'d','D','d',D
          WRITE(Nio,1050)'bd','BD','bd',Bd
          WRITE(Nio,1050)'noadmiss','NOADMISS','noadmiss',Noadmiss
          WRITE(Nio,1060)'rmod','RMOD','rmod',Rmod
          CALL writTag(Nio,'</tr>')

          CALL writTag(Nio,'<tr>')
          WRITE(Nio,1050)'m','M','m',M
          WRITE(Nio,1050)'qmax','QMAX','qmax',qmax
          WRITE(Nio,1050)'bias','BIAS','bias',Bias
          WRITE(Nio,1050)'out','OUT','out',Out
          CALL writTag(Nio,'</tr>')

          CALL writTag(Nio,'<tr>')
          WRITE(Nio,1060)'thlim','THLIM','thlim',thlim
          WRITE(Nio,1060)'bthlim','BTHLIM','bthlim',bthlim
          WRITE(Nio,1050)'iqm','IQM','iqm',Iqm
          CALL writTag(Nio,'</tr>')

          CALL writTag(Nio,'<tr>')
          WRITE(Nio,1060)'epsphi','EPSPHI','epsphi',epsphi
          WRITE(Nio,1050)'maxit','MAXIT','maxit',Maxit
          WRITE(Nio,1060)'xl','XL','xl',Xl
          WRITE(Nio,1050)'StochTD','StochTD','StochTD',StochTD
          CALL writTag(Nio,'</tr>')
          CALL writTag(Nio,'</table>')
          CALL mkPOneLine(Nio,'@','&nbsp;')
      end if
 1050 FORMAT('<th id="',a,'">',a,'=</th><td headers="',a,'">',
     &           i3,'</td>')
 1060 FORMAT('<th id="',a,'">',a,'=</th><td headers="',a,'">',
     &           f8.3,'</td>')
cc
*      if (out.eq.0 .and. pg.eq.0 .and. iter.eq.0) then
*c   calculamos el espectro del modelo de tramo y generamos el fichero spect.t3
*          write(*,*)'   after summary'
*         call PLOTOrigSpectrum(p,d,q,bp,bd,bq,mq,Th,Phi,BTh,BPhi)
*      end if 
c
c         if ((out.eq.0).and.(tramo.ne.0)) then
c	     call ShowFirstModel(HTML,Nio,p,d,q,bp,bd,bq,th,
c     $          Bth,phi,Bphi,imean) 
c	   end if
*         ntry = 0
C   LINES OF CODE COMMENTED FOR X-13A-S : 6          
c         if ((Rsa.eq.1) .or. (Rsa.eq.2)) then
c          ntry = 1
C          call OPENDEVSCRATCH(12)
C          Nio = 12
C          Nidx = 12
c         end if
C   END OF CODE BLOCK         
c   Llamamos a rutina para cambiar modelos iniciale de tramo no apropiados
*                call profiler(2,'before changemodel (1)')
*                    CALL outARMAParam()
*          write(*,*)'   before changemodel'
      auxInt=changemodel(nio,init,nochmodel,statseas,posbphi,
     $                   rmod,p,d,q,bp,bd,bq,th,bth,phi,bphi,imean,
     $                   remMeanMCS,out,tramo,inputModel)
*          write(*,*)'   after changemodel'
*                call profiler(2,'after changemodel (1)')
*                    CALL outARMAParam()
c
c
c4000     if ((Tramo.ne.0) .and. (Init.eq.2) .and. (Rsa.ne.1) .and.
C     $       (Rsa.ne.2) .and. (nochmodel.eq.0)) then
C          call OPENDEVSCRATCH(12)
C          Nio = 12
C          Nidx = 12
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 4
 4000    if ((Tramo.ne.0) .and. (Init.eq.2)) then          
          call OPENDEVSCRATCH(42)
          Nio = 42
*          Nidx = 42
c   END OF CODE BLOCK          
          noretry = 0
         end if
      do i=1,n10
        fixParam(i)=0
      enddo
      do 10 while (T)
C   LINES OF CODE COMMENTED FOR X-13A-S : 3         
C          if ((Nio.eq.12) .and. (Noadmiss.eq.2)) then
C           Nio = ndevice
C           call CLOSEDEVICE(12)
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 3
        if ((Nio.eq.42) .and. (Noadmiss.eq.2)) then
           Nio = ndevice          
           call CLOSEDEVICE(42)
C   END OF CODE BLOCK           
        end if
*        if (ncrazy .eq. 1) then
*          CALL wWritln('A PURE (SEASONAL) MA IMPLIES A SMALL '//
*     $      'AND SHORT-LIVED SEASONAL CORRELATION.'//
*     $      ' SEASONALITY IS TOO WEAK AND UNSTABLE TO BE RELIABLY '//
*     $      'CAPTURED. SEASONAL COMPONENT MADE ZERO.',Nio,0,T,T)
*          ncrazy = 0
*        end if
CC
C TEST P AND Q AGAINST CONSTRAINTS
C
        if (P .le. 3*n1) then
          if (Q .le. 3*n1) then
C
C NOW WE CHECK IF THE MODEL IS DEGENERATE (TILL 87)
C
            if (Init.ne.0 .and. Q.ne.0 .and. P.eq.Q) then
              do i = 1,Q
                if (ABS(Th(i)-Phi(i)) .gt. ceps) THEN
*                  call profiler(2,'**GO TO 5000**, line 1704')
                  goto 5000
                END IF
              end do
*              call profiler(2,'**GO TO 5015**, line 1708')
              goto 5015
            end if
C
C TEST BP,BQ AGAINST CONSTRAINTS
C
 5000       if (Bp .le. 2*n1) then
              if (Bq .le. 2*n1) then
C
C WE CHECK ALSO IF THE SEASONAL PART IS DEGENERATE (TILL 93)
C
                if (Init.ne.0 .and. Bq.ne.0 .and. Bp.eq.Bq) then
                  do i = 1,Bq
                    if (ABS(Bth(i)-Bphi(i)) .gt. ceps) then
*                       call profiler(2,'**GO TO 5001**, line 1722')
                       goto 5001
                    END IF
                  end do
*                  call profiler(2,'**GO TO 5016**, line 1726')
                  goto 5016
                end if
 5001           do i = 1,10
                  xmin(i) = -Xl
                  xmax(i) = Xl
                end do
C
C TRANSFORM INPUT DEPENDING ON LAMDA
C
                Pbp = P + Bp
                Pq = Pbp + Q
                mq2 = Mq * 2
                Bpq = P + Q + Bp + Bq
                nx = Bpq
                Pstar = P + Bp*Mq
                Qstar = Q + Bq*Mq
                if (noserie .ne. 1) then
                  if ((Mq.ne.12) .and. (Mq.ne.6) .and. (Mq.ne.4) .and.
     $             (Mq.ne.3) .and. (Mq.ne.2) .and. (Mq.ne.1) .and.
     $             (Mq.gt.12)) then
                    call wWritln('FREQUENCY OF OBSERVATIONS NOT '//
     $                             'APPROPIATE FOR SEATS',Nio,0,T,T)
                    Iq = 24
                  end if
                  if (iqm .eq. 999) then
                    if ((Mq.ne.12) .and. (Mq.ne.6) .and. (Mq.ne.4) .and.
     $              (Mq.ne.3) .and. (Mq.ne.2) .and. (Mq.ne.1) .and.
     $              (Mq.gt.12)) then
                    call wWritln('FREQUENCY OF OBSERVATIONS NOT '//
     $                             'APPROPIATE FOR SEATS',Nio,0,T,T)
                    Iq = 24
                      iqm = 24
                    end if
                    if ((Mq.eq.12) .or. (Mq.eq.6)) then
                      iqm = 24
                    end if
                    if (Mq .eq. 4) then
                      iqm = 16
                    end if
                    if (Mq .eq. 3) then
                      iqm = 12
                    end if
                    if ((Mq.eq.1) .or. (Mq.eq.2)) then
                      iqm = 8
                    end if
                  end if
                  if (M .ge. Nz-(D+Bd*Mq)) then
                    M = (Nz-(D+Bd*Mq))
                    if (iqm .ge. M) then
                      iqm = M - 2
                    end if
                    if (iqm .lt. P+Bp+Q+Bq+Imean) then
                      iqm = M
                    end if
                  end if
                  if ((M.lt.Mq) .or. (iqm.le.0) .or.
     $               (Nz-(D+Bd*Mq+P+Bp*Mq).le.0)) THEN
*                       call profiler(2,'**GO TO 5017**, line 1784')
                       goto 5017
                  end if
                  if (iqm .gt. M) then
                    iqm = M
                  end if
                  Iq = iqm
                  if (Ilam .eq. 0) then
                    do i = 1,Nz
                      if (oz(i) .le. 0) then
*                        call profiler(2,'**GO TO 5002**, line 1794')
                        goto 5002
                      end if
                    end do
                    do i = 1,Nz
                      z(i) = LOG(oz(i))
                    end do
                    if (Out .eq. 0) then
*                        iTab=iTab+1
*                        iId=iId+1
                      CALL mkPOneLine(Nio,'em',
     $                                'TRANSFORMATION: Z -> LOG Z')
                    end if
*                    call profiler(2,'**GO TO 5003**, line 1807')
                    goto 5003
 5002               Ilam = 1
                    CALL nWritln(
     $                   'ILam CHANGED TO 1 SERIES HAS NEGATIVE VALUES',
     $                       Nio,0,T,T)
                  end if
                  do i = 1,Nz
                    z(i) = oz(i)
                  end do
C
C  SET VARIOUS PARAMETERS
C
 5003             Pbp = P + Bp
                  Pq = Pbp + Q
                  mq2 = Mq * 2
                  Bpq = P + Q + Bp + Bq
                  nx = Bpq
                  Pstar = P + Bp*Mq
                  Qstar = Q + Bq*Mq
C
C MEAN AND VARIANCE CALCULATED. DIFFERENCING OF THE Z SERIES
C
                  if (noserie .ne. 1) then
                    Nw = Nz
                    zm = ZERO
                    do i = 1,Nz
                      zm = zm + z(i)
                      Wd(i) = z(i)
                    end do
                    zm = zm / Nz
                    Zvar = ZERO
                    do j = 1,Nz
                      Zvar = Zvar + (z(j)-zm)**2
                    end do
                    Zvar = Zvar / Nz
!                 if ((Ilam.eq.0) .and. (noserie.eq.0) .and.
!      $              (Pg.eq.0) .and. (Out.ne.2)) then
!                  fname = 'RSERIE.T'
!                  if (Mq .eq. 12) then
!                   subtitle = 'SERIES MONTHLY RATE of GROWTH ( % )'
!                  end if
!                  if (Mq .eq. 4) then
!                   subtitle = 'SERIES QUARTERLY RATE of GROWTH ( % )'
!                  end if
!                  if ((Mq.ne.12) .and. (Mq.ne.4)) then
!                   subtitle = 'SERIES RATE of GROWTH in PERIOD ( % )'
!                  end if
!                  do i = 2,Nw
!                   bz(i-1) = 100.0d0 * (Wd(i)-Wd(i-1))
!                  end do
!                  nyer2 = Nyer
!                  nper2 = Nper
!                  Nper=Nper+1
!                  if (Nper .gt. Mq) then
!                   Nper = 1
!                   Nyer = Nyer + 1
!                  end if
!                  call PLOTSERIES(fname,subtitle,bz,Nw-1,1,0.0d0)
!                  Nyer = nyer2
!                  Nper = nper2
!                 end if
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
                    Nwdif=Nw
                    do i=1,Nw
                      Wdif(i)=Wd(i)  
                    enddo
C
C MEAN CORRECT Wd SERIES IF IMEAN = 1
C
                    wmDifXL = ZERO
                    if ((crmean.eq.0) .or. (MOD(Nw,Mq).eq.0)) then
                      do j = 1,Nw
                        wmDifXL = wmDifXL + Wd(j)
                      end do
                      wmDifXL = wmDifXL / Nw
                    else
                      nn = Nw - MOD(Nw,Mq)
                      do j = 1,nn
                        wmDifXL = wmDifXL + Wd(j)
                      end do
                      wmDifXL = wmDifXL / nn
                    end if
                    wm=wmDifXL
                    if (Imean.ne.0 .or. D.ne.0) then
                      if (Imean .ne. 0) then
                        do j = 1,Nw
                          Wd(j) = Wd(j) - wmDifXL
                        end do
                        do j = 1,Nw
                          WDifCen(j) = Wd(j)
                        end do
                      end if
                    end if
                    ImeanOut=Imean
C
C CALCULATE VARIANCE OF NONDIFFERENCED SERIES AND DIFFERENCED SERIES
C
 5004               VdifXL = ZERO
                    do i = 1,Nw
                      VdifXL = VdifXL + Wd(i)*Wd(i)
                    end do
                    VdifXL = VdifXL / Nw
                    if (Nio .eq. ndevice) then
                      dvec(1)=wmDifXL
                      call USRENTRY(dvec,1,1,1,1,1024)
                    end if
                    if (M .gt. 48) then
                      if (Out .eq. 0) then
                        write (Nio,7011) M
 7011                   format (/,'<p>ONLY ALLOWS 48 AUTOCORRELATIONS-',
     $                            i4,' IS TOO MANY</p>')
                      end if
                      M = 48
                    end if
                    if (Imean .eq. 0) then
                      wm = ZERO
                    end if
*                  end if
*                call AUTO(Nw,Wd,M,r,0,Nw,Bpq,Nfreq,0,Qstat,df,se,Ierr,
*     $                    Errext)
*                  call profiler(2,'before AUTO')
                  call AUTO(Nw,Wd,M,rXL,0,Nw,Bpq,Nfreq,0,QstatXL,df,
     $                      seRxl,Ierr,Errext)
                  IF(Ierr.eq.1)RETURN
                  do i=1,m
                    r(i)=rXL(i)
                  enddo
                if (Nio .eq. ndevice) then
                 call USRENTRY(r,1,m,1,5*n10,2163)
                end if
                  iout = Out
*                  call profiler(2,'before PART')
                  call PART(Nw,M,rXL,iout,partAcf,SEpartAcf)
                end if
              end if
C
C INITIALIZE DETPRI
C
              if (model .eq. 1) then
                call setTmcs('Y')
              end if
              Detpri = ONE
              Inoadmiss = 0
C
C CALCULATE TRANSFORMED VALUES OF MODEL PARAMETERS AND OUTPUT INITIAL
C VALUES OF MODEL PARAMETERS
C
              if (Init .lt. 1) then
C
C THIS SUBROUTINE COMPUTES THE STARTING VALUES OF ESTIMATION
C
*                call profiler(2,'before STAVAL')
*                CALL outARMAParam()
                call STAVAL(P,Q,Bp,Bq,Phi,Th,Bphi,Bth,r,Mq,mq2)
*                call profiler(2,'after STAVAL')
*                CALL outARMAParam()
              end if
              do 15 while (T)
               if ((noadmiss.eq.2).and.(init.eq.2)) then                   
                Pbp = P + Bp
                Pq = Pbp + Q
                mq2 = Mq * 2
                Bpq = P + Q + Bp + Bq
                nx = Bpq
                Pstar = P + Bp*Mq
                Qstar = Q + Bq*Mq
               end if           
C   LINES OF CODE COMMENTED FOR X-13A-S : 4
C               if ((Nio.eq.12) .and. (Noadmiss.eq.2)) then
C                Nio = ndevice
C                Nidx = nidevice
C                call CLOSEDEVICE(12)
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 3
                if ((Nio.eq.42) .and. (Noadmiss.eq.2)) then
                  Nio = ndevice
*                  Nidx = nidevice
                  call CLOSEDEVICE(42)
C   END OF CODE BLOCK                
                end if
*                call profiler(2,'before TRANS1')
*                CALL outARMAParam()
                if (P .ne. 0) then
C                call CHECKI2(Phi,x,1,P,xl,ONE)
*                  call profiler(2,'before TRANS1')
                  call TRANS1(Phi,P,xtmp,xmin,xmax,1,P,out)
                end if
                if (Bp .ne. 0) then
C                call CHECKI2(Bphi,x,P+1,Pbp,xl,ONE)
*                  call profiler(2,'before TRANS1')
                  call TRANS1(Bphi,Bp,xtmp,xmin,xmax,P+1,Pbp,out)
                end if
                if (Q .ne. 0) then
C                call CHECKI2(Th,x,Pbp+1,Pq,xl,xl)
*                  call profiler(2,'before TRANS1')
                  call TRANS1(Th,Q,xtmp,xmin,xmax,Pbp+1,Pq,out)
                end if
                if (Bq .ne. 0) then
C                call CHECKI2(Bth,x,Pq+1,Bpq,xl,xl)
*                  call profiler(2,'before TRANS1')
                  call TRANS1(Bth,Bq,xtmp,xmin,xmax,Pq+1,Bpq,out)
                end if
*                call profiler(2,'after TRANS1')
*                CALL outARMAParam()
                do i=1,nx
                  if (FixParam(i).eq.0) then
                    x(i)=xtmp(i)
                  end if
                enddo
                if (Qstar .ne. 0) then
                  do i = 1,Qstar
                    Thstar(i) = ZERO
                  end do
                end if
                if (Pstar .ne. 0) then
                  do i = 1,Pstar
                    Phist(i) = ZERO
                  end do
                end if
C
C OUTPUT INITIAL VALUES OF TRANSFORMED PARAMETERS AND BOUNDS
C PARAMETERS ARE CONSTRAINED TO NOT QUITE REACH BOUNDARIES OF
C STABILITY/INVERTIBILITY OF MODEL
C
cc
c Spectral Analysis Linearized Series
cc             
                if ((noserie.ne.1).and.((mq.eq.4).or.(mq.eq.12))) then
                  do i = 1,61
                    Szz(i) = ZERO
                    ow(i) = ZERO
                  end do
                  cname='Linealized Series   '
*                  call profiler(2,'before SpectrumComputation')
*                    CALL outARMAParam()
                  call SpectrumComputation(z,nz,mq,cname,'Xl',0,1,
     $                           PicosXl,totalSeasXL)
*                  call profiler(2,'after SpectrumComputation')
*                    CALL outARMAParam()
                end if
cc
c
cc
                if (Init .eq. 2) then
C
C RESIDUALS $ STANDARD ERROR IF PARAMETERS NOT ESTIMATED, INIT=2
C
                  if (noserie .eq. 1) THEN
*                    call profiler(2,'**GO TO 5008**, line 2066')
                    goto 5008
                  END IF
                  Jfac = 1
                  Na = Nw - Pstar + Qstar
                  Dof = Nw - Pstar - nx - Imean
*                  call profiler(2,'before CALCFX, 1')
*                    CALL outARMAParam()
c       write(*,*)'  entering CALCFX line 2092'
                  call CALCFX(nx,x,s,Na,a,Ierr,Errext,out,*5007)
c       write(*,*)'  exiting CALCFX'
*                  call profiler(2,'CALCFX: did not go to 5007')
*                    CALL outARMAParam()
                  if (Ierr.ne.0) then
                    Dstdres(ntrace) = -99999.99
                    TrTitle(ntrace) = titleg
                    ntrace = ntrace + 1
                    handle=1
                    Ierr=0
                    Errext=''
*                    call profiler(2,'**GO TO 5020**, line 2082')
                    goto 5020
                    call closealls()
                    return
                  end if
 5007             s = s / Detpri**2
                  f0 = s / Dof
                  Sqf = SQRT(f0)
                else
C
C SET PARAMETERS FOR SEARCH
C E(I) INDICATES FIXED RANGES. CONV1(I) IS NOT USED.
C TEST FOR MODELS WITH CONSTRAINED COEFFICIENTS
C
                  do i = 1,nx
                    if (fixParam(i).eq.0) then
                      e(i) = 0
                    else if (fixParam(i).eq.1) then
                      e(i) = 1
                    end if
                    conv1(i) = 0.0
                  end do
                  if (P .gt. 1) then
                    kkp = P - 1
                    do i = 1,kkp
                      if (ABS(Phi(i)) .gt. ceps) THEN
*                        call profiler(2,'**GO TO 5005**, line 2108')
                        goto 5005
                      end if
                    end do
                    do i = 1,kkp
                      e(i) = 1
                    end do
                  end if
 5005             if (Q .gt. 1) then
                    kq = Q - 1
                    do i = 1,kq
                      if (ABS(Th(i)) .gt. ceps) THEN
*                        call profiler(2,'**GO TO 5006**, line 2120')
                        goto 5006
                      END IF
                    end do
                    do i = 1,kq
                      e(Pbp+i) = 1
                    end do
                  end if
 5006             Na = Nw - Pstar + Qstar
                  Ifac = 0
                  Jfac = 0
C
C TST IS A FLAG TO TEST IF SOME PARAMETERS ARE FIXED
C
                  tst = 0
                  if ((Nreestimated.eq.0) .and. (Tramo.eq.1) .and.
     $               (Nio.eq.ndevice)) then
                    Nreestimated = 1
                  end if
*                  call profiler(2,'before SEARCH, line 2139')
*                    CALL outARMAParam()
                  call SEARCH(nx,x,xmin,xmax,epsiv,e,conv1,Na,a,s,maxit,
     $                    maxf,iprint,se,c,tst,Pbp,p,ONE,Out,ItnSearch,
     $                    bd,d,Ierr,Errext,*5119)                
*                  call profiler(2,'SEARCH: did not go to 5119')
*                    CALL outARMAParam()
                  if (Ierr.ne.0) then
                    Dstdres(ntrace) = -99999.99
                    TrTitle(ntrace) = titleg
                    ntrace = ntrace + 1
                    handle=1
                    Ierr=0
                    Errext=''
*                    call profiler(2,'**GO TO 5020**, line 2151')
                    goto 5020
                    call closealls()
                    return
                  else
                    IfnSearch=Ifn
                    FIsearch=S
                    do i=1,nx
                      xSearch(i)=x(i)
                      Esearch(i)=E(i)
                    enddo
                    nxSearch=nx                
                  end if
                  if (Nhtofix .eq. 1) then
                    x(P+Bp+Q) = -Xl
                    Nhtofix = 0
                  end if
                  if (tst .gt. 0) then
*                    call profiler(2,'before CHMODEL, line 2169')
*                    CALL outARMAParam()
                    call CHMODEL(x,se,nx,P,Q,Bp,Bq,D,Bd,Wd,Nw,wm,VdifXL,
     $                     Mq,ONE,Xl,Phi,tst,Imean,seas,Pbp,Pq,Bpq,
     $                     Pstar,z,Nz,out,*10)
*                    call profiler(2,'CHMODEL: did not go to 10')
*                    CALL outARMAParam()
                    Pbp = P + Bp
                    Pq = Pbp + Q
                    Bpq = P + Q + Bp + Bq
                    Pstar = P + Bp*Mq
                  end if
C      Chequeo por si APPROXIMATE o chmodel nos dan un modelo que no queremos (muy poco probable)
C      lo metemos por si acaso pero podríamos quitarlo         
*                call profiler(2,'before changemodel (2)')
*                    CALL outARMAParam()
                if (changemodel(nio,init,nochmodel,statseas,
     $                posbphi,rmod,p,d,q,bp,bd,bq,th,bth,phi,bphi,
     $                imean,remMeanMCS,out,tramo,inputModel).gt.0)then
*                    call profiler(2,'**GO TO 10**, line 2185')
                    goto 10
                end if  
*                call profiler(2,'after changemodel (2)')
*                    CALL outARMAParam()
c
                if (tst .le. 0) then
                  seMEan = ZERO
*                  call profiler(2,'before CHECK (called from analts)')
                  call CHECK(VdifXL,wm,Nw,Phi,P,Bphi,Bp,Th,Q,Bth,Bq,Mq,
     $                       seMean)
                end if
                s = s / Detpri**2
                Dof = Nw - Pstar - nx - Imean
                f0 = s / Dof
                Sqf = SQRT(f0)
                dof1 = SQRT((Dof+Qstar)/Dof)
                do i = 1,nx
                  se(i) = se(i) * dof1 / Detpri
                end do
C
C OUTPUT VALUES OF TRANSFORMED PARAMETERS AND THEIR STANDARD ERRORS
C OUTPUT CORRELATION MATRIX OF TRANSFORMED PARAMETERS AND FORM
C COVARIANCE MATRIX
C
C
                if (tst .gt. 0) then
                  seMean=ZERO
                end if
                tstMean=tst
                dvec(1)=seMEan
                call USRENTRY(dvec,1,1,1,1,1025)
                do i=1,nx
                  do j=1,i
                    cMatrix(i,j)=c(i,j)
                  enddo
                enddo
                do i = 1,nx
                  do j = 1,i
                    c(i,j) = c(i,j) * se(i) * se(j)
                  end do
                end do
              end if
C
C CALCULATE DURBIN-WATSON STATISTIC
C
              sfd = ZERO
              do i = 2,Na
                sfd = sfd + (a(i)-a(i-1))**2
              end do
              sfd = sfd / Detpri**2
              dw = sfd / s
C
C MODEL PARAMETERS ,CALCULATE THEIR STANDARD ERRORS IN VAR
C SUBROUTINE 
C
 5008         if (Init .ne. 2) then
*                call profiler(2,'before VARMP')
                call VARMP(x,c,P,sePHI,1,P)
                call VARMP(x,c,Bp,seBPHI,P+1,Pbp)
                call VARMP(x,c,Q,seTH,Pbp+1,Pq)
                call VARMP(x,c,Bq,seBTH,Pq+1,Bpq)
              else
                do i = 1,P
                  sePHI(i) = ZERO
                end do
                seBPHI(1)=ZERO
                do i = 1,Q
                  seTH(i) = ZERO
                end do           
                seBTH(1)=ZERO
              end if     ! of p<>0
              call USRENTRY(sePHI,1,P,1,n10,1110)
              call USRENTRY(seBPHI,1,Bp,1,n10,1112)
              call USRENTRY(seTH,1,Q,1,n10,1111)
              call USRENTRY(seBTH,1,Bq,1,n10,1113)
c
c               if (Noadmiss .eq. 2) then
c                call OPENDEVSCRATCH(18)
c                niosave = Nio
c                Nio = 18
c               Nidx = 18
c               end if
              if (P .ne. 0) then
                do i = 1,P
                  phis(i+1) = -Phi(i)
                end do
              end if
              phis(1) = ONE
              nphi = P + 1
              if (Q .ne. 0) then
                do i = 1,Q
                  ths(i+1) = -Th(i)
                end do
              end if
              ths(1) = ONE
              nth = Q + 1
              if (noserie.ne.1 .and. Q.gt.1) then
*                call profiler(2,'before RPQ')
                call RPQ(ths,nth,MArez,MAimz,MAmodul,MAar,MApr,1,out)
C   LINES OF CODE ADDED FOR X-13A-S : 1
                IF(Lfatal)RETURN
C   END OF CODE BLOCK
              end if
C
C EVEN IF P=1 CALL RPQ BECAUSE SIGEX WANTS THE ROOT IN  REZ,IMZ,
C MODUL,AR,PR
C
              if (noserie.eq.1) then
               if (p.gt.0) then
*                call profiler(2,'before RPQ')
                call RPQ(phis,nphi,rez,imz,modul,ar,pr,1,out)
C   LINES OF CODE ADDED FOR X-13A-S : 1
                IF(Lfatal)RETURN
C   END OF CODE BLOCK
               end if
               goto 5011
              else
                call RPQ(phis,nphi,rez,imz,modul,ar,pr,1,out)
C
C CORRECT RESIDUALS FOR FACTOR OF DETPRI
C
                do i = 1,Na
                  a(i) = a(i) / Detpri
                  aa(i) = a(i)
                end do
C
C  COMPUTES THE  STATISTCS OF RESIDUAL
C
                if (Type .eq. 1) then
                 rmean = ZERO
                 do i = Qstar+1,Na
                   rmean = rmean + a(i)
                 end do
C
C WITH CLS THE FIRST QSTAR RESIDUALS ARE ZERO SO TO COMPUTE THE MEAN
C AND VARIANCE THE NUMBER OF RESIDUALS NA=NA-QSTAR
C
                rmean = rmean / (Na-Qstar)
                rvar = ZERO
                do i = Qstar+1,Na
                  rvar = rvar + a(i)*a(i)
                end do
                rvar = rvar / (Na-Qstar)
              else
                rmean = DMEAN(Na,a)
                rvar = DVAR(Na,a)
              end if
              rstd = (rvar/Na)**0.5d0
              rtval = rmean / rstd
C
C T-VALUE OF RESIDUALS IS GREATER THEN TA (INPUT PARAMETER)
C THE RESIDUALS ARE MEAN CORRECTED
C
              if ((Imean.eq.1) .and. (rtval.gt.ta)) then
                do i = 1,Na
                  a(i) = a(i) - rmean
                end do
                phi1(1) = ONE
                do i = 1,P
                  phi1(i+1) = -Phi(i)
                end do
                bphi1(1) = ONE
                do i = 1,Bp*Mq
                  bphi1(i+1) = ZERO
                end do
                if (Bp .gt. 0) then
                  do i = 1,Bp
                    bphi1(i*Mq) = -Bphi(i)
                  end do
                end if
*                call profiler(2,'before CONV')
                call CONV(phi1,P+1,bphi1,1+Bp*Mq,bphi1,lll)
                th1(1) = ONE
                do i = 1,Q
                  th1(i+1) = -Th(i)
                end do
                bth1(1) = ONE
                do i = 1,Bq*Mq
                  bth1(i) = ZERO
                end do
                if (Bq .gt. 0) then
                  do i = 1,Bq
                    bth1(i*Mq) = -Bth(i)
                  end do
                end if
*                call profiler(2,'before CONV')
                call CONV(th1,Q+1,bth1,1+Q*Mq,bth1,lll1)
                first = ZERO
                do i = 1,lll
                  first = first + bphi1(i)
                end do
                second = ZERO
                do i = 1,lll1
                  second = second + bth1(i)
                end do
                first = second / first
                wm = wm + first*rmean
                if (Out .eq. 0) then
 6040             format ('<p>',a,'<strong>CORRECTED MEAN OF ',
     $                    'DIFF. SERIES =</strong></p>',d12.4)
                  write (Nio,6040) Cbr,wm
                end if
                if (Type .eq. 1) then
                  rmean = ZERO
                  do i = Qstar+1,Na
                    rmean = rmean + a(i)
                  end do
                  rmean = rmean / (Na-Qstar)
                  rvar = ZERO
                  do i = Qstar+1,Na
                    rvar = rvar + a(i)*a(i)
                  end do
                  rvar = (rvar-rmean**2) / (Na-Qstar)
                else
                  rmean = DMEAN(Na,a)
                  rvar = DVAR(Na,a)
                end if
                rstd = (rvar/Na)**0.5d0
                rtval = rmean / rstd
              end if
              skewne = ZERO
              rkurt = ZERO
              nna = Na
              if (Type .eq. 1) then
                nna = Na - Pstar
              end if
              do i = 1,Na
                skewne = skewne + ((a(i)-rmean)**3)/(rvar**1.50d0*nna)
                rkurt = rkurt + ((a(i)-rmean)**4)/(rvar**TWO*nna)
              end do
              rvar = rvar / Na
              test1 = SQRT(6.0d0/Na)
              test = SQRT(24.0d0/Na)
              wnormtes = (skewne**2)/(test1**2) +
     $                     ((rkurt-3)**2)/(test**2)
              nyer2 = Nyer
              nper2 = Nper
              nyer1 = Nyer
              nper1 = Nper + Nz - Na
              do while (nper1 .gt. Nfreq)
                nper1 = nper1 - Nfreq
                nyer1 = nyer1 + 1
              end do
              do while (nper1 .le. 0)
                nper1 = nper1 + Nfreq
                nyer1 = nyer1 - 1
              end do
              nz1 = Nz
              do i=1,Na
                Resid(i)=a(i)
              enddo
              SumSres=s
              numEresid=na
              do i = 1,na
                eresid(i) = a(i)
              end do
              call USRENTRY(eresid,1,numEresid,1,mpkp,1100)
              Nz = nz1
cc
c Here Introduce the fitted graph
cc
*              if ((pg .eq. 0).and.(out.lt.2).and.(iter.eq.0)) then
**                call profiler(2,'before PlotFitted')
*                if (tramo.gt.0) then
*                  call PlotFitted(tram,eresid,nz,numEresid,ilam,
*     $                            nyer2,nper2,mq)
*                else
*                  call PlotFitted(oz,eresid,nz,numEresid,ilam,
*     $                            nyer2,nper2,mq)
*                end if               
*              end if
              Nper = nper2
              Nyer = nyer2
C
C  COMPUTES THE STUDENTISED RESIDUAL
C
*                if (Out .eq. 2) then
*                 if (HTML .eq. 1) then
*                  write (Nio,'(''<br><b>EXTENDED RESIDUAL'',
*     $                         '' STANDARD ERROR : </b>'',D12.4)') Sqf
*                  write (Nio,'(''<br><b><u>DIAGNOSIS (*)</b></u>'')')
*                 else
*                  write (Nio,
*     $'(6X,''EXTENDED RESIDUAL STANDARD ERROR :'',2X,D12.4)') Sqf
*                  write (Nio,
*     $                  '(2X,''DIAGNOSIS (*)'',/,2X,''-------------'')')
*                 end if
*                 nsr = 0
*                end if
              flagTstu = 0
              do i = 1,Na
                aa(i) = a(i)
              end do
              Ken = kendalls(a,Na,Nfreq)
c 21/08/2009
c                 if ((Out.eq.0) .and. ((a(i).lt.-Sek).or.(a(i).gt.Sek)))
c     $              then
c                  nsr = 1 + nsr
c                  if (HTML .eq. 1) then
c                   if (nsr .eq. 1) then
c                    write (Nio,'(''<TABLE>'')')
c                   write (Nio,'(''<caption>OUTLIERS IN EXTENDED '',
c     $                           ''RESIDUALS ( > '',f5.2,
c     $                           '') :</caption>'')') Sek
c                    write (Nio,'(''<tr><th>MONTH</th>'',
c     $                        ''<th>YEAR</th>'',
c     $                        ''<th>T-VALUE</th></tr>'')')
c 6042               format ('<tr><td>',i2,
c     $                      '</td><td>',i4,
c     $                      '</td><td>',f5.2,
c     $                      '</td></tr>')
c                    write (Nio,6042) iper, iyear, a(i)
*                 else
c                    write (Nio,6042) iper, iyear, a(i)
c                   end if
c                  write (Nio,'("</table>")')
c                  else
c                   if (nsr .eq. 1) then
c                    write (Nio,'(6x,''OUTLIERS IN EXTENDED '',
c     $                 ''RESIDUALS ( > '',f5.2,'') :'')') Sek
c                    write (Nio,
c     $                     '(42X,''MONTH'',4X,''YEAR'',4X,''T-VALUE'')')
c 7042               format (44x,i2,5x,i4,5x,f5.2)
c                    write (Nio,7042) iper, iyear, a(i)
c                   else
c                    write (Nio,7042) iper, iyear, a(i)
c                   end if
c                  end if
c                 end if                
*              if ((Pg.eq.0) .and. (Out.lt.2).and.(iter.eq.0)) then
*                fname = 'RESID.T'
*                subtitle = 'EXTENDED RESIDUALS'
*                Nyer = nyer1
*                Nper = nper1
*                call PLOTSERIES(fname,subtitle,eresid,numEresid,
*     $                          555,2.0d0*Sqf)
*                cname=subtitle
*                shortName='a '
*                if (out.eq.0) then
**                  call profiler(2,'before SpectrumComputation')
*                  call SpectrumComputation(a,Na,mq,cname,shortName,
*     $                                     1,0,PicosRes,totalSeasRes)
*                end if
*                Nyer = nyer2
*                Nper = nper2
*              end if
C
C CALCULATE PSI COEFFICIENTS AFTER DIFFERENCING FOR COMPARISONS
C OF MODELS
C
              lp = MAX(L,Qstar+1)
              lp = MIN(lp,5*N12-N12/3)
*              call profiler(2,'before RATF')
              call RATF(Thstar,Qstar,Phist,Pstar,ps,lp,1)
C
C CALCULATE AUTOCORRELATIONS AND PARTIAL AUTOCORRELATIONS OF RESIDUALS
C
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
C                call AUTO(Na,a,M,r,Iq,Nw,Bpq,Nfreq,iauto,
C     $              Qstat,df,sea)
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 3                
              iauto = 1
*              call profiler(2,'before AUTO')
              call AUTO(Na,a,M,r,Iq,Na,Bpq,Nfreq,iauto,Qstat,df,sea,
     &                  Ierr,Errext)
              IF(Ierr.eq.1)RETURN
C   END OF CODE BLOCK                
              dvec(1)=dble(Iq-Bpq-Imean)
              call USRENTRY(dvec,1,1,1,1,1000)
              dvec(1)=Bjstat1            
              call USRENTRY(dvec,1,1,1,1,1002)
              dvec(1)=Pstat1             
              call USRENTRY(dvec,1,1,1,1,1003)
              call USRENTRY(r,1,M,1,5*n10,2161)            
*                if ((ntry.gt.0) .and. ((Rsa.eq.1).or.(Rsa.eq.2))) then
*                 call AMI(Bjstat1,Sqf,qmax,ntry,P,D,Q,Bp,Bd,Bq,Imean,Mq,
*     $                    Init,Type,Th,Bth,Phi,Bphi,Rmod,Epsphi,
*     $                status,Noadmiss,prec,out,fixparam,varwnc,*10,*15)
*C   LINES OF CODE ADDED FOR X-13A-S : 1
*                 IF(Lfatal)RETURN
*C   END OF CODE BLOCK
*                end if
              if ((Tramo.ne.0) .and. (Init.eq.2)
     $              .and. (Rsa.eq.0) .and. (Noadmiss.ne.2) .and.
     $              (noretry.eq.0).and.(nochmodel.eq.0)) THEN
*                 call profiler(2,'**GO TO 5013**, line 2576')
                 goto 5013
              END IF
              if (Out .eq. 0) then
                sbjstat1 = Bjstat1
                sbjstat2 = Bjstat2
                spstat1 = Pstat1
              end if
C                call PART(Na,M,r,iout)
              if (Out .eq. 0) then
C
C COMPUTES THE RACES TESTS
C
                xmed = DMED(a,Na)
*                call profiler(2,'before RACES')
                call RACES(a,Na,xmed,1,tvalRUNS,n_1,n0)
                dvec(1)=DBLE(Na)
                call USRENTRY(dvec,1,1,1,1,1008)
                dvec(1)=tvalRUNS           
                call USRENTRY(dvec,1,1,1,1,1007)
              end if
C
C COMMENTED 01-11-1999
C
C      IF (OUT.NE.2) WRITE(NIO,188)
C  188 FORMAT(/' APPROXIMATE TEST OF RUNS ON RESIDUALS ',
C     $        'AUTOCORRELATION FUNCTION'/
C     $        ' --------------------------------------',
C     $        '------------------------')
C      AMED=DMED(R,M)
C      IF (OUT.NE.2) THEN
C        CALL RACES(R,M,AMED,1,TVAL)
C        CALL USRENTRY(M*1.0D0,1,1,1009)
C        CALL USRENTRY(TVAL,1,1,1006)
C      end if
C
C
C
C
C COMPUTES SQUARED RESIDUAL
C
cc
c Spectral Analysis Residuals
cc             
              if ((mq.eq.4).or.(mq.eq.12)) then
                do i = 1,61
                  Szz(i) = ZERO
                  ow(i) = ZERO
                end do
                cname='Extended Residuals  '
*                call profiler(2,'before SpectrumComputation')
                call SpectrumComputation(a,Na,mq,cname,'At',0,0,
     $                            PicosRes,totalSeasRes)
              end if
cc
c
cc
                
              do i = 1,Na
                ba(i) = a(i)**2
              end do
C
C CALCULATE AUTOCORRELATIONS AND PART. AUTOCORR. OF SQUARED RESIDUALS
C
              if (M.gt.48 .and. Out.ne.2) then
                  write (Nio,7011) M
              end if
              M = MIN(M,48)
              iauto = 1
                
C   LINES OF CODE COMMENTED FOR X-13A-S : 1
C                call AUTO(Na,ba,M,sr,Iq,Nw,0,Nfreq,iauto,
C     $                    sQStat,sDF,sSE)
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 3                
*              call profiler(2,'before AUTO')
              call AUTO(Na,ba,M,sr,Iq,Nw,0,Nfreq,iauto,sQStat,sDF,sSE,
     &                  Ierr,Errext)
              IF(Ierr.eq.1)RETURN
C   END OF CODE BLOCK
              dvec(1)=dble(Iq-Bpq-Imean)
              call USRENTRY(dvec,1,1,1,1,1001)
              dvec(1)=Bjstat1            
              call USRENTRY(dvec,1,1,1,1,1004)
              dvec(1)=Pstat1             
              call USRENTRY(dvec,1,1,1,1,1005)
              call USRENTRY(sr,1,M,1,5*n10,2162)            
              if (P+D+Bp+Bd .le. 0) then
*                call profiler(2,'**GO TO 5018**, line 2664')
                goto 5018
              END IF
                ilsave = -1
                lsig = -1
                if (lsig .ne. 0) then
C
C
*                 if ((THLIM.lt.0.0d0) .or. (BTHLIM .lt. 0.0d0)) then
*                  if (Noadmiss .eq. 2) then
*                   smtr = 0
*                   if (out.eq.0) then
*                    if (HTML .eq. 1) then
*                     call Snote(Nio)
*                     write (Nio,'(''WHEN THE MODEL IS '',
*     $                           ''APPROXIMATED, SMOOTHING OF THE '',
*     $                 ''TREND-CYCLE IS NOT ALLOWED.'')')
*                     call Enote(Nio)
*                    else
*                     write (Nio,'(//,8x,
*     $      ''WHEN THE MODEL IS APPROXIMATED,'',/,8x,
*     $      ''SMOOTHING OF THE TREND-CYCLE IS NOT'',/,8x,
*     $      ''ALLOWED.'')')
*                    end if
*                   end if
*                  else
*                   call SMOOTHING(p,d,q,bp,bd,bq,mq,smtr,thlim,bthlim,
*     $                     ths,th,bth,bths,thstar)
*                  end if
*                 end if
C
C
                 qbqMQ=q+bq*mq
                 fhi=max(fh,qbqMQ+max(qbqmq,p+bp*MQ))
                  if (NOADMISS.eq.-1) then
                   fhi=max(fhi,2*(p+d+MQ*(bp+bd)))
                  end if
*                  call profiler(2,'before FCAST')
                  call FCAST(Phist,Thstar,bphist,bpstar,z,Nz,wm,a,Na,
     $                       lsig,f0,iLam,D,Bd,Imean,zaf,fhi,Out,
     $                       Bias,forbias,Noadmiss,alpha)
                end if
                if (bp .eq. 2) then
*                  call profiler(2,'**GO TO 5019**, line 2707')
                  goto 5119
                end if
                printBack=F
                if (lsig .ne. -2) then
                  lsig = -2
C
C REVERSE SERIES AND DIFFERENCED SERIES WITH PROPER SIGN
C
                  jdd = D + Bd
                  kd = (-1)**jdd
                  j = Nw
                  do i = 1,Nw
                    ws = Wd(i) * kd
                    Wd(i) = Wd(Nw-i+1) * kd
                    Wd(Nw-i+1) = ws
                    j = j - 2
                    if (j .le. 0) THEN
*                      call profiler(2,'**GO TO 5009**, line 2725')
                      goto 5009
                    END IF
                  end do
 5009             zab = zaf * kd
                  do i = 1,Nz
                    bz(Nz-i+1) = z(i)
                  end do
C
C GENERATE BACKWARDS RESIDUALS AND REMOVE FACTOR DETPRI
C
                  Jfac = 1
*                  call profiler(2,'before CALCFX, line 2737')
c       write(*,*)'  entering CALCFX,2'
c       write(*,*)' x, bpq, s = ',(x(i), i=1,bpq), bpq, s
c       write(*,*)' first 12 elements of a = ',(a(i),i=1,12)
                  call CALCFX(Bpq,x,s,Na,a,Ierr,Errext,out,*5010)
c       write(*,*)'  exiting CALCFX '
*                  call profiler(2,'CALCFX: did not go to 5010')
                  if (Ierr.ne.0) then
                    Dstdres(ntrace) = -99999.99
                    TrTitle(ntrace) = titleg
                    ntrace = ntrace + 1
                    handle=1
                    Ierr=0
                    Errext=''
*                    call profiler(2,'**GO TO 5020**, line 2747')
                    goto 5020
                    call closealls()
                    return
                  end if
 5010             do i = 1,Na
                    a(i) = a(i) / Detpri
                  end do
                  do i = 1,Na
                    ba(Na-i+1) = a(i)
                    Nz = Na
                  end do
                  printBack=T
                  Nz = nz1
                 qbqMQ=q+bq*mq
                 fhi=max(fh,qbqMQ+max(qbqmq,p+bp*MQ))
                  if (NOADMISS.eq.-1) then
                   fhi=max(fhi,2*(p+d+MQ*(bp+bd)))
                  end if
*                  call profiler(2,'before FCAST')
                  call FCAST(Phist,Thstar,bphist,bpstar,bz,Nz,wm,a,Na,
     $                       lsig,f0,iLam,D,Bd,Imean,zab,fhi,Out,
     $                       -300,forbias,Noadmiss,alpha)
                end if
              end if
 5011         if (P .ne. 0) then
                do i = 1,P
                  phis(i+1) = -Phi(i)
                end do
              end if
              phis(1) = ONE
              if (Q .ne. 0) then
                do i = 1,Q
                  ths(i+1) = -Th(i)
                end do
              end if
              ths(1) = ONE
              if (Bp .ne. 0) then
                do j = 1,Bp
                  bphis(Mq*j+1) = -Bphi(j)
                  do i = (j-1)*Mq+2,j*Mq
                    bphis(i) = ZERO
                  end do
                end do
              end if
              bphis(1) = ONE
              if (Bq .ne. 0) then
                do i = 1,Bq
                  j = i*Mq + 1
                  bths(j) = -Bth(i)
                  do k = 2,Mq
                    jk = k + Mq*(i-1)
                    bths(jk) = ZERO
                  end do
                end do
              end if
              bths(1) = ONE
*               if (Noadmiss .eq. 2) then
C   LINES OF CODE COMMENTED FOR X-13A-S : 1               
C                call CLOSEDEVICE(18)
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 1                
*                call CLOSEDEVICE(38)
C   END OF CODE BLOCK
*                Nio = niosave
*                Nidx = nidevice
*               end if
              if (Noadmiss .ne. 2) then
                if ((matopened) .and. (Iter .gt. 0)) then
                    write (buffS,7012)niter, mattitle(1:22)
                end if
              end if  
 7012         format('''<tr><td>''',i4,'''</td><td class="aleft">''',A,
     &               '''</td>'')')
c              Inicializamos nPeakSA       
*              call profiler(2,'before PicosReset')
              call PicosReset(picosSA)
              call PicosReset(picosIr)
              call PicosReset(picosTr)
c
c           call OutPart2(nio,nidx,HTML,z,nz,Lam,ImeanOut,noserie,Pg,Out,
c     $                    iter,Itab,Iid,p,D,q,bp,BD,bq,Nper,Nyer,mq,
c     $                    Wdif,WdifCen,nwDif,WmDifXL,Zvar,VdifXL,
c     $                 QstatXL,df,rXL,seRxl,M,partACF,sePartACF,model,
c     $                    PicosXL,init,tstmean,Wm,seMean,nx,Cmatrix,
c     $                    PHI,TH,BPHI,BTH,sePHI,seTH,seBPHI,seBTH,
c     $                    MArez,MAimz,MAmodul,MAar,MApr,
c     $                    rez,imz,modul,ar,pr,THstar,isVa0)
c
C Modified by REG on 30 Aug 2005 to add nfixed to SIGEX parameter list
*              call profiler(2,'before SIGEX')
*      write(Mtprof,*)' nio = ',nio
*      write(Mtprof,*)' out = ',out
*      write(Mtprof,*)' ItnSearch = ',ItnSearch
*      write(Mtprof,*)' IfnSearch = ',IfnSearch
*      write(Mtprof,*)' FIsearch = ',FIsearch
*      write(Mtprof,*)' nxSearch = ',nxSearch
*      do j=1,nxSearch
*       write(Mtprof,*)' xSearch(',j,'), Esearch(',j,') = ',xSearch(j),
*     *              Esearch(j)
*      end do
              qstar_seats=qstar
              pstar_seats=pstar
*              call profiler(2,'before SIGEX, line 2873')
*              write(Mtprof,*) '  z(1) = ',z(1)
              call SIGEX(z,bz,oz,a,aa,forbias,Ilam,P,D,Q,Bp,Bd,Bq,Mq,
     $               phis,bphis,ths,bths,zaf,zab,imz,rez,modul,ar,
     $               fh,fhi,noserie,Init,Imean,phi,bphi,Th,Bth,status,
     $               hpcycle,rogtable,hplan,HPper,maxSpect,
     $               Type,alpha,acfe,posbphi,printphtrf,
     $               tabtables,IOUT,Ndevice,
     $               printBack,ba,sr,SQSTAT,SDF,SSE,m,
     $               n_1,n0,tvalRUNS,
     $               Qstat,DF,Pstat1,spstat1,
     $               wnormtes,wsk,skewne,test1,wkk,rkurt,test,r,SEa,
     $               Resid,flagTstu,it,iper,iyear,
     $               rmean,rstd,DW,KEN,RTVAL,SumSres,F0,Nyer1,Nper1,
     $               Pstar_seats,Qstar_seats,InputModel,niter,
     $               mattitle,Lgraf,nfixed,
     $               IsCloseToTD,FixParam,x,
     $               ImeanOut,Wdif,WdifCen,nwDif,WmDifXL,VdifXL,
     $               QstatXL,rXL,seRxl,partACF,sePartACF,model,
     $               PicosXL,tstmean,Wm,seMean,nx,Cmatrix,
     $               sePHI,seTH,seBPHI,seBTH,
     $               MArez,MAimz,MAmodul,MAar,MApr,pr,
     $               outNA,stochTD,
     $               ItnSearch,IfnSearch,nxSearch,Esearch,
     $               FIsearch,xSearch,varwnc,numSer,remMeanMCS,*10,*15)
*                  call profiler(2,'SIGEX: did not go to 10,15')
*              call profiler(2,'before addToSumS')
              call addToSumS(mq,IsCloseToTD,crQs,crSNP,crPeaks,F)
              if (IsCloseToTD) then
                aux1=ZERO
                aux2=getSdc()
              else
                aux1=getSdc()
                aux2=ZERO
              end if
              if ((matopened) .and. (Iter .eq. 0)) then
*                  call profiler(2,'before wrHeadTGenSumS')
                  call wrHeadTGenSumS(65)                 
                  CALL writTag(65,'<tr>')
                  write (65,6502)getPat(),getTmcs(),getAna()
 6502             FORMAT(3('<td>',A,'</td>'))
                  write (65,6503)getNmmu(),getNmp(),getNmd(),getNmq(),
     $                           getNmBp(),getNmBd(),getNmBq()
 6503             FORMAT(7('<td>',i2,'</td>'))
                  write (65,6504)getSd(),Ken
 6504             FORMAT(2('<td>',g11.4,'</td>'))
                  write (65,6502)getSf(),getCvar(),getCcc()
                  write (65,6502)getCmtTc(),getCmtS(),getCmtIR()
                  write (65,6505)getCmtTs(),getCmtSA()
 6505             FORMAT(2('<td>',A,'</td>'))
                  CALL writTag(65,'</tr>')
                  CALL writTag(65,'</table>')
                  
                  call wrHeadTparISumS(65,IsCloseToTD)
                  CALL writTag(65,'<tr>')
                  write (65,6506)getSdt(),getSds(),getSdc()
                  write (65,6506)getSdi(),getSdSa(),getSeCect()
                  write (65,6506)getSeCecSa(),getRseCect(),getRseCecSa()
 6506             FORMAT(3('<td>',g11.4,'</td>'))
                  write (65,6507)getT11t(),getT11Sa(),
     $                           getT112x(),getT112t(),getT112Sa()
 6507             FORMAT(5('<td>',f9.2,'</td>'))
                  CALL writTag(65,'</tr>')
                  CALL writTag(65,'</table>')
c
*                  call profiler(2,'before wrHeadTparIISumS')
                  call wrHeadTparIISumS(65)            
                  CALL writTag(65,'<tbody>')
                  CALL writTag(65,'<tr>')
                  write (65,6508)getCovt1(),getCovSa1(),getCovt5(),
     $                           getCovSa5()
 6508             FORMAT(4('<td>',f9.1,'</td>'))
                  write (65,6509)getSsh(),getSsp2(),getSsp2(),
     $             getDaat(),getDaaSa()
 6509             FORMAT(3('<td>',i3,'</td>'),2('<td>',f9.2,'</td>'))
                  CALL writTag(65,'</tr>')
                  CALL writTag(65,'</tbody>')
                  CALL writTag(65,'</table>')
c    Picos espectrales                         
                  if ((mq.eq.12) .or. (mq.eq.4)) then
*                    call profiler(2,'before tablaPicos')
                    call tablaPicos(65,picosSA,picosTr,picosIr,mq,
     $                    totalSeasTR,totalSeasSA,totalSeasIR)
                    call wrResidSeasTest(OST,crQs,crSNP,crPeaks,65)
                  end if     
c   modelo de los componentes
                  if (lu61.ne.' ' .or. lu62.ne.' ' .or. lu63.ne.' ' .or.
     &                lu64.ne.' ' .or. lu64I.ne.' ')
     &                CALL writTagOneLine(65,'h2','@',
     &                                    'Model for the components:')
                  lptag=T
                  if (lu61.ne.' ') then
                   CALL writln('<em>Trend-cycle</em> : '//
     $                         lu61(1:istrlen(lu61))//Cbr,65,0,lptag,F)
                   lptag=F
                  end if
                  if (lu62.ne.' ') then
                   CALL writln('<em>Seasonal</em> : '//
     $                         lu62(1:istrlen(lu62))//Cbr,65,0,lptag,F)
                   if(lptag)lptag=F
                  end if
                  if (lu63.ne.' ') then
                   CALL writln('<em>Seasonally adjusted series</em> :'//
     $                         ' '//lu63(1:istrlen(lu63))//Cbr,65,0,
     $                         lptag,F)
                   if(lptag)lptag=F
                  end if
                  if (lu64.ne.' ') then
                   if (IsCloseToTD) then
                    CALL writln('<em>Stochastic Trading Day</em> : '//
     $                          lu64(1:istrlen(lu64))//Cbr,65,0,lptag,F)
                   else
                    CALL writln('<em>Transitory</em> : '//
     $                          lu64(1:istrlen(lu64))//Cbr,65,0,lptag,F)
                   end if
                   if(lptag)lptag=F
                  end if
                  if (lu64I.ne.' ') then
                   CALL writln('<em>Irregular</em> : '//
     $                        lu64I(1:istrlen(lu64I))//Cbr,65,0,lptag,F)
                  end if
                  if (lu61.ne.' ' .or. lu62.ne.' ' .or. lu63.ne.' ' .or.
     &                lu64.ne.' ' .or. lu64I.ne.' ')
     &                CALL writTag(65,'</p>')
                  CALL writTag(65,'</body></html>')
              else if ((matopened) .and. (Iter .gt. 0)) then
                  write (65,6510)
     $             niter, mattitle(1:22), getPat(),getTmcs(),
     $             getAna(),getNmmu(),getNmp(),getNmd(),getNmq(),
     $             getNmBp(),getNmBd(),getNmBq(),getSd(),Ken,
     $             getSf(),getCvar(),getCcc(),getCmtTc(),getCmtS(),
     $             getCmtIR(),getCmtTs(),getCmtSA()
 6510             format('<tr><td scope="row">',i4,'</td>',
     $                   4('<td>',A,'</td>'),
     $                   3('<td>',I1,'</td>'),'<td>',I2,'</td>',
     $                   3('<td>',I1,'</td>'),/,
     $                   2('<td>',g11.4,'</td>'),
     $                   8('<td>',A,'</td>'),'</tr>')
                  write (66, 6610) niter,mattitle(1:22),
     $             getSdt(),getSds(),aux1,aux2,getSdi(),getSdSa(),
     $             getSeCect(),getSeCecSa(),getRseCect(),getRseCecSa(),
     $             getT11t(),getT11Sa(),getT112x(),getT112t(),
     $             getT112Sa()
 6610             format('<tr><td scope="row">',i4,'</td><td>',
     $                   a,'</td>',/,5('<td>',g11.4,'</td>'),/,
     $                   5('<td>',g11.4,'</td>'),/
     $                   5('<td>',f9.2,'</td>'),'</tr>')
                  write (67, 6710) niter,mattitle(1:22),
     $             getCovt1(),getCovSa1(),getCovt5(),getCovSa5(),
     $             getSsh(),getSsp2(),getSsp2(),getDaat(),getDaaSa()
 6710             format('<tr><td scope="row">',i4,
     $                   '</td><td>',a,'</td>',/,
     $                   4('<td>',f9.1,'</td>'),/,
     $                   3('<td>',I2,'</td>'),
     $                   2('<td>',f9.2,'</td>'),'</tr>')
c                  
                  if ((mq.eq.12) .or. (mq.eq.4)) then
*                    call profiler(2,'before wrLnTabPeaks')
                    call wrLnTabPeaks(69,niter,matTitle,picosSA,1)
                    call wrLnTabPeaks(72,niter,matTitle,picosIr,1)
                    call wrLnTabPeaks(73,niter,matTitle,picosTr,1)
                  end if
                call Mtx1Reset()
                call Mtx2Reset()
              end if
              if (kunits .ne. 0) then
                if (out.eq.0) then
                  ipos=1
                  CALL itoc(-3*kunits,str,ipos)
                  CALL wWritln('to recover the units of the original '//
     $               'input file, the series should be multiplied by '//
     $               '10**'//str(1:(ipos-1))//'.',Nio,0,T,T)
                end if
              end if
              if ((Tramo .eq. 0) .and. (UNITS.eq.1)) then
                if ((sunits.gt.0).and.(Out. eq. 0)) then
                  ipos=1
                  CALL itoc(3*sunits,str,ipos)
                  CALL wWritln('to recover the units of the original '//
     $               'input file, the series should be multiplied by '//
     $               '10**'//str(1:(ipos-1))//'.',Nio,0,T,T)
                end if
                if ((sunits.lt.0) .and. (Out .eq. 0)) then
                  ipos=1
                  CALL itoc(-3*sunits,str,ipos)
                  CALL wWritln('to recover the units of the original '//
     $               'input file, the series should be multiplied by '//
     $               '10**'//str(1:(ipos-1))//'.',Nio,0,T,T)
                end if
              end if
*              call profiler(2,'**GO TO 5119**, line 3040')
              goto 5119
 15           continue
c
 6050         format ('<p><strong>NOTE:</strong> ',
     $                'WHEN BPHI > 0, THE SEASONAL COMPONENT',
     $                ' CANNOT BE PROPERLY DEFINED.',a,
     $                'MODEL IS MODIFIED ACCORDINGLY.</p>')
C   LINES OF CODE COMMENTED FOR X-13A-S : 1              
C 5013         if ((Bjstat1.gt.1.5d0*blqt) .and. (Bjstat1.gt.qmax)) then
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 2
 5013         if ((Bjstat1.gt.1.5d0*blqt) .and.
     &            (Bjstat1.gt.dble(qmax))) then
C   END OF CODE BLOCK 
                if ((Imean.eq.1) .and. (ABS(tmu).lt.1.90d0) .and.
     $             (nprova.eq.0)) then
                  imeansave = Imean
                  Imean = 0
                  nprova = 1
                else
                  Init = 0
                  if (nprova .eq. 1) then
                    nprova = 0
                    Imean = imeansave
                  end if
                  noretry = 1
                  call CLOSEDEVICE(42)
                  Nio = ndevice
*                  Nidx = nidevice
                  WRITE(NIO,623)BJSTAT1,QMAX
  623             FORMAT('<p>RESETTING INIT = 0 BECAUSE RESIDUAL ',
     &                   'LJUNG-BOX Q (',F12.3,') > QMAX (',I5,')</p>')
                end if
              else
                noretry = 1
                call CLOSEDEVICE(42)
                Nio = ndevice
*                Nidx = nidevice
              end if
*              call profiler(2,'**GO TO 10**, line 3080')
              goto 10
            else
              ipos=1
              CALL itoc(2*n1,str,ipos)
              CALL wWritln('BQ GREATER THAN '//str(1:(ipos-1)),
     $                     Nio,0,T,T)
*              call profiler(2,'**GO TO 5119**, line 3087')
              goto 5119
            end if
          else
            ipos=1
            CALL itoc(2*n1,str,ipos)
            CALL wWritln('BP GREATER THAN '//str(1:(ipos-1)),
     $                   Nio,0,T,T)
*            call profiler(2,'**GO TO 5119**, line 3095')
            goto 5119
          end if
        else
          ipos=1
          CALL itoc(3*n1,str,ipos)
          CALL wWritln('Q GREATER THAN '//str(1:(ipos-1)),
     $                 Nio,0,T,T)
*          call profiler(2,'**GO TO 5119**, line 3103')
          goto 5119
        end if
      else
        ipos=1
        CALL itoc(3*n1,str,ipos)
        CALL wWritln('P GREATER THAN '//str(1:(ipos-1)),
     $               Nio,0,T,T)
*        call profiler(2,'**GO TO 5119**, line 3111')
        goto 5119
      end if
 10   continue
 5015 if (Out .eq. 0) then
        CALL wWritln('THE INITIAL VALUES OF THETA AND PHI ARE EQUAL; '//
     $               'THE MODEL IS DEGENERATE',Nio,0,T,T)
      end if
*      call profiler(2,'**GO TO 5119**, line 3119')
      goto 5119
 5016 continue
      CALL wWritln('THE INITIAL VALUES OF BTHETA AND BPHI ARE EQUAL; '//
     $             'THE MODEL IS DEGENERATE',Nio,0,T,T)
*      call profiler(2,'**GO TO 5119**, line 3124')
      goto 5119
 5017 continue
      CALL wWritln('NOT ENOUGHT OBSERVATIONS',Nio,0,T,T)
      CALL wWritln('POSSIBLE ERROR IN SERIES LENGTH',STDERR,Mt2,T,F)
      CALL writln('         PLEASE CHECK SERIES LENGTH FOR THE '//
     &            'SERIES :',STDERR,Mt2,F,F)
      CALL writln('         '//Titleg,STDERR,Mt2,F,T)
      zerr=TWO
      dvec(1)=zerr
      call usrentry(dvec,1,1,1,1,-3)
      if ((matopened) .and. (iter .gt. 0)) then
        noTratadas=noTratadas+1
        call MTX1RESET
        call MTX2RESET
*        call ErrorLog(HTML,'NOT ENOUGHT OBSERVATIONS',1)
        CALL eWritln('NOT ENOUGH OBSERVATIONS',Mt2,STDERR,T,T)
        write (65,6511)
     $        niter, mattitle(1:22),'u','u', 'u', 
     $            -1, -1, -1, -1, -1, -1, -1,DONE, DONE
 6511   format('<tr><td>',i4,'$</td>',/,
     $        4('<td>',a,'</td>'),7('<td>',i2,'</td>'),/,
     $        2('<td>',f9.0,'</td>'),8('<td>u</td>'),'</tr>')
        call OutNoPar(74,niter,mattitle)
        write (66,6611)
     $            niter, mattitle(1:22), DONE, DONE, DONE, DONE, DONE,
     $            DONE, DONE,DONE,DONE, DONE, DONE, DONE, DONE, DONE
 6611   format('<tr><td>',i4,'$</td><td>',a,'</td>',/,
     $         7('<td>',f9.0,'</td>'),/,7('<td>',f9.0,'</td>'),'</tr>')
        write (67,6711)
     $            niter, mattitle(1:22),DONE,DONE, DONE,DONE,
     $            -1, -1, -1, DONE, DONE
 6711   format('<tr><td>',i4,'$</td><td>',a,'</td>',/,
     $         4('<td>',f9.0,'</td>'),3('<td>',i2,'</td>'),/,
     $         2('<td>',f9.0,'</td>'),'</tr>')
        if ((mq.eq.12) .or. (mq.eq.4)) then
            call PicosReset(picosSA)
            call wrLnTabPeaks(69,niter,matTitle,picosSA,1)
            call PicosReset(picosIr)
            call wrLnTabPeaks(72,niter,matTitle,picosIr,1)
            call PicosReset(picosTr)
            call wrLnTabPeaks(73,niter,matTitle,picosTr,1)
        end if
      end if
      if (Iter .eq. 1) then
*        call profiler(2,'**GO TO 20**, line 3169')
        goto 20
      else
*        call profiler(2,'**GO TO 5021**, line 3172')
        goto 5021
      end if
 5018 call OutPart2(nio,z,nz,iLam,ImeanOut,noserie,Pg,Out,Ndec,
     $              iter,p,D,q,bp,BD,bq,Nper,Nyer,mq,
     $              Wdif,WdifCen,nwDif,WmDifXL,Zvar,VdifXL,
     $              QstatXL,df,rXL,seRxl,M,partACF,sePartACF,model,
     $              PicosXL,init,tstmean,Wm,seMean,nx,Cmatrix,
     $              PHI,TH,BPHI,BTH,sePHI,seTH,seBPHI,seBTH,
     $              MArez,MAimz,MAmodul,MAar,MApr,
     $              rez,imz,modul,ar,pr,THstar,F)
      if (out.eq.0) then
        CALL nWritln('NO STOCHASTIC DECOMPOSITION IS PERFORMED '//
     $               'FOR A NOISE OR PURELY ,MOVING AVERAGE MODEL.',
     $               Nio,0,T,F)
        CALL writln(Cbr//'P+D+BP+BD>0 IS REQUIRED.'//Cbr,Nio,0,F,F)
        CALL writln('STOCHASTIC SA SERIES = LINEARIZED SERIES',
     $               Nio,0,F,T)
      end if
      zerr=4.0d0
      dvec(1)=zerr
      call usrentry(dvec,1,1,1,1,-3)
c
c        calculamos componentes y escribimos tablas en tables para Pure MA
      if (tramo .gt. 0) then
        if (Ilam .eq. 1) then
          do i=1,nz+fh
            trStoch(i) = ZERO
            seasStoch(i) = ZERO
            temp(i) = ZERO+ PAREG(i,5)
            trtemp(i) = wm + PAOUTR(i) + PAREG(i,1)+PAREG(i,7)
            stemp(i) = Paeast(i) + Patd(i) + Pareg(i,2) + Paous(i)
            satemp(i) = tram(i) - stemp(i)
            caltemp(i) = PAEAST(i) + PATD(i) + PAREG(i,6)
            pretemp(i) = tram(i) - oz(i)
            irtemp(i) = tram(i) - stemp(i) - trtemp(i)-temp(i)
          end do
          do i=1,nz+fh
            irStoch(i)=oz(i)*1000.0d0**dble(-Kunits)
          enddo
        else
          do i=1,nz+fh
            trStoch(i) = 1000.0d0**dble(Kunits)
            seasStoch(i) = ONE
            temp(i) = ONEHND* PAREG(i,5)
            trtemp(i) = Exp(wm) * PAOUTR(i) * PAREG(i,1) * PAREG(i,7)
            stemp(i) = Paeast(i) * Patd(i) * Pareg(i,2) * 
     $                 Paous(i) *ONEHND
            satemp(i) = tram(i) / (stemp(i)/ONEHND)
            caltemp(i) = PAEAST(i) * PATD(i) * PAREG(i,6)
            irtemp(i) = tram(i) / (stemp(i)/ONEHND) / trtemp(i)
          end do
          do i=1,nz
            pretemp(i) = tram(i) / oz(i)
          end do
          do i=nz+1,nz+fh
            pretemp(i) = tram(i)
          end do
          do i=nz+1,nz+fh
            oz(i) = trStoch(i)
          end do
          do i=1,nz+fh
             irStoch(i)=oz(i)*1000.0d0**dble(-Kunits)
          enddo
        end if
        call USRENTRY(trtemp,1,nz+fh,1,mpkp,1310)
        call USRENTRY(stemp,1,nz+fh,1,mpkp,1311)
        call USRENTRY(temp,1,nz+fh,1,mpkp,1313)
        call USRENTRY(SAtemp,1,nz+fh,1,mpkp,1309)
        call USRENTRY(IRtemp,1,nz+fh,1,mpkp,1312)
        call USRENTRY(trStoch,1,nz+fh,1,mpkp,1200)
        call USRENTRY(seasStoch,1,nz+fh,1,mpkp,1201)
        call USRENTRY(oz,1,nz+fh,1,mpkp,1203)
        call USRENTRY(irStoch,1,nz+fh,1,mpkp,1204)
        if (ITABLE .eq. 1) then
          call OUTTABLE2(Titleg,tram,trtemp,satemp,stemp,irtemp,temp,
     $                   pretemp,caltemp,eresid,numEresid,temp,temp,0,
     $                   Ilam,1,NZ,mq,2,SUNITS,fh,trStoch,oz,oz,
     $                   IsCloseToTD)
        end if
      else
        if (Ilam .eq. 0) then
          do i=1,nz+fh
            temp(i) = ONEHND
            trtemp(i) = Exp(wm)
            stemp(i) = ONEHND
            satemp(i) = oz(i) / (stemp(i)/ONEHND)
            caltemp(i) = ONE
            irtemp(i) = oz(i) / (stemp(i)/ONEHND) / trtemp(i)
            pretemp(i) = ONEHND
          end do
        else
          do i=1,nz+fh
            temp(i) = ZERO
            trtemp(i) = wm
            stemp(i) = ZERO
            satemp(i) = oz(i) - stemp(i)
            caltemp(i) = ZERO
            pretemp(i) = ZERO
            irtemp(i) = oz(i) - stemp(i) - trtemp(i)
          end do
        end if
        if (ITABLE .eq. 1) then
          call OUTTABLE2(Titleg,oz,trtemp,satemp,stemp,irtemp,temp,
     $                   pretemp,caltemp,eresid,numEresid,temp,temp,0,
     $                   ilam,1,NZ,mq,2,SUNITS,fh,trtemp,satemp,satemp,
     $                   IsCloseToTD)
        end if
      end if
c     graficos para los PURE MA        
*      if (pg.eq.0) then
*        call PlotPureMA(oz,satemp,trtemp,stemp,temp,irtemp,iter,out,
*     $                  ioneout,Ttlset,ntltst) 
*      end if
c        calculo de rates of growth para pure ma 

      wrmqx1=-1.d0
      wrmqa1=-1.d0 
c         if (((mq.eq.4) .or. (mq.eq.6) .or. (mq.eq.12)) 
c     $        .and.(tramo .gt. 0))  then
c           if (ilam .eq. 0) then
c            wrmqx1 = (tram(nz+mq/2)/tram(nz-mq/2)-1.0d0) * 100.0d0
c            wrmqa1 = (satemp(nz+mq/2)/satemp(nz-mq/2)-1.0d0) * 100.0d0
c           else
c            wrmqx1 = tram(nz+mq/2) - tram(nz-mq/2)
c            wrmqa1 = satemp(nz+mq/2) - satemp(nz-mq/2)
c           end if
c        end if
c    escribimos una linea en los ficheros sgeneral, sparamii y sparami (Pure Ma)
      if (matopened) then
        if (iter .gt. 0) then
c          call MTX1RESET
c          call MTX2RESET
*          call profiler(2,'before addToSumS')
          call addToSumS(mq,IsCloseToTD,crQs,crSNP,crPeaks,T)
          write (65,6512)
     $        niter, mattitle(1:22),getPat(),getTmcs(), 'N', 
     $        imean, p, d,q,bp,Bd,bq ,
     $        sqf, ken, '-', '-', '-', '-',
     $        '-', '-', '-', '-'
 6512     format('<tr><td scope="row">',i4,'^</td>',/,
     $           4('<td>',a,'</td>'),7('<td>',i2,'</td>'),/,
     $           2('<td>',f9.4,'</td>'),8('<td>',a,'</td>'),/,'</tr>')
c           call OutNoPar(html,74,niter,mattitle)
          NAiter=inputModel-1  
          call OutPara(74,niter,mattitle,NAiter,ImeanOut,
     $          p,d,q,bp,bd,bq,phi,bphi,1,th,bth,1,
     $          qstat,wm,0)
          write (66,6612)
     $       niter, mattitle(1:22), 0,0,0, 
     $       0, sqf, sqf, 0, 0,0,0,0, 0,0, '-','-','-'
c     $      0, 0, wrmqx1,'-',wrmqa1
 6612     format('<tr><td scope="row">',i4,
     $           '^</td>',2('<td>',a,'</td>'),/,
     $           5('<td>',g11.4,'</td>'),/,5('<td>',g11.4,'</td>'),/,
     $           2('<td>',f9.2,'</td>'),3('<td>',a,'</td>'),/,'</tr>')

          write (67,6712)
     $       niter, mattitle(1:22),ZERO,ONEHND,ZERO,ONEHND, 
     $       0, 0, 0, ZERO, ZERO
 6712     format('<tr><td scope="row">',i4,'^</td><td>',a,'</td>',/,
     $           4('<td>',f9.1,'</td>'),3('<td>',i2,'</td>'),/,
     $           2('<td>',f9.2,'</td>'),'</tr>')
          if ((mq.eq.12) .or. (mq.eq.4)) then
              call PicosReset(picosSA)
              call wrLnTabPeaks(69,niter,matTitle,picosSA,1)
              call PicosReset(picosIr)
              call wrLnTabPeaks(72,niter,matTitle,picosIr,1)
              call PicosReset(picosTr)
              call wrLnTabPeaks(73,niter,matTitle,picosTr,1)
          end if  
          call MTX1RESET
          call MTX2RESET
        else
          call wrHeadTGenSumS(65) 
          write (65,6513)
     $        getPat(),getTmcs(), 'N',imean, p, d,q,bp,Bd,bq ,
     $        sqf, ken, '-', '-', '-', '-','-', '-', '-', '-'
 6513     format('<tr>',3('<td>',a,'</td>'),/
     $           7('<td>',i2,'</td>'),/
     $           2('<td>',f9.4,'</td>'),/
     $           8('<td>',a,'</td>'),'</tr></table>')
          CALL mkPOneLine(65,'@','&nbsp;')
          call wrHeadTparISumS(65,F)  
          write (65,6514)
     $        0, 0, 0, sqf, sqf, 0, 0,0,0,0, 0, '-','-','-'
 6514     format('<tr>',5('<td>',g11.4,'</td>'),/
     $          5('<td>',g11.4,'</td>'),/,'<td>',g9.2,'</td>',
     $          3('<td>',a,'</td>'),'</tr></table>')
          call wrHeadTparIISumS(65)
          write (65,6515)
     $       ZERO, ONEHND, ZERO, ONEHND, 0, 0, 0, ZERO, ZERO
 6515     format('<tbody><tr>',4('<td>',f9.1,'</td>'),/,
     $          3('<td>',i2,'</td>'),
     $          2('<td>',f9.2,'</td>'),/,'</tr></tbody></table>')
          CALL mkPOneLine(65,'@','Model is a pure '//
     $                    '<abbr title="moving average">MA</abbr>. '//
     $                    'Not decomposed by Seats.')
          CALL writTag(65,'</body></html>')
        end if
      end if
*      call profiler(2,'**GO TO 5119**, line 3372')
      goto 5119
 5019 if (ilsave .eq. -1) then
          write (Nio,7054) Bp
 7054     format('<p><strong>NOTE:</strong> BP = ',i2,
     $           ', TOO LARGE.  NO DECOMPOSITION IS PERFORMED.</p>')
c           call Snote(Nio)
      end if
      ENTRY HANDLE_POINT ()
 5020 Nsfcast = 0
      Nsfcast1= 0
      if (Handle .eq. 1) then
        Handle = 0
        Nio = Ndevice
        zerr=ONE
        dvec(1)=zerr
        call usrentry(dvec,1,1,1,1,-3)
c          if (Iter .eq. 0) then
c           call closealls()
c          end if
        if ((matopened) .and. (iter .gt. 0)) then
          noTratadas=NoTratadas+1
*          call ErrorLog(HTML,'SEATS RUN TIME ERROR',1)
          CALL eWritln('SEATS RUN TIME ERROR',Mt2,STDERR,T,T)
          call NoTreat2(niter,mattitle)
        end if
        Outdir = soutdir
        Graphdir = sgraphdir
        Nover = inover
        Ioneout = iioneout
        outf=soutfile
      end if
 5119 Nsfcast = 0
      Nsfcast1=0
      if ((Itable.eq.1) .and. (Iter.eq.0)) then
        call CLOSEDEVICE2(36)
      end if
      if (Iter .eq. 2) then
*        call profiler(2,'**GO TO 5022**, line 3410')
        goto 5022
      else if (Iter .eq. 1) then
        niter = niter + 1
        itnSearch = 0
        if (Ioneout .eq. 0) then
          call CLOSEDEVICE2(ndevice)
        end if
      else
*        call profiler(2,'**GO TO 5023**, line 3419')
        goto 5023
      end if
*      call profiler(2,'**GO TO 20**, line 3422')
      go to 20
C
C Commented in order to permit the ENTRY Handle_Point
C 20     continue
 5021 if ((Iter.eq.2) .or. (Iter.eq.3)) then
*        call profiler(2,'**GO TO 25**, line 3428')
        goto 25
      else
*        call profiler(2,'**GO TO 5027**, line 3431')
        goto 5027
      end if
C Modified by REG on 30 Aug 2005 to add nfixed to NMLSTS parameter list
 5022 call NMLSTS(Nochmodel,Type,Init,Ilam,Imean,P,D,Q,Bp,Bd,Bq,
     $            Sqg,Mq,M,iqm,maxit,fh,noserie,Pg,modelsumm,
     $            Out,seas,Noadmiss,OutNA,StochTD,
     $            Iter,qmax,Har,Bias,Tramo,
     $            model,Noutr,Nouir,Nous,Npatd,Npareg,interp,Rsa,
     $            Fortr,Neast,epsiv,Epsphi,ta,Xl,Rmod,
     $            blqt,tmu,Phi,Th,Bphi,Bth,thlim,bthlim,crmean,hplan,
     $            hpcycle,rogtable,centrregs,
     $            statseas,units,kunits,
     $            acfe,posbphi,printphtrf,
     $            tabtables,psieinic,psiefin,
     $            StrFobs,StrLobs,HPper,maxSpect,brol,blamda,
     $            bserie,bmid,bcMark,ODate,OLen,DetSeas,
     $            nds,Nz,nfixed,4,ifail)
      IF(Lfatal)RETURN
      if ((tramo .eq.0) .or. (Tramo .eq. 999))then
        FirstObs=Date2Idx(StrFobs)
        if (FirstObs .eq. -1) then
          FirstObs=1
        end if
        LastObs=Date2Idx(StrLobs)
      else
        FirstObs=1
        LastObs=-1
      end if
      if (OUT .eq. -1) then
        if ((ITER .ge. 2) .and. (NumSer .gt. 25)) Then
          OUT=2
        else
          OUT=0
        end if
      end if
      SeasCheck = 0
      niter = niter + 1
      itnSearch = 0
      if (Ioneout .eq. 0) then
        call CLOSEDEVICE(ndevice)
        if (isopen(71)) then
          write (71,'(''</ul>'')')
          call CLOSEDEVICE2(71)
        end if
      end if
*      call profiler(2,'**GO TO 25**, line 3477')
      goto 25
 5023 if (Iter .eq. 3) then
        niter = niter + 1
        itnSearch = 0
        if (Ioneout .eq. 0) then
          call CLOSEDEVICE2(ndevice)
          if (isopen(71)) then
            write (71,'(''</ul>'')')
            call CLOSEDEVICE2(71)
          end if
        end if
      else
*        call profiler(2,'**GO TO 5027**, line 3490')
        goto 5027
      end if
*      call profiler(2,'**GO TO 25**, line 3493')
      go to 25
C
C Commented in order to permit the ENTRY Handle_Point
C 25    continue
 7055 format (//,6x,'ERROR IN THE NAMELIST "INPUT" ')
 5024 continue
      write (*,7055)
 7056 format (6x,'FOR THE SERIES : ',a,//)
      write (*,7056) Titleg
*      call profiler(2,'**GO TO 6000**, line 3503')
      go to 6000
 5025 continue
      CALL nWritln('TYPE SHOULD BE EITHER 0 OR 1.',Nio,0,T,F)
 6057 format (a,a,a,66('* '),a,24('* '),
     $        'PROCESSING COMPLETED',25('* '),a,66('* '))
      write (Nio,6057)Cbr,Cbr,Cbr,Cbr,Cbr
      CALL writTag(Nio,'</p>')
*      call profiler(2,'**GO TO 5028**, line 3511')
      goto 5028
 5026 continue
      ipos=1
      CALL itoc(mp,str,ipos)
      call nWritln('THE VARIABLE HAS TOO MANY OBSERVATIONS'//
     $             ' ONLY '//str(1:(ipos-1))//' ARE ALLOWED'//Cbr,
     $             Nio,0,T,F)
      write (Nio,6057)
      CALL writTag(Nio,'</p>')
      CALL wWritln('POSSIBLE ERROR IN INPUT FILE',STDERR,Mt2,T,F)
      CALL writln('         PLEASE CHECK SERIES LENGTH FOR THE SERIES:',
     &            STDERR,Mt2,F,F)
      CALL writln('         '//Titleg,STDERR,Mt2,F,T)
C
C Ifail .eq.0
C      end if
C
 5027 if (saved) then
C Modified by REG on 30 Aug 2005 to add nfixed to NMLSTS parameter list
        call NMLSTS(Nochmodel,Type,Init,Ilam,Imean,P,D,Q,Bp,Bd,Bq,
     $              Sqg,Mq,M,iqm,maxit,fh,noserie,Pg,modelsumm,
     $              Out,seas,Noadmiss,OutNA,StochTD,
     $              Iter,qmax,Har,Bias,Tramo,
     $              model,Noutr,Nouir,Nous,Npatd,Npareg,interp,Rsa,
     $              Fortr,Neast,epsiv,Epsphi,ta,Xl,Rmod,
     $              blqt,tmu,Phi,Th,Bphi,Bth,thlim,bthlim,crmean,hplan,
     $              hpcycle,rogtable,centrregs,
     $              statseas,units,kunits,
     $              acfe,posbphi,printphtrf,
     $              tabtables,psieinic,psiefin,
     $              StrFobs,StrLobs,HPper,maxSpect,brol,blamda,
     $              bserie,bmid,bcMark,ODate,OLen,DetSeas,
     $              nds,Nz,nfixed,3,ifail)
        IF(Lfatal)RETURN
      end if
      if ((tramo .eq.0) .or. (Tramo .eq. 999))then
        FirstObs=Date2Idx(StrFobs)
        if (FirstObs .eq. -1) then
          FirstObs=1
        end if
        LastObs=Date2Idx(StrLobs)
        FirstObs=1
        LastObs=-1
      end if
*      if ((Iter.eq.0) .and. (Out.eq.0).and.html.ne.1) then
*       write (Nio,7057)
*      end if
C   LINES OF CODE COMMENTED FOR X-13A-S : 2 
C      time1 = X05BAF()
C      Time = time1 - Time
C   END OF CODE BLOCK
      if ((Itable.eq.1) .and. (Iter.ne.0)) then
        call CLOSEDEVICE(36)
      end if
C   LINES OF CODE COMMENTED FOR X-13A-S : 9
c      if ((Iter.eq.0) .and. (Out.ne.2)) then
c       write (Nio,'(//,A,F7.4,A)') '  ELAPSED TIME : ', Time, ' "'
c      end if
c      if ((Iter.eq.0) .and. (Out.ne.2)) then
c       write (Nio,7057)
c      end if
c 5028 call CLOSEINFILE
c      call CLOSEDEVICE(ndevice)
c      if ((matopened) .and. (Iter .gt. 0)) then
C   END OF CODE BLOCK
 5028 continue
      if ((Itable.eq.1) .and. (Iter.ne.0)) then
        call CLOSEDEVICE2(36)
      end if
      if (isopen(71)) then
        CALL writTag(71,'</ul>')
        CALL writTag(71,'</body>')
        CALL writTag(71,'</html>')
      end if
      if ((matopened) .and. (Iter .gt. 0)) then
c  move call to aaamain
*        if (modelsumm.eq.1) then
*          call writeSumS(numser,noTratadas,serSet,wSposBphi,
*     $            wSstochTD,wSstatseas,wSrmod,wSxl)
*        end if
        call closeCompMatrix()
        call closeOldMatrix()
        call closePeaksMatrix(69)
        call closePeaksMatrix(72)
        call closePeaksMatrix(73)
      else if (matopened) then
        call CLOSEDEVICE(65)
      end if
      if (numser.gt.1) then
        call closedevice(86)
      end if
      if (Momopened) then
        call CLOSEDEVICE(80)
        call CLOSEDEVICE(81)
        call CLOSEDEVICE(82)
      end if
      if ((Iter.ne.0) .and. (Ioneout.eq.0)) then
        call CLOSEDEVICE(17)
      end if
      if ((Iter.ne.0) .and. (Ioneout.eq.0).and.(out.eq.0)) then
        call CLOSEDEVICE(47)
      end if
      if ((Iter.ne.0) .and. (Ioneout.eq.0)) then
       call CLOSEDEVICE(27)
      end if
      if ((Iter.ne.0) .and. (Ioneout.eq.1)) then
       call CLOSEDEVICE(22)
      end if
      if ((out.lt.3) .and.(rogtable.eq.1)) then
       call CLOSEDEVICE(54)
      end if
C..
C This part is for test to be removed
C
*      if (Itbl .eq. 1) then
*       call CLOSEDEVICE(87)
*      end if
C..
C End part for test to be removed
C
 6000 if ((Iter .gt. 0) .and. (niter .ge. 25)) then
cdos
cdos       filename=Outdir(1:ISTRLEN(Outdir)) // '\\Seats.log'
cunix
       filename=Outdir(1:ISTRLEN(Outdir)) // '/Seats.log'
       call OPENDEVICE (filename,44,0,ifail)
       call SEATSLOG(SerSet,niter-1)
       call CLOSEDEVICE(44)
      end if
*      close(17)
*      close(27)
*      close(22)
*      close(12)
*      close(18)
*      close(44)
*      close(36)
*      close(37)
*      close(16)
*      close(8)
*      close(70)
*      close(71)
      call closealls()
CUNX#ifdef PROFILER
!DEC$ IF DEFINED (PROFILER)
*      call profiler(1,'SEATS')
!DEC$ ENDIF             
CUNX#endif
      return
      end
      subroutine closealls()
      include 'stream.i'
      close(17)
*      close(27)
      close(47)
      close(22)
*      close(12)
      close(18)
      close(44)
      close(36)
      close(16)
*      close(8)
      close(70)
      close(71)
      close(61)
      close(62)
      close(63)
      close(64)
      close(65)
      close(69)
      close(72)
      close(73)
      close(56)
      return
      end

      subroutine PicosReset(picos)
C 
C.. Implicits .. 
      implicit none
C
C.. Formal Arguments ..
      character Picos(7)*2
C 
C.. Local Scalars .. 
      integer i
      do i=1,7
       Picos(i)='--'
      enddo
      return
      end
c
c
c     NoTreat2: write the matrix line corresponding to this series indicating that was not treated
      subroutine NoTreat2(niter,mattitle)
      implicit none
c     INPUT
      integer html,niter
      character mattitle*180
c     LOCAL
      real*8 DONE
      parameter (DONE=-1.0D0)
      character picos(7)*2
c -------------------------------
c          peaks.m   => unit=69
c          peaksIr.m => unit=72
c          peaksTr.m => unit=73
c          trendmod.m=> unit=61
c          SAmod.m   => unit=63
c          Seasmod.m => unit=62
c          transmod.m=> unit=64
c         
           call picosReset(picos)
           call wrLnTabPeaks(69,niter,mattitle,picos,1)
           call picosReset(picos)
           call wrLnTabPeaks(72,niter,mattitle,picos,1)
           call picosReset(picos)
           call wrLnTabPeaks(73,niter,mattitle,picos,1)
           call Mtx1Reset()
           call Mtx2Reset()
           write (61,6000) niter,mattitle(1:22)
           write (62,6000) niter,mattitle(1:22)
           write (63,6000) niter,mattitle(1:22)
           write (64,6000) niter,mattitle(1:22)
           write (65,6001)
     $         niter, mattitle(1:22),'u','u', 'u', 
     $         -1, -1, -1, -1, -1, -1, -1,
     $         DONE, DONE, 'u', 'u', 'u', 'u',
     $         'u', 'u', 'u', 'u'
            write (66,6002)
     $       niter, mattitle(1:22), DONE, 
     $       DONE, DONE, DONE, DONE,
     $       DONE, DONE, DONE, DONE,
     $       DONE, DONE, DONE, DONE, DONE
            write (67,6003)
     $       niter, mattitle(1:22),DONE, 
     $       DONE, DONE, DONE,
     $       -1, -1, -1, DONE, DONE
 6000 format('<tr><td>',i4,'*</td><td>',a,'</td></tr>')
 6001 format('<tr><td>',i4,'*</td>',4('<td>',a,'</td>'),/,
     $       7('<td>',i2,'</td>'),5('<td>',i2,'</td>'),/,
     $       2('<td>',f9.0,'</td>'),8('<td>',a,'</td>'),/,
     $       '</tr>')
 6002 format('<tr><td>',i4,'*</td><td>',a,'</td>',/,
     $       5('<td>',f9.0,'</td>'),/,5('<td>',f9.0,'</td>'),/,
     $       4('<td>',f9.0,'</td>'),/,'</tr>')
 6003 format('<tr><td>',i4,'*</td><td>',a,'</td>',
     $       4('<td>',f9.0,'</td>'),3('<td>',i2,'</td>'),
     $       2('<td>',f9.0,'</td>'),'</tr>')
      end
 
*      subroutine outARMAParam()
*      IMPLICIT NONE
*C-----------------------------------------------------------------------
*      integer n1,n12
*      parameter (n12 = 12, n1 = 1)
*C-----------------------------------------------------------------------
*      INCLUDE 'srslen.prm'
*      INCLUDE 'dimensions.i'
*      INCLUDE 'calc.i'
*      INCLUDE 'units.cmn'
*C-----------------------------------------------------------------------
*      INTEGER i
*C-----------------------------------------------------------------------
*      IF(P.gt.0)THEN
*       DO i = 1, P
*        WRITE(Mtprof,*) 'phi(',i,') = ',Phi(i)
*       END DO
*      END IF
*C-----------------------------------------------------------------------
*      IF(BP.gt.0)THEN
*       DO i = 1, BP
*        WRITE(Mtprof,*) 'bphi(',i,') = ',BPhi(i)
*       END DO
*      END IF
*C-----------------------------------------------------------------------
*      IF(Q.gt.0)THEN
*       DO i = 1, Q
*        WRITE(Mtprof,*) 'th(',i,') = ',Th(i)
*       END DO
*      END IF
*C-----------------------------------------------------------------------
*      IF(BQ.gt.0)THEN
*       DO i = 1, BQ
*        WRITE(Mtprof,*) 'bth(',i,') = ',BTh(i)
*       END DO
*      END IF
*C-----------------------------------------------------------------------
*      RETURN
*      END
